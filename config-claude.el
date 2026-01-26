;;; ~/.doom.d/config-claude.el -*- lexical-binding: t; -*-

;; Claude Code CLI integration via claude-code.el
;; https://github.com/stevemolitor/claude-code.el
;;
;; Keybindings (C-c c prefix):
;;   C-c c c - Start Claude in current project
;;   C-c c s - Send command/prompt to Claude
;;   C-c c r - Send region to Claude
;;   C-c c b - Send buffer to Claude
;;   C-c c t - Toggle Claude buffer visibility
;;   C-c c m - Transient menu with all commands
;;   C-c c y - Answer "yes" to Claude prompt
;;   C-c c n - Answer "no" to Claude prompt
;;   C-c c k - Kill Claude instance
;;   C-c c ? - Jump to last question (what is Claude asking?)
;;   C-c c a - Jump to last action (what did Claude do?)
;;   C-c c S - Show summary of recent activity
;;
;; Debugging & Session Management (C-c c d prefix):
;;   C-c c d d - Quick debug state (message)
;;   C-c c d x - Full debug dump (*Claude Debug* buffer)
;;   C-c c d k - Kill stuck Claude processes (in Emacs)
;;   C-c c d l - List all sessions (buffers + system processes)
;;   C-c c d o - Kill orphan processes (no Emacs buffer)
;;
;; In Claude/vterm buffer:
;;   C-c /   - Completion at point (works even if read-only)
;;   C-c TAB - Completion at point (alternate binding)
;;
;; Workflow commands (C-c c w prefix):
;;   C-c c w t - Generate test plan (/test-plan)
;;   C-c c w r - Review branch changes
;;   C-c c w f - Fix error (region or prompt)
;;   C-c c w e - Explain code (region or file)
;;   C-c c w c - Suggest commit message

;; Load claude-code after vterm is available
(after! vterm
  (require 'claude-code)

  ;; Use vterm as the terminal backend (already have it via Doom)
  (setq claude-code-terminal-backend 'vterm)

  ;; Global display rule: Claude buffers reuse existing Claude window
  ;; or display in same window - NEVER split or take over other windows
  (add-to-list 'display-buffer-alist
               '("\\*claude:"
                 (display-buffer-reuse-window
                  display-buffer-same-window)
                 (reusable-frames . visible)
                 (inhibit-same-window . nil)
                 ;; Prevent Claude from deleting other windows
                 (inhibit-switch-frame . t)
                 (preserve-size . (t . nil))))

  ;; Custom command that captures window first, then starts Claude
  (defun claude-code-here ()
    "Start Claude in the current window (replaces current buffer).
Use this instead of claude-code to ensure Claude opens HERE."
    (interactive)
    (let ((target-window (selected-window)))
      ;; Suppress all display-buffer shenanigans
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _)
                   (set-window-buffer target-window buffer)
                   target-window))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (set-window-buffer target-window buffer)
                   (select-window target-window)
                   target-window)))
        (claude-code))))

  ;; Also set the display fn as backup (wrap to provide required alist arg)
  (setq claude-code-display-window-fn
        (lambda (buf) (display-buffer-same-window buf nil)))

  ;; Bind to C-c c prefix via ashton-mode-map for consistency
  (define-key ashton-mode-map (kbd "C-c c c") #'claude-code-here)
  (define-key ashton-mode-map (kbd "C-c c s") #'claude-code-send-command)
  (define-key ashton-mode-map (kbd "C-c c r") #'claude-code-send-region)
  (define-key ashton-mode-map (kbd "C-c c b") #'claude-code-send-buffer)
  (define-key ashton-mode-map (kbd "C-c c t") #'claude-code-toggle-buffer)
  (define-key ashton-mode-map (kbd "C-c c m") #'claude-code-transient-menu)
  (define-key ashton-mode-map (kbd "C-c c y") #'claude-code-yes)
  (define-key ashton-mode-map (kbd "C-c c n") #'claude-code-no)
  (define-key ashton-mode-map (kbd "C-c c k") #'claude-code-kill)
  (define-key ashton-mode-map (kbd "C-c c z") #'claude-code-toggle-read-only-mode)
  (define-key ashton-mode-map (kbd "C-c c M") #'claude-code-cycle-mode)
  ;; Navigation - "what did Claude do?"
  (define-key ashton-mode-map (kbd "C-c c ?") #'claude-jump-to-last-question)
  (define-key ashton-mode-map (kbd "C-c c a") #'claude-jump-to-last-action)
  (define-key ashton-mode-map (kbd "C-c c S") #'claude-summary))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Window Resize Handling
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Claude Code's rich output (progress bars, task lists) doesn't handle
;; terminal width changes gracefully. This causes garbled display when
;; windows are resized smaller.
;;
;; Solution:
;; 1. Set minimum/maximum window width to prevent tiny or overly wide windows
;; 2. When window width changes significantly, force vterm to resize properly
;; 3. Lock windows to prevent accidental resize during output

(defcustom claude-min-window-width 80
  "Minimum width for Claude windows.
Windows showing Claude buffers will resist shrinking below this width."
  :type 'integer
  :group 'claude-code)

(defcustom claude-max-window-width 140
  "Maximum width for Claude windows.
New Claude windows will be constrained to this width."
  :type 'integer
  :group 'claude-code)

(defvar claude--window-widths (make-hash-table :test 'eq)
  "Hash table tracking initial width of windows showing Claude buffers.")

(defun claude--window-showing-claude-p (window)
  "Return t if WINDOW is showing a Claude buffer."
  (and (window-live-p window)
       (string-prefix-p "*claude:" (buffer-name (window-buffer window)))))

(defun claude--reset-vterm-size (buffer)
  "Force BUFFER's vterm to recalculate its size.
This helps fix garbled output after resize."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'vterm-mode)
        ;; Force vterm to recalculate its terminal size
        (when (fboundp 'vterm--set-size)
          (let* ((win (get-buffer-window buffer))
                 (width (when win (window-width win)))
                 (height (when win (window-height win))))
            (when (and width height)
              ;; Set size slightly smaller then back to force refresh
              (ignore-errors
                (vterm--set-size height (1- width))
                (run-at-time 0.1 nil
                             (lambda ()
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (when (and (fboundp 'vterm--set-size) win (window-live-p win))
                                     (vterm--set-size
                                      (window-height win)
                                      (window-width win)))))))))))))))

(defun claude--constrain-window-width (window)
  "Ensure WINDOW showing Claude is within width constraints.
Returns t if window was resized."
  (when (and (window-live-p window)
             (claude--window-showing-claude-p window))
    (let* ((width (window-width window))
           (frame-width (frame-width))
           (target-width nil))
      (cond
       ;; Too narrow - try to expand
       ((and (< width claude-min-window-width)
             (> frame-width claude-min-window-width))
        (setq target-width claude-min-window-width))
       ;; Too wide - try to shrink
       ((and (> width claude-max-window-width)
             ;; Only constrain if there's room for other content
             (> (length (window-list)) 1))
        (setq target-width claude-max-window-width)))
      (when target-width
        (let ((delta (- target-width width)))
          (ignore-errors
            (window-resize window delta t))
          t)))))

(defun claude--handle-window-resize (frame)
  "Handle window resize events for FRAME.
Resets vterm size when Claude windows change significantly."
  (dolist (window (window-list frame 'no-mini))
    (when (claude--window-showing-claude-p window)
      (let* ((buf (window-buffer window))
             (current-width (window-width window))
             (initial-width (gethash window claude--window-widths)))
        ;; Track initial width if not set
        (unless initial-width
          (puthash window current-width claude--window-widths)
          (setq initial-width current-width))
        ;; If width changed significantly, force vterm to recalculate
        (when (and initial-width
                   (> (abs (- current-width initial-width)) 5))
          (claude--reset-vterm-size buf)
          ;; Update tracked width
          (puthash window current-width claude--window-widths))))))

(defun claude--window-size-change-hook (frame)
  "Hook for `window-size-change-functions'.
Handles resize events for Claude windows in FRAME."
  ;; First enforce constraints
  (dolist (window (window-list frame 'no-mini))
    (claude--constrain-window-width window))
  ;; Then handle any resize effects
  (claude--handle-window-resize frame))

;; Register the hook
(add-hook 'window-size-change-functions #'claude--window-size-change-hook)

;; Clean up dead windows from tracking hash
(defun claude--cleanup-window-tracking ()
  "Remove dead windows from tracking hash table."
  (let ((dead-keys '()))
    (maphash (lambda (k _v)
               (unless (window-live-p k)
                 (push k dead-keys)))
             claude--window-widths)
    (dolist (k dead-keys)
      (remhash k claude--window-widths))))

(run-with-idle-timer 60 t #'claude--cleanup-window-tracking)

(defun claude-reset-window ()
  "Reset current Claude window - fixes garbled display after resize."
  (interactive)
  (when (claude-buffer-p)
    (claude--reset-vterm-size (current-buffer))
    (message "Reset vterm size")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Debugging - Hang diagnosis
;;; ════════════════════════════════════════════════════════════════════════════

(defun claude-debug-state ()
  "Dump Claude buffer state for debugging hangs."
  (interactive)
  (let ((buf (claude-get-buffer)))
    (if (not buf)
        (message "No Claude buffer found")
      (with-current-buffer buf
        (let* ((proc (get-buffer-process buf))
               (vterm-proc (bound-and-true-p vterm--process)))
          (message "Buffer: %s\nSize: %d bytes\nBuffer-proc: %s (%s)\nVterm-proc: %s (%s)\nReadOnly: %s\nPoint: %d/%d"
                   (buffer-name)
                   (buffer-size)
                   proc
                   (when proc (process-status proc))
                   vterm-proc
                   (when vterm-proc (process-status vterm-proc))
                   (bound-and-true-p claude-code-read-only-mode)
                   (point) (point-max)))))))

(defun claude-debug-dump ()
  "Dump extensive debug info to *Claude Debug* buffer."
  (interactive)
  (let ((debug-buf (get-buffer-create "*Claude Debug*"))
        (claude-bufs (cl-remove-if-not
                      (lambda (b) (string-prefix-p "*claude:" (buffer-name b)))
                      (buffer-list))))
    (with-current-buffer debug-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Claude Debug Dump ===\n")
        (insert (format "Time: %s\n\n" (current-time-string)))

        ;; All Claude buffers
        (insert "== Claude Buffers ==\n")
        (if (null claude-bufs)
            (insert "  (none)\n")
          (dolist (buf claude-bufs)
            (with-current-buffer buf
              (let* ((proc (get-buffer-process buf))
                     (vterm-proc (bound-and-true-p vterm--process)))
                (insert (format "\n%s:\n" (buffer-name)))
                (insert (format "  Size: %d bytes\n" (buffer-size)))
                (insert (format "  Buffer-proc: %s (%s)\n" proc
                                (when proc (process-status proc))))
                (insert (format "  Vterm-proc: %s (%s)\n" vterm-proc
                                (when vterm-proc (process-status vterm-proc))))
                (insert (format "  Read-only: %s\n"
                                (bound-and-true-p claude-code-read-only-mode)))
                (insert (format "  Point: %d / %d\n" (point) (point-max)))
                ;; Last 10 lines
                (insert "  Last output:\n")
                (save-excursion
                  (goto-char (point-max))
                  (forward-line -10)
                  (let ((tail (buffer-substring-no-properties (point) (point-max))))
                    (dolist (line (split-string tail "\n"))
                      (insert (format "    | %s\n" (truncate-string-to-width line 60 nil nil "..."))))))))))

        ;; Timers
        (insert "\n== Active Timers ==\n")
        (dolist (timer timer-list)
          (insert (format "  %s\n" timer)))

        ;; Processes
        (insert "\n== All Processes ==\n")
        (dolist (proc (process-list))
          (insert (format "  %s: %s\n" (process-name proc) (process-status proc))))

        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer debug-buf)))

(defun claude-kill-stuck ()
  "Force-kill stuck Claude processes."
  (interactive)
  (let ((killed 0))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*claude:" (buffer-name buf))
        (let ((proc (get-buffer-process buf)))
          (when proc
            (delete-process proc)
            (cl-incf killed)))))
    (message "Killed %d Claude process(es)" killed)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Process Management - Systemic cleanup
;;; ════════════════════════════════════════════════════════════════════════════

(defun claude--get-buffer-tty (buf)
  "Get the tty name for claude BUF."
  (when-let ((proc (get-buffer-process buf)))
    (process-tty-name proc)))

(defun claude--get-system-claude-pids ()
  "Get list of (pid . tty) for all system claude processes."
  (let ((output (shell-command-to-string "ps -eo pid,tty,cmd | grep -E '[c]laude$'"))
        (results nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^\\s-*\\([0-9]+\\)\\s-+\\(pts/[0-9]+\\|\\?\\)" line)
        (push (cons (string-to-number (match-string 1 line))
                    (match-string 2 line))
              results)))
    results))

(defun claude-list-sessions ()
  "List all Claude sessions - both Emacs buffers and system processes."
  (interactive)
  (let* ((buf (get-buffer-create "*Claude Sessions*"))
         (emacs-claudes (cl-remove-if-not
                         (lambda (b) (string-prefix-p "*claude:" (buffer-name b)))
                         (buffer-list)))
         (emacs-ttys (mapcar (lambda (b)
                               (cons (claude--get-buffer-tty b) b))
                             emacs-claudes))
         (system-pids (claude--get-system-claude-pids)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Claude Sessions ===\n\n")

        ;; Emacs buffers
        (insert (propertize "== Emacs Buffers ==\n" 'face 'bold))
        (if (null emacs-claudes)
            (insert "  (none)\n")
          (dolist (b emacs-claudes)
            (let* ((tty (claude--get-buffer-tty b))
                   (tty-short (when tty (replace-regexp-in-string "/dev/" "" tty))))
              (insert (format "  %s [%s]\n"
                              (buffer-name b)
                              (or tty-short "no tty"))))))

        ;; System processes
        (insert (propertize "\n== System Processes ==\n" 'face 'bold))
        (if (null system-pids)
            (insert "  (none)\n")
          (dolist (entry system-pids)
            (let* ((pid (car entry))
                   (tty (cdr entry))
                   (has-buffer (cl-find-if (lambda (pair)
                                             (and (car pair)
                                                  (string-suffix-p tty (car pair))))
                                           emacs-ttys)))
              (insert (format "  PID %d [%s] %s\n"
                              pid tty
                              (if has-buffer
                                  (propertize "← has buffer" 'face 'success)
                                (propertize "← ORPHAN" 'face 'warning)))))))

        ;; Summary
        (let ((orphans (cl-remove-if
                        (lambda (entry)
                          (let ((tty (cdr entry)))
                            (cl-find-if (lambda (pair)
                                          (and (car pair)
                                               (string-suffix-p tty (car pair))))
                                        emacs-ttys)))
                        system-pids)))
          (insert (format "\n%d buffers, %d processes, %d orphans\n"
                          (length emacs-claudes)
                          (length system-pids)
                          (length orphans)))
          (when orphans
            (insert "\nPress 'K' to kill orphan processes\n")))

        (goto-char (point-min))
        (special-mode)
        (local-set-key (kbd "K") #'claude-kill-orphans)
        (local-set-key (kbd "g") #'claude-list-sessions)
        (local-set-key (kbd "q") #'quit-window)))
    (pop-to-buffer buf)))

(defun claude-kill-orphans ()
  "Kill claude processes that have no associated Emacs buffer."
  (interactive)
  (let* ((emacs-claudes (cl-remove-if-not
                         (lambda (b) (string-prefix-p "*claude:" (buffer-name b)))
                         (buffer-list)))
         (emacs-ttys (delq nil (mapcar #'claude--get-buffer-tty emacs-claudes)))
         (system-pids (claude--get-system-claude-pids))
         (orphans (cl-remove-if
                   (lambda (entry)
                     (let ((tty (cdr entry)))
                       (cl-find-if (lambda (etty)
                                     (string-suffix-p tty etty))
                                   emacs-ttys)))
                   system-pids))
         (killed 0))
    (dolist (entry orphans)
      (let ((pid (car entry)))
        (when (yes-or-no-p (format "Kill orphan claude PID %d? " pid))
          (shell-command (format "kill -9 %d" pid))
          (cl-incf killed))))
    (message "Killed %d orphan process(es)" killed)
    (when (get-buffer "*Claude Sessions*")
      (claude-list-sessions))))

(defun claude--cleanup-on-buffer-kill ()
  "Kill the claude process when its buffer is killed."
  (when (string-prefix-p "*claude:" (buffer-name))
    (let ((proc (get-buffer-process (current-buffer))))
      (when (and proc (process-live-p proc))
        (let ((tty (process-tty-name proc)))
          ;; Kill the process
          (delete-process proc)
          ;; Also kill any system process on that tty (in case of orphaning)
          (when tty
            (let ((tty-short (replace-regexp-in-string "/dev/" "" tty)))
              (dolist (entry (claude--get-system-claude-pids))
                (when (equal (cdr entry) tty-short)
                  (shell-command (format "kill -9 %d 2>/dev/null" (car entry))))))))))))

;; Register cleanup hook
(add-hook 'kill-buffer-hook #'claude--cleanup-on-buffer-kill)

;; Debug bindings - use C-c c d prefix (terminal-safe)
(define-key ashton-mode-map (kbd "C-c c d d") #'claude-debug-state)
(define-key ashton-mode-map (kbd "C-c c d x") #'claude-debug-dump)
(define-key ashton-mode-map (kbd "C-c c d k") #'claude-kill-stuck)
(define-key ashton-mode-map (kbd "C-c c d l") #'claude-list-sessions)
(define-key ashton-mode-map (kbd "C-c c d o") #'claude-kill-orphans)
(define-key ashton-mode-map (kbd "C-c c d r") #'claude-reset-window)  ; Reset vterm size after garbled display

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Buffer Navigation - "What did Claude do?"
;;; ════════════════════════════════════════════════════════════════════════════

(defvar claude-action-patterns
  '(("Tool Use" . "\\(Read\\|Edit\\|Write\\|Bash\\|Glob\\|Grep\\|Task\\)")
    ("Question" . "\\?$")
    ("Waiting" . "\\(yes/no\\|Y/n\\|y/N\\|proceed\\|continue\\)")
    ("Error" . "\\(Error\\|error\\|failed\\|Failed\\|FAILED\\)")
    ("Created" . "\\(Created\\|created\\|Wrote\\|wrote\\)")
    ("Modified" . "\\(Modified\\|modified\\|Updated\\|updated\\|Edited\\|edited\\)"))
  "Patterns to identify Claude actions in output.")

(defun claude-buffer-p (&optional buffer)
  "Return t if BUFFER is a Claude Code buffer."
  (let ((buf (or buffer (current-buffer))))
    (string-match-p "\\*claude:" (buffer-name buf))))

(defun claude-get-buffer ()
  "Get the current project's Claude buffer."
  (let ((project-root (or (projectile-project-root) default-directory)))
    (cl-find-if (lambda (buf)
                  (and (claude-buffer-p buf)
                       (string-match-p (regexp-quote project-root) (buffer-name buf))))
                (buffer-list))))

(defun claude-jump-to-last-question ()
  "Jump to Claude's last question or prompt in the buffer."
  (interactive)
  (let ((buf (or (and (claude-buffer-p) (current-buffer))
                 (claude-get-buffer))))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (if (re-search-backward "\\(\\?$\\|yes/no\\|Y/n\\|proceed\\|continue\\)" nil t)
              (progn
                (pop-to-buffer buf)
                (recenter))
            (message "No questions found")))
      (message "No Claude buffer found"))))

(defun claude-jump-to-last-action ()
  "Jump to Claude's last tool use/action."
  (interactive)
  (let ((buf (or (and (claude-buffer-p) (current-buffer))
                 (claude-get-buffer))))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (if (re-search-backward "\\(Read\\|Edit\\|Write\\|Bash\\|Created\\|Modified\\)" nil t)
              (progn
                (pop-to-buffer buf)
                (beginning-of-line)
                (recenter))
            (message "No actions found")))
      (message "No Claude buffer found"))))

(defun claude-summary ()
  "Show a summary of Claude's recent actions in a popup."
  (interactive)
  (let ((buf (or (and (claude-buffer-p) (current-buffer))
                 (claude-get-buffer))))
    (if (not buf)
        (message "No Claude buffer found")
      (let ((summary '())
            (lines (with-current-buffer buf
                     (save-excursion
                       (goto-char (point-max))
                       ;; Get last ~100 lines
                       (forward-line -100)
                       (buffer-substring-no-properties (point) (point-max))))))
        ;; Find interesting lines
        (dolist (line (split-string lines "\n"))
          (dolist (pattern claude-action-patterns)
            (when (string-match-p (cdr pattern) line)
              (push (format "[%s] %s"
                            (car pattern)
                            (truncate-string-to-width line 70 nil nil "..."))
                    summary))))
        (if summary
            (with-current-buffer (get-buffer-create "*Claude Summary*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (propertize "CLAUDE RECENT ACTIVITY\n" 'face 'bold))
                (insert (propertize "Press 'q' to close, 'g' to refresh\n\n" 'face 'font-lock-comment-face))
                (dolist (item (seq-take (reverse summary) 20))
                  (insert item "\n"))
                (goto-char (point-min))
                (special-mode)
                (local-set-key (kbd "g") #'claude-summary)
                (local-set-key (kbd "j") #'claude-jump-to-last-action)
                (local-set-key (kbd "?") #'claude-jump-to-last-question))
              (pop-to-buffer (current-buffer)))
          (message "No recent activity found"))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Completion System - Corfu Integration
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Full Corfu-style completion for Claude buffers with:
;; - Slash commands with documentation
;; - File path completion
;; - Dabbrev from all buffers
;; - Orderless fuzzy matching
;; - Sexy inline popup via Corfu
;;
;; Keybindings:
;;   TAB       - Trigger completion (auto-triggers after 2 chars)
;;   C-c /     - Slash commands only
;;   M-/       - Force completion popup

(defun claude--ensure-writable ()
  "Ensure Claude buffer is not in read-only mode for insertion."
  (when (and (claude-buffer-p)
             (bound-and-true-p claude-code-read-only-mode))
    (claude-code-toggle-read-only-mode)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Slash Commands - with documentation for annotations
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar claude-slash-commands
  '(("/help"           . "Show help and available commands")
    ("/clear"          . "Clear conversation history")
    ("/compact"        . "Compact conversation to save context")
    ("/status"         . "Show Claude status and session info")
    ("/doctor"         . "Run diagnostics on Claude setup")
    ("/config"         . "Configure Claude settings")
    ("/init"           . "Initialize project configuration")
    ("/memory"         . "Manage conversation memory")
    ("/add-dir"        . "Add directory to context")
    ("/terminal-setup" . "Setup terminal integration")
    ("/login"          . "Login to Anthropic account")
    ("/logout"         . "Logout from Anthropic account")
    ("/model"          . "Select AI model (opus/sonnet/haiku)")
    ("/permissions"    . "Manage file/tool permissions")
    ("/cost"           . "Show token usage and costs")
    ("/review"         . "Review code changes")
    ("/pr_comments"    . "Fetch and show PR comments")
    ("/agents"         . "List and manage sub-agents")
    ("/vim"            . "Toggle vim keybinding mode")
    ("/mcp"            . "MCP server management")
    ("/bug"            . "Report a bug to Anthropic")
    ("/test-plan"      . "Generate test plan for changes")
    ("/pr-description" . "Generate PR description")
    ("/diff"           . "Show git diff context")
    ("/context"        . "Show current context window usage"))
  "Claude slash commands with descriptions for completion annotations.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Capf Functions - completion-at-point-functions for Corfu
;;; ─────────────────────────────────────────────────────────────────────────────

(defun claude-slash-capf ()
  "Completion-at-point function for Claude slash commands.
Returns completions with annotations showing command descriptions."
  (when (derived-mode-p 'vterm-mode)
    (let* ((line-start (save-excursion (vterm-beginning-of-line) (point)))
           (before-point (buffer-substring-no-properties line-start (point))))
      ;; Only complete if we're at beginning of input or after whitespace
      (when (string-match-p "\\(?:^\\|\\s-\\)/[a-z-]*$" before-point)
        (let* ((slash-pos (save-excursion
                            (when (re-search-backward "/" line-start t)
                              (point))))
               (start (or slash-pos (point)))
               (end (point))
               (prefix (buffer-substring-no-properties start end)))
          (when (string-prefix-p "/" prefix)
            (list start end
                  (mapcar #'car claude-slash-commands)
                  :annotation-function
                  (lambda (cmd)
                    (when-let ((desc (cdr (assoc cmd claude-slash-commands))))
                      (concat "  " (propertize desc 'face 'font-lock-comment-face))))
                  :company-docsig
                  (lambda (cmd)
                    (cdr (assoc cmd claude-slash-commands)))
                  :exclusive 'no)))))))

(defun claude-file-capf ()
  "Completion-at-point function for file paths in Claude buffers.
Activates when typing paths starting with / ~ or ./"
  (when (derived-mode-p 'vterm-mode)
    (let* ((bounds (bounds-of-thing-at-point 'filename))
           (start (car bounds))
           (end (cdr bounds)))
      (when (and start end)
        (let ((prefix (buffer-substring-no-properties start end)))
          (when (string-match-p "^[/~.]" prefix)
            (let* ((dir (or (file-name-directory prefix) default-directory))
                   (base (file-name-nondirectory prefix))
                   (files (when (file-directory-p dir)
                            (file-name-all-completions base dir))))
              (when files
                (list start end
                      (mapcar (lambda (f) (concat dir f)) files)
                      :annotation-function
                      (lambda (path)
                        (cond
                         ((file-directory-p path) "  [dir]")
                         ((file-executable-p path) "  [exe]")
                         (t nil)))
                      :exclusive 'no)))))))))

(defun claude-dabbrev-capf ()
  "Completion-at-point function for dabbrev in Claude buffers.
Searches all buffers for word completions."
  (when (derived-mode-p 'vterm-mode)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds)))
      (when (and start end (> (- end start) 1))
        (let ((prefix (buffer-substring-no-properties start end))
              (expansions nil))
          ;; Configure dabbrev to search all buffers
          (let ((dabbrev-check-all-buffers t)
                (dabbrev-check-other-buffers t))
            (setq expansions (ignore-errors
                               (dabbrev--find-all-expansions prefix nil))))
          (when expansions
            (list start end
                  (delete-dups expansions)
                  :exclusive 'no)))))))

(defun claude-env-capf ()
  "Completion-at-point function for environment variables.
Activates when typing $VAR patterns."
  (when (derived-mode-p 'vterm-mode)
    (save-excursion
      (when (re-search-backward "\\$\\([A-Za-z_][A-Za-z0-9_]*\\)?\\=" nil t)
        (let* ((start (match-beginning 0))
               (end (point))
               (prefix (buffer-substring-no-properties (1+ start) end))
               (env-vars (mapcar (lambda (e)
                                   (concat "$" (car (split-string e "="))))
                                 process-environment)))
          (list start end
                (if (string-empty-p prefix)
                    env-vars
                  (cl-remove-if-not
                   (lambda (v) (string-prefix-p (concat "$" prefix) v t))
                   env-vars))
                :exclusive 'no))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Combined Capf - merges all sources
;;; ─────────────────────────────────────────────────────────────────────────────

(defun claude-combined-capf ()
  "Combined completion-at-point function for Claude buffers.
Tries slash commands, then env vars, then files, then dabbrev."
  (or (claude-slash-capf)
      (claude-env-capf)
      (claude-file-capf)
      (claude-dabbrev-capf)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Vterm Completion Wrapper - handles insertion correctly
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar claude--completing nil
  "Non-nil when Claude completion is in progress.")

(defun claude-complete ()
  "Trigger completion in Claude buffer with Corfu integration.
Handles vterm's special insertion requirements."
  (interactive)
  (claude--ensure-writable)
  (if (not (derived-mode-p 'vterm-mode))
      (completion-at-point)
    ;; Get completion data from our capf
    (let* ((capf-result (claude-combined-capf)))
      (if (not capf-result)
          (message "No completions available")
        (let* ((start (nth 0 capf-result))
               (end (nth 1 capf-result))
               (collection (nth 2 capf-result))
               (props (nthcdr 3 capf-result))
               (prefix (buffer-substring-no-properties start end))
               (ann-fn (plist-get props :annotation-function))
               ;; Build completion table with metadata
               (table (lambda (string pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (category . claude)
                              (annotation-function . ,ann-fn))
                          (complete-with-action action collection string pred))))
               ;; Use completing-read (Vertico/Corfu will enhance this)
               (completion-extra-properties props)
               (choice (completing-read
                        "› "
                        table
                        nil nil prefix)))
          (when (and choice (not (string-equal choice prefix)))
            ;; Delete prefix via backspaces
            (dotimes (_ (length prefix))
              (vterm-send-backspace))
            ;; Insert completion
            (vterm-insert choice)))))))

(defun claude-complete-slash ()
  "Quick slash command completion.
Shows only slash commands with documentation."
  (interactive)
  (claude--ensure-writable)
  (when (derived-mode-p 'vterm-mode)
    (let* ((table (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata
                          (category . claude-slash)
                          (annotation-function .
                           (lambda (cmd)
                             (when-let ((desc (cdr (assoc cmd claude-slash-commands))))
                               (concat "  " (propertize desc 'face 'font-lock-comment-face))))))
                      (complete-with-action action (mapcar #'car claude-slash-commands) string pred))))
           (choice (completing-read "/ " table nil nil "/")))
      (when choice
        (vterm-insert choice)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Corfu Integration - auto-trigger in Claude buffers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun claude-setup-completion ()
  "Setup completion for Claude vterm buffers."
  ;; Register our capf
  (setq-local completion-at-point-functions '(claude-combined-capf))
  ;; Corfu settings for Claude
  (setq-local corfu-auto t
              corfu-auto-prefix 2
              corfu-auto-delay 0.15
              corfu-quit-no-match 'separator
              corfu-preselect 'prompt)
  ;; Orderless for fuzzy matching
  (setq-local completion-styles '(orderless basic)
              completion-category-overrides nil))

;; Hook into Claude buffers
(defun claude-maybe-setup-completion ()
  "Setup completion if this is a Claude buffer."
  (when (claude-buffer-p)
    (claude-setup-completion)))

(add-hook 'vterm-mode-hook #'claude-maybe-setup-completion)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Keybindings
;;; ─────────────────────────────────────────────────────────────────────────────

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "TAB") #'claude-complete)
  (define-key vterm-mode-map (kbd "C-c /") #'claude-complete-slash)
  (define-key vterm-mode-map (kbd "C-c TAB") #'claude-complete)
  (define-key vterm-mode-map (kbd "M-/") #'claude-complete)
  ;; s-z as quick shortcut for same function as C-c c z
  ;; Bind in ashton-mode-map (global) so it works in vterm
  (define-key ashton-mode-map (kbd "s-z") #'claude-code-toggle-read-only-mode))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Workflow Commands - Pre-built prompts with context
;;; ════════════════════════════════════════════════════════════════════════════

(defun claude-workflow-test-plan ()
  "Ask Claude to generate a test plan for current branch."
  (interactive)
  (claude-code-send-command "/test-plan"))

(defun claude-workflow-review ()
  "Ask Claude to review current branch changes."
  (interactive)
  (let* ((default-directory (or (locate-dominating-file default-directory ".git")
                                default-directory))
         (diff-stat (string-trim
                     (shell-command-to-string
                      "git diff --stat upstream/dev 2>/dev/null || git diff --stat origin/dev 2>/dev/null || git diff --stat HEAD~5 2>/dev/null"))))
    (claude-code-send-command
     (format "Review these changes for bugs, security issues, and improvements:\n```\n%s\n```"
             diff-stat))))

(defun claude-workflow-fix-error (error-text)
  "Send ERROR-TEXT to Claude for diagnosis and fix."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Error: "))))
  (claude-code-send-command
   (format "Fix this error:\n```\n%s\n```" error-text)))

(defun claude-workflow-explain ()
  "Ask Claude to explain the selected code or current file."
  (interactive)
  (if (use-region-p)
      (claude-code-send-region (region-beginning) (region-end))
    (claude-code-send-command
     (format "Explain this file: %s" (buffer-file-name)))))

(defun claude-workflow-commit ()
  "Ask Claude to suggest a commit message for staged changes."
  (interactive)
  (claude-code-send-command "Write a commit message for the staged changes"))

;; Keybindings (C-c c w prefix for workflow)
(define-key ashton-mode-map (kbd "C-c c w t") #'claude-workflow-test-plan)
(define-key ashton-mode-map (kbd "C-c c w r") #'claude-workflow-review)
(define-key ashton-mode-map (kbd "C-c c w f") #'claude-workflow-fix-error)
(define-key ashton-mode-map (kbd "C-c c w e") #'claude-workflow-explain)
(define-key ashton-mode-map (kbd "C-c c w c") #'claude-workflow-commit)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Git Worktree Protection Hook
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom orchard-protect-worktree-branches t
  "When non-nil, warn/block git checkout commands that would switch branches."
  :type 'boolean
  :group 'orchard)

(defun orchard--git-checkout-warning-hook (message)
  "Intercept git checkout/switch commands and warn user.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (and orchard-protect-worktree-branches
             (eq (plist-get message :type) 'pre-tool-use))
    (let* ((json-data (plist-get message :json-data))
           (parsed-data (when json-data
                          (condition-case nil
                              (json-read-from-string json-data)
                            (error nil))))
           (tool-name (when parsed-data (alist-get 'tool_name parsed-data)))
           (tool-input (when parsed-data (alist-get 'tool_input parsed-data))))

      ;; Check if this is a Bash command with git checkout/switch that changes branches
      (when (and (equal tool-name "Bash")
                 tool-input
                 (let ((cmd (alist-get 'command tool-input)))
                   (and (stringp cmd)
                        ;; Match git checkout/switch but NOT file operations
                        (string-match-p "git \\(checkout\\|switch\\)" cmd)
                        (not (string-match-p "git checkout \\(--\\|HEAD\\)" cmd)))))

        (let* ((command (alist-get 'command tool-input))
               (response (read-char-choice
                          (format "⚠️  WORKTREE WARNING: Claude wants to run:\n\n  %s\n\nThis may switch branches and break worktree integrity.\nAllow? (y)es / (n)o / (a)sk Claude Code: "
                                  command)
                          '(?y ?n ?a ?Y ?N ?A)))
               (decision (cond
                          ((memq response '(?y ?Y)) "allow")
                          ((memq response '(?n ?N)) "deny")
                          ((memq response '(?a ?A)) "ask")
                          (t "deny"))))

          (message "")  ; Clear minibuffer
          (json-encode
           `((hookSpecificOutput
              . ((hookEventName . "PreToolUse")
                 (permissionDecision . ,decision)
                 (permissionDecisionReason . "Worktree branch protection"))))))))))

;; DISABLED: read-char-choice is blocking and can freeze Emacs if prompt is buried
;; (add-hook 'claude-code-event-hook #'orchard--git-checkout-warning-hook)

(provide 'config-claude)
;;; config-claude.el ends here
