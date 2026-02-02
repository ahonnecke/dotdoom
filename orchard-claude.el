;;; orchard-claude.el --- Orchard Claude integration -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains Claude-related functionality:
;; - Claude status tracking (via hooks)
;; - Claude buffer management
;; - Starting/resuming Claude sessions
;; - Claude settings/commands setup
;; - Backend abstraction (agent-shell or claude-code.el)
;; - Workflow commands (analyze, implement, pr)

(require 'orchard-vars)
(require 'orchard-cache)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Backend Abstraction
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Support both agent-shell (ACP, preferred) and claude-code.el (vterm, fallback)

(defcustom orchard-claude-backend 'auto
  "Which Claude backend to use.
- `auto': Try agent-shell first, fall back to claude-code.el
- `agent-shell': Use agent-shell (ACP protocol, comint-based)
- `claude-code': Use claude-code.el (vterm-based)"
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Agent Shell (ACP)" agent-shell)
                 (const :tag "Claude Code (vterm)" claude-code))
  :group 'orchard)

(defvar orchard--plan-watchers (make-hash-table :test 'equal)
  "Hash table mapping worktree paths to file-notify descriptors for .plan.md.")

(defun orchard--detect-backend ()
  "Detect which Claude backend is available.
Prefers agent-shell over claude-code when auto-detecting."
  (cond
   ;; Explicit setting takes precedence
   ((eq orchard-claude-backend 'agent-shell) 'agent-shell)
   ((eq orchard-claude-backend 'claude-code) 'claude-code)
   ;; Auto-detect: prefer agent-shell if claude-code-acp is installed
   ((executable-find "claude-code-acp")
    (condition-case nil
        (progn (require 'agent-shell) 'agent-shell)
      (error 'claude-code)))
   ;; Fall back to claude-code
   ((or (featurep 'claude-code)
        (condition-case nil (progn (require 'claude-code) t) (error nil)))
    'claude-code)
   (t (user-error "No Claude backend available. Install agent-shell or claude-code.el"))))

(defun orchard--get-claude-buffer-for-backend (path)
  "Find Claude buffer for PATH using the current backend."
  (let ((backend (orchard--detect-backend)))
    (pcase backend
      ('agent-shell
       (orchard--get-agent-shell-buffer path))
      ('claude-code
       (orchard--claude-buffer-for-path path)))))

(defun orchard--start-claude-backend (path &optional command)
  "Start Claude in PATH using detected backend, optionally running COMMAND."
  (let ((backend (orchard--detect-backend)))
    (pcase backend
      ('agent-shell
       (orchard--start-agent-shell path command))
      ('claude-code
       (if command
           (orchard--start-claude-with-command path command)
         (orchard--start-claude-with-resume path))))))

(defun orchard--start-agent-shell (path &optional command)
  "Start agent-shell for PATH, optionally running COMMAND after init.
Automatically uses --continue to resume the most recent session in PATH."
  (require 'agent-shell)
  (let ((default-directory path)
        ;; Add --continue to resume sessions, --permission-mode dontAsk to reduce prompts
        (agent-shell-anthropic-claude-command '("claude-code-acp" "--continue" "--permission-mode" "dontAsk")))
    ;; Start agent-shell - let it handle its own window
    (agent-shell-anthropic-start-claude-code)
    ;; Send command after Claude initializes (needs time to start)
    (when command
      (run-at-time 4 nil
                   (lambda ()
                     ;; Find the agent buffer for this path
                     (let ((agent-buf (orchard--get-agent-shell-buffer path)))
                       (when (and agent-buf (buffer-live-p agent-buf))
                         (with-current-buffer agent-buf
                           (goto-char (point-max))
                           (insert command)
                           (comint-send-input)))))))))

(defun orchard--get-agent-shell-buffer (path)
  "Find agent-shell buffer for PATH."
  (let ((name (file-name-nondirectory (directory-file-name path))))
    (cl-find-if
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'shell-maker-mode)
                   (string-match-p (regexp-quote name) (buffer-name buf))))))
     (buffer-list))))

(defun orchard--send-to-claude (command &optional path)
  "Send COMMAND to Claude buffer for PATH (or current worktree)."
  (let* ((path (or path default-directory))
         (buf (orchard--get-claude-buffer-for-backend path))
         (backend (orchard--detect-backend)))
    (unless buf
      (user-error "No Claude buffer for %s" path))
    (with-current-buffer buf
      (pcase backend
        ('agent-shell
         (goto-char (point-max))
         (insert command)
         (comint-send-input))
        ('claude-code
         (vterm-send-string command)
         (vterm-send-return))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Workflow Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-claude-analyze (&optional path)
  "Start Claude and run /analyze for PATH (or current worktree).
Sets up a file watcher on .plan.md to notify when plan is ready."
  (interactive)
  (let* ((path (or path
                   (when-let ((wt (orchard--get-worktree-at-point)))
                     (alist-get 'path wt))
                   default-directory))
         (existing-buf (orchard--get-claude-buffer-for-backend path)))
    ;; Setup plan watcher before starting
    (orchard--setup-plan-watcher path)
    (if existing-buf
        ;; Claude exists - just send analyze
        (progn
          (pop-to-buffer existing-buf)
          (orchard--send-to-claude "/analyze" path))
      ;; Start new Claude with analyze
      (orchard--start-claude-backend path "/analyze"))))

(defun orchard-claude-implement (&optional path)
  "Send /implement to Claude for PATH (or current worktree)."
  (interactive)
  (orchard--send-to-claude "/ship" (or path default-directory)))

(defun orchard-claude-pr (&optional path)
  "Send /pr to Claude for PATH (or current worktree)."
  (interactive)
  (orchard--send-to-claude "/pr" (or path default-directory)))

(defun orchard-claude-finish (&optional path)
  "Send /finish to Claude for PATH (or current worktree)."
  (interactive)
  (orchard--send-to-claude "/finish" (or path default-directory)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Plan Watcher
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--setup-plan-watcher (path)
  "Setup file-notify watcher for .plan.md in PATH."
  (let ((plan-file (expand-file-name ".plan.md" path)))
    ;; Remove existing watcher if any
    (when-let ((old-desc (gethash path orchard--plan-watchers)))
      (file-notify-rm-watch old-desc)
      (remhash path orchard--plan-watchers))
    ;; Watch the directory for .plan.md changes
    (let ((desc (file-notify-add-watch
                 path
                 '(change)
                 (lambda (event)
                   (orchard--handle-plan-event event path)))))
      (puthash path desc orchard--plan-watchers))))

(defun orchard--handle-plan-event (event path)
  "Handle file-notify EVENT for plan file in PATH."
  (let ((event-type (nth 1 event))
        (event-file (nth 2 event))
        (plan-file (expand-file-name ".plan.md" path)))
    (when (and (or (eq event-type 'created)
                   (eq event-type 'changed))
               (string= (expand-file-name event-file path) plan-file))
      ;; Plan file was created or modified
      (orchard--on-plan-ready path))))

(defun orchard--on-plan-ready (path)
  "Called when .plan.md is ready in PATH. Pop up for review."
  (let ((plan-file (expand-file-name ".plan.md" path))
        (branch (file-name-nondirectory (directory-file-name path))))
    (when (file-exists-p plan-file)
      ;; Notify user
      (message "Plan ready for %s! Review and approve with `orchard-approve-plan`" branch)
      ;; Pop up the plan file in a side window
      (let ((buf (find-file-noselect plan-file)))
        (display-buffer buf
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.4))))
      ;; Add approval keybinding to the plan buffer
      (with-current-buffer (find-file-noselect plan-file)
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (orchard-approve-plan path)))
        (local-set-key (kbd "C-c C-k")
                       (lambda ()
                         (interactive)
                         (message "Plan rejected. Edit and save, or run /analyze again.")))
        (setq header-line-format
              (propertize " [C-c C-c] Approve & Implement  [C-c C-k] Reject "
                          'face 'mode-line-highlight))))))

(defun orchard-approve-plan (&optional path)
  "Approve the plan for PATH and trigger implementation."
  (interactive)
  (let* ((path (or path default-directory))
         (plan-file (expand-file-name ".plan.md" path)))
    (when (file-exists-p plan-file)
      ;; Close the plan window
      (when-let ((buf (get-file-buffer plan-file)))
        (when-let ((win (get-buffer-window buf)))
          (delete-window win)))
      ;; Remove the watcher
      (when-let ((desc (gethash path orchard--plan-watchers)))
        (file-notify-rm-watch desc)
        (remhash path orchard--plan-watchers))
      ;; Send /ship to implement
      (message "Plan approved! Starting implementation...")
      (orchard--send-to-claude "/ship" path))))

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard-refresh "orchard-dashboard")
(declare-function orchard--get-worktrees "orchard-worktree")
(declare-function orchard--find-worktree-for-issue "orchard")
(declare-function orchard--issue-workflow-stage "orchard")
(declare-function orchard--get-open-issues "orchard-cache")
(declare-function orchard--find-best-window "orchard")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Status - Simplified
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Minimal tracking - just check if process is running.
;; PR URL capture via hook for automatic orchard updates.

(defvar orchard--refresh-timer nil
  "Timer for debounced orchard refresh.")

(defun orchard--claude-status-hook (message)
  "Handle Claude hook events - only captures PR URLs.
MESSAGE is a plist with :type and :buffer-name."
  (let ((hook-type (plist-get message :type)))
    ;; Only care about post-tool-use for PR capture
    (when (eq hook-type 'post-tool-use)
      (orchard--maybe-capture-pr-url message))))

(defun orchard--maybe-capture-pr-url (message)
  "Check MESSAGE for PR creation and save URL to .pr-url file."
  (let* ((json-data (plist-get message :json-data))
         (parsed (condition-case nil
                     (when json-data (json-read-from-string json-data))
                   (error nil))))
    (when parsed
      (let* ((tool-name (alist-get 'tool_name parsed))
             (tool-input (alist-get 'tool_input parsed))
             (tool-result (alist-get 'tool_result parsed))
             (command (when tool-input (alist-get 'command tool-input))))
        ;; Check if this was a gh pr create command
        (when (and (equal tool-name "Bash")
                   command
                   (string-match-p "gh pr create" command)
                   tool-result)
          ;; Extract PR URL from result (gh outputs URL on success)
          (when (string-match "https://github\\.com/[^/]+/[^/]+/pull/[0-9]+" tool-result)
            (let ((pr-url (match-string 0 tool-result))
                  (buffer-name (plist-get message :buffer-name)))
              ;; Extract worktree path from buffer name
              (when (and buffer-name
                         (string-match "\\*claude:\\s-*\\(.+?\\)\\*?" buffer-name))
                (let* ((path (match-string 1 buffer-name))
                       (pr-file (expand-file-name ".pr-url" path)))
                  (when (file-directory-p path)
                    (with-temp-file pr-file
                      (insert pr-url))
                    (message "Captured PR URL: %s" pr-url)
                    ;; Trigger orchard refresh
                    (orchard--schedule-refresh)))))))))))

(defun orchard--schedule-refresh ()
  "Schedule an orchard refresh, debounced."
  (when orchard--refresh-timer
    (cancel-timer orchard--refresh-timer))
  (setq orchard--refresh-timer
        (run-with-timer 0.3 nil
                        (lambda ()
                          (when-let ((buf (get-buffer "*orchard*")))
                            (with-current-buffer buf
                              (let ((orchard--inhibit-cache-refresh t))
                                (orchard-refresh))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Buffer Management
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--ensure-claude-loaded ()
  "Ensure claude-code is loaded."
  (condition-case err
      (progn
        (unless (featurep 'vterm)
          (require 'vterm))
        (unless (featurep 'claude-code)
          (require 'claude-code)))
    (error
     (user-error "Failed to load claude-code (vterm required): %s"
                 (error-message-string err)))))

(defun orchard--claude-buffer-for-path (path)
  "Find Claude buffer for worktree at PATH."
  (let ((name (file-name-nondirectory (directory-file-name path))))
    (cl-find-if (lambda (buf)
                  (and (buffer-live-p buf)
                       (string-prefix-p "*claude:" (buffer-name buf))
                       (string-match-p (regexp-quote name) (buffer-name buf))))
                (buffer-list))))

(defun orchard--get-claude-buffers ()
  "Get all Claude buffers."
  (cl-remove-if-not
   (lambda (buf)
     (string-prefix-p "*claude:" (buffer-name buf)))
   (buffer-list)))

(defun orchard--claude-status (buffer)
  "Get Claude BUFFER status: 'waiting, 'active, or nil.
Checks if process is alive, then looks for prompt to detect waiting state.
Returns symbols that match dashboard formatter expectations."
  (when (and (buffer-live-p buffer)
             (orchard--claude-process-running-p buffer))
    ;; Check if Claude is waiting for input (prompt visible at end)
    (if (orchard--claude-at-prompt-p buffer)
        'waiting
      'active)))

(defun orchard--claude-at-prompt-p (buffer)
  "Check if BUFFER shows Claude waiting at a prompt.
Simple heuristic: looks for > or ❯ at end of buffer content."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        ;; Look at last 50 chars for prompt indicators
        (let ((end-text (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 50))
                         (point-max))))
          ;; Claude shows > or ❯ when waiting for input
          (string-match-p "[>❯]\\s-*$" end-text))))))

(defun orchard--claude-waiting-p (buffer)
  "Check if Claude BUFFER is running (simplified)."
  (orchard--claude-status buffer))

(defun orchard--claude-process-running-p (buffer)
  "Check if the vterm process in BUFFER is still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (boundp 'vterm--process)
           vterm--process
           (process-live-p vterm--process)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Session Management
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--setup-claude-settings (worktree-path)
  "Setup shared Claude settings and commands symlinks in WORKTREE-PATH."
  (when (or orchard-shared-claude-settings orchard-shared-claude-commands)
    (let ((claude-dir (expand-file-name ".claude" worktree-path))
          (commands-dir (expand-file-name ".claude/commands" worktree-path)))
      ;; Ensure directories exist
      (unless (file-directory-p claude-dir)
        (make-directory claude-dir t))
      (unless (file-directory-p commands-dir)
        (make-directory commands-dir t))
      ;; Setup settings.local.json symlink
      (when orchard-shared-claude-settings
        (let ((settings-file (expand-file-name "settings.local.json" claude-dir)))
          (when (file-exists-p settings-file)
            (delete-file settings-file))
          (when (file-exists-p orchard-shared-claude-settings)
            (make-symbolic-link orchard-shared-claude-settings settings-file))))
      ;; Setup command symlinks
      (when (and orchard-shared-claude-commands
                 (file-directory-p orchard-shared-claude-commands))
        (dolist (cmd-file (directory-files orchard-shared-claude-commands t "\\.md$"))
          (let ((target (expand-file-name (file-name-nondirectory cmd-file) commands-dir)))
            (when (file-exists-p target)
              (delete-file target))
            (make-symbolic-link cmd-file target)))))))

(defun orchard-sync-claude-commands ()
  "Re-sync Claude commands for all worktrees.
Use this when you add new commands to the shared directory."
  (interactive)
  (let ((worktrees (orchard--get-worktrees t))
        (synced 0))
    (dolist (wt worktrees)
      (let ((path (alist-get 'path wt)))
        (when (and path (file-directory-p path))
          (orchard--setup-claude-settings path)
          (cl-incf synced))))
    (message "Synced Claude commands for %d worktrees" synced)))

(defun orchard--start-background-claude (path)
  "Start Claude in PATH without displaying it.
Returns the Claude buffer."
  (orchard--ensure-claude-loaded)
  (let ((default-directory path)
        (buffers-before (buffer-list))
        (original-window (selected-window))
        (original-config (current-window-configuration))
        new-claude-buf)
    ;; Start Claude - it will try to display but we'll fix that
    (save-window-excursion
      (claude-code))
    ;; Find the new Claude buffer
    (setq new-claude-buf
          (cl-find-if
           (lambda (buf)
             (and (string-prefix-p "*claude:" (buffer-name buf))
                  (not (memq buf buffers-before))))
           (buffer-list)))
    ;; Restore window configuration and hide Claude from all windows
    (set-window-configuration original-config)
    (when new-claude-buf
      ;; Make sure Claude isn't displayed anywhere
      (dolist (win (get-buffer-window-list new-claude-buf nil t))
        (delete-window win)))
    new-claude-buf))

(defun orchard--start-background-claude-with-command (path command)
  "Start Claude in PATH and run COMMAND (e.g., '/refine-ticket 42').
Keeps Claude in background - no window shown."
  (let ((claude-buf (orchard--start-background-claude path)))
    (when claude-buf
      ;; Send command after startup delay (Claude needs time to initialize)
      (run-at-time 3 nil
                   (lambda ()
                     (when (buffer-live-p claude-buf)
                       (with-current-buffer claude-buf
                         (vterm-send-string command)
                         (vterm-send-return))))))
    claude-buf))

(defun orchard--fix-claude-size (buf win)
  "Fix Claude BUF size to match WIN dimensions.
Claude Code queries terminal size at startup, but our window restoration
can leave it mismatched. This tells vterm the correct size."
  (run-at-time 0.3 nil
               (lambda ()
                 (when (and (buffer-live-p buf)
                            (window-live-p win)
                            (eq (window-buffer win) buf))
                   (with-current-buffer buf
                     (when (fboundp 'vterm--set-size)
                       ;; Subtract 2 from width for safety margin
                       (let ((h (window-height win))
                             (w (- (window-width win) 2)))
                         (vterm--set-size h w))))))))

(defun orchard--start-claude-with-resume (path)
  "Start Claude for PATH in current window. Does NOT auto-resume."
  (orchard--ensure-claude-loaded)
  (let ((existing-claude (orchard--claude-buffer-for-path path)))
    (if existing-claude
        ;; Claude exists - just switch to it in current window
        (switch-to-buffer existing-claude)
      ;; Start new Claude - brute force window preservation
      (let* ((target-win (selected-window))
             (win-config (current-window-configuration))
             (buffers-before (buffer-list))
             (default-directory path))
        ;; Start Claude - let it do whatever crazy window stuff it wants
        (claude-code)
        ;; Find the new Claude buffer
        (let ((new-claude (cl-find-if
                           (lambda (buf)
                             (and (string-prefix-p "*claude:" (buffer-name buf))
                                  (not (memq buf buffers-before))))
                           (buffer-list))))
          ;; Restore our window layout
          (set-window-configuration win-config)
          ;; Now put Claude in the window we wanted
          (when new-claude
            (set-window-buffer target-win new-claude)
            (select-window target-win)
            ;; Fix vterm size to match window (Claude queried size before restore)
            (orchard--fix-claude-size new-claude target-win)
            ;; Register for session persistence
            (orchard--register-claude-buffer path)))))))

(defun orchard--send-resume-to-claude (claude-buf delay)
  "Send /resume and Enter to CLAUDE-BUF after DELAY seconds."
  (run-at-time delay nil
               (lambda ()
                 (when (buffer-live-p claude-buf)
                   (with-current-buffer claude-buf
                     (vterm-send-string "/resume")
                     (vterm-send-return)
                     ;; Select most recent session after list appears
                     (run-at-time 1.5 nil
                                  (lambda ()
                                    (when (buffer-live-p claude-buf)
                                      (with-current-buffer claude-buf
                                        (vterm-send-return))))))))))

(defun orchard--send-command-to-claude (claude-buf delay command)
  "Send COMMAND to CLAUDE-BUF after DELAY seconds."
  (run-at-time delay nil
               (lambda ()
                 (when (buffer-live-p claude-buf)
                   (with-current-buffer claude-buf
                     (vterm-send-string command)
                     (vterm-send-return))))))

(defun orchard--start-claude-with-command (path command)
  "Start Claude for PATH in current window and run COMMAND after initialization."
  (orchard--ensure-claude-loaded)
  (let ((existing-claude (orchard--claude-buffer-for-path path)))
    (if existing-claude
        ;; Claude exists - switch to it and send command
        (progn
          (switch-to-buffer existing-claude)
          (orchard--send-command-to-claude existing-claude 0.5 command))
      ;; Start new Claude with window preservation
      (let* ((target-win (selected-window))
             (win-config (current-window-configuration))
             (buffers-before (buffer-list))
             (default-directory path))
        (claude-code)
        (let ((new-claude (cl-find-if
                           (lambda (buf)
                             (and (string-prefix-p "*claude:" (buffer-name buf))
                                  (not (memq buf buffers-before))))
                           (buffer-list))))
          ;; Restore window layout
          (set-window-configuration win-config)
          (when new-claude
            (set-window-buffer target-win new-claude)
            (select-window target-win)
            ;; Fix vterm size
            (orchard--fix-claude-size new-claude target-win)
            ;; Send command after init
            (run-at-time 3 nil
                         (lambda ()
                           (when (buffer-live-p new-claude)
                             (orchard--send-command-to-claude new-claude 0 command))))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Listing and Debugging
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-list-claudes ()
  "List all Claude sessions. Shows [WAITING] for those needing input."
  (interactive)
  (let ((claudes (orchard--get-claude-buffers)))
    (if (null claudes)
        (message "No Claude sessions running")
      (let* ((choices (mapcar
                       (lambda (buf)
                         (let* ((name (buffer-name buf))
                                (waiting (orchard--claude-waiting-p buf))
                                (display (if waiting
                                             (format "%s [WAITING]" name)
                                           name)))
                           (cons display buf)))
                       claudes))
             (selection (completing-read "Claude session: "
                                         (mapcar #'car choices)
                                         nil t)))
        (when-let ((buf (cdr (assoc selection choices))))
          (pop-to-buffer buf))))))

(defun orchard-tile-claudes ()
  "Tile up to 4 Claude windows in a 2x2 grid.
Prioritizes sessions that are waiting for input."
  (interactive)
  (let* ((all-claudes (orchard--get-claude-buffers))
         ;; Sort: waiting first, then by buffer name
         (sorted (sort (copy-sequence all-claudes)
                       (lambda (a b)
                         (let ((a-waiting (orchard--claude-waiting-p a))
                               (b-waiting (orchard--claude-waiting-p b)))
                           (or (and a-waiting (not b-waiting))
                               (and (eq a-waiting b-waiting)
                                    (string< (buffer-name a) (buffer-name b))))))))
         (claudes (seq-take sorted 4))
         (count (length claudes)))
    (cond
     ((= count 0)
      (message "No Claude sessions running"))
     ((= count 1)
      (delete-other-windows)
      (switch-to-buffer (car claudes)))
     ((= count 2)
      (delete-other-windows)
      (switch-to-buffer (car claudes))
      (split-window-right)
      (other-window 1)
      (switch-to-buffer (cadr claudes))
      (other-window 1)
      (balance-windows))
     (t
      ;; 3 or 4 claudes: 2x2 grid
      (delete-other-windows)
      (switch-to-buffer (car claudes))
      (split-window-right)
      (split-window-below)
      (other-window 1)
      (switch-to-buffer (nth 1 claudes))
      (other-window 1)
      (split-window-below)
      (switch-to-buffer (nth 2 claudes))
      (other-window 1)
      (when (nth 3 claudes)
        (switch-to-buffer (nth 3 claudes)))
      (other-window 1)
      (balance-windows)))
    (when (> count 0)
      (message "Tiled %d Claude session%s%s"
               count
               (if (= count 1) "" "s")
               (if (> (length all-claudes) 4)
                   (format " (%d more available)" (- (length all-claudes) 4))
                 "")))))

(defun orchard-debug-windows ()
  "Debug window state - run this BEFORE starting Claude to see what orchard sees."
  (interactive)
  (let* ((windows (window-list nil 'no-mini))
         (leftmost (orchard--leftmost-window))
         (non-leftmost (cl-remove leftmost windows)))
    (with-output-to-temp-buffer "*orchard-window-debug*"
      (princ "=== WINDOW STATE ===\n\n")
      (princ (format "Total windows: %d\n" (length windows)))
      (princ (format "Frame width: %d\n\n" (frame-width)))
      (dolist (win windows)
        (let* ((buf (window-buffer win))
               (buf-name (buffer-name buf))
               (edges (window-edges win))
               (width (window-width win))
               (height (window-height win))
               (is-leftmost (eq win leftmost))
               (is-reusable (orchard--window-reusable-p win))
               (is-claude (orchard--window-showing-claude-p win))
               (is-orchard (orchard--window-showing-orchard-p win)))
          (princ (format "Window: %s\n" win))
          (princ (format "  Buffer: %s\n" buf-name))
          (princ (format "  Size: %dx%d (WxH)\n" width height))
          (princ (format "  Edges: %s\n" edges))
          (princ (format "  Flags: %s%s%s%s\n"
                         (if is-leftmost "LEFTMOST " "")
                         (if is-reusable "REUSABLE " "")
                         (if is-claude "CLAUDE " "")
                         (if is-orchard "ORCHARD " "")))
          (princ "\n")))
      (princ "=== BEST WINDOW SELECTION ===\n")
      (let ((best (orchard--find-best-window)))
        (princ (format "orchard--find-best-window returns: %s\n" best))
        (princ (format "  showing: %s\n" (buffer-name (window-buffer best))))))))

(defun orchard-debug-claude-status ()
  "Show diagnostic info for all Claude buffers and issue mappings.
Use this to debug why Claude status isn't showing in orchard."
  (interactive)
  (let* ((claude-bufs (orchard--get-claude-buffers))
         (worktrees (orchard--get-worktrees))
         (issues (orchard--get-open-issues)))
    (with-output-to-temp-buffer "*orchard-claude-debug*"
      (princ "=== CLAUDE BUFFERS ===\n\n")
      (if (null claude-bufs)
          (princ "NO CLAUDE BUFFERS FOUND\n")
        (dolist (buf claude-bufs)
          (let* ((name (buffer-name buf))
                 (status (orchard--claude-status buf))
                 (process-alive (orchard--claude-process-running-p buf))
                 (end-text (with-current-buffer buf
                             (buffer-substring-no-properties
                              (max (point-min) (- (point-max) 300))
                              (point-max)))))
            (princ (format "Buffer: %s\n" name))
            (princ (format "Status: %s (process: %s)\n"
                           (or status "NIL")
                           (if process-alive "alive" "dead")))
            (princ (format "Last 300 chars:\n---\n%s\n---\n\n" end-text)))))

      (princ "\n=== WORKTREES WITH CLAUDE ===\n\n")
      (dolist (wt worktrees)
        (let* ((path (alist-get 'path wt))
               (branch (alist-get 'branch wt))
               (claude-buf (orchard--claude-buffer-for-path path)))
          (when claude-buf
            (princ (format "Worktree: %s\n" branch))
            (princ (format "  Path: %s\n" path))
            (princ (format "  Claude: %s\n" (buffer-name claude-buf)))
            (princ (format "  Status: %s\n\n" (orchard--claude-status claude-buf))))))

      (princ "\n=== ISSUES WITH WORKTREES ===\n\n")
      (when (fboundp 'orchard--find-worktree-for-issue)
        (dolist (issue issues)
          (let* ((num (alist-get 'number issue))
                 (title (alist-get 'title issue))
                 (wt (orchard--find-worktree-for-issue num worktrees)))
            (when wt
              (let* ((path (alist-get 'path wt))
                     (stage (when (fboundp 'orchard--issue-workflow-stage)
                              (orchard--issue-workflow-stage num worktrees)))
                     (claude-status (when stage (alist-get 'claude-status stage))))
                (princ (format "#%d: %s\n" num (truncate-string-to-width title 40)))
                (princ (format "  Worktree: %s\n" path))
                (princ (format "  Claude status: %s\n" (or claude-status "NO CLAUDE")))
                (princ (format "  Stage: %s\n\n" stage))))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Session Persistence
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Save which Claude sessions were running on exit so they can be resumed.

(defvar orchard--claude-sessions-file
  (expand-file-name "~/.orchard-claude-sessions.eld")
  "File to persist active Claude session paths.")

(defvar orchard--claude-buffer-paths (make-hash-table :test 'equal)
  "Hash table mapping Claude buffer names to worktree paths.
Populated when Claude is started via orchard.")

(defun orchard--register-claude-buffer (path)
  "Register that a Claude buffer was started for PATH.
Called after starting Claude to track the association."
  (run-at-time 0.5 nil
               (lambda ()
                 (when-let ((buf (orchard--claude-buffer-for-path path)))
                   (puthash (buffer-name buf) path orchard--claude-buffer-paths)))))

(defun orchard--sync-claude-buffer-paths ()
  "Sync the buffer→path hash table with all existing Claude buffers.
Call this to catch Claude buffers not started through orchard."
  (when (fboundp 'orchard--get-worktrees)
    (let ((worktrees (orchard--get-worktrees t)))
      (dolist (buf (orchard--get-claude-buffers))
        (let ((buf-name (buffer-name buf)))
          (unless (gethash buf-name orchard--claude-buffer-paths)
            ;; Try to find matching worktree
            (dolist (wt worktrees)
              (let* ((path (alist-get 'path wt))
                     (wt-name (file-name-nondirectory (directory-file-name path))))
                (when (string-match-p (regexp-quote wt-name) buf-name)
                  (puthash buf-name path orchard--claude-buffer-paths))))))))))

(defun orchard--get-active-claude-paths ()
  "Get list of worktree paths with active Claude sessions."
  ;; Sync hash table first to catch buffers not started through orchard
  (orchard--sync-claude-buffer-paths)
  (let ((paths nil))
    (dolist (buf (orchard--get-claude-buffers))
      (when (buffer-live-p buf)
        (let* ((buf-name (buffer-name buf))
               ;; First try our tracking hash
               (tracked-path (gethash buf-name orchard--claude-buffer-paths)))
          (if (and tracked-path (file-directory-p tracked-path))
              (push tracked-path paths)
            ;; Fallback: try to find matching worktree
            (let* ((name-part (when (string-match "\\*claude:\\s-*\\(.+?\\)\\*?" buf-name)
                                (match-string 1 buf-name)))
                   (worktrees (when (fboundp 'orchard--get-worktrees)
                                (orchard--get-worktrees t))))
              (when name-part
                (dolist (wt worktrees)
                  (let ((wt-path (alist-get 'path wt))
                        (wt-name (file-name-nondirectory
                                  (directory-file-name (alist-get 'path wt)))))
                    (when (string-match-p (regexp-quote wt-name) name-part)
                      (push wt-path paths))))))))))
    (delete-dups paths)))

(defun orchard--save-claude-sessions ()
  "Save active Claude session paths to file."
  (let ((paths (orchard--get-active-claude-paths)))
    (when paths
      (with-temp-file orchard--claude-sessions-file
        (insert ";; Orchard Claude sessions - auto-generated\n")
        (insert (format ";; Saved: %s\n" (current-time-string)))
        (prin1 paths (current-buffer))
        (insert "\n"))
      (message "Saved %d Claude session(s) for resume" (length paths)))))

(defun orchard--load-claude-sessions ()
  "Load saved Claude session paths from file.
Returns list of paths or nil."
  (when (file-exists-p orchard--claude-sessions-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents orchard--claude-sessions-file)
          (goto-char (point-min))
          ;; Skip comments
          (while (looking-at "^;")
            (forward-line 1))
          (read (current-buffer)))
      (error
       (message "Error loading Claude sessions: %s" err)
       nil))))

(defun orchard-resume-sessions ()
  "Resume Claude sessions that were active in the previous Emacs session."
  (interactive)
  (let ((paths (orchard--load-claude-sessions)))
    (if (null paths)
        (message "No saved Claude sessions to resume")
      (let ((resumed 0)
            (skipped 0))
        (dolist (path paths)
          (cond
           ((not (file-directory-p path))
            (cl-incf skipped)
            (message "Skipping %s (directory not found)" path))
           ((orchard--get-claude-buffer-for-backend path)
            (cl-incf skipped)) ; already running
           (t
            (orchard--start-claude-backend path)
            (cl-incf resumed))))
        (message "Resumed %d Claude session(s)%s"
                 resumed
                 (if (> skipped 0)
                     (format " (%d skipped)" skipped)
                   ""))
        ;; Clear the saved sessions file after resuming
        (when (file-exists-p orchard--claude-sessions-file)
          (delete-file orchard--claude-sessions-file))))))

(defun orchard-resume-sessions-prompt ()
  "Prompt to resume saved Claude sessions if any exist."
  (interactive)
  (let ((paths (orchard--load-claude-sessions)))
    (when paths
      (let ((valid-paths (cl-remove-if-not #'file-directory-p paths)))
        (when (and valid-paths
                   (yes-or-no-p
                    (format "Resume %d Claude session(s) from last session? "
                            (length valid-paths))))
          (orchard-resume-sessions))))))

;; Session persistence - save active Claude paths on exit
;; Uses buffer→path hash table to track associations reliably
(add-hook 'kill-emacs-hook #'orchard--save-claude-sessions)

;; Register Claude event hook for status tracking and PR capture
(with-eval-after-load 'claude-code
  (add-hook 'claude-code-event-hook #'orchard--claude-status-hook))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Slash Commands (for agent-shell)
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard-claude-commands
  '(("/analyze" . "Analyze the issue and create implementation plan")
    ("/ship" . "Implement the approved plan")
    ("/pr" . "Create a pull request")
    ("/fix-ci" . "Check CI status and fix failures")
    ("/help" . "Show Claude help")
    ("/clear" . "Clear conversation context")
    ("/compact" . "Summarize and compact conversation")
    ("/resume" . "Resume a previous session")
    ("/config" . "Show/edit Claude configuration")
    ("/cost" . "Show token usage and cost")
    ("/memory" . "Show Claude's memory"))
  "Alist of Claude slash commands and descriptions.")

(defun orchard-claude-command-menu ()
  "Show a menu of Claude slash commands and send the selected one."
  (interactive)
  (let* ((choices (mapcar (lambda (cmd)
                            (format "%-12s %s" (car cmd) (cdr cmd)))
                          orchard-claude-commands))
         (selection (completing-read "Claude command: " choices nil t))
         (command (car (split-string selection))))
    (orchard--send-to-claude command)))

(defun orchard-claude-send-analyze ()
  "Send /analyze to Claude."
  (interactive)
  (orchard--send-to-claude "/analyze"))

(defun orchard-claude-send-ship ()
  "Send /ship to Claude."
  (interactive)
  (orchard--send-to-claude "/ship"))

(defun orchard-claude-send-pr ()
  "Send /pr to Claude."
  (interactive)
  (orchard--send-to-claude "/pr"))

(defun orchard-claude-send-help ()
  "Send /help to Claude."
  (interactive)
  (orchard--send-to-claude "/help"))

(defun orchard-claude-command-help ()
  "Show Claude command keybindings."
  (interactive)
  (let ((help-buf (get-buffer-create "*Claude Commands*")))
    (with-current-buffer help-buf
      (erase-buffer)
      (insert (propertize "Claude Commands (C-c /)\n" 'face 'bold))
      (insert (make-string 40 ?─) "\n\n")
      (insert (propertize "Workflow:\n" 'face 'font-lock-keyword-face))
      (insert "  a   /analyze    Analyze issue, create plan\n")
      (insert "  s   /ship       Implement approved plan\n")
      (insert "  p   /pr         Create pull request\n")
      (insert "  !   /fix-ci     Check and fix CI failures\n\n")
      (insert (propertize "Session:\n" 'face 'font-lock-keyword-face))
      (insert "  h   /help       Show Claude help\n")
      (insert "  c   /clear      Clear conversation\n")
      (insert "  C   /compact    Summarize conversation\n")
      (insert "  r   /resume     Resume previous session\n\n")
      (insert (propertize "Other:\n" 'face 'font-lock-keyword-face))
      (insert "  /   Menu        Pick from all commands\n")
      (insert "  $   /cost       Show token usage\n")
      (insert "  ?   Help        This buffer\n")
      (insert "  q   Quit        Close this help\n")
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (display-buffer help-buf '((display-buffer-in-side-window)
                               (side . bottom)
                               (window-height . 16)))))

(defun orchard-claude-send-clear ()
  "Send /clear to Claude."
  (interactive)
  (orchard--send-to-claude "/clear"))

(defun orchard-claude-send-compact ()
  "Send /compact to Claude."
  (interactive)
  (orchard--send-to-claude "/compact"))

(defun orchard-claude-send-resume ()
  "Send /resume to Claude."
  (interactive)
  (orchard--send-to-claude "/resume"))

(defun orchard-claude-send-cost ()
  "Send /cost to Claude."
  (interactive)
  (orchard--send-to-claude "/cost"))

(defun orchard-claude-send-fix-ci ()
  "Send /fix-ci to Claude to check and fix CI failures."
  (interactive)
  (orchard--send-to-claude "/fix-ci"))

;; Keymap for agent-shell Claude buffers
(defvar orchard-claude-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") #'orchard-claude-command-menu)
    (define-key map (kbd "?") #'orchard-claude-command-help)
    (define-key map (kbd "a") #'orchard-claude-send-analyze)
    (define-key map (kbd "s") #'orchard-claude-send-ship)
    (define-key map (kbd "p") #'orchard-claude-send-pr)
    (define-key map (kbd "!") #'orchard-claude-send-fix-ci)
    (define-key map (kbd "h") #'orchard-claude-send-help)
    (define-key map (kbd "c") #'orchard-claude-send-clear)
    (define-key map (kbd "C") #'orchard-claude-send-compact)
    (define-key map (kbd "r") #'orchard-claude-send-resume)
    (define-key map (kbd "$") #'orchard-claude-send-cost)
    map)
  "Keymap for Claude commands. Bind to a prefix in shell-maker-mode.")

;; Completion for /commands
(defun orchard--claude-command-completion ()
  "Completion-at-point function for Claude slash commands."
  (when (and (derived-mode-p 'shell-maker-mode)
             (string-match-p "Claude" (buffer-name)))
    (let* ((line-start (save-excursion
                         (comint-bol)
                         (point)))
           (input (buffer-substring-no-properties line-start (point))))
      ;; Only complete if input starts with /
      (when (string-prefix-p "/" input)
        (list line-start
              (point)
              (mapcar #'car orchard-claude-commands)
              :exclusive 'no)))))

(defun orchard--slash-or-complete ()
  "Insert / or complete if at start of input with /."
  (interactive)
  (let* ((line-start (save-excursion (comint-bol) (point)))
         (input (buffer-substring-no-properties line-start (point))))
    (if (string= input "")
        ;; At start of line - insert / and show completions
        (progn
          (insert "/")
          (completion-at-point))
      ;; Not at start - just insert /
      (insert "/"))))

(defun orchard--question-or-help ()
  "Show help if at start of input, otherwise insert ?."
  (interactive)
  (let* ((line-start (save-excursion (comint-bol) (point)))
         (input (buffer-substring-no-properties line-start (point))))
    (if (string= input "")
        (orchard-claude-command-help)
      (insert "?"))))

;; Hook to add keybindings in agent-shell buffers
(defun orchard--setup-agent-shell-keys ()
  "Setup Claude command keybindings in agent-shell buffers."
  (when (and (derived-mode-p 'shell-maker-mode)
             (string-match-p "Claude" (buffer-name)))
    ;; Prefix map
    (local-set-key (kbd "C-c /") orchard-claude-command-map)
    (local-set-key (kbd "C-c C-/") #'orchard-claude-command-menu)
    ;; Smart / and ? at prompt
    (local-set-key (kbd "/") #'orchard--slash-or-complete)
    (local-set-key (kbd "?") #'orchard--question-or-help)
    ;; Add completion function
    (add-hook 'completion-at-point-functions
              #'orchard--claude-command-completion nil t)))

(add-hook 'shell-maker-mode-hook #'orchard--setup-agent-shell-keys)

(provide 'orchard-claude)
;;; orchard-claude.el ends here
