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

(use-package! claude-code
  :after vterm
  :config
  ;; Use vterm as the terminal backend (already have it via Doom)
  (setq claude-code-terminal-backend 'vterm)

  ;; Display Claude below the current window (same column)
  ;; This keeps branch work in a single column: magit on top, claude below
  (defun claude-code-display-buffer-below (buffer)
    "Display Claude BUFFER below current window, staying in same column."
    (let* ((current-win (selected-window))
           (new-window (split-window-below)))
      (set-window-buffer new-window buffer)
      (select-window new-window)
      new-window))

  (setq claude-code-display-window-fn #'claude-code-display-buffer-below)

  ;; Bind to C-c c prefix via ashton-mode-map for consistency
  (define-key ashton-mode-map (kbd "C-c c c") #'claude-code)
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
            (content (with-current-buffer buf (buffer-string)))
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
;;; Vterm Completion (handles read-only mode)
;;; ════════════════════════════════════════════════════════════════════════════

(defun claude--ensure-writable ()
  "Ensure Claude buffer is not in read-only mode for insertion."
  (when (and (claude-buffer-p)
             (bound-and-true-p claude-code-read-only-mode))
    (claude-code-toggle-read-only-mode)))

(defun vterm-dabbrev-expand ()
  "Dabbrev expand in vterm by entering copy mode, completing, then returning."
  (interactive)
  (claude--ensure-writable)
  (if (derived-mode-p 'vterm-mode)
      (let ((word (thing-at-point 'word t)))
        (if word
            ;; Use dabbrev to find expansion from other buffers
            (let* ((dabbrev-check-all-buffers t)
                   (expansion (dabbrev--find-expansion word 0 nil)))
              (if expansion
                  (progn
                    ;; Delete the partial word and insert expansion
                    (vterm-send-key "C-w")  ; Delete word backward
                    (vterm-insert expansion))
                (message "No expansion found for: %s" word)))
          (message "No word at point")))
    ;; Not in vterm, use regular dabbrev
    (dabbrev-expand nil)))

(defun vterm-completion-at-point ()
  "Show completion candidates for word at point in vterm."
  (interactive)
  (claude--ensure-writable)
  (if (derived-mode-p 'vterm-mode)
      (let ((word (thing-at-point 'word t)))
        (if word
            (let* ((dabbrev-check-all-buffers t)
                   (candidates (dabbrev--find-all-expansions word nil)))
              (if candidates
                  (let ((choice (completing-read
                                 (format "Complete '%s': " word)
                                 candidates nil nil)))
                    (when choice
                      (vterm-send-key "C-w")
                      (vterm-insert choice)))
                (message "No completions found")))
          (message "No word at point")))
    (completion-at-point)))

;; Bind in vterm-mode-map (C-c / for completion since M-/ is captured)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c /") #'vterm-completion-at-point)
  (define-key vterm-mode-map (kbd "C-c TAB") #'vterm-completion-at-point))

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

(provide 'config-claude)
;;; config-claude.el ends here
