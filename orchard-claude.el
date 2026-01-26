;;; orchard-claude.el --- Orchard Claude integration -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains Claude-related functionality:
;; - Claude status tracking (via hooks)
;; - Claude buffer management
;; - Starting/resuming Claude sessions
;; - Claude settings/commands setup

(require 'orchard-vars)
(require 'orchard-cache)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard-refresh "orchard-dashboard")
(declare-function orchard--get-worktrees "orchard-worktree")
(declare-function orchard--find-worktree-for-issue "orchard")
(declare-function orchard--issue-workflow-stage "orchard")
(declare-function orchard--get-open-issues "orchard-cache")
(declare-function orchard--find-best-window "orchard")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Status Tracking (via hooks)
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Uses Claude Code's Notification and Stop hooks for reliable status tracking.
;; Much more reliable than text pattern matching in vterm buffers.

(defvar orchard--claude-status-table (make-hash-table :test 'equal)
  "Hash table mapping buffer names to Claude status.
Values: `waiting' (needs input), `idle' (finished), or `active' (working).")

(defvar orchard--refresh-timer nil
  "Timer for debounced orchard refresh.")

(defun orchard--claude-status-hook (message)
  "Handle Claude hook events to track status.
MESSAGE is a plist with :type and :buffer-name."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name)))
    (when buffer-name
      (cond
       ((eq hook-type 'notification)
        ;; Claude is waiting for user input
        (puthash buffer-name 'waiting orchard--claude-status-table)
        ;; Trigger orchard refresh if visible
        (when (get-buffer "*orchard*")
          (orchard--schedule-refresh)))
       ((eq hook-type 'stop)
        ;; Claude finished working, now idle
        (puthash buffer-name 'idle orchard--claude-status-table)
        (when (get-buffer "*orchard*")
          (orchard--schedule-refresh)))
       ((memq hook-type '(pre-tool-use post-tool-use))
        ;; Claude is actively working (using tools)
        (puthash buffer-name 'active orchard--claude-status-table))))))

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

(defun orchard--claude-status-cleanup ()
  "Remove dead buffer entries from status table."
  (let ((dead-keys '()))
    (maphash (lambda (k _v)
               (unless (get-buffer k)
                 (push k dead-keys)))
             orchard--claude-status-table)
    (dolist (k dead-keys)
      (remhash k orchard--claude-status-table))))

(defun orchard--claude-mark-active (buffer-name)
  "Mark BUFFER-NAME as active (user is typing)."
  (puthash buffer-name 'active orchard--claude-status-table))

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
  "Get Claude BUFFER status: 'waiting, 'idle, 'active, or nil.
- waiting: needs approval (prompts, questions)
- idle: finished work, at prompt
- active: still processing

Uses live pattern matching to detect actual terminal state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((end-text (buffer-substring-no-properties
                         (max (point-min) (- (point) 1000))
                         (point))))
          (when end-text
            (cond
             ;; Waiting for approval - prompts (highest priority)
             ((or (string-match-p "Do you want to proceed" end-text)
                  (string-match-p "❯ 1\\. Yes" end-text)
                  (string-match-p "yes/no" end-text)
                  (string-match-p "Y/n\\|y/N" end-text)
                  (string-match-p "Esc to cancel" end-text))
              'waiting)
             ;; Actively working - "esc to interrupt" is always shown
             ((string-match-p "esc to interrupt" end-text)
              'active)
             ;; Idle/Done - at prompt, finished (past tense verbs)
             ((or (string-match-p " for [0-9]+m?s\\b" end-text)  ; "Cooked for 5s"
                  (string-match-p "accept edits on" end-text)
                  (string-match-p "\n❯[[:space:]]*$" end-text)
                  (string-match-p "───+\n❯" end-text))
              'idle)
             ;; Default: active
             (t 'active))))))))

(defun orchard--claude-waiting-p (buffer)
  "Check if Claude BUFFER needs attention (waiting or idle)."
  (memq (orchard--claude-status buffer) '(waiting idle)))

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

(defun orchard--start-claude-with-resume (path)
  "Start Claude for PATH in best window. Does NOT auto-resume - user can /resume manually."
  (orchard--ensure-claude-loaded)
  (let* ((target-win (orchard--find-best-window))
         (existing-claude (orchard--claude-buffer-for-path path)))
    (select-window target-win)
    (if existing-claude
        ;; Claude exists - just switch to it
        (switch-to-buffer existing-claude)
      ;; Start new Claude
      (let ((default-directory path))
        (claude-code)))))

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
  "Start Claude for PATH in best window and run COMMAND after initialization."
  (orchard--ensure-claude-loaded)
  (let* ((target-win (orchard--find-best-window))
         (existing-claude (orchard--claude-buffer-for-path path)))
    (select-window target-win)
    (if existing-claude
        ;; Claude exists - switch to it and send command
        (progn
          (switch-to-buffer existing-claude)
          (orchard--send-command-to-claude existing-claude 0.5 command))
      ;; Start new Claude
      (let ((default-directory path)
            (buffers-before (buffer-list)))
        (claude-code)
        ;; Find the new Claude buffer and send command after init
        (run-at-time 3 nil
                     (lambda ()
                       (when-let ((new-claude
                                   (cl-find-if
                                    (lambda (buf)
                                      (and (string-prefix-p "*claude:" (buffer-name buf))
                                           (not (memq buf buffers-before))
                                           (buffer-live-p buf)))
                                    (buffer-list))))
                         (orchard--send-command-to-claude new-claude 0 command))))))))

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

(defun orchard-debug-claude-status ()
  "Show diagnostic info for all Claude buffers and issue mappings.
Use this to debug why Claude status isn't showing in orchard."
  (interactive)
  (let* ((claude-bufs (orchard--get-claude-buffers))
         (worktrees (orchard--get-worktrees))
         (issues (orchard--get-open-issues)))
    (with-output-to-temp-buffer "*orchard-claude-debug*"
      (princ "=== HOOK STATUS TABLE ===\n\n")
      (if (= 0 (hash-table-count orchard--claude-status-table))
          (princ "  (empty - no hook events received yet)\n")
        (maphash (lambda (k v)
                   (princ (format "  %s -> %s\n" k v)))
                 orchard--claude-status-table))
      (princ "\n")

      (princ "=== CLAUDE BUFFERS ===\n\n")
      (if (null claude-bufs)
          (princ "NO CLAUDE BUFFERS FOUND\n")
        (dolist (buf claude-bufs)
          (let* ((name (buffer-name buf))
                 (hook-status (gethash name orchard--claude-status-table))
                 (full-status (orchard--claude-status buf))
                 (source (if hook-status "hook" "pattern"))
                 (end-text (with-current-buffer buf
                             (buffer-substring-no-properties
                              (max (point-min) (- (point-max) 300))
                              (point-max)))))
            (princ (format "Buffer: %s\n" name))
            (princ (format "Status: %s (via %s)\n" (or full-status "NIL") source))
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

(provide 'orchard-claude)
;;; orchard-claude.el ends here
