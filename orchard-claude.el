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
  (let ((existing-claude (orchard--claude-buffer-for-path path)))
    (if existing-claude
        ;; Claude exists - find its window or use best window
        (let ((existing-win (get-buffer-window existing-claude)))
          (if existing-win
              (select-window existing-win)
            ;; Buffer exists but no window - find a good window for it
            (let ((target-win (orchard--find-best-window)))
              (select-window target-win)
              (switch-to-buffer existing-claude))))
      ;; Start new Claude in best window
      (let ((target-win (orchard--find-best-window)))
        (select-window target-win)
        (let ((default-directory path))
          (claude-code))))))

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

(defun orchard--get-active-claude-paths ()
  "Get list of worktree paths with active Claude sessions."
  (let ((paths nil))
    (dolist (buf (orchard--get-claude-buffers))
      (when (buffer-live-p buf)
        (let ((buf-name (buffer-name buf)))
          ;; Extract path from buffer name like "*claude: /path/to/worktree*"
          (when (string-match "\\*claude:\\s-*\\(.+?\\)\\*?" buf-name)
            (let ((path (match-string 1 buf-name)))
              (when (file-directory-p path)
                (push path paths)))))))
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
           ((orchard--claude-buffer-for-path path)
            (cl-incf skipped)) ; already running
           (t
            (orchard--start-claude-with-resume path)
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

;; Session persistence DISABLED - path extraction was buggy (saving "~" instead of real paths)
;; Manual resume still available via orchard-resume-sessions if needed
;; (add-hook 'kill-emacs-hook #'orchard--save-claude-sessions)

;; Register Claude event hook for status tracking and PR capture
(with-eval-after-load 'claude-code
  (add-hook 'claude-code-event-hook #'orchard--claude-status-hook))

(provide 'orchard-claude)
;;; orchard-claude.el ends here
