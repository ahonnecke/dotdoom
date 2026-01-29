;;; orchard.el --- Orchard - Issue-centric worktree manager -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This is the main entry point for Orchard. It loads all sub-modules
;; and provides the core functionality:
;; - Core utility functions used by multiple modules
;; - Mode cycling (magit/claude)
;; - Branch opening with smart window selection
;; - Issue diagnostics and resolution
;; - Transient dispatch menu
;; - Global keybindings

(require 'orchard-vars)
(require 'orchard-cache)
(require 'orchard-worktree)
(require 'orchard-window)
(require 'orchard-claude)
(require 'orchard-dashboard)
(require 'orchard-actions)

;;; Forward declarations
(declare-function magit-status "magit")
(declare-function magit-get-mode-buffer "magit")
(declare-function claude-code "claude-code")
(declare-function orchard "orchard-dashboard")
(declare-function orchard-force-refresh "orchard-dashboard")
(declare-function orchard-view-all "orchard-actions")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Utility Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--is-main-worktree-p (path)
  "Return t if PATH is the main (bare) worktree."
  (let ((repo-root (orchard--get-repo-root)))
    (and repo-root
         (string= (file-name-as-directory (expand-file-name path))
                  (file-name-as-directory (expand-file-name repo-root))))))

(defun orchard--current-worktree ()
  "Get the worktree for the current buffer, if any."
  (let ((dir (expand-file-name default-directory)))
    (cl-find-if (lambda (wt)
                  (string-prefix-p (file-name-as-directory (alist-get 'path wt))
                                   (file-name-as-directory dir)))
                (orchard--get-worktrees))))

(defun orchard--project-root ()
  "Get project root for current directory (fallback when not in orchard worktree)."
  (or (locate-dominating-file default-directory ".git")
      default-directory))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Window Selection
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--leftmost-window ()
  "Get the leftmost window."
  (car (sort (window-list nil 'no-mini)
             (lambda (a b) (< (car (window-edges a)) (car (window-edges b)))))))

(defun orchard--window-showing-claude-p (win)
  "Return t if WIN is showing a Claude buffer."
  (string-prefix-p "*claude:" (buffer-name (window-buffer win))))

(defun orchard--window-showing-orchard-p (win)
  "Return t if WIN is showing the Orchard buffer."
  (string= "*Orchard*" (buffer-name (window-buffer win))))

(defun orchard--window-reusable-p (window)
  "Return t if WINDOW is showing a buffer we can take over for Claude.
Includes empty buffers and magit buffers (since we're opening Claude, not magit)."
  (let ((buf-name (buffer-name (window-buffer window))))
    (or (string= buf-name "*scratch*")
        (string= buf-name "*Messages*")
        (string= buf-name "*doom*")
        (string-prefix-p " " buf-name)  ; internal buffers
        (string-prefix-p "*Help" buf-name)
        (string-prefix-p "*Completions" buf-name)
        (string-prefix-p "*magit" buf-name)  ; magit buffers are fine to reuse
        (string-prefix-p "magit" buf-name)   ; magit-diff, magit-log, etc
        (string-prefix-p "*forge" buf-name)  ; forge PR buffers
        (string-prefix-p "*Diff" buf-name)   ; diff buffers
        )))

;; Keep old name as alias for compatibility
(defalias 'orchard--window-empty-p 'orchard--window-reusable-p)

(defun orchard--find-best-window ()
  "Find the best window for opening Claude/magit.
Priority:
1. Empty window (scratch, messages, etc.)
2. Split a non-orchard window if frame is wide enough
3. Largest non-orchard, non-claude window
4. Current window as fallback
NEVER takes over an existing Claude window."
  (let* ((windows (window-list nil 'no-mini))
         (leftmost (orchard--leftmost-window))
         (non-leftmost (cl-remove leftmost windows))
         (frame-width (frame-width)))
    (or
     ;; 1. Find empty window
     (cl-find-if #'orchard--window-empty-p non-leftmost)
     ;; 2. Split if we have room (frame > 160 cols and only 1-2 windows)
     (when (and (>= frame-width 160)
                (<= (length windows) 2))
       (let ((splittable (cl-find-if
                          (lambda (w)
                            (and (not (orchard--window-showing-orchard-p w))
                                 (not (orchard--window-showing-claude-p w))
                                 (>= (window-width w) 80)))
                          non-leftmost)))
         (when splittable
           (split-window splittable nil 'right))))
     ;; 3. Largest non-orchard, non-claude window
     (car (sort (cl-remove-if
                 (lambda (w)
                   (or (orchard--window-showing-orchard-p w)
                       (orchard--window-showing-claude-p w)))
                 non-leftmost)
                (lambda (a b)
                  (> (window-width a) (window-width b)))))
     ;; 4. Fallback: largest window that isn't orchard (may be claude - last resort)
     (car (sort (cl-remove-if #'orchard--window-showing-orchard-p windows)
                (lambda (a b)
                  (> (window-width a) (window-width b)))))
     ;; 5. Last resort: current window
     (selected-window))))

(defun orchard--find-sibling-window (mode-predicate)
  "Find a sibling window in the same column matching MODE-PREDICATE."
  (let ((current (selected-window))
        (current-left (car (window-edges))))
    (cl-find-if
     (lambda (w)
       (and (not (eq w current))
            ;; Same column (same left edge)
            (= (car (window-edges w)) current-left)
            ;; Matches mode
            (with-current-buffer (window-buffer w)
              (funcall mode-predicate))))
     (window-list))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Mode Cycling
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-cycle-mode ()
  "Simple toggle between magit ↔ claude for current project.
No fancy window management - just switch buffers."
  (interactive)
  (let ((project-root (or (locate-dominating-file default-directory ".git")
                          default-directory)))
    (cond
     ;; In magit - go to claude (find existing or start new)
     ((derived-mode-p 'magit-mode)
      (let ((claude-buf (orchard--claude-buffer-for-path project-root)))
        (if (and claude-buf (buffer-live-p claude-buf))
            (switch-to-buffer claude-buf)
          ;; Start new Claude - preserve windows
          (let* ((target-win (selected-window))
                 (win-config (current-window-configuration))
                 (buffers-before (buffer-list))
                 (default-directory project-root))
            (claude-code)
            (let ((new-claude (cl-find-if
                               (lambda (buf)
                                 (and (string-prefix-p "*claude:" (buffer-name buf))
                                      (not (memq buf buffers-before))))
                               (buffer-list))))
              (set-window-configuration win-config)
              (when new-claude
                (set-window-buffer target-win new-claude)
                (orchard--fix-claude-size new-claude target-win)))))))

     ;; In claude/vterm - go to magit
     ((derived-mode-p 'vterm-mode)
      (magit-status project-root))

     ;; Elsewhere - go to magit
     (t
      (magit-status project-root)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch Opening
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-open-branch (wt)
  "Open magit for worktree WT in best available window.
Preserves Claude buffers, keeps Orchard on far left."
  (let* ((path (alist-get 'path wt))
         (branch (alist-get 'branch wt))
         (target-win (orchard--find-best-window)))
    (message "Opening %s" branch)
    (select-window target-win)
    (magit-status path)))

(defun orchard--open-magit-and-claude (path window)
  "Open magit and Claude for PATH in WINDOW (split vertically).
Magit on top, Claude below."
  ;; Undedicate window if needed
  (set-window-dedicated-p window nil)
  ;; Put magit in the window
  (select-window window)
  (magit-status path)
  ;; After magit-status, selected window might have changed - get the magit window
  (let ((magit-win (get-buffer-window (magit-get-mode-buffer 'magit-status-mode))))
    (when magit-win
      ;; Split for Claude below
      (let ((claude-win (split-window magit-win nil 'below)))
        ;; Resize - give magit 40%, claude 60%
        (let ((target-height (floor (* 0.4 (+ (window-height magit-win)
                                               (window-height claude-win))))))
          (window-resize magit-win (- target-height (window-height magit-win))))
        ;; Put Claude in the lower window
        (select-window claude-win)
        (orchard--ensure-claude-loaded)
        (let ((existing-claude (orchard--claude-buffer-for-path path)))
          (if (and existing-claude (buffer-live-p existing-claude))
              (progn
                (message "Orchard: reusing Claude %s" (buffer-name existing-claude))
                (set-window-buffer claude-win existing-claude))
            ;; Start new Claude - save our layout, let claude-code mess it up, restore
            (let* ((split-config (current-window-configuration))
                   (default-directory path)
                   (buffers-before (buffer-list)))
              (message "Orchard: starting Claude for %s" path)
              (condition-case err
                  (claude-code)
                (error (message "Orchard: claude-code error: %s" err)))
              ;; Find the new buffer
              (let ((new-claude (cl-find-if
                                 (lambda (buf)
                                   (and (string-prefix-p "*claude:" (buffer-name buf))
                                        (not (memq buf buffers-before))))
                                 (buffer-list))))
                ;; Restore our split layout
                (set-window-configuration split-config)
                (if new-claude
                    (progn
                      (message "Orchard: found Claude %s" (buffer-name new-claude))
                      (set-window-buffer claude-win new-claude)
                      (orchard--fix-claude-size new-claude claude-win))
                  (message "Orchard: no Claude buffer, removing split")
                  (delete-window claude-win))))))
        ;; Return focus to magit
        (when (window-live-p magit-win)
          (select-window magit-win))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Issue Browser
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-issue-browse ()
  "Open issue at point in browser."
  (interactive)
  (if-let ((issue (orchard--get-issue-at-point)))
      (let ((url (alist-get 'url issue)))
        (if url
            (progn
              (browse-url url)
              (message "Opened issue #%d in browser" (alist-get 'number issue)))
          (user-error "No URL for issue")))
    (user-error "No issue at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Issue Diagnostics
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-issue-state (issue-num)
  "Get complete state information for ISSUE-NUM.
Returns plist with :category, :issue, :worktree, :gh-state, :pr-state, etc."
  (let* ((worktrees (orchard--get-worktrees t))  ; include hidden
         (open-issues (orchard--get-open-issues))
         (issue (cl-find issue-num open-issues
                         :key (lambda (i) (alist-get 'number i))))
         (wt (orchard--find-worktree-for-issue issue-num worktrees))
         (wt-path (when wt (alist-get 'path wt)))
         (branch (when wt (alist-get 'branch wt)))
         (stage (when wt (alist-get 'stage wt)))
         (hidden-wt (when wt-path (orchard--worktree-hidden-p wt-path)))
         (hidden-issue (orchard--issue-hidden-p issue-num))
         (session-info (when wt-path (orchard--get-worktree-session-info wt-path)))
         ;; Check GitHub state
         (gh-issue-open (not (null issue)))
         (gh-issue-closed (orchard--issue-closed-p issue-num))
         ;; Determine category
         (category (cond
                    ((and hidden-issue hidden-wt) 'hidden)
                    (hidden-issue 'hidden-issue)
                    (hidden-wt 'hidden-worktree)
                    ((and gh-issue-closed wt) 'done)
                    ((and (not wt) gh-issue-open) 'up-next)
                    ((and wt (eq stage 'merged) gh-issue-open) 'qa-verify)
                    ((and wt gh-issue-open) 'in-progress)
                    ((and wt (not gh-issue-open) (not gh-issue-closed)) 'orphan)
                    (t 'unknown))))
    (list :issue-num issue-num
          :category category
          :issue issue
          :worktree wt
          :branch branch
          :stage stage
          :hidden-worktree hidden-wt
          :hidden-issue hidden-issue
          :gh-issue-open gh-issue-open
          :gh-issue-closed gh-issue-closed
          :session-info session-info)))

(defun orchard--format-issue-state (state)
  "Format STATE plist as human-readable status string."
  (let* ((issue-num (plist-get state :issue-num))
         (category (plist-get state :category))
         (issue (plist-get state :issue))
         (wt (plist-get state :worktree))
         (branch (plist-get state :branch))
         (stage (plist-get state :stage))
         (hidden-wt (plist-get state :hidden-worktree))
         (hidden-issue (plist-get state :hidden-issue))
         (gh-open (plist-get state :gh-issue-open))
         (gh-closed (plist-get state :gh-issue-closed))
         (session (plist-get state :session-info))
         (title (when issue (alist-get 'title issue))))
    (concat
     (format "Issue #%d Status\n" issue-num)
     (make-string 40 ?─) "\n\n"
     (format "Category: %s\n" (propertize (symbol-name category) 'face 'font-lock-keyword-face))
     (format "Title: %s\n" (or title "(not in open issues cache)"))
     "\n"
     "GitHub State:\n"
     (format "  Issue open: %s\n" (if gh-open "yes" "no"))
     (format "  Issue closed: %s\n" (if gh-closed "yes" "no"))
     "\n"
     "Local State:\n"
     (format "  Has worktree: %s\n" (if wt "yes" "no"))
     (when branch (format "  Branch: %s\n" branch))
     (when wt (format "  Path: %s\n" (alist-get 'path wt)))
     (when stage (format "  Stage: %s\n" stage))
     (when (alist-get 'dirty wt) "  Dirty: yes (uncommitted changes)\n")
     (when (> (or (alist-get 'ahead wt) 0) 0)
       (format "  Ahead: %d commits\n" (alist-get 'ahead wt)))
     (when (> (or (alist-get 'behind wt) 0) 0)
       (format "  Behind: %d commits\n" (alist-get 'behind wt)))
     "\n"
     "Visibility:\n"
     (format "  Issue hidden: %s\n" (if hidden-issue "yes" "no"))
     (format "  Worktree hidden: %s\n" (if hidden-wt "yes" "no"))
     "\n"
     (when session
       (format "Claude Session:\n  Messages: %d\n  Last active: %s\n"
               (plist-get session :message-count)
               (or (orchard--format-relative-time (plist-get session :modified)) "unknown")))
     "\n"
     "View visibility:\n"
     (format "  Current view: %s\n" orchard--current-view)
     (format "  Visible in current view: %s\n"
             (if (orchard--section-visible-p category) "yes" "NO - switch view with 'v a'")))))

(defvar-local orchard--issue-state nil
  "Buffer-local variable storing issue state for status buffer.")

(defun orchard-find-issue (issue-num)
  "Find and display status for ISSUE-NUM.
Shows category, GitHub state, worktree info, and visibility."
  (interactive "nIssue number: ")
  (let* ((state (orchard--get-issue-state issue-num))
         (buf (get-buffer-create "*Orchard Issue Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (orchard--format-issue-state state))
        (insert "\n")
        (insert (make-string 40 ?─) "\n")
        (insert "Actions:\n")
        (insert "  [o] Open in browser\n")
        (insert "  [g] Go to issue in dashboard\n")
        (when (plist-get state :worktree)
          (insert "  [c] Open Claude for worktree\n")
          (insert "  [m] Open Magit for worktree\n"))
        (insert "  [r] Resolve/fix issues\n")
        (insert "  [q] Close\n")
        ;; Store state for actions
        (setq orchard--issue-state state))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "o") (lambda () (interactive)
                                 (let ((repo (orchard--get-repo-root)))
                                   (browse-url (format "%s/issues/%d"
                                                       (string-trim (shell-command-to-string
                                                                     (format "cd %s && gh repo view --json url -q .url" repo)))
                                                       (plist-get orchard--issue-state :issue-num))))))
      (local-set-key (kbd "g") (lambda () (interactive)
                                 (orchard-goto-issue (plist-get orchard--issue-state :issue-num))))
      (local-set-key (kbd "c") (lambda () (interactive)
                                 (when-let ((wt (plist-get orchard--issue-state :worktree)))
                                   (orchard--start-claude-with-resume (alist-get 'path wt)))))
      (local-set-key (kbd "m") (lambda () (interactive)
                                 (when-let ((wt (plist-get orchard--issue-state :worktree)))
                                   (magit-status (alist-get 'path wt)))))
      (local-set-key (kbd "r") (lambda () (interactive)
                                 (orchard-resolve-issue (plist-get orchard--issue-state :issue-num)))))
    (pop-to-buffer buf)))

(defun orchard-goto-issue (issue-num)
  "Jump to ISSUE-NUM in the orchard dashboard."
  (interactive "nIssue number: ")
  (orchard)  ; ensure dashboard is open
  (goto-char (point-min))
  (let ((pattern (format "#%d " issue-num)))
    (if (search-forward pattern nil t)
        (progn
          (beginning-of-line)
          (message "Found issue #%d" issue-num))
      ;; Try with 'v a' to show all sections
      (orchard-view-all)
      (orchard-refresh)
      (goto-char (point-min))
      (if (search-forward pattern nil t)
          (progn
            (beginning-of-line)
            (message "Found issue #%d (switched to 'all' view)" issue-num))
        (message "Issue #%d not found in dashboard" issue-num)))))

(defun orchard--archive-worktree (wt)
  "Archive worktree WT (delete worktree but keep remote branch)."
  (let* ((path (alist-get 'path wt))
         (branch (alist-get 'branch wt))
         (default-directory (orchard--get-repo-root)))
    ;; Remove worktree
    (shell-command-to-string (format "git worktree remove --force %s" (shell-quote-argument path)))
    (message "Archived worktree for branch %s" branch)))

(defun orchard-resolve-issue (issue-num)
  "Diagnose and offer to fix issues with ISSUE-NUM's state."
  (interactive "nIssue number: ")
  (let* ((state (orchard--get-issue-state issue-num))
         (category (plist-get state :category))
         (wt (plist-get state :worktree))
         (hidden-wt (plist-get state :hidden-worktree))
         (hidden-issue (plist-get state :hidden-issue))
         (gh-open (plist-get state :gh-issue-open))
         (gh-closed (plist-get state :gh-issue-closed))
         (stage (plist-get state :stage))
         (problems '())
         (fixes '()))
    ;; Detect problems
    (when (and hidden-issue (not hidden-wt) wt)
      (push "Issue is hidden but worktree is visible" problems)
      (push '("Unhide the issue" . unhide-issue) fixes))
    (when (and hidden-wt (not hidden-issue) gh-open)
      (push "Worktree is hidden but issue is visible" problems)
      (push '("Unhide the worktree" . unhide-worktree) fixes))
    (when (and wt (eq stage 'merged) gh-open)
      (push "PR merged but issue still open - needs QA verification" problems)
      (push '("Open issue in browser to close" . browse-issue) fixes))
    (when (and gh-closed wt (not (eq category 'done)))
      (push "Issue is closed but worktree exists - ready to archive" problems)
      (push '("Archive the worktree" . archive-worktree) fixes))
    (when (and (not wt) (not gh-open) (not gh-closed))
      (push "Issue not found in cache - may need refresh" problems)
      (push '("Force refresh caches" . force-refresh) fixes))
    (when (eq category 'unknown)
      (push "Issue is in an unknown state" problems)
      (push '("Force refresh caches" . force-refresh) fixes))
    ;; Always offer view switch if not visible
    (unless (orchard--section-visible-p category)
      (push (format "Issue not visible in '%s' view" orchard--current-view) problems)
      (push '("Switch to 'all' view" . switch-view) fixes))
    ;; Show results
    (if (null problems)
        (message "Issue #%d appears healthy (category: %s)" issue-num category)
      (let* ((prompt (format "Issue #%d problems:\n%s\n\nFix: "
                             issue-num
                             (mapconcat (lambda (p) (concat "  - " p)) problems "\n")))
             (choice (completing-read prompt
                                      (mapcar #'car fixes)
                                      nil t)))
        (when choice
          (let ((action (cdr (assoc choice fixes))))
            (pcase action
              ('unhide-issue
               (orchard--unhide-issue issue-num)
               (orchard-refresh)
               (message "Unhid issue #%d" issue-num))
              ('unhide-worktree
               (orchard--unhide-worktree (alist-get 'path wt))
               (orchard-refresh)
               (message "Unhid worktree for #%d" issue-num))
              ('browse-issue
               (let ((repo (orchard--get-repo-root)))
                 (browse-url (format "%s/issues/%d"
                                     (string-trim (shell-command-to-string
                                                   (format "cd %s && gh repo view --json url -q .url" repo)))
                                     issue-num))))
              ('archive-worktree
               (when (yes-or-no-p (format "Archive worktree for #%d? " issue-num))
                 (orchard--archive-worktree wt)
                 (orchard-refresh)
                 (message "Archived worktree for #%d" issue-num)))
              ('force-refresh
               (orchard-force-refresh))
              ('switch-view
               (orchard-view-all)
               (orchard-refresh)))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Test Results Tracking
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-test-results (path)
  "Get test results for worktree at PATH from state."
  (let ((results (plist-get orchard--state :test-results)))
    (cdr (assoc path results))))

(defun orchard--set-test-results (path passed failed total)
  "Store test results for PATH."
  (let ((results (plist-get orchard--state :test-results)))
    (setq results (assoc-delete-all path results))
    (push (cons path (list :passed passed :failed failed :total total
                           :complete (zerop (- total passed failed))))
          results)
    (setq orchard--state (plist-put orchard--state :test-results results))
    (orchard--save-state)))

(defun orchard--clear-test-results (path)
  "Clear test results for PATH."
  (let ((results (plist-get orchard--state :test-results)))
    (setq results (assoc-delete-all path results))
    (setq orchard--state (plist-put orchard--state :test-results results))
    (orchard--save-state)))

(defun orchard-update-test-results ()
  "Interactively update test results for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (existing (orchard--get-test-results path))
             (total (read-number "Total tests: " (or (plist-get existing :total) 0)))
             (passed (read-number "Passed: " (or (plist-get existing :passed) 0)))
             (failed (read-number "Failed: " (or (plist-get existing :failed) 0))))
        (orchard--set-test-results path passed failed total)
        (message "Test results: %d/%d passed, %d failed" passed total failed)
        (orchard--refresh-if-visible))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Commando Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-commando-at-point ()
  "Open commando in the worktree at point."
  (interactive)
  (when-let* ((wt (orchard--get-worktree-at-point))
              (path (alist-get 'path wt)))
    (let ((default-directory path))
      (if (fboundp 'commando)
          (commando)
        (user-error "Commando not available")))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix orchard-dispatch ()
  "Orchard command menu."
  ["Orchard - Worktree Manager"
   ["Create (TAB-completable)"
    ("F" "Feature (FEATURE/)" orchard-new-feature)
    ("B" "Bugfix (BUGFIX/)" orchard-new-bugfix)
    ("C" "Chore (CHORE/)" orchard-new-chore)
    ("R" "Refactor (REFACTOR/)" orchard-new-refactor)
    ("D" "Docs (DOCS/)" orchard-new-docs)
    ("E" "Experiment (EXPERIMENT/)" orchard-new-experiment)]
   ["GitHub Issues"
    ("I" "Start from issue" orchard-issue-start)
    ("o" "Open issue in browser" orchard-issue-browse)
    ("#" "Find issue by number" orchard-find-issue)
    ("!" "Diagnose/resolve issue" orchard-resolve-issue)
    ("i" "Pick issue (prompt)" orchard-issue-start-prompt)
    ("/" "Search (free text)" orchard-filter-by-text)
    ("L" "Filter by label" orchard-filter-by-label)
    ("\\" "Clear all filters" orchard-clear-filters)
    ("s" "Toggle staging issues" orchard-toggle-staging-issues)]
   ["At Point"
    ("RET" "Open in column" orchard-open-at-point)
    ("m" "Magit" orchard-magit-at-point)
    ("c" "Claude" orchard-claude-at-point)
    ("d" "Dired" orchard-dired-at-point)
    ("t" "Test (testicular)" orchard-test-at-point)
    ("T" "Update test results" orchard-update-test-results)
    ("`" "Commands (commando)" orchard-commando-at-point)]
   ["Lifecycle"
    ("N" "Next step" orchard-next-step)
    ("u" "Push (upload)" orchard-push-at-point)
    ("P" "Create PR" orchard-pr-at-point)
    ("S-m" "Merge PR" orchard-merge-pr-at-point)
    ("-" "Hide (dismiss)" orchard-hide-at-point)
    ("H" "Show hidden" orchard-show-hidden)
    ("a" "Archive (rm worktree)" orchard-archive-at-point)
    ("X" "Delete (rm branch too)" orchard-delete-at-point)]
   ["Services"
    ("V" "Vercel" vercel-transient)
    ("S" "Supabase" supabase-transient)
    ("A" "AWS" aws-transient)]
   ["Maintenance"
    ("K" "Cleanup stale entries" orchard-cleanup)
    ("M" "Cleanup completed (issues+PRs)" orchard-cleanup-merged)
    ("!" "Toggle window dedication" orchard-toggle-window-dedication)
    ("g" "Refresh" orchard-refresh)
    ("G" "Force refresh (ignore cache)" orchard-force-refresh)
    ("q" "Quit" transient-quit-one)
    ("Q" "Quit + kill Claudes" orchard-quit-all)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Global Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Global (assuming ashton-mode-map exists)
(when (boundp 'ashton-mode-map)
  (define-key ashton-mode-map (kbd "C-c O O") #'orchard)
  (define-key ashton-mode-map (kbd "C-c O ?") #'orchard-dispatch)
  ;; Create branches (uppercase, unique first char)
  (define-key ashton-mode-map (kbd "C-c O F") #'orchard-new-feature)
  (define-key ashton-mode-map (kbd "C-c O B") #'orchard-new-bugfix)
  (define-key ashton-mode-map (kbd "C-c O C") #'orchard-new-chore)
  (define-key ashton-mode-map (kbd "C-c O R") #'orchard-new-refactor)
  (define-key ashton-mode-map (kbd "C-c O D") #'orchard-new-docs)
  (define-key ashton-mode-map (kbd "C-c O E") #'orchard-new-experiment)
  ;; Legacy aliases (lowercase)
  (define-key ashton-mode-map (kbd "C-c O f") #'orchard-new-feature)
  (define-key ashton-mode-map (kbd "C-c O n") #'orchard-new-feature)
  (define-key ashton-mode-map (kbd "C-c O x") #'orchard-new-bugfix)
  (define-key ashton-mode-map (kbd "C-c O h") #'orchard-new-chore)
  ;; Claude management
  (define-key ashton-mode-map (kbd "C-c O l") #'orchard-list-claudes)
  ;; GitHub Issues
  (define-key ashton-mode-map (kbd "C-c O I") #'orchard-issue-start-prompt)
  ;; M-m cycles magit/claude everywhere
  (define-key ashton-mode-map (kbd "M-m") #'orchard-cycle-mode))

;; For magit-mode: bind M-m after magit loads
(with-eval-after-load 'magit
  ;; Unbind M-m first if it's bound to something else
  (define-key magit-mode-map (kbd "M-m") nil)
  (define-key magit-mode-map (kbd "M-m") #'orchard-cycle-mode)

  ;; Bind in all sub-mode maps
  (dolist (map (list magit-mode-map
                     magit-status-mode-map
                     magit-log-mode-map
                     magit-diff-mode-map
                     magit-revision-mode-map
                     magit-stash-mode-map
                     magit-refs-mode-map
                     magit-cherry-mode-map))
    (when (and map (keymapp map))
      (define-key map (kbd "M-m") #'orchard-cycle-mode))))

;; Hook for buffer-local override (runs late)
(defun orchard--bind-m-m-in-magit ()
  "Bind M-m to orchard-cycle-mode in magit buffers."
  (local-set-key (kbd "M-m") #'orchard-cycle-mode))

(add-hook 'magit-mode-hook #'orchard--bind-m-m-in-magit 100)

(provide 'orchard)
;;; orchard.el ends here
