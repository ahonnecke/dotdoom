;;; orchard-actions.el --- Orchard dashboard actions and commands -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains dashboard actions and commands:
;; - Open/magit/claude/dired at point
;; - Hide/show management
;; - View presets and section collapsing
;; - Push/PR/Archive/Delete actions
;; - Port allocation
;; - Cleanup functions
;; - Branch creation
;; - Issue-based branch start

(require 'orchard-vars)
(require 'orchard-cache)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard-refresh "orchard-dashboard")
(declare-function orchard--get-worktree-at-point "orchard-dashboard")
(declare-function orchard--get-issue-at-point "orchard-dashboard")
(declare-function orchard--get-worktrees "orchard-worktree")
(declare-function orchard--get-repo-root "orchard-worktree")
(declare-function orchard--detect-stage "orchard-worktree")
(declare-function orchard--get-prunable-worktrees "orchard-worktree")
(declare-function orchard--save-feature-description "orchard-worktree")
(declare-function orchard--claude-buffer-for-path "orchard-claude")
(declare-function orchard--get-claude-buffers "orchard-claude")
(declare-function orchard--start-claude-with-resume "orchard-claude")
(declare-function orchard--start-claude-with-command "orchard-claude")
(declare-function orchard--setup-claude-settings "orchard-claude")
(declare-function orchard--column-for-branch "orchard-window")
(declare-function orchard-open-branch "orchard")
(declare-function orchard--find-worktree-for-issue "orchard")
(declare-function orchard--is-main-worktree-p "orchard")
(declare-function orchard--current-worktree "orchard")
(declare-function orchard--format-section-header "orchard-dashboard")
(declare-function orchard--branch-icon "orchard-dashboard")
(declare-function orchard--branch-face "orchard-dashboard")
(declare-function ghq--get-worktree-port "config-ghq")
(declare-function ghq--allocate-port-for-path "config-ghq")
(declare-function ghq--generate-workspace-env "config-ghq")
(declare-function ghq--port-slots-status "config-ghq")
(declare-function ghq--unregister-worktree "config-ghq")
(declare-function ghq--free-worktree-port "config-ghq")
(declare-function ghq--load-port-registry "config-ghq")
(declare-function ghq--save-port-registry "config-ghq")
(declare-function testicular-start "config-testicular")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Actions - Open/Navigate
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-open-at-point ()
  "Open item at point with Claude and auto-resume.
If on a worktree, open Claude for it.
If on an issue, start a branch (or jump to existing) and open Claude."
  (interactive)
  (cond
   ((orchard--get-worktree-at-point)
    (let* ((wt (orchard--get-worktree-at-point))
           (path (alist-get 'path wt))
           (branch (alist-get 'branch wt)))
      (message "Opening Claude for %s..." branch)
      (orchard--start-claude-with-resume path)))
   ((orchard--get-issue-at-point)
    (orchard-issue-start))
   (t
    (user-error "No worktree or issue at point"))))

(defun orchard-magit-at-point ()
  "Open magit for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (orchard-open-branch wt)
    (user-error "No worktree at point")))

(defun orchard-claude-at-point ()
  "Open Claude for worktree at point with auto-resume.
If on an issue line, find the associated worktree.
Preserves existing Claude buffers, keeps Orchard on far left."
  (interactive)
  (let ((wt (or (orchard--get-worktree-at-point)
                ;; Fall back to finding worktree for issue at point
                (when-let ((issue (orchard--get-issue-at-point)))
                  (orchard--find-worktree-for-issue (alist-get 'number issue))))))
    (if wt
        (let* ((path (alist-get 'path wt))
               (branch (alist-get 'branch wt)))
          (message "Opening Claude for %s..." branch)
          (orchard--start-claude-with-resume path))
      (user-error "No worktree at point (or issue has no worktree)"))))

(defun orchard-dired-at-point ()
  "Open dired for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (dired (alist-get 'path wt))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hide/Show Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-hide-at-point ()
  "Hide the item at point from the dashboard.
Hides the issue (by number) or orphan worktree (by path).
Hidden items are persisted and can be shown with `orchard-show-hidden'."
  (interactive)
  (cond
   ;; If on an issue, hide by issue number
   ((orchard--get-issue-at-point)
    (let* ((issue (orchard--get-issue-at-point))
           (number (alist-get 'number issue))
           (title (alist-get 'title issue)))
      (when (y-or-n-p (format "Hide issue #%d: %s? " number
                              (truncate-string-to-width (or title "") 40 nil nil "...")))
        (orchard--hide-issue number)
        (message "Hidden issue #%d (press H to show hidden)" number)
        (orchard-refresh))))
   ;; If on an orphan worktree (no issue), hide by path
   ((orchard--get-worktree-at-point)
    (let* ((wt (orchard--get-worktree-at-point))
           (path (alist-get 'path wt))
           (branch (alist-get 'branch wt)))
      (when (y-or-n-p (format "Hide branch %s? " branch))
        (orchard--hide-worktree path)
        (message "Hidden %s (press H to show hidden)" branch)
        (orchard-refresh))))
   (t
    (user-error "No item at point to hide"))))

(defun orchard-show-hidden ()
  "Show and manage hidden items.
Displays a list of hidden issues and worktrees with option to unhide."
  (interactive)
  (let* ((hidden-issues (orchard--get-hidden-issues))
         (hidden-worktrees (orchard--get-hidden))
         (all-hidden (append
                      (mapcar (lambda (n) (cons 'issue n)) hidden-issues)
                      (mapcar (lambda (p) (cons 'worktree p)) hidden-worktrees))))
    (if (null all-hidden)
        (message "No hidden items")
      (let* ((choices (mapcar
                       (lambda (item)
                         (if (eq (car item) 'issue)
                             (format "Issue #%d" (cdr item))
                           (format "Branch: %s"
                                   (file-name-nondirectory
                                    (directory-file-name (cdr item))))))
                       all-hidden))
             (selected (completing-read-multiple
                        "Unhide (comma-separated, or C-g to cancel): "
                        choices nil t)))
        (dolist (sel selected)
          (let ((idx (cl-position sel choices :test #'string=)))
            (when idx
              (let ((item (nth idx all-hidden)))
                (if (eq (car item) 'issue)
                    (progn
                      (orchard--unhide-issue (cdr item))
                      (message "Unhid issue #%d" (cdr item)))
                  (orchard--unhide-worktree (cdr item))
                  (message "Unhid %s" (file-name-nondirectory
                                       (directory-file-name (cdr item)))))))))
        (orchard-refresh)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; View Presets
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-set-view (view)
  "Set the dashboard VIEW preset."
  (setq orchard--current-view view)
  (message "View: %s" (symbol-name view))
  (orchard-refresh))

(defun orchard-view-working ()
  "Show working view: UP NEXT and IN PROGRESS only."
  (interactive)
  (orchard-set-view 'working))

(defun orchard-view-all ()
  "Show all sections."
  (interactive)
  (orchard-set-view 'all))

(defun orchard-view-next ()
  "Show only UP NEXT section."
  (interactive)
  (orchard-set-view 'next))

(defun orchard-view-qa ()
  "Show only QA/VERIFY section."
  (interactive)
  (orchard-set-view 'qa))

(defun orchard-view-progress ()
  "Show only IN PROGRESS section."
  (interactive)
  (orchard-set-view 'progress))

(defun orchard-view-recent ()
  "Show items with recent Claude sessions, sorted by activity.
Useful for finding what you were working on after restarting Emacs."
  (interactive)
  (orchard-set-view 'recent))

(defun orchard--get-sessions-with-worktrees ()
  "Get all worktrees with Claude sessions, sorted by recency.
Returns list of (worktree . session-info) pairs."
  (orchard--ensure-claude-sessions-cache)
  (let ((worktrees (orchard--get-worktrees t)))  ; include hidden
    (cl-loop for wt in worktrees
             for path = (alist-get 'path wt)
             for session = (cdr (assoc path orchard--claude-sessions-cache))
             when session
             collect (cons wt session) into result
             finally return
             (sort result
                   (lambda (a b)
                     (let ((time-a (plist-get (cdr a) :modified))
                           (time-b (plist-get (cdr b) :modified)))
                       (when (and time-a time-b)
                         (string> time-a time-b))))))))

(defun orchard--format-recent-sessions ()
  "Format recent Claude sessions section for dashboard."
  (let* ((session-items (orchard--get-sessions-with-worktrees))
         (count (length session-items)))
    (if (= count 0)
        (propertize "  No Claude sessions found\n\n"
                    'face 'font-lock-comment-face)
      (concat
       (orchard--format-section-header "RECENT SESSIONS" count "by activity" 'recent-sessions)
       (unless (orchard--section-collapsed-p 'recent-sessions)
         (let ((current (orchard--current-worktree))
               (current-path (when current (alist-get 'path current))))
           (mapconcat
            (lambda (item)
              (let* ((wt (car item))
                     (session (cdr item))
                     (path (alist-get 'path wt))
                     (branch (or (alist-get 'branch wt) "(detached)"))
                     (issue-num (orchard--get-worktree-issue nil branch))
                     (msg-count (plist-get session :message-count))
                     (modified (plist-get session :modified))
                     (summary (plist-get session :summary))
                     (rel-time (orchard--format-relative-time modified))
                     (is-current (and current-path (string= path current-path)))
                     (icon (orchard--branch-icon branch)))
                (propertize
                 (concat
                  "   " icon " "
                  (propertize (format "💾%d" msg-count) 'face '(:foreground "#61AFEF"))
                  " "
                  (propertize (or rel-time "?") 'face '(:foreground "#E5C07B"))
                  " "
                  (if issue-num
                      (propertize (format "#%d" issue-num) 'face 'orchard-issue)
                    "")
                  " "
                  (propertize (truncate-string-to-width branch 28 nil nil "…")
                              'face (if is-current 'orchard-current
                                      (orchard--branch-face branch)))
                  (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
                  "\n"
                  (when summary
                    (concat "      "
                            (propertize (truncate-string-to-width summary 60 nil nil "…")
                                        'face 'font-lock-comment-face)
                            "\n")))
                 'orchard-worktree wt)))
            session-items "")))))))

(defun orchard--section-visible-p (section)
  "Return t if SECTION should be visible based on current view."
  (pcase orchard--current-view
    ('all t)
    ('working (memq section '(up-next in-progress unlinked)))
    ('next (eq section 'up-next))
    ('qa (eq section 'qa-verify))
    ('progress (eq section 'in-progress))
    ('recent (eq section 'recent-sessions))
    (_ t)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Section Collapsing
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--section-collapsed-p (section)
  "Return t if SECTION is collapsed."
  (member section orchard--collapsed-sections))

(defun orchard--toggle-section-collapsed (section)
  "Toggle collapsed state of SECTION."
  (if (orchard--section-collapsed-p section)
      (setq orchard--collapsed-sections (delete section orchard--collapsed-sections))
    (push section orchard--collapsed-sections)))

(defun orchard-toggle-section ()
  "Toggle collapse state of section at point."
  (interactive)
  (let ((section (get-text-property (point) 'orchard-section)))
    (if section
        (progn
          (orchard--toggle-section-collapsed section)
          (orchard-refresh))
      (user-error "Not on a section header"))))

(defun orchard-expand-all-sections ()
  "Expand all collapsed sections."
  (interactive)
  (setq orchard--collapsed-sections nil)
  (orchard-refresh))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Filter Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-filter-by-text ()
  "Filter issues/branches by free text search.
Shows only items where title or branch name contains the search string."
  (interactive)
  (let ((text (read-string "Search: " orchard--text-filter)))
    (if (string-empty-p text)
        (progn
          (setq orchard--text-filter nil)
          (message "Text filter cleared"))
      (setq orchard--text-filter text)
      (message "Filtering by: %s" text))
    (orchard-refresh)))

(defun orchard-filter-by-label ()
  "Filter issues by label.
Prompts for a label from the available labels in cached issues."
  (interactive)
  (let ((labels (orchard--get-all-labels)))
    (if (null labels)
        (message "No labels found in cached issues")
      (let ((label (completing-read "Filter by label: " labels nil t)))
        (setq orchard--label-filter label)
        (message "Filtering by label: %s" label)
        (orchard-refresh)))))

(defun orchard-clear-filters ()
  "Clear all filters (text and label)."
  (interactive)
  (setq orchard--label-filter nil
        orchard--text-filter nil)
  (message "All filters cleared")
  (orchard-refresh))

(defun orchard-toggle-staging-issues ()
  "Toggle visibility of issues with 'staging' label."
  (interactive)
  (setq orchard--hide-staging-issues (not orchard--hide-staging-issues))
  (message "Staging issues: %s" (if orchard--hide-staging-issues "hidden" "visible"))
  (orchard-refresh))

(defun orchard-expand-all-sections ()
  "Expand all collapsed sections."
  (interactive)
  (setq orchard--collapsed-sections nil)
  (orchard-refresh))

;;; ════════════════════════════════════════════════════════════════════════════
;;; PR-Ready Stage Management
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-mark-pr-ready ()
  "Mark worktree at point as PR-ready (pushed, CI passed, ready to create PR).
Toggle off if already marked. Automatically clears when PR is created."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (current-stage (orchard--get-stage-override path)))
        (if (eq current-stage 'pr-ready)
            (progn
              (orchard--set-stage-override path nil)
              (message "Cleared PR-ready status"))
          (orchard--set-stage-override path 'pr-ready)
          (message "Marked as PR-ready 🚀"))
        (orchard-refresh))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Filter Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix orchard-filter-menu ()
  "Filter and view options for Orchard dashboard."
  ["View Presets"
   ("w" "Working (next + progress)" orchard-view-working)
   ("a" "All sections" orchard-view-all)
   ("n" "Next only" orchard-view-next)
   ("p" "In Progress only" orchard-view-progress)
   ("q" "QA/Verify only" orchard-view-qa)
   ("r" "Recent sessions" orchard-view-recent)]
  ["Filters"
   ("/" "Search (free text)" orchard-filter-by-text)
   ("L" "Filter by label" orchard-filter-by-label)
   ("\\" "Clear all filters" orchard-clear-filters)
   ("s" "Toggle staging" orchard-toggle-staging-issues)]
  ["Hidden"
   ("-" "Hide at point" orchard-hide-at-point)
   ("H" "Show/unhide items" orchard-show-hidden)]
  ["Sections"
   ("TAB" "Toggle section" orchard-toggle-section)
   ("E" "Expand all" orchard-expand-all-sections)])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Push/PR/Archive/Delete Actions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-push-at-point ()
  "Push branch at point to origin."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (default-directory path))
        (when (yes-or-no-p (format "Push %s to origin? " branch))
          (shell-command (format "git push -u origin %s" (shell-quote-argument branch)))
          (message "Pushed %s" branch)
          (orchard-refresh)))
    (user-error "No worktree at point")))

(defun orchard--generate-pr-title (branch description issue-num issue-title)
  "Generate PR title from BRANCH, DESCRIPTION, ISSUE-NUM, and ISSUE-TITLE.
Prefers: description > issue title > cleaned branch name."
  (or description
      (when issue-title (format "#%d: %s" issue-num issue-title))
      (replace-regexp-in-string "^[A-Z]+/[0-9]*-?" "" branch)))

(defun orchard--generate-pr-body (description issue-num issue-title)
  "Generate PR body from DESCRIPTION, ISSUE-NUM, and ISSUE-TITLE."
  (let ((summary (or description ""))
        (closes (if issue-num (format "Closes #%d" issue-num) "")))
    (string-trim
     (format "## Summary\n%s\n\n%s\n\n## Test Plan\n- [ ] Tests pass\n- [ ] Manual verification\n"
             summary closes))))

(defun orchard-pr-at-point ()
  "Create PR for branch at point.
Auto-populates title and body from worktree description and linked issue."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (description (alist-get 'description wt))
             (issue-num (orchard--get-worktree-issue path branch))
             (issue (when issue-num (orchard--get-issue-by-number issue-num)))
             (issue-title (when issue (alist-get 'title issue)))
             (default-directory path))
        ;; Check if PR already exists
        (let ((existing-pr (string-trim
                            (shell-command-to-string
                             (format "gh pr view %s --json url -q .url 2>/dev/null || true"
                                     (shell-quote-argument branch))))))
          (when (and existing-pr (not (string-empty-p existing-pr)))
            (if (y-or-n-p (format "PR already exists at %s. Open in browser? " existing-pr))
                (browse-url existing-pr))
            (user-error "PR already exists")))
        ;; Push first
        (message "Pushing %s..." branch)
        (shell-command (format "git push -u origin %s" (shell-quote-argument branch)))
        ;; Generate defaults
        (let* ((default-title (orchard--generate-pr-title branch description issue-num issue-title))
               (default-body (orchard--generate-pr-body description issue-num issue-title))
               (title (read-string "PR Title: " default-title))
               (body (read-string "PR Body (edit in browser for full): " "")))
          (when (string-empty-p body)
            (setq body default-body))
          (let ((result (shell-command-to-string
                         (format "gh pr create --title %s --body %s --base dev 2>&1"
                                 (shell-quote-argument title)
                                 (shell-quote-argument body)))))
            (message "%s" (string-trim result))
            ;; Extract PR URL from result and save to file for auto-detection
            (when (string-match "https://github.com/[^\n]+" result)
              (let ((pr-url (match-string 0 result)))
                (with-temp-file (expand-file-name ".pr-url" path)
                  (insert pr-url))
                (message "PR created: %s" pr-url)))
            (orchard-refresh))))
    (user-error "No worktree at point")))

(defun orchard-archive-at-point ()
  "Archive worktree at point (remove worktree, keep branch, free port).
For main worktree, just hides it instead."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (repo-root (orchard--get-repo-root)))
        ;; Check if this is the main worktree
        (if (orchard--is-main-worktree-p path)
            ;; Can't remove main worktree - just hide it
            (progn
              (orchard--hide-worktree path)
              (when-let ((col (orchard--column-for-branch branch)))
                (remhash branch orchard--branch-to-column))
              (orchard-refresh)
              (message "Hidden %s (main worktree cannot be removed)" branch))
          ;; Regular worktree - archive it
          (when (yes-or-no-p (format "Archive %s? (removes worktree, keeps branch) " branch))
            ;; Kill Claude buffer if any
            (when-let ((claude-buf (orchard--claude-buffer-for-path path)))
              (kill-buffer claude-buf))
            ;; Remove from column tracking
            (when-let ((col (orchard--column-for-branch branch)))
              (remhash branch orchard--branch-to-column))
            ;; Remove worktree (synchronous)
            (let ((default-directory repo-root))
              (let ((result (shell-command-to-string
                             (format "git worktree remove --force %s 2>&1"
                                     (shell-quote-argument path)))))
                (when (string-match-p "error\\|fatal" result)
                  (user-error "Failed to archive: %s" (string-trim result)))))
            ;; Unregister port
            (when (fboundp 'ghq--unregister-worktree)
              (ghq--unregister-worktree path))
            ;; Prune stale worktree references
            (let ((default-directory repo-root))
              (shell-command-to-string "git worktree prune"))
            (orchard-refresh)
            (message "Archived %s (branch preserved)" branch))))
    (user-error "No worktree at point")))

(defun orchard-delete-at-point ()
  "Delete worktree AND branch at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (repo-root (orchard--get-repo-root)))
        (when (yes-or-no-p (format "DELETE %s? (removes worktree AND branch) " branch))
          ;; Kill Claude buffer if any
          (when-let ((claude-buf (orchard--claude-buffer-for-path path)))
            (kill-buffer claude-buf))
          ;; Remove from column tracking
          (when-let ((col (orchard--column-for-branch branch)))
            (remhash branch orchard--branch-to-column))
          ;; Remove worktree (synchronous)
          (let ((default-directory repo-root))
            (let ((result (shell-command-to-string
                           (format "git worktree remove --force %s 2>&1"
                                   (shell-quote-argument path)))))
              (when (string-match-p "error\\|fatal" result)
                (message "Warning removing worktree: %s" (string-trim result)))))
          ;; Delete branch (unless it's a protected branch)
          (when (and branch (not (member branch '("dev" "main" "master"))))
            (let ((default-directory repo-root))
              (shell-command-to-string
               (format "git branch -D %s 2>&1" (shell-quote-argument branch)))))
          ;; Unregister port
          (when (fboundp 'ghq--unregister-worktree)
            (ghq--unregister-worktree path))
          ;; Prune stale worktree references
          (let ((default-directory repo-root))
            (shell-command-to-string "git worktree prune"))
          (orchard-refresh)
          (message "Deleted %s (worktree and branch)" branch)))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Port Allocation
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-allocate-port ()
  "Allocate a port for the worktree at point.
Needed for running dev server (make dev). Most work doesn't need a port."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (existing-port (when (fboundp 'ghq--get-worktree-port)
                              (ghq--get-worktree-port path))))
        (if existing-port
            (message "Already has port :%d" (+ 3000 existing-port))
          ;; Try to allocate
          (if (fboundp 'ghq--allocate-port-for-path)
              (if-let ((port-num (ghq--allocate-port-for-path path)))
                  (progn
                    ;; Generate .env.workspace
                    (when (fboundp 'ghq--generate-workspace-env)
                      (ghq--generate-workspace-env path port-num))
                    (orchard-refresh)
                    (message "✓ Allocated port :%d for %s" (+ 3000 port-num) branch))
                ;; Failed to allocate - suggest cleanup
                (let ((status (ghq--port-slots-status)))
                  (user-error "No available ports (%d/%d used). Use 'R' to release or clean up old worktrees"
                              (car status) (cdr status))))
            (user-error "Port allocation not available (ghq not configured)"))))
    (user-error "No worktree at point")))

(defun orchard-release-port ()
  "Release the port allocated to the worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (existing-port (when (fboundp 'ghq--get-worktree-port)
                              (ghq--get-worktree-port path))))
        (if existing-port
            (progn
              (when (fboundp 'ghq--unregister-worktree)
                (ghq--unregister-worktree path))
              ;; Remove .env.workspace
              (let ((env-file (expand-file-name ".env.workspace" path)))
                (when (file-exists-p env-file)
                  (delete-file env-file)))
              (orchard-refresh)
              (message "✓ Released port :%d from %s" (+ 3000 existing-port) branch))
          (message "No port allocated for %s" branch)))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Window Utilities
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--undedicate-all-columns ()
  "Remove dedication from all branch columns.
Useful when exiting orchard workflow."
  (dolist (win (window-list nil 'no-mini))
    (set-window-dedicated-p win nil)))

(defun orchard-quit-all ()
  "Quit and optionally kill all Claude instances.
Also undedicates all windows."
  (interactive)
  (let ((claudes (orchard--get-claude-buffers)))
    (when (and claudes
               (yes-or-no-p (format "Kill %d Claude instance(s)? " (length claudes))))
      (dolist (buf claudes)
        (kill-buffer buf))))
  (orchard--undedicate-all-columns)
  (quit-window))

(defun orchard-toggle-window-dedication ()
  "Toggle dedication of current window (for debugging)."
  (interactive)
  (let* ((win (selected-window))
         (dedicated (window-dedicated-p win)))
    (set-window-dedicated-p win (not dedicated))
    (message "Window %s now %s"
             (window-buffer win)
             (if (window-dedicated-p win) "DEDICATED" "undedicated"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Cleanup Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-stale-port-entries ()
  "Get list of port registry entries whose paths no longer exist.
Returns list of (path . port) pairs."
  (when (fboundp 'ghq--load-port-registry)
    (let ((registry (ghq--load-port-registry)))
      (cl-remove-if (lambda (entry)
                      (file-directory-p (car entry)))
                    registry))))

(defun orchard--cleanup-port-registry ()
  "Remove stale entries from port registry.
Returns number of entries removed."
  (if (fboundp 'ghq--load-port-registry)
      (let* ((registry (ghq--load-port-registry))
             (valid (cl-remove-if-not (lambda (entry)
                                        (file-directory-p (car entry)))
                                      registry))
             (removed-count (- (length registry) (length valid))))
        (when (> removed-count 0)
          (ghq--save-port-registry valid))
        removed-count)
    0))

(defun orchard--prune-git-worktrees ()
  "Prune stale git worktree entries.
Returns output from git worktree prune."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (string-trim (shell-command-to-string "git worktree prune 2>&1"))))))

(defun orchard--refresh-if-visible ()
  "Refresh orchard dashboard if it's visible."
  (when-let ((buf (get-buffer "*Orchard*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (orchard-refresh)))))

(defun orchard-cleanup (&optional silent)
  "Clean up stale worktree entries and port allocations.
Removes:
  - Git worktree entries pointing to non-existent directories
  - Port registry entries for missing worktrees
When called interactively, shows what will be cleaned and asks for confirmation.
With SILENT non-nil, performs cleanup without prompting."
  (interactive)
  (let* ((prunable-worktrees (orchard--get-prunable-worktrees))
         (stale-ports (orchard--get-stale-port-entries))
         (has-cleanup (or prunable-worktrees stale-ports)))
    (if (not has-cleanup)
        (unless silent
          (message "Nothing to clean up - all worktrees and ports are valid"))
      ;; Show what we'll clean
      (let ((msg (concat
                  (when prunable-worktrees
                    (format "Prunable worktrees (%d):\n  %s\n"
                            (length prunable-worktrees)
                            (mapconcat #'identity prunable-worktrees "\n  ")))
                  (when stale-ports
                    (format "Stale port entries (%d):\n  %s\n"
                            (length stale-ports)
                            (mapconcat (lambda (e)
                                         (format "%s (port %d)"
                                                 (file-name-nondirectory
                                                  (directory-file-name (car e)))
                                                 (cdr e)))
                                       stale-ports "\n  "))))))
        (when (or silent (yes-or-no-p (format "Clean up?\n%s" msg)))
          ;; Prune git worktrees
          (when prunable-worktrees
            (orchard--prune-git-worktrees))
          ;; Clean port registry
          (let ((removed-ports (orchard--cleanup-port-registry)))
            (unless silent
              (message "Cleaned up: %d worktree(s), %d port(s)"
                       (length prunable-worktrees)
                       removed-ports)))
          ;; Refresh if in orchard buffer
          (orchard--refresh-if-visible))))))

(defun orchard-cleanup-dry-run ()
  "Show what would be cleaned up without actually doing it."
  (interactive)
  (let* ((prunable-worktrees (orchard--get-prunable-worktrees))
         (stale-ports (orchard--get-stale-port-entries)))
    (if (not (or prunable-worktrees stale-ports))
        (message "Nothing to clean up - all worktrees and ports are valid")
      (with-output-to-temp-buffer "*Orchard Cleanup*"
        (princ "=== Orchard Cleanup Preview ===\n\n")
        (if prunable-worktrees
            (progn
              (princ (format "Prunable Git Worktrees (%d):\n" (length prunable-worktrees)))
              (dolist (path prunable-worktrees)
                (princ (format "  - %s\n" path))))
          (princ "No prunable git worktrees.\n"))
        (princ "\n")
        (if stale-ports
            (progn
              (princ (format "Stale Port Registry Entries (%d):\n" (length stale-ports)))
              (dolist (entry stale-ports)
                (princ (format "  - %s (port slot %d → :%d)\n"
                               (file-name-nondirectory (directory-file-name (car entry)))
                               (cdr entry)
                               (+ 3000 (cdr entry))))))
          (princ "No stale port entries.\n"))
        (princ "\nRun `orchard-cleanup' (or press 'C' in Orchard) to clean these up.\n")))))

(defun orchard--worktree-archivable-p (wt)
  "Return t if worktree WT should be archived.
A worktree is archivable if:
  - Has linked issue AND issue is closed, OR
  - Has no linked issue AND branch PR is merged
Never archives main/dev branches."
  (let* ((path (alist-get 'path wt))
         (branch (alist-get 'branch wt))
         (issue-num (orchard--get-worktree-issue path branch)))
    (and branch
         (not (member branch '("dev" "main" "master")))
         (if issue-num
             ;; Has linked issue - archivable when issue is closed
             (orchard--issue-closed-p issue-num)
           ;; No linked issue - archivable when PR is merged
           (orchard--branch-merged-p branch)))))

(defun orchard--get-archivable-worktrees ()
  "Get list of worktrees ready to archive.
Includes worktrees where:
  - Linked issue is closed, OR
  - PR is merged (for unlinked branches)"
  (orchard--ensure-merged-cache)
  (orchard--ensure-closed-issues-cache)
  (let ((worktrees (orchard--get-worktrees)))
    (cl-remove-if-not #'orchard--worktree-archivable-p worktrees)))

(defun orchard--get-merged-worktrees ()
  "Get list of worktrees whose branches have been merged.
Returns list of worktree alists with 'merged stage.
NOTE: Use `orchard--get-archivable-worktrees' for issue-aware cleanup."
  (orchard--ensure-merged-cache)
  (let ((worktrees (orchard--get-worktrees)))
    (cl-remove-if-not
     (lambda (wt)
       (let ((branch (alist-get 'branch wt)))
         (and branch
              (not (string= branch "dev"))
              (not (string= branch "main"))
              (orchard--branch-merged-p branch))))
     worktrees)))

(defun orchard-cleanup-merged (&optional silent)
  "Archive completed worktrees (merged PRs or closed issues).
Uses issue-aware logic: worktrees with linked issues are archived
when the issue is closed, not just when the PR is merged.
With SILENT non-nil, archives without prompting."
  (interactive)
  ;; Force cache refresh for accurate results
  (orchard--refresh-merged-cache)
  (orchard--refresh-closed-issues-cache)
  (let ((archivable (orchard--get-archivable-worktrees)))
    (if (null archivable)
        (unless silent
          (message "No completed worktrees to archive"))
      (let ((msg (format "Completed worktrees to archive (%d):\n  %s\n\nThis will:\n  - Remove the worktree directory\n  - Free the allocated port\n  - Keep the git branch (for reference)\n"
                         (length archivable)
                         (mapconcat (lambda (wt)
                                      (let* ((path (alist-get 'path wt))
                                             (branch (alist-get 'branch wt))
                                             (issue-num (orchard--get-worktree-issue path branch)))
                                        (if issue-num
                                            (format "%s (%s) - issue #%d closed"
                                                    (file-name-nondirectory
                                                     (directory-file-name path))
                                                    branch
                                                    issue-num)
                                          (format "%s (%s) - PR merged"
                                                  (file-name-nondirectory
                                                   (directory-file-name path))
                                                  branch))))
                                    archivable "\n  "))))
        (when (or silent (yes-or-no-p (format "%s\nArchive these worktrees? " msg)))
          (let ((archived 0)
                (repo-root (orchard--get-repo-root)))
            (dolist (wt archivable)
              (let* ((path (alist-get 'path wt))
                     (branch (alist-get 'branch wt)))
                (message "Archiving %s..." branch)
                ;; Free port
                (when (fboundp 'ghq--free-worktree-port)
                  (ghq--free-worktree-port path))
                ;; Remove worktree
                (when repo-root
                  (let ((default-directory repo-root))
                    (shell-command-to-string
                     (format "git worktree remove --force %s 2>/dev/null"
                             (shell-quote-argument path))))
                  (cl-incf archived))))
            ;; Prune stale references
            (when repo-root
              (let ((default-directory repo-root))
                (shell-command-to-string "git worktree prune 2>/dev/null")))
            (message "Archived %d completed worktree(s)" archived)
            (orchard--refresh-if-visible)))))))

(defun orchard-cleanup-merged-dry-run ()
  "Show completed worktrees that would be archived.
Uses issue-aware logic (closed issues or merged PRs)."
  (interactive)
  (orchard--refresh-merged-cache)
  (orchard--refresh-closed-issues-cache)
  (let ((archivable (orchard--get-archivable-worktrees)))
    (if (null archivable)
        (message "No completed worktrees found")
      (with-output-to-temp-buffer "*Orchard Completed*"
        (princ "=== Completed Worktrees (ready to archive) ===\n\n")
        (dolist (wt archivable)
          (let* ((branch (alist-get 'branch wt))
                 (path (alist-get 'path wt))
                 (issue-num (orchard--get-worktree-issue path branch))
                 (merge-time (orchard--branch-merged-p branch))
                 (close-time (when issue-num (orchard--issue-closed-p issue-num))))
            (princ (format "  %s\n    Branch: %s\n"
                           (file-name-nondirectory (directory-file-name path))
                           branch))
            (when issue-num
              (princ (format "    Issue: #%d (closed: %s)\n"
                             issue-num (or close-time "yes"))))
            (when (and (not issue-num) merge-time)
              (princ (format "    Merged: %s\n" merge-time)))
            (princ (format "    Path: %s\n\n" path))))
        (princ (format "\nTotal: %d worktree(s)\n" (length archivable)))
        (princ "\nRun `orchard-cleanup-merged' (or press 'M' in Orchard) to archive.\n")))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Testing Support
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-test-at-point ()
  "Start testicular (manual testing) for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let ((default-directory (alist-get 'path wt)))
        (if (fboundp 'testicular-start)
            (testicular-start)
          (user-error "testicular not loaded - check config-testicular.el")))
    (user-error "No worktree at point")))

(defun orchard--get-test-results (path)
  "Get test results for worktree at PATH.
Returns plist with :complete and :failed keys, or nil."
  ;; Placeholder - integrate with testicular
  nil)

(defun orchard-next-step ()
  "Advance worktree at point to the next stage in the lifecycle.
Based on current stage, performs the appropriate action:
  requirements  → Start working (open Claude)
  in-progress   → Generate test plan (tell user to run /test-plan)
  ready-to-test → Start testicular
  testing       → Mark tests complete (if all pass)
  ready-to-pr   → Create PR
  pr-open       → Check PR status
  merged        → Archive/delete worktree"
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (stage (orchard--detect-stage path branch)))
        (pcase stage
          ('requirements
           ;; Move to in-progress: open Claude to start working
           (message "Starting work on %s..." branch)
           (orchard-claude-at-point))

          ('in-progress
           ;; Need to generate test plan
           (message "Generate test plan: In Claude, run /test-plan to create .test-plan.md")
           (orchard-claude-at-point))

          ('ready-to-test
           ;; Start testicular
           (message "Starting manual testing...")
           (orchard-test-at-point))

          ('testing
           ;; Check test results
           (let ((results (orchard--get-test-results path)))
             (if results
                 (if (and (plist-get results :complete)
                          (zerop (plist-get results :failed)))
                     (progn
                       (message "All tests passed! Use 'P' to create PR.")
                       (orchard-refresh))
                   (message "Tests incomplete or have failures. Continue testing with 't'."))
               (message "No test results yet. Use 't' to run tests."))))

          ('ready-to-pr
           ;; Create PR
           (when (yes-or-no-p (format "Create PR for %s? " branch))
             (orchard-pr-at-point)))

          ('pr-open
           ;; Check PR status
           (let* ((pr-url-file (expand-file-name ".pr-url" path))
                  (pr-url (when (file-exists-p pr-url-file)
                            (with-temp-buffer
                              (insert-file-contents pr-url-file)
                              (string-trim (buffer-string))))))
             (if pr-url
                 (progn
                   (message "PR: %s" pr-url)
                   (when (yes-or-no-p "Open PR in browser? ")
                     (browse-url pr-url)))
               (message "No PR URL found. Use 'P' to create PR."))))

          ('merged
           ;; Offer to delete
           (when (yes-or-no-p (format "Branch %s is merged. Delete worktree? " branch))
             (orchard-delete-at-point)))

          (_
           (message "Unknown stage: %s. Use '?' for help." stage))))
    (user-error "No worktree at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch Creation
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--normalize-branch-name (name)
  "Normalize NAME for branch names and filesystem."
  (let ((normalized name))
    (setq normalized (replace-regexp-in-string "[ _]+" "-" normalized))
    (setq normalized (replace-regexp-in-string "[^a-zA-Z0-9-]" "" normalized))
    (setq normalized (replace-regexp-in-string "-+" "-" normalized))
    (setq normalized (replace-regexp-in-string "^-+\\|-+$" "" normalized))
    (downcase normalized)))

(defun orchard--create-branch (type name description &optional issue-number)
  "Create new branch of TYPE with NAME and DESCRIPTION.
If ISSUE-NUMBER is provided, link the worktree to that GitHub issue."
  (let* ((repo-root (orchard--get-repo-root))
         (prefix (upcase type))
         (normalized-name (orchard--normalize-branch-name name))
         (full-branch (format "%s/%s" prefix normalized-name))
         (safe-branch (replace-regexp-in-string "/" "-" full-branch))
         (wt-prefix (or orchard-worktree-prefix
                        (file-name-nondirectory (directory-file-name repo-root))))
         ;; Nested: ~/src/project/FEATURE-name
         ;; Flat:   ~/src/project--FEATURE-name
         (worktree-path (if orchard-nested-worktrees
                            (expand-file-name
                             safe-branch
                             (expand-file-name wt-prefix orchard-worktree-parent))
                          (expand-file-name
                           (concat wt-prefix "--" safe-branch)
                           orchard-worktree-parent))))
    (unless repo-root
      (user-error "No repository root configured"))
    (when (file-exists-p worktree-path)
      (user-error "Worktree already exists: %s" worktree-path))
    ;; Ensure parent directory exists (for nested structure)
    (when orchard-nested-worktrees
      (let ((parent-dir (expand-file-name wt-prefix orchard-worktree-parent)))
        (unless (file-directory-p parent-dir)
          (make-directory parent-dir t))))
    ;; Fetch upstream
    (message "Fetching upstream...")
    (let ((default-directory repo-root))
      (shell-command "git fetch upstream 2>/dev/null"))
    ;; Check if branch already exists
    (let* ((default-directory repo-root)
           (branch-exists (zerop (call-process "git" nil nil nil
                                               "show-ref" "--verify" "--quiet"
                                               (concat "refs/heads/" full-branch))))
           (cmd (if branch-exists
                    (format "git worktree add %s %s 2>&1"
                            (shell-quote-argument worktree-path)
                            (shell-quote-argument full-branch))
                  (format "git worktree add -b %s %s %s 2>&1"
                          (shell-quote-argument full-branch)
                          (shell-quote-argument worktree-path)
                          orchard-upstream-branch)))
           (result (shell-command-to-string cmd)))
      (message "Creating worktree %s...%s" full-branch
               (if branch-exists " (using existing branch)" ""))
      ;; Check if worktree was created successfully
      (unless (file-directory-p worktree-path)
        (user-error "Failed to create worktree: %s" (string-trim result))))
    ;; Port allocation is now lazy - use P in dashboard to allocate when needed
    ;; Setup Claude settings
    (orchard--setup-claude-settings worktree-path)
    ;; Save description
    (when (and description (not (string-empty-p description)))
      (orchard--save-feature-description worktree-path description))
    ;; Save issue link if provided
    (when issue-number
      (orchard--save-worktree-issue worktree-path issue-number))
    ;; Run post-create hook
    (run-hook-with-args 'orchard-post-create-hook worktree-path)
    ;; Open Claude - use /issue-analyse for new issues, /resume otherwise
    (if issue-number
        (orchard--start-claude-with-command worktree-path "/issue-analyse")
      (orchard--start-claude-with-resume worktree-path))
    ;; Refresh dashboard
    (when-let ((buf (get-buffer "*Orchard*")))
      (with-current-buffer buf (orchard-refresh)))
    (message "✨ Created %s" full-branch)
    worktree-path))

(defun orchard-new-feature (name)
  "Create new FEATURE/NAME branch."
  (interactive "sFeature name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "FEATURE" name desc)))

(defun orchard-new-bugfix (name)
  "Create new BUGFIX/NAME branch."
  (interactive "sBugfix name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "BUGFIX" name desc)))

(defun orchard-new-chore (name)
  "Create new CHORE/NAME branch."
  (interactive "sChore name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "CHORE" name desc)))

(defun orchard-new-refactor (name)
  "Create new REFACTOR/NAME branch."
  (interactive "sRefactor name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "REFACTOR" name desc)))

(defun orchard-new-docs (name)
  "Create new DOCS/NAME branch."
  (interactive "sDocs name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "DOCS" name desc)))

(defun orchard-new-experiment (name)
  "Create new EXPERIMENT/NAME branch."
  (interactive "sExperiment name: ")
  (let ((desc (read-string "Description (optional): ")))
    (orchard--create-branch "EXPERIMENT" name desc)))

;; Legacy aliases
(defalias 'orchard-new-fix 'orchard-new-bugfix)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Start Branch from Issue
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--infer-branch-type-from-labels (labels)
  "Infer branch type from issue LABELS.
Returns branch type string like \"FEATURE\", \"BUGFIX\", etc."
  ;; Labels may be vector from JSON, convert to list for mapcar
  (let ((label-names (mapcar (lambda (l) (downcase (alist-get 'name l))) (append labels nil))))
    (cond
     ((cl-some (lambda (n) (string-match-p "bug\\|fix" n)) label-names) "BUGFIX")
     ((cl-some (lambda (n) (string-match-p "chore\\|maintenance" n)) label-names) "CHORE")
     ((cl-some (lambda (n) (string-match-p "doc" n)) label-names) "DOCS")
     ((cl-some (lambda (n) (string-match-p "refactor" n)) label-names) "REFACTOR")
     ((cl-some (lambda (n) (string-match-p "test" n)) label-names) "TEST")
     ((cl-some (lambda (n) (string-match-p "experiment\\|spike" n)) label-names) "EXPERIMENT")
     (t "FEATURE"))))

(defun orchard-issue-start ()
  "Start a new branch from the GitHub issue at point.
Auto-generates branch name from issue number and title.
Branch type is inferred from labels or prompts for selection.
Opens Claude with auto-resume after creation or when jumping to existing."
  (interactive)
  (if-let ((issue (orchard--get-issue-at-point)))
      (let* ((number (alist-get 'number issue))
             (title (alist-get 'title issue))
             (labels (alist-get 'labels issue))
             (worktrees (orchard--get-worktrees)))
        ;; Check if already has worktree FIRST
        (if (orchard--issue-has-worktree-p number worktrees)
            ;; Already has worktree - jump to it with Claude + resume
            (when-let ((wt (orchard--find-worktree-for-issue number)))
              (message "Opening Claude for #%d..." number)
              (orchard--start-claude-with-resume (alist-get 'path wt)))
          ;; No worktree - proceed with creation (which now opens Claude)
          (let* ((inferred-type (orchard--infer-branch-type-from-labels labels))
                 (branch-type (completing-read
                               (format "Branch type for #%d (default %s): " number inferred-type)
                               '("FEATURE" "BUGFIX" "CHORE" "REFACTOR" "DOCS" "EXPERIMENT" "TEST")
                               nil t nil nil inferred-type))
                 (branch-name (format "%d-%s"
                                      number
                                      (orchard--normalize-branch-name title)))
                 (description (format "#%d: %s" number title)))
            (message "Starting branch for issue #%d: %s..." number title)
            (orchard--create-branch branch-type branch-name description number)
            (message "Created %s/%s linked to issue #%d" branch-type branch-name number))))
    (user-error "No issue at point")))

(defun orchard-issue-start-prompt ()
  "Prompt for issue number and start a branch from it.
Use this when not in the orchard dashboard."
  (interactive)
  (let* ((issues (orchard--get-open-issues))
         (choices (mapcar (lambda (issue)
                            (cons (format "#%d: %s"
                                          (alist-get 'number issue)
                                          (alist-get 'title issue))
                                  issue))
                          issues))
         (selection (completing-read "Start branch from issue: " choices nil t))
         (issue (cdr (assoc selection choices))))
    (if issue
        (let ((orchard--temp-issue issue))
          ;; Temporarily bind issue for orchard-issue-start
          (cl-letf (((symbol-function 'orchard--get-issue-at-point)
                     (lambda () orchard--temp-issue)))
            (orchard-issue-start)))
      (user-error "No issue selected"))))

(provide 'orchard-actions)
;;; orchard-actions.el ends here
