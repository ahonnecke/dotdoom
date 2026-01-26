;;; orchard-dashboard.el --- Orchard dashboard buffer and formatting -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains dashboard-related code:
;; - orchard-mode (major mode)
;; - Dashboard formatting functions
;; - Navigation (next/prev item)
;; - View management
;; - Section collapsing

(require 'orchard-vars)
(require 'orchard-cache)
(require 'orchard-worktree)
(require 'orchard-claude)
(require 'orchard-window)
(require 'transient)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard-dispatch "orchard")
(declare-function orchard-new-feature "orchard-actions")
(declare-function orchard-new-bugfix "orchard-actions")
(declare-function orchard-new-chore "orchard-actions")
(declare-function orchard-new-refactor "orchard-actions")
(declare-function orchard-new-docs "orchard-actions")
(declare-function orchard-new-experiment "orchard-actions")
(declare-function orchard-open-at-point "orchard-actions")
(declare-function orchard-claude-at-point "orchard-actions")
(declare-function orchard-magit-at-point "orchard-actions")
(declare-function orchard-dired-at-point "orchard-actions")
(declare-function orchard-test-at-point "orchard-actions")
(declare-function orchard-issue-start "orchard-actions")
(declare-function orchard-issue-browse "orchard")
(declare-function orchard-find-issue "orchard")
(declare-function orchard-resolve-issue "orchard")
(declare-function orchard-next-step "orchard-actions")
(declare-function orchard-push-at-point "orchard-actions")
(declare-function orchard-pr-at-point "orchard-actions")
(declare-function orchard-hide-at-point "orchard-actions")
(declare-function orchard-show-hidden "orchard-actions")
(declare-function orchard-archive-at-point "orchard-actions")
(declare-function orchard-delete-at-point "orchard-actions")
(declare-function orchard-allocate-port "orchard-actions")
(declare-function orchard-release-port "orchard-actions")
(declare-function orchard-cleanup "orchard-actions")
(declare-function orchard-cleanup-dry-run "orchard-actions")
(declare-function orchard-cleanup-merged "orchard-actions")
(declare-function orchard-cleanup-merged-dry-run "orchard-actions")
(declare-function orchard-quit-all "orchard-actions")
(declare-function orchard-toggle-window-dedication "orchard-actions")
(declare-function orchard--get-archivable-worktrees "orchard-actions")
(declare-function ghq--cleanup-stale-ports "config-ghq" nil t)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Additional Faces (dashboard-specific)
;;; ════════════════════════════════════════════════════════════════════════════

(defface orchard-issue
  '((t :foreground "#61AFEF"))
  "Face for GitHub issue numbers."
  :group 'orchard)

(defface orchard-issue-title
  '((t :foreground "#ABB2BF"))
  "Face for GitHub issue titles."
  :group 'orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Mode
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Suppress inherited bindings that interfere with navigation
    (suppress-keymap map t)
    ;; Help
    (define-key map (kbd "?") #'orchard-dispatch)
    ;; Create branches (uppercase = create, unique first char for TAB completion)
    (define-key map (kbd "F") #'orchard-new-feature)
    (define-key map (kbd "B") #'orchard-new-bugfix)
    (define-key map (kbd "C") #'orchard-new-chore)
    (define-key map (kbd "R") #'orchard-new-refactor)
    (define-key map (kbd "D") #'orchard-new-docs)
    (define-key map (kbd "E") #'orchard-new-experiment)
    ;; Legacy shortcuts (lowercase, will be removed eventually)
    (define-key map (kbd "f") #'orchard-filter-menu)
    ;; Open/interact with worktree at point
    (define-key map (kbd "RET") #'orchard-open-at-point)
    (define-key map (kbd "c") #'orchard-claude-at-point)
    (define-key map (kbd "m") #'orchard-magit-at-point)
    (define-key map (kbd "d") #'orchard-dired-at-point)
    (define-key map (kbd "t") #'orchard-test-at-point)
    (define-key map (kbd "l") #'orchard-list-claudes)
    ;; GitHub Issues
    (define-key map (kbd "I") #'orchard-issue-start)
    (define-key map (kbd "o") #'orchard-issue-browse)
    (define-key map (kbd "#") #'orchard-find-issue)
    (define-key map (kbd "!") #'orchard-resolve-issue)
    (define-key map (kbd "s") #'orchard-toggle-staging-issues)
    (define-key map (kbd "/") #'orchard-filter-by-label)
    (define-key map (kbd "\\") #'orchard-clear-label-filter)
    ;; Lifecycle actions
    (define-key map (kbd "N") #'orchard-next-step)
    (define-key map (kbd "u") #'orchard-push-at-point)
    (define-key map (kbd "P") #'orchard-pr-at-point)
    (define-key map (kbd "-") #'orchard-hide-at-point)
    (define-key map (kbd "H") #'orchard-show-hidden)
    (define-key map (kbd "a") #'orchard-archive-at-point)
    ;; Port management
    (define-key map (kbd "+") #'orchard-allocate-port)
    (define-key map (kbd "_") #'orchard-release-port)
    ;; Filtering and views
    (define-key map (kbd "v w") #'orchard-view-working)
    (define-key map (kbd "v a") #'orchard-view-all)
    (define-key map (kbd "v n") #'orchard-view-next)
    (define-key map (kbd "v p") #'orchard-view-progress)
    (define-key map (kbd "v q") #'orchard-view-qa)
    (define-key map (kbd "v r") #'orchard-view-recent)
    ;; Section collapsing
    (define-key map (kbd "TAB") #'orchard-toggle-section)
    (define-key map (kbd "<tab>") #'orchard-toggle-section)
    ;; Navigation
    (define-key map (kbd "n") #'orchard-next-item)
    (define-key map (kbd "p") #'orchard-prev-item)
    (define-key map (kbd "j") #'orchard-next-item)
    (define-key map (kbd "k") #'orchard-prev-item)
    (define-key map (kbd "C-n") #'orchard-next-item)
    (define-key map (kbd "C-p") #'orchard-prev-item)
    (define-key map (kbd "<down>") #'orchard-next-item)
    (define-key map (kbd "<up>") #'orchard-prev-item)
    ;; Cleanup
    (define-key map (kbd "K") #'orchard-cleanup)
    (define-key map (kbd "C-c C-c") #'orchard-cleanup-dry-run)
    (define-key map (kbd "M") #'orchard-cleanup-merged)
    (define-key map (kbd "C-c m") #'orchard-cleanup-merged-dry-run)
    ;; Sync
    (define-key map (kbd "S") #'orchard-sync)
    (define-key map (kbd "O") #'orchard-cleanup-orphans)
    ;; Refresh and quit
    (define-key map (kbd "g") #'orchard-refresh)
    (define-key map (kbd "r") #'orchard-refresh)
    (define-key map (kbd "G") #'orchard-force-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'orchard-quit-all)
    map)
  "Keymap for orchard-mode.")

(define-derived-mode orchard-mode special-mode "Orchard"
  "Major mode for Orchard worktree dashboard.

\\{orchard-mode-map}"
  (setq buffer-read-only t
        truncate-lines t
        cursor-type 'bar)
  (hl-line-mode 1)
  (setq-local hl-line-face 'orchard-hl-line))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch and Icon Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--branch-face (branch)
  "Return face for BRANCH based on prefix."
  (cond
   ((string-prefix-p "FEATURE/" branch) 'orchard-branch-feature)
   ((string-prefix-p "BUGFIX/" branch) 'orchard-branch-bugfix)
   ((string-prefix-p "CHORE/" branch) 'orchard-branch-chore)
   ((string-prefix-p "REFACTOR/" branch) 'orchard-branch-refactor)
   ((string-prefix-p "DOCS/" branch) 'orchard-branch-docs)
   ((string-prefix-p "EXPERIMENT/" branch) 'orchard-branch-experiment)
   ((string-prefix-p "TEST/" branch) 'orchard-branch-test)
   ;; Legacy prefixes
   ((string-prefix-p "FEAT/" branch) 'orchard-branch-feature)
   ((string-prefix-p "FIX/" branch) 'orchard-branch-bugfix)
   ;; Main branches
   ((member branch '("dev" "main" "master")) 'orchard-branch-main)
   (t 'default)))

(defun orchard--branch-icon (branch)
  "Return icon for BRANCH type."
  (cond
   ((string-prefix-p "FEATURE/" branch) "✨")
   ((string-prefix-p "BUGFIX/" branch) "🐛")
   ((string-prefix-p "CHORE/" branch) "🧹")
   ((string-prefix-p "REFACTOR/" branch) "♻️")
   ((string-prefix-p "DOCS/" branch) "📖")
   ((string-prefix-p "EXPERIMENT/" branch) "🧪")
   ((string-prefix-p "TEST/" branch) "✅")
   ;; Legacy
   ((string-prefix-p "FEAT/" branch) "✨")
   ((string-prefix-p "FIX/" branch) "🐛")
   ((member branch '("dev" "main" "master")) "📦")
   (t "📁")))

(defun orchard--issue-type-icon (labels)
  "Return icon based on issue LABELS."
  (let ((label-names (mapcar (lambda (l) (alist-get 'name l)) labels)))
    (cond
     ((cl-some (lambda (n) (string-match-p "bug\\|fix" n)) label-names) "🐛")
     ((cl-some (lambda (n) (string-match-p "feature\\|enhancement" n)) label-names) "✨")
     ((cl-some (lambda (n) (string-match-p "chore\\|maintenance" n)) label-names) "🧹")
     ((cl-some (lambda (n) (string-match-p "doc" n)) label-names) "📖")
     (t "📋"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Issue/Worktree Linking Helpers
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--issue-has-worktree-p (issue-number worktrees)
  "Check if ISSUE-NUMBER already has a worktree in WORKTREES."
  (cl-some (lambda (wt)
             (let ((linked-issue (orchard--get-worktree-issue
                                  (alist-get 'path wt)
                                  (alist-get 'branch wt))))
               (and linked-issue (= linked-issue issue-number))))
           worktrees))

(defun orchard--find-worktree-for-issue (issue-number &optional worktrees)
  "Find and return the worktree associated with ISSUE-NUMBER, or nil."
  (cl-find-if (lambda (wt)
                (let ((linked-issue (orchard--get-worktree-issue
                                     (alist-get 'path wt)
                                     (alist-get 'branch wt))))
                  (and linked-issue (= linked-issue issue-number))))
              (or worktrees (orchard--get-worktrees))))

(defun orchard--issue-has-pr-p (issue-number worktrees)
  "Check if ISSUE-NUMBER has a PR created. Returns PR URL if found."
  (when-let ((wt (orchard--find-worktree-for-issue issue-number worktrees)))
    (let ((pr-url-file (expand-file-name ".pr-url" (alist-get 'path wt))))
      (when (file-exists-p pr-url-file)
        (with-temp-buffer
          (insert-file-contents pr-url-file)
          (string-trim (buffer-string)))))))

(defun orchard--issue-claude-waiting-p (issue-number worktrees)
  "Check if ISSUE-NUMBER has a Claude buffer waiting for input."
  (when-let ((wt (orchard--find-worktree-for-issue issue-number worktrees)))
    (when-let ((claude-buf (orchard--claude-buffer-for-path (alist-get 'path wt))))
      (orchard--claude-waiting-p claude-buf))))

(defun orchard--issue-workflow-stage (issue-number worktrees)
  "Get workflow stage indicators for ISSUE-NUMBER.
Returns alist with keys: has-analysis, has-plan, has-pr, claude-status."
  (when-let ((wt (orchard--find-worktree-for-issue issue-number worktrees)))
    (let ((path (alist-get 'path wt)))
      (list
       (cons 'has-analysis
             (file-exists-p (expand-file-name ".feature-description" path)))
       (cons 'has-plan
             (or (file-exists-p (expand-file-name ".test-plan.md" path))
                 (file-exists-p (expand-file-name ".plan.md" path))))
       (cons 'has-pr
             (file-exists-p (expand-file-name ".pr-url" path)))
       (cons 'claude-status
             (when-let ((buf (orchard--claude-buffer-for-path path)))
               (orchard--claude-status buf)))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Label Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-label (label)
  "Format a single LABEL for display with color."
  (let* ((name (alist-get 'name label))
         (color (alist-get 'color label))
         (fg-color (if color (format "#%s" color) "#888888")))
    (propertize (format "[%s]" name)
                'face `(:foreground ,fg-color :weight bold))))

(defun orchard--format-labels (labels)
  "Format LABELS list for display."
  (if (and labels (> (length labels) 0))
      (concat " " (mapconcat #'orchard--format-label labels " "))
    ""))

(defun orchard--format-workflow-indicator (stage)
  "Format workflow indicator showing what's DONE and Claude status."
  (if (null stage)
      ""
    (let ((a (alist-get 'has-analysis stage))
          (p (alist-get 'has-plan stage))
          (r (alist-get 'has-pr stage))
          (cs (alist-get 'claude-status stage)))
      (concat
       (pcase cs
         ('waiting (propertize "⏳WAIT " 'face '(:foreground "#E06C75" :weight bold)))
         ('idle (propertize "✓DONE " 'face '(:foreground "#E5C07B" :weight bold)))
         ('active (propertize "⟳ " 'face '(:foreground "#61AFEF")))
         (_ ""))
       (cond
        (r (propertize "PR" 'face '(:foreground "#61AFEF" :weight bold)))
        (p (propertize "planned" 'face '(:foreground "#98C379")))
        (a (propertize "analyzed" 'face '(:foreground "#98C379")))
        (t (propertize "wip" 'face '(:foreground "#5C6370"))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Section Headers and Categorization
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-section-header (title count &optional extra section-id)
  "Format a section header with TITLE, COUNT, optional EXTRA, and SECTION-ID."
  (let* ((collapsed (and section-id (orchard--section-collapsed-p section-id)))
         (indicator (if collapsed "▶" "▼"))
         (header-text (format "  %s %s (%d%s) " indicator title count
                              (if extra (format " %s" extra) "")))
         (line-length (max 0 (- 60 (length title) (length (number-to-string count))
                                (if extra (+ 1 (length extra)) 0)))))
    (propertize
     (concat
      "\n"
      (propertize header-text 'face 'orchard-subheader)
      (propertize (make-string line-length ?─) 'face 'font-lock-comment-face)
      "\n")
     'orchard-section section-id)))

(defun orchard--categorize-issues (issues worktrees)
  "Categorize ISSUES into lifecycle groups based on WORKTREES state."
  (let (up-next in-progress qa-verify done)
    (dolist (issue issues)
      (let* ((issue-num (alist-get 'number issue))
             (wt (orchard--find-worktree-for-issue issue-num worktrees)))
        (if (not wt)
            (push (cons issue nil) up-next)
          (let ((stage (alist-get 'stage wt)))
            (if (eq stage 'merged)
                (push (cons issue wt) qa-verify)
              (push (cons issue wt) in-progress))))))
    ;; Get archivable worktrees for DONE section
    (unless orchard--inhibit-cache-refresh
      (when (fboundp 'orchard--get-archivable-worktrees)
        (dolist (wt (orchard--get-archivable-worktrees))
          (let* ((branch (alist-get 'branch wt))
                 (issue-num (orchard--get-worktree-issue nil branch)))
            (when issue-num
              (push (cons `((number . ,issue-num)
                            (title . ,(format "Issue #%d" issue-num))
                            (labels . nil)
                            (closed . t))
                          wt)
                    done))))))
    `((up-next . ,(nreverse up-next))
      (in-progress . ,(nreverse in-progress))
      (qa-verify . ,(nreverse qa-verify))
      (done . ,(nreverse done)))))

(defun orchard--get-orphan-worktrees (worktrees)
  "Get WORKTREES that don't have linked issues."
  (cl-remove-if
   (lambda (wt)
     (let ((branch (alist-get 'branch wt)))
       (or (member branch '("dev" "main" "master"))
           (orchard--get-worktree-issue nil branch))))
   worktrees))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Issue and Worktree Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-branch-inline (wt current-path)
  "Format worktree WT as an inline branch line."
  (let* ((path (alist-get 'path wt))
         (branch (or (alist-get 'branch wt) "(detached)"))
         (port (alist-get 'port wt))
         (dirty (alist-get 'dirty wt))
         (ahead (or (alist-get 'ahead wt) 0))
         (behind (or (alist-get 'behind wt) 0))
         (is-current (and current-path (string= path current-path)))
         (branch-face (if is-current 'orchard-current 'font-lock-comment-face)))
    (concat
     "       ↳ "
     (propertize (truncate-string-to-width branch 28 nil nil "…")
                 'face branch-face)
     " "
     (propertize (if dirty "●" "○")
                 'face (if dirty 'orchard-dirty 'orchard-clean))
     (if (> ahead 0) (format " ↑%d" ahead) "")
     (if (> behind 0) (format " ↓%d" behind) "")
     (if port
         (propertize (format " :%d" (+ 3000 port)) 'face '(:foreground "#98C379"))
       "")
     (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
     "\n")))

(defun orchard--format-issue-with-branch (issue-wt-pair current-path &optional show-merged-badge)
  "Format ISSUE-WT-PAIR as issue line with optional branch underneath."
  (let* ((issue (car issue-wt-pair))
         (wt (cdr issue-wt-pair))
         (number (alist-get 'number issue))
         (title (alist-get 'title issue))
         (labels (alist-get 'labels issue))
         (closed (alist-get 'closed issue))
         (icon (orchard--issue-type-icon labels))
         (wt-path (when wt (alist-get 'path wt)))
         (stage (when wt (orchard--issue-workflow-stage number (list wt))))
         (claude-status (alist-get 'claude-status stage))
         (needs-attention (memq claude-status '(waiting idle)))
         (workflow (if wt (orchard--format-workflow-indicator stage) ""))
         (session-indicator (if wt-path (orchard--format-session-indicator wt-path) ""))
         (label-str (orchard--format-labels labels))
         (attention-prefix (if needs-attention
                               (propertize ">>>" 'face '(:foreground "#E06C75" :weight bold))
                             "   "))
         (merged-badge (if show-merged-badge
                           (propertize " ✓Merged" 'face '(:foreground "#98C379"))
                         ""))
         (closed-badge (if closed
                           (propertize " ✓Closed" 'face '(:foreground "#98C379"))
                         ""))
         (issue-line (concat
                      attention-prefix " " icon " "
                      (propertize (format "#%d" number) 'face 'orchard-issue)
                      " "
                      (when (not (string-empty-p workflow))
                        (concat workflow " "))
                      (propertize (truncate-string-to-width (or title "") 30 nil nil "...")
                                  'face 'orchard-issue-title)
                      merged-badge
                      closed-badge
                      session-indicator
                      label-str
                      "\n")))
    (when needs-attention
      (setq issue-line (propertize issue-line 'face
                                   (if (eq claude-status 'waiting)
                                       '(:background "#3E2723")
                                     '(:background "#2E3B2E")))))
    (concat
     (propertize issue-line 'orchard-issue issue 'orchard-worktree wt)
     (when wt
       (propertize (orchard--format-branch-inline wt current-path)
                   'orchard-issue issue 'orchard-worktree wt)))))

(defun orchard--format-orphan-worktree (wt current-path)
  "Format orphan worktree WT (no linked issue)."
  (let* ((path (alist-get 'path wt))
         (branch (or (alist-get 'branch wt) "(detached)"))
         (port (alist-get 'port wt))
         (dirty (alist-get 'dirty wt))
         (ahead (or (alist-get 'ahead wt) 0))
         (behind (or (alist-get 'behind wt) 0))
         (stage (alist-get 'stage wt))
         (claude-buf (orchard--claude-buffer-for-path path))
         (claude-status (when claude-buf (orchard--claude-status claude-buf)))
         (needs-attention (memq claude-status '(waiting idle)))
         (is-current (and current-path (string= path current-path)))
         (icon (orchard--branch-icon branch))
         (branch-face (if is-current 'orchard-current (orchard--branch-face branch)))
         (session-indicator (orchard--format-session-indicator path))
         (attention-prefix (if needs-attention
                               (propertize ">>>" 'face '(:foreground "#E06C75" :weight bold))
                             "   "))
         (line (concat
                attention-prefix icon " "
                (propertize (truncate-string-to-width branch 32 nil nil "…")
                            'face branch-face)
                " "
                (propertize (if dirty "●" "○")
                            'face (if dirty 'orchard-dirty 'orchard-clean))
                (if (> ahead 0) (format " ↑%d" ahead) "")
                (if (> behind 0) (format " ↓%d" behind) "")
                (pcase claude-status
                  ('waiting (propertize " ⏳WAIT" 'face '(:foreground "#E06C75" :weight bold)))
                  ('idle (propertize " ✓DONE" 'face '(:foreground "#E5C07B" :weight bold)))
                  ('active (propertize " ⟳" 'face '(:foreground "#61AFEF")))
                  (_ ""))
                (if port
                    (propertize (format " :%d" (+ 3000 port)) 'face '(:foreground "#98C379"))
                  "")
                (when (eq stage 'merged)
                  (propertize " ✓Merged" 'face '(:foreground "#98C379")))
                session-indicator
                (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
                "\n")))
    (propertize line 'orchard-worktree wt)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-dashboard ()
  "Format the Orchard dashboard with issue-centric layout."
  (let* ((worktrees (orchard--get-worktrees))
         (current (orchard--current-worktree))
         (current-path (when current (alist-get 'path current)))
         (claude-bufs (cl-remove-if-not
                       (lambda (buf)
                         (let ((bufname (buffer-name buf)))
                           (cl-some (lambda (wt)
                                      (let ((name (file-name-nondirectory
                                                   (directory-file-name (alist-get 'path wt)))))
                                        (string-match-p (regexp-quote name) bufname)))
                                    worktrees)))
                       (orchard--get-claude-buffers)))
         (waiting-count (cl-count-if #'orchard--claude-waiting-p claude-bufs))
         (total-claudes (length claude-bufs))
         (orphans (unless orchard--inhibit-cache-refresh (orchard--get-orphan-directories)))
         (prunable (unless orchard--inhibit-cache-refresh (orchard--get-prunable-worktrees)))
         (sync-issues (+ (length orphans) (length prunable)))
         (all-open-issues (orchard--get-open-issues))
         (staging-count (cl-count-if #'orchard--issue-staging-p all-open-issues))
         (filtered-issues (let ((issues all-open-issues))
                            (when orchard--hide-staging-issues
                              (setq issues (cl-remove-if #'orchard--issue-staging-p issues)))
                            (when orchard--label-filter
                              (setq issues (cl-remove-if-not
                                            (lambda (i) (orchard--issue-has-exact-label-p i orchard--label-filter))
                                            issues)))
                            (setq issues (cl-remove-if
                                          (lambda (i) (orchard--issue-hidden-p (alist-get 'number i)))
                                          issues))
                            issues))
         (visible-worktrees (cl-remove-if
                             (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                             worktrees))
         (categories (orchard--categorize-issues filtered-issues visible-worktrees))
         (up-next (alist-get 'up-next categories))
         (in-progress (alist-get 'in-progress categories))
         (qa-verify (alist-get 'qa-verify categories))
         (done (alist-get 'done categories))
         (orphan-worktrees (cl-remove-if
                            (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                            (orchard--get-orphan-worktrees visible-worktrees)))
         (hidden-count (+ (length (orchard--get-hidden-issues)) (length (orchard--get-hidden))))
         (view-name (pcase orchard--current-view
                      ('working "working")
                      ('all "all")
                      ('next "next")
                      ('qa "qa")
                      ('progress "progress")
                      (_ "working"))))
    (concat
     "\n"
     (propertize "  🌳 Orchard" 'face 'orchard-header)
     (propertize (format "  📋 %d issues" (length filtered-issues))
                 'face 'font-lock-comment-face)
     (when (> total-claudes 0)
       (if (> waiting-count 0)
           (propertize (format "  🔔 %d/%d Claude NEED ATTENTION" waiting-count total-claudes)
                       'face '(:foreground "#E06C75" :weight bold))
         (propertize (format "  %d Claude" total-claudes) 'face 'font-lock-comment-face)))
     (when (> sync-issues 0)
       (propertize (format "  ⚠ %d sync issues" sync-issues)
                   'face 'orchard-branch-mismatch))
     (when orchard--label-filter
       (propertize (format "  🏷 %s" orchard--label-filter)
                   'face '(:foreground "#61AFEF" :weight bold)))
     (when (and (> staging-count 0) orchard--hide-staging-issues)
       (propertize (format "  (%d staging hidden)" staging-count)
                   'face 'font-lock-comment-face))
     (when (> hidden-count 0)
       (propertize (format "  [%d hidden]" hidden-count)
                   'face 'font-lock-comment-face))
     "\n"
     (unless (eq orchard--current-view 'all)
       (propertize (format "  View: %s (f for filter menu)\n" view-name)
                   'face 'font-lock-comment-face))
     "\n"
     ;; Quick actions
     (propertize "  " 'face 'default)
     (propertize "[RET]" 'face 'orchard-key)
     (propertize " Open  " 'face 'font-lock-comment-face)
     (propertize "[I]" 'face 'orchard-key)
     (propertize " New  " 'face 'font-lock-comment-face)
     (propertize "[f]" 'face 'orchard-key)
     (propertize " Filter  " 'face 'font-lock-comment-face)
     (propertize "[m]" 'face 'orchard-key)
     (propertize " Magit  " 'face 'font-lock-comment-face)
     (propertize "[?]" 'face 'orchard-key)
     (propertize " Help" 'face 'font-lock-comment-face)
     "\n"
     ;; Sections
     (when (and up-next (orchard--section-visible-p 'up-next))
       (concat
        (orchard--format-section-header "UP NEXT" (length up-next) "available" 'up-next)
        (unless (orchard--section-collapsed-p 'up-next)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     up-next ""))))
     (when (and in-progress (orchard--section-visible-p 'in-progress))
       (concat
        (orchard--format-section-header "IN PROGRESS" (length in-progress) nil 'in-progress)
        (unless (orchard--section-collapsed-p 'in-progress)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     in-progress ""))))
     (when (and qa-verify (orchard--section-visible-p 'qa-verify))
       (concat
        (orchard--format-section-header "QA/VERIFY" (length qa-verify) "merged, issue open" 'qa-verify)
        (unless (orchard--section-collapsed-p 'qa-verify)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     qa-verify ""))))
     (when (and done (orchard--section-visible-p 'done))
       (concat
        (orchard--format-section-header "DONE" (length done) "ready to archive" 'done)
        (unless (orchard--section-collapsed-p 'done)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     done ""))))
     (when (and orphan-worktrees (orchard--section-visible-p 'unlinked))
       (concat
        (orchard--format-section-header "UNLINKED BRANCHES" (length orphan-worktrees) nil 'unlinked)
        (unless (orchard--section-collapsed-p 'unlinked)
          (mapconcat (lambda (wt)
                       (orchard--format-orphan-worktree wt current-path))
                     orphan-worktrees ""))))
     (when (orchard--section-visible-p 'recent-sessions)
       (orchard--format-recent-sessions))
     ;; Empty state
     (when (and (null up-next) (null in-progress) (null qa-verify)
                (null done) (null orphan-worktrees)
                (not (eq orchard--current-view 'recent)))
       (propertize "\n  No issues or worktrees found. Press I to start from an issue.\n"
                   'face 'font-lock-comment-face))
     "\n"
     ;; Footer
     (propertize "  " 'face 'default)
     (propertize "g" 'face 'orchard-key)
     (propertize " refresh  " 'face 'font-lock-comment-face)
     (propertize "G" 'face 'orchard-key)
     (propertize " force-refresh  " 'face 'font-lock-comment-face)
     (propertize "M" 'face 'orchard-key)
     (propertize " archive done  " 'face 'font-lock-comment-face)
     (propertize "s" 'face 'orchard-key)
     (propertize " toggle staging" 'face 'font-lock-comment-face))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Point/Item Retrieval
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-worktree-at-point ()
  "Get worktree at point."
  (or (get-text-property (point) 'orchard-worktree)
      (save-excursion
        (beginning-of-line)
        (let ((end (line-end-position)) result)
          (while (and (< (point) end) (not result))
            (setq result (get-text-property (point) 'orchard-worktree))
            (forward-char 1))
          result))))

(defun orchard--get-issue-at-point ()
  "Get GitHub issue at point."
  (or (get-text-property (point) 'orchard-issue)
      (save-excursion
        (beginning-of-line)
        (let ((end (line-end-position)) result)
          (while (and (< (point) end) (not result))
            (setq result (get-text-property (point) 'orchard-issue))
            (forward-char 1))
          result))))

(defun orchard--current-worktree ()
  "Get the worktree for the current buffer, if any."
  (let ((dir (expand-file-name default-directory)))
    (cl-find-if (lambda (wt)
                  (string-prefix-p (file-name-as-directory (alist-get 'path wt))
                                   (file-name-as-directory dir)))
                (orchard--get-worktrees))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Navigation
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--find-next-item-pos ()
  "Find position of next item after point."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point))
          (current-issue (orchard--get-issue-at-point)))
      (forward-line 1)
      (while (and (not (eobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree))
                        (issue (get-text-property (point) 'orchard-issue)))
                    (or (and (null wt) (null issue))
                        (and wt current-wt
                             (equal (alist-get 'path wt) (alist-get 'path current-wt)))
                        (and issue current-issue
                             (equal (alist-get 'number issue) (alist-get 'number current-issue))))))
        (forward-line 1))
      (when (or (get-text-property (point) 'orchard-worktree)
                (get-text-property (point) 'orchard-issue))
        (point)))))

(defun orchard--find-prev-item-pos ()
  "Find position of previous item before point."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point))
          (current-issue (orchard--get-issue-at-point)))
      (forward-line -1)
      (while (and (not (bobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree))
                        (issue (get-text-property (point) 'orchard-issue)))
                    (or (and (null wt) (null issue))
                        (and wt current-wt
                             (equal (alist-get 'path wt) (alist-get 'path current-wt)))
                        (and issue current-issue
                             (equal (alist-get 'number issue) (alist-get 'number current-issue))))))
        (forward-line -1))
      (when (or (get-text-property (point) 'orchard-worktree)
                (get-text-property (point) 'orchard-issue))
        (point)))))

(defun orchard-next-item ()
  "Move to next item."
  (interactive)
  (if-let ((pos (orchard--find-next-item-pos)))
      (goto-char pos)
    (message "No more items")))

(defun orchard-prev-item ()
  "Move to previous item."
  (interactive)
  (if-let ((pos (orchard--find-prev-item-pos)))
      (goto-char pos)
    (message "No previous items")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; View Management
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
  "Show items with recent Claude sessions."
  (interactive)
  (orchard-set-view 'recent))

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

;;; ════════════════════════════════════════════════════════════════════════════
;;; Recent Sessions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-sessions-with-worktrees ()
  "Get all worktrees with Claude sessions, sorted by recency."
  (orchard--ensure-claude-sessions-cache)
  (let ((worktrees (orchard--get-worktrees t)))
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
  "Format recent Claude sessions section."
  (let* ((session-items (orchard--get-sessions-with-worktrees))
         (count (length session-items)))
    (if (= count 0)
        (propertize "  No Claude sessions found\n\n" 'face 'font-lock-comment-face)
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
                  (if issue-num (propertize (format "#%d" issue-num) 'face 'orchard-issue) "")
                  " "
                  (propertize (truncate-string-to-width branch 28 nil nil "…")
                              'face (if is-current 'orchard-current (orchard--branch-face branch)))
                  (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
                  "\n"
                  (when summary
                    (concat "      "
                            (propertize (truncate-string-to-width summary 60 nil nil "…")
                                        'face 'font-lock-comment-face)
                            "\n")))
                 'orchard-worktree wt)))
            session-items "")))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Refresh
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-refresh ()
  "Refresh the dashboard using cached data."
  (interactive)
  (when (fboundp 'ghq--cleanup-stale-ports)
    (ghq--cleanup-stale-ports))
  (when (eq major-mode 'orchard-mode)
    (let ((inhibit-read-only t)
          (orchard--inhibit-cache-refresh t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (orchard--format-dashboard))
      (goto-char (point-min))
      (forward-line (1- (min line (count-lines (point-min) (point-max))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Filter Transient
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix orchard-filter-menu ()
  "Filter and view options for Orchard dashboard."
  ["View Presets"
   ("w" "Working (next + progress)" orchard-view-working)
   ("a" "All sections" orchard-view-all)
   ("n" "Next only" orchard-view-next)
   ("p" "In Progress only" orchard-view-progress)
   ("q" "QA only" orchard-view-qa)
   ("r" "Recent sessions" orchard-view-recent)]
  ["Filters"
   ("/" "Filter by label" orchard-filter-by-label)
   ("\\" "Clear label filter" orchard-clear-label-filter)
   ("s" "Toggle staging" orchard-toggle-staging-issues)])

(provide 'orchard-dashboard)
;;; orchard-dashboard.el ends here
