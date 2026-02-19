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
(declare-function orchard-merge-pr-at-point "orchard-actions")
(declare-function orchard-mark-pr-ready "orchard-actions")
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
(declare-function orchard-filter-by-text "orchard-actions")
(declare-function orchard-filter-by-label "orchard-actions")
(declare-function orchard-clear-filters "orchard-actions")
(declare-function orchard-toggle-staging-issues "orchard-actions")
(declare-function orchard-filter-menu "orchard-actions")
(declare-function orchard-research-open "orchard-actions")
(declare-function orchard-research-set-context "orchard-actions")
(declare-function orchard-resume-sessions "orchard-claude")
(declare-function orchard-resume-sessions-prompt "orchard-claude")
(defvar orchard--claude-sessions-file)
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
    (suppress-keymap map t)

    ;; ═══ ESSENTIAL: What you actually use ═══
    ;; Search/Find
    (define-key map (kbd "/") #'orchard-filter-by-text)   ; search by text
    (define-key map (kbd "#") #'orchard-find-issue)       ; jump to issue number
    (define-key map (kbd "\\") #'orchard-clear-filters)   ; clear search
    (define-key map (kbd "f") #'orchard-filter-menu)      ; filter/view transient

    ;; Refresh
    (define-key map (kbd "g") #'orchard-refresh)          ; quick refresh
    (define-key map (kbd "G") #'orchard-force-refresh)    ; full refresh from GitHub

    ;; Open/Work (at point)
    (define-key map (kbd "RET") #'orchard-open-at-point)  ; default action (claude)
    (define-key map (kbd "c") #'orchard-claude-at-point)  ; open claude
    (define-key map (kbd "m") #'orchard-magit-at-point)   ; open magit
    (define-key map (kbd "d") #'orchard-dired-at-point)   ; open dired
    (define-key map (kbd "o") #'orchard-issue-browse)     ; open in browser
    (define-key map (kbd "t") #'orchard-test-at-point)    ; run tests

    ;; Lifecycle
    (define-key map (kbd "u") #'orchard-push-at-point)    ; push branch
    (define-key map (kbd "P") #'orchard-pr-at-point)      ; create PR
    (define-key map (kbd "M") #'orchard-merge-pr-at-point) ; merge PR
    (define-key map (kbd "-") #'orchard-hide-at-point)    ; hide item
    (define-key map (kbd "H") #'orchard-show-hidden)      ; show hidden
    (define-key map (kbd "A") #'orchard-archive-at-point) ; archive done items
    (define-key map (kbd "D") #'orchard-delete-at-point)  ; delete worktree+branch

    ;; Sessions
    (define-key map (kbd "z") #'orchard-resume-sessions)  ; resume saved sessions
    (define-key map (kbd "Z") #'orchard--clear-previous-sessions)
    (define-key map (kbd "l") #'orchard-list-claudes)     ; list all claude sessions
    (define-key map (kbd "W") #'orchard-tile-claudes)     ; tile up to 4 claudes
    (define-key map (kbd ">") #'orchard-next-claude)      ; step through claudes
    (define-key map (kbd "<") #'orchard-prev-claude)      ; step back through claudes

    ;; Create branches (from issue or scratch)
    (define-key map (kbd "I") #'orchard-issue-start)      ; start from GitHub issue
    (define-key map (kbd "E") #'orchard-new-experiment)   ; experiment/demo branch
    (define-key map (kbd "X") #'orchard-new-research)     ; research/debug branch

    ;; ═══ NAVIGATION ═══
    (define-key map (kbd "n") #'orchard-next-item)
    (define-key map (kbd "p") #'orchard-prev-item)
    (define-key map (kbd "TAB") #'orchard-toggle-section)
    (define-key map (kbd "<tab>") #'orchard-toggle-section)

    ;; ═══ LESS COMMON ═══
    (define-key map (kbd "?") #'orchard-dispatch)         ; help/all commands
    (define-key map (kbd "s") #'orchard-toggle-staging-issues)
    (define-key map (kbd "K") #'orchard-cleanup)          ; cleanup stale
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'orchard-quit-all)         ; quit + kill claudes
    map)
  "Keymap for orchard-mode.

Essential keys:
  RET/c    Open Claude for item at point
  m        Open Magit for item
  d        Open Dired for item
  /        Search issues by text
  #        Jump to issue by number
  f        Filter/view menu
  g/G      Refresh (quick/full)
  I        Start work from GitHub issue
  u/P/M    Push / Create PR / Merge PR
  l        List all Claude sessions
  ?        Full command menu")

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
  ;; Labels may be vector from JSON, convert to list for mapcar
  (let ((label-names (mapcar (lambda (l) (alist-get 'name l)) (append labels nil))))
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
Returns alist with keys: has-analysis, has-plan, has-pr, pr-ready, claude-status."
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
       (cons 'pr-ready
             (eq (orchard--get-stage-override path) 'pr-ready))
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
  ;; Labels may be vector from JSON, convert to list for mapconcat
  (let ((label-list (append labels nil)))
    (if (and label-list (> (length label-list) 0))
        (concat " " (mapconcat #'orchard--format-label label-list " "))
      "")))

(defun orchard--format-pr-status-indicator (branch)
  "Format PR status indicator for BRANCH showing CI and merge status."
  (when-let ((pr-status (orchard--get-pr-status branch)))
    (let ((ci (plist-get pr-status :ci-status))
          (mergeable (plist-get pr-status :mergeable))
          (review (plist-get pr-status :review-decision)))
      (concat
       ;; CI status
       (pcase ci
         ('success (propertize "✅" 'face '(:foreground "#98C379")))
         ('failure (propertize "❌" 'face '(:foreground "#E06C75")))
         ('pending (propertize "⏳" 'face '(:foreground "#E5C07B")))
         (_ ""))
       ;; Merge conflict
       (when (equal mergeable "CONFLICTING")
         (propertize "⚠️" 'face '(:foreground "#E06C75" :weight bold)))
       ;; Review status
       (pcase review
         ("APPROVED" (propertize "👍" 'face '(:foreground "#98C379")))
         ("CHANGES_REQUESTED" (propertize "🔄" 'face '(:foreground "#E5C07B")))
         (_ ""))))))

(defun orchard--format-workflow-indicator (stage &optional branch)
  "Format workflow indicator showing what's DONE, Claude status, and PR status.
BRANCH is used to look up PR status from cache."
  (if (null stage)
      ""
    (let ((a (alist-get 'has-analysis stage))
          (p (alist-get 'has-plan stage))
          (r (alist-get 'has-pr stage))
          (pr-ready (alist-get 'pr-ready stage))
          (cs (alist-get 'claude-status stage))
          (pr-status-str (when branch (orchard--format-pr-status-indicator branch))))
      (concat
       (pcase cs
         ('waiting (propertize "⏳WAIT " 'face '(:foreground "#E06C75" :weight bold)))
         ('idle (propertize "✓DONE " 'face '(:foreground "#E5C07B" :weight bold)))
         ('active (propertize "⟳ " 'face '(:foreground "#61AFEF")))
         (_ ""))
       (cond
        (r (concat (propertize "PR" 'face '(:foreground "#61AFEF" :weight bold))
                   (when pr-status-str (concat " " pr-status-str))))
        (pr-ready (propertize "🚀READY" 'face '(:foreground "#C678DD" :weight bold)))
        (p (propertize "planned" 'face '(:foreground "#98C379")))
        (a (propertize "analyzed" 'face '(:foreground "#98C379")))
        (t (propertize "wip" 'face '(:foreground "#5C6370"))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Milestone Bar
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-milestone-bar ()
  "Format a compact milestone bar showing available milestones with counts.
Each milestone is clickable (RET filters to it)."
  (let ((milestones (orchard--get-all-milestones)))
    (when milestones
      (concat
       (propertize "  🎯 " 'face 'font-lock-comment-face)
       (mapconcat
        (lambda (ms)
          (let* ((title (car ms))
                 (count (cdr ms))
                 (active (and orchard--milestone-filter
                              (string-equal-ignore-case title orchard--milestone-filter)))
                 (face (if active
                           '(:foreground "#C678DD" :weight bold :underline t)
                         '(:foreground "#C678DD"))))
            (propertize (format "%s (%d)" title count)
                        'face face
                        'orchard-milestone title
                        'help-echo (format "RET to filter by %s" title))))
        milestones
        (propertize "  " 'face 'default))
       (when orchard--milestone-filter
         (concat "  " (propertize "[\\] clear" 'face 'font-lock-comment-face)))
       "\n"))))

(defun orchard-milestone-at-point ()
  "Filter by milestone at point, or clear if already active."
  (interactive)
  (when-let ((title (get-text-property (point) 'orchard-milestone)))
    (if (and orchard--milestone-filter
             (string-equal-ignore-case title orchard--milestone-filter))
        (progn
          (setq orchard--milestone-filter nil)
          (message "Milestone filter cleared"))
      (setq orchard--milestone-filter title)
      (message "Filtering by milestone: %s" title))
    (orchard-refresh)))

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
  "Categorize ISSUES into lifecycle groups based on WORKTREES state.
Returns alist with keys:
  uat-urgent      - Has P1 label AND recent comment from UAT reviewer (HIGHEST)
  claude-waiting  - Claude buffer is waiting for input (HIGHEST PRIORITY)
  uat-failed      - Has P1 label (failed UAT, needs revisiting)
  ready-to-deploy - Merged to dev but not on staging yet
  next-up         - Issues active today with NO worktree (not started)
  needs-analysis  - Has worktree but no commits ahead of upstream (analyzed only)
  in-flight       - Has worktree with commits, no PR (work in progress)
  stale-work      - Has plan but no activity in 3+ days
  pr-failing      - Has PR with CI failing
  pr-review       - Has PR, CI passing, needs review
  pr-approved     - Has PR, approved, needs merge
  qa-verify       - PR merged or has [production] label, issue still open
  done            - Ready to archive
  backlog         - Older issues without worktrees"
  (let (uat-urgent claude-waiting uat-failed ready-to-deploy next-up needs-analysis in-flight stale-work pr-failing pr-review pr-approved qa-verify done backlog)
    (dolist (issue issues)
      (let* ((issue-num (alist-get 'number issue))
             (wt (orchard--find-worktree-for-issue issue-num worktrees))
             (wt-path (when wt (alist-get 'path wt)))
             (claude-buf (when wt-path (orchard--claude-buffer-for-path wt-path)))
             (claude-status (when claude-buf (orchard--claude-status claude-buf)))
             (has-saved-session (when wt-path (orchard--worktree-has-claude-session-p wt-path)))
             (has-production-label (orchard--issue-has-label-p issue "production"))
             (has-p1-label (orchard--issue-has-exact-label-p issue "P1"))
             (active-today (orchard--issue-active-today-p issue)))
        (cond
         ;; P1 issues - only check UAT status if staging data is loaded
         ;; (staging data loads on force-refresh G, not initial open)
         ((and has-p1-label
               orchard--staging-fetched-time
               (orchard--p1-needs-dev-attention-p issue))
          (if (orchard--issue-needs-urgent-attention-p issue)
              ;; Recent UAT comment = highest priority
              (push (cons issue wt) uat-urgent)
            ;; Otherwise just failed UAT
            (push (cons issue wt) uat-failed)))
         ;; P1 with fix deployed (staging newer than comment) - skip, awaiting re-test
         ;; Only check if staging data is loaded
         ((and has-p1-label
               orchard--staging-fetched-time
               (orchard--get-staging-merge-time issue-num))
          nil) ; Don't show - fix deployed, awaiting UAT re-test
         ;; Claude is WAITING for input - very high priority
         ((eq claude-status 'waiting)
          (push (cons issue wt) claude-waiting))
         ;; Issues with [production] label are deployed - need verification/close
         (has-production-label
          (push (cons issue wt) qa-verify))
         ;; Merged to dev but not on staging - ready to deploy
         ;; Only check if merged data is loaded (loads on G)
         ((and orchard--merged-branches-cache
               (orchard--issue-ready-to-deploy-p issue-num))
          (push (cons issue wt) ready-to-deploy))
         ;; No worktree
         ((not wt)
          (cond
           ;; Active today - show in NEXT UP (not started yet)
           (active-today
            (push (cons issue nil) next-up))
           ;; Not active today - backlog
           (t
            (push (cons issue nil) backlog))))
         ;; Has worktree - categorize by workflow stage
         (t
          (let* ((stage (alist-get 'stage wt))
                 (path (alist-get 'path wt))
                 (branch (alist-get 'branch wt))
                 (ahead (or (alist-get 'ahead wt) 0))
                 (has-plan (or (file-exists-p (expand-file-name ".plan.md" path))
                               (file-exists-p (expand-file-name ".test-plan.md" path))))
                 ;; Check both .pr-url file AND GitHub PR cache
                 (has-pr-file (file-exists-p (expand-file-name ".pr-url" path)))
                 (pr-status (orchard--get-pr-status branch))
                 (has-pr (or has-pr-file pr-status))
                 (ci-status (when pr-status (plist-get pr-status :ci-status)))
                 (review (when pr-status (plist-get pr-status :review-decision)))
                 ;; Real work = commits ahead of upstream
                 (has-real-work (> ahead 0)))
            (cond
             ;; PR merged - QA/verify
             ((eq stage 'merged)
              (push (cons issue wt) qa-verify))
             ;; Has PR - categorize by CI/review status
             (has-pr
              (cond
               ((eq ci-status 'failure)
                (push (cons issue wt) pr-failing))
               ((equal review "APPROVED")
                (push (cons issue wt) pr-approved))
               (t
                (push (cons issue wt) pr-review))))
             ;; Has plan but no PR - check if stale (no activity in 3+ days)
             (has-plan
              (let* ((session-info (orchard--get-worktree-session-info path))
                     (modified (when session-info (plist-get session-info :modified)))
                     (stale (or (null modified)
                                (> (/ (float-time (time-subtract (current-time)
                                                                  (date-to-time modified)))
                                      86400)
                                   3))))  ; 3 days = stale
                (if stale
                    (push (cons issue wt) stale-work)
                  (push (cons issue wt) in-flight))))
             ;; Active Claude (not waiting) - in flight regardless of commits
             ((eq claude-status 'active)
              (push (cons issue wt) in-flight))
             ;; Has commits ahead — real work done, in flight
             (has-real-work
              (push (cons issue wt) in-flight))
             ;; No commits ahead — worktree exists but no code written yet
             ;; (analysis ran, but implementation hasn't started)
             (t
              (push (cons issue wt) needs-analysis))))))))
    ;; Get archivable worktrees for DONE section
    ;; Only when merged data is loaded (loads on G, not initial open)
    (when (and (not orchard--inhibit-cache-refresh)
               orchard--merged-branches-cache)
      (when (fboundp 'orchard--get-archivable-worktrees)
        (dolist (wt (orchard--get-archivable-worktrees))
          (let* ((branch (alist-get 'branch wt))
                 (issue-num (orchard--get-worktree-issue (alist-get 'path wt) branch)))
            (when issue-num
              (push (cons `((number . ,issue-num)
                            (title . ,(format "Issue #%d" issue-num))
                            (labels . nil)
                            (closed . t))
                          wt)
                    done))))))
    `((uat-urgent . ,(nreverse uat-urgent))
      (claude-waiting . ,(nreverse claude-waiting))
      (uat-failed . ,(nreverse uat-failed))
      (ready-to-deploy . ,(nreverse ready-to-deploy))
      (next-up . ,(nreverse next-up))
      (needs-analysis . ,(nreverse needs-analysis))
      (in-flight . ,(nreverse in-flight))
      (stale-work . ,(nreverse stale-work))
      (pr-failing . ,(nreverse pr-failing))
      (pr-review . ,(nreverse pr-review))
      (pr-approved . ,(nreverse pr-approved))
      (qa-verify . ,(nreverse qa-verify))
      (done . ,(nreverse done))
      (backlog . ,(nreverse backlog)))))

(defun orchard--get-orphan-worktrees (worktrees)
  "Get WORKTREES that don't have linked issues."
  (cl-remove-if
   (lambda (wt)
     (let ((branch (alist-get 'branch wt)))
       (or (member branch '("dev" "main" "master"))
           (orchard--get-worktree-issue nil branch))))
   worktrees))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Priority Sorting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--issue-priority (issue)
  "Extract priority number from ISSUE labels (P1=1, P2=2, etc).
Returns 99 if no priority label found (sorts to end)."
  ;; Labels come as vectors from JSON, convert to list for cl-loop
  (let ((labels (append (alist-get 'labels issue) nil)))
    (or (cl-loop for label in labels
                 for name = (if (stringp label) label (alist-get 'name label))
                 when (and name (string-match "^P\\([1-5]\\)$" name))
                 return (string-to-number (match-string 1 name)))
        99)))

(defun orchard--sort-by-priority (issue-pairs)
  "Sort ISSUE-PAIRS by priority label (P1 first, P5 last, no-priority at end)."
  (sort (copy-sequence issue-pairs)
        (lambda (a b)
          (< (orchard--issue-priority (car a))
             (orchard--issue-priority (car b))))))

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
         (labels (append (alist-get 'labels issue) nil))
         (closed (alist-get 'closed issue))
         (icon (orchard--issue-type-icon labels))
         (wt-path (when wt (alist-get 'path wt)))
         (wt-branch (when wt (alist-get 'branch wt)))
         (stage (when wt (orchard--issue-workflow-stage number (list wt))))
         (claude-status (alist-get 'claude-status stage))
         (needs-attention (memq claude-status '(waiting idle)))
         (workflow (if wt (orchard--format-workflow-indicator stage wt-branch) ""))
         (session-indicator (if wt-path (orchard--format-session-indicator wt-path) ""))
         (milestone (alist-get 'milestone issue))
         (milestone-str (if milestone
                            (propertize (format " [%s]" (alist-get 'title milestone))
                                        'face '(:foreground "#C678DD" :weight bold))
                          ""))
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
                      (propertize (truncate-string-to-width (or title "") 45 nil nil "...")
                                  'face 'orchard-issue-title)
                      merged-badge
                      closed-badge
                      milestone-str
                      session-indicator
                      label-str
                      "\n")))
    (when needs-attention
      (setq issue-line (propertize issue-line 'face
                                   (if (eq claude-status 'waiting)
                                       '(:background "#3E2723")
                                     '(:background "#2E3B2E")))))
    (concat
     (propertize issue-line
                 'orchard-issue issue
                 'orchard-worktree wt
                 'help-echo (format "#%d: %s" number (or title "")))
     (when wt
       (propertize (orchard--format-branch-inline wt current-path)
                   'orchard-issue issue
                   'orchard-worktree wt
                   'help-echo (format "#%d: %s" number (or title "")))))))

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
;;; Previous Sessions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--load-previous-sessions ()
  "Load previous session paths from saved file.
Only loads once per Emacs session, clears the file after loading."
  (unless orchard--previous-sessions-loaded
    (setq orchard--previous-sessions-loaded t)
    (when (fboundp 'orchard--load-claude-sessions)
      (let ((paths (orchard--load-claude-sessions)))
        (when paths
          ;; Filter to valid directories
          (setq orchard--previous-session-paths
                (cl-remove-if-not #'file-directory-p paths))
          ;; Clear the file so we don't keep showing old sessions
          (when (and (boundp 'orchard--claude-sessions-file)
                     (file-exists-p orchard--claude-sessions-file))
            (delete-file orchard--claude-sessions-file)))))))

(defun orchard--get-previous-session-items (worktrees)
  "Get list of (worktree . issue) pairs for previously active sessions.
WORKTREES is the list of current worktrees."
  (when orchard--previous-session-paths
    (let (items)
      (dolist (path orchard--previous-session-paths)
        ;; Find matching worktree
        (let ((wt (cl-find-if
                   (lambda (w)
                     (string= (file-name-as-directory (alist-get 'path w))
                              (file-name-as-directory path)))
                   worktrees)))
          (when wt
            ;; Find linked issue if any
            (let* ((issue-num (orchard--get-worktree-issue path (alist-get 'branch wt)))
                   (issue (when issue-num
                            (cl-find-if (lambda (i) (= (alist-get 'number i) issue-num))
                                        (orchard--get-open-issues)))))
              (push (cons wt issue) items)))))
      (nreverse items))))

(defun orchard--clear-previous-sessions ()
  "Clear the previously active sessions display."
  (interactive)
  (setq orchard--previous-session-paths nil)
  (when (eq major-mode 'orchard-mode)
    (orchard-refresh))
  (message "Cleared previous sessions"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-dashboard ()
  "Format the Orchard dashboard with issue-centric layout."
  ;; Load previous sessions on first dashboard open
  (orchard--load-previous-sessions)
  ;; Ensure essential caches (issues + open PRs) for fast render.
  ;; Staging/merged/closed load on force-refresh (G).
  (orchard--ensure-essential-caches)
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
                              ;; Filter staging BUT always keep P1 (high priority)
                              (setq issues (cl-remove-if
                                            (lambda (i)
                                              (and (orchard--issue-staging-p i)
                                                   (not (orchard--issue-has-exact-label-p i "P1"))))
                                            issues)))
                            (when orchard--label-filter
                              (setq issues (cl-remove-if-not
                                            (lambda (i) (orchard--issue-has-exact-label-p i orchard--label-filter))
                                            issues)))
                            (when orchard--milestone-filter
                              (setq issues (cl-remove-if-not
                                            (lambda (i) (orchard--issue-has-milestone-p i orchard--milestone-filter))
                                            issues)))
                            (when orchard--text-filter
                              ;; Use search API to find issues (including closed)
                              (let ((search-results (orchard--search-issues orchard--text-filter)))
                                (if search-results
                                    ;; Use search results, dedupe with open issues
                                    (let ((seen-numbers (make-hash-table :test 'eq)))
                                      (dolist (i search-results)
                                        (puthash (alist-get 'number i) i seen-numbers))
                                      ;; Also include any open issues that match by title
                                      (dolist (i issues)
                                        (let ((title (or (alist-get 'title i) "")))
                                          (when (string-match-p (regexp-quote orchard--text-filter) title)
                                            (puthash (alist-get 'number i) i seen-numbers))))
                                      (setq issues (hash-table-values seen-numbers)))
                                  ;; Fallback: filter open issues by title
                                  (setq issues (cl-remove-if-not
                                                (lambda (i)
                                                  (let ((title (or (alist-get 'title i) "")))
                                                    (string-match-p (regexp-quote orchard--text-filter)
                                                                    title)))
                                                issues)))))
                            (setq issues (cl-remove-if
                                          (lambda (i) (orchard--issue-hidden-p (alist-get 'number i)))
                                          issues))
                            issues))
         (visible-worktrees (let ((wts (cl-remove-if
                                        (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                                        worktrees)))
                              ;; Also filter worktrees by text
                              (when orchard--text-filter
                                (setq wts (cl-remove-if-not
                                           (lambda (wt)
                                             (let ((branch (or (alist-get 'branch wt) "")))
                                               (string-match-p (regexp-quote orchard--text-filter)
                                                               branch)))
                                           wts)))
                              wts))
         (categories (orchard--categorize-issues filtered-issues visible-worktrees))
         (uat-urgent (alist-get 'uat-urgent categories))
         (claude-waiting (alist-get 'claude-waiting categories))
         (uat-failed (alist-get 'uat-failed categories))
         (ready-to-deploy (alist-get 'ready-to-deploy categories))
         (next-up (alist-get 'next-up categories))
         (needs-analysis (alist-get 'needs-analysis categories))
         (in-flight (alist-get 'in-flight categories))
         (stale-work (alist-get 'stale-work categories))
         (pr-failing (alist-get 'pr-failing categories))
         (pr-review (alist-get 'pr-review categories))
         (pr-approved (alist-get 'pr-approved categories))
         (qa-verify (alist-get 'qa-verify categories))
         (done (alist-get 'done categories))
         (backlog (alist-get 'backlog categories))
         (orphan-worktrees (cl-remove-if
                            (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                            (orchard--get-orphan-worktrees visible-worktrees)))
         (previous-sessions (orchard--get-previous-session-items worktrees))
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
     (when orchard--text-filter
       (propertize (format "  /%s" orchard--text-filter)
                   'face '(:foreground "#C678DD" :weight bold)))
     (when orchard--label-filter
       (propertize (format "  🏷 %s" orchard--label-filter)
                   'face '(:foreground "#61AFEF" :weight bold)))
     (when orchard--milestone-filter
       (propertize (format "  🎯 %s" orchard--milestone-filter)
                   'face '(:foreground "#C678DD" :weight bold)))
     (when (and (> staging-count 0) orchard--hide-staging-issues)
       (propertize (format "  (%d staging hidden)" staging-count)
                   'face 'font-lock-comment-face))
     (when (> hidden-count 0)
       (propertize (format "  [%d hidden]" hidden-count)
                   'face 'font-lock-comment-face))
     (when previous-sessions
       (propertize (format "  ⏮ %d from last session" (length previous-sessions))
                   'face '(:foreground "#C678DD" :weight bold)))
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
     ;; Milestone bar
     (orchard--format-milestone-bar)
     ;; Sections - Workflow pipeline order
     ;; PREVIOUSLY ACTIVE - sessions from last Emacs session (shows until dismissed)
     (when previous-sessions
       (concat
        (orchard--format-section-header "⏮ PREVIOUSLY ACTIVE" (length previous-sessions)
                                        "from last session - X to clear" 'previous-sessions)
        (unless (orchard--section-collapsed-p 'previous-sessions)
          (mapconcat (lambda (pair)
                       (let ((wt (car pair))
                             (issue (cdr pair)))
                         (if issue
                             (orchard--format-issue-with-branch (cons issue wt) current-path)
                           ;; No linked issue - just show the worktree
                           (orchard--format-orphan-worktree wt current-path))))
                     previous-sessions ""))))
     ;; UAT URGENT - P1 with recent daly comment = HIGHEST priority
     (when (and uat-urgent (orchard--section-visible-p 'uat-urgent))
       (concat
        (orchard--format-section-header "🔥 UAT URGENT" (length uat-urgent)
                                        (format "P1 + %s comment today!" orchard-uat-commenter)
                                        'uat-urgent)
        (unless (orchard--section-collapsed-p 'uat-urgent)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     uat-urgent ""))))
     ;; CLAUDE WAITING - very high priority, Claude needs input
     (when (and claude-waiting (orchard--section-visible-p 'claude-waiting))
       (concat
        (orchard--format-section-header "🔔 CLAUDE WAITING" (length claude-waiting) "needs your input!" 'claude-waiting)
        (unless (orchard--section-collapsed-p 'claude-waiting)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     claude-waiting ""))))
     ;; UAT FAILED - P1 label, needs revisiting
     (when (and uat-failed (orchard--section-visible-p 'uat-failed))
       (concat
        (orchard--format-section-header "🚨 UAT FAILED" (length uat-failed) "P1 - needs revisiting" 'uat-failed)
        (unless (orchard--section-collapsed-p 'uat-failed)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     uat-failed ""))))
     ;; READY TO DEPLOY - merged to dev, not on staging
     (when (and ready-to-deploy (orchard--section-visible-p 'ready-to-deploy))
       (concat
        (orchard--format-section-header "🚀 READY TO DEPLOY" (length ready-to-deploy) "merged to dev, not on staging" 'ready-to-deploy)
        (unless (orchard--section-collapsed-p 'ready-to-deploy)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     ready-to-deploy ""))))
     ;; NEXT UP - active today but no worktree (not started yet)
     (when (and next-up (orchard--section-visible-p 'next-up))
       (concat
        (orchard--format-section-header "📋 NEXT UP" (length next-up) "not started" 'next-up)
        (unless (orchard--section-collapsed-p 'next-up)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     (orchard--sort-by-priority next-up) ""))))
     ;; NEEDS ANALYSIS - has worktree, no plan
     (when (and needs-analysis (orchard--section-visible-p 'needs-analysis))
       (concat
        (orchard--format-section-header "📋 ANALYZED" (length needs-analysis) "worktree, no commits" 'needs-analysis)
        (unless (orchard--section-collapsed-p 'needs-analysis)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     needs-analysis ""))))
     ;; IN FLIGHT - has plan, no PR, active (touched in last 3 days)
     (when (and in-flight (orchard--section-visible-p 'in-flight))
       (concat
        (orchard--format-section-header "🚧 IN FLIGHT" (length in-flight) "active, no PR" 'in-flight)
        (unless (orchard--section-collapsed-p 'in-flight)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     in-flight ""))))
     ;; STALE WORK - has plan, no PR, not touched in 3+ days
     (when (and stale-work (orchard--section-visible-p 'stale-work))
       (concat
        (orchard--format-section-header "💤 STALE" (length stale-work) "no activity 3+ days" 'stale-work)
        (unless (orchard--section-collapsed-p 'stale-work)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     stale-work ""))))
     ;; PR FAILING - has PR, CI failing
     (when (and pr-failing (orchard--section-visible-p 'pr-failing))
       (concat
        (orchard--format-section-header "❌ PR FAILING CI" (length pr-failing) "fix needed" 'pr-failing)
        (unless (orchard--section-collapsed-p 'pr-failing)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     pr-failing ""))))
     ;; PR NEEDS REVIEW - has PR, CI passing, needs review
     (when (and pr-review (orchard--section-visible-p 'pr-review))
       (concat
        (orchard--format-section-header "👀 PR NEEDS REVIEW" (length pr-review) "CI passing" 'pr-review)
        (unless (orchard--section-collapsed-p 'pr-review)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     pr-review ""))))
     ;; PR APPROVED - ready to merge
     (when (and pr-approved (orchard--section-visible-p 'pr-approved))
       (concat
        (orchard--format-section-header "✅ PR APPROVED" (length pr-approved) "ready to merge" 'pr-approved)
        (unless (orchard--section-collapsed-p 'pr-approved)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     pr-approved ""))))
     ;; QA/VERIFY - merged but issue open
     (when (and qa-verify (orchard--section-visible-p 'qa-verify))
       (concat
        (orchard--format-section-header "🔍 QA/VERIFY" (length qa-verify) "merged, issue open" 'qa-verify)
        (unless (orchard--section-collapsed-p 'qa-verify)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     qa-verify ""))))
     ;; DONE - ready to archive
     (when (and done (orchard--section-visible-p 'done))
       (concat
        (orchard--format-section-header "✓ DONE" (length done) "ready to archive" 'done)
        (unless (orchard--section-collapsed-p 'done)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     done ""))))
     ;; BACKLOG - older issues without worktrees (collapsed by default)
     (when (and backlog (orchard--section-visible-p 'backlog))
       (concat
        (orchard--format-section-header "📦 BACKLOG" (length backlog) "no worktree" 'backlog)
        (unless (orchard--section-collapsed-p 'backlog)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     backlog ""))))
     (when (and orphan-worktrees (orchard--section-visible-p 'unlinked))
       (concat
        (orchard--format-section-header "UNLINKED BRANCHES" (length orphan-worktrees) nil 'unlinked)
        (unless (orchard--section-collapsed-p 'unlinked)
          (mapconcat (lambda (wt)
                       (orchard--format-orphan-worktree wt current-path))
                     orphan-worktrees ""))))
     (when (orchard--section-visible-p 'recent-sessions)
       (orchard--format-recent-sessions))
     ;; Research section
     (when (orchard--section-visible-p 'research)
       (orchard--format-research-section))
     ;; Empty state
     (when (and (null uat-urgent) (null claude-waiting) (null uat-failed) (null next-up) (null needs-analysis) (null in-flight)
                (null stale-work) (null pr-failing) (null pr-review) (null pr-approved)
                (null qa-verify) (null done) (null backlog) (null orphan-worktrees)
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

(defun orchard--get-research-at-point ()
  "Get research info plist at point."
  (or (get-text-property (point) 'orchard-research)
      (save-excursion
        (beginning-of-line)
        (let ((end (line-end-position)) result)
          (while (and (< (point) end) (not result))
            (setq result (get-text-property (point) 'orchard-research))
            (forward-char 1))
          result))))

(defun orchard-show-at-point ()
  "Show full details of issue or worktree at point in echo area.
Use this to see the complete issue title without truncation."
  (interactive)
  (let ((issue (orchard--get-issue-at-point))
        (wt (orchard--get-worktree-at-point))
        (research (orchard--get-research-at-point)))
    (cond
     (issue
      (let* ((number (alist-get 'number issue))
             (title (alist-get 'title issue))
             (url (alist-get 'url issue))
             (labels (append (alist-get 'labels issue) nil))
             (label-names (mapcar (lambda (l) (alist-get 'name l)) labels)))
        (message "#%d: %s%s%s"
                 number
                 (or title "")
                 (if label-names (format " [%s]" (string-join label-names ", ")) "")
                 (if url (format "\n%s" url) ""))))
     (wt
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (desc (alist-get 'description wt)))
        (message "%s\n%s%s"
                 (or branch "(detached)")
                 path
                 (if desc (format "\n%s" desc) ""))))
     (research
      (let* ((name (plist-get research :name))
             (path (plist-get research :path))
             (context (plist-get research :context)))
        (message "🔬 %s: %s%s"
                 name
                 path
                 (if context (format "\n\"%s\"" context) ""))))
     (t
      (message "No issue or worktree at point")))))

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
    ('working (memq section '(uat-urgent claude-waiting uat-failed ready-to-deploy next-up needs-analysis in-flight stale-work pr-failing pr-review pr-approved unlinked)))
    ('next (memq section '(uat-urgent claude-waiting uat-failed next-up backlog)))
    ('qa (memq section '(uat-urgent qa-verify uat-failed)))  ; P1/UAT failures show in QA view too
    ('progress (memq section '(uat-urgent claude-waiting uat-failed needs-analysis in-flight stale-work pr-failing pr-review pr-approved)))
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
;;; Research Section
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--format-research-item (research-info)
  "Format a single RESEARCH-INFO plist for display."
  (let* ((name (plist-get research-info :name))
         (path (plist-get research-info :path))
         (context (plist-get research-info :context))
         (session (plist-get research-info :session-info))
         (msg-count (when session (plist-get session :message-count)))
         (modified (when session (plist-get session :modified)))
         (rel-time (when modified (orchard--format-relative-time modified)))
         (claude-buf (orchard--claude-buffer-for-path path))
         (claude-status (when claude-buf (orchard--claude-status claude-buf))))
    (concat
     (propertize
      (concat
       "   🔬 "
       (propertize name 'face '(:foreground "#C678DD" :weight bold))
       (when (and msg-count (> msg-count 0))
         (concat " "
                 (propertize (format "💾%d" msg-count) 'face '(:foreground "#61AFEF"))
                 (when rel-time
                   (concat "/" (propertize rel-time 'face '(:foreground "#E5C07B"))))))
       (pcase claude-status
         ('waiting (propertize " ⏳WAIT" 'face '(:foreground "#E06C75" :weight bold)))
         ('idle (propertize " ✓DONE" 'face '(:foreground "#E5C07B" :weight bold)))
         ('active (propertize " ⟳" 'face '(:foreground "#61AFEF")))
         (_ ""))
       "\n")
      'orchard-research research-info)
     (when context
       (propertize
        (concat "      \""
                (propertize (truncate-string-to-width context 55 nil nil "…")
                            'face 'font-lock-comment-face)
                "\"\n")
        'orchard-research research-info)))))

(defun orchard--format-research-section ()
  "Format the RESEARCH section for the dashboard."
  (let ((research-dirs (orchard--get-research-dirs)))
    (when research-dirs
      (concat
       (orchard--format-section-header "RESEARCH" (length research-dirs) nil 'research)
       (unless (orchard--section-collapsed-p 'research)
         (mapconcat #'orchard--format-research-item research-dirs ""))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Main Entry Point
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--checked-saved-sessions nil
  "Non-nil if we've already checked for saved Claude sessions this Emacs session.")

(defun orchard--maybe-prompt-resume ()
  "No-op - resume disabled. Sessions shown in dashboard instead."
  nil)

(defun orchard--find-empty-window ()
  "Find an empty window (scratch, Messages, doom dashboard).
Returns the window or nil."
  (cl-find-if
   (lambda (win)
     (let ((buf-name (buffer-name (window-buffer win))))
       (or (string= buf-name "*scratch*")
           (string= buf-name "*Messages*")
           (string= buf-name "*doom*")
           (string-prefix-p " " buf-name)))) ; internal buffers
   (window-list nil 'no-mini)))

(defun orchard ()
  "Open the Orchard dashboard.
This is the main entry point for the worktree manager."
  (interactive)
  (let ((buf (get-buffer-create "*Orchard*")))
    (with-current-buffer buf
      (unless (eq major-mode 'orchard-mode)
        (orchard-mode))
      (orchard-refresh))
    ;; Prefer empty windows, avoid Claude windows
    (let ((empty-win (orchard--find-empty-window))
          (existing-win (get-buffer-window buf)))
      (cond
       ;; Already displayed - just select it
       (existing-win (select-window existing-win))
       ;; Found empty window - use it
       (empty-win
        (set-window-buffer empty-win buf)
        (select-window empty-win))
       ;; Fallback - use pop-to-buffer
       (t (pop-to-buffer buf))))
    ;; Check for saved sessions on first open (run in background)
    (orchard--maybe-prompt-resume)
    ;; Return the buffer (important for initial-buffer-choice)
    buf))

(defun orchard-force-refresh ()
  "Force refresh with fresh data from GitHub (bypasses all caches).
Loads everything: staging, merged, closed, issues, PRs."
  (interactive)
  (setq orchard--worktrees-cache nil
        orchard--worktrees-cache-time nil
        orchard--issues-cache nil
        orchard--issues-cache-time nil
        orchard--pr-status-cache nil
        orchard--pr-status-cache-time nil
        orchard--merged-branches-cache nil
        orchard--merged-branches-cache-time nil
        orchard--closed-issues-cache nil
        orchard--closed-issues-cache-time nil
        orchard--staging-fetched-time nil)
  (let ((orchard--inhibit-cache-refresh nil))
    (orchard--ensure-all-caches))
  (orchard-refresh)
  (message "Orchard refreshed with fresh data"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Refresh
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-refresh ()
  "Refresh the dashboard using cached data.
On first open (when caches are empty), allows initial fetch.
Subsequent refreshes use cached data for instant response."
  (interactive)
  (when (fboundp 'ghq--cleanup-stale-ports)
    (ghq--cleanup-stale-ports))
  (when (eq major-mode 'orchard-mode)
    ;; Only inhibit cache refresh if we already have cached data
    ;; On first open, allow the initial fetch to happen
    (let ((inhibit-read-only t)
          (orchard--inhibit-cache-refresh (and orchard--issues-cache
                                               orchard--worktrees-cache))
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (orchard--format-dashboard))
      (goto-char (point-min))
      (forward-line (1- (min line (count-lines (point-min) (point-max))))))))

(provide 'orchard-dashboard)
;;; orchard-dashboard.el ends here
