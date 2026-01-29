;;; orchard-cache.el --- Orchard caching and persistence -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains all caching logic:
;; - Claude session persistence
;; - Merged branch detection
;; - GitHub issues cache
;; - State persistence (dev ownership, hidden items)

(require 'json)
(require 'orchard-vars)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard--get-repo-root "orchard-worktree")
(declare-function orchard--get-worktrees "orchard-worktree")
(declare-function orchard-refresh "orchard-dashboard")
(declare-function orchard--refresh-if-visible "orchard-actions")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Stage Tracking Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard-stages
  '(requirements in-progress ready-to-test testing ready-to-pr pr-open merged)
  "Ordered list of feature stages.")

(defvar orchard--state nil
  "In-memory state: plist with :dev-owner, :stage-overrides, :test-results.")

(defvar orchard--dev-owner nil
  "Path of worktree currently owning dev mode, or nil.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Cache Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--merged-branches-cache nil
  "Cache of merged branch names from GitHub.
Alist of (branch-name . merge-time) pairs.")

(defvar orchard--merged-branches-cache-time nil
  "Time when merged branches cache was last updated.")

(defvar orchard--worktrees-cache nil
  "Cache of enriched worktrees data.")

(defvar orchard--worktrees-cache-time nil
  "Time when worktrees cache was last updated.")

(defvar orchard--inhibit-cache-refresh nil
  "When non-nil, skip auto-refresh of caches.
Used by quick refresh to ensure instant response.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Session Persistence (via ~/.claude/projects/)
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Claude Code stores session history in ~/.claude/projects/<encoded-path>/
;; We can use this to detect resumable sessions even after Emacs restart.

(defvar orchard--claude-sessions-cache nil
  "Cache of Claude session info per worktree path.
Alist of (path . session-info) where session-info is a plist.")

(defvar orchard--claude-sessions-cache-time nil
  "Time when sessions cache was last updated.")

(defun orchard--encode-path-for-claude (path)
  "Encode PATH to Claude's project directory format.
Replaces / with - and removes leading -."
  (let ((encoded (replace-regexp-in-string "/" "-" (expand-file-name path))))
    ;; Claude uses the path as-is with / replaced by -
    encoded))

(defun orchard--get-claude-project-dir (path)
  "Get Claude project directory for worktree at PATH, or nil if none."
  (let* ((encoded (orchard--encode-path-for-claude path))
         (projects-dir (expand-file-name "~/.claude/projects/"))
         (project-dir (expand-file-name encoded projects-dir)))
    (when (file-directory-p project-dir)
      project-dir)))

(defun orchard--get-claude-session-info (path)
  "Get Claude session info for worktree at PATH.
Returns plist with :has-session, :message-count, :modified, :summary, :session-id
or nil if no session exists."
  (when-let ((project-dir (orchard--get-claude-project-dir path)))
    (let ((index-file (expand-file-name "sessions-index.json" project-dir)))
      (when (file-exists-p index-file)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (data (json-read-file index-file))
                   (entries (alist-get 'entries data))
                   ;; Get most recent session (last in list, or sort by modified)
                   (latest (car (last entries))))
              (when latest
                (list :has-session t
                      :session-id (alist-get 'sessionId latest)
                      :message-count (or (alist-get 'messageCount latest) 0)
                      :modified (alist-get 'modified latest)
                      :summary (alist-get 'summary latest)
                      :first-prompt (alist-get 'firstPrompt latest))))
          (error nil))))))

(defun orchard--refresh-claude-sessions-cache ()
  "Refresh the Claude sessions cache for all worktrees."
  (let ((worktrees (orchard--get-worktrees t)))  ; include hidden
    (setq orchard--claude-sessions-cache
          (cl-loop for wt in worktrees
                   for path = (alist-get 'path wt)
                   for info = (orchard--get-claude-session-info path)
                   when info
                   collect (cons path info)))
    (setq orchard--claude-sessions-cache-time (current-time))))

(defun orchard--ensure-claude-sessions-cache ()
  "Ensure Claude sessions cache is fresh."
  (unless orchard--inhibit-cache-refresh
    (when (or (null orchard--claude-sessions-cache-time)
              (> (float-time (time-subtract (current-time)
                                            orchard--claude-sessions-cache-time))
                 orchard-claude-sessions-cache-ttl))
      (orchard--refresh-claude-sessions-cache))))

(defun orchard--worktree-has-claude-session-p (path)
  "Check if worktree at PATH has a resumable Claude session."
  (orchard--ensure-claude-sessions-cache)
  (assoc path orchard--claude-sessions-cache))

(defun orchard--get-worktree-session-info (path)
  "Get cached Claude session info for worktree at PATH."
  (orchard--ensure-claude-sessions-cache)
  (cdr (assoc path orchard--claude-sessions-cache)))

(defun orchard--format-relative-time (iso-time)
  "Format ISO-TIME string as relative time (e.g., '2h ago', '3d ago')."
  (when iso-time
    (condition-case nil
        (let* ((parsed (parse-iso8601-time-string iso-time))
               (secs-ago (float-time (time-subtract (current-time) parsed)))
               (mins (/ secs-ago 60))
               (hours (/ mins 60))
               (days (/ hours 24)))
          (cond
           ((< mins 1) "now")
           ((< mins 60) (format "%dm" (truncate mins)))
           ((< hours 24) (format "%dh" (truncate hours)))
           ((< days 7) (format "%dd" (truncate days)))
           (t (format "%dw" (truncate (/ days 7))))))
      (error nil))))

(defun orchard--format-session-indicator (path)
  "Format session indicator for worktree at PATH.
Returns indicator string or empty string."
  (when-let ((info (orchard--get-worktree-session-info path)))
    (let* ((msg-count (plist-get info :message-count))
           (modified (plist-get info :modified))
           (rel-time (orchard--format-relative-time modified)))
      (propertize
       (if rel-time
           (format " 💾%d/%s" msg-count rel-time)
         (format " 💾%d" msg-count))
       'face '(:foreground "#61AFEF")
       'help-echo (format "Claude session: %d messages, %s"
                          msg-count (or rel-time "unknown"))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Merged Branch Detection (via GitHub PR)
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--refresh-merged-cache ()
  "Refresh the merged branches cache from GitHub.
Uses `gh pr list --state merged` to get recently merged PRs."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (condition-case err
            (let* ((output (shell-command-to-string
                            "gh pr list --state merged --limit 100 --json headRefName,mergedAt 2>/dev/null"))
                   (json (ignore-errors (json-read-from-string output))))
              (when (vectorp json)
                (setq orchard--merged-branches-cache
                      (mapcar (lambda (pr)
                                (cons (alist-get 'headRefName pr)
                                      (alist-get 'mergedAt pr)))
                              json))
                (setq orchard--merged-branches-cache-time (current-time))
                (message "Refreshed merged branches cache: %d entries"
                         (length orchard--merged-branches-cache))))
          (error
           (message "Failed to refresh merged cache: %s" err)))))))

(defun orchard--ensure-merged-cache ()
  "Ensure merged branches cache is fresh, refresh if stale.
Does nothing if `orchard--inhibit-cache-refresh' is non-nil."
  (unless orchard--inhibit-cache-refresh
    (when (or (null orchard--merged-branches-cache-time)
              (> (float-time (time-subtract (current-time)
                                            orchard--merged-branches-cache-time))
                 orchard-merged-cache-ttl))
      (orchard--refresh-merged-cache))))

(defun orchard--branch-merged-p (branch)
  "Check if BRANCH has been merged to upstream via PR (from cache).
Returns the merge timestamp if merged, nil otherwise.
Does NOT auto-refresh - use `orchard-force-refresh' (G) to fetch from GitHub."
  (cdr (assoc branch orchard--merged-branches-cache)))

(defun orchard--issue-branch-merged-p (issue-number)
  "Check if any branch for ISSUE-NUMBER has been merged.
Searches merged-branches-cache for branches matching patterns like
FEATURE/123-description or BUGFIX-123-description.
Returns the merge timestamp if found, nil otherwise."
  (when (and issue-number (> issue-number 0))
    (let ((pattern (format "^[A-Za-z]+[/-]%d-" issue-number)))
      (cl-loop for (branch . merged-at) in orchard--merged-branches-cache
               when (string-match-p pattern branch)
               return merged-at))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Open PR Status Cache
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--pr-status-cache nil
  "Cache of open PR status by branch name.
Alist of (branch . status-plist) where status-plist has:
  :pr-number, :mergeable, :ci-status, :review-decision")

(defvar orchard--pr-status-cache-time nil
  "Time when PR status cache was last updated.")

(defun orchard--summarize-ci-status (checks)
  "Summarize CI status from CHECKS array.
Returns: 'success, 'failure, 'pending, or nil."
  (when (and checks (> (length checks) 0))
    (let ((dominated-statuses (make-hash-table :test 'equal))
          (dominated-check-runs (make-hash-table :test 'equal))
          has-failure has-pending)
      ;; Look at all checks, considering only most recent of each type/name
      (dolist (check (append checks nil))
        (let ((status (alist-get 'status check))
              (conclusion (alist-get 'conclusion check))
              (typename (alist-get '__typename check)))
          (cond
           ;; Still running
           ((and (equal status "IN_PROGRESS")
                 (equal typename "CheckRun"))
            (setq has-pending t))
           ;; Completed - check conclusion
           ((equal status "COMPLETED")
            (cond
             ((member conclusion '("FAILURE" "TIMED_OUT" "CANCELLED"))
              (setq has-failure t))
             ((equal conclusion "SUCCESS")
              nil)))  ; success, keep going
           ;; StatusContext uses 'state' not 'conclusion'
           ((equal typename "StatusContext")
            (let ((state (alist-get 'state check)))
              (cond
               ((equal state "FAILURE") (setq has-failure t))
               ((equal state "PENDING") (setq has-pending t))))))))
      (cond
       (has-failure 'failure)
       (has-pending 'pending)
       (t 'success)))))

(defun orchard--refresh-pr-status-cache ()
  "Refresh the open PR status cache from GitHub."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (condition-case err
            (let* ((output (shell-command-to-string
                            "gh pr list --state open --limit 50 --json number,headRefName,mergeable,reviewDecision,statusCheckRollup 2>/dev/null"))
                   (json (ignore-errors (json-read-from-string output))))
              (when (vectorp json)
                (setq orchard--pr-status-cache
                      (mapcar (lambda (pr)
                                (let ((branch (alist-get 'headRefName pr))
                                      (checks (alist-get 'statusCheckRollup pr)))
                                  (cons branch
                                        (list :pr-number (alist-get 'number pr)
                                              :mergeable (alist-get 'mergeable pr)
                                              :ci-status (orchard--summarize-ci-status checks)
                                              :review-decision (alist-get 'reviewDecision pr)))))
                              (append json nil)))
                (setq orchard--pr-status-cache-time (current-time))))
          (error
           (message "Failed to refresh PR status cache: %s" err)))))))

(defun orchard--ensure-pr-status-cache ()
  "Ensure PR status cache is fresh, refresh if stale."
  (unless orchard--inhibit-cache-refresh
    (when (or (null orchard--pr-status-cache-time)
              (> (float-time (time-subtract (current-time)
                                            orchard--pr-status-cache-time))
                 orchard-issues-cache-ttl))  ; reuse issues TTL (5 min)
      (orchard--refresh-pr-status-cache))))

(defun orchard--get-pr-status (branch)
  "Get PR status for BRANCH from cache.
Returns plist with :pr-number, :mergeable, :ci-status, :review-decision or nil."
  (cdr (assoc branch orchard--pr-status-cache)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; GitHub Issues Cache
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--issues-cache nil
  "Cache of open GitHub issues.
Vector of issue alists with keys: number, title, labels, assignees, url.")

(defvar orchard--issues-cache-time nil
  "Time when issues cache was last updated.")

(defvar orchard--closed-issues-cache nil
  "Cache of closed issue numbers for archival detection.
Alist of (issue-number . closed-at) pairs.")

(defvar orchard--closed-issues-cache-time nil
  "Time when closed issues cache was last updated.")

(defvar orchard--hide-staging-issues nil
  "When non-nil, hide issues with 'staging' label from the dashboard.")

(defvar orchard--label-filter nil
  "When non-nil, only show issues with this label (exact match).")

(defvar orchard--text-filter nil
  "When non-nil, only show issues/branches matching this text (case-insensitive).")

(defvar orchard--current-view 'working
  "Current dashboard view preset.
Possible values:
  'working  - Hide QA/VERIFY and DONE sections (default)
  'all      - Show all sections
  'next     - Show only UP NEXT section
  'qa       - Show only QA/VERIFY section
  'progress - Show only IN PROGRESS section")

(defvar orchard--collapsed-sections '(backlog)
  "List of section names that are collapsed.
Possible values: 'new-issues, 'needs-analysis, 'in-flight, 'pr-failing,
'pr-review, 'pr-approved, 'qa-verify, 'done, 'backlog, 'unlinked.
BACKLOG is collapsed by default.")

(defun orchard--issue-has-label-p (issue label-pattern)
  "Return t if ISSUE has a label matching LABEL-PATTERN (case-insensitive)."
  (let ((labels (alist-get 'labels issue)))
    (cl-some (lambda (l)
               (string-match-p label-pattern (downcase (or (alist-get 'name l) ""))))
             labels)))

(defun orchard--issue-has-exact-label-p (issue label-name)
  "Return t if ISSUE has a label with exact LABEL-NAME (case-insensitive)."
  (let ((labels (alist-get 'labels issue)))
    (cl-some (lambda (l)
               (string-equal (downcase (or (alist-get 'name l) ""))
                            (downcase label-name)))
             labels)))

(defun orchard--issue-staging-p (issue)
  "Return t if ISSUE has 'staging' label."
  (orchard--issue-has-label-p issue "staging"))

(defun orchard--get-all-labels ()
  "Get list of all unique label names from cached issues."
  (let ((labels-set (make-hash-table :test 'equal)))
    (dolist (issue (orchard--get-open-issues))
      ;; Labels come as vector from JSON, convert to list for dolist
      (dolist (label (append (alist-get 'labels issue) nil))
        (puthash (alist-get 'name label) t labels-set)))
    (sort (hash-table-keys labels-set) #'string<)))

(defun orchard--refresh-issues-cache ()
  "Refresh the open issues cache from GitHub."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (condition-case err
            (let* ((output (shell-command-to-string
                            "gh issue list --state open --limit 50 --json number,title,labels,assignees,url,createdAt 2>/dev/null"))
                   (json (ignore-errors (json-read-from-string output))))
              (when (vectorp json)
                (setq orchard--issues-cache json)
                (setq orchard--issues-cache-time (current-time))
                (message "Refreshed issues cache: %d open issues"
                         (length orchard--issues-cache))))
          (error
           (message "Failed to refresh issues cache: %s" err)))))))

(defun orchard--issue-recent-p (issue &optional days)
  "Return t if ISSUE was created within DAYS (default 7)."
  (let* ((days (or days 7))
         (created (alist-get 'createdAt issue)))
    (when created
      (condition-case nil
          (let* ((created-time (date-to-time created))
                 (age-seconds (float-time (time-subtract (current-time) created-time)))
                 (age-days (/ age-seconds 86400)))
            (< age-days days))
        (error nil)))))

(defun orchard--ensure-issues-cache ()
  "Ensure issues cache is fresh, refresh if stale.
Does nothing if `orchard--inhibit-cache-refresh' is non-nil."
  (unless orchard--inhibit-cache-refresh
    (when (or (null orchard--issues-cache-time)
              (> (float-time (time-subtract (current-time)
                                            orchard--issues-cache-time))
                 orchard-issues-cache-ttl))
      (orchard--refresh-issues-cache))))

(defun orchard--get-open-issues ()
  "Get list of open GitHub issues from cache.
Auto-refreshes cache if stale or empty (unless inhibited)."
  (orchard--ensure-issues-cache)
  (when (vectorp orchard--issues-cache)
    (append orchard--issues-cache nil)))  ; Convert vector to list

(defun orchard--refresh-closed-issues-cache ()
  "Refresh the closed issues cache from GitHub.
Only fetches recently closed issues for archival detection."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (condition-case err
            (let* ((output (shell-command-to-string
                            "gh issue list --state closed --limit 100 --json number,closedAt 2>/dev/null"))
                   (json (ignore-errors (json-read-from-string output))))
              (when (vectorp json)
                (setq orchard--closed-issues-cache
                      (mapcar (lambda (issue)
                                (cons (alist-get 'number issue)
                                      (alist-get 'closedAt issue)))
                              json))
                (setq orchard--closed-issues-cache-time (current-time))))
          (error
           (message "Failed to refresh closed issues cache: %s" err)))))))

(defun orchard--ensure-closed-issues-cache ()
  "Ensure closed issues cache is fresh.
Does nothing if `orchard--inhibit-cache-refresh' is non-nil."
  (unless orchard--inhibit-cache-refresh
    (when (or (null orchard--closed-issues-cache-time)
              (> (float-time (time-subtract (current-time)
                                            orchard--closed-issues-cache-time))
                 orchard-issues-cache-ttl))
      (orchard--refresh-closed-issues-cache))))

(defun orchard--issue-closed-p (issue-number)
  "Check if ISSUE-NUMBER is closed (from cache, no API call).
Returns t if not in open issues cache."
  (not (orchard--get-issue-by-number issue-number)))

(defun orchard--get-issue-by-number (issue-number)
  "Get issue alist by ISSUE-NUMBER from cache.
Does NOT auto-refresh - use `orchard-force-refresh' (G) to fetch from GitHub."
  (when (vectorp orchard--issues-cache)
    (cl-find-if (lambda (issue)
                  (eq (alist-get 'number issue) issue-number))
                orchard--issues-cache)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Worktree-Issue Linking
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--parse-issue-from-branch (branch)
  "Extract issue number from BRANCH name, or nil.
Parses patterns like:
  FEATURE/42-description -> 42
  FEATURE-42-description -> 42  (hyphen separator)
  FIX/123-bug-fix -> 123
  BUGFIX-123-bug-fix -> 123
  CHORE/7-cleanup -> 7
  feature/42-foo -> 42 (case-insensitive)"
  (when (and branch (string-match "^[A-Za-z]+[/-]\\([0-9]+\\)-" branch))
    (string-to-number (match-string 1 branch))))

(defun orchard--get-worktree-issue (path &optional branch)
  "Get linked GitHub issue number for worktree at PATH.
First checks .github-issue file (persistent link), then falls back to
parsing BRANCH name like FEATURE-123-description."
  (or
   ;; Primary: read from .github-issue file (persists even if branch changes)
   (when path
     (let ((file (expand-file-name ".github-issue" path)))
       (when (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (let ((num (string-to-number (string-trim (buffer-string)))))
             (when (> num 0) num))))))
   ;; Fallback: parse from branch name
   (orchard--parse-issue-from-branch branch)))

(defun orchard--save-worktree-issue (path issue-number)
  "Save ISSUE-NUMBER link for worktree at PATH."
  (let ((file (expand-file-name ".github-issue" path)))
    (with-temp-file file
      (insert (number-to-string issue-number)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; State Persistence
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--load-state ()
  "Load state from state file."
  (when (file-exists-p orchard-state-file)
    (with-temp-buffer
      (insert-file-contents orchard-state-file)
      (ignore-errors
        (setq orchard--state (read (buffer-string))))))
  (setq orchard--dev-owner (plist-get orchard--state :dev-owner)))

(defun orchard--save-state ()
  "Save state to state file."
  (setq orchard--state (plist-put orchard--state :dev-owner orchard--dev-owner))
  (with-temp-file orchard-state-file
    (prin1 orchard--state (current-buffer))))

(defun orchard--set-dev-owner (path)
  "Set PATH as the dev owner. Pass nil to release."
  (setq orchard--dev-owner path)
  (orchard--save-state)
  (orchard--refresh-if-visible))

(defun orchard--get-stage-override (path)
  "Get manual stage override for worktree at PATH."
  (let ((overrides (plist-get orchard--state :stage-overrides)))
    (cdr (assoc path overrides))))

(defun orchard--set-stage-override (path stage)
  "Set manual STAGE override for worktree at PATH."
  (let ((overrides (plist-get orchard--state :stage-overrides)))
    (setq overrides (assoc-delete-all path overrides))
    (when stage
      (push (cons path stage) overrides))
    (setq orchard--state (plist-put orchard--state :stage-overrides overrides))
    (orchard--save-state)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hidden Items
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-hidden ()
  "Get list of hidden worktree paths."
  (plist-get orchard--state :hidden))

(defun orchard--get-hidden-issues ()
  "Get list of hidden issue numbers."
  (plist-get orchard--state :hidden-issues))

(defun orchard--hide-worktree (path)
  "Hide worktree at PATH from orchard display."
  (let ((hidden (plist-get orchard--state :hidden)))
    (unless (member path hidden)
      (push path hidden)
      (setq orchard--state (plist-put orchard--state :hidden hidden))
      (orchard--save-state))))

(defun orchard--hide-issue (issue-number)
  "Hide issue ISSUE-NUMBER from orchard display."
  (let ((hidden (plist-get orchard--state :hidden-issues)))
    (unless (member issue-number hidden)
      (push issue-number hidden)
      (setq orchard--state (plist-put orchard--state :hidden-issues hidden))
      (orchard--save-state))))

(defun orchard--unhide-worktree (path)
  "Unhide worktree at PATH."
  (let ((hidden (plist-get orchard--state :hidden)))
    (setq hidden (delete path hidden))
    (setq orchard--state (plist-put orchard--state :hidden hidden))
    (orchard--save-state)))

(defun orchard--unhide-issue (issue-number)
  "Unhide issue ISSUE-NUMBER."
  (let ((hidden (plist-get orchard--state :hidden-issues)))
    (setq hidden (delete issue-number hidden))
    (setq orchard--state (plist-put orchard--state :hidden-issues hidden))
    (orchard--save-state)))

(defun orchard--worktree-hidden-p (path)
  "Return t if worktree at PATH is hidden."
  (member path (orchard--get-hidden)))

(defun orchard--issue-hidden-p (issue-number)
  "Return t if issue ISSUE-NUMBER is hidden."
  (member issue-number (orchard--get-hidden-issues)))

(defun orchard--is-main-worktree-p (path)
  "Return t if PATH is the main (bare) worktree."
  (let ((repo-root (orchard--get-repo-root)))
    (and repo-root
         (string= (file-name-as-directory (expand-file-name path))
                  (file-name-as-directory (expand-file-name repo-root))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Test Results Storage
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

;;; ════════════════════════════════════════════════════════════════════════════
;;; Research Directory Context
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--load-research-context (path)
  "Load research context from .research-context file in PATH.
Returns the context string or nil if not set."
  (let ((context-file (expand-file-name ".research-context" path)))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (string-trim (buffer-string))))))

(defun orchard--save-research-context (path context)
  "Save research CONTEXT to .research-context file in PATH."
  (let ((context-file (expand-file-name ".research-context" path)))
    (if (or (null context) (string-empty-p context))
        (when (file-exists-p context-file)
          (delete-file context-file))
      (with-temp-file context-file
        (insert context)))))

(defun orchard--get-research-dirs ()
  "Get list of research directories with their info.
Returns list of plists with :name, :path, :context, :session-info."
  (cl-loop for (name . path) in orchard-research-paths
           for expanded = (expand-file-name path)
           when (file-directory-p expanded)
           collect (list :name name
                         :path expanded
                         :context (orchard--load-research-context expanded)
                         :session-info (orchard--get-claude-session-info expanded))))

(provide 'orchard-cache)
;;; orchard-cache.el ends here
