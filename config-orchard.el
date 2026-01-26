;;; config-orchard.el --- Worktree manager with column-locked windows -*- lexical-binding: t; -*-
;;
;; ╔═══════════════════════════════════════════════════════════════════════════╗
;; ║                              ORCHARD                                       ║
;; ║                                                                            ║
;; ║         A place where you tend many branches                               ║
;; ║                                                                            ║
;; ║  Worktree dashboard + column-locked window management                      ║
;; ╚═══════════════════════════════════════════════════════════════════════════╝
;;
;; WINDOW MODEL:
;;   - Dashboard stays in column 0 (leftmost), never replaced
;;   - Each branch gets its own column (1, 2, 3...)
;;   - M-m cycles magit ↔ claude in the SAME window
;;   - Commando takes over the full column, q returns to magit
;;   - Max columns configurable (default 4)
;;
;; KEYBINDINGS:
;;   C-c O O  - Open Orchard dashboard
;;   M-m      - Cycle magit/claude in current column
;;   `        - Commando (in magit)

(require 'transient)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup orchard nil
  "Worktree management with column-locked windows."
  :group 'tools
  :prefix "orchard-")

(defcustom orchard-max-columns 4
  "Maximum number of columns (including dashboard).
When exceeded, new branches replace existing columns."
  :type 'integer
  :group 'orchard)

(defcustom orchard-repo-path nil
  "Path to the main repository (primary worktree).
Set this in your project-specific config."
  :type '(choice directory (const nil))
  :group 'orchard)

(defcustom orchard-worktree-parent (expand-file-name "~/src")
  "Parent directory where worktrees are created."
  :type 'directory
  :group 'orchard)

(defcustom orchard-upstream-branch "upstream/dev"
  "Upstream branch to base new features on."
  :type 'string
  :group 'orchard)

(defcustom orchard-worktree-prefix nil
  "Prefix for worktree directories (e.g., \"myproject\").
With nested structure: worktrees are PREFIX/BRANCH-name.
With flat structure: worktrees are PREFIX--BRANCH-name.
If nil, uses repo name."
  :type '(choice string (const nil))
  :group 'orchard)

(defcustom orchard-nested-worktrees t
  "If non-nil, use nested directory structure for worktrees.
Nested:  ~/src/project/FEATURE-name/
Flat:    ~/src/project--FEATURE-name/ (legacy)"
  :type 'boolean
  :group 'orchard)

(defcustom orchard-branch-types
  '(("FEATURE" . "f")   ; F - new functionality
    ("BUGFIX"  . "b")   ; B - bug fix
    ("CHORE"   . "c")   ; C - maintenance
    ("REFACTOR" . "r")  ; R - code improvement
    ("DOCS"    . "d")   ; D - documentation
    ("EXPERIMENT" . "e") ; E - spike/experiment
    ("TEST"    . "t"))  ; T - test improvements
  "Branch type prefixes with unique first character for tab completion.
Each entry is (PREFIX . shortcut-key)."
  :type '(alist :key-type string :value-type string)
  :group 'orchard)

(defcustom orchard-shared-claude-settings nil
  "Path to shared Claude settings.local.json file, or nil."
  :type '(choice file (const nil))
  :group 'orchard)

(defcustom orchard-shared-claude-commands nil
  "Path to shared Claude commands directory, or nil."
  :type '(choice directory (const nil))
  :group 'orchard)


(defcustom orchard-post-create-hook nil
  "Hook run after creating a new worktree.
Called with the worktree path as argument."
  :type 'hook
  :group 'orchard)

(defcustom orchard-state-file (expand-file-name "~/.orchard-state.eld")
  "File to persist orchard state (dev ownership, stage overrides)."
  :type 'file
  :group 'orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Stage Tracking
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Stages (in order):
;;   requirements  - Has .feature-requirements (gathering requirements)
;;   in-progress   - Default working state
;;   ready-to-test - Has .test-plan.md
;;   testing       - Testicular session active
;;   ready-to-pr   - Tests complete (all passed)
;;   pr-open       - Has .pr-url file
;;   merged        - Branch merged to upstream
;;
;; Resource modes:
;;   full-dev      - Running `make dev` (only one at a time)
;;   frontend-only - Can run multiple in parallel
;;   headless      - No local server needed

(defvar orchard-stages
  '(requirements in-progress ready-to-test testing ready-to-pr pr-open merged)
  "Ordered list of feature stages.")

(defvar orchard--state nil
  "In-memory state: plist with :dev-owner, :stage-overrides, :test-results.")

(defvar orchard--dev-owner nil
  "Path of worktree currently owning dev mode, or nil.")

(defcustom orchard-dev-mode-enforcement 'confirm
  "How to handle dev mode conflicts.
Possible values:
  warn    - Just warn in minibuffer (original behavior)
  confirm - Ask for confirmation before taking dev mode
  block   - Block the command entirely"
  :type '(choice (const :tag "Warn only" warn)
                 (const :tag "Confirm takeover" confirm)
                 (const :tag "Block command" block))
  :group 'orchard)

(defvar orchard--merged-branches-cache nil
  "Cache of merged branch names from GitHub.
Alist of (branch-name . merge-time) pairs.")

(defvar orchard--merged-branches-cache-time nil
  "Time when merged branches cache was last updated.")

(defcustom orchard-merged-cache-ttl 300
  "Time-to-live for merged branches cache in seconds."
  :type 'integer
  :group 'orchard)

(defvar orchard--worktrees-cache nil
  "Cache of enriched worktrees data.")

(defvar orchard--worktrees-cache-time nil
  "Time when worktrees cache was last updated.")

(defcustom orchard-worktrees-cache-ttl 30
  "Time-to-live for worktrees cache in seconds.
Quick refresh (g) uses cached data within TTL."
  :type 'integer
  :group 'orchard)

(defvar orchard--inhibit-cache-refresh nil
  "When non-nil, skip auto-refresh of caches.
Used by quick refresh to ensure instant response.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Status Tracking (via hooks)
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Uses Claude Code's Notification and Stop hooks for reliable status tracking.
;; Much more reliable than text pattern matching in vterm buffers.

(defvar orchard--claude-status-table (make-hash-table :test 'equal)
  "Hash table mapping buffer names to Claude status.
Values: `waiting' (needs input), `idle' (finished), or `active' (working).")

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

(defvar orchard--refresh-timer nil
  "Timer for debounced orchard refresh.")

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

(defcustom orchard-claude-sessions-cache-ttl 60
  "Time-to-live for Claude sessions cache in seconds."
  :type 'integer
  :group 'orchard)

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

;; DISABLED: Hook was causing Emacs hangs - see CLAUDE-HANG-DEBUG.md
;; (with-eval-after-load 'claude-code
;;   (add-hook 'claude-code-event-hook #'orchard--claude-status-hook)
;;   ;; Clean up dead entries periodically
;;   (run-with-idle-timer 60 t #'orchard--claude-status-cleanup))

;;; ────────────────────────────────────────────────────────────────────────────
;;; Background Resume - Hide Claude until /resume completes
;;; ────────────────────────────────────────────────────────────────────────────

(defvar orchard--claude-resuming (make-hash-table :test 'equal)
  "Tracks paths currently resuming: path -> t when resuming, nil when done.")

(defcustom orchard-claude-resume-timeout 60
  "Maximum seconds to wait for resume to complete before showing anyway."
  :type 'integer
  :group 'orchard)

(defun orchard--claude-resuming-p (path)
  "Return t if PATH is currently resuming."
  (gethash (expand-file-name path) orchard--claude-resuming))

(defun orchard--mark-resuming (path)
  "Mark PATH as currently resuming."
  (puthash (expand-file-name path) (current-time) orchard--claude-resuming))

(defun orchard--mark-resume-complete (path)
  "Mark PATH as done resuming."
  (remhash (expand-file-name path) orchard--claude-resuming))

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

(defcustom orchard-issues-cache-ttl 120
  "Time-to-live for issues cache in seconds."
  :type 'integer
  :group 'orchard)

(defvar orchard--hide-staging-issues nil
  "When non-nil, hide issues with 'staging' label from the dashboard.")

(defvar orchard--label-filter nil
  "When non-nil, only show issues with this label (exact match).")

(defvar orchard--current-view 'working
  "Current dashboard view preset.
Possible values:
  'working  - Hide QA/VERIFY and DONE sections (default)
  'all      - Show all sections
  'next     - Show only UP NEXT section
  'qa       - Show only QA/VERIFY section
  'progress - Show only IN PROGRESS section")

(defvar orchard--collapsed-sections nil
  "List of section names that are collapsed.
Possible values: 'up-next, 'in-progress, 'qa-verify, 'done, 'unlinked.")

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
      (dolist (label (alist-get 'labels issue))
        (puthash (alist-get 'name label) t labels-set)))
    (sort (hash-table-keys labels-set) #'string<)))

(defun orchard-filter-by-label ()
  "Filter issues to show only those with a specific label."
  (interactive)
  (let* ((labels (orchard--get-all-labels))
         (selection (completing-read
                     (format "Filter by label%s: "
                             (if orchard--label-filter
                                 (format " (current: %s)" orchard--label-filter)
                               ""))
                     (cons "[Clear filter]" labels)
                     nil t)))
    (if (string= selection "[Clear filter]")
        (progn
          (setq orchard--label-filter nil)
          (message "Label filter cleared"))
      (setq orchard--label-filter selection)
      (message "Filtering by label: %s" selection))
    (orchard--refresh-if-visible)))

(defun orchard-clear-label-filter ()
  "Clear the label filter."
  (interactive)
  (setq orchard--label-filter nil)
  (message "Label filter cleared")
  (orchard--refresh-if-visible))

(defun orchard-toggle-staging-issues ()
  "Toggle visibility of issues with 'staging' label."
  (interactive)
  (setq orchard--hide-staging-issues (not orchard--hide-staging-issues))
  (message "Staging issues: %s" (if orchard--hide-staging-issues "hidden" "visible"))
  (orchard--refresh-if-visible))

(defun orchard-force-refresh ()
  "Force refresh all caches and redraw the dashboard.
Unlike `orchard-refresh', this ignores TTL and fetches fresh data."
  (interactive)
  ;; Clear all cache times to force refresh
  (setq orchard--issues-cache-time nil)
  (setq orchard--closed-issues-cache-time nil)
  (setq orchard--merged-branches-cache-time nil)
  (setq orchard--worktrees-cache-time nil)
  (setq orchard--claude-sessions-cache-time nil)
  ;; Force refresh all caches
  (orchard--refresh-issues-cache)
  (orchard--refresh-closed-issues-cache)
  (orchard--refresh-merged-cache)
  (orchard--refresh-claude-sessions-cache)
  ;; Force refresh worktrees (runs git status for each)
  (orchard--get-worktrees nil t)
  ;; Refresh dashboard
  (orchard-refresh)
  (message "Force refreshed all caches"))

(defun orchard--refresh-issues-cache ()
  "Refresh the open issues cache from GitHub."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (condition-case err
            (let* ((output (shell-command-to-string
                            "gh issue list --state open --limit 50 --json number,title,labels,assignees,url 2>/dev/null"))
                   (json (ignore-errors (json-read-from-string output))))
              (when (vectorp json)
                (setq orchard--issues-cache json)
                (setq orchard--issues-cache-time (current-time))
                (message "Refreshed issues cache: %d open issues"
                         (length orchard--issues-cache))))
          (error
           (message "Failed to refresh issues cache: %s" err)))))))

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
Does NOT auto-refresh - use `orchard-force-refresh' (G) to fetch from GitHub."
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

(defun orchard--get-worktree-issue (_path &optional branch)
  "Get linked GitHub issue number for worktree, parsed from BRANCH name.
Fast: no file I/O, just parses branch name like FEATURE-123-description."
  (orchard--parse-issue-from-branch branch))

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

(defun orchard-force-dev-takeover ()
  "Force take over dev mode regardless of current owner.
Use when `orchard-dev-mode-enforcement' is set to 'block."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let ((path (alist-get 'path wt)))
        (when orchard--dev-owner
          (message "Taking dev mode from %s"
                   (file-name-nondirectory (directory-file-name orchard--dev-owner))))
        (orchard--set-dev-owner path)
        (message "Dev mode now owned by %s"
                 (file-name-nondirectory (directory-file-name path))))
    (user-error "No worktree at point")))

(defun orchard-release-dev-mode ()
  "Release dev mode from current owner."
  (interactive)
  (if orchard--dev-owner
      (progn
        (message "Released dev mode from %s"
                 (file-name-nondirectory (directory-file-name orchard--dev-owner)))
        (orchard--set-dev-owner nil))
    (message "No worktree currently owns dev mode")))

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
;;; Hidden Worktrees (dismiss from orchard without deleting)
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
;;; Stage Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--detect-stage (path &optional branch)
  "Detect the current stage for worktree at PATH with BRANCH name.
Simplified stages derived from GitHub state:
  - merged: branch merged to upstream
  - pr-open: has open PR
  - in-progress: default working state"
  (let ((pr-url-file (expand-file-name ".pr-url" path)))
    (cond
     ;; Check for manual override first
     ((orchard--get-stage-override path))
     ;; Branch is merged to upstream (via squash PR)
     ((and branch (orchard--branch-merged-p branch)) 'merged)
     ;; PR is open
     ((file-exists-p pr-url-file) 'pr-open)
     ;; Default - in progress
     (t 'in-progress))))

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

(defun orchard--stage-display-name (stage)
  "Get display name for STAGE."
  (pcase stage
    ('in-progress "In Progress")
    ('pr-open "PR Open")
    ('merged "Merged")
    (_ "")))

(defun orchard--stage-icon (stage)
  "Get icon for STAGE."
  (pcase stage
    ('in-progress "🔧")
    ('pr-open "🔀")
    ('merged "✓")
    (_ "")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hook Handlers
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--on-commando-start (command path)
  "Handle commando command start. Track dev mode ownership.
Enforces dev mode based on `orchard-dev-mode-enforcement'."
  (when (and (boundp 'commando-dev-commands)
             (member command commando-dev-commands))
    ;; Check if another worktree owns dev
    (if (and orchard--dev-owner
             (not (string= path orchard--dev-owner)))
        (let ((current-owner (file-name-nondirectory
                              (directory-file-name orchard--dev-owner))))
          (pcase orchard-dev-mode-enforcement
            ('block
             (user-error "Dev mode blocked: %s owns dev. Release it first or use M-x orchard-force-dev-takeover"
                         current-owner))
            ('confirm
             (if (y-or-n-p (format "%s owns dev mode. Take over? " current-owner))
                 (progn
                   (orchard--set-dev-owner path)
                   (message "Dev mode taken from %s → %s"
                            current-owner
                            (file-name-nondirectory (directory-file-name path))))
               (user-error "Dev mode not taken over")))
            (_  ; warn
             (message "Warning: %s already owns dev mode" current-owner)
             (orchard--set-dev-owner path)
             (message "Dev mode: %s" (file-name-nondirectory (directory-file-name path))))))
      ;; No conflict - just claim it
      (orchard--set-dev-owner path)
      (message "Dev mode: %s" (file-name-nondirectory (directory-file-name path))))))

(defun orchard--on-commando-finish (command path _status)
  "Handle commando command finish. Release dev mode if needed."
  (when (and (boundp 'commando-dev-commands)
             (member command commando-dev-commands)
             orchard--dev-owner
             (string= path orchard--dev-owner))
    (orchard--set-dev-owner nil)
    (message "Dev mode released")))

(defun orchard--on-testicular-start (_project-root)
  "Handle testicular session start."
  (orchard--refresh-if-visible))

(defun orchard--on-testicular-complete (project-root passed failed total)
  "Handle testicular completion."
  (orchard--set-test-results project-root passed failed total)
  (orchard--refresh-if-visible)
  (if (zerop failed)
      (message "All %d tests passed! Ready for PR." total)
    (message "%d/%d tests failed" failed total)))

(defun orchard--refresh-if-visible ()
  "Refresh orchard buffer if visible."
  (when-let ((buf (get-buffer "*Orchard*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (orchard-refresh)))))

;; Register hooks (will be called if commando/testicular are loaded)
(with-eval-after-load 'config-commando
  (add-hook 'commando-command-start-hook #'orchard--on-commando-start)
  (add-hook 'commando-command-finish-hook #'orchard--on-commando-finish))

(with-eval-after-load 'config-testicular
  (add-hook 'testicular-start-hook #'orchard--on-testicular-start)
  (add-hook 'testicular-complete-hook #'orchard--on-testicular-complete))

;; Magit hooks - removed auto-refresh on every magit operation (was slow)
;; Use `g` in orchard buffer to refresh manually when needed
(with-eval-after-load 'magit
  ;; Only refresh after push operations (less frequent)
  (advice-add 'magit-push-current-to-upstream :after
              (lambda (&rest _) (run-at-time 2 nil #'orchard--refresh-if-visible)))
  (advice-add 'magit-push-current-to-pushremote :after
              (lambda (&rest _) (run-at-time 2 nil #'orchard--refresh-if-visible))))

;; Load state on init
(orchard--load-state)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Faces
;;; ════════════════════════════════════════════════════════════════════════════

(defface orchard-header
  '((t :foreground "#61AFEF" :weight bold :height 1.2))
  "Face for dashboard header."
  :group 'orchard)

(defface orchard-subheader
  '((t :foreground "#ABB2BF" :weight bold))
  "Face for section headers."
  :group 'orchard)

(defface orchard-branch-feature
  '((t :foreground "#98C379"))
  "Face for FEATURE branches (green)."
  :group 'orchard)

(defface orchard-branch-bugfix
  '((t :foreground "#E06C75"))
  "Face for BUGFIX branches (red)."
  :group 'orchard)

(defface orchard-branch-chore
  '((t :foreground "#56B6C2"))
  "Face for CHORE branches (cyan)."
  :group 'orchard)

(defface orchard-branch-refactor
  '((t :foreground "#E5C07B"))
  "Face for REFACTOR branches (yellow)."
  :group 'orchard)

(defface orchard-branch-docs
  '((t :foreground "#61AFEF"))
  "Face for DOCS branches (blue)."
  :group 'orchard)

(defface orchard-branch-experiment
  '((t :foreground "#C678DD"))
  "Face for EXPERIMENT branches (purple)."
  :group 'orchard)

(defface orchard-branch-test
  '((t :foreground "#ABB2BF"))
  "Face for TEST branches (gray)."
  :group 'orchard)

(defface orchard-branch-main
  '((t :foreground "#5C6370"))
  "Face for main/dev branch (gray)."
  :group 'orchard)

(defface orchard-current
  '((t :foreground "#C678DD" :weight bold))
  "Face for current worktree (magenta)."
  :group 'orchard)

(defface orchard-dirty
  '((t :foreground "#E06C75"))
  "Face for dirty indicator (red)."
  :group 'orchard)

(defface orchard-clean
  '((t :foreground "#98C379"))
  "Face for clean indicator (green)."
  :group 'orchard)

(defface orchard-claude-running
  '((t :foreground "#61AFEF" :weight bold))
  "Face for running Claude indicator (blue)."
  :group 'orchard)

(defface orchard-claude-stopped
  '((t :foreground "#E06C75"))
  "Face for stopped Claude indicator (red)."
  :group 'orchard)

(defface orchard-branch-mismatch
  '((t :foreground "#E06C75" :weight bold))
  "Face for branch mismatch warning (red bold)."
  :group 'orchard)

(defface orchard-key
  '((t :foreground "#E5C07B" :weight bold))
  "Face for keybinding hints."
  :group 'orchard)

(defface orchard-hl-line
  '((((class color) (background light))
     :background "#c8ddf8" :extend t)
    (((class color) (background dark))
     :background "#2c4a6e" :extend t))
  "Face for highlighting the current line."
  :group 'orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Column Management - The Heart of Orchard
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Columns are numbered 0 to (orchard-max-columns - 1)
;; Column 0 is ALWAYS the dashboard
;; Columns 1+ are for branches
;;
;; Each column tracks:
;;   - branch: the branch name (nil for dashboard)
;;   - window: the current window object
;;   - mode: 'magit, 'claude, or 'compile
;;   - previous-mode: for restoring after compile
;;
;; WINDOW LOCKING STRATEGY:
;;   - Use `set-window-buffer` instead of `switch-to-buffer` to bypass display machinery
;;   - Mark branch windows as dedicated to prevent buffer poaching
;;   - Override `display-buffer-alist` for orchard-managed buffer patterns

(defvar orchard--columns (make-hash-table :test 'eq)
  "Hash table mapping column index to column state plist.")

(defvar orchard--branch-to-column (make-hash-table :test 'equal)
  "Hash table mapping branch names to column indices.")

(defun orchard--init-columns ()
  "Initialize column tracking."
  (clrhash orchard--columns)
  (clrhash orchard--branch-to-column)
  ;; Column 0 is always dashboard
  (puthash 0 (list :branch nil :window nil :mode 'dashboard) orchard--columns))

(defun orchard--get-column (index)
  "Get column state for INDEX."
  (gethash index orchard--columns))

(defun orchard--set-column (index plist)
  "Set column state for INDEX to PLIST."
  (puthash index plist orchard--columns))

(defun orchard--column-for-branch (branch)
  "Get column index for BRANCH, or nil if not assigned."
  (gethash branch orchard--branch-to-column))

(defun orchard--assign-branch-to-column (branch column)
  "Assign BRANCH to COLUMN."
  ;; Remove old branch from this column if any
  (let ((old-state (orchard--get-column column)))
    (when-let ((old-branch (plist-get old-state :branch)))
      (remhash old-branch orchard--branch-to-column)))
  ;; Assign new branch
  (puthash branch column orchard--branch-to-column)
  (let ((state (or (orchard--get-column column) (list))))
    (orchard--set-column column (plist-put state :branch branch))))

(defun orchard--find-available-column ()
  "Find an available column for a new branch.
Returns column index, possibly replacing an existing one."
  ;; Try columns 1 to max-1
  (or (cl-loop for i from 1 below orchard-max-columns
               unless (orchard--get-column i)
               return i)
      ;; All columns used - find one to replace (not the current one)
      (let ((current-col (orchard--window-column (selected-window))))
        (cl-loop for i from 1 below orchard-max-columns
                 unless (eq i current-col)
                 return i
                 finally return 1))))

(defun orchard--window-column (window)
  "Get the column index for WINDOW based on position."
  (let* ((edges (window-edges window))
         (left (car edges))
         (windows (window-list nil 'no-mini))
         (lefts (sort (delete-dups (mapcar (lambda (w) (car (window-edges w))) windows)) #'<)))
    (cl-position left lefts)))

(defun orchard--get-column-window (column)
  "Get the topmost window for COLUMN, or nil.
Handles vertically split columns correctly by grouping windows by left edge."
  (let* ((windows (cl-remove-if-not #'window-live-p (window-list nil 'no-mini)))
         ;; Group windows by left edge (same column)
         (columns-alist nil))
    ;; Build alist of (left-edge . topmost-window)
    (dolist (win windows)
      (let* ((edges (window-edges win))
             (left (car edges))
             (top (cadr edges))
             (existing (assoc left columns-alist)))
        (if existing
            ;; Keep the topmost window (smallest top value)
            (when (< top (cadr (window-edges (cdr existing))))
              (setcdr existing win))
          ;; New column
          (push (cons left win) columns-alist))))
    ;; Sort by left edge and get the nth column
    (let ((sorted (sort columns-alist (lambda (a b) (< (car a) (car b))))))
      (cdr (nth column sorted)))))

(defun orchard--count-columns ()
  "Count the number of columns (unique left edges)."
  (let ((windows (window-list nil 'no-mini)))
    (length (delete-dups (mapcar (lambda (w) (car (window-edges w))) windows)))))

(defun orchard--ensure-columns ()
  "Ensure we have the right number of columns.
Creates columns if needed, up to orchard-max-columns."
  (let ((current-count (orchard--count-columns)))
    (when (< current-count orchard-max-columns)
      ;; Need more columns - split from rightmost
      (let ((rightmost (car (last (sort (window-list nil 'no-mini)
                                        (lambda (a b)
                                          (> (car (window-edges a))
                                             (car (window-edges b)))))))))
        (dotimes (_ (- orchard-max-columns current-count))
          (select-window rightmost)
          (setq rightmost (split-window-right))
          (balance-windows))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Window Locking - Prevent Buffer Escape
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--display-buffer-in-window (buffer window)
  "Display BUFFER in WINDOW, bypassing display-buffer machinery.
This is the core function for enforcing column locking."
  (when (and buffer window (window-live-p window))
    (select-window window)
    (set-window-buffer window buffer)
    buffer))

(defun orchard--display-buffer-in-column (buffer column)
  "Display BUFFER in COLUMN, creating window if needed."
  (orchard--ensure-columns)
  (let ((win (orchard--get-column-window column)))
    (orchard--display-buffer-in-window buffer win)))

(defun orchard--set-window-dedicated (window dedicated)
  "Set WINDOW dedication to DEDICATED.
Dedicated windows won't be commandeered by other buffer displays."
  (when (window-live-p window)
    (set-window-dedicated-p window dedicated)))

(defun orchard--dedicate-branch-column (column)
  "Mark COLUMN as dedicated to its branch."
  (when-let ((win (orchard--get-column-window column)))
    (orchard--set-window-dedicated win t)))

(defun orchard--undedicate-branch-column (column)
  "Remove dedication from COLUMN (needed before switching buffers)."
  (when-let ((win (orchard--get-column-window column)))
    (orchard--set-window-dedicated win nil)))

(defun orchard--with-undedicated-window (window fn)
  "Temporarily undedicate WINDOW, run FN, re-dedicate.
FN is called with no arguments."
  (let ((was-dedicated (window-dedicated-p window)))
    (when was-dedicated
      (set-window-dedicated-p window nil))
    (unwind-protect
        (funcall fn)
      (when was-dedicated
        (set-window-dedicated-p window t)))))

(defun orchard--magit-in-window (path window)
  "Open magit for PATH in WINDOW without letting it escape."
  (orchard--with-undedicated-window window
    (lambda ()
      (select-window window)
      (let ((display-buffer-overriding-action '(display-buffer-same-window)))
        (magit-status path)))))

(defun orchard--claude-in-window (path window)
  "Start or switch to Claude for PATH in WINDOW without letting it escape."
  (orchard--ensure-claude-loaded)
  (orchard--with-undedicated-window window
    (lambda ()
      (select-window window)
      ;; Check for existing buffer
      (let ((claude-buf (orchard--claude-buffer-for-path path)))
        (if (and claude-buf (buffer-live-p claude-buf))
            ;; Existing - force into window
            (set-window-buffer window claude-buf)
          ;; New - start Claude and capture the buffer
          (let ((default-directory path)
                (buffers-before (buffer-list)))
            (claude-code)
            ;; Find the new Claude buffer
            (let ((new-claude (cl-find-if
                               (lambda (buf)
                                 (and (string-prefix-p "*claude:" (buffer-name buf))
                                      (not (memq buf buffers-before))))
                               (buffer-list))))
              (when new-claude
                (set-window-buffer window new-claude)))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Display Buffer Rules for Orchard
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--buffer-branch (buffer)
  "Get branch name for BUFFER based on its default-directory."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((wt (orchard--current-worktree)))
        (alist-get 'branch wt)))))

(defun orchard--display-buffer-in-branch-column (buffer _alist)
  "Display BUFFER in its branch's column if tracked by orchard.
For use in `display-buffer-alist'."
  (when-let* ((branch (orchard--buffer-branch buffer))
              (column (orchard--column-for-branch branch))
              (window (orchard--get-column-window column)))
    (orchard--with-undedicated-window window
      (lambda ()
        (set-window-buffer window buffer)))
    window))

;; Add display-buffer rules for orchard-managed buffers
(defun orchard--setup-display-buffer-rules ()
  "Setup display-buffer-alist rules for orchard window locking."
  (dolist (pattern '("^\\*claude:" "^magit:" "^\\*compilation\\*"))
    (add-to-list 'display-buffer-alist
                 `(,pattern
                   (orchard--display-buffer-in-branch-column
                    display-buffer-same-window)
                   (inhibit-same-window . nil)))))

;; Setup rules when orchard loads
(orchard--setup-display-buffer-rules)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Worktree Data
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--feature-descriptions (make-hash-table :test 'equal)
  "Hash table mapping worktree paths to feature descriptions.")

(defun orchard--get-repo-root ()
  "Get the repository root, either from config or current directory."
  (or orchard-repo-path
      (locate-dominating-file default-directory ".git")))

(defun orchard--get-worktrees (&optional include-hidden force-refresh)
  "Get all worktrees with status info.
If INCLUDE-HIDDEN is non-nil, include hidden worktrees.
If FORCE-REFRESH is non-nil, bypass cache and fetch fresh data.
When `orchard--inhibit-cache-refresh' is t, ALWAYS uses cache (instant).
Use G to populate/refresh cache."
  ;; When inhibit is set, never refresh - just return cache (or nil)
  (unless orchard--inhibit-cache-refresh
    (let ((cache-valid (and orchard--worktrees-cache
                            orchard--worktrees-cache-time
                            (not force-refresh)
                            (< (float-time (time-subtract (current-time)
                                                          orchard--worktrees-cache-time))
                               orchard-worktrees-cache-ttl))))
      (unless cache-valid
        ;; Refresh cache
        (let ((repo-root (orchard--get-repo-root)))
          (when (and repo-root (file-directory-p repo-root))
            (let ((default-directory repo-root))
              (let* ((output (shell-command-to-string "git worktree list --porcelain"))
                     (worktrees (orchard--parse-worktrees output))
                     (enriched (delq nil (mapcar #'orchard--enrich-worktree worktrees))))
                (setq orchard--worktrees-cache enriched)
                (setq orchard--worktrees-cache-time (current-time)))))))))
  ;; Return filtered results from cache
  (when orchard--worktrees-cache
    (if include-hidden
        orchard--worktrees-cache
      (cl-remove-if (lambda (wt)
                      (orchard--worktree-hidden-p (alist-get 'path wt)))
                    orchard--worktrees-cache))))

(defun orchard--parse-worktrees (output)
  "Parse OUTPUT from git worktree list --porcelain."
  (let ((worktrees '())
        (current nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-prefix-p "worktree " line)
        (when current (push current worktrees))
        (setq current (list (cons 'path (substring line 9)))))
       ((string-prefix-p "HEAD " line)
        (push (cons 'head (substring line 5)) current))
       ((string-prefix-p "branch " line)
        (let ((branch (replace-regexp-in-string "^refs/heads/" "" (substring line 7))))
          (push (cons 'branch branch) current)))
       ((string= "bare" line)
        (push (cons 'bare t) current))
       ((string= "detached" line)
        (push (cons 'detached t) current))))
    (when current (push current worktrees))
    (nreverse worktrees)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Orphan Directory & Prunable Worktree Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-worktree-parent ()
  "Get the parent directory where worktrees are created."
  (if orchard-nested-worktrees
      (expand-file-name (or orchard-worktree-prefix "crewcapableai")
                        orchard-worktree-parent)
    orchard-worktree-parent))

(defun orchard--get-orphan-directories ()
  "Find directories in worktree parent that aren't registered git worktrees.
Returns list of paths."
  (let* ((parent (orchard--get-worktree-parent))
         (git-worktrees (mapcar (lambda (wt) (alist-get 'path wt))
                                (orchard--get-worktrees t)))  ; include hidden
         (orphans '()))
    (when (file-directory-p parent)
      (dolist (entry (directory-files parent t "^[A-Z]+" t))
        (when (and (file-directory-p entry)
                   (not (member (file-name-as-directory entry)
                                (mapcar #'file-name-as-directory git-worktrees)))
                   ;; Looks like a worktree dir (has .git or is a git repo)
                   (or (file-exists-p (expand-file-name ".git" entry))
                       (file-directory-p (expand-file-name ".git" entry))))
          (push entry orphans))))
    (nreverse orphans)))

(defun orchard--get-prunable-worktrees ()
  "Find worktrees marked as prunable by git.
Returns list of paths."
  (let ((repo-root (orchard--get-repo-root))
        (prunable '()))
    (when (and repo-root (file-directory-p repo-root))
      (let* ((default-directory repo-root)
             (output (shell-command-to-string "git worktree list --porcelain")))
        (let ((current-path nil))
          (dolist (line (split-string output "\n" t))
            (cond
             ((string-prefix-p "worktree " line)
              (setq current-path (substring line 9)))
             ((string= "prunable" line)
              (when current-path
                (push current-path prunable))))))))
    (nreverse prunable)))

(defun orchard-cleanup-orphans ()
  "Interactively delete orphan directories."
  (interactive)
  (let ((orphans (orchard--get-orphan-directories)))
    (if (null orphans)
        (message "No orphan directories found")
      (let ((to-delete (completing-read-multiple
                        "Delete orphan directories (comma-separated): "
                        orphans nil t)))
        (dolist (dir to-delete)
          (when (yes-or-no-p (format "Really delete %s? " dir))
            (delete-directory dir t t)
            (message "Deleted: %s" dir)))
        (orchard-refresh)))))

(defun orchard-prune-worktrees ()
  "Prune stale worktree references."
  (interactive)
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let ((default-directory repo-root))
        (if (yes-or-no-p "Run 'git worktree prune' to clean stale references? ")
            (progn
              (shell-command "git worktree prune")
              (message "Pruned stale worktree references")
              (orchard-refresh))
          (message "Cancelled"))))))

(defun orchard-sync ()
  "Show sync status - orphans and prunable worktrees."
  (interactive)
  (let ((orphans (orchard--get-orphan-directories))
        (prunable (orchard--get-prunable-worktrees)))
    (with-current-buffer (get-buffer-create "*Orchard Sync*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Orchard Sync Status\n" 'face 'orchard-header))
        (insert (make-string 40 ?═) "\n\n")

        (if orphans
            (progn
              (insert (propertize (format "Orphan Directories (%d):\n" (length orphans))
                                  'face '(:foreground "#E06C75" :weight bold)))
              (insert "These directories exist but aren't git worktrees:\n")
              (dolist (o orphans)
                (insert (format "  • %s\n" o)))
              (insert "\nUse 'O' to delete orphans or delete manually.\n\n"))
          (insert (propertize "✓ No orphan directories\n\n" 'face '(:foreground "#98C379"))))

        (if prunable
            (progn
              (insert (propertize (format "Prunable Worktrees (%d):\n" (length prunable))
                                  'face '(:foreground "#E5C07B" :weight bold)))
              (insert "These worktrees reference deleted branches:\n")
              (dolist (p prunable)
                (insert (format "  • %s\n" p)))
              (insert "\nUse 'P' to prune stale references.\n"))
          (insert (propertize "✓ No prunable worktrees\n" 'face '(:foreground "#98C379"))))

        (insert "\n")
        (insert (propertize "[q] " 'face 'orchard-key))
        (insert "close  ")
        (insert (propertize "[O] " 'face 'orchard-key))
        (insert "cleanup orphans  ")
        (insert (propertize "[P] " 'face 'orchard-key))
        (insert "prune worktrees")

        (goto-char (point-min))
        (special-mode)
        (local-set-key (kbd "q") #'quit-window)
        (local-set-key (kbd "O") #'orchard-cleanup-orphans)
        (local-set-key (kbd "P") #'orchard-prune-worktrees)))
    (pop-to-buffer "*Orchard Sync*")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch Mismatch Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--expected-branch-from-path (path)
  "Extract expected branch name from worktree PATH.
Converts path like ~/src/crewcapableai/FEAT-my-feature to FEAT/my-feature."
  (let ((dir-name (file-name-nondirectory (directory-file-name path))))
    ;; Convert FEAT-foo-bar to FEAT/foo-bar (first hyphen becomes slash)
    (if (string-match "^\\([A-Z]+\\)-\\(.+\\)$" dir-name)
        (concat (match-string 1 dir-name) "/" (match-string 2 dir-name))
      dir-name)))

(defun orchard--branch-mismatch-p (wt)
  "Return mismatch info if worktree WT's actual branch differs from expected.
Returns nil if branches match, or a cons of (expected . actual) if mismatched."
  (let* ((path (alist-get 'path wt))
         (git-branch (alist-get 'branch wt))  ; From git worktree list
         (expected (orchard--expected-branch-from-path path)))
    (unless (or (string= git-branch expected)
                (alist-get 'bare wt)      ; Skip main worktree
                (alist-get 'detached wt)) ; Skip detached HEAD
      (cons expected git-branch))))

(defun orchard--enrich-worktree (wt)
  "Add status info to worktree WT.
Returns nil if worktree path doesn't exist (skip it)."
  (let* ((path (alist-get 'path wt))
         (branch (alist-get 'branch wt)))
    ;; Skip if path doesn't exist
    (when (file-directory-p path)
      (let ((default-directory path))
        ;; Port allocation (from ghq)
        (when (fboundp 'ghq--get-worktree-port)
          (when-let ((port-slot (ghq--get-worktree-port path)))
            (push (cons 'port port-slot) wt)))
        ;; Dirty status
        (let ((dirty (not (string-empty-p
                           (shell-command-to-string "git status --porcelain 2>/dev/null")))))
          (push (cons 'dirty dirty) wt))
        ;; Ahead/behind upstream
        (let ((counts (shell-command-to-string
                       (format "git rev-list --left-right --count %s...HEAD 2>/dev/null"
                               orchard-upstream-branch))))
          (when (string-match "\\([0-9]+\\)\\s-+\\([0-9]+\\)" counts)
            (push (cons 'behind (string-to-number (match-string 1 counts))) wt)
            (push (cons 'ahead (string-to-number (match-string 2 counts))) wt)))
        ;; Claude status: nil, 'stopped, or 'running
        (let ((claude-buf (orchard--claude-buffer-for-path path)))
          (when (and claude-buf (buffer-live-p claude-buf))
            (let ((running (orchard--claude-process-running-p claude-buf)))
              (push (cons 'claude-status (if running 'running 'stopped)) wt))))
        ;; Column assignment
        (when branch
          (push (cons 'column (orchard--column-for-branch branch)) wt))
        ;; Feature description
        (when-let ((desc (orchard--load-feature-description path)))
          (push (cons 'description desc) wt))
        ;; Stage tracking (pass branch for merged detection)
        (push (cons 'stage (orchard--detect-stage path branch)) wt)
        ;; Dev mode ownership
        (when (and orchard--dev-owner
                   (string= (file-name-as-directory path)
                            (file-name-as-directory orchard--dev-owner)))
          (push (cons 'dev-owner t) wt))
        ;; Branch mismatch detection
        (when-let ((mismatch (orchard--branch-mismatch-p wt)))
          (push (cons 'branch-mismatch mismatch) wt))
        wt))))

(defun orchard--save-feature-description (path description)
  "Save DESCRIPTION for worktree at PATH."
  (puthash path description orchard--feature-descriptions)
  (let ((desc-file (expand-file-name ".feature-description" path)))
    (with-temp-file desc-file
      (insert description))))

(defun orchard--load-feature-description (path)
  "Load feature description for worktree at PATH."
  (or (gethash path orchard--feature-descriptions)
      (let ((desc-file (expand-file-name ".feature-description" path)))
        (when (file-exists-p desc-file)
          (with-temp-buffer
            (insert-file-contents desc-file)
            (let ((desc (string-trim (buffer-string))))
              (puthash path desc orchard--feature-descriptions)
              desc))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Integration
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
             ((or (string-match-p " for [0-9]+m?s\\b" end-text)  ; "Cooked for 5s", "Cogitated for 1m 30s"
                  (string-match-p "accept edits on" end-text)
                  (string-match-p "\n❯[[:space:]]*$" end-text)
                  (string-match-p "───+\n❯" end-text))
              'idle)
             ;; Default: idle (at prompt, nothing happening)
             (t 'active))))))))

(defun orchard--claude-waiting-p (buffer)
  "Check if Claude BUFFER needs attention (waiting or idle)."
  (memq (orchard--claude-status buffer) '(waiting idle)))

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
      (dolist (issue issues)
        (let* ((num (alist-get 'number issue))
               (title (alist-get 'title issue))
               (wt (orchard--find-worktree-for-issue num worktrees)))
          (when wt
            (let* ((path (alist-get 'path wt))
                   (stage (orchard--issue-workflow-stage num worktrees))
                   (claude-status (alist-get 'claude-status stage)))
              (princ (format "#%d: %s\n" num (truncate-string-to-width title 40)))
              (princ (format "  Worktree: %s\n" path))
              (princ (format "  Claude status: %s\n" (or claude-status "NO CLAUDE")))
              (princ (format "  Stage: %s\n\n" stage)))))))))

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

(defun orchard--claude-process-running-p (buffer)
  "Check if the vterm process in BUFFER is still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (boundp 'vterm--process)
           vterm--process
           (process-live-p vterm--process)))))

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
;;; Mode Cycling (M-m) - The Key Feature
;;; ════════════════════════════════════════════════════════════════════════════

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
            (pop-to-buffer claude-buf)
          ;; Start new Claude
          (let ((default-directory project-root))
            (claude-code)))))

     ;; In claude/vterm - go to magit
     ((derived-mode-p 'vterm-mode)
      (magit-status project-root))

     ;; Elsewhere - go to magit
     (t
      (magit-status project-root)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch Opening - Smart Window Selection
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

(defun orchard--find-best-window ()
  "Find the best window for opening Claude/magit.
Priority:
1. Existing Claude window (reuse, don't spawn new)
2. Largest non-orchard window
3. Current window as fallback
NEVER splits - predictable window management."
  (let* ((windows (window-list nil 'no-mini))
         (leftmost (orchard--leftmost-window))
         (non-leftmost (cl-remove leftmost windows)))
    (or
     ;; Reuse existing Claude window
     (cl-find-if #'orchard--window-showing-claude-p non-leftmost)
     ;; Use largest non-orchard window
     (car (sort (cl-remove-if #'orchard--window-showing-orchard-p non-leftmost)
                (lambda (a b)
                  (> (window-width a) (window-width b)))))
     ;; Fallback: largest window that isn't orchard
     (car (sort (cl-remove-if #'orchard--window-showing-orchard-p windows)
                (lambda (a b)
                  (> (window-width a) (window-width b)))))
     ;; Last resort: current window
     (selected-window))))

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
            ;; Start new Claude
            (let ((default-directory path)
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
                (if new-claude
                    (progn
                      (message "Orchard: found Claude %s" (buffer-name new-claude))
                      (set-window-buffer claude-win new-claude))
                  (message "Orchard: no Claude buffer, removing split")
                  (delete-window claude-win))))))
        ;; Return focus to magit
        (when (window-live-p magit-win)
          (select-window magit-win))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Buffer
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
    (define-key map (kbd "f") #'orchard-new-feature)
    ;; Open/interact with worktree at point
    (define-key map (kbd "RET") #'orchard-open-at-point)
    (define-key map (kbd "c") #'orchard-claude-at-point)
    (define-key map (kbd "m") #'orchard-magit-at-point)
    (define-key map (kbd "d") #'orchard-dired-at-point)
    (define-key map (kbd "t") #'orchard-test-at-point)
    (define-key map (kbd "l") #'orchard-list-claudes)
    ;; GitHub Issues
    (define-key map (kbd "I") #'orchard-issue-start)       ; start branch from issue
    (define-key map (kbd "o") #'orchard-issue-browse)      ; open issue in browser
    (define-key map (kbd "#") #'orchard-find-issue)        ; find issue by number
    (define-key map (kbd "!") #'orchard-resolve-issue)     ; diagnose/resolve issue
    (define-key map (kbd "s") #'orchard-toggle-staging-issues) ; toggle staging visibility
    (define-key map (kbd "/") #'orchard-filter-by-label)   ; filter by label
    (define-key map (kbd "\\") #'orchard-clear-label-filter) ; clear label filter
    ;; Lifecycle actions
    (define-key map (kbd "N") #'orchard-next-step)
    (define-key map (kbd "u") #'orchard-push-at-point)   ; u for "upload"/push
    (define-key map (kbd "P") #'orchard-pr-at-point)
    (define-key map (kbd "-") #'orchard-hide-at-point)    ; hide (just remove from list)
    (define-key map (kbd "H") #'orchard-show-hidden)      ; show/unhide hidden
    (define-key map (kbd "a") #'orchard-archive-at-point) ; archive (remove worktree, keep branch)
    (define-key map (kbd "D") #'orchard-delete-at-point)  ; delete (remove worktree and branch)
    ;; Port management (lazy allocation)
    (define-key map (kbd "+") #'orchard-allocate-port)    ; allocate port for dev server
    (define-key map (kbd "_") #'orchard-release-port)     ; release allocated port
    ;; Filtering and views
    (define-key map (kbd "f") #'orchard-filter-menu)        ; filter transient menu
    (define-key map (kbd "v w") #'orchard-view-working)     ; working view (default)
    (define-key map (kbd "v a") #'orchard-view-all)         ; all sections
    (define-key map (kbd "v n") #'orchard-view-next)        ; next only
    (define-key map (kbd "v p") #'orchard-view-progress)    ; in progress only
    (define-key map (kbd "v q") #'orchard-view-qa)          ; QA only
    (define-key map (kbd "v r") #'orchard-view-recent)      ; recent sessions
    ;; Section collapsing
    (define-key map (kbd "TAB") #'orchard-toggle-section)   ; toggle section collapse
    (define-key map (kbd "<tab>") #'orchard-toggle-section)
    ;; Navigation - moves between worktrees AND issues
    (define-key map (kbd "n") #'orchard-next-item)
    (define-key map (kbd "p") #'orchard-prev-item)
    (define-key map (kbd "j") #'orchard-next-item)
    (define-key map (kbd "k") #'orchard-prev-item)
    (define-key map (kbd "C-n") #'orchard-next-item)
    (define-key map (kbd "C-p") #'orchard-prev-item)
    (define-key map (kbd "<down>") #'orchard-next-item)
    (define-key map (kbd "<up>") #'orchard-prev-item)
    (define-key map (kbd "<backtab>") #'orchard-prev-item)
    ;; Cleanup
    (define-key map (kbd "K") #'orchard-cleanup)           ; K for "kill stale"
    (define-key map (kbd "C-c C-c") #'orchard-cleanup-dry-run)
    (define-key map (kbd "M") #'orchard-cleanup-merged)    ; M for "merged"
    (define-key map (kbd "C-c m") #'orchard-cleanup-merged-dry-run)
    ;; Sync (orphans, prunable)
    (define-key map (kbd "S") #'orchard-sync)
    (define-key map (kbd "O") #'orchard-cleanup-orphans)
    ;; Refresh and quit
    (define-key map (kbd "g") #'orchard-refresh)
    (define-key map (kbd "r") #'orchard-refresh)
    (define-key map (kbd "G") #'orchard-force-refresh)  ; Force refresh (ignore TTL)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'orchard-quit-all)
    ;; Debug
    (define-key map (kbd "!") #'orchard-toggle-window-dedication)
    ;; M-x orchard-debug-claude-status for Claude status diagnostics
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

(defun orchard--branch-face (branch)
  "Return face for BRANCH based on prefix."
  (cond
   ;; New prefixes (unique first char for tab completion)
   ((string-prefix-p "FEATURE/" branch) 'orchard-branch-feature)
   ((string-prefix-p "BUGFIX/" branch) 'orchard-branch-bugfix)
   ((string-prefix-p "CHORE/" branch) 'orchard-branch-chore)
   ((string-prefix-p "REFACTOR/" branch) 'orchard-branch-refactor)
   ((string-prefix-p "DOCS/" branch) 'orchard-branch-docs)
   ((string-prefix-p "EXPERIMENT/" branch) 'orchard-branch-experiment)
   ((string-prefix-p "TEST/" branch) 'orchard-branch-test)
   ;; Legacy prefixes (for existing branches)
   ((string-prefix-p "FEAT/" branch) 'orchard-branch-feature)
   ((string-prefix-p "FIX/" branch) 'orchard-branch-bugfix)
   ;; Main branches
   ((member branch '("dev" "main" "master")) 'orchard-branch-main)
   (t 'default)))

(defun orchard--branch-icon (branch)
  "Return icon for BRANCH type."
  (cond
   ;; New prefixes
   ((string-prefix-p "FEATURE/" branch) "✨")
   ((string-prefix-p "BUGFIX/" branch) "🐛")
   ((string-prefix-p "CHORE/" branch) "🧹")
   ((string-prefix-p "REFACTOR/" branch) "♻️")
   ((string-prefix-p "DOCS/" branch) "📖")
   ((string-prefix-p "EXPERIMENT/" branch) "🧪")
   ((string-prefix-p "TEST/" branch) "✅")
   ;; Legacy prefixes
   ((string-prefix-p "FEAT/" branch) "✨")
   ((string-prefix-p "FIX/" branch) "🐛")
   ;; Main branches
   ((member branch '("dev" "main" "master")) "📦")
   (t "📁")))

(defun orchard--count-dedicated-windows ()
  "Count windows that are currently dedicated."
  (cl-count-if #'window-dedicated-p (window-list nil 'no-mini)))

(defface orchard-issue
  '((t :foreground "#61AFEF"))
  "Face for GitHub issue numbers."
  :group 'orchard)

(defface orchard-issue-title
  '((t :foreground "#ABB2BF"))
  "Face for GitHub issue titles."
  :group 'orchard)

(defun orchard--issue-type-icon (labels)
  "Return icon based on issue LABELS."
  (let ((label-names (mapcar (lambda (l) (alist-get 'name l)) labels)))
    (cond
     ((cl-some (lambda (n) (string-match-p "bug\\|fix" n)) label-names) "🐛")
     ((cl-some (lambda (n) (string-match-p "feature\\|enhancement" n)) label-names) "✨")
     ((cl-some (lambda (n) (string-match-p "chore\\|maintenance" n)) label-names) "🧹")
     ((cl-some (lambda (n) (string-match-p "doc" n)) label-names) "📖")
     (t "📋"))))

(defun orchard--issue-has-worktree-p (issue-number worktrees)
  "Check if ISSUE-NUMBER already has a worktree in WORKTREES."
  (cl-some (lambda (wt)
             (let ((linked-issue (orchard--get-worktree-issue
                                  (alist-get 'path wt)
                                  (alist-get 'branch wt))))
               (and linked-issue (= linked-issue issue-number))))
           worktrees))

(defun orchard--find-worktree-for-issue (issue-number &optional worktrees)
  "Find and return the worktree associated with ISSUE-NUMBER, or nil.
If WORKTREES is provided, search in that list instead of fetching."
  (cl-find-if (lambda (wt)
                (let ((linked-issue (orchard--get-worktree-issue
                                     (alist-get 'path wt)
                                     (alist-get 'branch wt))))
                  (and linked-issue (= linked-issue issue-number))))
              (or worktrees (orchard--get-worktrees))))

(defun orchard--issue-has-pr-p (issue-number worktrees)
  "Check if ISSUE-NUMBER has a PR created (via its worktree).
Returns the PR URL if found, nil otherwise."
  (when-let ((wt (orchard--find-worktree-for-issue issue-number worktrees)))
    (let ((pr-url-file (expand-file-name ".pr-url" (alist-get 'path wt))))
      (when (file-exists-p pr-url-file)
        (with-temp-buffer
          (insert-file-contents pr-url-file)
          (string-trim (buffer-string)))))))

(defun orchard--issue-claude-waiting-p (issue-number worktrees)
  "Check if ISSUE-NUMBER has a Claude buffer that's waiting for input.
Returns t if waiting, nil otherwise."
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
       ;; Claude status indicator
       (pcase cs
         ('waiting (propertize "⏳WAIT " 'face '(:foreground "#E06C75" :weight bold)))
         ('idle (propertize "✓DONE " 'face '(:foreground "#E5C07B" :weight bold)))
         ('active (propertize "⟳ " 'face '(:foreground "#61AFEF")))
         (_ ""))
       ;; Workflow stage
       (cond
        (r (propertize "PR" 'face '(:foreground "#61AFEF" :weight bold)))
        (p (propertize "planned" 'face '(:foreground "#98C379")))
        (a (propertize "analyzed" 'face '(:foreground "#98C379")))
        (t (propertize "wip" 'face '(:foreground "#5C6370"))))))))

(defun orchard--format-issue (issue worktrees)
  "Format ISSUE for dashboard display. WORKTREES used to check for existing work."
  (let* ((number (alist-get 'number issue))
         (title (alist-get 'title issue))
         (labels (alist-get 'labels issue))
         (icon (orchard--issue-type-icon labels))
         (has-worktree (orchard--issue-has-worktree-p number worktrees))
         (stage (orchard--issue-workflow-stage number worktrees))
         (claude-status (alist-get 'claude-status stage))
         (needs-attention (memq claude-status '(waiting idle)))
         (workflow (if has-worktree
                       (concat (orchard--format-workflow-indicator stage) " ")
                     ""))
         (label-str (orchard--format-labels labels))
         ;; Leading attention indicator
         (attention-prefix (if needs-attention
                               (propertize ">>>" 'face '(:foreground "#E06C75" :weight bold))
                             "   "))
         (line (concat
                attention-prefix " " icon " "
                (propertize (format "#%d" number) 'face 'orchard-issue)
                " " workflow
                (propertize (truncate-string-to-width title 25 nil nil "...")
                            'face 'orchard-issue-title)
                label-str
                "\n")))
    ;; Highlight entire line if Claude needs attention
    (when needs-attention
      (setq line (propertize line 'face
                             (if (eq claude-status 'waiting)
                                 '(:background "#3E2723")  ; dark red bg
                               '(:background "#2E3B2E"))))) ; dark green bg
    (propertize line 'orchard-issue issue)))

(defun orchard-timing-test ()
  "Test timing of dashboard components. Run M-x orchard-timing-test."
  (interactive)
  (let ((orchard--inhibit-cache-refresh t))  ; Simulate quick refresh
    (message "Testing with inhibit=t (quick refresh mode)...")
    (let* ((t0 (float-time))
           (_ (orchard--get-worktrees))
           (t1 (float-time))
           (_ (orchard--get-open-issues))
           (t2 (float-time))
           (wts (orchard--get-worktrees))
           (issues (orchard--get-open-issues))
           (_ (dolist (wt wts) (orchard--format-worktree wt nil)))
           (t3 (float-time))
           (_ (dolist (i issues) (orchard--format-issue i wts)))
           (t4 (float-time)))
      (message "Timing: worktrees=%.2fs issues=%.2fs format-wt=%.2fs format-issues=%.2fs TOTAL=%.2fs"
               (- t1 t0) (- t2 t1) (- t3 t2) (- t4 t3) (- t4 t0)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Issue-Centric Dashboard Formatting
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--categorize-issues (issues worktrees)
  "Categorize ISSUES into lifecycle groups based on WORKTREES state.
Returns alist: ((up-next . list) (in-progress . list)
                (qa-verify . list) (done . list))
Each list item is a cons of (issue . worktree-or-nil)."
  (let (up-next in-progress qa-verify done)
    ;; Categorize open issues
    (dolist (issue issues)
      (let* ((issue-num (alist-get 'number issue))
             (wt (orchard--find-worktree-for-issue issue-num worktrees)))
        (if (not wt)
            ;; No worktree = available to start
            (push (cons issue nil) up-next)
          ;; Has worktree - check stage
          (let ((stage (alist-get 'stage wt)))
            (if (eq stage 'merged)
                ;; PR merged but issue still open = QA/verify
                (push (cons issue wt) qa-verify)
              ;; In progress (including pr-open)
              (push (cons issue wt) in-progress))))))
    ;; Get archivable worktrees for DONE section (closed issues or merged orphans)
    (unless orchard--inhibit-cache-refresh
      (dolist (wt (orchard--get-archivable-worktrees))
        (let* ((branch (alist-get 'branch wt))
               (issue-num (orchard--get-worktree-issue nil branch)))
          (when issue-num
            ;; Create pseudo-issue for display
            (push (cons `((number . ,issue-num)
                          (title . ,(format "Issue #%d" issue-num))
                          (labels . nil)
                          (closed . t))
                        wt)
                  done)))))
    ;; Return in display order (reverse because we pushed)
    `((up-next . ,(nreverse up-next))
      (in-progress . ,(nreverse in-progress))
      (qa-verify . ,(nreverse qa-verify))
      (done . ,(nreverse done)))))

(defun orchard--get-orphan-worktrees (worktrees)
  "Get WORKTREES that don't have linked issues (excluding main/dev/master)."
  (cl-remove-if
   (lambda (wt)
     (let ((branch (alist-get 'branch wt)))
       (or (member branch '("dev" "main" "master"))
           (orchard--get-worktree-issue nil branch))))
   worktrees))

(defun orchard--format-section-header (title count &optional extra section-id)
  "Format a section header with TITLE, COUNT, optional EXTRA text, and SECTION-ID.
SECTION-ID is used for collapse/expand functionality."
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

(defun orchard--format-branch-inline (wt current-path)
  "Format worktree WT as an inline branch line (indented under issue).
CURRENT-PATH used to highlight if this is the current worktree."
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
  "Format ISSUE-WT-PAIR as issue line with optional branch underneath.
ISSUE-WT-PAIR is (issue . worktree-or-nil).
CURRENT-PATH used to highlight current worktree.
SHOW-MERGED-BADGE adds ✓Merged indicator for QA section items."
  (let* ((issue (car issue-wt-pair))
         (wt (cdr issue-wt-pair))
         (number (alist-get 'number issue))
         (title (alist-get 'title issue))
         (labels (alist-get 'labels issue))
         (closed (alist-get 'closed issue))
         (icon (orchard--issue-type-icon labels))
         ;; Get Claude/workflow status if worktree exists
         (wt-path (when wt (alist-get 'path wt)))
         (stage (when wt (orchard--issue-workflow-stage number (list wt))))
         (claude-status (alist-get 'claude-status stage))
         (needs-attention (memq claude-status '(waiting idle)))
         (workflow (if wt
                       (orchard--format-workflow-indicator stage)
                     ""))
         ;; Check for persisted Claude session
         (session-indicator (if wt-path
                                (orchard--format-session-indicator wt-path)
                              ""))
         (label-str (orchard--format-labels labels))
         ;; Issue line
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
    ;; Apply attention highlighting to issue line
    (when needs-attention
      (setq issue-line (propertize issue-line 'face
                                   (if (eq claude-status 'waiting)
                                       '(:background "#3E2723")
                                     '(:background "#2E3B2E")))))
    ;; Set text properties and combine with branch line
    (concat
     (propertize issue-line
                 'orchard-issue issue
                 'orchard-worktree wt)
     (when wt
       (propertize (orchard--format-branch-inline wt current-path)
                   'orchard-issue issue
                   'orchard-worktree wt)))))

(defun orchard--format-orphan-worktree (wt current-path)
  "Format orphan worktree WT (no linked issue) for display."
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

(defun orchard--format-dashboard ()
  "Format the Orchard dashboard with issue-centric layout."
  (let* ((worktrees (orchard--get-worktrees))
         (current (orchard--current-worktree))
         (current-path (when current (alist-get 'path current)))
         ;; Only count Claude sessions that belong to displayed worktrees
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
         ;; Skip slow checks during quick refresh
         (orphans (unless orchard--inhibit-cache-refresh
                    (orchard--get-orphan-directories)))
         (prunable (unless orchard--inhibit-cache-refresh
                     (orchard--get-prunable-worktrees)))
         (sync-issues (+ (length orphans) (length prunable)))
         ;; Get and filter issues
         (all-open-issues (orchard--get-open-issues))
         (staging-count (cl-count-if #'orchard--issue-staging-p all-open-issues))
         ;; Apply filters: staging, label, AND hidden
         (filtered-issues (let ((issues all-open-issues))
                            (when orchard--hide-staging-issues
                              (setq issues (cl-remove-if #'orchard--issue-staging-p issues)))
                            (when orchard--label-filter
                              (setq issues (cl-remove-if-not
                                            (lambda (i) (orchard--issue-has-exact-label-p i orchard--label-filter))
                                            issues)))
                            ;; Remove hidden issues
                            (setq issues (cl-remove-if
                                          (lambda (i) (orchard--issue-hidden-p (alist-get 'number i)))
                                          issues))
                            issues))
         ;; Filter out hidden worktrees
         (visible-worktrees (cl-remove-if
                             (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                             worktrees))
         ;; Categorize issues into lifecycle groups
         (categories (orchard--categorize-issues filtered-issues visible-worktrees))
         (up-next (alist-get 'up-next categories))
         (in-progress (alist-get 'in-progress categories))
         (qa-verify (alist-get 'qa-verify categories))
         (done (alist-get 'done categories))
         ;; Orphan worktrees (no linked issue), excluding hidden
         (orphan-worktrees (cl-remove-if
                            (lambda (wt) (orchard--worktree-hidden-p (alist-get 'path wt)))
                            (orchard--get-orphan-worktrees visible-worktrees)))
         ;; Count hidden items
         (hidden-count (+ (length (orchard--get-hidden-issues))
                          (length (orchard--get-hidden))))
         ;; View name for display
         (view-name (pcase orchard--current-view
                      ('working "working")
                      ('all "all")
                      ('next "next")
                      ('qa "qa")
                      ('progress "progress")
                      (_ "working"))))
    (concat
     "\n"
     ;; Header
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
     ;; View indicator
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

     ;; UP NEXT section (issues without worktrees)
     (when (and up-next (orchard--section-visible-p 'up-next))
       (concat
        (orchard--format-section-header "UP NEXT" (length up-next) "available" 'up-next)
        (unless (orchard--section-collapsed-p 'up-next)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     up-next ""))))

     ;; IN PROGRESS section (issues with active worktrees)
     (when (and in-progress (orchard--section-visible-p 'in-progress))
       (concat
        (orchard--format-section-header "IN PROGRESS" (length in-progress) nil 'in-progress)
        (unless (orchard--section-collapsed-p 'in-progress)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path))
                     in-progress ""))))

     ;; QA/VERIFY section (merged PRs, issue still open)
     (when (and qa-verify (orchard--section-visible-p 'qa-verify))
       (concat
        (orchard--format-section-header "QA/VERIFY" (length qa-verify) "merged, issue open" 'qa-verify)
        (unless (orchard--section-collapsed-p 'qa-verify)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     qa-verify ""))))

     ;; DONE section (closed issues, ready to archive)
     (when (and done (orchard--section-visible-p 'done))
       (concat
        (orchard--format-section-header "DONE" (length done) "ready to archive" 'done)
        (unless (orchard--section-collapsed-p 'done)
          (mapconcat (lambda (pair)
                       (orchard--format-issue-with-branch pair current-path t))
                     done ""))))

     ;; UNLINKED BRANCHES section (worktrees without issues)
     (when (and orphan-worktrees (orchard--section-visible-p 'unlinked))
       (concat
        (orchard--format-section-header "UNLINKED BRANCHES" (length orphan-worktrees) nil 'unlinked)
        (unless (orchard--section-collapsed-p 'unlinked)
          (mapconcat (lambda (wt)
                       (orchard--format-orphan-worktree wt current-path))
                     orphan-worktrees ""))))

     ;; RECENT SESSIONS section (only in 'recent' view)
     (when (orchard--section-visible-p 'recent-sessions)
       (orchard--format-recent-sessions))

     ;; Empty state
     (when (and (null up-next) (null in-progress) (null qa-verify)
                (null done) (null orphan-worktrees)
                (not (eq orchard--current-view 'recent)))
       (propertize "\n  No issues or worktrees found. Press I to start from an issue.\n"
                   'face 'font-lock-comment-face))

     ;; Footer
     "\n"
     (propertize "  " 'face 'default)
     (propertize "g" 'face 'orchard-key)
     (propertize " refresh  " 'face 'font-lock-comment-face)
     (propertize "G" 'face 'orchard-key)
     (propertize " force-refresh  " 'face 'font-lock-comment-face)
     (propertize "M" 'face 'orchard-key)
     (propertize " archive done  " 'face 'font-lock-comment-face)
     (propertize "s" 'face 'orchard-key)
     (propertize " toggle staging" 'face 'font-lock-comment-face))))

(defun orchard--column-dedicated-p (column)
  "Return t if COLUMN's window is dedicated."
  (when-let ((win (orchard--get-column-window column)))
    (window-dedicated-p win)))

(defun orchard--format-worktree (wt current-path)
  "Format worktree WT for display. CURRENT-PATH highlights current."
  (let* ((path (alist-get 'path wt))
         (branch (or (alist-get 'branch wt) "(detached)"))
         (port (alist-get 'port wt))
         (dirty (alist-get 'dirty wt))
         (ahead (or (alist-get 'ahead wt) 0))
         (behind (or (alist-get 'behind wt) 0))
         (claude-buf (orchard--claude-buffer-for-path path))
         (claude-status (when claude-buf (orchard--claude-status claude-buf)))  ; waiting, idle, active, or nil
         (needs-attention (memq claude-status '(waiting idle)))
         (column (alist-get 'column wt))
         (column-locked nil)  ; Skip expensive window check during display
         (description (alist-get 'description wt))
         (stage (alist-get 'stage wt))
         (dev-owner (alist-get 'dev-owner wt))
         (branch-mismatch (alist-get 'branch-mismatch wt))
         (is-current (and current-path (string= path current-path)))
         (icon (orchard--branch-icon branch))
         (branch-face (if is-current 'orchard-current (orchard--branch-face branch)))
         ;; Linked GitHub issue (auto-detects from branch name if not explicit)
         (linked-issue-num (orchard--get-worktree-issue path branch))
         (linked-issue (when linked-issue-num
                         (orchard--get-issue-by-number linked-issue-num))))
    (let ((line (concat
                 (if needs-attention
                     (propertize ">>>" 'face '(:foreground "#E06C75" :weight bold))
                   "   ")
                 icon " "
                 (propertize (truncate-string-to-width branch 32 nil nil "…")
                             'face branch-face)
                 " "
                 (propertize (if dirty "●" "○")
                             'face (if dirty 'orchard-dirty 'orchard-clean))
                 (if (> ahead 0) (format " ↑%d" ahead) "")
                 (if (> behind 0) (format " ↓%d" behind) "")
                 ;; Claude status indicator
                 (pcase claude-status
                   ('waiting (propertize " ⏳WAIT" 'face '(:foreground "#E06C75" :weight bold)))
                   ('idle (propertize " ✓DONE" 'face '(:foreground "#E5C07B" :weight bold)))
                   ('active (propertize " ⟳" 'face '(:foreground "#61AFEF")))
                   (_ ""))
                 ;; Dev mode indicator
                 (if dev-owner (propertize " [DEV]" 'face '(:foreground "#E5C07B" :weight bold)) "")
                 ;; Column + lock status
                 (if column
                     (if column-locked
                         (propertize (format " [%d🔒]" column) 'face 'font-lock-keyword-face)
                       (format " [%d]" column))
                   "")
                 ;; Port status - show port number or hint that none is allocated
                 (if port
                     (propertize (format " :%d" (+ 3000 port)) 'face '(:foreground "#98C379"))
                   ;; Only show "no port" for feature branches, not main/dev
                   (when (and branch (not (member branch '("dev" "main" "master"))))
                     (propertize " [+port]" 'face 'font-lock-comment-face)))
                 (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
                 ;; Branch mismatch warning
                 (if branch-mismatch
                     (propertize " ⚠ MISMATCH" 'face 'orchard-branch-mismatch)
                   "")
                 "\n"
                 ;; Stage indicator line (simple: in-progress, pr-open, merged)
                 (when (and stage (not (eq stage 'in-progress)))
                   (concat "     "
                           (propertize (orchard--stage-icon stage) 'face 'default)
                           " "
                           (propertize (orchard--stage-display-name stage)
                                       'face 'font-lock-type-face)
                           "\n"))
                 (if description
                     (concat "     "
                             (propertize (truncate-string-to-width description 60 nil nil "…")
                                         'face 'font-lock-comment-face)
                             "\n")
                   "")
                 ;; Linked GitHub issue
                 (if linked-issue-num
                     (concat "     "
                             (propertize "📎 " 'face 'default)
                             (propertize (format "#%d" linked-issue-num) 'face 'orchard-issue)
                             (when linked-issue
                               (propertize (format ": %s"
                                                   (truncate-string-to-width
                                                    (alist-get 'title linked-issue) 45 nil nil "…"))
                                           'face 'font-lock-comment-face))
                             (when (orchard--issue-closed-p linked-issue-num)
                               (propertize " [closed]" 'face '(:foreground "#98C379")))
                             "\n")
                   "")
                 ;; Branch mismatch detail line
                 (if branch-mismatch
                     (concat "     "
                             (propertize (format "⚠ Expected: %s, Actual: %s"
                                                 (car branch-mismatch)
                                                 (cdr branch-mismatch))
                                         'face 'orchard-branch-mismatch)
                             "\n")
                   ""))))
      (propertize line 'orchard-worktree wt))))

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

(defun orchard--find-next-issue-pos ()
  "Find position of next issue after point, or nil if none."
  (save-excursion
    (let ((current-issue (orchard--get-issue-at-point)))
      (forward-line 1)
      (while (and (not (eobp))
                  (let ((issue (get-text-property (point) 'orchard-issue)))
                    (or (null issue)
                        (and current-issue
                             (equal (alist-get 'number issue)
                                    (alist-get 'number current-issue))))))
        (forward-line 1))
      (if (and (not (eobp))
               (get-text-property (point) 'orchard-issue))
          (point)
        nil))))

(defun orchard--find-prev-issue-pos ()
  "Find position of previous issue before point, or nil if none."
  (save-excursion
    (let ((current-issue (orchard--get-issue-at-point)))
      (forward-line -1)
      (while (and (not (bobp))
                  (let ((issue (get-text-property (point) 'orchard-issue)))
                    (or (null issue)
                        (and current-issue
                             (equal (alist-get 'number issue)
                                    (alist-get 'number current-issue))))))
        (forward-line -1))
      (when (get-text-property (point) 'orchard-issue)
        (point)))))

(defun orchard-next-issue ()
  "Move to next issue."
  (interactive)
  (if-let ((pos (orchard--find-next-issue-pos)))
      (goto-char pos)
    (message "No more issues")))

(defun orchard-prev-issue ()
  "Move to previous issue."
  (interactive)
  (if-let ((pos (orchard--find-prev-issue-pos)))
      (goto-char pos)
    (message "No previous issues")))

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
;;; Issue Diagnostics - Find and Resolve
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
                                   (orchard-open-branch wt))))
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
               (orchard-goto-issue issue-num)))))))))

;;;###autoload
(defun orchard ()
  "Open the Orchard dashboard.
On first open, fetches issues/PRs from GitHub.
Use `g' to refresh locally, `G' to fetch fresh data from GitHub."
  (interactive)
  (orchard--init-columns)
  ;; Populate caches on first open only
  (unless orchard--issues-cache
    (message "Orchard: fetching GitHub data...")
    (orchard--refresh-issues-cache)
    (orchard--refresh-merged-cache))
  (let ((buf (get-buffer-create "*Orchard*")))
    (with-current-buffer buf
      (orchard-mode)
      (orchard-refresh))
    ;; Always show in column 0
    (if (= (length (window-list nil 'no-mini)) 1)
        (switch-to-buffer buf)
      (let ((win (orchard--get-column-window 0)))
        (if win
            (progn (select-window win)
                   (switch-to-buffer buf))
          (switch-to-buffer buf))))))

(defun orchard-refresh ()
  "Refresh the dashboard using cached data (instant).
Use `orchard-force-refresh' (G) to fetch fresh data from git/GitHub."
  (interactive)
  ;; Cleanup stale port allocations (silent unless something was cleaned)
  (when (fboundp 'ghq--cleanup-stale-ports)
    (ghq--cleanup-stale-ports))
  (when (eq major-mode 'orchard-mode)
    (let ((inhibit-read-only t)
          (orchard--inhibit-cache-refresh t)  ; Use cached data only
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (orchard--format-dashboard))
      (goto-char (point-min))
      (forward-line (1- (min line (count-lines (point-min) (point-max))))))))

(defun orchard--find-next-worktree-pos ()
  "Find position of next worktree after point, or nil if none."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point)))
      ;; Move forward until we find a different worktree
      (forward-line 1)
      (while (and (not (eobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree)))
                    (or (null wt)
                        (and current-wt
                             (equal (alist-get 'path wt)
                                    (alist-get 'path current-wt))))))
        (forward-line 1))
      (if (and (not (eobp))
               (get-text-property (point) 'orchard-worktree))
          (point)
        nil))))

(defun orchard--find-prev-worktree-pos ()
  "Find position of previous worktree before point, or nil if none."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point)))
      ;; Move backward until we find a different worktree
      (forward-line -1)
      (while (and (not (bobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree)))
                    (or (null wt)
                        (and current-wt
                             (equal (alist-get 'path wt)
                                    (alist-get 'path current-wt))))))
        (forward-line -1))
      ;; Now find the start of this worktree's block
      (when (get-text-property (point) 'orchard-worktree)
        (let ((this-wt (get-text-property (point) 'orchard-worktree)))
          (while (and (not (bobp))
                      (let ((wt (get-text-property (1- (line-beginning-position)) 'orchard-worktree)))
                        (and wt (equal (alist-get 'path wt)
                                       (alist-get 'path this-wt)))))
            (forward-line -1))
          (point))))))

(defun orchard-next-worktree ()
  "Move to next worktree (branch)."
  (interactive)
  (if-let ((pos (orchard--find-next-worktree-pos)))
      (goto-char pos)
    (message "No more worktrees")))

(defun orchard-prev-worktree ()
  "Move to previous worktree (branch)."
  (interactive)
  (if-let ((pos (orchard--find-prev-worktree-pos)))
      (goto-char pos)
    (message "No previous worktrees")))

(defun orchard--find-next-item-pos ()
  "Find position of next item (worktree or issue) after point."
  (save-excursion
    (let ((start-pos (point))
          (current-wt (orchard--get-worktree-at-point))
          (current-issue (orchard--get-issue-at-point)))
      (forward-line 1)
      (while (and (not (eobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree))
                        (issue (get-text-property (point) 'orchard-issue)))
                    (or (and (null wt) (null issue))
                        ;; Still on same worktree
                        (and wt current-wt
                             (equal (alist-get 'path wt)
                                    (alist-get 'path current-wt)))
                        ;; Still on same issue
                        (and issue current-issue
                             (equal (alist-get 'number issue)
                                    (alist-get 'number current-issue))))))
        (forward-line 1))
      (when (or (get-text-property (point) 'orchard-worktree)
                (get-text-property (point) 'orchard-issue))
        (point)))))

(defun orchard--find-prev-item-pos ()
  "Find position of previous item (worktree or issue) before point."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point))
          (current-issue (orchard--get-issue-at-point)))
      (forward-line -1)
      (while (and (not (bobp))
                  (let ((wt (get-text-property (point) 'orchard-worktree))
                        (issue (get-text-property (point) 'orchard-issue)))
                    (or (and (null wt) (null issue))
                        (and wt current-wt
                             (equal (alist-get 'path wt)
                                    (alist-get 'path current-wt)))
                        (and issue current-issue
                             (equal (alist-get 'number issue)
                                    (alist-get 'number current-issue))))))
        (forward-line -1))
      ;; Find the start of this item's block
      (when (or (get-text-property (point) 'orchard-worktree)
                (get-text-property (point) 'orchard-issue))
        (point)))))

(defun orchard-next-item ()
  "Move to next item (worktree or issue)."
  (interactive)
  (if-let ((pos (orchard--find-next-item-pos)))
      (goto-char pos)
    (message "No more items")))

(defun orchard-prev-item ()
  "Move to previous item (worktree or issue)."
  (interactive)
  (if-let ((pos (orchard--find-prev-item-pos)))
      (goto-char pos)
    (message "No previous items")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Actions
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

;;; ────────────────────────────────────────────────────────────────────────────
;;; Hide/Show Functions
;;; ────────────────────────────────────────────────────────────────────────────

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

;;; ────────────────────────────────────────────────────────────────────────────
;;; View Presets
;;; ────────────────────────────────────────────────────────────────────────────

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

;;; ────────────────────────────────────────────────────────────────────────────
;;; Section Collapsing
;;; ────────────────────────────────────────────────────────────────────────────

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

;;; ────────────────────────────────────────────────────────────────────────────
;;; Filter Transient Menu
;;; ────────────────────────────────────────────────────────────────────────────

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
   ("/" "Search by label" orchard-filter-by-label)
   ("\\" "Clear label filter" orchard-clear-label-filter)
   ("s" "Toggle staging" orchard-toggle-staging-issues)]
  ["Hidden"
   ("-" "Hide at point" orchard-hide-at-point)
   ("H" "Show/unhide items" orchard-show-hidden)]
  ["Sections"
   ("TAB" "Toggle section" orchard-toggle-section)
   ("E" "Expand all" orchard-expand-all-sections)])

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

(defun orchard-hide-at-point ()
  "Hide worktree at point from orchard (does not delete anything).
Use `orchard-show-hidden' to see and unhide."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt)))
        ;; Remove from column tracking
        (when-let ((col (orchard--column-for-branch branch)))
          (remhash branch orchard--branch-to-column))
        ;; Hide it
        (orchard--hide-worktree path)
        (orchard-refresh)
        (message "Hidden %s (use 'H' to show hidden)" branch))
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

(defun orchard-show-hidden ()
  "Show hidden worktrees and allow unhiding them."
  (interactive)
  (let ((hidden (orchard--get-hidden)))
    (if (null hidden)
        (message "No hidden worktrees")
      (let* ((choices (mapcar (lambda (path)
                                (cons (file-name-nondirectory (directory-file-name path)) path))
                              hidden))
             (selected (completing-read "Unhide worktree: " choices nil t)))
        (when selected
          (let ((path (cdr (assoc selected choices))))
            (orchard--unhide-worktree path)
            (orchard-refresh)
            (message "Unhidden %s" selected)))))))

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

(defun orchard-test-at-point ()
  "Start testicular (manual testing) for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let ((default-directory (alist-get 'path wt)))
        (if (fboundp 'testicular-start)
            (testicular-start)
          (user-error "testicular not loaded - check config-testicular.el")))
    (user-error "No worktree at point")))

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
  (let ((label-names (mapcar (lambda (l) (downcase (alist-get 'name l))) labels)))
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

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
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
    ("/" "Filter by label" orchard-filter-by-label)
    ("\\" "Clear label filter" orchard-clear-label-filter)
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
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Global
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
;; C-c M is now meetings prefix, so only M-m for orchard-cycle
(define-key ashton-mode-map (kbd "M-m") #'orchard-cycle-mode)

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

(provide 'config-orchard)
;;; config-orchard.el ends here
