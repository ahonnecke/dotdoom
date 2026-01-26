;;; orchard-worktree.el --- Orchard worktree data and detection -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains worktree-related logic:
;; - Worktree data retrieval and parsing
;; - Orphan directory detection
;; - Prunable worktree detection
;; - Branch mismatch detection
;; - Feature descriptions

(require 'orchard-vars)
(require 'orchard-cache)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard--claude-buffer-for-path "orchard-claude")
(declare-function orchard--claude-process-running-p "orchard-claude")
(declare-function orchard--column-for-branch "orchard-window")
(declare-function orchard-refresh "orchard-dashboard")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Worktree Data
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--feature-descriptions (make-hash-table :test 'equal)
  "Hash table mapping worktree paths to feature descriptions.")

(defun orchard--get-repo-root ()
  "Get the repository root from config.
If `orchard-repo-path' is not set, attempts to detect from current directory
only if it looks like a project worktree (not random git repos like ~/.doom.d)."
  (or orchard-repo-path
      ;; Only fall back to detection if we're in a likely worktree directory
      ;; (under orchard-worktree-parent with branch-like naming)
      (let ((git-root (locate-dominating-file default-directory ".git")))
        (when (and git-root
                   orchard-worktree-parent
                   (string-prefix-p (expand-file-name orchard-worktree-parent)
                                    (expand-file-name git-root)))
          git-root))))

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
        (when (fboundp 'orchard--claude-buffer-for-path)
          (let ((claude-buf (orchard--claude-buffer-for-path path)))
            (when (and claude-buf (buffer-live-p claude-buf))
              (let ((running (orchard--claude-process-running-p claude-buf)))
                (push (cons 'claude-status (if running 'running 'stopped)) wt)))))
        ;; Column assignment
        (when (and branch (fboundp 'orchard--column-for-branch))
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

;;; ════════════════════════════════════════════════════════════════════════════
;;; Feature Descriptions
;;; ════════════════════════════════════════════════════════════════════════════

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

(provide 'orchard-worktree)
;;; orchard-worktree.el ends here
