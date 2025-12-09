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
;;; Hidden Worktrees (dismiss from orchard without deleting)
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--get-hidden ()
  "Get list of hidden worktree paths."
  (plist-get orchard--state :hidden))

(defun orchard--hide-worktree (path)
  "Hide worktree at PATH from orchard display."
  (let ((hidden (plist-get orchard--state :hidden)))
    (unless (member path hidden)
      (push path hidden)
      (setq orchard--state (plist-put orchard--state :hidden hidden))
      (orchard--save-state))))

(defun orchard--unhide-worktree (path)
  "Unhide worktree at PATH."
  (let ((hidden (plist-get orchard--state :hidden)))
    (setq hidden (delete path hidden))
    (setq orchard--state (plist-put orchard--state :hidden hidden))
    (orchard--save-state)))

(defun orchard--worktree-hidden-p (path)
  "Return t if worktree at PATH is hidden."
  (member path (orchard--get-hidden)))

(defun orchard--is-main-worktree-p (path)
  "Return t if PATH is the main (bare) worktree."
  (let ((repo-root (orchard--get-repo-root)))
    (and repo-root
         (string= (file-name-as-directory (expand-file-name path))
                  (file-name-as-directory (expand-file-name repo-root))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Stage Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--detect-stage (path)
  "Detect the current stage for worktree at PATH."
  (let ((requirements-file (expand-file-name ".feature-requirements" path))
        (test-plan-file (expand-file-name ".test-plan.md" path))
        (pr-url-file (expand-file-name ".pr-url" path))
        (test-results (orchard--get-test-results path)))
    (cond
     ;; Check for manual override first
     ((orchard--get-stage-override path))
     ;; PR is open
     ((file-exists-p pr-url-file) 'pr-open)
     ;; Tests complete and all passed
     ((and test-results (plist-get test-results :complete)
           (zerop (plist-get test-results :failed)))
      'ready-to-pr)
     ;; Testicular session active
     ((and (boundp 'testicular-project-root)
           testicular-project-root
           (string= (file-name-as-directory path)
                    (file-name-as-directory testicular-project-root)))
      'testing)
     ;; Has test plan
     ((file-exists-p test-plan-file) 'ready-to-test)
     ;; Has requirements
     ((file-exists-p requirements-file) 'requirements)
     ;; Default
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

(defun orchard--stage-display-name (stage)
  "Get display name for STAGE."
  (pcase stage
    ('requirements "Requirements")
    ('in-progress "In Progress")
    ('ready-to-test "Ready to Test")
    ('testing "Testing")
    ('ready-to-pr "Ready for PR")
    ('pr-open "PR Open")
    ('merged "Merged")
    (_ (symbol-name stage))))

(defun orchard--stage-icon (stage)
  "Get icon for STAGE."
  (pcase stage
    ('requirements "📋")
    ('in-progress "🔧")
    ('ready-to-test "📝")
    ('testing "🧪")
    ('ready-to-pr "✅")
    ('pr-open "🔀")
    ('merged "🎉")
    (_ "❓")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hook Handlers
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--on-commando-start (command path)
  "Handle commando command start. Track dev mode ownership."
  (when (and (boundp 'commando-dev-commands)
             (member command commando-dev-commands))
    ;; Check if another worktree owns dev
    (when (and orchard--dev-owner
               (not (string= path orchard--dev-owner)))
      (message "Warning: %s already owns dev mode"
               (file-name-nondirectory (directory-file-name orchard--dev-owner))))
    (orchard--set-dev-owner path)
    (message "Dev mode: %s" (file-name-nondirectory (directory-file-name path)))))

(defun orchard--on-commando-finish (command path status)
  "Handle commando command finish. Release dev mode if needed."
  (when (and (boundp 'commando-dev-commands)
             (member command commando-dev-commands)
             orchard--dev-owner
             (string= path orchard--dev-owner))
    (orchard--set-dev-owner nil)
    (message "Dev mode released")))

(defun orchard--on-testicular-start (project-root)
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
  "Get the window for COLUMN, or nil."
  (let* ((windows (window-list nil 'no-mini))
         (sorted (sort windows (lambda (a b)
                                 (< (car (window-edges a))
                                    (car (window-edges b)))))))
    (nth column sorted)))

(defun orchard--ensure-columns ()
  "Ensure we have the right number of columns.
Creates columns if needed, up to orchard-max-columns."
  (let ((current-count (length (window-list nil 'no-mini))))
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
;;; Worktree Data
;;; ════════════════════════════════════════════════════════════════════════════

(defvar orchard--feature-descriptions (make-hash-table :test 'equal)
  "Hash table mapping worktree paths to feature descriptions.")

(defun orchard--get-repo-root ()
  "Get the repository root, either from config or current directory."
  (or orchard-repo-path
      (locate-dominating-file default-directory ".git")))

(defun orchard--get-worktrees (&optional include-hidden)
  "Get all worktrees with status info.
If INCLUDE-HIDDEN is non-nil, include hidden worktrees."
  (let ((repo-root (orchard--get-repo-root)))
    (when (and repo-root (file-directory-p repo-root))
      (let ((default-directory repo-root))
        (let* ((output (shell-command-to-string "git worktree list --porcelain"))
               (worktrees (orchard--parse-worktrees output))
               (enriched (delq nil (mapcar #'orchard--enrich-worktree worktrees))))
          ;; Filter out hidden unless requested
          (if include-hidden
              enriched
            (cl-remove-if (lambda (wt)
                            (orchard--worktree-hidden-p (alist-get 'path wt)))
                          enriched)))))))

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
        ;; Stage tracking
        (push (cons 'stage (orchard--detect-stage path)) wt)
        ;; Dev mode ownership
        (when (and orchard--dev-owner
                   (string= (file-name-as-directory path)
                            (file-name-as-directory orchard--dev-owner)))
          (push (cons 'dev-owner t) wt))
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
                       (string-match-p (regexp-quote name) (buffer-name buf))))
                (buffer-list))))

(defun orchard--get-claude-buffers ()
  "Get all Claude buffers."
  (cl-remove-if-not
   (lambda (buf)
     (string-prefix-p "*claude:" (buffer-name buf)))
   (buffer-list)))

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

(defun orchard-cycle-mode ()
  "Cycle between magit and claude in the current column.
If in magit → switch to claude (start if needed).
If in claude → switch to magit.
If in compile → close compile, go to magit.

Works both in orchard-managed worktrees and regular git repos."
  (interactive)
  (let* ((wt (orchard--current-worktree))
         (path (or (when wt (alist-get 'path wt))
                   (orchard--project-root)))  ; Fallback to git root
         (branch (when wt (alist-get 'branch wt))))
    (cond
     ;; In compilation buffer - close it, go to magit
     ((derived-mode-p 'compilation-mode)
      (quit-window)
      (when path
        (magit-status path)))
     ;; In magit - go to claude
     ((derived-mode-p 'magit-mode)
      (if path
          (let ((claude-buf (orchard--claude-buffer-for-path path)))
            (if claude-buf
                (switch-to-buffer claude-buf)
              ;; Start new Claude
              (orchard--ensure-claude-loaded)
              (let ((default-directory path))
                (claude-code))))
        ;; Fallback: start claude in current directory
        (orchard--ensure-claude-loaded)
        (claude-code)))
     ;; In vterm/claude - go to magit
     ((derived-mode-p 'vterm-mode)
      (if path
          (magit-status path)
        (magit-status)))  ; Fallback to magit in current dir
     ;; Elsewhere in a worktree - go to magit
     (path
      (magit-status path))
     ;; Not in a worktree - just open magit
     (t
      (magit-status)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Branch Opening - Column Assignment
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-open-branch (wt)
  "Open worktree WT in its assigned column (or assign one)."
  (let* ((path (alist-get 'path wt))
         (branch (alist-get 'branch wt))
         (existing-col (orchard--column-for-branch branch)))
    (if existing-col
        ;; Already has a column - switch to it
        (let ((win (orchard--get-column-window existing-col)))
          (when win
            (select-window win)
            (magit-status path)))
      ;; Need to assign a column
      (let ((col (orchard--find-available-column)))
        (orchard--assign-branch-to-column branch col)
        ;; Ensure we have enough columns
        (orchard--ensure-columns)
        ;; Switch to that column
        (let ((win (orchard--get-column-window col)))
          (when win
            (select-window win)
            (magit-status path)))))))

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
    ;; Lifecycle actions
    (define-key map (kbd "N") #'orchard-next-step)
    (define-key map (kbd "u") #'orchard-push-at-point)   ; u for "upload"/push
    (define-key map (kbd "P") #'orchard-pr-at-point)
    (define-key map (kbd "-") #'orchard-hide-at-point)    ; hide (just remove from list)
    (define-key map (kbd "H") #'orchard-show-hidden)      ; show/unhide hidden
    (define-key map (kbd "a") #'orchard-archive-at-point) ; archive (remove worktree, keep branch)
    (define-key map (kbd "D") #'orchard-delete-at-point)  ; delete (remove worktree and branch)
    ;; Navigation - branch to branch (all these do the same thing)
    (define-key map (kbd "n") #'orchard-next-worktree)
    (define-key map (kbd "p") #'orchard-prev-worktree)
    (define-key map (kbd "j") #'orchard-next-worktree)
    (define-key map (kbd "k") #'orchard-prev-worktree)
    (define-key map (kbd "C-n") #'orchard-next-worktree)
    (define-key map (kbd "C-p") #'orchard-prev-worktree)
    (define-key map (kbd "<down>") #'orchard-next-worktree)
    (define-key map (kbd "<up>") #'orchard-prev-worktree)
    (define-key map (kbd "TAB") #'orchard-next-worktree)
    (define-key map (kbd "<backtab>") #'orchard-prev-worktree)
    ;; Cleanup
    (define-key map (kbd "C") #'orchard-cleanup)
    (define-key map (kbd "C-c C-c") #'orchard-cleanup-dry-run)
    ;; Refresh and quit
    (define-key map (kbd "g") #'orchard-refresh)
    (define-key map (kbd "r") #'orchard-refresh)
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

(defun orchard--format-dashboard ()
  "Format the Orchard dashboard."
  (let* ((worktrees (orchard--get-worktrees))
         (current (orchard--current-worktree))
         (current-path (when current (alist-get 'path current)))
         (claude-bufs (orchard--get-claude-buffers))
         (running-count (cl-count-if #'orchard--claude-process-running-p claude-bufs))
         (stopped-count (- (length claude-bufs) running-count)))
    (concat
     "\n"
     (propertize "  🌳 Orchard" 'face 'orchard-header)
     (propertize (format "  %d worktrees" (length worktrees)) 'face 'font-lock-comment-face)
     (when (> running-count 0)
       (propertize (format "  %d◉" running-count) 'face 'orchard-claude-running))
     (when (> stopped-count 0)
       (propertize (format "  %d○" stopped-count) 'face 'orchard-claude-stopped))
     "\n\n"
     ;; Quick actions
     (propertize "  " 'face 'default)
     (propertize "[f]" 'face 'orchard-key)
     (propertize " Feat  " 'face 'font-lock-comment-face)
     (propertize "[m]" 'face 'orchard-key)
     (propertize " Magit  " 'face 'font-lock-comment-face)
     (propertize "[c]" 'face 'orchard-key)
     (propertize " Claude  " 'face 'font-lock-comment-face)
     (propertize "[-]" 'face 'orchard-key)
     (propertize " Hide  " 'face 'font-lock-comment-face)
     (propertize "[H]" 'face 'orchard-key)
     (propertize " Unhide" 'face 'font-lock-comment-face)
     "\n\n"
     ;; Worktree list
     (propertize "  Branches\n" 'face 'orchard-subheader)
     (propertize (concat "  " (make-string 70 ?─) "\n") 'face 'font-lock-comment-face)
     (if worktrees
         (mapconcat (lambda (wt)
                      (orchard--format-worktree wt current-path))
                    worktrees "")
       (propertize "  No worktrees found.\n" 'face 'font-lock-comment-face))
     "\n"
     ;; Footer
     (propertize "  " 'face 'default)
     (propertize "?" 'face 'orchard-key)
     (propertize " help  " 'face 'font-lock-comment-face)
     (propertize "M-m" 'face 'orchard-key)
     (propertize " cycle magit/claude  " 'face 'font-lock-comment-face)
     (propertize "`" 'face 'orchard-key)
     (propertize " commando  " 'face 'font-lock-comment-face)
     (propertize "C" 'face 'orchard-key)
     (propertize " cleanup" 'face 'font-lock-comment-face))))

(defun orchard--format-worktree (wt current-path)
  "Format worktree WT for display. CURRENT-PATH highlights current."
  (let* ((path (alist-get 'path wt))
         (branch (or (alist-get 'branch wt) "(detached)"))
         (port (alist-get 'port wt))
         (dirty (alist-get 'dirty wt))
         (ahead (or (alist-get 'ahead wt) 0))
         (behind (or (alist-get 'behind wt) 0))
         (claude-status (alist-get 'claude-status wt))  ; nil, 'running, or 'stopped
         (column (alist-get 'column wt))
         (description (alist-get 'description wt))
         (stage (alist-get 'stage wt))
         (dev-owner (alist-get 'dev-owner wt))
         (is-current (and current-path (string= path current-path)))
         (icon (orchard--branch-icon branch))
         (branch-face (if is-current 'orchard-current (orchard--branch-face branch))))
    (let ((line (concat
                 "  "
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
                   ('running (propertize " ◉" 'face 'orchard-claude-running))
                   ('stopped (propertize " ○" 'face 'orchard-claude-stopped))
                   (_ ""))
                 ;; Dev mode indicator
                 (if dev-owner (propertize " [DEV]" 'face '(:foreground "#E5C07B" :weight bold)) "")
                 (if column (format " [%d]" column) "")
                 (if port (format " :%d" (+ 3000 port)) "")
                 (if is-current (propertize " ← here" 'face 'font-lock-comment-face) "")
                 "\n"
                 ;; Stage indicator line
                 (when stage
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

;;;###autoload
(defun orchard ()
  "Open the Orchard dashboard."
  (interactive)
  (orchard--init-columns)
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
  "Refresh the dashboard."
  (interactive)
  (when (eq major-mode 'orchard-mode)
    (let ((inhibit-read-only t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (orchard--format-dashboard))
      (goto-char (point-min))
      (forward-line (1- (min line (count-lines (point-min) (point-max))))))))

(defun orchard--find-next-worktree-pos ()
  "Find position of next worktree after point, or nil if none."
  (save-excursion
    (let ((current-wt (orchard--get-worktree-at-point))
          (start-pos (point)))
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

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dashboard Actions
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-open-at-point ()
  "Open worktree at point in its column."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (orchard-open-branch wt)
    (user-error "No worktree at point")))

(defun orchard-magit-at-point ()
  "Open magit for worktree at point."
  (interactive)
  (orchard-open-at-point))

(defun orchard-claude-at-point ()
  "Open Claude for worktree at point in its assigned column."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (existing-col (orchard--column-for-branch branch)))
        ;; Ensure branch has a column
        (unless existing-col
          (setq existing-col (orchard--find-available-column))
          (orchard--assign-branch-to-column branch existing-col)
          (orchard--ensure-columns))
        ;; Check for existing Claude buffer
        (let ((claude-buf (orchard--claude-buffer-for-path path)))
          (if (and claude-buf (buffer-live-p claude-buf))
              ;; Existing buffer - display it forcefully
              (progn
                (message "Switching to Claude for %s" branch)
                ;; Try multiple methods to ensure it displays
                (let ((win (orchard--get-column-window existing-col)))
                  (if win
                      (progn
                        (select-window win)
                        (set-window-buffer win claude-buf))
                    ;; Fallback: pop-to-buffer
                    (pop-to-buffer claude-buf))))
            ;; Start new Claude
            (let ((win (orchard--get-column-window existing-col)))
              (when win (select-window win)))
            (message "Starting Claude for %s..." branch)
            (condition-case err
                (progn
                  (orchard--ensure-claude-loaded)
                  (let ((default-directory path))
                    (if (fboundp 'claude-code)
                        (claude-code)
                      (user-error "claude-code function not found"))))
              (error
               (user-error "Failed to start Claude: %s" (error-message-string err)))))))
    (user-error "No worktree at point")))

(defun orchard-dired-at-point ()
  "Open dired for worktree at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (dired (alist-get 'path wt))
    (user-error "No worktree at point")))

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

(defun orchard-pr-at-point ()
  "Create PR for branch at point."
  (interactive)
  (if-let ((wt (orchard--get-worktree-at-point)))
      (let* ((path (alist-get 'path wt))
             (branch (alist-get 'branch wt))
             (description (alist-get 'description wt))
             (default-directory path))
        (message "Pushing %s..." branch)
        (shell-command (format "git push -u origin %s" (shell-quote-argument branch)))
        (let ((title (read-string "PR Title: " (replace-regexp-in-string "^[A-Z]+/" "" branch)))
              (body (read-string "PR Body: " (or description ""))))
          (shell-command
           (format "gh pr create --title %s --body %s --base dev"
                   (shell-quote-argument title)
                   (shell-quote-argument body)))
          (message "Created PR for %s" branch)))
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

(defun orchard-quit-all ()
  "Quit and optionally kill all Claude instances."
  (interactive)
  (let ((claudes (orchard--get-claude-buffers)))
    (when (and claudes
               (yes-or-no-p (format "Kill %d Claude instance(s)? " (length claudes))))
      (dolist (buf claudes)
        (kill-buffer buf))))
  (quit-window))

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

(defun orchard--get-prunable-worktrees ()
  "Get list of worktrees marked as prunable.
These are worktrees whose directories no longer exist or have issues."
  (let ((repo-root (orchard--get-repo-root)))
    (when repo-root
      (let* ((default-directory repo-root)
             (output (shell-command-to-string "git worktree list --porcelain"))
             (lines (split-string output "\n" t))
             (prunable '())
             (current-path nil))
        (dolist (line lines)
          (cond
           ((string-prefix-p "worktree " line)
            (setq current-path (substring line 9)))
           ((string= "prunable" line)
            (when current-path
              (push current-path prunable)))))
        (nreverse prunable)))))

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
             (stage (orchard--detect-stage path)))
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

(defun orchard--create-branch (type name description)
  "Create new branch of TYPE with NAME and DESCRIPTION."
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
                           orchard-worktree-parent)))
         (port-num (when (fboundp 'ghq--allocate-port)
                     (ghq--allocate-port))))
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
    ;; Create worktree
    (message "Creating worktree %s..." full-branch)
    (let ((default-directory repo-root))
      (shell-command (format "git worktree add -b %s %s %s"
                             (shell-quote-argument full-branch)
                             (shell-quote-argument worktree-path)
                             orchard-upstream-branch)))
    ;; Register port
    (when port-num
      (when (fboundp 'ghq--register-worktree)
        (ghq--register-worktree worktree-path port-num))
      (when (fboundp 'ghq--generate-workspace-env)
        (ghq--generate-workspace-env worktree-path port-num)))
    ;; Setup Claude settings
    (orchard--setup-claude-settings worktree-path)
    ;; Save description
    (when (and description (not (string-empty-p description)))
      (orchard--save-feature-description worktree-path description))
    ;; Run post-create hook
    (run-hook-with-args 'orchard-post-create-hook worktree-path)
    ;; Assign to column and open
    (let ((col (orchard--find-available-column)))
      (orchard--assign-branch-to-column full-branch col)
      (orchard--ensure-columns)
      (let ((win (orchard--get-column-window col)))
        (when win
          (select-window win)
          (magit-status worktree-path))))
    ;; Refresh dashboard
    (when-let ((buf (get-buffer "*Orchard*")))
      (with-current-buffer buf (orchard-refresh)))
    (message "✨ Created %s%s" full-branch
             (if port-num (format " (port :%d)" (+ 3000 port-num)) ""))
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
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard-commando-at-point ()
  "Open commando in the worktree at point."
  (interactive)
  (when-let* ((wt (orchard--worktree-at-point))
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
   ["At Point"
    ("RET" "Open in column" orchard-open-at-point)
    ("m" "Magit" orchard-magit-at-point)
    ("c" "Claude" orchard-claude-at-point)
    ("d" "Dired" orchard-dired-at-point)
    ("t" "Test (testicular)" orchard-test-at-point)
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
    ("g" "Refresh" orchard-refresh)
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

;; M-m cycles magit/claude everywhere
;; Also bind C-c m m as backup since M-m can be tricky in some modes
(define-key ashton-mode-map (kbd "M-m") #'orchard-cycle-mode)
(define-key ashton-mode-map (kbd "C-c M") #'orchard-cycle-mode)  ; Backup binding

;; For magit-mode: Use Doom's after! macro for better integration
;; This runs AFTER Doom's own magit configuration
(after! magit
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
      (define-key map (kbd "M-m") #'orchard-cycle-mode)
      (define-key map (kbd "C-c M") #'orchard-cycle-mode))))

;; Hook for buffer-local override (runs late)
(defun orchard--bind-m-m-in-magit ()
  "Bind M-m to orchard-cycle-mode in magit buffers."
  (local-set-key (kbd "M-m") #'orchard-cycle-mode)
  (local-set-key (kbd "C-c M") #'orchard-cycle-mode))

(add-hook 'magit-mode-hook #'orchard--bind-m-m-in-magit 100)

(provide 'config-orchard)
;;; config-orchard.el ends here
