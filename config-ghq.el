;;; ~/.doom.d/config-ghq.el -*- lexical-binding: t; -*-

;; GHQ + Git Worktree management for multi-branch development
;; Replaces numbered workspace pattern with on-demand worktrees
;;
;; Dependencies:
;;   - ghq CLI: /usr/local/go/bin/go install github.com/x-motemen/ghq@latest
;;   - git worktree (built into git)
;;   - claude-code.el for Claude integration
;;
;; Keybindings (C-c g prefix - "g" for ghq/git):
;;   C-c g g - Jump to any ghq-managed repo
;;   C-c g c - Clone a repo via ghq
;;   C-c g w - Create worktree for feature branch + start Claude
;;   C-c g l - List worktrees for current repo
;;   C-c g d - Remove worktree (after merge)
;;   C-c g j - Jump between worktrees of same project
;;   C-c g ? - Transient menu

(require 'transient)

;;; Configuration

(defgroup ghq nil
  "GHQ and worktree management."
  :group 'tools
  :prefix "ghq-")

(defcustom ghq-root (expand-file-name "~/ghq")
  "Root directory for ghq-managed repositories."
  :type 'directory
  :group 'ghq)

(defcustom ghq-command (or (executable-find "ghq")
                           (expand-file-name "~/go/bin/ghq"))
  "Path to the ghq command."
  :type 'file
  :group 'ghq)

(defcustom ghq-worktree-port-registry (expand-file-name "~/.ghq-worktree-ports")
  "File to store worktree-to-port mappings."
  :type 'file
  :group 'ghq)

(defcustom ghq-base-port 3000
  "Base port number for worktree allocation."
  :type 'integer
  :group 'ghq)

(defcustom ghq-port-increment 100
  "Port increment between worktrees (for services using port ranges)."
  :type 'integer
  :group 'ghq)

(defcustom ghq-max-worktrees 10
  "Maximum number of concurrent worktrees (port slots 0-9)."
  :type 'integer
  :group 'ghq)

;;; Port Allocation

(defun ghq--load-port-registry ()
  "Load the port registry from file. Returns alist of (path . port-num)."
  (if (file-exists-p ghq-worktree-port-registry)
      (with-temp-buffer
        (insert-file-contents ghq-worktree-port-registry)
        (read (buffer-string)))
    '()))

(defun ghq--save-port-registry (registry)
  "Save REGISTRY to file."
  (with-temp-file ghq-worktree-port-registry
    (prin1 registry (current-buffer))))

(defun ghq--allocate-port ()
  "Allocate the next available port number (0 to ghq-max-worktrees-1)."
  (let* ((registry (ghq--load-port-registry))
         (used-ports (mapcar #'cdr registry)))
    (cl-loop for n from 0 below ghq-max-worktrees
             unless (member n used-ports)
             return n
             finally (error "No available port slots (max %d worktrees)" ghq-max-worktrees))))

(defun ghq--register-worktree (path port-num)
  "Register worktree PATH with PORT-NUM in registry."
  (let ((registry (ghq--load-port-registry)))
    (push (cons path port-num) registry)
    (ghq--save-port-registry registry)))

(defun ghq--unregister-worktree (path)
  "Remove worktree PATH from registry."
  (let ((registry (ghq--load-port-registry)))
    (setq registry (cl-remove-if (lambda (entry) (string= (car entry) path)) registry))
    (ghq--save-port-registry registry)))

(defun ghq--get-worktree-port (path)
  "Get port number for worktree at PATH, or nil if not registered."
  (let ((registry (ghq--load-port-registry)))
    (cdr (assoc path registry))))

(defun ghq--cleanup-stale-ports ()
  "Remove port allocations for worktrees that no longer exist.
Returns the number of stale entries removed."
  (let* ((registry (ghq--load-port-registry))
         (valid-entries (cl-remove-if-not
                         (lambda (entry)
                           (file-directory-p (car entry)))
                         registry))
         (removed-count (- (length registry) (length valid-entries))))
    (when (> removed-count 0)
      (ghq--save-port-registry valid-entries)
      (message "Cleaned up %d stale port allocation(s)" removed-count))
    removed-count))

(defun ghq--allocate-port-for-path (path)
  "Allocate a port for PATH and register it. Returns port number or nil."
  (condition-case nil
      (let ((port-num (ghq--allocate-port)))
        (ghq--register-worktree path port-num)
        port-num)
    (error nil)))

(defun ghq--port-slots-status ()
  "Return (used . total) port slots."
  (let ((registry (ghq--load-port-registry)))
    (cons (length registry) ghq-max-worktrees)))

;;; GHQ Commands

(defun ghq--list-repos ()
  "Return list of all ghq-managed repository paths."
  (let ((output (shell-command-to-string (format "%s list -p" ghq-command))))
    (split-string output "\n" t)))

;;;###autoload
(defun ghq-find-repo ()
  "Jump to a ghq-managed repository using completion."
  (interactive)
  (let* ((repos (ghq--list-repos))
         (display-repos (mapcar (lambda (path)
                                  (cons (string-remove-prefix (concat ghq-root "/") path) path))
                                repos))
         (selection (completing-read "Repository: " display-repos nil t)))
    (when selection
      (let ((path (cdr (assoc selection display-repos))))
        (if (file-directory-p path)
            (dired path)
          (message "Repository not found: %s" path))))))

;;;###autoload
(defun ghq-clone (url)
  "Clone repository URL using ghq."
  (interactive "sRepository URL or owner/repo: ")
  (let ((default-directory ghq-root))
    (async-shell-command (format "%s get %s" ghq-command url) "*ghq-clone*")))

;;; Worktree Commands

(defun ghq--repo-root ()
  "Get the git repository root for current directory."
  (let ((root (locate-dominating-file default-directory ".git")))
    (when root (expand-file-name root))))

(defun ghq--worktree-list ()
  "Return list of worktrees for current repo as alists."
  (let ((default-directory (or (ghq--repo-root) default-directory)))
    (when (ghq--repo-root)
      (let ((output (shell-command-to-string "git worktree list --porcelain")))
        (ghq--parse-worktree-porcelain output)))))

(defun ghq--parse-worktree-porcelain (output)
  "Parse OUTPUT from git worktree list --porcelain into list of alists."
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
        (push (cons 'branch (substring line 7)) current))
       ((string= "bare" line)
        (push (cons 'bare t) current))
       ((string= "detached" line)
        (push (cons 'detached t) current))))
    (when current (push current worktrees))
    (nreverse worktrees)))

(defun ghq--normalize-branch-name (name)
  "Normalize NAME for use in branch names and filesystem paths.
Converts spaces to dashes, removes special chars, collapses multiple dashes."
  (let ((normalized name))
    ;; Convert spaces and underscores to dashes
    (setq normalized (replace-regexp-in-string "[ _]+" "-" normalized))
    ;; Remove any characters that aren't alphanumeric or dashes
    (setq normalized (replace-regexp-in-string "[^a-zA-Z0-9-]" "" normalized))
    ;; Collapse multiple dashes into one
    (setq normalized (replace-regexp-in-string "-+" "-" normalized))
    ;; Remove leading/trailing dashes
    (setq normalized (replace-regexp-in-string "^-+\\|-+$" "" normalized))
    ;; Convert to lowercase for consistency
    (downcase normalized)))

(defun ghq--worktree-sibling-path (branch-name)
  "Generate sibling worktree path for BRANCH-NAME."
  (let* ((repo-root (ghq--repo-root))
         (repo-name (file-name-nondirectory (directory-file-name repo-root)))
         (parent (file-name-directory (directory-file-name repo-root)))
         ;; Sanitize branch name for filesystem (slashes, spaces, special chars)
         (safe-branch (ghq--normalize-branch-name branch-name)))
    (expand-file-name (concat repo-name "--" safe-branch) parent)))

;;;###autoload
(defun ghq-worktree-create (branch-name)
  "Create a new worktree for BRANCH-NAME and start Claude there."
  (interactive "sBranch name (without FEAT/ prefix): ")
  (let* ((repo-root (ghq--repo-root))
         ;; Normalize the branch name (handles spaces, special chars, etc.)
         (normalized-name (ghq--normalize-branch-name branch-name))
         (full-branch (if (string-prefix-p "feat-" normalized-name)
                          (concat "FEAT/" (substring normalized-name 5))
                        (concat "FEAT/" normalized-name)))
         (worktree-path (ghq--worktree-sibling-path full-branch))
         (port-num (ghq--allocate-port)))
    (unless repo-root
      (user-error "Not in a git repository"))
    (when (file-exists-p worktree-path)
      (user-error "Worktree path already exists: %s" worktree-path))
    ;; Create the worktree
    (let ((default-directory repo-root))
      (message "Creating worktree %s on branch %s..." worktree-path full-branch)
      (shell-command (format "git worktree add -b %s %s"
                             (shell-quote-argument full-branch)
                             (shell-quote-argument worktree-path))))
    ;; Register port allocation
    (ghq--register-worktree worktree-path port-num)
    ;; Generate workspace env
    (ghq--generate-workspace-env worktree-path port-num)
    ;; Open and start Claude
    (dired worktree-path)
    (message "Created worktree at %s (port slot %d)" worktree-path port-num)
    ;; Start Claude if claude-code is available
    (when (fboundp 'claude-code)
      (let ((default-directory worktree-path))
        (claude-code)))))

(defun ghq--generate-workspace-env (worktree-path port-num)
  "Generate .env.workspace for WORKTREE-PATH with PORT-NUM."
  (let ((env-file (expand-file-name ".env.workspace" worktree-path))
        (generate-script (expand-file-name "bin/generate-workspace-env.sh" worktree-path)))
    ;; First create .workspace file with the port number
    (with-temp-file (expand-file-name ".workspace" worktree-path)
      (insert (format "WORKSPACE_NUM=%d\n" port-num)))
    ;; Run generate script if it exists
    (if (file-executable-p generate-script)
        (let ((default-directory worktree-path))
          (shell-command generate-script))
      ;; Otherwise create minimal .env.workspace
      (with-temp-file env-file
        (insert (format "# Generated by ghq-worktree-create\n"))
        (insert (format "WORKSPACE_NUM=%d\n" port-num))
        (insert (format "NEXT_PORT=%d\n" (+ ghq-base-port port-num)))))))

;;;###autoload
(defun ghq-worktree-list ()
  "Show worktrees for current repository."
  (interactive)
  (let ((worktrees (ghq--worktree-list)))
    (if worktrees
        (let ((buf (get-buffer-create "*Worktrees*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize "Git Worktrees\n" 'face 'bold))
              (insert (make-string 60 ?-) "\n")
              (dolist (wt worktrees)
                (let* ((path (alist-get 'path wt))
                       (branch (or (alist-get 'branch wt) "(detached)"))
                       (port (ghq--get-worktree-port path))
                       (short-branch (replace-regexp-in-string "^refs/heads/" "" branch)))
                  (insert (format "%-40s %s"
                                  (propertize short-branch 'face 'font-lock-keyword-face)
                                  path))
                  (when port
                    (insert (format " [port:%d]" port)))
                  (insert "\n")))
              (insert (make-string 60 ?-) "\n")
              (insert (propertize "Press 'q' to close" 'face 'font-lock-comment-face)))
            (special-mode)
            (local-set-key (kbd "q") #'quit-window))
          (pop-to-buffer buf))
      (message "No worktrees found (or not in a git repository)"))))

;;;###autoload
(defun ghq-worktree-remove ()
  "Remove a worktree (prompts for selection)."
  (interactive)
  (let* ((worktrees (ghq--worktree-list))
         (choices (mapcar (lambda (wt)
                            (let ((path (alist-get 'path wt))
                                  (branch (or (alist-get 'branch wt) "(detached)")))
                              (cons (format "%s (%s)"
                                            (replace-regexp-in-string "^refs/heads/" "" branch)
                                            path)
                                    path)))
                          worktrees))
         (selection (completing-read "Remove worktree: " choices nil t)))
    (when selection
      (let ((path (cdr (assoc selection choices))))
        (when (yes-or-no-p (format "Remove worktree at %s? " path))
          (shell-command (format "git worktree remove %s" (shell-quote-argument path)))
          (ghq--unregister-worktree path)
          (message "Removed worktree: %s" path))))))

;;;###autoload
(defun ghq-worktree-jump ()
  "Jump to another worktree of the current repository."
  (interactive)
  (let* ((worktrees (ghq--worktree-list))
         (choices (mapcar (lambda (wt)
                            (let ((path (alist-get 'path wt))
                                  (branch (or (alist-get 'branch wt) "(detached)")))
                              (cons (replace-regexp-in-string "^refs/heads/" "" branch)
                                    path)))
                          worktrees))
         (selection (completing-read "Jump to worktree: " choices nil t)))
    (when selection
      (let ((path (cdr (assoc selection choices))))
        (dired path)))))

;;; Transient Menu

(transient-define-prefix ghq-transient ()
  "GHQ and worktree management."
  ["GHQ - Repository Management"
   ["Repositories"
    ("g" "Find repo" ghq-find-repo)
    ("c" "Clone repo" ghq-clone)]
   ["Worktrees"
    ("w" "Create worktree + Claude" ghq-worktree-create)
    ("l" "List worktrees" ghq-worktree-list)
    ("j" "Jump to worktree" ghq-worktree-jump)
    ("d" "Remove worktree" ghq-worktree-remove)]
   ["Other"
    ("?" "This menu" ghq-transient)
    ("q" "Quit" transient-quit-one)]])

;;; Keybindings

;; Note: These shadow some existing C-c g bindings from config-bindings.el
;; Consider if you want to reorganize (C-c g for ghq, move goto bindings elsewhere)
(define-key ashton-mode-map (kbd "C-c G g") #'ghq-find-repo)
(define-key ashton-mode-map (kbd "C-c G c") #'ghq-clone)
(define-key ashton-mode-map (kbd "C-c G w") #'ghq-worktree-create)
(define-key ashton-mode-map (kbd "C-c G l") #'ghq-worktree-list)
(define-key ashton-mode-map (kbd "C-c G j") #'ghq-worktree-jump)
(define-key ashton-mode-map (kbd "C-c G d") #'ghq-worktree-remove)
(define-key ashton-mode-map (kbd "C-c G ?") #'ghq-transient)

(provide 'config-ghq)
;;; config-ghq.el ends here
