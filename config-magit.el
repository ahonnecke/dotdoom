(global-set-key (kbd "C-c m") 'magit-status)

;; Override M-m in dirvish-mode to use magit-status
(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "M-m") nil)  ; First unbind it
  (define-key dirvish-mode-map (kbd "M-m") 'magit-status))

(with-eval-after-load "magit"
  (define-key magit-process-mode-map (kbd "<return>")
              'find-file-at-point-with-line))


;;; ../src/home/.doom.d/config-org-mode.el -*- lexical-binding: t; -*-

(with-eval-after-load "magit"
  (define-key magit-mode-map (kbd "C-c n") 'forge-create-pullreq)
  (define-key magit-mode-map (kbd "C-c w") 'forge-browse-dwim)
  (define-key magit-mode-map (kbd "C-c a") 'magit-abort-dwim)
  (define-key magit-mode-map [tab] 'magit-section-toggle)
  )

;;(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

(defun magit-branch-alter-name ()
  "Rename the current branch, pre-filling the minibuffer with the existing branch name."
  (interactive)
  (let* ((current-branch (magit-get-current-branch))
         (new-name (read-string "Modify branch name: " current-branch))) ; Pre-fill current name
    (unless (string= current-branch new-name) ; Avoid renaming if names are the same
      (magit-branch-rename current-branch new-name))))

(defun magit-fetch-create-clean-branch ()
  "Fetch all, create a new branch from main, and merge the current branch into it as a no-commit merge."
  (interactive)
  (let* ((current-branch (magit-get-current-branch))
         (clean-branch-name (concat current-branch "-clean"))
         (main-branch "main")) ;; Adjust if your main branch has a different name
    (magit-git-fetch nil '("--all")) ;; Fetch all remotes
    (magit-branch-create clean-branch-name main-branch) ;; Create the new branch from main
    (magit-checkout clean-branch-name) ;; Switch to the new branch
    (magit-merge current-branch 'no-commit))) ;; No-commit merge

;; (with-eval-after-load 'magit
;;   (define-key magit-mode-map (kbd "C-c c") 'magit-fetch-create-clean-branch)) ;;

(with-eval-after-load 'magit-branch
  (transient-append-suffix 'magit-branch
    "r" ;; Insert after "r" (rename)
    '("a" "Alter to name" magit-branch-alter-name)))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; When editing files over TRAMP, don't try to run remote git commit
(setq tramp-git-commit-style 'separate)

(defun magit-pop-stash-without-index ()
  "Apply the latest stash (stash@{0}) without dropping it, using 'git stash apply'."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (when (not default-directory)
      (user-error "Not inside a Git repository"))
    (magit-run-git "stash" "apply" "stash@{0}")))
