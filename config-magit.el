(global-set-key (kbd "C-c m") 'magit-status)

;; Override M-m in dirvish-mode to use magit-status
(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "M-m") nil)  ; First unbind it
  (define-key dirvish-mode-map (kbd "M-m") 'magit-status))

(with-eval-after-load "magit" (define-key magit-process-mode-map (kbd "<return>") 'find-file-at-point-with-line))

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

(with-eval-after-load 'magit-branch
  (transient-append-suffix 'magit-branch
    "r" ;; Insert after "r" (rename)
    '("a" "Alter to name" magit-branch-alter-name)))
