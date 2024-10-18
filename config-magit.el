(global-set-key (kbd "C-c m") 'magit-status)

(with-eval-after-load "magit" (define-key magit-process-mode-map (kbd "<return>") 'find-file-at-point-with-line))

;;; ../src/home/.doom.d/config-org-mode.el -*- lexical-binding: t; -*-

(with-eval-after-load "magit"
  (define-key magit-mode-map (kbd "C-c n") 'forge-create-pullreq)
  (define-key magit-mode-map (kbd "C-c w") 'forge-browse-dwim)
  (define-key magit-mode-map (kbd "C-c a") 'magit-abort-dwim)
  (define-key magit-mode-map [tab] 'magit-section-toggle)
  )

;;(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
