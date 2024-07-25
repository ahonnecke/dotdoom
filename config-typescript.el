;;; ../src/home/.doom.d/config-typescript.el -*- lexical-binding: t; -*-

(add-hook 'typescript-mode
          (lambda ()
            (setq-local fill-column 120)

            (setq-local ffip-patterns '("*.ty"))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(add-hook 'typescript-mode-hook 'turn-off-auto-fill)
;; (add-hook 'typescript-mode-hook
;;           (lambda ()
;;             (comment-auto-fill))))
