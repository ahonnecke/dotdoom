;;; config-ts-yaml.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;; clear paredit bindings paredit mode
;; (eval-after-load "paredit"
;;   #'(define-key paredit-mode-map (kbd "C-j") nil))

;; (eval-after-load "paredit"
;;   #'(define-key paredit-mode-map (kbd "C-c C-j") nil))

;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-c C-j") 'eval-region)
;;              (local-set-key (kbd "C-j") 'eval-last-sexp)))
