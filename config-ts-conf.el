;;; config-conf-ts.el -*- lexical-binding: t; -*-

;;; config-conf-ts.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-mode))

;; clear paredit bindings paredit mode
;; (eval-after-load "paredit"
;;   #'(define-key paredit-mode-map (kbd "C-j") nil))

;; (eval-after-load "paredit"
;;   #'(define-key paredit-mode-map (kbd "C-c C-j") nil))

;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-c C-j") 'eval-region)
;;              (local-set-key (kbd "C-j") 'eval-last-sexp)))

(add-hook 'conf-mode
          #'(lambda ()
              (paredit-mode)
              (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
              ))
