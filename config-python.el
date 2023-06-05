;;; ../src/home/.doom.d/config-python.el -*- lexical-binding: t; -*-

;; (defun comment-auto-fill-only-comments ()
;;   (auto-fill-mode 1)
;;   (set (make-local-variable 'fill-nobreak-predicate)
;;        (lambda ()
;;          (not (eq (get-text-property (point) 'face)
;;                   'font-lock-comment-face)))))

;;; Perform isorting py-isort-buffer on save or something
(with-eval-after-load "python"
  (define-key python-mode-map  (kbd "TAB") 'indent-according-to-mode)
  (define-key python-mode-map  (kbd "M-/") '+company/complete)
  (global-set-key  (kbd "C->") 'python-indent-shift-right)
  (global-set-key  (kbd "C-<") 'python-indent-shift-left)

  ;; (add-hook 'before-save-hook 'py-isort-before-save)
  ;;(add-hook 'python-mode-hook '+format|enable-on-save)
  (setq-local lsp-pylsp-plugins-flake8-max-line-length 88)

  ;; https://www.emacswiki.org/emacs/AutoFillMode
  ;; this does not seem to work, it breaks the formatting
  ;;(add-hook 'python-mode-hook 'comment-auto-fill-only-comments)
  )

;; (add-hook 'python-mode-hook (lambda () (
;;                                         (setq-local comment-auto-fill-only-comments t)
;;                                         (auto-fill-mode nil)
;;                                         )))

(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1)))

;; (add-hook 'python-mode-hook (lambda () (
;;                                         ;; (flycheck-select-checker 'python-ruff)
;;                                         ;; (setq lsp-diagnostics-provider :auto)
;;                                         (setq flycheck-checkers (remove 'lsp flycheck-checkers))
;;                                         )))
