;;; config-ruff.el -*- lexical-binding: t; -*-

;;(add-hook 'python-mode-hook #'flymake-ruff-load)

;; Replace default (black) to use ruff for sorting import and formatting.
;; (setf (alist-get 'python-mode apheleia-mode-alist)
;;       '(ruff-isort ruff))
;; (setf (alist-get 'python-ts-mode apheleia-mode-alist)
;;       '(ruff-isort ruff))
