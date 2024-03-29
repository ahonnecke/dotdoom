;; ;;; ../src/home/.doom.d/config-flycheck-ruff.el -*- lexical-binding: t; -*-
;; ;;; This file gets ignored by .gitignore because it has flycheck in the name

;; (with-eval-after-load "flycheck"
;;   (setq flycheck-python-ruff-executable "/home/ahonnecke/.pyenv/shims/ruff")
;;   (flycheck-define-checker python-ruff
;;     "A Python syntax and style checker using the ruff utility.
;; To override the path to the ruff executable, set
;; `flycheck-python-ruff-executable'.
;; See URL `http://pypi.python.org/pypi/ruff'."
;;     :command ("ruff"
;;               "--format=text"
;;               (eval (when buffer-file-name
;;                       (concat "--stdin-filename=" buffer-file-name)))
;;               "-")
;;     :standard-input t
;;     :error-filter (lambda (errors)
;;                     (let ((errors (flycheck-sanitize-errors errors)))
;;                       (seq-map #'flycheck-flake8-fix-error-level errors)))
;;     :error-patterns
;;     ((warning line-start
;;               (file-name) ":" line ":" (optional column ":") " "
;;               (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;               (message (one-or-more not-newline))
;;               line-end))
;;     :modes python-mode)

;;   (add-to-list 'flycheck-checkers 'python-ruff)

;;   ;; this breaks lsp autocomplete..
;;   ;; (add-hook 'python-mode-hook (lambda () (flycheck-select-checker
;;   ;; "python-ruff")))

;; ;;   ;; Seems to not break autocomplete... but also does not work
;; ;; (with-eval-after-load "lsp"
;; ;;   (flycheck-add-next-checker 'lsp 'python-ruff)
;; ;;   )

;;   )

(require 'flycheck)

;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--output-format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-ts-mode)

;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
;; This is an MVP example:
(setq python-ts-mode-hook
      (list (defun my-python-hook ()
              (unless (bound-and-true-p org-src-mode)
                (when (buffer-file-name)
                  (setq-local flycheck-checkers '(python-ruff))
                  (flycheck-mode))))))
