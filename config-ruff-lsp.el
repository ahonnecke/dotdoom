;; ;;; ../src/home/.doom.d/config-ruff-lsp.el -*- lexical-binding: t; -*-

;; ;; (defcustom lsp-ruff-executable "ruff-lsp"
;; ;;   "Command to start the Ruff language server."
;; ;;   :group 'lsp-python
;; ;;   :risky t
;; ;;   :type 'file)

;; ;; ;; Register ruff-lsp with the LSP client.
;; ;; (lsp-register-client
;; ;;     (make-lsp-client
;; ;;         :new-connection (lsp-stdio-connection (lambda () (list lsp-ruff-executable)))
;; ;;         :activation-fn (lsp-activate-on "python")
;; ;;         :add-on? t
;; ;;         :server-id 'ruff
;; ;;         :initialization-options (lambda ()
;; ;;                                     (list :settings
;; ;;                                         (cl-list*
;; ;;                                           (when
;; ;;                                             poetry-project-venv
;; ;;                                                 (list
;; ;;                                                 :interpreter (vector (f-join (f-long poetry-project-venv) "bin" "python3"))
;; ;;                                                 :workspace (f-long poetry-project-venv)
;; ;;                                                 :path (vector (f-join (f-long poetry-project-venv) "bin" "ruff")))
;; ;;                                                 )
;; ;;                                             ))
;; ;;                                     )))


;; ;; (require 'flymake-ruff)
;; ;; (add-hook 'python-mode-hook #'flymake-ruff-load)

;; (require 'flycheck)

;; ;; ;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
;; ;; (flycheck-define-checker python-ruff
;; ;;   "A Python syntax and style checker using the ruff utility.
;; ;; To override the path to the ruff executable, set
;; ;; `flycheck-python-ruff-executable'.
;; ;; See URL `http://pypi.python.org/pypi/ruff'."
;; ;;   :command ("ruff"
;; ;;             "--format=text"
;; ;;             (eval (when buffer-file-name
;; ;;                     (concat "--stdin-filename=" buffer-file-name)))
;; ;;             "-")
;; ;;   :standard-input t
;; ;;   :error-filter (lambda (errors)
;; ;;                   (let ((errors (flycheck-sanitize-errors errors)))
;; ;;                     (seq-map #'flycheck-flake8-fix-error-level errors)))
;; ;;   :error-patterns
;; ;;   ((warning line-start
;; ;;             (file-name) ":" line ":" (optional column ":") " "
;; ;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;; ;;             (message (one-or-more not-newline))
;; ;;             line-end))
;; ;;   :modes python-mode)

;; ;; ;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
;; ;; ;; This is an MVP example:
;; ;; (setq python-mode-hook
;; ;;       (list (defun my-python-hook ()
;; ;;               (unless (bound-and-true-p org-src-mode)
;; ;;                 (when (buffer-file-name)
;; ;;                   (setq-local flycheck-checkers '(python-ruff))
;; ;;                   (flycheck-mode))))))
