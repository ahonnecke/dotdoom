;;; ../src/home/.doom.d/config-python.el -*- lexical-binding: t; -*-

;; Set Python line length configurations globally
(setq fill-column 88)

;; Configure LSP-mode python settings
(setq lsp-pylsp-configuration-sources ["pycodestyle"])
(setq lsp-pylsp-plugins-pycodestyle-enabled t)
(setq lsp-pylsp-plugins-pycodestyle-max-line-length 88)
(setq lsp-pylsp-plugins-flake8-max-line-length 88)
(setq ruff-format-line-length 88)

;;; Perform isorting py-isort-buffer on save or something
(with-eval-after-load "python"
  (define-key python-ts-mode-map  (kbd "M-/") '+company/complete)
  (global-set-key  (kbd "C->") 'python-indent-shift-right)
  (global-set-key  (kbd "C-<") 'python-indent-shift-left)

  (define-key python-ts-mode-map  (kbd "C-c m b") 'ruff-format-buffer)
  (define-key python-ts-mode-map  (kbd "C-c m r") 'ruff-format-region)
  (define-key python-ts-mode-map (kbd "C-;") 'comment-or-uncomment-region)
  (define-key python-ts-mode-map (kbd "C-c t") 'python-pytest-dispatch)

  ;; (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  ;; (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))

  ;; (add-hook 'before-save-hook 'py-isort-before-save)
  ;;(add-hook 'python-ts-mode-hook '+format|enable-on-save)

  ;; https://www.emacswiki.org/emacs/AutoFillMode
  ;; this does not seem to work, it breaks the formatting
  ;;(add-hook 'python-ts-mode-hook 'comment-auto-fill-only-comments)
  )

;; (add-hook 'python-ts-mode-hook (lambda () (
;;                                         (setq-local comment-auto-fill-only-comments t)
;;                                         (auto-fill-mode nil)
;;                                         )))

(add-hook 'python-ts-mode-hook (lambda () (auto-fill-mode -1)))
;;(add-hook 'python-ts-mode-hook (lambda () (ruff-format-on-save-mode t)))
;;(add-hook 'python-ts-mode-hook (lambda () (combobulate-mode t)))

;; (add-hook 'python-ts-mode-hook (lambda () (
;;                                         ;; (flycheck-select-checker 'python-ruff)
;;                                         ;; (setq lsp-diagnostics-provider :auto)
;;                                         (setq flycheck-checkers (remove 'lsp flycheck-checkers))
;;                                         )))

;; (add-hook 'python-ts-mode-hook
;;           (lambda ()
;;             (when-let ((r (locate-dominating-file default-directory ".pyroot")))
;;               (setq python-pytest-executable
;;                     (concat "PYTHONPATH=" r " " "pytest")))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (when-let ((r (locate-dominating-file default-directory "Pipenv")))
              (setq python-pytest-executable
                    (concat "PYTHONPATH=" r " " "pipenv run pytest")))))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(defun insert-python-shebang ()
  "Insert a Python shebang line if the buffer is empty."
  (when (and (string-match "\\.py\\'" (buffer-file-name))
             (= (point-min) (point-max)))
    (insert "#!/usr/bin/env python3\n")
    (save-buffer)))

(add-hook 'find-file-hook 'insert-python-shebang)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
