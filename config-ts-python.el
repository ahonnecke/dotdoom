;;; ../src/home/.doom.d/config-python.el -*- lexical-binding: t; -*-

;; (defun comment-auto-fill-only-comments ()
;;   (auto-fill-mode 1)
;;   (set (make-local-variable 'fill-nobreak-predicate)
;;        (lambda ()
;;          (not (eq (get-text-property (point) 'face)
;;                   'font-lock-comment-face)))))

;;; Perform isorting py-isort-buffer on save or something
(with-eval-after-load "python"
  (define-key python-ts-mode-map  (kbd "M-/") '+company/complete)
  (global-set-key  (kbd "C->") 'python-indent-shift-right)
  (global-set-key  (kbd "C-<") 'python-indent-shift-left)
  (add-hook 'before-save-hook 'py-isort-before-save)

  (define-key python-ts-mode-map  (kbd "C-c m b") 'python-black-buffer)
  (define-key python-ts-mode-map  (kbd "C-c m r") 'python-black-region)
  (define-key python-ts-mode-map  (kbd "C-c m s") 'python-black-statement)
  (define-key python-ts-mode-map (kbd "C-;") 'comment-or-uncomment-region)


  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))

  ;; (add-hook 'before-save-hook 'py-isort-before-save)
  ;;(add-hook 'python-ts-mode-hook '+format|enable-on-save)
  (setq-local lsp-pylsp-plugins-flake8-max-line-length 88)

  ;; https://www.emacswiki.org/emacs/AutoFillMode
  ;; this does not seem to work, it breaks the formatting
  ;;(add-hook 'python-ts-mode-hook 'comment-auto-fill-only-comments)
  )

;; (add-hook 'python-ts-mode-hook (lambda () (
;;                                         (setq-local comment-auto-fill-only-comments t)
;;                                         (auto-fill-mode nil)
;;                                         )))

(add-hook 'python-ts-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'python-ts-mode-hook (lambda () (python-black-on-save-mode t)))
;; (add-hook 'python-ts-mode-hook (lambda () (py-isort-before-save t)))

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
