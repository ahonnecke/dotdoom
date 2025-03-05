;; Enhanced completion configuration

(use-package company
  :config
  ;; Enable company-mode globally
  (global-company-mode)

  ;; Basic company settings from your config-company-dict.el
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-tooltip-minimum-width 88
        company-dabbrev-other-buffers t
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-show-quick-access t
        company-dabbrev-code-everywhere t
        tab-always-indent 'complete
        completion-ignore-case 'keep-prefix
        company-auto-complete nil
        company-auto-complete-chars nil
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-active-map (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "TAB") #'company-complete-selection)
                             (define-key map (kbd "RET") #'company-complete-selection)
                             map))

  ;; Dictionary configuration
  (use-package company-dict
    :config
    (setq company-dict-dir "/home/ahonnecke/src/doomemacs/dict/"))

  ;; Create custom completion functions for different sources
  (defun my/company-complete-buffer ()
    "Complete using symbols from all open buffers."
    (interactive)
    (let ((company-backends '(company-dabbrev-code
                              company-dabbrev
                              company-capf))
          (company-dabbrev-other-buffers 'all)
          (company-dabbrev-code-other-buffers 'all)
          (company-dabbrev-code-modes t)
          (company-dabbrev-code-everywhere t)
          (company-dabbrev-minimum-length 2)
          (company-dabbrev-ignore-case t)
          (company-dabbrev-downcase nil))
      (company-complete)))

  (defun my/company-complete-project ()
    "Complete using symbols from project files."
    (interactive)
    (require 'projectile)
    (let ((company-backends '((company-capf
                               company-files
                               company-dabbrev-code
                               :with company-yasnippet)
                              company-dabbrev))
          (company-dabbrev-code-other-buffers 'all)
          (company-dabbrev-code-everywhere t))
      (company-complete)))

  (defun my/company-complete-lsp ()
    "Complete using LSP suggestions."
    (interactive)
    (let ((company-backends '(company-capf)))
      (company-complete)))

  ;; Define keybindings for different completion sources using C-c c prefix
  (define-key ashton-mode-map (kbd "C-c c b") #'my/company-complete-buffer)
  (define-key ashton-mode-map (kbd "C-c c p") #'my/company-complete-project)
  (define-key ashton-mode-map (kbd "C-c c l") #'my/company-complete-lsp)
  (define-key ashton-mode-map (kbd "M-C-/") #'my/company-complete-project)
  (define-key ashton-mode-map (kbd "C-c c f") #'company-files)

  ;; Configure the default completion backend to include all sources
  (setq company-backends
        '((company-capf
           company-dict
           company-files
           company-dabbrev-code
           company-dabbrev
           company-projectile
           company-keywords
           company-yasnippet))))

;; SQL completion configuration
(with-eval-after-load 'sql
  (require 'company)
  (require 'cl-lib)
  (require 'sql)

  ;; Import your existing SQL completion configuration
  (defvar-local company-sql-comp '("") "completions cache")

  (defun company-sql-clear-cache ()
    (interactive)
    (setq company-sql-comp '("")))

  ;; Add SQL-specific backend when in SQL mode
  (add-hook 'sql-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-sql))))

;; VTerm completion configuration
(with-eval-after-load 'vterm
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf
                             company-files
                             company-keywords
                             company-dabbrev))))))

;; Enable completion in specific modes
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'sql-mode-hook 'company-mode)
(add-hook 'vterm-mode-hook 'company-mode)

;; Make dired buffers available for completion
(add-hook 'dired-mode-hook 'company-mode)

(setq dabbrev-case-replace nil
      dabbrev-case-fold-search nil
      dabbrev-upcase-means-case-search t)
