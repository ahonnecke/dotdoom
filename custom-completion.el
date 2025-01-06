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
        completion-ignore-case 'keep-prefix)

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
          (company-dabbrev-other-buffers 'all)     ; search in all buffers
          (company-dabbrev-code-other-buffers 'all) ; search in all buffers
          (company-dabbrev-code-modes t)           ; search in all modes
          (company-dabbrev-code-everywhere t)      ; search everywhere, not just code
          (company-dabbrev-minimum-length 2)       ; minimum length for completion
          (company-dabbrev-ignore-case t)          ; ignore case when matching
          (company-dabbrev-downcase nil))          ; keep original case
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
  (global-set-key (kbd "C-c c b") #'my/company-complete-buffer)
  (global-set-key (kbd "C-c c p") #'my/company-complete-project)
  (global-set-key (kbd "C-c c l") #'my/company-complete-lsp)

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
