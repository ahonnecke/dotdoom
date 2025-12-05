;;; config-aider.el -*- lexical-binding: t; -*-

;; Aidermacs: Modern Aider integration for Emacs
;; https://github.com/MatthewZMD/aidermacs
;;
;; Upgraded from aider.el to aidermacs (2025)
;; Features:
;; - Architect mode: two-model approach (reasoning + editing)
;; - Project-aware analysis
;; - Comprehensive transient menu

;; Keep aider CLI in PATH
(add-to-list 'exec-path "~/.venvs/aider/bin")
(setenv "PATH" (concat "~/.venvs/aider/bin:" (getenv "PATH")))

(use-package! aidermacs
  :config
  ;; Use Claude as the main model (you have claude-code anyway)
  ;; Or fall back to GPT-4o if you prefer
  (setq aidermacs-default-model "claude-3-5-sonnet-20241022")

  ;; Alternative: Use architect mode with two models
  ;; (setq aidermacs-use-architect-mode t)
  ;; (setq aidermacs-architect-model "claude-3-5-sonnet-20241022")
  ;; (setq aidermacs-editor-model "claude-3-5-sonnet-20241022")

  ;; C-c a opens transient menu - all commands available from there
  ;; Don't add sub-bindings (C-c a a, etc.) - transient handles it
  (define-key ashton-mode-map (kbd "C-c a") #'aidermacs-transient-menu))

;;; config-aider.el ends here
