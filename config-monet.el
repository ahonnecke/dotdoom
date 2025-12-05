;;; ~/.doom.d/config-monet.el -*- lexical-binding: t; -*-

;; Monet: Claude Code IDE Integration
;; https://github.com/stevemolitor/monet
;;
;; Bridges Emacs and Claude Code via WebSocket for:
;; - Selection sharing (current selection auto-shared with Claude)
;; - Diagnostics (Flymake/Flycheck errors sent to Claude)
;; - Diff viewing (side-by-side diffs before applying changes)
;;
;; Same author as claude-code.el - they complement each other.

(use-package! monet
  :after claude-code
  :config
  ;; Auto-enable in supported modes
  (add-hook 'prog-mode-hook #'monet-mode)
  (add-hook 'text-mode-hook #'monet-mode)

  ;; Send diagnostics from Flycheck (our primary linter)
  (setq monet-diagnostics-backend 'flycheck)

  ;; Keybindings under C-c m (monet) prefix
  (define-key ashton-mode-map (kbd "C-c m m") #'monet-mode)
  (define-key ashton-mode-map (kbd "C-c m s") #'monet-send-selection)
  (define-key ashton-mode-map (kbd "C-c m d") #'monet-send-diagnostics)
  (define-key ashton-mode-map (kbd "C-c m r") #'monet-reconnect))

;; NOTE: To enable IDE integration, launch Claude with:
;;   ENABLE_IDE_INTEGRATION=t claude
;; Or set in your shell profile:
;;   export ENABLE_IDE_INTEGRATION=t

(provide 'config-monet)
;;; config-monet.el ends here
