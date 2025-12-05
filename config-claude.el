;;; ~/.doom.d/config-claude.el -*- lexical-binding: t; -*-

;; Claude Code CLI integration via claude-code.el
;; https://github.com/stevemolitor/claude-code.el
;;
;; Keybindings (C-c c prefix):
;;   C-c c c - Start Claude in current project
;;   C-c c s - Send command/prompt to Claude
;;   C-c c r - Send region to Claude
;;   C-c c b - Send buffer to Claude
;;   C-c c t - Toggle Claude buffer visibility
;;   C-c c m - Transient menu with all commands
;;   C-c c y - Answer "yes" to Claude prompt
;;   C-c c n - Answer "no" to Claude prompt
;;   C-c c k - Kill Claude instance

(use-package! claude-code
  :after vterm
  :config
  ;; Use vterm as the terminal backend (already have it via Doom)
  (setq claude-code-terminal-backend 'vterm)

  ;; Display Claude below the current window (same column)
  ;; This keeps branch work in a single column: magit on top, claude below
  (defun claude-code-display-buffer-below (buffer)
    "Display Claude BUFFER below current window, staying in same column."
    (let* ((current-win (selected-window))
           (new-window (split-window-below)))
      (set-window-buffer new-window buffer)
      (select-window new-window)
      new-window))

  (setq claude-code-display-window-fn #'claude-code-display-buffer-below)

  ;; Bind to C-c c prefix via ashton-mode-map for consistency
  (define-key ashton-mode-map (kbd "C-c c c") #'claude-code)
  (define-key ashton-mode-map (kbd "C-c c s") #'claude-code-send-command)
  (define-key ashton-mode-map (kbd "C-c c r") #'claude-code-send-region)
  (define-key ashton-mode-map (kbd "C-c c b") #'claude-code-send-buffer)
  (define-key ashton-mode-map (kbd "C-c c t") #'claude-code-toggle-buffer)
  (define-key ashton-mode-map (kbd "C-c c m") #'claude-code-transient-menu)
  (define-key ashton-mode-map (kbd "C-c c y") #'claude-code-yes)
  (define-key ashton-mode-map (kbd "C-c c n") #'claude-code-no)
  (define-key ashton-mode-map (kbd "C-c c k") #'claude-code-kill)
  (define-key ashton-mode-map (kbd "C-c c z") #'claude-code-toggle-read-only-mode)
  (define-key ashton-mode-map (kbd "C-c c M") #'claude-code-cycle-mode))

(provide 'config-claude)
;;; config-claude.el ends here
