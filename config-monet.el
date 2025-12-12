;;; ~/.doom.d/config-monet.el -*- lexical-binding: t; -*-

;; Monet: Claude Code IDE Integration
;; https://github.com/stevemolitor/monet
;;
;; Bridges Emacs and Claude Code via WebSocket (MCP protocol) for:
;; - Selection sharing (current selection auto-shared with Claude)
;; - Diagnostics (Flymake/Flycheck errors sent to Claude)
;; - Diff viewing (side-by-side diffs before applying changes)
;; - File browsing (Claude can open/read project files)
;;
;; REQUIRED: export ENABLE_IDE_INTEGRATION=t in ~/.bashrc
;;
;; Keybindings (C-c m prefix):
;;   C-c m m - Toggle monet-mode
;;   C-c m s - Start server (manual)
;;   C-c m q - Stop server
;;   C-c m l - List active sessions
;;   C-c m S - Send selection to Claude
;;   C-c m d - Send diagnostics to Claude
;;   C-c m f - Fix diagnostics (send + ask Claude to fix)
;;   C-c m r - Reconnect

(use-package! monet
  :after claude-code
  :config
  ;; Auto-enable in code buffers
  (add-hook 'prog-mode-hook #'monet-mode)

  ;; Send diagnostics from Flycheck (our primary linter)
  (setq monet-diagnostics-backend 'flycheck)

  ;; Auto-start server when monet-mode enables
  (setq monet-auto-start-server t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Enhanced Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun monet-fix-diagnostics ()
  "Send current diagnostics to Claude and ask to fix them."
  (interactive)
  (if (fboundp 'monet-send-diagnostics)
      (progn
        (monet-send-diagnostics)
        ;; Give monet a moment to send, then ask Claude to fix
        (run-at-time 0.5 nil
                     (lambda ()
                       (when (fboundp 'claude-code-send-command)
                         (claude-code-send-command
                          "Fix the diagnostics/errors I just shared")))))
    (message "monet-send-diagnostics not available")))

(defun monet--select-defun-if-needed ()
  "If no region, select the function at point."
  (unless (use-region-p)
    (when-let* ((bounds (bounds-of-thing-at-point 'defun)))
      (goto-char (car bounds))
      (push-mark (cdr bounds) t t)
      t)))

(defun monet-explain-selection ()
  "Send selection to Claude via monet and ask for explanation.
Uses region if active, otherwise current function."
  (interactive)
  (let ((selected-defun (monet--select-defun-if-needed)))
    (if (or (use-region-p) selected-defun)
        (progn
          (when (fboundp 'monet-send-selection)
            (monet-send-selection))
          (run-at-time 0.5 nil
                       (lambda ()
                         (when (fboundp 'claude-code-send-command)
                           (claude-code-send-command
                            "Explain the code I just shared")))))
      (message "No region or function at point"))))

(defun monet-refactor-selection ()
  "Send selection to Claude via monet and ask for refactoring suggestions.
Uses region if active, otherwise current function."
  (interactive)
  (let ((selected-defun (monet--select-defun-if-needed)))
    (if (or (use-region-p) selected-defun)
        (progn
          (when (fboundp 'monet-send-selection)
            (monet-send-selection))
          (run-at-time 0.5 nil
                       (lambda ()
                         (when (fboundp 'claude-code-send-command)
                           (claude-code-send-command
                            "Suggest refactoring improvements for the code I just shared")))))
      (message "No region or function at point"))))

(defun monet-status ()
  "Show monet connection status."
  (interactive)
  (if (and (boundp 'monet-mode) monet-mode)
      (message "Monet: enabled%s"
               (if (and (fboundp 'monet-server-running-p)
                        (monet-server-running-p))
                   " (server running)"
                 " (server not running - start with C-c m s)"))
    (message "Monet: disabled (enable with C-c m m)")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings (C-c m prefix)
;;; ════════════════════════════════════════════════════════════════════════════

(with-eval-after-load 'monet
  ;; Core
  (define-key ashton-mode-map (kbd "C-c m m") #'monet-mode)
  (define-key ashton-mode-map (kbd "C-c m s") #'monet-start-server)
  (define-key ashton-mode-map (kbd "C-c m q") #'monet-stop-server)
  (define-key ashton-mode-map (kbd "C-c m l") #'monet-list-sessions)
  (define-key ashton-mode-map (kbd "C-c m r") #'monet-reconnect)
  (define-key ashton-mode-map (kbd "C-c m ?") #'monet-status)

  ;; Send to Claude
  (define-key ashton-mode-map (kbd "C-c m S") #'monet-send-selection)
  (define-key ashton-mode-map (kbd "C-c m d") #'monet-send-diagnostics)

  ;; Enhanced (send + act)
  (define-key ashton-mode-map (kbd "C-c m f") #'monet-fix-diagnostics)
  (define-key ashton-mode-map (kbd "C-c m e") #'monet-explain-selection)
  (define-key ashton-mode-map (kbd "C-c m R") #'monet-refactor-selection))

(provide 'config-monet)
;;; config-monet.el ends here
