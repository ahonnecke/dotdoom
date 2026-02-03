;;; config-agent-shell.el --- Agent Shell configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Native comint-based Claude Code integration via ACP protocol.
;; Replaces vterm-based claude-code.el to avoid hook/permission issues.
;; See ~/.doom.d/CLAUDE-HOOKS.md for background.

;;; Code:

(use-package! shell-maker
  :defer t)

(use-package! acp
  :defer t)

(use-package! agent-shell
  :after (shell-maker acp)
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code)
  :config
  ;; Use login-based auth (same as regular claude CLI)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; Inherit environment (for AWS profiles, etc) but EXCLUDE ANTHROPIC_API_KEY
  ;; to ensure we use subscription login, not pay-per-token API billing
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         :inherit-env t
         "ANTHROPIC_API_KEY" ""))

  ;; Set Claude Code as default agent
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))

  ;; Buffer naming for multiple sessions
  (setq agent-shell-buffer-name-function
        (lambda (config)
          (format "*agent:%s*"
                  (or (projectile-project-name)
                      "global")))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Slash Commands for Agent Shell
;;; ════════════════════════════════════════════════════════════════════════════

(defun agent-shell--get-buffer ()
  "Get the current agent-shell buffer or the most recent one.
Looks for buffers with Claude Code prompt or *agent:* naming."
  (or (and (string-match-p "Claude Code\\|\\*agent:" (buffer-name))
           (current-buffer))
      (seq-find (lambda (b)
                  (string-match-p "Claude Code\\|\\*agent:" (buffer-name b)))
                (buffer-list))))

(defun agent-shell-send-command (cmd)
  "Send CMD to the agent-shell buffer."
  (interactive "sCommand: ")
  (if-let ((buf (agent-shell--get-buffer)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert cmd)
        (comint-send-input))
    (user-error "No agent-shell buffer found. Start one with M-x agent-shell-anthropic-start-claude-code")))

(defun agent-shell-send-slash (cmd &optional arg)
  "Send slash command CMD with optional ARG to agent-shell."
  (let ((full-cmd (if arg (format "/%s %s" cmd arg) (format "/%s" cmd))))
    (agent-shell-send-command full-cmd)))

;; Individual command functions
(defun agent-shell-cmd-clear ()
  "Clear conversation history."
  (interactive)
  (agent-shell-send-slash "clear"))

(defun agent-shell-cmd-compact (instructions)
  "Compact conversation with optional INSTRUCTIONS."
  (interactive "sCompact instructions (optional): ")
  (agent-shell-send-slash "compact" (unless (string-empty-p instructions) instructions)))

(defun agent-shell-cmd-config ()
  "Open Claude settings."
  (interactive)
  (agent-shell-send-slash "config"))

(defun agent-shell-cmd-context ()
  "Visualize current context usage."
  (interactive)
  (agent-shell-send-slash "context"))

(defun agent-shell-cmd-cost ()
  "Show token usage statistics."
  (interactive)
  (agent-shell-send-slash "cost"))

(defun agent-shell-cmd-doctor ()
  "Check Claude Code installation health."
  (interactive)
  (agent-shell-send-slash "doctor"))

(defun agent-shell-cmd-exit ()
  "Exit the Claude session."
  (interactive)
  (agent-shell-send-slash "exit"))

(defun agent-shell-cmd-export (filename)
  "Export conversation to FILENAME or clipboard."
  (interactive "sExport filename (blank for clipboard): ")
  (agent-shell-send-slash "export" (unless (string-empty-p filename) filename)))

(defun agent-shell-cmd-help ()
  "Show help."
  (interactive)
  (agent-shell-send-slash "help"))

(defun agent-shell-cmd-init ()
  "Initialize project with CLAUDE.md."
  (interactive)
  (agent-shell-send-slash "init"))

(defun agent-shell-cmd-mcp ()
  "Manage MCP server connections."
  (interactive)
  (agent-shell-send-slash "mcp"))

(defun agent-shell-cmd-memory ()
  "Edit CLAUDE.md memory files."
  (interactive)
  (agent-shell-send-slash "memory"))

(defun agent-shell-cmd-model ()
  "Select or change AI model."
  (interactive)
  (agent-shell-send-slash "model"))

(defun agent-shell-cmd-permissions ()
  "View or update permissions."
  (interactive)
  (agent-shell-send-slash "permissions"))

(defun agent-shell-cmd-plan ()
  "Enter plan mode."
  (interactive)
  (agent-shell-send-slash "plan"))

(defun agent-shell-cmd-rename (name)
  "Rename current session to NAME."
  (interactive "sNew session name: ")
  (agent-shell-send-slash "rename" name))

(defun agent-shell-cmd-resume ()
  "Resume a conversation."
  (interactive)
  (agent-shell-send-slash "resume"))

(defun agent-shell-cmd-rewind ()
  "Rewind conversation and/or code."
  (interactive)
  (agent-shell-send-slash "rewind"))

(defun agent-shell-cmd-stats ()
  "Visualize daily usage and session history."
  (interactive)
  (agent-shell-send-slash "stats"))

(defun agent-shell-cmd-status ()
  "Show version, model, account status."
  (interactive)
  (agent-shell-send-slash "status"))

(defun agent-shell-cmd-copy ()
  "Copy last assistant response to clipboard."
  (interactive)
  (agent-shell-send-slash "copy"))

(defun agent-shell-cmd-tasks ()
  "List and manage background tasks."
  (interactive)
  (agent-shell-send-slash "tasks"))

(defun agent-shell-cmd-theme ()
  "Change color theme."
  (interactive)
  (agent-shell-send-slash "theme"))

(defun agent-shell-cmd-todos ()
  "List current TODO items."
  (interactive)
  (agent-shell-send-slash "todos"))

(defun agent-shell-cmd-usage ()
  "Show plan usage limits."
  (interactive)
  (agent-shell-send-slash "usage"))

(defun agent-shell-cmd-vim ()
  "Toggle vim editing mode."
  (interactive)
  (agent-shell-send-slash "vim"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix agent-shell-transient ()
  "Claude Code commands via agent-shell."
  ["Session"
   ("c" "Clear history" agent-shell-cmd-clear)
   ("C" "Compact" agent-shell-cmd-compact)
   ("r" "Resume session" agent-shell-cmd-resume)
   ("R" "Rename session" agent-shell-cmd-rename)
   ("x" "Exit" agent-shell-cmd-exit)]

  ["Context & Cost"
   ("$" "Cost/tokens" agent-shell-cmd-cost)
   ("%" "Context usage" agent-shell-cmd-context)
   ("s" "Stats" agent-shell-cmd-stats)
   ("u" "Usage limits" agent-shell-cmd-usage)]

  ["Settings"
   ("m" "Model" agent-shell-cmd-model)
   ("p" "Permissions" agent-shell-cmd-permissions)
   ("g" "Config" agent-shell-cmd-config)
   ("t" "Theme" agent-shell-cmd-theme)
   ("S" "Status" agent-shell-cmd-status)]

  ["Project"
   ("i" "Init CLAUDE.md" agent-shell-cmd-init)
   ("M" "Memory (CLAUDE.md)" agent-shell-cmd-memory)
   ("P" "Plan mode" agent-shell-cmd-plan)
   ("T" "Tasks" agent-shell-cmd-tasks)
   ("d" "Todos" agent-shell-cmd-todos)]

  ["Tools"
   ("e" "Export" agent-shell-cmd-export)
   ("y" "Copy last response" agent-shell-cmd-copy)
   ("w" "Rewind" agent-shell-cmd-rewind)
   ("D" "Doctor" agent-shell-cmd-doctor)
   ("?" "Help" agent-shell-cmd-help)
   ("n" "MCP servers" agent-shell-cmd-mcp)
   ("v" "Vim mode" agent-shell-cmd-vim)]

  ["Launch"
   ("a" "Start Claude" agent-shell-anthropic-start-claude-code)
   ("A" "New session" (lambda () (interactive)
                        (let ((current-prefix-arg '(4)))
                          (call-interactively #'agent-shell-anthropic-start-claude-code))))])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Main transient menu
(map! :leader
      :prefix ("c" . "claude")
      :desc "Agent Shell menu" "a" #'agent-shell-transient
      :desc "Start Claude" "A" #'agent-shell-anthropic-start-claude-code)

;; Quick access without leader
(map! :n "M-C" #'agent-shell-transient)

;; Mode-specific bindings in agent-shell buffers
(add-hook 'agent-shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c /") #'agent-shell-transient)
            (local-set-key (kbd "C-c c") #'agent-shell-cmd-clear)
            (local-set-key (kbd "C-c C") #'agent-shell-cmd-compact)
            (local-set-key (kbd "C-c m") #'agent-shell-cmd-model)
            (local-set-key (kbd "C-c $") #'agent-shell-cmd-cost)))

;; Auto-send initial greeting after agent starts
;; Claude needs a kick to display its welcome - send "hi" after startup
(defun agent-shell--send-greeting ()
  "Send greeting to wake up Claude agent."
  (run-at-time 3 nil
               (lambda ()
                 (when-let ((buf (seq-find
                                  (lambda (b)
                                    (string-match-p "\\*agent:\\|Claude Code" (buffer-name b)))
                                  (buffer-list))))
                   (with-current-buffer buf
                     (goto-char (point-max))
                     (insert "hi")
                     (ignore-errors (shell-maker-submit)))))))

(advice-add 'agent-shell-anthropic-start-claude-code :after
            (lambda (&rest _) (agent-shell--send-greeting)))

(provide 'config-agent-shell)
;;; config-agent-shell.el ends here
