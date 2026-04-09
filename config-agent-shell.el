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
                      "global"))))

  ;; Show token/cost after each turn and context fill in header
  (setq agent-shell-show-usage-at-turn-end t)
  (setq agent-shell-show-context-usage-indicator 'detailed)

  ;; Theme-aware tinted background labels for status/kind
  (setq agent-shell-status-kind-label-function
        #'agent-shell--background-tint-status-kind-label)

  ;; Ask whether to resume existing session on start
  (setq agent-shell-session-strategy 'prompt)

  ;; Less visual noise on the busy spinner
  (setq agent-shell-busy-indicator-frames 'narrow))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Slash Commands for Agent Shell
;;; ════════════════════════════════════════════════════════════════════════════

(defun agent-shell--get-buffer ()
  "Get the current agent-shell buffer or the most recent one.
Looks for buffers named Claude Agent/Code or *agent:* naming."
  (or (and (string-match-p "Claude \\(Agent\\|Code\\)\\|\\*agent:" (buffer-name))
           (current-buffer))
      (seq-find (lambda (b)
                  (string-match-p "Claude \\(Agent\\|Code\\)\\|\\*agent:" (buffer-name b)))
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
   ("U" "Open last URL" agent-shell-grab-url)
   ("w" "Rewind" agent-shell-cmd-rewind)
   ("!" "Doctor" agent-shell-cmd-doctor)
   ("?" "Help" agent-shell-cmd-help)
   ("n" "MCP servers" agent-shell-cmd-mcp)
   ("v" "Vim mode" agent-shell-cmd-vim)
   ("h" "Health check" agent-shell-health)]

  ["Dispatch"
   ("D" "Start dispatch" agent-shell-dispatch-start)
   ("K" "Kill all agents" agent-shell-dispatch-kill-agents)
   ("V" "View agent output" agent-shell-dispatch-view-agent-output)
   ("Q" "Stop dispatch" agent-shell-dispatch-stop)]

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
            (local-set-key (kbd "C-c $") #'agent-shell-cmd-cost)
            ;; Buffer-wide permission response keys (work from anywhere in buffer)
            (local-set-key (kbd "y") #'agent-shell-permission-allow)
            (local-set-key (kbd "n") #'agent-shell-permission-reject)
            (local-set-key (kbd "!") #'agent-shell-permission-always)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Buffer-wide Permission Response
;;; ════════════════════════════════════════════════════════════════════════════

(defun agent-shell--find-permission-button (key-char)
  "Find a permission button in buffer that responds to KEY-CHAR.
Returns the position of the button, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (let* ((keymap (get-text-property (point) 'keymap))
               (binding (when keymap (lookup-key keymap (kbd key-char)))))
          (if (and binding (commandp binding))
              (setq found (point))
            (goto-char (or (next-single-property-change (point) 'keymap)
                           (point-max))))))
      found)))

(defun agent-shell--invoke-permission-key (key-char fallback-char)
  "Invoke permission button for KEY-CHAR, or insert FALLBACK-CHAR if none pending."
  (if-let ((pos (agent-shell--find-permission-button key-char)))
      (save-excursion
        (goto-char pos)
        (let ((keymap (get-text-property (point) 'keymap)))
          (when-let ((cmd (lookup-key keymap (kbd key-char))))
            (call-interactively cmd))))
    ;; No permission button found - insert the character normally
    (insert fallback-char)))

(defun agent-shell-permission-allow ()
  "Allow the pending permission (press y anywhere in buffer)."
  (interactive)
  (agent-shell--invoke-permission-key "y" "y"))

(defun agent-shell-permission-reject ()
  "Reject the pending permission (press n anywhere in buffer)."
  (interactive)
  (agent-shell--invoke-permission-key "n" "n"))

(defun agent-shell-permission-always ()
  "Always allow this permission (press ! anywhere in buffer)."
  (interactive)
  (agent-shell--invoke-permission-key "!" "!"))

;; Auto-send session context after agent starts
;; Runs session-context script and sends output so Claude has full context immediately
(defvar agent-shell--pending-worktree nil
  "Worktree path for the agent being started.")

(defun agent-shell--send-session-context ()
  "Send session context to Claude agent on startup."
  (run-at-time 1.5 nil
               (lambda ()
                 (when-let ((buf (seq-find
                                  (lambda (b)
                                    (string-match-p "\\*agent:\\|Claude Code" (buffer-name b)))
                                  (buffer-list))))
                   (with-current-buffer buf
                     (let* ((worktree (or agent-shell--pending-worktree default-directory))
                            (context (shell-command-to-string
                                      (format "/home/ahonnecke/src/.crewcapableai.shared/bin/session-context %s"
                                              (shell-quote-argument worktree)))))
                       (goto-char (point-max))
                       (insert context)
                       (ignore-errors (shell-maker-submit))
                       (setq agent-shell--pending-worktree nil)))))))

(advice-add 'agent-shell-anthropic-start-claude-code :after
            (lambda (&rest _)
              (setq agent-shell--pending-worktree default-directory)
              (agent-shell--send-session-context)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Health Check — is the agent-shell session alive or stuck?
;;; ════════════════════════════════════════════════════════════════════════════

(defvar-local agent-shell--health-prev-size nil
  "Buffer size at last health check, for detecting stalls.")


(defun agent-shell--get-all-buffers ()
  "Get all agent-shell Claude buffers."
  (cl-remove-if-not
   (lambda (b)
     (string-match-p "Claude \\(Agent\\|Code\\)\\|\\*agent:" (buffer-name b)))
   (buffer-list)))

(defun agent-shell--health-pick-buffer ()
  "Pick the Claude buffer to check.
Uses current buffer if it's a Claude buffer, else prompts."
  (let ((all (agent-shell--get-all-buffers)))
    (cond
     ((member (current-buffer) all) (current-buffer))
     ((= (length all) 1) (car all))
     (all (get-buffer
           (completing-read "Check which Claude? "
                            (mapcar #'buffer-name all) nil t)))
     (t nil))))

(defun agent-shell--health-get-acp-process (buf)
  "Get the ACP process for agent-shell BUF.
Traverses: buffer → agent-shell--state → :client → :process."
  (when-let* ((state (buffer-local-value 'agent-shell--state buf))
              (client (map-elt state :client))
              (proc (map-elt client :process)))
    proc))

(defun agent-shell--health-get-active-requests (buf)
  "Get active ACP requests for agent-shell BUF."
  (when-let ((state (buffer-local-value 'agent-shell--state buf)))
    (map-elt state :active-requests)))

(defun agent-shell--health-get-pending-requests (buf)
  "Get pending ACP requests (awaiting response) for BUF.
Returns list of (id . method) pairs."
  (when-let* ((state (buffer-local-value 'agent-shell--state buf))
              (client (map-elt state :client))
              (pending (map-elt client :pending-requests)))
    (mapcar (lambda (entry)
              ;; entry is (request-id . ((:request . ((jsonrpc ...) (method . X) (id . N) ...)) ...))
              (let* ((id (car entry))
                     (data (cdr entry))
                     (request (alist-get :request data))
                     (method (alist-get 'method request)))
                (cons id (or method "unknown"))))
            pending)))

(defun agent-shell--health-process-tree (pid)
  "Get the full process tree under PID as a string."
  (when pid
    (let ((result (string-trim
                   (shell-command-to-string
                    (format "ps -o pid,%%cpu,etime,args --no-headers --forest -g $(ps -o sid= -p %d) 2>/dev/null" pid)))))
      (unless (string-empty-p result) result))))

(defun agent-shell-health ()
  "Check if the agent-shell Claude process is alive or stuck.
Shows ACP process state, active requests, and buffer activity.
Run twice ~5s apart to get a definitive stuck/working verdict."
  (interactive)
  (let ((buf (agent-shell--health-pick-buffer)))
    (if (not buf)
        (message "No Claude Agent buffer found")
      (agent-shell-health--run buf))))

(defun agent-shell-health--run (buf)
  "Run health check on Claude Agent BUF and display results."
  (let* ((acp-proc (agent-shell--health-get-acp-process buf))
         (alive (and acp-proc (process-live-p acp-proc)))
         (pid (when alive (process-id acp-proc)))
         (size (buffer-size buf))
         (prev-size (buffer-local-value 'agent-shell--health-prev-size buf))
         (growing (and prev-size (> size prev-size)))
         (active-reqs (agent-shell--health-get-active-requests buf))
         (pending-reqs (agent-shell--health-get-pending-requests buf))
         (tree (agent-shell--health-process-tree pid))
         (report-buf (get-buffer-create "*Agent Health*")))
    ;; Store size for next comparison
    (with-current-buffer buf
      (setq agent-shell--health-prev-size size))
    (with-current-buffer report-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== Agent Health === %s\n\n" (format-time-string "%H:%M:%S")))
        (insert (format "Buffer: %s\n" (buffer-name buf)))
        (insert (format "ACP process: %s (PID %s)\n"
                        (cond (alive "ALIVE") (acp-proc "DEAD") (t "NONE"))
                        (or pid "n/a")))
        (insert (format "Buffer size: %d bytes%s\n"
                        size
                        (cond
                         ((null prev-size) " (first check — run again in 5s)")
                         (growing (format " (+%d since last check)" (- size prev-size)))
                         ((= size prev-size) " (UNCHANGED since last check)")
                         (t (format " (%+d since last check)" (- size prev-size))))))
        (insert (format "Active requests: %d\n" (length active-reqs)))
        (insert (format "Pending responses: %d\n" (length pending-reqs)))
        (when pending-reqs
          (insert "\n-- Pending (waiting for response) --\n")
          (dolist (req pending-reqs)
            (insert (format "  id:%s  method:%s\n" (car req) (cdr req)))))
        (when tree
          (insert "\n-- Process Tree --\n")
          (insert tree)
          (insert "\n"))
        (insert "\n-- Verdict --\n")
        (cond
         ((not alive)
          (insert "DEAD. Process exited. Press K to bounce (kill + restart).\n"))
         ((and prev-size (= size prev-size) (null active-reqs) (null pending-reqs))
          (insert "IDLE. Waiting for input — nothing in flight.\n"))
         ((and prev-size (= size prev-size) (or active-reqs pending-reqs))
          ;; Check if claude has any non-MCP children (actual tool work)
          (let* ((claude-pid
                  (when tree
                    (let ((line (seq-find (lambda (l) (string-match-p "\\bclaude$" l))
                                         (split-string tree "\n" t))))
                      (when (and line (string-match "^\\s-*\\([0-9]+\\)" line))
                        (match-string 1 line)))))
                 (claude-children
                  (when claude-pid
                    (string-trim
                     (shell-command-to-string
                      (format "ps --ppid %s -o args= 2>/dev/null" claude-pid)))))
                 (has-tool-child
                  (when (and claude-children (not (string-empty-p claude-children)))
                    (seq-find (lambda (l)
                                (not (or (string-match-p "notmuch_mcp\\|engram\\|token-counter" l)
                                         (string-empty-p (string-trim l)))))
                              (split-string claude-children "\n" t)))))
            (cond
             (has-tool-child
              (insert (format "WAITING. Tool subprocess running: %s\n" has-tool-child)))
             (t
              (insert "UNCHANGED. Request pending, no tool running.\n")
              (insert "Press g to check again — if still unchanged, it's stuck.\n")
              (insert "Press K to bounce.\n")))))
         (growing
          (insert "WORKING. Buffer is growing — output is flowing.\n"))
         ((or active-reqs pending-reqs)
          (insert "BUSY. Requests in flight. Press g to check if buffer grows.\n"))
         (t
          (insert "ALIVE.\n")))
        (insert "\n-- Keys --\n")
        (insert "g: refresh    K: bounce (kill + restart)    k: kill only    q: quit\n")
        (setq agent-shell--health-target-buffer buf)
        (goto-char (point-min))
        (agent-shell-health-mode)))
    (display-buffer report-buf)))

(defvar agent-shell--health-target-buffer nil
  "The Claude Agent buffer that the *Agent Health* report is about.")

(defvar agent-shell-health-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "K") #'agent-shell-health-bounce)
    (define-key map (kbd "g") #'agent-shell-health-refresh)
    (define-key map (kbd "k") #'agent-shell-health-kill)
    map)
  "Keymap for *Agent Health* buffer.")

(define-derived-mode agent-shell-health-mode special-mode "AgentHealth"
  "Mode for the *Agent Health* report buffer.
\\{agent-shell-health-mode-map}")

(defun agent-shell-health-bounce ()
  "Kill the stuck Claude session and restart it.
Preserves the working directory so --continue picks up the session.
Sends 'keep going' automatically after the new session starts."
  (interactive)
  (unless agent-shell--health-target-buffer
    (user-error "No target buffer — run agent-shell-health first"))
  (unless (buffer-live-p agent-shell--health-target-buffer)
    (user-error "Target buffer already killed"))
  (let* ((buf agent-shell--health-target-buffer)
         (dir (buffer-local-value 'default-directory buf))
         (name (buffer-name buf)))
    (when (yes-or-no-p (format "Bounce '%s'? " name))
      ;; Kill the ACP process and buffer
      (when-let ((proc (agent-shell--health-get-acp-process buf)))
        (when (process-live-p proc)
          (delete-process proc)))
      (kill-buffer buf)
      ;; Restart with --continue in the same directory
      (orchard--start-agent-shell dir "keep going")
      (message "Bounced %s — restarting with --continue" (file-name-nondirectory (directory-file-name dir))))))

(defun agent-shell-health-kill ()
  "Kill the stuck Claude session without restarting."
  (interactive)
  (unless agent-shell--health-target-buffer
    (user-error "No target buffer"))
  (unless (buffer-live-p agent-shell--health-target-buffer)
    (user-error "Target buffer already killed"))
  (let* ((buf agent-shell--health-target-buffer)
         (name (buffer-name buf)))
    (when (yes-or-no-p (format "Kill '%s'? " name))
      (when-let ((proc (agent-shell--health-get-acp-process buf)))
        (when (process-live-p proc)
          (delete-process proc)))
      (kill-buffer buf)
      (message "Killed %s" name))))

(defun agent-shell-health-refresh ()
  "Re-run health check on the same buffer."
  (interactive)
  (if (and agent-shell--health-target-buffer
           (buffer-live-p agent-shell--health-target-buffer))
      (agent-shell-health--run agent-shell--health-target-buffer)
    (call-interactively #'agent-shell-health)))

(define-key ashton-mode-map (kbd "C-c c h") #'agent-shell-health)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Grab Last URL — agent-shell version (comint, no vterm unwrap needed)
;;; ════════════════════════════════════════════════════════════════════════════

(defun agent-shell-grab-url ()
  "Find last URL in the current agent-shell buffer, copy to clipboard and open.
Only searches the buffer you're in (or the one associated with your project),
so you won't accidentally open a URL from a different worktree's session.
Searches the raw text (no properties) so markdown-overlays don't hide URLs."
  (interactive)
  (let ((buf (or (and (derived-mode-p 'agent-shell-mode) (current-buffer))
                 (agent-shell--get-buffer))))
    (unless buf (user-error "No agent-shell buffer found"))
    (with-current-buffer buf
      (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
             (url-re "https?://[^ \t\n\r\"<>]+")
             url)
        ;; Search raw text from the end to find the most recent URL
        (with-temp-buffer
          (insert raw)
          (goto-char (point-max))
          (unless (re-search-backward url-re nil t)
            (user-error "No URL found in %s (%d chars)" (buffer-name buf) (length raw)))
          (setq url (match-string 0)))
        ;; Strip trailing punctuation unlikely to be part of URL
        (setq url (replace-regexp-in-string "[.,;:!?)]+$" "" url))
        (kill-new url)
        (when (fboundp 'gui-set-selection)
          (gui-set-selection 'CLIPBOARD url))
        (message "URL → clipboard: %s" url)
        (browse-url url)))))

(define-key ashton-mode-map (kbd "C-c c u") #'agent-shell-grab-url)

;; Bind in agent-shell-mode-map directly (works for all buffers, not just new ones)
(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "C-c u") #'agent-shell-grab-url))

(provide 'config-agent-shell)
;;; config-agent-shell.el ends here
