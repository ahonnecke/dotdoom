;;; ~/.doom.d/config-slack.el -*- lexical-binding: t; -*-

;; Slack client for Emacs
;; https://github.com/emacs-slack/emacs-slack
;;
;; Setup:
;;   1. Run ~/bin/slack-token-refresh.sh to extract token/cookie from browser
;;   2. Token stored in ~/.authinfo: machine TEAM.slack.com login USER@email.com password xoxc-...
;;   3. Cookie stored in ~/.authinfo: machine TEAM.slack.com login USER@email.com^cookie password xoxd-...
;;
;; Keybindings (C-c K prefix - K for slacK):
;;   C-c K K - Open Slack (select channel)
;;   C-c K s - Start Slack connection
;;   C-c K c - Switch channel
;;   C-c K d - Direct message
;;   C-c K t - Show thread
;;   C-c K u - Unread channels
;;   C-c K r - Reply in thread
;;   C-c K e - Edit message
;;   C-c K R - Add reaction
;;   C-c K ? - Transient menu

(use-package! slack
  :commands (slack-start slack-channel-select slack-im-select)
  :init
  ;; Defer loading until explicitly called
  (setq slack-prefer-current-team t)

  :config
  ;; Performance: don't fetch all channels on large teams
  (setq slack-quick-update t)

  ;; Buffer names
  (setq slack-buffer-emojify t
        slack-buffer-function #'switch-to-buffer)

  ;; Modeline: show unread count
  (setq slack-modeline-count-only-subscribed-channel t)

  ;; Alert integration (uses your existing alert setup)
  (setq slack-alert-icon nil)  ; Use system default

  ;; Register team(s) - tokens from auth-source
  ;; Add your team(s) here after running slack-token-refresh.sh
  (defun slack--get-auth (host user)
    "Get password from auth-source for HOST and USER."
    (let ((found (car (auth-source-search :host host :user user :max 1))))
      (when found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret)
              (funcall secret)
            secret)))))

  ;; Example team registration - customize for your workspace
  ;; Uncomment and modify after setting up auth:
  ;;
  ;; (slack-register-team
  ;;  :name "crewcapable"
  ;;  :default t
  ;;  :token (slack--get-auth "crewcapable.slack.com" "ashton@crewcapable.ai")
  ;;  :cookie (slack--get-auth "crewcapable.slack.com" "ashton@crewcapable.ai^cookie")
  ;;  :subscribed-channels '(general engineering)
  ;;  :full-and-display-names t)

  ;; Message formatting
  (setq slack-message-notification-title-format-function
        (lambda (team room threadp)
          (concat (if threadp "Thread: " "")
                  (slack-room-name room team))))

  ;; Don't ask for confirmation on reactions
  (setq slack-completing-read-function #'completing-read))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix slack-transient ()
  "Slack commands."
  ["Slack"
   ["Connect"
    ("s" "Start Slack" slack-start)
    ("q" "Quit Slack" slack-ws-close)]
   ["Navigate"
    ("c" "Channel" slack-channel-select)
    ("d" "Direct Message" slack-im-select)
    ("u" "Unread" slack-select-unread-rooms)
    ("t" "Thread" slack-thread-show-or-create)]
   ["Message"
    ("r" "Reply (thread)" slack-message-reply-to)
    ("e" "Edit" slack-message-edit)
    ("D" "Delete" slack-message-delete)
    ("R" "Reaction" slack-message-add-reaction)]
   ["Misc"
    ("/" "Search" slack-search-from-messages)
    ("@" "Mentions" slack-all-mentions)
    ("S" "Stars" slack-stars-list)
    ("f" "File upload" slack-file-upload)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Interactive Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun slack-open ()
  "Open Slack - start if needed, then select channel."
  (interactive)
  (unless (and (boundp 'slack-teams) slack-teams)
    (slack-start))
  (slack-channel-select))

(defun slack-check-unread ()
  "Check for unread Slack messages (for cron/timer)."
  (interactive)
  (when (and (boundp 'slack-teams) slack-teams)
    (let ((unread-count 0))
      (dolist (team slack-teams)
        (dolist (room (slack-team-rooms team))
          (when (slack-room-has-unread-p room team)
            (cl-incf unread-count))))
      (when (> unread-count 0)
        (message "Slack: %d unread" unread-count))
      unread-count)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Token Refresh Helper
;;; ════════════════════════════════════════════════════════════════════════════

(defun slack-refresh-token-help ()
  "Show instructions for refreshing Slack token."
  (interactive)
  (with-current-buffer (get-buffer-create "*Slack Token Help*")
    (erase-buffer)
    (insert "SLACK TOKEN REFRESH\n")
    (insert "===================\n\n")
    (insert "Option 1: Run the helper script\n")
    (insert "  $ ~/bin/slack-token-refresh.sh\n\n")
    (insert "Option 2: Manual extraction\n")
    (insert "  1. Open Slack in browser (not desktop app)\n")
    (insert "  2. Open DevTools (F12) → Application → Local Storage\n")
    (insert "  3. Find 'localConfig_v2' → copy 'token' value (xoxc-...)\n")
    (insert "  4. Go to Cookies → copy 'd' cookie value (xoxd-...)\n")
    (insert "  5. Add to ~/.authinfo:\n")
    (insert "     machine TEAM.slack.com login YOU@email.com password xoxc-TOKEN\n")
    (insert "     machine TEAM.slack.com login YOU@email.com^cookie password xoxd-COOKIE\n\n")
    (insert "Option 3: Use slack-refresh-token command\n")
    (insert "  M-x slack-refresh-token (built-in guided flow)\n")
    (pop-to-buffer (current-buffer))
    (special-mode)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Org Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun slack-copy-link-at-point ()
  "Copy a link to the current Slack message for org/notes."
  (interactive)
  (when-let* ((buf (current-buffer))
              (team (slack-buffer-team buf))
              (room (slack-buffer-room buf))
              (ts (slack-buffer-message-ts buf)))
    (let ((link (format "https://%s.slack.com/archives/%s/p%s"
                        (slack-team-name team)
                        (slack-room-id room)
                        (replace-regexp-in-string "\\." "" ts))))
      (kill-new link)
      (message "Copied: %s" link))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings (C-c K prefix)
;;; ════════════════════════════════════════════════════════════════════════════

(define-key ashton-mode-map (kbd "C-c K K") #'slack-open)
(define-key ashton-mode-map (kbd "C-c K s") #'slack-start)
(define-key ashton-mode-map (kbd "C-c K c") #'slack-channel-select)
(define-key ashton-mode-map (kbd "C-c K d") #'slack-im-select)
(define-key ashton-mode-map (kbd "C-c K t") #'slack-thread-show-or-create)
(define-key ashton-mode-map (kbd "C-c K u") #'slack-select-unread-rooms)
(define-key ashton-mode-map (kbd "C-c K r") #'slack-message-reply-to)
(define-key ashton-mode-map (kbd "C-c K e") #'slack-message-edit)
(define-key ashton-mode-map (kbd "C-c K R") #'slack-message-add-reaction)
(define-key ashton-mode-map (kbd "C-c K /") #'slack-search-from-messages)
(define-key ashton-mode-map (kbd "C-c K @") #'slack-all-mentions)
(define-key ashton-mode-map (kbd "C-c K l") #'slack-copy-link-at-point)
(define-key ashton-mode-map (kbd "C-c K h") #'slack-refresh-token-help)
(define-key ashton-mode-map (kbd "C-c K ?") #'slack-transient)

(provide 'config-slack)
;;; config-slack.el ends here
