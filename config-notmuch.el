;;; config-notmuch.el --- Notmuch email configuration -*- lexical-binding: t; -*-

;; Notmuch search/show buffers should always use same window
(add-to-list 'display-buffer-alist
             '("\\*notmuch-.*"
               (display-buffer-same-window)
               (inhibit-same-window . nil)))

(after! notmuch

  ;; Sending mail via msmtp (auto-selects account from From header)
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'sendmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'header)

  ;; Identities (for auto-selecting From on reply)
  (setq notmuch-identities
        '("Ashton Honnecke <ashton@novemberuniform.com>"
          "Ashton Honnecke <ashton@crewcapable.com>"
          "Ashton Honnecke <ahonneck@gmail.com>"))

  ;; Sent mail filing per account
  (setq notmuch-fcc-dirs
        '(("ashton@novemberuniform.com" . "novemberuniform/Sent")
          ("ashton@crewcapable.com" . "crewcapable/[Gmail]/Sent Mail")
          ("ahonneck@gmail.com" . "personal/[Gmail]/Sent Mail")))

  ;; Draft folder
  (setq notmuch-draft-folder "novemberuniform/Drafts")

  ;; Saved searches
  (setq notmuch-saved-searches
        '((:name "Mail"
           :query "(tag:priority and tag:unread) or tag:notice"
           :key "i"
           :sort-order newest-first)
          (:name "Priority"
           :query "tag:priority and tag:unread"
           :key "p"
           :sort-order newest-first)
          (:name "All Inbox"
           :query "tag:inbox and not tag:shitbox and not tag:low"
           :key "a"
           :sort-order newest-first)
          (:name "Jobs"
           :query "tag:job_offer"
           :key "j"
           :sort-order newest-first)
          (:name "Drafts"
           :query "folder:novemberuniform/Drafts"
           :key "D"
           :sort-order newest-first)))

  ;; Show newest first
  (setq notmuch-search-oldest-first nil)

  ;; Color coding by tier tag
  (setq notmuch-search-line-faces
        '(("priority" . (:foreground "#EF2929" :weight bold))
          ("notice" . (:foreground "#3465A4"))
          ("low" . (:foreground "#75507B"))
          ("shitbox" . (:foreground "#888888" :strike-through t))
          ("job_offer" . (:foreground "#4E9A06"))
          ("unread" . (:weight bold))))

  ;; Tag display formatting
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face '(:foreground "#EF2929" :weight bold)))
          ("priority" (propertize tag 'face '(:foreground "#EF2929")))
          ("notice" (propertize tag 'face '(:foreground "#3465A4")))
          ("low" (propertize tag 'face '(:foreground "#75507B")))
          ("shitbox" nil)
          ("inbox" nil)))

  ;; --- Keybindings (emacs-style, no evil) ---

  ;; Search mode (list view)
  (define-key notmuch-search-mode-map (kbd "d") #'my/notmuch-done)
  (define-key notmuch-search-mode-map (kbd "u") #'my/notmuch-unread)
  (define-key notmuch-search-mode-map (kbd "-") #'my/notmuch-demote)
  (define-key notmuch-search-mode-map (kbd "+") #'my/notmuch-promote)
  (define-key notmuch-search-mode-map (kbd "c") #'my/notmuch-appointment)
  (define-key notmuch-search-mode-map (kbd "G") #'my/notmuch-sync)
  (define-key notmuch-search-mode-map (kbd "?") #'my/notmuch-help)

  ;; Show mode (reading a thread)
  (define-key notmuch-show-mode-map (kbd "d") #'my/notmuch-show-done)
  (define-key notmuch-show-mode-map (kbd "u") #'my/notmuch-show-unread)
  (define-key notmuch-show-mode-map (kbd "-") #'my/notmuch-show-demote)
  (define-key notmuch-show-mode-map (kbd "+") #'my/notmuch-show-promote)
  (define-key notmuch-show-mode-map (kbd "c") #'my/notmuch-show-appointment)
  (define-key notmuch-show-mode-map (kbd "?") #'my/notmuch-help)

  ;; Hello mode (landing page)
  (define-key notmuch-hello-mode-map (kbd "?") #'my/notmuch-help)
  (define-key notmuch-hello-mode-map (kbd "G") #'my/notmuch-sync)

  ;; Override q in all notmuch modes to go back to previous layout
  (define-key notmuch-hello-mode-map (kbd "q") #'my/notmuch-quit)
  (define-key notmuch-search-mode-map (kbd "q") #'my/notmuch-quit)
  (define-key notmuch-show-mode-map (kbd "q") #'my/notmuch-quit))

;; Global binding to open notmuch
(global-set-key (kbd "C-c N") #'my/notmuch)

(defvar my/notmuch--prior-config nil
  "Window configuration before entering notmuch.")

(defun my/notmuch ()
  "Open notmuch in a full-frame layout, escaping orchard columns."
  (interactive)
  (setq my/notmuch--prior-config (current-window-configuration))
  ;; Undedicate all windows so we can rearrange freely
  (dolist (win (window-list))
    (set-window-dedicated-p win nil))
  (delete-other-windows)
  (notmuch))

(defun my/notmuch-quit ()
  "Quit notmuch and restore the previous window layout."
  (interactive)
  (if my/notmuch--prior-config
      (progn
        (set-window-configuration my/notmuch--prior-config)
        (setq my/notmuch--prior-config nil))
    (quit-window)))

;; --- Help ---

(defun my/notmuch-help ()
  "Show notmuch keybinding help."
  (interactive)
  (let ((buf (get-buffer-create "*notmuch help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "Notmuch Keybindings\n" 'face 'bold)
         (propertize "═══════════════════\n\n" 'face 'shadow)

         (propertize "Saved Searches (from hello screen):\n" 'face 'bold)
         "  i   Mail (priority unread + notice)\n"
         "  p   Priority only\n"
         "  a   All Inbox (no shitbox/low)\n"
         "  j   Job Offers\n"
         "  D   Drafts\n\n"

         (propertize "Actions (search + show modes):\n" 'face 'bold)
         "  d   Done — archive, mark read, next\n"
         "  u   Mark unread\n"
         "  -   Demote — pick lower tier\n"
         "  +   Promote to priority\n"
         "  c   Create appointment (actionable)\n\n"

         (propertize "Navigation:\n" 'face 'bold)
         "  RET Open thread\n"
         "  n   Next thread\n"
         "  p   Previous thread\n"
         "  q   Quit / back\n"
         "  G   Sync mail (mbsync + notmuch new)\n"
         "  =   Refresh current view\n"
         "  s   Search\n"
         "  r   Reply\n"
         "  R   Reply-all\n"
         "  m   Compose new\n"
         "  ?   This help\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

;; --- Action functions (search mode) ---

(defun my/notmuch-done ()
  "Mark as done: remove unread, archive, next thread."
  (interactive)
  (notmuch-search-tag '("-unread" "-inbox" "+archive"))
  (notmuch-search-next-thread))

(defun my/notmuch-unread ()
  "Mark as unread for later review."
  (interactive)
  (notmuch-search-tag '("+unread")))

(defun my/notmuch-demote ()
  "Demote email to a lower tier."
  (interactive)
  (let ((tier (completing-read "Demote to: " '("notice" "low" "shitbox"))))
    (notmuch-search-tag
     (list "+inbox" (concat "+" tier)
           "-priority" "-notice" "-low" "-shitbox" "-job_offer"
           "-unread"))
    (message "Demoted to %s" tier)
    (notmuch-search-next-thread)))

(defun my/notmuch-promote ()
  "Promote email to priority."
  (interactive)
  (notmuch-search-tag
   '("+priority" "+unread" "-notice" "-low" "-shitbox"))
  (message "Promoted to priority"))

(defun my/notmuch-appointment ()
  "Create calendar event from this email."
  (interactive)
  (let* ((thread-id (notmuch-search-find-thread-id))
         (body (shell-command-to-string
                (format "notmuch show --format=raw %s" thread-id))))
    (with-temp-buffer
      (insert body)
      (shell-command-on-region (point-min) (point-max)
                               "actionable --stdin --yes" nil nil nil t))
    (message "Sent to actionable for calendar detection")))

(defun my/notmuch-sync ()
  "Sync mail: mbsync + notmuch new."
  (interactive)
  (message "Syncing mail...")
  (set-process-sentinel
   (start-process "mbsync" "*mbsync*" "bash" "-c" "mbsync -a && notmuch new")
   (lambda (_proc event)
     (when (string-match-p "finished" event)
       (message "Mail sync complete")
       (when (derived-mode-p 'notmuch-search-mode)
         (notmuch-refresh-this-buffer))))))

;; --- Action functions (show mode) ---

(defun my/notmuch-show-done ()
  "Mark as done from show mode."
  (interactive)
  (notmuch-show-tag '("-unread" "-inbox" "+archive"))
  (notmuch-show-next-thread-show))

(defun my/notmuch-show-unread ()
  "Mark as unread from show mode."
  (interactive)
  (notmuch-show-tag '("+unread")))

(defun my/notmuch-show-demote ()
  "Demote from show mode."
  (interactive)
  (let ((tier (completing-read "Demote to: " '("notice" "low" "shitbox"))))
    (notmuch-show-tag
     (list "+inbox" (concat "+" tier)
           "-priority" "-notice" "-low" "-shitbox" "-job_offer"
           "-unread"))
    (message "Demoted to %s" tier)
    (notmuch-show-next-thread-show)))

(defun my/notmuch-show-promote ()
  "Promote from show mode."
  (interactive)
  (notmuch-show-tag
   '("+priority" "+unread" "-notice" "-low" "-shitbox"))
  (message "Promoted to priority"))

(defun my/notmuch-show-appointment ()
  "Create calendar event from show mode."
  (interactive)
  (notmuch-show-pipe-message nil "actionable --stdin --yes")
  (message "Sent to actionable for calendar detection"))
