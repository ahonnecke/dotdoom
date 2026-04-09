;;; config-notmuch.el --- Notmuch email configuration -*- lexical-binding: t; -*-

;; Tell Doom's popup system to leave notmuch buffers alone
(after! notmuch
  (set-popup-rule! "^\\*notmuch" :ignore t))

;; Notmuch buffers always use same window
(add-to-list 'display-buffer-alist
             '("\\*notmuch"
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

  ;; Address completion from notmuch database (sent folder, not tag)
  (setq notmuch-address-command 'internal
        notmuch-address-use-company t
        notmuch-address-internal-completion
        '(sent "folder:novemberuniform/Sent or folder:crewcapable/[Gmail]/Sent Mail or folder:personal/[Gmail]/Sent Mail"))

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

  ;; Show newest first
  (setq notmuch-search-oldest-first nil)

  ;; Result format
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)")))

  ;; Account prefix is shown via tags instead of per-line shell calls.
  ;; The notmuch post-new hook tags messages with fm/cc/gm based on folder path.

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
          ("fm" (propertize tag 'face '(:foreground "#729FCF")))
          ("cc" (propertize tag 'face '(:foreground "#8AE234")))
          ("gm" (propertize tag 'face '(:foreground "#FCE94F")))
          ("shitbox" nil)
          ("inbox" nil)
          ("archive" nil)
          ("new" nil)))

  ;; Saved searches — powers J (jump) and notmuch-hello
  (setq notmuch-saved-searches
        '((:name "Inbox"     :key "i" :query "tag:inbox and tag:unread and not tag:shitbox and not tag:low and date:2weeks..")
          (:name "Priority"  :key "p" :query "tag:priority and date:1month..")
          (:name "Normal"    :key "o" :query "tag:inbox and tag:unread and not tag:priority and not tag:shitbox and not tag:low and not tag:notice and date:2weeks..")
          (:name "Notices"   :key "n" :query "tag:notice and date:2w..")
          (:name "All inbox" :key "a" :query "tag:inbox and not tag:shitbox and not tag:low and date:1month..")
          (:name "Job offers" :key "j" :query "tag:job_offer")
          (:name "Sent"      :key "t" :query "(folder:novemberuniform/Sent or folder:crewcapable/[Gmail]/Sent Mail or folder:personal/[Gmail]/Sent Mail) and date:1w..")))

  ;; --- Keybindings ---
  ;; J = jump between saved searches (built-in, reads notmuch-saved-searches)
  ;; Direct view shortcuts override p/n prev/next — use C-p/C-n or arrows instead.

  ;; Search mode — direct view access
  (define-key notmuch-search-mode-map (kbd "P") (lambda () (interactive) (my/notmuch-go "tag:priority and date:1month..")))
  (define-key notmuch-search-mode-map (kbd "I") (lambda () (interactive) (my/notmuch-go "tag:inbox and tag:unread and not tag:shitbox and not tag:low and date:2weeks..")))
  (define-key notmuch-search-mode-map (kbd "N") (lambda () (interactive) (my/notmuch-go "tag:notice and date:2w..")))

  ;; Search mode — actions
  (define-key notmuch-search-mode-map (kbd "d") #'my/notmuch-done)
  (define-key notmuch-search-mode-map (kbd "u") #'my/notmuch-unread)
  (define-key notmuch-search-mode-map (kbd "-") #'my/notmuch-demote)
  (define-key notmuch-search-mode-map (kbd "+") #'my/notmuch-promote)
  (define-key notmuch-search-mode-map (kbd "c") #'my/notmuch-appointment)
  (define-key notmuch-search-mode-map (kbd "g") #'my/notmuch-sync)
  (define-key notmuch-search-mode-map (kbd "G") #'my/notmuch-sync)
  (define-key notmuch-search-mode-map (kbd "?") #'my/notmuch-menu)
  (define-key notmuch-search-mode-map (kbd "q") #'my/notmuch-quit)

  ;; Show mode
  (define-key notmuch-show-mode-map (kbd "d") #'my/notmuch-show-done)
  (define-key notmuch-show-mode-map (kbd "u") #'my/notmuch-show-unread)
  (define-key notmuch-show-mode-map (kbd "-") #'my/notmuch-show-demote)
  (define-key notmuch-show-mode-map (kbd "+") #'my/notmuch-show-promote)
  (define-key notmuch-show-mode-map (kbd "c") #'my/notmuch-show-appointment)
  (define-key notmuch-show-mode-map (kbd "g") #'my/notmuch-sync)
  (define-key notmuch-show-mode-map (kbd "G") #'my/notmuch-sync)
  (define-key notmuch-show-mode-map (kbd "?") #'my/notmuch-menu)
  (define-key notmuch-show-mode-map (kbd "q") #'my/notmuch-quit))

;; ════════════════════════════════════════════════════════════════
;; Transient menu — the one entry point for navigation
;; ════════════════════════════════════════════════════════════════

(require 'transient)

(defun my/notmuch-go (query)
  "Run a notmuch search in the current window."
  (notmuch-search query))

(transient-define-prefix my/notmuch-menu ()
  "Notmuch mail.  Also: P=Priority  I=Inbox  N=Notices  J=Jump"
  ["Views"
   ("i" "Inbox (unread)"       (lambda () (interactive) (my/notmuch-go "tag:inbox and tag:unread and not tag:shitbox and not tag:low and date:2weeks..")))
   ("p" "Priority"             (lambda () (interactive) (my/notmuch-go "tag:priority and date:1month..")))
   ("o" "Normal (unread)"      (lambda () (interactive) (my/notmuch-go "tag:inbox and tag:unread and not tag:priority and not tag:shitbox and not tag:low and not tag:notice and date:2weeks..")))
   ("n" "Notices (2w)"         (lambda () (interactive) (my/notmuch-go "tag:notice and date:2w..")))
   ("a" "All inbox (1m)"       (lambda () (interactive) (my/notmuch-go "tag:inbox and not tag:shitbox and not tag:low and date:1month..")))
   ("j" "Job offers"           (lambda () (interactive) (my/notmuch-go "tag:job_offer")))
   ("t" "Sent (1w)"            (lambda () (interactive) (my/notmuch-go "(folder:novemberuniform/Sent or folder:crewcapable/[Gmail]/Sent Mail or folder:personal/[Gmail]/Sent Mail) and date:1w..")))]
  ["Compose"
   ("m" "New message"          notmuch-mua-new-mail)
   ("D" "Drafts"               (lambda () (interactive) (my/notmuch-go "folder:novemberuniform/Drafts")))]
  ["Tools"
   ("s" "Search"               notmuch-search)
   ("G" "Sync mail"            my/notmuch-sync)
   ("q" "Quit notmuch"         my/notmuch-quit)])

;; ════════════════════════════════════════════════════════════════
;; Entry point — C-c N opens inbox, shows menu
;; ════════════════════════════════════════════════════════════════

(global-set-key (kbd "C-c N") #'my/notmuch)

(defvar my/notmuch--prior-config nil
  "Window configuration before entering notmuch.")

(defun my/notmuch ()
  "Open notmuch full-frame to inbox, show transient menu."
  (interactive)
  (setq my/notmuch--prior-config (current-window-configuration))
  (dolist (win (window-list))
    (set-window-dedicated-p win nil))
  (delete-other-windows)
  (notmuch-search "tag:inbox and tag:unread and not tag:shitbox and not tag:low and date:2weeks.."))

(defun my/notmuch-quit ()
  "Quit notmuch and restore previous window layout."
  (interactive)
  (my/notmuch--stop-refresh-timer)
  (if my/notmuch--prior-config
      (progn
        (set-window-configuration my/notmuch--prior-config)
        (setq my/notmuch--prior-config nil))
    (quit-window)))

;; ════════════════════════════════════════════════════════════════
;; Actions — search mode
;; ════════════════════════════════════════════════════════════════

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
  "Sync mail: mbsync + notmuch new, then refresh the current view."
  (interactive)
  (if (get-process "mbsync")
      (message "Sync already running...")
    (let ((search-buf (current-buffer)))
      (message "Syncing mail...")
      (set-process-sentinel
       (start-process "mbsync" "*mbsync*" "bash" "-c" "mbsync -a && notmuch new")
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (message "Mail sync complete")
           (when (buffer-live-p search-buf)
             (with-current-buffer search-buf
               (notmuch-refresh-this-buffer)))))))))

;; Auto-refresh notmuch buffers every 2 minutes to pick up external syncs
(defvar my/notmuch--refresh-timer nil)

(defun my/notmuch--auto-refresh ()
  "Refresh notmuch search buffers if any are visible."
  (dolist (win (window-list))
    (let ((buf (window-buffer win)))
      (when (with-current-buffer buf
              (derived-mode-p 'notmuch-search-mode))
        (with-current-buffer buf
          (notmuch-refresh-this-buffer))))))

(defun my/notmuch--start-refresh-timer ()
  "Start the auto-refresh timer."
  (unless my/notmuch--refresh-timer
    (setq my/notmuch--refresh-timer
          (run-with-timer 120 120 #'my/notmuch--auto-refresh))))

(defun my/notmuch--stop-refresh-timer ()
  "Stop the auto-refresh timer."
  (when my/notmuch--refresh-timer
    (cancel-timer my/notmuch--refresh-timer)
    (setq my/notmuch--refresh-timer nil)))

(add-hook 'notmuch-search-mode-hook #'my/notmuch--start-refresh-timer)

;; ════════════════════════════════════════════════════════════════
;; Actions — show mode
;; ════════════════════════════════════════════════════════════════

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
