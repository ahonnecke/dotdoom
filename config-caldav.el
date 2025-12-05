;;; ~/.doom.d/config-caldav.el -*- lexical-binding: t; -*-
;;
;; Org-caldav: Sync org-mode with CalDAV calendars (Google, Fastmail, etc)
;;
;; SETUP REQUIRED:
;; 1. Add credentials to ~/.authinfo:
;;
;;    For Google Calendar:
;;    machine apidata.googleusercontent.com login YOUR_EMAIL password YOUR_APP_PASSWORD
;;
;;    For Fastmail:
;;    machine caldav.fastmail.com login YOUR_EMAIL password YOUR_APP_PASSWORD
;;
;; 2. Get your Google Calendar ID:
;;    - Go to calendar.google.com -> Settings -> your calendar -> "Integrate calendar"
;;    - Primary calendar ID is usually your email address
;;    - Other calendars have a long ID like "abc123@group.calendar.google.com"
;;
;; 3. For Google, create an App Password:
;;    - Go to myaccount.google.com -> Security -> 2-Step Verification -> App passwords
;;    - Generate a password for "Mail" or "Other"
;;

(use-package! org-caldav
  :commands (org-caldav-sync)
  :config
  ;; Where to store synced calendar events
  (setq org-caldav-inbox "~/org/calendar-inbox.org")

  ;; Org files that contain events to sync TO the calendar
  ;; Events with timestamps in these files will be pushed to CalDAV
  (setq org-caldav-files '("~/org/calendar.org"))

  ;; Google Calendar configuration
  (setq org-caldav-url "https://apidata.googleusercontent.com/caldav/v2"
        ;; Your primary calendar - usually your email address
        ;; For other calendars, use the Calendar ID from Google settings
        org-caldav-calendar-id "YOUR_EMAIL@gmail.com")

  ;; ;; Fastmail configuration (uncomment to use instead of Google)
  ;; (setq org-caldav-url "https://caldav.fastmail.com/dav/calendars/user/YOUR_EMAIL@fastmail.com/"
  ;;       org-caldav-calendar-id "DEFAULT")  ; or specific calendar name

  ;; Sync behavior
  (setq org-caldav-delete-org-entries 'ask      ; Ask before deleting local entries
        org-caldav-delete-calendar-entries 'ask  ; Ask before deleting remote entries
        org-caldav-sync-changes-to-org 'all)     ; Sync all changes from calendar

  ;; Save org buffers after sync
  (setq org-caldav-save-directory "~/org/.org-caldav/")

  ;; Debug - set to t if you have sync issues
  (setq org-caldav-debug-level 0))

;; Create the save directory if it doesn't exist
(make-directory "~/org/.org-caldav/" t)

;; Keybindings under C-c C-s (calendar Sync)
(map! :leader
      (:prefix ("C" . "calendar")
       :desc "Sync calendar" "s" #'org-caldav-sync
       :desc "Sync (no confirm)" "S" (cmd! (let ((org-caldav-delete-org-entries 'never)
                                                  (org-caldav-delete-calendar-entries 'never))
                                             (org-caldav-sync)))))

;; Optional: Auto-sync on a timer (uncomment to enable)
;; (run-with-timer 0 (* 30 60) #'org-caldav-sync)  ; Sync every 30 minutes

(provide 'config-caldav)
