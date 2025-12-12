;;; ~/.doom.d/config-calendar.el -*- lexical-binding: t; -*-

;; Calendar Integration - Read-only aggregate view with meeting integration
;;
;; Uses ~/bin/calendar-sync.py to fetch calendars to local cache
;;
;; Setup:
;;   1. Edit ~/.config/calendar-sync/config.toml with your iCal URLs
;;   2. Run: calendar-sync.py (to test)
;;   3. Add cron: */15 * * * * ~/bin/calendar-sync.py 2>&1 | logger -t calendar-sync
;;
;; Keybindings (C-c @ prefix):
;;   C-c @ a - Show agenda (all calendars)
;;   C-c @ t - Today's events
;;   C-c @ n - Next meeting
;;   C-c @ s - Sync now
;;   C-c @ ? - Calendar transient menu

(require 'json)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup calendar-view nil
  "Calendar viewing settings."
  :group 'applications)

(defcustom calendar-sync-script (expand-file-name "~/bin/calendar-sync.py")
  "Path to calendar sync script."
  :type 'string
  :group 'calendar-view)

(defcustom calendar-cache-file (expand-file-name "~/.cache/calendars/events.json")
  "Path to cached events JSON."
  :type 'string
  :group 'calendar-view)

(defcustom calendar-check-interval (* 5 60)
  "Seconds between background calendar checks (default 5 min)."
  :type 'integer
  :group 'calendar-view)

(defcustom calendar-upcoming-warning-minutes 15
  "Minutes before meeting to show notification."
  :type 'integer
  :group 'calendar-view)

(defcustom calendar-meeting-patterns
  '(("standup" . standup)
    ("daily" . standup)
    ("stand-up" . standup)
    ("psc.*sync" . psc-sync)
    ("sync" . sync))
  "Alist of (REGEX . MEETING-TYPE) to match calendar events to meeting types."
  :type '(alist :key-type string :value-type symbol)
  :group 'calendar-view)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Event Data Structure
;;; ════════════════════════════════════════════════════════════════════════════

(cl-defstruct calendar-event
  summary start end location description calendar uid)

(defun calendar--parse-iso-time (str)
  "Parse ISO 8601 time string STR to Emacs time."
  (when (and str (stringp str))
    (condition-case nil
        (encode-time (iso8601-parse str))
      (error nil))))

(defun calendar--event-from-json (json)
  "Create calendar-event from JSON alist."
  (make-calendar-event
   :summary (alist-get 'summary json)
   :start (calendar--parse-iso-time (alist-get 'start json))
   :end (calendar--parse-iso-time (alist-get 'end json))
   :location (alist-get 'location json)
   :description (alist-get 'description json)
   :calendar (alist-get 'calendar json)
   :uid (alist-get 'uid json)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Cache Reading
;;; ════════════════════════════════════════════════════════════════════════════

(defvar calendar--events-cache nil
  "Cached list of calendar-event structs.")

(defvar calendar--cache-time nil
  "Time when cache was last loaded.")

(defun calendar--load-cache ()
  "Load events from JSON cache file."
  (when (file-exists-p calendar-cache-file)
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-file calendar-cache-file))
               (events-json (alist-get 'events data)))
          (setq calendar--events-cache
                (mapcar #'calendar--event-from-json events-json))
          (setq calendar--cache-time (current-time))
          calendar--events-cache)
      (error
       (message "Error loading calendar cache: %s" err)
       nil))))

(defun calendar--get-events (&optional force-reload)
  "Get cached events, reloading if needed or FORCE-RELOAD."
  (when (or force-reload
            (null calendar--events-cache)
            (null calendar--cache-time)
            ;; Reload if cache file is newer
            (and (file-exists-p calendar-cache-file)
                 (time-less-p calendar--cache-time
                              (nth 5 (file-attributes calendar-cache-file)))))
    (calendar--load-cache))
  calendar--events-cache)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Sync
;;; ════════════════════════════════════════════════════════════════════════════

(defun calendar-sync ()
  "Run calendar sync script."
  (interactive)
  (if (file-executable-p calendar-sync-script)
      (progn
        (message "Syncing calendars...")
        (set-process-sentinel
         (start-process "calendar-sync" "*calendar-sync*" calendar-sync-script)
         (lambda (proc event)
           (when (string-match-p "finished" event)
             (calendar--load-cache)
             (message "Calendar sync complete: %d events"
                      (length calendar--events-cache))))))
    (message "Sync script not found: %s" calendar-sync-script)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Display Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun calendar--format-time (time)
  "Format TIME for display."
  (if time
      (format-time-string "%H:%M" time)
    "??:??"))

(defun calendar--format-date (time)
  "Format TIME as date."
  (if time
      (format-time-string "%Y-%m-%d %a" time)
    "????-??-??"))

(defun calendar--format-event (event &optional show-date)
  "Format EVENT for display. Include date if SHOW-DATE."
  (let ((time-str (if show-date
                      (format-time-string "%Y-%m-%d %H:%M" (calendar-event-start event))
                    (calendar--format-time (calendar-event-start event))))
        (summary (calendar-event-summary event))
        (cal (calendar-event-calendar event)))
    (format "%-16s  %-40s [%s]" time-str summary cal)))

(defun calendar--events-in-range (events start end)
  "Filter EVENTS to those between START and END times."
  (seq-filter
   (lambda (e)
     (let ((event-start (calendar-event-start e)))
       (and event-start
            (time-less-p start event-start)
            (time-less-p event-start end))))
   events))

(defun calendar-show-agenda (&optional days)
  "Show combined calendar agenda for DAYS (default 7)."
  (interactive "P")
  (let* ((days (or days 7))
         (events (calendar--get-events))
         (now (current-time))
         (end (time-add now (days-to-time days)))
         (filtered (calendar--events-in-range events now end))
         (buf (get-buffer-create "*Calendar Agenda*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "CALENDAR AGENDA - Next %d days\n" days)
                            'face '(:weight bold :height 1.2)))
        (insert (propertize (format "Updated: %s  |  %d events\n\n"
                                    (if calendar--cache-time
                                        (format-time-string "%H:%M" calendar--cache-time)
                                      "never")
                                    (length filtered))
                            'face 'font-lock-comment-face))

        (if (null filtered)
            (insert "No events (run C-c @ s to sync)\n")
          ;; Group by date
          (let ((current-date nil))
            (dolist (event filtered)
              (let ((event-date (calendar--format-date (calendar-event-start event))))
                (unless (equal event-date current-date)
                  (setq current-date event-date)
                  (insert (propertize (format "\n%s\n" event-date)
                                      'face '(:foreground "cyan" :weight bold)))))
              (insert (format "  %s\n" (calendar--format-event event))))))

        (goto-char (point-min))
        (special-mode)
        (local-set-key (kbd "g") #'calendar-show-agenda)
        (local-set-key (kbd "s") #'calendar-sync)
        (local-set-key (kbd "t") #'calendar-show-today)
        (local-set-key (kbd "n") #'calendar-next-meeting)))
    (pop-to-buffer buf)))

(defun calendar-show-today ()
  "Show today's calendar events."
  (interactive)
  (let* ((events (calendar--get-events))
         (now (current-time))
         (today-start (encode-time (decode-time now)))
         (today-end (time-add today-start (days-to-time 1)))
         (filtered (calendar--events-in-range events
                                              (time-subtract today-start (days-to-time 1))
                                              today-end))
         (buf (get-buffer-create "*Calendar Today*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "TODAY'S EVENTS\n" 'face '(:weight bold :height 1.2)))
        (insert (propertize (format-time-string "%A, %B %d, %Y\n\n")
                            'face 'font-lock-comment-face))

        (if (null filtered)
            (insert "No events today\n")
          (dolist (event filtered)
            (insert (format "%s\n" (calendar--format-event event)))))

        (goto-char (point-min))
        (special-mode)
        (local-set-key (kbd "g") #'calendar-show-today)
        (local-set-key (kbd "a") #'calendar-show-agenda)))
    (pop-to-buffer buf)))

(defun calendar-next-meeting ()
  "Show the next upcoming meeting."
  (interactive)
  (let* ((events (calendar--get-events))
         (now (current-time))
         (next (seq-find (lambda (e)
                           (time-less-p now (calendar-event-start e)))
                         events)))
    (if next
        (let* ((start (calendar-event-start next))
               (delta (time-subtract start now))
               (minutes (/ (float-time delta) 60)))
          (message "Next: %s in %.0f min [%s]"
                   (calendar-event-summary next)
                   minutes
                   (calendar-event-calendar next)))
      (message "No upcoming events"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Meeting Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun calendar--match-meeting-type (summary)
  "Match SUMMARY against `calendar-meeting-patterns', return meeting type or nil."
  (let ((summary-lower (downcase summary)))
    (cl-some (lambda (pattern)
               (when (string-match-p (car pattern) summary-lower)
                 (cdr pattern)))
             calendar-meeting-patterns)))

(defun calendar--upcoming-meetings (&optional minutes)
  "Get meetings starting in the next MINUTES (default 15)."
  (let* ((events (calendar--get-events))
         (now (current-time))
         (soon (time-add now (seconds-to-time (* (or minutes 15) 60)))))
    (seq-filter
     (lambda (e)
       (let ((start (calendar-event-start e)))
         (and start
              (time-less-p now start)
              (time-less-p start soon))))
     events)))

(defvar calendar--notified-events nil
  "List of event UIDs we've already notified about.")

(defun calendar--check-upcoming ()
  "Check for upcoming meetings and notify/act."
  (let ((upcoming (calendar--upcoming-meetings calendar-upcoming-warning-minutes)))
    (dolist (event upcoming)
      (let ((uid (calendar-event-uid event)))
        (unless (member uid calendar--notified-events)
          (push uid calendar--notified-events)
          (let* ((summary (calendar-event-summary event))
                 (meeting-type (calendar--match-meeting-type summary))
                 (minutes (/ (float-time
                              (time-subtract (calendar-event-start event)
                                             (current-time)))
                             60)))
            ;; Notify
            (message "Upcoming: %s in %.0f min" summary minutes)

            ;; If it matches a meeting type, offer to open notes
            (when (and meeting-type
                       (fboundp 'meeting-open-today))
              (run-at-time 1 nil
                           (lambda (type sum)
                             (when (y-or-n-p
                                    (format "%s starting soon. Open notes? " sum))
                               (meeting-open-today type)))
                           meeting-type summary))))))))

;; Clean up old notification tracking daily
(defun calendar--clean-notified ()
  "Clear notification tracking."
  (setq calendar--notified-events nil))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Background Timer
;;; ════════════════════════════════════════════════════════════════════════════

(defvar calendar--timer nil
  "Timer for background calendar checks.")

(defun calendar-start-background-checks ()
  "Start background timer for calendar checks."
  (interactive)
  (calendar-stop-background-checks)
  (setq calendar--timer
        (run-with-timer 60  ; Start after 1 min
                        calendar-check-interval
                        #'calendar--check-upcoming))
  (message "Calendar background checks started (every %d sec)"
           calendar-check-interval))

(defun calendar-stop-background-checks ()
  "Stop background calendar timer."
  (interactive)
  (when calendar--timer
    (cancel-timer calendar--timer)
    (setq calendar--timer nil)
    (message "Calendar background checks stopped")))

;; Auto-start on load (comment out if you don't want this)
(add-hook 'emacs-startup-hook #'calendar-start-background-checks)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix calendar-transient ()
  "Calendar commands."
  ["Calendar"
   ["View"
    ("a" "Agenda (7 days)" calendar-show-agenda)
    ("t" "Today" calendar-show-today)
    ("w" "Week" (lambda () (interactive) (calendar-show-agenda 7)))
    ("m" "Month" (lambda () (interactive) (calendar-show-agenda 30)))]
   ["Actions"
    ("n" "Next meeting" calendar-next-meeting)
    ("s" "Sync now" calendar-sync)]
   ["Background"
    ("B" "Start checks" calendar-start-background-checks)
    ("Q" "Stop checks" calendar-stop-background-checks)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings (C-c @ prefix - @ looks like a clock)
;;; ════════════════════════════════════════════════════════════════════════════

(define-key ashton-mode-map (kbd "C-c @ a") #'calendar-show-agenda)
(define-key ashton-mode-map (kbd "C-c @ t") #'calendar-show-today)
(define-key ashton-mode-map (kbd "C-c @ n") #'calendar-next-meeting)
(define-key ashton-mode-map (kbd "C-c @ s") #'calendar-sync)
(define-key ashton-mode-map (kbd "C-c @ w") (lambda () (interactive) (calendar-show-agenda 7)))
(define-key ashton-mode-map (kbd "C-c @ ?") #'calendar-transient)

(provide 'config-calendar)
;;; config-calendar.el ends here
