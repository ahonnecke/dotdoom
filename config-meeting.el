;;; ~/.doom.d/config-meeting.el -*- lexical-binding: t; -*-

;; Meeting Notes System
;; A generalized system for meeting notes, agenda items, and templates.
;; Standup is just one type of meeting.
;;
;; Usage:
;;   C-c M n   - Add item to next meeting (smart detection)
;;   C-c M o   - Open a specific meeting's notes
;;   C-c M t   - Open today's occurrence of a meeting
;;   C-c M c   - Carryover items from previous occurrence
;;   C-c M e   - Export meeting notes
;;   C-c M ?   - Transient menu
;;
;; Standup shortcuts (C-c M s prefix):
;;   C-c M s d - Add to Done
;;   C-c M s g - Add to Doing
;;   C-c M s b - Add to Blockers
;;   C-c M s a - Add to Agenda

(require 'org)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup meeting nil
  "Meeting notes configuration."
  :group 'org
  :prefix "meeting-")

(defcustom meeting-directory "~/org/meetings/"
  "Root directory for meeting notes."
  :type 'directory
  :group 'meeting)

(defcustom meeting-timestamp-format "%Y-%m-%d %a"
  "Format for meeting occurrence headers."
  :type 'string
  :group 'meeting)

(defcustom meeting-file-format "%Y-%m-%d.org"
  "Format for meeting occurrence filenames."
  :type 'string
  :group 'meeting)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Meeting Type Registry
;;; ════════════════════════════════════════════════════════════════════════════

(defvar meeting-types (make-hash-table :test 'equal)
  "Hash table of registered meeting types.
Keys are meeting type symbols, values are plists.")

(cl-defun meeting-define (type &key name calendar-match fallback-time
                                fallback-days sections directory
                                export-format)
  "Define a meeting type.

TYPE is a symbol identifying this meeting type.
NAME is a human-readable name.
CALENDAR-MATCH is a regex to match calendar event names.
FALLBACK-TIME is the default meeting time (HH:MM) when no calendar.
FALLBACK-DAYS is a list of days (0=Sun, 1=Mon, ...) when meeting occurs.
SECTIONS is a list of section names for the template.
DIRECTORY is the subdirectory under `meeting-directory' for this meeting's files.
EXPORT-FORMAT is optional custom export format."
  (let ((dir (or directory
                 (expand-file-name (symbol-name type) meeting-directory))))
    (puthash type
             (list :name (or name (symbol-name type))
                   :calendar-match calendar-match
                   :fallback-time (or fallback-time "09:00")
                   :fallback-days (or fallback-days '(1 2 3 4 5))
                   :sections (or sections '("Notes"))
                   :directory dir
                   :export-format export-format)
             meeting-types)))

(defun meeting-get (type property)
  "Get PROPERTY for meeting TYPE."
  (when-let ((props (gethash type meeting-types)))
    (plist-get props property)))

(defun meeting-types-list ()
  "Return list of registered meeting type symbols."
  (let (types)
    (maphash (lambda (k _v) (push k types)) meeting-types)
    (nreverse types)))

(defun meeting--completing-read (prompt)
  "Prompt user to select a meeting type."
  (let* ((types (meeting-types-list))
         (names (mapcar (lambda (type)
                          (cons (meeting-get type :name) type))
                        types))
         (selection (completing-read prompt (mapcar #'car names) nil t)))
    (cdr (assoc selection names))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; File/Directory Management
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--ensure-directory (type)
  "Ensure the directory for meeting TYPE exists."
  (let ((dir (meeting-get type :directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun meeting--occurrence-file (type &optional time)
  "Get the file path for meeting TYPE occurrence at TIME (default now)."
  (let* ((time (or time (current-time)))
         (dir (meeting--ensure-directory type))
         (filename (format-time-string meeting-file-format time)))
    (expand-file-name filename dir)))

(defun meeting--occurrence-exists-p (type &optional time)
  "Return t if an occurrence file exists for TYPE at TIME."
  (file-exists-p (meeting--occurrence-file type time)))

(defun meeting--occurrence-has-content-p (type &optional time)
  "Return t if TYPE's occurrence at TIME has content beyond template."
  (let ((file (meeting--occurrence-file type time)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        ;; Check if there are any *** items (actual content)
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* " nil t)))))

(defun meeting--previous-occurrence-file (type &optional before-time)
  "Find the most recent occurrence file for TYPE before BEFORE-TIME."
  (let* ((dir (meeting-get type :directory))
         (before (or before-time (current-time)))
         (before-file (format-time-string meeting-file-format before))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.org$" t))))
    (when files
      ;; Sort by filename (date-based names sort correctly)
      (setq files (sort files #'string>))
      ;; Find first file that's before the current one
      (cl-find-if (lambda (f)
                    (string< (file-name-nondirectory f) before-file))
                  files))))

(defun meeting--create-occurrence (type &optional time)
  "Create a new occurrence file for TYPE at TIME with template."
  (let* ((time (or time (current-time)))
         (file (meeting--occurrence-file type time))
         (sections (meeting-get type :sections))
         (name (meeting-get type :name))
         (date-str (format-time-string meeting-timestamp-format time)))
    (with-temp-file file
      (insert (format "#+title: %s - %s\n\n" name date-str))
      (insert (format "* %s\n\n" date-str))
      (dolist (section sections)
        (insert (format "** %s\n\n" section))))
    file))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Schedule Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--parse-time (time-str)
  "Parse TIME-STR (HH:MM) into a time value for today."
  (let* ((parts (split-string time-str ":"))
         (hour (string-to-number (car parts)))
         (minute (string-to-number (cadr parts)))
         (now (decode-time))
         (today (encode-time 0 minute hour
                             (nth 3 now) (nth 4 now) (nth 5 now))))
    today))

(defun meeting--is-meeting-day-p (type &optional time)
  "Return t if TIME falls on a day when TYPE meeting occurs."
  (let* ((time (or time (current-time)))
         (dow (string-to-number (format-time-string "%w" time)))
         (days (meeting-get type :fallback-days)))
    (memq dow days)))

(defun meeting--meeting-time-today (type)
  "Get the scheduled time for TYPE today, or nil if not scheduled."
  ;; TODO: Check calendar first, fall back to fallback-time
  (when (meeting--is-meeting-day-p type)
    (meeting--parse-time (meeting-get type :fallback-time))))

(defun meeting--meeting-passed-p (type &optional time)
  "Return t if TYPE's meeting has passed for the day at TIME."
  (let* ((time (or time (current-time)))
         (meeting-time (meeting--meeting-time-today type)))
    (and meeting-time
         (time-less-p meeting-time time)
         (meeting--occurrence-has-content-p type time))))

(defun meeting--next-occurrence-time (type &optional from-time)
  "Get the time of the next occurrence of TYPE from FROM-TIME.
Returns a time value."
  (let* ((from (or from-time (current-time)))
         (days (meeting-get type :fallback-days))
         (time-str (meeting-get type :fallback-time)))
    (if (and (meeting--is-meeting-day-p type from)
             (not (meeting--meeting-passed-p type from)))
        ;; Today, hasn't happened yet
        (meeting--parse-time time-str)
      ;; Find next occurrence day
      (let ((current-dow (string-to-number (format-time-string "%w" from)))
            (days-ahead 1))
        (while (not (memq (mod (+ current-dow days-ahead) 7) days))
          (cl-incf days-ahead))
        (let* ((next-day (time-add from (days-to-time days-ahead)))
               (decoded (decode-time next-day))
               (parts (split-string time-str ":"))
               (hour (string-to-number (car parts)))
               (minute (string-to-number (cadr parts))))
          (encode-time 0 minute hour
                       (nth 3 decoded) (nth 4 decoded) (nth 5 decoded)))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Operations
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--goto-section (buffer section-name)
  "In BUFFER, go to SECTION-NAME and return point, or nil if not found."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*\\* %s" (regexp-quote section-name)) nil t)
      (end-of-line)
      (point))))

(defun meeting--add-item (type section item &optional time)
  "Add ITEM to SECTION of TYPE's occurrence at TIME."
  (let* ((time (or time (current-time)))
         (file (meeting--occurrence-file type time)))
    (unless (file-exists-p file)
      (meeting--create-occurrence type time))
    (with-current-buffer (find-file-noselect file)
      (if-let ((pos (meeting--goto-section (current-buffer) section)))
          (progn
            (goto-char pos)
            ;; Find end of section
            (let ((section-end (save-excursion
                                (if (re-search-forward "^\\*\\* " nil t)
                                    (line-beginning-position)
                                  (point-max)))))
              (goto-char section-end)
              (skip-chars-backward "\n\t ")
              (end-of-line)
              (insert (format "\n*** %s" item))
              (save-buffer)
              (message "Added to %s: %s" section item)))
        (error "Section '%s' not found in meeting template" section)))))

(defun meeting--add-to-next (type section item)
  "Add ITEM to SECTION of TYPE's next occurrence."
  (let ((next-time (meeting--next-occurrence-time type)))
    (meeting--add-item type section item next-time)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Interactive Commands
;;; ════════════════════════════════════════════════════════════════════════════

;;;###autoload
(defun meeting-open (type)
  "Open meeting notes for TYPE, prompting for which occurrence."
  (interactive (list (meeting--completing-read "Meeting: ")))
  (let* ((dir (meeting-get type :directory))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.org$" t))))
    (if files
        (let* ((file-alist (mapcar (lambda (f)
                                     (cons (file-name-nondirectory f) f))
                                   (sort files #'string>)))
               (choice (completing-read "Occurrence: "
                                        (mapcar #'car file-alist) nil t))
               (file (cdr (assoc choice file-alist))))
          (find-file file))
      (message "No occurrences found for %s" (meeting-get type :name)))))

;;;###autoload
(defun meeting-open-today (type)
  "Open or create today's occurrence of TYPE."
  (interactive (list (meeting--completing-read "Meeting: ")))
  (let ((file (meeting--occurrence-file type)))
    (unless (file-exists-p file)
      (meeting--create-occurrence type))
    (find-file file)
    (goto-char (point-min))))

;;;###autoload
(defun meeting-add-to-next (type section item)
  "Add ITEM to SECTION of TYPE's next occurrence.
If called interactively, prompts for meeting, section, and item."
  (interactive
   (let* ((type (meeting--completing-read "Meeting: "))
          (sections (meeting-get type :sections))
          (section (completing-read "Section: " sections nil t))
          (item (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string (format "%s: " section)))))
     (list type section item)))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next type section item)))

;;;###autoload
(defun meeting-carryover (type section)
  "Copy incomplete items from TYPE's previous occurrence to next.
Copies items from SECTION."
  (interactive
   (let* ((type (meeting--completing-read "Meeting: "))
          (sections (meeting-get type :sections))
          (section (completing-read "Section to carryover: " sections nil t)))
     (list type section)))
  (let ((prev-file (meeting--previous-occurrence-file type)))
    (if prev-file
        (let ((items '()))
          (with-temp-buffer
            (insert-file-contents prev-file)
            (goto-char (point-min))
            (when (re-search-forward (format "^\\*\\* %s" (regexp-quote section)) nil t)
              (let ((section-end (save-excursion
                                  (if (re-search-forward "^\\*\\* " nil t)
                                      (line-beginning-position)
                                    (point-max)))))
                (while (re-search-forward "^\\*\\*\\* \\(.+\\)" section-end t)
                  (push (match-string 1) items)))))
          (if items
              (progn
                (dolist (item (nreverse items))
                  (meeting--add-to-next type section item))
                (message "Carried over %d items to next %s"
                         (length items) (meeting-get type :name)))
            (message "No items to carry over from %s" section)))
      (message "No previous occurrence found for %s" (meeting-get type :name)))))

;;;###autoload
(defun meeting-export (type)
  "Export TYPE's current/today's occurrence to kill ring."
  (interactive (list (meeting--completing-read "Meeting: ")))
  (let ((file (meeting--occurrence-file type)))
    (if (file-exists-p file)
        (let ((export-text "")
              (name (meeting-get type :name)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Skip title
            (when (re-search-forward "^\\* " nil t)
              (forward-line)
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
                  (cond
                   ;; Section header
                   ((string-match "^\\*\\* \\(.+\\)" line)
                    (setq export-text
                          (concat export-text
                                  (if (string-empty-p export-text) "" "\n")
                                  "*" (match-string 1 line) "*\n")))
                   ;; Item
                   ((string-match "^\\*\\*\\* \\(.+\\)" line)
                    (setq export-text
                          (concat export-text "• " (match-string 1 line) "\n")))
                   ;; Sub-item
                   ((string-match "^\\*\\*\\*\\* \\(.+\\)" line)
                    (setq export-text
                          (concat export-text "  ◦ " (match-string 1 line) "\n")))))
                (forward-line))))
          (if (string-empty-p export-text)
              (message "No content to export for %s" name)
            (kill-new export-text)
            (message "Exported %s to clipboard" name)))
      (message "No occurrence file for %s today" (meeting-get type :name)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Standup-Specific Commands
;;; ════════════════════════════════════════════════════════════════════════════

;;;###autoload
(defun meeting-standup-done (item)
  "Add ITEM to standup Done section."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Done: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next 'standup "Done" item)))

;;;###autoload
(defun meeting-standup-doing (item)
  "Add ITEM to standup Doing section."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Doing: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next 'standup "Doing" item)))

;;;###autoload
(defun meeting-standup-blocker (item)
  "Add ITEM to standup Blockers section."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Blocker: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next 'standup "Blockers" item)))

;;;###autoload
(defun meeting-standup-question (item)
  "Add ITEM to standup Questions section."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Question: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next 'standup "Questions" item)))

;;;###autoload
(defun meeting-standup-agenda (item)
  "Add ITEM to standup Agenda section (discussion topics)."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Agenda: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-next 'standup "Agenda" item)))

;;;###autoload
(defun meeting-standup ()
  "Open today's standup notes."
  (interactive)
  (meeting-open-today 'standup))

;;;###autoload
(defun meeting-standup-export ()
  "Export today's standup to clipboard."
  (interactive)
  (meeting-export 'standup))

;;;###autoload
(defun meeting-standup-carryover ()
  "Carry over Doing items from previous standup."
  (interactive)
  (meeting-carryover 'standup "Doing"))

;;;###autoload
(defun meeting-standup-post-slack ()
  "Export today's standup and post to Slack channel.
Requires emacs-slack to be configured."
  (interactive)
  (if (and (fboundp 'slack-channel-select)
           (boundp 'slack-teams)
           slack-teams)
      (progn
        ;; Get the export text
        (meeting-export 'standup)
        (let ((export-text (current-kill 0)))
          (if (and export-text (not (string-empty-p export-text)))
              ;; Post to slack - select channel then send
              (slack-channel-select
               :on-select (lambda (room team)
                            (slack-message-send-internal
                             export-text room team)
                            (message "Standup posted to #%s" (slack-room-name room team))))
            (message "No standup content to post"))))
    (message "Slack not configured. Run M-x slack-start first")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(with-eval-after-load 'transient
  (transient-define-prefix meeting-transient ()
    "Meeting notes commands."
    ["Meeting Notes"
     ["Open"
      ("o" "Open occurrence" meeting-open)
      ("t" "Today's notes" meeting-open-today)]
     ["Add"
      ("n" "Add to next" meeting-add-to-next)
      ("c" "Carryover" meeting-carryover)]
     ["Export"
      ("e" "Export" meeting-export)]]
    ["Standup"
     [("s s" "Open standup" meeting-standup)
      ("s d" "Done" meeting-standup-done)
      ("s g" "Doing" meeting-standup-doing)
      ("s b" "Blocker" meeting-standup-blocker)
      ("s q" "Question" meeting-standup-question)]
     [("s a" "Agenda" meeting-standup-agenda)
      ("s c" "Carryover" meeting-standup-carryover)
      ("s e" "Export" meeting-standup-export)
      ("s p" "Post to Slack" meeting-standup-post-slack)]]))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Main prefix: C-c M
(global-set-key (kbd "C-c M o") #'meeting-open)
(global-set-key (kbd "C-c M t") #'meeting-open-today)
(global-set-key (kbd "C-c M n") #'meeting-add-to-next)
(global-set-key (kbd "C-c M c") #'meeting-carryover)
(global-set-key (kbd "C-c M e") #'meeting-export)
(global-set-key (kbd "C-c M ?") #'meeting-transient)

;; Standup shortcuts: C-c M s
(global-set-key (kbd "C-c M s s") #'meeting-standup)
(global-set-key (kbd "C-c M s d") #'meeting-standup-done)
(global-set-key (kbd "C-c M s g") #'meeting-standup-doing)
(global-set-key (kbd "C-c M s b") #'meeting-standup-blocker)
(global-set-key (kbd "C-c M s q") #'meeting-standup-question)
(global-set-key (kbd "C-c M s a") #'meeting-standup-agenda)
(global-set-key (kbd "C-c M s c") #'meeting-standup-carryover)
(global-set-key (kbd "C-c M s e") #'meeting-standup-export)
(global-set-key (kbd "C-c M s p") #'meeting-standup-post-slack)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Default Meeting Types
;;; ════════════════════════════════════════════════════════════════════════════

;; Standup - daily team sync
(meeting-define 'standup
  :name "Standup"
  :calendar-match "standup\\|daily\\|stand-up"
  :fallback-time "08:00"
  :fallback-days '(1 2 3 4 5)  ; Mon-Fri
  :sections '("Done" "Doing" "Blockers" "Questions" "Agenda"))

;; PSC IT Sync
(meeting-define 'psc-it-sync
  :name "PSC IT Sync"
  :calendar-match "PSC.*IT\\|IT.*sync"
  :fallback-time "10:00"
  :fallback-days '(2 4)  ; Tue, Thu (adjust as needed)
  :sections '("Updates" "Discussion" "Action Items" "Notes"))

;; PSC PM Sync
(meeting-define 'psc-pm-sync
  :name "PSC PM Sync"
  :calendar-match "PSC.*PM\\|PM.*sync"
  :fallback-time "14:00"
  :fallback-days '(1 3)  ; Mon, Wed (adjust as needed)
  :sections '("Updates" "Discussion" "Action Items" "Notes"))

(provide 'config-meeting)
;;; config-meeting.el ends here
