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

(defcustom meeting-use-org-todo t
  "When non-nil, use native org-mode TODO states for items.
Items in `meeting-todo-sections' will be inserted with TODO prefix."
  :type 'boolean
  :group 'meeting)

(defcustom meeting-todo-sections '("Doing")
  "Sections where new items default to TODO state.
Only applies when `meeting-use-org-todo' is non-nil."
  :type '(repeat string)
  :group 'meeting)

(defcustom meeting-share-default-user "Daly"
  "Default user display name for sharing standup updates via Slack DM."
  :type 'string
  :group 'meeting)

(defcustom meeting-share-default-sections '("Doing" "Blockers")
  "Sections to include when sharing standup with a user."
  :type '(repeat string)
  :group 'meeting)

(defcustom meeting-magit-mark-done-enabled nil
  "When non-nil, offer to mark standup items done after commits."
  :type 'boolean
  :group 'meeting)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Org-Mode Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--setup-org-todo ()
  "Setup org-todo-keywords for meeting files."
  (when (and buffer-file-name
             (string-match-p "meetings/" buffer-file-name))
    (setq-local org-todo-keywords '((sequence "TODO" "DONE")))))

(add-hook 'org-mode-hook #'meeting--setup-org-todo)

;; Convenient in-buffer keybindings for standup files
(defvar meeting-standup-map (make-sparse-keymap)
  "Keymap for standup meeting files.")

(define-key meeting-standup-map (kbd "x") #'org-todo)  ; Toggle TODO/DONE at point
(define-key meeting-standup-map (kbd "d") #'meeting-standup-mark-done)  ; Pick from list

(defun meeting--setup-standup-keys ()
  "Setup convenient keybindings for standup files."
  (when (and buffer-file-name
             (string-match-p "meetings/standup" buffer-file-name))
    ;; Make x and d work without prefix in standup files
    (local-set-key (kbd "C-c x") #'org-todo)
    (local-set-key (kbd "C-c d") #'meeting-standup-mark-done)))

(add-hook 'org-mode-hook #'meeting--setup-standup-keys)

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
      (insert (format "* %s\n" date-str))
      (if sections
          ;; Traditional section-based template
          (dolist (section sections)
            (insert (format "\n** %s\n" section)))
        ;; Pure org-mode template (no sections)
        (insert "\n")))
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
  "Add ITEM to SECTION of TYPE's occurrence at TIME.
If `meeting-use-org-todo' is non-nil and SECTION is in
`meeting-todo-sections', the item is inserted with TODO prefix."
  (let* ((time (or time (current-time)))
         (file (meeting--occurrence-file type time))
         (use-todo (and meeting-use-org-todo
                        (member section meeting-todo-sections))))
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
              (if use-todo
                  (insert (format "\n*** TODO %s" item))
                (insert (format "\n*** %s" item)))
              (save-buffer)
              (message "Added to %s: %s" section item)))
        (error "Section '%s' not found in meeting template" section)))))

(defun meeting--add-to-next (type section item)
  "Add ITEM to SECTION of TYPE's next occurrence."
  (let ((next-time (meeting--next-occurrence-time type)))
    (meeting--add-item type section item next-time)))

(defun meeting--add-to-today (type section item)
  "Add ITEM to SECTION of TYPE's today occurrence."
  (meeting--add-item type section item (current-time)))

;;;###autoload
(defun meeting-add-to-today (type section item)
  "Add ITEM to SECTION of TYPE's today occurrence.
Use this to pre-fill agenda items for today's meeting."
  (interactive
   (let* ((type (meeting--completing-read "Meeting: "))
          (sections (meeting-get type :sections))
          (section (completing-read "Section: " sections nil t))
          (item (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string (format "%s: " section)))))
     (list type section item)))
  (when (and item (not (string-empty-p item)))
    (meeting--add-to-today type section item)))

(defun meeting--add-org-item (type item &optional state tags time)
  "Add ITEM to TYPE's occurrence at TIME using pure org structure.
STATE is TODO, DONE, or nil (plain item).
TAGS is a list of tag strings (without colons)."
  (let* ((time (or time (current-time)))
         (file (meeting--occurrence-file type time))
         (tag-str (if tags (format ":%s:" (string-join tags ":")) "")))
    (unless (file-exists-p file)
      (meeting--create-occurrence type time))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      ;; Find the date heading
      (if (re-search-forward "^\\* " nil t)
          (progn
            ;; Go to end of content under this heading
            (let ((heading-end (save-excursion
                                (if (re-search-forward "^\\* " nil t)
                                    (line-beginning-position)
                                  (point-max)))))
              (goto-char heading-end)
              (skip-chars-backward "\n\t ")
              (end-of-line)
              ;; Insert the item
              (insert (format "\n** %s%s%s"
                              (if state (format "%s " state) "")
                              item
                              (if (string-empty-p tag-str) ""
                                (format "%s%s"
                                        (make-string (max 1 (- 70 (length item))) ? )
                                        tag-str))))
              (save-buffer)
              (message "%s: %s" (or state "Added") item)))
        (error "No date heading found in %s" file)))))

(defun meeting--add-org-item-next (type item &optional state tags)
  "Add ITEM to TYPE's next occurrence using pure org structure."
  (let ((next-time (meeting--next-occurrence-time type)))
    (meeting--add-org-item type item state tags next-time)))

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

(defun meeting--item-is-done-p (item-text)
  "Return non-nil if ITEM-TEXT is marked as DONE.
Handles both `DONE:' prefix and org-mode native DONE state."
  (string-match-p "^DONE:?\\s-" item-text))

(defun meeting--strip-todo-prefix (item-text)
  "Remove TODO prefix from ITEM-TEXT if present."
  (if (string-match "^TODO:?\\s-*\\(.+\\)" item-text)
      (match-string 1 item-text)
    item-text))

;;;###autoload
(defun meeting-carryover (type section &optional incomplete-only)
  "Copy items from TYPE's previous occurrence to next.
Copies items from SECTION.
If INCOMPLETE-ONLY is non-nil, skip items marked DONE."
  (interactive
   (let* ((type (meeting--completing-read "Meeting: "))
          (sections (meeting-get type :sections))
          (section (completing-read "Section to carryover: " sections nil t)))
     (list type section nil)))
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
                  (let ((item (match-string 1)))
                    (unless (and incomplete-only (meeting--item-is-done-p item))
                      (push (meeting--strip-todo-prefix item) items)))))))
          (if items
              (progn
                (dolist (item (nreverse items))
                  (meeting--add-to-next type section item))
                (message "Carried over %d %sitems to next %s"
                         (length items)
                         (if incomplete-only "incomplete " "")
                         (meeting-get type :name)))
            (message "No %sitems to carry over from %s"
                     (if incomplete-only "incomplete " "")
                     section)))
      (message "No previous occurrence found for %s" (meeting-get type :name)))))

;;;###autoload
(defun meeting-carryover-incomplete (type section)
  "Copy only incomplete (non-DONE) items from TYPE's previous occurrence.
Like `meeting-carryover' but filters out DONE items."
  (interactive
   (let* ((type (meeting--completing-read "Meeting: "))
          (sections (meeting-get type :sections))
          (section (completing-read "Section to carryover: " sections nil t)))
     (list type section)))
  (meeting-carryover type section t))

(defun meeting--format-item-for-export (item-text)
  "Format ITEM-TEXT for export, handling TODO/DONE states.
Returns (prefix . text) where prefix is the bullet/checkbox."
  (cond
   ;; TODO item -> checkbox unchecked
   ((string-match "^TODO:?\\s-*\\(.+\\)" item-text)
    (cons "- [ ] " (match-string 1 item-text)))
   ;; DONE item -> checkbox checked
   ((string-match "^DONE:?\\s-*\\(.+\\)" item-text)
    (cons "- [x] " (match-string 1 item-text)))
   ;; Regular item -> bullet
   (t (cons "• " item-text))))

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
                   ;; Item (handle TODO/DONE states)
                   ((string-match "^\\*\\*\\* \\(.+\\)" line)
                    (let* ((item-text (match-string 1 line))
                           (formatted (meeting--format-item-for-export item-text)))
                      (setq export-text
                            (concat export-text (car formatted) (cdr formatted) "\n"))))
                   ;; Sub-item (handle TODO/DONE states)
                   ((string-match "^\\*\\*\\*\\* \\(.+\\)" line)
                    (let* ((item-text (match-string 1 line))
                           (formatted (meeting--format-item-for-export item-text)))
                      (setq export-text
                            (concat export-text "  " (car formatted) (cdr formatted) "\n"))))))
                (forward-line))))
          (if (string-empty-p export-text)
              (message "No content to export for %s" name)
            (kill-new export-text)
            (message "Exported %s to clipboard" name)))
      (message "No occurrence file for %s today" (meeting-get type :name)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Standup Helpers (Pure Org-Mode)
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--get-standup-items ()
  "Get all items from today's standup as list of plists.
Each plist has :text :state :tags :line properties."
  (let ((file (meeting--occurrence-file 'standup))
        (items '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Find all ** headings (items)
        (while (re-search-forward "^\\*\\* \\(TODO\\|DONE\\)?\\s-*\\(.+?\\)\\(\\s-+:\\([^:]+:\\)+\\)?$" nil t)
          (let* ((state (match-string 1))
                 (text (string-trim (match-string 2)))
                 (tag-str (match-string 3))
                 (tags (when tag-str
                         (split-string (string-trim tag-str ":") ":" t))))
            (push (list :text text :state state :tags tags :line (line-number-at-pos))
                  items)))))
    (nreverse items)))

(defun meeting--get-standup-todos ()
  "Get TODO items from today's standup.
Returns list of (line . text) for items with TODO state."
  (let ((items (meeting--get-standup-items)))
    (cl-loop for item in items
             when (equal (plist-get item :state) "TODO")
             collect (cons (plist-get item :line) (plist-get item :text)))))

(defun meeting-standup-export-org ()
  "Export today's standup grouped by state and tags.
Format: DONE items first, then TODO, then by tag."
  (let ((items (meeting--get-standup-items)))
    (if (not items)
        (message "No items in today's standup")
      (let ((done '()) (todo '()) (blocked '()) (questions '()) (other '()))
        ;; Group items
        (dolist (item items)
          (let ((state (plist-get item :state))
                (tags (plist-get item :tags))
                (text (plist-get item :text)))
            (cond
             ((equal state "DONE") (push text done))
             ((member "blocked" tags) (push text blocked))
             ((member "question" tags) (push text questions))
             ((equal state "TODO") (push text todo))
             (t (push text other)))))
        ;; Build export
        (let ((export ""))
          (when done
            (setq export (concat export "*Done*\n"
                                (mapconcat (lambda (t) (format "- [x] %s" t))
                                           (nreverse done) "\n") "\n\n")))
          (when todo
            (setq export (concat export "*Doing*\n"
                                (mapconcat (lambda (t) (format "- [ ] %s" t))
                                           (nreverse todo) "\n") "\n\n")))
          (when blocked
            (setq export (concat export "*Blocked*\n"
                                (mapconcat (lambda (t) (format "- [ ] %s" t))
                                           (nreverse blocked) "\n") "\n\n")))
          (when questions
            (setq export (concat export "*Questions*\n"
                                (mapconcat (lambda (t) (format "- %s" t))
                                           (nreverse questions) "\n") "\n\n")))
          (when other
            (setq export (concat export "*Notes*\n"
                                (mapconcat (lambda (t) (format "- %s" t))
                                           (nreverse other) "\n") "\n")))
          (kill-new (string-trim export))
          (message "Exported standup to clipboard"))))))

(defun meeting-standup-carryover-todos ()
  "Carry over incomplete TODO items from previous standup.
Finds TODO items in the most recent standup and adds them to today's."
  (let ((prev-file (meeting--previous-occurrence-file 'standup)))
    (if (not prev-file)
        (message "No previous standup found")
      (let ((todos '()))
        (with-temp-buffer
          (insert-file-contents prev-file)
          (goto-char (point-min))
          ;; Find TODO items (not DONE)
          (while (re-search-forward "^\\*\\* TODO\\s-+\\(.+?\\)\\(\\s-+:[^:]+:\\)?$" nil t)
            (push (match-string 1) todos)))
        (if (not todos)
            (message "No TODO items to carry over")
          (dolist (item (nreverse todos))
            (meeting--add-org-item-next 'standup item "TODO"))
          (message "Carried over %d TODO items" (length todos)))))))

(defun meeting-standup-mark-done ()
  "Mark a TODO item from today's standup as DONE.
Shows numbered list, select by number or search."
  (interactive)
  (let ((todos (meeting--get-standup-todos)))
    (if (not todos)
        (message "No TODO items in today's standup")
      (let* ((choices (cl-loop for (_line . text) in todos
                               for i from 1
                               collect (format "%d: %s" i text)))
             (selection (completing-read "Mark done: " choices nil t)))
        (when (and selection (string-match "^[0-9]+: \\(.+\\)" selection))
          (let ((text (match-string 1 selection))
                (file (meeting--occurrence-file 'standup)))
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward
                       (format "^\\(\\*\\*\\) TODO\\s-+%s" (regexp-quote text)) nil t)
                  (replace-match (format "%s DONE %s" (match-string 1) text))
                  (save-buffer)
                  (message "✓ %s" text)
                  (when (string= buffer-file-name file)
                    (revert-buffer t t t)))))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Standup-Specific Commands (Pure Org-Mode)
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Standup uses pure org-mode structure:
;;   ** TODO item text               - task to do
;;   ** DONE item text               - completed task
;;   ** TODO blocked item   :blocked: - blocked task
;;   ** Question about X   :question: - discussion item
;;   ** Note about Y         :agenda: - meeting topic
;;
;; Mark done: use org-todo (C-c C-t) at point, or M-x meeting-standup-mark-done

;;;###autoload
(defun meeting-standup-add (item)
  "Add a TODO item to today's standup."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "TODO: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-org-item-next 'standup item "TODO")))

;;;###autoload
(defun meeting-standup-add-done (item)
  "Add a DONE item to today's standup (for retroactive logging)."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Done: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-org-item-next 'standup item "DONE")))

;;;###autoload
(defun meeting-standup-add-blocked (item)
  "Add a blocked TODO item to today's standup."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Blocked: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-org-item-next 'standup item "TODO" '("blocked"))))

;;;###autoload
(defun meeting-standup-add-question (item)
  "Add a question/discussion item to today's standup."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Question: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-org-item-next 'standup item nil '("question"))))

;;;###autoload
(defun meeting-standup-add-agenda (item)
  "Add an agenda/discussion topic to today's standup."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Agenda: "))))
  (when (and item (not (string-empty-p item)))
    (meeting--add-org-item-next 'standup item nil '("agenda"))))

;; Aliases for backward compatibility and discoverability
(defalias 'meeting-standup-done 'meeting-standup-add-done)
(defalias 'meeting-standup-doing 'meeting-standup-add)
(defalias 'meeting-standup-blocker 'meeting-standup-add-blocked)
(defalias 'meeting-standup-question 'meeting-standup-add-question)
(defalias 'meeting-standup-agenda 'meeting-standup-add-agenda)

(defcustom meeting-standup-auto-carryover t
  "When non-nil, auto-carryover incomplete TODOs when opening a new standup."
  :type 'boolean
  :group 'meeting)

;;;###autoload
(defun meeting-standup ()
  "Open today's standup notes.
If `meeting-standup-auto-carryover' is non-nil and today's file
is being created fresh, carry over incomplete TODOs from previous standup."
  (interactive)
  (let ((file (meeting--occurrence-file 'standup))
        (is-new (not (meeting--occurrence-exists-p 'standup))))
    (meeting-open-today 'standup)
    ;; Auto-carryover if this is a fresh file
    (when (and is-new meeting-standup-auto-carryover)
      (message "Auto-carrying over incomplete TODOs...")
      (meeting-standup-carryover-todos))))

;;;###autoload
(defun meeting-standup-export ()
  "Export today's standup to clipboard (grouped by state/tag)."
  (interactive)
  (meeting-standup-export-org))

;;;###autoload
(defun meeting-standup-carryover ()
  "Carry over incomplete TODO items from previous standup."
  (interactive)
  (meeting-standup-carryover-todos))

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

(defun meeting--export-sections (type sections)
  "Export only SECTIONS from TYPE's today occurrence.
Returns the export text as a string."
  (let ((file (meeting--occurrence-file type)))
    (if (file-exists-p file)
        (let ((export-text "")
              (section-set (mapcar #'downcase sections)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward "^\\* " nil t)
              (forward-line)
              (let ((in-wanted-section nil))
                (while (not (eobp))
                  (let ((line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
                    (cond
                     ;; Section header - check if we want this section
                     ((string-match "^\\*\\* \\(.+\\)" line)
                      (let ((section-name (match-string 1 line)))
                        (setq in-wanted-section
                              (member (downcase section-name) section-set))
                        (when in-wanted-section
                          (setq export-text
                                (concat export-text
                                        (if (string-empty-p export-text) "" "\n")
                                        "*" section-name "*\n")))))
                     ;; Item - only if in wanted section
                     ((and in-wanted-section
                           (string-match "^\\*\\*\\* \\(.+\\)" line))
                      (let* ((item-text (match-string 1 line))
                             (formatted (meeting--format-item-for-export item-text)))
                        (setq export-text
                              (concat export-text (car formatted) (cdr formatted) "\n"))))
                     ;; Sub-item
                     ((and in-wanted-section
                           (string-match "^\\*\\*\\*\\* \\(.+\\)" line))
                      (let* ((item-text (match-string 1 line))
                             (formatted (meeting--format-item-for-export item-text)))
                        (setq export-text
                              (concat export-text "  " (car formatted) (cdr formatted) "\n"))))))
                  (forward-line)))))
          export-text)
      "")))

(defun meeting--find-slack-user (user-name team)
  "Find Slack user by USER-NAME in TEAM.
Returns the user plist or nil if not found."
  (when (fboundp 'slack-user-name-alist)
    (let ((user-alist (slack-user-name-alist team)))
      (cdr (assoc user-name user-alist #'string-equal-ignore-case)))))

;;;###autoload
(defun meeting-standup-share-with-user (user-name)
  "Share standup sections with USER-NAME via Slack DM.
Sections to share are configured via `meeting-share-default-sections'."
  (interactive
   (if (and (fboundp 'slack-user-name-alist)
            (boundp 'slack-teams)
            slack-teams)
       (let* ((team (car slack-teams))
              (users (slack-user-name-alist team))
              (names (mapcar #'car users)))
         (list (completing-read "Share with: " names nil t
                                meeting-share-default-user)))
     (user-error "Slack not configured. Run M-x slack-start first")))
  (let* ((team (car slack-teams))
         (user (meeting--find-slack-user user-name team))
         (export-text (meeting--export-sections 'standup meeting-share-default-sections)))
    (if (not user)
        (message "User '%s' not found in Slack" user-name)
      (if (string-empty-p export-text)
          (message "No standup content to share")
        (let ((user-id (plist-get user :id)))
          ;; Open DM conversation and send
          (slack-conversations-open
           team
           :user-ids (list user-id)
           :on-success
           (lambda (response)
             (let* ((channel-data (plist-get response :channel))
                    (channel-id (plist-get channel-data :id)))
               (when channel-id
                 (slack-chat-post-message
                  team
                  (list :channel channel-id
                        :text export-text)
                  :on-success
                  (lambda (_)
                    (message "Standup shared with %s" user-name))
                  :on-error
                  (lambda (err)
                    (message "Failed to send: %s" err))))))
           :on-error
           (lambda (err)
             (message "Failed to open DM: %s" err))))))))

;;;###autoload
(defun meeting-standup-share-daly ()
  "Quick share standup with default user (Daly).
Shares sections configured in `meeting-share-default-sections'."
  (interactive)
  (meeting-standup-share-with-user meeting-share-default-user))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Orchard Integration (Feature Creation from Standup)
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--item-at-point ()
  "Get the standup item text at point.
Returns the item text with TODO/DONE prefix stripped, or nil if not on an item."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*\\*\\*\\*? \\(TODO:?\\|DONE:?\\)?\\s-*\\(.+\\)")
      (match-string-no-properties 2))))

(defun meeting--normalize-feature-name (text)
  "Normalize TEXT into a valid branch name.
Converts spaces and special chars to hyphens, downcases."
  (let ((name (downcase text)))
    ;; Remove leading/trailing whitespace
    (setq name (string-trim name))
    ;; Replace spaces and special chars with hyphens
    (setq name (replace-regexp-in-string "[^a-z0-9]+" "-" name))
    ;; Remove leading/trailing hyphens
    (setq name (string-trim name "-"))
    ;; Truncate to reasonable length
    (if (> (length name) 50)
        (substring name 0 50)
      name)))

;;;###autoload
(defun meeting-standup-create-feature ()
  "Create an orchard feature from the standup item at point.
The item text is used as the feature description and normalized into a branch name.
Stores the branch name in the ORCHARD_BRANCH org property."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (let ((item-text (meeting--item-at-point)))
    (unless item-text
      (user-error "Not on a standup item"))
    (let ((name (meeting--normalize-feature-name item-text)))
      (when (string-empty-p name)
        (user-error "Could not derive a feature name from item"))
      (when (yes-or-no-p (format "Create FEATURE/%s? " name))
        ;; Try to use orchard if available
        (if (fboundp 'orchard-new-feature)
            (progn
              (orchard-new-feature name)
              ;; Store branch reference in org property
              (org-set-property "ORCHARD_BRANCH" (format "FEATURE/%s" name))
              (message "Created FEATURE/%s from standup item" name))
          ;; Fallback: just store the intended branch name
          (org-set-property "ORCHARD_BRANCH" (format "FEATURE/%s" name))
          (message "Orchard not loaded. Branch name stored: FEATURE/%s" name))))))

;;;###autoload
(defun meeting-standup-jump-to-branch ()
  "Jump to the orchard branch linked from the standup item at point.
Reads the ORCHARD_BRANCH property and opens that worktree in orchard."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (let ((branch (org-entry-get (point) "ORCHARD_BRANCH")))
    (if branch
        (if (fboundp 'orchard-jump-to-branch)
            (orchard-jump-to-branch branch)
          ;; Fallback: try to find worktree and open magit
          (if (fboundp 'orchard--find-worktree-by-branch)
              (let ((path (orchard--find-worktree-by-branch branch)))
                (if path
                    (magit-status path)
                  (message "Worktree not found for branch: %s" branch)))
            (message "Branch: %s (orchard not loaded)" branch)))
      (message "No ORCHARD_BRANCH property on this item"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Magit Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--get-today-todos ()
  "Get TODO items from today's standup Doing section.
Returns alist of (line-number . item-text) for items with TODO state."
  (let ((file (meeting--occurrence-file 'standup))
        (todos '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Find Doing section
        (when (re-search-forward "^\\*\\* Doing" nil t)
          (let ((section-end (save-excursion
                               (if (re-search-forward "^\\*\\* " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
            ;; Find TODO items
            (while (re-search-forward "^\\*\\*\\* \\(TODO:?\\s-*\\(.+\\)\\)" section-end t)
              (push (cons (line-number-at-pos) (match-string 2)) todos))))))
    (nreverse todos)))

(defun meeting--mark-standup-item-done (item-text)
  "Mark ITEM-TEXT as DONE in today's standup (pure org structure).
Finds the item and changes TODO to DONE."
  (let ((file (meeting--occurrence-file 'standup)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (let ((escaped-text (regexp-quote item-text)))
            (when (re-search-forward
                   (format "^\\(\\*\\*\\) TODO\\s-+%s" escaped-text)
                   nil t)
              (replace-match (format "%s DONE %s" (match-string 1) item-text))
              (save-buffer)
              t)))))))

;; Keep old name for compatibility
(defalias 'meeting--mark-item-done 'meeting--mark-standup-item-done)

(defun meeting--magit-post-commit-hook ()
  "After commit, offer to mark standup items done.
Only runs if `meeting-magit-mark-done-enabled' is non-nil."
  (when (and meeting-magit-mark-done-enabled
             (meeting--occurrence-exists-p 'standup))
    (let ((todos (meeting--get-standup-todos)))
      (when todos
        (let* ((choices (mapcar #'cdr todos))
               (selected (completing-read-multiple
                          "Mark DONE (comma-sep, empty to skip): "
                          choices)))
          (dolist (item selected)
            (when (meeting--mark-standup-item-done item)
              (message "Marked done: %s" item))))))))

(with-eval-after-load 'magit
  (add-hook 'git-commit-post-finish-hook #'meeting--magit-post-commit-hook))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Org-Agenda Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun meeting--standup-agenda-files ()
  "Return list of recent standup files for org-agenda."
  (let ((dir (meeting-get 'standup :directory)))
    (when (file-directory-p dir)
      ;; Get last 7 days of standups
      (let ((files (directory-files dir t "\\.org$" t)))
        (seq-take (sort files #'string>) 7)))))

;;;###autoload
(defun meeting-standup-agenda ()
  "Show org-agenda view of standup TODOs."
  (interactive)
  (let ((org-agenda-files (meeting--standup-agenda-files)))
    (if (not org-agenda-files)
        (message "No standup files found")
      (org-agenda nil "t"))))  ; "t" = TODO list

;; Add standup directory to org-agenda-files when loaded
(with-eval-after-load 'org-agenda
  (let ((standup-dir (expand-file-name "standup" meeting-directory)))
    (when (file-directory-p standup-dir)
      (add-to-list 'org-agenda-files standup-dir t))))

;;;###autoload
(defun meeting-toggle-magit-integration ()
  "Toggle magit commit -> standup integration.
When enabled, you'll be prompted to mark standup items done after each commit."
  (interactive)
  (setq meeting-magit-mark-done-enabled (not meeting-magit-mark-done-enabled))
  (message "Magit standup integration %s"
           (if meeting-magit-mark-done-enabled "enabled" "disabled")))

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
      ("T" "Add to today" meeting-add-to-today)
      ("c" "Carryover" meeting-carryover)
      ("C" "Carryover (incomplete)" meeting-carryover-incomplete)]
     ["Export"
      ("e" "Export" meeting-export)]]
    ["Standup (pure org-mode)"
     [("s s" "Open standup" meeting-standup)
      ("s t" "Add TODO" meeting-standup-add)
      ("s d" "Mark done" meeting-standup-mark-done)
      ("s +" "Add DONE" meeting-standup-add-done)]
     [("s b" "Add blocked" meeting-standup-add-blocked)
      ("s q" "Add question" meeting-standup-add-question)
      ("s a" "Add agenda" meeting-standup-add-agenda)]
     [("s c" "Carryover TODOs" meeting-standup-carryover)
      ("s e" "Export" meeting-standup-export)
      ("s p" "Post to Slack" meeting-standup-post-slack)]]
    ["Standup - Integrations"
     [("s A" "Org agenda" meeting-standup-agenda)
      ("s D" "Share w/ Daly" meeting-standup-share-daly)
      ("s S" "Share w/ User" meeting-standup-share-with-user)]
     [("s F" "Create Feature" meeting-standup-create-feature)
      ("s j" "Jump to Branch" meeting-standup-jump-to-branch)
      ("s m" "Toggle Magit" meeting-toggle-magit-integration)]])
  ;; Set keybinding after transient is defined
  (global-set-key (kbd "C-c M ?") #'meeting-transient))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Main prefix: C-c M
(global-set-key (kbd "C-c M o") #'meeting-open)
(global-set-key (kbd "C-c M t") #'meeting-open-today)
(global-set-key (kbd "C-c M T") #'meeting-add-to-today)  ; Pre-fill today's agenda
(global-set-key (kbd "C-c M n") #'meeting-add-to-next)
(global-set-key (kbd "C-c M c") #'meeting-carryover)
(global-set-key (kbd "C-c M e") #'meeting-export)
;; C-c M ? binding set in with-eval-after-load transient block above

;; Standup shortcuts: C-c M s (pure org-mode structure)
(global-set-key (kbd "C-c M s s") #'meeting-standup)           ; Open today's standup
(global-set-key (kbd "C-c M s t") #'meeting-standup-add)       ; Add TODO item
(global-set-key (kbd "C-c M s d") #'meeting-standup-mark-done) ; Mark item done (picker)
(global-set-key (kbd "C-c M s +") #'meeting-standup-add-done)  ; Add DONE item (retroactive)
(global-set-key (kbd "C-c M s b") #'meeting-standup-add-blocked)
(global-set-key (kbd "C-c M s q") #'meeting-standup-add-question)
(global-set-key (kbd "C-c M s a") #'meeting-standup-add-agenda)
(global-set-key (kbd "C-c M s c") #'meeting-standup-carryover) ; Carryover TODOs
(global-set-key (kbd "C-c M s e") #'meeting-standup-export)
(global-set-key (kbd "C-c M s p") #'meeting-standup-post-slack)

;; Standup integrations: C-c M s (continued)
(global-set-key (kbd "C-c M s A") #'meeting-standup-agenda)  ; org-agenda view
(global-set-key (kbd "C-c M s D") #'meeting-standup-share-daly)
(global-set-key (kbd "C-c M s S") #'meeting-standup-share-with-user)
(global-set-key (kbd "C-c M s F") #'meeting-standup-create-feature)
(global-set-key (kbd "C-c M s j") #'meeting-standup-jump-to-branch)
(global-set-key (kbd "C-c M s m") #'meeting-toggle-magit-integration)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Default Meeting Types
;;; ════════════════════════════════════════════════════════════════════════════

;; Standup - daily team sync (pure org-mode TODO structure)
(meeting-define 'standup
  :name "Standup"
  :calendar-match "standup\\|daily\\|stand-up"
  :fallback-time "08:00"
  :fallback-days '(1 2 3 4 5)  ; Mon-Fri
  :sections nil)  ; No sections - pure TODO items with tags

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
