;;; ~/.doom.d/config-standup.el -*- lexical-binding: t; -*-

;; Standup Mode - Daily standup tracking that integrates with org-mode
;; Designed for capturing work items throughout the day and reviewing at standup

(require 'org)

;;; Configuration

(defgroup standup nil
  "Standup tracking configuration."
  :group 'org
  :prefix "standup-")

(defcustom standup-file "/home/ahonnecke/Documents/crew_capable/STANDUP.org"
  "Path to the standup org file."
  :type 'file
  :group 'standup)

(defcustom standup-sections '("Done" "Doing" "Blockers")
  "Sections to create for each day's standup entry."
  :type '(repeat string)
  :group 'standup)

(defcustom standup-timestamp-format "%a %d %b %Y %I:%M:%S %p %Z"
  "Format for standup day headers. Default matches: Mon 01 Dec 2025 08:10:48 AM MST"
  :type 'string
  :group 'standup)

;;; Helper Functions

(defun standup--get-today-timestamp ()
  "Return today's timestamp string for standup header."
  (format-time-string standup-timestamp-format))

(defun standup--get-today-date-pattern ()
  "Return regex pattern to match today's date (ignoring time)."
  (format-time-string "%a %d %b %Y"))

(defun standup--get-yesterday-date-pattern ()
  "Return regex pattern to match yesterday's date."
  (format-time-string "%a %d %b %Y" (time-subtract (current-time) (days-to-time 1))))

(defun standup--get-tomorrow-date-pattern ()
  "Return regex pattern to match tomorrow's date."
  (format-time-string "%a %d %b %Y" (time-add (current-time) (days-to-time 1))))

(defun standup--get-tomorrow-timestamp ()
  "Return tomorrow's timestamp string for standup header."
  (format-time-string standup-timestamp-format (time-add (current-time) (days-to-time 1))))

(defun standup--ensure-file-open ()
  "Ensure standup file is open, return the buffer."
  (or (find-buffer-visiting standup-file)
      (find-file-noselect standup-file)))

(defun standup--goto-or-create-today ()
  "Go to today's entry, creating it if it doesn't exist.
Returns point at the beginning of today's entry."
  (let ((buf (standup--ensure-file-open))
        (today-pattern (standup--get-today-date-pattern)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " today-pattern) nil t)
          (beginning-of-line)
        ;; Create today's entry
        (standup--create-day-entry)
        (goto-char (point-min))
        (re-search-forward (concat "^\\* " today-pattern) nil t)
        (beginning-of-line))
      (point))))

(defun standup--create-day-entry (&optional timestamp)
  "Create a new day entry with template sections at the top of the file.
If TIMESTAMP is provided, use it; otherwise use today's timestamp."
  (let ((buf (standup--ensure-file-open))
        (ts (or timestamp (standup--get-today-timestamp))))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Skip past #+title: line if present
      (when (looking-at "^#\\+")
        (forward-line)
        (while (looking-at "^#\\+")
          (forward-line))
        ;; Skip blank line after headers
        (when (looking-at "^$")
          (forward-line)))
      ;; Insert new day entry
      (insert (format "* %s\n" ts))
      (dolist (section standup-sections)
        (insert (format "** %s\n" section)))
      (insert "\n")
      (save-buffer))))

(defun standup--goto-section (section-name)
  "Go to SECTION-NAME under today's entry, creating if needed.
Returns point at end of section header line."
  (let ((buf (standup--ensure-file-open)))
    (with-current-buffer buf
      (standup--goto-or-create-today)
      (let ((today-pattern (standup--get-today-date-pattern))
            (section-point nil))
        ;; Find the section under today
        (save-excursion
          (when (re-search-forward (concat "^\\* " today-pattern) nil t)
            (let ((day-end (save-excursion
                            (if (re-search-forward "^\\* " nil t)
                                (line-beginning-position)
                              (point-max)))))
              (if (re-search-forward (concat "^\\*\\* " section-name) day-end t)
                  (setq section-point (point))
                ;; Section doesn't exist, create it
                (goto-char day-end)
                (forward-line -1)
                (end-of-line)
                (insert (format "\n** %s" section-name))
                (setq section-point (point))))))
        (when section-point
          (goto-char section-point))
        section-point))))

(defun standup--add-item-to-section (section-name item-text)
  "Add ITEM-TEXT to SECTION-NAME under today's entry."
  (let ((buf (standup--ensure-file-open)))
    (with-current-buffer buf
      (standup--goto-section section-name)
      ;; Find end of section (next ** or * or end of file)
      (let ((section-end (save-excursion
                          (if (re-search-forward "^\\*\\*? " nil t)
                              (line-beginning-position)
                            (point-max)))))
        (goto-char section-end)
        ;; Back up past any blank lines
        (skip-chars-backward "\n\t ")
        (end-of-line)
        (insert (format "\n*** %s" item-text))
        (save-buffer)))))

(defun standup--goto-or-create-tomorrow ()
  "Go to tomorrow's entry, creating it if it doesn't exist.
Returns point at the beginning of tomorrow's entry."
  (let ((buf (standup--ensure-file-open))
        (tomorrow-pattern (standup--get-tomorrow-date-pattern)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " tomorrow-pattern) nil t)
          (beginning-of-line)
        ;; Create tomorrow's entry
        (standup--create-day-entry (standup--get-tomorrow-timestamp))
        (goto-char (point-min))
        (re-search-forward (concat "^\\* " tomorrow-pattern) nil t)
        (beginning-of-line))
      (point))))

(defun standup--goto-section-for-date (section-name date-pattern)
  "Go to SECTION-NAME under the entry matching DATE-PATTERN, creating if needed.
Returns point at end of section header line."
  (let ((buf (standup--ensure-file-open)))
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((section-point nil))
        ;; Find the section under the date
        (save-excursion
          (when (re-search-forward (concat "^\\* " date-pattern) nil t)
            (let ((day-end (save-excursion
                            (if (re-search-forward "^\\* " nil t)
                                (line-beginning-position)
                              (point-max)))))
              (if (re-search-forward (concat "^\\*\\* " section-name) day-end t)
                  (setq section-point (point))
                ;; Section doesn't exist, create it
                (goto-char day-end)
                (forward-line -1)
                (end-of-line)
                (insert (format "\n** %s" section-name))
                (setq section-point (point))))))
        (when section-point
          (goto-char section-point))
        section-point))))

(defun standup--add-item-to-tomorrow (section-name item-text)
  "Add ITEM-TEXT to SECTION-NAME under tomorrow's entry."
  (let ((buf (standup--ensure-file-open))
        (tomorrow-pattern (standup--get-tomorrow-date-pattern)))
    (with-current-buffer buf
      ;; Ensure tomorrow's entry exists
      (standup--goto-or-create-tomorrow)
      ;; Go to the section
      (standup--goto-section-for-date section-name tomorrow-pattern)
      ;; Find end of section (next ** or * or end of file)
      (let ((section-end (save-excursion
                          (if (re-search-forward "^\\*\\*? " nil t)
                              (line-beginning-position)
                            (point-max)))))
        (goto-char section-end)
        ;; Back up past any blank lines
        (skip-chars-backward "\n\t ")
        (end-of-line)
        (insert (format "\n*** %s" item-text))
        (save-buffer)))))

;;; Interactive Commands

;;;###autoload
(defun standup ()
  "Open standup file and jump to today's entry (create if needed)."
  (interactive)
  (find-file standup-file)
  (standup--goto-or-create-today)
  (recenter 0))

;;;###autoload
(defun standup-done (item)
  "Add ITEM to today's Done section. Works from any buffer.
If region is active, use region text. Otherwise prompt."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Done: "))))
  (when (and item (not (string-empty-p item)))
    (standup--add-item-to-section "Done" item)
    (message "Added to Done: %s" item)))

;;;###autoload
(defun standup-doing (item)
  "Add ITEM to today's Doing section. Works from any buffer.
If region is active, use region text. Otherwise prompt."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Doing: "))))
  (when (and item (not (string-empty-p item)))
    (standup--add-item-to-section "Doing" item)
    (message "Added to Doing: %s" item)))

;;;###autoload
(defun standup-blocker (item)
  "Add ITEM to today's Blockers section. Works from any buffer.
If region is active, use region text. Otherwise prompt."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Blocker: "))))
  (when (and item (not (string-empty-p item)))
    (standup--add-item-to-section "Blockers" item)
    (message "Added to Blockers: %s" item)))

;;;###autoload
(defun standup-agenda (item)
  "Add ITEM to tomorrow's Doing section. Works from any buffer.
Use this to add agenda items for tomorrow's standup.
If region is active, use region text. Otherwise prompt."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Tomorrow: "))))
  (when (and item (not (string-empty-p item)))
    (standup--add-item-to-tomorrow "Doing" item)
    (message "Added to tomorrow's Doing: %s" item)))

;;;###autoload
(defun standup-tomorrow ()
  "Jump to tomorrow's standup entry (create if needed)."
  (interactive)
  (find-file standup-file)
  (standup--goto-or-create-tomorrow)
  (recenter 0))

;;;###autoload
(defun standup-new-day ()
  "Create a new day entry with template sections."
  (interactive)
  (let ((today-pattern (standup--get-today-date-pattern))
        (buf (standup--ensure-file-open)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " today-pattern) nil t)
          (message "Today's entry already exists")
        (standup--create-day-entry)
        (message "Created new standup entry for today")))
    (standup)))

;;;###autoload
(defun standup-today ()
  "Jump to today's standup entry."
  (interactive)
  (standup))

;;;###autoload
(defun standup-yesterday ()
  "Jump to yesterday's standup entry."
  (interactive)
  (find-file standup-file)
  (goto-char (point-min))
  (let ((yesterday-pattern (standup--get-yesterday-date-pattern)))
    (if (re-search-forward (concat "^\\* " yesterday-pattern) nil t)
        (progn
          (beginning-of-line)
          (recenter 0))
      (message "No entry found for yesterday"))))

;;;###autoload
(defun standup-export ()
  "Export today's standup to kill ring in clean format for Slack/email."
  (interactive)
  (let ((buf (standup--ensure-file-open))
        (today-pattern (standup--get-today-date-pattern))
        (export-text ""))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^\\* " today-pattern) nil t)
          (let ((day-start (line-beginning-position))
                (day-end (save-excursion
                          (if (re-search-forward "^\\* " nil t)
                              (line-beginning-position)
                            (point-max)))))
            (goto-char day-start)
            (forward-line) ;; Skip the date header
            (while (< (point) day-end)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (cond
                 ;; Section header
                 ((string-match "^\\*\\* \\(.+\\)" line)
                  (setq export-text
                        (concat export-text
                                (if (string-empty-p export-text) "" "\n")
                                (match-string 1 line) ":\n")))
                 ;; Item
                 ((string-match "^\\*\\*\\* \\(.+\\)" line)
                  (setq export-text
                        (concat export-text "- " (match-string 1 line) "\n")))
                 ;; Sub-item
                 ((string-match "^\\*\\*\\*\\* \\(.+\\)" line)
                  (setq export-text
                        (concat export-text "  - " (match-string 1 line) "\n")))))
              (forward-line))))))
    (if (string-empty-p export-text)
        (message "No standup entry found for today")
      (kill-new export-text)
      (message "Standup copied to clipboard:\n%s" export-text))))

;;;###autoload
(defun standup-move-to-done ()
  "Move the current item from Doing to Done section."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (string= (buffer-file-name) (expand-file-name standup-file)))
    (let ((item-text nil))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "^\\*\\*\\* \\(.+\\)")
          (setq item-text (match-string 1))
          (kill-whole-line)))
      (when item-text
        (standup--add-item-to-section "Done" item-text)
        (message "Moved to Done: %s" item-text)))))

;;;###autoload
(defun standup-post-to-slack ()
  "Export today's standup and post to Slack channel.
Requires emacs-slack to be configured."
  (interactive)
  (if (and (fboundp 'slack-channel-select)
           (boundp 'slack-teams)
           slack-teams)
      (let ((export-text nil))
        ;; Get the export text
        (standup-export)
        (setq export-text (current-kill 0))
        (if (and export-text (not (string-empty-p export-text)))
            ;; Post to slack
            (slack-channel-select
             :on-select (lambda (room team)
                          (slack-message-send-internal
                           export-text room team)
                          (message "Standup posted to #%s" (slack-room-name room team))))
          (message "No standup to post")))
    (message "Slack not configured. Run M-x slack-start first")))

;;;###autoload
(defun standup-carryover ()
  "Copy unfinished Doing items from yesterday to today's Doing section."
  (interactive)
  (let ((buf (standup--ensure-file-open))
        (yesterday-pattern (standup--get-yesterday-date-pattern))
        (items '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^\\* " yesterday-pattern) nil t)
          (let ((day-end (save-excursion
                          (if (re-search-forward "^\\* " nil t)
                              (line-beginning-position)
                            (point-max)))))
            (when (re-search-forward "^\\*\\* Doing" day-end t)
              (let ((section-end (save-excursion
                                  (if (re-search-forward "^\\*\\* " day-end t)
                                      (line-beginning-position)
                                    day-end))))
                (while (re-search-forward "^\\*\\*\\* \\(.+\\)" section-end t)
                  (push (match-string 1) items))))))))
    (if items
        (progn
          (dolist (item (reverse items))
            (standup--add-item-to-section "Doing" item))
          (message "Carried over %d items from yesterday" (length items)))
      (message "No items to carry over from yesterday"))))

;;; Minor Mode (for standup file specific behaviors)

(defvar standup-mode-map
  (let ((map (make-sparse-keymap)))
    ;; M-s prefix for all standup commands (overrides swiper in this buffer only)
    (define-key map (kbd "M-s d") 'standup-done)
    (define-key map (kbd "M-s g") 'standup-doing)
    (define-key map (kbd "M-s b") 'standup-blocker)
    (define-key map (kbd "M-s a") 'standup-agenda)      ; add to tomorrow
    (define-key map (kbd "M-s T") 'standup-tomorrow)    ; jump to tomorrow
    (define-key map (kbd "M-s e") 'standup-export)
    (define-key map (kbd "M-s y") 'standup-yesterday)
    (define-key map (kbd "M-s c") 'standup-carryover)
    (define-key map (kbd "M-s m") 'standup-move-to-done)
    (define-key map (kbd "M-s n") 'standup-new-day)
    (define-key map (kbd "M-s t") 'standup-today)
    (define-key map (kbd "M-s p") 'standup-post-to-slack)
    map)
  "Keymap for standup-mode.")

(define-minor-mode standup-mode
  "Minor mode for standup file specific behaviors."
  :lighter " Standup"
  :keymap standup-mode-map
  :group 'standup)

;; Auto-enable standup-mode for the standup file
(defun standup--maybe-enable-mode ()
  "Enable standup-mode if visiting the standup file."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name standup-file)))
    (standup-mode 1)))

(add-hook 'org-mode-hook 'standup--maybe-enable-mode)

(provide 'config-standup)
;;; config-standup.el ends here
