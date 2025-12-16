;;; aws-s3-ext.el --- S3 browsing extension for aws.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025, Ashton Honnecke

;;; Commentary:
;; Adds dired-like browsing to aws.el S3 mode.
;; RET enters buckets/directories, ^ goes up, o opens files.

;;; Code:
(require 'json)
(require 'transient)

(declare-function aws-cmd "aws")
(declare-function aws--pop-to-buffer "aws")
(declare-function aws--buffer-name "aws")
(declare-function aws-set-profile "aws")
(declare-function aws "aws")
(declare-function aws-s3 "aws-s3")

(defvar aws-profile)
(defvar aws-s3-mode-map)

;;; ════════════════════════════════════════════════════════════════════════════
;;; State
;;; ════════════════════════════════════════════════════════════════════════════

(defvar-local aws-s3-ext--current-bucket nil
  "Current bucket being browsed.")

(defvar-local aws-s3-ext--current-prefix nil
  "Current prefix (directory) within bucket.")

(defvar-local aws-s3-ext--history nil
  "Navigation history for going back.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-s3-ext-enter ()
  "Enter bucket or directory under cursor, or open file."
  (interactive)
  (let ((item (tabulated-list-get-id)))
    (cond
     ((or (null item) (string-empty-p item))
      (message "No item under cursor"))
     ;; In bucket list - enter the bucket
     ((null aws-s3-ext--current-bucket)
      (aws-s3-ext--enter-bucket item))
     ;; Directory (ends with /) - navigate into it
     ((string-suffix-p "/" item)
      (aws-s3-ext--enter-prefix item))
     ;; File - open it
     (t
      (aws-s3-ext--open-file item)))))

(defun aws-s3-ext--enter-bucket (bucket)
  "Enter BUCKET and list its contents."
  (push (list aws-s3-ext--current-bucket aws-s3-ext--current-prefix) aws-s3-ext--history)
  (setq aws-s3-ext--current-bucket bucket)
  (setq aws-s3-ext--current-prefix nil)
  (aws-s3-ext--list-contents))

(defun aws-s3-ext--enter-prefix (prefix)
  "Navigate into PREFIX directory."
  (push (list aws-s3-ext--current-bucket aws-s3-ext--current-prefix) aws-s3-ext--history)
  (setq aws-s3-ext--current-prefix
        (concat (or aws-s3-ext--current-prefix "") prefix))
  (aws-s3-ext--list-contents))

(defun aws-s3-ext-up ()
  "Go up one level or back to bucket list."
  (interactive)
  (cond
   ;; Have history - pop it
   (aws-s3-ext--history
    (let ((prev (pop aws-s3-ext--history)))
      (setq aws-s3-ext--current-bucket (car prev))
      (setq aws-s3-ext--current-prefix (cadr prev))
      (if aws-s3-ext--current-bucket
          (aws-s3-ext--list-contents)
        (aws-s3-ext--back-to-buckets))))
   ;; In a prefix - go up one level
   (aws-s3-ext--current-prefix
    (let ((parts (split-string aws-s3-ext--current-prefix "/" t)))
      (if (> (length parts) 1)
          (progn
            (setq aws-s3-ext--current-prefix
                  (concat (string-join (butlast parts) "/") "/"))
            (aws-s3-ext--list-contents))
        (setq aws-s3-ext--current-prefix nil)
        (aws-s3-ext--list-contents))))
   ;; In bucket root - back to bucket list
   (aws-s3-ext--current-bucket
    (aws-s3-ext--back-to-buckets))
   ;; Already at bucket list
   (t
    (message "Already at bucket list"))))

(defun aws-s3-ext--back-to-buckets ()
  "Return to bucket listing."
  (setq aws-s3-ext--current-bucket nil)
  (setq aws-s3-ext--current-prefix nil)
  (setq aws-s3-ext--history nil)
  (aws-s3))

(defun aws-s3-ext--list-contents ()
  "List contents of current bucket/prefix."
  (let* ((s3-path (concat "s3://" aws-s3-ext--current-bucket "/"
                          (or aws-s3-ext--current-prefix "")))
         (cmd (concat (aws-cmd) "s3 ls " s3-path))
         (output (shell-command-to-string cmd))
         (lines (split-string output "\n" t))
         (rows (mapcar #'aws-s3-ext--parse-ls-line lines)))
    ;; Update buffer name to show location
    (rename-buffer (aws--buffer-name (concat "s3:" aws-s3-ext--current-bucket
                                              "/" (or aws-s3-ext--current-prefix ""))))
    (setq tabulated-list-format [("Name" 60) ("Size" 15) ("Date" 20)])
    (setq tabulated-list-entries (delq nil rows))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-s3-ext--parse-ls-line (line)
  "Parse a line from `aws s3 ls` output.
Returns (id [name size date]) or nil."
  (cond
   ;; Directory: PRE dirname/
   ((string-match "^\\s-*PRE \\(.+/\\)$" line)
    (let ((name (match-string 1 line)))
      (list name (vector name "DIR" ""))))
   ;; File: 2024-01-15 10:30:45 12345 filename
   ((string-match "^\\([0-9-]+\\)\\s-+\\([0-9:]+\\)\\s-+\\([0-9]+\\)\\s-+\\(.+\\)$" line)
    (let ((date (match-string 1 line))
          (time (match-string 2 line))
          (size (match-string 3 line))
          (name (match-string 4 line)))
      (list name (vector name (aws-s3-ext--human-size size) (concat date " " time)))))
   (t nil)))

(defun aws-s3-ext--human-size (size-str)
  "Convert SIZE-STR bytes to human readable."
  (let ((size (string-to-number size-str)))
    (cond
     ((>= size 1073741824) (format "%.1f GB" (/ size 1073741824.0)))
     ((>= size 1048576) (format "%.1f MB" (/ size 1048576.0)))
     ((>= size 1024) (format "%.1f KB" (/ size 1024.0)))
     (t (format "%d B" size)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; File Operations
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-s3-ext--open-file (filename)
  "Download and open FILENAME from current bucket/prefix."
  (let* ((s3-path (concat "s3://" aws-s3-ext--current-bucket "/"
                          (or aws-s3-ext--current-prefix "") filename))
         (local-path (expand-file-name filename temporary-file-directory))
         (cmd (concat (aws-cmd) "s3 cp " (shell-quote-argument s3-path) " "
                      (shell-quote-argument local-path))))
    (message "Downloading %s..." filename)
    (if (= 0 (call-process-shell-command cmd))
        (progn
          (find-file-other-window local-path)
          (message "Downloaded %s" filename))
      (message "Failed to download %s" filename))))

(defun aws-s3-ext-copy-path ()
  "Copy S3 path of item under cursor."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (path (if aws-s3-ext--current-bucket
                   (concat "s3://" aws-s3-ext--current-bucket "/"
                           (or aws-s3-ext--current-prefix "") item)
                 (concat "s3://" item "/"))))
    (kill-new path)
    (message "Copied: %s" path)))

(defun aws-s3-ext-refresh ()
  "Refresh current view."
  (interactive)
  (if aws-s3-ext--current-bucket
      (aws-s3-ext--list-contents)
    (aws-s3)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient & Keymap
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix aws-s3-ext-help-popup ()
  "AWS S3 Browser Help"
  ["Navigation"
   ("o" "Enter/Open (bucket, dir, or file)" aws-s3-ext-enter)
   ("^" "Up directory" aws-s3-ext-up)
   ("g" "Refresh" aws-s3-ext-refresh)]
  ["Actions"
   ("w" "Copy S3 path" aws-s3-ext-copy-path)
   ("P" "Set Profile" aws-set-profile)
   ("q" "Back to services" aws)])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Integration (advice to add keybindings to aws-s3-mode)
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-s3-ext--setup-keybindings ()
  "Setup keybindings for S3 browsing."
  ;; Use local-set-key to override any button bindings
  (local-set-key (kbd "RET") 'aws-s3-ext-enter)
  (local-set-key (kbd "<return>") 'aws-s3-ext-enter)
  (local-set-key (kbd "^") 'aws-s3-ext-up)
  (local-set-key (kbd "w") 'aws-s3-ext-copy-path)
  (local-set-key (kbd "o") 'aws-s3-ext-enter)
  (local-set-key (kbd "?") 'aws-s3-ext-help-popup))

;; Apply via hook when mode activates
(add-hook 'aws-s3-mode-hook #'aws-s3-ext--setup-keybindings)

(provide 'aws-s3-ext)
;;; aws-s3-ext.el ends here
