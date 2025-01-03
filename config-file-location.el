;;; ../src/home/.doom.d/config-file-location.el -*- lexical-binding: t; -*-

;; TODO: make this into a "relative to the project" function

(defun ash-copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(global-set-key (kbd "C-c C-a l") 'ash-copy-current-line-position-to-clipboard)

(defun copy-full-path-to-clipboard ()
  "Copy the full path of the current file to the clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new (file-truename buffer-file-name))
        (message "Copied file path to clipboard: %s" (file-truename buffer-file-name)))
    (message "Buffer is not visiting a file!")))

(defun copy-wrapped-full-path-to-clipboard ()
  "Copy the full path of the current file to the clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new (concat "`" (file-truename buffer-file-name) "`"))
        (message "Copied file path to clipboard: `%s`" (file-truename buffer-file-name)))
    (message "Buffer is not visiting a file!")))

(defun create-empty-file (filename)
  "Create a new empty file with the specified FILENAME in the current directory.
If the file already exists, it does nothing."
  (interactive "sEnter filename: ")
  (let ((full-path (expand-file-name filename default-directory)))
    (if (file-exists-p full-path)
        (message "File already exists: %s" full-path)
      (write-region "" nil full-path)
      (message "File created: %s" full-path))))

;; Bind to C-c f in Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c f") #'create-empty-file))
