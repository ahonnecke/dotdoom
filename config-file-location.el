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
