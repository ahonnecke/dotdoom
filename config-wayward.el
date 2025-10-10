;;; config-wayward.el -*- lexical-binding: t; -*-

(defun my/open-todays-image-dir ()
  "Open dired in ~/Downloads/images/YYYY-MM-DD/ where the date is today."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (path (expand-file-name (concat "~/Downloads/images/" today "/"))))
    (unless (file-directory-p path)
      (make-directory path t))
    (dired path)))
