;;; config-wayward.el -*- lexical-binding: t; -*-
;;; open-dated-media.el --- Open today's dated media folders  -*- lexical-binding: t; -*-

;; Commands:
;;   M-x open-todays-images
;;   M-x open-todays-screenshots
;;   M-x open-todays-media-chooser  ;; quick picker between the two
;;
;; Optional keybindings:
;; (global-set-key (kbd "C-c i") #'open-todays-images)
;; (global-set-key (kbd "C-c s") #'open-todays-screenshots)
;; (global-set-key (kbd "C-c m") #'open-todays-media-chooser)

(require 'subr-x)

(defgroup open-dated-media nil
  "Open today’s YYYY-MM-DD subfolders for images/screenshots."
  :group 'files)

(defcustom odm-images-root (expand-file-name "~/Downloads/images")
  "Root folder containing date-based image subdirectories."
  :type 'directory)

(defcustom odm-screenshots-root (expand-file-name "~/screenshots")
  "Root folder containing date-based screenshot subdirectories."
  :type 'directory)

(defcustom odm-date-format "%Y-%m-%d"
  "Date format used for daily subfolder names."
  :type 'string)

(defun odm--today-subdir (root)
  "Return today’s subdir under ROOT, creating it if needed.
Ensures ROOT exists and then ROOT/$(date +FORMAT)."
  (let* ((root* (file-name-as-directory (expand-file-name root)))
         (today (format-time-string odm-date-format))
         (dir   (expand-file-name today root*)))
    (unless (file-directory-p root*)
      (make-directory root* t))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;;###autoload
(defun open-todays-images ()
  "Open today’s images folder (odm-images-root/YYYY-MM-DD/) in Dired."
  (interactive)
  (let ((dir (odm--today-subdir odm-images-root)))
    (message "Opening %s" dir)
    (dired dir)))

;;;###autoload
(defun open-todays-screenshots ()
  "Open today’s screenshots folder (odm-screenshots-root/YYYY-MM-DD/) in Dired."
  (interactive)
  (let ((dir (odm--today-subdir odm-screenshots-root)))
    (message "Opening %s" dir)
    (dired dir)))

;;;###autoload
(defun open-todays-media-chooser ()
  "Pick which today folder to open: Images or Screenshots."
  (interactive)
  (pcase (completing-read "Open today’s: " '("Images" "Screenshots") nil t)
    ("Images"      (open-todays-images))
    ("Screenshots" (open-todays-screenshots))))

(provide 'open-dated-media)
;;; config-wayward.el ends here
