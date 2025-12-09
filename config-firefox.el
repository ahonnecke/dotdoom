;;; config-firefox.el -*- lexical-binding: t; -*-

(defun open-current-file-in-firefox ()
  "Open the current file in Firefox browser."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (concat "firefox " (shell-quote-argument file))))))

;; Use embark for this instead: C-. on file → open with browser
;; Or use C-c g b (browse-url-at-point) for URLs
;; Removed C-c g f conflict - keeping find-file-at-point-with-line there
