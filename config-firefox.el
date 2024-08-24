;;; config-firefox.el -*- lexical-binding: t; -*-

(defun open-current-file-in-firefox ()
  "Open the current file in Visual Studio Code."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (concat "firefox " (shell-quote-argument file))))))

(define-key ashton-mode-map (kbd "C-c g f") 'open-current-file-in-firefox)
