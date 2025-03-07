(defun open-in-windsurf ()
  "Open Windsurf with the current file or the root of the Git repository."
  (interactive)
  (let* ((file (buffer-file-name))
         (root (magit-toplevel)))
    (when root
      (shell-command (concat "windsurf --reuse-window " (shell-quote-argument root)))
      (when file
        (shell-command (concat "windsurf --reuse-window " (shell-quote-argument file))))
      (message "Opened in Windsurf: %s" (if file file root)))
    (unless root
      (message "Not in a Git repository!"))))


;; Bind to ashton-mode-map
(define-key ashton-mode-map (kbd "C-c g w") #'open-in-windsurf)
