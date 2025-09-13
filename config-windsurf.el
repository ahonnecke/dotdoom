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

(defun windsurf-open-folder ()
  "Prompt for a folder path (defaulting to `default-directory`) and open it in Windsurf."
  (interactive)
  (let* ((folder (read-directory-name "Open folder in Windsurf: " default-directory))
         (windsurf-command (concat "windsurf --reuse-window " (shell-quote-argument folder))))
    (message "Running: %s" windsurf-command)
    (shell-command windsurf-command)))
