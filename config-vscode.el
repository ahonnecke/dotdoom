;;; ../src/home/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun open-in-vscode ()
  "Open Windsurf with the current file or the root of the Git repository."
  (interactive)
  (let* ((file (buffer-file-name))
         (root (magit-toplevel)))
    (when root
      (shell-command (concat "code -r " (shell-quote-argument root)))
      (when file
        (shell-command (concat "code -r " (shell-quote-argument file))))
      (message "Opened in Windsurf: %s" (if file file root)))
    (unless root
      (message "Not in a Git repository!"))))

(defun vscode-open-folder ()
  "Prompt for a folder path (defaulting to `default-directory`) and open it in VS Code."
  (interactive)
  (let* ((folder (read-directory-name "Open folder in VS Code: " default-directory))
         (vscode-command (concat "code -r \"" folder "\"")))
    (shell-command vscode-command)))
