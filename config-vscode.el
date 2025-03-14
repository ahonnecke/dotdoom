;;; ../src/home/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun vscode-open-filepath ()
  "Open the selected region in vscode."
  (interactive)
  (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
         (vscode-command (concat "code -r " region)))
    (shell-command vscode-command)))

(defun open-current-file-in-vscode ()
  "Open the current file in Visual Studio Code."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (concat "code -r " (shell-quote-argument file))))))

