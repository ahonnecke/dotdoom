;;; ../src/home/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun vscode-open-filepath ()
  "Open the selected region in vscode."
  (interactive)
  (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
         (firefox-command (concat "code -r " region)))
    (shell-command firefox-command)))
