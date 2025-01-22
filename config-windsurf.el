(defun open-in-windsurf ()
  "Open Windsurf with the current file or the root of the Git repository."
  (interactive)
  (let ((target (or (buffer-file-name)
                    (magit-toplevel)))) ;; Get the current file or repo root
    (if target
        (progn
          (shell-command (concat "windsurf --reuse-window " (shell-quote-argument target)))
          (message "Opened in Windsurf: %s" target))
      (message "Not in a Git repository or visiting a file!"))))


;; Bind to ashton-mode-map
(define-key ashton-mode-map (kbd "C-c g w") #'open-in-windsurf)
