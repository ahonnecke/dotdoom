(defun open-repo-root-in-windsurf ()
  "Open the root of the current Git repository in Windsurf, reusing the existing window."
  (interactive)
  (let ((repo-root (magit-toplevel))) ;; Get the repo root using Magit
    (if repo-root
        (progn
          (shell-command (concat "windsurf --reuse-window " (shell-quote-argument repo-root)))
          (message "Opened repo root in Windsurf: %s" repo-root))
      (message "Not in a Git repository!"))))

;; Add the keybinding to ashton-mode
(with-eval-after-load 'ashton-mode
  (define-key ashton-mode-map (kbd "C-c w") #'open-repo-root-in-windsurf))
