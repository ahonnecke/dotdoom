;;; ../src/home/.doom.d/init-vterm-completion.el -*- lexical-binding: t; -*-

(use-package vterm
  :config
  (defun get-full-list ()
    (let ((program-list (split-string (shell-command-to-string "compgen -c") "\n" t ))
          (file-directory-list (split-string (shell-command-to-string "compgen -f") "\n" t ))
          (history-list (with-temp-buffer
                          (insert-file-contents "~/.bash_history")
                          (split-string (buffer-string) "\n" t))))

      (delete-dups (append program-list file-directory-list history-list))))

  (defun vterm-completion-choose-item ()
    (completing-read "Choose: " (get-full-list) nil nil (thing-at-point 'word 'no-properties)))

  (defun vterm-completion ()
    (interactive)
    (vterm-directory-sync)
    (let ((vterm-chosen-item (vterm-completion-choose-item)))
      (when (thing-at-point 'word)
        (vterm-send-meta-backspace))
      (vterm-send-string vterm-chosen-item)))

  (defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))

  :general
  (:states 'insert
           :keymaps 'vterm-mode-map
           "<tab>" 'vterm-completion))
