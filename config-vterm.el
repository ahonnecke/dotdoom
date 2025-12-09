;;; ../src/home/.doom.d/config-vterm.el -*- lexical-binding: t; -*-

;;vterm or something overwrites the bindings
(map! :after vterm :map vterm-mode-map "M-r" #'consult-recent-file)

;;;###autoload
(defun +vterm/open-here-or-fallback (arg)
  "Open a terminal buffer in the current context.

If in a Dired buffer, open the terminal in the Dired directory.
If in a file buffer, open the terminal in the file's directory.
Otherwise, fall back to the project root.

With prefix ARG, always use the project root.

Returns the vterm buffer."
  (interactive "P")
  (let ((target-dir
         (cond
          ((and (derived-mode-p 'dired-mode)
                (dired-current-directory)))
          ((buffer-file-name)
           (file-name-directory (buffer-file-name)))
          (t (or (doom-project-root) default-directory)))))
    (unless target-dir
      (user-error "Unable to determine a suitable directory for the terminal"))
    (+vterm--configure-project-root-and-display
     arg
     (lambda ()
       (require 'vterm)
       ;; HACK: Forces vterm to redraw, fixing strange artifacting in the tty.
       (save-window-excursion
         (pop-to-buffer "*scratch*"))
       (let ((vterm-buffer-name (format "*vterm:%s*" target-dir))
             display-buffer-alist)
         (with-temp-buffer
           (cd target-dir))
         (vterm vterm-buffer-name))))))

(defun +vterm--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.

If prefix ARG is non-nil, cd into `default-directory` instead of the project root.

Returns the vterm buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
          (if arg
              default-directory
            project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

;; C-c v e = vterm here (in current directory)
(global-set-key (kbd "C-c v e") '+vterm/open-here-or-fallback)

;; Move this

(add-hook 'text-mode-hook 'turn-on-auto-fill)

