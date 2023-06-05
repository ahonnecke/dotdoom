;;; ../src/home/.doom.d/init-ffap.el -*- lexical-binding: t; -*-

(defun find-file-at-point-with-line (&optional filename)
  "Opens file at point and moves point to line specified next to file name."
  (interactive)
  (let* ((filename (or filename (if current-prefix-arg (ffap-prompter) (ffap-guesser))))
         (line-number
          (and (or (looking-at ".* line \\(\[0-9\]+\\)")
                   (looking-at "[^:]*:\\(\[0-9\]+\\)"))
               (string-to-number (match-string-no-properties 1))))
         (column-number
          (or
           (and (looking-at "[^:]*:\[0-9\]+:\\(\[0-9\]+\\)")
                (string-to-number (match-string-no-properties 1)))
           (let 'column-number 0))))

    (let ((real (car (s-split "\\:" filename))))
          (message "%s --> %s:%s" real line-number column-number)
    (cond ((ffap-url-p real)
           (let (current-prefix-arg)
             (funcall ffap-url-fetcher real)))
          ((and line-number
                (file-exists-p real))
           (progn (find-file-other-window real)
                  ;; goto-line is for interactive use
                  (goto-char (point-min))
                  (forward-line (1- line-number))
                  (forward-char column-number)))
          ((and ffap-pass-wildcards-to-dired
                ffap-dired-wildcards
                (string-match ffap-dired-wildcards real))
           (funcall ffap-directory-finder real))
          ((and ffap-dired-wildcards
                (string-match ffap-dired-wildcards real)
                find-file-wildcards
                ;; Check if it's find-file that supports wildcards arg
                (memq ffap-file-finder '(find-file find-alternate-file)))
           (funcall ffap-file-finder (expand-file-name real) t))
          ((or (not ffap-newfile-prompt)
               (file-exists-p real)
               (y-or-n-p "File does not exist, create buffer? "))
           (funcall ffap-file-finder
                    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
                    (expand-file-name real)))
          ;; User does not want to find a non-existent file:
          ((signal 'file-error (list "Opening file buffer"
                                     "no such file or directory"
                                     real))))
      )
))

;; Move to magit?
(add-hook 'magit-process-mode-hook
          #'(lambda ()
             (local-set-key (kbd "<return>") 'find-file-at-point-with-line)
             ))
