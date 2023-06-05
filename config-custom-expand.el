;;; ../src/home/.doom.d/init-custom-expand.el -*- lexical-binding: t; -*-


(setq dcsh-command-list '("cirrus", "ashton", "honnecke"))

(defun he-dcsh-command-beg ()
  (let ((p))
    (save-excursion
      (backward-word 1)
      (setq p (point)))
    p))

(defun try-expand-dcsh-command (old)
  (unless old
    (he-init-string (he-dcsh-command-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string (mapcar 'list dcsh-command-list))
                          'string-lessp)))
  (while (and he-expand-list
          (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    t))


(global-set-key (kbd "C-c e") (make-hippie-expand-function
                           '(try-expand-dcsh-command
                             try-expand-dabbrev-visible
                             try-expand-dabbrev
                             try-expand-dabbrev-all-buffers) t))
