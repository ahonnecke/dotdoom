;;; region-nudge.el --- transient region/line nudging -*- lexical-binding: t; -*-

(defvar region-nudge--map
  (let ((m (make-sparse-keymap)))
    ;; Horizontal
    (define-key m (kbd "h")   #'region-nudge-left)
    (define-key m (kbd ";")   #'region-nudge-right)
    (define-key m (kbd "C-h") #'region-nudge-left-tabstop)
    (define-key m (kbd "C-;") #'region-nudge-right-tabstop)
    ;; Vertical
    (define-key m (kbd "k")   #'region-nudge-down)
    (define-key m (kbd "l")   #'region-nudge-up)
    ;; Exit
    (define-key m (kbd "RET") #'region-nudge-exit)
    (define-key m (kbd "C-g") #'region-nudge-exit)
    m)
  "Keymap for `region-nudge-mode'.")

(define-minor-mode region-nudge-mode
  "Minor mode for nudging region/line around.
\\{region-nudge--map}"
  :lighter " ⬌"
  :keymap region-nudge--map)

(defun region-nudge-exit ()
  "Exit `region-nudge-mode'."
  (interactive)
  (region-nudge-mode -1)
  (message "Exited nudge mode"))

(defun region-nudge--bounds ()
  "Return (BEG END) spanning whole lines if region active, else current line."
  (if (use-region-p)
      (list (save-excursion (goto-char (region-beginning)) (line-beginning-position))
            (save-excursion (goto-char (region-end))
                            (if (bolp) (point) (line-end-position))))
    (list (line-beginning-position) (line-end-position))))

(defun region-nudge--keep-active (beg end)
  (when (use-region-p)
    (goto-char beg)
    (set-mark end)
    (activate-mark)))

(defun region-nudge--indent (cols)
  (cl-destructuring-bind (beg end) (region-nudge--bounds)
    (indent-rigidly beg end cols)
    (region-nudge--keep-active beg end)))

(defun region-nudge-left ()  (interactive) (region-nudge--indent -1))
(defun region-nudge-right () (interactive) (region-nudge--indent 1))

(defun region-nudge-left-tabstop () (interactive)
       (cl-destructuring-bind (beg end) (region-nudge--bounds)
         (let* ((tw tab-width)
                (col (save-excursion (goto-char beg) (current-indentation)))
                (delta (if (zerop (mod col tw)) tw (mod col tw))))
           (indent-rigidly beg end (- delta)))
         (region-nudge--keep-active beg end)))

(defun region-nudge-right-tabstop () (interactive)
       (cl-destructuring-bind (beg end) (region-nudge--bounds)
         (let* ((tw tab-width)
                (col (save-excursion (goto-char beg) (current-indentation)))
                (rem (mod col tw))
                (delta (if (zerop rem) tw (- tw rem))))
           (indent-rigidly beg end delta))
         (region-nudge--keep-active beg end)))

(defun region-nudge--move-block (dir)
  "Move region/line up/down by one line. DIR = +1 down, -1 up."
  (cl-destructuring-bind (beg end) (region-nudge--bounds)
    (let ((text (delete-and-extract-region beg end)))
      (forward-line dir)
      (let ((pt (point)))
        (insert text)
        (let ((newbeg pt)
              (newend (point)))
          (region-nudge--keep-active newbeg newend))))))

(defun region-nudge-up ()   (interactive) (region-nudge--move-block -1))
(defun region-nudge-down () (interactive) (region-nudge--move-block +1))

;;;###autoload
(defun region-nudge ()
  "Activate region-nudge-mode transiently."
  (interactive)
  (region-nudge-mode 1)
  (message "Nudge mode: h/; left/right, C-h/C-; tabstops, k down, l up. RET or C-g to exit."))

;; Entry binding
(global-set-key (kbd "C-c TAB") #'region-nudge)

(provide 'region-nudge)
;;; region-nudge.el ends here
