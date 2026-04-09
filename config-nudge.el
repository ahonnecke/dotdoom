;;; region-nudge.el --- Vim-like nudging for regions/lines  -*- lexical-binding: t; -*-

(defvar region-nudge--map
  (let ((m (make-sparse-keymap)))
    ;; Horizontal (vim: h/l)
    (define-key m (kbd "h")   #'region-nudge-left)
    (define-key m (kbd "l")   #'region-nudge-right)
    ;; Tab-stop jumps on Meta-h / Meta-l
    (define-key m (kbd "M-h") #'region-nudge-left-tabstop)
    (define-key m (kbd "M-l") #'region-nudge-right-tabstop)
    ;; Vertical (vim: j/k)
    (define-key m (kbd "j")   #'region-nudge-down)
    (define-key m (kbd "k")   #'region-nudge-up)
    ;; Exit
    (define-key m (kbd "RET") #'region-nudge-exit)
    (define-key m (kbd "C-g") #'region-nudge-exit)
    m)
  "Keymap for `region-nudge-mode'.")

(define-minor-mode region-nudge-mode
  "Sticky nudge mode for moving/indenting region/line.
h/l = left/right, M-h/M-l = to tab stop, j/k = down/up.  RET or C-g exits."
  :lighter " ⬌"
  :keymap region-nudge--map)

;;; Entry
;;;###autoload
(defun region-nudge ()
  "Activate `region-nudge-mode' for nudging."
  (interactive)
  (region-nudge-mode 1)
  (message "Nudge: h/l left/right, M-h/M-l tabstops, j down, k up. RET or C-g to exit."))

(global-set-key (kbd "C-c TAB") #'region-nudge)

;;; Helpers
(defun region-nudge--bounds ()
  "Return (BEG END) spanning whole lines if region active; else current line."
  (if (use-region-p)
      (let ((rb (region-beginning))
            (re (region-end)))
        (list (save-excursion (goto-char rb) (line-beginning-position))
              (save-excursion (goto-char re)
                              (if (bolp) re (line-end-position)))))
    (list (line-beginning-position) (line-end-position))))

(defun region-nudge--keep-active (beg end)
  "Re-activate the region from BEG to END after a nudge operation."
  (when (use-region-p)
    (goto-char beg)
    (set-mark end)
    (activate-mark)))

(defun region-nudge--indent (cols)
  "Indent region or current line by COLS columns and keep region active."
  (cl-destructuring-bind (beg end) (region-nudge--bounds)
    (indent-rigidly beg end cols)
    (region-nudge--keep-active beg end)))

;;; Horizontal
(defun region-nudge-left ()
  "Nudge region or current line one column left."
  (interactive) (region-nudge--indent -1))
(defun region-nudge-right ()
  "Nudge region or current line one column right."
  (interactive) (region-nudge--indent  1))

(defun region-nudge-left-tabstop ()
  "Nudge region or current line left to the previous tab stop."
  (interactive)
       (cl-destructuring-bind (beg end) (region-nudge--bounds)
         (let* ((tw (or tab-width 8))
                (col (save-excursion (goto-char beg) (current-indentation)))
                (delta (if (zerop (mod col tw)) tw (mod col tw))))
           (indent-rigidly beg end (- delta))
           (region-nudge--keep-active beg end))))

(defun region-nudge-right-tabstop ()
  "Nudge region or current line right to the next tab stop."
  (interactive)
       (cl-destructuring-bind (beg end) (region-nudge--bounds)
         (let* ((tw (or tab-width 8))
                (col (save-excursion (goto-char beg) (current-indentation)))
                (rem (mod col tw))
                (delta (if (zerop rem) tw (- tw rem))))
           (indent-rigidly beg end delta)
           (region-nudge--keep-active beg end))))

;;; Vertical
(defun region-nudge--move-block (dir)
  "Move region/line by one line. DIR = +1 down, -1 up."
  (cl-destructuring-bind (beg end) (region-nudge--bounds)
    (let* ((text (delete-and-extract-region beg end))
           (inhibit-read-only t)
           (deactivate-mark nil))
      (goto-char beg)
      (cond
       ((and (= dir -1) (bobp))
        (insert text) (forward-line 0))
       ((= dir -1)
        (forward-line -1) (beginning-of-line) (insert text))
       ((= dir +1)
        (goto-char beg)
        (forward-line 1)
        (when (eobp) (insert "\n"))
        (beginning-of-line)
        (insert text)))
      (let ((nb (line-beginning-position))
            (ne (save-excursion
                  (search-forward text nil t)
                  (match-end 0))))
        (goto-char nb)
        (set-mark ne)
        (activate-mark)))))

(defun region-nudge-up ()
  "Move region or current line up one line."
  (interactive) (region-nudge--move-block -1))
(defun region-nudge-down ()
  "Move region or current line down one line."
  (interactive) (region-nudge--move-block +1))

;;; Exit
(defun region-nudge-exit ()
  "Deactivate `region-nudge-mode'."
  (interactive)
       (region-nudge-mode -1)
       (message "Exited nudge mode"))

(provide 'region-nudge)
;;; region-nudge.el ends here
