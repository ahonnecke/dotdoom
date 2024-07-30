;;;  -*- lexical-binding: t; -*-


(electric-indent-mode -1)

;; (defun my-comment-region-lines (&optional beg end arg)
;;   "Like `comment-region' (which see), but comment or uncomment whole lines.
;; If the region isn't active, or there is no mark in the buffer, then
;; just comment the current line, or uncomment it if it is commented."
;;   (interactive (progn (barf-if-buffer-read-only)
;;                       (list (and (mark)  (region-beginning))
;;                             (and (mark)  (region-end))
;;                             current-prefix-arg)))
;;   (if (not (and beg  end  mark-active))
;;       (comment-or-uncomment-region (line-beginning-position) (line-beginning-position 2) arg)
;;     (when (> beg end) (setq beg  (prog1 end (setq end  beg))))
;;     (let ((bol  (save-excursion (goto-char beg) (line-beginning-position)))
;;           (eol  (save-excursion (goto-char end) (if (bolp) (point) (line-end-position)))))
;;       (comment-region bol eol arg))))

(with-eval-after-load 'indent-tools-mode
  (defun enable-indent-tools-mode ()
    (global-set-key (kbd "C-c >") 'indent-tools-hydra/body)
    ))
