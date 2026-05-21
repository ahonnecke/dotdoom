;;; config-windmove.el -*- lexical-binding: t; -*-


(global-set-key (kbd "C-<next>") 'windmove-right)
(global-set-key (kbd "C-<prior>") 'windmove-left)

;; Window resize (recovered from pre-incident init-func.el, 2026-05-21).
;; M-W / M-E are distinct from stock M-w (kill-ring-save) and M-e
;; (forward-sentence) — both prefixes are unbound in emacs -Q.
(defun ash/window-width-increase ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun ash/window-width-decrease ()
  (interactive)
  (shrink-window-horizontally 5))

(defun ash/window-height-increase ()
  (interactive)
  (enlarge-window 5))

(defun ash/window-height-decrease ()
  (interactive)
  (shrink-window 5))

(global-set-key (kbd "M-W =") #'ash/window-width-increase)
(global-set-key (kbd "M-W -") #'ash/window-width-decrease)
(global-set-key (kbd "M-E =") #'ash/window-height-increase)
(global-set-key (kbd "M-E -") #'ash/window-height-decrease)
