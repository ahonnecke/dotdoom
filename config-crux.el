;; (use-package crux
;;   :bind
;;   (("C-a" . crux-move-beginning-of-line)
;;    ("C-x 4 t" . crux-transpose-windows)
;;    ("C-x K" . crux-kill-other-buffers)
;;    ("C-k" . crux-smart-kill-line))
;;   :config
;;   (crux-with-region-or-buffer indent-region)
;;   (crux-with-region-or-buffer untabify)
;;   (crux-with-region-or-point-to-eol kill-ring-save)
;;   (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))
