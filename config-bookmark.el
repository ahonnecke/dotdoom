;;; config-bookmark.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x r s") 'bookmark-save)

;; Fix: "Buffer is read-only" when opening bookmarks (especially FTP/TRAMP)
;; The issue is that hooks (like recentf) try to write while the
;; bookmark list buffer is still current. We advise the jump function
;; to ensure we're not in a read-only buffer when the jump completes.
(with-eval-after-load 'bookmark
  (defun bookmark-bmenu-this-window--handle-readonly (orig-fun &rest args)
    "Advice to handle read-only buffer errors when jumping to bookmarks."
    (let ((inhibit-read-only t))
      (apply orig-fun args)))

  (advice-add 'bookmark-bmenu-this-window :around #'bookmark-bmenu-this-window--handle-readonly)
  (advice-add 'bookmark-bmenu-other-window :around #'bookmark-bmenu-this-window--handle-readonly)
  (advice-add 'bookmark-jump :around #'bookmark-bmenu-this-window--handle-readonly))
