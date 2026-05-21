;;; ~/.doom.d/config-crux.el -*- lexical-binding: t; -*-

(with-eval-after-load "crux"
  (define-key ashton-mode-map (kbd "C-c i d") 'crux-insert-date)
  ;; Recovered from pre-incident lain-emacs init-bindings.el (2026-05-21).
  ;; Both use C-c prefix (user-reserved) so emacs -Q stays untouched.
  (define-key ashton-mode-map (kbd "C-c f r") 'crux-rename-file-and-buffer)
  (define-key ashton-mode-map (kbd "C-c o a") 'crux-smart-open-line-above))
