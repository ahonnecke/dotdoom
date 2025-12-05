;;; ~/.doom.d/config-crux.el -*- lexical-binding: t; -*-

(with-eval-after-load "crux"
  (define-key ashton-mode-map (kbd "C-c i d") 'crux-insert-date))
