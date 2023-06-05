;;; ../src/home/.doom.d/config-vterm.el -*- lexical-binding: t; -*-

;;vterm or something overwrites the bindings
(map! :after vterm :map vterm-mode-map "M-r" #'consult-recent-file)

;; move this

(add-hook 'text-mode-hook 'turn-on-auto-fill)

