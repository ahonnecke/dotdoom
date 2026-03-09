;;; ~/.doom.d/config-rainbow.el -*- lexical-binding: t; -*-
;;
;; rainbow-delimiters: Doom enables for emacs-lisp. Extend to more modes.
;; rainbow-mode: Doom enables for CSS. Extend to more modes.

;; Rainbow delimiters in all prog modes (not just elisp)
(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

;; Rainbow-mode in additional modes where colors appear
(add-hook! '(typescript-ts-mode-hook
             tsx-ts-mode-hook
             js-ts-mode-hook
             web-mode-hook
             html-mode-hook
             conf-mode-hook)
           #'rainbow-mode)
