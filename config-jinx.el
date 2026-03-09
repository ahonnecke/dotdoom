;;; ~/.doom.d/config-jinx.el -*- lexical-binding: t; -*-
;;
;; Jinx: Fast spell-checker that respects fontlock faces.
;; Only checks comments and strings in code buffers.

(use-package! jinx
  :hook (emacs-startup . global-jinx-mode)
  :config
  ;; Disable flyspell since jinx replaces it
  (after! flyspell
    (remove-hook 'text-mode-hook #'flyspell-mode)
    (remove-hook 'prog-mode-hook #'flyspell-prog-mode))

  ;; Keybindings
  (define-key ashton-mode-map (kbd "C-c j j") #'jinx-correct)
  (define-key ashton-mode-map (kbd "C-c j n") #'jinx-next)
  (define-key ashton-mode-map (kbd "C-c j p") #'jinx-previous)
  (define-key ashton-mode-map (kbd "C-c j l") #'jinx-languages))
