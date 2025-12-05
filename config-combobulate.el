;;; ~/.doom.d/config-combobulate.el -*- lexical-binding: t; -*-

;; Combobulate - Structured editing for tree-sitter modes
;; `M-x combobulate' or `C-c o o' to start using Combobulate

(add-to-list 'load-path "/home/ahonnecke/src/combobulate")

(use-package! combobulate
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode))
  :config
  (setq combobulate-key-prefix "C-c o"))
