;;; ~/.doom.d/config-whole-line-or-region.el -*- lexical-binding: t; -*-
;;
;; whole-line-or-region: C-w/M-w operate on whole line when no region active.
;; No conflict with region-bindings-mode (which only fires WITH an active region).

(use-package! whole-line-or-region
  :hook (emacs-startup . whole-line-or-region-global-mode))
