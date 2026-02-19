;;; orchard-window.el --- Orchard window utilities -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; Window placement is handled by orchard-claude.el (orchard--place-claude-buffer,
;; orchard--get-claude-target-window).  This file exists only to satisfy
;; (require 'orchard-window) from other orchard modules.
;;
;; The column management system that was here (column tracking, branch→column
;; mapping, window dedication, display-buffer-alist rules) was never activated
;; and was removed in Feb 2026.

(require 'orchard-vars)

(provide 'orchard-window)
;;; orchard-window.el ends here
