;;; config-elisp.el -*- lexical-binding: t; -*-

;; Auto-check parentheses on save in elisp buffers (catches unbalanced parens early)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'check-parens nil t)))

