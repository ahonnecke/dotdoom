;;; config-elisp.el -*- lexical-binding: t; -*-

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'check-parens nil t)))

