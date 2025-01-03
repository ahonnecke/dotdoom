;;; config-make.el -*- lexical-binding: t; -*-

(defun my-disable-line-length-for-makefile ()
  "Disable line length checking in Makefile modes."
  (when (derived-mode-p 'makefile-gmake-mode)
    (setq-local whitespace-line-column nil) ;; For whitespace-mode
    (setq-local fill-column most-positive-fixnum))) ;; For auto-fill-mode

(add-hook 'makefile-gmake-mode-hook #'my-disable-line-length-for-makefile)
