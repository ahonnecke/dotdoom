;;; config-json-mode.el -*- lexical-binding: t; -*-


(defun my-custom-settings-fn ()
  (setq indent-tabs-mode t)
  (setq tab-stop-list (number-sequence 2 4 8))
  (setq tab-width 2)
  (setq indent-line-function 'insert-tab))

(add-hook 'json-mode-hook 'my-custom-settings-fn)
