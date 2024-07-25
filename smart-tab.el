;;; smart-tab.el -*- lexical-binding: t; -*-

(defun my-return-function ()
  (interactive)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "RET") 'my-return-function)


;;; smart-tab.el ends here
