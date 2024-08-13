;;; config-jolly-brancher.el -*- lexical-binding: t; -*-

(defun jolly-brancher ()
  "Open vterm in the root of the project and run 'jolly-brancher' command."
  (interactive)
  (let ((default-directory (projectile-project-root))) ; Assumes you're using Projectile
    (vterm)
    (vterm-send-string "jolly-brancher\n"))) ; Send the command to the vterm

;; Bind the function to a key combination (optional)
(global-set-key (kbd "C-c j") #'jolly-brancher)
