;;; config-projectile.el -*- lexical-binding: t; -*-

(setq projectile-globally-ignored-files
      (append '(
                "uv.lock"
                )))

;; Enable caching for better performance
(setq projectile-enable-caching t)

;; Use native indexing for better performance
(setq projectile-indexing-method 'native)

;; Ensure projectile and counsel-projectile configurations load after the packages
(with-eval-after-load 'counsel-projectile
  ;; Custom action for switching projects and opening magit-status
  (defun my/switch-project-and-magit-status (project)
    "Switch to PROJECT and open Magit status."
    (let ((default-directory project))
      (magit-status)
      (message "Switched to project: %s" project)))

  ;; Set the default action for counsel-projectile-switch-project
  (setq counsel-projectile-switch-project-action
        #'my/switch-project-and-magit-status))

;; Keybinding for invoking this behavior with M-o
(with-eval-after-load 'projectile
  (global-set-key (kbd "M-o") #'counsel-projectile-switch-project))

;; Enable projectile globally
(projectile-mode +1)

;; Ensure completion works with project files
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-files))
