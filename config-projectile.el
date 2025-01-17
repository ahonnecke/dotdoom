;;; config-projectile.el -*- lexical-binding: t; -*-

;; Enable caching for better performance
(setq projectile-enable-caching t)

;; Set default action when switching projects to open Magit status
(setq projectile-switch-project-action #'magit-status)

;; Automatically discover projects in specified paths
;;(setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
(setq projectile-project-search-path '("~/src/"))
(setq projectile-auto-discover t)

;; Use native indexing for better performance
(setq projectile-indexing-method 'native)

;; Ensure projectile configurations load after the package
(with-eval-after-load 'projectile
  ;; Globally ignored files
  (setq projectile-globally-ignored-files
        (append '("uv.lock")
                projectile-globally-ignored-files))

  ;; Globally ignored directories
  (setq projectile-globally-ignored-directories
        (append '(".cache" "node_modules")
                projectile-globally-ignored-directories)))

;; Ensure completion works with project files
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-files))

;; Configure counsel-projectile for enhanced project switching
(with-eval-after-load 'counsel-projectile
  ;; Custom action for switching projects and opening Magit status
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
