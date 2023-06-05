;;; ../src/home/.doom.d/config-counsel.el -*- lexical-binding: t; -*-

;; TODO: make this only fire if we are in a git project
;; TODO make this defer, right now this throws on load
(with-eval-after-load "projectile"
  (with-eval-after-load "counsel"

    (setq projectile-switch-project-action #'projectile-vc)

    (setcar
     counsel-projectile-switch-project-action
     (1+ (cl-position #'counsel-projectile-switch-project-action-vc
                      (cdr counsel-projectile-switch-project-action)
                      :key #'cadr)))

    (setcar counsel-projectile-switch-project-action 12)

    (counsel-projectile-modify-action
 'counsel-projectile-switch-project-action
 '((default counsel-projectile-switch-project-action-vc)))

    ))

;; maybe something like this
;; (defun my-switch-project ()
;;   (interactive)
;;   (if (projectile-project-p)
;;       (projectile-switch-project)
;;     (ivy-switch-buffer)))
