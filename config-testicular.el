;;; ~/.doom.d/config-testicular.el -*- lexical-binding: t; -*-
;;
;; Testicular configuration for Doom Emacs
;; The core package is in ~/.doom.d/lisp/testicular/

;; Add package to load path
(add-to-list 'load-path (expand-file-name "lisp/testicular" doom-user-dir))
(require 'testicular)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Project-Specific Defaults (can be overridden via .dir-locals.el)
;;; ════════════════════════════════════════════════════════════════════════════

;; CrewCapable defaults
(setq testicular-base-branch "upstream/dev"
      testicular-pr-remote "upstream"
      testicular-pr-base-branch "dev"
      testicular-pr-backend 'github)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Service Integration (Doom-specific)
;;; ════════════════════════════════════════════════════════════════════════════

;; Add service shortcuts to testicular-mode
(with-eval-after-load 'testicular
  (define-key testicular-mode-map (kbd "V") #'testicular-vercel)
  (define-key testicular-mode-map (kbd "B") #'testicular-supabase)
  (define-key testicular-mode-map (kbd "A") #'testicular-aws)
  (define-key testicular-mode-map (kbd "O") #'testicular-open-url))

(defun testicular-vercel ()
  "Open Vercel transient for deployment verification."
  (interactive)
  (if (fboundp 'vercel-transient)
      (vercel-transient)
    (user-error "Vercel mode not available")))

(defun testicular-supabase ()
  "Open Supabase transient for database verification."
  (interactive)
  (if (fboundp 'supabase-transient)
      (supabase-transient)
    (user-error "Supabase mode not available")))

(defun testicular-aws ()
  "Open AWS transient for cloud verification."
  (interactive)
  (if (fboundp 'aws-transient)
      (aws-transient)
    (user-error "AWS mode not available")))

(defun testicular-open-url ()
  "Open URL based on current environment."
  (interactive)
  (let ((url (pcase testicular-current-environment
               ("local" "http://localhost:3000")
               ("staging" (read-string "Staging URL: " "https://staging.crewcapable.com"))
               ("prod" (read-string "Prod URL: " "https://crewcapable.com")))))
    (browse-url url)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Orchard Integration
;;; ════════════════════════════════════════════════════════════════════════════

;; Hook into orchard for status display
(add-hook 'testicular-start-hook
          (lambda (root)
            (when (fboundp 'orchard--refresh-if-visible)
              (orchard--refresh-if-visible))))

(add-hook 'testicular-complete-hook
          (lambda (root passed failed total)
            (when (fboundp 'orchard--set-test-results)
              (orchard--set-test-results root passed failed total))
            (when (fboundp 'orchard--refresh-if-visible)
              (orchard--refresh-if-visible))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Magit Integration
;;; ════════════════════════════════════════════════════════════════════════════

;; Add testicular to magit dispatch (,)
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-dispatch "!"
    '("T" "Testicular" testicular-transient)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Global Keybinding
;;; ════════════════════════════════════════════════════════════════════════════

(global-set-key (kbd "C-c T") #'testicular-transient)

(provide 'config-testicular)
;;; config-testicular.el ends here
