;;; ~/.doom.d/config-vercel.el -*- lexical-binding: t; -*-

;; Vercel Mode: Transient interface for Vercel CLI
;;
;; Install CLI: npm i -g vercel
;;
;; Usage:
;;   C-c V   - Open vercel transient menu
;;   C-c V d - Deploy current project
;;   C-c V l - View logs

(require 'transient)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar vercel-project-root nil
  "Current Vercel project root. Auto-detected or set manually.")

(defcustom vercel-default-env "preview"
  "Default environment for deployments."
  :type '(choice (const "production")
                 (const "preview")
                 (const "development"))
  :group 'vercel)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun vercel--project-root ()
  "Get Vercel project root (directory with vercel.json or .vercel)."
  (or vercel-project-root
      (locate-dominating-file default-directory "vercel.json")
      (locate-dominating-file default-directory ".vercel")
      (projectile-project-root)
      default-directory))

(defun vercel--run (cmd &optional buffer-name)
  "Run vercel CMD in compilation buffer."
  (let ((default-directory (vercel--project-root))
        (buf-name (or buffer-name "*vercel*")))
    (compile (concat "vercel " cmd) t)
    (with-current-buffer "*compilation*"
      (rename-buffer buf-name t))))

(defun vercel--run-async (cmd callback)
  "Run vercel CMD asynchronously, call CALLBACK with output."
  (let ((default-directory (vercel--project-root)))
    (async-shell-command (concat "vercel " cmd))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun vercel-deploy ()
  "Deploy to Vercel (preview by default)."
  (interactive)
  (vercel--run "deploy" "*vercel-deploy*"))

(defun vercel-deploy-prod ()
  "Deploy to production."
  (interactive)
  (when (yes-or-no-p "Deploy to PRODUCTION? ")
    (vercel--run "deploy --prod" "*vercel-deploy*")))

(defun vercel-logs ()
  "View deployment logs."
  (interactive)
  (vercel--run "logs" "*vercel-logs*"))

(defun vercel-logs-follow ()
  "Follow deployment logs in real-time."
  (interactive)
  (let ((default-directory (vercel--project-root)))
    (async-shell-command "vercel logs --follow" "*vercel-logs*")))

(defun vercel-list ()
  "List deployments."
  (interactive)
  (vercel--run "list" "*vercel-list*"))

(defun vercel-env-list ()
  "List environment variables."
  (interactive)
  (vercel--run "env ls" "*vercel-env*"))

(defun vercel-env-pull ()
  "Pull environment variables to .env.local."
  (interactive)
  (vercel--run "env pull" "*vercel-env*"))

(defun vercel-dev ()
  "Start local development server."
  (interactive)
  (let ((default-directory (vercel--project-root)))
    (async-shell-command "vercel dev" "*vercel-dev*")))

(defun vercel-open ()
  "Open project in browser."
  (interactive)
  (let ((default-directory (vercel--project-root)))
    (shell-command "vercel open")))

(defun vercel-whoami ()
  "Show current Vercel user."
  (interactive)
  (vercel--run "whoami"))

(defun vercel-link ()
  "Link directory to Vercel project."
  (interactive)
  (vercel--run "link"))

(defun vercel-inspect ()
  "Inspect last deployment."
  (interactive)
  (vercel--run "inspect"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix vercel-transient ()
  "Vercel CLI commands."
  ["Vercel"
   ["Deploy"
    ("d" "Deploy (preview)" vercel-deploy)
    ("D" "Deploy PRODUCTION" vercel-deploy-prod)
    ("i" "Inspect deployment" vercel-inspect)]
   ["View"
    ("l" "Logs" vercel-logs)
    ("L" "Logs (follow)" vercel-logs-follow)
    ("s" "List deployments" vercel-list)
    ("o" "Open in browser" vercel-open)]
   ["Environment"
    ("e" "List env vars" vercel-env-list)
    ("E" "Pull env to .env.local" vercel-env-pull)]
   ["Project"
    ("v" "Dev server" vercel-dev)
    ("k" "Link project" vercel-link)
    ("w" "Whoami" vercel-whoami)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

(define-key ashton-mode-map (kbd "C-c V") #'vercel-transient)

(provide 'config-vercel)
;;; config-vercel.el ends here
