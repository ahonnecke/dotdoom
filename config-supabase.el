;;; ~/.doom.d/config-supabase.el -*- lexical-binding: t; -*-

;; Supabase Mode: Transient interface for Supabase CLI
;;
;; Install CLI: brew install supabase/tap/supabase
;;              or: npm i -g supabase
;;
;; Usage:
;;   C-c B   - Open supabase transient menu
;;   C-c B s - Start local supabase
;;   C-c B d - Open dashboard

(require 'transient)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar supabase-project-root nil
  "Current Supabase project root. Auto-detected or set manually.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase--project-root ()
  "Get Supabase project root (directory with supabase/ folder)."
  (or supabase-project-root
      (locate-dominating-file default-directory "supabase")
      (projectile-project-root)
      default-directory))

(defun supabase--run (cmd &optional buffer-name)
  "Run supabase CMD in compilation buffer."
  (let ((default-directory (supabase--project-root))
        (buf-name (or buffer-name "*supabase*")))
    (compile (concat "supabase " cmd) t)
    (with-current-buffer "*compilation*"
      (rename-buffer buf-name t))))

(defun supabase--run-vterm (cmd buffer-name)
  "Run supabase CMD in vterm (for interactive commands)."
  (let ((default-directory (supabase--project-root)))
    (if (fboundp 'vterm)
        (progn
          (vterm buffer-name)
          (vterm-send-string (concat "supabase " cmd))
          (vterm-send-return))
      (async-shell-command (concat "supabase " cmd) buffer-name))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Local Development Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase-start ()
  "Start local Supabase stack."
  (interactive)
  (supabase--run-vterm "start" "*supabase-local*"))

(defun supabase-stop ()
  "Stop local Supabase stack."
  (interactive)
  (supabase--run "stop"))

(defun supabase-status ()
  "Show status of local Supabase."
  (interactive)
  (supabase--run "status"))

(defun supabase-db-reset ()
  "Reset local database (runs migrations + seed)."
  (interactive)
  (when (yes-or-no-p "Reset local database? This will delete all data. ")
    (supabase--run "db reset" "*supabase-db*")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Database Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase-db-diff ()
  "Diff local schema against migrations."
  (interactive)
  (supabase--run "db diff" "*supabase-db*"))

(defun supabase-db-push ()
  "Push migrations to remote database."
  (interactive)
  (when (yes-or-no-p "Push migrations to REMOTE database? ")
    (supabase--run "db push" "*supabase-db*")))

(defun supabase-db-pull ()
  "Pull schema from remote database."
  (interactive)
  (supabase--run "db pull" "*supabase-db*"))

(defun supabase-migration-new (name)
  "Create new migration with NAME."
  (interactive "sMigration name: ")
  (supabase--run (format "migration new %s" (shell-quote-argument name))))

(defun supabase-migration-list ()
  "List migrations."
  (interactive)
  (supabase--run "migration list"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Functions (Edge Functions)
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase-functions-serve ()
  "Serve edge functions locally."
  (interactive)
  (supabase--run-vterm "functions serve" "*supabase-functions*"))

(defun supabase-functions-deploy ()
  "Deploy all edge functions."
  (interactive)
  (supabase--run "functions deploy" "*supabase-functions*"))

(defun supabase-functions-new (name)
  "Create new edge function with NAME."
  (interactive "sFunction name: ")
  (supabase--run (format "functions new %s" (shell-quote-argument name))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Remote/Dashboard Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase-link ()
  "Link to remote Supabase project."
  (interactive)
  (supabase--run "link"))

(defun supabase-projects-list ()
  "List Supabase projects."
  (interactive)
  (supabase--run "projects list"))

(defun supabase-open-dashboard ()
  "Open Supabase dashboard in browser."
  (interactive)
  (let ((default-directory (supabase--project-root)))
    ;; Try to get project ref from .supabase/config
    (browse-url "https://supabase.com/dashboard")))

(defun supabase-open-studio ()
  "Open local Supabase Studio (localhost:54323)."
  (interactive)
  (browse-url "http://localhost:54323"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Gen/Types Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun supabase-gen-types ()
  "Generate TypeScript types from database schema."
  (interactive)
  (supabase--run "gen types typescript --local > types/supabase.ts" "*supabase-gen*"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix supabase-transient ()
  "Supabase CLI commands."
  ["Supabase"
   ["Local Stack"
    ("s" "Start" supabase-start)
    ("S" "Stop" supabase-stop)
    ("t" "Status" supabase-status)
    ("o" "Open Studio (local)" supabase-open-studio)]
   ["Database"
    ("r" "Reset (local)" supabase-db-reset)
    ("d" "Diff" supabase-db-diff)
    ("p" "Pull (remote→local)" supabase-db-pull)
    ("P" "Push (local→remote)" supabase-db-push)]
   ["Migrations"
    ("m" "New migration" supabase-migration-new)
    ("M" "List migrations" supabase-migration-list)]
   ["Functions"
    ("f" "Serve locally" supabase-functions-serve)
    ("F" "Deploy all" supabase-functions-deploy)
    ("n" "New function" supabase-functions-new)]
   ["Project"
    ("l" "Link project" supabase-link)
    ("L" "List projects" supabase-projects-list)
    ("g" "Generate types" supabase-gen-types)
    ("O" "Open dashboard" supabase-open-dashboard)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

(define-key ashton-mode-map (kbd "C-c B") #'supabase-transient)

(provide 'config-supabase)
;;; config-supabase.el ends here
