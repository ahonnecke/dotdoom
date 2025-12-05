;;; config-crewcapable-services.el --- CrewCapable Vercel/Supabase/AWS layer -*- lexical-binding: t; -*-
;;
;; Project-specific service commands for crewcapableai.
;; Layers on top of generic vercel/supabase modes.
;;
;; Bindings: C-c C s (services transient)

;; These are loaded via config.el, just declare dependency
(declare-function vercel-transient "config-vercel")
(declare-function supabase-transient "config-supabase")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Project Constants
;;; ════════════════════════════════════════════════════════════════════════════

(defvar crewcapable-main-repo "~/src/.crewcapableai.main"
  "Main crewcapable repository path.")

(defvar crewcapable-worktree-parent "~/src"
  "Parent directory for worktrees.")

(defvar crewcapable-vercel-project "crewcapableai"
  "Vercel project name.")

(defvar crewcapable-supabase-project-id nil
  "Supabase project ID (set from supabase/.temp/project-ref).")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Helper Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun crewcapable--current-worktree ()
  "Get current worktree root if in a crewcapable project."
  (when-let ((root (locate-dominating-file default-directory ".git")))
    (when (string-match-p "crewcapable" root)
      root)))

(defun crewcapable--worktree-or-main ()
  "Return current worktree or main repo."
  (or (crewcapable--current-worktree)
      (expand-file-name crewcapable-main-repo)))

(defun crewcapable--run (cmd &optional buffer-name)
  "Run CMD in crewcapable project context."
  (let ((default-directory (crewcapable--worktree-or-main)))
    (compile cmd t)
    (when buffer-name
      (with-current-buffer "*compilation*"
        (rename-buffer buffer-name t)))))

(defun crewcapable--run-vterm (cmd buffer-name)
  "Run CMD in vterm in crewcapable context."
  (let ((default-directory (crewcapable--worktree-or-main)))
    (if (fboundp 'vterm)
        (progn
          (vterm buffer-name)
          (vterm-send-string cmd)
          (vterm-send-return))
      (async-shell-command cmd buffer-name))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Vercel Commands (CrewCapable-specific)
;;; ════════════════════════════════════════════════════════════════════════════

(defun crewcapable-vercel-deploy-preview ()
  "Deploy current worktree to Vercel preview."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (message "Deploying %s to preview..." branch)
    (crewcapable--run "vercel deploy" "*cc-vercel-deploy*")))

(defun crewcapable-vercel-deploy-prod ()
  "Deploy to production (from main only)."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if (not (string= branch "main"))
        (user-error "Production deploy only from main branch (currently on %s)" branch)
      (when (yes-or-no-p "Deploy to PRODUCTION? ")
        (crewcapable--run "vercel deploy --prod" "*cc-vercel-deploy*")))))

(defun crewcapable-vercel-logs ()
  "View logs for crewcapable deployments."
  (interactive)
  (crewcapable--run-vterm "vercel logs --follow" "*cc-vercel-logs*"))

(defun crewcapable-vercel-env-pull ()
  "Pull Vercel env vars to .env.local."
  (interactive)
  (crewcapable--run "vercel env pull .env.local" "*cc-vercel-env*"))

(defun crewcapable-vercel-open ()
  "Open current deployment in browser."
  (interactive)
  (let ((default-directory (crewcapable--worktree-or-main)))
    (shell-command "vercel open")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Supabase Commands (CrewCapable-specific)
;;; ════════════════════════════════════════════════════════════════════════════

(defun crewcapable-supabase-start ()
  "Start local Supabase for crewcapable."
  (interactive)
  (crewcapable--run-vterm "supabase start" "*cc-supabase*"))

(defun crewcapable-supabase-stop ()
  "Stop local Supabase."
  (interactive)
  (crewcapable--run "supabase stop"))

(defun crewcapable-supabase-status ()
  "Show Supabase status."
  (interactive)
  (crewcapable--run "supabase status" "*cc-supabase-status*"))

(defun crewcapable-supabase-reset ()
  "Reset local database with migrations and seed."
  (interactive)
  (when (yes-or-no-p "Reset crewcapable local database? ")
    (crewcapable--run "supabase db reset" "*cc-supabase-db*")))

(defun crewcapable-supabase-migrate ()
  "Run pending migrations."
  (interactive)
  (crewcapable--run "supabase migration up" "*cc-supabase-db*"))

(defun crewcapable-supabase-new-migration (name)
  "Create new migration for crewcapable."
  (interactive "sMigration name: ")
  (crewcapable--run (format "supabase migration new %s"
                            (shell-quote-argument name))))

(defun crewcapable-supabase-diff ()
  "Diff local schema against migrations."
  (interactive)
  (crewcapable--run "supabase db diff" "*cc-supabase-diff*"))

(defun crewcapable-supabase-push ()
  "Push migrations to remote (DANGEROUS)."
  (interactive)
  (when (yes-or-no-p "Push migrations to REMOTE crewcapable database? ")
    (crewcapable--run "supabase db push" "*cc-supabase-push*")))

(defun crewcapable-supabase-gen-types ()
  "Generate TypeScript types from local schema."
  (interactive)
  (crewcapable--run
   "supabase gen types typescript --local > src/types/supabase.ts"
   "*cc-supabase-types*")
  (message "Types generated to src/types/supabase.ts"))

(defun crewcapable-supabase-studio ()
  "Open local Supabase Studio."
  (interactive)
  (browse-url "http://localhost:54323"))

(defun crewcapable-supabase-functions-serve ()
  "Serve edge functions locally."
  (interactive)
  (crewcapable--run-vterm "supabase functions serve" "*cc-functions*"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dev Environment Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun crewcapable-dev ()
  "Start full dev environment (next dev)."
  (interactive)
  (crewcapable--run-vterm "npm run dev" "*cc-dev*"))

(defun crewcapable-dev-full ()
  "Start dev with Supabase (supabase start && npm run dev)."
  (interactive)
  (crewcapable--run-vterm "supabase start && npm run dev" "*cc-dev-full*"))

(defun crewcapable-build ()
  "Build the project."
  (interactive)
  (crewcapable--run "npm run build" "*cc-build*"))

(defun crewcapable-test ()
  "Run tests."
  (interactive)
  (crewcapable--run "npm test" "*cc-test*"))

(defun crewcapable-lint ()
  "Run linter."
  (interactive)
  (crewcapable--run "npm run lint" "*cc-lint*"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix crewcapable-services-transient ()
  "CrewCapable services menu."
  ["CrewCapable Services"
   ["Vercel"
    ("v d" "Deploy preview" crewcapable-vercel-deploy-preview)
    ("v p" "Deploy PROD" crewcapable-vercel-deploy-prod)
    ("v l" "Logs (follow)" crewcapable-vercel-logs)
    ("v e" "Pull env" crewcapable-vercel-env-pull)
    ("v o" "Open" crewcapable-vercel-open)]
   ["Supabase"
    ("s s" "Start" crewcapable-supabase-start)
    ("s S" "Stop" crewcapable-supabase-stop)
    ("s t" "Status" crewcapable-supabase-status)
    ("s r" "Reset DB" crewcapable-supabase-reset)
    ("s o" "Studio" crewcapable-supabase-studio)]
   ["Supabase DB"
    ("d m" "Migrate up" crewcapable-supabase-migrate)
    ("d n" "New migration" crewcapable-supabase-new-migration)
    ("d d" "Diff" crewcapable-supabase-diff)
    ("d p" "Push (remote)" crewcapable-supabase-push)
    ("d g" "Gen types" crewcapable-supabase-gen-types)]
   ["Dev"
    ("D" "npm run dev" crewcapable-dev)
    ("F" "Full (supa+dev)" crewcapable-dev-full)
    ("B" "Build" crewcapable-build)
    ("T" "Test" crewcapable-test)
    ("L" "Lint" crewcapable-lint)
    ("f" "Functions serve" crewcapable-supabase-functions-serve)]])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; C-c C s = CrewCapable Services
(define-key ashton-mode-map (kbd "C-c C s") #'crewcapable-services-transient)

;; Quick access
(define-key ashton-mode-map (kbd "C-c C d") #'crewcapable-dev)
(define-key ashton-mode-map (kbd "C-c C D") #'crewcapable-dev-full)

(provide 'config-crewcapable-services)
;;; config-crewcapable-services.el ends here
