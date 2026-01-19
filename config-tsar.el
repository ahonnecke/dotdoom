;;; ~/.doom.d/config-tsar.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tsar-mode: Deployment Dashboard
;; "Tsar Bomba - the bomb that deploys"
;;
;; Shows what's running on local, staging, prod with commands for promoting
;; between environments. Deploys in clean /tmp directory via existing make targets.

;;; Code:

(require 'transient)

;;;; Custom Variables

(defgroup tsar nil
  "Deployment dashboard for crewcapableai."
  :group 'tools
  :prefix "tsar-")

(defcustom tsar-repo-root nil
  "Root directory of crewcapableai repo. Auto-detected if nil."
  :type '(choice (const nil) directory)
  :group 'tsar)

(defcustom tsar-staging-url "https://crewcapableai-staging.vercel.app"
  "Staging frontend URL."
  :type 'string
  :group 'tsar)

(defcustom tsar-prod-url "https://app.crewcapable.com"
  "Production frontend URL."
  :type 'string
  :group 'tsar)

(defcustom tsar-local-url "https://1.local.crewcapable.com"
  "Local development URL."
  :type 'string
  :group 'tsar)

(defcustom tsar-github-repo "crewcapable/crewcapableai"
  "GitHub repository for tag links."
  :type 'string
  :group 'tsar)

;;;; Faces

(defface tsar-env-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for environment headers."
  :group 'tsar)

(defface tsar-env-local
  '((t :foreground "#61AFEF"))
  "Face for LOCAL environment."
  :group 'tsar)

(defface tsar-env-staging
  '((t :foreground "#E5C07B"))
  "Face for STAGING environment."
  :group 'tsar)

(defface tsar-env-prod
  '((t :foreground "#98C379"))
  "Face for PRODUCTION environment."
  :group 'tsar)

(defface tsar-success
  '((t :foreground "#98C379"))
  "Face for success indicators."
  :group 'tsar)

(defface tsar-warning
  '((t :foreground "#E5C07B"))
  "Face for warning indicators."
  :group 'tsar)

(defface tsar-tag
  '((t :foreground "#C678DD" :underline t))
  "Face for clickable tags."
  :group 'tsar)

(defface tsar-url
  '((t :foreground "#56B6C2" :underline t))
  "Face for clickable URLs."
  :group 'tsar)

(defface tsar-commit
  '((t :foreground "#ABB2BF"))
  "Face for commit hashes."
  :group 'tsar)

(defface tsar-box
  '((t :foreground "#5C6370"))
  "Face for box drawing characters."
  :group 'tsar)

;;;; Internal Variables

(defvar tsar--refresh-timer nil
  "Timer for auto-refresh.")

(defvar tsar--current-data nil
  "Cached dashboard data.")

;;;; Utility Functions

(defun tsar--get-repo-root ()
  "Get the crewcapableai repo root."
  (or tsar-repo-root
      (let ((default-directory (or default-directory "~/")))
        (locate-dominating-file default-directory "CLAUDE.md"))
      (expand-file-name "~/src/.crewcapableai.main")))

(defun tsar--run-git (&rest args)
  "Run git command with ARGS, return trimmed output."
  (let ((default-directory (tsar--get-repo-root)))
    (string-trim
     (shell-command-to-string
      (concat "git " (mapconcat #'shell-quote-argument args " ") " 2>/dev/null")))))

(defun tsar--run-cmd (cmd)
  "Run shell CMD, return trimmed output."
  (let ((default-directory (tsar--get-repo-root)))
    (string-trim (shell-command-to-string (concat cmd " 2>/dev/null")))))

(defun tsar--relative-time (timestamp)
  "Convert TIMESTAMP to relative time string."
  (if (or (null timestamp) (string-empty-p timestamp))
      ""
    (let* ((time (date-to-time timestamp))
           (diff (float-time (time-subtract (current-time) time)))
           (days (floor (/ diff 86400)))
           (hours (floor (/ diff 3600)))
           (mins (floor (/ diff 60))))
      (cond
       ((< days 1) (if (< hours 1)
                       (format "%d mins ago" mins)
                     (format "%d hours ago" hours)))
       ((= days 1) "1 day ago")
       (t (format "%d days ago" days))))))

;;;; Data Fetching

(defun tsar--get-latest-tag (prefix)
  "Get latest tag matching PREFIX (e.g., staging- or prod-)."
  (let ((tags (tsar--run-git "tag" "-l" (concat prefix "*") "--sort=-creatordate")))
    (if (string-empty-p tags)
        nil
      (car (split-string tags "\n")))))

(defun tsar--get-tag-info (tag)
  "Get info about TAG: commit, message, date."
  (when tag
    (let* ((commit (tsar--run-git "rev-list" "-1" "--abbrev-commit" tag))
           (message (tsar--run-git "log" "-1" "--format=%s" tag))
           (date (tsar--run-git "log" "-1" "--format=%ci" tag)))
      (list :tag tag
            :commit commit
            :message (if (> (length message) 50)
                         (concat (substring message 0 47) "...")
                       message)
            :date date
            :relative (tsar--relative-time date)))))

(defun tsar--get-commits-ahead (base target)
  "Count commits TARGET is ahead of BASE."
  (let ((count (tsar--run-git "rev-list" "--count" (concat base ".." target))))
    (if (string-match-p "^[0-9]+$" count)
        (string-to-number count)
      0)))

(defun tsar--get-local-status ()
  "Get local development status."
  (let* ((branch (tsar--run-git "rev-parse" "--abbrev-ref" "HEAD"))
         (commit (tsar--run-git "rev-parse" "--short" "HEAD"))
         (message (tsar--run-git "log" "-1" "--format=%s"))
         (make-dev-running (tsar--run-cmd "pgrep -f 'make dev' >/dev/null && echo yes || echo no"))
         (staging-tag (tsar--get-latest-tag "staging-"))
         (ahead-of-staging (if staging-tag
                               (tsar--get-commits-ahead staging-tag "HEAD")
                             0)))
    (list :env 'local
          :branch branch
          :commit commit
          :message (if (> (length message) 50)
                       (concat (substring message 0 47) "...")
                     message)
          :running (string= make-dev-running "yes")
          :ahead ahead-of-staging
          :url tsar-local-url)))

(defun tsar--get-staging-status ()
  "Get staging environment status."
  (let* ((tag-info (tsar--get-tag-info (tsar--get-latest-tag "staging-")))
         (prod-tag (tsar--get-latest-tag "prod-"))
         (ahead-of-prod (if (and tag-info prod-tag)
                            (tsar--get-commits-ahead prod-tag (plist-get tag-info :tag))
                          0)))
    (list :env 'staging
          :tag (plist-get tag-info :tag)
          :commit (plist-get tag-info :commit)
          :message (plist-get tag-info :message)
          :date (plist-get tag-info :date)
          :relative (plist-get tag-info :relative)
          :ahead ahead-of-prod
          :url tsar-staging-url)))

(defun tsar--get-prod-status ()
  "Get production environment status."
  (let ((tag-info (tsar--get-tag-info (tsar--get-latest-tag "prod-"))))
    (list :env 'prod
          :tag (plist-get tag-info :tag)
          :commit (plist-get tag-info :commit)
          :message (plist-get tag-info :message)
          :date (plist-get tag-info :date)
          :relative (plist-get tag-info :relative)
          :url tsar-prod-url)))

(defun tsar--get-recent-deploys (&optional limit)
  "Get recent deploy tags with metadata. LIMIT defaults to 5."
  (let* ((limit (or limit 5))
         (all-tags (tsar--run-git "tag" "-l" "--sort=-creatordate"))
         (tags (seq-take
                (seq-filter (lambda (tag)
                              (or (string-prefix-p "staging-" tag)
                                  (string-prefix-p "prod-" tag)))
                            (split-string all-tags "\n" t))
                limit)))
    (mapcar #'tsar--get-tag-info tags)))

;;;; Dashboard Rendering

(defun tsar--format-header ()
  "Format the dashboard header."
  (concat
   (propertize "  💣 Tsar - Deployment Dashboard\n\n" 'face 'tsar-env-header)
   (propertize "  [r] Promote  [g] Refresh  [l] Logs  [o] Open URL  [?] Help\n\n"
               'face 'font-lock-comment-face)))

(defun tsar--make-box (width)
  "Make box drawing strings for WIDTH."
  (list :top (concat "┌" (make-string (- width 2) ?─) "┐")
        :bot (concat "└" (make-string (- width 2) ?─) "┘")
        :mid "│"))

(defun tsar--format-env-card (status)
  "Format an environment card for STATUS plist."
  (let* ((env (plist-get status :env))
         (width 73)
         (box (tsar--make-box width))
         (env-face (pcase env
                     ('local 'tsar-env-local)
                     ('staging 'tsar-env-staging)
                     ('prod 'tsar-env-prod)))
         (env-name (pcase env
                     ('local "LOCAL")
                     ('staging "STAGING")
                     ('prod "PRODUCTION")))
         (lines '()))

    ;; Top border
    (push (propertize (concat "  " (plist-get box :top) "\n") 'face 'tsar-box) lines)

    ;; Header line
    (let* ((header (propertize (concat "  " env-name) 'face env-face))
           (promote-btn (if (eq env 'staging)
                            (propertize "[R] Promote" 'face 'font-lock-keyword-face)
                          ""))
           (padding (- width 4 (length env-name) (length promote-btn) 2)))
      (push (concat "  " (propertize "│ " 'face 'tsar-box)
                    header
                    (make-string (max 1 padding) ? )
                    promote-btn
                    (propertize " │\n" 'face 'tsar-box))
            lines))

    ;; Content based on environment
    (pcase env
      ('local
       ;; Branch line
       (push (tsar--card-line width
                              (format "Branch: %s  Commit: %s"
                                      (propertize (or (plist-get status :branch) "?")
                                                  'face 'font-lock-function-name-face)
                                      (propertize (or (plist-get status :commit) "?")
                                                  'face 'tsar-commit)))
             lines)
       ;; Status line
       (push (tsar--card-line width
                              (if (plist-get status :running)
                                  (propertize "Status: make dev running" 'face 'tsar-success)
                                (propertize "Status: not running" 'face 'tsar-warning)))
             lines)
       ;; Ahead line
       (let ((ahead (plist-get status :ahead)))
         (when (and ahead (> ahead 0))
           (push (tsar--card-line width
                                  (format "↓ %d commits ahead of staging" ahead))
                 lines))))

      ((or 'staging 'prod)
       ;; Tag line
       (let ((tag (plist-get status :tag))
             (rel (plist-get status :relative)))
         (push (tsar--card-line width
                                (concat "Tag: "
                                        (if tag
                                            (propertize tag 'face 'tsar-tag
                                                        'tsar-tag tag
                                                        'mouse-face 'highlight
                                                        'help-echo "Click to browse tag")
                                          "none")
                                        (if rel (format "  (%s)" rel) "")))
               lines))
       ;; Commit line
       (push (tsar--card-line width
                              (format "Commit: %s %s"
                                      (propertize (or (plist-get status :commit) "?")
                                                  'face 'tsar-commit)
                                      (propertize (concat "\"" (or (plist-get status :message) "") "\"")
                                                  'face 'font-lock-string-face)))
             lines)
       ;; URL line
       (let ((url (plist-get status :url)))
         (push (tsar--card-line width
                                (concat "Frontend: "
                                        (propertize url 'face 'tsar-url
                                                    'tsar-url url
                                                    'mouse-face 'highlight
                                                    'help-echo "Click to open")))
               lines))
       ;; Ahead line (staging only)
       (when (eq env 'staging)
         (let ((ahead (plist-get status :ahead)))
           (when (and ahead (> ahead 0))
             (push (tsar--card-line width
                                    (format "↓ %d commits ahead of production" ahead))
                   lines))))))

    ;; Bottom border
    (push (propertize (concat "  " (plist-get box :bot) "\n\n") 'face 'tsar-box) lines)

    (apply #'concat (nreverse lines))))

(defun tsar--card-line (width content)
  "Format a card line with WIDTH containing CONTENT."
  (let* ((content-str (if (stringp content) content (format "%s" content)))
         (visible-len (length (substring-no-properties content-str)))
         (padding (- width 4 visible-len)))
    (concat "  " (propertize "│ " 'face 'tsar-box)
            content-str
            (make-string (max 1 padding) ? )
            (propertize " │\n" 'face 'tsar-box))))

(defun tsar--format-recent-deploys (deploys)
  "Format recent DEPLOYS section."
  (if (null deploys)
      ""
    (concat
     (propertize "  Recent Deploys:\n" 'face 'font-lock-comment-face)
     (mapconcat
      (lambda (d)
        (let ((tag (plist-get d :tag))
              (rel (plist-get d :relative)))
          (format "  • %s  %s\n"
                  (propertize (or tag "?") 'face 'tsar-tag)
                  (propertize (or rel "") 'face 'font-lock-comment-face))))
      deploys
      ""))))

(defun tsar--render-dashboard ()
  "Render the full dashboard."
  (let* ((local (tsar--get-local-status))
         (staging (tsar--get-staging-status))
         (prod (tsar--get-prod-status))
         (recent (tsar--get-recent-deploys 5)))
    (setq tsar--current-data
          (list :local local :staging staging :prod prod :recent recent))
    (concat
     (tsar--format-header)
     (tsar--format-env-card local)
     (tsar--format-env-card staging)
     (tsar--format-env-card prod)
     (tsar--format-recent-deploys recent))))

;;;; Promote Commands

(defun tsar-promote (target &optional components)
  "Promote to TARGET environment.
TARGET is \\='staging or \\='prod.
COMPONENTS is a list of \\='(backend database frontend) or nil for all."
  (interactive
   (list (intern (completing-read "Promote to: " '("staging" "prod")))))
  (let* ((components (or components '(all)))
         (cmd (tsar--build-promote-command target components)))
    (tsar--run-deploy cmd target)))

(defun tsar--build-promote-command (target components)
  "Build the make promote command for TARGET and COMPONENTS."
  (let ((env (if (eq target 'staging) "staging" "prod")))
    (cond
     ((memq 'backend components) (format "make promote.%s.backend" env))
     ((memq 'database components) (format "make promote.%s.database" env))
     ((memq 'frontend components) (format "make deploy.web.%s" env))
     (t (format "make promote.%s" env)))))

(require 'comint)
(require 'ansi-color)

(define-derived-mode tsar-deploy-mode comint-mode "Tsar-Deploy"
  "Major mode for tsar deploy output with ANSI color support."
  (setq-local comint-prompt-read-only nil)
  (setq-local comint-process-echoes nil)
  (ansi-color-for-comint-mode-on))

(defun tsar--run-deploy (command target)
  "Run COMMAND for deploying to TARGET.
Shows progress in a dedicated buffer with ANSI colors and interactivity."
  (let* ((buf-name (format "*tsar-deploy-%s*" target))
         (default-directory (tsar--get-repo-root))
         (buf (get-buffer-create buf-name)))
    ;; Kill any existing process
    (when-let ((existing-proc (get-buffer-process buf)))
      (delete-process existing-proc))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (tsar-deploy-mode)
      (insert (propertize (format "Deploying to %s...\n\n" target)
                          'face 'tsar-env-header))
      (insert (format "Command: %s\n" command))
      (insert (make-string 60 ?─) "\n\n"))

    ;; Start process via comint for interactivity
    (let ((proc (start-process-shell-command "tsar-deploy" buf command)))
      (set-process-filter proc #'comint-output-filter)
      (set-process-sentinel proc #'tsar--deploy-sentinel))

    (pop-to-buffer buf)
    (goto-char (point-max))
    (message "Deploy to %s started... (buffer is interactive for y/n prompts)" target)))

(defun tsar--deploy-sentinel (proc _event)
  "Handle deploy completion for PROC."
  (when (memq (process-status proc) '(exit signal))
    (let ((status (process-exit-status proc))
          (buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n" (make-string 60 ?─) "\n")
            (if (= status 0)
                (insert (propertize "✅ Deploy completed successfully!\n" 'face 'tsar-success))
              (insert (propertize (format "❌ Deploy failed with status %d\n" status)
                                  'face 'error)))))
        (if (= status 0)
            (progn
              (message "Deploy completed successfully!")
              (when-let ((tsar-buf (get-buffer "*Tsar*")))
                (with-current-buffer tsar-buf
                  (tsar-refresh))))
          (message "Deploy failed with status %d" status))))))

(defun tsar-promote-staging ()
  "Promote dev to staging."
  (interactive)
  (tsar-promote 'staging))

(defun tsar-promote-prod ()
  "Promote staging to prod."
  (interactive)
  (tsar-promote 'prod))

(defun tsar-promote-backend ()
  "Promote backend only."
  (interactive)
  (let ((target (intern (completing-read "Promote backend to: " '("staging" "prod")))))
    (tsar-promote target '(backend))))

(defun tsar-promote-database ()
  "Promote database only."
  (interactive)
  (let ((target (intern (completing-read "Promote database to: " '("staging" "prod")))))
    (tsar-promote target '(database))))

(defun tsar-promote-web ()
  "Promote web frontend only."
  (interactive)
  (let ((target (intern (completing-read "Promote web to: " '("staging" "prod")))))
    (tsar-promote target '(frontend))))

;;;; URL and Tag Browsing

(defun tsar-open-url (&optional env)
  "Open the URL for ENV in browser."
  (interactive
   (list (intern (completing-read "Environment: " '("local" "staging" "prod")))))
  (let ((url (pcase env
               ('local tsar-local-url)
               ('staging tsar-staging-url)
               ('prod tsar-prod-url)
               (_ tsar-staging-url))))
    (browse-url url)
    (message "Opened %s" url)))

(defun tsar-browse-tag (&optional tag)
  "Open TAG on GitHub releases page."
  (interactive
   (list (completing-read "Tag: "
                          (split-string (tsar--run-git "tag" "-l" "--sort=-creatordate") "\n" t))))
  (when tag
    (browse-url (format "https://github.com/%s/releases/tag/%s" tsar-github-repo tag))
    (message "Opened tag %s on GitHub" tag)))

(defun tsar-browse-tag-at-point ()
  "Browse the tag at point."
  (interactive)
  (let ((tag (get-text-property (point) 'tsar-tag)))
    (if tag
        (tsar-browse-tag tag)
      (message "No tag at point"))))

(defun tsar-open-url-at-point ()
  "Open the URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'tsar-url)))
    (if url
        (browse-url url)
      (message "No URL at point"))))

;;;; Log Viewing

(define-derived-mode tsar-logs-mode comint-mode "Tsar-Logs"
  "Major mode for tsar logs with ANSI color support."
  (setq-local comint-prompt-read-only nil)
  (ansi-color-for-comint-mode-on))

(defun tsar-logs (&optional env component)
  "View logs for ENV and COMPONENT."
  (interactive
   (list (intern (completing-read "Environment: " '("staging" "prod")))
         (intern (completing-read "Component: " '("backend" "web" "stepfunctions")))))
  (let* ((profile (if (eq env 'staging) "crew.dev" "crew.prod"))
         (cmd (pcase component
                ('backend
                 (format "aws --profile %s logs tail /aws/lambda/cc-%s-triage --follow"
                         profile (if (eq env 'staging) "staging" "prod")))
                ('stepfunctions
                 (format "aws --profile %s logs tail /aws/stepfunctions/cc-%s --follow"
                         profile (if (eq env 'staging) "staging" "prod")))
                (_ (format "echo 'Web logs available at Vercel dashboard'"))))
         (buf-name (format "*tsar-logs-%s-%s*" env component))
         (buf (get-buffer-create buf-name)))
    ;; Kill any existing process
    (when-let ((existing-proc (get-buffer-process buf)))
      (delete-process existing-proc))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (tsar-logs-mode))
    (let ((proc (start-process-shell-command "tsar-logs" buf cmd)))
      (set-process-filter proc #'comint-output-filter))
    (pop-to-buffer buf)))

;;;; Mode Definition

(defvar tsar-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; Promote commands (r prefix - "release")
    (define-key map (kbd "r") #'tsar-promote)
    (define-key map (kbd "R") #'tsar-promote-prod)
    (define-key map (kbd "s") #'tsar-promote-staging)
    ;; Component-specific
    (define-key map (kbd "b") #'tsar-promote-backend)
    (define-key map (kbd "D") #'tsar-promote-database)
    (define-key map (kbd "w") #'tsar-promote-web)
    ;; View
    (define-key map (kbd "l") #'tsar-logs)
    (define-key map (kbd "o") #'tsar-open-url)
    (define-key map (kbd "O") #'tsar-open-url-at-point)
    (define-key map (kbd "t") #'tsar-browse-tag)
    (define-key map (kbd "RET") #'tsar-action-at-point)
    ;; Navigation
    (define-key map (kbd "n") #'tsar-next-section)
    (define-key map (kbd "p") #'tsar-prev-section)
    ;; Refresh
    (define-key map (kbd "g") #'tsar-refresh)
    ;; Help/Quit
    (define-key map (kbd "?") #'tsar-dispatch)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Tsar mode.")

(define-derived-mode tsar-mode special-mode "Tsar"
  "Deployment dashboard mode.
\\{tsar-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

(defun tsar-action-at-point ()
  "Perform action based on what's at point."
  (interactive)
  (cond
   ((get-text-property (point) 'tsar-tag)
    (tsar-browse-tag-at-point))
   ((get-text-property (point) 'tsar-url)
    (tsar-open-url-at-point))
   (t (message "Nothing actionable at point"))))

(defun tsar-next-section ()
  "Move to next environment section."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (not (looking-at ".*LOCAL\\|.*STAGING\\|.*PRODUCTION")))
    (forward-line 1)))

(defun tsar-prev-section ()
  "Move to previous environment section."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (looking-at ".*LOCAL\\|.*STAGING\\|.*PRODUCTION")))
    (forward-line -1)))

;;;; Transient Menu

(transient-define-prefix tsar-dispatch ()
  "Tsar deployment commands."
  ["Tsar - Deployment Dashboard"
   ["Promote"
    ("s" "Dev → Staging" tsar-promote-staging)
    ("R" "Staging → Prod" tsar-promote-prod)
    ("r" "Promote..." tsar-promote)]
   ["Component Deploy"
    ("b" "Backend only" tsar-promote-backend)
    ("D" "Database only" tsar-promote-database)
    ("w" "Web only" tsar-promote-web)]
   ["View"
    ("l" "Logs" tsar-logs)
    ("o" "Open URL" tsar-open-url)
    ("t" "Browse tag" tsar-browse-tag)]
   ["Actions"
    ("g" "Refresh" tsar-refresh)
    ("q" "Quit" quit-window)]])

;;;; Public API

(defun tsar-refresh ()
  "Refresh the Tsar dashboard."
  (interactive)
  (when-let ((buf (get-buffer "*Tsar*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (insert (tsar--render-dashboard))
        (goto-char (min pos (point-max)))))))

;;;###autoload
(defun tsar ()
  "Open the Tsar deployment dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Tsar*")))
    (with-current-buffer buf
      (tsar-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (tsar--render-dashboard))))
    (pop-to-buffer buf)))

;;;; Keybindings

(with-eval-after-load 'config-ash-mode
  (define-key ashton-mode-map (kbd "C-c T") #'tsar)
  (define-key ashton-mode-map (kbd "C-c T T") #'tsar)
  (define-key ashton-mode-map (kbd "C-c T ?") #'tsar-dispatch)
  (define-key ashton-mode-map (kbd "C-c T s") #'tsar-promote-staging)
  (define-key ashton-mode-map (kbd "C-c T r") #'tsar-promote-prod)
  (define-key ashton-mode-map (kbd "C-c T o") #'tsar-open-url)
  (define-key ashton-mode-map (kbd "C-c T l") #'tsar-logs))

(provide 'config-tsar)

;;; config-tsar.el ends here
