;;; ~/.doom.d/config-testicular.el -*- lexical-binding: t; -*-

;; Testicular: Manual testing mode for stepping through test plans
;; Captures screenshots, tracks pass/fail, exports with base64 images
;;
;; Named after testing (obviously) - because good tests need... fortitude.

(require 'cl-lib)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hooks
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom testicular-start-hook nil
  "Hook run when testicular starts a test session.
Called with argument: (project-root)."
  :type 'hook
  :group 'testicular)

(defcustom testicular-complete-hook nil
  "Hook run when all tests are completed (passed or failed).
Called with arguments: (project-root passed-count failed-count total-count)."
  :type 'hook
  :group 'testicular)

(defcustom testicular-test-status-hook nil
  "Hook run when a test status changes.
Called with arguments: (project-root test-index status).
STATUS is 'passed, 'failed, or 'pending."
  :type 'hook
  :group 'testicular)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar testicular-tests nil "List of parsed test cases.")
(defvar testicular-current-index 0 "Current test index.")
(defvar testicular-results nil "Alist of (index . (:status :screenshots :notes)).")
(defvar testicular-plan-file nil "Path to the test plan file.")
(defvar testicular-screenshot-dir nil "Directory for screenshots.")
(defvar testicular-project-root nil "Project root for current test flow.")
(defvar testicular--return-buffer nil "Buffer to return to after Claude session.")
(defvar testicular--claude-buffer nil "Claude buffer spawned from testicular.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Environment Support
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom testicular-environments '("local" "staging" "prod")
  "List of available test environments."
  :type '(repeat string)
  :group 'testicular)

(defvar testicular-current-environment "local"
  "Current test environment.")

(defvar testicular-results-by-env nil
  "Hash table mapping environment -> results alist.
Allows tracking test results across multiple environments.")

(defun testicular--init-env-results ()
  "Initialize results storage for all environments."
  (setq testicular-results-by-env (make-hash-table :test 'equal))
  (dolist (env testicular-environments)
    (puthash env nil testicular-results-by-env)))

(defun testicular--get-env-results (env)
  "Get results for ENV, initializing if needed."
  (or (gethash env testicular-results-by-env)
      (let ((results nil))
        (dotimes (i (length testicular-tests))
          (push (cons i (list :status 'pending :screenshots nil :notes nil)) results))
        (setq results (nreverse results))
        (puthash env results testicular-results-by-env)
        results)))

(defun testicular--save-current-results ()
  "Save current results to the environment hash."
  (puthash testicular-current-environment testicular-results testicular-results-by-env))

(defun testicular--switch-to-env (env)
  "Switch to ENV, saving current results first."
  (testicular--save-current-results)
  (setq testicular-current-environment env)
  (setq testicular-results (testicular--get-env-results env)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; State Persistence
;;; ════════════════════════════════════════════════════════════════════════════

(defvar testicular-state-file ".test-evidence/results.json"
  "Relative path to state file within project root.")

(defun testicular--plan-hash ()
  "Return MD5 hash of current test plan file."
  (when (and testicular-plan-file (file-exists-p testicular-plan-file))
    (with-temp-buffer
      (insert-file-contents testicular-plan-file)
      (secure-hash 'md5 (buffer-string)))))

(defun testicular--state-file-path ()
  "Return absolute path to state file."
  (expand-file-name testicular-state-file testicular-project-root))

(defun testicular--results-to-alist (results)
  "Convert RESULTS to alist suitable for JSON encoding."
  (mapcar (lambda (r)
            (let ((idx (car r))
                  (data (cdr r)))
              `((index . ,idx)
                (status . ,(symbol-name (plist-get data :status)))
                (notes . ,(or (plist-get data :notes) ""))
                (screenshots . ,(or (plist-get data :screenshots) [])))))
          results))

(defun testicular--alist-to-results (alist)
  "Convert ALIST from JSON back to results format."
  (mapcar (lambda (item)
            (cons (alist-get 'index item)
                  (list :status (intern (alist-get 'status item))
                        :notes (let ((n (alist-get 'notes item)))
                                 (if (string-empty-p n) nil n))
                        :screenshots (let ((s (alist-get 'screenshots item)))
                                       (if (vectorp s) (append s nil) s)))))
          alist))

(defun testicular--save-state ()
  "Save current state to JSON file."
  (when testicular-project-root
    ;; Save current results to env hash first
    (testicular--save-current-results)
    (let* ((state-file (testicular--state-file-path))
           (state-dir (file-name-directory state-file))
           (env-data (make-hash-table :test 'equal)))
      ;; Build env data from hash table
      (dolist (env testicular-environments)
        (let ((results (gethash env testicular-results-by-env)))
          (when results
            (puthash env (testicular--results-to-alist results) env-data))))
      ;; Build state object
      (let ((state `((plan_hash . ,(testicular--plan-hash))
                     (current_environment . ,testicular-current-environment)
                     (current_index . ,testicular-current-index)
                     (environments . ,env-data))))
        ;; Ensure directory exists
        (make-directory state-dir t)
        ;; Write JSON
        (with-temp-file state-file
          (insert (json-encode state)))
        (message "Testicular state saved")))))

(defun testicular--load-state ()
  "Load state from JSON file. Returns t if loaded successfully, nil otherwise."
  (let ((state-file (testicular--state-file-path)))
    (when (file-exists-p state-file)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (state (json-read-file state-file))
                 (saved-hash (alist-get 'plan_hash state))
                 (current-hash (testicular--plan-hash)))
            ;; Check if plan has changed
            (if (not (string= saved-hash current-hash))
                (progn
                  (if (yes-or-no-p "Test plan has changed since last run. Load old results anyway? ")
                      (testicular--apply-loaded-state state)
                    (message "Starting fresh"))
                  nil)
              ;; Plan unchanged, load state
              (testicular--apply-loaded-state state)
              t))
        (error
         (message "Warning: Could not load state file: %s" (error-message-string err))
         nil)))))

(defun testicular--apply-loaded-state (state)
  "Apply loaded STATE to current session."
  (let ((envs (alist-get 'environments state))
        (saved-env (alist-get 'current_environment state))
        (saved-index (alist-get 'current_index state)))
    ;; Initialize env hash
    (testicular--init-env-results)
    ;; Load each environment's results
    (dolist (env-pair envs)
      (let ((env (symbol-name (car env-pair)))
            (results-alist (cdr env-pair)))
        (puthash env (testicular--alist-to-results results-alist) testicular-results-by-env)))
    ;; Restore current environment and index
    (setq testicular-current-environment (or saved-env "local"))
    (setq testicular-current-index (or saved-index 0))
    (setq testicular-results (testicular--get-env-results testicular-current-environment))
    (message "Loaded testicular state (%s environment, test %d)"
             testicular-current-environment (1+ testicular-current-index))))

(defvar testicular-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'testicular-next)
    (define-key map (kbd "p") #'testicular-prev)
    (define-key map (kbd "P") #'testicular-mark-passed)
    (define-key map (kbd "f") #'testicular-mark-failed)
    (define-key map (kbd "S") #'testicular-mark-skipped)
    (define-key map (kbd "s") #'testicular-screenshot-clipboard)
    (define-key map (kbd "F") #'testicular-screenshot-file)
    (define-key map (kbd "v") #'testicular-view-screenshots)
    (define-key map (kbd "N") #'testicular-edit-notes)
    (define-key map (kbd "E") #'testicular-switch-environment)
    (define-key map (kbd "e") #'testicular-export-to-file)
    (define-key map (kbd "r") #'testicular-refresh)
    (define-key map (kbd "g") #'testicular-refresh)
    (define-key map (kbd "RET") #'testicular-finish)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'testicular-help)
    ;; Service shortcuts for quick verification
    (define-key map (kbd "V") #'testicular-vercel)
    (define-key map (kbd "B") #'testicular-supabase)
    (define-key map (kbd "A") #'testicular-aws)
    (define-key map (kbd "O") #'testicular-open-url)
    map)
  "Keymap for testicular-mode.")

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
               ("staging" (read-string "Staging URL: " "https://staging."))
               ("prod" (read-string "Prod URL: " "https://")))))
    (browse-url url)))

(define-derived-mode testicular-mode special-mode "Testicular"
  "Mode for stepping through test cases."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-start (&optional environment)
  "Start testicular for current project.
With optional ENVIRONMENT, start in that environment (default: local).
Loads existing state if available and test plan hasn't changed."
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
         (plan-file (expand-file-name ".test-plan.md" root)))
    (unless (file-exists-p plan-file)
      (user-error "No .test-plan.md found. Run /test-plan in Claude first"))
    (setq testicular-plan-file plan-file)
    (setq testicular-project-root root)
    (setq testicular-screenshot-dir (expand-file-name ".test-evidence/screenshots" root))
    (make-directory testicular-screenshot-dir t)
    (setq testicular-tests (testicular-parse-plan plan-file))
    (setq testicular--return-buffer nil)
    (setq testicular--claude-buffer nil)
    ;; Try to load existing state
    (if (testicular--load-state)
        ;; State loaded successfully - override environment if specified
        (when environment
          (testicular--switch-to-env environment))
      ;; No state or plan changed - initialize fresh
      (testicular--init-env-results)
      (setq testicular-current-environment (or environment "local"))
      (setq testicular-current-index 0)
      (setq testicular-results nil)
      (dotimes (i (length testicular-tests))
        (push (cons i (list :status 'pending :screenshots nil :notes nil)) testicular-results))
      (setq testicular-results (nreverse testicular-results))
      (puthash testicular-current-environment testicular-results testicular-results-by-env))
    ;; Run start hook
    (run-hook-with-args 'testicular-start-hook root)
    (testicular-show-buffer)))

(defun testicular-parse-plan (file)
  "Parse test plan FILE into list of test alists."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string))
          (tests nil)
          (case-fold-search t))
      ;; Split by --- separator and find test sections
      (dolist (section (split-string content "\n---\n" t))
        (when (string-match "^## Test \\([0-9]+\\): \\(.+\\)" section)
          (let* ((num (match-string 1 section))
                 (name (string-trim (match-string 2 section)))
                 (body (substring section (match-end 0))))
            (push (list :number (string-to-number num)
                        :name name
                        :body (string-trim body))
                  tests))))
      (nreverse tests))))

(defun testicular-show-buffer ()
  "Display the testicular buffer."
  (let ((buf (get-buffer-create "*Testicular*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (testicular-mode)
        (testicular-render)))
    (pop-to-buffer buf)))

(defun testicular--count-by-status ()
  "Return plist with :passed :failed :skipped :pending counts."
  (let ((passed 0) (failed 0) (skipped 0) (pending 0))
    (dolist (r testicular-results)
      (pcase (plist-get (cdr r) :status)
        ('passed (cl-incf passed))
        ('failed (cl-incf failed))
        ('skipped (cl-incf skipped))
        ('pending (cl-incf pending))))
    (list :passed passed :failed failed :skipped skipped :pending pending)))

(defun testicular--all-resolved-p ()
  "Return t if all tests have been passed, failed, or skipped."
  (zerop (plist-get (testicular--count-by-status) :pending)))

(defun testicular--env-face (env)
  "Return face for environment ENV."
  (pcase env
    ("local" '(:foreground "green" :weight bold))
    ("staging" '(:foreground "yellow" :weight bold))
    ("prod" '(:foreground "red" :weight bold))
    (_ '(:foreground "cyan" :weight bold))))

(defun testicular--env-summary ()
  "Return summary of results across all environments."
  (let ((summary nil))
    (dolist (env testicular-environments)
      (let* ((results (gethash env testicular-results-by-env))
             (has-results (and results
                               (cl-some (lambda (r)
                                          (not (eq (plist-get (cdr r) :status) 'pending)))
                                        results))))
        (when has-results
          (let ((passed 0) (failed 0) (total 0))
            (dolist (r results)
              (cl-incf total)
              (pcase (plist-get (cdr r) :status)
                ('passed (cl-incf passed))
                ('failed (cl-incf failed))))
            (push (format "%s:%d/%d" env passed total) summary)))))
    (nreverse summary)))

(defun testicular-render ()
  "Render current test in buffer."
  (let ((inhibit-read-only t)
        (test (nth testicular-current-index testicular-tests))
        (result (cdr (assoc testicular-current-index testicular-results)))
        (total (length testicular-tests))
        (counts (testicular--count-by-status)))
    (erase-buffer)
    ;; Header with environment
    (insert (propertize "TESTICULAR" 'face 'bold))
    (insert "  ")
    (insert (propertize (format "[%s]" (upcase testicular-current-environment))
                        'face (testicular--env-face testicular-current-environment)))
    (insert "\n")
    (insert (format "Test %d of %d" (1+ testicular-current-index) total))
    (insert "  |  ")
    ;; Progress indicators
    (dotimes (i total)
      (let* ((r (cdr (assoc i testicular-results)))
             (status (plist-get r :status))
             (face (pcase status
                     ('passed '(:foreground "green"))
                     ('failed '(:foreground "red"))
                     ('skipped '(:foreground "blue"))
                     (_ '(:foreground "gray")))))
        (insert (propertize (if (= i testicular-current-index) "[*]" "[ ]") 'face face))))
    ;; Summary
    (insert (format "  (%d/%d/%d)"
                    (plist-get counts :passed)
                    (plist-get counts :failed)
                    (plist-get counts :skipped)))
    ;; Cross-environment summary
    (let ((env-summary (testicular--env-summary)))
      (when (> (length env-summary) 1)
        (insert "\n")
        (insert (propertize "Environments: " 'face 'font-lock-comment-face))
        (insert (mapconcat #'identity env-summary "  "))))
    (insert "\n\n")
    (insert (make-string 60 ?-) "\n\n")
    ;; Test content
    (when test
      (let ((status (plist-get result :status))
            (screenshots (plist-get result :screenshots))
            (notes (plist-get result :notes)))
        (insert (propertize (format "## Test %d: %s"
                                    (plist-get test :number)
                                    (plist-get test :name))
                            'face '(:weight bold :height 1.2)))
        (insert "  ")
        (insert (propertize (format "[%s]" (upcase (symbol-name status)))
                            'face (pcase status
                                    ('passed '(:foreground "green" :weight bold))
                                    ('failed '(:foreground "red" :weight bold))
                                    ('skipped '(:foreground "blue" :weight bold))
                                    (_ '(:foreground "yellow")))))
        (insert "\n\n")
        (insert (plist-get test :body))
        (insert "\n\n")
        ;; Notes section
        (when (and notes (not (string-empty-p notes)))
          (insert (make-string 40 ?-) "\n")
          (insert (propertize "Notes:\n" 'face '(:foreground "cyan" :weight bold)))
          (insert notes)
          (insert "\n\n"))
        ;; Screenshots
        (when screenshots
          (insert (make-string 40 ?-) "\n")
          (insert (propertize (format "Screenshots: %d attached\n" (length screenshots))
                              'face 'font-lock-comment-face))
          (dolist (s screenshots)
            (insert (format "  - %s\n" (file-name-nondirectory s)))))))
    (insert "\n" (make-string 60 ?-) "\n\n")
    ;; Show completion prompt if all resolved
    (if (testicular--all-resolved-p)
        (progn
          (insert (propertize "All tests resolved! " 'face '(:foreground "green" :weight bold)))
          (insert (propertize "Press RET to finish and create PR\n\n" 'face 'bold)))
      (insert ""))
    (insert (propertize "Keys: " 'face 'bold))
    (insert "n/p:nav  P:pass  f:fail  S:skip  s:screenshot  N:notes  E:env  ?:help")
    (goto-char (point-min))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Navigation
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-next ()
  "Go to next test."
  (interactive)
  (when (< testicular-current-index (1- (length testicular-tests)))
    (cl-incf testicular-current-index)
    (testicular-render)))

(defun testicular-prev ()
  "Go to previous test."
  (interactive)
  (when (> testicular-current-index 0)
    (cl-decf testicular-current-index)
    (testicular-render)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Status Management
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--set-status (status)
  "Set STATUS for current test and run hooks."
  (let ((result (assoc testicular-current-index testicular-results)))
    (setcdr result (plist-put (cdr result) :status status)))
  ;; Run status change hook
  (run-hook-with-args 'testicular-test-status-hook
                      testicular-project-root
                      testicular-current-index
                      status)
  ;; Auto-save state
  (testicular--save-state)
  ;; Check if all tests are complete
  (testicular--check-completion))

(defun testicular--check-completion ()
  "Check if all tests are complete and run completion hook if so."
  (let ((passed 0)
        (failed 0)
        (pending 0))
    (dolist (r testicular-results)
      (pcase (plist-get (cdr r) :status)
        ('passed (cl-incf passed))
        ('failed (cl-incf failed))
        ('pending (cl-incf pending))))
    (when (zerop pending)
      (run-hook-with-args 'testicular-complete-hook
                          testicular-project-root
                          passed
                          failed
                          (length testicular-results)))))

(defun testicular-mark-passed ()
  "Mark current test as passed and advance."
  (interactive)
  (testicular--set-status 'passed)
  (testicular-render)
  (when (< testicular-current-index (1- (length testicular-tests)))
    (testicular-next)))

(defun testicular-mark-failed ()
  "Mark current test as failed and open Claude to fix."
  (interactive)
  (testicular--set-status 'failed)
  (testicular-render)
  ;; Open Claude in same window
  (testicular--open-claude-for-fix))

(defun testicular-mark-skipped ()
  "Mark current test as skipped and advance."
  (interactive)
  (testicular--set-status 'skipped)
  (testicular-render)
  (when (< testicular-current-index (1- (length testicular-tests)))
    (testicular-next)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Notes
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--get-notes ()
  "Get notes for current test."
  (let ((result (cdr (assoc testicular-current-index testicular-results))))
    (or (plist-get result :notes) "")))

(defun testicular--set-notes (notes)
  "Set NOTES for current test."
  (let ((result (assoc testicular-current-index testicular-results)))
    (setcdr result (plist-put (cdr result) :notes notes))))

(defun testicular-edit-notes ()
  "Edit notes for current test.
Opens a buffer to edit multiline notes."
  (interactive)
  (let* ((test (nth testicular-current-index testicular-tests))
         (test-name (plist-get test :name))
         (current-notes (testicular--get-notes))
         (buf (get-buffer-create "*Testicular Notes*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert current-notes)
      (goto-char (point-min))
      (setq-local testicular--notes-test-index testicular-current-index)
      (setq-local header-line-format
                  (format " Notes for Test %d: %s  |  C-c C-c: save  C-c C-k: cancel"
                          (1+ testicular-current-index) test-name))
      (local-set-key (kbd "C-c C-c") #'testicular--save-notes)
      (local-set-key (kbd "C-c C-k") #'testicular--cancel-notes)
      (text-mode))
    (pop-to-buffer buf)))

(defun testicular--save-notes ()
  "Save notes and return to testicular."
  (interactive)
  (let ((notes (buffer-string))
        (test-index (buffer-local-value 'testicular--notes-test-index (current-buffer))))
    (kill-buffer)
    ;; Update notes in results
    (let ((result (assoc test-index testicular-results)))
      (setcdr result (plist-put (cdr result) :notes (string-trim notes))))
    ;; Auto-save state
    (testicular--save-state)
    (testicular-render)
    (message "Notes saved")))

(defun testicular--cancel-notes ()
  "Cancel notes editing."
  (interactive)
  (kill-buffer)
  (message "Notes cancelled"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Environment Switching
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-switch-environment ()
  "Switch to a different test environment.
Results are preserved per-environment."
  (interactive)
  (let* ((choices (mapcar (lambda (env)
                            (let* ((results (gethash env testicular-results-by-env))
                                   (status (if results
                                               (let ((passed 0) (total 0))
                                                 (dolist (r results)
                                                   (cl-incf total)
                                                   (when (eq (plist-get (cdr r) :status) 'passed)
                                                     (cl-incf passed)))
                                                 (format " (%d/%d)" passed total))
                                             " (new)")))
                              (cons (concat env status) env)))
                          testicular-environments))
         (selected (completing-read
                    (format "Switch environment (current: %s): " testicular-current-environment)
                    choices nil t))
         (env (cdr (assoc selected choices))))
    (when (and env (not (string= env testicular-current-environment)))
      (testicular--switch-to-env env)
      ;; Auto-save state after switch
      (testicular--save-state)
      (testicular-render)
      (message "Switched to %s environment" env))))

(defun testicular-start-staging ()
  "Start testicular in staging environment."
  (interactive)
  (testicular-start "staging"))

(defun testicular-start-prod ()
  "Start testicular in prod environment."
  (interactive)
  (testicular-start "prod"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Screenshots
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-screenshot-clipboard ()
  "Capture screenshot from clipboard (Fireshot)."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "test-%d-%s.png"
                           (1+ testicular-current-index)
                           timestamp))
         (filepath (expand-file-name filename testicular-screenshot-dir)))
    (if (zerop (call-process "xclip" nil nil nil
                             "-selection" "clipboard"
                             "-t" "image/png"
                             "-o"))
        (progn
          (with-temp-file filepath
            (set-buffer-multibyte nil)
            (call-process "xclip" nil t nil
                          "-selection" "clipboard"
                          "-t" "image/png"
                          "-o"))
          (testicular--add-screenshot filepath)
          (testicular-render)
          (message "Screenshot saved: %s" filename))
      (user-error "No image in clipboard"))))

(defun testicular-screenshot-file ()
  "Attach screenshot from file."
  (interactive)
  (let ((file (read-file-name "Screenshot file: " "~/screenshots/")))
    (when (and file (file-exists-p file))
      (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (ext (file-name-extension file))
             (filename (format "test-%d-%s.%s"
                               (1+ testicular-current-index)
                               timestamp
                               ext))
             (filepath (expand-file-name filename testicular-screenshot-dir)))
        (copy-file file filepath t)
        (testicular--add-screenshot filepath)
        (testicular-render)
        (message "Screenshot attached: %s" filename)))))

(defun testicular--add-screenshot (filepath)
  "Add FILEPATH to current test's screenshots."
  (let* ((result (assoc testicular-current-index testicular-results))
         (screenshots (plist-get (cdr result) :screenshots)))
    (setcdr result (plist-put (cdr result) :screenshots
                              (append screenshots (list filepath)))))
  ;; Auto-save state
  (testicular--save-state))

(defun testicular-view-screenshots ()
  "View screenshots for current test."
  (interactive)
  (let* ((result (cdr (assoc testicular-current-index testicular-results)))
         (screenshots (plist-get result :screenshots)))
    (if screenshots
        (dolist (s screenshots)
          (find-file-other-window s))
      (message "No screenshots attached"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Export & Finish
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-export-to-file ()
  "Export test results to .test-results.md in project root."
  (interactive)
  (let* ((output (testicular--generate-export))
         (results-file (expand-file-name ".test-results.md" testicular-project-root)))
    (with-temp-file results-file
      (insert output))
    (message "Exported to %s" results-file)))

(defun testicular--generate-export ()
  "Generate markdown export of test results."
  (with-temp-buffer
    (let ((counts (testicular--count-by-status)))
      (insert "# Test Results\n\n")
      (insert (format "**Environment:** %s\n\n" testicular-current-environment))
      (insert (format "- **Passed:** %d\n" (plist-get counts :passed)))
      (insert (format "- **Failed:** %d\n" (plist-get counts :failed)))
      (insert (format "- **Skipped:** %d\n\n" (plist-get counts :skipped))))
    (dotimes (i (length testicular-tests))
      (let* ((test (nth i testicular-tests))
             (result (cdr (assoc i testicular-results)))
             (status (plist-get result :status))
             (screenshots (plist-get result :screenshots))
             (notes (plist-get result :notes))
             (icon (pcase status
                     ('passed ":white_check_mark:")
                     ('failed ":x:")
                     ('skipped ":fast_forward:")
                     (_ ":hourglass:"))))
        (insert (format "## %s Test %d: %s\n\n"
                        icon
                        (plist-get test :number)
                        (plist-get test :name)))
        ;; Notes
        (when (and notes (not (string-empty-p notes)))
          (insert "**Notes:**\n")
          (insert notes)
          (insert "\n\n"))
        ;; Reference screenshots by relative path (for GitHub)
        (when screenshots
          (insert "**Screenshots:**\n")
          (dolist (s screenshots)
            (let ((rel-path (file-relative-name s testicular-project-root)))
              (insert (format "- `%s`\n" rel-path))))
          (insert "\n"))
        (insert "\n")))
    (buffer-string)))

(defun testicular--generate-multi-env-export ()
  "Generate markdown export with results from all tested environments."
  (with-temp-buffer
    (insert "# Test Results\n\n")
    ;; Summary across environments
    (insert "## Environment Summary\n\n")
    (insert "| Environment | Passed | Failed | Skipped |\n")
    (insert "|-------------|--------|--------|----------|\n")
    (dolist (env testicular-environments)
      (let* ((results (gethash env testicular-results-by-env))
             (has-results (and results
                               (cl-some (lambda (r)
                                          (not (eq (plist-get (cdr r) :status) 'pending)))
                                        results))))
        (when has-results
          (let ((passed 0) (failed 0) (skipped 0))
            (dolist (r results)
              (pcase (plist-get (cdr r) :status)
                ('passed (cl-incf passed))
                ('failed (cl-incf failed))
                ('skipped (cl-incf skipped))))
            (insert (format "| %s | %d | %d | %d |\n" env passed failed skipped))))))
    (insert "\n")
    ;; Detailed results per environment
    (dolist (env testicular-environments)
      (let* ((results (gethash env testicular-results-by-env))
             (has-results (and results
                               (cl-some (lambda (r)
                                          (not (eq (plist-get (cdr r) :status) 'pending)))
                                        results))))
        (when has-results
          (insert (format "## %s Environment\n\n" (capitalize env)))
          (dotimes (i (length testicular-tests))
            (let* ((test (nth i testicular-tests))
                   (result (cdr (assoc i results)))
                   (status (plist-get result :status))
                   (notes (plist-get result :notes))
                   (icon (pcase status
                           ('passed ":white_check_mark:")
                           ('failed ":x:")
                           ('skipped ":fast_forward:")
                           (_ ":hourglass:"))))
              (insert (format "### %s Test %d: %s\n\n"
                              icon
                              (plist-get test :number)
                              (plist-get test :name)))
              (when (and notes (not (string-empty-p notes)))
                (insert notes)
                (insert "\n\n"))))
          (insert "\n"))))
    (buffer-string)))

(defun testicular-finish ()
  "Finish testing: export results, commit, push, and create PR.
No prompts - just ships it."
  (interactive)
  (let ((counts (testicular--count-by-status)))
    (if (not (testicular--all-resolved-p))
        (user-error "Not all tests resolved. %d pending" (plist-get counts :pending))
      ;; Note failures in message but don't block
      (when (> (plist-get counts :failed) 0)
        (message "Note: %d test(s) failed - shipping anyway" (plist-get counts :failed)))
      ;; Export results to file
      (testicular-export-to-file)
      ;; Run completion hook for orchard integration
      (run-hook-with-args 'testicular-complete-hook
                          testicular-project-root
                          (plist-get counts :passed)
                          (plist-get counts :failed)
                          (length testicular-results))
      ;; Ship it - commit, push, create PR
      (testicular--finish-workflow counts))))

(defun testicular--finish-workflow (counts)
  "Run the finish workflow: commit, push, PR.
COUNTS is plist with test result counts.
No prompts - just ships it."
  (let ((default-directory testicular-project-root))
    ;; Stage test evidence
    (when (file-exists-p (expand-file-name ".test-results.md" testicular-project-root))
      (shell-command "git add .test-results.md"))
    (when (file-directory-p (expand-file-name ".test-evidence" testicular-project-root))
      (shell-command "git add .test-evidence/"))
    ;; Check if there's anything to commit
    (let ((staged (string-trim (shell-command-to-string "git diff --cached --name-only"))))
      (if (string-empty-p staged)
          (message "No test evidence to commit - pushing and creating PR...")
        ;; Commit test results
        (let ((commit-msg (format "Add test results: %d passed, %d failed, %d skipped"
                                  (plist-get counts :passed)
                                  (plist-get counts :failed)
                                  (plist-get counts :skipped))))
          (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
          (message "Committed test results"))))
    ;; Ship it - no questions asked
    (testicular--push-and-pr)))

(defun testicular--push-and-pr ()
  "Push current branch and create PR. No prompts."
  (let* ((default-directory testicular-project-root)
         (branch (string-trim (shell-command-to-string "git branch --show-current"))))
    ;; Refuse to ship main/dev
    (when (member branch '("dev" "main" "master"))
      (user-error "Won't ship directly to %s" branch))
    ;; Push
    (message "Pushing %s..." branch)
    (shell-command (format "git push -u origin %s" (shell-quote-argument branch)))
    ;; Get PR title from branch name
    (let* ((title (replace-regexp-in-string "^[A-Z]+[-/]" "" branch))
           (title (replace-regexp-in-string "-" " " title))
           (title (capitalize title))
           ;; Auto-generate description with test results
           (description (testicular--generate-pr-body)))
      ;; Create PR
      (let ((pr-cmd (format "gh pr create --title %s --body %s --base dev 2>&1"
                            (shell-quote-argument title)
                            (shell-quote-argument description))))
        (let ((result (shell-command-to-string pr-cmd)))
          (if (string-match-p "https://github" result)
              (let ((pr-url (string-trim result)))
                ;; Save PR URL
                (with-temp-file (expand-file-name ".pr-url" testicular-project-root)
                  (insert pr-url))
                (message "PR created: %s" pr-url)
                ;; Auto-open in browser
                (browse-url pr-url))
            (message "PR creation output: %s" result)))))))

(defun testicular--generate-pr-body ()
  "Generate default PR body from test results."
  (let ((counts (testicular--count-by-status)))
    (format "## Test Results\n\n- Passed: %d\n- Failed: %d\n- Skipped: %d\n\nSee `.test-results.md` for details."
            (plist-get counts :passed)
            (plist-get counts :failed)
            (plist-get counts :skipped))))

(defun testicular-refresh ()
  "Refresh the display."
  (interactive)
  (testicular-render))

(defun testicular-help ()
  "Show detailed help for testicular."
  (interactive)
  (with-output-to-temp-buffer "*Testicular Help*"
    (princ "TESTICULAR - Manual Test Runner\n")
    (princ "================================\n\n")
    (princ "NAVIGATION\n")
    (princ "  n/p     Move to next/previous test\n\n")
    (princ "STATUS\n")
    (princ "  P       Mark PASSED and advance\n")
    (princ "  f       Mark FAILED and open Claude to fix\n")
    (princ "  S       Mark SKIPPED and advance\n\n")
    (princ "NOTES & ENVIRONMENT\n")
    (princ "  N       Edit notes for current test (multiline, C-c C-c to save)\n")
    (princ "  E       Switch environment (local/staging/prod)\n")
    (princ "          Results are preserved per-environment\n\n")
    (princ "SCREENSHOTS (optional, multiple per test allowed)\n")
    (princ "  s       Capture from clipboard (take shot with Flameshot etc, then press s)\n")
    (princ "  F       Attach from file (prompts for path)\n")
    (princ "  v       View attached screenshots for current test\n")
    (princ "          Screenshots saved to .test-evidence/screenshots/\n\n")
    (princ "SERVICES (for verification during testing)\n")
    (princ "  V       Vercel menu (deployments, logs)\n")
    (princ "  B       Supabase menu (database, functions)\n")
    (princ "  A       AWS menu (lambda, s3, cloudwatch)\n")
    (princ "  O       Open URL (environment-aware: local/staging/prod)\n\n")
    (princ "COMPLETION\n")
    (princ "  e       Export results to .test-results.md\n")
    (princ "  RET     Finish: export, commit evidence, push, create PR\n")
    (princ "          (only available when all tests resolved)\n\n")
    (princ "OTHER\n")
    (princ "  g/r     Refresh display\n")
    (princ "  q       Quit testicular\n")
    (princ "  ?       This help\n\n")
    (princ "WORKFLOW\n")
    (princ "  1. Start with C-c T (defaults to 'local' environment)\n")
    (princ "  2. Step through tests with n/p\n")
    (princ "  3. Mark each: P=pass, f=fail (opens Claude), S=skip\n")
    (princ "  4. Press N to add notes explaining pass/fail reasoning\n")
    (princ "  5. Use V/B/A to check services during testing\n")
    (princ "  6. Optionally attach screenshots with s or F\n")
    (princ "  7. Press E to switch to staging/prod and re-run tests\n")
    (princ "  8. When all resolved, press RET to finish and create PR\n\n")
    (princ "MULTI-ENVIRONMENT TESTING\n")
    (princ "  - Same test plan, different environments\n")
    (princ "  - Results tracked separately per environment\n")
    (princ "  - Cross-env summary shown when multiple envs tested\n")
    (princ "  - Export includes all environment results\n")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Integration - Drop into Claude on failure, return on close
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--open-claude-for-fix ()
  "Open Claude in same window to fix the failed test."
  (let* ((test (nth testicular-current-index testicular-tests))
         (test-name (plist-get test :name))
         (test-body (plist-get test :body))
         (original-window (selected-window)))
    ;; Save current buffer to return to
    (setq testicular--return-buffer (current-buffer))
    ;; Ensure claude-code is loaded
    (unless (featurep 'vterm)
      (require 'vterm))
    (unless (featurep 'claude-code)
      (require 'claude-code))
    ;; Start Claude in the project root
    (let ((default-directory testicular-project-root))
      (if (fboundp 'claude-code)
          (progn
            ;; Call claude-code - it will create/switch to the buffer
            (claude-code)
            ;; Force the claude buffer into the original window (where Testicular was)
            (let ((claude-buf (current-buffer)))
              (setq testicular--claude-buffer claude-buf)
              ;; Delete any extra windows claude may have created and show in original
              (select-window original-window)
              (switch-to-buffer claude-buf)
              ;; Clean up other windows showing this buffer
              (delete-other-windows-on claude-buf)
              ;; Add hook to return when Claude buffer is killed
              (with-current-buffer claude-buf
                (add-hook 'kill-buffer-hook #'testicular--on-claude-kill nil t))
              ;; Send context about the failed test
              (run-at-time 1.5 nil
                           (lambda ()
                             (when (and (fboundp 'claude-code-send-command)
                                        testicular--claude-buffer
                                        (buffer-live-p testicular--claude-buffer))
                               (with-current-buffer testicular--claude-buffer
                                 (claude-code-send-command
                                  (format "Test failed: %s\n\n%s\n\nPlease help me fix this."
                                          test-name test-body))))))))
        (user-error "claude-code not available")))))

(defun testicular--on-claude-kill ()
  "Hook called when Claude buffer is killed. Return to testicular."
  (when (and testicular--return-buffer
             (buffer-live-p testicular--return-buffer))
    ;; Use run-at-time to switch after the kill completes
    (let ((return-buf testicular--return-buffer))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p return-buf)
                       (switch-to-buffer return-buf)
                       (message "Back to Testicular. P:pass if fixed, f:fail again to retry")))))))

(defun testicular-return ()
  "Manually return to testicular from Claude."
  (interactive)
  (if (and testicular--return-buffer
           (buffer-live-p testicular--return-buffer))
      (progn
        (switch-to-buffer testicular--return-buffer)
        (message "Back to Testicular"))
    (if-let ((buf (get-buffer "*Testicular*")))
        (switch-to-buffer buf)
      (user-error "No testicular session active"))))

;; Advice to catch C-x 0 (delete-window) when in testicular Claude session
(defun testicular--around-delete-window (orig-fun &optional window)
  "Return to testicular when closing Claude window from test session."
  (let ((buf (window-buffer (or window (selected-window)))))
    (if (and testicular--claude-buffer
             (eq buf testicular--claude-buffer)
             testicular--return-buffer
             (buffer-live-p testicular--return-buffer))
        ;; We're closing the testicular Claude window - switch to testicular instead
        (progn
          (switch-to-buffer testicular--return-buffer)
          (message "Back to Testicular. P:pass if fixed, f:fail to retry with Claude"))
      ;; Normal delete-window behavior
      (funcall orig-fun window))))

(advice-add 'delete-window :around #'testicular--around-delete-window)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Global binding
(global-set-key (kbd "C-c T") #'testicular-start)

(provide 'config-testicular)
;;; config-testicular.el ends here
