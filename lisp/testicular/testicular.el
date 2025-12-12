;;; testicular.el --- Manual test runner for stepping through test plans -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ashton Honnecke
;;
;; Author: Ashton Honnecke <ashton@pixelstub.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: tools, testing
;; URL: https://github.com/ahonnecke/testicular
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Testicular is a manual testing mode for stepping through test plans.
;; It captures screenshots, tracks pass/fail status, and can integrate
;; with your PR workflow.
;;
;; Features:
;; - Parse markdown test plans
;; - Step through tests with keyboard navigation
;; - Mark tests as passed/failed/skipped
;; - Attach screenshots from clipboard or file
;; - Track results across multiple environments
;; - Export results to markdown
;; - Pluggable PR creation backends
;;
;; Usage:
;;   M-x testicular-start    - Start testing in current project
;;   M-x testicular-init-project - Initialize testicular for a new project
;;
;;; Code:

(require 'cl-lib)
(require 'json)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Customization
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup testicular nil
  "Manual test runner."
  :group 'tools
  :prefix "testicular-")

(defcustom testicular-base-branch "main"
  "Base branch for comparing changes."
  :type 'string
  :group 'testicular)

(defcustom testicular-pr-remote "origin"
  "Git remote for pushing and creating PRs."
  :type 'string
  :group 'testicular)

(defcustom testicular-pr-base-branch "main"
  "Base branch for PRs."
  :type 'string
  :group 'testicular)

(defcustom testicular-pr-backend 'github
  "Backend for creating PRs.
Available backends: `github', `gitlab', `none'."
  :type '(choice (const :tag "GitHub (gh)" github)
                 (const :tag "GitLab (glab)" gitlab)
                 (const :tag "No PR creation" none))
  :group 'testicular)

(defcustom testicular-environments '("local" "staging" "prod")
  "List of available test environments."
  :type '(repeat string)
  :group 'testicular)

(defcustom testicular-default-environment "local"
  "Default environment when starting tests."
  :type 'string
  :group 'testicular)

(defcustom testicular-plan-filenames '(".test-plan.md" "test-plan.md")
  "Filenames to search for test plans, in order of preference."
  :type '(repeat string)
  :group 'testicular)

(defcustom testicular-evidence-dir ".test-evidence"
  "Directory for test evidence (relative to project root)."
  :type 'string
  :group 'testicular)

(defcustom testicular-results-filename ".test-results.md"
  "Filename for exported test results."
  :type 'string
  :group 'testicular)

(defcustom testicular-screenshot-command "xclip"
  "Command for getting images from clipboard."
  :type 'string
  :group 'testicular)

(defcustom testicular-open-browser-on-pr t
  "Whether to open browser when PR is created."
  :type 'boolean
  :group 'testicular)

(defcustom testicular-project-root-function #'testicular--default-project-root
  "Function to determine project root.
Should return a directory path."
  :type 'function
  :group 'testicular)

(defcustom testicular-fail-opens-claude t
  "Whether marking a test as failed opens Claude."
  :type 'boolean
  :group 'testicular)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hooks
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom testicular-start-hook nil
  "Hook run when testicular starts a test session.
Called with argument: (project-root)."
  :type 'hook
  :group 'testicular)

(defcustom testicular-complete-hook nil
  "Hook run when all tests are completed.
Called with arguments: (project-root passed-count failed-count total-count)."
  :type 'hook
  :group 'testicular)

(defcustom testicular-test-status-hook nil
  "Hook run when a test status changes.
Called with arguments: (project-root test-index status).
STATUS is `passed', `failed', or `pending'."
  :type 'hook
  :group 'testicular)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Internal Variables
;;; ════════════════════════════════════════════════════════════════════════════

(defvar testicular-tests nil "List of parsed test cases.")
(defvar testicular-current-index 0 "Current test index.")
(defvar testicular-results nil "Alist of (index . (:status :screenshots :notes)).")
(defvar testicular-plan-file nil "Path to the test plan file.")
(defvar testicular-screenshot-dir nil "Directory for screenshots.")
(defvar testicular-project-root nil "Project root for current test flow.")
(defvar testicular-current-environment nil "Current test environment.")
(defvar testicular-results-by-env nil "Hash table mapping environment -> results.")
(defvar testicular--return-buffer nil "Buffer to return to after Claude session.")
(defvar testicular--claude-buffer nil "Claude buffer spawned from testicular.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Format Specification
;;; ════════════════════════════════════════════════════════════════════════════

(defconst testicular-format-template
  "# Test Plan: <Brief Feature Description>

Branch: `<branch-name>`
Generated: <YYYY-MM-DD>

## Summary
- [ ] Test 1: <name>
- [ ] Test 2: <name>
- [ ] Test 3: <name>

---

## Test 1: <Short Test Name>

**Preconditions:**
- Any setup required before testing

**Steps:**
1. First action to take
2. Second action to take
3. Continue as needed

**Expected Result:**
What should happen when the test passes

---

## Test 2: <Short Test Name>

**Preconditions:**
- Setup if any

**Steps:**
1. Action
2. Action

**Expected Result:**
Expected outcome

---

## Test 3: <Short Test Name>

**Preconditions:**
- None

**Steps:**
1. Action

**Expected Result:**
Expected outcome

---
"
  "Template showing the expected test plan format.")

(defconst testicular-claude-command-template
  "# Generate Test Plan

<!-- AUTO-GENERATED by testicular-export-claude-command -->
<!-- Do not edit manually - changes will be overwritten -->

Generate a test plan for the current branch's changes.

**Usage:** `/test-plan`

## Instructions

1. First, understand what changed on this branch:
   ```bash
   git log --oneline %s..HEAD
   git diff %s...HEAD --stat
   ```

2. Read the relevant changed files to understand the feature/fix.

3. Generate a test plan and write it to `.test-plan.md` in the repo root.

## Test Plan Format

**IMPORTANT:** The format below is parsed by Emacs testicular-mode. Follow it exactly.

```markdown
%s
```

## Format Rules (MUST FOLLOW)

1. **Test headings:** Use exactly `## Test N: Name` format (e.g., `## Test 1: Login Flow`)
2. **Separator:** Use `---` on its own line between each test section
3. **Numbering:** Sequential starting from 1
4. **Sections:** Include Preconditions, Steps, and Expected Result for each test

## Guidelines

- Be explicit about ALL setup steps (e.g., `make dev`, database seeding, URLs)
- Each test should verify behavior changed/added by this branch
- Skip exhaustive edge cases (save for unit tests)
- Do NOT test functionality that didn't change
- 3-7 tests is typical; fewer for small changes, more for large features
- Include specific URLs, commands, UI elements to check

After writing the file, report:
```
Test plan written to .test-plan.md with N test cases.
Run C-c T in Emacs to start testicular.
```
"
  "Template for the Claude command.
First two %s are replaced with base branch, third with format template.")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Project Root Detection
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--default-project-root ()
  "Default project root detection using projectile or git."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun testicular--project-root ()
  "Get project root using configured function."
  (funcall testicular-project-root-function))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Test Plan Finding and Parsing
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--find-plan-file (root)
  "Find test plan file in ROOT using configured filenames."
  (cl-loop for name in testicular-plan-filenames
           for path = (expand-file-name name root)
           when (file-exists-p path)
           return path))

(defun testicular-parse-plan (file)
  "Parse test plan FILE into list of test alists.
Accepts multiple formats:
  - ## Test 1: Name
  - ## 1. Name
  - ## 1: Name"
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string))
          (tests nil)
          (case-fold-search t))
      (dolist (section (split-string content "\n---\n" t))
        (when (or (string-match "^## Test \\([0-9]+\\)[.:] *\\(.+\\)" section)
                  (string-match "^## \\([0-9]+\\)[.:] *\\(.+\\)" section))
          (let* ((num (match-string 1 section))
                 (name (string-trim (match-string 2 section)))
                 (body (substring section (match-end 0))))
            (push (list :number (string-to-number num)
                        :name name
                        :body (string-trim body))
                  tests))))
      (nreverse tests))))

(defun testicular-validate-plan (&optional file)
  "Validate that FILE will parse correctly.
Returns list of issues, or nil if valid."
  (interactive)
  (let* ((root (testicular--project-root))
         (plan-file (or file (testicular--find-plan-file root)))
         (issues nil))
    (unless plan-file
      (user-error "No test plan file found"))
    (with-temp-buffer
      (insert-file-contents plan-file)
      (let ((content (buffer-string))
            (test-count 0))
        (unless (string-match-p "\n---\n" content)
          (push "Missing `---` separators between test sections" issues))
        (dolist (section (split-string content "\n---\n" t))
          (when (or (string-match "^## Test \\([0-9]+\\)[.:] *\\(.+\\)" section)
                    (string-match "^## \\([0-9]+\\)[.:] *\\(.+\\)" section))
            (cl-incf test-count)))
        (when (zerop test-count)
          (push "No test headings found. Use format: `## Test 1: Name`" issues))
        (let ((numbers nil))
          (dolist (section (split-string content "\n---\n" t))
            (when (or (string-match "^## Test \\([0-9]+\\)" section)
                      (string-match "^## \\([0-9]+\\)" section))
              (push (string-to-number (match-string 1 section)) numbers)))
          (setq numbers (sort numbers #'<))
          (when (and numbers (not (equal numbers (number-sequence 1 (length numbers)))))
            (push (format "Test numbers should be sequential 1-%d, got: %s"
                          (length numbers) numbers) issues)))))
    (if issues
        (progn
          (message "Validation failed:\n- %s" (string-join issues "\n- "))
          issues)
      (message "Test plan is valid!")
      nil)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Project Initialization
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-init-project ()
  "Initialize testicular for the current project.
Creates .claude/commands/test-plan.md and optionally .dir-locals.el."
  (interactive)
  (let* ((root (testicular--project-root))
         (claude-dir (expand-file-name ".claude/commands" root))
         (command-file (expand-file-name "test-plan.md" claude-dir))
         (dir-locals-file (expand-file-name ".dir-locals.el" root)))
    ;; Create Claude command
    (make-directory claude-dir t)
    (testicular-export-claude-command command-file)
    (message "Created %s" command-file)
    ;; Optionally create .dir-locals.el
    (when (and (not (file-exists-p dir-locals-file))
               (yes-or-no-p "Create .dir-locals.el with testicular settings? "))
      (let ((base (read-string "PR base branch: " testicular-pr-base-branch))
            (remote (read-string "Git remote: " testicular-pr-remote)))
        (with-temp-file dir-locals-file
          (insert ";;; Directory Local Variables\n")
          (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
          (insert (format "((nil . ((testicular-pr-base-branch . %S)\n" base))
          (insert (format "         (testicular-pr-remote . %S)\n" remote))
          (insert (format "         (testicular-base-branch . %S))))\n"
                          (concat remote "/" base))))
        (message "Created %s" dir-locals-file)))
    ;; Create sample test plan if none exists
    (unless (testicular--find-plan-file root)
      (when (yes-or-no-p "Create sample .test-plan.md? ")
        (with-temp-file (expand-file-name ".test-plan.md" root)
          (insert testicular-format-template))
        (message "Created sample .test-plan.md")))
    (message "Testicular initialized for %s" root)))

(defun testicular-export-claude-command (&optional target-file)
  "Export the Claude command to TARGET-FILE."
  (interactive
   (list (read-file-name
          "Export Claude command to: "
          (expand-file-name ".claude/commands/" (testicular--project-root))
          nil nil "test-plan.md")))
  (let ((content (format testicular-claude-command-template
                         testicular-base-branch
                         testicular-base-branch
                         (string-trim testicular-format-template))))
    (with-temp-file target-file
      (insert content))
    (message "Exported testicular Claude command to %s" target-file)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Environment Support
;;; ════════════════════════════════════════════════════════════════════════════

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

(defun testicular--state-file-path ()
  "Return absolute path to state file."
  (expand-file-name (concat testicular-evidence-dir "/results.json")
                    testicular-project-root))

(defun testicular--plan-hash ()
  "Return MD5 hash of current test plan file."
  (when (and testicular-plan-file (file-exists-p testicular-plan-file))
    (with-temp-buffer
      (insert-file-contents testicular-plan-file)
      (secure-hash 'md5 (buffer-string)))))

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
    (testicular--save-current-results)
    (let* ((state-file (testicular--state-file-path))
           (state-dir (file-name-directory state-file))
           (env-data (make-hash-table :test 'equal)))
      (dolist (env testicular-environments)
        (let ((results (gethash env testicular-results-by-env)))
          (when results
            (puthash env (testicular--results-to-alist results) env-data))))
      (let ((state `((plan_hash . ,(testicular--plan-hash))
                     (current_environment . ,testicular-current-environment)
                     (current_index . ,testicular-current-index)
                     (environments . ,env-data))))
        (make-directory state-dir t)
        (with-temp-file state-file
          (insert (json-encode state)))
        (message "Testicular state saved")))))

(defun testicular--load-state ()
  "Load state from JSON file. Returns t if loaded successfully."
  (let ((state-file (testicular--state-file-path)))
    (when (file-exists-p state-file)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (state (json-read-file state-file))
                 (saved-hash (alist-get 'plan_hash state))
                 (current-hash (testicular--plan-hash)))
            (if (not (string= saved-hash current-hash))
                (progn
                  (if (yes-or-no-p "Test plan changed. Load old results anyway? ")
                      (testicular--apply-loaded-state state)
                    (message "Starting fresh"))
                  nil)
              (testicular--apply-loaded-state state)
              t))
        (error
         (message "Warning: Could not load state: %s" (error-message-string err))
         nil)))))

(defun testicular--apply-loaded-state (state)
  "Apply loaded STATE to current session."
  (let ((envs (alist-get 'environments state))
        (saved-env (alist-get 'current_environment state))
        (saved-index (alist-get 'current_index state)))
    (testicular--init-env-results)
    (dolist (env-pair envs)
      (let ((env (symbol-name (car env-pair)))
            (results-alist (cdr env-pair)))
        (puthash env (testicular--alist-to-results results-alist) testicular-results-by-env)))
    (setq testicular-current-environment (or saved-env testicular-default-environment))
    (setq testicular-current-index (or saved-index 0))
    (setq testicular-results (testicular--get-env-results testicular-current-environment))
    (message "Loaded testicular state (%s, test %d)"
             testicular-current-environment (1+ testicular-current-index))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Mode Definition
;;; ════════════════════════════════════════════════════════════════════════════

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
    (define-key map (kbd "X") #'testicular-reset)
    (define-key map (kbd "?") #'testicular-help)
    map)
  "Keymap for testicular-mode.")

(define-derived-mode testicular-mode special-mode "Testicular"
  "Mode for stepping through test cases."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Commands
;;; ════════════════════════════════════════════════════════════════════════════

;;;###autoload
(defun testicular-start (&optional environment)
  "Start testicular for current project.
With optional ENVIRONMENT, start in that environment."
  (interactive)
  (let* ((root (testicular--project-root))
         (plan-file (testicular--find-plan-file root)))
    (unless plan-file
      (user-error "No test plan found. Run M-x testicular-init-project first"))
    (let ((issues (testicular-validate-plan plan-file)))
      (when issues
        (unless (yes-or-no-p (format "Plan has issues:\n- %s\n\nStart anyway? "
                                     (string-join issues "\n- ")))
          (user-error "Fix the test plan format and try again"))))
    (setq testicular-plan-file plan-file)
    (setq testicular-project-root root)
    (setq testicular-screenshot-dir
          (expand-file-name (concat testicular-evidence-dir "/screenshots") root))
    (make-directory testicular-screenshot-dir t)
    (setq testicular-tests (testicular-parse-plan plan-file))
    (setq testicular--return-buffer nil)
    (setq testicular--claude-buffer nil)
    (if (testicular--load-state)
        (when environment
          (testicular--switch-to-env environment))
      (testicular--init-env-results)
      (setq testicular-current-environment (or environment testicular-default-environment))
      (setq testicular-current-index 0)
      (setq testicular-results nil)
      (dotimes (i (length testicular-tests))
        (push (cons i (list :status 'pending :screenshots nil :notes nil)) testicular-results))
      (setq testicular-results (nreverse testicular-results))
      (puthash testicular-current-environment testicular-results testicular-results-by-env))
    (run-hook-with-args 'testicular-start-hook root)
    (testicular-show-buffer)))

(defun testicular-reset ()
  "Reset testicular state for current project."
  (interactive)
  (let* ((root (testicular--project-root))
         (state-file (expand-file-name (concat testicular-evidence-dir "/results.json") root)))
    (when (file-exists-p state-file)
      (delete-file state-file)
      (message "Deleted %s" state-file))
    (setq testicular-tests nil
          testicular-current-index 0
          testicular-results nil
          testicular-plan-file nil
          testicular-project-root nil
          testicular-results-by-env nil)
    (message "Testicular state reset.")))

(defun testicular-show-buffer ()
  "Display the testicular buffer."
  (let ((buf (get-buffer-create "*Testicular*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (testicular-mode)
        (testicular-render)))
    (pop-to-buffer buf)))

(defun testicular-refresh ()
  "Refresh the display."
  (interactive)
  (testicular-render))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Rendering
;;; ════════════════════════════════════════════════════════════════════════════

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
  "Return t if all tests have been resolved."
  (zerop (plist-get (testicular--count-by-status) :pending)))

(defun testicular--env-face (env)
  "Return face for environment ENV."
  (pcase env
    ("local" '(:foreground "green" :weight bold))
    ("staging" '(:foreground "yellow" :weight bold))
    ("prod" '(:foreground "red" :weight bold))
    (_ '(:foreground "cyan" :weight bold))))

(defun testicular-render ()
  "Render current test in buffer."
  (let ((inhibit-read-only t)
        (test (nth testicular-current-index testicular-tests))
        (result (cdr (assoc testicular-current-index testicular-results)))
        (total (length testicular-tests))
        (counts (testicular--count-by-status)))
    (erase-buffer)
    (insert (propertize "TESTICULAR" 'face 'bold))
    (insert "  ")
    (insert (propertize (format "[%s]" (upcase testicular-current-environment))
                        'face (testicular--env-face testicular-current-environment)))
    (insert "\n")
    (insert (format "Test %d of %d" (1+ testicular-current-index) total))
    (insert "  |  ")
    (dotimes (i total)
      (let* ((r (cdr (assoc i testicular-results)))
             (status (plist-get r :status))
             (face (pcase status
                     ('passed '(:foreground "green"))
                     ('failed '(:foreground "red"))
                     ('skipped '(:foreground "blue"))
                     (_ '(:foreground "gray")))))
        (insert (propertize (if (= i testicular-current-index) "[*]" "[ ]") 'face face))))
    (insert (format "  (%d/%d/%d)"
                    (plist-get counts :passed)
                    (plist-get counts :failed)
                    (plist-get counts :skipped)))
    (insert "\n\n")
    (insert (make-string 60 ?-) "\n\n")
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
        (when (and notes (not (string-empty-p notes)))
          (insert (make-string 40 ?-) "\n")
          (insert (propertize "Notes:\n" 'face '(:foreground "cyan" :weight bold)))
          (insert notes)
          (insert "\n\n"))
        (when screenshots
          (insert (make-string 40 ?-) "\n")
          (insert (propertize (format "Screenshots: %d attached\n" (length screenshots))
                              'face 'font-lock-comment-face))
          (dolist (s screenshots)
            (insert (format "  - %s\n" (file-name-nondirectory s)))))))
    (insert "\n" (make-string 60 ?-) "\n\n")
    (if (testicular--all-resolved-p)
        (progn
          (insert (propertize "All tests resolved! " 'face '(:foreground "green" :weight bold)))
          (insert (propertize "Press RET to finish\n\n" 'face 'bold)))
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
  (run-hook-with-args 'testicular-test-status-hook
                      testicular-project-root
                      testicular-current-index
                      status)
  (testicular--save-state)
  (testicular--check-completion))

(defun testicular--check-completion ()
  "Check if all tests complete and run hook."
  (let ((counts (testicular--count-by-status)))
    (when (zerop (plist-get counts :pending))
      (run-hook-with-args 'testicular-complete-hook
                          testicular-project-root
                          (plist-get counts :passed)
                          (plist-get counts :failed)
                          (length testicular-results)))))

(defun testicular-mark-passed ()
  "Mark current test as passed and advance."
  (interactive)
  (testicular--set-status 'passed)
  (testicular-render)
  (when (< testicular-current-index (1- (length testicular-tests)))
    (testicular-next)))

(defun testicular-mark-failed ()
  "Mark current test as failed."
  (interactive)
  (testicular--set-status 'failed)
  (testicular-render)
  (when testicular-fail-opens-claude
    (testicular--open-claude-for-fix)))

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

(defun testicular-edit-notes ()
  "Edit notes for current test."
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
    (let ((result (assoc test-index testicular-results)))
      (setcdr result (plist-put (cdr result) :notes (string-trim notes))))
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
  "Switch to a different test environment."
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
      (testicular--save-state)
      (testicular-render)
      (message "Switched to %s environment" env))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Screenshots
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-screenshot-clipboard ()
  "Capture screenshot from clipboard."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "test-%d-%s.png"
                           (1+ testicular-current-index)
                           timestamp))
         (filepath (expand-file-name filename testicular-screenshot-dir)))
    (if (zerop (call-process testicular-screenshot-command nil nil nil
                             "-selection" "clipboard"
                             "-t" "image/png"
                             "-o"))
        (progn
          (with-temp-file filepath
            (set-buffer-multibyte nil)
            (call-process testicular-screenshot-command nil t nil
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
;;; Export
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-export-to-file ()
  "Export test results to markdown file."
  (interactive)
  (let* ((output (testicular--generate-export))
         (results-file (expand-file-name testicular-results-filename testicular-project-root)))
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
        (when (and notes (not (string-empty-p notes)))
          (insert "**Notes:**\n")
          (insert notes)
          (insert "\n\n"))
        (when screenshots
          (insert "**Screenshots:**\n")
          (dolist (s screenshots)
            (let ((rel-path (file-relative-name s testicular-project-root)))
              (insert (format "- `%s`\n" rel-path))))
          (insert "\n"))
        (insert "\n")))
    (buffer-string)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Finish Workflow
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-finish ()
  "Finish testing: export, commit, push, create PR."
  (interactive)
  (let ((counts (testicular--count-by-status)))
    (if (not (testicular--all-resolved-p))
        (user-error "Not all tests resolved. %d pending" (plist-get counts :pending))
      (when (> (plist-get counts :failed) 0)
        (message "Note: %d test(s) failed" (plist-get counts :failed)))
      (testicular-export-to-file)
      (run-hook-with-args 'testicular-complete-hook
                          testicular-project-root
                          (plist-get counts :passed)
                          (plist-get counts :failed)
                          (length testicular-results))
      (testicular--finish-workflow counts))))

(defun testicular--finish-workflow (counts)
  "Run the finish workflow with COUNTS."
  (let ((default-directory testicular-project-root))
    (when (file-exists-p (expand-file-name testicular-results-filename testicular-project-root))
      (shell-command (format "git add %s" testicular-results-filename)))
    (when (file-directory-p (expand-file-name testicular-evidence-dir testicular-project-root))
      (shell-command (format "git add %s/" testicular-evidence-dir)))
    (let ((staged (string-trim (shell-command-to-string "git diff --cached --name-only"))))
      (unless (string-empty-p staged)
        (let ((commit-msg (format "Add test results: %d passed, %d failed, %d skipped"
                                  (plist-get counts :passed)
                                  (plist-get counts :failed)
                                  (plist-get counts :skipped))))
          (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
          (message "Committed test results"))))
    (testicular--push-and-pr)))

(defun testicular--push-and-pr ()
  "Push and create PR using configured backend."
  (let* ((default-directory testicular-project-root)
         (branch (string-trim (shell-command-to-string "git branch --show-current"))))
    (when (member branch (list testicular-pr-base-branch "main" "master"))
      (user-error "Won't ship directly to %s" branch))
    (message "Pushing %s to %s..." branch testicular-pr-remote)
    (shell-command (format "git push -u %s %s"
                           testicular-pr-remote
                           (shell-quote-argument branch)))
    (pcase testicular-pr-backend
      ('github (testicular--create-pr-github branch))
      ('gitlab (testicular--create-pr-gitlab branch))
      ('none (message "Push complete. Create PR manually.")))))

(defun testicular--create-pr-github (branch)
  "Create GitHub PR for BRANCH."
  (let* ((title (replace-regexp-in-string "^[A-Z]+[-/]" "" branch))
         (title (replace-regexp-in-string "-" " " title))
         (title (capitalize title))
         (description (testicular--generate-pr-body))
         (cmd (format "gh pr create --title %s --body %s --base %s 2>&1"
                      (shell-quote-argument title)
                      (shell-quote-argument description)
                      (shell-quote-argument testicular-pr-base-branch)))
         (result (shell-command-to-string cmd)))
    (if (string-match-p "https://github" result)
        (let ((pr-url (string-trim result)))
          (message "PR created: %s" pr-url)
          (when testicular-open-browser-on-pr
            (browse-url pr-url)))
      (message "PR creation output: %s" result))))

(defun testicular--create-pr-gitlab (branch)
  "Create GitLab MR for BRANCH."
  (let* ((title (replace-regexp-in-string "^[A-Z]+[-/]" "" branch))
         (title (replace-regexp-in-string "-" " " title))
         (title (capitalize title))
         (description (testicular--generate-pr-body))
         (cmd (format "glab mr create --title %s --description %s --target-branch %s 2>&1"
                      (shell-quote-argument title)
                      (shell-quote-argument description)
                      (shell-quote-argument testicular-pr-base-branch)))
         (result (shell-command-to-string cmd)))
    (if (string-match-p "https://gitlab" result)
        (let ((mr-url (string-trim result)))
          (message "MR created: %s" mr-url)
          (when testicular-open-browser-on-pr
            (browse-url mr-url)))
      (message "MR creation output: %s" result))))

(defun testicular--generate-pr-body ()
  "Generate PR body from test results."
  (let ((counts (testicular--count-by-status)))
    (format "## Test Results\n\n- Passed: %d\n- Failed: %d\n- Skipped: %d\n\nSee `%s` for details."
            (plist-get counts :passed)
            (plist-get counts :failed)
            (plist-get counts :skipped)
            testicular-results-filename)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Claude Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular--open-claude-for-fix ()
  "Open Claude to fix failed test."
  (let* ((test (nth testicular-current-index testicular-tests))
         (test-name (plist-get test :name))
         (test-body (plist-get test :body))
         (original-window (selected-window)))
    (setq testicular--return-buffer (current-buffer))
    (when (and (fboundp 'claude-code)
               (fboundp 'claude-code-send-command))
      (let ((default-directory testicular-project-root))
        (claude-code)
        (let ((claude-buf (current-buffer)))
          (setq testicular--claude-buffer claude-buf)
          (select-window original-window)
          (switch-to-buffer claude-buf)
          (with-current-buffer claude-buf
            (add-hook 'kill-buffer-hook #'testicular--on-claude-kill nil t))
          (run-at-time 1.5 nil
                       (lambda ()
                         (when (and testicular--claude-buffer
                                    (buffer-live-p testicular--claude-buffer))
                           (with-current-buffer testicular--claude-buffer
                             (claude-code-send-command
                              (format "Test failed: %s\n\n%s\n\nPlease help me fix this."
                                      test-name test-body)))))))))))

(defun testicular--on-claude-kill ()
  "Return to testicular when Claude killed."
  (when (and testicular--return-buffer
             (buffer-live-p testicular--return-buffer))
    (let ((return-buf testicular--return-buffer))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p return-buf)
                       (switch-to-buffer return-buf)
                       (message "Back to Testicular")))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Help
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-help ()
  "Show help for testicular."
  (interactive)
  (with-output-to-temp-buffer "*Testicular Help*"
    (princ "TESTICULAR - Manual Test Runner\n")
    (princ "================================\n\n")
    (princ "NAVIGATION\n")
    (princ "  n/p     Next/previous test\n\n")
    (princ "STATUS\n")
    (princ "  P       Mark PASSED and advance\n")
    (princ "  f       Mark FAILED (opens Claude if configured)\n")
    (princ "  S       Mark SKIPPED and advance\n\n")
    (princ "NOTES & ENVIRONMENT\n")
    (princ "  N       Edit notes (C-c C-c to save)\n")
    (princ "  E       Switch environment\n\n")
    (princ "SCREENSHOTS\n")
    (princ "  s       Capture from clipboard\n")
    (princ "  F       Attach from file\n")
    (princ "  v       View attached screenshots\n\n")
    (princ "COMPLETION\n")
    (princ "  e       Export results to markdown\n")
    (princ "  RET     Finish: export, commit, push, create PR\n\n")
    (princ "OTHER\n")
    (princ "  g/r     Refresh\n")
    (princ "  X       Reset state\n")
    (princ "  q       Quit\n")
    (princ "  ?       This help\n\n")
    (princ "SETUP\n")
    (princ "  M-x testicular-init-project  Initialize for new project\n")
    (princ "  M-x testicular-export-claude-command  Regenerate /test-plan\n")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(require 'transient)

;;;###autoload (autoload 'testicular-transient "testicular" nil t)
(transient-define-prefix testicular-transient ()
  "Testicular manual testing menu."
  ["Testicular"
   ["Start"
    ("t" "Start testing" testicular-start)
    ("s" "Start (staging)" testicular-start-staging)
    ("p" "Start (prod)" testicular-start-prod)]
   ["Setup"
    ("i" "Init project" testicular-init-project)
    ("e" "Export Claude command" testicular-export-claude-command)
    ("v" "Validate plan" testicular-validate-plan)]
   ["State"
    ("X" "Reset state" testicular-reset)]])

(defun testicular-start-staging ()
  "Start testicular in staging environment."
  (interactive)
  (testicular-start "staging"))

(defun testicular-start-prod ()
  "Start testicular in prod environment."
  (interactive)
  (testicular-start "prod"))

;; Add to testicular-mode-map
(define-key testicular-mode-map (kbd "?") #'testicular-transient-in-buffer)

(transient-define-prefix testicular-transient-in-buffer ()
  "Testicular commands while testing."
  ["Navigation"
   ("n" "Next test" testicular-next)
   ("p" "Previous test" testicular-prev)]
  ["Mark Status"
   ("P" "Pass" testicular-mark-passed)
   ("f" "Fail" testicular-mark-failed)
   ("S" "Skip" testicular-mark-skipped)]
  ["Evidence"
   ("s" "Screenshot (clipboard)" testicular-screenshot-clipboard)
   ("F" "Screenshot (file)" testicular-screenshot-file)
   ("v" "View screenshots" testicular-view-screenshots)
   ("N" "Edit notes" testicular-edit-notes)]
  ["Environment"
   ("E" "Switch environment" testicular-switch-environment)]
  ["Finish"
   ("e" "Export results" testicular-export-to-file)
   ("RET" "Finish & PR" testicular-finish)]
  ["Other"
   ("g" "Refresh" testicular-refresh)
   ("X" "Reset" testicular-reset)
   ("q" "Quit" quit-window)])

(provide 'testicular)
;;; testicular.el ends here
