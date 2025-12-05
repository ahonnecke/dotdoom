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
(defvar testicular-results nil "Alist of (index . (:status :screenshots)).")
(defvar testicular-plan-file nil "Path to the test plan file.")
(defvar testicular-screenshot-dir nil "Directory for screenshots.")
(defvar testicular-project-root nil "Project root for current test flow.")
(defvar testicular--return-buffer nil "Buffer to return to after Claude session.")
(defvar testicular--claude-buffer nil "Claude buffer spawned from testicular.")

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
    (define-key map (kbd "e") #'testicular-export-to-file)
    (define-key map (kbd "r") #'testicular-refresh)
    (define-key map (kbd "g") #'testicular-refresh)
    (define-key map (kbd "RET") #'testicular-finish)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'testicular-help)
    map)
  "Keymap for testicular-mode.")

(define-derived-mode testicular-mode special-mode "Testicular"
  "Mode for stepping through test cases."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun testicular-start ()
  "Start testicular for current project."
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
    (setq testicular-current-index 0)
    (setq testicular-results nil)
    (setq testicular--return-buffer nil)
    (setq testicular--claude-buffer nil)
    ;; Initialize results
    (dotimes (i (length testicular-tests))
      (push (cons i (list :status 'pending :screenshots nil)) testicular-results))
    (setq testicular-results (nreverse testicular-results))
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

(defun testicular-render ()
  "Render current test in buffer."
  (let ((inhibit-read-only t)
        (test (nth testicular-current-index testicular-tests))
        (result (cdr (assoc testicular-current-index testicular-results)))
        (total (length testicular-tests))
        (counts (testicular--count-by-status)))
    (erase-buffer)
    (insert (propertize "TESTICULAR" 'face 'bold) "\n")
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
    (insert "\n\n")
    (insert (make-string 60 ?-) "\n\n")
    ;; Test content
    (when test
      (let ((status (plist-get result :status))
            (screenshots (plist-get result :screenshots)))
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
    (insert "n/p:nav  P:pass  f:fail  S:skip  s:screenshot  RET:finish  q:quit  ?:help")
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
                              (append screenshots (list filepath))))))

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
      (insert (format "- **Passed:** %d\n" (plist-get counts :passed)))
      (insert (format "- **Failed:** %d\n" (plist-get counts :failed)))
      (insert (format "- **Skipped:** %d\n\n" (plist-get counts :skipped))))
    (dotimes (i (length testicular-tests))
      (let* ((test (nth i testicular-tests))
             (result (cdr (assoc i testicular-results)))
             (status (plist-get result :status))
             (screenshots (plist-get result :screenshots))
             (icon (pcase status
                     ('passed ":white_check_mark:")
                     ('failed ":x:")
                     ('skipped ":fast_forward:")
                     (_ ":hourglass:"))))
        (insert (format "## %s Test %d: %s\n\n"
                        icon
                        (plist-get test :number)
                        (plist-get test :name)))
        ;; Reference screenshots by relative path (for GitHub)
        (when screenshots
          (insert "**Screenshots:**\n")
          (dolist (s screenshots)
            (let ((rel-path (file-relative-name s testicular-project-root)))
              (insert (format "- `%s`\n" rel-path))))
          (insert "\n"))
        (insert "\n")))
    (buffer-string)))

(defun testicular-finish ()
  "Finish testing: export results, commit, push, and create PR."
  (interactive)
  (let ((counts (testicular--count-by-status)))
    (if (not (testicular--all-resolved-p))
        (user-error "Not all tests resolved. %d pending" (plist-get counts :pending))
      ;; Check for failures - warn but allow proceeding
      (when (> (plist-get counts :failed) 0)
        (unless (yes-or-no-p (format "%d test(s) failed. Continue anyway? "
                                     (plist-get counts :failed)))
          (user-error "Aborted")))
      ;; Export results to file
      (testicular-export-to-file)
      ;; Run completion hook for orchard integration
      (run-hook-with-args 'testicular-complete-hook
                          testicular-project-root
                          (plist-get counts :passed)
                          (plist-get counts :failed)
                          (length testicular-results))
      ;; Offer to commit, push, create PR
      (testicular--finish-workflow counts))))

(defun testicular--finish-workflow (counts)
  "Run the finish workflow: commit, push, PR.
COUNTS is plist with test result counts."
  (let ((default-directory testicular-project-root))
    ;; Stage test evidence
    (when (file-exists-p (expand-file-name ".test-results.md" testicular-project-root))
      (shell-command "git add .test-results.md"))
    (when (file-directory-p (expand-file-name ".test-evidence" testicular-project-root))
      (shell-command "git add .test-evidence/"))
    ;; Check if there's anything to commit
    (let ((staged (string-trim (shell-command-to-string "git diff --cached --name-only"))))
      (if (string-empty-p staged)
          (message "No test evidence to commit")
        ;; Commit test results
        (let ((commit-msg (format "Add test results: %d passed, %d failed, %d skipped"
                                  (plist-get counts :passed)
                                  (plist-get counts :failed)
                                  (plist-get counts :skipped))))
          (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
          (message "Committed test results"))))
    ;; Ask about push and PR
    (when (yes-or-no-p "Push and create PR? ")
      (testicular--push-and-pr))))

(defun testicular--push-and-pr ()
  "Push current branch and create PR."
  (let* ((default-directory testicular-project-root)
         (branch (string-trim (shell-command-to-string "git branch --show-current"))))
    ;; Push
    (message "Pushing %s..." branch)
    (shell-command (format "git push -u origin %s" (shell-quote-argument branch)))
    ;; Get PR title from branch name
    (let* ((title (replace-regexp-in-string "^[A-Z]+/" "" branch))
           (title (replace-regexp-in-string "-" " " title))
           (title (capitalize title))
           ;; Read description or use default
           (description (read-string "PR description (or empty for default): ")))
      (when (string-empty-p description)
        (setq description (testicular--generate-pr-body)))
      ;; Create PR
      (let ((pr-cmd (format "gh pr create --title %s --body %s --base dev 2>&1"
                            (shell-quote-argument title)
                            (shell-quote-argument description))))
        (let ((result (shell-command-to-string pr-cmd)))
          (if (string-match-p "https://github" result)
              (progn
                ;; Save PR URL
                (let ((pr-url (string-trim result)))
                  (with-temp-file (expand-file-name ".pr-url" testicular-project-root)
                    (insert pr-url))
                  (message "PR created: %s" pr-url)
                  (when (yes-or-no-p "Open PR in browser? ")
                    (browse-url pr-url))))
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
    (princ "SCREENSHOTS (optional, multiple per test allowed)\n")
    (princ "  s       Capture from clipboard (take shot with Flameshot etc, then press s)\n")
    (princ "  F       Attach from file (prompts for path)\n")
    (princ "  v       View attached screenshots for current test\n")
    (princ "          Screenshots saved to .test-evidence/screenshots/\n\n")
    (princ "COMPLETION\n")
    (princ "  e       Export results to .test-results.md\n")
    (princ "  RET     Finish: export, commit evidence, push, create PR\n")
    (princ "          (only available when all tests resolved)\n\n")
    (princ "OTHER\n")
    (princ "  g/r     Refresh display\n")
    (princ "  q       Quit testicular\n")
    (princ "  ?       This help\n\n")
    (princ "WORKFLOW\n")
    (princ "  1. Step through tests with n/p\n")
    (princ "  2. Mark each: P=pass, f=fail (opens Claude), S=skip\n")
    (princ "  3. Optionally attach screenshots with s or F\n")
    (princ "  4. When all resolved, press RET to finish and create PR\n")))

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
