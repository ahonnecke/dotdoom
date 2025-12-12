;;; config-pr.el --- Create PRs from magit with AI-generated descriptions -*- lexical-binding: t; -*-
;;
;; Quick PR creation from magit:
;;   1. Push current branch to origin
;;   2. Generate PR description from commits
;;   3. Open PR against dev (or specified base)
;;
;; Usage: From magit, press 'P r' or run M-x pr-create

(require 'magit)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup pr nil
  "PR creation from magit."
  :group 'tools
  :prefix "pr-")

(defcustom pr-default-base "dev"
  "Default base branch for PRs."
  :type 'string
  :group 'pr)

(defcustom pr-remote "upstream"
  "Remote to push to and create PR against."
  :type 'string
  :group 'pr)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun pr--current-branch ()
  "Get current git branch name."
  (magit-get-current-branch))

(defun pr--branch-title (branch)
  "Generate PR title from BRANCH name.
Strips prefix like FEAT/, FIX/, CHORE/ and converts to title case."
  (let ((name (replace-regexp-in-string "^[A-Z]+/" "" branch)))
    (replace-regexp-in-string "-" " " name)))

(defun pr--fetch-upstream ()
  "Fetch from upstream remote to ensure we have latest refs."
  (message "Fetching upstream...")
  (shell-command-to-string (format "git fetch %s 2>/dev/null" pr-remote)))

(defun pr--upstream-base ()
  "Get the upstream version of the base branch (e.g., upstream/dev)."
  (let ((upstream-ref (format "%s/%s" pr-remote pr-default-base)))
    ;; Verify it exists
    (if (string-empty-p
         (string-trim
          (shell-command-to-string
           (format "git rev-parse --verify %s 2>/dev/null" upstream-ref))))
        ;; Fall back to local base if upstream doesn't exist
        pr-default-base
      upstream-ref)))

(defun pr--merge-base ()
  "Get merge-base between upstream base and HEAD.
This ensures we only get commits unique to this branch."
  (pr--fetch-upstream)
  (let ((upstream-base (pr--upstream-base)))
    (string-trim
     (shell-command-to-string
      (format "git merge-base %s HEAD 2>/dev/null" upstream-base)))))

(defun pr--get-commits-since-base (base)
  "Get commit messages since BASE branch.
Uses merge-base to only get commits unique to this branch (not already in upstream)."
  (let* ((merge-base (pr--merge-base))
         (range (if (string-empty-p merge-base)
                    (format "%s..HEAD" base)
                  (format "%s..HEAD" merge-base))))
    (shell-command-to-string
     (format "git log %s --pretty=format:'%%s%%n%%b%%n---' 2>/dev/null" range))))

(defun pr--get-diff-stat (base)
  "Get diff stat since BASE branch.
Uses merge-base to only show changes unique to this branch."
  (let ((merge-base (pr--merge-base)))
    (shell-command-to-string
     (format "git diff %s --stat 2>/dev/null"
             (if (string-empty-p merge-base) base merge-base)))))

(defun pr--get-changed-files (base)
  "Get list of changed files since BASE branch.
Uses merge-base to only show files changed in this branch."
  (let ((merge-base (pr--merge-base)))
    (shell-command-to-string
     (format "git diff %s --name-only 2>/dev/null"
             (if (string-empty-p merge-base) base merge-base)))))

(defun pr--generate-description (branch base)
  "Generate PR description for BRANCH against BASE.
Includes commits, changed files, and prompts for summary."
  (let* ((commits (pr--get-commits-since-base base))
         (diff-stat (pr--get-diff-stat base))
         (files (pr--get-changed-files base))
         (file-count (length (split-string files "\n" t)))
         (summary (read-string "PR Summary (one line): ")))
    (format "## Summary
%s

## Changes
- %d files changed

## Commits
```
%s
```

## Files Changed
```
%s
```
"
            summary
            file-count
            (string-trim commits)
            (string-trim diff-stat))))

(defun pr--push-branch (branch)
  "Push BRANCH to remote."
  (message "Pushing %s to %s..." branch pr-remote)
  (let ((output (shell-command-to-string
                 (format "git push -u %s %s 2>&1"
                         pr-remote
                         (shell-quote-argument branch)))))
    (if (string-match-p "error\\|fatal" output)
        (error "Push failed: %s" output)
      (message "Pushed %s" branch))))

(defun pr--create-pr (title body base)
  "Create PR with TITLE and BODY against BASE."
  (let* ((cmd (format "gh pr create --title %s --body %s --base %s 2>&1"
                      (shell-quote-argument title)
                      (shell-quote-argument body)
                      (shell-quote-argument base)))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "https://github.com" output)
        (progn
          (message "PR created: %s" (string-trim output))
          ;; Open in browser
          (when (yes-or-no-p "Open PR in browser? ")
            (browse-url (string-trim output)))
          output)
      (error "PR creation failed: %s" output))))

(defun pr--create-pr-silent (title body base)
  "Create PR with TITLE and BODY against BASE - no prompts, auto-open browser."
  (let* ((cmd (format "gh pr create --title %s --body %s --base %s 2>&1"
                      (shell-quote-argument title)
                      (shell-quote-argument body)
                      (shell-quote-argument base)))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "https://github.com" output)
        (let ((url (string-trim output)))
          (message "PR created: %s" url)
          (browse-url url)
          url)
      (error "PR creation failed: %s" output))))

(defun pr--create-pr-no-browser (title body base)
  "Create PR with TITLE and BODY against BASE - no prompts, no browser."
  (let* ((cmd (format "gh pr create --title %s --body %s --base %s 2>&1"
                      (shell-quote-argument title)
                      (shell-quote-argument body)
                      (shell-quote-argument base)))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "https://github.com" output)
        (let ((url (string-trim output)))
          (message "PR created: %s" url)
          (kill-new url)  ; Copy URL to clipboard instead
          url)
      (error "PR creation failed: %s" output))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; AI-Powered Description Generation
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom pr-ai-model nil
  "Model to use for AI PR generation. If nil, uses llm default."
  :type '(choice (const :tag "Default" nil) string)
  :group 'pr)

(defun pr--ai-generate-description (branch base)
  "Generate AI-powered PR description for BRANCH against BASE.
Uses llm CLI to create a concise, well-structured summary."
  (let* ((merge-base (pr--merge-base))
         (diff-base (if (string-empty-p merge-base) base merge-base))
         (commits (pr--get-commits-since-base base))
         (diff (shell-command-to-string
                (format "git diff %s --no-color 2>/dev/null | head -c 15000" diff-base)))
         (files (pr--get-changed-files base))
         (prompt (format "Generate a concise PR description for these changes.

Branch: %s
Base: %s

Commits:
%s

Changed files:
%s

Diff (truncated):
%s

Return ONLY markdown in this exact format:
## Summary
One clear sentence describing the change.

## What Changed
- Bullet point 1 (what, not which file)
- Bullet point 2
- (3-5 bullets max, group related changes)

## Why
Brief context on the motivation (skip if obvious from summary).

Be specific and accurate. No fluff. No \"This PR...\" prefixes."
                         branch base
                         (string-trim commits)
                         (string-trim files)
                         diff))
         (model-arg (if pr-ai-model (format " -m %s" pr-ai-model) ""))
         (cmd (format "echo %s | llm%s 2>/dev/null"
                      (shell-quote-argument prompt)
                      model-arg)))
    (message "Generating PR description...")
    (let ((result (string-trim (shell-command-to-string cmd))))
      (if (string-empty-p result)
          (progn
            (message "AI generation failed, falling back to basic format")
            (pr--generate-description branch base))
        result))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Interactive Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun pr--check-test-plan ()
  "Check if a test plan exists and has been executed.
Returns a plist with :has-plan :has-results :passed :failed."
  (let* ((root (or (projectile-project-root) default-directory))
         (plan-file (expand-file-name ".test-plan.md" root))
         (results-file (expand-file-name ".test-results.md" root))
         (has-plan (file-exists-p plan-file))
         (has-results (file-exists-p results-file)))
    (list :has-plan has-plan
          :has-results has-results
          :plan-file plan-file
          :results-file results-file)))

(defun pr--prompt-for-test-plan ()
  "Prompt user about missing test plan/results.
Returns t if user wants to proceed, nil to abort."
  (let* ((status (pr--check-test-plan))
         (has-plan (plist-get status :has-plan))
         (has-results (plist-get status :has-results)))
    (cond
     ;; No test plan at all
     ((not has-plan)
      (if (yes-or-no-p "No .test-plan.md found. Create PR anyway? (Consider running /test-plan in Claude first) ")
          t
        (progn
          (message "Create test plan: run /test-plan in Claude, then use C-c T (testicular) to execute")
          nil)))
     ;; Test plan exists but not executed
     ((and has-plan (not has-results))
      (if (yes-or-no-p ".test-plan.md exists but tests not run. Create PR anyway? (Consider C-c T to run testicular) ")
          t
        (progn
          (message "Run tests: C-c T starts testicular to step through the test plan")
          nil)))
     ;; All good
     (t t))))

;;;###autoload
(defun pr-create-ai (&optional base)
  "Create a PR with AI-generated description - NO PROMPTS.
Pushes to origin, uses AI to generate description, creates PR against BASE.
Auto-opens in browser when done."
  (interactive)
  (let* ((branch (pr--current-branch))
         (base (or base pr-default-base))
         (title (pr--branch-title branch)))
    ;; Sanity check - don't PR from main branches
    (when (member branch '("dev" "main" "master"))
      (user-error "Won't create PR from %s - use a feature branch" branch))
    (message "Creating AI PR: %s -> %s..." branch base)
    ;; Generate description
    (let ((body (pr--ai-generate-description branch base)))
      ;; Push
      (pr--push-branch branch)
      ;; Create PR (no prompts)
      (pr--create-pr-silent title body base))))

;;;###autoload
(defun pr-create (&optional base)
  "Create a PR for the current branch.
Pushes to origin, generates description, creates PR against BASE (default: dev)."
  (interactive)
  (unless (pr--prompt-for-test-plan)
    (user-error "PR creation aborted"))
  (let* ((branch (pr--current-branch))
         (base (or base pr-default-base))
         (title (read-string "PR Title: " (pr--branch-title branch)))
         (body (pr--generate-description branch base)))
    ;; Confirm
    (when (yes-or-no-p (format "Create PR: %s -> %s? " branch base))
      ;; Push
      (pr--push-branch branch)
      ;; Create PR
      (pr--create-pr title body base))))

;;;###autoload
(defun pr-create-quick ()
  "Quick PR creation - no prompts, no browser.
Uses branch name as title, commits as body. URL copied to clipboard."
  (interactive)
  (let* ((branch (pr--current-branch))
         (base pr-default-base)
         (title (pr--branch-title branch))
         (commits (pr--get-commits-since-base base))
         (body (format "## Commits\n```\n%s\n```" (string-trim commits))))
    (pr--push-branch branch)
    (pr--create-pr-no-browser title body base)))

;;;###autoload
(defun pr-view ()
  "View the PR for the current branch in browser."
  (interactive)
  (let ((output (shell-command-to-string "gh pr view --web 2>&1")))
    (when (string-match-p "no pull requests found" output)
      (message "No PR found for this branch"))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Ship It - One command to commit, push, PR
;;; ════════════════════════════════════════════════════════════════════════════

(defun pr--generate-commit-message ()
  "Generate a commit message from staged changes using AI."
  (let* ((diff (shell-command-to-string "git diff --cached --stat 2>/dev/null"))
         (full-diff (shell-command-to-string "git diff --cached 2>/dev/null | head -c 8000"))
         (prompt (format "Generate a concise git commit message for these changes.

Staged files:
%s

Diff (truncated):
%s

Rules:
- First line: type(scope): description (50 chars max)
- Types: feat, fix, chore, refactor, docs, test
- Be specific about WHAT changed
- No body needed unless complex
- Return ONLY the commit message, nothing else"
                         (string-trim diff)
                         full-diff))
         (cmd (format "echo %s | llm 2>/dev/null" (shell-quote-argument prompt))))
    (string-trim (shell-command-to-string cmd))))

(defun pr--has-staged-changes-p ()
  "Return t if there are staged changes."
  (not (string-empty-p
        (string-trim
         (shell-command-to-string "git diff --cached --name-only 2>/dev/null")))))

(defun pr--commit-staged (message)
  "Commit staged changes with MESSAGE."
  (let* ((msg-with-trailer (format "%s\n\n🤖 Generated with Claude Code\n\nCo-Authored-By: Claude <noreply@anthropic.com>" message))
         (cmd (format "git commit -m %s 2>&1" (shell-quote-argument msg-with-trailer)))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "\\[.*\\]" output)  ; Success shows [branch hash]
        (progn
          (message "Committed: %s" (car (split-string message "\n")))
          t)
      (error "Commit failed: %s" output))))

;;;###autoload
(defun pr-ship-it ()
  "Ship it! Commit staged changes, push, and create PR - no questions asked.
Uses AI to generate commit message and PR description."
  (interactive)
  (let ((branch (pr--current-branch)))
    ;; Sanity checks
    (when (member branch '("dev" "main" "master"))
      (user-error "Won't ship directly to %s - create a feature branch first" branch))
    (unless (pr--has-staged-changes-p)
      (user-error "No staged changes - stage something first (in magit: s)"))

    ;; Generate commit message
    (message "Generating commit message...")
    (let ((commit-msg (pr--generate-commit-message)))
      (when (string-empty-p commit-msg)
        (setq commit-msg (read-string "Commit message (AI failed): ")))

      ;; Show what we're about to do
      (message "Shipping: %s" (car (split-string commit-msg "\n")))

      ;; Commit
      (pr--commit-staged commit-msg)

      ;; Push
      (message "Pushing to %s..." pr-remote)
      (pr--push-branch branch)

      ;; Create PR with AI description
      (message "Creating PR...")
      (let* ((title (pr--branch-title branch))
             (body (pr--ai-generate-description branch pr-default-base)))
        (pr--create-pr title body pr-default-base)))))

;;;###autoload
(defun pr-ship-it-hierarchically ()
  "Ship it with interactive commit message editing.
Shows generated message, lets you edit, then ships."
  (interactive)
  (let ((branch (pr--current-branch)))
    ;; Sanity checks
    (when (member branch '("dev" "main" "master"))
      (user-error "Won't ship directly to %s" branch))
    (unless (pr--has-staged-changes-p)
      (user-error "No staged changes"))

    ;; Generate and edit commit message
    (message "Generating commit message...")
    (let* ((ai-msg (pr--generate-commit-message))
           (commit-msg (read-string "Commit: " ai-msg)))

      (when (string-empty-p commit-msg)
        (user-error "Aborted - empty commit message"))

      ;; Commit
      (pr--commit-staged commit-msg)

      ;; Push
      (pr--push-branch branch)

      ;; PR
      (let* ((title (read-string "PR Title: " (pr--branch-title branch)))
             (body (pr--ai-generate-description branch pr-default-base)))
        (pr--create-pr title body pr-default-base)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Magit Integration
;;; ════════════════════════════════════════════════════════════════════════════

;; Add to magit's push transient
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-push "p"
    '("r" "Create PR" pr-create))
  (transient-append-suffix 'magit-push "r"
    '("R" "Quick PR" pr-create-quick))
  (transient-append-suffix 'magit-push "R"
    '("A" "AI PR (no prompts)" pr-create-ai))
  (transient-append-suffix 'magit-push "A"
    '("v" "View PR" pr-view))
  (transient-append-suffix 'magit-push "v"
    '("!" "SHIP IT (commit+push+PR)" pr-ship-it))
  (transient-append-suffix 'magit-push "!"
    '("@" "Ship (edit msg first)" pr-ship-it-hierarchically)))

(provide 'config-pr)
;;; config-pr.el ends here
