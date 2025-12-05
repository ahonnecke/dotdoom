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

(defun pr--get-commits-since-base (base)
  "Get commit messages since BASE branch."
  (let ((range (format "%s..HEAD" base)))
    (shell-command-to-string
     (format "git log %s --pretty=format:'%%s%%n%%b%%n---' 2>/dev/null" range))))

(defun pr--get-diff-stat (base)
  "Get diff stat since BASE branch."
  (shell-command-to-string
   (format "git diff %s --stat 2>/dev/null" base)))

(defun pr--get-changed-files (base)
  "Get list of changed files since BASE branch."
  (shell-command-to-string
   (format "git diff %s --name-only 2>/dev/null" base)))

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
  (let* ((commits (pr--get-commits-since-base base))
         (diff (shell-command-to-string
                (format "git diff %s --no-color 2>/dev/null | head -c 15000" base)))
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

;;;###autoload
(defun pr-create-ai (&optional base)
  "Create a PR with AI-generated description.
Pushes to origin, uses AI to generate description, creates PR against BASE."
  (interactive)
  (let* ((branch (pr--current-branch))
         (base (or base pr-default-base))
         (title (read-string "PR Title: " (pr--branch-title branch)))
         (body (pr--ai-generate-description branch base)))
    ;; Show preview
    (with-current-buffer (get-buffer-create "*PR Preview*")
      (erase-buffer)
      (insert (format "# %s\n\n%s" title body))
      (markdown-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    ;; Confirm
    (when (yes-or-no-p (format "Create PR: %s -> %s? " branch base))
      (kill-buffer "*PR Preview*")
      (pr--push-branch branch)
      (pr--create-pr title body base))))

;;;###autoload
(defun pr-create (&optional base)
  "Create a PR for the current branch.
Pushes to origin, generates description, creates PR against BASE (default: dev)."
  (interactive)
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
  "Quick PR creation with minimal prompts.
Uses branch name as title, commits as body."
  (interactive)
  (let* ((branch (pr--current-branch))
         (base pr-default-base)
         (title (pr--branch-title branch))
         (commits (pr--get-commits-since-base base))
         (body (format "## Commits\n```\n%s\n```" (string-trim commits))))
    (when (yes-or-no-p (format "Quick PR: %s -> %s? " branch base))
      (pr--push-branch branch)
      (pr--create-pr title body base))))

;;;###autoload
(defun pr-view ()
  "View the PR for the current branch in browser."
  (interactive)
  (let ((output (shell-command-to-string "gh pr view --web 2>&1")))
    (when (string-match-p "no pull requests found" output)
      (message "No PR found for this branch"))))

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
    '("A" "AI PR (smart desc)" pr-create-ai))
  (transient-append-suffix 'magit-push "A"
    '("v" "View PR" pr-view)))

(provide 'config-pr)
;;; config-pr.el ends here
