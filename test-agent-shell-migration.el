;;; test-agent-shell-migration.el --- Smoke tests for agent-shell integration -*- lexical-binding: t; -*-
;;
;; Usage:
;;   ~/.emacs.d/bin/doom run -- -l ~/.doom.d/test-agent-shell-migration.el -f run-agent-shell-tests
;;
;; Or from a running Emacs:
;;   M-x eval-buffer  then  M-x run-agent-shell-tests
;;
;; Exits with code 1 on failure (batch mode) or shows *Test Results* buffer (interactive).

(require 'cl-lib)

;; Add Doom's package paths so require works in batch mode
(let ((build-dir (expand-file-name "~/.emacs.d/.local/straight/build-30.1/")))
  (when (file-directory-p build-dir)
    (dolist (dir (directory-files build-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))
;; Also add our config dir
(add-to-list 'load-path (expand-file-name "~/.doom.d/"))

(defvar ast--results nil "Alist of (test-name . (pass-p . message)).")

(defun ast--pass (name msg)
  (push (cons name (cons t msg)) ast--results))

(defun ast--fail (name msg)
  (push (cons name (cons nil msg)) ast--results))

(defun ast--check (name predicate msg-pass msg-fail)
  "Run a single test. PREDICATE is a boolean."
  (if predicate
      (ast--pass name msg-pass)
    (ast--fail name msg-fail)))

;; ─── Tests ───────────────────────────────────────────────────────────────────

(defun ast--test-executable-in-path ()
  "Test that claude-agent-acp is findable by Emacs."
  (let ((found (executable-find "claude-agent-acp")))
    (ast--check "exec:claude-agent-acp"
                found
                (format "Found at %s" found)
                (format "NOT FOUND. exec-path includes: %s"
                        (string-join (cl-remove-if-not
                                      (lambda (p) (string-match-p "node\\|nvm\\|bin" p))
                                      exec-path)
                                     ", ")))))

(defun ast--test-package-loadable (pkg)
  "Test that package PKG can be required."
  (let ((name (format "load:%s" pkg)))
    (condition-case err
        (progn (require pkg)
               (ast--pass name "loaded"))
      (error (ast--fail name (format "FAILED: %s" (error-message-string err)))))))

(defun ast--test-version (pkg var min-version)
  "Test that PKG's version variable VAR meets MIN-VERSION."
  (let ((name (format "version:%s>=%s" pkg min-version)))
    (condition-case err
        (progn
          (require pkg)
          (let ((actual (symbol-value var)))
            (ast--check name
                        (version<= min-version actual)
                        (format "%s" actual)
                        (format "NEED %s, GOT %s" min-version actual))))
      (error (ast--fail name (format "Can't check: %s" (error-message-string err)))))))

(defun ast--test-variable-value (name var expected)
  "Test that VAR equals EXPECTED."
  (let ((actual (if (boundp var) (symbol-value var) 'UNBOUND)))
    (ast--check name
                (equal actual expected)
                (format "%S" actual)
                (format "EXPECTED %S, GOT %S" expected actual))))

(defun ast--test-function-defined-in-source (name fn-name file)
  "Test that FN-NAME appears as a defun in FILE."
  (let ((path (expand-file-name file)))
    (if (not (file-exists-p path))
        (ast--fail name (format "%s not found" file))
      (with-temp-buffer
        (insert-file-contents path)
        (ast--check name
                    (re-search-forward (format "(defun %s " (regexp-quote (symbol-name fn-name))) nil t)
                    (format "defined in %s" (file-name-nondirectory file))
                    (format "NOT FOUND in %s" (file-name-nondirectory file)))))))

(defun ast--test-command-var ()
  "Test that agent-shell uses the correct ACP command variable."
  (condition-case err
      (progn
        (require 'agent-shell-anthropic)
        ;; New variable name (post-rename)
        (if (boundp 'agent-shell-anthropic-claude-acp-command)
            (let ((cmd (car agent-shell-anthropic-claude-acp-command)))
              (ast--check "cmd:acp-command-var"
                          (executable-find cmd)
                          (format "command '%s' found at %s" cmd (executable-find cmd))
                          (format "command '%s' NOT IN exec-path" cmd)))
          ;; Old variable name (pre-rename)
          (if (boundp 'agent-shell-anthropic-claude-command)
              (let ((cmd (car agent-shell-anthropic-claude-command)))
                (ast--check "cmd:acp-command-var"
                            (executable-find cmd)
                            (format "(old var) command '%s' found" cmd)
                            (format "(old var) command '%s' NOT IN exec-path" cmd)))
            (ast--fail "cmd:acp-command-var" "Neither command variable is bound"))))
    (error (ast--fail "cmd:acp-command-var" (format "%s" (error-message-string err))))))

(defun ast--test-orchard-backend ()
  "Test orchard backend detection."
  (condition-case err
      (progn
        (require 'orchard-claude)
        (let ((backend (orchard--detect-backend)))
          (ast--check "orchard:backend"
                      (memq backend '(agent-shell claude-code))
                      (format "detected '%s'" backend)
                      (format "unexpected backend: %S" backend))))
    (error (ast--fail "orchard:backend" (format "%s" (error-message-string err))))))

(defun ast--test-orchard-cycle-handles-shell-maker ()
  "Test that orchard-cycle-mode checks for shell-maker-mode."
  (let ((file (expand-file-name "~/.doom.d/orchard.el")))
    (if (not (file-exists-p file))
        (ast--fail "orchard:cycle-shell-maker" "orchard.el not found")
      (with-temp-buffer
        (insert-file-contents file)
        ;; Find the orchard-cycle-mode defun and check it mentions shell-maker-mode
        (if (re-search-forward "(defun orchard-cycle-mode " nil t)
            (let* ((start (match-beginning 0))
                   (end (save-excursion (goto-char start) (forward-sexp) (point)))
                   (body (buffer-substring-no-properties start end)))
              (ast--check "orchard:cycle-shell-maker"
                          (string-match-p "shell-maker-mode" body)
                          "handles shell-maker-mode"
                          "MISSING shell-maker-mode check — M-m won't cycle from agent-shell buffers"))
          (ast--fail "orchard:cycle-shell-maker" "orchard-cycle-mode defun not found in orchard.el"))))))

(defun ast--test-config-agent-shell-vars ()
  "Test that config-agent-shell.el doesn't set obsolete variables."
  (let ((file (expand-file-name "~/.doom.d/config-agent-shell.el")))
    (if (not (file-exists-p file))
        (ast--fail "config:no-obsolete-vars" "config-agent-shell.el not found")
      (with-temp-buffer
        (insert-file-contents file)
        (let ((contents (buffer-string)))
          ;; Check for old variable name
          (ast--check "config:no-obsolete-claude-command"
                      (not (string-match-p "agent-shell-anthropic-claude-command[^-]" contents))
                      "not using obsolete agent-shell-anthropic-claude-command"
                      "USES OBSOLETE VAR agent-shell-anthropic-claude-command (renamed to -claude-acp-command)"))))))

(defun ast--test-orchard-claude-command ()
  "Test that orchard-claude.el uses correct command variable."
  (let ((file (expand-file-name "~/.doom.d/orchard-claude.el")))
    (if (not (file-exists-p file))
        (ast--fail "orchard:command-var" "orchard-claude.el not found")
      (with-temp-buffer
        (insert-file-contents file)
        (let ((contents (buffer-string)))
          (when (string-match-p "agent-shell-anthropic-claude-command[^-]" contents)
            (ast--fail "orchard:obsolete-command-var"
                       "orchard-claude.el uses OBSOLETE agent-shell-anthropic-claude-command"))
          (ast--check "orchard:command-var"
                      (or (string-match-p "agent-shell-anthropic-claude-acp-command" contents)
                          (not (string-match-p "claude-command" contents)))
                      "command variable OK"
                      "uses wrong command variable name"))))))

(defun ast--test-parens (file)
  "Test that FILE has balanced parentheses."
  (let ((name (format "parens:%s" (file-name-nondirectory file))))
    (if (not (file-exists-p (expand-file-name file)))
        (ast--fail name "file not found")
      (condition-case err
          (with-temp-buffer
            (insert-file-contents (expand-file-name file))
            (emacs-lisp-mode)
            (check-parens)
            (ast--pass name "balanced"))
        (error (ast--fail name (format "UNBALANCED: %s" (error-message-string err))))))))

;; ─── Runner ──────────────────────────────────────────────────────────────────

(defun run-agent-shell-tests ()
  "Run all agent-shell migration smoke tests."
  (interactive)
  (setq ast--results nil)

  ;; Parens checks (fast, catches dumb mistakes)
  (ast--test-parens "~/.doom.d/config-agent-shell.el")
  (ast--test-parens "~/.doom.d/config-claude.el")
  (ast--test-parens "~/.doom.d/orchard-claude.el")
  (ast--test-parens "~/.doom.d/orchard.el")

  ;; Executable
  (ast--test-executable-in-path)

  ;; Package loading
  (ast--test-package-loadable 'shell-maker)
  (ast--test-package-loadable 'acp)
  (ast--test-package-loadable 'agent-shell)
  (ast--test-package-loadable 'agent-shell-anthropic)

  ;; Version requirements
  (ast--test-version 'shell-maker 'shell-maker-version "0.90.1")
  (ast--test-version 'acp 'acp-package-version "0.11.1")

  ;; Command variable (the rename that bit us)
  (ast--test-command-var)

  ;; Static analysis of config files for obsolete vars
  (ast--test-config-agent-shell-vars)
  (ast--test-orchard-claude-command)

  ;; Orchard integration
  (ast--test-orchard-backend)
  (ast--test-function-defined-in-source "fn:orchard-cycle-mode" 'orchard-cycle-mode "~/.doom.d/orchard.el")
  (ast--test-function-defined-in-source "fn:claude-toggle-copy-mode" 'claude-toggle-copy-mode "~/.doom.d/config-claude.el")
  (ast--test-orchard-cycle-handles-shell-maker)

  ;; Report
  (setq ast--results (nreverse ast--results))
  (let* ((failures (cl-remove-if (lambda (r) (cadr r)) ast--results))
         (passes (cl-remove-if-not (lambda (r) (cadr r)) ast--results))
         (report (with-temp-buffer
                   (insert (format "Agent Shell Migration Tests: %d passed, %d FAILED\n"
                                   (length passes) (length failures)))
                   (insert (make-string 70 ?─) "\n")
                   (dolist (r ast--results)
                     (let ((name (car r))
                           (pass (cadr r))
                           (msg (cddr r)))
                       (insert (format "  %s %-40s %s\n"
                                       (if pass "✓" "✗")
                                       name msg))))
                   (insert (make-string 70 ?─) "\n")
                   (when failures
                     (insert "\nFAILURES:\n")
                     (dolist (f failures)
                       (insert (format "  ✗ %s: %s\n" (car f) (cddr f)))))
                   (buffer-string))))
    (if noninteractive
        ;; Batch mode: print and exit
        (progn
          (message "%s" report)
          (when failures
            (kill-emacs 1)))
      ;; Interactive: show buffer
      (with-current-buffer (get-buffer-create "*Agent Shell Tests*")
        (erase-buffer)
        (insert report)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(provide 'test-agent-shell-migration)
;;; test-agent-shell-migration.el ends here
