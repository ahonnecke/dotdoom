;;; ~/.doom.d/config-direnv.el -*- lexical-binding: t; -*-

;; Auto-allow direnv for trusted project directories
;; Eliminates the "direnv: error .envrc is blocked" messages

(after! envrc
  ;; Directories where .envrc files should be automatically trusted
  (defvar envrc-trusted-directories
    '("~/src/crewcapableai"  ; matches crewcapableai.0, crewcapableai--FEAT-*, etc.
      "~/src/crewcapable"
      "~/ghq")
    "List of directory prefixes where .envrc files are automatically allowed.")

  (defun envrc--auto-allow-trusted ()
    "Automatically run `direnv allow` for trusted directories."
    (when-let ((env-dir (envrc--find-env-dir)))
      (when (cl-some (lambda (trusted)
                       (string-prefix-p (expand-file-name trusted) env-dir))
                     envrc-trusted-directories)
        ;; Silently allow the .envrc
        (let ((default-directory env-dir))
          (call-process "direnv" nil nil nil "allow")))))

  ;; Advice: envrc--update is from envrc.el (third-party, can't modify directly).
  ;; Auto-allows .envrc in trusted directories before envrc loads the environment,
  ;; eliminating interactive "direnv: error .envrc is blocked" prompts.
  (advice-add 'envrc--update :before
              (lambda (&rest _)
                (ignore-errors (envrc--auto-allow-trusted)))))

;; Enable envrc globally
(envrc-global-mode 1)

(provide 'config-direnv)
;;; config-direnv.el ends here
