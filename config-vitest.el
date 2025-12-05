;;; ~/.doom.d/config-vitest.el -*- lexical-binding: t; -*-

(defun run-vitest-current-test-file ()
  "Run Vitest for the current test file from the client directory."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (or (projectile-project-root) default-directory))
         (client-dir (concat project-root "client/"))
         (relative-file (file-relative-name current-file client-dir))
         (command (format "cd %s && npm run test -- %s" client-dir relative-file)))
    (compile command)))

(defun run-vitest-current-project ()
  "Run all Vitest tests for the current project from the client directory."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (client-dir (concat project-root "client/"))
         (command (format "cd %s && npm run test" client-dir)))
    (compile command)))
