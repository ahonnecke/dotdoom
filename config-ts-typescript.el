;;; ../src/home/.doom.d/config-typescript.el -*- lexical-binding: t; -*-

(load "~/.doom.d/config-vitest")

;; Define the Vitest command for the current test file.
(defun run-vitest-current-test-file ()
  "Run Vitest for the current test file from the client directory."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (or (projectile-project-root) default-directory))
         (client-dir (concat project-root "client/"))
         (relative-file (file-relative-name current-file client-dir))
         (command (format "cd %s && npm run test -- %s" client-dir relative-file)))
    (compile command)))

;; Add TypeScript-TSX mode configuration.
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq-local fill-column 138)
            (setq-local ffip-patterns '("*.ts" "*.tsx"))
            )
          )

;; Function to run Biome check and update buffer.
(defun run-biome-check-write ()
  "Run `npx @biomejs/biome check --write` on the current file and update the buffer."
  (when (and buffer-file-name
             (string-match-p "\\.\\(ts\\|tsx\\|js\\|jsx\\)\\'" buffer-file-name))
    (let ((exit-code (call-process "npx" nil nil nil
                                   "@biomejs/biome" "check" "--write" buffer-file-name)))
      (if (zerop exit-code)
          (progn
            (revert-buffer t t t) ; Reload the buffer to reflect changes
            (message "Biome check --write applied successfully"))
        (message "Biome check --write failed with exit code %d" exit-code)))))

;; Add after-save-hook for Biome check.
(add-hook 'after-save-hook #'run-biome-check-write)

;; Define a command to run Vitest in watch mode for the current project.
(defun run-vitest-current-project-watch ()
  "Run Vitest in watch mode for the current project from the client directory."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (client-dir (concat project-root "client/"))
         (command (format "cd %s && npm run test:watch --" client-dir)))
    ;; Use `compilation-start` to make the buffer persistent.
    (compilation-start command 'compilation-mode)))

(add-hook 'typescript-tsx-mode-hook
          (lambda ()
            (define-key typescript-tsx-mode-map (kbd "C-c t w") 'run-vitest-current-project-watch)
            (define-key typescript-tsx-mode-map (kbd "C-c t f") 'run-vitest-current-test-file)
            (define-key typescript-tsx-mode-map (kbd "C-c t t") 'run-vitest-current-project)))
