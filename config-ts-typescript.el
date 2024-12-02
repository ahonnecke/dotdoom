;;; ../src/home/.doom.d/config-typescript.el -*- lexical-binding: t; -*-

(add-hook 'typescript-ts-mode
          (lambda ()
            (setq-local fill-column 138)
            (setq-local ffip-patterns '("*.ts"))))

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

(add-hook 'after-save-hook #'run-biome-check-write)
