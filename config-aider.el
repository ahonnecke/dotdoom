;;; config-aider.el -*- lexical-binding: t; -*-

;; Load local aider.el package
(add-to-list 'load-path "~/src/aider.el")
(require 'aider)

;; Ensure Emacs can find the aider Python CLI in your venv
(add-to-list 'exec-path "~/.venvs/aider/bin")
(setenv "PATH" (concat "~/.venvs/aider/bin:" (getenv "PATH")))

;; ;; Optional: sync shell environment if starting Emacs from GUI
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package aider
  :config
  ;; Use GPT-4o-mini with your OPENAI_API_KEY from env
  (setq aider-args '("--model" "gpt-4o-mini-high"))

  ;; Optional: transient menu key
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;; config-aider.el ends here
