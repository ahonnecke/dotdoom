;;; config-aider.el -*- lexical-binding: t; -*-


;; Add the directory containing `aider.el` to the `load-path`
(add-to-list 'load-path "~/src/aider.el")

;; Require the main `aider` file
(require 'aider)

(use-package aider
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;;Or use chatgpt model since it is most well known
  (setq aider-args '("--model" "gpt-4o-mini"))
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;; config-aider.el ends here
