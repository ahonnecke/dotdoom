;;; config-gptel.el -*- lexical-binding: t; -*-

;; load ~/src/gptel

;;; Code:

;; Add the directory containing `gptel.el` to the `load-path`
(add-to-list 'load-path "~/src/gptel")

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "10.0.1.111:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(qwen2.5-coder:32b))          ;List of models

;; Make default
;; (setq
;;  gptel-model 'mistral:latest
;;  gptel-backend (gptel-make-ollama "Ollama"
;;                  :host "10.0.1.111:11434:11434"
;;                  :stream t
;;                  :models '(qwen2.5-coder:32b)))

;; Require the main `gptel` file
(require 'gptel)

;; C-c L = Local LLM (gptel via Ollama)
(global-set-key (kbd "C-c L") #'gptel)

;;; config-gptel.el ends here
