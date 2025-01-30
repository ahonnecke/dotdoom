;;; #+BEGIN_SRC emacs-lisp
;;; ~/.emacs.d/config-ellama.el -*- lexical-binding: t; -*-

;; Add the directory containing `ellama.el` to the `load-path`
(add-to-list 'load-path "~/src/ellama")

;; Set the server URL and model name BEFORE loading ellama
(setq ellama-server-url "http://10.0.1.111:11434")
(setq ellama-model-name "qwen2.5-coder:32b")

;; Manually set the provider (overrides the default "zephyr")
(setq ellama-provider
      (make-llm-ollama :chat-model ellama-model-name
                       :embedding-model ellama-model-name
                       :host "10.0.1.111"
                       :port 11434))

;; Set user and assistant nicknames
(setq ellama-user-nick "Ashton")
(setq ellama-assistant-nick "SmartRock")

;; Load ellama AFTER setting the provider
(require 'ellama)

;;; config-ellama.el ends here
;;; #+END_SRC
