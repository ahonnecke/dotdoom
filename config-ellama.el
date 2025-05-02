;;; ~/.emacs.d/config-ellama.el -*- lexical-binding: t; -*-

;; Add the directory containing `ellama.el` to the `load-path`
;; (add-to-list 'load-path "~/src/ellama")

;; ;; Set the server URL and model name BEFORE loading ellama
;; (setq ellama-server-url "http://10.0.1.111:11434")
;; (setq ellama-model-name "qwen2.5-coder:32b")

;; ;; Load ellama AFTER setting the variables
;; (require 'ellama)

;; ;; Force ellama to use the new model
;; (ellama-set-model ellama-model-name)

;;; config-ellama.el ends here
