;;; config-gptel.el -*- lexical-binding: t; -*-

;; gptel from local checkout (~/src/gptel) — bleeding edge with tool-use support.
;; MELPA version does NOT have gptel-make-tool yet.

;;; Code:

(defvar autoload-compute-prefixes nil
  "Suppress autoload prefix computation; workaround for straight.el.")

(add-to-list 'load-path "~/src/gptel")
(require 'gptel)
(require 'gptel-curl)
(require 'gptel-ollama)
(require 'gptel-anthropic)

;; Anthropic backend (uses ANTHROPIC_API_KEY from env)
;; NOTE: This is pay-per-token API, not the Max subscription.
;; To avoid accidental spend, switch to Ollama for casual use.
(gptel-make-anthropic "Claude"
  :key (getenv "ANTHROPIC_API_KEY")
  :stream t
  :models '(claude-sonnet-4-20250514
            claude-opus-4-20250514
            claude-haiku-4-5-20251001))

;; Ollama backend on local network (free, for local model testing)
(gptel-make-ollama "Ollama"
  :host "10.0.1.111:11434"
  :stream t
  :models '(qwen2.5-coder:32b))

;; Default to Claude
(setq gptel-backend (gptel-make-anthropic "Claude"
                      :key (getenv "ANTHROPIC_API_KEY")
                      :stream t
                      :models '(claude-sonnet-4-20250514))
      gptel-model 'claude-sonnet-4-20250514)

;; gptel-talon shelved (2026-04-01) — gptel tool-use too immature.
;; See ~/src/gptel-talon/PLAN.md for re-evaluation triggers.
;; (add-to-list 'load-path "~/src/gptel-talon")
;; (require 'gptel-talon-tools)
;; (gptel-talon-activate)

;; C-c L = LLM chat (gptel)
(global-set-key (kbd "C-c L") #'gptel)

;;; config-gptel.el ends here
