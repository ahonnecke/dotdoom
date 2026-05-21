;;; ~/.doom.d/config-anvil.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Anvil — Emacs as MCP server for LLM agents.
;; Exposes IDE context (xref, diagnostics, tree-sitter, buffers, git)
;; so Claude can query Emacs state directly via MCP tools.
;; See: https://github.com/ahonnecke/anvil.el

;;; Code:

(use-package! anvil
  :demand t
  :config
  ;; Default modules + ide (the unique value — xref, diagnostics, tree-sitter)
  (setq anvil-modules '(worker eval file host git proc fs emacs text clipboard data net ide))
  ;; Skip org, xlsx, pdf, cron for now — not needed for the eval trial
  (setq anvil-optional-modules nil)
  (anvil-enable))

(provide 'config-anvil)
;;; config-anvil.el ends here
