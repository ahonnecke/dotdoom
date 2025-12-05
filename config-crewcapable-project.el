;;; config-crewcapable-project.el --- CrewCapable-specific orchard settings -*- lexical-binding: t; -*-
;;
;; Project-specific configuration for crewcapableai development.
;; This sets up orchard variables for the crewcapable project.

(require 'config-orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; CrewCapable Orchard Settings
;;; ════════════════════════════════════════════════════════════════════════════

(setq orchard-repo-path (expand-file-name "~/src/.crewcapableai.main"))
(setq orchard-worktree-parent (expand-file-name "~/src"))
(setq orchard-worktree-prefix "crewcapableai")
(setq orchard-upstream-branch "upstream/dev")
(setq orchard-shared-claude-settings
      (expand-file-name "~/src/.crewcapableai.shared/settings.local.json"))
(setq orchard-shared-claude-commands
      (expand-file-name "~/src/.crewcapableai.shared/.claude/commands"))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Legacy Keybindings (for muscle memory)
;;; ════════════════════════════════════════════════════════════════════════════

;; Keep C-c C C working for now (alias to orchard)
(define-key ashton-mode-map (kbd "C-c C C") #'orchard)
(define-key ashton-mode-map (kbd "C-c C ?") #'orchard-dispatch)
(define-key ashton-mode-map (kbd "C-c C n") #'orchard-new-feature)
(define-key ashton-mode-map (kbd "C-c C f") #'orchard-new-feature)

(provide 'config-crewcapable-project)
;;; config-crewcapable-project.el ends here
