;;; orchard-vars.el --- Orchard variables and faces -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains all defgroup, defcustom, defvar, and defface
;; definitions used by other orchard-*.el files.

(require 'transient)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup orchard nil
  "Worktree management with column-locked windows."
  :group 'tools
  :prefix "orchard-")

(defcustom orchard-max-columns 4
  "Maximum number of columns (including dashboard).
When exceeded, new branches replace existing columns."
  :type 'integer
  :group 'orchard)

(defcustom orchard-repo-path nil
  "Path to the main repository (primary worktree).
Set this in your project-specific config."
  :type '(choice directory (const nil))
  :group 'orchard)

(defcustom orchard-worktree-parent (expand-file-name "~/src")
  "Parent directory where worktrees are created."
  :type 'directory
  :group 'orchard)

(defcustom orchard-upstream-branch "upstream/dev"
  "Upstream branch to base new features on."
  :type 'string
  :group 'orchard)

(defcustom orchard-worktree-prefix nil
  "Prefix for worktree directories (e.g., \"myproject\").
With nested structure: worktrees are PREFIX/BRANCH-name.
With flat structure: worktrees are PREFIX--BRANCH-name.
If nil, uses repo name."
  :type '(choice string (const nil))
  :group 'orchard)

(defcustom orchard-nested-worktrees t
  "If non-nil, use nested directory structure for worktrees.
Nested:  ~/src/project/FEATURE-name/
Flat:    ~/src/project--FEATURE-name/ (legacy)"
  :type 'boolean
  :group 'orchard)

(defcustom orchard-branch-types
  '(("FEATURE" . "f")
    ("BUGFIX"  . "b")
    ("CHORE"   . "c")
    ("REFACTOR" . "r")
    ("DOCS"    . "d")
    ("EXPERIMENT" . "e")
    ("TEST"    . "t"))
  "Branch type prefixes with unique first character for tab completion."
  :type '(alist :key-type string :value-type string)
  :group 'orchard)

(defcustom orchard-shared-claude-settings nil
  "Path to shared Claude settings.local.json file, or nil."
  :type '(choice file (const nil))
  :group 'orchard)

(defcustom orchard-shared-claude-commands nil
  "Path to shared Claude commands directory, or nil."
  :type '(choice directory (const nil))
  :group 'orchard)

(defcustom orchard-post-create-hook nil
  "Hook run after creating a new worktree.
Called with the worktree path as argument."
  :type 'hook
  :group 'orchard)

(defcustom orchard-state-file (expand-file-name "~/.orchard-state.eld")
  "File to persist orchard state (dev ownership, stage overrides)."
  :type 'file
  :group 'orchard)

(defcustom orchard-research-paths
  '(("triage" . "~/src/cc.rnd/"))
  "Alist of (name . path) for research directories.
These are non-development directories used for debugging, demos,
meeting prep, PR triage, and general investigation.

Example:
  \\='((\"triage\" . \"~/src/cc.rnd/\")
    (\"psc-sync\" . \"~/src/psc-sync-notes/\"))"
  :type '(alist :key-type string :value-type directory)
  :group 'orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Cache TTLs
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom orchard-worktrees-cache-ttl 30
  "Seconds before worktrees cache is considered stale."
  :type 'integer
  :group 'orchard)

(defcustom orchard-issues-cache-ttl 300
  "Seconds before GitHub issues cache is considered stale (5 minutes)."
  :type 'integer
  :group 'orchard)

(defcustom orchard-merged-cache-ttl 600
  "Seconds before merged branches cache is considered stale (10 minutes)."
  :type 'integer
  :group 'orchard)

(defcustom orchard-claude-sessions-cache-ttl 60
  "Seconds before Claude sessions cache is considered stale."
  :type 'integer
  :group 'orchard)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Faces
;;; ════════════════════════════════════════════════════════════════════════════

(defface orchard-header
  '((t :foreground "#61AFEF" :weight bold :height 1.2))
  "Face for dashboard header."
  :group 'orchard)

(defface orchard-subheader
  '((t :foreground "#ABB2BF" :weight bold))
  "Face for section headers."
  :group 'orchard)

(defface orchard-branch-feature
  '((t :foreground "#98C379"))
  "Face for FEATURE branches (green)."
  :group 'orchard)

(defface orchard-branch-bugfix
  '((t :foreground "#E06C75"))
  "Face for BUGFIX branches (red)."
  :group 'orchard)

(defface orchard-branch-chore
  '((t :foreground "#56B6C2"))
  "Face for CHORE branches (cyan)."
  :group 'orchard)

(defface orchard-branch-refactor
  '((t :foreground "#E5C07B"))
  "Face for REFACTOR branches (yellow)."
  :group 'orchard)

(defface orchard-branch-docs
  '((t :foreground "#61AFEF"))
  "Face for DOCS branches (blue)."
  :group 'orchard)

(defface orchard-branch-experiment
  '((t :foreground "#C678DD"))
  "Face for EXPERIMENT branches (purple)."
  :group 'orchard)

(defface orchard-branch-test
  '((t :foreground "#ABB2BF"))
  "Face for TEST branches (gray)."
  :group 'orchard)

(defface orchard-branch-main
  '((t :foreground "#5C6370"))
  "Face for main/dev branch (gray)."
  :group 'orchard)

(defface orchard-current
  '((t :foreground "#C678DD" :weight bold))
  "Face for current worktree (magenta)."
  :group 'orchard)

(defface orchard-dirty
  '((t :foreground "#E06C75"))
  "Face for dirty indicator (red)."
  :group 'orchard)

(defface orchard-clean
  '((t :foreground "#98C379"))
  "Face for clean indicator (green)."
  :group 'orchard)

(defface orchard-claude-running
  '((t :foreground "#61AFEF" :weight bold))
  "Face for running Claude indicator (blue)."
  :group 'orchard)

(defface orchard-claude-stopped
  '((t :foreground "#E06C75"))
  "Face for stopped Claude indicator (red)."
  :group 'orchard)

(defface orchard-branch-mismatch
  '((t :foreground "#E06C75" :weight bold))
  "Face for branch mismatch warning (red bold)."
  :group 'orchard)

(defface orchard-key
  '((t :foreground "#E5C07B" :weight bold))
  "Face for keybinding hints."
  :group 'orchard)

(defface orchard-hl-line
  '((((class color) (background light))
     :background "#c8ddf8" :extend t)
    (((class color) (background dark))
     :background "#2c4a6e" :extend t))
  "Face for highlighting the current line."
  :group 'orchard)

(provide 'orchard-vars)
;;; orchard-vars.el ends here
