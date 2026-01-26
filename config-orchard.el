;;; config-orchard.el --- Worktree manager with column-locked windows -*- lexical-binding: t; -*-
;;
;; ╔═══════════════════════════════════════════════════════════════════════════╗
;; ║                              ORCHARD                                       ║
;; ║                                                                            ║
;; ║         A place where you tend many branches                               ║
;; ║                                                                            ║
;; ║  Worktree dashboard + column-locked window management                      ║
;; ╚═══════════════════════════════════════════════════════════════════════════╝
;;
;; WINDOW MODEL:
;;   - Dashboard stays in column 0 (leftmost), never replaced
;;   - Each branch gets its own column (1, 2, 3...)
;;   - M-m cycles magit ↔ claude in the SAME window
;;   - Commando takes over the full column, q returns to magit
;;   - Max columns configurable (default 4)
;;
;; KEYBINDINGS:
;;   C-c O O  - Open Orchard dashboard
;;   M-m      - Cycle magit/claude in current column
;;   `        - Commando (in magit)
;;
;; This file is now a thin shim that loads the modular orchard system.
;; The actual implementation is split across:
;;   - orchard-vars.el     - Variables, customization, faces
;;   - orchard-cache.el    - Caching and persistence
;;   - orchard-worktree.el - Worktree data and detection
;;   - orchard-window.el   - Column and window management
;;   - orchard-claude.el   - Claude integration
;;   - orchard-dashboard.el - Dashboard mode and formatting
;;   - orchard-actions.el  - Commands and actions
;;   - orchard.el          - Main entry point (ties it all together)

;; Add doom.d to load path if not already there
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load the modular orchard system
(require 'orchard)

(provide 'config-orchard)
;;; config-orchard.el ends here
