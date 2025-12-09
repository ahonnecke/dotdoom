;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! magit-delta)
(package! ob-graphql)
(package! direnv)
                                        ;(package! gist)
(package! xml+)
(package! python-black)
(package! region-bindings-mode)
(package! crux)

;; Claude Code CLI integration
(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el"))

;; ghq repo management
(package! ghq
  :recipe (:host github :repo "lafrenierejm/emacs-ghq"))

;; installed, but not working...
;;(package! combobulate)

;; Calendar sync with CalDAV (Google, Fastmail, etc)
(package! org-caldav)

;;; ════════════════════════════════════════════════════════════════════════════
;;; 2025 Modernization Packages
;;; ════════════════════════════════════════════════════════════════════════════

;; Monet: Claude IDE integration (selection sharing, diagnostics, diffs)
;; Same author as claude-code.el - complements it
(package! monet
  :recipe (:host github :repo "stevemolitor/monet"))

;; Aidermacs: Modern Aider integration (replaces aider.el)
;; Has architect mode with SOTA benchmark results
(package! aidermacs)

;; Casual Suite: Transient menus for built-in modes
;; (Calc, Dired, Info, Bookmarks, IBuffer, I-Search, Avy, Agenda)
(package! casual-suite)

;; Cape: Completion At Point Extensions (for when we switch to Corfu)
;; Provides modular completion backends
(package! cape)

;; MCP: Model Context Protocol support for gptel
;; Gives LLMs access to tools
(package! mcp
  :recipe (:host github :repo "lizqwerscott/mcp.el"))

;; Slack: Full Slack client in Emacs
;; Token/cookie auth via auth-source, can be automated via cron
(package! slack
  :recipe (:host github :repo "emacs-slack/emacs-slack"))

;; Wgrep: Editable grep buffers
;; Use with embark-export (C-. E) on grep results → edit → C-c C-c to apply
(package! wgrep)

;; Consult-flycheck: Navigate errors with consult preview
(package! consult-flycheck)

;; Consult-dir: Quick directory switching in minibuffer
;; Insert directory paths with C-x C-d, switch default-directory with C-x C-j
(package! consult-dir)

;; String-inflection: Convert between snake_case, camelCase, PascalCase, etc.
(package! string-inflection)

;; Posframe: Pop up frames at point (used for ace-window centering)
(package! posframe)
