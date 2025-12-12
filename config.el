;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ashton Honnecke"
      user-mail-address "ashton@pixelstub.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;(load! config-bindings)
(load "~/.doom.d/config-quad-screen")
;;(triple-screen)
(load "~/.doom.d/config-inflection")  ; replaces config-string-inflection
(load "~/.doom.d/config-custom-movement")
;;(load "~/.doom.d/config-black")
(load "~/.doom.d/config-ffap")
(load "~/.doom.d/config-magit")


;;TODO: make this conditional
(setq-default auto-fill-function 'do-auto-fill)

;; ;;this line ostensibly disables lsp formatting for python because it does not work
;; (setq-hook! 'python-mode-hook +format-with-lsp nil)
;; (setq +format-with-lsp nil)

;;(straight-use-package 'org-jira)

(setq auth-sources '("~/.authinfo"))

;; TODO: figure out how to use this:
;; snippet from here: https://www.emacswiki.org/emacs/AutoFillMode
;; (defun comment-auto-fill ()
;;   (setq-local comment-auto-fill-only-comments t)
;;   (auto-fill-mode 1)
;;   )

(load "~/.doom.d/config-ash-mode")
(load "~/.doom.d/config-custom-expand")

;; lsp module completion working here with sync and restart

(load "~/.doom.d/config-vterm")
(load "~/.doom.d/config-undo-tree")
(load "~/.doom.d/config-org-mode")

;; config-python.el deprecated - using config-ts-python.el instead
(load "~/.doom.d/config-file-location")
(load "~/.doom.d/config-geojson")

(load "~/.doom.d/config-tree-sitter")
(load "~/.doom.d/config-ts-conf")
(load "~/.doom.d/config-ts-python")
(load "~/.doom.d/config-ts-yaml")
(load "~/.doom.d/config-latex")
(load "~/.doom.d/config-ts-typescript")
(load "~/.doom.d/config-region-bindings-mode")
(load "~/.doom.d/config-indent-tools")

;; M-m is handled by orchard-cycle-mode in config-orchard.el
;; (global-set-key (kbd "M-m") 'magit-status)

(global-set-key (kbd "C-.") 'company-files)

;; (set-company-backend!
;;   '(text-mode
;;     markdown-mode
;;     gfm-mode
;;     forge-post-mode
;;     )
;;   '(:seperate
;;     company-ispell
;;     company-files
;;     company-yasnippet
;;     company-dict
;;     ))


;; ;; ;; ;; I'm not really sure what this does
;; ;; ;; (load "~/.doom.d/config-vterm-completion.el")

;; ;; ;; ;; this is breaking vterm completion
;; ;; (load "~/.doom.d/config-company-dict")

;; ;; ;; TODO: get ispell working in python mode
;; ;; ;; TODO: add company-files to yaml-mode
;; ;; ;; TODO: add aws logs to company all

;; ;; ;; hook stuff
;; ;; ;; https://emacs.stackexchange.com/questions/10966/global-autorevert-mode-doesnt-seem-to-work/10971#10971

;; ;; ;; ostensibly will disallow a minor mode in a given major mode
;; ;; ;; specfically hoping to use this to not turn on minor modes in vterm
;; ;; ;;(add-hook 'SOME-HOOK (lambda () (MINOR-MODE-NAME -1)))
;; ;; ;; completely untested example (and not currently needed)
;; ;; ;;(add-hook 'vterm-mode-hook (lambda () (undo-tree-mode -1)))

;; ;; lsp module completion working here with sync and restart

;; ;; TODO: configure gists

(setq exec-path (append exec-path '("/home/ahonnecke/.nvm/versions/node/v19.3.0/bin")))

(setq doom--prefer-lexical-binding t)

(load "~/.doom.d/config-crux")
(load "~/.doom.d/config-firefox")
(load "~/.doom.d/config-projectile")
(load "~/.doom.d/config-biomejs-format")
(load "~/.doom.d/config-ruff")
(load "~/.doom.d/config-flycheck-biomejs")
(load "~/.doom.d/config-rc")
;;(load "~/.doom.d/config-igist")
(load "~/.doom.d/config-elisp")
(load "~/.doom.d/config-make")
(load "~/.doom.d/config-ripgrep")

;; Additional useful configs
(load "~/.doom.d/config-windmove")
(load "~/.doom.d/config-tabs")
(load "~/.doom.d/config-browse")
(load "~/.doom.d/config-json-mode")
(load "~/.doom.d/config-org-download")
(load "~/.doom.d/config-nudge")
(load "~/.doom.d/config-vitest")
(load "~/.doom.d/config-wayward")
(load "~/.doom.d/config-combobulate")

(setq projectile-create-missing-test-files t)

(put 'projectile-ripgrep 'disabled nil)

;;(setq fancy-splash-image (concat doom-private-dir "black-hole-png-222.png"))

(load "~/.doom.d/config-direnv")
(load "~/.doom.d/config-jolly-brancher")
(load "~/.doom.d/config-llm")
(load "~/.doom.d/config-mcp")  ; MCP tools for gptel (filesystem, fetch)
(load "~/.doom.d/config-consult-embark")  ; MOVEC stack: consult, embark, marginalia, orderless
;; config-ellama.el disabled - entirely commented out
(load "~/.doom.d/config-bookmark")
(load "~/.doom.d/config-aider")
(load "~/.doom.d/config-smerge")
;; (load "~/.doom.d/custom-completion")  ; ARCHIVED - replaced by config-corfu.el (2025-12-05)
(load "~/.doom.d/config-corfu")          ; Corfu + Cape completion
;; (load "~/.doom.d/config-standup")  ; DEPRECATED - replaced by config-meeting.el
(load "~/.doom.d/config-meeting")      ; Meeting notes system (standup, syncs, etc.)
(load "~/.doom.d/config-slack")     ; Slack client (C-c K)
(load "~/.doom.d/config-workspace")
(load "~/.doom.d/config-claude")
(load "~/.doom.d/config-monet")  ; Claude IDE integration (selection sharing, diagnostics)
(load "~/.doom.d/config-casual")  ; Transient menus for built-in modes
(load "~/.doom.d/config-ghq")
(load "~/.doom.d/config-orchard")
(load "~/.doom.d/config-crewcapable-project")  ; Project-specific orchard settings
(load "~/.doom.d/config-commando")
(load "~/.doom.d/config-pr")
(load "~/.doom.d/config-caldav")
(load "~/.doom.d/config-calendar")  ; Read-only calendar viewer (C-c @)
(load "~/.doom.d/config-testicular")  ; Manual testing mode (renamed from test-flow)
(load "~/.doom.d/config-vercel")      ; Vercel CLI transient (C-c V)
(load "~/.doom.d/config-supabase")    ; Supabase CLI transient (C-c B)
(load "~/.doom.d/config-aws")         ; AWS CLI transient (C-c A)
(load "~/.doom.d/config-crewcapable-services") ; Vercel/Supabase layer for crewcapable

;;(load "~/.doom.d/config-jolly-brancher")
;;Load my ash-mode binding last
(load "~/.doom.d/config-bindings")
(load "~/.doom.d/config-cheatsheet")  ; C-c ? for help

;; Open Orchard on startup
(setq inhibit-startup-screen t
      initial-buffer-choice #'orchard)
