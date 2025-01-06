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


(load "~/.doom.d/config-bindings.el")
;;(load! config-bindings)
(load "~/.doom.d/config-quad-screen")
;;(triple-screen)
(load "~/.doom.d/config-string-inflection")
(load "~/.doom.d/config-custom-movement")
;;(load "~/.doom.d/config-black")
(load "~/.doom.d/config-ffap")
(load "~/.doom.d/config-magit")
(load "~/.doom.d/config-windsurf")


;;TODO: make this conditional
(setq-default auto-fill-function 'do-auto-fill)

;; ;;this line ostensibly disables lsp formatting for python because it does not work
;; (setq-hook! 'python-mode-hook +format-with-lsp nil)
;; (setq +format-with-lsp nil)

;;(straight-use-package 'org-jira)

(setq auth-sources '("~/.authinfo"))

;;Figure out why this only works on open projects...
(setq projectile-switch-project-action #'magit-status)
(setq projectile-enable-caching t)

;;(setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
;; this claims to be working, but it does not work
(setq projectile-project-search-path '("~/src/"))
(setq projectile-auto-discover t)

;; TODO: figure out how to use this:
;; snippet from here: https://www.emacswiki.org/emacs/AutoFillMode
;; (defun comment-auto-fill ()
;;   (setq-local comment-auto-fill-only-comments t)
;;   (auto-fill-mode 1)
;;   )

;;(load "~/.doom.d/config-ejira")
;;(load "~/.doom.d/config-sql")
(load "~/.doom.d/config-custom-expand")
;;(load "~/.doom.d/config-rotate-text")

;; lsp module completion working here with sync and restart

(load "~/.doom.d/config-vterm")
(load "~/.doom.d/config-undo-tree")
(load "~/.doom.d/config-org-mode")

(load "~/.doom.d/config-python")
(load "~/.doom.d/config-file-location")
(load "~/.doom.d/config-geojson")

(load "~/.doom.d/config-tree-sitter.el")
(load "~/.doom.d/config-ts-conf.el")
(load "~/.doom.d/config-ts-python.el")
(load "~/.doom.d/config-ts-yaml.el")
(load "~/.doom.d/config-latex.el")
(load "~/.doom.d/config-ts-typescript.el")
(load "~/.doom.d/config-region-bindings-mode.el")
(load "~/.doom.d/config-indent-tools.el")
(load "~/.doom.d/config-vscode.el")

;; Restore magit keybinding
(global-set-key (kbd "M-m") 'magit-status)

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

(load "~/.doom.d/config-tree-sitter")
(load "~/.doom.d/config-vscode")
(load "~/.doom.d/config-crux")
(load "~/.doom.d/config-firefox")
(load "~/.doom.d/config-projectile")
(load "~/.doom.d/config-biomejs-format")
(load "~/.doom.d/config-ruff")
(load "~/.doom.d/config-flycheck-biomejs")
(load "~/.doom.d/config-rc")
(load "~/.doom.d/config-igist")
(load "~/.doom.d/config-elisp")
(load "~/.doom.d/config-make")
(load "~/.doom.d/custom-completion.el")

(setq projectile-create-missing-test-files t)

(put 'projectile-ripgrep 'disabled nil)

(setq fancy-splash-image (concat doom-private-dir "black-hole-png-222.png"))

(load "~/.doom.d/config-jolly-brancher")
(load "~/.doom.d/config-llm")
