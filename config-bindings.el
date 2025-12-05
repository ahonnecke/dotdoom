;;; ../src/home/.doom.d/config-bindings.el -*- lexical-binding: t; -*-

;; Helper for consult-line with thing-at-point (replaces swiper-thing-at-point)
(defun consult-line-thing-at-point ()
  "Search for thing at point using consult-line."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(define-key ashton-mode-map (kbd "C-;") 'comment-or-uncomment-region)
(define-key ashton-mode-map (kbd "C-'") 'er/expand-region)
(define-key ashton-mode-map (kbd "C-\"") 'er/contract-region)
;;(define-key ashton-mode-map (kbd "TAB") 'indent-region)
(define-key ashton-mode-map (kbd "C-x b") 'consult-buffer)
;; M-g is a PREFIX key in stock Emacs (goto-map)
;; Don't override it directly - use M-s r for ripgrep instead (search prefix)
;; (define-key ashton-mode-map (kbd "M-g") '+default/search-project)  ; REMOVED - breaks M-g prefix
(define-key ashton-mode-map (kbd "C-x f") 'projectile-find-file)
(define-key ashton-mode-map (kbd "C-x C-f") 'find-file)

(define-key ashton-mode-map (kbd "M-n") 'sp-forward-sexp)
(define-key ashton-mode-map (kbd "M-p") 'sp-backward-sexp)

(define-key ashton-mode-map (kbd "C-w") 'kill-region)
;; they all close when i restart emacs, maybe after server mode is working...
;;(define-key ashton-mode-map (kbd "M-o") 'projectile-switch-open-project)

(define-key ashton-mode-map (kbd "C-x C-k") 'kill-buffer)
(define-key ashton-mode-map (kbd "C-c C-d") 'deadgrep)

(define-key ashton-mode-map (kbd "C-c C-g") '+default/search-project)

;; Change indent to match vscode... sigh.

(define-key ashton-mode-map (kbd "M-r") 'consult-recent-file)

(define-key ashton-mode-map (kbd "C-c q") 'query-replace)

;;projectile
(define-key ashton-mode-map (kbd "C-c M-p r") 'projectile-replace)

;; M-s is a PREFIX key in stock Emacs (search-map)
;; Use M-s . for thing-at-point (matches stock isearch-forward-symbol-at-point)
(define-key ashton-mode-map (kbd "M-s .") 'consult-line-thing-at-point)

;; move to only python mode, or maybe just get leader working?
(define-key ashton-mode-map (kbd "C-c t") 'python-pytest-dispatch)

(define-key ashton-mode-map (kbd "C-c v h") '+vterm/here)
(define-key ashton-mode-map (kbd "C-c v t") '+vterm/toggle)
(define-key ashton-mode-map (kbd "C-M-t") '+vterm/toggle)
(define-key ashton-mode-map (kbd "C-M-v") '+vterm/toggle)
;;(define-key ashton-mode-map (kbd "C-M-t") '+vterm/toggle)

;; C-c g (go to thing)
;;(define-key ashton-mode-map (kbd "C-c g r") 'browse-at-remote)
;; TODO: move to hydra?
;; GOTO keys:
(define-key ashton-mode-map (kbd "C-c g g") '+vc/browse-at-remote)
(define-key ashton-mode-map (kbd "C-c g f") 'find-file-at-point-with-line)
(define-key ashton-mode-map (kbd "C-c g u") 'browse-url-of-file)
(define-key ashton-mode-map (kbd "C-c g t") 'projectile-toggle-between-implementation-and-test)
(define-key ashton-mode-map (kbd "C-c g b") 'browse-url-at-point)
(define-key ashton-mode-map (kbd "C-c g s") 'firefox-search-region)
(define-key ashton-mode-map (kbd "C-c g l") 'avy-goto-line)
(define-key ashton-mode-map (kbd "C-c g c") 'avy-goto-char)


;; String inflection bindings (C-c i = inflection)
(define-key ashton-mode-map (kbd "C-c i _") 'xah-cycle-hyphen-underscore-space)
(define-key ashton-mode-map (kbd "C-c i c") 'string-inflection-camelcase)
(define-key ashton-mode-map (kbd "C-c i s") 'string-inflection-underscore)  ; s = snake_case
(define-key ashton-mode-map (kbd "C-c i k") 'string-inflection-kebab-case)
(define-key ashton-mode-map (kbd "C-c i u") 'string-inflection-upcase)
(define-key ashton-mode-map (kbd "C-c i p") 'string-inflection-python-style-cycle)
(define-key ashton-mode-map (kbd "C-c i U") 'crux-upcase-region)
(define-key ashton-mode-map (kbd "C-c i l") 'crux-downcase-region)

;; Global standup bindings (C-c S = Standup, works from any buffer)
(define-key ashton-mode-map (kbd "C-c S s") 'standup)
(define-key ashton-mode-map (kbd "C-c S d") 'standup-done)
(define-key ashton-mode-map (kbd "C-c S g") 'standup-doing)
(define-key ashton-mode-map (kbd "C-c S b") 'standup-blocker)

;; Global workspace bindings (C-c w = Workspace)
(define-key ashton-mode-map (kbd "C-c w w") 'workspace-show)
(define-key ashton-mode-map (kbd "C-c w n") 'workspace-new-feature)
(define-key ashton-mode-map (kbd "C-c w s") 'workspace-sync)
(define-key ashton-mode-map (kbd "C-c w p") 'workspace-pull)
(define-key ashton-mode-map (kbd "C-c w P") 'workspace-pull-all)
(define-key ashton-mode-map (kbd "C-c w c") 'workspace-clean)
(define-key ashton-mode-map (kbd "C-c w C") 'workspace-clean-execute)
(define-key ashton-mode-map (kbd "C-c w j") 'workspace-jump)
(define-key ashton-mode-map (kbd "C-c w m") 'workspace-magit)
(define-key ashton-mode-map (kbd "C-c w e") 'workspace-test-evidence)
(define-key ashton-mode-map (kbd "C-c w ?") 'workspace-transient)

(define-key ashton-mode-map (kbd "M-<return>") 'hippie-expand)
;;(define-key ashton-mode-map (kbd "<tab>") 'hippie-expand)
(define-key ashton-mode-map (kbd "C-c n") 'goto-line)

(define-key ashton-mode-map (kbd "C-c C-p") 'beginning-of-buffer)
(define-key ashton-mode-map (kbd "C-c C-n") 'find-file)
(define-key ashton-mode-map (kbd "C-x 4") 'quad-screen)
(define-key ashton-mode-map (kbd "C-x 5") 'penta-screen)

(define-key ashton-mode-map (kbd "C-x k") 'kill-current-buffer)

;; use C-c c for all "complete here" bindings
;;(define-key ashton-mode-map (kbd "M-m") 'magit-status)
;; C-c m is now monet prefix - move dirvish-mark-menu to C-c d m (dired mark)
(define-key ashton-mode-map (kbd "C-c d m") 'dirvish-mark-menu)

(define-key ashton-mode-map (kbd "C-M-g") 'ripgrep-regexp)

(setq which-key-idle-delay 0.5)

;; (defun fix-bindings ()
;;   (interactive)
;;   (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
;;   (local-set-key (kbd "C-'") 'er/expand-region)
;;   (local-set-key (kbd "C-\"") 'er/contract-region)
;;   (local-set-key (kbd "TAB") 'indent-region)
;;   )

;; (define-key ashton-mode-map (kbd "C-x b") 'ivy-switch-buffer)
;; (local-unset-key (kbd "M-g"))
;; (define-key ashton-mode-map (kbd "M-g") 'projectile-ripgrep)


;; (defvar ashton-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map parent-mode-shared-map)    
;;     (define-key map (kbd "C-;") 'comment-or-uncomment-region)
;;     (define-key map (kbd "C-'") 'er/expand-region)
;;     (define-key map (kbd "C-\"") 'er/contract-region)
;;     (define-key map (kbd "TAB") 'indent-region)        
;;     map))

;; (use-local-map ashton-mode-map)

;;TODO:
;; update vscode to ahve this too
;; https://stackoverflow.com/questions/40203303/shortcut-key-for-selecting-a-word-and-extending-the-selection-in-vs-code

(define-key ashton-mode-map (kbd "C-c l c") 'ash-copy-current-line-position-to-clipboard)
(define-key ashton-mode-map (kbd "C-c l f") 'copy-full-path-to-clipboard)
(define-key ashton-mode-map (kbd "C-c l w") 'copy-wrapped-full-path-to-clipboard)

(define-key ashton-mode-map (kbd "C-c g n") 'goto-line)

;; Bind to C-c f in Dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c f") #'create-empty-file))


(define-minor-mode ashton-mode
  "A custom minor mode to bind ashton's keymaps."
  :init-value nil
  :global 1
  :lighter " AshtonMode"
  :keymap ashton-mode-map)

(ashton-mode 1)

;; ════════════════════════════════════════════════════════════════════════════
;; High-priority keybindings (set AFTER mode is enabled)
;; ════════════════════════════════════════════════════════════════════════════

;; M-m: Cycle between magit and claude (orchard-cycle-mode)
;; This must be set here at the end to ensure it takes precedence
(when (fboundp 'orchard-cycle-mode)
  (define-key ashton-mode-map (kbd "M-m") #'orchard-cycle-mode))

;; Ensure ashton-mode-map is at the front of minor-mode-map-alist
;; This gives it highest priority
(let ((ashton-entry (assq 'ashton-mode minor-mode-map-alist)))
  (when ashton-entry
    (setq minor-mode-map-alist
          (cons ashton-entry
                (delq ashton-entry minor-mode-map-alist)))))
