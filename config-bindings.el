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

;;; ════════════════════════════════════════════════════════════════════════════
;;; C-c g = Go to (navigation)
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Mnemonic single letters, organized by type:
;;   Code: d(efinition) r(eferences) i(menu) e(rror) t(est)
;;   Avy:  l(ine) c(har) w(ord)
;;   External: g(ithub) b(rowser) f(ile-at-point)

;; Code navigation (LSP/xref)
(define-key ashton-mode-map (kbd "C-c g d") 'xref-find-definitions)
(define-key ashton-mode-map (kbd "C-c g r") 'xref-find-references)
(define-key ashton-mode-map (kbd "C-c g i") 'consult-imenu)
(define-key ashton-mode-map (kbd "C-c g I") 'consult-imenu-multi)  ; across project
(define-key ashton-mode-map (kbd "C-c g e") 'consult-flycheck)
(define-key ashton-mode-map (kbd "C-c g t") 'projectile-toggle-between-implementation-and-test)

;; Visual jump (avy) - for when you SEE where you want to go
(define-key ashton-mode-map (kbd "C-c g l") 'avy-goto-line)
(define-key ashton-mode-map (kbd "C-c g c") 'avy-goto-char-timer)  ; type chars, then jump
(define-key ashton-mode-map (kbd "C-c g w") 'avy-goto-word-1)      ; first char of word

;; External/files
(define-key ashton-mode-map (kbd "C-c g g") '+vc/browse-at-remote) ; GitHub/GitLab
(define-key ashton-mode-map (kbd "C-c g b") 'browse-url-at-point)
(define-key ashton-mode-map (kbd "C-c g f") 'find-file-at-point-with-line)

;; Transient menu for discoverability
(transient-define-prefix goto-transient ()
  "Go to places - navigation commands."
  ["Go to (C-c g)"
   ["Code"
    ("d" "Definition" xref-find-definitions)
    ("r" "References" xref-find-references)
    ("i" "Symbol (imenu)" consult-imenu)
    ("I" "Symbol (project)" consult-imenu-multi)
    ("e" "Error" consult-flycheck)
    ("t" "Test/Impl toggle" projectile-toggle-between-implementation-and-test)]
   ["Visual (avy)"
    ("l" "Line" avy-goto-line)
    ("c" "Char(s)" avy-goto-char-timer)
    ("w" "Word" avy-goto-word-1)]
   ["External"
    ("g" "GitHub" +vc/browse-at-remote)
    ("b" "URL at point" browse-url-at-point)
    ("f" "File at point" find-file-at-point-with-line)]])

(define-key ashton-mode-map (kbd "C-c g ?") 'goto-transient)


;; String inflection bindings moved to config-inflection.el
;; C-c i prefix: s=snake, c=camel, p=Pascal, u=CONST, k=kebab, i=smart-cycle, ?=menu

;;; ════════════════════════════════════════════════════════════════════════════
;;; Indent region left/right - with repeat mode!
;;; ════════════════════════════════════════════════════════════════════════════
;; C-c < or C-c > to start, then just < or > to keep adjusting
;; Press any other key to exit

(defun indent--get-step ()
  "Get the appropriate indent step for current buffer.
Uses mode-specific settings: python-indent-offset, typescript-indent-level,
js-indent-level, or defaults to tab-width (usually 4)."
  (or (and (boundp 'python-indent-offset) python-indent-offset)
      (and (boundp 'typescript-indent-level) typescript-indent-level)
      (and (boundp 'js-indent-level) js-indent-level)
      (and (boundp 'css-indent-offset) css-indent-offset)
      tab-width
      4))

(defun indent--region-bounds ()
  "Get region bounds, or current line if no region."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

(defun indent-region-left ()
  "Indent region/line left by mode-appropriate amount. Repeatable with <."
  (interactive)
  (let* ((bounds (indent--region-bounds))
         (step (indent--get-step))
         (deactivate-mark nil))
    (indent-rigidly (car bounds) (cdr bounds) (- step))
    (message "Indent: < or > to continue, any other key to stop")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<") #'indent-region-left)
       (define-key map (kbd ">") #'indent-region-right)
       map)
     t)))

(defun indent-region-right ()
  "Indent region/line right by mode-appropriate amount. Repeatable with >."
  (interactive)
  (let* ((bounds (indent--region-bounds))
         (step (indent--get-step))
         (deactivate-mark nil))
    (indent-rigidly (car bounds) (cdr bounds) step)
    (message "Indent: < or > to continue, any other key to stop")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<") #'indent-region-left)
       (define-key map (kbd ">") #'indent-region-right)
       map)
     t)))

(define-key ashton-mode-map (kbd "C-c <") #'indent-region-left)
(define-key ashton-mode-map (kbd "C-c >") #'indent-region-right)
;; M-<left>/M-<right> removed - conflicts with org-mode promote/demote

;; Global standup bindings (C-c S = Standup, works from any buffer)
(define-key ashton-mode-map (kbd "C-c S s") 'standup)
(define-key ashton-mode-map (kbd "C-c S d") 'standup-done)
(define-key ashton-mode-map (kbd "C-c S g") 'standup-doing)
(define-key ashton-mode-map (kbd "C-c S b") 'standup-blocker)
(define-key ashton-mode-map (kbd "C-c S a") 'standup-agenda)    ; add to tomorrow
(define-key ashton-mode-map (kbd "C-c S T") 'standup-tomorrow)  ; jump to tomorrow

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

;; C-c g n removed - use M-g g (consult-goto-line) instead

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
