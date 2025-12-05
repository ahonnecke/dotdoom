;;; ~/.doom.d/config-consult-embark.el -*- lexical-binding: t; -*-

;; Consult + Embark + Marginalia + Orderless configuration
;; Part of the MOVEC stack (all included with Doom's vertico module)
;;
;; Consult: Enhanced completing-read commands with previews
;; Embark: Contextual actions on any target (like right-click)
;; Marginalia: Rich annotations in minibuffer
;; Orderless: Space-separated fuzzy matching

;;; ════════════════════════════════════════════════════════════════════════════
;;; Consult Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(after! consult
  ;; Use ripgrep for project search (you love rg)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Preview settings
  (setq consult-preview-key "M-.")  ; Manual preview with M-.

  ;; Narrowing keys for consult-buffer
  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          consult--source-recent-file
          consult--source-file-register
          consult--source-bookmark
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Embark Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(after! embark
  ;; Use which-key style indicator
  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-consult integration (for exporting search results)
(after! embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Marginalia Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(after! marginalia
  ;; Cycle through annotator levels with M-A
  (setq marginalia-align 'right)
  (setq marginalia-max-relative-age 0))  ; Always show absolute dates

;;; ════════════════════════════════════════════════════════════════════════════
;;; Orderless Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(after! orderless
  ;; Configure matching styles
  (setq orderless-matching-styles '(orderless-literal
                                     orderless-regexp
                                     orderless-initialism))

  ;; Use orderless everywhere
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings - The Good Stuff
;;; ════════════════════════════════════════════════════════════════════════════

;; Embark - THE game changer
;; C-. on any candidate or thing at point → contextual actions menu
(define-key ashton-mode-map (kbd "C-.") #'embark-act)
(define-key ashton-mode-map (kbd "C->") #'embark-dwim)  ; Do What I Mean
(define-key ashton-mode-map (kbd "C-h B") #'embark-bindings)  ; Show all bindings

;; Consult replacements - LAYER over stock Emacs bindings
;; Philosophy: Same keys do same general action, just better
;;
;; Stock Emacs    → Consult (enhanced)
;; C-x b          → consult-buffer (buffer switch, with preview)
;; M-g g          → consult-goto-line (goto line, with preview)
;; M-y            → consult-yank-pop (kill ring, with preview)
;; M-s .          → consult-line (search in buffer - stock is isearch-forward-symbol-at-point)
;; C-s            → stays isearch (don't mess with this muscle memory)

(define-key ashton-mode-map (kbd "C-x b") #'consult-buffer)      ; Enhanced buffer switch
(define-key ashton-mode-map (kbd "M-g g") #'consult-goto-line)   ; Enhanced goto-line
(define-key ashton-mode-map (kbd "M-g M-g") #'consult-goto-line) ; Both variants
(define-key ashton-mode-map (kbd "M-y") #'consult-yank-pop)      ; Enhanced kill ring

;; M-s prefix: search commands (stock Emacs convention)
(define-key ashton-mode-map (kbd "M-s l") #'consult-line)        ; Search lines in buffer
(define-key ashton-mode-map (kbd "M-s L") #'consult-line-multi)  ; Across all buffers
(define-key ashton-mode-map (kbd "M-s r") #'consult-ripgrep)     ; Project-wide rg (r for ripgrep)
(define-key ashton-mode-map (kbd "M-s i") #'consult-imenu)       ; Jump to symbol
(define-key ashton-mode-map (kbd "M-s I") #'consult-imenu-multi) ; Across project

;; Additional consult goodies
(define-key ashton-mode-map (kbd "C-x r b") #'consult-bookmark)
(define-key ashton-mode-map (kbd "M-#") #'consult-register-load)
(define-key ashton-mode-map (kbd "M-'") #'consult-register-store)
(define-key ashton-mode-map (kbd "C-M-#") #'consult-register)

;; Search history
(define-key ashton-mode-map (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)

;; Git integration
(define-key ashton-mode-map (kbd "M-s g") #'consult-git-grep)

;; Minibuffer history with Consult
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)
(define-key minibuffer-local-map (kbd "M-r") #'consult-history)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Embark Actions - Custom extensions
;;; ════════════════════════════════════════════════════════════════════════════

(after! embark
  ;; Add useful actions for files
  (define-key embark-file-map (kbd "M") #'magit-status)
  (define-key embark-file-map (kbd "C") #'claude-code)

  ;; Add useful actions for buffers
  (define-key embark-buffer-map (kbd "M") #'magit-status))

(provide 'config-consult-embark)
;;; config-consult-embark.el ends here
