;;; ~/.doom.d/config-corfu.el -*- lexical-binding: t; -*-

;; Corfu + Cape: Modern completion system
;; Replaces Company (2025-12-05)
;;
;; Corfu: In-buffer completion UI using child frames
;; Cape: Completion At Point Extensions - modular completion backends
;;
;; Philosophy: Layer over stock Emacs
;; - TAB/C-n/C-p still work for completion
;; - C-c p prefix for explicit Cape completion sources

;;; ════════════════════════════════════════════════════════════════════════════
;;; Corfu Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(after! corfu
  ;; Popup behavior
  (setq corfu-auto t              ; Enable auto-completion
        corfu-auto-delay 0.1      ; Fast popup (was company-idle-delay)
        corfu-auto-prefix 2       ; Min chars before popup (was company-minimum-prefix-length)
        corfu-cycle t             ; Wrap around candidates
        corfu-preselect 'prompt   ; Don't preselect first candidate
        corfu-scroll-margin 5)    ; Scroll margin

  ;; Quit behaviors
  (setq corfu-quit-no-match 'separator  ; Quit if no match after separator
        corfu-quit-at-boundary 'separator)

  ;; Preview current candidate (optional - can be distracting)
  ;; (setq corfu-preview-current nil)

  ;; Popupinfo (documentation popup)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))  ; Show docs after 0.5s

  ;; Enable popupinfo mode for documentation
  (corfu-popupinfo-mode 1))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Cape Configuration - Completion backends
;;; ════════════════════════════════════════════════════════════════════════════

(use-package! cape
  :after corfu
  :config
  ;; Add Cape completions to default capf
  ;; Order matters - first ones have priority
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)

  ;; Configure dabbrev
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers t)

  ;; Make dabbrev case-sensitive in code
  (setq dabbrev-case-fold-search nil
        dabbrev-case-replace nil
        dabbrev-upcase-means-case-search t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings - C-c p prefix (Cape/comPletion)
;;; ════════════════════════════════════════════════════════════════════════════

;; Explicit completion source triggers
;; Use when auto-complete doesn't give what you want
(define-key ashton-mode-map (kbd "C-c p p") #'completion-at-point)  ; Default capf
(define-key ashton-mode-map (kbd "C-c p d") #'cape-dabbrev)         ; Words from buffers
(define-key ashton-mode-map (kbd "C-c p f") #'cape-file)            ; File paths
(define-key ashton-mode-map (kbd "C-c p l") #'cape-line)            ; Complete whole lines
(define-key ashton-mode-map (kbd "C-c p k") #'cape-keyword)         ; Programming keywords
(define-key ashton-mode-map (kbd "C-c p s") #'cape-elisp-symbol)    ; Elisp symbols
(define-key ashton-mode-map (kbd "C-c p h") #'cape-history)         ; History completion
(define-key ashton-mode-map (kbd "C-c p w") #'cape-dict)            ; Dictionary words
(define-key ashton-mode-map (kbd "C-c p :") #'cape-emoji)           ; Emoji :smile:
(define-key ashton-mode-map (kbd "C-c p \\") #'cape-tex)            ; TeX symbols
(define-key ashton-mode-map (kbd "C-c p &") #'cape-sgml)            ; SGML entities
(define-key ashton-mode-map (kbd "C-c p r") #'cape-rfc1345)         ; RFC1345 mnemonics

;;; ════════════════════════════════════════════════════════════════════════════
;;; Mode-specific completions
;;; ════════════════════════════════════════════════════════════════════════════

;; Python: prioritize LSP, then dabbrev
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'eglot-completion-at-point
                              #'cape-dabbrev
                              #'cape-file))))

;; Elisp: add elisp-specific completions
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'elisp-completion-at-point
                              #'cape-elisp-symbol
                              #'cape-dabbrev
                              #'cape-file))))

;; Shell/vterm: file completion priority
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'cape-file
                              #'cape-dabbrev
                              #'cape-history))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Migration notes from Company
;;; ════════════════════════════════════════════════════════════════════════════

;; Old Company config (custom-completion.el) mapped to Cape:
;;
;; | Company                  | Cape                    |
;; |--------------------------|-------------------------|
;; | company-dabbrev          | cape-dabbrev            |
;; | company-dabbrev-code     | cape-dabbrev            |
;; | company-files            | cape-file               |
;; | company-capf             | (native - corfu uses capfs) |
;; | company-yasnippet        | (doom handles via corfu) |
;; | company-keywords         | cape-keyword            |
;;
;; Old keybindings migrated:
;; | Old                      | New                     |
;; |--------------------------|-------------------------|
;; | C-c c b (buffer)         | C-c p d (dabbrev)       |
;; | C-c c p (project)        | C-c p p (completion)    |
;; | C-c c l (lsp)            | (auto via eglot capf)   |
;; | C-c c f (files)          | C-c p f (file)          |

(provide 'config-corfu)
;;; config-corfu.el ends here
