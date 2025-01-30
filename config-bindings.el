;;; ../src/home/.doom.d/config-bindings.el -*- lexical-binding: t; -*-

(defvar ashton-mode-map (make-sparse-keymap)
  "Keymap for `ashton-mode'.")

(define-key ashton-mode-map (kbd "C-;") 'comment-or-uncomment-region)
(define-key ashton-mode-map (kbd "C-'") 'er/expand-region)
(define-key ashton-mode-map (kbd "C-\"") 'er/contract-region)
;;(define-key ashton-mode-map (kbd "TAB") 'indent-region)
(define-key ashton-mode-map (kbd "C-x b") 'ivy-switch-buffer)
;;(define-key ashton-mode-map (kbd "M-g") 'projectile-ripgrep)
(define-key ashton-mode-map (kbd "M-g") '+default/search-project)
(define-key ashton-mode-map (kbd "C-x C-f") '+ivy/projectile-find-file)
(define-key ashton-mode-map (kbd "C-x f") 'counsel-find-file)


(define-key ashton-mode-map (kbd "M-n") 'sp-forward-sexp)
(define-key ashton-mode-map (kbd "M-p") 'sp-backward-sexp)

(define-key ashton-mode-map (kbd "C-w") 'kill-region)
;; they all close when i restart emacs, maybe after server mode is working...
;;(define-key ashton-mode-map (kbd "M-o") 'projectile-switch-open-project)

(define-key ashton-mode-map (kbd "C-x C-k") 'kill-buffer)

(define-key ashton-mode-map (kbd "C-c C-g") '+default/search-project)

;; Change indent to match vscode... sigh.

(define-key ashton-mode-map (kbd "M-r") 'consult-recent-file)

(define-key ashton-mode-map (kbd "C-c q") 'query-replace)

;;projectile
(define-key ashton-mode-map (kbd "C-c M-p r") 'projectile-replace)

(define-key ashton-mode-map (kbd "M-s") 'swiper-thing-at-point)

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


(define-key ashton-mode-map (kbd "C-c s _") 'xah-cycle-hyphen-underscore-space)
(define-key ashton-mode-map (kbd "C-c s c") 'string-inflection-camelcase)
(define-key ashton-mode-map (kbd "C-c s u") 'string-inflection-underscore)
(define-key ashton-mode-map (kbd "C-c s k") 'string-inflection-kebab-case)
(define-key ashton-mode-map (kbd "C-c s u") 'string-inflection-upcase)
(define-key ashton-mode-map (kbd "C-c s p") 'string-inflection-python-style-cycle)
(define-key ashton-mode-map (kbd "C-c s U") 'crux-upcase-region)
(define-key ashton-mode-map (kbd "C-c s l") 'crux-downcase-region)

(define-key ashton-mode-map (kbd "M-<return>") 'hippie-expand)
;;(define-key ashton-mode-map (kbd "<tab>") 'hippie-expand)
(define-key ashton-mode-map (kbd "C-c n") 'goto-line)

(define-key ashton-mode-map (kbd "C-c C-p") 'beginning-of-buffer)
(define-key ashton-mode-map (kbd "C-c C-n") 'find-file)
(define-key ashton-mode-map (kbd "C-x 4") 'quad-screen)
(define-key ashton-mode-map (kbd "C-x 5") 'penta-screen)

(define-key ashton-mode-map (kbd "C-x k") 'kill-current-buffer)

;; use C-c c for all "complete here" bindings
(define-key ashton-mode-map (kbd "C-c c f") #'company-files)
;;(define-key ashton-mode-map (kbd "M-m") 'magit-status)
(define-key ashton-mode-map (kbd "C-c m") 'dirvish-mark-menu)

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

(define-minor-mode ashton-mode
  "A custom minor mode to bind ashton's keymaps."
  :init-value nil
  :global 1
  :lighter " AshtonMode"
  :keymap ashton-mode-map)

(ashton-mode 1)
