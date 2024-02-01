;;; ../src/home/.doom.d/config-bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-n") 'end-of-buffer)
(global-set-key (kbd "C-M-p") 'beginning-of-buffer)

(global-set-key (kbd "M-n") 'sp-forward-sexp)
(global-set-key (kbd "M-p") 'sp-backward-sexp)

(global-set-key (kbd "C-w") 'kill-region)

;; (define-key global-map (kbd "C-x C-f") 'projectile-find-file)
(define-key global-map (kbd "C-x C-f") '+ivy/projectile-find-file)
(define-key global-map (kbd "C-x f") 'counsel-find-file)

(global-set-key (kbd "M-o") 'projectile-switch-project)
;; they all close when i restart emacs, maybe after server mode is working...
;;(global-set-key (kbd "M-o") 'projectile-switch-open-project)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)

(global-set-key (kbd "M-g") '+default/search-project)
(global-set-key (kbd "") '+default/search-project)

;; Change indent to match vscode... sigh.

;;TODO:
;; update vscode to ahve this too
;; https://stackoverflow.com/questions/40203303/shortcut-key-for-selecting-a-word-and-extending-the-selection-in-vs-code
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/contract-region)

(global-set-key (kbd "M-r") 'consult-recent-file)

(global-set-key (kbd "C-c q") 'query-replace)

;;projectile
(global-set-key (kbd "C-c M-p r") 'projectile-replace)

(global-set-key (kbd "M-s") 'swiper-thing-at-point)


;; move to only python mode, or maybe just get leader working?
(global-set-key (kbd "C-c t") 'python-pytest-dispatch)

(global-set-key (kbd "C-c v h") '+vterm/here)
(global-set-key (kbd "C-c v t") '+vterm/toggle)
(global-set-key (kbd "C-M-t") '+vterm/toggle)
(global-set-key (kbd "C-M-v") '+vterm/toggle)
;;(global-set-key (kbd "C-M-t") '+vterm/toggle)

;; C-c g (go to thing)
;;(global-set-key (kbd "C-c g r") 'browse-at-remote)
;; TODO: move to hydra?
;; GOTO keys:
(global-set-key (kbd "C-c g r") '+vc/browse-at-remote)
(global-set-key (kbd "C-c g f") 'find-file-at-point-with-line)
(global-set-key (kbd "C-c g u") 'browse-url-of-file)
(global-set-key (kbd "C-c g t") 'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "C-c g b") 'browse-url-at-point)

(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c j") 'avy-goto-char)
(global-set-key (kbd "M-<return>") 'hippie-expand)
(global-set-key (kbd "C-c n") 'goto-line)

(global-set-key (kbd "C-c C-p") 'beginning-of-buffer)
(global-set-key (kbd "C-c C-n") 'end-of-buffer)
(global-set-key (kbd "C-x 3") 'triple-screen)
(global-set-key (kbd "C-x 4") 'quad-screen)
(global-set-key (kbd "C-x 5") 'penta-screen)

;; use C-c c for all "complete here" bindings
(global-set-key (kbd "C-c c f") #'company-files)

(setq which-key-idle-delay 0.5)

(defun fix-bindings ()
  (interactive)
  (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (local-set-key (kbd "C-M-n") 'end-of-buffer)
  (local-set-key (kbd "C-M-p") 'beginning-of-buffer)
  (local-set-key (kbd "C-'") 'er/expand-region)
  (local-set-key (kbd "C-\"") 'er/contract-region)
  )

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
