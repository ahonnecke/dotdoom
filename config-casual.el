;;; ~/.doom.d/config-casual.el -*- lexical-binding: t; -*-

;; Casual Suite: Transient menus for built-in Emacs modes
;; https://github.com/kickingvegas/casual-suite
;;
;; Provides discoverable, keyboard-driven menus for:
;; - Calc, Dired, Info, Bookmarks, IBuffer, I-Search, Avy, Agenda, RE-Builder
;;
;; Convention: Press ? to see available commands in any mode

(use-package! casual-suite
  :config
  ;; Casual Dired - transient menu in dired
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "?") #'casual-dired-tmenu))

  ;; Casual IBuffer - transient menu in ibuffer
  (with-eval-after-load 'ibuffer
    (define-key ibuffer-mode-map (kbd "?") #'casual-ibuffer-tmenu))

  ;; Casual Info - transient menu in info
  (with-eval-after-load 'info
    (define-key Info-mode-map (kbd "?") #'casual-info-tmenu))

  ;; Casual Bookmarks - transient menu in bookmarks
  (with-eval-after-load 'bookmark
    (define-key bookmark-bmenu-mode-map (kbd "?") #'casual-bookmarks-tmenu))

  ;; Casual I-Search - transient during isearch
  (with-eval-after-load 'isearch
    (define-key isearch-mode-map (kbd "C-?") #'casual-isearch-tmenu))

  ;; Casual Calc - transient menu in calc
  (with-eval-after-load 'calc
    (define-key calc-mode-map (kbd "?") #'casual-calc-tmenu))

  ;; Casual RE-Builder - transient in re-builder
  (with-eval-after-load 're-builder
    (define-key reb-mode-map (kbd "?") #'casual-re-builder-tmenu))

  ;; Casual Avy - enhanced avy with transient
  ;; Bind to C-; (was comment-or-uncomment-region - move that)
  ;; Actually, let's use M-j for avy (jump)
  (with-eval-after-load 'avy
    (define-key ashton-mode-map (kbd "M-j") #'casual-avy-tmenu)))

(provide 'config-casual)
;;; config-casual.el ends here
