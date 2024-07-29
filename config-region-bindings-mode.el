;;; config-region-bindings-mode.el -*- lexical-binding: t; -*-

(define-key region-bindings-mode-map (kbd "TAB") 'indent-rigidly-right-to-tab-stop)
(define-key region-bindings-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key region-bindings-mode-map (kbd "C-f") 'firefox-search-region)
