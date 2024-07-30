;;; config-region-bindings-mode.el -*- lexical-binding: t; -*-

(with-eval-after-load 'region-bindings-mode
  (defun enable-region-bindings-mode ()
    (region-bindings-mode-enable)
    (define-key region-bindings-mode-map (kbd "TAB") 'indent-rigidly-right-to-tab-stop)
    (define-key region-bindings-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
    (define-key region-bindings-mode-map (kbd "C-M-s") 'firefox-search-region)
    (define-key region-bindings-mode-map (kbd "C-v") 'vscode-open-filepath)
    ))
