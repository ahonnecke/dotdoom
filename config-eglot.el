;;; config-eglot.el -*- lexical-binding: t; -*-

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

;;   (setq-default eglot-workspace-configuration
;;                 '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

;;   :hook
;;   ((python-mode . eglot-ensure)))
;;
;; aaaabbbbcccc

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("M-f" . xref-find-definitions)
              ("M-F" . xref-go-back)
              ("M-r" . eglot-rename)
              ("M-t" . xref-find-references)
              ("M-j" . flymake-goto-next-error)
              ("M-J" . flymake-goto-prev-error))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 50000)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
  :hook (python-mode . eglot-ensure)
  )

;; aaaabbbbcccc
;; aaaaccccbbbb
