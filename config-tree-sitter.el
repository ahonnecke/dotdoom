;;; ../src/home/.doom.d/config-tree-sitter.el -*- lexical-binding: t; -*-

;; (treesit-language-available-p 'python)

;; (require 'tree-sitter-langs)
;; (require 'tree-sitter)

;; ;; Manually register Python Tree-sitter language
;; (tree-sitter-require 'python)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python))

;; ;; Enable tree-sitter for Python
;; (add-hook 'python-ts-mode-hook #'tree-sitter-mode)
;; (add-hook 'python-ts-mode-hook #'tree-sitter-hl-mode)

;; (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))

(when (fboundp 'treesit-language-available-p)
  ;; Tree-sitter setup for Emacs 29+
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Remap python-mode to python-ts-mode if Tree-sitter is available
  (when (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

  ;; Tree-sitter highlighting setup
  (add-hook 'python-ts-mode-hook #'treesit-major-mode-setup)

  ;; Automatic grammar installation
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (unless (treesit-language-available-p 'python)
                (treesit-install-language-grammar 'python)))))
