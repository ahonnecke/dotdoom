;;; ~/.doom.d/config-tree-sitter.el -*- lexical-binding: t; -*-

;; Tree-sitter: Modern syntax parsing for Emacs 29+
;; Provides better syntax highlighting, code navigation, and structural editing
;;
;; Install grammars: M-x treesit-install-language-grammar
;; Check availability: (treesit-language-available-p 'python)

(when (fboundp 'treesit-language-available-p)
  ;; Grammar sources for auto-installation
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (sql "https://github.com/DerekStride/tree-sitter-sql")))

  ;;; ══════════════════════════════════════════════════════════════════════════
  ;;; Mode Remapping - Use tree-sitter modes when grammars available
  ;;; ══════════════════════════════════════════════════════════════════════════

  ;; Python
  (when (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

  ;; JavaScript
  (when (treesit-language-available-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode)))

  ;; TypeScript
  (when (treesit-language-available-p 'typescript)
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))

  ;; TSX
  (when (treesit-language-available-p 'tsx)
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

  ;; JSON
  (when (treesit-language-available-p 'json)
    (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode)))

  ;; YAML
  (when (treesit-language-available-p 'yaml)
    (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

  ;; CSS
  (when (treesit-language-available-p 'css)
    (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

  ;; Go
  (when (treesit-language-available-p 'go)
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

  ;; Bash
  (when (treesit-language-available-p 'bash)
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

  ;; Dockerfile
  (when (treesit-language-available-p 'dockerfile)
    (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)))

  ;; TOML
  (when (treesit-language-available-p 'toml)
    (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

  ;;; ══════════════════════════════════════════════════════════════════════════
  ;;; Helper Functions
  ;;; ══════════════════════════════════════════════════════════════════════════

  (defun treesit-install-all-grammars ()
    "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (unless (treesit-language-available-p lang)
          (message "Installing %s grammar..." lang)
          (treesit-install-language-grammar lang)))))

  (defun treesit-check-grammars ()
    "Show which tree-sitter grammars are installed."
    (interactive)
    (let ((installed nil)
          (missing nil))
      (dolist (grammar treesit-language-source-alist)
        (let ((lang (car grammar)))
          (if (treesit-language-available-p lang)
              (push lang installed)
            (push lang missing))))
      (message "Installed: %s\nMissing: %s"
               (mapconcat #'symbol-name (nreverse installed) ", ")
               (if missing
                   (mapconcat #'symbol-name (nreverse missing) ", ")
                 "none")))))
