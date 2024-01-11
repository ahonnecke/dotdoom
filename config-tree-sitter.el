;;; ../src/home/.doom.d/config-tree-sitter.el -*- lexical-binding: t; -*-

;; (use-package! tree-sitter
;;    :hook (prog-mode . turn-on-tree-sitter-mode)
;;    :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;    :config
;;    (require 'tree-sitter-langs)
;;    ;; This makes every node a link to a section of code
;;    (setq tree-sitter-debug-jump-buttons t
;;          ;; and this highlights the entire sub tree in your code
;;          tree-sitter-debug-highlight-jump-region t))

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
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/contract-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)


;; ;; UNTESTED BELOW HERE, I COMMENTED OUT THE arch specific shit
;; ;; FROM: https://www.reddit.com/r/emacs/comments/10iuim1/getting_emacs_29_to_automatically_use_treesitter/
;; ;; FIRST: git clone https://github.com/casouri/tree-sitter-module
;; ;;        bash batch.sh
;; ;; THEN : sudo cp dist/* /usr/local/lib
;; ;; FINALLY:
;; (setq treesit-extra-load-path '("/usr/local/lib"))
;; ;; Treesit ;; Eglot
;; (setq treesit-eglot-modes
;;       '((:ts (bash-mode . bash-ts-mode))
;;         (:ts (c++-mode . c++-ts-mode))
;;         (:ts (c-mode . c-ts-mode))
;;         (:ts (cpp-mode . cpp-ts-mode))
;;         (:ts (c-sharp-mode . sharp-ts-mode))
;;         (:ts (cmake-mode . cmake-ts-mode))
;;         (:ts (css-mode . css-ts-mode))
;;         (:ts (dockerfile-mode . dockerfile-ts-mode))
;;         (:ts (elixir-mode . elixir-ts-mode))
;;         (:ts (glsl-mode . glsl-ts-mode))
;;         (:ts (go-mode . go-ts-mode))
;;         (:ts (heex-mode . heex-ts-mode))
;;         (:ts (html-mode . html-ts-mode))
;;         (:ts (java-mode . java-ts-mode))
;;         (:ts (javascript-mode . js-ts-mode))
;;         (:ts (js-json-mode . json-ts-mode))
;;         (:ts (julia-mode . julia-ts-mode))
;;         (:ts (make-mode . make-ts-mode))
;;         (:ts (markdown-mode . markdown-ts-mode))
;;         (:ts (python-mode . python-ts-mode))
;;         (:ts (typescript-mode . typescript-ts-mode))
;;         (:ts (proto-mode . proto-ts-mode))
;;         (:ts (ruby-mode . ruby-ts-mode))
;;         (:ts (rust-mode . rust-ts-mode))
;;         (:ts (sql-mode . sql-ts-mode))
;;         (:ts (toml-mode . toml-ts-mode))
;;         (:ts (tsx-mode . tsx-ts-mode))
;;         (:ts (verilog-mode . verilog-ts-mode))
;;         (:ts (vhdl-mode . vhdl-ts-mode))
;;         (:ts (wgsl-mode . wgsl-ts-mode))
;;         (:ts (yaml-mode . yaml-ts-mode))
;;         ;; Not mature yet:
;;         ;; (push '(org-mode . org-ts-mode) major-mode-remap-alist)
;;         ;; (push '(perl-mode . perl-ts-mode) major-mode-remap-alist)              ;; cpan Perl::LanguageServer
;;         (require 'treesit)

;;         ;; Function to parse the above and make an install command
;;         ;; (if (treesit-available-p)
;;         ;;     (let ((pacman-install-list (list )))
;;         ;;       (dolist (ts-pm treesit-eglot-modes)
;;         ;;         (let ((majmode-remap (plist-get ts-pm :ts))
;;         ;;               (pacman-cmd (plist-get ts-pm :pacman)))
;;         ;;           ;; bind default major-mode to ts-mode
;;         ;;           (push majmode-remap major-mode-remap-alist)
;;         ;;           ;; populate install cmd
;;         ;;           (if pacman-cmd
;;         ;;               (unless (member pacman-cmd pacman-install-list)
;;         ;;                 (push pacman-cmd pacman-install-list)))))
;;         ;;       (let ((install-cmd (concat
;;         ;;                           "sudo apt -Y install "
;;         ;;                           (--reduce (concat acc " " it) pacman-install-list))))
;;         ;;         (message install-cmd)))
;;         ;;   (user-error "Treesitter not available"))
