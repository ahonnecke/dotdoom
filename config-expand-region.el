;;; ~/.doom.d/config-expand-region.el -*- lexical-binding: t; -*-
;;
;; Register language-specific expand-region expansions for tree-sitter modes.
;;
;; Problem: expand-region's `eval-after-load` mechanism registers expansions
;; via mode hooks (e.g., `python-mode-hook`), but tree-sitter modes use
;; separate hooks (`python-ts-mode-hook`). Without this, only basic
;; expansions (word → symbol → quotes → pairs → defun → ts-node) work.

(after! expand-region
  ;; --- Language-specific expansions for tree-sitter modes ---

  ;; Python: er/mark-python-statement, er/mark-python-block, etc.
  (er/enable-mode-expansions 'python-ts-mode #'er/add-python-mode-expansions)

  ;; JS/TS: er/mark-js-function, er/mark-js-object-property, etc.
  (er/enable-mode-expansions 'js-ts-mode #'er/add-js-mode-expansions)
  (er/enable-mode-expansions 'typescript-ts-mode #'er/add-js-mode-expansions)
  (er/enable-mode-expansions 'tsx-ts-mode #'er/add-js-mode-expansions)

  ;; YAML: er/mark-yaml-block, er/mark-yaml-key-value, etc.
  (er/enable-mode-expansions 'yaml-ts-mode #'er/add-yaml-mode-expansions)

  ;; CSS: er/mark-css-declaration
  (er/enable-mode-expansions 'css-ts-mode #'er/add-css-mode-expansions)

  ;; --- Smarter tree-sitter node expansion ---
  ;;
  ;; The built-in `er/mark-ts-node` walks up one node at a time, including
  ;; boring nodes (punctuation, single-char operators, whitespace-only spans).
  ;; This replacement skips those, giving more useful expansion steps.

  (when (and (>= emacs-major-version 29)
             (treesit-available-p))

    (defvar er--treesit-boring-types
      '("," ";" "." ":" "(" ")" "[" "]" "{" "}"
        "comment_delimiter" "newline" "ERROR")
      "Tree-sitter node types to skip during expansion.")

    (defun er--treesit-boring-node-p (node)
      "Return non-nil if NODE is too boring to be an expansion step.
Boring = punctuation, single-char, or same span as current region."
      (or (null node)
          (member (treesit-node-type node) er--treesit-boring-types)
          (<= (- (treesit-node-end node) (treesit-node-start node)) 1)))

    (defun er/mark-treesit-node-smart ()
      "Mark the next semantically interesting tree-sitter ancestor.
Skips punctuation, single-char nodes, and nodes that don't grow
the region beyond its current bounds."
      (interactive)
      (when (treesit-language-at (point))
        (let* ((node (if (use-region-p)
                         (treesit-node-on (region-beginning) (region-end))
                       (treesit-node-at (point))))
               (node-start (treesit-node-start node))
               (node-end (treesit-node-end node))
               (orig-start (if (use-region-p) (region-beginning) (point)))
               (orig-end (if (use-region-p) (region-end) (point))))
          ;; Walk up until we find a node that actually grows the region
          ;; and isn't boring
          (while (and node
                      (or (and (= node-start orig-start)
                               (= node-end orig-end))
                          (er--treesit-boring-node-p node)))
            (setq node (treesit-node-parent node))
            (when node
              (setq node-start (treesit-node-start node)
                    node-end (treesit-node-end node))))
          (when node
            (goto-char node-start)
            (set-mark node-end)))))

    ;; Replace the basic er/mark-ts-node with our smarter version in
    ;; tree-sitter buffers via a mode hook on each ts mode
    (defun er--add-smart-treesit-expansion ()
      "Replace `er/mark-ts-node' with `er/mark-treesit-node-smart' in
the buffer-local `er/try-expand-list'."
      (when (bound-and-true-p er/try-expand-list)
        (setq-local er/try-expand-list
                    (mapcar (lambda (fn)
                              (if (eq fn 'er/mark-ts-node)
                                  'er/mark-treesit-node-smart
                                fn))
                            er/try-expand-list))))

    (dolist (hook '(python-ts-mode-hook
                    js-ts-mode-hook
                    typescript-ts-mode-hook
                    tsx-ts-mode-hook
                    yaml-ts-mode-hook
                    css-ts-mode-hook
                    json-ts-mode-hook
                    go-ts-mode-hook))
      ;; Priority 90: run late so mode-specific expansions are already registered
      (add-hook hook #'er--add-smart-treesit-expansion 90))))
