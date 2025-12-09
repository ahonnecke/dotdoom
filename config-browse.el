;;; ~/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun firefox-search-region ()
  "URL encode the selected region and open it in Firefox Google search."
  (interactive)
  (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
         (encoded-region (url-hexify-string region))
         (firefox-command (concat "firefox https://www.google.com/search?q=" encoded-region)))
    (shell-command firefox-command)))

;; Add as embark action for regions instead of dedicated keybinding
(with-eval-after-load 'embark
  (define-key embark-region-map (kbd "S") #'firefox-search-region))

;; Removed C-c g b conflict - use embark: select region, C-. S to search
