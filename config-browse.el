;;; ../src/home/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun firefox-search-region ()
  "URL encode the selected region and open it in Firefox."
  (interactive)
  (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
         (encoded-region (url-hexify-string region))
         (firefox-command (concat "firefox https://www.google.com/search?q=" encoded-region)))
    (shell-command firefox-command)))

(global-set-key (kbd "C-c g b") 'firefox-search-region)
