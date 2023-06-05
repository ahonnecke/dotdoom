;;; ../src/home/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun my-search-or-browse ()
  "If selected region, or thing at point, is a url, go there. Otherwise,
use region/thing as a keyword for a google search."
  (interactive)
  (let ((target
         (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point 'symbol))))
    (if (ffap-url-p target)
        (browse-url target)
      (browse-url (concat "http://www.google.com/search?q="
                          (url-hexify-string target))))))

(global-set-key (kbd "C-c g b") 'my-search-or-browse)
