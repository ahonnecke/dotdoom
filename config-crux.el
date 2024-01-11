;;; ../src/home/.doom.d/config-crux.el

(unless (package-installed-p 'crux)
  (package-refresh-contents)
  (package-install 'crux))

(global-set-key (kbd "C-c i d") 'crux-insert-date)
