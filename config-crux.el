;;; ../src/home/.doom.d/config-crux.el

;; (unless (package-installed-p 'crux)
;;   (package-refresh-contents)
;;   (package-install 'crux))


(with-eval-after-load "crux"
  (define-key ashton-mode-map (kbd "C-M-v") (kbd "C-c i d") 'crux-insert-date)
  )
