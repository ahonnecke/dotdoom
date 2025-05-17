(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia biomejs-format cmake-mode consult crux deadgrep docker-compose-mode
     embark expand-region flymake-ruff gist igist indent-tools llm magit-imerge
     marginalia npm-mode nvm org-bullets org-download ox-gist ox-report
     prettify-greek projectile-ripgrep region-bindings-mode rg ruff-format
     spinner tree-sitter-langs typescript-mode unfill)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-to-list 'exec-path "/home/ahonnecke/.pyenv/shims/")
(put 'projectile-grep 'disabled nil)
(put 'projectile-ripgrep 'disabled nil)
(put 'downcase-region 'disabled nil)
