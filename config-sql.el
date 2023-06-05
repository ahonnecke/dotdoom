;; ;;; ../src/home/.doom.d/init-sql.el -*- lexical-binding: t; -*-

(defun upcase-sql-keywords ()
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

;; (global-set-key (kbd "C-c s") 'sql-connect-server)
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (toggle-truncate-lines t)))

;; (after! sql
;;   (set-company-backend! 'sql-mode 'company-dabbrev-code))

;; (after! sql
;;   (set-company-backend! 'sql-mode
;;     '(company-dabbrev-code)))

;; (add-hook 'sql-mode-hook
;;           #'(lambda ()
;;               (setq company-backends nil)
;;              ))


;; ;; To apply it to a specific mode:
;; (setq-hook! 'sql-hook +company-backends '(company-dabbrev-code))
;; ;; To apply it to a specific mode:
;;(setq-hook! 'sql-hook +lsp-company-backends '(company-dabbrev-code))

;; (defvar +company-backend-alist
;;   '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
;;     (prog-mode company-capf company-yasnippet)
;;     (conf-mode company-capf company-dabbrev-code company-yasnippet))
;;   "An alist matching modes to company backends. The backends for any mode is
;; built from this.")



 ;; (setq sql-interactive-mode-hook
 ;;         (lambda ()
 ;;           (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
 ;;           (sql-mysql-completion-init)))

(load "~/.sql.alist.el")

(defun my-sql-docker.rsudb ()
  (interactive)
  (my-sql-connect 'postgres 'docker.rsudb))

(defun my-sql-dev.rsudb ()
  (interactive)
  (my-sql-connect 'postgres 'dev.rsudb))

(defvar my-sql-servers-list
  '(("docker.rsudb" my-sql-docker.rsudb)
    ("dev.rsudb" my-sql-dev.rsudb))
  "Alist of server name and the function to connect")

;; (defun my-sql-connect (product connection)
;;   ;; remember to set the sql-product, otherwise, it will fail for the first time
;;   ;; you call the function
;;   (setq sql-product product)
;;   (sql-connect connection))

(setq my-sql-password
      '((server1 "password1")
        (docker.rsudb "postgres")
        (server2 "password2")))

(defun my-sql-connect (product connection)
  ;; load the password
  ;;(require my-password "my-password.el")

  (setq my-sql-password
        '((server1 "password1")
          (docker.rsudb "postgres")
          (server2 "password2")))

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))

;; (defun my-sql-connect-server (func)
;;   "Connect to the input server using my-sql-servers-list"
;;   (interactive
;;    (completing-read "Select server: " my-sql-servers-list))
;;   (funcall func))

(add-to-list 'load-path "~/emacs.auth")

(defun my-sql-mode-hook ()
  (setq company-backends '((company-dabbrev-code company-dict :with company-ispell)))
  )

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-local company-backends '((company-files company-dabbrev)))))


(add-hook 'sql-mode-hook #'my-sql-mode-hook)
(add-hook 'sql-interactive-mode-hook #'my-sql-mode-hook)

(require 'company-dict)

(setq company-dict-dir "/home/ahonnecke/src/doomemacs/dict/")

(add-to-list 'company-backends 'company-dict)

(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))

;; (add-hook 'sql-mode-hook
;;           (lambda ()
;;             (setq-local ac-ignore-case t)
;;             (define-key sql-interactive-mode-map "\t" 'company-sql)
;;             (auto-complete-mode)))

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (setq-local ac-ignore-case t)
;;             (define-key sql-interactive-mode-map "\t" 'company-sql)
;;             (auto-complete-mode)))
