;;; ../src/home/.doom.d/config-aws-mode.el -*- lexical-binding: t; -*-

(use-package aws-mode
  :bind ;; some functions which make sense to bind to something
  ("C-c a a" . aws)
  ("C-c a l" . aws-login)
  ("C-c a n" . aws-organizations-get-account-name)
  ("C-c a i" . aws-organizations-get-account-id)
  :load-path "~/src/aws.el/"
  :custom
  (aws-vault nil) ;; when t use aws-vault cmd to get into aws session
  (aws-output "json") ;; optional: yaml, json, text (default: yaml)
  (aws-organizations-account "root")) ;; profile of organizations account. organizations commands are automatically executed against this account, when specified
