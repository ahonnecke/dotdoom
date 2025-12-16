;;; ~/.doom.d/config-aws.el -*- lexical-binding: t; -*-

;; aws.el: Magit-style interface for AWS CLI
;; https://github.com/snowiow/aws.el
;;
;; Features:
;; - Bedrock AI chat (Claude!)
;; - Lambda, S3, CloudFormation, CloudWatch, IAM, CodeBuild, Step Functions
;; - Profile switching, SSO login
;;
;; Usage:
;;   C-c A     - AWS transient menu
;;   C-c A a   - Main AWS view
;;   C-c A b   - Bedrock chat
;;   C-c A f   - Step Functions

(add-to-list 'load-path "~/src/aws.el")

;; Step Functions extension (local, not in upstream yet)
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

(use-package aws
  :commands (aws aws-login aws-bedrock-chat aws-stepfunctions)
  :custom
  ;; Auth method: 'profile, 'vault, or 'sso
  ;; Use 'profile with native `aws sso login`, 'sso requires aws-sso tool
  (aws-login-method 'profile)

  ;; Output format: yaml, json, text
  (aws-output "json")

  ;; Bedrock model for AI chat
  ;; Claude Sonnet via Bedrock
  (aws-bedrock-model "anthropic.claude-3-5-sonnet-20241022-v2:0")

  ;; Default region
  (aws-region "us-east-1")

  :config
  ;; Default profile (instead of first from list)
  (setq aws-profile "crew.dev")
  ;; Load extensions (integrate via advice, no upstream mods)
  (require 'aws-stepfunctions)
  (require 'aws-s3-ext)

  ;; Keybindings in aws buffers
  (with-eval-after-load 'aws
    ;; Press ? in any aws buffer for help
    t))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; C-c A prefix for AWS
(define-key ashton-mode-map (kbd "C-c A a") #'aws)
(define-key ashton-mode-map (kbd "C-c A l") #'aws-login)
(define-key ashton-mode-map (kbd "C-c A b") #'aws-bedrock-chat)
(define-key ashton-mode-map (kbd "C-c A s") #'aws-s3)
(define-key ashton-mode-map (kbd "C-c A L") #'aws-lambda)
(define-key ashton-mode-map (kbd "C-c A c") #'aws-cloudformation)
(define-key ashton-mode-map (kbd "C-c A w") #'aws-cloudwatch)
(define-key ashton-mode-map (kbd "C-c A i") #'aws-iam)
(define-key ashton-mode-map (kbd "C-c A f") #'aws-stepfunctions)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix aws-transient ()
  "AWS commands."
  ["AWS"
   ["Services"
    ("a" "AWS (main)" aws)
    ("s" "S3" aws-s3)
    ("l" "Lambda" aws-lambda)
    ("f" "Step Functions" aws-stepfunctions)
    ("c" "CloudFormation" aws-cloudformation)
    ("w" "CloudWatch" aws-cloudwatch)
    ("i" "IAM" aws-iam)]
   ["AI"
    ("b" "Bedrock Chat" aws-bedrock-chat)]
   ["Auth"
    ("L" "Login (SSO)" aws-login)
    ("p" "Switch Profile" aws-set-profile)]])

;; C-c A is a prefix, so use ? for transient menu
(define-key ashton-mode-map (kbd "C-c A ?") #'aws-transient)

(provide 'config-aws)
;;; config-aws.el ends here
