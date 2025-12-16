;;; aws-s3-ext.el --- S3 browsing via rclone+tramp for aws.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025, Ashton Honnecke

;;; Commentary:
;; Opens S3 buckets in dired via tramp+rclone.
;; Requires rclone configured with s3-dev and s3-prod remotes.
;; RET/o on bucket opens /rclone:s3-{profile}:/bucket/ in dired.

;;; Code:
(require 'transient)

(declare-function aws-set-profile "aws")
(declare-function aws "aws")

(defvar aws-profile)
(defvar aws-s3-mode-map)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom aws-s3-ext-rclone-remotes
  '(("crew.dev" . "s3-dev")
    ("crew.prod" . "s3-prod"))
  "Mapping of AWS profiles to rclone remote names."
  :type '(alist :key-type string :value-type string)
  :group 'aws)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Core Functions
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-s3-ext--get-rclone-remote ()
  "Get rclone remote name for current AWS profile."
  (or (cdr (assoc aws-profile aws-s3-ext-rclone-remotes))
      (error "No rclone remote configured for profile: %s" aws-profile)))

(defun aws-s3-ext-enter ()
  "Open S3 bucket under cursor in dired via tramp+rclone."
  (interactive)
  (let ((bucket (tabulated-list-get-id)))
    (if (or (null bucket) (string-empty-p bucket))
        (message "No bucket under cursor")
      (let* ((remote (aws-s3-ext--get-rclone-remote))
             (path (format "/rclone:%s:/%s/" remote bucket)))
        (message "Opening %s..." path)
        (find-file path)))))

(defun aws-s3-ext-copy-path ()
  "Copy S3 URI of bucket under cursor."
  (interactive)
  (let ((bucket (tabulated-list-get-id)))
    (when bucket
      (let ((uri (format "s3://%s/" bucket)))
        (kill-new uri)
        (message "Copied: %s" uri)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix aws-s3-ext-help-popup ()
  "AWS S3 Browser Help"
  ["Actions"
   ("o" "Open bucket in dired (rclone)" aws-s3-ext-enter)
   ("RET" "Open bucket in dired (rclone)" aws-s3-ext-enter)
   ("w" "Copy S3 URI" aws-s3-ext-copy-path)
   ("g" "Refresh" revert-buffer)]
  ["Navigation"
   ("P" "Set Profile" aws-set-profile)
   ("q" "Back to services" aws)])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Integration
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-s3-ext--setup-keybindings ()
  "Setup keybindings for S3 mode."
  (local-set-key [remap push-button] 'aws-s3-ext-enter)
  (local-set-key (kbd "RET") 'aws-s3-ext-enter)
  (local-set-key (kbd "<return>") 'aws-s3-ext-enter)
  (local-set-key (kbd "o") 'aws-s3-ext-enter)
  (local-set-key (kbd "w") 'aws-s3-ext-copy-path)
  (local-set-key (kbd "?") 'aws-s3-ext-help-popup))

(add-hook 'aws-s3-mode-hook #'aws-s3-ext--setup-keybindings)

(provide 'aws-s3-ext)
;;; aws-s3-ext.el ends here
