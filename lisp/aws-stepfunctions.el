;;; aws-stepfunctions.el --- AWS Step Functions support for aws.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025, Ashton Honnecke

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Author: Ashton Honnecke

;;; Commentary:

;; AWS Step Functions support for aws.el
;; Provides listing, describing, and execution management for state machines.

;;; Code:
(require 'transient)
(require 'json)

;; Forward declarations for functions defined in aws.el/aws-core.el
(declare-function aws-cmd "aws")
(declare-function aws--pop-to-buffer "aws")
(declare-function aws--buffer-name "aws")
(declare-function aws--get-view-mode "aws")
(declare-function aws-set-profile "aws")
(declare-function aws "aws")
(declare-function aws-core--tabulated-list-from-command-multi-column "aws-core")
(declare-function aws-core--refresh-list-view "aws-core")
(declare-function aws-core--describe-current-resource "aws-core")

(defvar aws-output)

;;; ════════════════════════════════════════════════════════════════════════════
;;; State Machines List View
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-stepfunctions--list-state-machines ()
  "List all Step Functions State Machines."
  (fset 'aws--last-view 'aws-stepfunctions)
  (aws-core--tabulated-list-from-command-multi-column
   "stepfunctions list-state-machines --output=text --query 'stateMachines[*].[name,type,creationDate]'"
   [("Name" 50) ("Type" 12) ("Created" 30)]))

(defun aws-stepfunctions-list-refresh ()
  "Refresh the State Machines Overview."
  (interactive)
  (aws-core--refresh-list-view 'aws-stepfunctions--list-state-machines))

(defun aws-stepfunctions-describe ()
  "Describe the State Machine under the cursor."
  (interactive)
  (let* ((state-machine-name (tabulated-list-get-id))
         (arn (aws-stepfunctions--get-arn-from-name state-machine-name)))
    (aws-core--describe-current-resource "stepfunctions describe-state-machine --state-machine-arn" arn)))

(defun aws-stepfunctions--get-arn-from-name (name)
  "Get the ARN for state machine NAME."
  (string-trim
   (shell-command-to-string
    (concat (aws-cmd)
            "stepfunctions list-state-machines --output=text --query \"stateMachines[?name=='"
            name
            "'].stateMachineArn | [0]\""))))

(defun aws-stepfunctions-get-definition ()
  "Show the ASL definition of the State Machine under the cursor."
  (interactive)
  (let* ((state-machine-name (tabulated-list-get-id))
         (arn (aws-stepfunctions--get-arn-from-name state-machine-name))
         (buffer (concat "*aws.el stepfunctions definition: " state-machine-name "*"))
         (cmd (concat (aws-cmd)
                      "stepfunctions describe-state-machine --state-machine-arn "
                      arn
                      " --query 'definition' --output text")))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (aws-stepfunctions--setup-json-buffer))
    (goto-char (point-min))))

(defun aws-stepfunctions--setup-json-buffer ()
  "Setup buffer for JSON display with pretty printing."
  (condition-case nil
      (progn
        (json-pretty-print-buffer)
        (if (fboundp 'json-ts-mode)
            (json-ts-mode)
          (js-mode)))
    (error (js-mode))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Executions List View
;;; ════════════════════════════════════════════════════════════════════════════

(defvar aws-stepfunctions--current-state-machine-arn nil
  "ARN of the currently viewed state machine.")

(defvar aws-stepfunctions--current-state-machine-name nil
  "Name of the currently viewed state machine.")

(defun aws-stepfunctions-list-executions ()
  "List executions for the State Machine under the cursor."
  (interactive)
  (let* ((state-machine-name (tabulated-list-get-id))
         (arn (aws-stepfunctions--get-arn-from-name state-machine-name)))
    (setq aws-stepfunctions--current-state-machine-arn arn)
    (setq aws-stepfunctions--current-state-machine-name state-machine-name)
    (aws--pop-to-buffer (aws--buffer-name (concat "stepfunctions-executions:" state-machine-name)))
    (aws-stepfunctions-executions-mode)))

(defun aws-stepfunctions--list-executions-internal ()
  "Internal function to list executions."
  (fset 'aws--last-view 'aws-stepfunctions--list-executions-internal)
  (aws-core--tabulated-list-from-command-multi-column
   (concat "stepfunctions list-executions --state-machine-arn "
           aws-stepfunctions--current-state-machine-arn
           " --output=text --query 'executions[*].[name,status,startDate,stopDate]'")
   [("Execution" 40) ("Status" 12) ("Started" 25) ("Stopped" 25)]))

(defun aws-stepfunctions-executions-list-refresh ()
  "Refresh the Executions list."
  (interactive)
  (aws-core--refresh-list-view 'aws-stepfunctions--list-executions-internal))

(defun aws-stepfunctions-describe-execution ()
  "Describe the execution under the cursor."
  (interactive)
  (let* ((execution-name (tabulated-list-get-id))
         (execution-arn (aws-stepfunctions--get-execution-arn execution-name)))
    (aws-core--describe-current-resource "stepfunctions describe-execution --execution-arn" execution-arn)))

(defun aws-stepfunctions--get-execution-arn (execution-name)
  "Get ARN for EXECUTION-NAME in current state machine."
  (string-trim
   (shell-command-to-string
    (concat (aws-cmd)
            "stepfunctions list-executions --state-machine-arn "
            aws-stepfunctions--current-state-machine-arn
            " --output=text --query \"executions[?name=='"
            execution-name
            "'].executionArn | [0]\""))))

(defun aws-stepfunctions-get-execution-history ()
  "Show execution history for the execution under the cursor."
  (interactive)
  (let* ((execution-name (tabulated-list-get-id))
         (execution-arn (aws-stepfunctions--get-execution-arn execution-name))
         (buffer (concat "*aws.el stepfunctions history: " execution-name "*"))
         (cmd (concat (aws-cmd)
                      "--output "
                      aws-output
                      " stepfunctions get-execution-history --execution-arn "
                      execution-arn)))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (aws--get-view-mode))
    (goto-char (point-min))))

(defun aws-stepfunctions-get-execution-input ()
  "Show input for the execution under the cursor."
  (interactive)
  (let* ((execution-name (tabulated-list-get-id))
         (execution-arn (aws-stepfunctions--get-execution-arn execution-name))
         (buffer (concat "*aws.el stepfunctions input: " execution-name "*"))
         (cmd (concat (aws-cmd)
                      "stepfunctions describe-execution --execution-arn "
                      execution-arn
                      " --query 'input' --output text")))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (aws-stepfunctions--setup-json-buffer))
    (goto-char (point-min))))

(defun aws-stepfunctions-get-execution-output ()
  "Show output for the execution under the cursor."
  (interactive)
  (let* ((execution-name (tabulated-list-get-id))
         (execution-arn (aws-stepfunctions--get-execution-arn execution-name))
         (buffer (concat "*aws.el stepfunctions output: " execution-name "*"))
         (cmd (concat (aws-cmd)
                      "stepfunctions describe-execution --execution-arn "
                      execution-arn
                      " --query 'output' --output text")))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (aws-stepfunctions--setup-json-buffer))
    (goto-char (point-min))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Filtered Execution Views
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-stepfunctions-list-executions-by-status (status)
  "List executions filtered by STATUS."
  (interactive
   (list (completing-read "Status: " '("RUNNING" "SUCCEEDED" "FAILED" "TIMED_OUT" "ABORTED"))))
  (let* ((state-machine-name (tabulated-list-get-id))
         (arn (if aws-stepfunctions--current-state-machine-arn
                  aws-stepfunctions--current-state-machine-arn
                (aws-stepfunctions--get-arn-from-name state-machine-name))))
    (setq aws-stepfunctions--current-state-machine-arn arn)
    (setq aws-stepfunctions--current-state-machine-name (or aws-stepfunctions--current-state-machine-name state-machine-name))
    (let ((rows
           (mapcar (lambda (x)
                     (let ((splitted (split-string x "\t")))
                       (list (car splitted) (vconcat splitted))))
                   (butlast
                    (split-string
                     (shell-command-to-string
                      (concat
                       (aws-cmd)
                       "stepfunctions list-executions --state-machine-arn "
                       arn
                       " --status-filter " status
                       " --output=text --query 'executions[*].[name,status,startDate,stopDate]'")) "\n")))))
      (aws--pop-to-buffer (aws--buffer-name (concat "stepfunctions-executions:" aws-stepfunctions--current-state-machine-name ":" status)))
      (setq tabulated-list-format [("Execution" 40) ("Status" 12) ("Started" 25) ("Stopped" 25)])
      (setq tabulated-list-entries rows)
      (aws-stepfunctions-executions-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (hl-line-mode 1))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menus
;;; ════════════════════════════════════════════════════════════════════════════

(transient-define-prefix aws-stepfunctions-help-popup ()
  "AWS Step Functions Menu"
  ["Actions"
   ("RET" "Describe State Machine" aws-stepfunctions-describe)
   ("d" "Get ASL Definition" aws-stepfunctions-get-definition)
   ("e" "List Executions" aws-stepfunctions-list-executions)
   ("g" "Refresh" aws-stepfunctions-list-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(transient-define-prefix aws-stepfunctions-executions-help-popup ()
  "AWS Step Functions Executions Menu"
  ["Actions"
   ("RET" "Describe Execution" aws-stepfunctions-describe-execution)
   ("h" "Execution History" aws-stepfunctions-get-execution-history)
   ("i" "View Input" aws-stepfunctions-get-execution-input)
   ("o" "View Output" aws-stepfunctions-get-execution-output)
   ("g" "Refresh" aws-stepfunctions-executions-list-refresh)]
  ["Filter"
   ("r" "Running" (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "RUNNING")))
   ("s" "Succeeded" (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "SUCCEEDED")))
   ("f" "Failed" (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "FAILED")))
   ("t" "Timed Out" (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "TIMED_OUT")))
   ("a" "Aborted" (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "ABORTED")))]
  ["Navigation"
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Back to State Machines" aws-stepfunctions)])

;;; ════════════════════════════════════════════════════════════════════════════
;;; Mode Definitions
;;; ════════════════════════════════════════════════════════════════════════════

(defvar aws-stepfunctions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-stepfunctions-describe)
    (define-key map (kbd "?") 'aws-stepfunctions-help-popup)
    (define-key map (kbd "d") 'aws-stepfunctions-get-definition)
    (define-key map (kbd "e") 'aws-stepfunctions-list-executions)
    (define-key map (kbd "g") 'aws-stepfunctions-list-refresh)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defvar aws-stepfunctions-executions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-stepfunctions-describe-execution)
    (define-key map (kbd "?") 'aws-stepfunctions-executions-help-popup)
    (define-key map (kbd "g") 'aws-stepfunctions-executions-list-refresh)
    (define-key map (kbd "h") 'aws-stepfunctions-get-execution-history)
    (define-key map (kbd "i") 'aws-stepfunctions-get-execution-input)
    (define-key map (kbd "o") 'aws-stepfunctions-get-execution-output)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-stepfunctions)
    ;; Filter shortcuts
    (define-key map (kbd "r") (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "RUNNING")))
    (define-key map (kbd "s") (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "SUCCEEDED")))
    (define-key map (kbd "f") (lambda () (interactive) (aws-stepfunctions-list-executions-by-status "FAILED")))
    map))

;;;###autoload
(defun aws-stepfunctions ()
  "Open the AWS Step Functions Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "stepfunctions"))
  (aws-stepfunctions-mode))

(define-derived-mode aws-stepfunctions-mode tabulated-list-mode "aws-stepfunctions"
  "AWS Step Functions mode"
  (setq major-mode 'aws-stepfunctions-mode)
  (use-local-map aws-stepfunctions-mode-map)
  (aws-stepfunctions--list-state-machines))

(define-derived-mode aws-stepfunctions-executions-mode tabulated-list-mode "aws-stepfunctions-executions"
  "AWS Step Functions Executions mode"
  (setq major-mode 'aws-stepfunctions-executions-mode)
  (use-local-map aws-stepfunctions-executions-mode-map)
  (aws-stepfunctions--list-executions-internal))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Integration with aws.el (via advice - no upstream modifications needed)
;;; ════════════════════════════════════════════════════════════════════════════

(defun aws-stepfunctions--add-to-service-list (orig-fun)
  "Advice to add stepfunctions to the service list.
ORIG-FUN is the original `aws--list-services'."
  (funcall orig-fun)
  ;; Add stepfunctions to the existing entries
  (setq tabulated-list-entries
        (append tabulated-list-entries
                '(("stepfunctions" ["stepfunctions"]))))
  (tabulated-list-print))

(defun aws-stepfunctions--handle-service (orig-fun)
  "Advice to handle stepfunctions in service dispatcher.
ORIG-FUN is the original `aws-get-service'."
  (let ((service (tabulated-list-get-id)))
    (if (equal service "stepfunctions")
        (aws-stepfunctions)
      (funcall orig-fun))))

(with-eval-after-load 'aws
  (advice-add 'aws--list-services :around #'aws-stepfunctions--add-to-service-list)
  (advice-add 'aws-get-service :around #'aws-stepfunctions--handle-service))

(provide 'aws-stepfunctions)
;;; aws-stepfunctions.el ends here
