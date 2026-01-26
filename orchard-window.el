;;; orchard-window.el --- Orchard window and column management -*- lexical-binding: t; -*-
;;
;; Part of Orchard - A worktree manager for Emacs
;;
;; This file contains window/column management:
;; - Column tracking and assignment
;; - Window locking (prevent buffer escape)
;; - Display buffer rules

(require 'orchard-vars)

;;; Forward declarations for functions defined in other orchard files
(declare-function orchard--ensure-claude-loaded "config-orchard")
(declare-function orchard--claude-buffer-for-path "config-orchard")
(declare-function orchard--current-worktree "config-orchard")
(declare-function claude-code "claude-code")

;;; ════════════════════════════════════════════════════════════════════════════
;;; Column Management - The Heart of Orchard
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Columns are numbered 0 to (orchard-max-columns - 1)
;; Column 0 is ALWAYS the dashboard
;; Columns 1+ are for branches
;;
;; Each column tracks:
;;   - branch: the branch name (nil for dashboard)
;;   - window: the current window object
;;   - mode: 'magit, 'claude, or 'compile
;;   - previous-mode: for restoring after compile
;;
;; WINDOW LOCKING STRATEGY:
;;   - Use `set-window-buffer` instead of `switch-to-buffer` to bypass display machinery
;;   - Mark branch windows as dedicated to prevent buffer poaching
;;   - Override `display-buffer-alist` for orchard-managed buffer patterns

(defvar orchard--columns (make-hash-table :test 'eq)
  "Hash table mapping column index to column state plist.")

(defvar orchard--branch-to-column (make-hash-table :test 'equal)
  "Hash table mapping branch names to column indices.")

(defun orchard--init-columns ()
  "Initialize column tracking."
  (clrhash orchard--columns)
  (clrhash orchard--branch-to-column)
  ;; Column 0 is always dashboard
  (puthash 0 (list :branch nil :window nil :mode 'dashboard) orchard--columns))

(defun orchard--get-column (index)
  "Get column state for INDEX."
  (gethash index orchard--columns))

(defun orchard--set-column (index plist)
  "Set column state for INDEX to PLIST."
  (puthash index plist orchard--columns))

(defun orchard--column-for-branch (branch)
  "Get column index for BRANCH, or nil if not assigned."
  (gethash branch orchard--branch-to-column))

(defun orchard--assign-branch-to-column (branch column)
  "Assign BRANCH to COLUMN."
  ;; Remove old branch from this column if any
  (let ((old-state (orchard--get-column column)))
    (when-let ((old-branch (plist-get old-state :branch)))
      (remhash old-branch orchard--branch-to-column)))
  ;; Assign new branch
  (puthash branch column orchard--branch-to-column)
  (let ((state (or (orchard--get-column column) (list))))
    (orchard--set-column column (plist-put state :branch branch))))

(defun orchard--find-available-column ()
  "Find an available column for a new branch.
Returns column index, possibly replacing an existing one."
  ;; Try columns 1 to max-1
  (or (cl-loop for i from 1 below orchard-max-columns
               unless (orchard--get-column i)
               return i)
      ;; All columns used - find one to replace (not the current one)
      (let ((current-col (orchard--window-column (selected-window))))
        (cl-loop for i from 1 below orchard-max-columns
                 unless (eq i current-col)
                 return i
                 finally return 1))))

(defun orchard--window-column (window)
  "Get the column index for WINDOW based on position."
  (let* ((edges (window-edges window))
         (left (car edges))
         (windows (window-list nil 'no-mini))
         (lefts (sort (delete-dups (mapcar (lambda (w) (car (window-edges w))) windows)) #'<)))
    (cl-position left lefts)))

(defun orchard--get-column-window (column)
  "Get the topmost window for COLUMN, or nil.
Handles vertically split columns correctly by grouping windows by left edge."
  (let* ((windows (cl-remove-if-not #'window-live-p (window-list nil 'no-mini)))
         ;; Group windows by left edge (same column)
         (columns-alist nil))
    ;; Build alist of (left-edge . topmost-window)
    (dolist (win windows)
      (let* ((edges (window-edges win))
             (left (car edges))
             (top (cadr edges))
             (existing (assoc left columns-alist)))
        (if existing
            ;; Keep the topmost window (smallest top value)
            (when (< top (cadr (window-edges (cdr existing))))
              (setcdr existing win))
          ;; New column
          (push (cons left win) columns-alist))))
    ;; Sort by left edge and get the nth column
    (let ((sorted (sort columns-alist (lambda (a b) (< (car a) (car b))))))
      (cdr (nth column sorted)))))

(defun orchard--count-columns ()
  "Count the number of columns (unique left edges)."
  (let ((windows (window-list nil 'no-mini)))
    (length (delete-dups (mapcar (lambda (w) (car (window-edges w))) windows)))))

(defun orchard--ensure-columns ()
  "Ensure we have the right number of columns.
Creates columns if needed, up to orchard-max-columns."
  (let ((current-count (orchard--count-columns)))
    (when (< current-count orchard-max-columns)
      ;; Need more columns - split from rightmost
      (let ((rightmost (car (last (sort (window-list nil 'no-mini)
                                        (lambda (a b)
                                          (> (car (window-edges a))
                                             (car (window-edges b)))))))))
        (dotimes (_ (- orchard-max-columns current-count))
          (select-window rightmost)
          (setq rightmost (split-window-right))
          (balance-windows))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Window Locking - Prevent Buffer Escape
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--display-buffer-in-window (buffer window)
  "Display BUFFER in WINDOW, bypassing display-buffer machinery.
This is the core function for enforcing column locking."
  (when (and buffer window (window-live-p window))
    (select-window window)
    (set-window-buffer window buffer)
    buffer))

(defun orchard--display-buffer-in-column (buffer column)
  "Display BUFFER in COLUMN, creating window if needed."
  (orchard--ensure-columns)
  (let ((win (orchard--get-column-window column)))
    (orchard--display-buffer-in-window buffer win)))

(defun orchard--set-window-dedicated (window dedicated)
  "Set WINDOW dedication to DEDICATED.
Dedicated windows won't be commandeered by other buffer displays."
  (when (window-live-p window)
    (set-window-dedicated-p window dedicated)))

(defun orchard--dedicate-branch-column (column)
  "Mark COLUMN as dedicated to its branch."
  (when-let ((win (orchard--get-column-window column)))
    (orchard--set-window-dedicated win t)))

(defun orchard--undedicate-branch-column (column)
  "Remove dedication from COLUMN (needed before switching buffers)."
  (when-let ((win (orchard--get-column-window column)))
    (orchard--set-window-dedicated win nil)))

(defun orchard--with-undedicated-window (window fn)
  "Temporarily undedicate WINDOW, run FN, re-dedicate.
FN is called with no arguments."
  (let ((was-dedicated (window-dedicated-p window)))
    (when was-dedicated
      (set-window-dedicated-p window nil))
    (unwind-protect
        (funcall fn)
      (when was-dedicated
        (set-window-dedicated-p window t)))))

(defun orchard--magit-in-window (path window)
  "Open magit for PATH in WINDOW without letting it escape."
  (orchard--with-undedicated-window window
    (lambda ()
      (select-window window)
      (let ((display-buffer-overriding-action '(display-buffer-same-window)))
        (magit-status path)))))

(defun orchard--claude-in-window (path window)
  "Start or switch to Claude for PATH in WINDOW without letting it escape."
  (orchard--ensure-claude-loaded)
  (orchard--with-undedicated-window window
    (lambda ()
      (select-window window)
      ;; Check for existing buffer
      (let ((claude-buf (orchard--claude-buffer-for-path path)))
        (if (and claude-buf (buffer-live-p claude-buf))
            ;; Existing - force into window
            (set-window-buffer window claude-buf)
          ;; New - start Claude and capture the buffer
          (let ((default-directory path)
                (buffers-before (buffer-list)))
            (claude-code)
            ;; Find the new Claude buffer
            (let ((new-claude (cl-find-if
                               (lambda (buf)
                                 (and (string-prefix-p "*claude:" (buffer-name buf))
                                      (not (memq buf buffers-before))))
                               (buffer-list))))
              (when new-claude
                (set-window-buffer window new-claude)))))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Display Buffer Rules for Orchard
;;; ════════════════════════════════════════════════════════════════════════════

(defun orchard--buffer-branch (buffer)
  "Get branch name for BUFFER based on its default-directory."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((wt (orchard--current-worktree)))
        (alist-get 'branch wt)))))

(defun orchard--display-buffer-in-branch-column (buffer _alist)
  "Display BUFFER in its branch's column if tracked by orchard.
For use in `display-buffer-alist'."
  (when-let* ((branch (orchard--buffer-branch buffer))
              (column (orchard--column-for-branch branch))
              (window (orchard--get-column-window column)))
    (orchard--with-undedicated-window window
      (lambda ()
        (set-window-buffer window buffer)))
    window))

;; Add display-buffer rules for orchard-managed buffers
(defun orchard--setup-display-buffer-rules ()
  "Setup display-buffer-alist rules for orchard window locking."
  (dolist (pattern '("^\\*claude:" "^magit:" "^\\*compilation\\*"))
    (add-to-list 'display-buffer-alist
                 `(,pattern
                   (orchard--display-buffer-in-branch-column
                    display-buffer-same-window)
                   (inhibit-same-window . nil)))))

(provide 'orchard-window)
;;; orchard-window.el ends here
