;;; config-commando.el --- Hierarchical command runner with transient UI -*- lexical-binding: t; -*-
;;
;; Commando: A fast, tactical command runner for Emacs
;;
;; Features:
;;   - Hierarchical command menus via transient
;;   - Project-local command definitions (commando.eld)
;;   - Favorites tracking (recently used commands)
;;   - Split-window compile output
;;
;; Usage:
;;   1. Create commando.eld in your project root (see format below)
;;   2. From magit (or any buffer): press X to open command menu
;;   3. Navigate with single keys, run commands
;;
;; commando.eld format:
;;   Each category: (key "name" "description" &rest entries)
;;   Leaf command:  (key "make-target" "description")        ; 3 elements
;;   Submenu:       (key "name" "description" &rest entries) ; 4+ elements
;;
;; Example:
;;   (("d" "dev" "Development"
;;     ("s" "dev.start" "Start dev server")
;;     ("t" "test" "Testing submenu"
;;      ("a" "test.all" "Run all tests")
;;      ("u" "test.unit" "Run unit tests"))))

(require 'transient)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ════════════════════════════════════════════════════════════════════════════

(defgroup commando nil
  "Hierarchical command runner."
  :group 'tools
  :prefix "commando-")

(defcustom commando-file-name "commando.eld"
  "Name of the command definition file to search for."
  :type 'string
  :group 'commando)

(defcustom commando-history-file
  (expand-file-name "~/.commando-history")
  "File to store command history for favorites."
  :type 'file
  :group 'commando)

(defcustom commando-history-max 15
  "Maximum number of commands to track in favorites."
  :type 'integer
  :group 'commando)

(defcustom commando-command-prefix "make"
  "Command prefix for running targets (e.g., \"make\", \"just\", \"task\")."
  :type 'string
  :group 'commando)

(defcustom commando-dev-commands '("dev" "dev.start" "dev.run" "everything")
  "Commands considered 'dev mode' that lock resources.
When one of these runs, other worktrees should not run them."
  :type '(repeat string)
  :group 'commando)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Hooks
;;; ════════════════════════════════════════════════════════════════════════════

(defcustom commando-command-start-hook nil
  "Hook run when a command starts.
Called with arguments: (command path).
COMMAND is the make target (e.g., \"dev\", \"test.all\").
PATH is the directory where the command runs."
  :type 'hook
  :group 'commando)

(defcustom commando-command-finish-hook nil
  "Hook run when a command finishes.
Called with arguments: (command path status).
STATUS is the exit status string from compilation."
  :type 'hook
  :group 'commando)

;;; ════════════════════════════════════════════════════════════════════════════
;;; History / Favorites
;;; ════════════════════════════════════════════════════════════════════════════

(defvar commando--history nil
  "List of recently used commands (most recent first).")

(defun commando--load-history ()
  "Load command history from file."
  (when (file-exists-p commando-history-file)
    (with-temp-buffer
      (insert-file-contents commando-history-file)
      (ignore-errors
        (setq commando--history (read (buffer-string)))))))

(defun commando--save-history ()
  "Save command history to file."
  (with-temp-file commando-history-file
    (prin1 commando--history (current-buffer))))

(defun commando--add-to-history (command)
  "Add COMMAND to history."
  (setq commando--history
        (cons command (cl-remove command commando--history :test #'equal)))
  (when (> (length commando--history) commando-history-max)
    (setq commando--history
          (cl-subseq commando--history 0 commando-history-max)))
  (commando--save-history))

;; Initialize history on load
(commando--load-history)

;;; ════════════════════════════════════════════════════════════════════════════
;;; File Discovery
;;; ════════════════════════════════════════════════════════════════════════════

(defun commando--find-file ()
  "Find commando.eld by walking up from `default-directory'.
Returns the file path or nil if not found."
  (let ((dir (locate-dominating-file default-directory commando-file-name)))
    (when dir
      (expand-file-name commando-file-name dir))))

(defun commando--load-file ()
  "Load and parse the commando.eld file.
Returns list of categories or nil if not found."
  (let ((file (commando--find-file)))
    (if file
        (with-temp-buffer
          (insert-file-contents file)
          (ignore-errors (read (buffer-string))))
      (message "No %s found in project hierarchy" commando-file-name)
      nil)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; ════════════════════════════════════════════════════════════════════════════
;;; Command Execution - Orchard Integration
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; Commando takes over the current column. When you quit (q), it returns
;; to magit for that worktree.

(defvar-local commando--return-to-buffer nil
  "Buffer to return to when compilation finishes or is quit.")

(defvar-local commando--return-to-path nil
  "Path to return to (for magit-status) when compilation is quit.")

(defvar-local commando--running-command nil
  "The command that started this compilation.")

(defvar-local commando--running-path nil
  "The path where the command is running.")

(defun commando--is-dev-command-p (command)
  "Return non-nil if COMMAND is a dev mode command."
  (member command commando-dev-commands))

(defcustom commando-interactive-commands '("dev" "dev.start" "everything")
  "Commands that may require interactive input (run in vterm instead of compile)."
  :type '(repeat string)
  :group 'commando)

(defun commando--run-in-vterm (command path)
  "Run COMMAND in a vterm buffer at PATH."
  (require 'vterm)
  (let* ((full-command (format "%s %s" commando-command-prefix command))
         (buf-name (format "*commando:%s*" command))
         (default-directory path))
    ;; Kill existing buffer if any
    (when-let ((existing (get-buffer buf-name)))
      (kill-buffer existing))
    ;; Create new vterm
    (let ((buf (vterm buf-name)))
      (with-current-buffer buf
        (setq-local commando--return-to-path path)
        (setq-local commando--running-command command)
        (setq-local commando--running-path path)
        ;; Send the command
        (vterm-send-string full-command)
        (vterm-send-return)
        ;; Track in hooks when buffer is killed
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (run-hook-with-args 'commando-command-finish-hook
                                        command path "killed"))
                  nil t))
      buf)))

(defun commando-run (command)
  "Run COMMAND in current directory.
Uses vterm for interactive commands, compile for others.
Takes over the current window. Press q to return to magit."
  (let* ((full-command (format "%s %s" commando-command-prefix command))
         (current-buf (current-buffer))
         (current-path default-directory)
         (interactive-p (member command commando-interactive-commands)))
    ;; Add to history
    (commando--add-to-history command)
    ;; Run start hook
    (run-hook-with-args 'commando-command-start-hook command current-path)
    (if interactive-p
        ;; Use vterm for interactive commands
        (let ((vterm-buf (commando--run-in-vterm command current-path)))
          (switch-to-buffer vterm-buf)
          (message "Running interactively: %s (C-c C-t to toggle, kill buffer to stop)" full-command))
      ;; Use compile for non-interactive commands
      (let ((display-buffer-overriding-action '(display-buffer-same-window)))
        (compile full-command))
      ;; Store return info and command tracking in compilation buffer
      (with-current-buffer "*compilation*"
        (setq-local commando--return-to-buffer current-buf)
        (setq-local commando--return-to-path current-path)
        (setq-local commando--running-command command)
        (setq-local commando--running-path current-path))
      (message "Running: %s (press q to return)" full-command))))

(defun commando--compilation-finish (buffer status)
  "Called when compilation in BUFFER finishes with STATUS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when commando--running-command
        (run-hook-with-args 'commando-command-finish-hook
                            commando--running-command
                            commando--running-path
                            status)))))

;; Register our finish handler
(add-hook 'compilation-finish-functions #'commando--compilation-finish)

(defun commando-quit-to-magit ()
  "Quit compilation and return to magit for this worktree."
  (interactive)
  (let ((path commando--return-to-path)
        (command commando--running-command))
    ;; Run finish hook if there was a running command (user quit early)
    (when command
      (run-hook-with-args 'commando-command-finish-hook command path "quit"))
    (quit-window)
    (when path
      (magit-status path))))

;; Override q in compilation-mode to use our quit
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "q") #'commando-quit-to-magit))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dynamic Transient Generation
;;; ════════════════════════════════════════════════════════════════════════════
;;
;; The commando.eld format supports hierarchical commands:
;;   (key "target" "description")                        - leaf command (3 elements)
;;   (key "name" "description" :group GROUP &rest entries) - category with group
;;   (key "name" "description" &rest entries)            - submenu (4+ elements, no group)
;;
;; Groups control column layout in the main menu:
;;   dev    → Development (dev, test, lint, build)
;;   infra  → Infrastructure (db, docker, s3, monitoring)
;;   ops    → Operations (deploy, promote, aws, web)
;;   domain → Domain (business-specific commands)
;;   util   → Utilities (health, misc)

(defvar commando--transient-counter 0
  "Counter for generating unique transient names.")

(defvar commando--group-labels
  '((dev    . "Development")
    (infra  . "Infrastructure")
    (ops    . "Operations")
    (domain . "Domain")
    (util   . "Utilities"))
  "Mapping of group symbols to display labels.")

(defvar commando--group-order '(dev infra ops domain util)
  "Order in which groups appear in the transient.")

(defun commando--category-group (category)
  "Extract the :group value from CATEGORY, or 'util as default."
  (let ((rest (nthcdr 3 category)))
    (if (and rest (eq (car rest) :group))
        (cadr rest)
      'util)))

(defun commando--category-entries (category)
  "Extract the command entries from CATEGORY (after :group if present)."
  (let ((rest (nthcdr 3 category)))
    (if (and rest (eq (car rest) :group))
        (cddr rest)
      rest)))

(defun commando--entry-is-submenu-p (entry)
  "Return t if ENTRY is a submenu (has 4+ elements, not counting :group)."
  (let ((rest (nthcdr 3 entry)))
    (and rest
         (not (eq (car rest) :group))  ; not a top-level category with :group
         (> (length entry) 3))))

(defun commando--make-transient-entries (entries prefix)
  "Convert ENTRIES to transient suffix specs.
PREFIX is used for generating unique function names."
  (mapcar
   (lambda (entry)
     (let ((key (nth 0 entry))
           (name (nth 1 entry))
           (desc (nth 2 entry)))
       (if (commando--entry-is-submenu-p entry)
           ;; Submenu - create a nested transient
           (let* ((sub-entries (nthcdr 3 entry))
                  (sub-prefix (format "%s-%s" prefix name))
                  (dispatch-fn (commando--make-submenu-transient name desc sub-entries sub-prefix)))
             (list key (format "%s →" desc) dispatch-fn))
         ;; Leaf command - run target
         (list key
               (format "%s" desc)
               `(lambda () (interactive) (commando-run ,name))))))
   entries))

(defun commando--make-submenu-transient (name desc entries prefix)
  "Create a transient for submenu NAME with DESC and ENTRIES.
PREFIX ensures unique function names."
  (let ((dispatch-name (intern (format "commando--%s-%d"
                                       prefix
                                       (cl-incf commando--transient-counter)))))
    (eval
     `(transient-define-prefix ,dispatch-name ()
        ,(format "Commando: %s" desc)
        [,(format " %s" desc)
         ,@(commando--make-transient-entries entries prefix)]
        [""
         ("q" "Back" transient-quit-one)]))
    dispatch-name))

(defun commando--group-categories (categories)
  "Group CATEGORIES by their :group value.
Returns alist of (group-symbol . list-of-category-entries)."
  (let ((groups (make-hash-table :test 'eq)))
    ;; Initialize all groups to empty lists
    (dolist (g commando--group-order)
      (puthash g '() groups))
    ;; Sort categories into groups
    (dolist (cat categories)
      (let* ((key (nth 0 cat))
             (name (nth 1 cat))
             (desc (nth 2 cat))
             (group (commando--category-group cat))
             (entries (commando--category-entries cat)))
        (when entries
          (let ((dispatch-fn (commando--make-submenu-transient name desc entries name)))
            (puthash group
                     (append (gethash group groups)
                             (list (list key (format "%s →" desc) dispatch-fn)))
                     groups)))))
    ;; Convert to alist in order
    (cl-loop for g in commando--group-order
             for entries = (gethash g groups)
             when entries
             collect (cons g entries))))

(defun commando--make-group-columns (grouped-categories)
  "Create transient column specs from GROUPED-CATEGORIES.
Returns a list suitable for use in transient-define-prefix."
  ;; Split into rows of 2-3 columns each for better layout
  (let* ((groups grouped-categories)
         (columns '()))
    ;; Build column vector for each group
    (dolist (group-data groups)
      (let* ((group-sym (car group-data))
             (entries (cdr group-data))
             (label (or (cdr (assq group-sym commando--group-labels))
                        (symbol-name group-sym))))
        (push `[,label ,@entries] columns)))
    (nreverse columns)))

(defun commando--generate-transient ()
  "Generate the main commando dispatch transient."
  (let ((categories (commando--load-file)))
    (when categories
      ;; Reset counter for consistent naming
      (setq commando--transient-counter 0)
      ;; Build favorites section (horizontal, up to 5)
      (let* ((favorites-entries
              (when commando--history
                (cl-loop for cmd in (cl-subseq commando--history 0
                                                (min 5 (length commando--history)))
                         for i from 1
                         collect (list (number-to-string i)
                                       (truncate-string-to-width cmd 20 nil nil "…")
                                       `(lambda () (interactive)
                                          (commando-run ,cmd))))))
             ;; Group categories by :group
             (grouped (commando--group-categories categories))
             ;; Create column specs
             (group-columns (commando--make-group-columns grouped)))
        ;; Define the main transient with multi-column layout
        (eval
         `(transient-define-prefix commando-dispatch ()
            "Commando - Project Commands"
            ,@(when favorites-entries
                `([" ★ Recent"
                   ,@favorites-entries]))
            ,@group-columns
            [""
             ("!" "Run any command" commando-run-any)
             ("q" "Quit" transient-quit-one)]))))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Interactive Commands
;;; ════════════════════════════════════════════════════════════════════════════

(defun commando--collect-all-commands (entries)
  "Recursively collect all leaf commands from ENTRIES."
  (let ((result '()))
    (dolist (entry entries)
      (if (commando--entry-is-submenu-p entry)
          ;; Submenu - recurse (skip :group keyword if present)
          (let ((sub-entries (nthcdr 3 entry)))
            (when (eq (car sub-entries) :group)
              (setq sub-entries (cddr sub-entries)))
            (setq result (append result (commando--collect-all-commands sub-entries))))
        ;; Leaf command
        (let ((name (nth 1 entry))
              (desc (nth 2 entry)))
          (push (cons (format "%s - %s" name desc) name) result))))
    result))

(defun commando-run-any ()
  "Prompt for any command and run it."
  (interactive)
  (let* ((categories (commando--load-file))
         (all-commands '()))
    ;; Collect all commands recursively (using new category-entries accessor)
    (dolist (cat categories)
      (setq all-commands
            (append all-commands
                    (commando--collect-all-commands (commando--category-entries cat)))))
    ;; Add history to the top
    (dolist (hist-cmd (reverse commando--history))
      (unless (cl-find hist-cmd all-commands :key #'cdr :test #'equal)
        (push (cons (format "★ %s" hist-cmd) hist-cmd) all-commands)))
    ;; Prompt
    (let* ((selection (completing-read "Command: " all-commands nil t))
           (command (or (cdr (assoc selection all-commands)) selection)))
      (commando-run command))))

;;;###autoload
(defun commando ()
  "Open the commando dispatch.
Finds commando.eld in project hierarchy and shows command menu."
  (interactive)
  (commando--generate-transient)
  (if (fboundp 'commando-dispatch)
      (commando-dispatch)
    (message "No commands found - create %s in your project" commando-file-name)))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings
;;; ════════════════════════════════════════════════════════════════════════════

;; Bind ` (backtick) in magit-mode for quick access
;; X is taken by magit-reset
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "`") #'commando))

(provide 'config-commando)
;;; config-commando.el ends here
