;;; ~/.doom.d/config-workspace.el -*- lexical-binding: t; -*-

;; Workspace Manager Mode - Manage parallel crewcapableai workspaces
;; Integrates with ws.py utility for workspace operations

(require 'json)
(require 'transient)

;;; Configuration

(defgroup workspace nil
  "Workspace management configuration."
  :group 'tools
  :prefix "workspace-")

(defcustom workspace-ws-command "/home/ahonnecke/src/crewcapableai.shared/bin/ws.py"
  "Path to the ws.py workspace management script."
  :type 'file
  :group 'workspace)

(defcustom workspace-root "/home/ahonnecke/src"
  "Root directory containing workspace directories."
  :type 'directory
  :group 'workspace)

(defcustom workspace-count 4
  "Number of numbered workspaces (0 to N-1)."
  :type 'integer
  :group 'workspace)

(defcustom workspace-use-external-terminal nil
  "If non-nil, use external terminal (gnome-terminal) for Claude.
Otherwise attempt to use vterm inside Emacs."
  :type 'boolean
  :group 'workspace)

(defcustom workspace-external-terminal "gnome-terminal"
  "External terminal command to use when `workspace-use-external-terminal' is t."
  :type 'string
  :group 'workspace)

;;; Faces

(defface workspace-clean-face
  '((t :foreground "green"))
  "Face for clean/empty workspaces."
  :group 'workspace)

(defface workspace-dirty-face
  '((t :foreground "yellow"))
  "Face for dirty workspaces."
  :group 'workspace)

(defface workspace-behind-face
  '((t :foreground "red"))
  "Face for workspaces behind upstream."
  :group 'workspace)

(defface workspace-current-face
  '((t :foreground "cyan" :weight bold))
  "Face for the current workspace."
  :group 'workspace)

(defface workspace-header-face
  '((t :weight bold :underline t))
  "Face for dashboard headers."
  :group 'workspace)

;;; Data Fetching

(defun workspace--run-ws (&rest args)
  "Run ws.py with ARGS and return output as string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process workspace-ws-command nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "ws.py failed with exit code %d: %s" exit-code (buffer-string))))))

(defun workspace--run-ws-async (callback &rest args)
  "Run ws.py with ARGS asynchronously, call CALLBACK with output when done."
  (let ((buf (generate-new-buffer " *ws-async*")))
    (set-process-sentinel
     (apply #'start-process "ws" buf workspace-ws-command args)
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (funcall callback (buffer-string)))
         (kill-buffer (process-buffer proc)))))))

(defun workspace--get-status ()
  "Get workspace status as parsed JSON."
  (let ((json-output (workspace--run-ws "--json")))
    (json-parse-string json-output :object-type 'alist :array-type 'list)))

(defun workspace--get-workspace-at-point ()
  "Get the workspace alist for the line at point in the dashboard."
  (when (eq major-mode 'workspace-mode)
    (get-text-property (point) 'workspace-data)))

;;; Dashboard Buffer

(defvar workspace-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "RET") #'workspace-open-magit-at-point)
    (define-key map (kbd "d") #'workspace-open-dired-at-point)
    (define-key map (kbd "t") #'workspace-open-terminal-at-point)
    (define-key map (kbd "c") #'workspace-open-claude-at-point)
    ;; Operations
    (define-key map (kbd "n") #'workspace-new-feature)
    (define-key map (kbd "p") #'workspace-pull)
    (define-key map (kbd "P") #'workspace-pull-all)
    (define-key map (kbd "s") #'workspace-sync)
    (define-key map (kbd "C") #'workspace-clean)
    (define-key map (kbd "X") #'workspace-clean-execute)
    ;; Buffer
    (define-key map (kbd "r") #'workspace-refresh)
    (define-key map (kbd "g") #'workspace-refresh)
    (define-key map (kbd "q") #'quit-window)
    ;; Transient
    (define-key map (kbd "?") #'workspace-transient)
    map)
  "Keymap for workspace-mode.")

(define-derived-mode workspace-mode special-mode "Workspaces"
  "Major mode for workspace dashboard buffer.

\\{workspace-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

(defun workspace--format-dashboard (workspaces)
  "Format WORKSPACES data into dashboard string."
  (let ((header (format "%-3s %-25s %-25s %-6s %-6s %-6s\n"
                        "#" "Path" "Branch" "Dirty" "Ahead" "Behind"))
        (separator (make-string 78 ?-)))
    (concat
     (propertize "Workspace Status" 'face 'workspace-header-face)
     (format "  [%s]\n" (format-time-string "%H:%M:%S"))
     separator "\n"
     (propertize header 'face 'workspace-header-face)
     separator "\n"
     (mapconcat #'workspace--format-workspace-line workspaces "\n")
     "\n" separator "\n\n"
     (propertize "[n]ew feature  [s]ync  [p]ull  [P]ull all  [C]lean  [r]efresh  [?]menu  [q]uit"
                 'face 'font-lock-comment-face))))

(defun workspace--format-workspace-line (ws)
  "Format a single workspace WS as a dashboard line."
  (let* ((num (alist-get 'number ws))
         (path (file-name-nondirectory (alist-get 'path ws)))
         (branch (alist-get 'branch ws))
         (dirty (alist-get 'dirty ws))
         (empty (alist-get 'empty ws))
         (ahead (alist-get 'ahead ws))
         (behind (alist-get 'behind ws))
         (is-current (alist-get 'is_current ws))
         (face (cond
                (is-current 'workspace-current-face)
                ((> behind 0) 'workspace-behind-face)
                (dirty 'workspace-dirty-face)
                (empty 'workspace-clean-face)
                (t 'default)))
         (line (format "%-3d %-25s %-25s %-6s %-6d %-6d"
                       num
                       (truncate-string-to-width path 25 nil nil "…")
                       (truncate-string-to-width branch 25 nil nil "…")
                       (if dirty "*" "")
                       ahead
                       behind)))
    (propertize line
                'face face
                'workspace-data ws
                'workspace-number num)))

(defun workspace-refresh ()
  "Refresh the workspace dashboard."
  (interactive)
  (when (eq major-mode 'workspace-mode)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (workspace--format-dashboard (workspace--get-status)))
      (goto-char (min pos (point-max)))
      ;; Move to first workspace line if we're in header
      (when (< (line-number-at-pos) 4)
        (goto-char (point-min))
        (forward-line 3)))))

;;;###autoload
(defun workspace-show ()
  "Show the workspace dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Workspaces*")))
    (with-current-buffer buf
      (workspace-mode)
      (workspace-refresh))
    (pop-to-buffer buf)))

;;; Navigation Commands

(defun workspace--get-path-at-point ()
  "Get workspace path at point."
  (when-let ((ws (workspace--get-workspace-at-point)))
    (alist-get 'path ws)))

;;;###autoload
(defun workspace-open-magit-at-point ()
  "Open magit for workspace at point."
  (interactive)
  (if-let ((path (workspace--get-path-at-point)))
      (magit-status path)
    (user-error "No workspace at point")))

;;;###autoload
(defun workspace-open-dired-at-point ()
  "Open dired for workspace at point."
  (interactive)
  (if-let ((path (workspace--get-path-at-point)))
      (dired path)
    (user-error "No workspace at point")))

;;;###autoload
(defun workspace-open-terminal-at-point ()
  "Open terminal in workspace at point."
  (interactive)
  (if-let ((path (workspace--get-path-at-point)))
      (workspace--open-terminal path)
    (user-error "No workspace at point")))

;;;###autoload
(defun workspace-open-claude-at-point ()
  "Open Claude in workspace at point."
  (interactive)
  (if-let ((path (workspace--get-path-at-point)))
      (workspace--open-claude path)
    (user-error "No workspace at point")))

;;;###autoload
(defun workspace-jump (n)
  "Jump to workspace N (0-3)."
  (interactive "nWorkspace number (0-3): ")
  (let ((path (format "%s/crewcapableai.%d" workspace-root n)))
    (if (file-directory-p path)
        (dired path)
      (user-error "Workspace %d does not exist" n))))

;;;###autoload
(defun workspace-magit (n)
  "Open magit for workspace N."
  (interactive "nWorkspace number (0-3): ")
  (let ((path (format "%s/crewcapableai.%d" workspace-root n)))
    (if (file-directory-p path)
        (magit-status path)
      (user-error "Workspace %d does not exist" n))))

;;; Terminal Integration

(defun workspace--open-terminal (path)
  "Open a terminal in PATH."
  (if workspace-use-external-terminal
      (workspace--open-external-terminal path)
    (workspace--open-vterm path)))

(defun workspace--open-vterm (path)
  "Open vterm in PATH."
  (let ((default-directory path))
    (if (fboundp '+vterm/here)
        (+vterm/here nil)
      (vterm))))

(defun workspace--open-external-terminal (path)
  "Open external terminal in PATH."
  (let ((default-directory path))
    (start-process "workspace-terminal" nil
                   workspace-external-terminal
                   (format "--working-directory=%s" path))))

(defun workspace--open-claude (path)
  "Open Claude CLI in PATH.
Tries vterm first, falls back to external terminal if issues arise."
  (if workspace-use-external-terminal
      (workspace--open-claude-external path)
    (workspace--open-claude-vterm path)))

(defun workspace--open-claude-vterm (path)
  "Open Claude in vterm at PATH."
  (let ((default-directory path))
    (if (fboundp 'vterm)
        (progn
          (require 'vterm)
          (let ((orig-shell vterm-shell))
            (setq vterm-shell "claude")
            (unwind-protect
                (vterm (format "*claude:%s*" (file-name-nondirectory path)))
              (setq vterm-shell orig-shell))))
      (workspace--open-claude-external path))))

(defun workspace--open-claude-external (path)
  "Open Claude in external terminal at PATH."
  (start-process "workspace-claude" nil
                 workspace-external-terminal
                 (format "--working-directory=%s" path)
                 "--" "claude"))

;;; Workspace Operations

;;;###autoload
(defun workspace-new-feature (name)
  "Create new feature branch NAME in first empty workspace and start Claude."
  (interactive "sFeature name: ")
  (message "Creating feature branch FEAT/%s..." name)
  (let ((output (workspace--run-ws "-n" name "--no-claude")))
    (message "%s" output)
    ;; Find which workspace got the branch and open claude there
    (let* ((workspaces (workspace--get-status))
           (target (cl-find-if (lambda (ws)
                                 (string-match-p (regexp-quote name)
                                                 (or (alist-get 'branch ws) "")))
                               workspaces)))
      (when target
        (workspace--open-claude (alist-get 'path target))))
    (when (get-buffer "*Workspaces*")
      (with-current-buffer "*Workspaces*"
        (workspace-refresh)))))

;;;###autoload
(defun workspace-pull ()
  "Pull/rebase current workspace."
  (interactive)
  (message "Pulling current workspace...")
  (workspace--run-ws-async
   (lambda (output)
     (message "%s" output)
     (when (get-buffer "*Workspaces*")
       (with-current-buffer "*Workspaces*"
         (workspace-refresh))))
   "--pull"))

;;;###autoload
(defun workspace-pull-all ()
  "Pull/rebase all workspaces."
  (interactive)
  (message "Pulling all workspaces...")
  (workspace--run-ws-async
   (lambda (output)
     (message "%s" output)
     (when (get-buffer "*Workspaces*")
       (with-current-buffer "*Workspaces*"
         (workspace-refresh))))
   "--pull-all"))

;;;###autoload
(defun workspace-sync ()
  "Sync (fetch) all workspaces."
  (interactive)
  (message "Syncing all workspaces...")
  (workspace--run-ws-async
   (lambda (output)
     (message "%s" output)
     (when (get-buffer "*Workspaces*")
       (with-current-buffer "*Workspaces*"
         (workspace-refresh))))
   "--sync"))

;;;###autoload
(defun workspace-clean ()
  "Show workspaces that can be cleaned (dry-run)."
  (interactive)
  (let ((output (workspace--run-ws "--clean")))
    (message "%s" output)))

;;;###autoload
(defun workspace-clean-execute ()
  "Clean workspaces with no useful changes."
  (interactive)
  (when (yes-or-no-p "Really clean workspaces? ")
    (message "Cleaning workspaces...")
    (let ((output (workspace--run-ws "--clean" "--execute")))
      (message "%s" output)
      (when (get-buffer "*Workspaces*")
        (with-current-buffer "*Workspaces*"
          (workspace-refresh))))))

;;; Transient Menu

(transient-define-prefix workspace-transient ()
  "Workspace management commands."
  ["Workspace Management"
   ["Navigate"
    ("w" "Show dashboard" workspace-show)
    ("j" "Jump to workspace" workspace-jump)
    ("m" "Magit in workspace" workspace-magit)]
   ["Operations"
    ("n" "New feature" workspace-new-feature)
    ("s" "Sync all" workspace-sync)
    ("p" "Pull current" workspace-pull)
    ("P" "Pull all" workspace-pull-all)]
   ["Cleanup"
    ("c" "Clean (dry-run)" workspace-clean)
    ("C" "Clean (execute)" workspace-clean-execute)]
   ["Buffer"
    ("r" "Refresh" workspace-refresh)
    ("q" "Quit" transient-quit-one)]])

;;; Global Keybindings (to be added to ashton-mode-map)

;; These will be added in config-bindings.el:
;; (define-key ashton-mode-map (kbd "C-c w w") #'workspace-show)
;; (define-key ashton-mode-map (kbd "C-c w n") #'workspace-new-feature)
;; (define-key ashton-mode-map (kbd "C-c w s") #'workspace-sync)
;; (define-key ashton-mode-map (kbd "C-c w p") #'workspace-pull)
;; (define-key ashton-mode-map (kbd "C-c w P") #'workspace-pull-all)
;; (define-key ashton-mode-map (kbd "C-c w c") #'workspace-clean)
;; (define-key ashton-mode-map (kbd "C-c w C") #'workspace-clean-execute)
;; (define-key ashton-mode-map (kbd "C-c w j") #'workspace-jump)
;; (define-key ashton-mode-map (kbd "C-c w m") #'workspace-magit)
;; (define-key ashton-mode-map (kbd "C-c w ?") #'workspace-transient)

;;; ==========================================================================
;;; Phase 3: Test Evidence Buffer
;;; ==========================================================================

(defcustom workspace-evidence-dir ".test-evidence"
  "Directory name for test evidence within workspace."
  :type 'string
  :group 'workspace)

(defcustom workspace-screenshot-dir (expand-file-name "~/screenshots")
  "Root directory for screenshots (organized by date)."
  :type 'directory
  :group 'workspace)

;;; Faces for evidence buffer

(defface workspace-evidence-complete-face
  '((t :foreground "green" :weight bold))
  "Face for completed test items."
  :group 'workspace)

(defface workspace-evidence-pending-face
  '((t :foreground "yellow"))
  "Face for pending test items."
  :group 'workspace)

(defface workspace-evidence-screenshot-face
  '((t :foreground "cyan" :slant italic))
  "Face for screenshot indicators."
  :group 'workspace)

;;; Evidence Data Structure

(defvar-local workspace-evidence--items nil
  "List of test evidence items for current buffer.
Each item is an alist with keys: id, text, complete, screenshots.")

(defvar-local workspace-evidence--workspace-path nil
  "Path to workspace for current evidence buffer.")

(defvar-local workspace-evidence--branch nil
  "Branch name for current evidence buffer.")

(defvar-local workspace-evidence--rev nil
  "Revision hash for current evidence buffer.")

(defun workspace-evidence--get-rev (workspace-path)
  "Get short revision hash for WORKSPACE-PATH."
  (let ((default-directory workspace-path))
    (string-trim (shell-command-to-string "git rev-parse --short HEAD"))))

(defun workspace-evidence--get-branch (workspace-path)
  "Get branch name for WORKSPACE-PATH."
  (let ((default-directory workspace-path))
    (string-trim (shell-command-to-string "git branch --show-current"))))

(defun workspace-evidence--dir (workspace-path rev)
  "Get evidence directory for WORKSPACE-PATH and REV."
  (expand-file-name (format "%s/%s" workspace-evidence-dir rev) workspace-path))

(defun workspace-evidence--screenshots-dir (workspace-path rev)
  "Get screenshots directory for WORKSPACE-PATH and REV."
  (expand-file-name "screenshots" (workspace-evidence--dir workspace-path rev)))

(defun workspace-evidence--data-file (workspace-path rev)
  "Get evidence data file path for WORKSPACE-PATH and REV."
  (expand-file-name "evidence.json" (workspace-evidence--dir workspace-path rev)))

(defun workspace-evidence--ensure-dirs (workspace-path rev)
  "Ensure evidence directories exist for WORKSPACE-PATH and REV."
  (let ((screenshots-dir (workspace-evidence--screenshots-dir workspace-path rev)))
    (unless (file-directory-p screenshots-dir)
      (make-directory screenshots-dir t))))

(defun workspace-evidence--load (workspace-path rev)
  "Load evidence items from file for WORKSPACE-PATH and REV."
  (let ((file (workspace-evidence--data-file workspace-path rev)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
      nil)))

(defun workspace-evidence--save ()
  "Save current evidence items to file."
  (when (and workspace-evidence--workspace-path workspace-evidence--rev)
    (workspace-evidence--ensure-dirs workspace-evidence--workspace-path workspace-evidence--rev)
    (let ((file (workspace-evidence--data-file workspace-evidence--workspace-path workspace-evidence--rev)))
      (with-temp-file file
        (insert (json-encode workspace-evidence--items))))))

(defun workspace-evidence--next-id ()
  "Generate next item ID."
  (1+ (apply #'max 0 (mapcar (lambda (item) (or (alist-get 'id item) 0))
                              workspace-evidence--items))))

;;; Evidence Buffer Mode

(defvar workspace-evidence-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'workspace-evidence-add-item)
    (define-key map (kbd "x") #'workspace-evidence-toggle-complete)
    (define-key map (kbd "s") #'workspace-evidence-attach-screenshot-clipboard)
    (define-key map (kbd "S") #'workspace-evidence-attach-screenshot-file)
    (define-key map (kbd "v") #'workspace-evidence-view-screenshots)
    (define-key map (kbd "e") #'workspace-evidence-export-markdown)
    (define-key map (kbd "r") #'workspace-evidence-refresh)
    (define-key map (kbd "g") #'workspace-evidence-refresh)
    (define-key map (kbd "d") #'workspace-evidence-delete-item)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'workspace-evidence-transient)
    (define-key map (kbd "RET") #'workspace-evidence-view-screenshots)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for workspace-evidence-mode.")

(define-derived-mode workspace-evidence-mode special-mode "Evidence"
  "Major mode for test evidence tracking.

\\{workspace-evidence-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

;;; Evidence Buffer Display

(defun workspace-evidence--format-buffer ()
  "Format the evidence buffer contents."
  (let ((header (format "Test Evidence for %s @ %s\n"
                        workspace-evidence--branch
                        workspace-evidence--rev))
        (separator (make-string 60 ?=)))
    (concat
     (propertize header 'face 'workspace-header-face)
     separator "\n\n"
     (if workspace-evidence--items
         (mapconcat #'workspace-evidence--format-item workspace-evidence--items "\n")
       (propertize "(No test items yet. Press 'a' to add one.)" 'face 'font-lock-comment-face))
     "\n\n" separator "\n"
     (propertize "[a]dd  [x]toggle  [s]screenshot  [v]iew  [e]xport  [d]elete  [q]uit"
                 'face 'font-lock-comment-face))))

(defun workspace-evidence--format-item (item)
  "Format a single evidence ITEM."
  (let* ((id (alist-get 'id item))
         (text (alist-get 'text item))
         (complete (alist-get 'complete item))
         (screenshots (alist-get 'screenshots item))
         (screenshot-count (length screenshots))
         (checkbox (if complete "[x]" "[ ]"))
         (face (if complete 'workspace-evidence-complete-face 'workspace-evidence-pending-face))
         (screenshot-info (if (> screenshot-count 0)
                              (propertize (format "  [%d screenshot%s]"
                                                  screenshot-count
                                                  (if (= screenshot-count 1) "" "s"))
                                          'face 'workspace-evidence-screenshot-face)
                            "")))
    (propertize (format "%s %s%s" checkbox text screenshot-info)
                'face face
                'evidence-item item
                'evidence-id id)))

(defun workspace-evidence-refresh ()
  "Refresh the evidence buffer."
  (interactive)
  (when (eq major-mode 'workspace-evidence-mode)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (workspace-evidence--format-buffer))
      (goto-char (min pos (point-max))))))

;;; Evidence Item Management

(defun workspace-evidence--get-item-at-point ()
  "Get evidence item at point."
  (get-text-property (point) 'evidence-item))

(defun workspace-evidence-add-item (text)
  "Add new test item with TEXT."
  (interactive "sTest item: ")
  (when (and text (not (string-empty-p text)))
    (let ((item `((id . ,(workspace-evidence--next-id))
                  (text . ,text)
                  (complete . :false)
                  (screenshots . []))))
      (push item workspace-evidence--items)
      (workspace-evidence--save)
      (workspace-evidence-refresh))))

(defun workspace-evidence-toggle-complete ()
  "Toggle completion status of item at point."
  (interactive)
  (when-let ((item (workspace-evidence--get-item-at-point)))
    (let* ((id (alist-get 'id item))
           (current (alist-get 'complete item)))
      (setf (alist-get 'complete (assq id workspace-evidence--items))
            (if (eq current :false) t :false))
      (workspace-evidence--save)
      (workspace-evidence-refresh))))

(defun workspace-evidence-delete-item ()
  "Delete item at point."
  (interactive)
  (when-let ((item (workspace-evidence--get-item-at-point)))
    (when (yes-or-no-p (format "Delete '%s'? " (alist-get 'text item)))
      (let ((id (alist-get 'id item)))
        (setq workspace-evidence--items
              (cl-remove-if (lambda (i) (= (alist-get 'id i) id))
                            workspace-evidence--items))
        (workspace-evidence--save)
        (workspace-evidence-refresh)))))

;;; Screenshot Management

(defun workspace-evidence--save-clipboard-image (dest-path)
  "Save clipboard image to DEST-PATH using xclip. Return t if successful."
  (let ((exit-code (call-process "xclip" nil nil nil
                                  "-selection" "clipboard"
                                  "-t" "image/png"
                                  "-o")))
    (if (zerop exit-code)
        (progn
          (with-temp-file dest-path
            (set-buffer-multibyte nil)
            (call-process "xclip" nil t nil
                          "-selection" "clipboard"
                          "-t" "image/png"
                          "-o"))
          t)
      nil)))

(defun workspace-evidence--newest-screenshot ()
  "Find newest screenshot from today's folder."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (today-dir (expand-file-name today workspace-screenshot-dir)))
    (when (file-directory-p today-dir)
      (let* ((files (directory-files today-dir t "\\.png\\'"))
             (sorted (sort files (lambda (a b)
                                   (time-less-p (nth 5 (file-attributes b))
                                                (nth 5 (file-attributes a)))))))
        (car sorted)))))

(defun workspace-evidence-attach-screenshot-clipboard ()
  "Attach screenshot from clipboard to item at point."
  (interactive)
  (unless (workspace-evidence--get-item-at-point)
    (user-error "No test item at point"))
  (let* ((item (workspace-evidence--get-item-at-point))
         (id (alist-get 'id item))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "item-%d-%s.png" id timestamp))
         (screenshots-dir (workspace-evidence--screenshots-dir
                           workspace-evidence--workspace-path
                           workspace-evidence--rev))
         (dest-path (expand-file-name filename screenshots-dir)))
    (workspace-evidence--ensure-dirs workspace-evidence--workspace-path workspace-evidence--rev)
    (if (workspace-evidence--save-clipboard-image dest-path)
        (progn
          ;; Add to item's screenshots list
          (let* ((item-in-list (cl-find-if (lambda (i) (= (alist-get 'id i) id))
                                           workspace-evidence--items))
                 (screenshots (alist-get 'screenshots item-in-list)))
            (setf (alist-get 'screenshots item-in-list)
                  (vconcat screenshots (vector dest-path))))
          (workspace-evidence--save)
          (workspace-evidence-refresh)
          (message "Screenshot attached: %s" filename))
      (user-error "No image in clipboard (or xclip failed)"))))

(defun workspace-evidence-attach-screenshot-file ()
  "Attach screenshot from file picker to item at point."
  (interactive)
  (unless (workspace-evidence--get-item-at-point)
    (user-error "No test item at point"))
  (let* ((default-dir (or (let* ((today (format-time-string "%Y-%m-%d"))
                                  (today-dir (expand-file-name today workspace-screenshot-dir)))
                            (when (file-directory-p today-dir) today-dir))
                          workspace-screenshot-dir))
         (file (read-file-name "Screenshot file: " default-dir nil t nil
                               (lambda (f) (string-match-p "\\.png\\'" f)))))
    (when (and file (file-exists-p file))
      (let* ((item (workspace-evidence--get-item-at-point))
             (id (alist-get 'id item))
             (timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (filename (format "item-%d-%s.png" id timestamp))
             (screenshots-dir (workspace-evidence--screenshots-dir
                               workspace-evidence--workspace-path
                               workspace-evidence--rev))
             (dest-path (expand-file-name filename screenshots-dir)))
        (workspace-evidence--ensure-dirs workspace-evidence--workspace-path workspace-evidence--rev)
        (copy-file file dest-path t)
        ;; Add to item's screenshots list
        (let* ((item-in-list (cl-find-if (lambda (i) (= (alist-get 'id i) id))
                                         workspace-evidence--items))
               (screenshots (alist-get 'screenshots item-in-list)))
          (setf (alist-get 'screenshots item-in-list)
                (vconcat screenshots (vector dest-path))))
        (workspace-evidence--save)
        (workspace-evidence-refresh)
        (message "Screenshot attached: %s" filename)))))

(defun workspace-evidence-view-screenshots ()
  "View screenshots for item at point."
  (interactive)
  (if-let ((item (workspace-evidence--get-item-at-point)))
      (let ((screenshots (alist-get 'screenshots item)))
        (if (and screenshots (> (length screenshots) 0))
            (let ((first-shot (aref screenshots 0)))
              (if (file-exists-p first-shot)
                  (find-file first-shot)
                (user-error "Screenshot file not found: %s" first-shot)))
          (user-error "No screenshots for this item")))
    (user-error "No test item at point")))

;;; Export to Markdown

(defun workspace-evidence-export-markdown ()
  "Export test evidence to markdown and copy to clipboard."
  (interactive)
  (let* ((branch workspace-evidence--branch)
         (rev workspace-evidence--rev)
         (markdown (workspace-evidence--generate-markdown)))
    (kill-new markdown)
    (message "Test evidence copied to clipboard (%d items)" (length workspace-evidence--items))))

(defun workspace-evidence--generate-markdown ()
  "Generate markdown for current evidence."
  (let ((lines '()))
    (push (format "## Test Evidence\n") lines)
    (push (format "Branch: `%s` @ `%s`\n" workspace-evidence--branch workspace-evidence--rev) lines)
    (dolist (item workspace-evidence--items)
      (let* ((text (alist-get 'text item))
             (complete (alist-get 'complete item))
             (screenshots (alist-get 'screenshots item))
             (checkbox (if (eq complete :false) "- [ ]" "- [x]")))
        (push (format "%s %s" checkbox text) lines)
        (when (and screenshots (> (length screenshots) 0))
          (dotimes (i (length screenshots))
            (let ((shot (aref screenshots i)))
              ;; Just reference the filename - actual upload is separate
              (push (format "  - Screenshot %d: `%s`" (1+ i) (file-name-nondirectory shot)) lines))))))
    (string-join (nreverse lines) "\n")))

;;; Transient Menu for Evidence

(transient-define-prefix workspace-evidence-transient ()
  "Test evidence commands."
  ["Test Evidence"
   ["Items"
    ("a" "Add item" workspace-evidence-add-item)
    ("x" "Toggle complete" workspace-evidence-toggle-complete)
    ("d" "Delete item" workspace-evidence-delete-item)]
   ["Screenshots"
    ("s" "Attach from clipboard" workspace-evidence-attach-screenshot-clipboard)
    ("S" "Attach from file" workspace-evidence-attach-screenshot-file)
    ("v" "View screenshots" workspace-evidence-view-screenshots)]
   ["Export"
    ("e" "Export markdown" workspace-evidence-export-markdown)]
   ["Buffer"
    ("r" "Refresh" workspace-evidence-refresh)
    ("q" "Quit" transient-quit-one)]])

;;; Entry Point

;;;###autoload
(defun workspace-test-evidence ()
  "Open test evidence buffer for current workspace."
  (interactive)
  (let* ((workspace-path (or (workspace--current-workspace-path)
                             (user-error "Not in a workspace")))
         (rev (workspace-evidence--get-rev workspace-path))
         (branch (workspace-evidence--get-branch workspace-path))
         (buf-name (format "*Evidence: %s @ %s*" branch rev))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (workspace-evidence-mode)
      (setq workspace-evidence--workspace-path workspace-path
            workspace-evidence--rev rev
            workspace-evidence--branch branch
            workspace-evidence--items (or (workspace-evidence--load workspace-path rev) '()))
      (workspace-evidence-refresh))
    (pop-to-buffer buf)))

(defun workspace--current-workspace-path ()
  "Get current workspace path if in one."
  (let ((default-directory (or default-directory "~")))
    (cl-find-if (lambda (ws-path)
                  (string-prefix-p ws-path (expand-file-name default-directory)))
                (mapcar (lambda (n) (format "%s/crewcapableai.%d/" workspace-root n))
                        (number-sequence 0 (1- workspace-count))))))

;; Add to dashboard keybindings
(define-key workspace-mode-map (kbd "e") #'workspace-test-evidence-at-point)

;;;###autoload
(defun workspace-test-evidence-at-point ()
  "Open test evidence for workspace at point in dashboard."
  (interactive)
  (if-let ((path (workspace--get-path-at-point)))
      (let ((default-directory path))
        (workspace-test-evidence))
    (user-error "No workspace at point")))

(provide 'config-workspace)
;;; config-workspace.el ends here
