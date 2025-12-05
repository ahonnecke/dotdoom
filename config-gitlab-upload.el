;;; gitlab-upload-latest.el --- Upload today's newest screenshot to GitLab  -*- lexical-binding: t; -*-

;; Usage:
;;   ;; Token (api scope): env or setq
;;   export GITLAB_TOKEN="glpat-xxxxxxxxxxxxxxxx"
;;   ;; or in Emacs: (setq gitlab-upload-token "glpat-xxxxxxxxxxxxxxxx")
;;
;;   ;; Choose target project (pick ONE style):
;;   (setq gitlab-upload-project 12345678)                 ;; numeric ID (best for API)
;;   ;; or explicit path:
;;   (setq gitlab-upload-project "org/subgroup/repo")      ;; "group/subgroup/project"
;;   ;; or derive from a remote (default "origin"):
;;   (setq gitlab-project-remote "upstream")
;;
;;   ;; Insert absolute URLs (project-qualified) instead of /uploads/relative:
;;   (setq gitlab-insert-absolute-urls t)
;;
;;   M-x gitlab-upload-latest-screenshot
;;
;; Optional keybindings:
;; (with-eval-after-load 'markdown-mode
;;   (define-key markdown-mode-map (kbd "C-c C-u") #'gitlab-upload-latest-screenshot))
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c C-u") #'gitlab-upload-latest-screenshot))

(require 'json)
(require 'subr-x)
(require 'url-util) ;; url-hexify-string

(defgroup gitlab-upload-latest nil
  "Upload newest screenshot from today's folder to GitLab and insert Markdown."
  :group 'tools)

;; ------------------------- Configuration -------------------------------------

(defcustom gitlab-upload-host (or (getenv "GITLAB_HOST") "https://gitlab.com")
  "Base URL of the GitLab instance, e.g., https://gitlab.com."
  :type 'string)

;;TODO change this to a token that only has image upload
;;this is the sysmic one

(defcustom gitlab-upload-project nil
  "Target project for uploads. Use numeric ID or \"group/subgroup/project\".
When nil, derive from `gitlab-project-remote`."
  :type '(choice (integer :tag "Numeric project ID")
          (string  :tag "group/subgroup/project")
          (const   :tag "Derive from remote" nil)))

(defcustom gitlab-project-remote "origin"
  "Git remote to use when deriving the project path (e.g., \"origin\" or \"upstream\")."
  :type 'string)

(defcustom gitlab-insert-absolute-urls t
  "If non-nil, rewrite returned Markdown to use absolute project-qualified URLs.
This avoids repo-relative `/uploads/...` links that 404 outside the project context."
  :type 'boolean)

(defcustom gitlab-screenshot-root (expand-file-name "~/screenshots")
  "Root directory where screenshots are stored, organized by YYYY-MM-DD subfolders."
  :type 'directory)

(defcustom gitlab-screenshot-date-format "%Y-%m-%d"
  "Date format for the daily subdirectory under `gitlab-screenshot-root`."
  :type 'string)

(defcustom gitlab-ocr-alt-maxlen 120
  "Maximum length for alt text extracted from *.ocr.txt."
  :type 'integer)

(defvar gitlab-upload-debug nil
  "When non-nil, log extra info about the file and curl invocation.")

;; ----------------------------- Helpers ---------------------------------------

(defun gitlab--token ()
  "Return a non-empty token from `gitlab-upload-token` or $GITLAB_TOKEN, else error."
  (let* ((val (or gitlab-upload-token (getenv "GITLAB_TOKEN")))
         (val (and val (string-trim val))))
    (if (and val (not (string-empty-p val)))
        val
      (user-error "Set GITLAB_TOKEN or customize `gitlab-upload-token'"))))

(defun gitlab--remote-url (remote)
  "Get git remote URL string for REMOTE (e.g. origin/upstream), or empty string."
  (string-trim (shell-command-to-string
                (format "git config --get remote.%s.url 2>/dev/null" remote))))

(defun gitlab--project-from-url (url)
  "Extract \"group/subgroup/project\" from a Git remote URL."
  (cond
   ;; git@host:group/sub/project.git
   ((string-match "^[^@]+@[^:]+:\\([^ ]+?\\)\\(?:\\.git\\)?$" url)
    (match-string 1 url))
   ;; https?://host/group/sub/project(.git)
   ((string-match "^https?://[^/]+/\\([^ ]+?\\)\\(?:\\.git\\)?$" url)
    (match-string 1 url))
   (t nil)))

(defun gitlab--project-identifier-api ()
  "Return project identifier string for API: numeric ID or URL-encoded path.
Precedence: `gitlab-upload-project` (if set), else derive from `gitlab-project-remote`."
  (cond
   ((and gitlab-upload-project
         (not (string-empty-p (format "%s" gitlab-upload-project))))
    (if (integerp gitlab-upload-project)
        (number-to-string gitlab-upload-project)
      (url-hexify-string gitlab-upload-project)))
   (t
    (let* ((remote-url (gitlab--remote-url gitlab-project-remote))
           (proj-path  (and (not (string-empty-p remote-url))
                            (gitlab--project-from-url remote-url))))
      (unless proj-path
        (user-error "Cannot derive project from remote '%s' (URL: %S). Set `gitlab-upload-project`."
                    gitlab-project-remote remote-url))
      (url-hexify-string proj-path)))))

(defun gitlab--project-web-path ()
  "Return unencoded \"group/subgroup/project\" for building absolute URLs.
If only a numeric `gitlab-upload-project` is provided, derive from remote. May return nil."
  (cond
   ((and (stringp gitlab-upload-project)
         (not (string-empty-p gitlab-upload-project)))
    gitlab-upload-project)
   (t
    (let* ((remote-url (gitlab--remote-url gitlab-project-remote))
           (proj-path  (gitlab--project-from-url remote-url)))
      proj-path))))

(defun gitlab--today-dir ()
  "Return today's screenshot directory (create if missing)."
  (let* ((d (format-time-string gitlab-screenshot-date-format))
         (dir (expand-file-name d gitlab-screenshot-root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

(defun gitlab--newest-png (dir)
  "Return the newest .png file path in DIR, or nil if none."
  (let* ((files (directory-files dir t "\\.png\\'"))
         (sorted (sort files (lambda (a b)
                               (time-less-p (nth 5 (file-attributes b))
                                            (nth 5 (file-attributes a)))))))
    (car sorted)))

(defun gitlab--ocr-alt-text (png-path)
  "Return a short alt text from matching *.ocr.txt if present, else nil."
  (let* ((txt (concat (file-name-sans-extension png-path) ".ocr.txt")))
    (when (file-readable-p txt)
      (with-temp-buffer
        (insert-file-contents txt)
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (point-min)
                       (save-excursion (goto-char (point-min))
                                       (line-end-position))))))
          (when (> (length line) gitlab-ocr-alt-maxlen)
            (setq line (concat (substring line 0 (- gitlab-ocr-alt-maxlen 3)) "...")))
          line)))))

(defun gitlab--apply-alt (markdown alt)
  "Replace alt text in MARKDOWN with ALT, if ALT non-nil."
  (if (and alt (string-match "!\\[[^]]*\\](" markdown))
      (replace-match (format "![%s](" alt) t t markdown)
    markdown))

(defun gitlab--temp-copy-for-upload (path)
  "Copy PATH to a temp file with a simple basename; return temp path.
Caller must delete the returned file."
  (let* ((ext (file-name-extension path t)) ;; keep .png
         (tmp (make-temp-file "gl-upload-" nil ext)))
    (copy-file path tmp t) ; ok-if-already-exists
    tmp))

(defun gitlab--rewrite-to-absolute (markdown url full-path)
  "Return MARKDOWN rewritten to absolute URL when possible.
URL is the relative `/uploads/...` path from API. FULL-PATH (if present)
is `/group/project/uploads/...` from newer GitLab versions. When neither
is usable, returns MARKDOWN unchanged."
  (if (not gitlab-insert-absolute-urls)
      markdown
    (let* ((abs
            (cond
             ;; Newer GitLab returns full_path — easiest case
             ((and full-path (string-prefix-p "/" full-path))
              (concat gitlab-upload-host full-path))
             ;; Build from project web path + url
             ((and url (string-prefix-p "/uploads/" url))
              (let ((proj (gitlab--project-web-path)))
                (when proj
                  ;; prefer "-/uploads" variant for modern GitLab, but URL had "/uploads"
                  ;; Both typically work; we’ll stick to the literal 'url' suffix for safety.
                  (concat (file-name-as-directory (concat gitlab-upload-host "/" proj))
                          (string-remove-prefix "/" url)))))
             (t nil))))
      (if (not abs)
          markdown
        ;; Replace the first "](/uploads/" with "](ABS)"
        (replace-regexp-in-string
         "\\](\\(/uploads/[^)]+\\))"
         (format "](%s)" abs)
         markdown
         t t)))))

;; -------------------------- Core uploader ------------------------------------

(defun gitlab--upload-file (file)
  "Upload FILE to selected GitLab project; return (markdown url full_path)."
  (let* ((orig (expand-file-name file)))
    ;; Pre-checks
    (unless (file-exists-p orig)   (user-error "File does not exist: %s" orig))
    (unless (file-regular-p orig)  (user-error "Not a regular file: %s" orig))
    (let* ((size (nth 7 (file-attributes orig))))
      (when (or (null size) (<= size 0))
        (user-error "File is empty (size=%s): %s" size orig)))

    (let* ((tmp (gitlab--temp-copy-for-upload orig)))
      (unwind-protect
          (let* ((token   (gitlab--token))
                 (proj-id (gitlab--project-identifier-api))
                 (endpoint (format "%s/api/v4/projects/%s/uploads" gitlab-upload-host proj-id))
                 (argv (list "curl" "-sS" "-f"
                             "-H" (format "PRIVATE-TOKEN: %s" token)
                             "-F" (format "file=@%s" tmp)
                             endpoint))
                 (json-str
                  (with-temp-buffer
                    (let ((exit (apply #'call-process (car argv) nil t nil (cdr argv))))
                      (when (not (zerop exit))
                        (let ((stderr (buffer-string)))
                          (error (concat
                                  "curl failed (exit %s)\n"
                                  "  file: %s (tmp: %s)\n"
                                  "  endpoint: %s\n"
                                  "  stderr/stdout:\n%s")
                                 exit orig tmp endpoint stderr)))
                      (buffer-string))))
                 (obj  (json-parse-string json-str :object-type 'alist))
                 (md   (alist-get 'markdown obj))
                 (url  (alist-get 'url obj))        ;; /uploads/<hash>/file.png
                 (fp   (or (alist-get 'full_path obj) ;; /group/proj/uploads/...
                           (alist-get 'full-path obj)))) ;; just in case
            (unless md
              (error "No 'markdown' in response: %s" json-str))
            (when gitlab-upload-debug
              (message "[gitlab-upload] OK -> %s" md))
            (list md url fp))
        ;; cleanup
        (ignore-errors (delete-file tmp))))))

;; ----------------------------- Command ---------------------------------------

;;;###autoload
(defun gitlab-upload-latest-screenshot (&optional pick)
  "Upload newest PNG from today's screenshots dir and insert returned Markdown.
With prefix arg PICK, prompt for a file to upload (defaulting to today's dir)."
  (interactive "P")
  (let* ((dir (gitlab--today-dir))
         (file (if pick
                   (read-file-name "Image file: " dir nil t nil
                                   (lambda (f) (string-match-p "\\.png\\'" f)))
                 (or (gitlab--newest-png dir)
                     (user-error "No PNGs found in %s" dir))))
         (triple (gitlab--upload-file file))
         (md     (nth 0 triple))
         (url    (nth 1 triple))
         (fp     (nth 2 triple))
         (alt    (gitlab--ocr-alt-text file))
         (md1    (gitlab--apply-alt md alt))
         (md2    (gitlab--rewrite-to-absolute md1 url fp)))
    (when (derived-mode-p 'markdown-mode 'org-mode 'text-mode)
      (insert md2))
    (kill-new md2)
    (message "Uploaded %s and %s Markdown%s%s"
             (file-name-nondirectory file)
             (if (derived-mode-p 'markdown-mode 'org-mode 'text-mode) "inserted" "copied")
             (if alt " (alt from .ocr.txt)" "")
             (if (and gitlab-insert-absolute-urls (not (equal md1 md2))) " [absolute URL]" ""))))

(provide 'gitlab-upload-latest)
;;; gitlab-upload-latest.el ends here
