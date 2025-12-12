;;; ~/.doom.d/config-browse.el -*- lexical-binding: t; -*-

(defun firefox-search ()
  "Search in Firefox. Uses region if active, otherwise word/symbol at point."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (or (thing-at-point 'symbol t)
                     (thing-at-point 'word t)
                     (read-string "Search: "))))
         (encoded (url-hexify-string text))
         (cmd (concat "firefox https://www.google.com/search?q=" encoded)))
    (message "Searching: %s" text)
    (shell-command cmd)))

;; Keep old name as alias
(defalias 'firefox-search-region 'firefox-search)

;; Add as embark action for regions
(with-eval-after-load 'embark
  (define-key embark-region-map (kbd "S") #'firefox-search)
  (define-key embark-symbol-map (kbd "S") #'firefox-search))

;; Direct binding: C-c g s = search (firefox)
(define-key ashton-mode-map (kbd "C-c g s") #'firefox-search)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Smart GitHub Browse (handles magit revision buffers)
;;; ════════════════════════════════════════════════════════════════════════════

(defun browse-at-remote-smart ()
  "Open current file in GitHub/GitLab.
If in a magit revision buffer (file.ext~commit~), opens that specific commit.
Otherwise uses current branch."
  (interactive)
  (let* ((buf-name (buffer-name))
         (buf-file (buffer-file-name)))
    (cond
     ;; Magit revision buffer: file.ext~abc123~
     ((and buf-file (string-match "\\(.+\\)\\.~\\([a-f0-9]+\\)~\\'" buf-file))
      (let* ((real-file (match-string 1 buf-file))
             (commit (match-string 2 buf-file))
             (project-root (or (vc-git-root real-file)
                               (locate-dominating-file real-file ".git")))
             (relative-path (when project-root
                              (file-relative-name real-file project-root)))
             (remote-url (when project-root
                           (let ((default-directory project-root))
                             (string-trim
                              (shell-command-to-string
                               "git remote get-url origin 2>/dev/null || git remote get-url upstream 2>/dev/null"))))))
        (if (and remote-url relative-path (not (string-empty-p remote-url)))
            (let ((github-url (browse-at-remote--build-url remote-url commit relative-path)))
              (message "Opening %s at %s" relative-path (substring commit 0 7))
              (browse-url github-url))
          (user-error "Could not determine GitHub URL"))))

     ;; Normal file - use browse-at-remote
     (t
      (if (fboundp '+vc/browse-at-remote)
          (+vc/browse-at-remote)
        (browse-at-remote))))))

(defun browse-at-remote--build-url (remote-url commit relative-path)
  "Build GitHub/GitLab URL from REMOTE-URL, COMMIT, and RELATIVE-PATH."
  (let* ((clean-url (replace-regexp-in-string "\\.git\\'" "" remote-url))
         (clean-url (replace-regexp-in-string "^git@\\([^:]+\\):" "https://\\1/" clean-url))
         (clean-url (replace-regexp-in-string "^ssh://git@" "https://" clean-url)))
    (cond
     ;; GitHub
     ((string-match-p "github\\.com" clean-url)
      (format "%s/blob/%s/%s" clean-url commit relative-path))
     ;; GitLab
     ((string-match-p "gitlab" clean-url)
      (format "%s/-/blob/%s/%s" clean-url commit relative-path))
     ;; Generic (assume GitHub-style)
     (t
      (format "%s/blob/%s/%s" clean-url commit relative-path)))))

;; Override the binding
(define-key ashton-mode-map (kbd "C-c g g") #'browse-at-remote-smart)
