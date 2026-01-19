;;; config-claude-modeline.el -*- lexical-binding: t; -*-

;; Global modeline indicator for Claude Code status
;; Shows when any Claude instances are running or waiting for input

(defvar claude-modeline--timer nil
  "Timer for updating Claude status.")

(defvar claude-modeline--waiting-count 0
  "Number of Claude buffers waiting for input.")

(defvar claude-modeline--running-count 0
  "Number of Claude buffers running (not waiting).")

(defvar claude-modeline--total-count 0
  "Total number of Claude buffers.")

(defface claude-modeline-waiting
  '((t :foreground "#E5C07B" :weight bold))
  "Face for Claude waiting indicator (needs attention).")

(defface claude-modeline-running
  '((t :foreground "#98C379"))
  "Face for Claude running indicator (working).")

(defun claude-modeline--claude-buffer-p (buf)
  "Return t if BUF is a Claude Code buffer."
  (string-prefix-p "*claude:" (buffer-name buf)))

(defun claude-modeline--claude-waiting-p (buffer)
  "Check if Claude BUFFER appears to be waiting for input.
Looks for common prompt patterns at end of buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (forward-line -5)
        (let ((end-text (buffer-substring-no-properties (point) (point-max))))
          ;; Look for prompt indicators - Claude waiting for input
          (or (string-match-p "^>" end-text)
              (string-match-p "\\[Y/n\\]" end-text)
              (string-match-p "\\[y/N\\]" end-text)
              (string-match-p "yes/no" end-text)
              (string-match-p "proceed\\?" end-text)
              (string-match-p "continue\\?" end-text)))))))

(defun claude-modeline--update ()
  "Update Claude status counts."
  (let ((claudes (cl-remove-if-not #'claude-modeline--claude-buffer-p (buffer-list))))
    (setq claude-modeline--total-count (length claudes))
    (setq claude-modeline--waiting-count
          (cl-count-if #'claude-modeline--claude-waiting-p claudes))
    (setq claude-modeline--running-count
          (- claude-modeline--total-count claude-modeline--waiting-count))
    (force-mode-line-update t)))

(defun claude-modeline--format ()
  "Format modeline string for Claude status."
  (when (> claude-modeline--total-count 0)
    (cond
     ;; Some waiting - show waiting count prominently
     ((> claude-modeline--waiting-count 0)
      (if (> claude-modeline--running-count 0)
          ;; Both waiting and running
          (concat
           (propertize (format " [Claude: %d" claude-modeline--waiting-count)
                       'face 'claude-modeline-waiting)
           (propertize "⏳" 'face 'claude-modeline-waiting)
           (propertize (format " %d" claude-modeline--running-count)
                       'face 'claude-modeline-running)
           (propertize "🟢]" 'face 'claude-modeline-running))
        ;; Only waiting
        (propertize (format " [Claude: %d⏳]" claude-modeline--waiting-count)
                    'face 'claude-modeline-waiting)))
     ;; All running
     (t
      (propertize (format " [Claude: %d🟢]" claude-modeline--total-count)
                  'face 'claude-modeline-running)))))

;;;###autoload
(define-minor-mode claude-modeline-mode
  "Show Claude status in modeline."
  :global t
  :lighter nil
  (if claude-modeline-mode
      (progn
        ;; Start update timer (every 2 seconds)
        (setq claude-modeline--timer
              (run-with-timer 1 2 #'claude-modeline--update))
        ;; Add to global mode string
        (unless (member '(:eval (claude-modeline--format)) global-mode-string)
          (setq global-mode-string
                (append global-mode-string '((:eval (claude-modeline--format)))))))
    ;; Disable
    (when claude-modeline--timer
      (cancel-timer claude-modeline--timer)
      (setq claude-modeline--timer nil))
    (setq global-mode-string
          (delete '(:eval (claude-modeline--format)) global-mode-string))))

;; Auto-enable on load
(claude-modeline-mode 1)

(provide 'config-claude-modeline)
;;; config-claude-modeline.el ends here
