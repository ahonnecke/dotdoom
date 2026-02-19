;;; vterm-resize-diag.el --- Diagnose vterm resize issues -*- lexical-binding: t; -*-
;; M-x eval-buffer, then switch to a Claude buffer and run M-x vterm-resize-diag

(defun vterm-resize-diag ()
  "Diagnose vterm resize state in the current buffer."
  (interactive)
  (if (not (derived-mode-p 'vterm-mode))
      (message "Not in a vterm buffer. Switch to a Claude buffer first.")
    (let* ((buf (current-buffer))
           (win (get-buffer-window buf))
           (proc vterm--process)
           (proc-alive (and proc (process-live-p proc)))
           (win-width (when win (window-body-width win)))
           (win-max-chars (when win (window-max-chars-per-line win)))
           (win-height (when win (window-body-height win)))
           (min-width (if (local-variable-p 'vterm-min-window-width)
                          vterm-min-window-width
                        (default-value 'vterm-min-window-width)))
           (min-width-local-p (local-variable-p 'vterm-min-window-width))
           (global-min-width (default-value 'vterm-min-window-width))
           (copy-mode (bound-and-true-p vterm-copy-mode))
           (margin (if (fboundp 'vterm--get-margin-width)
                       (vterm--get-margin-width)
                     0))
           (stty-size
            (when (and proc-alive (process-tty-name proc))
              (ignore-errors
                (with-temp-buffer
                  (call-process "stty" nil t nil
                                "-F" (process-tty-name proc)
                                "size")
                  (string-trim (buffer-string))))))
           (advice-active (advice--p (advice--symbol-function 'claude-code--term-make)))
           (vterm-advice (advice--p (advice--symbol-function 'vterm-mode))))
      (message (concat
                (format "[DIAG] buf=%s win=%sx%s max-chars=%s"
                        (buffer-name buf) win-width win-height win-max-chars)
                (format " | min-w=%s(%s) global-min=%s margin=%d"
                        min-width (if min-width-local-p "local" "GLOBAL")
                        global-min-width margin)
                (format " | stty=%s" (or stty-size "?"))
                (format " | proc=%s copy=%s"
                        (if proc-alive "alive" "DEAD") copy-mode)
                (format " | term-make-advice=%s vterm-mode-advice=%s"
                        (if advice-active "Y" "N")
                        (if vterm-advice "Y" "N"))
                (format " | kill-on-exit=%s"
                        (if (local-variable-p 'vterm-kill-buffer-on-exit)
                            vterm-kill-buffer-on-exit "GLOBAL")))))))

(defun vterm-force-resize ()
  "Force vterm to re-read window dimensions via physical shrink+restore."
  (interactive)
  (if (not (derived-mode-p 'vterm-mode))
      (message "Not in a vterm buffer.")
    (let* ((buf (current-buffer))
           (win (get-buffer-window buf))
           (proc vterm--process))
      (when (and win proc (process-live-p proc))
        (when (bound-and-true-p vterm-copy-mode)
          (vterm-copy-mode -1))
        (let ((shrink (min 10 (- (window-body-width win) 20))))
          (when (> shrink 0)
            (window-resize win (- shrink) t)
            (run-at-time 0.3 nil
                         (lambda ()
                           (when (and (window-live-p win)
                                      (buffer-live-p buf)
                                      (eq (window-buffer win) buf))
                             (window-resize win shrink t)
                             (message "Forced resize to %dx%d"
                                      (window-body-width win)
                                      (window-body-height win)))))))))))

(message "Loaded. M-x vterm-resize-diag (check) or M-x vterm-force-resize (fix)")
