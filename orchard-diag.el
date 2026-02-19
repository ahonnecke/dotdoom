;;; orchard-diag.el --- Diagnose orchard → claude startup -*- lexical-binding: t; -*-
;; M-x eval-buffer, then press RET on an orchard line.
;; Check *Messages* buffer for "[DIAG]" lines to see what happened.

;; Trace orchard--place-claude-buffer to see which branch it takes
(advice-add 'orchard--place-claude-buffer :around
            (lambda (orig-fn claude-buf)
              (message "[DIAG] place-claude-buffer called. buf=%s live=%s"
                       (when claude-buf (buffer-name claude-buf))
                       (buffer-live-p claude-buf))
              (if (not (buffer-live-p claude-buf))
                  (message "[DIAG] ABORT: buffer not live!")
                (let* ((existing-win (get-buffer-window claude-buf))
                       (orchard-win (get-buffer-window "*Orchard*"))
                       (all-wins (window-list nil 'no-mini))
                       (reusable-win
                        (cl-find-if
                         (lambda (win)
                           (let ((buf (window-buffer win)))
                             (and (not (eq win orchard-win))
                                  (not (eq buf claude-buf))
                                  (not (string-prefix-p "*claude:" (buffer-name buf))))))
                         all-wins)))
                  (message "[DIAG] windows: %d existing=%s orchard=%s reusable=%s"
                           (length all-wins) existing-win orchard-win reusable-win)
                  (message "[DIAG] window buffers: %s"
                           (mapcar (lambda (w) (buffer-name (window-buffer w))) all-wins))))
              (funcall orig-fn claude-buf)
              (message "[DIAG] after placement: window=%s"
                       (get-buffer-window claude-buf)))
            '((name . orchard-diag-place)))

;; Trace orchard--start-claude-with-resume
(advice-add 'orchard--start-claude-with-resume :around
            (lambda (orig-fn path &optional command)
              (message "[DIAG] start-claude-with-resume path=%s" path)
              (let ((existing (orchard--claude-buffer-for-path path)))
                (message "[DIAG] existing claude buffer: %s"
                         (when existing (buffer-name existing))))
              (condition-case err
                  (funcall orig-fn path command)
                (error (message "[DIAG] ERROR in start-claude-with-resume: %s" err)))
              ;; Check result
              (let ((claude-buf (orchard--claude-buffer-for-path path)))
                (message "[DIAG] after start: claude-buf=%s in-window=%s"
                         (when claude-buf (buffer-name claude-buf))
                         (when claude-buf (get-buffer-window claude-buf)))))
            '((name . orchard-diag-start)))

;; Trace orchard--start-claude-backend to see which backend
(advice-add 'orchard--start-claude-backend :before
            (lambda (path &optional command)
              (message "[DIAG] start-claude-backend path=%s backend=%s"
                       path (orchard--detect-backend)))
            '((name . orchard-diag-backend)))

(defun orchard-diag-remove ()
  "Remove all diagnostic advice."
  (interactive)
  (advice-remove 'orchard--place-claude-buffer 'orchard-diag-place)
  (advice-remove 'orchard--start-claude-with-resume 'orchard-diag-start)
  (advice-remove 'orchard--start-claude-backend 'orchard-diag-backend)
  (message "Diagnostic advice removed."))

(message "Orchard diagnostic advice installed. Press RET on an orchard line, then check *Messages*. Run M-x orchard-diag-remove when done.")
