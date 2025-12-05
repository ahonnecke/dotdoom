;;; ~/.doom.d/config-quad-screen.el -*- lexical-binding: t; -*-

(defun quad-screen (&optional arg)
  "Split the screen into four buffers."
  (interactive "p")
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun penta-screen (&optional arg)
  "Split the screen into five buffers."
  (interactive "p")
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (balance-windows))
