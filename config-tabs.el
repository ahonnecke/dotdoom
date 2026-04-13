;;; config-tabs.el -*- lexical-binding: t; -*-

(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-stop-list (my-generate-tab-stops))

;; JSON files use 2-space indentation
(add-hook 'json-mode-hook
          (lambda ()
            ;; Buffer-local in json-mode: 2-space indent via js-indent-level
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))
