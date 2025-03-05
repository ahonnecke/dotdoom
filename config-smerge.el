;;; config-smerge.el -*- lexical-binding: t; -*-

(with-eval-after-load 'smerge-mode
  (require 'hydra)

  (defhydra hydra-smerge
    (:color pink :hint nil)
    "
      ^Move^       ^Keep^               ^Diff^                 ^Other^
      ^^-----------^^-------------------^^---------------------^^-------
      _n_: next    _b_: base            _<_: upper/base        _C_: Combine
      _p_: prev    _u_: upper           _=_: upper/lower       _r_: resolve
                 _l_: lower            _>_: base/lower        _k_: kill current
                 _a_: all              _R_: refine
                 _RET_: current        _E_: ediff
    "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

  ;; Automatically open smerge-mode when detecting merge markers
  (defun my/enable-smerge-mode ()
    "Enable `smerge-mode` automatically if a file contains Git merge conflicts."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)
        (message "Enabled smerge-mode automatically"))))

  (add-hook 'find-file-hook #'my/enable-smerge-mode)

  ;; Open the Hydra automatically when smerge-mode is activated
  (add-hook 'smerge-mode-hook
            (lambda ()
              (when (boundp 'smerge-mode-map)
                (define-key smerge-mode-map (kbd "C-c ^ h") #'hydra-smerge/body))
              (hydra-smerge/body))) ;; Auto-open Hydra

  ;; Alternative global keybindings
  (global-set-key (kbd "C-x g m") #'hydra-smerge/body)  ;; Git Merge mode
  (global-set-key (kbd "<f8>") #'hydra-smerge/body)     ;; F8 for quick access

  ;; Easier navigation between conflicts
  (define-key smerge-mode-map (kbd "M-n") #'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") #'smerge-prev))
