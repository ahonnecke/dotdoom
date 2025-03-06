;;; config-smerge.el -*- lexical-binding: t; -*-

(with-eval-after-load 'smerge-mode
  (require 'hydra)

  ;; Define the smerge Hydra
  (defhydra hydra-smerge
    (:color pink :hint nil :post (smerge-mode -1)) ;; Disable smerge-mode when exiting Hydra
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
            (bury-buffer)
            (hydra-keyboard-quit)) ;; Close Hydra properly
     "Save and bury buffer" :color blue)
    ("q" hydra-keyboard-quit "cancel" :color blue))

  ;; Function to start a transient smerge-mode session
  (defun my/smerge-session ()
    "Start a transient smerge-mode session, enabling it only while Hydra is active."
    (interactive)
    (smerge-mode 1)  ;; Enable smerge-mode temporarily
    (hydra-smerge/body) ;; Open Hydra
    (smerge-mode -1)) ;; Disable smerge-mode when Hydra closes

  ;; Bind the toggle key to start smerge-mode manually
  (global-set-key (kbd "C-x g m") #'my/smerge-session))
