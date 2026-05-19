;;; config-org-download.el -*- lexical-binding: t; -*-

;; Only attach the hook if the package actually loads — otherwise
;; dired-mode-hook fires `org-download-enable` (void function) every
;; time a dired buffer opens. The package isn't declared in packages.el.
(with-eval-after-load 'org-download
  (add-hook 'dired-mode-hook 'org-download-enable))
