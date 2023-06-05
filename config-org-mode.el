;;; ../src/home/.doom.d/config-org-mode.el -*- lexical-binding: t; -*-

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-, ;") #'org-metaright)
  (define-key org-mode-map (kbd "M-, j") #'org-metaleft)
  (define-key org-mode-map (kbd "M-, l") #'org-metaup)
  (define-key org-mode-map (kbd "M-, k") #'org-metadown)
  (define-key org-mode-map (kbd "M-, :") #'org-shiftmetaright)
  (define-key org-mode-map (kbd "M-, J") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-, L") #'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-, K") #'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-, <return>") #'org-insert-heading)
  (define-key org-mode-map (kbd "M-<return>") #'org-insert-heading)
  )

;; ;; https://github.com/syl20bnr/spacemacs/issues/9603
;; ;; seems to apply to me too, this fixes it
;; (org-defkey org-mode-map [(meta return)] 'org-meta-return)
;; (org-defkey org-mode-map (kbd "C-;") 'er/expand-region)
