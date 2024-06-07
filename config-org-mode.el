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
  ;; what the fuck is wrong with org-mode?
  (define-key org-mode-map (kbd "C-e") #'end-of-line)
  (define-key org-mode-map (kbd "<return>") #'newline)

  (global-set-key (kbd "C-;") 'er/expand-region)
  (define-key org-mode-map (kbd "C-;") 'er/expand-region)

  (define-key org-mode-map (kbd "C-c x h") #'org-html-export-to-html)
  (define-key org-mode-map (kbd "C-c w") #'org-html-export-to-html)
  )

(global-set-key (kbd "C-c c f") #'company-files)
;; ;; https://github.com/syl20bnr/spacemacs/issues/9603
;; ;; seems to apply to me too, this fixes it
;; (org-defkey org-mode-map [(meta return)] 'org-meta-return)


(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
;; (setq org-latex-prefer-user-labels t)

;; deleted unwanted file extensions after latexMK
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(setq org-latex-pdf-process
      '("latexmk -f -silent -output-directory=./tex %f \n cp ./tex/%b.pdf ./%b.pdf"))


(use-package ox-extra
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
