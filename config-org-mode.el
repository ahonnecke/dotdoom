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

(setq org-hide-emphasis-markers t)

;(use-package org-bullets
                                        ;  :config
; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)
