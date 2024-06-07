;;; ../src/home/.doom.d/init-string-inflection.el -*- lexical-binding: t; -*-



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom shit that was probably copied from SO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

;; (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key [?\C-c ?d] 'duplicate-line-or-region)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO, move to it's own file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String-inflection.el --- underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names -*- lexical-binding: t -*-

;; Copyright (C) 2004,2014,2016,2017,2018,2020,2021 Free Software Foundation, Inc.

;; Author: akicho8 <akicho8@gmail.com>
;; Keywords: elisp
;; Version: 1.0.16

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; There are three main functions:
;;
;;   1. For Ruby   -> string-inflection-ruby-style-cycle   (foo_bar => FOO_BAR => FooBar => foo_bar)
;;   2. For Elixir -> string-inflection-elixir-style-cycle (foo_bar => FooBar => foo_bar)
;;   3. For Python -> string-inflection-python-style-cycle (foo_bar => FOO_BAR => FooBar => foo_bar)
;;   4. For Java   -> string-inflection-java-style-cycle   (fooBar  => FOO_BAR => FooBar => fooBar)
;;   5. For All    -> string-inflection-all-cycle          (foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar)
;;
;;
;; Example 1:
;;
;;   (require 'string-inflection)
;;   (global-unset-key (kbd "C-q"))
;;   ;; C-q C-u is the key bindings similar to Vz Editor.
;;   (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)
;;
;;   (defun my-string-inflection-cycle-auto ()
;;     "switching by major-mode"
;;     (interactive)
;;     (cond
;;      ;; for emacs-lisp-mode
;;      ((eq major-mode 'emacs-lisp-mode)
;;       (string-inflection-all-cycle))
;;      ;; for java
;;      ((eq major-mode 'java-mode)
;;       (string-inflection-java-style-cycle))
;;      ;; for python
;;      ((eq major-mode 'python-mode)
;;       (string-inflection-python-style-cycle))
;;      ;; for elixir
;;      ((eq major-mode 'elixir-mode)
;;       (string-inflection-elixir-style-cycle))
;;      (t
;;       ;; default
;;       (string-inflection-ruby-style-cycle))))
;;
;;
;; Example 2:
;;
;;   (require 'string-inflection)
;;
;;   ;; default
;;   (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)
;;
;;   ;; for ruby
;;   (add-hook 'ruby-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
;;
;;   ;; for elixir
;;   (add-hook 'elixir-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-elixir-style-cycle)))
;;
;;   ;; for python
;;   (add-hook 'python-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle)))
;;
;;   ;; for java
;;   (add-hook 'java-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))
;;
;; You can also set `string-inflection-skip-backward-when-done' to `t' if
;; you don't like `string-inflect' moving your point to the end of the word.

;;; Code:

(defgroup string-inflection nil
  "Change the casing of words."
  :group 'convenience)

(defcustom string-inflection-skip-backward-when-done nil
  "Controls the position of the cursor after an inflection.

If nil remain at the end of the string after inflecting, else move backward to
the beginning."
  :group 'string-inflection
  :type 'boolean)

(defconst string-inflection-word-chars "a-zA-Z0-9_-")

(defcustom string-inflection-erase-chars-when-region "./"
  "When selected in the region, this character is included in the transformation
as part of the string.

Exactly assume that the underscore exists.
For example, when you select `Foo/Bar', it is considered that `Foo_Bar' is
selected. If include `:', select `FOO::VERSION' to run
`M-x\ string-inflection-underscore' to `foo_version'."
  :group 'string-inflection
  :type 'string)

;; --------------------------------------------------------------------------------

;;;###autoload
(defun string-inflection-ruby-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-ruby-style-cycle-function (string-inflection-get-current-word))))

(fset 'string-inflection-cycle 'string-inflection-ruby-style-cycle)

;;;###autoload
(defun string-inflection-elixir-style-cycle ()
  "foo_bar => FooBar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-elixir-style-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-python-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-python-style-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-java-style-cycle ()
  "fooBar => FOO_BAR => FooBar => fooBar"
  (interactive)
  (string-inflection-insert
   (string-inflection-java-style-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-all-cycle ()
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-all-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-toggle ()
  "toggle foo_bar <=> FooBar"
  (interactive)
  (string-inflection-insert
   (string-inflection-toggle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-camelcase ()
  "FooBar format"
  (interactive)
  (string-inflection-insert
   (string-inflection-pascal-case-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-lower-camelcase ()
  "fooBar format"
  (interactive)
  (string-inflection-insert
   (string-inflection-camelcase-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-underscore ()
  "foo_bar format"
  (interactive)
  (string-inflection-insert
   (string-inflection-underscore-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-capital-underscore ()
  "Foo_Bar format"
  (interactive)
  (string-inflection-insert
   (string-inflection-capital-underscore-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-upcase ()
  "FOO_BAR format"
  (interactive)
  (string-inflection-insert
   (string-inflection-upcase-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-kebab-case ()
  "foo-bar format"
  (interactive)
  (string-inflection-insert
   (string-inflection-kebab-case-function (string-inflection-get-current-word))))

(fset 'string-inflection-lisp 'string-inflection-kebab-case)

;; --------------------------------------------------------------------------------

(defun string-inflection-insert (s)
  (insert s)
  (when string-inflection-skip-backward-when-done (skip-chars-backward string-inflection-word-chars)))

(defun string-inflection-non-word-chars ()
  (concat "^" string-inflection-word-chars))

(defun string-inflection-get-current-word ()
  "Gets the symbol near the cursor"
  (interactive)
  (let* ((start (if (use-region-p)
                    (region-end)
                  (progn
                    (skip-chars-forward string-inflection-word-chars)

                    ;; https://github.com/akicho8/string-inflection/issues/30
                    ;;
                    ;;   objectName->method --> "objectName-" NG
                    ;;                      --> "objectName"  OK
                    (when (and (not (eobp)) (not (bobp)))
                      (when (string= (buffer-substring (1- (point)) (1+ (point))) "->")
                        (forward-char -1)))

                    (point))))
         (end (if (use-region-p)
                  (region-beginning)
                (progn
                  (skip-chars-backward string-inflection-word-chars)
                  (point))))
         (str (buffer-substring start end)))
    (prog1
        (progn
          (when (use-region-p)
            ;; https://github.com/akicho8/string-inflection/issues/31
            ;; Multiple lines will be one line because [:space:] are included to line breaks
            (setq str (replace-regexp-in-string (concat "[" string-inflection-erase-chars-when-region "]+") "_" str)) ; 'aa::bb.cc dd/ee' => 'aa_bb_cc dd_ee'

            ;; kebabing a region can insert an unexpected hyphen
            ;; https://github.com/akicho8/string-inflection/issues/34
            (with-syntax-table (copy-syntax-table)
              (modify-syntax-entry ?_ "w")
              (setq str (replace-regexp-in-string "_+\\b" "" str)) ; '__aA__ __aA__' => '__aA __aA'
              (setq str (replace-regexp-in-string "\\b_+" "" str)) ; '__aA __aA'     => 'aA aA'
              )
            )
          str)
      (delete-region start end))))

;; --------------------------------------------------------------------------------

(defun string-inflection-pascal-case-function (str)
  "foo_bar => FooBar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") ""))

(fset 'string-inflection-upper-camelcase-function 'string-inflection-pascal-case-function)

(defun string-inflection-camelcase-function (str)
  "foo_bar => fooBar"
  (setq str (split-string (string-inflection-underscore-function str) "_"))
  (concat (downcase (car str))
          (mapconcat 'capitalize (cdr str) "")))

(fset 'string-inflection-lower-camelcase-function 'string-inflection-camelcase-function)

(defun string-inflection-upcase-function (str)
  "FooBar => FOO_BAR"
  (upcase (string-inflection-underscore-function str)))

(defun string-inflection-underscore-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun string-inflection-capital-underscore-function (str)
  "foo_bar => Foo_Bar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") "_"))

(defun string-inflection-kebab-case-function (str)
  "foo_bar => foo-bar"
  (let ((case-fold-search nil))
    (setq str (string-inflection-underscore-function str))
    (setq str (replace-regexp-in-string "_" "-" str))))

(defun string-inflection-all-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar
   foo     => FOO     => Foo    => foo"
  (cond
   ;; foo => FOO
   ((string-inflection-word-p str)
    (string-inflection-upcase-function str))
   ;; foo_bar => FOO_BAR
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ;; FOO_BAR => FooBar
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   ;; FooBar => fooBar
   ;; Foo    => foo
   ((string-inflection-pascal-case-p str)
    (string-inflection-camelcase-function str))
   ;; fooBar => foo-bar
   ((string-inflection-camelcase-p str)
    (string-inflection-kebab-case-function str))
   ;; foo-bar => Foo_Bar
   ((string-inflection-kebab-case-p str)
    (string-inflection-capital-underscore-function str))
   ;; foo-bar => foo_bar
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-ruby-style-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-underscore-function str))))

(defalias 'string-inflection-python-style-cycle-function
  'string-inflection-ruby-style-cycle-function)

(defun string-inflection-elixir-style-cycle-function (str)
  "foo_bar => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-java-style-cycle-function (str)
  "fooBar => FOO_BAR => FooBar => fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-camelcase-function str))))

;; Toggle function. But cycle function.
(defun string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> FooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-pascal-case-function str))
   ((string-inflection-pascal-case-p str)
    (string-inflection-camelcase-function str))
   (t
    (string-inflection-underscore-function str))))

;; --------------------------------------------------------------------------------

(defun string-inflection-word-p (str)
  "if foo => t"
  (let ((case-fold-search nil))
    (string-match "\\`[a-z0-9]+\\'" str)))

(defun string-inflection-underscore-p (str)
  "if foo_bar => t"
  (let ((case-fold-search nil))
    (string-match "\\`[a-z0-9_]+\\'" str)))

(defun string-inflection-upcase-p (str)
  "if FOO_BAR => t"
  (let ((case-fold-search nil))
    (string-match "\\`[A-Z0-9_]+\\'" str)))

(defun string-inflection-pascal-case-p (str)
  "if FooBar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[a-z]" str)
     (string-match "\\`[A-Z][a-zA-Z0-9]+\\'" str))))

(fset 'string-inflection-upper-camelcase-p 'string-inflection-pascal-case-p)

(defun string-inflection-camelcase-p (str)
  "if fooBar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[A-Z]" str)
     (string-match "\\`[a-z][a-zA-Z0-9]+\\'" str))))

(fset 'string-inflection-lower-camelcase-p 'string-inflection-camelcase-p)

(defun string-inflection-kebab-case-p (str)
  "if foo-bar => t"
  (string-match "-" str))

(defun string-inflection-capital-underscore-p (str)
  "if Foo_Bar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[A-Z]" str)
     (string-match "_" str)
     (string-match "\\`[A-Z][a-zA-Z0-9_]+\\'" str))))

(provide 'string-inflection)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-inflection.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-q C-u is SIMILAR to the keybinding used by Vz Editor.

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))


(global-set-key (kbd "C-c C-s s") 'my-string-inflection-cycle-auto)
(global-set-key (kbd "C-c C-s p") 'string-inflection-pascal-case-p)
(global-set-key (kbd "C-c C-s l") 'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c C-s k") 'string-inflection-kebab-case)
(global-set-key (kbd "C-c C-s c") 'string-inflection-camelcase)


;; string-inflection-ruby-style-cycle
;; string-inflection-elixir-style-cycle
;; string-inflection-python-style-cycle
;; string-inflection-java-style-cycle
;; string-inflection-all-cycle
;; string-inflection-toggle
;; string-inflection-camelcase
;; string-inflection-lower-camelcase
;; string-inflection-underscore
;; string-inflection-capital-underscore
;; string-inflection-upcase
;; string-inflection-kebab-casenn
;; string-inflection-insert
;; string-inflection-non-word-chars
;; string-inflection-get-current-word

(defun xah-cycle-hyphen-underscore-space ()
  "Cycle {underscore, space, hypen} chars of current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2016-07-17"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of -charArray.
  (let (-p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        ;; 2016-01-14 not use (bounds-of-thing-at-point 'symbol), because if at end of buffer, it returns nil. also, it's syntax table dependent
        (skip-chars-backward "-_[:alnum:]")
        (setq -p1 (point))
        (skip-chars-forward "-_[:alnum:]")
        (setq -p2 (point))))
    (let* ((-inputText (buffer-substring-no-properties -p1 -p2))
           (-charArray ["_" "-" " "])
           (-length (length -charArray))
           (-regionWasActive-p (region-active-p))
           (-nowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           (-changeTo (elt -charArray -nowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region -p1 -p2)
          (goto-char (point-min))
          (while
              (search-forward-regexp
               (elt -charArray (% (+ -nowState 2) -length))
               ;; (concat
               ;;  (elt -charArray (% (+ -nowState 1) -length))
               ;;  "\\|"
               ;;  (elt -charArray (% (+ -nowState 2) -length)))
               (point-max)
               'NOERROR)
            (replace-match -changeTo 'FIXEDCASE 'LITERAL))))
      (when (or (string= -changeTo " ") -regionWasActive-p)
        (goto-char -p2)
        (set-mark -p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ -nowState 1) -length)))))
