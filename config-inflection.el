;;; ~/.doom.d/config-inflection.el -*- lexical-binding: t; -*-

;; String Inflection: Modern setup with transient menu and mode-aware cycling
;;
;; C-c i   = Inflection prefix
;; C-c i i = Smart cycle (mode-aware)
;; C-c i ? = Transient menu with all options
;;
;; Works on region if active, otherwise symbol at point

(require 'string-inflection)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Smart Mode-Aware Cycling
;;; ════════════════════════════════════════════════════════════════════════════

(defun inflection-cycle ()
  "Cycle through inflections appropriate for current major mode.
Python/Ruby: snake_case → CONST_CASE → PascalCase → snake_case
TypeScript/JS: camelCase → CONST_CASE → PascalCase → camelCase
Elisp/Lisp: kebab-case → CONST_CASE → snake_case → kebab-case
Default: all styles"
  (interactive)
  (cond
   ;; Python: snake_case, CONST_CASE, PascalCase
   ((derived-mode-p 'python-mode 'python-ts-mode)
    (string-inflection-python-style-cycle))
   ;; TypeScript/JavaScript: camelCase, CONST_CASE, PascalCase
   ((derived-mode-p 'typescript-mode 'typescript-ts-mode
                    'js-mode 'js-ts-mode 'tsx-ts-mode
                    'javascript-mode 'js2-mode)
    (string-inflection-java-style-cycle))
   ;; Elisp/Lisp: kebab-case focused
   ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'clojure-mode)
    (string-inflection-all-cycle))
   ;; Go: PascalCase, camelCase, snake_case
   ((derived-mode-p 'go-mode 'go-ts-mode)
    (string-inflection-all-cycle))
   ;; Default: cycle through all
   (t (string-inflection-all-cycle))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Preview Helper
;;; ════════════════════════════════════════════════════════════════════════════

(defun inflection--get-word ()
  "Get word at point or region for preview."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun inflection--preview-string ()
  "Generate preview string showing all inflections of current word."
  (let ((word (inflection--get-word)))
    (if word
        (format "Current: %s
  s) snake_case:    %s
  c) camelCase:     %s
  p) PascalCase:    %s
  u) CONST_CASE:    %s
  k) kebab-case:    %s
  _) Cycle _/-/spc: (cycles through separators)"
                word
                (string-inflection-underscore-function word)
                (string-inflection-camelcase-function word)
                (string-inflection-pascal-case-function word)
                (string-inflection-upcase-function word)
                (string-inflection-kebab-case-function word))
      "No word at point")))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Transient Menu
;;; ════════════════════════════════════════════════════════════════════════════

(with-eval-after-load 'transient
  (transient-define-prefix inflection-transient ()
    "String inflection commands with preview."
    [:description
     (lambda () (inflection--preview-string))
     ""]
    ["Convert to"
     ("s" "snake_case" string-inflection-underscore)
     ("c" "camelCase" string-inflection-lower-camelcase)
     ("p" "PascalCase" string-inflection-camelcase)
     ("u" "CONST_CASE" string-inflection-upcase)
     ("k" "kebab-case" string-inflection-kebab-case)
     ("_" "Cycle _/-/spc" xah-cycle-hyphen-underscore-space)]
    ["Cycle"
     ("i" "Smart cycle (mode-aware)" inflection-cycle)
     ("a" "All styles" string-inflection-all-cycle)]))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Separator Cycling (underscore/hyphen/space)
;;; ════════════════════════════════════════════════════════════════════════════

(defun xah-cycle-hyphen-underscore-space ()
  "Cycle {underscore, hyphen, space} in current word or region.
Repeated calls cycle through: _ → - → space → _"
  (interactive)
  (let (p1 p2)
    (if (use-region-p)
        (setq p1 (region-beginning) p2 (region-end))
      (save-excursion
        (skip-chars-backward "-_[:alnum:]")
        (setq p1 (point))
        (skip-chars-forward "-_[:alnum:]")
        (setq p2 (point))))
    (let* ((chars ["_" "-" " "])
           (len (length chars))
           (state (if (eq last-command this-command)
                      (get 'xah-cycle-hyphen-underscore-space 'state)
                    0))
           (new-char (aref chars state)))
      (save-excursion
        (save-restriction
          (narrow-to-region p1 p2)
          (goto-char (point-min))
          (while (search-forward-regexp (aref chars (% (+ state 2) len)) nil t)
            (replace-match new-char t t))))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (1+ state) len)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Duplicate Line/Region (was in old file)
;;; ════════════════════════════════════════════════════════════════════════════

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (_ (abs (or n 1)))
          (insert text))))
    (unless use-region
      (let ((pos (- (point) (line-beginning-position))))
        (forward-line 1)
        (forward-char pos)))))

;;; ════════════════════════════════════════════════════════════════════════════
;;; Keybindings (C-c i = Inflection)
;;; ════════════════════════════════════════════════════════════════════════════

;; Keybindings are set after ashton-mode-map is defined (in config-bindings.el)
;; Using global-set-key as fallback, will be overridden by ashton-mode-map later

(global-set-key (kbd "C-c i i") #'inflection-cycle)
(global-set-key (kbd "C-c i ?") #'inflection-transient)
(global-set-key (kbd "C-c i s") #'string-inflection-underscore)      ; snake_case
(global-set-key (kbd "C-c i c") #'string-inflection-lower-camelcase) ; camelCase
(global-set-key (kbd "C-c i p") #'string-inflection-camelcase)       ; PascalCase
(global-set-key (kbd "C-c i u") #'string-inflection-upcase)          ; CONST_CASE
(global-set-key (kbd "C-c i k") #'string-inflection-kebab-case)      ; kebab-case
(global-set-key (kbd "C-c i _") #'xah-cycle-hyphen-underscore-space) ; cycle separators
(global-set-key (kbd "C-c i U") #'upcase-region)
(global-set-key (kbd "C-c i l") #'downcase-region)
(global-set-key (kbd "C-c i C") #'capitalize-region)
(global-set-key (kbd "C-c d") #'duplicate-line-or-region)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Ace-window - HUGE centered labels
;;; ════════════════════════════════════════════════════════════════════════════

(after! ace-window
  ;; Use home row keys for window selection
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; Display in the center of the window with posframe for true centering
  (setq aw-display-mode-overlay t)

  ;; Make the leading char MASSIVE
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "white"
      :background "red"
      :weight ultra-bold
      :height 30.0
      :box (:line-width 20 :color "red"))))

;; For even bigger centered display, use ace-window's posframe if available
(use-package! ace-window
  :config
  ;; Try posframe for truly centered display
  (when (and (display-graphic-p)
             (require 'posframe nil t))
    (setq aw-char-position 'center)))

(provide 'config-inflection)
;;; config-inflection.el ends here
