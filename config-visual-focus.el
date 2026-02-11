;;; config-visual-focus.el -*- lexical-binding: t; -*-
;;; Visual focus: Make active window/cursor/line obviously visible
;;;
;;; Packages:
;;;   - beacon: Flash cursor on scroll/window switch
;;;   - winpulse: Pulse window background on switch
;;;   - hl-line: Built-in line highlighting (enhanced)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Beacon - Cursor flash on movement
;;; ════════════════════════════════════════════════════════════════════════════

(require 'beacon)

(setq beacon-blink-when-window-scrolls t   ; Flash on scroll
      beacon-blink-when-window-changes t   ; Flash on window switch
      beacon-blink-when-buffer-changes t   ; Flash on buffer switch
      beacon-blink-when-focused t          ; Flash when Emacs gains focus
      beacon-blink-duration 0.4            ; Duration in seconds (longer = more visible)
      beacon-blink-delay 0.1               ; Delay before blink
      beacon-size 80                       ; Size of the beacon (bigger = more visible)
      beacon-color "#ff4400")              ; Bright orange-red

;; Don't blink in these modes (performance/annoyance)
(setq beacon-dont-blink-major-modes
      '(vterm-mode term-mode shell-mode eshell-mode))

(beacon-mode 1)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Winpulse - Window background flash on switch
;;; ════════════════════════════════════════════════════════════════════════════

(require 'winpulse)

(setq winpulse-brightness 60              ; How much to shift color (0-255), higher = more obvious
      winpulse-duration 0.5               ; Total animation duration in seconds
      winpulse-step-interval 0.03)        ; Seconds between frames

(winpulse-mode 1)

;;; ════════════════════════════════════════════════════════════════════════════
;;; hl-line - Current line highlighting (enhanced)
;;; ════════════════════════════════════════════════════════════════════════════

;; Only highlight in the active window
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

;; Make the highlight more prominent - bright enough to notice
(custom-set-faces!
  '(hl-line :background "#2d4a5a" :extend t))  ; Brighter blue-teal

;; Ensure global-hl-line-mode is on
(global-hl-line-mode 1)

;;; ════════════════════════════════════════════════════════════════════════════
;;; Dim inactive windows
;;; ════════════════════════════════════════════════════════════════════════════

;; Make inactive window modelines obviously dimmer
(custom-set-faces!
  '(mode-line-inactive :background "#151515" :foreground "#444444"))

(provide 'config-visual-focus)
;;; config-visual-focus.el ends here
