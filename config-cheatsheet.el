;;; ~/.doom.d/config-cheatsheet.el -*- lexical-binding: t; -*-

;; Quick reference for Ashton's modernized keybindings
;; Invoke with C-c ? or M-x ashton-cheatsheet

(defun ashton-cheatsheet ()
  "Display Ashton's keybinding cheat sheet."
  (interactive)
  (let ((buf (get-buffer-create "*Ashton Cheatsheet*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "ASHTON'S KEYBINDING CHEATSHEET\n" 'face '(:weight bold :height 1.3)))
        (insert (propertize "Press q to close\n\n" 'face 'font-lock-comment-face))

        ;; MOVEC Stack
        (insert (propertize "═══ MOVEC STACK (Minibuffer) ═══\n" 'face '(:foreground "cyan" :weight bold)))
        (insert "
C-.         embark-act          Context actions on ANYTHING (THE game changer)
C->         embark-dwim         Do what I mean
C-h B       embark-bindings     Show all embark bindings

C-x b       consult-buffer      Switch buffer with preview
M-y         consult-yank-pop    Kill ring with preview
M-g g       consult-goto-line   Goto line with preview

M-s r       consult-ripgrep     Project-wide search (rg)
M-s l       consult-line        Search lines in buffer
M-s L       consult-line-multi  Search across all buffers
M-s .       consult-line-thing  Search for thing at point
M-s i       consult-imenu       Jump to symbol
M-s g       consult-git-grep    Git grep

")
        ;; LLM Tools
        (insert (propertize "═══ LLM / AI ═══\n" 'face '(:foreground "magenta" :weight bold)))
        (insert "
C-c c c     claude-code         Start Claude in project
C-c c s     claude-send-command Send prompt to Claude
C-c c r     claude-send-region  Send selection to Claude
C-c c t     claude-toggle       Show/hide Claude window

C-c a       aidermacs-transient Aider menu (architect mode)

C-c m m     monet-mode          Toggle IDE integration
C-c m s     monet-send-selection
C-c m d     monet-send-diagnostics

")
        ;; Navigation
        (insert (propertize "═══ NAVIGATION ═══\n" 'face '(:foreground "green" :weight bold)))
        (insert "
C-x f       projectile-find-file   Find file in project
C-x C-f     find-file              Find any file
M-r         consult-recent-file    Recent files
M-m         orchard-cycle-mode     Cycle magit/claude

C-c g g     browse-at-remote       Open in browser
C-c g f     find-file-at-point     Goto file:line
C-c g t     toggle-impl/test       Switch test<->impl
C-c g l     avy-goto-line          Jump to line (avy)
C-c g c     avy-goto-char          Jump to char (avy)

")
        ;; Project Tools
        (insert (propertize "═══ PROJECT / GIT ═══\n" 'face '(:foreground "yellow" :weight bold)))
        (insert "
M-m         orchard-cycle-mode  Magit/Claude toggle
C-c C-g     search-project      Project search (fallback)
C-M-g       ripgrep-regexp      Ripgrep with regex

C-c G g     ghq-find-repo       Jump to ghq repo
C-c G w     ghq-worktree-create Create worktree + Claude
C-c G j     ghq-worktree-jump   Jump between worktrees

C-c C C     crewcapable-dashboard  Worktree dashboard

")
        ;; Editing
        (insert (propertize "═══ EDITING ═══\n" 'face '(:foreground "orange" :weight bold)))
        (insert "
C-;         comment-region      Toggle comment
C-'         expand-region       Expand selection
C-\"        contract-region     Contract selection
C-c q       query-replace       Search & replace

C-c i s     snake_case          String inflection
C-c i c     camelCase
C-c i k     kebab-case
C-c i u     UPCASE

")
        ;; Transient Menus (Casual)
        (insert (propertize "═══ TRANSIENT MENUS (press ? in mode) ═══\n" 'face '(:foreground "blue" :weight bold)))
        (insert "
?           casual-*-tmenu      In Dired, IBuffer, Info, Bookmarks, Calc
M-j         casual-avy          Avy jump menu
C-c t       python-pytest       Python test menu
C-c w ?     workspace-transient Workspace menu

")
        ;; Completion (Corfu + Cape)
        (insert (propertize "═══ COMPLETION (Corfu + Cape) ═══\n" 'face '(:foreground "purple" :weight bold)))
        (insert "
TAB/C-n/C-p Navigate completion popup (auto-shows after 2 chars)

C-c p p     completion-at-point Default completion
C-c p d     cape-dabbrev        Words from all buffers
C-c p f     cape-file           File paths (the one you wanted!)
C-c p l     cape-line           Complete whole lines
C-c p k     cape-keyword        Programming keywords
C-c p s     cape-elisp-symbol   Elisp symbols
C-c p h     cape-history        Minibuffer history
C-c p :     cape-emoji          Emoji :smile:

")
        ;; Services
        (insert (propertize "═══ SERVICES (Vercel/Supabase) ═══\n" 'face '(:foreground "cyan" :weight bold)))
        (insert "
C-c V       vercel-transient    Generic Vercel commands
C-c B       supabase-transient  Generic Supabase commands

C-c C s     crewcapable-services CrewCapable: Vercel + Supabase + Dev
C-c C d     crewcapable-dev      npm run dev
C-c C D     crewcapable-dev-full Supabase + dev

C-c A       aws-transient        AWS: S3, Lambda, CloudFormation, etc.
C-c A b     aws-bedrock-chat     Bedrock AI chat (Claude!)

")
        ;; Quick Reference
        (insert (propertize "═══ STOCK EMACS LAYERING ═══\n" 'face '(:foreground "gray" :weight bold)))
        (insert "
Same muscle memory works on remote servers:
  C-x b   → switch buffer (consult-buffer locally)
  M-g g   → goto line (consult-goto-line locally)
  M-y     → yank pop (consult-yank-pop locally)
  M-s *   → search prefix (consult-* locally)
  C-s     → isearch (unchanged, muscle memory safe)
")
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

;; Bind to C-c ?
(define-key ashton-mode-map (kbd "C-c ?") #'ashton-cheatsheet)

;; Also available via F1 (help key) prefix
(global-set-key (kbd "<f1> a") #'ashton-cheatsheet)

(provide 'config-cheatsheet)
;;; config-cheatsheet.el ends here
