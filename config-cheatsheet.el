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
C-c c ?     claude-last-question Jump to last question
C-c c a     claude-last-action  Jump to last action
C-c c S     claude-summary      Summary of recent activity

Workflow (C-c c w):
C-c c w t   test-plan           Generate test plan
C-c c w r   review              Review branch changes
C-c c w f   fix-error           Fix error (region/prompt)
C-c c w e   explain             Explain code
C-c c w c   commit              Suggest commit message

In Claude buffer:
C-c /       completion          Complete word (handles read-only)

C-c a       aidermacs-transient Aider menu (architect mode)

Monet IDE (C-c m) - requires ENABLE_IDE_INTEGRATION=t:
C-c m m     monet-mode          Toggle IDE integration
C-c m s     monet-start-server  Start WebSocket server
C-c m q     monet-stop-server   Stop server
C-c m ?     monet-status        Check connection status
C-c m S     monet-send-selection Send selection to Claude
C-c m d     monet-send-diagnostics Send errors to Claude
C-c m f     monet-fix-diagnostics  Send + ask Claude to fix
C-c m e     monet-explain-selection Send + explain
C-c m R     monet-refactor-selection Send + suggest refactoring

")
        ;; Navigation
        (insert (propertize "═══ NAVIGATION ═══\n" 'face '(:foreground "green" :weight bold)))
        (insert "
C-x f       projectile-find-file   Find file in project
C-x C-f     find-file              Find any file
M-r         consult-recent-file    Recent files
M-m         orchard-cycle-mode     Cycle magit/claude

C-c g ? shows transient menu with all these:

Code (xref/LSP):
C-c g d     xref-find-definitions  Go to definition
C-c g r     xref-find-references   Find all references
C-c g i     consult-imenu          Jump to symbol
C-c g I     consult-imenu-multi    Symbol across project
C-c g e     consult-flycheck       Jump to error (with preview!)
C-c g t     toggle-impl/test       Switch test<->impl

Visual (avy):
C-c g l     avy-goto-line          Jump to visible line
C-c g c     avy-goto-char-timer    Type chars, jump to match
C-c g w     avy-goto-word-1        Jump to word

External:
C-c g g     browse-at-remote       Open in GitHub/GitLab
C-c g b     browse-url-at-point    Open URL under cursor
C-c g f     find-file-at-point     Goto file:line
C-c g s     firefox-search         Search region/symbol (Firefox)

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
C-c d       duplicate           Duplicate line/region

Indent (works on region or line, uses mode indent: 4 for Python, etc):
C-c <       indent-left         Then just < or > to keep adjusting!
C-c >       indent-right        Any other key exits repeat mode

String inflection (C-c i ? shows transient with preview):
C-c i i     smart-cycle         Mode-aware cycling (recommended!)
C-c i s     snake_case
C-c i c     camelCase
C-c i p     PascalCase
C-c i u     CONST_CASE
C-c i k     kebab-case
C-c i _     cycle-separator     Cycle _/-/space
C-c i U/l/C upcase/down/cap     Region case

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
        ;; Slack
        (insert (propertize "═══ SLACK (C-c K prefix) ═══\n" 'face '(:foreground "magenta" :weight bold)))
        (insert "
C-c K K     slack-open          Open Slack (select channel)
C-c K s     slack-start         Start Slack connection
C-c K c     slack-channel       Select channel
C-c K d     slack-im            Direct message
C-c K u     slack-unread        Unread channels
C-c K t     slack-thread        Show thread
C-c K r     slack-reply         Reply in thread
C-c K @     slack-mentions      All mentions
C-c K /     slack-search        Search messages
C-c K ?     slack-transient     Full menu

")
        ;; Meeting Notes
        (insert (propertize "═══ MEETING NOTES (C-c M prefix) ═══\n" 'face '(:foreground "yellow" :weight bold)))
        (insert "
Generic (works for any meeting type):
C-c M o     meeting-open        Open a specific occurrence
C-c M t     meeting-open-today  Open/create today's notes
C-c M n     meeting-add-to-next Add item to next occurrence
C-c M c     meeting-carryover   Copy items from previous
C-c M e     meeting-export      Export to clipboard
C-c M ?     meeting-transient   Full menu

Standup shortcuts (C-c M s prefix):
C-c M s s   standup             Open today's standup
C-c M s d   done                Add to Done
C-c M s g   doing               Add to Doing
C-c M s b   blocker             Add to Blockers
C-c M s q   question            Add to Questions
C-c M s a   agenda              Add to Agenda (discussion topics)
C-c M s c   carryover           Carry over Doing from yesterday
C-c M s e   export              Export standup
C-c M s p   post-slack          Post standup to Slack

Meeting types: standup, psc-it-sync, psc-pm-sync
Files: ~/org/meetings/<type>/<date>.org

")
        ;; Calendar
        (insert (propertize "═══ CALENDAR (C-c @ prefix) ═══\n" 'face '(:foreground "cyan" :weight bold)))
        (insert "
C-c @ a     calendar-show-agenda   Show agenda (7 days)
C-c @ t     calendar-show-today    Today's events
C-c @ n     calendar-next-meeting  Next meeting
C-c @ w     calendar-show-week     This week
C-c @ ?     calendar-transient     Menu

Setup: gcalcli init (for Google calendars)

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
        ;; Feature Workflow
        (insert (propertize "═══ FEATURE WORKFLOW (Branch → PR) ═══\n" 'face '(:foreground "orange" :weight bold)))
        (insert "
1. CREATE BRANCH (uppercase, TAB-completable prefixes)
   C-c O F         FEATURE/name           New feature
   C-c O B         BUGFIX/name            Bug fix
   C-c O C         CHORE/name             Maintenance
   C-c O R         REFACTOR/name          Code improvement
   C-c O D         DOCS/name              Documentation
   C-c O E         EXPERIMENT/name        Spike/experiment
   C-c O O         orchard-dashboard      See all worktrees

   Structure: ~/src/project/FEATURE-name/ (nested)

2. DEVELOP (cycle between tools)
   M-m             orchard-cycle-mode     Toggle: magit ↔ claude ↔ compile
   C-c c c         claude-code            Start Claude explicitly
   `               commando               Run project commands (make dev, etc)

3. CREATE TEST PLAN (in Claude)
   /test-plan      Claude command         Generates .test-plan.md

4. EXECUTE TESTS
   C-c T           testicular-start       Step through test plan
   P/f/S           pass/fail/skip         Mark each test
   s               screenshot             Attach evidence (clipboard)
   RET             finish                 Export results, create PR

5. CREATE PR (from magit P menu)
   P r             pr-create              Create PR (prompts for test plan)
   P A             pr-create-ai           AI-generated PR description
   P v             pr-view                View existing PR

6. CLEANUP
   C-c O a         orchard-archive        Remove worktree, keep branch
   C-c O D         orchard-delete         Remove worktree AND branch
   C-c O -         orchard-hide           Hide from dashboard (dismiss)

")
        ;; Combobulate (structural editing)
        (insert (propertize "═══ COMBOBULATE - Structural Editing ═══\n" 'face '(:foreground "green" :weight bold)))
        (insert "
Tree-sitter powered navigation/manipulation (works in *-ts-mode buffers)

Navigation (standard Emacs keys):
C-M-u       navigate-up          Move to parent node
C-M-d       navigate-down        Move to first child
C-M-n       navigate-next        Move to next sibling
C-M-p       navigate-previous    Move to previous sibling
C-M-a       beginning-of-defun   Start of function
C-M-e       end-of-defun         End of function

Manipulation:
M-h         mark-node            Mark current node
M-k         kill-node            Kill current node
M-N         drag-down            Drag node down
M-P         drag-up              Drag node up
C-M-t       transpose            Swap with sibling

Menu:
C-c o o     combobulate          Main menu (all commands)
C-c o c     clone-node           Clone/duplicate node

")
        ;; Tree-sitter
        (insert (propertize "═══ TREE-SITTER ═══\n" 'face '(:foreground "cyan" :weight bold)))
        (insert "
Auto-enabled for: Python, TypeScript, JavaScript, TSX, JSON, YAML, CSS, Go, Bash

M-x treesit-install-all-grammars   Install all grammars
M-x treesit-check-grammars         Show installed/missing grammars

Benefits: Better syntax highlighting, Combobulate structural editing,
          faster parsing, consistent indentation

")
        ;; Wgrep + consult extras
        (insert (propertize "═══ WGREP + CONSULT EXTRAS ═══\n" 'face '(:foreground "yellow" :weight bold)))
        (insert "
Wgrep - Edit grep results directly:
  M-s r       consult-ripgrep      Search project
  C-. E       embark-export        Export to grep buffer
  C-c C-p     wgrep-mode           Make buffer editable
  (edit)      ...                  Change text in results
  C-c C-c     wgrep-finish         Apply changes to all files!
  C-c C-k     wgrep-abort          Discard changes

Consult extras:
  M-g f       consult-flycheck     Jump to errors with preview
  C-x C-d     consult-dir          Switch directory (global)
  C-x C-d     consult-dir          Insert directory (in minibuffer)
  C-x C-j     consult-dir-jump     Jump to file in dir (minibuffer)

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
