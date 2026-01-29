# Doom Emacs Configuration - LLM Reference

## Overview

This is **Ashton Honnecke's** heavily customized Doom Emacs configuration. It's a **non-vi** setup (no evil-mode bindings) optimized for full-stack development with extensive custom tooling.

**Owner**: Ashton Honnecke <ashton@pixelstub.com>

---

## Directory Structure

```
~/.doom.d/
├── init.el                    # Doom module declarations
├── config.el                  # Main config (loads all config-*.el modules)
├── packages.el                # Additional package declarations
├── custom.el                  # Emacs custom-set-variables
├── config-*.el                # Modular configuration files (70+)
├── snippets/                  # YASnippet templates
└── *.el                       # Utility/legacy elisp files
```

---

## Core Architecture

### Module Loading Pattern

All configuration is modular via `config-*.el` files loaded in `config.el`:

```elisp
(load "~/.doom.d/config-<feature>")
```

The load order matters - `config-bindings.el` loads **last** to ensure keybindings take precedence.

### Custom Minor Mode: `ashton-mode`

A global minor mode (`ashton-mode`) provides a unified keymap that overrides mode-specific bindings:

- Defined in: `config-ash-mode.el` (keymap) and `config-bindings.el` (mode + bindings)
- Keymap: `ashton-mode-map`
- Enabled globally on startup

---

## Key Technologies & Languages

| Language | LSP | Formatter | Linter | Test Runner |
|----------|-----|-----------|--------|-------------|
| Python | eglot/lsp | ruff, black | ruff, flycheck | pytest |
| TypeScript/JS | lsp | biome | biome | vitest |
| Go | lsp | gofmt | - | - |
| YAML | lsp | - | - | - |
| SQL | - | - | - | - |

---

## Critical Keybindings

### Global (via ashton-mode-map)

| Binding | Function | Description |
|---------|----------|-------------|
| `M-m` | `orchard-cycle-mode` | Cycle magit/claude/compile |
| `C-x f` | `projectile-find-file` | Find file in project |
| `C-;` | `comment-or-uncomment-region` | Toggle comments |
| `C-'` | `er/expand-region` | Expand selection |
| `C-"` | `er/contract-region` | Contract selection |
| `C-c v t` | `+vterm/toggle` | Toggle terminal |
| `C-c t` | `python-pytest-dispatch` | Python test dispatch |
| `C-c a` | `aidermacs-transient` | Aider AI assistant |

### Navigation (C-c g prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c g d` | `xref-find-definitions` | Go to definition |
| `C-c g r` | `xref-find-references` | Find all references |
| `C-c g i` | `consult-imenu` | Jump to symbol |
| `C-c g e` | `consult-flycheck` | Jump to error |
| `C-c g t` | `projectile-toggle-impl/test` | Toggle impl/test file |
| `C-c g l` | `avy-goto-line` | Jump to visible line |
| `C-c g c` | `avy-goto-char-timer` | Type chars, jump |
| `C-c g g` | `+vc/browse-at-remote` | Open in GitHub |
| `C-c g f` | `find-file-at-point-with-line` | Goto file:line |
| `C-c g ?` | `goto-transient` | Navigation menu |

### String Inflection (C-c i prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c i i` | `inflection-cycle` | Smart mode-aware cycle |
| `C-c i ?` | `inflection-transient` | Menu with preview |
| `C-c i s` | snake_case | |
| `C-c i c` | camelCase | |
| `C-c i p` | PascalCase | |
| `C-c i u` | CONST_CASE | |
| `C-c i k` | kebab-case | |
| `C-c i _` | cycle separator | underscore/hyphen/space |

### Meeting Notes (C-c M prefix)

Generalized meeting system - standup is one meeting type. Per-occurrence files in `~/org/meetings/<type>/<date>.org`.

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c M o` | `meeting-open` | Open any occurrence |
| `C-c M t` | `meeting-open-today` | Open today's notes |
| `C-c M n` | `meeting-add-to-next` | Add to next meeting |
| `C-c M c` | `meeting-carryover` | Copy from previous |
| `C-c M e` | `meeting-export` | Export to clipboard |
| `C-c M ?` | `meeting-transient` | Full menu |

### Standup Shortcuts (C-c M s prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c M s s` | `meeting-standup` | Open today's standup |
| `C-c M s d` | Done | Add to Done section |
| `C-c M s g` | Doing | Add to Doing section |
| `C-c M s b` | Blocker | Add to Blockers |
| `C-c M s q` | Question | Add to Questions |
| `C-c M s a` | Agenda | Add to Agenda |
| `C-c M s p` | Post Slack | Post standup to Slack |

Meeting types: `standup`, `psc-it-sync`, `psc-pm-sync`

### Org-Mode Specific

| Binding | Function |
|---------|----------|
| `M-, j/;/k/l` | org-metaleft/right/down/up |
| `M-, J/:/ K/L` | org-shiftmeta variants |
| `M-<return>` | org-insert-heading |
| `C-c x h` | Export to HTML |

---

## Important Files

### Startup Behavior

- Opens **Orchard dashboard** on startup (worktree manager)
- `initial-buffer-choice` set to `#'orchard` in `config.el`

### Org Directory

```elisp
(setq org-directory "~/org/")
```

### Auth/Credentials

```elisp
(setq auth-sources '("~/.authinfo"))
```

**Setup**: `~/.netrc` is symlinked to `~/.authinfo` so both Emacs auth-source (TRAMP, forge) and ange-ftp share the same credential file. CLI tools (curl, ftp) also benefit.

```bash
ln -s ~/.authinfo ~/.netrc
```

**Format** (one entry per line):
```
machine ftp.example.com login user password secret
machine api.example.com login user@email.com password token123
```

---

## Magit Customizations

Located in `config-magit.el`:

- `magit-branch-alter-name` - Rename branch with prefill
- `magit-fetch-create-clean-branch` - Create branch from upstream main
- `magit-flatten` - Flatten branch to single commit
- Custom EdDiff merge function (key 'd')
- Forge integration for PRs

---

## AI/LLM Integration

| Package | Config File | Purpose |
|---------|-------------|---------|
| claude-code.el | `config-claude.el` | Claude CLI integration (primary) |
| aider | `config-aider.el` | AI pair programming |
| gptel | `config-gptel.el` | Chat-style LLM (Ollama) |
| llm | `config-llm.el` | Region-based LLM ops |

### Claude Code (C-c c prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c c c` | `claude-code-start` | Start Claude in current project |
| `C-c c s` | `claude-code-send-command` | Send prompt to Claude |
| `C-c c r` | `claude-code-send-region` | Send selected region |
| `C-c c b` | `claude-code-send-buffer` | Send current buffer |
| `C-c c t` | `claude-code-toggle-buffer` | Show/hide Claude window |
| `C-c c m` | `claude-code-transient-menu` | All commands menu |
| `C-c c y` | `claude-code-yes` | Answer "yes" to prompt |
| `C-c c n` | `claude-code-no` | Answer "no" to prompt |
| `C-c c k` | `claude-code-kill` | Kill Claude instance |
| `C-c c z` / `s-z` | `claude-code-toggle-read-only-mode` | Toggle copy mode |

**Modeline indicator**: Shows Claude status globally: `[Claude: 2🟢]` (running count)

**Note**: Claude status tracking is simplified to only check if the process is alive (no pattern matching). PR URLs are automatically captured when `gh pr create` is run.

---

## Orchard - Issue-Centric Worktree Manager (C-c O prefix)

Opens on startup. Entry point: `config-orchard.el` (thin shim that loads modular system).

### Module Architecture (Jan 2025 refactor)

The orchard system was split from a 4375-line monolith into 8 focused modules:

| Module | Lines | Purpose |
|--------|-------|---------|
| `orchard-vars.el` | 211 | Variables, customization (`defcustom`), faces, TTLs |
| `orchard-cache.el` | 502 | Claude sessions, merged branches, GitHub issues, state |
| `orchard-worktree.el` | 347 | Worktree data, orphan detection, stage detection |
| `orchard-window.el` | 247 | Column management, window locking |
| `orchard-claude.el` | 383 | Claude status tracking, buffer/session management |
| `orchard-dashboard.el` | 924 | Major mode, formatting, navigation, views |
| `orchard-actions.el` | 1102 | Commands: push/PR/archive/delete, branch creation |
| `orchard.el` | 702 | Main entry point, transient menu, keybindings |

**Restore point**: `git checkout orchard-modular-v1.0`

Load order: `config-orchard.el` → `orchard.el` → all sub-modules via `require`.

### Workflow

```
Issue → Branch → Claude → PR → Merge → QA/Verify → Close Issue → Archive
```

### Dashboard Layout

Issue-centric layout with lifecycle sections:

```
🌳 Orchard  📋 43 issues  🔔 2 Claude NEED ATTENTION  ⏮ 3 from last session
View: working (f for filter menu)

▼ ⏮ PREVIOUSLY ACTIVE (3) ────────────────────────────
  📋 #713 Email body parsing            (from last session - X to clear)

▼ 🔔 CLAUDE WAITING (2) ──────────────────────────────
  📋 #640 Make a "ship to" catalog...   ⏳WAIT

▼ ⚡ CURRENT (5) ─────────────────────────────────────
  📋 #720 New issue created today       [bug]

▼ 🚧 IN FLIGHT (12) ──────────────────────────────────
  📋 #640 Make a "ship to" catalog...   ⏳WAIT PR
     ↳ FEATURE/640-make-a-ship-to   ○ ↓2 :3007

▼ QA/VERIFY (8 merged, issue open) ────────────────────
  📋 #372 staging label not applied     ✓Merged

▼ DONE (ready to archive) ─────────────────────────────
  📋 #596 Commit date didn't push       ✓Merged ✓Closed
```

### Lifecycle Sections

| Section | Description |
|---------|-------------|
| **PREVIOUSLY ACTIVE** | Sessions from last Emacs (shown on restart, X to clear) |
| **CLAUDE WAITING** | Claude sessions waiting for your input |
| **CURRENT** | Issues active today (created/updated) |
| **IN PROGRESS** | Issues with active worktrees |
| **QA/VERIFY** | PR merged but issue still open (awaiting verification) |
| **DONE** | Closed issues ready to archive with `M` |
| **UNLINKED** | Worktrees without linked issues |

### View Presets

| Binding | View | Shows |
|---------|------|-------|
| `v w` | Working (default) | UP NEXT + IN PROGRESS |
| `v a` | All | All sections |
| `v n` | Next | UP NEXT only |
| `v p` | Progress | IN PROGRESS only |
| `v q` | QA | QA/VERIFY only |

### Dashboard Keybindings

| Binding | Function | Description |
|---------|----------|-------------|
| `RET` | Open Claude | For issue/worktree at point |
| `I` | Start from issue | Creates worktree linked to GitHub issue |
| `m` | Open magit | For worktree at point |
| `c` | Open Claude | Same as RET |
| `d` | Open dired | For worktree at point |
| `P` | Create PR | With auto-populated title/body |
| `u` | Push branch | To origin |
| `X` | Clear previous | Dismiss "PREVIOUSLY ACTIVE" section |
| `W` | Tile Claudes | Show up to 4 Claude windows in 2x2 grid |
| `l` | List Claudes | Select from all Claude sessions |

### Filtering & Views

| Binding | Function | Description |
|---------|----------|-------------|
| `f` | Filter menu | Open transient with all options |
| `v w/a/n/p/q` | View presets | Quick view switching |
| `/` | Filter by label | Select label to filter |
| `\\` | Clear filter | Remove label filter |
| `s` | Toggle staging | Show/hide staging-labeled issues |
| `-` | Hide at point | Hide issue/branch from view |
| `H` | Show hidden | Unhide previously hidden items |
| `TAB` | Toggle section | Collapse/expand section |

### Cleanup & Refresh

| Binding | Function | Description |
|---------|----------|-------------|
| `g` | Refresh | Quick refresh (cached data) |
| `G` | Force refresh | Fetch fresh from GitHub |
| `M` | Archive done | Archive DONE section items |
| `K` | Cleanup stale | Remove orphan worktrees |

### Port Allocation

Ports are allocated lazily - only when needed for `make dev`:
- Press `+` to allocate a port (max 10 worktrees)
- Stale ports auto-cleaned on refresh
- Deleting worktree frees the port

### Session Persistence

When Emacs exits, active Claude session paths are saved to `~/.orchard-claude-sessions.eld`.

On next startup:
- Dashboard shows **PREVIOUSLY ACTIVE** section with sessions from last time
- Header shows `⏮ N from last session` indicator
- Press `X` to dismiss the section (clears the saved file)
- Sessions remain visible until dismissed so you can resume work

This helps answer "what was I working on?" after restarting Emacs.

---

## Slack Integration (C-c K prefix)

Located in `config-slack.el`:

Full Slack client via emacs-slack. Requires token/cookie authentication.

### Setup

```bash
# Run token extraction helper
~/bin/slack-token-refresh.sh --team TEAMNAME --email you@example.com

# Or set up cron for automatic refresh (tokens expire periodically)
0 8 * * * ~/bin/slack-token-refresh.sh --cron --team crewcapable --email ashton@crewcapable.ai
```

Then configure team in `config-slack.el`:
```elisp
(slack-register-team
 :name "crewcapable"
 :default t
 :token (slack--get-auth "crewcapable.slack.com" "ashton@crewcapable.ai")
 :cookie (slack--get-auth "crewcapable.slack.com" "ashton@crewcapable.ai^cookie")
 :subscribed-channels '(general engineering))
```

### Keybindings

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c K K` | `slack-open` | Open Slack (start + select channel) |
| `C-c K s` | `slack-start` | Start Slack connection |
| `C-c K c` | `slack-channel-select` | Switch channel |
| `C-c K d` | `slack-im-select` | Direct message |
| `C-c K u` | `slack-select-unread-rooms` | Unread channels |
| `C-c K t` | `slack-thread-show-or-create` | Show thread |
| `C-c K r` | `slack-message-reply-to` | Reply in thread |
| `C-c K e` | `slack-message-edit` | Edit message |
| `C-c K R` | `slack-message-add-reaction` | Add reaction |
| `C-c K /` | `slack-search-from-messages` | Search messages |
| `C-c K @` | `slack-all-mentions` | All mentions |
| `C-c K l` | `slack-copy-link-at-point` | Copy message link |
| `C-c K ?` | `slack-transient` | Full command menu |

### Meeting Integration

From any buffer, `C-c M s p` posts today's standup to Slack.

---

## GHQ + Worktree Management (NEW)

Located in `config-ghq.el`:

Modern repo management using ghq for organization and git worktrees for parallel branch work.
Replaces the old numbered workspace pattern with on-demand, feature-named worktrees.

### Setup

```bash
# Install ghq
/usr/local/go/bin/go install github.com/x-motemen/ghq@latest

# Add to PATH
export PATH="$HOME/go/bin:$PATH"

# Configure root
git config --global ghq.root ~/ghq
```

### Keybindings (C-c G prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c G g` | `ghq-find-repo` | Jump to any ghq-managed repo |
| `C-c G c` | `ghq-clone` | Clone repo via ghq |
| `C-c G w` | `ghq-worktree-create` | Create worktree + start Claude |
| `C-c G l` | `ghq-worktree-list` | List worktrees for current repo |
| `C-c G j` | `ghq-worktree-jump` | Jump between worktrees |
| `C-c G d` | `ghq-worktree-remove` | Remove worktree |
| `C-c G ?` | `ghq-transient` | Command menu |

### Worktree Workflow

1. `C-c G g` - Jump to main repo
2. `C-c G w` - Create worktree for feature (auto-allocates port, generates .env.workspace)
3. Work on feature with Claude (`C-c c c` to start)
4. `C-c G j` - Jump between worktrees
5. `C-c G d` - Remove worktree after merge

### Port Allocation

Worktrees get automatic port slots (0-9) stored in `~/.ghq-worktree-ports`.
Each slot offsets ports by 100 (e.g., slot 2 = port 3002, 4766, etc.).
The `.env.workspace` file is auto-generated with correct port mappings.

---

## CrewCapable Mode (Project-Specific)

Located in `config-crewcapable.el`:

Focused layer for crewcapableai development with easy access to worktrees, Claude instances, and workflow commands.

### Quick Access: `C-c C` prefix

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c C C` | `crewcapable-dashboard` | **Open dashboard** (list all worktrees) |
| `C-c C f` | `crewcapable-new-feature` | New FEAT/name branch |
| `C-c C x` | `crewcapable-new-fix` | New FIX/name branch |
| `C-c C h` | `crewcapable-new-chore` | New CHORE/name branch |
| `C-c C m` | `crewcapable-switch-magit` | Select worktree → open magit |
| `C-c C c` | `crewcapable-switch-claude` | Select worktree → open Claude |
| `C-c C l` | `crewcapable-list-claudes` | List all active Claude instances |
| `C-c C t` | `workspace-test-evidence` | Open test evidence buffer |
| `C-c C ?` | `crewcapable-transient` | Show all commands |

### Dashboard Keybindings (in *CrewCapable* buffer)

| Binding | Function |
|---------|----------|
| `RET` | Open magit for worktree at point |
| `c` | Open Claude for worktree at point |
| `d` | Open dired for worktree at point |
| `t` | Open test evidence for worktree |
| `f` | Create new feature branch |
| `x` | Create new fix branch |
| `h` | Create new chore branch |
| `p` | Push branch at point |
| `D` | Delete worktree at point |
| `l` | List all Claude instances |
| `r/g` | Refresh |
| `q` | Quit |

### Typical Workflow

1. `C-c C C` - Open dashboard to see all worktrees
2. `C-c C f` - Create new feature branch (auto-allocates port, starts Claude)
3. Work with Claude, run `make dev` for local services
4. `C-c C t` - Add test evidence with screenshots
5. `C-c C m` - Switch between worktrees via magit
6. `p` in dashboard - Push branch when ready
7. `D` in dashboard - Delete worktree after merge

---

## Workspace Manager (LEGACY)

Located in `config-workspace.el`:

**Note:** Being replaced by GHQ + Worktree system above.

Manages parallel `crewcapableai.*` workspaces, integrating with `ws.py` utility.

### Global Keybindings (C-c w prefix)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c w w` | `workspace-show` | Open dashboard buffer |
| `C-c w n` | `workspace-new-feature` | Create feature branch + start Claude |
| `C-c w s` | `workspace-sync` | Fetch upstream in all workspaces |
| `C-c w p` | `workspace-pull` | Pull/rebase current workspace |
| `C-c w P` | `workspace-pull-all` | Pull/rebase all workspaces |
| `C-c w c` | `workspace-clean` | Show cleanable workspaces (dry-run) |
| `C-c w C` | `workspace-clean-execute` | Clean workspaces |
| `C-c w j` | `workspace-jump` | Jump to workspace N |
| `C-c w m` | `workspace-magit` | Open magit in workspace N |
| `C-c w ?` | `workspace-transient` | Show command menu |

### Dashboard Buffer Keybindings

| Binding | Function |
|---------|----------|
| `RET` | Open magit for workspace at point |
| `d` | Open dired for workspace at point |
| `t` | Open terminal in workspace |
| `c` | Open Claude in workspace |
| `e` | Open test evidence buffer |
| `n` | New feature branch |
| `p` | Pull current |
| `P` | Pull all |
| `s` | Sync all |
| `r/g` | Refresh |
| `q` | Quit |
| `?` | Transient menu |

### Test Evidence Buffer

Track test items with attached Fireshot screenshots for PR descriptions.

**Open with**: `C-c w e` (from workspace) or `e` (from dashboard)

| Binding | Function |
|---------|----------|
| `a` | Add new test item |
| `x` | Toggle item complete |
| `s` | Attach screenshot from clipboard (xclip) |
| `S` | Attach screenshot from file picker |
| `v` | View screenshots for item |
| `e` | Export to markdown (copies to clipboard) |
| `d` | Delete item |
| `r/g` | Refresh |
| `q` | Quit |
| `?` | Transient menu |

**Data storage**: `<workspace>/.test-evidence/<rev>/evidence.json`
**Screenshots**: `<workspace>/.test-evidence/<rev>/screenshots/`

**Workflow**:
1. Use Fireshot to capture screenshot → copies to clipboard
2. In evidence buffer, position on test item
3. Press `s` to attach screenshot from clipboard
4. Press `e` to export markdown for PR description

### Configuration

```elisp
(setq workspace-use-external-terminal nil)  ; Use vterm (default) or gnome-terminal
(setq workspace-external-terminal "gnome-terminal")  ; If using external
(setq workspace-screenshot-dir "~/screenshots")  ; Root for file picker default
```

---

## Combobulate (Structural Editing)

Located in `config-combobulate.el`:

- **Source**: `/home/ahonnecke/src/combobulate`
- **Enabled for**: python-ts-mode, js-ts-mode, typescript-ts-mode, tsx-ts-mode, json-ts-mode, yaml-ts-mode, css-ts-mode

Tree-sitter powered structural editing - navigate and manipulate code by AST nodes.

### Keybindings (standard Emacs keys)

| Binding | Function | Description |
|---------|----------|-------------|
| `C-M-u` | `combobulate-navigate-up` | Parent node |
| `C-M-d` | `combobulate-navigate-down` | First child |
| `C-M-n` | `combobulate-navigate-next` | Next sibling |
| `C-M-p` | `combobulate-navigate-previous` | Previous sibling |
| `M-h` | `combobulate-mark-node-dwim` | Mark node |
| `M-k` | `combobulate-kill-node-dwim` | Kill node |
| `M-N` / `M-P` | drag down/up | Move node |
| `C-c o o` | `combobulate` | Main menu |

---

## Completion System

### Minibuffer Completion (Vertico)

- **Vertico** with `+childframe` flag for centered popup (via vertico-posframe)
- **Note**: vertico-multiform is disabled to prevent conflict with posframe
- Configured in `config.el` and `config-consult-embark.el`

### In-Buffer Completion (Corfu)

Located in `config-corfu.el`:

- **Corfu** for in-buffer completion popup
- **Cape** for completion-at-point extensions

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c p p` | `completion-at-point` | Default completion |
| `C-c p d` | `cape-dabbrev` | Words from buffers |
| `C-c p f` | `cape-file` | File paths |
| `C-c p l` | `cape-line` | Complete lines |
| `C-c p k` | `cape-keyword` | Programming keywords |

---

## Projectile Configuration

Located in `config-projectile.el`:

- Project root: `~/src/`
- Indexing: native
- Ignored: `.cache`, `node_modules`, `uv.lock`
- `M-o` - Switch project

---

## Tree-sitter

Enabled for: Python, TypeScript, YAML, Go, JSON, and more.
Config files: `config-tree-sitter.el`, `config-ts-*.el`

---

## VTerm Configuration

- 10000 line scrollback
- Toggle: `C-M-t` or `C-c v t`
- Here: `C-c v h`

---

## File Patterns

When creating new configuration:

1. Create `config-<feature>.el` with lexical-binding header
2. Add `(load "~/.doom.d/config-<feature>")` to `config.el`
3. If keybindings needed, add to `ashton-mode-map` in new file or `config-bindings.el`

### Standard File Header

```elisp
;;; ~/.doom.d/config-<feature>.el -*- lexical-binding: t; -*-
```

---

## External Tool Integration

- **Firefox**: `firefox-search-region` (config-firefox.el)
- **GitLab**: Screenshot upload (config-gitlab-upload.el)

---

## Notes for LLM Assistance

1. **No evil-mode** - Don't suggest vi keybindings
2. **Modular config** - New features should be separate `config-*.el` files
3. **ashton-mode-map** - Use this for global keybindings
4. **Prefer upstream** - Use upstream git remote for operations
5. **Linux first** - Container/linux solutions preferred
6. **Don't apologize** - Be direct, ask expert-level questions

---

## Archived/Deleted Configurations (Dec 2025)

The following files were removed during cleanup. They are preserved in git history.

### Completion Experiments (Future Work)

These files attempted clever completion features that never fully worked:

| File | Goal | Why Removed |
|------|------|-------------|
| `config-company-sql.el` | PostgreSQL schema completion via live DB queries | Entirely commented out, complex SQL introspection for table/column names |
| `config-company-dict.el` | Dictionary-based completion with custom dict files | Entirely commented out, was trying to add domain-specific completions |
| `config-vterm-completion.el` | Tab completion in vterm using bash compgen + history | Broke vterm completion, used compgen -c/-f + bash_history |

**Future goal**: Implement smart completion that combines:
- Project symbols (current)
- LSP completions (current)
- Database schema introspection (PostgreSQL)
- Custom domain dictionaries
- Shell command/history completion in vterm

### Security Risks (Contained Hardcoded Credentials)

| File | Issue |
|------|-------|
| `config-ejira.el` | JIRA token exposed - full jiralib2 implementation |
| `config-slack.el` | Slack token and cookie exposed |

### Dead/Unused Code

| File | What It Was |
|------|-------------|
| `config-lmap.el` | Abandoned C-l prefix keymap experiment (had syntax errors) |
| `config-rotate-text.el` | Text rotation (true/false, yes/no toggling) |
| `config-ox-confluence.el` | Org export to Confluence wiki format |
| `config-sql.el` | SQL mode config with multiple false starts |
| `config-ruff-lsp.el` | Old ruff-as-LSP attempts (superseded by config-ruff.el) |
| `config-flycheck-ruff.el` | Duplicate ruff flycheck (redundant) |
| `config-projectile-ripgrep.el` | Old projectile-ripgrep setup (entirely commented) |
| `custominit.el` | Old backup init file |
| `stockinit.el` | Old backup init file |
| `magit-flatten.el` | Standalone flatten (now in config-magit.el) |
| `sqlup-mode.el` | SQL uppercase mode package |
| `string_underscore.el` | String underscore utilities |
| `smart-tab.el` | Smart tab completion |
| `init-python-coverage.el` | Python test coverage |
| `z-last.el` | Unknown purpose |
| `config-python.el` | Old python-mode config (replaced by config-ts-python.el) |
| `config-ellama.el` | Local LLM via Ellama (entirely commented out) |
| `config-vscode.el` | VSCode integration (no longer used) |
| `config-windsurf.el` | Windsurf integration (no longer used) |
| `config-standup.el` | Replaced by config-meeting.el (Dec 2025) |
| `config-string-inflection.el` | Replaced by config-inflection.el (Dec 2025) |
| `config-aws-mode.el` | Not loaded, config-aws.el used instead |
| `config-gitlab-upload.el` | Not loaded, GitLab screenshot upload |

---

## Elisp Development Guidelines

**CRITICAL: Follow these rules when editing ANY .el file in this codebase.**

### Mandatory Practices

1. **ALWAYS test after edits**:
   ```bash
   emacs --batch -l ~/.doom.d/<file>.el 2>&1 | tail -10
   ```

2. **Verify parentheses before committing**:
   ```bash
   emacs --batch --eval "(progn (find-file \"~/.doom.d/<file>.el\") (check-parens))"
   ```

3. **ALWAYS use lexical binding** - First line of every file:
   ```elisp
   ;;; ~/.doom.d/config-<feature>.el -*- lexical-binding: t; -*-
   ```

### Syntax Rules

| Do | Don't |
|----|-------|
| `(when condition body)` | `(if condition body nil)` |
| `(unless condition body)` | `(if (not condition) body)` |
| `(let* ((a 1) (b (+ a 1))))` | `(let ((a 1)) (let ((b ...))))` when b depends on a |
| `(with-current-buffer buf ...)` | `(set-buffer buf) ...` |
| `(save-excursion ...)` | Moving point without restoring |
| `(pcase x ...)` | Nested `cond` with `equal` tests |
| `(-map #'fn list)` | `(mapcar #'fn list)` (dash.el available) |
| `(-filter #'pred list)` | `(cl-remove-if-not #'pred list)` |
| `(s-trim str)` | `(string-trim str)` (s.el available) |

### Variable Binding

```elisp
;; CORRECT - use let for local variables
(let ((x 1)
      (y 2))
  (+ x y))

;; WRONG - setq creates/modifies global state
(setq x 1)  ; Don't do this for local vars

;; CORRECT - let* when bindings depend on each other
(let* ((dir (project-root))
       (file (expand-file-name "foo" dir)))
  ...)

;; WRONG - let when second binding needs first
(let ((dir (project-root))
      (file (expand-file-name "foo" dir)))  ; dir not bound yet!
  ...)
```

### Interactive Commands

```elisp
;; Commands users call need (interactive)
(defun my-command ()
  "Docstring explaining what this does."
  (interactive)
  ...)

;; With prefix arg
(defun my-command (arg)
  "With ARG, do alternate behavior."
  (interactive "P")
  ...)

;; Functions called by code don't need interactive
(defun my-helper (x y)
  "Internal helper."
  (+ x y))
```

### Error Handling

```elisp
;; CORRECT - condition-case for expected errors
(condition-case err
    (risky-operation)
  (error (message "Failed: %s" (error-message-string err))))

;; CORRECT - condition-case-unless-debug for debugging
(condition-case-unless-debug err
    (risky-operation)
  (error (message "Failed: %s" err)))

;; CORRECT - ignore-errors when you don't care
(ignore-errors (delete-file temp-file))
```

### Doom Emacs Patterns

```elisp
;; Package config (in config.el or config-*.el)
(after! magit
  (setq magit-save-repository-buffers 'dontask))

;; Keybindings via map!
(map! :leader
      :desc "Find file" "f f" #'find-file)

;; Or via ashton-mode-map (preferred in this config)
(define-key ashton-mode-map (kbd "C-c x") #'my-command)

;; Hooks
(add-hook! 'python-mode-hook #'my-setup)

;; use-package! for packages
(use-package! some-package
  :commands (some-command)
  :config
  (setq some-var t))
```

### Common Mistakes to Avoid

1. **Unbalanced parens** - Use `check-parens` or rainbow-delimiters
2. **Missing `#'` before function names** - `(mapcar #'fn list)` not `(mapcar 'fn list)`
3. **Wrong quote type** - `'symbol` for symbols, `#'function` for functions, `` `(,var) `` for quasiquote
4. **Forgetting `interactive`** - Commands won't appear in M-x
5. **Using `setq` in let body** - Just use the let binding
6. **Not requiring dependencies** - Add `(require 'dash)` if using dash functions
7. **Assuming buffer context** - Use `with-current-buffer` explicitly
8. **String vs symbol confusion** - `"string"` vs `'symbol` vs `:keyword`

### Quoting Reference

```elisp
'foo           ; Symbol foo (not evaluated)
#'foo          ; Function foo (for passing functions)
`(a ,b c)      ; Quasiquote: a and c literal, b evaluated
'(a b c)       ; List of symbols (nothing evaluated)
(list a b c)   ; List with a, b, c evaluated
```

### Transient Menus (used heavily in this config)

```elisp
(transient-define-prefix my-transient ()
  "My command menu."
  ["Actions"
   ("a" "Action A" my-action-a)
   ("b" "Action B" my-action-b)]
  ["Navigation"
   ("n" "Next" my-next)
   ("p" "Previous" my-prev)])
```

### Learning from Good Packages

**When unsure how to implement something, read magit's source code first.**

Magit is exceptionally well-written elisp. Copy its patterns for:
- Buffer management (`magit-mode`, `magit-section`)
- Process output handling (`magit-process-sentinel`)
- Transient menus (magit invented them)
- Refresh/revert patterns (`magit-refresh`)
- Text properties and navigation

Other good references:
- `transient.el` - Menu system (by magit author)
- `dash.el` - Functional list operations
- `s.el` - String manipulation
- `f.el` - File operations

To read package source: `M-x find-library RET magit RET`

### Testing Elisp Interactively

- `M-:` (eval-expression) - Evaluate single expression
- `C-x C-e` - Evaluate sexp before point
- `C-M-x` - Evaluate defun at point
- `M-x ielm` - Interactive elisp REPL
- `*Messages*` buffer - See errors and output

---

## Development Rules (Added after broken session 2026-01-26)

**CRITICAL: When editing config-orchard.el or any large elisp file:**

1. **Commit after each working change** - Never accumulate 800+ lines of uncommitted changes
2. **Test after EVERY edit**: `emacs --batch -l ~/.doom.d/config-orchard.el 2>&1 | tail -5`
3. **Small incremental changes** - One function at a time, test, commit
4. **To fully revert**: `git checkout HEAD -- file` (not just `git checkout -- file`)
5. **If it breaks, STOP** - Revert immediately, don't pile on more changes
6. **Verify parens**: `emacs --batch --eval "(progn (find-file \"file.el\") (check-parens))"`

