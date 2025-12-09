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
| `M-m` | `magit-status` | Open Magit |
| `M-g` | `+default/search-project` | Project-wide search |
| `C-x f` | `+ivy/projectile-find-file` | Find file in project |
| `C-;` | `comment-or-uncomment-region` | Toggle comments |
| `C-'` | `er/expand-region` | Expand selection |
| `C-"` | `er/contract-region` | Contract selection |
| `C-c v t` | `+vterm/toggle` | Toggle terminal |
| `C-c t` | `python-pytest-dispatch` | Python test dispatch |
| `C-c a` | aider transient | AI assistant |

### Navigation (C-c g prefix)

| Binding | Function |
|---------|----------|
| `C-c g g` | Browse file at remote |
| `C-c g f` | Find file at point with line |
| `C-c g t` | Toggle impl/test file |

### String Inflection (C-c i prefix)

| Binding | Function |
|---------|----------|
| `C-c i c` | camelCase |
| `C-c i s` | snake_case |
| `C-c i k` | kebab-case |
| `C-c i u` | UPCASE |
| `C-c i p` | Python style cycle |
| `C-c i U` | Upcase region |
| `C-c i l` | Downcase region |

### Standup Mode (C-c S prefix - global)

| Binding | Function |
|---------|----------|
| `C-c S s` | Open standup file |
| `C-c S d` | Add done item (from any buffer) |
| `C-c S g` | Add doing item |
| `C-c S b` | Add blocker |

### Standup Mode (M-s prefix - in STANDUP.org only)

| Binding | Function |
|---------|----------|
| `M-s d` | Add done item |
| `M-s g` | Add doing item |
| `M-s b` | Add blocker |
| `M-s e` | Export to clipboard |
| `M-s y` | Jump to yesterday |
| `M-s c` | Carryover from yesterday |
| `M-s m` | Move item to done |
| `M-s n` | New day entry |
| `M-s t` | Jump to today |

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

- Opens `~/MAIN_ASHTON_TODO.org` on startup (not splash screen)
- `initial-buffer-choice` configured in `config.el:210-211`

### Org Directory

```elisp
(setq org-directory "~/org/")
```

### Auth/Credentials

```elisp
(setq auth-sources '("~/.authinfo"))
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
| `C-c c z` | `claude-code-toggle-read-only-mode` | Copy text from Claude |

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

### Standup Integration

From STANDUP.org, press `M-s p` to post today's standup directly to a Slack channel.

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
- **Prefix**: `C-c o`
- **Enabled for**: python-ts-mode, js-ts-mode, typescript-ts-mode, tsx-ts-mode, json-ts-mode, yaml-ts-mode, css-ts-mode

Combobulate provides tree-sitter powered structural editing - navigate and manipulate code by AST nodes rather than text.

---

## Completion System

- **Company-mode** with 0.1s idle delay, 2-char minimum
- Custom completion functions in `custom-completion.el`:
  - `C-c c b` - Buffer completion
  - `C-c c p` - Project completion
  - `C-c c l` - LSP completion
  - `C-c c f` - File completion

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
