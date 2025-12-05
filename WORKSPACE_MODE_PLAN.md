# Plan: Claude Workspace Manager Mode (config-workspace.el)

## Overview

Create a new Doom Emacs mode for managing multiple parallel `crewcapableai.*` workspaces with Claude instances, integrating with the existing `ws.py` utility at `/home/ahonnecke/src/crewcapableai.shared/bin/ws.py`.

## Core Features

### 1. Workspace Dashboard Buffer (`*Workspaces*`)

A dedicated buffer showing workspace status:

```
Workspace Status                                    [Last updated: 12:34:56]
================================================================================
#  Path                    Branch              Dirty  Ahead  Behind  Server
-- ----------------------- ------------------- ------ ------ ------- -------
0  crewcapableai.0         FEAT/new-auth       *      3      0       :3000
1  crewcapableai.1         dev                        0      2
2  crewcapableai.2         FEAT/fix-export     *      1      0       :8000
3  crewcapableai.3         dev                        0      0
S  crewcapableai.shared    main                       0      0

[n]ew feature  [s]ync all  [p]ull current  [P]ull all  [c]lean  [r]efresh  [q]uit
```

Features:
- Refreshes via `ws --json` for machine-readable output
- Color coding: green=empty/clean, yellow=dirty, red=behind upstream
- Clickable rows to open workspace in dired/magit

### 2. Keybindings (via `ashton-mode-map`)

**Global prefix: `C-c w`**

| Binding | Function | Description |
|---------|----------|-------------|
| `C-c w w` | `workspace-show` | Open dashboard buffer |
| `C-c w n` | `workspace-new-feature` | Create feature branch in empty workspace |
| `C-c w s` | `workspace-sync` | Sync all (fetch upstream) |
| `C-c w p` | `workspace-pull` | Pull/rebase current workspace |
| `C-c w P` | `workspace-pull-all` | Pull/rebase all workspaces |
| `C-c w c` | `workspace-clean` | Clean workspaces (dry-run) |
| `C-c w C` | `workspace-clean-execute` | Clean workspaces (execute) |
| `C-c w j` | `workspace-jump` | Jump to workspace N |
| `C-c w m` | `workspace-magit` | Open magit in workspace N |
| `C-c w t` | `workspace-terminal` | Open terminal in workspace N |

### 3. Dashboard-Local Keybindings (in `*Workspaces*` buffer)

| Binding | Function |
|---------|----------|
| `RET` | Open magit for workspace at point |
| `d` | Open dired for workspace at point |
| `t` | Open terminal in workspace at point |
| `n` | New feature (prompts for name) |
| `p` | Pull current workspace |
| `P` | Pull all workspaces |
| `c` | Clean (dry-run) |
| `C` | Clean (execute) |
| `s` | Sync all |
| `r` / `g` | Refresh buffer |
| `q` | Quit window |

### 4. PR Testing Screenshot Management (Test Evidence Buffer)

A structured buffer for tracking test items with Fireshot-captured screenshots:

```
Test Evidence for FEAT/new-auth @ abc123
==========================================
[ ] Login with valid credentials
[x] Login with invalid password      [2 screenshots]
[ ] Password reset flow
[ ] Session timeout handling

[a]dd item  [s] attach screenshot from clipboard  [e]xport to PR  [q]uit
```

**Workflow:**
1. Use Fireshot to capture/annotate screenshots (saves to clipboard or Downloads)
2. In test evidence buffer, position on test item
3. Press `s` to attach screenshot from clipboard (or pick from recent Downloads)
4. Screenshot uploaded to GitLab, linked to test item
5. Press `e` to export full test evidence as markdown for PR

**Keybindings in test evidence buffer:**
| Binding | Function |
|---------|----------|
| `a` | Add new test item |
| `s` | Attach screenshot to item at point (from clipboard) |
| `S` | Attach screenshot from file picker |
| `x` | Toggle item complete |
| `e` | Export to PR markdown (copies to clipboard) |
| `v` | View screenshots for item at point |
| `q` | Quit |

**Storage:**
- `<workspace>/.test-evidence/<rev-hash>/evidence.org` - test items
- `<workspace>/.test-evidence/<rev-hash>/screenshots/` - images
- GitLab upload for PR-visible URLs

### 5. Workflow Integration Commands

```elisp
(defun workspace-start-task (task-name)
  "Start task: find empty workspace, create branch, open Claude."
  (interactive "sTask name: ")
  ...)

(defun workspace-submit-pr ()
  "Push current branch and open PR creation."
  (interactive)
  ...)
```

## ws.py Command Reference

The `ws.py` utility provides these commands to wrap:

| Command | Purpose |
|---------|---------|
| `ws` | Quick list of all workspaces |
| `ws --status` / `ws -s` | Detailed status |
| `ws -n NAME` | Create `FEAT/NAME` branch in first empty workspace + auto-start claude |
| `ws -n NAME --no-claude` | Create branch without starting claude |
| `ws --pull` / `ws -p` | Rebase current branch onto `upstream/dev` |
| `ws --pull-all` / `ws -P` | Fetch and pull/rebase all workspaces |
| `ws --sync` | Fetch upstream in all workspaces |
| `ws --clean` | Show workspaces with no useful changes (dry-run) |
| `ws --clean --execute` | Actually clean identified workspaces |
| `ws --json` / `ws -j` | Machine-readable JSON output |
| `ws --current` / `ws -c` | Show which workspace pwd is in |

## Decisions Made

1. **Claude integration**: Auto-start claude in terminal after branch creation (matches `ws -n` behavior)
2. **UI style**: Both direct keybindings AND transient menu for discovery
3. **Test evidence**: Full tracker buffer with Fireshot clipboard integration

## Implementation Order

### Phase 1: Core Workspace Management
1. Create `~/.doom.d/config-workspace.el` with:
   - `defcustom` for workspace root path, ws.py path
   - JSON parsing for `ws --json` output
   - `workspace-mode` major mode for dashboard buffer
   - Basic dashboard display with workspace status
   - Navigation commands (jump to workspace, open magit/dired/terminal)

2. Add transient menu (`workspace-transient`) for command discovery

3. Add keybindings to `ashton-mode-map` (`C-c w` prefix)

4. Add load statement to `config.el`

### Phase 2: Workspace Operations
1. `workspace-new-feature` - wraps `ws -n NAME` + opens vterm with claude
2. `workspace-pull` / `workspace-pull-all` - wraps `ws -p` / `ws -P`
3. `workspace-sync` - wraps `ws --sync`
4. `workspace-clean` / `workspace-clean-execute` - wraps `ws --clean`

### Phase 3: Test Evidence Buffer
1. `workspace-test-evidence` - opens/creates test evidence buffer for current branch
2. Test item management (add, toggle complete)
3. Screenshot attachment from clipboard (integrates with `config-gitlab-upload.el`)
4. Markdown export for PR

## Files to Modify
- `~/.doom.d/config.el` - add load statement
- `~/.doom.d/config-bindings.el` - add `C-c w` keybindings

## Files to Create
- `~/.doom.d/config-workspace.el` - all new functionality (~400-500 lines)

## Reference Files
- `~/.doom.d/config-standup.el` - Pattern for minor mode with auto-activation
- `~/.doom.d/config-magit.el` - Git integration patterns
- `~/.doom.d/config-gitlab-upload.el` - Screenshot upload to GitLab
- `/home/ahonnecke/src/crewcapableai.shared/bin/ws.py` - The ws utility to wrap
