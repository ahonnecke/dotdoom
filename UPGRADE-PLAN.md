# Doom Emacs Modernization Plan

**Created**: 2025-12-05
**Status**: IN PROGRESS

---

## Critical Findings

### Keybinding Conflict: `C-c c` prefix
Both `custom-completion.el` and `config-claude.el` use `C-c c`:
- `C-c c b` → `my/company-complete-buffer` (completion)
- `C-c c c` → `claude-code` (claude)
- `C-c c f` → `company-files` (completion)
- `C-c c r` → `claude-code-send-region` (claude)

**Resolution**: Move completion to `C-c p` (Cape prefix) when switching to Corfu:
- `C-c p f` → `cape-file`
- `C-c p d` → `cape-dabbrev`
- `C-c p l` → `cape-line`
- Keep `C-c c` for Claude (more frequently used)

### aws.el Was Missing
- Cloned to `~/src/aws.el/` ✓
- Config at `config-aws-mode.el` already points there

### Custom Modes Auto-Benefit
Your modes (commando, orchard, testicular) use `completing-read` - they'll
automatically get Consult/Embark/Marginalia/Orderless enhancements with zero changes.

---

## Overview

Upgrade from legacy packages to the modern Emacs ecosystem. You've already made the Ivy → Vertico jump. Now we complete the stack.

---

## Phase 1: The MOVEC Stack (Minibuffer)

The foundation - these packages work together as a cohesive unit.

### 1.1 Add Orderless
**What**: Space-separated fuzzy matching ("ins pac" → `package-install`)
**Why**: Makes Vertico 10x more powerful
**Effort**: Trivial - just enable the Doom module

```elisp
;; init.el - already have vertico, add orderless
(vertico +icons)  ; you have this
;; orderless is bundled with vertico in Doom
```

### 1.2 Add Marginalia
**What**: Rich annotations in minibuffer (file sizes, docstrings, variable values)
**Why**: See what you're selecting before you select it
**Effort**: Trivial - Doom module

```elisp
;; packages.el
(package! marginalia)
```

### 1.3 Add Consult
**What**: Enhanced completing-read commands with live previews
**Why**: `consult-buffer` (preview buffers), `consult-ripgrep` (live grep), `consult-line` (goto line with preview)
**Effort**: Low - replace some keybindings
**Replaces**: Some of your `+default/search-project` usage

Key commands:
- `consult-buffer` - switch buffer with preview
- `consult-ripgrep` - live grep with preview
- `consult-line` - goto line in buffer
- `consult-imenu` - jump to symbol
- `consult-outline` - jump to heading
- `consult-git-grep` - grep in git files

### 1.4 Add Embark
**What**: Context actions on ANY target (like right-click but keyboard)
**Why**: From any minibuffer candidate: open, delete, rename, grep, export to buffer
**Effort**: Medium - learn new workflow
**Game changer**: `embark-act` on a file candidate → delete/rename/copy/grep it without leaving minibuffer

Key commands:
- `embark-act` (bind to `C-.`) - act on thing at point or minibuffer candidate
- `embark-dwim` - do what I mean
- `embark-collect` - export candidates to buffer
- `embark-export` - export to appropriate mode (grep results → grep-mode)

---

## Phase 2: Completion (Company → Corfu)

### 2.1 Switch to Corfu + Cape
**What**: Modern in-buffer completion that uses native Emacs Capfs
**Why**:
- Lighter weight (1/3 the size of Company)
- Native integration with Vertico ecosystem
- Child frames (pretty popups)
- Cape provides modular completion sources

**Effort**: Medium - need to migrate custom-completion.el

```elisp
;; init.el
:completion
;;company           ; OLD
(corfu +icons)      ; NEW

;; packages.el
(package! cape)
```

### 2.2 Migrate Company Config
Your `custom-completion.el` Company backends map to Cape:

| Company | Cape |
|---------|------|
| `company-dabbrev` | `cape-dabbrev` |
| `company-dabbrev-code` | `cape-dabbrev` with config |
| `company-files` | `cape-file` |
| `company-capf` | Native (corfu uses capfs directly) |
| `company-yasnippet` | `cape-yasnippet` (via yasnippet-capf) |
| `company-keywords` | `cape-keyword` |

### 2.3 Create config-corfu.el
New file with Cape setup and keybindings.

---

## Phase 3: LLM Upgrades

### 3.1 Upgrade aider.el → Aidermacs
**What**: Modern Aider integration with architect mode
**Why**:
- Actively maintained (NonGNU ELPA)
- Two-model architect mode (SOTA benchmark results)
- Better Emacs integration

**Effort**: Low - similar API

```elisp
;; packages.el
(package! aidermacs)

;; Remove from packages.el:
;; (load-path "~/src/aider.el")
```

### 3.2 Add Monet (Claude IDE Integration)
**What**: WebSocket bridge between Emacs and Claude Code
**Why**:
- Auto-shares current selection with Claude
- Sends linter diagnostics to Claude
- Interactive diff review before applying changes
- Same author as claude-code.el (Steve Molitor)

**Effort**: Low

```elisp
;; packages.el
(package! monet
  :recipe (:host github :repo "stevemolitor/monet"))
```

Usage: Set `ENABLE_IDE_INTEGRATION=t` before launching Claude.

### 3.3 Add mcp.el (Model Context Protocol)
**What**: MCP tool support for gptel
**Why**: Give LLMs access to tools (file operations, web search, etc.)
**Effort**: Medium - need to configure MCP servers

```elisp
;; packages.el
(package! mcp
  :recipe (:host github :repo "lizqwerscott/mcp.el"))
```

---

## Phase 4: Transient Menus

### 4.1 Add Casual Suite
**What**: Transient menus for built-in Emacs modes
**Why**: Discoverable commands - press `?` to see what's available
**Modes covered**: Calc, Dired, Info, Bookmarks, IBuffer, I-Search, Avy, Agenda, RE-Builder

**Effort**: Low

```elisp
;; packages.el
(package! casual-suite)
```

### 4.2 Explore Your aws.el
**What**: You already have this at ~/src/aws.el/
**Action**: Actually use it! Try the Bedrock AI chat integration.

---

## Implementation Order

```
[x] Phase 1.1 - Orderless (DONE - included with Doom vertico)
[x] Phase 1.2 - Marginalia (DONE - included with Doom vertico)
[x] Phase 1.3 - Consult + keybindings (DONE - config-consult-embark.el)
[x] Phase 1.4 - Embark (DONE - config-consult-embark.el)
[x] Phase 2.1 - Corfu base (DONE - init.el updated)
[x] Phase 2.2 - Cape setup (DONE - config-corfu.el)
[x] Phase 2.3 - Migrate company config (DONE - custom-completion.el.archived)
[x] Phase 3.1 - Aidermacs (DONE - config-aider.el updated)
[x] Phase 3.2 - Monet (DONE - config-monet.el)
[x] Phase 3.3 - mcp.el (DONE - package installed, needs config)
[x] Phase 4.1 - Casual Suite (DONE - config-casual.el)
[x] Phase 4.2 - aws.el cloned to ~/src/aws.el/
```

---

## Files to Create

1. `config-corfu.el` - Corfu + Cape setup
2. `config-consult.el` - Consult commands and bindings
3. `config-embark.el` - Embark configuration
4. `config-monet.el` - Monet IDE integration
5. `config-mcp.el` - MCP protocol setup
6. `config-casual.el` - Casual suite bindings

## Files to Modify

1. `init.el` - Switch company → corfu, ensure vertico flags
2. `packages.el` - Add new packages
3. `config-aider.el` - Replace with aidermacs
4. `custom-completion.el` - Archive or delete after migration

## Files to Archive

1. `custom-completion.el` → `custom-completion.el.archived`

---

## Keybinding Plan

### New Global Bindings (ashton-mode-map)

```elisp
;; Embark
(define-key ashton-mode-map (kbd "C-.") #'embark-act)
(define-key ashton-mode-map (kbd "C->") #'embark-dwim)
(define-key ashton-mode-map (kbd "C-h B") #'embark-bindings)

;; Consult (replacing/augmenting existing)
(define-key ashton-mode-map (kbd "C-x b") #'consult-buffer)
(define-key ashton-mode-map (kbd "M-g") #'consult-ripgrep)  ; was +default/search-project
(define-key ashton-mode-map (kbd "M-s l") #'consult-line)
(define-key ashton-mode-map (kbd "M-s i") #'consult-imenu)
(define-key ashton-mode-map (kbd "M-g g") #'consult-goto-line)
(define-key ashton-mode-map (kbd "M-y") #'consult-yank-pop)

;; Corfu/Cape
(define-key ashton-mode-map (kbd "C-c p f") #'cape-file)
(define-key ashton-mode-map (kbd "C-c p d") #'cape-dabbrev)
(define-key ashton-mode-map (kbd "C-c p l") #'cape-line)
```

---

## Rollback Plan

If shit breaks:
1. `git stash` your changes
2. Comment out new packages in packages.el
3. Re-enable `company` in init.el
4. `doom sync`

---

## Questions to Decide

1. **Corfu popup style**: Child frame (pretty) or overlay (terminal-safe)?
2. **Consult ripgrep**: Replace `M-g` binding or use different key?
3. **Embark**: `C-.` is common but conflicts with `company-files` - ok to change?
4. **MCP servers**: Which tools do you want available to gptel?

---

## Resources

- [Prot's Modern Minibuffer Tutorial](https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/)
- [Karthinks: 15 Ways to Use Embark](https://karthinks.com/software/fifteen-ways-to-use-embark/)
- [Doom Corfu Module Docs](https://github.com/doomemacs/doomemacs/tree/master/modules/completion/corfu)
- [EmacsConf 2024: Casual Suite](https://emacsconf.org/2024/talks/casual/)
