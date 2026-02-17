# Orchard-Claude Window Management — Architecture & Invariants

## Purpose

This document captures the behavioral contracts, implementation details, and
known fragilities of the Orchard → Claude integration. It exists because this
subsystem has regressed repeatedly due to undocumented invariants and competing
window management strategies.

**If you are modifying any of these files, read this first:**
- `orchard-claude.el` — backend dispatch, buffer lifecycle, window placement
- `config-claude.el` — display-buffer-alist, vterm settings, start hook
- `orchard-window.el` — column system (VESTIGIAL — see §Dead Code)
- Upstream `claude-code.el` — `claude-code--term-make`, `claude-code--start`

---

## 1. System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│ User presses RET / c / I in Orchard dashboard                      │
└──────────────────────────┬──────────────────────────────────────────┘
                           │
                    orchard-actions.el
                    orchard-open-at-point / orchard-claude-at-point / orchard-issue-start
                           │
                           ▼
                    orchard-claude.el
                    orchard--start-claude-backend(path, command?)
                           │
                 ┌─────────┴──────────┐
                 │                    │
          backend = agent-shell   backend = claude-code  (DEFAULT)
                 │                    │
     orchard--start-agent-shell   orchard--start-claude-with-resume
                                      │
                              ┌───────┴────────┐
                              │                │
                         existing buf?     new session
                              │                │
                   orchard--place-claude-buffer │
                                               │
                              ┌─────────────── │ ─── Upstream ────────────┐
                              │  claude-code-continue                     │
                              │    → claude-code--start                   │
                              │      → claude-code--term-make             │
                              │        → pop-to-buffer (WINDOW SIDE-FX)  │
                              │        → vterm-mode (STARTS PROCESS)      │
                              │        → delete-window (HIDES BUFFER)     │
                              │      → display-window-fn (SUPPRESSED)     │
                              └───────────────────────────────────────────┘
                                               │
                              detect if Claude replaced Orchard
                              restore Orchard if needed
                                               │
                              orchard--place-claude-buffer
                              orchard--fix-claude-size
```

---

## 2. Window Invariants

These MUST hold after any Orchard→Claude operation completes:

| # | Invariant | Enforced by |
|---|-----------|-------------|
| W1 | **Orchard stays visible in leftmost window** | `orchard--start-claude-with-resume` restores Orchard if Claude displaced it |
| W2 | **Claude never replaces Orchard permanently** | Same as W1 — detected by checking `(not (get-buffer-window "*Orchard*"))` |
| W3 | **Claude never replaces another Claude** | `orchard--place-claude-buffer` skips windows with `*claude:` buffers |
| W4 | **New Claude gets a split, not a takeover** | `orchard--place-claude-buffer` splits Orchard rightward when no reusable window exists |
| W5 | **vterm-kill-buffer-on-exit is nil for all Claude buffers** | `claude-code-start-hook` in config-claude.el; also set redundantly in `orchard--start-claude-with-resume` |
| W6 | **display-buffer-alist is bypassed for Orchard-initiated Claude** | `orchard--place-claude-buffer` uses `set-window-buffer` directly; `cl-letf` suppresses `claude-code-display-window-fn` |

### Why W5 matters

Claude CLI can exit/restart its process in several scenarios:
- First run in a directory (permissions dialog)
- Context compaction
- Model switching
- Crash recovery

Without `vterm-kill-buffer-on-exit` = nil, the vterm sentinel kills the buffer
on process exit, closing the window and losing the session.

### Why W6 matters

There are TWO `display-buffer-alist` entries that match `*claude:` buffers:

1. **config-claude.el** (ACTIVE): `"\\*claude:"` →
   `claude--display-buffer-reuse-same` → `claude--display-buffer-prefer-empty`
   → `display-buffer-same-window`

2. **orchard-window.el** (DEAD — never activated): `"^\\*claude:"` →
   `orchard--display-buffer-in-branch-column` → `display-buffer-same-window`

Only #1 is active. Orchard bypasses it entirely by using `set-window-buffer`
directly. The alist entry is for non-Orchard Claude usage (e.g., `C-c c c`).

---

## 3. The Upstream Dance: `claude-code--term-make`

This is the #1 source of regressions. Upstream's vterm backend does:

```elisp
;; In claude-code--term-make (vterm backend):
(pop-to-buffer buffer)          ; 1. Shows buffer in some window (SIDE EFFECTS)
(vterm-mode)                    ; 2. Starts the Claude CLI process
(when (not (one-window-p))      ; 3. If not sole window...
  (delete-window win))          ; 4. ...hide the buffer
```

### Why pop-to-buffer exists

vterm reads the window dimensions at `vterm-mode` init time. If the buffer has
no window, vterm gets zero width and Claude's TUI renders wrong. So upstream
MUST display the buffer before calling `vterm-mode`, then hides it.

### What pop-to-buffer does (the trap)

`pop-to-buffer` calls `display-buffer` with `inhibit-same-window` = t.
This means:
- It will NOT reuse the current window
- It goes through `display-buffer-alist` (the config-claude.el rules)
- If no rule matches, Emacs falls back to `display-buffer-pop-up-window`
  which SPLITS the current window

### Single-window scenario (Orchard is sole window)

1. `pop-to-buffer` splits Orchard → now 2 windows (Orchard + Claude)
2. `vterm-mode` starts process
3. `(one-window-p)` is false → `delete-window` removes Claude window
4. Back to 1 window (Orchard). Claude buffer exists but is hidden.

### Multi-window scenario

1. `pop-to-buffer` may reuse an existing window or split
2. `delete-window` removes whichever window Claude landed in
3. Claude buffer exists but is hidden

### How Orchard handles this

`orchard--start-claude-with-resume` uses `cl-letf` to override
`claude-code-display-window-fn` to `(lambda (_buf) nil)`, which suppresses
the second display attempt in `claude-code--start`. It does NOT suppress the
`pop-to-buffer` inside `claude-code--term-make` — that must happen for vterm
to get correct dimensions.

After `claude-code-continue` returns:
1. Check if Claude displaced Orchard (Claude visible, Orchard not)
2. If so, restore Orchard to its window
3. Call `orchard--place-claude-buffer` for proper placement
4. Call `orchard--fix-claude-size` to fix vterm dimensions (they were read
   from the temporary pop-to-buffer window, not the final one)

---

## 4. `orchard--place-claude-buffer` — Decision Tree

```
Is the buffer already in a window?
  YES → select that window. Done.
  NO  ↓

Is there a non-Orchard, non-Claude window?
  YES → set-window-buffer on it. Done.
  NO  ↓

Is there an *Orchard* window?
  YES → split-window rightward, put Claude there. Done.
  NO  ↓

Fallback: put Claude in current window.
```

This function uses `set-window-buffer` (direct) NOT `pop-to-buffer` or
`display-buffer`. This is intentional — it avoids all `display-buffer-alist`
rules and gives Orchard full control over placement.

---

## 5. Buffer Naming & Lookup

Claude buffers are named by `claude-code.el` upstream:
```
*claude:<abbreviated-truename>:default*
```

Example: `*claude:~/s/c/BUGFIX-1056-foo:default*`

`orchard--claude-buffer-for-path` matches by checking if the **last path
component** (e.g., `BUGFIX-1056-foo`) appears in the buffer name. This is a
substring match, not an exact match.

**Known risk**: If two worktrees have names where one is a substring of the
other, the wrong buffer could match.

---

## 6. Backend Abstraction

```elisp
(defcustom orchard-claude-backend 'claude-code ...)
```

| Backend | Implementation | Window mgmt | Process type |
|---------|---------------|-------------|--------------|
| `claude-code` | `orchard--start-claude-with-resume` | `orchard--place-claude-buffer` | vterm |
| `agent-shell` | `orchard--start-agent-shell` | Emacs default (comint `pop-to-buffer`) | comint |
| `auto` | Prefers agent-shell if `claude-code-acp` found | Depends on detection | Depends |

**agent-shell does NOT use `orchard--place-claude-buffer`**. It has no window
invariant enforcement. If you switch to agent-shell, invariants W1-W4 are not
guaranteed.

---

## 7. Dead Code

### Column system (orchard-window.el)

The following are defined but functionally unused:

| Symbol | Status |
|--------|--------|
| `orchard--columns` hash table | Initialized but never populated meaningfully |
| `orchard--branch-to-column` hash table | Only `remhash` calls exist (cleanup), no `puthash` |
| `orchard--setup-display-buffer-rules` | Defined, never called |
| `orchard--display-buffer-in-branch-column` | Only referenced by the uncalled setup fn |
| `orchard--assign-branch-to-column` | Never called outside orchard-window.el |
| `orchard--ensure-columns` | Never called outside orchard-window.el |
| `orchard--dedicate-branch-column` | Never called outside orchard-window.el |

The column system was designed for a multi-column layout where each branch gets
a dedicated window column. It was superseded by `orchard--place-claude-buffer`
which uses simpler heuristic placement. The column system should be either
completed or removed.

### `orchard--claude-in-window` (orchard-window.el:190)

Uses `save-window-excursion` + `set-window-configuration` to start Claude
while preserving layout. This is an OLDER approach — `orchard--start-claude-with-resume`
now handles this. `orchard--claude-in-window` is not called from the main flow.

### `orchard--ensure-claude-target-window`

If present, this was added during a previous fix attempt and is no longer
called by `orchard--start-claude-with-resume`. Should be removed.

---

## 8. Upstream Patches

### `claude-code--term-make` sole-window guard

**File**: `~/.emacs.d/.local/straight/repos/claude-code.el/claude-code.el`

Original code called `(delete-window win)` unconditionally. Patched to:
```elisp
(when (and win (not (one-window-p 'nomini)))
  (delete-window win))
```

**Warning**: `doom sync` or package updates overwrite this. Must also delete
compiled `.elc` and `.eln` files for the source edit to take effect:
- `~/.emacs.d/.local/straight/build-30.1/claude-code.el`
- `~/.emacs.d/.local/cache/eln/` (search for claude-code)

---

## 9. config-claude.el Settings That Affect Orchard

| Setting | Value | Effect |
|---------|-------|--------|
| `claude-code-terminal-backend` | `'vterm` | Uses vterm, not eat |
| `claude-code-no-delete-other-windows` | `t` | Claude windows survive `delete-other-windows` |
| `claude-code-display-window-fn` | `(lambda (buf) (display-buffer-same-window buf nil))` | Default display; Orchard overrides to nil via `cl-letf` |
| `vterm-kill-buffer-on-exit` | `nil` (via start hook) | Prevents buffer death on process exit |
| `CLAUDECODE` env var | Stripped to nil | Prevents nested-session error |

---

## 10. Testing Checklist

No automated tests exist. When modifying this subsystem, manually verify:

### Scenario 1: Fresh start (single Orchard window)
1. Start Emacs (Orchard is sole window)
2. Press RET on a worktree that has no existing Claude
3. **Expected**: Orchard left, Claude right. Claude shows startup or permissions.
4. Wait 10s. **Expected**: Claude still visible, not disappeared.

### Scenario 2: Existing Claude buffer
1. Have Orchard + Claude visible
2. Navigate to a different worktree in Orchard, press RET
3. **Expected**: New Claude replaces old Claude (or appears in reusable window).
   Orchard stays left.

### Scenario 3: Permissions dialog
1. Press RET on a worktree Claude hasn't seen before
2. Claude shows folder permissions prompt
3. Accept the permissions
4. **Expected**: Claude continues in the same window. Buffer not killed.

### Scenario 4: Multiple Claude sessions
1. Open Claude for worktree A (Orchard + Claude-A)
2. Open Claude for worktree B
3. **Expected**: Claude-B replaces Claude-A in the right window (or uses
   reusable window). Claude-A buffer still exists.

### Scenario 5: claude-reset-window
1. Have Claude visible with garbled display
2. From Orchard window, run `M-x claude-reset-window`
3. **Expected**: Finds nearest visible Claude, resizes it. Works from any buffer.

### Scenario 6: C-z copy mode
1. Have Claude visible
2. Press C-z
3. **Expected**: "Claude copy mode ON", cursor visible, can navigate with C-p/C-n
4. Press C-z again
5. **Expected**: "Claude copy mode OFF", back to vterm input mode

---

## 11. Regression Prevention Rules

1. **Never use `pop-to-buffer` for Orchard-initiated Claude display**.
   Use `set-window-buffer` or `orchard--place-claude-buffer`. The
   `display-buffer-alist` machinery is unpredictable.

2. **Never use `set-window-configuration` to restore layout after starting
   Claude**. vterm's process is tied to the window it was created in. Restoring
   a saved config can orphan the process or create zombie buffers.

3. **Always set `vterm-kill-buffer-on-exit` to nil** before Claude's process
   can exit. The hook in config-claude.el handles this, but defensive code in
   orchard-claude.el sets it redundantly.

4. **After calling `claude-code-continue`, always check if Orchard was
   displaced**. The `pop-to-buffer` inside `claude-code--term-make` can replace
   Orchard with Claude depending on `display-buffer-alist` evaluation.

5. **`orchard--fix-claude-size` must run AFTER final placement**, not after
   `claude-code-continue`. vterm reads dimensions from whichever window it was
   in during `pop-to-buffer`, which is NOT the final window.

6. **Test from a single-window frame**. Most regressions only manifest when
   Orchard is the sole window (startup state). Multi-window scenarios are
   more forgiving because `delete-window` has somewhere to fall back to.
