# Claude Code Hang Debugging Notes

**Date**: 2026-01-23
**Status**: Partially resolved - disabled blocking hook, monitoring

## Symptoms

- Claude vterm buffers freeze completely (spinner stops, no response)
- Emacs main loop blocked - can't type, scroll, or interact
- `Esc` and `C-c C-c` don't interrupt
- Process shows as "run" but 96%+ CPU
- Session works fine when resumed in plain terminal

## Root Cause Analysis

**Confirmed**: Issue is Emacs/vterm integration, NOT Claude CLI (works in terminal).

**Primary suspect**: `orchard--git-checkout-warning-hook` in `config-claude.el`
- Uses `read-char-choice` which is a **synchronous blocking call**
- If prompt gets buried or focus is wrong, Emacs freezes waiting for input
- Added in commit 03f5062 (2026-01-21 14:10)
- **DISABLED** on 2026-01-23

**Secondary suspect**: `orchard--claude-status-hook` in `config-orchard.el`
- Fires on Claude events, triggers orchard refresh
- Added in commit 5c9ac8b (2026-01-22 11:56)
- Still enabled - less likely to cause hard freeze

## Debugging Tools Added

All bindings under `C-c c d` prefix:

| Binding | Function | Purpose |
|---------|----------|---------|
| `C-c c d d` | `claude-debug-state` | Quick status message |
| `C-c c d x` | `claude-debug-dump` | Full dump to `*Claude Debug*` buffer |
| `C-c c d k` | `claude-kill-stuck` | Kill stuck processes (Emacs side) |
| `C-c c d l` | `claude-list-sessions` | List all sessions, identify orphans |
| `C-c c d o` | `claude-kill-orphans` | Kill processes without Emacs buffers |

## Session Management

**Automatic cleanup**: `kill-buffer-hook` now kills claude process when buffer is closed.

**Manual cleanup**:
```bash
# List all claude processes
ps aux | grep -E '[c]laude$'

# Kill specific PID
kill -9 <PID>

# Find which tty a buffer uses (M-:)
(process-tty-name (get-buffer-process "*claude:...*"))
```

## If Hang Occurs Again

1. **If Emacs responds at all**:
   - `C-c c d d` - check buffer state
   - `C-c c d x` - full dump
   - `C-c c d k` - kill stuck processes

2. **If Emacs is completely frozen**:
   - Try `C-g` repeatedly
   - Check if minibuffer is waiting for input (buried prompt)
   - Kill from terminal: `pkill -9 -f "claude"` (nuclear option)

3. **Investigate**:
   ```elisp
   ;; Check what hooks are on claude-code-event-hook
   claude-code-event-hook

   ;; Disable orchard status hook if needed
   (remove-hook 'claude-code-event-hook #'orchard--claude-status-hook)
   ```

4. **Check recent config changes**:
   ```bash
   cd ~/.doom.d && git log --oneline -10
   ```

## Files Involved

- `~/.doom.d/config-claude.el` - Main claude integration, debugging tools, disabled hook
- `~/.doom.d/config-claude-modeline.el` - Modeline timer (runs every 2s, reads buffers)
- `~/.doom.d/config-orchard.el` - Orchard status hook, refresh logic

## Observed Freeze States

**Freeze #1** (2026-01-23 ~08:45):
- "Baking" for 3m41s, "thought for 3s", 12.1k tokens
- 96% CPU on PID 2423331 (pts/29)
- Hooks were enabled

**Freeze #2** (2026-01-23, after disabling hooks):
- `aws secretsmanager list-secrets --profile crew.prod` stuck at "Running..."
- `claude-code-event-hook` confirmed nil
- Modeline timer still active

## Future Work

If worktree branch protection is needed, rewrite `orchard--git-checkout-warning-hook` to be non-blocking:
- Use `message` + notification instead of `read-char-choice`
- Or use async prompt with callback
- Or integrate with Claude's permission system differently
