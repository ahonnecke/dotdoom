# Claude Code Hooks & Permissions

**Date**: 2026-01-29
**Status**: Hook disabled due to bug, permissions working

## Overview

Claude Code supports hooks (shell scripts that run in response to events) and a permission system (allow/deny patterns for tool use). This documents issues encountered and fixes.

## Permission Pattern Format

Permissions in `settings.local.json` use this format:
```
"Bash(<command-prefix>:*)"
```

The `:*` is **required** for wildcard matching. Common mistakes:

| Pattern | Matches | Does NOT Match |
|---------|---------|----------------|
| `Bash(aws logs:*)` | `aws logs filter-log-events ...` | - |
| `Bash(aws logs*)` | `aws logsXYZ` (no space) | `aws logs filter-log-events` |
| `Bash(aws:*)` | Any `aws` command | - |

**Fix applied**: Changed `Bash(aws logs*)` to `Bash(aws logs:*)` in shared settings.

Location: `/home/ahonnecke/src/.crewcapableai.shared/settings.local.json`

## Hook Architecture

Hooks are defined in `settings.local.json`:

```json
"hooks": {
  "PreToolUse": [
    {
      "matcher": "",
      "hooks": [
        {
          "type": "command",
          "command": "/path/to/script pre-tool-use"
        }
      ]
    }
  ]
}
```

Available hook types:
- `PreToolUse` - Runs before each tool invocation (can block)
- `Notification` - Runs on Claude notifications
- `Stop` - Runs when Claude stops

## Bug: PreToolUse Hook Rejecting Commands

### Symptom

Commands that should be allowed by permissions (e.g., `aws logs filter-log-events`) were being rejected with:
```
Error: Permission for this tool use was denied. The tool use was rejected
```

### Root Cause

The hook script `~/.doom.d/bin/claude-hook-safe` had a bug on line 12:

```bash
response=$(timeout "$TIMEOUT_SECS" emacsclient --eval "(claude-code-handle-hook '$hook_type \"$CLAUDE_BUFFER_NAME\")" "$json_input" "$@" 2>/dev/null)
```

**Problem**: `$json_input` is passed as a **separate positional argument** to `emacsclient`, NOT to the elisp function.

`emacsclient` treats arguments after `--eval "..."` as **files to open**, not as function parameters. The JSON containing tool details (command, args, etc.) never reaches the elisp function `claude-code-handle-hook`.

When the elisp function receives no data:
1. It may error or return unexpected output
2. Lines 21-24 echo any non-nil response back
3. Claude interprets any output as a **rejection**

### Fix

**Option 1** (applied): Remove the `PreToolUse` hook entirely from settings.

**Option 2**: Fix the script to embed JSON in the elisp expression:

```bash
# Escape JSON for elisp string
escaped_json=$(echo "$json_input" | sed 's/\\/\\\\/g; s/"/\\"/g')
response=$(timeout "$TIMEOUT_SECS" emacsclient --eval \
  "(claude-code-handle-hook '$hook_type \"$CLAUDE_BUFFER_NAME\" \"$escaped_json\")" 2>/dev/null)
```

**Option 3**: Use stdin in elisp (more complex):

```bash
response=$(echo "$json_input" | timeout "$TIMEOUT_SECS" emacsclient --eval \
  "(with-temp-buffer
     (insert (shell-command-to-string \"cat\"))
     (claude-code-handle-hook '$hook_type \"$CLAUDE_BUFFER_NAME\" (buffer-string)))" 2>/dev/null)
```

## Files Involved

| File | Purpose |
|------|---------|
| `~/.doom.d/bin/claude-hook-safe` | Hook wrapper script (buggy) |
| `/home/ahonnecke/src/.crewcapableai.shared/settings.local.json` | Shared permissions/hooks config |
| `~/src/crewcapableai/*/.claude/settings.local.json` | Symlinks to shared config |

## Current State (2026-01-29)

- `PreToolUse` hook **removed** from settings
- Permission patterns fixed (`:*` suffix)
- `aws logs` commands now work without prompting
- `Notification` and `Stop` hooks still active (don't affect permissions)

## Testing Permissions

To verify a command is allowed without prompting:

```bash
# In Claude session, run command that should be auto-allowed
aws logs describe-log-groups --profile crew.prod --max-items 1
```

If prompted, check:
1. Pattern format in settings (needs `:*` suffix)
2. PreToolUse hook isn't blocking
3. Correct settings file is being read (check symlinks)

## Related

- `CLAUDE-HANG-DEBUG.md` - Other Claude/Emacs integration issues
- `config-claude.el` - Emacs-side Claude integration
