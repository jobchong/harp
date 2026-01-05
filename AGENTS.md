# AGENTS.md

This file provides guidance to Codex CLI agents when working with code in this repository.

## Project Overview

emacs-harp is an Emacs package providing a coding assistant with a two-pane interface (chat + file). It supports Claude (Anthropic) and GPT (OpenAI) with streaming responses and configurable tool approval.

## File Structure

- `harp.el` - Entry point, window layout (`harp-start`, `harp-quit`)
- `harp-chat.el` - Chat buffer mode, RET to send, y/n for approvals
- `harp-api.el` - Provider abstraction, SSE streaming, message builders
- `harp-tools.el` - Tool registry (read_file, write_file, edit_file, run_shell, glob, grep)
- `harp-approval.el` - Approval system (none/dangerous-only/full), per-project config
- `harp-context.el` - Gathers git status, project root, current file for system prompt

## Development Commands

```bash
# Byte-compile all .el files
emacs -Q -batch -L . -f batch-byte-compile *.el

# Check for warnings during compilation
emacs -Q -batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile *.el

# Run ERT tests
emacs -Q -batch -L . -l harp-test.el -f ert-run-tests-batch-and-exit

# Run a single test
emacs -Q -batch -L . -l harp-test.el --eval "(ert-run-tests-batch-and-exit 'test-name)"
```

## Key Patterns

- Agent loop in `harp-chat--call-api` → streaming → tool calls → execute → loop
- Tools registered via `harp-register-tool` with JSON schema for API
- Approval flows through `harp-approval-execute-with-approval` which prompts in chat buffer
- File pane auto-updates via `harp-file-display-hook` when tools access files
- All interaction happens in chat buffer (no minibuffer) - RET sends, y/n approves

## Autonomy loop expectations

- **Mode**: Work mostly autonomously. Only ask when a shell command or URL needs permission; do not spend time on workarounds.
- **Loop**: Identify a concrete product improvement, implement it, review output, commit, then immediately move to the next improvement.
- **Commits**: Commit after each improvement loop so changes are easy to revert. Use concise conventional commit messages (e.g., `fix: ...`, `feat: ...`).
- **Libraries**: It's OK to introduce external libraries if they clearly improve readability or maintainability; prefer to do so when it meaningfully simplifies custom code.
- **Compilation**: Ensure byte-compilation succeeds after each code change.
- **Compilation warnings**: Warnings are acceptable; do not block on them.
