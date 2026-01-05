# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

emacs-harness is an Emacs package providing a coding assistant with a two-pane interface (chat + file). It supports Claude (Anthropic) and GPT (OpenAI) with streaming responses and configurable tool approval.

## File Structure

- `harness.el` - Entry point, window layout (`harness-start`, `harness-quit`)
- `harness-chat.el` - Chat buffer mode, RET to send, y/n for approvals
- `harness-api.el` - Provider abstraction, SSE streaming, message builders
- `harness-tools.el` - Tool registry (read_file, write_file, edit_file, run_shell, glob, grep)
- `harness-approval.el` - Approval system (none/dangerous-only/full), per-project config
- `harness-context.el` - Gathers git status, project root, current file for system prompt

## Development Commands

```bash
# Byte-compile all .el files
emacs -Q -batch -L . -f batch-byte-compile *.el

# Check for warnings during compilation
emacs -Q -batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile *.el

# Run ERT tests
emacs -Q -batch -L . -l harness-test.el -f ert-run-tests-batch-and-exit

# Run a single test
emacs -Q -batch -L . -l harness-test.el --eval "(ert-run-tests-batch-and-exit 'test-name)"
```

## Key Patterns

- Agent loop in `harness-chat--call-api` → streaming → tool calls → execute → loop
- Tools registered via `harness-register-tool` with JSON schema for API
- Approval flows through `harness-approval-execute-with-approval` which prompts in chat buffer
- File pane auto-updates via `harness-file-display-hook` when tools access files
- All interaction happens in chat buffer (no minibuffer) - RET sends, y/n approves

## Autonomy loop expectations

- **Mode**: Work mostly autonomously. Only ask when a shell command or URL needs permission; do not spend time on workarounds.
- **Loop**: Identify a concrete product improvement, implement it, review output, commit, then immediately move to the next improvement.
- **Commits**: Commit after each improvement loop so changes are easy to revert. Use concise conventional commit messages (e.g., `fix: ...`, `feat: ...`).
- **Libraries**: It's OK to introduce external libraries if they clearly improve readability or maintainability; prefer to do so when it meaningfully simplifies custom code.
