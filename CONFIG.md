# Configuration

This document lists user-facing customization options. You can also browse
them in Emacs via `M-x customize-group RET harp RET`.

## API and models (`harp-api`)

- `harp-api-key-anthropic` (string or nil, default `nil`): Anthropic API key.
- `harp-api-key-openai` (string or nil, default `nil`): OpenAI API key.
- `harp-model` (string, default `"claude-sonnet-4-20250514"`): Current model name (used by `harp-select-model`).
- `harp-default-provider` (symbol, default `anthropic`): Default provider, `anthropic` or `openai`.
- `harp-max-tokens` (integer, default `8192`): Max output tokens for responses.
- `harp-model-provider-alist` (alist, default below): Map model names to providers.
- `harp-debug-sse` (boolean, default `nil`): Log raw SSE lines.

Default `harp-model-provider-alist`:

```elisp
(("claude-opus-4-5-20251101" . anthropic)
 ("claude-sonnet-4-20250514" . anthropic)
 ("gpt-5.1-codex-max" . openai))
```

## Chat behavior (`harp-chat`)

- `harp-chat-max-tool-calls` (integer, default `3`): Max external tool calls per request.
- `harp-chat-listing-limit` (integer, default `1`): Max directory listings per request.

## Tool approvals (`harp-approval`)

- `harp-approval-mode` (symbol, default `dangerous-only`): `none`, `dangerous-only`, or `full`.
- `harp-project-approval-alist` (alist, default `nil`): Per-project approval overrides.

## Context collection (`harp-context`)

- `harp-context-include-git` (boolean, default `t`): Include git status/branch in prompt.
- `harp-context-include-file-content` (boolean, default `t`): Include current file content.
- `harp-context-include-readme` (boolean, default `t`): Include README content.
- `harp-context-include-slash-skills` (boolean, default `t`): Include slash skills discovered under `.codex/skills`, `.agents/skills`, or `.claude/skills`.
- `harp-context-max-file-size` (integer, default `50000`): Max chars per file included.

## Debugging (`harp-debug`)

- `harp-debug-level` (symbol or nil, default `nil`): `nil`, `info`, or `verbose`.
- `harp-debug-log-file` (file or nil, default `nil`): Append debug logs to this file.
- `harp-debug-log-backtrace` (boolean, default `nil`): Include backtraces on tool errors.
- `harp-debug-dump-directory` (string, default `"/tmp/harp-logs"`): Directory for debug dumps.
- `harp-debug-auto-dump` (boolean, default `nil`): Dump state after each response.
