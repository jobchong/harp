# emacs-harp

An Emacs coding assistant that provides a two-pane interface: chat on the left
and the current file on the right. It supports Claude (Anthropic) and GPT
(OpenAI), streams responses, and can run tools with configurable approvals.

## Features

- Two-pane layout for chat + file view
- Streaming responses from Anthropic and OpenAI
- Built-in tools for file I/O, shell commands, and search
- Configurable approval modes for tool execution
- Model selection across providers

## Requirements

- Emacs 27.1+
- An API key for Anthropic or OpenAI

## Install

Manual setup:

```elisp
(add-to-list 'load-path "/path/to/harp")
(require 'harp)
```

## Configure

```elisp
;; API keys
(setq harp-api-key-anthropic "your-anthropic-key")
;; or
(setq harp-api-key-openai "your-openai-key")

;; Provider and model
(setq harp-default-provider 'anthropic)
(setq harp-model "claude-sonnet-4-20250514")
(setq harp-max-tokens 8192)

;; Tool approvals: none, dangerous-only, full
(setq harp-approval-mode 'dangerous-only)

;; Optional per-project overrides
;; (setq harp-project-approval-alist '(("/abs/project/root" . full)))
```

## Use

- `M-x harp-start` to open the two-pane interface
- `M-x harp-quit` to close it
- `M-x harp-select-model` to switch models and providers

In the chat buffer:

- `RET` or `C-c C-c` send message / approve tool
- `y` / `n` approve or reject tool prompts
- `C-c C-k` cancel the current request

## Built-in Tools

- `read_file` - read a file
- `write_file` - write a file
- `edit_file` - replace exact text in a file
- `run_shell` - run a shell command
- `glob` - find files by pattern
- `grep` - search with regex
- `list_directory` - list directory contents

Tool approvals are controlled by `harp-approval-mode` and can be overridden
per project with `harp-project-approval-alist`.

## Development

```bash
# Byte-compile all .el files
emacs -Q -batch -L . -f batch-byte-compile *.el

# Treat warnings as errors
emacs -Q -batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile *.el

# Run tests (if present)
emacs -Q -batch -L . -l harp-test.el -f ert-run-tests-batch-and-exit
```

## License

MIT. See `LICENSE`.
