;;; harness.el --- Emacs coding assistant harness -*- lexical-binding: t -*-

;; Author: Emacs Harness
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/username/emacs-harness

;;; Commentary:
;; An Emacs coding assistant harness similar to Claude Code or Codex.
;; Provides a two-pane interface with chat on one side and files on the other.
;;
;; Usage:
;;   M-x harness-start - Start the harness with two-pane layout
;;   M-x harness-quit  - Close harness and restore window configuration
;;
;; In the chat buffer:
;;   RET     - Send message or approve pending tool
;;   y/n     - Approve/reject tool when prompted
;;   C-c C-k - Cancel current request
;;
;; Configuration:
;;   (setq harness-api-key-anthropic "your-api-key")
;;   (setq harness-default-provider 'anthropic)
;;   (setq harness-approval-mode 'dangerous-only)

;;; Code:

(require 'harness-api)
(require 'harness-tools)
(require 'harness-approval)
(require 'harness-context)
(require 'harness-chat)

;;; Customization

(defgroup harness nil
  "Emacs coding assistant harness."
  :group 'tools
  :prefix "harness-")

;;; Window management

(defvar harness--previous-window-config nil
  "Window configuration before starting harness.")

(defvar harness--chat-window nil
  "The chat pane window.")

(defvar harness--file-window nil
  "The file pane window.")

(defvar harness--file-buffer nil
  "Current buffer in the file pane.")

(defun harness--setup-windows ()
  "Set up the two-pane window layout."
  ;; Save current config
  (setq harness--previous-window-config (current-window-configuration))
  ;; Delete other windows and split
  (delete-other-windows)
  ;; Create chat buffer
  (let ((chat-buf (harness-chat-setup-buffer)))
    ;; Split vertically (side by side)
    (split-window-right)
    ;; Left window is chat
    (setq harness--chat-window (selected-window))
    (set-window-buffer harness--chat-window chat-buf)
    ;; Right window is file
    (setq harness--file-window (next-window))
    ;; Set up file buffer (use current file or scratch)
    (let ((file-buf (or (and buffer-file-name (current-buffer))
                        (get-buffer-create "*scratch*"))))
      (set-window-buffer harness--file-window file-buf)
      (setq harness--file-buffer file-buf)
      (harness-chat-set-file-buffer file-buf))
    ;; Focus chat window
    (select-window harness--chat-window)
    (goto-char (point-max))))

(defun harness--show-file (filepath)
  "Display FILEPATH in the file pane."
  (when (and harness--file-window (window-live-p harness--file-window))
    (let ((buf (find-file-noselect filepath)))
      (set-window-buffer harness--file-window buf)
      (setq harness--file-buffer buf)
      (harness-chat-set-file-buffer buf))))

;;; Entry points

;;;###autoload
(defun harness-start ()
  "Start the harness with two-pane layout.
Left pane is the chat interface, right pane shows files."
  (interactive)
  ;; Validate API key
  (unless (or harness-api-key-anthropic harness-api-key-openai)
    (user-error "Set `harness-api-key-anthropic' or `harness-api-key-openai' first"))
  ;; Set up file display hook
  (add-hook 'harness-file-display-hook #'harness--show-file)
  ;; Set up windows
  (harness--setup-windows)
  (message "Harness started. Type your message and press RET to send."))

;;;###autoload
(defun harness-quit ()
  "Quit harness and restore previous window configuration."
  (interactive)
  ;; Cancel any pending request
  (harness-cancel-request)
  ;; Remove hooks
  (remove-hook 'harness-file-display-hook #'harness--show-file)
  ;; Kill chat buffer
  (when-let ((buf (get-buffer harness-chat-buffer-name)))
    (kill-buffer buf))
  ;; Restore windows
  (when harness--previous-window-config
    (set-window-configuration harness--previous-window-config)
    (setq harness--previous-window-config nil))
  ;; Clear state
  (setq harness--chat-window nil)
  (setq harness--file-window nil)
  (setq harness--file-buffer nil)
  (message "Harness closed."))

;;;###autoload
(defun harness-switch-provider (provider)
  "Switch to PROVIDER (anthropic or openai)."
  (interactive
   (list (intern (completing-read "Provider: " '("anthropic" "openai")))))
  (setq harness-default-provider provider)
  (message "Switched to %s" provider))

;;;###autoload
(defun harness-set-approval-mode (mode)
  "Set approval MODE (none, dangerous-only, or full)."
  (interactive
   (list (intern (completing-read "Approval mode: "
                                  '("none" "dangerous-only" "full")))))
  (setq harness-approval-mode mode)
  (message "Approval mode set to %s" mode))

(defvar harness-anthropic-models
  '("claude-opus-4-5-20250514"
    "claude-sonnet-4-20250514"
    "claude-3-5-sonnet-20241022"
    "claude-3-5-haiku-20241022")
  "Available Anthropic models.")

(defvar harness-openai-models
  '("gpt-5.1-codex"
    "gpt-4o"
    "gpt-4-turbo"
    "o1-preview")
  "Available OpenAI models.")

;;;###autoload
(defun harness-select-model (model)
  "Select MODEL for the current provider."
  (interactive
   (let* ((models (pcase harness-default-provider
                    ('anthropic harness-anthropic-models)
                    ('openai harness-openai-models)
                    (_ (user-error "Unknown provider"))))
          (current (pcase harness-default-provider
                     ('anthropic harness-model-anthropic)
                     ('openai harness-model-openai))))
     (list (completing-read (format "Model [%s]: " current) models nil nil nil nil current))))
  (pcase harness-default-provider
    ('anthropic (setq harness-model-anthropic model))
    ('openai (setq harness-model-openai model)))
  (message "Model set to %s" model))

(provide 'harness)
;;; harness.el ends here
