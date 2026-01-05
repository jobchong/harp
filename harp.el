;;; harp.el --- Emacs coding assistant harp -*- lexical-binding: t -*-

;; Author: Emacs Harp
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/username/emacs-harp

;;; Commentary:
;; An Emacs coding assistant harp similar to Claude Code or Codex.
;; Provides a two-pane interface with chat on one side and files on the other.
;;
;; Usage:
;;   M-x harp-start - Start the harp with two-pane layout
;;   M-x harp-quit  - Close harp and restore window configuration
;;
;; In the chat buffer:
;;   RET     - Send message or approve pending tool
;;   y/n     - Approve/reject tool when prompted
;;   C-c C-k - Cancel current request
;;
;; Configuration:
;;   (setq harp-api-key-anthropic "your-api-key")
;;   (setq harp-default-provider 'anthropic)
;;   (setq harp-approval-mode 'dangerous-only)

;;; Code:

(let ((load-prefer-newer t))
  (require 'harp-api)
  (require 'harp-tools)
  (require 'harp-approval)
  (require 'harp-context)
  (require 'harp-chat))

;;; Customization

(defgroup harp nil
  "Emacs coding assistant harp."
  :group 'tools
  :prefix "harp-")

;;; Window management

(defvar harp--previous-window-config nil
  "Window configuration before starting harp.")

(defvar harp--chat-window nil
  "The chat pane window.")

(defvar harp--file-window nil
  "The file pane window.")

(defvar harp--file-buffer nil
  "Current buffer in the file pane.")

(defun harp--setup-windows ()
  "Set up the two-pane window layout."
  ;; Save current config
  (setq harp--previous-window-config (current-window-configuration))
  ;; Delete other windows and split
  (delete-other-windows)
  ;; Create chat buffer
  (let ((chat-buf (harp-chat-setup-buffer)))
    ;; Split vertically (side by side)
    (split-window-right)
    ;; Left window is chat
    (setq harp--chat-window (selected-window))
    (set-window-buffer harp--chat-window chat-buf)
    ;; Right window is file
    (setq harp--file-window (next-window))
    ;; Set up file buffer (use current file or scratch)
    (let ((file-buf (or (and buffer-file-name (current-buffer))
                        (get-buffer-create "*scratch*"))))
      (set-window-buffer harp--file-window file-buf)
      (setq harp--file-buffer file-buf)
      (harp-chat-set-file-buffer file-buf))
    ;; Focus chat window
    (select-window harp--chat-window)
    (goto-char (point-max))))

(defun harp--show-file (filepath)
  "Display FILEPATH in the file pane."
  (when (and harp--file-window (window-live-p harp--file-window))
    (let ((buf (find-file-noselect filepath)))
      (set-window-buffer harp--file-window buf)
      (setq harp--file-buffer buf)
      (harp-chat-set-file-buffer buf))))

;;; Entry points

;;;###autoload
(defun harp-start ()
  "Start the harp with two-pane layout.
Left pane is the chat interface, right pane shows files."
  (interactive)
  ;; Validate API key
  (unless (or harp-api-key-anthropic harp-api-key-openai)
    (user-error "Set `harp-api-key-anthropic' or `harp-api-key-openai' first"))
  ;; Set up file display hook
  (add-hook 'harp-file-display-hook #'harp--show-file)
  ;; Set up windows
  (harp--setup-windows)
  (message "Harp started. Type your message and press RET to send."))

;;;###autoload
(defun harp-quit ()
  "Quit harp and restore previous window configuration."
  (interactive)
  ;; Cancel any pending request
  (harp-cancel-request)
  ;; Remove hooks
  (remove-hook 'harp-file-display-hook #'harp--show-file)
  ;; Kill chat buffer
  (when-let ((buf (get-buffer harp-chat-buffer-name)))
    (kill-buffer buf))
  ;; Restore windows
  (when harp--previous-window-config
    (set-window-configuration harp--previous-window-config)
    (setq harp--previous-window-config nil))
  ;; Clear state
  (setq harp--chat-window nil)
  (setq harp--file-window nil)
  (setq harp--file-buffer nil)
  (message "Harp closed."))

;;;###autoload
(defun harp-set-approval-mode (mode)
  "Set approval MODE (none, dangerous-only, or full)."
  (interactive
   (list (intern (completing-read "Approval mode: "
                                  '("none" "dangerous-only" "full")))))
  (setq harp-approval-mode mode)
  (message "Approval mode set to %s" mode))

;;;###autoload
(defun harp-select-model (model)
  "Select MODEL and automatically set the appropriate provider."
  (interactive
   (list (completing-read
          (format "Model [%s]: " harp-model)
          (mapcar #'car harp-model-provider-alist) nil t nil nil harp-model)))
  (let ((provider (alist-get model harp-model-provider-alist nil nil #'string=)))
    (unless provider
      (user-error "Unknown model: %s" model))
    (setq harp-model model)
    (setq harp-default-provider provider)
    (message "Selected %s (%s)" model provider)))

(provide 'harp)
;;; harp.el ends here
