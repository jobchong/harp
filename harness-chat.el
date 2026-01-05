;;; harness-chat.el --- Chat buffer interface for harness -*- lexical-binding: t -*-

;;; Commentary:
;; Provides the chat buffer major mode and interaction handling.
;; Users type messages and press RET to send. No minibuffer interaction needed.

;;; Code:

(require 'harness-api)
(require 'harness-tools)
(require 'harness-approval)
(require 'harness-context)

;;; Customization

(defgroup harness-chat nil
  "Chat interface settings for harness."
  :group 'harness)

(defface harness-user-face
  '((t :foreground "cyan" :weight bold))
  "Face for user messages."
  :group 'harness-chat)

(defface harness-assistant-face
  '((t :foreground "green"))
  "Face for assistant messages."
  :group 'harness-chat)

(defface harness-tool-face
  '((t :foreground "yellow" :slant italic))
  "Face for tool calls and results."
  :group 'harness-chat)

(defface harness-prompt-face
  '((t :foreground "magenta" :weight bold))
  "Face for the input prompt."
  :group 'harness-chat)

(defface harness-separator-face
  '((t :foreground "gray50"))
  "Face for separators."
  :group 'harness-chat)

(defface harness-approval-face
  '((t :foreground "orange" :weight bold))
  "Face for approval prompts."
  :group 'harness-chat)

;;; Buffer-local state

(defvar-local harness-chat--input-marker nil
  "Marker for the start of the input area.")

(defvar-local harness-chat--assistant-marker nil
  "Marker for where to insert assistant streaming text.")

(defvar-local harness-chat--messages nil
  "List of messages in the conversation for API calls.")

(defvar-local harness-chat--file-buffer nil
  "The file buffer displayed in the other pane.")

(defvar-local harness-chat--processing nil
  "Non-nil when waiting for API response.")

(defvar-local harness-chat--pending-tool-results nil
  "Alist of (tool-use-id . result) for pending tool results.")

(defvar-local harness-chat--current-tool-calls nil
  "Tool calls from the current response, for building assistant message.")

;;; Buffer name

(defconst harness-chat-buffer-name "*harness*"
  "Name of the harness chat buffer.")

;;; Mode definition

(defvar harness-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "RET") #'harness-chat-send-or-approve)
    (define-key map (kbd "C-c C-c") #'harness-chat-send-or-approve)
    (define-key map (kbd "C-c C-k") #'harness-chat-cancel)
    map)
  "Keymap for `harness-chat-mode'.")

(define-derived-mode harness-chat-mode text-mode "Harness"
  "Major mode for harness chat interface.

\\{harness-chat-mode-map}"
  (setq-local harness-chat--input-marker (make-marker))
  (setq-local harness-chat--assistant-marker (make-marker))
  (setq-local harness-chat--messages nil)
  (setq-local harness-chat--processing nil)
  (setq-local harness-chat--pending-tool-results nil)
  (setq-local harness-chat--current-tool-calls nil)
  ;; Set up approval hook
  (add-hook 'harness-approval-request-hook #'harness-chat--show-approval nil t))

;;; Display functions

(defun harness-chat--insert-separator ()
  "Insert a visual separator line."
  (insert (propertize (make-string 50 ?─) 'face 'harness-separator-face) "\n"))

(defun harness-chat--insert-prompt ()
  "Insert the input prompt."
  (harness-chat--insert-separator)
  (insert (propertize "> " 'face 'harness-prompt-face))
  (set-marker harness-chat--input-marker (point)))

(defun harness-chat--insert-user-message (text)
  "Insert user message TEXT into the chat buffer."
  (goto-char harness-chat--input-marker)
  (delete-region harness-chat--input-marker (point-max))
  (insert (propertize "User: " 'face 'harness-user-face))
  (insert text "\n\n"))

(defun harness-chat--insert-assistant-start ()
  "Insert the start of an assistant message and set up streaming marker."
  (insert (propertize "Assistant: " 'face 'harness-assistant-face))
  (set-marker harness-chat--assistant-marker (point)))

(defun harness-chat--insert-streaming-text (text)
  "Insert streaming TEXT at the assistant marker."
  (save-excursion
    (goto-char harness-chat--assistant-marker)
    (insert text)
    (set-marker harness-chat--assistant-marker (point))))

(defun harness-chat--insert-tool-call (name input)
  "Insert a tool call display for NAME with INPUT."
  (save-excursion
    (goto-char harness-chat--assistant-marker)
    (insert "\n" (propertize (format "[Tool: %s]" name) 'face 'harness-tool-face))
    (when input
      (let ((input-str (if (stringp input)
                           input
                         (json-encode input))))
        (when (> (length input-str) 200)
          (setq input-str (concat (substring input-str 0 200) "...")))
        (insert " " input-str)))
    (insert "\n")
    (set-marker harness-chat--assistant-marker (point))))

(defun harness-chat--insert-tool-result (name result)
  "Insert tool RESULT display for tool NAME."
  (save-excursion
    (goto-char harness-chat--assistant-marker)
    (let ((result-str (if (> (length result) 500)
                          (concat (substring result 0 500) "\n... [truncated]")
                        result)))
      (insert (propertize (format "[Result: %s]\n" name) 'face 'harness-tool-face))
      (insert result-str "\n"))
    (set-marker harness-chat--assistant-marker (point))))

(defun harness-chat--show-approval ()
  "Display approval prompt for pending tool execution."
  (when-let ((pending (harness-approval-get-pending)))
    (let ((tool-name (car pending))
          (input (cadr pending)))
      (save-excursion
        (goto-char harness-chat--assistant-marker)
        (insert "\n"
                (propertize (format "[Approve %s? (y/n)]" tool-name)
                            'face 'harness-approval-face))
        (when input
          (let ((input-str (if (stringp input) input (json-encode input))))
            (when (> (length input-str) 300)
              (setq input-str (concat (substring input-str 0 300) "...")))
            (insert " " input-str)))
        (insert "\n")
        (set-marker harness-chat--assistant-marker (point))))))

(defun harness-chat--finish-response ()
  "Clean up after response is complete."
  (save-excursion
    (goto-char harness-chat--assistant-marker)
    (insert "\n"))
  (harness-chat--insert-prompt)
  (goto-char (point-max))
  (setq harness-chat--processing nil))

;;; Input handling

(defun harness-chat-send-or-approve ()
  "Send input or approve pending tool, depending on state."
  (interactive)
  (cond
   ;; If waiting for approval, check for y/n input
   ((harness-approval-pending-p)
    (let ((input (string-trim
                  (buffer-substring-no-properties
                   harness-chat--input-marker (point-max)))))
      (cond
       ((or (string= input "") (string-match-p "^[yY]" input))
        (delete-region harness-chat--input-marker (point-max))
        (harness-chat-approve))
       ((string-match-p "^[nN]" input)
        (delete-region harness-chat--input-marker (point-max))
        (harness-chat-reject))
       (t
        (message "Type 'y' to approve or 'n' to reject")))))
   ;; If processing, ignore
   (harness-chat--processing
    (message "Still processing..."))
   ;; Otherwise send input
   (t
    (harness-chat-send-input))))

(defun harness-chat-send-input ()
  "Send the current input to the LLM."
  (interactive)
  (when harness-chat--processing
    (user-error "Already processing a request"))
  (let ((input (string-trim
                (buffer-substring-no-properties
                 harness-chat--input-marker (point-max)))))
    (when (string-empty-p input)
      (user-error "No input to send"))
    ;; Display user message
    (harness-chat--insert-user-message input)
    ;; Add to messages
    (push (harness-make-user-message input) harness-chat--messages)
    ;; Start processing
    (setq harness-chat--processing t)
    (setq harness-chat--current-tool-calls nil)
    (harness-chat--insert-assistant-start)
    ;; Call API
    (harness-chat--call-api)))

(defun harness-chat--call-api ()
  "Make API call with current messages."
  (let* ((context (harness-context-gather harness-chat--file-buffer))
         (system (harness-context-build-system-prompt context))
         (tools (harness-get-tool-schemas))
         (messages (reverse harness-chat--messages)))
    (harness-api-call-streaming
     messages system tools
     ;; On event
     (lambda (event)
       (with-current-buffer harness-chat-buffer-name
         (pcase (car event)
           ('text
            (harness-chat--insert-streaming-text (cdr event)))
           ('tool-call
            (let* ((tc (cdr event))
                   (name (alist-get 'name tc))
                   (input (alist-get 'input tc)))
              (push tc harness-chat--current-tool-calls)
              (harness-chat--insert-tool-call name input))))))
     ;; On done
     (lambda (result)
       (with-current-buffer harness-chat-buffer-name
         (if-let ((err (alist-get 'error result)))
             (progn
               (harness-chat--insert-streaming-text
                (format "\n[Error: %s]" err))
               (harness-chat--finish-response))
           ;; Process tool calls if any
           (let ((content (alist-get 'content result))
                 (tool-calls (alist-get 'tool-calls result)))
             ;; Add assistant message to history
             (push (harness-make-assistant-message
                    content harness-chat--current-tool-calls)
                   harness-chat--messages)
             (if tool-calls
                 ;; Execute tools
                 (harness-chat--execute-tools tool-calls)
               ;; No tools, we're done
               (harness-chat--finish-response)))))))))

(defun harness-chat--execute-tools (tool-calls)
  "Execute TOOL-CALLS sequentially with approval handling."
  (setq harness-chat--pending-tool-results nil)
  (harness-chat--execute-next-tool tool-calls))

(defun harness-chat--execute-next-tool (remaining-tools)
  "Execute next tool in REMAINING-TOOLS list."
  (if (null remaining-tools)
      ;; All tools done, add results to messages and continue
      (harness-chat--tools-complete)
    (let* ((tc (car remaining-tools))
           (id (alist-get 'id tc))
           (name (alist-get 'name tc))
           (input (alist-get 'input tc)))
      (harness-approval-execute-with-approval
       name input
       (lambda (result)
         (with-current-buffer harness-chat-buffer-name
           (harness-chat--insert-tool-result name result)
           (push (cons id result) harness-chat--pending-tool-results)
           (harness-chat--execute-next-tool (cdr remaining-tools))))))))

(defun harness-chat--tools-complete ()
  "Called when all tools have been executed. Add results and continue loop."
  ;; Add tool results to messages
  (dolist (result harness-chat--pending-tool-results)
    (push (harness-make-tool-result (car result) (cdr result))
          harness-chat--messages))
  ;; Reset for next round
  (setq harness-chat--current-tool-calls nil)
  (setq harness-chat--pending-tool-results nil)
  ;; Continue the loop - call API again
  (harness-chat--insert-assistant-start)
  (harness-chat--call-api))

;;; Approval handling

(defun harness-chat-approve ()
  "Approve the pending tool execution."
  (interactive)
  (when (harness-approval-pending-p)
    (save-excursion
      (goto-char harness-chat--assistant-marker)
      (insert (propertize "[Approved]\n" 'face 'harness-approval-face))
      (set-marker harness-chat--assistant-marker (point)))
    (harness-approval-respond t)))

(defun harness-chat-reject ()
  "Reject the pending tool execution."
  (interactive)
  (when (harness-approval-pending-p)
    (save-excursion
      (goto-char harness-chat--assistant-marker)
      (insert (propertize "[Rejected]\n" 'face 'harness-approval-face))
      (set-marker harness-chat--assistant-marker (point)))
    (harness-approval-respond nil)))

(defun harness-chat-cancel ()
  "Cancel the current request."
  (interactive)
  (harness-cancel-request)
  (setq harness-chat--processing nil)
  (harness-chat--insert-streaming-text "\n[Cancelled]\n")
  (harness-chat--finish-response))

;;; Buffer setup

(defun harness-chat-setup-buffer ()
  "Set up the chat buffer and return it."
  (let ((buf (get-buffer-create harness-chat-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'harness-chat-mode)
        (harness-chat-mode)
        (erase-buffer)
        (insert (propertize "Harness Chat\n" 'face '(:weight bold :height 1.2)))
        (insert "Type your message and press RET to send.\n")
        (insert "Tools requiring approval will prompt with [y/n].\n\n")
        (harness-chat--insert-prompt)
        (goto-char (point-max))))
    buf))

(defun harness-chat-set-file-buffer (buffer)
  "Set the file BUFFER that this chat is associated with."
  (with-current-buffer harness-chat-buffer-name
    (setq harness-chat--file-buffer buffer)))

(provide 'harness-chat)
;;; harness-chat.el ends here
