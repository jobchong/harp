;;; harp-chat.el --- Chat buffer interface for harp -*- lexical-binding: t -*-

;;; Commentary:
;; Provides the chat buffer major mode and interaction handling.
;; Users type messages and press RET to send. No minibuffer interaction needed.

;;; Code:

(require 'harp-api)
(require 'harp-tools)
(require 'harp-approval)
(require 'harp-context)
(require 'seq)
(require 'project)

;;; Customization

(defgroup harp-chat nil
  "Chat interface settings for harp."
  :group 'harp)

(defface harp-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for user messages."
  :group 'harp-chat)

(defface harp-assistant-face
  '((t :inherit font-lock-function-name-face))
  "Face for assistant messages."
  :group 'harp-chat)

(defface harp-tool-face
  '((t :inherit font-lock-type-face :slant italic))
  "Face for tool calls and results."
  :group 'harp-chat)

(defface harp-status-face
  '((t :inherit shadow))
  "Face for assistant status lines."
  :group 'harp-chat)

(defface harp-prompt-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face for the input prompt."
  :group 'harp-chat)

(defface harp-separator-face
  '((t :inherit shadow))
  "Face for separators."
  :group 'harp-chat)

(defface harp-approval-face
  '((t :inherit warning :weight bold))
  "Face for approval prompts."
  :group 'harp-chat)

;;; Buffer-local state

(defvar-local harp-chat--input-marker nil
  "Marker for the start of the input area.")

(defvar-local harp-chat--assistant-marker nil
  "Marker for where to insert assistant streaming text.")

(defvar-local harp-chat--messages nil
  "List of messages in the conversation for API calls.")

(defvar-local harp-chat--file-buffer nil
  "The file buffer displayed in the other pane.")

(defvar-local harp-chat--processing nil
  "Non-nil when waiting for API response.")

(defvar-local harp-chat--status-start nil
  "Marker for the start of the assistant status line.")

(defvar-local harp-chat--status-end nil
  "Marker for the end of the assistant status line.")

(defvar-local harp-chat--status-text nil
  "Current status text for the assistant response.")

(defvar-local harp-chat--pending-tool-results nil
  "Alist of (tool-use-id . result) for pending tool results.")

(defvar-local harp-chat--current-tool-calls nil
  "Tool calls from the current response, for building assistant message.")

;;; Buffer name

(defconst harp-chat-buffer-name "*harp*"
  "Name of the harp chat buffer.")

;;; Mode definition

(defvar harp-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "RET") #'harp-chat-send-or-approve)
    (define-key map (kbd "C-c C-c") #'harp-chat-send-or-approve)
    (define-key map (kbd "C-c C-k") #'harp-chat-cancel)
    map)
  "Keymap for `harp-chat-mode'.")

(define-derived-mode harp-chat-mode text-mode "Harp"
  "Major mode for harp chat interface.

\\{harp-chat-mode-map}"
  (setq-local harp-chat--input-marker (make-marker))
  (setq-local harp-chat--assistant-marker (make-marker))
  (setq-local harp-chat--status-start (make-marker))
  (setq-local harp-chat--status-end (make-marker))
  (setq-local harp-chat--messages nil)
  (setq-local harp-chat--processing nil)
  (setq-local harp-chat--pending-tool-results nil)
  (setq-local harp-chat--current-tool-calls nil)
  ;; Set up approval hook
  (add-hook 'harp-approval-request-hook #'harp-chat--show-approval nil t))

;;; Display functions

(defun harp-chat--insert-separator ()
  "Insert a visual separator line."
  (insert (propertize (make-string 50 ?─) 'face 'harp-separator-face) "\n"))

(defun harp-chat--insert-prompt ()
  "Insert the input prompt."
  (harp-chat--insert-separator)
  (insert (propertize "> " 'face 'harp-prompt-face))
  (set-marker harp-chat--input-marker (point)))

(defun harp-chat--insert-user-message (text)
  "Insert user message TEXT into the chat buffer."
  (goto-char harp-chat--input-marker)
  (delete-region harp-chat--input-marker (point-max))
  (insert (propertize "User: " 'face 'harp-user-face))
  (insert text "\n\n"))

(defun harp-chat--insert-assistant-start ()
  "Insert the start of an assistant message and set up streaming marker."
  (insert (propertize "Assistant: " 'face 'harp-assistant-face))
  (insert "\n")
  (let ((start (point)))
    (insert (propertize "Status: thinking..." 'face 'harp-status-face))
    (let ((end (point)))
      (insert "\n")
      (set-marker harp-chat--status-start start)
      (set-marker harp-chat--status-end end)
      (setq harp-chat--status-text "thinking...")))
  (set-marker harp-chat--assistant-marker (point)))

(defun harp-chat--set-status (text)
  "Update the status line for the current assistant response."
  (when (and harp-chat--status-start harp-chat--status-end)
    (setq harp-chat--status-text text)
    (save-excursion
      (goto-char harp-chat--status-start)
      (delete-region harp-chat--status-start harp-chat--status-end)
      (insert (propertize (format "Status: %s" text) 'face 'harp-status-face))
      (set-marker harp-chat--status-end (point)))))

(defun harp-chat--set-status-if-default (text)
  "Update the status line when it is still on a default value."
  (when (member harp-chat--status-text
                '("thinking..." "drafting response..." "running tools..."))
    (harp-chat--set-status text)))

(defun harp-chat--status-from-input (input)
  "Extract a status summary from INPUT."
  (cond
   ((stringp input)
    (let ((trimmed (string-trim input)))
      (if (and (not (string-empty-p trimmed))
               (or (string-prefix-p "{" trimmed)
                   (string-prefix-p "[" trimmed)))
          (condition-case nil
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-false nil)
                     (json-null nil)
                     (parsed (json-read-from-string trimmed)))
                (or (alist-get 'summary parsed)
                    (alist-get 'status parsed)
                    input))
            (error input))
        input)))
   ((and (listp input) (alist-get 'summary input)) (alist-get 'summary input))
   ((and (listp input) (alist-get 'status input)) (alist-get 'status input))
   (t nil)))

(defun harp-chat--insert-streaming-text (text)
  "Insert streaming TEXT at the assistant marker."
  (when (and (stringp text)
             (string= harp-chat--status-text "thinking..."))
    (harp-chat--set-status "drafting response..."))
  (save-excursion
    (goto-char harp-chat--assistant-marker)
    (insert (harp-chat--decode-text text))
    (set-marker harp-chat--assistant-marker (point))))

(defun harp-chat--decode-text (text)
  "Decode TEXT as UTF-8 if it is unibyte."
  (if (and (stringp text) (not (multibyte-string-p text)))
      (decode-coding-string text 'utf-8)
    text))

(defun harp-chat--insert-tool-call (name input)
  "Insert a tool call display for NAME with INPUT."
  (if (string= name "set_status")
      (when-let ((summary (harp-chat--status-from-input input)))
        (harp-chat--set-status (string-trim summary)))
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (insert "\n" (propertize (format "[Tool: %s]" name) 'face 'harp-tool-face))
      (when input
        (let ((input-str (if (stringp input)
                             input
                           (json-encode input))))
          (when (> (length input-str) 200)
            (setq input-str (concat (substring input-str 0 200) "...")))
          (insert " " input-str)))
      (insert "\n")
      (set-marker harp-chat--assistant-marker (point)))))

(defun harp-chat--insert-tool-result (name result)
  "Insert tool RESULT display for tool NAME."
  (unless (string= name "set_status")
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (let ((result-str (if (> (length result) 500)
                            (concat (substring result 0 500) "\n... [truncated]")
                          result)))
        (insert (propertize (format "[Result: %s]\n" name) 'face 'harp-tool-face))
        (insert result-str "\n"))
      (set-marker harp-chat--assistant-marker (point)))))

(defun harp-chat--show-approval ()
  "Display approval prompt for pending tool execution."
  (when-let ((pending (harp-approval-get-pending)))
    (let ((tool-name (car pending))
          (input (cadr pending)))
      (save-excursion
        (goto-char harp-chat--assistant-marker)
        (insert "\n"
                (propertize (format "[Approve %s? (y/n)]" tool-name)
                            'face 'harp-approval-face))
        (when input
          (let ((input-str (if (stringp input) input (json-encode input))))
            (when (> (length input-str) 300)
              (setq input-str (concat (substring input-str 0 300) "...")))
            (insert " " input-str)))
        (insert "\n")
        (set-marker harp-chat--assistant-marker (point))))
    (goto-char (point-max))
    (insert (propertize "> " 'face 'harp-prompt-face))
    (set-marker harp-chat--input-marker (point))
    (goto-char (point-max))))

(defun harp-chat--finish-response ()
  "Clean up after response is complete."
  (harp-chat--set-status-if-default "done")
  (goto-char harp-chat--assistant-marker)
  (insert "\n")
  (harp-chat--insert-prompt)
  (goto-char (point-max))
  (setq harp-chat--processing nil))

;;; Input handling

(defun harp-chat-send-or-approve ()
  "Send input or approve pending tool, depending on state."
  (interactive)
  (cond
   ;; If waiting for approval, check for y/n input
   ((harp-approval-pending-p)
    (let ((input (string-trim
                  (buffer-substring-no-properties
                   harp-chat--input-marker (point-max)))))
      (cond
       ((or (string= input "") (string-match-p "^[yY]" input))
        (delete-region harp-chat--input-marker (point-max))
        (harp-chat-approve))
       ((string-match-p "^[nN]" input)
        (delete-region harp-chat--input-marker (point-max))
        (harp-chat-reject))
       (t
        (message "Type 'y' to approve or 'n' to reject")))))
   ;; If processing, ignore
   (harp-chat--processing
    (message "Still processing..."))
   ;; Otherwise send input
   (t
    (harp-chat-send-input))))

(defun harp-chat-send-input ()
  "Send the current input to the LLM."
  (interactive)
  (when harp-chat--processing
    (user-error "Already processing a request"))
  (let ((input (string-trim
                (buffer-substring-no-properties
                 harp-chat--input-marker (point-max)))))
    (when (string-empty-p input)
      (user-error "No input to send"))
    ;; Display user message
    (harp-chat--insert-user-message input)
    ;; Add to messages
    (push (harp-make-user-message input) harp-chat--messages)
    ;; Start processing
    (setq harp-chat--processing t)
    (setq harp-chat--current-tool-calls nil)
    (harp-chat--insert-assistant-start)
    ;; Call API
    (harp-chat--call-api)))

(defun harp-chat--call-api ()
  "Make API call with current messages."
  (let* ((context (harp-context-gather harp-chat--file-buffer))
         (system (harp-context-build-system-prompt context))
         (messages (reverse harp-chat--messages))
         (last-user (car (last (seq-filter (lambda (m)
                                             (string= (harp--msg-get 'role m) "user"))
                                           messages))))
         (input (and last-user (harp--msg-get 'content last-user)))
         (tools (if (harp-chat--should-use-tools input)
                    (harp-get-tool-schemas)
                  (when-let ((status-tool (harp-get-tool-schema "set_status")))
                    (list status-tool)))))
    (harp-api-call-streaming
     messages system tools
     ;; On event
     (lambda (event)
       (with-current-buffer harp-chat-buffer-name
         (pcase (car event)
           ('text
            (harp-chat--insert-streaming-text (cdr event)))
           ('tool-call
            (let* ((tc (cdr event))
                   (name (alist-get 'name tc))
                   (input (alist-get 'input tc)))
              (push tc harp-chat--current-tool-calls)
              (harp-chat--insert-tool-call name input))))))
     ;; On done
     (lambda (result)
       (with-current-buffer harp-chat-buffer-name
         (if-let ((err (alist-get 'error result)))
             (progn
               (harp-chat--insert-streaming-text
                (format "\n[Error: %s]" err))
               (harp-chat--finish-response))
           ;; Process tool calls if any
           (let ((content (alist-get 'content result))
                 (tool-calls (alist-get 'tool-calls result)))
             ;; Add assistant message to history
             (push (harp-make-assistant-message
                    content harp-chat--current-tool-calls)
                   harp-chat--messages)
             (if tool-calls
                 ;; Execute tools
                 (harp-chat--execute-tools tool-calls)
               ;; No tools, we're done
               (harp-chat--finish-response)))))))))

(defun harp-chat--execute-tools (tool-calls)
  "Execute TOOL-CALLS sequentially with approval handling."
  (harp-chat--set-status-if-default "running tools...")
  (setq harp-chat--pending-tool-results nil)
  (harp-chat--execute-next-tool tool-calls))

(defun harp-chat--should-use-tools (input)
  "Return non-nil if INPUT should enable tools."
  (when (stringp input)
    (let* ((text (downcase input))
           (tool-hints '("file" "directory" "folder" "project" "repo" "path"
                         "search" "grep" "read" "write" "edit" "run"
                         "shell" "command" "open" "list" "tree" "status"
                         "git" "error" "stack trace" "stacktrace"))
           (contains-hint (seq-some (lambda (h) (string-match-p (regexp-quote h) text))
                                    tool-hints)))
      contains-hint)))

(defun harp-chat--execute-next-tool (remaining-tools)
  "Execute next tool in REMAINING-TOOLS list."
  (if (null remaining-tools)
      ;; All tools done, add results to messages and continue
      (harp-chat--tools-complete)
    (let* ((tc (car remaining-tools))
           (id (alist-get 'id tc))
           (name (alist-get 'name tc))
           (input (alist-get 'input tc)))
      (harp-approval-execute-with-approval
       name input
       (lambda (result)
         (with-current-buffer harp-chat-buffer-name
           (harp-chat--insert-tool-result name result)
           (push (cons id result) harp-chat--pending-tool-results)
           (harp-chat--execute-next-tool (cdr remaining-tools))))))))

(defun harp-chat--tools-complete ()
  "Called when all tools have been executed. Add results and continue loop."
  ;; Add tool results to messages
  (dolist (result harp-chat--pending-tool-results)
    (push (harp-make-tool-result (car result) (cdr result))
          harp-chat--messages))
  ;; Reset for next round
  (setq harp-chat--current-tool-calls nil)
  (setq harp-chat--pending-tool-results nil)
  ;; Continue the loop - call API again
  (goto-char harp-chat--assistant-marker)
  (insert "\n")
  (harp-chat--insert-assistant-start)
  (harp-chat--call-api))

;;; Approval handling

(defun harp-chat-approve ()
  "Approve the pending tool execution."
  (interactive)
  (when (harp-approval-pending-p)
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (insert (propertize "[Approved]\n" 'face 'harp-approval-face))
      (set-marker harp-chat--assistant-marker (point)))
    (harp-approval-respond t)))

(defun harp-chat-reject ()
  "Reject the pending tool execution."
  (interactive)
  (when (harp-approval-pending-p)
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (insert (propertize "[Rejected]\n" 'face 'harp-approval-face))
      (set-marker harp-chat--assistant-marker (point)))
    (harp-approval-respond nil)))

(defun harp-chat-cancel ()
  "Cancel the current request."
  (interactive)
  (harp-cancel-request)
  (setq harp-chat--processing nil)
  (harp-chat--insert-streaming-text "\n[Cancelled]\n")
  (harp-chat--finish-response))

;;; Buffer setup

(defun harp-chat-setup-buffer ()
  "Set up the chat buffer and return it."
  (let ((buf (get-buffer-create harp-chat-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'harp-chat-mode)
        (harp-chat-mode)
        (erase-buffer)
        (insert (propertize "Harp\n" 'face '(:weight bold :height 1.2)))
        (insert "Type your message and press RET to send.\n")
        (insert "Tools requiring approval will prompt with [y/n].\n\n")
        (harp-chat--insert-prompt)
        (when-let ((proj (project-current)))
          (setq default-directory (project-root proj)))
        (goto-char (point-max))))
    buf))

(defun harp-chat-set-file-buffer (buffer)
  "Set the file BUFFER that this chat is associated with."
  (with-current-buffer harp-chat-buffer-name
    (setq harp-chat--file-buffer buffer)
    (when buffer
      (let ((proj (project-current nil buffer)))
        (setq default-directory
              (or (and proj (project-root proj))
                  (and (buffer-file-name buffer)
                       (file-name-directory (buffer-file-name buffer)))
                  default-directory))))))

(provide 'harp-chat)
;;; harp-chat.el ends here
