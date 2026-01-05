;;; harp-api.el --- API abstraction for LLM providers -*- lexical-binding: t -*-

;;; Commentary:
;; Provides abstraction layer for Anthropic and OpenAI APIs with streaming support.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

;;; Customization

(defgroup harp-api nil
  "API settings for harp."
  :group 'harp)

(defcustom harp-api-key-anthropic nil
  "API key for Anthropic Claude."
  :type 'string
  :group 'harp-api)

(defcustom harp-api-key-openai nil
  "API key for OpenAI."
  :type 'string
  :group 'harp-api)

(defvar harp-model "claude-sonnet-4-20250514"
  "Current model - set via `harp-select-model'.")

(defcustom harp-default-provider 'anthropic
  "Default LLM provider."
  :type '(choice (const anthropic) (const openai))
  :group 'harp-api)

(defcustom harp-max-tokens 8192
  "Maximum tokens in response."
  :type 'integer
  :group 'harp-api)

;;; Provider definitions

(cl-defstruct harp-provider
  "Structure representing an LLM provider."
  name endpoint api-key-fn headers-fn request-fn parse-fn tools-fn)

(defun harp--anthropic-headers ()
  "Return headers for Anthropic API."
  `(("x-api-key" . ,harp-api-key-anthropic)
    ("anthropic-version" . "2023-06-01")
    ("content-type" . "application/json")))

(defun harp--openai-headers ()
  "Return headers for OpenAI API."
  `(("Authorization" . ,(concat "Bearer " harp-api-key-openai))
    ("content-type" . "application/json")))

(defun harp--anthropic-request (messages system tools)
  "Build Anthropic request body from MESSAGES, SYSTEM prompt, and TOOLS."
  (let ((body `(("model" . ,harp-model)
                ("max_tokens" . ,harp-max-tokens)
                ("stream" . t)
                ("messages" . ,(vconcat messages)))))
    (when system
      (push `("system" . ,system) body))
    (when tools
      (push `("tools" . ,(vconcat tools)) body))
    body))

(defun harp--openai-request (messages system tools)
  "Build OpenAI request body from MESSAGES, SYSTEM prompt, and TOOLS."
  (let* ((sys-msg (when system `((("role" . "system") ("content" . ,system)))))
         (all-msgs (vconcat sys-msg messages))
         (body `(("model" . ,harp-model)
                 ("max_tokens" . ,harp-max-tokens)
                 ("stream" . t)
                 ("messages" . ,all-msgs))))
    (when tools
      (push `("tools" . ,(vconcat (mapcar #'harp--tool-to-openai tools))) body))
    body))

(defun harp--tool-to-openai (tool)
  "Convert Anthropic TOOL format to OpenAI function format."
  `(("type" . "function")
    ("function" . (("name" . ,(alist-get 'name tool))
                   ("description" . ,(alist-get 'description tool))
                   ("parameters" . ,(alist-get 'input_schema tool))))))

(defun harp--anthropic-tools (tools)
  "Return TOOLS in Anthropic format (passthrough)."
  tools)

(defvar harp-provider-anthropic
  (make-harp-provider
   :name "anthropic"
   :endpoint "https://api.anthropic.com/v1/messages"
   :api-key-fn (lambda () harp-api-key-anthropic)
   :headers-fn #'harp--anthropic-headers
   :request-fn #'harp--anthropic-request
   :tools-fn #'harp--anthropic-tools)
  "Anthropic provider instance.")

(defvar harp-provider-openai
  (make-harp-provider
   :name "openai"
   :endpoint "https://api.openai.com/v1/chat/completions"
   :api-key-fn (lambda () harp-api-key-openai)
   :headers-fn #'harp--openai-headers
   :request-fn #'harp--openai-request
   :tools-fn (lambda (tools) (mapcar #'harp--tool-to-openai tools)))
  "OpenAI provider instance.")

(defun harp-get-provider ()
  "Get the current provider instance."
  (pcase harp-default-provider
    ('anthropic harp-provider-anthropic)
    ('openai harp-provider-openai)
    (_ (error "Unknown provider: %s" harp-default-provider))))

;;; Streaming

(defvar-local harp--stream-buffer ""
  "Buffer for accumulating SSE data.")

(defvar-local harp--stream-callback nil
  "Callback for streaming events.")

(defvar-local harp--stream-done-callback nil
  "Callback when streaming is complete.")

(defvar-local harp--current-content ""
  "Accumulated content from current response.")

(defvar-local harp--tool-calls nil
  "Accumulated tool calls from current response.")

(defvar-local harp--current-tool-call nil
  "Current tool call being built.")

(defun harp--parse-sse-line (line _provider)
  "Parse a single SSE LINE for _PROVIDER, return event data or nil."
  (when (string-prefix-p "data: " line)
    (let ((data (substring line 6)))
      (unless (string= data "[DONE]")
        (condition-case nil
            (json-read-from-string data)
          (error nil))))))

(defun harp--handle-anthropic-event (event)
  "Handle Anthropic streaming EVENT, return (type . data)."
  (let ((type (alist-get 'type event)))
    (pcase type
      ("content_block_start"
       (let ((block (alist-get 'content_block event)))
         (when (string= (alist-get 'type block) "tool_use")
           (setq harp--current-tool-call
                 `((id . ,(alist-get 'id block))
                   (name . ,(alist-get 'name block))
                   (input . ""))))))
      ("content_block_delta"
       (let ((delta (alist-get 'delta event)))
         (pcase (alist-get 'type delta)
           ("text_delta"
            (let ((text (alist-get 'text delta)))
              (setq harp--current-content
                    (concat harp--current-content text))
              (cons 'text text)))
           ("input_json_delta"
            (when harp--current-tool-call
              (let ((json-part (alist-get 'partial_json delta)))
                (setf (alist-get 'input harp--current-tool-call)
                      (concat (alist-get 'input harp--current-tool-call)
                              json-part))))))))
      ("content_block_stop"
       (when harp--current-tool-call
         (let ((tool-call harp--current-tool-call))
           ;; Parse accumulated JSON input
           (condition-case nil
               (setf (alist-get 'input tool-call)
                     (json-read-from-string (alist-get 'input tool-call)))
             (error nil))
           (push tool-call harp--tool-calls)
           (setq harp--current-tool-call nil)
           (cons 'tool-call tool-call))))
      ("message_stop"
       (cons 'done t))
      (_ nil))))

(defun harp--handle-openai-event (event)
  "Handle OpenAI streaming EVENT, return (type . data)."
  (let* ((choices (alist-get 'choices event))
         (choice (and choices (> (length choices) 0) (aref choices 0)))
         (delta (alist-get 'delta choice))
         (finish (alist-get 'finish_reason choice)))
    (cond
     (finish
      (cons 'done t))
     (delta
      (let ((content (alist-get 'content delta))
            (tool-calls (alist-get 'tool_calls delta)))
        (cond
         (content
          (setq harp--current-content
                (concat harp--current-content content))
          (cons 'text content))
         (tool-calls
          (dolist (tc (append tool-calls nil))
            (let* ((idx (alist-get 'index tc))
                   (fn (alist-get 'function tc))
                   (id (alist-get 'id tc))
                   (name (alist-get 'name fn))
                   (args (alist-get 'arguments fn)))
              (if id
                  ;; New tool call
                  (push `((id . ,id) (name . ,name) (input . ,args))
                        harp--tool-calls)
                ;; Continuation of arguments
                (when-let ((existing (nth idx harp--tool-calls)))
                  (setf (alist-get 'input existing)
                        (concat (alist-get 'input existing) args))))))
          nil)))))))

(defun harp--process-stream-chunk (chunk provider)
  "Process a CHUNK of SSE data for PROVIDER."
  (setq harp--stream-buffer (concat harp--stream-buffer chunk))
  (let ((lines (split-string harp--stream-buffer "\n"))
        (events nil))
    ;; Keep incomplete last line in buffer
    (setq harp--stream-buffer (car (last lines)))
    (setq lines (butlast lines))
    (dolist (line lines)
      (when-let ((event (harp--parse-sse-line line provider)))
        (let ((result (pcase (harp-provider-name provider)
                        ("anthropic" (harp--handle-anthropic-event event))
                        ("openai" (harp--handle-openai-event event)))))
          (when result
            (push result events)))))
    (nreverse events)))

;;; Main API call

(defvar harp--active-process nil
  "Currently active API process.")

(defun harp-api-call (messages system tools on-event on-done)
  "Call LLM API with MESSAGES, SYSTEM prompt, and TOOLS.
ON-EVENT is called with (type . data) for each streaming event.
ON-DONE is called with (content . tool-calls) when complete."
  (let* ((provider (harp-get-provider))
         (url (harp-provider-endpoint provider))
         (headers (funcall (harp-provider-headers-fn provider)))
         (body (funcall (harp-provider-request-fn provider)
                        messages system tools))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (encode-coding-string (json-encode body) 'utf-8)))
    ;; Reset state
    (setq harp--current-content "")
    (setq harp--tool-calls nil)
    (setq harp--current-tool-call nil)
    ;; Make async request
    (let ((buf (url-retrieve
                url
                (lambda (status)
                  (if-let ((err (plist-get status :error)))
                      (funcall on-done `((error . ,(format "%s" err))))
                    (goto-char (point-min))
                    (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring (point) (point-max))))
                      (with-temp-buffer
                        (insert body)
                        (goto-char (point-min))
                        (let ((events (harp--process-stream-chunk
                                       (buffer-string) provider)))
                          (dolist (event events)
                            (when on-event
                              (funcall on-event event)))
                          (funcall on-done
                                   `((content . ,harp--current-content)
                                     (tool-calls . ,(nreverse harp--tool-calls)))))))))
                nil t t)))
      (setq harp--active-process buf)
      (when-let ((proc (get-buffer-process buf)))
        (set-process-query-on-exit-flag proc nil)))))

(defun harp-api-call-streaming (messages system tools on-event on-done)
  "Call LLM API with streaming, processing events as they arrive.
MESSAGES, SYSTEM, TOOLS as in `harp-api-call'.
ON-EVENT called incrementally, ON-DONE when complete."
  ;; Validate API key for current provider
  (pcase harp-default-provider
    ('anthropic (unless harp-api-key-anthropic
                  (user-error "Set `harp-api-key-anthropic' first")))
    ('openai (unless harp-api-key-openai
               (user-error "Set `harp-api-key-openai' first"))))
  (let* ((provider (harp-get-provider))
         (url (harp-provider-endpoint provider))
         (headers (funcall (harp-provider-headers-fn provider)))
         (body (funcall (harp-provider-request-fn provider)
                        messages system tools))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (encode-coding-string (json-encode body) 'utf-8))
         (response-buffer nil)
         (process-pos 1)
         (done-called nil))
    (message "harp: calling %s with model %s" url harp-model)
    ;; Reset state
    (setq harp--current-content "")
    (setq harp--tool-calls nil)
    (setq harp--current-tool-call nil)
    (setq harp--stream-buffer "")
    ;; Make request with process filter for streaming
    (setq response-buffer
          (url-retrieve
           url
           (lambda (status)
             (message "harp: url-retrieve callback, status=%S" status)
             (message "harp: response buffer contents (first 500 chars): %s"
                      (buffer-substring (point-min) (min (point-max) 500)))
             ;; Fallback: call on-done if not already called via stream
             (unless done-called
               (setq done-called t)
               (when on-done
                 (if-let ((err (plist-get status :error)))
                     (funcall on-done `((error . ,(format "%s" err))))
                   (funcall on-done
                            `((content . ,harp--current-content)
                              (tool-calls . ,(nreverse harp--tool-calls))))))))
           nil t t))
    ;; Set up process filter for incremental parsing
    (when-let ((proc (get-buffer-process response-buffer)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter
       proc
       (lambda (proc chunk)
         (message "harp: received chunk (%d bytes)" (length chunk))
         ;; Default filter behavior
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert chunk)
             ;; Process new content
             (let* ((new-content (buffer-substring process-pos (point-max)))
                    (events (harp--process-stream-chunk new-content provider)))
               (when events (message "harp: parsed %d events" (length events)))
               (setq process-pos (point-max))
               (dolist (event events)
                 (when on-event
                   (funcall on-event event))
                 ;; Call on-done when stream signals completion
                 (when (and (eq (car event) 'done)
                            (not done-called))
                   (setq done-called t)
                   (when on-done
                     (funcall on-done
                              `((content . ,harp--current-content)
                                (tool-calls . ,(nreverse harp--tool-calls)))))))))))))))

(defun harp-cancel-request ()
  "Cancel any active API request."
  (when (and harp--active-process
             (process-live-p harp--active-process))
    (delete-process harp--active-process)
    (setq harp--active-process nil)))

(defun harp--cleanup-connections ()
  "Kill any open API connections."
  (harp-cancel-request)
  ;; Kill any lingering url-retrieve buffers/processes
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*http.*api\\." (buffer-name buf))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc)))
      (kill-buffer buf))))

(add-hook 'kill-emacs-hook #'harp--cleanup-connections)

;;; Message building helpers

(defun harp-make-user-message (content)
  "Create a user message with CONTENT."
  `(("role" . "user")
    ("content" . ,content)))

(defun harp-make-assistant-message (content &optional tool-calls)
  "Create an assistant message with CONTENT and optional TOOL-CALLS."
  (let ((msg `(("role" . "assistant"))))
    (if tool-calls
        ;; Anthropic format with content blocks
        (let ((blocks (if (string-empty-p content)
                          '()
                        `((("type" . "text") ("text" . ,content))))))
          (dolist (tc tool-calls)
            (push `(("type" . "tool_use")
                    ("id" . ,(alist-get 'id tc))
                    ("name" . ,(alist-get 'name tc))
                    ("input" . ,(alist-get 'input tc)))
                  blocks))
          (push `("content" . ,(vconcat (nreverse blocks))) msg))
      (push `("content" . ,content) msg))
    msg))

(defun harp-make-tool-result (tool-use-id result &optional is-error)
  "Create a tool result message for TOOL-USE-ID with RESULT.
If IS-ERROR is non-nil, mark as error."
  `(("role" . "user")
    ("content" . [(("type" . "tool_result")
                   ("tool_use_id" . ,tool-use-id)
                   ("content" . ,result)
                   ,@(when is-error '(("is_error" . t))))])))

(provide 'harp-api)
;;; harp-api.el ends here
