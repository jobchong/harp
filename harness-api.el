;;; harness-api.el --- API abstraction for LLM providers -*- lexical-binding: t -*-

;;; Commentary:
;; Provides abstraction layer for Anthropic and OpenAI APIs with streaming support.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

;;; Customization

(defgroup harness-api nil
  "API settings for harness."
  :group 'harness)

(defcustom harness-api-key-anthropic nil
  "API key for Anthropic Claude."
  :type 'string
  :group 'harness-api)

(defcustom harness-api-key-openai nil
  "API key for OpenAI."
  :type 'string
  :group 'harness-api)

(defcustom harness-model-anthropic "claude-opus-4-5-20250514"
  "Default model for Anthropic."
  :type 'string
  :group 'harness-api)

(defcustom harness-model-openai "gpt-5.1-codex"
  "Default model for OpenAI."
  :type 'string
  :group 'harness-api)

(defcustom harness-default-provider 'anthropic
  "Default LLM provider."
  :type '(choice (const anthropic) (const openai))
  :group 'harness-api)

(defcustom harness-max-tokens 8192
  "Maximum tokens in response."
  :type 'integer
  :group 'harness-api)

;;; Provider definitions

(cl-defstruct harness-provider
  "Structure representing an LLM provider."
  name endpoint api-key-fn model headers-fn request-fn parse-fn tools-fn)

(defun harness--anthropic-headers ()
  "Return headers for Anthropic API."
  `(("x-api-key" . ,harness-api-key-anthropic)
    ("anthropic-version" . "2023-06-01")
    ("content-type" . "application/json")))

(defun harness--openai-headers ()
  "Return headers for OpenAI API."
  `(("Authorization" . ,(concat "Bearer " harness-api-key-openai))
    ("content-type" . "application/json")))

(defun harness--anthropic-request (messages system tools)
  "Build Anthropic request body from MESSAGES, SYSTEM prompt, and TOOLS."
  (let ((body `(("model" . ,harness-model-anthropic)
                ("max_tokens" . ,harness-max-tokens)
                ("stream" . t)
                ("messages" . ,(vconcat messages)))))
    (when system
      (push `("system" . ,system) body))
    (when tools
      (push `("tools" . ,(vconcat tools)) body))
    body))

(defun harness--openai-request (messages system tools)
  "Build OpenAI request body from MESSAGES, SYSTEM prompt, and TOOLS."
  (let* ((sys-msg (when system `((("role" . "system") ("content" . ,system)))))
         (all-msgs (vconcat sys-msg messages))
         (body `(("model" . ,harness-model-openai)
                 ("max_tokens" . ,harness-max-tokens)
                 ("stream" . t)
                 ("messages" . ,all-msgs))))
    (when tools
      (push `("tools" . ,(vconcat (mapcar #'harness--tool-to-openai tools))) body))
    body))

(defun harness--tool-to-openai (tool)
  "Convert Anthropic TOOL format to OpenAI function format."
  `(("type" . "function")
    ("function" . (("name" . ,(alist-get 'name tool))
                   ("description" . ,(alist-get 'description tool))
                   ("parameters" . ,(alist-get 'input_schema tool))))))

(defun harness--anthropic-tools (tools)
  "Return TOOLS in Anthropic format (passthrough)."
  tools)

(defvar harness-provider-anthropic
  (make-harness-provider
   :name "anthropic"
   :endpoint "https://api.anthropic.com/v1/messages"
   :api-key-fn (lambda () harness-api-key-anthropic)
   :model harness-model-anthropic
   :headers-fn #'harness--anthropic-headers
   :request-fn #'harness--anthropic-request
   :tools-fn #'harness--anthropic-tools)
  "Anthropic provider instance.")

(defvar harness-provider-openai
  (make-harness-provider
   :name "openai"
   :endpoint "https://api.openai.com/v1/chat/completions"
   :api-key-fn (lambda () harness-api-key-openai)
   :model harness-model-openai
   :headers-fn #'harness--openai-headers
   :request-fn #'harness--openai-request
   :tools-fn (lambda (tools) (mapcar #'harness--tool-to-openai tools)))
  "OpenAI provider instance.")

(defun harness-get-provider ()
  "Get the current provider instance."
  (pcase harness-default-provider
    ('anthropic harness-provider-anthropic)
    ('openai harness-provider-openai)
    (_ (error "Unknown provider: %s" harness-default-provider))))

;;; Streaming

(defvar-local harness--stream-buffer ""
  "Buffer for accumulating SSE data.")

(defvar-local harness--stream-callback nil
  "Callback for streaming events.")

(defvar-local harness--stream-done-callback nil
  "Callback when streaming is complete.")

(defvar-local harness--current-content ""
  "Accumulated content from current response.")

(defvar-local harness--tool-calls nil
  "Accumulated tool calls from current response.")

(defvar-local harness--current-tool-call nil
  "Current tool call being built.")

(defun harness--parse-sse-line (line _provider)
  "Parse a single SSE LINE for _PROVIDER, return event data or nil."
  (when (string-prefix-p "data: " line)
    (let ((data (substring line 6)))
      (unless (string= data "[DONE]")
        (condition-case nil
            (json-read-from-string data)
          (error nil))))))

(defun harness--handle-anthropic-event (event)
  "Handle Anthropic streaming EVENT, return (type . data)."
  (let ((type (alist-get 'type event)))
    (pcase type
      ("content_block_start"
       (let ((block (alist-get 'content_block event)))
         (when (string= (alist-get 'type block) "tool_use")
           (setq harness--current-tool-call
                 `((id . ,(alist-get 'id block))
                   (name . ,(alist-get 'name block))
                   (input . ""))))))
      ("content_block_delta"
       (let ((delta (alist-get 'delta event)))
         (pcase (alist-get 'type delta)
           ("text_delta"
            (let ((text (alist-get 'text delta)))
              (setq harness--current-content
                    (concat harness--current-content text))
              (cons 'text text)))
           ("input_json_delta"
            (when harness--current-tool-call
              (let ((json-part (alist-get 'partial_json delta)))
                (setf (alist-get 'input harness--current-tool-call)
                      (concat (alist-get 'input harness--current-tool-call)
                              json-part))))))))
      ("content_block_stop"
       (when harness--current-tool-call
         (let ((tool-call harness--current-tool-call))
           ;; Parse accumulated JSON input
           (condition-case nil
               (setf (alist-get 'input tool-call)
                     (json-read-from-string (alist-get 'input tool-call)))
             (error nil))
           (push tool-call harness--tool-calls)
           (setq harness--current-tool-call nil)
           (cons 'tool-call tool-call))))
      ("message_stop"
       (cons 'done t))
      (_ nil))))

(defun harness--handle-openai-event (event)
  "Handle OpenAI streaming EVENT, return (type . data)."
  (let* ((choices (alist-get 'choices event))
         (choice (and choices (aref choices 0)))
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
          (setq harness--current-content
                (concat harness--current-content content))
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
                        harness--tool-calls)
                ;; Continuation of arguments
                (when-let ((existing (nth idx harness--tool-calls)))
                  (setf (alist-get 'input existing)
                        (concat (alist-get 'input existing) args))))))
          nil)))))))

(defun harness--process-stream-chunk (chunk provider)
  "Process a CHUNK of SSE data for PROVIDER."
  (setq harness--stream-buffer (concat harness--stream-buffer chunk))
  (let ((lines (split-string harness--stream-buffer "\n"))
        (events nil))
    ;; Keep incomplete last line in buffer
    (setq harness--stream-buffer (car (last lines)))
    (setq lines (butlast lines))
    (dolist (line lines)
      (when-let ((event (harness--parse-sse-line line provider)))
        (let ((result (pcase (harness-provider-name provider)
                        ("anthropic" (harness--handle-anthropic-event event))
                        ("openai" (harness--handle-openai-event event)))))
          (when result
            (push result events)))))
    (nreverse events)))

;;; Main API call

(defvar harness--active-process nil
  "Currently active API process.")

(defun harness-api-call (messages system tools on-event on-done)
  "Call LLM API with MESSAGES, SYSTEM prompt, and TOOLS.
ON-EVENT is called with (type . data) for each streaming event.
ON-DONE is called with (content . tool-calls) when complete."
  (let* ((provider (harness-get-provider))
         (url (harness-provider-endpoint provider))
         (headers (funcall (harness-provider-headers-fn provider)))
         (body (funcall (harness-provider-request-fn provider)
                        messages system tools))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (encode-coding-string (json-encode body) 'utf-8)))
    ;; Reset state
    (setq harness--current-content "")
    (setq harness--tool-calls nil)
    (setq harness--current-tool-call nil)
    ;; Make async request
    (setq harness--active-process
          (url-retrieve
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
                   (let ((events (harness--process-stream-chunk
                                  (buffer-string) provider)))
                     (dolist (event events)
                       (when on-event
                         (funcall on-event event)))
                     (funcall on-done
                              `((content . ,harness--current-content)
                                (tool-calls . ,(nreverse harness--tool-calls)))))))))
           nil t t))))

(defun harness-api-call-streaming (messages system tools on-event on-done)
  "Call LLM API with streaming, processing events as they arrive.
MESSAGES, SYSTEM, TOOLS as in `harness-api-call'.
ON-EVENT called incrementally, ON-DONE when complete."
  (let* ((provider (harness-get-provider))
         (url (harness-provider-endpoint provider))
         (headers (funcall (harness-provider-headers-fn provider)))
         (body (funcall (harness-provider-request-fn provider)
                        messages system tools))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (encode-coding-string (json-encode body) 'utf-8))
         (response-buffer nil)
         (process-pos 1))
    ;; Reset state
    (setq harness--current-content "")
    (setq harness--tool-calls nil)
    (setq harness--current-tool-call nil)
    (setq harness--stream-buffer "")
    ;; Make request with process filter for streaming
    (setq response-buffer
          (url-retrieve
           url
           (lambda (status)
             (when on-done
               (if-let ((err (plist-get status :error)))
                   (funcall on-done `((error . ,(format "%s" err))))
                 (funcall on-done
                          `((content . ,harness--current-content)
                            (tool-calls . ,(nreverse harness--tool-calls)))))))
           nil t t))
    ;; Set up process filter for incremental parsing
    (when-let ((proc (get-buffer-process response-buffer)))
      (set-process-filter
       proc
       (lambda (proc chunk)
         ;; Default filter behavior
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert chunk)
             ;; Process new content
             (let* ((new-content (buffer-substring process-pos (point-max)))
                    (events (harness--process-stream-chunk new-content provider)))
               (setq process-pos (point-max))
               (dolist (event events)
                 (when on-event
                   (funcall on-event event)))))))))))

(defun harness-cancel-request ()
  "Cancel any active API request."
  (when (and harness--active-process
             (process-live-p harness--active-process))
    (delete-process harness--active-process)
    (setq harness--active-process nil)))

;;; Message building helpers

(defun harness-make-user-message (content)
  "Create a user message with CONTENT."
  `(("role" . "user")
    ("content" . ,content)))

(defun harness-make-assistant-message (content &optional tool-calls)
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

(defun harness-make-tool-result (tool-use-id result &optional is-error)
  "Create a tool result message for TOOL-USE-ID with RESULT.
If IS-ERROR is non-nil, mark as error."
  `(("role" . "user")
    ("content" . [(("type" . "tool_result")
                   ("tool_use_id" . ,tool-use-id)
                   ("content" . ,result)
                   ,@(when is-error '(("is_error" . t))))])))

(provide 'harness-api)
;;; harness-api.el ends here
