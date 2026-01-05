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

(defcustom harp-model "claude-sonnet-4-20250514"
  "Current model - set via `harp-select-model'."
  :type 'string
  :group 'harp-api)

(defcustom harp-default-provider 'anthropic
  "Default LLM provider."
  :type '(choice (const anthropic) (const openai))
  :group 'harp-api)

(defcustom harp-max-tokens 8192
  "Maximum tokens in response."
  :type 'integer
  :group 'harp-api)

(defcustom harp-model-provider-alist
  '(("claude-opus-4-5-20251101" . anthropic)
    ("claude-sonnet-4-20250514" . anthropic)
    ("claude-3-5-sonnet-20241022" . anthropic)
    ("claude-3-5-haiku-20241022" . anthropic)
    ("gpt-5.1-codex-max" . openai))
  "Map model names to providers for automatic routing."
  :type '(alist :key-type string :value-type symbol)
  :group 'harp-api)
(defcustom harp-debug-sse nil
  "When non-nil, log raw SSE lines in *Messages*."
  :type 'boolean
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

(defun harp--openai-content-text (content)
  "Extract plain text from CONTENT blocks or string."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat
     (lambda (block)
       (if (and (listp block)
                (string= (alist-get 'type block) "text"))
           (or (alist-get 'text block) "")
         ""))
     (append content nil)
     ""))
   (t "")))

(defun harp--openai-wrap-content (role text)
  "Build a Responses API content array for ROLE with TEXT."
  (let ((type (if (string= role "assistant") "output_text" "input_text")))
    (vector `((type . ,type) (text . ,text)))))

(defun harp--openai-assistant-content (content)
  "Convert assistant CONTENT blocks into Responses API content."
  (cond
   ((stringp content)
    (harp--openai-wrap-content "assistant" content))
   ((vectorp content)
    (let (blocks)
      (dolist (block (append content nil))
        (when (listp block)
          (pcase (alist-get 'type block)
            ("text"
             (when-let ((text (alist-get 'text block)))
               (push `((type . "output_text") (text . ,text)) blocks)))
            ("tool_use"
             (let* ((tool-id (alist-get 'id block))
                    (name (alist-get 'name block))
                    (input (alist-get 'input block))
                    (args (if (stringp input) input (json-encode input))))
               (push `((type . "tool_call")
                       (id . ,tool-id)
                       (name . ,name)
                       (arguments . ,args))
                     blocks))))))
      (vconcat (nreverse blocks))))
   (t nil)))

(defun harp--openai-normalize-messages (messages system)
  "Convert internal MESSAGES and SYSTEM into OpenAI-compatible messages."
  (let (normalized)
    (when system
      (push `(("role" . "system")
              ("content" . ,(harp--openai-wrap-content "system" system)))
            normalized))
    (dolist (msg (append messages nil))
      (let ((role (harp--msg-get 'role msg))
            (content (harp--msg-get 'content msg)))
        (cond
         ((null role)
          (when harp-debug-sse
            (message "harp: skipping message without role: %S" msg)))
         ((and (string= role "user") (vectorp content))
          (let (text)
            (dolist (block (append content nil))
              (cond
               ((and (listp block)
                     (string= (alist-get 'type block) "tool_result"))
                (let ((tool-id (alist-get 'tool_use_id block))
                      (result (alist-get 'content block)))
                  (push `(("role" . "tool")
                          ("tool_call_id" . ,tool-id)
                          ("content" . ,(harp--openai-wrap-content
                                         "assistant"
                                         (if (stringp result)
                                             result
                                           (format "%s" result)))))
                        normalized)))
               ((and (listp block)
                     (string= (alist-get 'type block) "text"))
                (let ((chunk (alist-get 'text block)))
                  (when (stringp chunk)
                    (setq text (concat text chunk)))))))
            (when (and text (not (string-empty-p text)))
              (push `(("role" . "user")
                      ("content" . ,(harp--openai-wrap-content "user" text)))
                    normalized))))
         ((string= role "assistant")
          (when-let ((content-blocks (harp--openai-assistant-content content)))
            (push `(("role" . "assistant")
                    ("content" . ,content-blocks))
                  normalized)))
         (t
          (let ((text (harp--openai-content-text content)))
            (when (and text (not (string-empty-p text)))
              (push `(("role" . ,role)
                      ("content" . ,(harp--openai-wrap-content role text)))
                    normalized)))))))
    (nreverse normalized)))

(defun harp--openai-request (messages system tools)
  "Build OpenAI request body from MESSAGES, SYSTEM prompt, and TOOLS."
  (let* ((all-msgs (harp--openai-normalize-messages messages system))
         (body `(("model" . ,harp-model)
                 ("max_output_tokens" . ,harp-max-tokens)
                 ("stream" . t)
                 ("input" . ,(vconcat all-msgs)))))
    (when tools
      (push `("tools" . ,(vconcat (mapcar #'harp--tool-to-openai tools))) body))
    body))

(defun harp--tool-to-openai (tool)
  "Convert Anthropic TOOL format to OpenAI Responses API function format."
  `(("type" . "function")
    ("name" . ,(alist-get 'name tool))
    ("description" . ,(alist-get 'description tool))
    ("parameters" . ,(alist-get 'input_schema tool))))

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
   :endpoint "https://api.openai.com/v1/responses"
   :api-key-fn (lambda () harp-api-key-openai)
   :headers-fn #'harp--openai-headers
   :request-fn #'harp--openai-request
   :tools-fn (lambda (tools) (mapcar #'harp--tool-to-openai tools)))
  "OpenAI provider instance.")

(defun harp-get-provider ()
  "Get the current provider instance."
  (let ((provider (or (alist-get harp-model harp-model-provider-alist nil nil #'string=)
                      harp-default-provider)))
    (pcase provider
    ('anthropic harp-provider-anthropic)
    ('openai harp-provider-openai)
    (_ (error "Unknown provider: %s" provider)))))

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

(defvar-local harp--openai-text-seen nil
  "Non-nil when OpenAI Responses stream emitted text content.")

(defvar-local harp--stream-process-pos nil
  "Position after headers for the current streaming response.")

(defvar-local harp--stream-provider nil
  "Provider for the current streaming response.")

(defvar-local harp--stream-on-event nil
  "On-event callback for the current streaming response.")

(defvar-local harp--stream-on-done nil
  "On-done callback for the current streaming response.")

(defvar-local harp--stream-done-called nil
  "Non-nil when on-done has been called for the current stream.")

(defvar-local harp--stream-orig-filter nil
  "Original process filter for the current streaming response.")

(defun harp--stream-call-on-done (payload)
  "Call on-done with PAYLOAD if configured."
  (when harp--stream-on-done
    (funcall harp--stream-on-done payload)))

(defun harp--stream-finalize-error (err)
  "Finalize the stream with ERR."
  (unless harp--stream-done-called
    (setq harp--stream-done-called t)
    (harp--stream-call-on-done `((error . ,err)))))

(defun harp--stream-finalize-success ()
  "Finalize the stream successfully."
  (unless harp--stream-done-called
    (setq harp--stream-done-called t)
    (harp--stream-call-on-done (harp--response-payload))))

(defun harp--stream-ensure-process-pos ()
  "Ensure `harp--stream-process-pos' is set after headers."
  (unless harp--stream-process-pos
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
        (setq harp--stream-process-pos (point))))))

(defun harp--stream-handle-event (event)
  "Handle a parsed streaming EVENT."
  (when harp--stream-on-event
    (funcall harp--stream-on-event event))
  (pcase (car event)
    ('error (harp--stream-finalize-error (cdr event)))
    ('done (harp--stream-finalize-success))))

(defun harp--stream-parse-events ()
  "Parse any new SSE events from the response buffer."
  (let* ((new-content (buffer-substring harp--stream-process-pos (point-max)))
         (events (harp--process-stream-chunk new-content harp--stream-provider)))
    (when events
      (message "harp: parsed %d events" (length events)))
    (setq harp--stream-process-pos (point-max))
    (dolist (event events)
      (harp--stream-handle-event event))))

(defun harp--stream-process-filter (proc chunk)
  "Process filter for streaming responses."
  (when harp--stream-orig-filter
    (funcall harp--stream-orig-filter proc chunk))
  (message "harp: received chunk (%d bytes)" (length chunk))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (harp--stream-ensure-process-pos)
      (when harp--stream-process-pos
        (harp--stream-parse-events)))))

(defun harp--reset-response-state (&optional streaming)
  "Reset response state for a new request.
When STREAMING is non-nil, also reset stream buffer state."
  (setq harp--current-content "")
  (setq harp--tool-calls nil)
  (setq harp--current-tool-call nil)
  (setq harp--openai-text-seen nil)
  (when streaming
    (setq harp--stream-buffer "")))

(defun harp--response-payload ()
  "Build the standard on-done response payload."
  `((content . ,harp--current-content)
    (tool-calls . ,(harp--normalize-tool-calls
                    (nreverse harp--tool-calls)))))

(defun harp--msg-get (key msg)
  "Return KEY from MSG, handling symbol or string keys."
  (if (symbolp key)
      (or (alist-get key msg)
          (alist-get (symbol-name key) msg nil nil #'string=))
    (or (alist-get key msg nil nil #'string=)
        (alist-get (intern key) msg))))

(defun harp--normalize-tool-calls (tool-calls)
  "Normalize TOOL-CALLS by parsing string inputs into JSON objects."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false nil)
        (json-null nil))
    (mapcar
     (lambda (tc)
       (let ((input (alist-get 'input tc)))
         (when (stringp input)
           (condition-case nil
               (setf (alist-get 'input tc)
                     (json-read-from-string input))
             (error nil)))
         tc))
     tool-calls)))

(defun harp--extract-error-message (body)
  "Extract an error message from BODY if it is JSON."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false nil)
        (json-null nil))
    (condition-case nil
        (let* ((payload (json-read-from-string body))
               (err (alist-get 'error payload)))
          (cond
           ((stringp err) err)
           ((and (listp err) (alist-get 'message err))
            (alist-get 'message err))
           ((and (listp err) (alist-get 'type err))
            (alist-get 'type err))
           ((and (alist-get 'message payload)
                 (string= (alist-get 'type payload) "error"))
            (alist-get 'message payload))
           (t nil)))
      (error nil))))

(defun harp--parse-sse-line (line _provider)
  "Parse a single SSE LINE for _PROVIDER, return event data or nil."
  (when (string-prefix-p "data:" line)
    (let ((data (string-trim-left (substring line 5))))
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

(defun harp--openai-find-tool-call (tool-id)
  "Find tool call by TOOL-ID in the current OpenAI response."
  (cl-find-if (lambda (tc)
                (string= (alist-get 'id tc) tool-id))
              harp--tool-calls))

(defun harp--openai-ensure-tool-call (tool-id name)
  "Ensure a tool call exists for TOOL-ID and NAME."
  (or (harp--openai-find-tool-call tool-id)
      (let ((tc `((id . ,tool-id) (name . ,name) (input . ""))))
        (push tc harp--tool-calls)
        tc)))

(defun harp--handle-openai-chat-event (event)
  "Handle OpenAI chat completions streaming EVENT."
  (let* ((err (alist-get 'error event))
         (choices (alist-get 'choices event))
         (choice (and choices (> (length choices) 0) (aref choices 0)))
         (delta (alist-get 'delta choice))
         (finish (alist-get 'finish_reason choice)))
    (cond
     (err
      (cons 'error (or (alist-get 'message err) (format "%s" err))))
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

(defun harp--handle-openai-response-event (event)
  "Handle OpenAI Responses API streaming EVENT."
  (let ((type (alist-get 'type event)))
    (pcase type
      ("error"
       (let ((err (alist-get 'error event)))
         (cons 'error (or (alist-get 'message err) (format "%s" err)))))
      ("response.output_text.delta"
       (let ((text (alist-get 'delta event)))
         (when (stringp text)
           (setq harp--openai-text-seen t)
           (setq harp--current-content
                 (concat harp--current-content text))
           (cons 'text text))))
      ("response.output_text.done"
       (let ((text (or (alist-get 'text event)
                       (alist-get 'content event)
                       (alist-get 'delta event))))
         (when (and (stringp text)
                    (not harp--openai-text-seen))
           (setq harp--openai-text-seen t)
           (setq harp--current-content
                 (concat harp--current-content text))
           (cons 'text text))))
      ("response.output_text.added"
       (let ((text (or (alist-get 'text event)
                       (alist-get 'content event))))
         (when (and (stringp text)
                    (not harp--openai-text-seen))
           (setq harp--openai-text-seen t)
           (setq harp--current-content
                 (concat harp--current-content text))
           (cons 'text text))))
      ("response.output_item.added"
       (let ((item (alist-get 'item event)))
         (when (and (listp item)
                    (string= (alist-get 'type item) "function_call"))
           (let* ((tool-id (alist-get 'id item))
                  (name (alist-get 'name item))
                  (args (or (alist-get 'arguments item) "")))
             (let ((tc (harp--openai-ensure-tool-call tool-id name)))
               (setf (alist-get 'input tc) args))))))
      ("response.function_call_arguments.delta"
       (let* ((tool-id (alist-get 'item_id event))
              (delta (alist-get 'delta event)))
         (when (and tool-id (stringp delta))
           (let ((tc (harp--openai-ensure-tool-call tool-id nil)))
             (setf (alist-get 'input tc)
                   (concat (alist-get 'input tc) delta))))))
      ("response.function_call_arguments.done"
       (let* ((tool-id (alist-get 'item_id event))
              (args (or (alist-get 'arguments event) "")))
         (when tool-id
           (let ((tc (harp--openai-ensure-tool-call tool-id nil)))
             (setf (alist-get 'input tc) args)
             (cons 'tool-call tc)))))
      ("response.output_tool_call.delta"
       (let* ((tool-id (or (alist-get 'item_id event) (alist-get 'id event)))
              (delta (or (alist-get 'delta event)
                         (alist-get 'arguments event))))
         (when (and tool-id (stringp delta))
           (let ((tc (harp--openai-ensure-tool-call tool-id nil)))
             (setf (alist-get 'input tc)
                   (concat (alist-get 'input tc) delta))))))
      ("response.output_tool_call.done"
       (let* ((tool-id (or (alist-get 'item_id event) (alist-get 'id event)))
              (args (or (alist-get 'arguments event) "")))
         (when tool-id
           (let ((tc (harp--openai-ensure-tool-call tool-id nil)))
             (setf (alist-get 'input tc) args)
             (cons 'tool-call tc)))))
      ("response.completed"
       (cons 'done t))
      (_ nil))))

(defun harp--handle-openai-event (event)
  "Handle OpenAI streaming EVENT, return (type . data)."
  (if (alist-get 'type event)
      (harp--handle-openai-response-event event)
    (harp--handle-openai-chat-event event)))

(defun harp--process-stream-chunk (chunk provider)
  "Process a CHUNK of SSE data for PROVIDER."
  (setq harp--stream-buffer (concat harp--stream-buffer chunk))
  (let ((lines (split-string harp--stream-buffer "\n"))
        (events nil))
    ;; Keep incomplete last line in buffer
    (setq harp--stream-buffer (car (last lines)))
    (setq lines (butlast lines))
    (dolist (line lines)
      (when harp-debug-sse
        (message "harp: sse line: %s" line))
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

(defun harp-api-call-streaming (messages system tools on-event on-done)
  "Call LLM API with streaming, processing events as they arrive.
MESSAGES, SYSTEM, TOOLS are used to build the request.
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
         (response-buffer nil))
    (message "harp: calling %s with model %s" url harp-model)
    ;; Reset state
    (harp--reset-response-state t)
    (setq response-buffer
          (url-retrieve
           url
           (lambda (status)
             (message "harp: url-retrieve callback, status=%S" status)
             (message "harp: response buffer contents (first 500 chars): %s"
                      (buffer-substring (point-min) (min (point-max) 500)))
             ;; Fallback: call on-done if not already called via stream
             (unless harp--stream-done-called
               (setq harp--stream-done-called t)
               (if-let ((err (plist-get status :error)))
                   (harp--stream-finalize-error (format "%s" err))
                 (let* ((body-start (when (re-search-forward "\n\n" nil t)
                                      (point)))
                        (body (when body-start
                                (buffer-substring body-start (point-max))))
                        (api-err (and body (harp--extract-error-message body))))
                   (if api-err
                       (harp--stream-finalize-error api-err)
                     (harp--stream-finalize-success))))))
           nil t t))
    (with-current-buffer response-buffer
      (setq-local harp--stream-process-pos nil)
      (setq-local harp--stream-provider provider)
      (setq-local harp--stream-on-event on-event)
      (setq-local harp--stream-on-done on-done)
      (setq-local harp--stream-done-called nil))
    ;; Set up process filter for incremental parsing
    (when-let ((proc (get-buffer-process response-buffer)))
      (set-process-query-on-exit-flag proc nil)
      (setq-local harp--stream-orig-filter (process-filter proc))
      (set-process-filter proc #'harp--stream-process-filter))))
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
