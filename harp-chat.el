;;; harp-chat.el --- Chat buffer interface for harp -*- lexical-binding: t -*-

;; Author: Job Chong
;; URL: https://github.com/jobchong/harp
;; Part of harp.el

;;; Commentary:
;; Provides the chat buffer major mode and interaction handling.
;; Users type messages and press RET to send.  No minibuffer interaction needed.

;;; Code:

(require 'harp-api)
(require 'harp-tools)
(require 'harp-approval)
(require 'harp-context)
(require 'harp-skills)
(require 'harp-debug)
(require 'seq)
(require 'project)

;;; Customization

(defgroup harp-chat nil
  "Chat interface settings for harp."
  :group 'harp)

(defcustom harp-chat-max-tool-calls 3
  "Maximum external tool calls allowed per user request."
  :type 'integer
  :group 'harp-chat)

(defcustom harp-chat-listing-limit 1
  "Maximum directory listing calls per user request."
  :type 'integer
  :group 'harp-chat)

(defface harp-user-face
  '((((class color) (background light))
     :foreground "#0066cc" :weight bold)
    (((class color) (background dark))
     :foreground "#6eb5ff" :weight bold)
    (t :weight bold))
  "Face for user messages."
  :group 'harp-chat)

(defface harp-assistant-face
  '((((class color) (background light))
     :foreground "#7c3aed" :weight bold)
    (((class color) (background dark))
     :foreground "#a78bfa" :weight bold)
    (t :weight bold))
  "Face for assistant messages."
  :group 'harp-chat)

(defface harp-tool-face
  '((((class color) (background light))
     :foreground "#047857" :slant italic)
    (((class color) (background dark))
     :foreground "#34d399" :slant italic)
    (t :slant italic))
  "Face for tool calls and results."
  :group 'harp-chat)

(defface harp-status-face
  '((((class color) (background light))
     :foreground "#6b7280")
    (((class color) (background dark))
     :foreground "#9ca3af"))
  "Face for assistant status lines."
  :group 'harp-chat)

(defface harp-prompt-face
  '((((class color) (background light))
     :foreground "#0066cc" :weight bold)
    (((class color) (background dark))
     :foreground "#6eb5ff" :weight bold)
    (t :weight bold))
  "Face for the input prompt."
  :group 'harp-chat)

(defface harp-separator-face
  '((((class color) (background light))
     :foreground "#d1d5db")
    (((class color) (background dark))
     :foreground "#4b5563"))
  "Face for separators."
  :group 'harp-chat)

(defface harp-approval-face
  '((((class color) (background light))
     :foreground "#d97706" :weight bold)
    (((class color) (background dark))
     :foreground "#fbbf24" :weight bold)
    (t :inherit warning :weight bold))
  "Face for approval prompts."
  :group 'harp-chat)

(defface harp-code-face
  '((((class color) (background light))
     :inherit fixed-pitch :background "#f3f4f6" :foreground "#1f2937")
    (((class color) (background dark))
     :inherit fixed-pitch :background "#374151" :foreground "#f9fafb"))
  "Face for inline code."
  :group 'harp-chat)

(defface harp-code-block-face
  '((((class color) (background light))
     :inherit fixed-pitch :background "#f9fafb" :extend t)
    (((class color) (background dark))
     :inherit fixed-pitch :background "#1f2937" :extend t))
  "Face for code blocks."
  :group 'harp-chat)

(defface harp-file-link-face
  '((((class color) (background light))
     :foreground "#2563eb" :underline t)
    (((class color) (background dark))
     :foreground "#60a5fa" :underline t))
  "Face for clickable file links."
  :group 'harp-chat)

(defface harp-file-modified-face
  '((((class color) (background light))
     :foreground "#ea580c" :underline t :weight bold)
    (((class color) (background dark))
     :foreground "#fb923c" :underline t :weight bold))
  "Face for files that were modified."
  :group 'harp-chat)

(defface harp-header-face
  '((((class color) (background light))
     :foreground "#7c3aed" :weight bold :height 1.4)
    (((class color) (background dark))
     :foreground "#a78bfa" :weight bold :height 1.4))
  "Face for the header title."
  :group 'harp-chat)

(defface harp-header-subtitle-face
  '((((class color) (background light))
     :foreground "#6b7280" :height 0.9)
    (((class color) (background dark))
     :foreground "#9ca3af" :height 0.9))
  "Face for header subtitle text."
  :group 'harp-chat)

(defface harp-success-face
  '((((class color) (background light))
     :foreground "#059669" :weight bold)
    (((class color) (background dark))
     :foreground "#34d399" :weight bold))
  "Face for success messages."
  :group 'harp-chat)

(defface harp-error-face
  '((((class color) (background light))
     :foreground "#dc2626" :weight bold)
    (((class color) (background dark))
     :foreground "#f87171" :weight bold))
  "Face for error messages."
  :group 'harp-chat)

;;; File link support

;; Forward declaration for byte-compiler
(defvar harp-chat--file-buffer)

(defvar harp-file-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'harp-file-link-open)
    (define-key map [mouse-2] #'harp-file-link-open)
    (define-key map (kbd "RET") #'harp-file-link-open)
    (define-key map (kbd "C-c C-o") #'harp-file-link-open)
    map)
  "Keymap active on file links in chat buffer.")

(defun harp-file-link-open (&optional event)
  "Open file link at point or mouse EVENT location."
  (interactive (list last-input-event))
  (let* ((pos (if (and event (mouse-event-p event))
                  (posn-point (event-end event))
                (point)))
         (file (get-text-property pos 'harp-file))
         (line (get-text-property pos 'harp-line))
         (chat-buffer (current-buffer)))
    (when file
      (let ((file-window (and (boundp 'harp-chat--file-buffer)
                              harp-chat--file-buffer
                              (get-buffer-window harp-chat--file-buffer))))
        (if file-window
            ;; Use the existing file pane
            (progn
              (select-window file-window)
              (find-file file)
              (let ((new-buffer (current-buffer)))
                (with-current-buffer chat-buffer
                  (setq harp-chat--file-buffer new-buffer)))
              (when line
                (goto-char (point-min))
                (forward-line (1- line))))
          ;; Open in other window
          (find-file-other-window file)
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))))))

(defconst harp-file-path-regexp
  (rx (group (or (seq (or "/" "~/" "./" "../")
                      (+ (any alnum "_/.-")))
                 (seq (+ (any alnum "_-"))
                      (or "/" ".")
                      (+ (any alnum "_/.-")))))
      (optional ":" (group (+ digit))
                (optional ":" (group (+ digit)))))
  "Regexp matching file paths with optional :line:col suffix.")

(defun harp-chat--linkify-file-paths (start end &optional modified)
  "Make file paths in region START to END clickable.
If MODIFIED is non-nil, use `harp-file-modified-face'."
  (save-excursion
    (goto-char start)
    (while (re-search-forward harp-file-path-regexp end t)
      (let* ((path (match-string 1))
             (line (and (match-string 2)
                        (string-to-number (match-string 2))))
             (beg (match-beginning 0))
             (match-end (match-end 0))
             (expanded (expand-file-name path)))
        (when (file-exists-p expanded)
          (add-text-properties
           beg match-end
           `(face ,(if modified 'harp-file-modified-face 'harp-file-link-face)
                  mouse-face highlight
                  keymap ,harp-file-link-keymap
                  harp-file ,expanded
                  harp-line ,line
                  help-echo ,(format "Click or RET to open %s%s"
                                     expanded
                                     (if line (format ":%d" line) "")))))))))

;;; Markdown highlighting

(defface harp-code-lang-face
  '((((class color) (background light))
     :foreground "#6b7280" :weight bold :height 0.85)
    (((class color) (background dark))
     :foreground "#9ca3af" :weight bold :height 0.85))
  "Face for code block language labels."
  :group 'harp-chat)

(defun harp-chat--highlight-markdown (start end)
  "Apply markdown syntax highlighting to region from START to END."
  (save-excursion
    ;; Fenced code blocks: ```lang\n...\n```
    (goto-char start)
    (while (re-search-forward "```\\([a-zA-Z0-9]*\\)?\n\\(\\(?:.\\|\n\\)*?\\)```" end t)
      (let* ((fence-start (match-beginning 0))
             (lang (match-string 1))
             (code-start (match-beginning 2))
             (code-end (match-end 2)))
        ;; Hide the opening fence and add language label
        (when (and lang (not (string-empty-p lang)))
          (add-text-properties fence-start (1+ fence-start)
                               `(display ,(propertize (format "─── %s " lang)
                                                      'face 'harp-code-lang-face))))
        ;; Apply code block face
        (add-text-properties code-start code-end
                             '(face harp-code-block-face))
        ;; Try language-specific highlighting if available
        (when (and lang (not (string-empty-p lang)))
          (harp-chat--fontify-code-block lang code-start code-end))))
    ;; Inline code: `code`
    (goto-char start)
    (while (re-search-forward "`\\([^`\n]+\\)`" end t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face harp-code-face)))
    ;; Linkify file paths in prose
    (harp-chat--linkify-file-paths start end)))

(defun harp-chat--fontify-code-block (lang start end)
  "Apply fontification for LANG to code block from START to END."
  (let* ((mode-name (intern (concat lang "-mode")))
         (mode (and (fboundp mode-name) mode-name))
         (target-buffer (current-buffer)))
    (when mode
      (condition-case nil
          (let ((code (buffer-substring-no-properties start end)))
            (with-temp-buffer
              (insert code)
              (delay-mode-hooks (funcall mode))
              (font-lock-ensure)
              (let ((pos (point-min)))
                (while (< pos (point-max))
                  (let ((next (next-single-property-change pos 'face nil (point-max)))
                        (face-val (get-text-property pos 'face)))
                    (when face-val
                      (with-current-buffer target-buffer
                        (add-face-text-property
                         (+ start (1- pos))
                         (+ start (1- next))
                         face-val nil)))
                    (setq pos next))))))
        (error nil)))))

;;; File link navigation

(defun harp-chat-next-file-link ()
  "Jump to the next file link in the chat buffer."
  (interactive)
  (let ((pos (next-single-property-change (point) 'harp-file)))
    (if pos
        (progn
          (goto-char pos)
          (message "%s" (get-text-property pos 'help-echo)))
      (message "No more file links"))))

(defun harp-chat-prev-file-link ()
  "Jump to the previous file link in the chat buffer."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'harp-file)))
    (if pos
        (let ((link-start (or (previous-single-property-change pos 'harp-file) pos)))
          (goto-char link-start)
          (message "%s" (or (get-text-property link-start 'help-echo) "")))
      (message "No previous file links"))))

(defun harp-chat-list-file-links ()
  "List all file links in the current chat buffer."
  (interactive)
  (let ((links nil)
        (pos (point-min)))
    (while (setq pos (next-single-property-change pos 'harp-file))
      (when-let ((file (get-text-property pos 'harp-file)))
        (unless (member file (mapcar #'car links))
          (push (cons file (get-text-property pos 'harp-line)) links)))
      (setq pos (or (next-single-property-change pos 'harp-file) (point-max))))
    (if links
        (let* ((choices (mapcar (lambda (link)
                                  (format "%s%s"
                                          (car link)
                                          (if (cdr link)
                                              (format ":%d" (cdr link))
                                            "")))
                                (nreverse links)))
               (choice (completing-read "Open file: " choices nil t)))
          (when choice
            (let* ((parts (split-string choice ":"))
                   (file (car parts))
                   (line (and (cadr parts) (string-to-number (cadr parts)))))
              (find-file-other-window file)
              (when (and line (> line 0))
                (goto-char (point-min))
                (forward-line (1- line))))))
      (message "No file links in buffer"))))

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

(defvar-local harp-chat--status-updated nil
  "Non-nil when status has been updated via set_status.")

(defvar-local harp-chat--pending-tool-results nil
  "List of (tool-use-id result is-error) entries for pending tool results.")

(defvar-local harp-chat--current-tool-calls nil
  "Tool calls from the current response, for building assistant message.")

(defvar-local harp-chat--tool-usage-counts nil
  "Alist of (tool-name . count) for the current user request.")

(defvar-local harp-chat--tool-usage-total 0
  "Count of external tool calls for the current user request.")

(defvar-local harp-chat--response-start nil
  "Marker for the start of the current assistant response content.")

;;; Buffer name

(defconst harp-chat-buffer-name "*harp*"
  "Name of the harp chat buffer.")

;;; Mode definition

(defvar harp-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "RET") #'harp-chat-send-or-approve)
    (define-key map (kbd "TAB") #'harp-chat--slash-complete-or-indent)
    (define-key map (kbd "C-c C-c") #'harp-chat-send-or-approve)
    (define-key map (kbd "C-c C-k") #'harp-chat-cancel)
    ;; File link navigation
    (define-key map (kbd "M-n") #'harp-chat-next-file-link)
    (define-key map (kbd "M-p") #'harp-chat-prev-file-link)
    (define-key map (kbd "C-c C-o") #'harp-file-link-open)
    (define-key map (kbd "C-c C-l") #'harp-chat-list-file-links)
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
  (setq-local harp-chat--tool-usage-counts nil)
  (setq-local harp-chat--tool-usage-total 0)
  (setq-local harp-chat--response-start (make-marker))
  (setq-local harp-chat--status-updated nil)
  ;; Set up approval hook
  (add-hook 'harp-approval-request-hook #'harp-chat--show-approval nil t)
  (add-hook 'completion-at-point-functions #'harp-chat--slash-skill-capf nil t)
  (add-hook 'post-command-hook #'harp-chat--maybe-trigger-slash-completion nil t))

;;; Display functions

(defun harp-chat--insert-separator ()
  "Insert a visual separator line."
  (insert (propertize "────────────────────────────────────────────────────"
                      'face 'harp-separator-face) "\n"))

(defun harp-chat--insert-prompt ()
  "Insert the input prompt."
  (harp-chat--insert-separator)
  (let ((inhibit-read-only t))
    (insert (propertize "❯ " 'face 'harp-prompt-face
                        'read-only t
                        'cursor-intangible t
                        'rear-nonsticky '(read-only cursor-intangible field)
                        'field 'harp-prompt)))
  (set-marker harp-chat--input-marker (point)))

(defun harp-chat--insert-user-message (text)
  "Insert user message TEXT into the chat buffer."
  (let ((inhibit-read-only t))
    (goto-char harp-chat--input-marker)
    (delete-region harp-chat--input-marker (point-max))
    (insert (propertize "● You\n" 'face 'harp-user-face))
    (insert text "\n\n")))

(defun harp-chat--insert-assistant-start ()
  "Insert the start of an assistant message and set up streaming marker."
  (insert (propertize "◆ Harp\n" 'face 'harp-assistant-face))
  (let ((start (point)))
    (insert (propertize "  ◌ thinking..." 'face 'harp-status-face))
    (let ((end (point)))
      (insert "\n")
      (set-marker harp-chat--status-start start)
      (set-marker harp-chat--status-end end)
      (setq harp-chat--status-text "thinking...")
      (setq harp-chat--status-updated nil)))
  (set-marker harp-chat--assistant-marker (point))
  (set-marker harp-chat--response-start (point)))

(defun harp-chat--status-symbol (text)
  "Return appropriate symbol for status TEXT."
  (cond
   ((string-match-p "thinking" text) "◌")
   ((string-match-p "drafting" text) "◐")
   ((string-match-p "running\\|tool" text) "◑")
   ((string-match-p "done\\|complete" text) "●")
   (t "◌")))

(defun harp-chat--set-status (text)
  "Update the status line to TEXT for the current assistant response."
  (when (and harp-chat--status-start harp-chat--status-end)
    (setq harp-chat--status-text text)
    (setq harp-chat--status-updated t)
    (save-excursion
      (goto-char harp-chat--status-start)
      (delete-region harp-chat--status-start harp-chat--status-end)
      (let ((symbol (harp-chat--status-symbol text)))
        (insert (propertize (format "  %s %s" symbol text) 'face 'harp-status-face)))
      (set-marker harp-chat--status-end (point)))))

(defun harp-chat--set-status-if-default (text)
  "Update the status line to TEXT when it is still on a default value."
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

(defun harp-chat--tool-icon (name)
  "Return an icon for tool NAME."
  (cond
   ((string-match-p "read\\|file" name) "📄")
   ((string-match-p "write\\|edit" name) "✏️")
   ((string-match-p "shell\\|run\\|exec" name) "⚡")
   ((string-match-p "glob\\|list\\|dir" name) "📁")
   ((string-match-p "grep\\|search" name) "🔍")
   (t "⚙️")))

(defun harp-chat--insert-tool-call (name input)
  "Insert a tool call display for NAME with INPUT."
  (if (string= name "set_status")
      (and (not harp-chat--status-updated)
           (when-let ((summary (harp-chat--status-from-input input)))
             (harp-chat--set-status (string-trim summary))))
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (let ((icon (harp-chat--tool-icon name)))
        (insert "\n  "
                (propertize (format "%s %s" icon name) 'face 'harp-tool-face)))
      (when input
        (let ((input-str (if (stringp input)
                             input
                           (json-encode input))))
          (when (> (length input-str) 200)
            (setq input-str (concat (substring input-str 0 200) "...")))
          (insert (propertize (format " %s" input-str) 'face 'harp-status-face))))
      (insert "\n")
      (set-marker harp-chat--assistant-marker (point)))))

(defun harp-chat--insert-tool-result (name result)
  "Insert tool RESULT display for tool NAME."
  (unless (string= name "set_status")
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (let* ((result-str (if (> (length result) 500)
                             (concat (substring result 0 500) "\n  ... [truncated]")
                           result))
             (modified-p (member name '("write_file" "edit_file")))
             (result-start (point))
             (is-error (string-match-p "\\`\\(Error\\|error\\|failed\\|Failed\\)" result))
             (icon (if is-error "✗" "✓"))
             (face (if is-error 'harp-error-face 'harp-success-face)))
        (insert "  " (propertize (format "%s result" icon) 'face face) "\n")
        (setq result-start (point))
        ;; Indent result text
        (let ((indented (replace-regexp-in-string "^" "    " result-str)))
          (insert indented "\n"))
        ;; Linkify file paths in the result
        (harp-chat--linkify-file-paths result-start (point) modified-p))
      (set-marker harp-chat--assistant-marker (point)))))

(defun harp-chat--show-approval ()
  "Display approval prompt for pending tool execution."
  (when-let ((pending (harp-approval-get-pending)))
    (let ((tool-name (car pending))
          (input (cadr pending)))
      (save-excursion
        (goto-char harp-chat--assistant-marker)
        (insert "\n  ╭─ "
                (propertize "Approval Required" 'face 'harp-approval-face)
                " ─────────────────────────────╮\n")
        (insert "  │ "
                (propertize (format "Tool: %s" tool-name) 'face 'harp-tool-face))
        (when input
          (let ((input-str (if (stringp input) input (json-encode input))))
            (when (> (length input-str) 200)
              (setq input-str (concat (substring input-str 0 200) "...")))
            (insert "\n  │ " (propertize input-str 'face 'harp-status-face))))
        (insert "\n  ╰─ "
                (propertize "Press " 'face 'harp-header-subtitle-face)
                (propertize "y" 'face 'harp-success-face)
                (propertize " to approve, " 'face 'harp-header-subtitle-face)
                (propertize "n" 'face 'harp-error-face)
                (propertize " to reject" 'face 'harp-header-subtitle-face)
                " ─────────╯\n")
        (set-marker harp-chat--assistant-marker (point))))
    (goto-char (point-max))
    (insert (propertize "❯ " 'face 'harp-prompt-face))
    (set-marker harp-chat--input-marker (point))
    (goto-char (point-max))))

(defun harp-chat--finish-response ()
  "Clean up after response is complete."
  (harp-chat--set-status-if-default "done")
  ;; Apply markdown highlighting to the completed response
  (when (and harp-chat--response-start harp-chat--assistant-marker)
    (harp-chat--highlight-markdown harp-chat--response-start
                                   harp-chat--assistant-marker))
  (harp-debug-maybe-dump-state "response")
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
  (harp-chat--slash-hide-completions)
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
    (harp-chat--reset-tool-usage)
    (harp-chat--insert-assistant-start)
    ;; Call API
    (harp-chat--call-api)))

(defun harp-chat--replace-last-user-message (content)
  "Replace the most recent user message content in history."
  (let ((msg (seq-find (lambda (m)
                         (string= (harp--msg-get 'role m) "user"))
                       harp-chat--messages)))
    (when msg
      (if (assoc "content" msg)
          (setf (alist-get "content" msg nil nil #'string=) content)
        (setf (alist-get 'content msg) content)))))

(defun harp-chat--slash-skills ()
  "Return discovered slash skills for the current directory."
  (when harp-context-include-slash-skills
    (let ((git-root (harp-context--git-root)))
      (harp-skills-discover default-directory git-root))))

(defun harp-chat--slash-token-bounds ()
  "Return (START . END) for the current slash token, or nil."
  (when (and harp-chat--input-marker
             (>= (point) harp-chat--input-marker))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "A-Za-z0-9_.:-")
                    (point)))
           (slash-pos (1- start)))
      (when (and (> start harp-chat--input-marker)
                 (eq (char-after slash-pos) ?/))
        (let ((before (when (> slash-pos harp-chat--input-marker)
                        (char-before slash-pos))))
          (when (or (= slash-pos harp-chat--input-marker)
                    (and before (string-match-p "[[:space:]]"
                                                (string before))))
            (cons start end)))))))

(defun harp-chat--slash-skill-capf ()
  "Completion at point for slash skills in the input area."
  (when-let ((bounds (harp-chat--slash-token-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (skills (harp-chat--slash-skills))
           (names (mapcar (lambda (skill)
                            (alist-get 'name skill))
                          skills)))
      (list start end (or names '()) :exclusive 'yes))))

(defvar-local harp-chat--slash-completion-timer nil
  "Idle timer for refreshing slash completion list.")

(defvar-local harp-chat--slash-completions-prefix nil
  "Last prefix used to render slash completions.")

(defvar-local harp-chat--slash-completions-candidates nil
  "Last candidate list rendered for slash completions.")

(defun harp-chat--slash-candidates (prefix)
  "Return slash completion candidates for PREFIX."
  (let ((skills (harp-chat--slash-skills))
        (cands '()))
    (dolist (skill skills (nreverse cands))
      (let ((name (alist-get 'name skill)))
        (when (and name (string-prefix-p prefix name))
          (push name cands))))))

(defun harp-chat--completion-replace (start end text)
  "Replace region between START and END with TEXT."
  (let ((inhibit-read-only t))
    (goto-char start)
    (delete-region start end)
    (insert text)))

(defun harp-chat--slash-hide-completions ()
  "Hide the completions window and clear cached slash completions."
  (setq harp-chat--slash-completions-prefix nil)
  (setq harp-chat--slash-completions-candidates nil)
  (minibuffer-hide-completions))

(defun harp-chat--slash-show-completions (start end prefix candidates)
  "Show slash completion CANDIDATES for PREFIX replacing START..END."
  (let* ((buffer (get-buffer-create "*Completions*"))
         (standard-output buffer)
         (completion-base-position (list start end))
         (completion-list-insert-choice-function #'harp-chat--completion-replace)
         (completion-show-help nil))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (display-completion-list candidates)
    (display-buffer buffer)
    (setq harp-chat--slash-completions-prefix prefix)
    (setq harp-chat--slash-completions-candidates candidates)))

(defun harp-chat--slash-refresh-current ()
  "Refresh the slash completion list for the current buffer state."
  (if (or (harp-approval-pending-p)
          (null harp-chat--input-marker)
          (< (point) harp-chat--input-marker))
      (harp-chat--slash-hide-completions)
    (if-let ((bounds (harp-chat--slash-token-bounds)))
        (let* ((start (car bounds))
               (end (cdr bounds))
               (prefix (buffer-substring-no-properties start end))
               (candidates (harp-chat--slash-candidates prefix)))
          (if (null candidates)
              (harp-chat--slash-hide-completions)
            (unless (and (equal prefix harp-chat--slash-completions-prefix)
                         (equal candidates harp-chat--slash-completions-candidates))
              (harp-chat--slash-show-completions start end prefix candidates))))
      (harp-chat--slash-hide-completions))))

(defun harp-chat--slash-refresh (buffer)
  "Refresh slash completions for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq harp-chat--slash-completion-timer nil)
      (harp-chat--slash-refresh-current))))

(defun harp-chat--maybe-trigger-slash-completion ()
  "Auto-refresh slash completion list when editing."
  (when harp-chat--slash-completion-timer
    (cancel-timer harp-chat--slash-completion-timer))
  (setq harp-chat--slash-completion-timer
        (run-with-idle-timer
         0.05 nil #'harp-chat--slash-refresh (current-buffer))))

(defun harp-chat--slash-complete-or-indent ()
  "Complete the current slash token to the first match, or indent."
  (interactive)
  (if-let ((bounds (harp-chat--slash-token-bounds)))
      (let* ((start (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties start end))
             (candidates (harp-chat--slash-candidates prefix)))
        (if (null candidates)
            (indent-for-tab-command)
          (harp-chat--completion-replace start end (car candidates))
          (harp-chat--slash-refresh-current)))
    (indent-for-tab-command)))

(defun harp-chat--call-api ()
  "Make API call with current messages."
  (let* ((context (harp-context-gather harp-chat--file-buffer))
         (messages (reverse harp-chat--messages))
         (last-user (car (last (seq-filter (lambda (m)
                                             (string= (harp--msg-get 'role m) "user"))
                                           messages))))
         (input (and last-user (harp--msg-get 'content last-user)))
         (skills (alist-get 'slash-skills context))
         (parsed (and skills
                      (harp-skills-parse-invocation
                       input skills harp-context-max-file-size)))
         (active (plist-get parsed :skill))
         (user-input (plist-get parsed :user-input))
         system
         tools)
    (when active
      (let ((final-input (if (string-empty-p user-input)
                             "Use the active slash skill."
                           user-input)))
        (harp-chat--replace-last-user-message final-input)
        (setq input final-input)
        (setf (alist-get 'active-skill context) active)
        (setq messages (reverse harp-chat--messages))))
    (setq system (harp-context-build-system-prompt context))
    (setq tools (if (and (harp-chat--should-use-tools input)
                         (harp-chat--tool-budget-remaining-p))
                    (harp-get-tool-schemas)
                  (when-let ((status-tool (harp-get-tool-schema "set_status")))
                    (list status-tool))))
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
                (concat "\n" (propertize (format "✗ Error: %s" err) 'face 'harp-error-face)))
               (harp-chat--finish-response))
           ;; Process tool calls if any
           (let* ((content (alist-get 'content result))
                  (tool-calls (alist-get 'tool-calls result))
                  (assistant-tool-calls (or harp-chat--current-tool-calls
                                            tool-calls))
                  (has-content (and (stringp content)
                                    (not (string-empty-p content))))
                  (has-external-tool
                   (seq-some
                    (lambda (tc)
                      (not (harp-tool-internal-p (alist-get 'name tc))))
                    tool-calls))
                  (has-internal-tool
                   (seq-some
                    (lambda (tc)
                      (harp-tool-internal-p (alist-get 'name tc)))
                    tool-calls)))
             ;; Add assistant message to history
             (push (harp-make-assistant-message content assistant-tool-calls)
                   harp-chat--messages)
             (cond
              (has-external-tool
               (harp-chat--execute-tools tool-calls))
              ((and has-internal-tool (not has-content))
               (harp-chat--execute-tools tool-calls))
              (t
               (harp-chat--finish-response))))))))))

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

(defun harp-chat--reset-tool-usage ()
  "Reset tool usage counts for a new user request."
  (setq harp-chat--tool-usage-counts nil)
  (setq harp-chat--tool-usage-total 0))

(defun harp-chat--tool-usage-count (name)
  "Return the usage count for tool NAME."
  (or (alist-get name harp-chat--tool-usage-counts nil nil #'string=) 0))

(defun harp-chat--tool-budget-remaining-p ()
  "Return non-nil if the tool budget has not been exhausted."
  (< harp-chat--tool-usage-total (max 0 harp-chat-max-tool-calls)))

(defun harp-chat--tool-input-command (input)
  "Extract a command string from tool INPUT if present."
  (cond
   ((and (listp input) (consp (car input))) (alist-get 'command input))
   ((listp input)
    (or (plist-get input :command)
        (plist-get input 'command)
        (plist-get input "command")))
   (t nil)))

(defun harp-chat--listing-command-p (command)
  "Return non-nil if COMMAND is a simple directory listing."
  (and (stringp command)
       (string-match-p "\\`[[:space:]]*ls\\b" command)))

(defun harp-chat--listing-tool-call-p (name input)
  "Return non-nil when NAME/INPUT represents a directory listing."
  (or (string= name "list_directory")
      (and (member name '("run_shell" "shell" "run_shell_command" "shell_command"))
           (harp-chat--listing-command-p (harp-chat--tool-input-command input)))))

(defun harp-chat--record-tool-usage (name &optional input)
  "Record that tool NAME with INPUT was used in the current request."
  (let ((current (harp-chat--tool-usage-count name)))
    (setf (alist-get name harp-chat--tool-usage-counts nil nil #'string=)
          (1+ current)))
  (unless (harp-tool-internal-p name)
    (setq harp-chat--tool-usage-total (1+ harp-chat--tool-usage-total))
    (when (harp-chat--listing-tool-call-p name input)
      (let ((listing (harp-chat--tool-usage-count "listing")))
        (setf (alist-get "listing" harp-chat--tool-usage-counts nil nil #'string=)
              (1+ listing))))))

(defun harp-chat--tool-skip-message (name input)
  "Return a skip message for tool NAME with INPUT when it should be throttled."
  (cond
   ((and (harp-chat--listing-tool-call-p name input)
         (>= (harp-chat--tool-usage-count "listing")
             (max 0 harp-chat-listing-limit)))
    "Skipped listing: already used in this request. Read README or a specific file instead.")
   ((and (not (harp-tool-internal-p name))
         (not (harp-chat--tool-budget-remaining-p)))
    "Skipped tool: budget reached for this request. Answer with current context or ask for a specific file.")
   (t nil)))

(defun harp-chat--execute-next-tool (remaining-tools)
  "Execute next tool in REMAINING-TOOLS list."
  (if (null remaining-tools)
      ;; All tools done, add results to messages and continue
      (harp-chat--tools-complete)
    (let* ((tc (car remaining-tools))
           (id (alist-get 'id tc))
           (name (alist-get 'name tc))
           (input (alist-get 'input tc)))
      (let ((skip-msg (harp-chat--tool-skip-message name input)))
        (if skip-msg
            (progn
              (harp-chat--insert-tool-result name skip-msg)
              (push (list id skip-msg nil) harp-chat--pending-tool-results)
              (harp-chat--execute-next-tool (cdr remaining-tools)))
          (harp-chat--record-tool-usage name input)
          (harp-approval-execute-with-approval
           name input
           (lambda (result)
             (with-current-buffer harp-chat-buffer-name
               (let* ((payload (if (and (listp result) (plist-member result :result))
                                   result
                                 (list :result (format "%s" result) :error nil)))
                      (result-str (plist-get payload :result))
                      (is-error (plist-get payload :error)))
                 (harp-chat--insert-tool-result name result-str)
                 (push (list id result-str is-error)
                       harp-chat--pending-tool-results))
               (harp-chat--execute-next-tool (cdr remaining-tools))))))))))

(defun harp-chat--tools-complete ()
  "Add tool results to messages and continue the agent loop."
  ;; Add tool results to messages
  (dolist (result harp-chat--pending-tool-results)
    (push (harp-make-tool-result (nth 0 result)
                                 (nth 1 result)
                                 (nth 2 result))
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
      (insert "  " (propertize "✓ Approved" 'face 'harp-success-face) "\n\n")
      (set-marker harp-chat--assistant-marker (point)))
    (harp-approval-respond t)))

(defun harp-chat-reject ()
  "Reject the pending tool execution."
  (interactive)
  (when (harp-approval-pending-p)
    (save-excursion
      (goto-char harp-chat--assistant-marker)
      (insert "  " (propertize "✗ Rejected" 'face 'harp-error-face) "\n\n")
      (set-marker harp-chat--assistant-marker (point)))
    (harp-approval-respond nil)))

(defun harp-chat-cancel ()
  "Cancel the current request."
  (interactive)
  (harp-cancel-request)
  (setq harp-chat--processing nil)
  (harp-chat--insert-streaming-text
   (concat "\n" (propertize "⊘ Cancelled" 'face 'harp-status-face) "\n"))
  (harp-chat--finish-response))

;;; Buffer setup

(defconst harp-chat--logo
  "  ╭─────────────────────────────────╮
  │  ♪  H A R P                     │
  │     AI Coding Assistant         │
  ╰─────────────────────────────────╯"
  "ASCII art logo for the chat buffer.")

(defun harp-chat-setup-buffer ()
  "Set up the chat buffer and return it."
  (let ((buf (get-buffer-create harp-chat-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'harp-chat-mode)
        (harp-chat-mode)
        (erase-buffer)
        ;; Insert logo
        (insert (propertize harp-chat--logo 'face 'harp-header-face) "\n\n")
        ;; Insert help hints
        (insert (propertize "  RET " 'face 'harp-prompt-face)
                (propertize "send message   " 'face 'harp-header-subtitle-face)
                (propertize "y/n " 'face 'harp-prompt-face)
                (propertize "approve/reject   " 'face 'harp-header-subtitle-face)
                (propertize "C-c C-k " 'face 'harp-prompt-face)
                (propertize "cancel\n\n" 'face 'harp-header-subtitle-face))
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
