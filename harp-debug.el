;;; harp-debug.el --- Debug logging for harp -*- lexical-binding: t -*-

;;; Commentary:
;; Lightweight debug logging helpers for harp components.

;;; Code:

(defgroup harp-debug nil
  "Debug logging settings for harp."
  :group 'harp)

(defcustom harp-debug-level nil
  "Current debug level for harp logging.
Supported values: nil, info, verbose."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Info" info)
                 (const :tag "Verbose" verbose))
  :group 'harp-debug)

(defcustom harp-debug-log-file nil
  "Optional file path to append debug logs to.
When nil, logs only go to *Messages*."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Log file path"))
  :group 'harp-debug)

(defcustom harp-debug-log-backtrace nil
  "When non-nil, include a backtrace in logs for tool errors."
  :type 'boolean
  :group 'harp-debug)

(defcustom harp-debug-dump-directory "/tmp/harp-logs"
  "Directory to write debug dumps into."
  :type 'string
  :group 'harp-debug)

(defcustom harp-debug-auto-dump nil
  "When non-nil, dump buffers/logs after each assistant response."
  :type 'boolean
  :group 'harp-debug)

(defun harp-debug--level-value (level)
  "Return numeric value for LEVEL."
  (pcase level
    ('verbose 2)
    ('info 1)
    (_ 0)))

(defun harp-debug--enabled-p (level)
  "Return non-nil if LEVEL should be logged."
  (>= (harp-debug--level-value level)
      (harp-debug--level-value harp-debug-level)))

(defun harp-debug-log (level fmt &rest args)
  "Log a debug message at LEVEL with FMT and ARGS."
  (when (harp-debug--enabled-p level)
    (let* ((msg (apply #'format fmt args))
           (line (format "[harp][%s] %s"
                         (or (and level (symbol-name level)) "info")
                         msg)))
      (message "%s" line)
      (when (stringp harp-debug-log-file)
        (condition-case nil
            (with-temp-buffer
              (insert (format-time-string "%Y-%m-%d %H:%M:%S "))
              (insert line)
              (insert "\n")
              (append-to-file (point-min) (point-max) harp-debug-log-file))
          (error nil))))))

(defun harp-debug--sanitize-label (label)
  "Return a filesystem-safe LABEL."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (or label "")))

(defun harp-debug--write-buffer (buffer path)
  "Write BUFFER contents to PATH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (write-region (point-min) (point-max) path nil 'silent))))

(defun harp-debug--write-file (source path)
  "Copy SOURCE file to PATH if it exists."
  (when (and (stringp source) (file-exists-p source))
    (copy-file source path t t t t)))

(defun harp-debug-dump-state (&optional reason)
  "Dump chat/messages/log buffers for debugging.
REASON is an optional label for the dump directory."
  (interactive "sReason (optional): ")
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (label (harp-debug--sanitize-label reason))
         (dir (expand-file-name
               (concat stamp (when (and label (not (string-empty-p label)))
                               (concat "-" label)))
               harp-debug-dump-directory)))
    (make-directory dir t)
    (when (and (boundp 'harp-chat-buffer-name)
               (stringp harp-chat-buffer-name))
      (when-let ((buf (get-buffer harp-chat-buffer-name)))
        (harp-debug--write-buffer buf (expand-file-name "harp-chat.txt" dir))))
    (when-let ((buf (get-buffer "*Messages*")))
      (harp-debug--write-buffer buf (expand-file-name "messages.txt" dir)))
    (harp-debug--write-file harp-debug-log-file
                            (expand-file-name "harp-debug.log" dir))
    (dolist (buf (buffer-list))
      (when (string-match-p "^\\*http.*api\\." (buffer-name buf))
        (harp-debug--write-buffer
         buf
         (expand-file-name
          (format "http-%s.txt" (harp-debug--sanitize-label (buffer-name buf)))
          dir))))
    dir))

(defun harp-debug-maybe-dump-state (&optional reason)
  "Dump state when `harp-debug-auto-dump' is non-nil."
  (when harp-debug-auto-dump
    (harp-debug-dump-state reason)))

(provide 'harp-debug)
;;; harp-debug.el ends here
