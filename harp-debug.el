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

(provide 'harp-debug)
;;; harp-debug.el ends here
