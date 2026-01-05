;;; harp-tools.el --- Tool definitions for harp -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools that the LLM can invoke: file operations, shell commands, search.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Tool registry

(defvar harp-tools-alist nil
  "Alist of (name . handler) for registered tools.")

(defvar harp-tools-schemas nil
  "List of tool schemas for API calls.")

(defun harp-register-tool (name description input-schema handler)
  "Register a tool with NAME, DESCRIPTION, INPUT-SCHEMA, and HANDLER function."
  (setf (alist-get name harp-tools-alist nil nil #'string=) handler)
  (let ((schema `((name . ,name)
                  (description . ,description)
                  (input_schema . ,input-schema))))
    ;; Update or add schema
    (setq harp-tools-schemas
          (cons schema
                (cl-remove-if (lambda (s) (string= (alist-get 'name s) name))
                              harp-tools-schemas)))))

(defun harp-execute-tool (name input)
  "Execute tool NAME with INPUT, return result string."
  (if-let ((handler (alist-get name harp-tools-alist nil nil #'string=)))
      (condition-case err
          (funcall handler input)
        (error (format "Error executing %s: %s" name (error-message-string err))))
    (format "Unknown tool: %s" name)))

(defun harp-get-tool-schemas ()
  "Return list of tool schemas for API calls."
  harp-tools-schemas)

;;; File display hook

(defvar harp-file-display-hook nil
  "Hook called with filepath when a file is accessed.
Used to display files in the file pane.")

(defun harp--notify-file-access (filepath)
  "Notify that FILEPATH was accessed."
  (run-hook-with-args 'harp-file-display-hook filepath))

;;; Tool: read_file

(harp-register-tool
 "read_file"
 "Read the contents of a file at the given path."
 '((type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Absolute path to the file to read")))))
   (required . ["path"]))
 (lambda (input)
   (let ((path (alist-get 'path input)))
     (harp--notify-file-access path)
     (if (file-exists-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (buffer-string))
       (format "File not found: %s" path)))))

;;; Tool: write_file

(harp-register-tool
 "write_file"
 "Write content to a file, creating it if it doesn't exist."
 '((type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Absolute path to the file to write")))
                  (content . ((type . "string")
                              (description . "Content to write to the file")))))
   (required . ["path" "content"]))
 (lambda (input)
   (let ((path (alist-get 'path input))
         (content (alist-get 'content input)))
     (make-directory (file-name-directory path) t)
     (with-temp-file path
       (insert content))
     (harp--notify-file-access path)
     ;; Revert buffer if open
     (when-let ((buf (find-buffer-visiting path)))
       (with-current-buffer buf
         (revert-buffer t t t)))
     (format "Wrote %d bytes to %s" (length content) path))))

;;; Tool: edit_file

(harp-register-tool
 "edit_file"
 "Edit a file by replacing old_string with new_string. The old_string must match exactly."
 '((type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Absolute path to the file to edit")))
                  (old_string . ((type . "string")
                                 (description . "Exact string to find and replace")))
                  (new_string . ((type . "string")
                                 (description . "String to replace old_string with")))))
   (required . ["path" "old_string" "new_string"]))
 (lambda (input)
   (let ((path (alist-get 'path input))
         (old-string (alist-get 'old_string input))
         (new-string (alist-get 'new_string input)))
     (harp--notify-file-access path)
     (if (not (file-exists-p path))
         (format "File not found: %s" path)
       (let ((content (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))
         (if (not (string-match-p (regexp-quote old-string) content))
             (format "old_string not found in %s" path)
           (let ((new-content (replace-regexp-in-string
                               (regexp-quote old-string)
                               new-string
                               content t t)))
             (with-temp-file path
               (insert new-content))
             ;; Revert buffer if open
             (when-let ((buf (find-buffer-visiting path)))
               (with-current-buffer buf
                 (revert-buffer t t t)))
             (format "Edited %s: replaced %d chars with %d chars"
                     path (length old-string) (length new-string)))))))))

;;; Tool: run_shell

(harp-register-tool
 "run_shell"
 "Execute a shell command and return its output."
 '((type . "object")
   (properties . ((command . ((type . "string")
                              (description . "Shell command to execute")))
                  (cwd . ((type . "string")
                          (description . "Working directory for the command (optional)")))))
   (required . ["command"]))
 (lambda (input)
   (let* ((command (alist-get 'command input))
          (cwd (or (alist-get 'cwd input) default-directory))
          (default-directory cwd))
     (with-temp-buffer
       (let ((exit-code (call-process-shell-command command nil t nil)))
         (format "Exit code: %d\n%s" exit-code (buffer-string)))))))

;;; Tool: glob

(harp-register-tool
 "glob"
 "Find files matching a glob pattern."
 '((type . "object")
   (properties . ((pattern . ((type . "string")
                              (description . "Glob pattern like **/*.el or src/*.js")))
                  (path . ((type . "string")
                           (description . "Base directory to search in (optional)")))))
   (required . ["pattern"]))
 (lambda (input)
   (let* ((pattern (alist-get 'pattern input))
          (path (or (alist-get 'path input) default-directory))
          (default-directory path)
          (files (file-expand-wildcards pattern t)))
     (if files
         (mapconcat #'identity files "\n")
       "No files found matching pattern"))))

;;; Tool: grep

(harp-register-tool
 "grep"
 "Search for a pattern in files using grep."
 '((type . "object")
   (properties . ((pattern . ((type . "string")
                              (description . "Regex pattern to search for")))
                  (path . ((type . "string")
                           (description . "Directory or file to search in (optional)")))
                  (glob . ((type . "string")
                           (description . "File glob pattern to filter files (optional)")))))
   (required . ["pattern"]))
 (lambda (input)
   (let* ((pattern (alist-get 'pattern input))
          (path (or (alist-get 'path input) default-directory))
          (glob (alist-get 'glob input))
          (cmd (concat "grep -rn "
                       (when glob (format "--include='%s' " glob))
                       (shell-quote-argument pattern)
                       " "
                       (shell-quote-argument path))))
     (with-temp-buffer
       (call-process-shell-command cmd nil t nil)
       (if (= (buffer-size) 0)
           "No matches found"
         (buffer-string))))))

;;; Tool: list_directory

(harp-register-tool
 "list_directory"
 "List contents of a directory."
 '((type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Directory path to list")))))
   (required . ["path"]))
 (lambda (input)
   (let ((path (alist-get 'path input)))
     (if (not (file-directory-p path))
         (format "Not a directory: %s" path)
       (mapconcat
        (lambda (f)
          (let ((full (expand-file-name f path)))
            (format "%s%s" f (if (file-directory-p full) "/" ""))))
        (directory-files path nil "^[^.]")
        "\n")))))

;;; Dangerous tools list

(defvar harp-dangerous-tools '("write_file" "edit_file" "run_shell")
  "List of tool names that require approval.")

(defun harp-tool-dangerous-p (name)
  "Return non-nil if tool NAME is dangerous and requires approval."
  (member name harp-dangerous-tools))

(provide 'harp-tools)
;;; harp-tools.el ends here
