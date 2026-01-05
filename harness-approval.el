;;; harness-approval.el --- Approval system for tool execution -*- lexical-binding: t -*-

;;; Commentary:
;; Configurable approval system for dangerous tool operations.
;; Supports global defaults and per-project overrides.

;;; Code:

(require 'project)

;; Forward declarations to avoid circular dependency
(declare-function harness-tool-dangerous-p "harness-tools")
(declare-function harness-execute-tool "harness-tools")

;;; Customization

(defgroup harness-approval nil
  "Approval settings for harness tool execution."
  :group 'harness)

(defcustom harness-approval-mode 'dangerous-only
  "Global approval mode for tool execution.
- `none': Auto-execute all tools without prompting
- `dangerous-only': Prompt for dangerous tools (write, edit, shell)
- `full': Prompt for every tool execution"
  :type '(choice (const :tag "No approval needed" none)
                 (const :tag "Approve dangerous tools only" dangerous-only)
                 (const :tag "Approve all tools" full))
  :group 'harness-approval)

(defcustom harness-project-approval-alist nil
  "Alist of (project-root . approval-mode) for per-project overrides.
Project root should be an absolute path string."
  :type '(alist :key-type string :value-type symbol)
  :group 'harness-approval)

;;; State

(defvar harness-approval--pending nil
  "Currently pending approval request: (tool-name input callback) or nil.")

(defvar harness-approval--response nil
  "Response to pending approval: `approved', `rejected', or nil.")

;;; Core functions

(defun harness-approval-get-mode ()
  "Get the approval mode for the current project, or global default."
  (let ((proj (project-current)))
    (if-let* ((root (and proj (project-root proj)))
              (override (alist-get root harness-project-approval-alist
                                   nil nil #'string=)))
        override
      harness-approval-mode)))

(defun harness-approval-needed-p (tool-name)
  "Return non-nil if TOOL-NAME requires approval in current context."
  (let ((mode (harness-approval-get-mode)))
    (pcase mode
      ('none nil)
      ('dangerous-only (harness-tool-dangerous-p tool-name))
      ('full t)
      (_ nil))))

(defun harness-approval-request (tool-name input callback)
  "Request approval for TOOL-NAME with INPUT.
CALLBACK is called with t if approved, nil if rejected."
  (setq harness-approval--pending (list tool-name input callback))
  (setq harness-approval--response nil)
  ;; The chat buffer will display the prompt and handle y/n
  (run-hooks 'harness-approval-request-hook))

(defun harness-approval-respond (approved)
  "Respond to pending approval with APPROVED (t or nil)."
  (when harness-approval--pending
    (let ((callback (nth 2 harness-approval--pending)))
      (setq harness-approval--pending nil)
      (setq harness-approval--response (if approved 'approved 'rejected))
      (funcall callback approved))))

(defun harness-approval-pending-p ()
  "Return non-nil if there is a pending approval request."
  (not (null harness-approval--pending)))

(defun harness-approval-get-pending ()
  "Return the pending approval request (tool-name input callback) or nil."
  harness-approval--pending)

;;; Hooks

(defvar harness-approval-request-hook nil
  "Hook run when approval is requested.
Called with no arguments. Use `harness-approval-get-pending' to get details.")

;;; Integration helper

(defun harness-approval-execute-with-approval (tool-name input on-result)
  "Execute TOOL-NAME with INPUT, handling approval if needed.
ON-RESULT is called with the result string when complete."
  (if (harness-approval-needed-p tool-name)
      (harness-approval-request
       tool-name input
       (lambda (approved)
         (if approved
             (funcall on-result (harness-execute-tool tool-name input))
           (funcall on-result (format "[Tool %s rejected by user]" tool-name)))))
    (funcall on-result (harness-execute-tool tool-name input))))

(provide 'harness-approval)
;;; harness-approval.el ends here
