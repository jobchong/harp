;;; harp-approval.el --- Approval system for tool execution -*- lexical-binding: t -*-

;;; Commentary:
;; Configurable approval system for dangerous tool operations.
;; Supports global defaults and per-project overrides.

;;; Code:

(require 'project)

;; Forward declarations to avoid circular dependency
(declare-function harp-tool-dangerous-p "harp-tools")
(declare-function harp-execute-tool "harp-tools")

;;; Customization

(defgroup harp-approval nil
  "Approval settings for harp tool execution."
  :group 'harp)

(defcustom harp-approval-mode 'dangerous-only
  "Global approval mode for tool execution.
- `none': Auto-execute all tools without prompting
- `dangerous-only': Prompt for dangerous tools (write, edit, shell)
- `full': Prompt for every tool execution"
  :type '(choice (const :tag "No approval needed" none)
                 (const :tag "Approve dangerous tools only" dangerous-only)
                 (const :tag "Approve all tools" full))
  :group 'harp-approval)

(defcustom harp-project-approval-alist nil
  "Alist of (project-root . approval-mode) for per-project overrides.
Project root should be an absolute path string."
  :type '(alist :key-type string :value-type symbol)
  :group 'harp-approval)

;;; State

(defvar harp-approval--pending nil
  "Currently pending approval request: (tool-name input callback) or nil.")

(defvar harp-approval--response nil
  "Response to pending approval: `approved', `rejected', or nil.")

;;; Core functions

(defun harp-approval-get-mode ()
  "Get the approval mode for the current project, or global default."
  (let ((proj (project-current)))
    (if-let* ((root (and proj (project-root proj)))
              (override (alist-get root harp-project-approval-alist
                                   nil nil #'string=)))
        override
      harp-approval-mode)))

(defun harp-approval-needed-p (tool-name)
  "Return non-nil if TOOL-NAME requires approval in current context."
  (let ((mode (harp-approval-get-mode)))
    (pcase mode
      ('none nil)
      ('dangerous-only (harp-tool-dangerous-p tool-name))
      ('full t)
      (_ nil))))

(defun harp-approval-request (tool-name input callback)
  "Request approval for TOOL-NAME with INPUT.
CALLBACK is called with t if approved, nil if rejected."
  (setq harp-approval--pending (list tool-name input callback))
  (setq harp-approval--response nil)
  ;; The chat buffer will display the prompt and handle y/n
  (run-hooks 'harp-approval-request-hook))

(defun harp-approval-respond (approved)
  "Respond to pending approval with APPROVED (t or nil)."
  (when harp-approval--pending
    (let ((callback (nth 2 harp-approval--pending)))
      (setq harp-approval--pending nil)
      (setq harp-approval--response (if approved 'approved 'rejected))
      (funcall callback approved))))

(defun harp-approval-pending-p ()
  "Return non-nil if there is a pending approval request."
  (not (null harp-approval--pending)))

(defun harp-approval-get-pending ()
  "Return the pending approval request (tool-name input callback) or nil."
  harp-approval--pending)

;;; Hooks

(defvar harp-approval-request-hook nil
  "Hook run when approval is requested.
Called with no arguments. Use `harp-approval-get-pending' to get details.")

;;; Integration helper

(defun harp-approval-execute-with-approval (tool-name input on-result)
  "Execute TOOL-NAME with INPUT, handling approval if needed.
ON-RESULT is called with the result string when complete."
  (if (harp-approval-needed-p tool-name)
      (harp-approval-request
       tool-name input
       (lambda (approved)
         (if approved
             (funcall on-result (harp-execute-tool tool-name input))
           (funcall on-result (format "[Tool %s rejected by user]" tool-name)))))
    (funcall on-result (harp-execute-tool tool-name input))))

(provide 'harp-approval)
;;; harp-approval.el ends here
