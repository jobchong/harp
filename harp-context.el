;;; harp-context.el --- Context gathering for harp -*- lexical-binding: t -*-

;; Author: Job Chong
;; URL: https://github.com/jobchong/harp
;; Part of harp.el

;;; Commentary:
;; Gathers project context (files, git status, etc.) for LLM prompts.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'harp-skills)

;;; Customization

(defgroup harp-context nil
  "Context gathering settings for harp."
  :group 'harp)

(defcustom harp-context-include-git t
  "Whether to include git status in context."
  :type 'boolean
  :group 'harp-context)

(defcustom harp-context-include-file-content t
  "Whether to include current file content in context."
  :type 'boolean
  :group 'harp-context)

(defcustom harp-context-include-readme t
  "Whether to include README content from the project root."
  :type 'boolean
  :group 'harp-context)

(defcustom harp-context-include-slash-skills t
  "Whether to include discovered slash skills/commands in context."
  :type 'boolean
  :group 'harp-context)

(defcustom harp-context-max-file-size 50000
  "Maximum file size to include in context (characters)."
  :type 'integer
  :group 'harp-context)

;;; Context gathering

(defun harp-context--git-root ()
  "Get git root directory, or nil if not in a git repo."
  (when-let ((output (shell-command-to-string "git rev-parse --show-toplevel 2>/dev/null")))
    (unless (string-empty-p output)
      (string-trim output))))

(defun harp-context--git-status ()
  "Get git status summary."
  (when (harp-context--git-root)
    (shell-command-to-string "git status --short 2>/dev/null")))

(defun harp-context--git-branch ()
  "Get current git branch."
  (when (harp-context--git-root)
    (string-trim (shell-command-to-string "git branch --show-current 2>/dev/null"))))

(defun harp-context--project-root ()
  "Get project root directory."
  (when-let ((proj (project-current)))
    (project-root proj)))

(defun harp-context--current-file (file-buffer)
  "Get info about FILE-BUFFER if it's visiting a file."
  (when (and file-buffer (buffer-live-p file-buffer))
    (with-current-buffer file-buffer
      (when buffer-file-name
        `((path . ,buffer-file-name)
          (mode . ,(symbol-name major-mode))
          (modified . ,(buffer-modified-p)))))))

(defun harp-context--current-file-content (file-buffer)
  "Get content of FILE-BUFFER, truncated if too large."
  (when (and file-buffer
             (buffer-live-p file-buffer)
             harp-context-include-file-content)
    (with-current-buffer file-buffer
      (when buffer-file-name
        (let ((content (buffer-string)))
          (if (> (length content) harp-context-max-file-size)
              (concat (substring content 0 harp-context-max-file-size)
                      "\n... [truncated]")
            content))))))

(defun harp-context--readme-path (root)
  "Return a README path under ROOT, or nil."
  (let ((candidates '("README.md" "README" "readme.md" "readme")))
    (cl-find-if (lambda (name)
                  (file-exists-p (expand-file-name name root)))
                candidates)))

(defun harp-context--readme-content (root)
  "Get README content under ROOT, truncated if too large."
  (when (and root harp-context-include-readme)
    (when-let ((name (harp-context--readme-path root)))
      (let* ((path (expand-file-name name root))
             (content (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))
        (list :path path
              :content (if (> (length content) harp-context-max-file-size)
                           (concat (substring content 0 harp-context-max-file-size)
                                   "\n... [truncated]")
                         content))))))

(defun harp-context-gather (file-buffer)
  "Gather context for LLM prompt.  FILE-BUFFER is the file pane buffer."
  (let ((ctx '())
        root)
    ;; Project info
    (when-let ((proj-root (harp-context--project-root)))
      (setq root proj-root)
      (push `(project-root . ,proj-root) ctx))
    ;; Git info
    (when harp-context-include-git
      (when-let ((branch (harp-context--git-branch)))
        (push `(git-branch . ,branch) ctx))
      (when-let ((status (harp-context--git-status)))
        (unless (string-empty-p status)
          (push `(git-status . ,status) ctx))))
    ;; Current file
    (when-let ((file-info (harp-context--current-file file-buffer)))
      (push `(current-file . ,file-info) ctx))
    (when-let ((content (harp-context--current-file-content file-buffer)))
      (push `(current-file-content . ,content) ctx))
    ;; README
    (when-let ((readme (and root (harp-context--readme-content root))))
      (push `(readme . ,readme) ctx))
    ;; Slash skills
    (when harp-context-include-slash-skills
      (let* ((git-root (harp-context--git-root))
             (skills (harp-skills-discover default-directory git-root)))
        (when skills
          (push `(slash-skills . ,skills) ctx))))
    ;; Working directory
    (push `(working-directory . ,default-directory) ctx)
    ;; Platform info
    (push `(platform . ,(symbol-name system-type)) ctx)
    (push `(emacs-version . ,emacs-version) ctx)
    (nreverse ctx)))

;;; System prompt generation

(defvar harp-system-prompt-template
  "You are a coding assistant integrated into Emacs. You help the user with software engineering tasks.

You have access to tools for reading and writing files, running shell commands, and searching the codebase. Use these tools to accomplish tasks.

## Environment
- Platform: %s
- Emacs version: %s
- Working directory: %s
%s%s%s%s%s%s

## Guidelines
- Read files before modifying them to understand the context
- Make targeted edits using edit_file rather than rewriting entire files
- Run tests after making changes when appropriate
- Use tools only when needed; keep to a few tool calls per request
- Avoid repeated directory listings; list at most once per request, then read README or specific files
- Use set_status sparingly (at most once per response) to share a short next-steps summary before long work or tool use
- Do not ask for permission to use tools; proceed when needed to answer the user
- Keep responses concise but informative
- When calling tools that accept paths, keep user-provided paths verbatim; do not substitute home directory usernames; ~ is acceptable"
  "Template for the system prompt.  Format args:
1. Platform
2. Emacs version
3. Working directory
4. Project root (or empty)
5. Git info (or empty)
6. Current file info (or empty)
7. README content (or empty)
8. Slash skills (or empty)
9. Active skill (or empty)")

(defun harp-context-build-system-prompt (context)
  "Build system prompt string from CONTEXT alist."
  (let ((platform (or (alist-get 'platform context) "unknown"))
        (emacs-ver (or (alist-get 'emacs-version context) emacs-version))
        (working-dir (or (alist-get 'working-directory context) default-directory))
        (project-str (if-let ((root (alist-get 'project-root context)))
                         (format "- Project root: %s\n" root)
                       ""))
        (git-str (let ((branch (alist-get 'git-branch context))
                       (status (alist-get 'git-status context)))
                   (if (or branch status)
                       (concat (when branch (format "- Git branch: %s\n" branch))
                               (when status (format "- Git status:\n```\n%s```\n" status)))
                     "")))
        (file-str (if-let ((file-info (alist-get 'current-file context)))
                      (format "- Current file: %s (%s)\n"
                              (alist-get 'path file-info)
                              (alist-get 'mode file-info))
                    ""))
        (readme-str (if-let ((readme (alist-get 'readme context)))
                        (format "- README: %s\n```\n%s\n```\n"
                                (plist-get readme :path)
                                (plist-get readme :content))
                      ""))
        (skills-str
         (if-let ((skills (alist-get 'slash-skills context)))
             (concat "## Slash Skills\n"
                     (mapconcat
                      (lambda (skill)
                        (let* ((name (alist-get 'name skill))
                               (desc (string-trim
                                      (or (alist-get 'description skill) "")))
                               (source (alist-get 'source skill))
                               (path (alist-get 'path skill)))
                          (if (string-empty-p desc)
                              (format "- /%s (%s, %s)" name source path)
                            (format "- /%s — %s (%s, %s)"
                                    name desc source path))))
                      skills
                      "\n")
                     "\n")
           ""))
        (active-skill-str
         (if-let ((skill (alist-get 'active-skill context)))
             (let* ((name (alist-get 'name skill))
                    (path (alist-get 'path skill))
                    (body (or (alist-get 'body skill) ""))
                    (body (if (> (length body) harp-context-max-file-size)
                              (concat (substring body 0 harp-context-max-file-size)
                                      "\n... [truncated]")
                            body)))
               (format "## Active Slash Skill\n- Name: /%s\n- Path: %s\n```\n%s\n```\n"
                       name path body))
           "")))
    (format harp-system-prompt-template
            platform emacs-ver working-dir
            project-str git-str file-str readme-str
            skills-str active-skill-str)))

(provide 'harp-context)
;;; harp-context.el ends here
