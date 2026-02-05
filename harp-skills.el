;;; harp-skills.el --- Slash skill discovery for harp -*- lexical-binding: t -*-

;; Author: Job Chong
;; URL: https://github.com/jobchong/harp
;; Part of harp.el

;;; Commentary:
;; Discover slash skills in .codex/.agents/.claude directories (including
;; Claude commands under .claude/commands) and parse slash command invocations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst harp-skills--source-order '(".codex" ".agents" ".claude")
  "Directory precedence for skills at the same root.")

(defconst harp-skills--command-regexp
  "\\`[[:space:]]*/\\([A-Za-z0-9_.:-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'"
  "Regexp for slash skill invocation.")

(defconst harp-skills--command-file-regexp "\\.md\\'"
  "Regexp for slash command files.")

(defun harp-skills--normalize-newlines (text)
  "Normalize newline sequences in TEXT."
  (replace-regexp-in-string "\r\n" "\n" text))

(defun harp-skills--split-frontmatter (content)
  "Split CONTENT into (frontmatter . body) if YAML frontmatter exists."
  (let ((text (harp-skills--normalize-newlines content)))
    (if (string-match "\\`---[ \t]*\n" text)
        (let ((start (match-end 0)))
          (if (string-match "\n---[ \t]*\n" text start)
              (cons (substring text start (match-beginning 0))
                    (substring text (match-end 0)))
            (cons nil text)))
      (cons nil text))))

(defun harp-skills--parse-frontmatter-fields (frontmatter)
  "Parse FRONTMATTER and return an alist with name/description."
  (let* ((lines (split-string (or frontmatter "") "\n"))
         (count (length lines))
         (i 0)
         name
         description)
    (while (< i count)
      (let ((line (nth i lines)))
        (cond
         ((string-match "\\`name:[ \t]*\\(.*\\)\\'" line)
          (setq name (string-trim (match-string 1 line))))
         ((string-match "\\`description:[ \t]*\\(.*\\)\\'" line)
          (let ((val (string-trim (match-string 1 line))))
            (if (member val '("|" ">"))
                (let ((block-lines '())
                      (indent nil)
                      (j (1+ i))
                      done)
                  (while (and (< j count) (not done))
                    (let ((cur (nth j lines)))
                      (cond
                       ((string-match "\\`[ \t]*\\'" cur)
                        (push "" block-lines)
                        (setq j (1+ j)))
                       ((string-match "\\`[ \t]+\\(.*\\)\\'" cur)
                        (let ((cur-indent (length (match-string 0 cur))))
                          (when (null indent)
                            (setq indent cur-indent))
                          (if (>= cur-indent indent)
                              (progn
                                (push (substring cur indent) block-lines)
                                (setq j (1+ j)))
                            (setq done t))))
                       (t (setq done t)))))
                  (setq description
                        (string-trim-right
                         (string-join (nreverse block-lines) "\n")))
                  (setq i (1- j)))
              (setq description val))))))
      (setq i (1+ i)))
    (delq nil
          (list (and name (cons 'name name))
                (and description (cons 'description description))))))

(defun harp-skills--read-file (path)
  "Read PATH and return its contents or nil on error."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error nil)))

(defun harp-skills--read-metadata (path default-name)
  "Read metadata from PATH, falling back to DEFAULT-NAME."
  (let* ((content (harp-skills--read-file path))
         (split (and content (harp-skills--split-frontmatter content)))
         (fields (and split (harp-skills--parse-frontmatter-fields (car split))))
         (name (or (alist-get 'name fields) default-name))
         (description (or (alist-get 'description fields) "")))
    `((name . ,name)
      (description . ,description))))

(defun harp-skills--read-body (path)
  "Read PATH and return the body without frontmatter."
  (let* ((content (harp-skills--read-file path))
         (split (and content (harp-skills--split-frontmatter content)))
         (body (or (cdr split) "")))
    (string-trim body)))

(defun harp-skills--register-skill (meta path source root seen results)
  "Register META from PATH into RESULTS if not already in SEEN."
  (let ((name (alist-get 'name meta)))
    (if (or (null name) (string-empty-p name) (gethash name seen))
        results
      (puthash name t seen)
      (cons (append meta
                    `((path . ,path)
                      (source . ,source)
                      (root . ,root)))
            results))))

(defun harp-skills--command-files (commands-dir)
  "Return a list of command files under COMMANDS-DIR."
  (when (file-directory-p commands-dir)
    (directory-files-recursively commands-dir harp-skills--command-file-regexp)))

(defun harp-skills--truncate (text max-size)
  "Truncate TEXT to MAX-SIZE characters, adding a marker when truncated."
  (if (and (stringp text) (integerp max-size) (> (length text) max-size))
      (concat (substring text 0 max-size) "\n... [truncated]")
    text))

(defun harp-skills--parent-dirs (base-dir git-root)
  "Return list of directories from BASE-DIR up to GIT-ROOT (inclusive)."
  (let* ((base (file-name-as-directory (expand-file-name base-dir)))
         (root (and git-root (file-name-as-directory (expand-file-name git-root))))
         (current base)
         (dirs '()))
    (while current
      (push current dirs)
      (if (and root (string= current root))
          (setq current nil)
        (let ((parent (file-name-directory (directory-file-name current))))
          (if (or (null parent) (string= parent current))
              (setq current nil)
            (setq current parent)))))
    (nreverse dirs)))

(defun harp-skills-discover (&optional base-dir git-root)
  "Discover skills under BASE-DIR and parents up to GIT-ROOT."
  (let* ((base (file-name-as-directory
                (expand-file-name (or base-dir default-directory))))
         (roots (if git-root
                    (harp-skills--parent-dirs base git-root)
                  (list base)))
         (seen (make-hash-table :test 'equal))
         (results '()))
    (dolist (root roots)
      (dolist (source harp-skills--source-order)
        (let ((skills-dir (expand-file-name (concat source "/skills") root)))
          (when (file-directory-p skills-dir)
            (dolist (entry (directory-files skills-dir t "^[^.]"))
              (when (file-directory-p entry)
                (let ((skill-path (expand-file-name "SKILL.md" entry)))
                  (when (file-exists-p skill-path)
                    (let ((meta (harp-skills--read-metadata
                                 skill-path
                                 (file-name-nondirectory entry))))
                      (setq results
                            (harp-skills--register-skill
                             meta skill-path source root seen results)))))))))
        (when (string= source ".claude")
          (let ((commands-dir (expand-file-name (concat source "/commands") root)))
            (dolist (command-path (harp-skills--command-files commands-dir))
              (let* ((default-name (file-name-base command-path))
                     (meta (harp-skills--read-metadata command-path default-name)))
                (setq results
                      (harp-skills--register-skill
                       meta command-path source root seen results))))))))
    (nreverse results)))

(defun harp-skills-parse-invocation (raw-input skills &optional max-body-size)
  "Parse RAW-INPUT for slash skill invocation using SKILLS."
  (when (and (stringp raw-input)
             (string-match harp-skills--command-regexp raw-input))
    (let* ((name (match-string 1 raw-input))
           (args (string-trim (or (match-string 2 raw-input) "")))
           (skill (cl-find-if (lambda (s)
                                (string= (alist-get 'name s) name))
                              skills)))
      (when skill
        (let* ((path (alist-get 'path skill))
               (body (and path (harp-skills--read-body path)))
               (body (harp-skills--truncate (or body "") max-body-size))
               (full-skill (append skill `((body . ,body)))))
          (list :skill full-skill :user-input args))))))

(provide 'harp-skills)
;;; harp-skills.el ends here
