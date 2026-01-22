;;; harp-test.el --- Tests for harp -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests covering core tool and context behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'harp-tools)
(require 'harp-context)
(require 'harp-chat)

(defmacro harp-test--with-temp-dir (dirvar &rest body)
  "Create a temp dir bound to DIRVAR and eval BODY."
  (declare (indent 1))
  `(let ((,dirvar (make-temp-file "harp-test" t)))
     (unwind-protect
         (let ((default-directory ,dirvar))
           ,@body)
       (delete-directory ,dirvar t))))

(ert-deftest harp-test-tool-registration-and-execute ()
  (let ((harp-tools-alist nil)
        (harp-tools-schemas nil))
    (harp-register-tool
     "test_tool"
     "Test tool"
     '((type . "object"))
     (lambda (_input) "ok"))
    (should (functionp (alist-get "test_tool" harp-tools-alist nil nil #'string=)))
    (should (equal (alist-get 'name (harp-get-tool-schema "test_tool")) "test_tool"))
    (should (equal (harp-execute-tool "test_tool" nil) "ok"))
    (should (string-match-p "Unknown tool" (harp-execute-tool "nope" nil)))))

(ert-deftest harp-test-tool-file-ops ()
  (harp-test--with-temp-dir dir
    (let* ((path (expand-file-name "sample.txt" dir))
           (missing (expand-file-name "missing.txt" dir))
           (write-result (harp-execute-tool
                          "write_file"
                          `((path . ,path) (content . "hello")))))
      (should (file-exists-p path))
      (should (string-match-p "Wrote" write-result))
      (should (equal (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))
                     "hello"))
      (should (equal (harp-execute-tool "read_file" `((path . ,path))) "hello"))
      (should (string-match-p "read_file requires a valid path"
                              (harp-execute-tool "read_file" `((path . ,missing)))))
      (should (string-match-p "old_string not found"
                              (harp-execute-tool
                               "edit_file"
                               `((path . ,path)
                                 (old_string . "nope")
                                 (new_string . "x")))))
      (should (string-match-p "Edited"
                              (harp-execute-tool
                               "edit_file"
                               `((path . ,path)
                                 (old_string . "hello")
                                 (new_string . "hi")))))
      (should (equal (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))
                     "hi"))
      (should (string-match-p "File not found"
                              (harp-execute-tool
                               "edit_file"
                               `((path . ,missing)
                                 (old_string . "x")
                                 (new_string . "y"))))))))

(ert-deftest harp-test-tool-search-and-listing ()
  (harp-test--with-temp-dir dir
    (let ((file-a (expand-file-name "one.el" dir))
          (file-b (expand-file-name "two.txt" dir))
          (subdir (expand-file-name "subdir" dir)))
      (make-directory subdir t)
      (with-temp-file file-a
        (insert "alpha\nbeta\n"))
      (with-temp-file file-b
        (insert "gamma\n"))
      (with-temp-file (expand-file-name "nested.txt" subdir)
        (insert "delta\n"))
      (let ((glob-result (harp-execute-tool
                          "glob"
                          `((pattern . "*.el") (path . ,dir)))))
        (should (string-match-p "one.el" glob-result)))
      (let ((grep-result (harp-execute-tool
                          "grep"
                          `((pattern . "alpha") (path . ,dir)))))
        (should (string-match-p "alpha" grep-result)))
      (let ((list-result (harp-execute-tool "list_directory" `((path . ,dir)))))
        (should (string-match-p "one.el" list-result))
        (should (string-match-p "subdir/" list-result))))))

(ert-deftest harp-test-tool-run-shell ()
  (let ((result (harp-execute-tool "run_shell" '((command . "printf 'hi'")))))
    (should (string-match-p "Exit code: 0" result))
    (should (string-match-p "hi" result))))

(ert-deftest harp-test-context-current-file-content ()
  (harp-test--with-temp-dir dir
    (let ((harp-context-include-git nil)
          (harp-context-include-file-content t)
          (harp-context-max-file-size 5)
          (path (expand-file-name "context.txt" dir)))
      (with-temp-file path
        (insert "abcdef"))
      (let ((buf (find-file-noselect path)))
        (unwind-protect
            (let* ((ctx (harp-context-gather buf))
                   (file-info (alist-get 'current-file ctx))
                   (content (alist-get 'current-file-content ctx)))
              (should (equal (alist-get 'path file-info) path))
              (should (stringp (alist-get 'mode file-info)))
              (should (string-match-p "^abcde" content))
              (should (string-match-p "truncated" content)))
          (kill-buffer buf))))))

(ert-deftest harp-test-context-readme-content ()
  (harp-test--with-temp-dir dir
    (let ((harp-context-include-git nil)
          (harp-context-include-readme t)
          (readme-path (expand-file-name "README.md" dir)))
      (with-temp-file readme-path
        (insert "Harp readme"))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _dir) (cons 'transient dir)))
                ((symbol-function 'project-root)
                 (lambda (_proj) dir)))
        (let* ((default-directory dir)
               (ctx (harp-context-gather nil))
               (readme (alist-get 'readme ctx)))
          (should (equal (plist-get readme :path) readme-path))
          (should (string-match-p "Harp readme"
                                  (plist-get readme :content))))))))

(ert-deftest harp-test-chat-list-directory-throttle ()
  (with-temp-buffer
    (harp-chat-mode)
    (harp-chat--reset-tool-usage)
    (should (equal (harp-chat--tool-usage-count "list_directory") 0))
    (should (null (harp-chat--tool-skip-message "list_directory" nil)))
    (harp-chat--record-tool-usage "list_directory")
    (should (string-match-p "Skipped listing"
                            (harp-chat--tool-skip-message "list_directory" nil)))))

(ert-deftest harp-test-chat-tool-budget-throttle ()
  (with-temp-buffer
    (harp-chat-mode)
    (let ((harp-chat-max-tool-calls 1))
      (harp-chat--reset-tool-usage)
      (harp-chat--record-tool-usage "read_file" nil)
      (should (string-match-p "budget"
                              (harp-chat--tool-skip-message "read_file" nil))))))

(ert-deftest harp-test-chat-listing-detects-shell-ls ()
  (with-temp-buffer
    (harp-chat-mode)
    (let ((harp-chat-listing-limit 0))
      (harp-chat--reset-tool-usage)
      (should (string-match-p "Skipped listing"
                              (harp-chat--tool-skip-message
                               "run_shell"
                               '((command . "ls -la"))))))))

(provide 'harp-test)
;;; harp-test.el ends here
