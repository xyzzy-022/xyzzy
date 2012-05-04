(let ((dir (if *load-pathname*
               (directory-namestring *load-pathname*)
             "."))
      (tests))
  (load-file (merge-pathnames "simple-test.l" dir))

  (defun process-command-line (arg)
    (cond ((string= "--run-all-tests" arg)
           (if tests
               (dolist (test (reverse tests))
                 (load-test-file test))
             (progn
               (load-test-file (merge-pathnames "nanri-master-change-tests.l" dir))
               (load-test-file (merge-pathnames "lisp-tests.l" dir))
               (load-test-file (merge-pathnames "common-lisp-tests.l" dir))
               (load-test-file (merge-pathnames "editor-tests.l" dir))
               (load-test-file (merge-pathnames "system-tests.l" dir))
               (load-test-file (merge-pathnames "foreign-test.l" dir))))
           ;(test-changes)
           (run-all-tests))
          (t
           (push (merge-pathnames arg *command-line-args-base-directory*)
                 tests))))

  (add-hook '*process-command-line-hook* 'process-command-line)
  (setq si:*command-line-args* (append si:*command-line-args* (list "--run-all-tests")))
  )
