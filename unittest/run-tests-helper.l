(let ((dir (if *load-pathname*
               (directory-namestring *load-pathname*)
             "."))
      (tests))
  (load-file (merge-pathnames "simple-test.l" dir))

  (defun process-command-line (arg)
    (cond ((string= "--run-all-tests" arg)
           (let ((*error-on-overwriting-test* t))
             (if tests
                 (dolist (test (reverse tests))
                   (load-test-file test))
               (dolist (test (sort (directory dir :absolute t :recursive t :wild "*-tests.l")
                                   #'string-lessp))
                 (load-test-file test))))
           ;(test-changes)
           (run-all-tests))
          (t
           (push (merge-pathnames arg *command-line-args-base-directory*)
                 tests))))

  (add-hook '*process-command-line-hook* 'process-command-line)
  (setq si:*command-line-args* (append si:*command-line-args* (list "--run-all-tests")))
  )
