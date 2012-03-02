;;; -*- mode: lisp; package: user -*-

(in-package :user)

(when (fboundp 'stop-watch-test-file)
  (stop-watch-test-file))

(defparameter +undefined+ '#:undefined)

(defun undefined-p (x) (eq x +undefined+))

#+unittest-meta-test
(defmacro ASSERT (form)
  `(unless ,form
     (error "ASSERTION FAILURE: ~S" ',form)))


;;;; test file loader

(defparameter *test-file-readtable* (copy-readtable nil))

(defun expected-output-reader (stream char)
  (let ((next (peek-char nil stream nil #1='#:eof t)))
    (case next
      (#\>
       (read-char stream nil nil t) ; discard 2nd >
       (if (peek-char #\space stream nil #1# t)
         (read-char stream nil nil t) ; discard following space
         (error 'reader-error :stream stream :datum "不正な期待出力です"))
       (list :expected-output
             (with-output-to-string (s)
               (while (and (setf next (read-char stream nil nil t))
                           (not (eql next #\LFD)))
                 (princ next s)))))
      ((#\space #1#) '>)
      (t
       (let ((follow (read stream nil "" t)))
         (intern (format nil ">~A" follow) *package*))))))

(set-macro-character #\> 'expected-output-reader t *test-file-readtable*)

(defun load-test-file (filename)
  (interactive "ftest file: ")
  (let ((*readtable* *test-file-readtable*))
    (load-file filename)))

#+unittest-meta-test
(progn
  (ASSERT (let ((*readtable* *test-file-readtable*))
            (equal (read-from-string ">> hello\n")
                   '(:expected-output "hello"))))
  (ASSERT (let ((*readtable* *test-file-readtable*))
            (equal (read-from-string ">> hello")
                   '(:expected-output "hello")))))


;;;; macro deftest

(defparameter *tests* nil)

(defparameter *output-capture-stream* nil)

(defun make-test-thunk-form (form output-stream-var)
  `(lambda ()
     (let ((*output-capture-stream* (make-string-output-stream)))
       (values (handler-case
                   (list* :return
                          (,@(if output-stream-var
                               `(let ((,output-stream-var *output-capture-stream*)))
                               `(progn))
                           (multiple-value-list ,form)))
                 (error (error)
                   (list :error error)))
               ,(when output-stream-var
                  `(get-output-stream-string *output-capture-stream*))))))

#+unittest-meta-test
(progn
  (ASSERT (equal (make-test-thunk-form '(+ 1 2) nil)
                 `(lambda ()
                    (let ((*output-capture-stream* (make-string-output-stream)))
                      (values (handler-case
                                  (list* :return (progn (multiple-value-list (+ 1 2))))
                                (error (error) (list :error error)))
                              nil)))))
  (ASSERT (equal (make-test-thunk-form '(princ :hello) '*standard-output*)
                 `(lambda ()
                    (let ((*output-capture-stream* (make-string-output-stream)))
                      (values (handler-case
                                  (list* :return
                                         (let ((*standard-output* *output-capture-stream*))
                                           (multiple-value-list (princ :hello))))
                                (error (error) (list :error error)))
                              (get-output-stream-string *output-capture-stream*)))))))

(defun non-nil-p (#:ignore actual) actual)

(defun parse-expectations (name expectations options)
  "Return a list describing expected behaviour.
=> (RESULT OUTPUT EXPANSION)
RESULT = (:return COMPARE-FN . VALUES) | (:error ERROR-TYPE)
OUTPUT = (STREAM-VAR . OUTPUT-STRING)
EXPANSION = FORM"
  (let ((result-type nil)
        (result-data '())
        (output nil)
        (dest   (getf options :output '*standard-output*))
        (state  nil)
        (expansion +undefined+))
    (dolist (x expectations)
      (cond
       ((member x '(=> !! ==)) (setf state x))
       ((and (consp x) (eql (first x) :expected-output))
        (setf output (if output (format nil "~A~%~A" output (second x)) (second x))))
       (t (case state
            (=> (case result-type
                  ((nil) (setf result-type :return))
                  (:return)
                  (:error (error "Don't expect both return value(s) and error: ~S" name)))
                (push x result-data))
            (!! (case result-type
                  (:return (error "Don't expect both return value(s) and error: ~S" name))
                  (:error  (error "Don't expect multiple errors: ~S" name)))
                (unless (symbolp x)
                  (error 'type-error :datum x :expected-type 'symbol))
                (setf result-type :error
                      result-data x))
            (== (setf expansion x))))))
    (list (case result-type
            (:return (list* :return (getf options :compare 'equal) (nreverse result-data)))
            (:error  (list  :error result-data))
            ((nil)   (list  :return 'non-nil-p ':non-nil))
            (t (error "parse-expectations - unknown result-type: ~S: ~S" result-type name)))
          (when output (list dest output))
          expansion)))

#+unittest-meta-test
(progn
  ;; results (return values)
  (ASSERT (equal (parse-expectations 'example '(=> 3) nil)
                 `((:return equal 3) nil ,+undefined+)))
  (ASSERT (equal (parse-expectations 'example '(=> 3 => 4) nil)
                 `((:return equal 3 4) nil ,+undefined+)))
  ;; results (error)
  (ASSERT (equal (parse-expectations 'example '(!! type-error) nil)
                 `((:error type-error) nil ,+undefined+)))
  ;; output
  (ASSERT (equal (parse-expectations 'example '((:expected-output "hello")) nil)
                 `((:return non-nil-p :non-nil)
                   (*standard-output* "hello")
                   ,+undefined+)))
  ;; expansion
  (ASSERT (equal (parse-expectations 'example '(== (a b c)) nil)
                 `((:return non-nil-p :non-nil) nil (a b c))))
  )


(defun expand-deftest (name options doc form expectations)
  (let* ((expectations (parse-expectations name expectations options))
         (expected-output-dest (when (second expectations)
                                 (car (second expectations))))
         (thunk-form   (make-test-thunk-form form expected-output-dest)))
    `(progn
       (when (find ',name *tests* :key 'car)
         (warn "テスト再定義してる: ~S" ',name))
       (setf *tests*
             (cons (list ',name ,doc ',form ,thunk-form ',expectations)
                   (delete ',name *tests* :key 'car)))
       ',name)))

(defmacro deftest (name (&rest options) form &rest expected)
  (multiple-value-bind (doc form expected)
      (if (stringp form)
        (values form (car expected) (cdr expected))
        (values nil form expected))
    (expand-deftest name options doc form expected)))
(setf (get 'deftest 'ed:lisp-indent-hook) 2)


;;;; check actual behaviour against expected one

(defparameter *failure-reporters* (make-hash-table :test 'equal))

(defmacro define-failure-reporter (kind name lambda-list &body body)
  `(setf (gethash '(,kind ,name) *failure-reporters*)
         (lambda ,lambda-list ,@body)))
(setf (get 'define-failure-reporter 'ed:lisp-indent-hook) t)

(defun report-failure (failure)
  (let ((reporter (gethash (list (first failure) (second failure)) *failure-reporters*)))
    (unless reporter
      (error "Failure reporter not found: ~S" failure))
    (apply reporter (cddr failure))))


;;;;; macro expansion

(defun form-equal (expected actual)
  (let ((seen (make-hash-table :test #'equal)))
    (macrolet ((seen-e () `(gethash e-name seen))
               (gensymp (x) `(and (symbolp ,x) (null (symbol-package ,x)))))
      (labels ((form-equal-1 (e a)
                 (cond ((null e) (null a))
                       ((gensymp e)
                        (and (gensymp a)
                             (let ((e-name (symbol-name e)))
                               (or (string-equal e-name (symbol-name a))
                                   (if (seen-e) (eq (seen-e) a) (setf (seen-e) a))))))
                       ((atom e) (equal e a))
                       ((consp e) (and (consp a)
                                       (form-equal-1 (car e) (car a))
                                       (form-equal-1 (cdr e) (cdr a))))
                       (t (warn "Should not reach here: ~S: ~S" e a)))))
        (form-equal-1 expected actual)))))

#+unittest-meta-test
(progn
  (ASSERT (form-equal '(:foo #:bar "baz")
                      `(:foo ,(gensym) "baz")))
  (ASSERT (form-equal '(#:foo #:foo)
                      (let ((s (gensym))) `(,s ,s))))
  (ASSERT (not (form-equal '(#:foo #:foo)
                           `(,(gensym) ,(gensym)))))
  (ASSERT (form-equal '(#:foo #:bar)
                      `(,(gensym) ,(gensym))))
  )

(defun check-macro-expansion (expected-expn form)
  "check macro expansion of FORM satisfies EXPECTED-EXPansioN.
Return nil if FORM expanded as expected. Otherwise, return a list describing
how actual expansion differs."
  (unless (undefined-p expected-expn)
    (multiple-value-bind (actual-expn1 ep actual-expn)
        (macroexpand-1 form) ; TODO: env
      (unless ep
        (return-from check-macro-expansion
          (list :expansion :not-expanded
                expected-expn)))
      (setq actual-expn (macroexpand form))
      (unless (or (form-equal expected-expn actual-expn1)
                  (form-equal expected-expn actual-expn))
        (return-from check-macro-expansion
          (list :expansion :wrong-expansion
                expected-expn
                actual-expn1 actual-expn))))))

#+unittest-meta-test
(progn
  (ASSERT (null  (check-macro-expansion '(setq x 1) '(setf x 1))))
  (ASSERT (equal (check-macro-expansion '(setq x 1) '(setq x 1))
                 `(:expansion :not-expanded (setq x 1))))
  (ASSERT (equal (check-macro-expansion '(set x 1) '(setf x 1))
                 `(:expansion :wrong-expansion (set x 1) (setq x 1) (setq x 1)))))

(define-failure-reporter :expansion :not-expanded (expected-expn)
  (format t "Macro expansion:~%")
  (format t "  Form did not expanded.~%"))

(define-failure-reporter :expansion :wrong-expansion (expected-expn actual-expn1 actual-expn)
  (format t "Macro expansion:~%")
  (format t "  Expected expansion:~%  == ~S~%" expected-expn)
  (format t "  Actual expansion (macroexpand-1):~%  == ~S~%" actual-expn1)
  (format t "  Actual expansion (macroexpand):~%  == ~S~%" actual-expn))

#+unittest-meta-test
(macrolet ((assert-report (failure output)
             `(ASSERT (string= (with-output-to-string (*standard-output*)
                                 (report-failure ',failure))
                               ,output))))
  (setf (get 'assert-report 'ed:lisp-indent-hook) 1)
  (assert-report (:expansion :not-expanded (setq x 1)) "\
Macro expansion:
  Form did not expanded.
")
  (assert-report (:expansion :wrong-expansion (setq x 1) (gethash 'foo *table*) (si:*fset 'foo #'bar)) "\
Macro expansion:
  Expected expansion:
  == (setq x 1)
  Actual expansion (macroexpand-1):
  == (gethash 'foo *table*)
  Actual expansion (macroexpand):
  == (system:*fset 'foo #'bar)
"))


;;;;; results (return values, or error)

(defun check-results (expected-results actual-results)
  "check result of execution (ACTUAL-RESULTS) against EXPECTED-RESULTS.
Results are either returned values or signalled error described by list.
Returns a list describing how ACTUAL-RESULTS differ from EXPECTED-RESULTS
only when ACTUAL-RESULTS does not match expectation."
  (case (first expected-results)
    (:return
     (let ((compare-fn (second expected-results))
           (expected-values (cddr expected-results)))
       (case (first actual-results)
         (:return (let* ((actual-values (cdr actual-results))
                         (nexpected (length expected-values))
                         (nactuals  (length actual-values)))
                    (let ((failure (cond ((< nactuals nexpected) :too-few-values)
                                         ((> nactuals nexpected) :too-many-values)
                                         ((not (every compare-fn expected-values actual-values))
                                          :wrong-values))))
                      (when failure
                        (list :results failure expected-values actual-values compare-fn)))))
         (:error  (let ((error (second actual-results)))
                    (return-from check-results
                      (list :results :unexpected-error
                            expected-values
                            error)))))))
    (:error
     (let ((expected-error-name (second expected-results)))
       (case (first actual-results)
         (:return (let ((actual-values (cdr actual-results)))
                    (return-from check-results
                      (list :results :unexpected-return
                            expected-error-name
                            actual-values))))
         (:error  (let ((actual-error (second actual-results)))
                    (unless (si:*structure-subtypep
                             (si:*structure-definition actual-error)
                             (get expected-error-name 'si:structure-definition))
                      (return-from check-results
                        (list :results :wrong-error
                              expected-error-name
                              actual-error))))))))
    (t (error "Invalid expected results: ~S" expected-results))))

#+unittest-meta-test
(progn
  ;; expecting return
  (ASSERT (null  (check-results '(:return equal 1) '(:return 1))))
  (ASSERT (equal (check-results '(:return equal 1) '(:return 1 2))
                 `(:results :too-many-values (1) (1 2) equal)))
  (ASSERT (equal (check-results '(:return equal 1 2) '(:return 1))
                 `(:results :too-few-values (1 2) (1) equal)))
  (ASSERT (equal (check-results '(:return equal "foo") '(:return "bar"))
                 `(:results :wrong-values ("foo") ("bar") equal)))
  (ASSERT (equal (check-results '(:return eql "foo") '(:return "foo"))
                 `(:results :wrong-values ("foo") ("foo") eql)))
  (let ((error (make-condition 'type-error)))
    (ASSERT (equal (check-results '(:return equal 1) `(:error ,error))
                   `(:results :unexpected-error (1) ,error))))
  ;; expecting error
  (ASSERT (null  (check-results '(:error type-error) `(:error ,(make-condition 'type-error)))))
  (ASSERT (equal (check-results '(:error type-error) '(:return 1))
                 `(:results :unexpected-return type-error (1))))
  (let ((error (make-condition 'simple-error :format-string "Bah!!")))
    (ASSERT (equal (check-results '(:error type-error) `(:error ,error))
                   `(:results :wrong-error type-error ,error))))
  )

(defun print-return-values (values)
  (format t "~{  => ~S~%~}" values))

(defun report-return-values (expected-values actual-values &optional fmt &rest args)
  (format t "Evaluation result (return values~@[, ~?~]):~%" fmt args)
  (format t "  Expected:~%")
  (print-return-values expected-values)
  (format t "  Actually:~%")
  (print-return-values actual-values))


(define-failure-reporter :results :too-few-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values))

(define-failure-reporter :results :too-many-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values))

(define-failure-reporter :results :wrong-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values "~S" compare-fn))

(define-failure-reporter :results :unexpected-error (expected-values error)
  (format t "Evaluation result (return values):~%")
  (format t "  Expected:~%~{  => ~S~%~}" expected-values)
  (format t "  Actually:~%  !! ~S: ~A~%"
        (si:*structure-definition-name (si:*structure-definition error))
        error))

(define-failure-reporter :results :unexpected-return (expected-error-name actual-values)
  (format t "Evaluation result (error):~%")
  (format t "  Expected:~%  !! ~S~%" expected-error-name)
  (format t "  Actually:~%~{  => ~S~%~}" actual-values))

(define-failure-reporter :results :wrong-error (expected-error-name actual-error)
  (format t "Evaluation result (error):~%")
  (format t "  Expected:~%  !! ~S~%" expected-error-name)
  (format t "  Actually:~%  !! ~S: ~A~%"
        (si:*structure-definition-name (si:*structure-definition actual-error))
        actual-error))

#+unittest-meta-test
(macrolet ((assert-report (failure output)
             `(ASSERT (string= (with-output-to-string (*standard-output*)
                                 (report-failure ',failure))
                               ,output))))
  (assert-report (:results :too-few-values (1 2) (1) equal) "\
Evaluation result (return values):
  Expected:
  => 1
  => 2
  Actually:
  => 1
")
  (assert-report (:results :too-many-values (1) (1 2) equal) "\
Evaluation result (return values):
  Expected:
  => 1
  Actually:
  => 1
  => 2
")
  (assert-report (:results :wrong-values (1 2) (2 1) equal) "\
Evaluation result (return values, equal):
  Expected:
  => 1
  => 2
  Actually:
  => 2
  => 1
")
  (assert-report (:results :unexpected-error (1) #.(make-condition 'simple-error :format-string "bah!!")) "\
Evaluation result (return values):
  Expected:
  => 1
  Actually:
  !! simple-error: bah!!
")
  (assert-report (:results :unexpected-return type-error (1)) "\
Evaluation result (error):
  Expected:
  !! type-error
  Actually:
  => 1
")
  (assert-report (:results :wrong-error type-error #.(make-condition 'simple-error :format-string "bah!!")) "\
Evaluation result (error):
  Expected:
  !! type-error
  Actually:
  !! simple-error: bah!!
"))


;;;;; output

(defun check-output (expected-output actual-output)
  "Check ACTUAL-OUTPUT is same as EXPECTED-OUTPUT."
  (unless expected-output (return-from check-output nil))
  (cond ((string= actual-output "")
         (list :output :nothing-printed expected-output))
        ((not (string= actual-output (second expected-output)))
         (list :output :wrong-output expected-output actual-output))))

#+unittest-meta-test
(progn
  ;; no output expected
  (ASSERT (null  (check-output nil nil)))
  ;; output to *standard-output*
  (ASSERT (null  (check-output '(*standard-output* "hello") "hello")))
  (ASSERT (equal (check-output '(*standard-output* "hello") "")
                 `(:output :nothing-printed (*standard-output* "hello"))))
  (ASSERT (equal (check-output '(*standard-output* "hello") "what?")
                 `(:output :wrong-output (*standard-output* "hello") "what?")))
  )

(define-failure-reporter :output :nothing-printed (expected-output)
  (multiple-value-bind (stream-var output) (values-list expected-output)
    (format t "Output (~S):~%" stream-var)
    (format t "  Expected:~%~{  >> ~A~%~}" (split-string output #\LFD))
    (format t "  Actually:~%  ;; nothing printed~%")))

(define-failure-reporter :output :wrong-output (expected-output actual-output)
  (multiple-value-bind (stream-var expected-output) (values-list expected-output)
    (format t "Output (~S):~%" stream-var)
    (format t "  Expected:~%~{  >> ~A~%~}" (split-string expected-output #\LFD))
    (format t "  Actually:~%~{  >> ~A~%~}" (split-string actual-output #\LFD))))

#+unittest-meta-test
(macrolet ((assert-report (failure output)
             `(ASSERT (string= (with-output-to-string (*standard-output*)
                                 (report-failure ',failure))
                               ,output))))
  (assert-report (:output :nothing-printed (*standard-output* "hello")) "\
Output (*standard-output*):
  Expected:
  >> hello
  Actually:
  ;; nothing printed
")
  (assert-report (:output :wrong-output (*trace-output* "hello") "what?") "\
Output (*trace-output*):
  Expected:
  >> hello
  Actually:
  >> what?
"))

(defun run-test-1 (test)
  (multiple-value-bind (name doc form thunk expectations)
      (values-list test)
    (multiple-value-bind (expected-results expected-output expected-expansion)
        (values-list expectations)
      (let ((failures '()))
        (macrolet ((check (form)
                     `(let ((failure ,form))
                        (when failure (push failure failures)))))
          (check (check-macro-expansion expected-expansion form))
          (multiple-value-bind (actual-results actual-output)
              (funcall thunk)
            (check (check-results expected-results actual-results))
            (check (check-output expected-output actual-output))))
        failures))))

(defun run-all-tests ()
  (interactive)
  (let ((buffer (get-buffer-create "*Test Results*"))
        (pass-count 0)
        (fail-count 0)
        (error-count 0))
    (setup-temp-buffer buffer)
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-output-to-buffer (buffer)
      (dolist (test *tests*)
        (multiple-value-bind (name doc form thunk expectations) (values-list test)
          (progn
            (format t "~&~S..." name)
            (message "Running test: ~S..." name))
          (handler-case
              (let ((failures (run-test-1 test)))
                (cond ((null failures) ; passed
                       (incf pass-count)
                       (format t "OK~%"))
                      (t ; failed
                       (incf fail-count) (ding)
                       (format t "Failed~%")
                       (when doc (format t "  Description:~%~{    ~A~%~}" (split-string doc #\LFD)))
                       (dolist (failure failures)
                         (let ((report (with-output-to-string (*standard-output*)
                                         (report-failure failure))))
                           (format t "~{  ~A~%~}" (split-string report #\LFD)))))))
            (error (error)
              (incf error-count)
              (format t "~&***  Error occured while running test ~S.~%    ~A~%" name error)
              (format t "  ;; This is not error from test code, but (most likely)~%")
              (format t "  ;; bug in testing framework.")))
          (refresh-screen)))
      (format t "----------------------------------------------------------------------------~%")
      (let ((total (+ pass-count fail-count error-count)))
        (if (= total pass-count)
          (format t "total ~D tests: ALL passed." total)
          (format t "total ~D tests: ~D passed, ~D failed~@[, ~D errors~]~%"
                total pass-count fail-count (unless (zerop error-count) error-count))))
      (goto-char (point-max)))))



;;;; utilities


;;;;; load and run tests automatically

(defparameter *testfile-name* nil)
(defparameter *testfile-last-update* nil)

(defun test-on-update ()
  (when (and (file-exist-p *testfile-name*)
             (> (file-write-time *testfile-name*) *testfile-last-update*))
    (setf *testfile-last-update* (file-write-time *testfile-name*)
          *tests* nil)
    (load-test-file *testfile-name*)
    (run-all-tests)
    (refresh-screen)))

(defun watch-test-file (filename)
  (interactive "ftest file: ")
  (setf *testfile-name* filename
        *testfile-last-update* (file-write-time filename))
  (start-timer 1 'test-on-update))

(defun stop-watch-test-file ()
  (interactive)
  (while (stop-timer 'test-on-update)))


;;;;; compile and evaluate

(defmacro compile-and-eval (&body body)
  `(funcall (compile nil (eval '(lambda () ,@body)))))
(setf (get 'compile-and-eval 'ed:lisp-indent-hook) 0)


;;;;; execute in another xyzzy
;;;
;;; 注意:
;;; - readable に印字できないものが BODY に含まれているとアウト
;;; - 戻り値も readable に印字できないものはアウト
(defmacro with-another-xyzzy ((&key (options "") (timeout 60) (show :show))
                              &body body)
  `(let* ((tmpfile (make-temp-file-name))
          (create-time (file-write-time tmpfile))
          (out (gensym "tmpfile-stream-")))
     (unwind-protect
         (let ((command-string
                 (format nil "~A ~A -e ~S"
                       (merge-pathnames "xyzzy.exe" (si:system-root))
                       ,options
                       (let ((*print-circle* t))
                         (prin1-to-string
                          ;; TODO: error handling
                          `(progn
                             (with-open-file (,out ,tmpfile
                                              :direction :output
                                              :if-does-not-exist :create)
                               (handler-case
                                   (let ((values (multiple-value-list (progn ,@',body))))
                                     (format ,out ":return~%~{~S~%~}" values))
                                 (error (error)
                                   (format ,out ":error~%~S" error))))
                             (kill-xyzzy)))))))
           (let ((tmpbuf (create-new-buffer "*Temp*")))
             (setup-temp-buffer tmpbuf)
             (unwind-protect
                 (let ((proc (make-process command-string :output tmpbuf)))
                   ;; wait for finish.
                   (while (eql (process-status proc) :run)
                     (sleep-for 1)
                     (when (> (get-universal-time) (+ create-time ,timeout))
                       (kill-process proc)
                       (error "with-another-xyzzy timeout.")))
                   ;; NOTE: xyzzy always exit with code 0?
                   )
               (delete-buffer tmpbuf)))
           ;; FIXME: what if error occured?
           (with-open-file (in tmpfile :direction :input)
             (case (read in nil '#1=#:eof)
               (:return (let ((values '()) r)
                          (while (setq r (read in nil nil))
                            (push r values))
                          (nreverse values)))
               (:error  (let ((error (read in nil nil)))
                          (if (si:*structurep error)
                            (error error)
                            (error "unknown error~@[: ~S~]" error))))
               (#1#     (error "result not sent.")))))
       (delete-file tmpfile))))
(setf (get 'with-another-xyzzy 'ed:lisp-indent-hook) 1)


;;;;; faking function definition

(defmacro with-fake-functions (fakes &body body)
  `(let ((#1=#:odefs
           (list ,@(mapcar (lambda (name)
                             `(cons ',name
                                    (if (fboundp ',name)
                                      (symbol-function ',name)
                                      #2='#:undefined)))
                         (mapcar 'first fakes)))))
     ,@(mapcar (lambda (fake)
                 (let ((name (first fake))
                       (args (second fake))
                       (body (cddr fake)))
                   `(setf (symbol-function ',name) (lambda ,args ,@body))))
             fakes)
     (unwind-protect
         (progn ,@body)
       (dolist (orig #1#)
         (let ((name (car orig))
               (odef (cdr orig)))
           (if (eq odef #2#)
             (fmakunbound name)
             (setf (symbol-function name) odef)))))))
(setf (get 'with-fake-functions 'ed:lisp-indent-hook) 1
      (get 'with-fake-functions 'ed::lisp-indent-flet) t)

#+unittest-meta-test
(progn
  (ASSERT (eql #1='#:identity
               (with-fake-functions ((test (x) #1#))
                 (test 33)))))

;;; simple-test.l ends here.
