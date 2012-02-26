;;; -*- mode: lisp; package: user -*-

#|
;; FORM の戻り値が VALUE と equal でないと fail
(deftest NAME ()
  FORM
  => VALUE)

;; FORM が多値を返すという指定
;; 実際に返ってきた多値が指定したより多くても少なくても fail
(deftest NAME ()
  FORM
  => VALUE-0
  => VALUE-1)

;; 罠注意: 戻り値を省略した場合、FORM が non-nil を返さないと fail
(deftest NAME ()
  FORM)

;; 比較関数を指定
(deftest NAME (:compare '=)
  0
  => 0/0)

;; 出力のテスト
(deftest NAME ()
  (princ "hello")
  >> "hello")

|#

(in-package :user)

(when (fboundp 'stop-watch-test-file)
  (stop-watch-test-file))

(defparameter *change-tests* nil)

(defun make-test-thunk (name form expected-result expected-output)
  (multiple-value-bind (expected-type expected-datum compare-fn)
      (case (car expected-result)
        (:return (values :return (cddr expected-result) (second expected-result)))
        (:error  (values :error  (second expected-result))))
    (multiple-value-bind (output-stream expected-output)
        (values (car expected-output) (cdr expected-output))
      (let ((capture-stream (gensym "capture-stream-")))
        `(lambda ()
           (format t "~&~A..." ',name)
           (multiple-value-bind (actual-type actual-datum actual-output)
               (let ((,capture-stream (make-string-output-stream)))
                 (handler-case
                     (let ((,output-stream ,capture-stream))
                       (values :return (multiple-value-list ,form)
                               #1=(get-output-stream-string ,capture-stream)))
                   (error (error)
                     (values :error error #1#))))
             (let ((failed-p nil))
               (labels ((fail (fmt &rest args)
                          (unless failed-p
                            (setf failed-p t)
                            (format t "Failed~%"))
                          (apply #'format t fmt args)))
                 ,(when (stringp expected-output)
                    `(unless (string= actual-output ,expected-output)
                       (fail "  Output (~S):~%    Expected:~%~{    >> ~A~%~}    Actually:~%~{    >> ~A~%~}"
                             ',output-stream
                             ',(split-string expected-output #\LFD t)
                             (split-string actual-output #\LFD t))))
                 ,(case expected-type
                    ;; WTF...
                    (:return
                     `(case actual-type
                        (:return
                         (unless (and (= (length actual-datum) ,(length expected-datum))
                                      (every ,compare-fn ',expected-datum actual-datum))
                           (fail "  Return values (~S):~%    Expected:~%~{    => ~S~%~}    Actually:~%~{    => ~S~%~}"
                                 ,compare-fn ',expected-datum actual-datum)))
                        (:error
                         (fail "  Return values (~S):~%    Expected:~%~{    => ~S~%~}    Actually:~%    !! ~S: ~A~%"
                               ,compare-fn ',expected-datum
                               #2=(si:*structure-definition-name (si:*structure-definition actual-datum))
                               actual-datum))))
                    (:error
                     `(case actual-type
                        (:return
                         (fail "  Error:~%    Expected:~%    !! ~S~%    Actually:~%~{    => ~S~%~}"
                               ',expected-datum actual-datum))
                        (:error
                         (unless (si:*structure-subtypep
                                  (si:*structure-definition actual-datum)
                                  (get ',expected-datum 'si:structure-definition))
                           (fail "  Error:~%    Expected:~%    !! ~S~%    Actually:~%    !! ~S: ~A~%"
                                 ',expected-datum #2# actual-datum))))))
                 (unless failed-p
                   (format t "OK.~%"))
                 (if failed-p :fail :pass)))))))))

(defun parse-expectations (name expectations options)
  "Return followings:
- expected values
- expected output"
  (let ((result-type nil)
        (result-data '())
        (output nil)
        (dest   (getf options :output '*standard-output*))
        (state  nil))
    (dolist (x expectations)
      (if (member x '(=> >> !!))
        (setf state x)
        (case state
          (=> (case result-type
                ((nil) (setf result-type :return))
                (:return)
                (:error (error "Don't expect both return value(s) and error: ~S" name)))
              (push x result-data))
          (>> (setf output (if output (format nil "~A~%~A" output x) x)))
          (!! (case result-type
                (:return (error "Don't expect both return value(s) and error: ~S" name))
                (:error  (error "Don't expect multiple errors: ~S" name)))
              (unless (symbolp x)
                (error 'type-error :datum x :expected-type 'symbol))
              (setf result-type :error
                    result-data x)))))
    (values (case result-type
              (:return (list* :return (getf options :compare ''equal) (nreverse result-data)))
              (:error  (list  :error result-data))
              ((nil)   (list  :return (lambda (#:ignore actual) actual) '(t)))
              (t (error "parse-expectations - unknown result-type: ~S: ~S" result-type name)))
            (cons dest output))))

(defun expand-deftest (name options form expectations)
  (multiple-value-bind (expected-result expected-output)
      (parse-expectations name expectations options)
    `(setf *change-tests*
           (acons ',name ,(make-test-thunk name form expected-result expected-output)
                  (remove ',name *change-tests* :key 'car)))))


(defmacro deftest (name (&rest options) form &rest expected)
  (expand-deftest name options form expected))
(setf (get 'deftest 'ed:lisp-indent-hook) 2)

(defun test-changes ()
  (interactive)
  (let ((buffer (get-buffer-create "*Test Results*"))
        (pass-count 0)
        (fail-count 0)
        (error-count 0))
    (setup-temp-buffer buffer)
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-output-to-buffer (buffer)
      (dolist (x *change-tests*)
        (case (funcall (cdr x))
          (:pass (incf pass-count))
          (:fail (incf fail-count) (ding))
          (:error (incf error-count) (ding))))
      (format t "----------------------------------------------------------------------~%")
      (format t "total ~D tests, ~D passed, ~D failed, ~D Errors"
            (+ pass-count fail-count error-count) pass-count fail-count error-count)
      (goto-char (point-max)))))

;;;; utilities

(defparameter *testfile-name* nil)
(defparameter *testfile-last-update* nil)

(defun test-on-update ()
  (when (and (file-exist-p *testfile-name*)
             (> (file-write-time *testfile-name*) *testfile-last-update*))
    (setf *testfile-last-update* (file-write-time *testfile-name*)
          *change-tests* nil)
    (load-file *testfile-name*)
    (test-changes)
    (refresh-screen)))

(defun watch-test-file (filename)
  (interactive "ftest file: ")
  (setf *testfile-name* filename
        *testfile-last-update* (file-write-time filename))
  (start-timer 1 'test-on-update))

(defun stop-watch-test-file ()
  (interactive)
  (while (stop-timer 'test-on-update)))

#|
(deftest I-am-t ()
  t
  => t)

(deftest I-am-keyword ()
  :keyword
  => :keyword)

(deftest I-am-nil ()
  nil
  => nil)

(deftest foo-eql-foo-00 ()
  (eql 'foo 'foo))

(deftest foo-eql-foo-01 ()
  (eql 'foo 'foo)
  => t)

(deftest 1-is-1 (:compare '=)
  1
  => 1/1)

(deftest output-test-test-00 ()
  (princ :hello)
  >> "hello"
  => :hello)

(deftest output-test-test-01 ()
  (princ :hello)
  >> "hi")

;|#

;;; test-changes.l ends here.
