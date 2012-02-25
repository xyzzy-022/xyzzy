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

(defparameter *change-tests* nil)

(defun make-test-thunk (name form expected-values compare dest expected-output)
  `(lambda ()
     (block ,name
       (format t "~&~A..." ',name)
       (handler-case
           (let ((capture (make-string-output-stream)))
             (let ((values (let ((,dest capture))
                             (multiple-value-list ,form)))
                   (output (get-output-stream-string capture))
                   (passed t))
               (labels ((fail (fmt &rest args)
                          (when passed
                            (format t "Failed~%")
                            (setf passed nil))
                          (apply #'format t fmt args)))
                 ,(when (stringp expected-output)
                    `(unless (string= output ,expected-output)
                       (fail "  Output:~%  - Expected ---~%~{  >> ~A~%~}  --------------~%  - Actually ---~%~{  >> ~A~%~}  --------------~%"
                             (split-string ,expected-output #\LFD)
                             (split-string output #\LFD))))
                 (unless (and (= (length values) ,(length expected-values))
                              (every ,compare values ',expected-values))
                   (fail "  Return values:~%  - Expected: ~{~S~^, ~}~%  - Actually: ~{~S~^, ~}~%"
                         ',expected-values values))
                 (when passed
                   (format t "OK.~%"))
                 (if passed :pass :fail))))
         (error (error)
           (format t "Error: ~A~%" error)
           :error)))))

(defun parse-expectations (expectations)
  "Return followings:
- expected values
- expected output"
  (let ((values '())
        (output nil)
        (state nil))
    (dolist (x expectations)
      (if (member x '(=> >>))
        (setf state x)
        (case state
          (=> (push x values))
          (>> (when output
                (error "don't put output expectation more than once."))
              (setf output x)))))
    (values (nreverse values)
            output)))


(defun expand-deftest (name options form expectations)
  ;(msgbox "name => ~S~%options => ~S~%form => ~S~%expected => ~S" name options form expected)
  (labels ((get-option (key &optional default) (getf options key default)))
    (multiple-value-bind (evalues eoutput)
        (parse-expectations expectations)
      (multiple-value-bind (evalues compare)
          (if evalues
            (values evalues (get-option :compare '#'equal))
            (values '(t) '(lambda (value #:ignore) value)))
        `(push (cons ',name ,(make-test-thunk name form
                                              evalues compare
                                              '*standard-output* eoutput))
               *change-tests*)))))

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
  (stop-timer 'test-on-update))


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
