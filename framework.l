;;; -*- mode: lisp; package: user -*-

#|
使い方（暫定）:
1. どこかへ framework.l と nanri-master-change-tests.l をダウンロード
2. M-x load-file
   load file: path/to/framework.l
3. M-x load-test-file
   test file: path/to/nanri-master-change-tests.l
4. M-x change-tests

で *Test Results* だかいう名前のバッファが開いて結果が表示される。
|#

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

;; 出力のテスト。string= で比較される。>> 直後の空白は区切りであって出力の一部ではない。
(deftest NAME ()
  (princ "hello")
  >> hello)

;; 改行もそのまま書ける
(deftest NAME ()
  (format t "foo~%bar")
  >> foo
  >> bar
  => nil)

;; 指定されたエラーを投げないと fail
(deftest NAME ()
  (+ 'foo 'bar)
  !! type-error)

|#

(in-package :user)

(when (fboundp 'stop-watch-test-file)
  (stop-watch-test-file))


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


;;;; macro deftest

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

(defun non-nil-p (#:ignore actual) actual)

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
      (cond
       ((member x '(=> !!)) (setf state x))
       ((and (consp x) (eql (first x) :expected-output))
        ;(msgbox "~S~% >> ~S" name x)
        (setf output (if output (format nil "~A~%~A" output (second x)) (second x))))
       (t (case state
            (=> (case result-type
                  ((nil) (setf result-type :return))
                  (:return)
                  (:error (error "Don't expect both return value(s) and error: ~S" name)))
                (push x result-data))
            ;(>> (setf output (if output (format nil "~A~%~A" output x) x)))
            (!! (case result-type
                  (:return (error "Don't expect both return value(s) and error: ~S" name))
                  (:error  (error "Don't expect multiple errors: ~S" name)))
                (unless (symbolp x)
                  (error 'type-error :datum x :expected-type 'symbol))
                (setf result-type :error
                      result-data x))))))
    ;(multiple-value-bind (r o)
    (values (case result-type
              (:return (list* :return (getf options :compare ''equal) (nreverse result-data)))
              (:error  (list  :error result-data))
              ((nil)   (list  :return ''non-nil-p ':non-nil))
              (t (error "parse-expectations - unknown result-type: ~S: ~S" result-type name)))
            (cons dest output))
      ;(msgbox "name = ~S~2%expected-result = ~S~%expected-output = ~S" name r o)
      ;(values r o))
    ))

(defun expand-deftest (name options form expectations)
  (multiple-value-bind (expected-result expected-output)
      (parse-expectations name expectations options)
    `(setf *change-tests*
           (acons ',name ,(make-test-thunk name form expected-result expected-output)
                  (remove ',name *change-tests* :key 'car)))))


(defmacro deftest (name (&rest options) form &rest expected)
  (expand-deftest name options form expected))
(setf (get 'deftest 'ed:lisp-indent-hook) 2)


;;;; runner

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
        (message "running test: ~S..." (car x))
        (case (funcall (cdr x))
          (:pass (incf pass-count))
          (:fail (incf fail-count) (ding))
          (:error (incf error-count) (ding)))
        (refresh-screen))
      (format t "----------------------------------------------------------------------~%")
      (format t "total ~D tests, ~D passed, ~D failed, ~D Errors"
            (+ pass-count fail-count error-count) pass-count fail-count error-count)
      (goto-char (point-max)))))


;;;; utilities


;;;;; load and run tests automatically

(defparameter *testfile-name* nil)
(defparameter *testfile-last-update* nil)

(defun test-on-update ()
  (when (and (file-exist-p *testfile-name*)
             (> (file-write-time *testfile-name*) *testfile-last-update*))
    (setf *testfile-last-update* (file-write-time *testfile-name*)
          *change-tests* nil)
    (load-test-file *testfile-name*)
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
                          `(progn
                             (with-open-file (,out ,tmpfile
                                              :direction :output
                                              :if-does-not-exist :create)
                               (dolist (r (multiple-value-list (progn ,@',body)))
                                 (print r ,out)))
                             (kill-xyzzy)))))))
           (let ((tmpbuf (create-new-buffer "*Temp*")))
             (setup-temp-buffer tmpbuf)
             (let ((proc (make-process command-string :output tmpbuf)))
               ;; wait for finish.
               (while (eql (process-status proc) :run)
                 (sleep-for 1)
                 (when (> (get-universal-time) (+ create-time ,timeout))
                   (kill-process proc)
                   (error "with-another-xyzzy timeout.")))
               ;; NOTE: xyzzy always exit with code 0?
               ))
           (let ((results '()) r)
             (with-open-file (in tmpfile :direction :input)
               (while (setq r (read in nil nil))
                 (push r results)))
             (values-list (nreverse results))))
       (delete-file tmpfile))))
(setf (get 'with-another-xyzzy 'ed:lisp-indent-hook) 1)

;;; framework.l ends here.
