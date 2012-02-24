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
|#

(in-package :user)

(defparameter *change-tests* nil)

(defun make-test-thunk (name form expected compare)
  `(lambda ()
     (block ,name
       (format t "~&~A..." ',name)
       (handler-case
           (let ((values (multiple-value-list ,form)))
             (if (and (= (length values) ,(length expected))
                      (every ,compare values ',expected))
               (progn
                 (format t "OK~%")
                 :pass)
               (progn
                 (format t "Failed~%  Expected: ~{~S~^, ~}~%  Actually: ~{~S~^, ~}~%"
                       ',expected values)
                 :fail)))
         (error (error)
           (format t "Error: ~A~%" error)
           :error)))))

(defun expand-deftest (name options form expected)
  ;(msgbox "name => ~S~%options => ~S~%form => ~S~%expected => ~S" name options form expected)
  (labels ((get-option (key &optional default) (getf options key default)))
    (multiple-value-bind (expected compare)
        (if expected
          (values (remove '=> expected) (get-option :compare '#'equal))
          (values '(t) '(lambda (value #:ignore) value)))
      `(push (cons ',name ,(make-test-thunk name form expected (get-option :compare '#'equal)))
             *change-tests*))))

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
;|#

;; 2011-12-20  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix equalp for hash-table.
;; 	equalp に hash-table を渡すと多値が返ってくる問題を修正。
;; 	https://gist.github.com/1480046
;; 
(deftest equalp-for-hash-table ()
  (let ((a (make-hash-table))
        (b (make-hash-table)))
    (setf (gethash 1 a) "foo"
          (gethash 1 b) "foo")
    (equalp a b))
  => t)

;; 2011-06-17  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix sxhash.
;; 	sxhash の値が equalp 用の値になっているが equal 用の値になるように修正。
;; 	(= (sxhash (list "ABC")) (sxhash (list "ABC"))) => t
;; 	(= (sxhash (list "abc")) (sxhash (list "ABC"))) => nil
;;
(deftest sxhash-fix-00 ()
  (= (sxhash (list "ABC")) (sxhash (list "ABC")))
  => t)

(deftest sxhash-fix-01 ()
  (= (sxhash (list "abc")) (sxhash (list "ABC")))
  => nil)




(when (yes-or-no-p "今すぐテストする？")
  (test-changes))
;;; test-changes.l ends here.
