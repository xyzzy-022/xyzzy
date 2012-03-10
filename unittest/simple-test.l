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

(defvar *test-files* nil)

(defun load-test-file (filename)
  (interactive "ftest file: ")
  (let ((*readtable* *test-file-readtable*))
    (load-file filename))
  (setf *test-files* (acons filename (file-write-time filename)
                            (remove filename *test-files* :key #'car))))

#+unittest-meta-test
(progn
  (ASSERT (let ((*readtable* *test-file-readtable*))
            (equal (read-from-string ">> hello\n")
                   '(:expected-output "hello"))))
  (ASSERT (let ((*readtable* *test-file-readtable*))
            (equal (read-from-string ">> hello")
                   '(:expected-output "hello")))))


;;;; Macro deftest
;;
;; You can define a "test" with this macro. A test consists of NAME,
;; DESCRIPTION and series of example FORM and EXPECTATIONS for that
;; form.
;; They are packed in a list and stored in variable `*tests*`.
;;
;; :: test = (NAME DESCRIPTION . EXAMPLES)

(defparameter *tests* nil)

(defun store-test (name description source examples)
  (when (member name *tests* :key #'car)
    (warn "Overwriting a test: ~S" name))
  (setf *tests*
        (cons (list* name description source examples)
              (remove name *tests* :key #'car)))
  name)

(defun find-test (name)
  (assoc name *tests*))

(defun test-name (test) (first test))
(defun test-description (test) (second test))
(defun test-source (test) (third test))
(defun test-examples (test) (cdddr test))


;;;;; test thunk
;;;
;;; Test thunk is a function (lexical closure) which returns actual
;;; behaviour of FORM.
;;; It returns followings as multiple values:
;;; - macro expansion if FORM is a macro call
;;; - result of evaluation of FORM, either returned one or more values
;;;   or signalled an error
;;; - output string printed to OUTPUT-STREAM-VAR

(defparameter *output-capture-stream* nil)

(defun make-test-thunk-form (form output-stream-var)
  `(lambda ()
     (let ((*output-capture-stream* (make-string-output-stream)))
       (values (multiple-value-bind (expn-1 expanded-p)
                   (macroexpand-1 ',form)  ; TODO: env?
                 (when expanded-p
                   (list expn-1 (macroexpand ',form))))
               (handler-case
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
                      (values (multiple-value-bind (expn-1 expanded-p)
                                  (macroexpand-1 '(+ 1 2))
                                (when expanded-p
                                  (list expn-1 (macroexpand '(+ 1 2)))))
                              (handler-case
                                  (list* :return (progn (multiple-value-list (+ 1 2))))
                                (error (error) (list :error error)))
                              nil)))))
  (ASSERT (equal (make-test-thunk-form '(princ :hello) '*standard-output*)
                 `(lambda ()
                    (let ((*output-capture-stream* (make-string-output-stream)))
                      (values (multiple-value-bind (expn-1 expanded-p)
                                  (macroexpand-1 '(princ :hello))
                                (when expanded-p
                                  (list expn-1 (macroexpand '(princ :hello)))))
                              (handler-case
                                  (list* :return
                                         (let ((*standard-output* *output-capture-stream*))
                                           (multiple-value-list (princ :hello))))
                                (error (error) (list :error error)))
                              (get-output-stream-string *output-capture-stream*)))))))


;;;;; example forms and corresponding expectations
;;;
;;; Each example form and corresponding expectations (expressed by
;;; things like `=> VALUE`) is called "example" here.
;;; 
;;; :: EXAMPLE = (FORM THUNK EXPECTED-EXPANSION EXPECTED-RESULTS EXPECTED-OUTPUT)

(defun non-nil-p (#:ignore actual) actual)

;; This is helper function for `make-examples-form`.
;; In macro `deftest` example forms and corresponding expectations
;; appear with no grouping. More badly, some of them must be paired
;; (e.g. `=>` and following value form) while expected output is
;; being a single list.
;; To make it easier to manipulate farther in `make-examples-form`,
;; they are grouped by example forms. Eache example form and corres-
;; ponding expectations are packed into a list of form:
;;
;;     (FORM EXPECTATION-0 EXPECTATION-1 .. EXPECTATION-N)
;;
;; where EXPECTATION-X is also a list of form:
;;
;;     ({=>,!!,>>,==} DATUM)
;;
(defun group-examples (forms-and-expectations)
  (labels ((output-expectation-p (x)
             (and (consp x) (eql (car x) :expected-output)))
           (recur (rest current examples)
             (let ((head (first rest))
                   (next (second rest)))
               (cond ((null rest)
                      (nreverse (cons (nreverse current) examples)))
                     ((member head '(=> !! ==))
                      (recur (cddr rest)
                             (cons (list head next) current)
                             examples))
                     ((output-expectation-p head)
                      (recur (cdr rest)
                             (cons (list '>> (second head)) current)
                             examples))
                     (t (recur (cdr rest)
                               (list head)
                               (cons (nreverse current) examples)))))))
    (let ((head (first forms-and-expectations)))
      (when (or (member head '(=> !! ==))
                (output-expectation-p head))
        (error "テスト式なしに期待とな！？: ~S" forms-and-expectations))
      (recur (cdr forms-and-expectations) (list head) nil))))

#+unittest-meta-test
(progn
  (ASSERT (equal (group-examples '(form-1 => t))
                 '((form-1 (=> t)))))
  (ASSERT (equal (group-examples '(form-1 => t form-2 (:expected-output "hi") => t))
                 '((form-1 (=> t))
                   (form-2 (>> "hi") (=> t))))))

(defun make-examples-form (name options forms-and-expectations)
  (let ((compare-fn (getf options :compare 'equal))
        (output-stream-var (getf options :output '*standard-output*)))
    (labels ((make-example-1 (x)
               ;(format t "x = ~S~%" x)
               (let ((form (first x))
                     (expectations (rest x))
                     (expected-expansion +undefined+)
                     (expected-output-string nil)
                     (expected-result-type '#1=#:undefined)
                     (expected-result-datum nil))
                 (dolist (y (rest x))
                   ;(format t "y = ~S~%" y)
                   (case (car y)
                     (=> (case expected-result-type
                           (:return (push (second y) expected-result-datum))
                           (:error  (error "Don't expect return and error at same time: ~S" name))
                           (#1#     (setf expected-result-type :return
                                          expected-result-datum (list (second y))))
                           (otherwise (error "Unknown type of expected result: ~S" name))))
                     (!! (case expected-result-type
                           (:return (error "Don't expect return and error at same time: ~S" name))
                           (:error  (error "Don't expect more than one error: ~S" name))
                           (#1#     (setf expected-result-type :error
                                          expected-result-datum (second y)))
                           (otherwise (error "Unknown type of expected result: ~S" name))))
                     (>> (setf expected-output-string
                               (format nil "~@[~A~%~]~A"
                                       expected-output-string
                                       (second y))))
                     (== (unless (undefined-p expected-expansion)
                           (error "Just one expansion of form, please: ~S" name))
                         (setf expected-expansion (second y)))
                     (otherwise (error "Unknown type of expectation: ~S: ~S" y name))))
                 `(list ',form ,(make-test-thunk-form form output-stream-var)
                        ',expected-expansion
                        ,(case expected-result-type
                           (:return `'(:return ,compare-fn ,@(nreverse expected-result-datum)))
                           (:error  `'(:error ,expected-result-datum))
                           ;; TODO: default expected-results - true? or ignore?
                           ;; With former implementation, you could have just one
                           ;; example form and default expected-results for that
                           ;; form was return true (non-nil).
                           ;; Since we now can have multiple example forms in a
                           ;; test, I doubt this is desirable for all forms. That
                           ;; is when it is just a preparation for actual test form
                           ;; return values shouldn't matter. I want to write just
                           ;; preparation form with no expectation to ignore any
                           ;; results in such case. But error might be a different
                           ;; story. Also, different behaviour for last form and
                           ;; non-last form may cause confusion.
                           ;; So, I haven't decided yet. Following is just to shut
                           ;; quick-test up.
                           (#1#     `'(:return :true))
                           (otherwise (error "Unknown type of expected result: ~S" name)))
                        '(,output-stream-var ,expected-output-string)))))
      `(list ,@(mapcar #'make-example-1
                       (group-examples forms-and-expectations))))))

#+unittest-meta-test
(let* ((examples-src
         (make-examples-form 'hi nil
                             '(form-1 => 1 => 2
                               form-2 == (expansion (of form-2)) => t
                               form-3 (:expected-output "hi")
                               form-4 (:expected-output "hi") (:expected-output "ho") => nil
                               form-5 !! type-error)))
       (examples (eval examples-src))
       ;; I need destructuring-bind...
       (1st (first examples))
       (2nd (second examples))
       (3rd (third examples))
       (4th (fourth examples))
       (5th (fifth examples)))
  (labels ((expected-expansion (example) (third example))
           (expected-results   (example) (fourth example))
           (expected-output    (example) (fifth example)))
    ;; form-1 => 1 => 2
    (ASSERT (equal (expected-results 1st)
                   '(:return equal 1 2)))
    ;; form-2 == (expansion (of form-2)) => t
    (ASSERT (equal (expected-expansion 2nd)
                   '(expansion (of form-2))))
    (ASSERT (equal (expected-results 2nd)
                   '(:return equal t)))
    ;; form-3 (:expected-output "hi")
    (ASSERT (equal (expected-output 3rd)
                   '(*standard-output* "hi")))
    ;; form-4 (:expected-output "hi") (:expected-output "ho") => nil
    (ASSERT (equal (expected-output 4th)
                   '(*standard-output* "hi\nho")))
    (ASSERT (equal (expected-results 4th)
                   '(:return equal nil)))
    ;; form-5 !! type-error
    (ASSERT (equal (expected-results 5th)
                   '(:error type-error)))
    ))

(defmacro deftest (name options doc &rest forms-and-expectations)
  (multiple-value-bind (doc forms-and-expectations)
      (if (stringp doc)
        (values doc forms-and-expectations)
        (values nil (cons doc forms-and-expectations)))
    `(store-test ',name ,doc *load-pathname*
                 ,(make-examples-form name options forms-and-expectations))))
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
;;;
;;; :: expected-expansion = EXPANSION
;;; :: actual-expansion   = (EXPANSION-1 EXPANSION) | nil

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

(defun check-macro-expansion (expected-expn actual-expns)
  "check macro expansion of FORM satisfies EXPECTED-EXPansioN.
Return nil if FORM expanded as expected. Otherwise, return a list describing
how actual expansion differs."
  (unless (undefined-p expected-expn)
    (cond ((null actual-expns)
           (list :expansion :not-expanded expected-expn))
          ((or (form-equal expected-expn (first actual-expns))
               (form-equal expected-expn (second actual-expns)))
           nil)
          (t
           (list :expansion :wrong-expansion
                 expected-expn
                 (first actual-expns)
                 (second actual-expns))))))

#+unittest-meta-test
(labels ((expn (form)
           (multiple-value-bind (expn expanded-p)
               (macroexpand-1 form)
             (when expanded-p
               (list expn (macroexpand form))))))
  (ASSERT (null  (check-macro-expansion '(setq x 1) (expn '(setf x 1)))))
  (ASSERT (equal (check-macro-expansion '(setq x 1) (expn '(setq x 1)))
                 `(:expansion :not-expanded (setq x 1))))
  (ASSERT (equal (check-macro-expansion '(set x 1) (expn '(setf x 1)))
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
;;;
;;; :: expected-results = (:return COMPARE-FN . VALUES) | (:error ERROR-TYPE)
;;; :: actual-results   = (:retrun . VALUES) | (:error ERROR)

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
         (:return
          (cond ((eql compare-fn :true)
                 (unless (second actual-results)
                   (list :results :false)))
                (t (let* ((actual-values (cdr actual-results))
                          (nexpected (length expected-values))
                          (nactuals  (length actual-values)))
                     (let ((failure (cond ((< nactuals nexpected) :too-few-values)
                                          ((> nactuals nexpected) :too-many-values)
                                          ((not (every compare-fn expected-values actual-values))
                                           :wrong-values))))
                       (when failure
                         (list :results failure expected-values actual-values compare-fn)))))))
         (:error  (let ((error (second actual-results)))
                    (return-from check-results
                      (list :results :unexpected-error
                            error
                            expected-values)))))))
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
    ((nil) (when (eql (first actual-results) :error)
             (list :results :enexpected-error (second actual-results))))
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

(define-failure-reporter :results :false ()
  (format t "Evaluation result (true):~%  Expected:~%  => #<non-nil>~%  Actually:~%  => nil~%"))

(define-failure-reporter :results :too-few-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values))

(define-failure-reporter :results :too-many-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values))

(define-failure-reporter :results :wrong-values (expected-values actual-values compare-fn)
  (report-return-values expected-values actual-values "~S" compare-fn))

(define-failure-reporter :results :unexpected-error (error &optional expected-values)
  (format t "Evaluation result (return values):~%")
  (if expected-values
    (format t "  Expected:~%~{  => ~S~%~}" expected-values)
    (format t "  Expected:~%  ;; Return normally~%"))
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
;;;
;;; :: expected-output = (STREAM-VAR OUTPUT-STRING)
;;; :: actual-output   = OUTPUT-STRING

(defun check-output (expected-output actual-output)
  "Check ACTUAL-OUTPUT is same as EXPECTED-OUTPUT."
  (unless (second expected-output) (return-from check-output nil))
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


;;;; running tests

(defvar *last-test-list* nil)

(defun get-report-buffer-stream ()
  (let ((buffer (get-buffer-create "*Test Results*")))
    (setup-temp-buffer buffer)
    (erase-buffer buffer)
    (set-buffer buffer)
    (make-buffer-stream buffer)))

(defparameter *current-session* nil)

(defstruct session
  (report-output (get-report-buffer-stream))
  (pass-count 0)
  (fail-count 0)
  finished-tests
  current-test
  remaining-tests)

(defun assert-in-session ()
  (unless *current-session*
    (error "Not in a run-tests session.")))

(macrolet ((def (slot)
             (let ((name (intern (format nil "ssn-~A" slot)))
                   (acc  (find-symbol (format nil "session-~A" slot))))
               `(defmacro ,name (&optional (session '*current-session*))
                  `(,',acc ,session)))))
  ;; (defmacro ssn-report-output (&optional (session '*current-session*))
  ;;   `(session-report-output ,session))
  (def report-output)
  (def pass-count)
  (def fail-count)
  (def finished-tests)
  (def current-test)
  (def remaining-tests))

;; :: ssn-finished-test = ((TEST (EXAMPLE . FAILURES))...)
;; :: ssn-current-test = (TEST FINISHED-EXAMPLES REMAINING-EXAMPLES)
;; :: FINISHED-EXAMPLES = ((EXAMPLE . FAILURES)...) // in reverse order

(defun run-one-example (example)
  "Execute and inspect EXAMPLE form and check if it is as expected.
Returns list of failures."
  (let ((failures '()))
    (multiple-value-bind (form thunk expected-expansion expected-results expected-output)
        (values-list example)
      (macrolet ((check (x)
                   `(let ((failure ,x))
                      (when failure (push failure failures)))))
        (multiple-value-bind (actual-expansion actual-results actual-output)
            (funcall thunk)
          (check (check-macro-expansion expected-expansion actual-expansion))
          (check (check-results expected-results actual-results))
          (check (check-output expected-output actual-output)))
        (nreverse failures)))))

(defun report-test-failures (test finished &optional (out (ssn-report-output)))
  (let ((desc (test-description test))
        (*print-circle* t))
    (when desc (format out "~{  ;; ~A~%~}" (split-string desc #\LFD)))
    (dolist (example/failures finished)
      (let ((example  (car example/failures))
            (failures (cdr example/failures)))
        (format out "  * ~S~%" (car example)) ; form
        (dolist (failure failures)
          (let ((report (with-output-to-string (*standard-output*)
                          (report-failure failure))))
            (format out "~{  ~A~%~}" (split-string report #\LFD))))))))

(defun run-current-test ()
  (assert-in-session)
  (macrolet ((test      () `(first (ssn-current-test)))
             (finished  () `(second (ssn-current-test)))
             (remaining () `(third (ssn-current-test))))
    (let ((example (first (remaining))))
      (cond ((eql (first example) :break)
             ;; FIXME: just stop (pause) execution properly.
             (setf (remaining) (rest (remaining)))
             (error "BREAK"))
            ((null (remaining))
             (let* ((finished (nreverse (finished)))
                    (passed-p (every #'null (mapcar #'cdr finished))))
               (format (ssn-report-output) "~:[Failed~;OK~]~%" passed-p)
               (message "running test: ~S...~:[Failed~;OK~]" (test-name (test)) passed-p)
               (if passed-p
                 (incf (ssn-pass-count))
                 (progn
                   (incf (ssn-fail-count))
                   (ding)
                   (report-test-failures (test) finished)))
               (push (cons (test) finished) (ssn-finished-tests))
               (setf (ssn-current-test) nil)
               (refresh-screen)
               (continue-run-tests)))
            (t
             (let ((failures (run-one-example example)))
               (setf (remaining) (rest (remaining)))
               (push (cons example failures) (finished))
               (run-current-test)))))))

(defun continue-run-tests ()
  (assert-in-session)
  (cond ((ssn-current-test)
         (run-current-test))
        ((ssn-remaining-tests)
         (let ((next (first (ssn-remaining-tests))))
           (format (ssn-report-output) "~&~S..." (test-name next))
           (message "running test: ~S..." (test-name next))
           (setf (ssn-current-test) (list next nil (test-examples next))
                 (ssn-remaining-tests) (rest (ssn-remaining-tests))))
         (continue-run-tests))
        (t ;; all tests finished. Report and die.
         (let* ((pass (ssn-pass-count))
                (fail (ssn-fail-count))
                (total (+ pass fail))
                (all-passed-p (zerop fail))
                (out (ssn-report-output)))
           (format out "~&----------------------------------------------------------------------~%")
           (format out "Total ~D tests: ~:[~D~;~*All~] passed~@[~*, ~A failed~]~%"
                   total all-passed-p pass (not all-passed-p) fail))
         (null (setf *current-session* nil)))))

(defun run-tests (&optional (test-list (or *last-test-list* *tests*)))
  (interactive)
  (unless (or (null *current-session*)
              (yes-or-no-p "Last run of tests is not finished. Abort that and run new tests?"))
    (return-from run-tests nil))
  (setf *last-test-list* test-list
        *current-session* (make-session :remaining-tests test-list))
  (continue-run-tests))


;;;;; convenience commands to run tests

(defun run-all-tests ()
  (interactive)
  (run-tests *tests*))

(defvar *run-tests-from-file-history* nil)

(setf (get 'run-tests-from-file 'ed::minibuffer-history-variable) '*run-tests-from-file-history*)

(defun run-tests-from-file (testfile)
  (interactive "ftestfile: "
    :history0 'run-tests-from-file
    :default0 (car *run-tests-from-file-history*))
  (let ((loaded (assoc testfile *test-files* :test #'path-equal)))
    (when (or (not loaded)
              (> (file-write-time testfile) (cdr loaded)))
      (load-test-file testfile)))
  (run-tests (remove-if-not (lambda (test)
                              (path-equal (test-source test) testfile))
                            *tests*)))



;;;; utilities

(defmacro with-temp-files ((&rest temp-file-vars) &body body)
  `(let (,@(mapcar (lambda (var)
                     `(,var (make-temp-file-name)))
                   temp-file-vars))
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (var)
                   `(when (file-exist-p ,var)
                      (delete-file ,var)))
                 temp-file-vars))))
(setf (get 'with-temp-files 'ed:lisp-indent-hook) 1)


(defvar *test-uid-counter* 0)
(defun test-uid ()
  (incf *test-uid-counter*))



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


(defvar *test-temp-buffer-delete* t)

(defmacro with-temp-buffer (&body body)
  `(let* ((#1=#:buf (get-buffer-create (format nil "*test-tmp-buffer-~A*" (symbol-name (gensym))))))
     (save-window-excursion
       (unwind-protect
	   (progn
	     (set-buffer #1#)
	     ,@body)
	 (and *test-temp-buffer-delete* (delete-buffer #1#))))))


;;;;; compile and evaluate

(defmacro compile-and-eval (&body body)
  `(funcall (compile nil (eval '(lambda () ,@body)))))
(setf (get 'compile-and-eval 'ed:lisp-indent-hook) 0)


;;;;; execute in another xyzzy
;;;
;;; 注意:
;;; - readable に印字できないものが BODY に含まれているとアウト
;;; - 戻り値も readable に印字できないものはアウト
;;; - レキシカル変数を持ち込めない...
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
                               (handler-case
                                   (let ((values (multiple-value-list (progn ,@',body))))
                                     (format ,out ":return~%~{~S~%~}" values))
                                 (error (error)
                                   (format ,out ":error~%~S" error))))
                             (kill-xyzzy)))))))
           (let ((tmpbuf (create-new-buffer "*Temp*")))
             (setup-temp-buffer tmpbuf)
             (let ((proc (make-process command-string :output tmpbuf)))
               (unwind-protect
                   ;; wait for finish.
                   (while (eql (process-status proc) :run)
                     (sleep-for 0.1)
                     (when (> (get-universal-time) (+ create-time ,timeout))
                       (signal-process proc)
                       (error "with-another-xyzzy timeout.")))
                 ;; FIXME: when another xyzzy doesn't close properly, it stay
                 ;; in front so user can't see test progress.
                 (activate-xyzzy-window (si:*instance-number))
                 (handler-case
                     (delete-buffer tmpbuf)
                   (error ()
                     (labels ((delete-tmpbuf ()
                                (labels ((again () (start-timer 1 #'delete-tmpbuf t)))
                                  (cond ((deleted-buffer-p tmpbuf)
                                         (message "tmpbuf deleted successfully."))
                                        ((eql (process-status proc) :exit)
                                         (handler-case
                                             (delete-buffer tmpbuf)
                                           (error () (again))))
                                        ((> (get-internal-real-time)
                                            (+ create-time 180000))
                                         (message "give up to delete tmpbuf ~S, sorry."
                                                  (buffer-name tmpbuf)))
                                        (t (again))))))
                       (start-timer 6 #'delete-tmpbuf t)))))))
           (with-open-file (in tmpfile :direction :input)
             (case (read in nil '#1=#:eof)
               (:return (let ((values '()) r)
                          (while (not (eq (setq r (read in nil '#1#)) '#1#))
                            (push r values))
                          (values-list (nreverse values))))
               (:error  (let ((error (read in nil nil)))
                          (if (si:*structurep error)
                            (error error)
                            (error "unknown error~@[: ~S~]" error))))
               (#1#     (error "result not sent.")))))
       (delete-file tmpfile))))
(setf (get 'with-another-xyzzy 'ed:lisp-indent-hook) 1)

#+unittest-meta-test
(progn
  (ASSERT (equal (multiple-value-list
                  (with-another-xyzzy (:options "-q")
                    (values 1 2 3)))
                 '(1 2 3)))
  (ASSERT (typep (handler-case
                     (with-another-xyzzy (:options "-q")
                       (error "Bah!!"))
                   (error (e) e))
                 'simple-error)))


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
