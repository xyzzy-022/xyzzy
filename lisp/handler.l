;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "handler")

(in-package "lisp")

(export '(handler-bind handler-case ignore-errors
	  signal error warn make-condition))

(setf (get 'handler-bind 'ed:lisp-indent-hook) 1)

(defmacro handler-bind (bindings &body body)
  (when (notevery #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "不正なコンディションハンドラフォームです"))
  `(let ((si:*condition-handlers* (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
							bindings))
					si:*condition-handlers*)))
     ,@body))

(setf (get 'handler-case 'ed:lisp-indent-hook) 1)

(defmacro handler-case (form &rest cases)
  (let ((no-error (assoc ':no-error cases)))
    (cond (no-error
	   (unless (eq (caar (last cases)) ':no-error)
	     (error ":no-errorが最後の句ではありません"))
	   `(block #1=#:error-return
	      (multiple-value-call #'(lambda ,@(cdr no-error))
		(block #2=#:normal-return
		  (return-from #1#
		    (handler-case (return-from #2# ,form)
		      ,@(nreverse (cdr (reverse cases)))))))))
	  (t
	   (let ((tag-cases (mapcar #'(lambda (x) (cons (gensym) x)) cases)))
	     `(block #3=#:block
		(let ((#4=#:var))
		  (tagbody
		    (handler-bind ,(mapcar #'(lambda (x)
					      (list (cadr x)
						    `#'(lambda (temp)
							,@(if (caddr x)
							      '((setq #4# temp)))
							(go ,(car x)))))
					   tag-cases)
		      (return-from #3# ,form))
		    ,@(mapcan #'(lambda (x)
				 (list (car x)
				       `(return-from #3#
					  ,(if (caddr x)
					       `(let ((,(caaddr x) #4#))
						  ,@(cdddr x))
					     `(progn ,@(cdddr x))))))
			      tag-cases)))))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c) (values 'nil c))))

(defun *error (datum args condition)
  (cond ((typep datum 'condition)
	 (if args
	     (error 'too-many-arguments))
	 (si:*throw-error datum))
	((stringp datum)
	 (si:*throw-error (make-condition condition
					  :format-string datum
					  :format-arguments args)))
	((subtypep datum 'condition)
	 (si:*throw-error (apply #'make-condition datum args)))
	(t
	 (error 'type-error
	   :datum datum
	   :expected-type '(or condition string)))))

(defun error (datum &rest args)
  (*error datum args 'simple-error))

(defun warn (datum &rest args)
  (*error datum args 'simple-warning))

(defun signal (datum &rest args)
  (*error datum args 'simple-condition))
