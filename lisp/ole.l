;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "ole")

(in-package "editor")

(export 'ole-for-each)

(defconstant *ole-reader-.* '#:|.|)
(defconstant *ole-reader-[* '#:[)
(defconstant *ole-reader-}* '#:})

(defvar .keyword-package. (find-package "keyword"))

(defvar *ole-readtable*
  (let ((readtable (copy-readtable)))
    (set-macro-character #\. #'(lambda (stream c) *ole-reader-.*) nil readtable)
    (set-macro-character #\[ #'(lambda (stream c) *ole-reader-[*) nil readtable)
    (set-macro-character #\} #'(lambda (stream c) *ole-reader-}*) nil readtable)
    (set-syntax-from-char #\] #\) readtable)
    readtable))

(defun ole-reader-unexpected (stream obj)
  (error 'reader-error
    :stream (if (buffer-stream-p stream)
		(buffer-name (buffer-stream-buffer stream))
	      (ignore-errors (namestring stream)))
    :linenum (si:*stream-line-number stream)
    :datum "Unexpected"
    :arguments (list obj)))

(defun ole-reader-read-obj (stream)
  (let ((obj (read stream t nil t)))
    (if (or (eq obj *ole-reader-.*)
	    (eq obj *ole-reader-[*)
	    (eq obj *ole-reader-}*))
	(ole-reader-unexpected stream (symbol-name obj))
      obj)))

(defun ole-reader (stream char param)
  (let ((*readtable* *ole-readtable*))
    (let* ((obj (ole-reader-read-obj stream))
	   (first obj))
      (loop
	(let ((sep (read stream t nil t)))
	  (cond ((eq sep *ole-reader-.*)
		 (setq obj `(ole-method ,obj ',(let ((*package* .keyword-package.))
						 (ole-reader-read-obj stream)))))
		((eq sep *ole-reader-[*)
		 (when (eq obj first)
		   (ole-reader-unexpected stream #\[))
		 (unwind-protect
		     (progn
		       (set-syntax-from-char #\. #\.)
		       (setq obj (nconc obj (read-delimited-list #\] stream t))))
		   (set-macro-character #\. #'(lambda (stream c) *ole-reader-.*))))
		((eq sep *ole-reader-}*)
		 (when (eq obj first)
		   (ole-reader-unexpected stream #\}))
		 (return obj))
		(t
		 (ole-reader-unexpected stream sep))))))))

(set-dispatch-macro-character #\# #\{ #'ole-reader)

(defsetf ole-method (obj prop &rest args) (x)
  `(progn
     (ole-putprop ,obj ,prop ,x ,@args)
     ,x))

(defmacro ole-for-each ((var obj &optional (resultform nil)) &body body)
  `(let ((#1=#:e #{,obj._NewEnum}))
     (ole-enumerator-reset #1#)
     (loop
       (let ((,var (ole-enumerator-next #1#)))
	 (when (eq ,var :empty)
	   (return ,resultform))
	 ,@body))))

(setf (get 'ole-for-each 'lisp-indent-hook) 1)
