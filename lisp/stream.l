;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "stream")

(in-package "lisp")

(export '(with-open-stream with-input-from-string with-output-to-string
	  with-open-file prin1 princ print prin1-to-string princ-to-string
	  read-from-string))

(setf (get 'with-open-stream 'ed:lisp-indent-hook) 1)

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (decl body)
      (find-declaration body)
    `(let ((,var ,stream))
       ,@decl
       (unwind-protect
	   (multiple-value-prog1
	       (progn ,@body)
	     (and ,var (close ,var)))
	 (and ,var (close ,var :abort t))))))

(setf (get 'with-input-from-string 'ed:lisp-indent-hook) 1)

(defmacro with-input-from-string ((var string &key index start end) &body body)
  (if index
      `(let ((,var (make-string-input-stream ,string, start, end)))
	 (multiple-value-prog1
	     (progn ,@body)
	   (setf ,index (si:*get-string-input-stream-index ,var))))
    `(let ((,var (make-string-input-stream ,string, start, end)))
       ,@body)))

(setf (get 'with-output-to-string 'ed:lisp-indent-hook) 1)

(defmacro with-output-to-string ((var &optional string) &body body)
  (if string
      `(let ((,var (si:*make-string-output-stream-from-string ,string)))
	 ,@body)
    `(let ((,var (make-string-output-stream)))
       ,@body
       (get-output-stream-string ,var))))

(setf (get 'with-open-file 'ed:lisp-indent-hook) 1)

(defmacro with-open-file ((stream filename &rest options) &body body)
  `(with-open-stream (,stream (open ,filename ,@options))
     ,@body))

(defun read-from-string (string &optional (eof-error-p t) eof-value
				&key start end preserve-white-space)
  (let (object index)
    (with-input-from-string (s string :index index :start start :end end)
      (setq object (if preserve-white-space
		       (read-preserving-whitespace s eof-error-p eof-value)
		     (read s eof-error-p eof-value))))
    (values object index)))

(defun prin1 (object &optional stream)
  (write object :stream stream :escape t))

(defun princ (object &optional stream)
  (write object :stream stream :escape nil :readably nil))

(defun print (object &optional stream)
  (prog2
      (terpri stream)
      (prin1 object stream)
    (write-char #\SPC stream)))

(defun prin1-to-string (object &optional stream)
  (write-to-string object :stream stream :escape t))

(defun princ-to-string (object &optional stream)
  (write-to-string object :stream stream :escape nil :readably nil))

(defsetf readtable-case (readtable) (readtable-case)
  `(si:*set-readtable-case ,readtable ,readtable-case))


(in-package "system")

(export '(output-debug-string
	  ))

(defun output-debug-string (fmt &rest args)
  (apply #'format ed:*debug-output* fmt args))
