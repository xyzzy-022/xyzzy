;;; -*- Mode: Lisp; Package: SYSTEM -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "hash")

(in-package "lisp")

(export '(maphash with-hash-table-iterator))

(in-package "system")

(defun maphash (fn hash-table)
  (let ((i 0)
	key val)
    (loop
      (multiple-value-setq (i key val)
	(si:*enum-hash-table hash-table i))
      (unless i
	(return))
      (funcall fn key val))))

(defmacro with-hash-table-iterator ((mname hash-table) &body body)
  `(let ((#1=#:hash-table ,hash-table)
	 (#2=#:i 0)
	 #3=#:key
	 #4=#:val)
     (flet ((,mname ()
	     (multiple-value-setq (#2# #3# #4#)
	       (si:*enum-hash-table #1# #2#))
	     (if #2# (values t #3# #4#))))
       ,@body)))
