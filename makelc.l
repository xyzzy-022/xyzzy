
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "compile"))

(defpackage :makelc
  (:use :lisp :ed))

(in-package :makelc)

(defmacro time (form)
  `(let ((start (get-internal-real-time))
	 (result (multiple-value-list ,form)))
     (format t "~&~V@{-~}~%~D msec~%~%" 30 (- (get-internal-real-time) start))
     (values-list result)))

(defun not-compiled-functions (pkg)
  (let ((fns nil)
	(p (find-package pkg)))
    (do-symbols (s p)
      (when (and (eq p (symbol-package s))
		 (fboundp s)
		 (not (macro-function s)))
	(let ((f (symbol-function s)))
	  (when (not (or (si:*builtin-function-p f)
			 (compiled-function-p f)
			 (not (functionp f))
			 (and (si:*closurep f)
			      (si:closure-variable f))))
	    (push s fns)))))
    (nreverse fns)))

(defun compile-pkg (pkg)
  (format t "compiling <~A> ...~%" pkg)
  (time
   (dolist (f (not-compiled-functions pkg))
     (format t "~A ..." f)
     (handler-case
	 (progn
	   (compile f)
	   (princ "ok"))
       (error (c)
	 (format t "error~%*****~%~A~%*****"
		 (si:*condition-string c))))
     (terpri))))

(defun compile-files (&optional (dir "~/lisp"))
  (let ((files (directory dir :absolute t :wild "*.l" :recursive t)))
    (time
     (dolist (f files)
       (compile-file f)))))

(defun user::makelc ()
  (interactive)
  (with-output-to-temp-buffer ((format nil "*compile log*"))
    (compile-pkg :compiler)
    (compile-pkg :lisp)
    (compile-files "~/lisp")
    (when (file-exist-p #0="~/xyzzy.wxp")
      (delete-file #0#))))
