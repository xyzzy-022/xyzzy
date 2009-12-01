;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "lispfns")

(in-package "editor")

(defun lisp-build-summary-of-functions ()
  (let (package
	(result nil))
    (unwind-protect
	(progn
	  (do ((i 0 (+ i 1)))
	      ((let ((name (format nil "LISPFNS-~D" i)))
		 (unless (find-package name)
		   (setq package (make-package name :external-size 0))))))
	  (with-input-from-buffer ((selected-buffer) (point-min))
	    (let ((*package* package))
	      (handler-case
		  (while (peek-char t nil nil nil)
		    (let ((linenum (si:*stream-line-number *standard-input*)))
		      (let ((form (read nil nil '#1=#:eof)))
			(when (eq form '#1#)
			  (return))
			(cond ((or (atom form)
				   (atom (cdr form))
				   (not (symbolp (cadr form)))))
			      ((member (car form) '(defun defmacro) :test #'eq)
			       (push (list linenum (symbol-name (cadr form)))
				     result))))))
		(package-error () nil)
		(reader-error () nil)))))
      (when package
	(delete-package package)))
    (nreverse result)))

(defun lisp-maketags (file count)
  (let (package)
    (unwind-protect
	(progn
	  (do ((i 0 (+ i 1)))
	      ((let ((name (format nil "MAKETAGS-~D" i)))
		 (unless (find-package name)
		   (setq package (make-package name :external-size 0))))))
	  (with-input-from-buffer ((selected-buffer) (point-min))
	    (let ((*package* package))
	      (handler-case
		  (do ((form (read nil nil '#1=#:eof) (read nil nil '#1#)))
		      ((eq form '#1#))
		    (do-events)
		    (cond ((or (atom form)
			       (atom (cdr form))
			       (not (symbolp (cadr form)))))
			  ((member (car form) '(defun defmacro) :test #'eq)
			   (format t "~A~A~D" (symbol-name (cadr form)) file
				   (buffer-stream-point *standard-input*))
			   (incf count))
			  ((member (car form) '(defvar defparameter defconstant) :test #'eq)
			   (format t "~A~A~D" (symbol-name (cadr form)) file
				   (buffer-stream-point *standard-input*))
			   (incf count))))
		(package-error () nil)
		(reader-error () nil)))))
      (when package
	(delete-package package))))
  count)
