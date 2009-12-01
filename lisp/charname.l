;;; -*- Mode: Lisp; Package: SYSTEM -*-
;;;
;;; This file is part of xyzzy.
;;;

(in-package "system")

(provide "charname")
(require "cnamedef")

(defun make-character-name-hash-table ()
  (let ((v (make-vector 65536))
	(h (make-hash-table :size 1 :test 'equal)))
    (dolist (x *character-name-alist*)
      (setf (svref v (cdr x)) (car x))
      (setf (gethash (car x) h) (ed:unicode-char (cdr x))))
    (dotimes (i char-code-limit)
      (let* ((c (code-char i))
	     (u (ed:char-unicode c)))
	(when (and u (svref v u))
	  (let ((cs (ed:iso-char-charset c)))
	    (when cs
	      (setf (gethash (concatenate 'string (symbol-name cs)
					  "_" (svref v u)) h)
		    (code-char i)))))))
    h))

(setq *character-name-hash-table*
      (make-character-name-hash-table))
