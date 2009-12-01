;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "imagehdr")
(in-package "editor")

(export '(get-image-size read-jpeg-header read-gif-header read-png-header))

(defun read-jpeg-header (filename)
  (with-open-file (s filename
		     :direction :input
		     :encoding :binary
		     :if-does-not-exist :error)
    (ignore-errors
     (when (and (eql (read-char s) #\xff)
		(eql (read-char s) #\xd8))
       (flet ((getw () (+ (ash (char-code (read-char s)) 8)
			  (char-code (read-char s)))))
	 (loop
	   (let (c)
	     (while (not (eql (read-char s) #\xff)))
	     (while (eql (setq c (read-char s)) #\xff))
	     (cond ((find c "\xc0\xc1\xc2\xc3\xc5\xc6\xc7\xc9\xca\xcb\xcd\xce\xcf")
		    (return-from read-jpeg-header
		      (progn
			(when (< (getw) 8)
			  (return nil))
			(read-char s)
			(let ((h (getw)))
			  (values (getw) h ':jpeg)))))
		   ((find c "\xc4\xc8\xcc\xdb\xdc\xdd\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xfe")
		    (let ((l (- (getw) 2)))
		      (when (minusp l)
			(return nil))
		      (dotimes (x l)
			(read-char s))))
		   ((find c "\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\x01"))
		   (t (return nil))))))))))

(defun read-gif-header (filename)
  (with-open-file (s filename
		     :direction :input
		     :encoding :binary
		     :if-does-not-exist :error)
    (ignore-errors
     (let ((buf (make-sequence 'string 6)))
       (dotimes (x 6)
	 (setf (svref buf x) (read-char s)))
       (if (or (string= buf "GIF87a")
	       (string= buf "GIF89a"))
	   (return-from read-gif-header
	     (values (+ (char-code (read-char s))
			(ash (char-code (read-char s)) 8))
		     (+ (char-code (read-char s))
			(ash (char-code (read-char s)) 8))
		     ':gif)))))))

(defun read-png-header (filename)
  (with-open-file (s filename
		     :direction :input
		     :encoding :binary
		     :if-does-not-exist :error)
    (ignore-errors
     (dolist (x '(#\x89 #\P #\N #\G #\x0d #\x0a #\x1a #\x0a
		  #\x00 #\x00 #\x00 #\x0d #\I #\H #\D #\R))
       (or (eql (read-char s) x)
	   (return-from read-png-header nil)))
     (flet ((getl () (+ (ash (char-code (read-char s)) 24)
			(ash (char-code (read-char s)) 16)
			(ash (char-code (read-char s)) 8)
			(char-code (read-char s)))))
       (return-from read-png-header
	 (values (getl) (getl) ':png))))))

(defun get-image-size (filename)
  (dolist (f '(read-jpeg-header read-gif-header read-png-header))
    (multiple-value-bind (w h fmt)
	(funcall f filename)
      (when w
	(return (values w h fmt))))))
