;;; -*-Lisp-*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "tarai")

(defun tak-n (n)
  (format t "Answer = ~D~%" (tak (* 3 n) (* 2 n) n)))

(defun tak (x y z)
  (if (< y x)
      (tak (tak (1- x) y z)
	   (tak (1- y) z x)
	   (tak (1- z) x y))
    z))

(let ((t1 (get-internal-real-time))
      (t2 (progn
	    (tak-n 4)
	    (get-internal-real-time))))
  (format t "Elapsed time = ~,3F seconds.~%" (/ (- t2 t1) 1000.0)))
