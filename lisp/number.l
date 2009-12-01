;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "number")

(in-package "lisp")

(export '(1+ 1-))
(export '(logand logior logxor logeqv lognand lognor
	  logandc1 logandc2 logorc1 logorc2))
(export '(boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2
	  boole-and boole-ior boole-xor boole-eqv boole-nand boole-nor
	  boole-andc1 boole-andc2 boole-orc1 boole-orc2))
(export '(sinh cosh tanh asinh acosh atanh phase cis))
(export '(byte byte-size byte-position ldb ldb-test mask-field
	  dpb deposit-field))

(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)

(defun logand (&rest args)
  (let ((r -1))
    (dolist (x args r)
      (setq r (boole boole-and r x)))))

(defun logior (&rest args)
  (let ((r 0))
    (dolist (x args r)
      (setq r (boole boole-ior r x)))))

(defun logxor (&rest args)
  (let ((r 0))
    (dolist (x args r)
      (setq r (boole boole-xor r x)))))

(defun logeqv (&rest args)
  (let ((r -1))
    (dolist (x args r)
      (setq r (boole boole-eqv r x)))))

(defun lognand (x y)
  (boole boole-nand x y))

(defun lognor (x y)
  (boole boole-nor x y))

(defun logandc1 (x y)
  (boole boole-andc1 x y))

(defun logandc2 (x y)
  (boole boole-andc2 x y))

(defun logorc1 (x y)
  (boole boole-orc1 x y))

(defun logorc2 (x y)
  (boole boole-orc2 x y))

(defun cis (z)
  (exp (complex (- (imagpart z)) (realpart z))))

(defun phase (z)
  (atan (imagpart z) (realpart z)))

(defun sinh (z)
  (/ (- (exp z) (exp (- z))) 2))

(defun cosh (z)
  (/ (+ (exp z) (exp (- z))) 2))

(defun tanh (z)
  (let ((z1 (exp z))
	(z2 (exp (- z))))
    (/ (- z1 z2) (+ z1 z2))))

(defun asinh (z)
  (log (+ z (sqrt (1+ (* z z))))))

(defun acosh (z)
  (log (+ z (* (1+ z) (sqrt (/ (1- z) (1+ z)))))))

(defun atanh (z)
  (log (* (1+ z) (sqrt (/ 1 (- 1 (* z z)))))))

(setf (symbol-function 'byte) #'cons)
(setf (symbol-function 'byte-size) #'car)
(setf (symbol-function 'byte-position) #'cdr)

(defun ldb (bytespec integer)
  (logandc2 (ash integer (- (byte-position bytespec)))
	    (- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  (not (zerop (ldb bytespec integer))))

(defun mask-field (bytespec integer)
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun dpb (newbyte bytespec integer)
  (logxor integer
          (mask-field bytespec integer)
          (ash (logandc2 newbyte (- (ash 1 (byte-size bytespec))))
	       (byte-position bytespec))))

(defun deposit-field (newbyte bytespec integer)
  (dpb (ash newbyte (- (byte-position bytespec))) bytespec integer))
