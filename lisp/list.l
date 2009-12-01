;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "list")

(in-package "lisp")

(export '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
	  cdadar cdaddr cddaar cddadr cdddar cddddr
	  first second third fourth fifth sixth seventh eighth ninth tenth
	  butlast nbutlast ldiff tailp union nunion intersection nintersection
	  set-difference nset-difference set-exclusive-or nset-exclusive-or subsetp))
(export '(safe-car safe-cdr safe-caar safe-cadr safe-cdar safe-cddr
	  safe-caaar safe-caadr safe-cadar safe-caddr
	  safe-cdaar safe-cdadr safe-cddar safe-cdddr
	  safe-caaaar safe-caaadr safe-caadar safe-caaddr
	  safe-cadaar safe-cadadr safe-caddar safe-cadddr
	  safe-cdaaar safe-cdaadr safe-cdadar safe-cdaddr
	  safe-cddaar safe-cddadr safe-cdddar safe-cddddr))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun caaaar (x) (car (car (car (car x)))))
(defun caaadr (x) (car (car (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
(si:*fset 'first #'car)
(si:*fset 'second #'cadr)
(si:*fset 'third #'caddr)
(si:*fset 'fourth #'cadddr)
(defun fifth (x) (nth 4 x))
(defun sixth (x) (nth 5 x))
(defun seventh (x) (nth 6 x))
(defun eighth (x) (nth 7 x))
(defun ninth (x) (nth 8 x))
(defun tenth (x) (nth 9 x))

(defun safe-car (x) (and (consp x) (car x)))
(defun safe-cdr (x) (and (consp x) (cdr x)))
(defun safe-caar (x) (safe-car (safe-car x)))
(defun safe-cadr (x) (safe-car (safe-cdr x)))
(defun safe-cdar (x) (safe-cdr (safe-car x)))
(defun safe-cddr (x) (safe-cdr (safe-cdr x)))
(defun safe-caaar (x) (safe-car (safe-car (safe-car x))))
(defun safe-caadr (x) (safe-car (safe-car (safe-cdr x))))
(defun safe-cadar (x) (safe-car (safe-cdr (safe-car x))))
(defun safe-caddr (x) (safe-car (safe-cdr (safe-cdr x))))
(defun safe-cdaar (x) (safe-cdr (safe-car (safe-car x))))
(defun safe-cdadr (x) (safe-cdr (safe-car (safe-cdr x))))
(defun safe-cddar (x) (safe-cdr (safe-cdr (safe-car x))))
(defun safe-cdddr (x) (safe-cdr (safe-cdr (safe-cdr x))))
(defun safe-caaaar (x) (safe-car (safe-car (safe-car (safe-car x)))))
(defun safe-caaadr (x) (safe-car (safe-car (safe-car (safe-cdr x)))))
(defun safe-caadar (x) (safe-car (safe-car (safe-cdr (safe-car x)))))
(defun safe-caaddr (x) (safe-car (safe-car (safe-cdr (safe-cdr x)))))
(defun safe-cadaar (x) (safe-car (safe-cdr (safe-car (safe-car x)))))
(defun safe-cadadr (x) (safe-car (safe-cdr (safe-car (safe-cdr x)))))
(defun safe-caddar (x) (safe-car (safe-cdr (safe-cdr (safe-car x)))))
(defun safe-cadddr (x) (safe-car (safe-cdr (safe-cdr (safe-cdr x)))))
(defun safe-cdaaar (x) (safe-cdr (safe-car (safe-car (safe-car x)))))
(defun safe-cdaadr (x) (safe-cdr (safe-car (safe-car (safe-cdr x)))))
(defun safe-cdadar (x) (safe-cdr (safe-car (safe-cdr (safe-car x)))))
(defun safe-cdaddr (x) (safe-cdr (safe-car (safe-cdr (safe-cdr x)))))
(defun safe-cddaar (x) (safe-cdr (safe-cdr (safe-car (safe-car x)))))
(defun safe-cddadr (x) (safe-cdr (safe-cdr (safe-car (safe-cdr x)))))
(defun safe-cdddar (x) (safe-cdr (safe-cdr (safe-cdr (safe-car x)))))
(defun safe-cddddr (x) (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))

(defun butlast (list &optional (n 1))
  (nreverse (nthcdr n (reverse list))))

(defun nbutlast (list &optional (n 1))
  (cond ((< n 0)
	 (error 'range-error :datum n))
	((= n 0)
	 list)
	(t
	 (setq n (- (length list) n 1))
	 (when (>= n 0)
	   (setf (cdr (nthcdr n list)) nil)
	   list))))

(defun ldiff (list object)
  (do* ((list list (cdr list))
	(r (list nil))
	(splice r (cdr splice)))
      ((atom list)
       (progn
	 (or (eq list object)
	     (rplacd splice list))
	 (cdr r)))
    (when (eq list object)
      (return (cdr r)))
    (rplacd splice (list (car list)))))

(defun tailp (sublist list)
  (do ((l list (cdr l)))
      ((eq l sublist) t)
    (when (atom l)
      (return nil))))

(defun union (list1 list2 &rest rest &key test test-not key)
  (let (head p)
    (dolist (l list1 (if p (progn (setf (cdr p) list2) head) list2))
      (unless (apply #'member (if key (funcall key l) l) list2 rest)
	(if p
	    (progn
	      (setf (cdr p) (cons l nil))
	      (setq p (cdr p)))
	  (progn
	    (setq head (cons l nil))
	    (setq p head)))))))

(defun nunion (list1 list2 &rest rest &key test test-not key)
  (let (head p)
    (do ((l list1 (cdr l)))
	((endp l) (if p (progn (setf (cdr p) list2) head) list2))
      (unless (apply #'member (if key (funcall key (car l)) (car l)) list2 rest)
	(if p
	    (setf (cdr p) l)
	  (setq head l))
	(setq p l)))))

(defun intersection (list1 list2 &rest rest &key test test-not key)
  (let ((x '()))
    (dolist (l list1 x)
      (when (apply #'member (if key (funcall key l) l) list2 rest)
	(push l x)))))

(defun nintersection (list1 list2 &rest rest &key test test-not key)
  (let (head p)
    (do ((l list1 (cdr l)))
	((endp l) (progn (if p (setf (cdr p) 'nil)) head))
      (when (apply #'member (if key (funcall key (car l)) (car l)) list2 rest)
	(if p
	    (setf (cdr p) l)
	  (setq head l))
	(setq p l)))))

(defun set-difference (list1 list2 &rest rest &key test test-not key)
  (let ((x '()))
    (dolist (l list1 x)
      (unless (apply #'member (if key (funcall key l) l) list2 rest)
	(push l x)))))

(defun nset-difference (list1 list2 &rest rest &key test test-not key)
  (let (head p)
    (do ((l list1 (cdr l)))
	((endp l) (progn (if p (setf (cdr p) 'nil)) head))
      (unless (apply #'member (if key (funcall key (car l)) (car l)) list2 rest)
	(if p
	    (setf (cdr p) l)
	  (setq head l))
	(setq p l)))))

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (nconc (apply #'set-difference list1 list2 rest)
	 (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (nconc (apply #'set-difference list1 list2 rest)
	 (apply #'nset-difference list2 list1 rest)))

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (dolist (l list1 t)
    (unless (apply #'member (if key (funcall key l) l) list2 rest)
      (return nil))))
