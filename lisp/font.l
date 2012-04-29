;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "font")

(in-package "editor")

(export '(get-text-font
	  set-text-font
	  decrease-text-font-size
	  increase-text-font-size
	  ))

(defconstant +font-size-points+
  '(6 7 8 9 10 11 12 13 14 16 18 20 22 24 26 28 36))
(defconstant +font-size-min-pixel+ 8)
(defconstant +font-size-max-pixel+ 48)


(defun get-text-font (lang)
  (or (cdr (assoc lang (get-text-fontset)))
      (error "不正な文字セットです: ~S" lang)))

(defun set-text-font (lang &key face size size-pixel-p)
  (let ((font (get-text-font lang)))
    (when font
      (setf (getf font :face) face)
      (setf (getf font :size) size)
      (setf (getf font :size-pixel-p) size-pixel-p)
      (set-text-fontset `((,lang ,@font))))))


(defun decrease-text-font-size (&optional (arg 1))
  (interactive "p")
  (increase-text-font-size (- arg)))

(defun increase-text-font-size (&optional (arg 1))
  (interactive "p")
  (let ((fontset (get-text-fontset)))
    (dolist (lang/font fontset)
      (let ((size (getf (cdr lang/font) :size)))
	(if (getf (cdr lang/font) :size-pixel-p)
	    (setf size (max +font-size-min-pixel+ (min (incf size arg) +font-size-max-pixel+)))
	  (let* ((nsize (find-nealest-font-size size +font-size-points+))
		 (pos (+ (position nsize +font-size-points+) arg)))
	    (setf size (nth (max 0 (min pos (1- (length +font-size-points+))))
			    +font-size-points+))))
	(setf (getf (cdr lang/font) :size) size)))
    (set-text-fontset fontset)))


(labels ((calc-pixel (n)
	   (+ +font-size-min-pixel+
	      (round (* (- +font-size-max-pixel+ +font-size-min-pixel+)
			(/ n 4)))))
	 (calc-point (n)
	   (nth n '(6 13 21 28 36))))

  (defun set-text-font-size (size)
    (let ((fontset (get-text-fontset)))
      (dolist (lang/font fontset)
	(setf (getf (cdr lang/font) :size)
	      (funcall (if (getf (cdr lang/font) :size-pixel-p)
			   #'calc-pixel #'calc-point)
		       (case size
			 (:x-small 0)
			 (:small 1)
			 (:medium 2)
			 (:large 3)
			 (:x-large 4)
			 (t (error "不正な文字サイズです: ~S" size))
			 ))))
      (set-text-fontset fontset)))

  (defun get-ascii-text-font-size ()
    (let ((font (get-text-font :ascii)))
      (when font
	(let ((size (getf font :size))
	      (candidates (mapcar #'cons
				  (mapcar (if (getf font :size-pixel-p)
					      #'calc-pixel #'calc-point)
					  '(0 1 2 3 4))
				  '(:x-small :small :medium :large :x-large))))
	  (values
	   (cdr (find-nealest-font-size size candidates :key #'car))
	   size))))))

(defun find-nealest-font-size (size candidates &key (key 'identity))
  (let (nearlest)
    (dolist (c candidates)
      (when (<= (abs (- (funcall key c)
			size))
		(abs (- (or (funcall key nearlest) most-positive-fixnum)
			size)))
	(setf nearlest c)))
    nearlest))
