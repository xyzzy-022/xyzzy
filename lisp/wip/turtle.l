;;; -*- Mode: Lisp; Package: WIN-USER -*-
;;;
;;;  This module is loosely based on those from
;;;  WinMul turtle.l, which is written by УnХ╙Л`Се.
;;;
;;; Usage:
;;; (load-library "wip/turtle")
;;; (win-user::star)
;;; (win-user::spirograph)
;;; (win-user::koch)
;;; (win-user::c-curve)
;;; (win-user::cross)
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "wip/winapi"))

(in-package "win-user")

(defconstant *turtle-class* "TurtleClass")

(defparameter *turtle-window-width* 400)
(defparameter *turtle-window-height* 400)

(defstruct turtle
  hwnd
  hbm
  obm
  hdc
  (x (/ *turtle-window-width* 2))
  (y (/ *turtle-window-height* 2))
  (angle 0))

(defvar *turtle* (make-turtle))

(defun turtle-moveto (l)
  (let ((th (turtle-angle *turtle*)))
    (incf (turtle-x *turtle*) (* l (cos th)))
    (decf (turtle-y *turtle*) (* l (sin th)))))

(defun turtle-lineto (l)
  (let ((ox (turtle-x *turtle*))
	(oy (turtle-y *turtle*)))
    (turtle-moveto l)
    (MoveToEx (turtle-hdc *turtle*) ox oy 0)
    (LineTo (turtle-hdc *turtle*)
	    (turtle-x *turtle*)
	    (turtle-y *turtle*))))

(defun turtle-setpos (x y)
  (setf (turtle-x *turtle*) x)
  (setf (turtle-y *turtle*) y))

(defun turtle-left (n)
  (incf (turtle-angle *turtle*) (/ (* n pi) 180)))

(defun turtle-right (n)
  (decf (turtle-angle *turtle*) (/ (* n pi) 180)))

(defun turtle-home ()
  (setf (turtle-x *turtle*) (/ *turtle-window-width* 2))
  (setf (turtle-y *turtle*) (/ *turtle-window-height* 2))
  (setf (turtle-angle *turtle*) (/ pi 2)))

(unless (fboundp 'turtle-wndproc-stub)
  (defun-c-callable LRESULT turtle-wndproc-stub
      ((HWND hwnd) (UINT msg) (WPARAM wparam) (LPARAM lparam))
    (turtle-wndproc hwnd msg wparam lparam)))

(defun turtle-wndproc (hwnd msg wparam lparam)
  (when (= msg WM_NCDESTROY)
    (setf (turtle-hwnd *turtle*) nil))
  (DefWindowProc hwnd msg wparam lparam))

(defun register-turtle-window ()
  (let ((wc (make-WNDCLASS)))
    (setf (WNDCLASS-style wc) (logior CS_HREDRAW CS_VREDRAW))
    (setf (WNDCLASS-lpfnWndProc wc) #'turtle-wndproc-stub)
    (setf (WNDCLASS-cbClsExtra wc) 0)
    (setf (WNDCLASS-cbWndExtra wc) 0)
    (setf (WNDCLASS-hInstance wc) (GetModuleHandle 0))
    (setf (WNDCLASS-hIcon wc) (LoadIcon 0 (MAKEINTRESOURCE IDI_APPLICATION)))
    (setf (WNDCLASS-hCursor wc) (LoadCursor 0 (MAKEINTRESOURCE IDC_ARROW)))
    (setf (WNDCLASS-hbrBackground wc) (+ 1 COLOR_WINDOW))
    (setf (WNDCLASS-lpszMenuName wc) 0)
    (setf (WNDCLASS-lpszClassName wc) (si:make-string-chunk *turtle-class*))
    (RegisterClass wc)))

(defun create-turtle-window ()
  (let ((hwnd (CreateWindow (si:make-string-chunk *turtle-class*)
			    (si:make-string-chunk "Turtle")
			    WS_OVERLAPPEDWINDOW
			    CW_USEDEFAULT CW_USEDEFAULT
			    *turtle-window-width* *turtle-window-height*
			    0 0 (GetModuleHandle 0) 0)))
    (ShowWindow hwnd SW_SHOW)
    (UpdateWindow hwnd)
    hwnd))

(defun turtle-init ()
  (cond ((turtle-hwnd *turtle*)
	 (InvalidateRect (turtle-hwnd *turtle*) 0 1)
	 (UpdateWindow (turtle-hwnd *turtle*)))
	(t
	 (register-turtle-window)
	 (setf (turtle-hwnd *turtle*) (create-turtle-window)))))

(setf (get 'with-turtle-paint 'ed:lisp-indent-hook) 0)

(defmacro with-turtle-paint (&body body)
  `(progn
     (turtle-init)
     (turtle-home)
     (let (hdc)
       (unwind-protect
	   (progn
	     (setq hdc (GetDC (turtle-hwnd *turtle*)))
	     (setf (turtle-hdc *turtle*) hdc)
	     ,@body)
	 (when hdc
	   (ReleaseDC (turtle-hwnd *turtle*) hdc)
	   (setf (turtle-hdc *turtle*) nil))))))

(defun star ()
  (with-turtle-paint
    (turtle-right 90)
    (dotimes (x 5)
      (turtle-lineto 100)
      (turtle-right 144)))
  t)

(defun spirograph ()
  (with-turtle-paint
    (dotimes (x 36)
      (turtle-lineto 100)
      (turtle-right 130)))
  t)

(defun koch ()
  (with-turtle-paint
    (turtle-setpos 20 200)
    (turtle-right 90)
    (labels ((koch (x)
		   (if (< x 2)
		       (turtle-lineto x)
		     (let ((x3 (/ x 3)))
		       (koch x3)
		       (turtle-left 60)
		       (koch x3)
		       (turtle-right 120)
		       (koch x3)
		       (turtle-left 60)
		       (koch x3)))))
      (koch 300)))
  t)

(defun c-curve ()
  (with-turtle-paint
    (labels ((c-curve (x)
		      (if (< x 2)
			  (turtle-lineto x)
			(let ((x2 (/ x 2)))
			  (turtle-left 90)
			  (c-curve x2)
			  (turtle-right 90)
			  (c-curve x2)
			  (c-curve x2)
			  (turtle-right 90)
			  (c-curve x2)
			  (turtle-left 90)))))
      (c-curve 100)))
  t)

(defun cross ()
  (with-turtle-paint
    (turtle-right 90)
    (turtle-setpos 100 250)
    (labels ((cross (length level)
		    (cond ((zerop level)
			   (turtle-lineto length))
			  (t
			   (setq length (/ length 3))
			   (decf level)
			   (cross length level)
			   (turtle-right 90)
			   (cross length level)
			   (turtle-left 90)
			   (cross length level)
			   (turtle-left 90)
			   (cross length level)
			   (turtle-right 90)
			   (cross length level)))))
      (dotimes (x 4)
	(cross 150 3)
	(turtle-left 90))))
  t)
