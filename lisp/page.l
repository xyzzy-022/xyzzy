;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "page")

(in-package "editor")

(export '(next-page previous-page *page-scroll-half-window* *next-screen-context-lines*
	  page-delimiter forward-page backward-page mark-page
	  *page-scroll-keep-window-position*))

(defvar *page-scroll-half-window* nil)
(defvar *next-screen-context-lines* 2)
(defvar *page-scroll-keep-window-position* t)

(or (boundp 'page-delimiter)
    (setq page-delimiter "^\f"))

(defun next-page (&optional (arg 1))
  (interactive)
  (cond ((scroll-window (* (max (if *page-scroll-half-window*
				    (truncate (window-height) 2)
				  (- (window-height) *next-screen-context-lines*))
				1)
			   arg)))
	((or *page-scroll-keep-window-position*
	     (zerop arg)
	     (if (plusp arg) (eobp) (bobp)))
	 nil)
	(t
	 (goto-char (if (plusp arg) (point-max) (point-min))))))

(defun previous-page ()
  (interactive)
  (next-page -1))

(defun forward-page (&optional (arg 1))
  (interactive "p")
  (if (plusp arg)
      (dotimes (x arg)
	(unless (scan-buffer page-delimiter :regexp t :no-dup t)
	  (goto-char (point-max))
	  (return)))
    (dotimes (x (- arg))
      (unless (scan-buffer page-delimiter :regexp t :no-dup t :reverse t)
	(goto-char (point-min))
	(return))))
  t)

(defun backward-page (&optional (arg 1))
  (interactive "p")
  (forward-page (- arg)))

(defun mark-page (&optional (arg 0))
  (interactive "p")
  (if (plusp arg)
      (forward-page arg)
    (if (minusp arg)
	(forward-page (- arg 1))))
  (forward-page 1)
  (set-mark-command)
  (forward-page -1))

(global-set-key #\PageDown 'next-page)
(global-set-key #\PageUp 'previous-page)
(global-set-key #\C-z 'previous-page)
(global-set-key #\M-v 'previous-page)
(global-set-key #\C-v 'next-page)
(define-key ctl-x-map #\[ 'backward-page)
(define-key ctl-x-map #\] 'forward-page)
(define-key ctl-x-map #\C-p 'mark-page)
