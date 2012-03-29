;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "window")

(in-package "editor")

(export '(count-windows pop-to-buffer other-window move-previous-window
	  *pop-up-windows* display-buffer
	  split-window-vertically shrink-window
	  enlarge-window-horizontally shrink-window-horizontally
	  pos-visible-in-window-p recenter
	  *window-flag-line-number* *window-flag-ruler*
	  *window-flag-newline*
	  *window-flag-tab* *window-flag-zenkaku-space*
	  *window-flag-mode-line* *window-flag-vscroll-bar*
	  *window-flag-hscroll-bar* *window-flag-eof*
	  *window-flag-fold-line* *window-flag-cursor-line*
	  *window-flag-fold-mark*
	  *window-flag-half-width-space* *window-flag-full-width-space*
	  *window-flag-just-inverse* *window-flag-alternate-vscroll-bar*
	  *window-flag-scrolling*
	  toggle-line-number toggle-ruler toggle-newline toggle-tab
	  toggle-zenkaku-space toggle-full-width-space
	  toggle-vscroll-bar toggle-hscroll-bar
	  toggle-eof toggle-mode-line toggle-function-bar
	  toggle-fold-mark toggle-fold-line toggle-cursor-line
	  toggle-half-width-space
	  buffer-fold-width
	  split-window-menu-update
	  close-window-menu-update
	  move-window-menu-update
	  set-buffer-fold-type-none
	  set-buffer-fold-type-window
	  set-buffer-fold-type-column
	  set-buffer-fold-type-none-update
	  set-buffer-fold-type-window-update
	  set-buffer-fold-type-column-update))

(defvar-local buffer-fold-width 80)

(defvar *pop-up-windows* t)

(define-condition no-window-error (range-error)
  ()
  (:report (lambda (c s) (format s "他にウィンドウがありません")))
  (:important nil))

(defun count-windows (&optional minibuf)
  (let ((count 1)
	(current (selected-window))
	next)
    (and minibuf
	 (minibuffer-window-p current)
	 (setq current (next-window current)))
    (do ((count 1 (+ count 1))
	 (next (next-window current minibuf) (next-window next minibuf)))
	((eq next current)
	 count))))

(defun pop-to-buffer (buffer &optional popup-windows vert-p)
  (if (eq buffer (selected-buffer))
      t
    (let ((w (get-buffer-window buffer (selected-window))))
      (if w
	  (set-window w)
	(let ((w (selected-window))
	      (nwindows (count-windows 'arg)))
	  (when (minibuffer-window-p w)
	    (other-window -1))
	  (when (and (= nwindows 1) popup-windows)
	    (split-window popup-windows vert-p)
	    (when (and (not (integerp popup-windows))
		       (minibuffer-window-p w))
	      (set-window w)
	      (other-window -1)))
	  (when (and (eq (selected-window) w)
		     (> nwindows 1))
	    (other-window 1)
	    (when (minibuffer-window-p (selected-window))
	      (other-window -2)))
	  (set-buffer buffer))))))

(defun display-buffer (buffer &optional not-this-window)
  (interactive "BDisplay Buffer: \np")
  (let ((curw (selected-window)))
    (when (and (null not-this-window)
	       (eq (window-buffer curw) buffer))
      (return-from display-buffer curw))
    (let ((w (get-buffer-window buffer)))
      (when (and w (null not-this-window)
		 (not (eq w curw)))
	(return-from display-buffer curw)))
    (pop-to-buffer buffer *pop-up-windows*)
    (setq w (selected-window))
    (if (and (eq w curw)
	     (> (count-windows) 1))
	(other-window)
      (set-window curw))
    w))

(defun other-window (&optional (arg 1 f) no-error)
  (interactive "p")
  (let* ((current (selected-window))
	 (target current))
    (if (> arg  0)
	(dotimes (x arg)
	  (setq target (next-window target))
	  (when (eq target current)
	    (and f (not no-error)
		 (error 'no-window-error :datum arg))
	    (return)))
      (dotimes (x (- arg))
	(setq target (previous-window target))
	  (when (eq target current)
	    (and f (not no-error)
		 (error 'no-window-error :datum arg))
	    (return))))
    (set-window target)))

(defun move-previous-window (&optional (arg 1 f))
  (interactive "p")
  (other-window (- arg) (null f)))

(defun split-window-vertically (&optional arg)
  (interactive "p")
  (split-window arg t))

(defun shrink-window (&optional (arg 1))
  (interactive "p")
  (enlarge-window (- arg)))

(defun enlarge-window-horizontally (&optional (arg 1))
  (interactive "p")
  (enlarge-window arg 't))

(defun shrink-window-horizontally (&optional (arg 1))
  (interactive "p")
  (enlarge-window-horizontally (- arg)))

(defun pos-visible-in-window-p (point &optional window)
  (not (pos-not-visible-in-window-p point window)))

(defun recenter (&optional arg)
  (interactive "p")
  (save-excursion
    (scroll-window (- (get-window-line)
		      (if arg
			  (if (minusp arg)
			      (max (- (window-height) arg) 0)
			    (min arg (1- (window-height))))
			(truncate (window-height) 2))))))

(defconstant *window-flag-line-number* #x0001)
(defconstant *window-flag-ruler* #x0002)
(defconstant *window-flag-newline* #x0004)
(defconstant *window-flag-tab* #x0008)
(defconstant *window-flag-full-width-space* #x0010)
(defconstant *window-flag-zenkaku-space* *window-flag-full-width-space*)
(defconstant *window-flag-vscroll-bar* #x0020)
(defconstant *window-flag-eof* #x0040)
(defconstant *window-flag-hscroll-bar* #x0080)
(defconstant *window-flag-mode-line* #x0100)
(defconstant *window-flag-function-bar* #x0200)
(defconstant *window-flag-fold-mark* #x0400)
(defconstant *window-flag-cursor-line* #x0800)
(defconstant *window-flag-half-width-space* #x1000)
(defconstant *window-flag-alternate-vscroll-bar* #x2000)
(defconstant *window-flag-scrolling* #x4000)
(defconstant *window-flag-just-inverse* #x8000)
(defconstant *window-flag-fold-line* #x10000)

(defun toggle-window-flag (f arg sv)
  (cond ((null sv)
	 (set-window-flags (logxor (get-window-flags) f)))
	((if (numberp arg)
	     (minusp arg)
	   (null arg))
	 (set-window-flags (logand (get-window-flags) (lognot f))))
	(t
	 (set-window-flags (logior (get-window-flags) f)))))

(defun toggle-line-number (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-line-number* arg sv))

(defun toggle-ruler (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-ruler* arg sv))

(defun toggle-newline (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-newline* arg sv))

(defun toggle-tab (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-tab* arg sv))

(defun toggle-zenkaku-space (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-zenkaku-space* arg sv))

(setf (symbol-function 'toggle-full-width-space) #'toggle-zenkaku-space)

(defun toggle-vscroll-bar (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-vscroll-bar* arg sv))

(defun toggle-eof (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-eof* arg sv))

(defun toggle-hscroll-bar (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-hscroll-bar* arg sv))

(defun toggle-mode-line (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-mode-line* arg sv))

(defun toggle-cursor-line (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-cursor-line* arg sv))

(defun toggle-function-bar (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-function-bar* arg sv))

(defun toggle-fold-line (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-fold-line* arg sv))

(defun toggle-fold-mark (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-fold-mark* arg sv))

(defun toggle-cursor-line (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-cursor-line* arg sv))

(defun toggle-half-width-space (&optional (arg nil sv))
  (interactive "p")
  (toggle-window-flag *window-flag-half-width-space* arg sv))

(defun split-window-menu-update ()
  (if (minibuffer-window-p (selected-window)) :disable))

(defun close-window-menu-update ()
  (if (or (minibuffer-window-p (selected-window))
	  (eq (selected-window) (next-window (selected-window) 'arg)))
      :disable))

(defun move-window-menu-update ()
  (if (eq (selected-window) (next-window (selected-window)))
      :disable))

(defun set-buffer-fold-type-none ()
  (interactive)
  (set-buffer-fold-width nil))

(defun set-buffer-fold-type-window ()
  (interactive)
  (set-buffer-fold-width t))

(defun set-buffer-fold-type-column ()
  (interactive)
  (set-buffer-fold-width buffer-fold-width))

(defun set-buffer-fold-type-none-update ()
  (and (null (buffer-fold-width)) :check))

(defun set-buffer-fold-type-window-update ()
  (and (eq (buffer-fold-width) t) :check))

(defun set-buffer-fold-type-column-update ()
  (and (integerp (buffer-fold-width)) :check))

(define-key ctl-x-map #\0 'delete-window)
(define-key ctl-x-map #\1 'delete-other-windows)
(define-key ctl-x-map #\2 'split-window)
(define-key ctl-x-map #\5 'split-window-vertically)
(define-key ctl-x-map #\o 'other-window)
(define-key ctl-x-map #\p 'move-previous-window)
(define-key ctl-x-map #\z 'enlarge-window)
(define-key ctl-x-map #\^ 'enlarge-window)
(define-key ctl-x-map #\C-z 'shrink-window)
(define-key ctl-x-map #\} 'enlarge-window-horizontally)
(define-key ctl-x-map #\{ 'shrink-window-horizontally)
(global-set-key #\C-l 'recenter)
