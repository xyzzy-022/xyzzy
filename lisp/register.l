;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "register")

(in-package "editor")

(export '(point-to-register jump-to-register insert-register
	  copy-to-register append-to-register copy-rectangle-to-register
	  save-window-configuration-to-register
	  restore-window-configuration-register
	  prepend-to-register view-register))

(defvar *register-alist* nil)

(defun set-register (r value)
  (let ((al (assoc r *register-alist*)))
    (cond (al
	   (setf (cdr al) value))
	  (t
	   (push (cons r value) *register-alist*))))
  value)

(defun get-register (r)
  (cdr (assoc r *register-alist*)))

(defun point-to-register (r)
  (interactive "cPoint to register: ")
  (set-register r (point-marker)))

(defun jump-to-register (r)
  (interactive "cJump to register: ")
  (let ((val (get-register r)))
    (cond ((markerp val)
	   (switch-to-buffer (marker-buffer val))
	   (goto-char val))
	  ((null val)
	   (error "レジスタ~Aが設定されていません" r))
	  (t
	   (error "レジスタにマーカが入っていません")))))

(defun insert-register (r)
  (interactive "cInsert register: ")
  (let ((val (get-register r)))
    (cond ((and (consp val)
		(not (eq (car val) 'window-configuration)))
	   (let ((*rectangle-kill-buffer* val))
	     (yank-rectangle)))
	  ((stringp val)
	   (insert val))
	  ((null val)
	   (error "レジスタ~Aが設定されていません" r))
	  (t
	   (error "レジスタにテキストが入っていません")))))

(defun copy-to-register (r start end &optional delete)
  (interactive "cCopy to register: \nr\nP")
  (set-register r (buffer-substring start end))
  (if delete
      (delete-region start end))
  t)

(defun append-to-register (r start end &optional delete)
  (interactive "cAppend to register: \nr\nP")
  (let ((val (get-register r)))
    (unless (stringp val)
      (error "レジスタにテキストが入っていません"))
    (set-register r (concat val (buffer-substring start end)))
    (if delete
	(delete-region start end))
    t))

(defun prepend-to-register (r start end &optional delete)
  (interactive "cAppend to register: \nr\nP")
  (let ((val (get-register r)))
    (unless (stringp val)
      (error "レジスタにテキストが入っていません"))
    (set-register r (concat (buffer-substring start end) val))
    (if delete
	(delete-region start end))
    t))

(defun copy-rectangle-to-register (r start end &optional delete)
  (interactive "cCopy rectangle to register: \nr\nP")
  (set-register r (let ((*rectangle-kill-buffer* nil))
		    (if delete
			(kill-rectangle start end)
		      (copy-rectangle start end))
		    *rectangle-kill-buffer*)))

(defun save-window-configuration-to-register (r)
  (interactive "cSave winconf to register: ")
  (set-register r (current-window-configuration)))

(defun restore-window-configuration-register (r)
  (interactive "cRestore winconf register: ")
  (let ((val (get-register r)))
    (cond ((and (consp val)
		(eq (car val) 'window-configuration))
	   (set-window-configuration val))
	  ((null val)
	   (error "レジスタ~Aが設定されていません" r))
	  (t
	   (error "レジスタにWindow-configurationが入っていません")))))

(defun view-register (r)
  (interactive "cView register: ")
  (let ((val (ed::get-register r)))
    (if (null val)
	(message "Register ~a is empty" r)
      (with-output-to-temp-buffer ("*output*")
	(format t "Register \"~a\" contains " r)
	(cond ((markerp val)
	       (let ((buffer (marker-buffer val)))
		 (if (null buffer)
		     (princ "a marker in deleted buffer.")
		   (format t "a buffer position:\nbuffer ~a, position ~a"
			   (buffer-name buffer) (marker-point val)))))
	      ((and (consp val)
		    (eq (car val) 'window-configuration))
	       (princ "a window configuration."))
	      ((consp val)
	       (format t "the rectangle:\n~{~a~^\n~}" val))
	      ((stringp val)
	       (format t "the text:\n~a" val))
	      (t
	       (format t "Garbage:\n~s" val)))))))

(define-key ctl-x-map #\r (make-sparse-keymap))
(define-key ctl-x-map '(#\r #\SPC) 'point-to-register)
(define-key ctl-x-map '(#\r #\j) 'jump-to-register)
(define-key ctl-x-map '(#\r #\s) 'copy-to-register)
(define-key ctl-x-map '(#\r #\x) 'copy-to-register)
(define-key ctl-x-map '(#\r #\i) 'insert-register)
(define-key ctl-x-map '(#\r #\g) 'insert-register)
(define-key ctl-x-map '(#\r #\a) 'append-to-register)
(define-key ctl-x-map '(#\r #\r) 'copy-rectangle-to-register)
(define-key ctl-x-map '(#\r #\w) 'save-window-configuration-to-register)
(define-key ctl-x-map '(#\r #\c) 'restore-window-configuration-register)
