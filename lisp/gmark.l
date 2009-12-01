;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "gmark")

(in-package "editor")

(export '(global-mark-goto global-mark-add global-mark-clear
	  global-mark-delete *global-mark-max* mark-dialog-box
	  *gmark-keep-column*))

(defvar *global-mark-list* nil)
(defvar *last-global-mark* nil)
(defvar *global-mark-max* 100)
(defvar *gmark-keep-column* nil)

(defun global-mark-add ()
  (let ((x (car *global-mark-list*)))
    (unless (and x
		 (eq (marker-buffer x) (selected-buffer))
		 (eql (marker-point x) (point)))
      (push (set-marker (make-marker)) *global-mark-list*)
      (setq *last-global-mark* nil)
      (let ((exceed (- (length *global-mark-list*) *global-mark-max*)))
	(when (> exceed 0)
	  (setq *global-mark-list* (nbutlast *global-mark-list* exceed))))))
  t)

(defun global-mark-goto (marker)
  (set-buffer (marker-buffer marker))
  (goto-char (marker-point marker))
  (or *gmark-keep-column*
      (goto-bol))
  t)

(defun global-mark-delete (marker)
  (setq *global-mark-list* (delete marker *global-mark-list* :test #'eq))
  (delete-marker marker)
  t)

(defun global-mark-clear ()
  (dolist (marker *global-mark-list*)
    (delete-marker marker))
  (setq *global-mark-list* nil)
  t)

(defun global-mark-make-list ()
  (let (result)
    (save-excursion
      (dolist (x *global-mark-list*)
	(let ((point (marker-point x))
	      (buffer (marker-buffer x)))
	  (when (and point buffer)
	    (set-buffer buffer)
	    (save-excursion
	      (goto-char point)
	      (goto-bol)
	      (setq point (point))
	      (push (list x
			  buffer
			  (current-line-number)
			  (buffer-substring point
					    (progn
					      (goto-eol)
					      (min (point)
						   (+ point 128)))))
		    result))))))
    (nreverse result)))

(defvar *mark-dialog-template*
  '(dialog 0 0 260 123
    (:caption "マーク")
    (:font 9 "MS UI Gothic")
    (:control
     (:listbox list nil #x50a10011 4 5 192 114)
     (:button go "移動(&G)" #x50030001 205 5 52 14)
     (:button add "追加(&A)" #x50030000 205 22 52 14)
     (:button delete "削除(&D)" #x50030000 205 39 52 14)
     (:button clear "全削除(&L)" #x50030000 205 56 52 14)
     (:button IDCANCEL "キャンセル" #x50030000 205 73 52 14))))

(defun mark-dialog-box ()
  (interactive)
  (let ((list (global-mark-make-list)))
    (multiple-value-bind (result data)
	(dialog-box *mark-dialog-template*
		    (list (cons 'list list)
			  (cons 'list (find *last-global-mark* list :key #'car)))
		    '((list :column (0 15 -6 128) :must-match t :enable (go delete))
		      (add :no-result t)
		      (clear :no-result t)))
      (cond ((eq result 'go)
	     (setq *last-global-mark* (cadr (assoc 'list data)))
	     (global-mark-goto *last-global-mark*))
	    ((eq result 'add)
	     (global-mark-add))
	    ((eq result 'delete)
	     (global-mark-delete (cadr (assoc 'list data))))
	    ((eq result 'clear)
	     (global-mark-clear))
	    (t nil)))))

(global-set-key #\C-F2 'mark-dialog-box)
