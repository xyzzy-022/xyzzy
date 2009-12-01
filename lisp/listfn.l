;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "listfn")

(in-package "editor")

(export 'list-function)

(defvar *list-function-buffer* nil)
(defvar *list-function-modified* nil)
(defvar *list-function-list* nil)
(defvar *list-function-selected* nil)

(defun list-function ()
  (interactive)
  (unless (and (eq *list-function-buffer* (selected-buffer))
	       (eql *list-function-modified* (buffer-modified-count)))
    (setq *list-function-list* (long-operation
				 (save-excursion
				   (funcall build-summary-function)))
	  *list-function-selected* nil
	  *list-function-buffer* (selected-buffer)
	  *list-function-modified* (buffer-modified-count)))
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 286 162
		    (:caption "ä÷êî(ÇæÇ¢ÇΩÇ¢)àÍóó")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:listbox list nil #x50b10111 7 7 216 148)
		     (:button IDOK "OK" #x50010001 229 7 50 14)
		     (:button IDCANCEL "ÉLÉÉÉìÉZÉã" #x50010000 229 24 50 14)))
		  (list (cons 'list *list-function-list*)
			(cons 'list *list-function-selected*)
		  )
		  `((list :column (-6 ,(apply #'max (mapcar #'(lambda (x)
								(length (cadr x)))
							    *list-function-list*)))
			  :must-match t :enable (IDOK) :label 1)))
    (when result
      (setq *list-function-selected* (cdr (assoc 'list data)))
      (goto-line (car *list-function-selected*)))))
