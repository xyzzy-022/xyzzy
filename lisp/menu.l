;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "menu")

(in-package "editor")

(export '(define-menu define-popup-menu))

(defun define-menu-expand (tag popup items)
  `(let ((#1=#:menu ,(if popup `(create-popup-menu ,tag) `(create-menu ,tag))))
     ,@(let ((result nil))
	 (dolist (i items)
	   (push (cond ((or (eq i :sep)
			    (eq i :separator))
			`(add-menu-separator #1#))
		       ((or (eq (car i) :sep)
			    (eq (car i) :separator))
			`(add-menu-separator #1# ,(cadr i)))
		       ((eq (car i) :popup)
			(when (< (length i) 3)
			  (error ":popupの形式が不正です: ~S" i))
			`(add-popup-menu #1# ,(define-menu-expand (cadr i)
								  nil
								  (cdddr i))
					 ,(caddr i)))
		       ((eq (car i) :item)
			(when (< (length i) 3)
			  (error ":itemの形式が不正です: ~S" i))
			`(add-menu-item #1# ,(if (eq (cadr i) ':tag-command)
						 (let ((command (cadddr i)))
						   (and (eq (safe-car command) 'quote)
							(symbolp (safe-cadr command))
							command))
					       (cadr i))
					,@(cddr i)))
		       ((eq (car i) :menu)
			(when (< (length i) 3)
			  (error ":menuの形式が不正です: ~S" i))
			`(add-popup-menu #1# ,(cadddr i) ,(caddr i)))
		       (t
			(error "未サポートのオプションです: ~S" (car i))))
		 result))
	 (nreverse result))
     #1#))

(setf (get 'define-menu 'lisp-indent-hook) 'defun)

(defmacro define-menu (&body body)
  (define-menu-expand nil nil body))

(setf (get 'define-popup-menu 'lisp-indent-hook) 'defun)

;(defmacro define-popup-menu (&body body)
;  (define-menu-expand nil t body))

(defmacro define-popup-menu (&body body)
  (if (eq (safe-caar body) 'quote)
      (ed::define-menu-expand (car body) t (cdr body))
    (ed::define-menu-expand nil t body)))
