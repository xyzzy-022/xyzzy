;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "package")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *package* (find-package "lisp")))

(export '(in-package defpackage find-all-symbols do-symbols do-external-symbols
	  do-all-symbols with-package-iterator))

(defun in-package-1 (name)
  (let ((package (find-package name)))
    (unless package
      (error "パッケージが存在しません: ~S" name))
    (setq *package* package)))

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (in-package-1 ,name)))

(defun coerce-to-package (x)
  (let ((p (find-package x)))
    (unless p
      (error "パッケージが見つかりません: ~S" x))
    p))

(defun string-or-symbol (x)
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	(t (error 'type-error :datum x :expected-type '(or string symbol)))))

(defun defpackage-check-dups (nsymbols shadow shadowing-import-from import-from intern export)
  (let* ((h (make-hash-table :size nsymbols :test #'equal))
	 option
	 (fn #'(lambda (x)
		(let ((o (gethash x h)))
		  (when o
		    (error "~Sと~Sで~Aが2回以上指定されています" o option x)))
		(setf (gethash x h) option))))
    (setq option :shadow)
    (mapc fn shadow)
    (setq option :shadowing-import-from)
    (dolist (pkg/syms shadowing-import-from)
      (mapc fn (cdr pkg/syms)))
    (setq option :import-from)
    (dolist (pkg/syms import-from)
      (mapc fn (cdr pkg/syms)))
    (setq option :intern)
    (mapc fn intern)
    (clrhash h)
    (setq option :intern)
    (mapc fn intern)
    (setq option :export)
    (mapc fn export)))

(defun defpackage-find-symbol (list)
  (let ((syms '()))
    (dolist (l list syms)
      (let ((p (car l)))
	(dolist (n (cdr l))
	  (multiple-value-bind (s f)
	      (find-symbol n p)
	    (unless (or (eq f :internal)
			(eq f :external))
	      (error "~Sにシンボル~Aが見つかりません" p n))
	    (push s syms)))))))

(defun defpackage-parse-options (package options)
  (let ((nicknames '())
	(shadow '())
	(shadowing-import-from '())
	(use '())
	(import-from '())
	(intern '())
	(export '())
	(nsymbols 0)
	internal
	external
	documentation)
    (dolist (opt options)
      (unless (consp opt)
	(error "defpackageのオプションの形式が不正です: ~S" opt))
      (let ((x (car opt))
	    (to-string #'(lambda (x)
			  (setq nsymbols (+ nsymbols 1))
			  (string-or-symbol x))))
	(cond ((eq x :nicknames)
	       (setq nicknames (nconc (mapcar #'(lambda (x)
						 (setq x (string-or-symbol x))
						 (let ((p (find-package x)))
						   (when (and p (not (eq p package)))
						     (error "同じ名前のパッケージが存在します: ~S" x)))
						 x)
					      (cdr opt))
				      nicknames)))
	      ((eq x :shadow)
	       (setq shadow (nconc (mapcar to-string (cdr opt)) shadow)))
	      ((eq x :shadowing-import-from)
	       (push (cons (coerce-to-package (cadr opt))
			   (mapcar to-string (cddr opt)))
		     shadowing-import-from))
	      ((eq x :use)
	       (setq use (nconc (mapcar #'(lambda (x)
					   (coerce-to-package x)
					   (string-or-symbol x))
					(cdr opt))
				use)))
	      ((eq x :import-from)
	       (push (cons (coerce-to-package (cadr opt))
			   (mapcar to-string (cddr opt)))
		     import-from))
	      ((eq x :intern)
	       (setq intern (nconc (mapcar to-string (cdr opt)) intern)))
	      ((eq x :export)
	       (setq export (nconc (mapcar to-string (cdr opt)) export)))
	      ((eq x :internal-size)
	       (setq internal (cadr opt)))
	      ((eq x :external-size)
	       (setq external (cadr opt)))
	      ((eq x :documentation)
	       (setq documentation (cadr opt)))
	      (t
	       (error "未サポートのオプションです: ~S" x)))))
    (defpackage-check-dups nsymbols shadow shadowing-import-from import-from intern export)
    (values nicknames
	    shadow
	    (defpackage-find-symbol shadowing-import-from)
	    use
	    (defpackage-find-symbol import-from)
	    intern
	    export
	    internal
	    external
	    documentation)))

(defun defpackage-helper (package-name options)
  (setq package-name (string-or-symbol package-name))
  (multiple-value-bind (nicknames shadow shadowing-import use import
				  intern export internal external documentation)
      (defpackage-parse-options (find-package package-name) options)
    (let ((nicknames (and nicknames `(:nicknames ',nicknames)))
	  (use (if (or shadow shadowing-import (null use)) use `(:use ',use)))
	  (internal (and internal `(:internal-size ,internal)))
	  (external (and external `(:external-size ,external)))
	  (forms '()))
      (and (or shadow shadowing-import)
	   (push '(unuse-package (package-use-list package) package) forms))
      (and shadow
	   (push `(mapc #'(lambda (x) (shadow (intern x package) package)) ',shadow) forms))
      (and shadowing-import
	   (push `(shadowing-import ',shadowing-import package) forms))
      (and (or shadow shadowing-import)
	   (push '(use-package use package) forms))
      (and import
	   (push `(import ',import package) forms))
      (and intern
	   (push `(mapc #'(lambda (x) (intern x package)) ',intern) forms))
      (and export
	   (push `(export (mapcar #'(lambda (x) (intern x package)) ',export) package) forms))
      (and documentation
	   (push `(si:*set-package-documentation package ',documentation) forms))
      (if forms
	  `(eval-when (:compile-toplevel :load-toplevel :execute)
	     (let* ((package (or (find-package ,package-name)
				 (make-package ,package-name
					       ,@nicknames
					       ,@(and (not (or shadow shadowing-import))
						      use)
					       ,@internal
					       ,@external)))
		    ,@(and (or shadow shadowing-import)
			   `((use ,(if use
				       (list 'quote use)
				     '(package-use-list package))))))
	       ,@(nreverse forms)
	       package))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (or (find-package ,package-name)
	       (make-package ,package-name ,@nicknames ,@use ,@internal ,@external)))))))

(defmacro defpackage (package-name &rest options)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defpackage-helper package-name options)))

(defun find-all-symbols (name)
  (let ((result '()))
    (setq name (string-or-symbol name))
    (dolist (p (list-all-packages) result)
      (multiple-value-bind (s f)
	  (find-symbol name p)
	(if (or (eq f ':internal)
		(eq f ':external))
	    (push s result))))))

(defmacro do-symbols ((var &optional (package nil sv) result-form) &body body)
  (let ((vl (gensym))
	(v (gensym))
	(l (gensym))
	(i (gensym))
	(break (gensym))
	(retval (gensym))
	(pkg (gensym)))
    (multiple-value-bind (decl body)
	(find-declaration body)
      `(let ((,vl '()))
	 ,@decl
	 (setq ,pkg (if ,sv ,package *package*))
	 (push (si:*package-external ,pkg) ,vl)
	 (push (si:*package-internal ,pkg) ,vl)
	 (dolist (x (package-use-list ,pkg))
	   (push (si:*package-external x) ,vl))
	 (block ,break
	   (dolist (,v ,vl (let ((,var nil)) ,result-form))
	     (let ((,l (length ,v)))
	       (dotimes (,i ,l)
		 (let* ((,break 't)
			(,retval (dolist (,var (svref ,v ,i) (setq ,break 'nil))
				   ,@body)))
		   (when ,break
		     (return-from ,break ,retval)))))))))))

(defmacro do-external-symbols ((var &optional (package nil sv) result-form) &body body)
  (let ((v (gensym))
	(l (gensym))
	(i (gensym))
	(break (gensym))
	(retval (gensym))
	(pkg (gensym)))
    (multiple-value-bind (decl body)
	(find-declaration body)
      `(let* ((,pkg (if ,sv ,package *package*))
	      (,v (si:*package-external ,pkg))
	      (,l (length ,v)))
	 ,@decl
	 (block ,break
	   (dotimes (,i ,l (let ((,var nil)) ,result-form))
	     (let* ((,break 't)
		    (,retval (dolist (,var (svref ,v ,i) (setq ,break 'nil))
			       ,@body)))
	       (when ,break
		 (return-from ,break ,retval)))))))))

(defmacro do-all-symbols ((var &optional result-form) &body body)
  (let ((vl (gensym))
	(v (gensym))
	(l (gensym))
	(i (gensym))
	(break (gensym))
	(retval (gensym)))
    (multiple-value-bind (decl body)
	(find-declaration body)
      `(let ((,vl '()))
	 ,@decl
	 (dolist (x (list-all-packages))
	   (push (si:*package-external x) ,vl)
	   (push (si:*package-internal x) ,vl))
	 (block ,break
	   (dolist (,v ,vl (let ((,var nil)) ,result-form))
	     (let ((,l (length ,v)))
	       (dotimes (,i ,l)
		 (let* ((,break 't)
			(,retval (dolist (,var (svref ,v ,i) (setq ,break 'nil))
				   ,@body)))
		   (when ,break
		     (return-from ,break ,retval)))))))))))

(defmacro with-package-iterator ((mname package-list &rest symbol-type)
				 &body body)
  (when (endp symbol-type)
    (error "symbol-typeが指定されていません"))
  (let ((internal nil)
	(external nil)
	(inherited nil)
	(packages (gensym))
	(vl (gensym))
	(x (gensym))
	(y (gensym))
	(v (gensym))
	(i (gensym))
	(l (gensym)))
    (dolist (x symbol-type)
      (case x
	(:internal (setq internal t))
	(:external (setq external t))
	(:inherited (setq inherited t))
	(t (error "未定義のシンボルタイプです: ~S" x))))
    `(let ((,vl nil)
	   (,packages ,package-list))
       (when (atom ,packages)
	 (setq ,packages (list ,packages)))
       (dolist (,x ,packages)
	 (setq ,x (lisp::coerce-to-package ,x))
	 ,(when internal
	    `(push (list (si:*package-internal ,x) ':internal ,x) ,vl))
	 ,(when external
	    `(push (list (si:*package-external ,x) ':external ,x) ,vl))
	 ,(when inherited
	    `(dolist (,y (package-use-list ,x))
	       (push (list (si:*package-external ,y) ':inherited ,x) ,vl))))
       (let ((,i 0)
	     (,l (if (null ,vl) 0 (length (caar ,vl))))
	     (,x nil))
	 (flet ((,mname ()
		 (loop
		   (when ,x
		     (return-from ,mname
		       (values t (pop ,x) (cadar ,vl) (caddar ,vl))))
		   (when (= ,i ,l)
		     (pop ,vl)
		     (unless ,vl
		       (return-from ,mname 'nil))
		     (setq ,i 0 ,l (length (caar ,vl))))
		   (unless (zerop ,l)
		     (setq ,x (svref (caar ,vl) ,i))
		     (incf ,i)))))
	   ,@body)))))
