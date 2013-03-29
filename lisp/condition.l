;;; -*- Mode: Lisp; Package: SYSTEM -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "condition")

(in-package "lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-condition define-condition)))

(in-package "system")

(defmacro define-condition-accessors (name exportp)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((def (gensym))
	  (object (gensym))
	  (slots (let ((def (get name 'structure-definition)))
		   (do ((i (1- (*structure-definition-nslots def)) (1- i))
			(slots))
		       ((< i 0) slots)
		     (push (*structure-definition-slot-description def i) slots)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((,def (get ',name 'structure-definition)))
	   (*structure-definition-add-constructor
	    ,def
	    #'(lambda (&key ,@(mapcar #'(lambda (x)
					  (intern (symbol-name x)))
				      slots))
		(let ((,object (*make-structure-data ,def)))
		  ,@(let ((inits '())
			  (i 0))
		      (dolist (x slots inits)
			(push `(*set-index-slot-value ,object ,i
						      ,(intern (symbol-name x)))
			      inits)
			(incf i)))
		  ,object)))
	   ,@(let ((forms '())
		   (i 0))
	       (dolist (x slots forms)
		 (let ((ac (intern (concatenate 'string (symbol-name name)
						"-" (symbol-name x)))))
		   (push `(defun ,ac (x)
			    (*index-slot-value x ,i))
			 forms)
		   (push `(defsetf ,ac (x) (y)
			    `(*set-index-slot-value ,x ,,i ,y))
			 forms)
		   (if exportp
		       (push `(export ',ac) forms)))
		 (incf i))))
	 (setf (get ',name 'ed:lisp-indent-handler) t)))))

(in-package "lisp")

(unless (si:*structure-definition-constructors
	 (get 'condition 'si:structure-definition))
  (si::define-condition-accessors condition t)
  (si::define-condition-accessors simple-condition t)
  (si::define-condition-accessors serious-condition t)
  (si::define-condition-accessors error t)
  (si::define-condition-accessors simple-error t)
  (si::define-condition-accessors plain-error t)
  (si::define-condition-accessors arithmetic-error t)
  (si::define-condition-accessors division-by-zero t)
  (si::define-condition-accessors floating-point-overflow t)
  (si::define-condition-accessors floating-point-underflow t)
  (si::define-condition-accessors domain-error t)
  (si::define-condition-accessors bignum-overflow t)
  (si::define-condition-accessors power-number-too-large t)
  (si::define-condition-accessors cell-error t)
  (si::define-condition-accessors unbound-variable t)
  (si::define-condition-accessors modify-constant t)
  (si::define-condition-accessors undefined-function t)
  (si::define-condition-accessors control-error t)
  (si::define-condition-accessors target-missing t)
  (si::define-condition-accessors file-error t)
  (si::define-condition-accessors file-not-found t)
  (si::define-condition-accessors path-not-found t)
  (si::define-condition-accessors access-denied t)
  (si::define-condition-accessors invalid-drive t)
  (si::define-condition-accessors current-directory t)
  (si::define-condition-accessors not-same-device t)
  (si::define-condition-accessors write-protected t)
  (si::define-condition-accessors bad-unit t)
  (si::define-condition-accessors device-not-ready t)
  (si::define-condition-accessors sharing-violation t)
  (si::define-condition-accessors lock-violation t)
  (si::define-condition-accessors wrong-disk t)
  (si::define-condition-accessors file-exists t)
  (si::define-condition-accessors not-empty t)
  (si::define-condition-accessors archiver-error t)
  (si::define-condition-accessors network-error t)
  (si::define-condition-accessors file-lost-error t)
  (si::define-condition-accessors package-error t)
  (si::define-condition-accessors simple-package-error t)
  (si::define-condition-accessors program-error t)
  (si::define-condition-accessors simple-program-error t)
  (si::define-condition-accessors format-error t)
  (si::define-condition-accessors no-target t)
  (si::define-condition-accessors bad-macro-form t)
  (si::define-condition-accessors invalid-function t)
  (si::define-condition-accessors invalid-variable-list t)
  (si::define-condition-accessors invalid-lambda-list t)
  (si::define-condition-accessors invalid-keyword-list t)
  (si::define-condition-accessors type-error t)
  (si::define-condition-accessors range-error t)
  (si::define-condition-accessors stream-error t)
  (si::define-condition-accessors end-of-file t)
  (si::define-condition-accessors reader-error t)
  (si::define-condition-accessors too-few-arguments t)
  (si::define-condition-accessors too-many-arguments t)
  (si::define-condition-accessors socket-error t)
  (si::define-condition-accessors bad-type-specifier t)
  (si::define-condition-accessors read-only-buffer t)
  (si::define-condition-accessors dde-error t)
  (si::define-condition-accessors dde-timeout t)
  (si::define-condition-accessors dde-busy t)
  (si::define-condition-accessors dde-low-memory t)
  (si::define-condition-accessors dde-no-conv t)
  (si::define-condition-accessors dde-not-processed t)
  (si::define-condition-accessors dde-server-died t)
  (si::define-condition-accessors dde-terminated-transaction t)
  (si::define-condition-accessors storage-condition t)
  (si::define-condition-accessors stack-overflow t)
  (si::define-condition-accessors win32-exception t)
  (si::define-condition-accessors invalid-byte-code t)
  (si::define-condition-accessors quit t)
  (si::define-condition-accessors silent-quit t)
  (si::define-condition-accessors warning t)
  (si::define-condition-accessors simple-warning t))

(in-package "system")

(defun *report-reader-error (c s)
  (if (reader-error-stream c)
      (format s "~A: ~A: ~A~{: ~A~}"
	      (reader-error-stream c)
	      (reader-error-linenum c)
	      (reader-error-datum c)
	      (reader-error-arguments c))
    (format s "Line ~A: ~A~{: ~A~}"
	    (reader-error-linenum c)
	    (reader-error-datum c)
	    (reader-error-arguments c))))

(defun *report-no-target-for (c s)
  (format s "`~S'のキャッチャを参照できません: ~S"
	  (no-target-operation c) (no-target-target c)))

(defun *report-file-lost-error (c s)
  (format s "~A~%非常に言いにくいことなのですが~%ファイルがなくなっちゃいました
~A~%に保存してあるので自力でなんとかしてください"
	  (file-lost-error-pathname c)
	  (file-lost-error-lost-pathname c)))

(defun *report-file-error (c s)
  (format s "~@[~A~%~]~A" (file-error-pathname c) (file-error-datum c)))

(defun *report-target-missing (c s)
  (format s "`~S'のキャッチャは利用可能ではありません: ~S"
	  (no-target-operation c) (no-target-target c)))

(defun *report-simple-condition (c s)
  (when (stringp (simple-condition-format-string c))
    (apply #'format s (simple-condition-format-string c)
	   (simple-condition-format-arguments c))))

(defun *report-simple-package-error (c s)
  (format s "~A: ~S"
	  (simple-package-error-datum1 c)
	  (simple-package-error-datum2 c)))

(defun *report-socket-error (c s)
  (format s "~A: ~A"
	  (socket-error-operation c)
	  (socket-error-datum c)))

(defun *report-win32-exception (c s)
  (format s "Win32例外が発生しました: ~A (~X) at 0x~X"
	  (win32-exception-description c)
	  (win32-exception-code c)
	  (win32-exception-address c)))

(defun define-condition-helper (name parent-type body)
  (let ((slot-spec (car body))
	(options (cdr body)))
    (when (or (get name 'type-predicate)
	      (get name 'type-specifier-list))
      (error "~Aは型指定子です" name))
    (let ((def (get name 'structure-definition)))
      (when (and def
		 (*structure-definition-p def)
		 (*structure-definition-read-only-p def))
	(error "~Aの再定義はできません" name)))
    (unless (subtypep parent-type 'condition)
      (error "~Aはコンディションではありません" parent-type))
    (let ((parent (get parent-type 'structure-definition))
	  (doc nil)
	  (report nil)
	  important)
      (setq important(*structure-definition-important-p parent))
      (dolist (x (cdr body))
	(case (car x)
	  (:documentation
	   (setq doc (cadr x)))
	  (:report
	   (setq report (cadr x)))
	  (:important
	   (setq important (cadr x)))
	  (t
	   (error "不正なdefine-conditionオプションです: ~A" x))))
      (unless report
	(setq report (*structure-definition-report parent)))
      (let ((slot-desc
	     (do ((i (1- (*structure-definition-nslots parent)) (1- i))
		  (slots '()))
		 ((< i 0) slots)
	       (push (multiple-value-list
		      (*structure-definition-slot-description parent i))
		     slots))))
	(mapc #'(lambda (x)
		  (let (name
			(init nil))
		    (cond ((symbolp x)
			   (setq name x))
			  ((consp x)
			   (setq name (car x))
			   (unless (symbolp name)
			     (error "不正なスロット名です: ~S" name))
			   (setq init (cadr x)))
			  (t
			   (error "不正なスロットです: ~S" x)))
		    (when (assoc name slot-desc :test #'string=)
		      (error "スロット~Aが重複しています" name))
		    (setq slot-desc
			  (nconc slot-desc
				 (list (list (intern (symbol-name name)
						     "keyword")
					     init t nil))))))
	      (car body))
	(let ((def (gensym)))
	  `(eval-when (:compile-toplevel :load-toplevel :execute)
	     (let ((,def (*make-structure-definition
			  ',name nil (get ',parent-type 'structure-definition)
			  nil nil nil ',slot-desc
			  ,(if (or (consp report)
				   (symbolp report))
			       `#',report
			     report)
			  ,important)))
	       (setf (get ',name 'structure-definition) ,def)
	       (setf (get ',name 'lisp::type-documentation) ,doc))
	     (define-condition-accessors ,name nil)))))))

(defmacro define-condition (name (parent-type) &rest body)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-condition-helper name parent-type body)))

(defun make-condition (type &rest args)
  (let ((def (get type 'structure-definition)))
    (unless (and def (subtypep type 'condition))
      (error "コンディションではありません: ~S" type))
    (let ((x (find-if #'atom (*structure-definition-constructors def))))
      (unless x
	(error "コンストラクタがありません: ~S" type))
      (apply x args))))

(define-condition check-type-error (type-error)
  (string place)
  (:report (lambda (c s)
	     (format s "`~A'の値`~S'は~:[~S~;~:*~A~]ではありません"
		     (check-type-error-place c)
		     (check-type-error-datum c)
		     (check-type-error-string c)
		     (check-type-error-expected-type c)))))

(defmacro check-type (place typespec &optional string)
  (let ((pred (assoc typespec typespec-alist :test #'eq)))
    (unless pred
      (setq pred (assoc typespec '((integer . integerp)
				   (float . floatp)
				   (rational . rationalp)
				   (short-float . short-float-p)
				   (single-float . single-float-p)
				   (double-float . double-float-p)
				   (long-float . long-float-p)
				   (complex . complexp)
				   (simple-array . si:*simple-array-p)
				   (simple-string . simple-string-p)
				   (simple-vector . simple-vector-p)
				   (array . arrayp)
				   (string . stringp)
				   (vector . vectorp))
			:test #'eq)))
    `(let ((#1=#:PLACE ,place))
       (or ,(if pred
		`(,(cdr pred) #1#)
	      `(typep #1# ',typespec))
	   (error 'check-type-error
	     :datum #1#
	     :expected-type ',typespec
	     :place ',place
	     :string ,string))
       nil)))
