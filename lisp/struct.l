;;; -*- Mode: Lisp; Package: SYSTEM -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "struct")

(in-package "lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'defstruct))

(in-package "system")

(defconstant *option-not-specified* (gensym))
(defconstant *named-slot* (gensym))

(defun defstruct-parse-slot-option (desc)
  (let (name
	(default-init *option-not-specified*)
	(type *option-not-specified*)
	(read-only *option-not-specified*))
    (cond ((symbolp desc)
	   (setq name desc))
	  ((consp desc)
	   (setq name (car desc))
	   (unless (symbolp name)
	     (error "不正なスロット名です: ~S" name))
	   (setq desc (cdr desc))
	   (when (not (endp desc))
	     (setq default-init (car desc))
	     (setq desc (cdr desc)))
	   (do ((d desc (cddr d)))
	       ((endp d))
	     (when (endp (cdr d))
	       (error "不正なスロット記述子です: ~S" desc))
	     (case (car d)
	       (:type
		(setq type (cadr d)))
	       (:read-only
		(setq read-only (cadr d)))
	       (t
		(error "不正なスロットオプションです: ~S" (car d))))))
	  (t
	   (error "不正なスロット記述子です: ~S" desc)))
    (values name default-init type read-only)))

(defun defstruct-parse-options (name options)
  (let ((conc-name (concatenate 'string (string name) "-"))
	(default-constructor t)
	(constructors '())
	(copier *option-not-specified*)
	(predicate *option-not-specified*)
	(include nil)
	(slots nil)
	(print-function *option-not-specified*)
	(type nil)
	(named nil)
	(initial-offset nil))
    (dolist (opt options)
      (let (o v)
	(if (consp opt)
	    (setq o (car opt) v (cdr opt))
	  (setq o opt v nil))
	(case o
	  (:conc-name
	   (cond ((endp v))
		 ((null (car v)) (setq conc-name ""))
		 (t (setq conc-name (string (car v))))))
	  (:constructor
	   (cond ((endp v))           ; :constructor 指定なし
		 ((null (car v)))     ; (:constructor)
		 ((endp (cdr v))      ; (:constructor make-foo)
		  (push (car v) constructors)
		  (setq default-constructor nil))
		 (t                   ; (:constructor make-foo (args...))
		  (push v constructors)
		  (setq default-constructor nil))))
	  (:copier
	   (cond ((endp v))
		 ((null (car v)) (setq copier nil))
		 ((symbolp (car v)) (setq copier (car v)))
		 (t (error "不正な:copierオプション引数です: ~S" (car v)))))
	  (:predicate
	   (cond ((endp v))
		 ((null (car v)) (setq predicate nil))
		 ((symbolp (car v)) (setq predicate (car v)))
		 (t (error "不正な:predicateオプション引数です: ~S" (car v)))))
	  (:include
	   (setq include v))
	  (:print-function
	   (setq print-function (car v)))
	  (:type
	   (unless (consp v)
	     (error ":typeオプションに引数がありません"))
	   (setq type (canonicalize-type (car v))))
	  (:named
	   (setq named t))
	  (:initial-offset
	   (setq initial-offset (car v)))
	  (t
	   (error "不正なdefstructオプションです: ~S" opt)))))
    (when include
      (let ((def (get (car include) 'structure-definition)))
	(unless def
	  (error "~Sは構造体ではありません" (car include)))
	(setq slots nil)
	(do ((i (1- (*structure-definition-nslots def)) (1- i)))
	    ((< i 0))
	  (push (multiple-value-list (*structure-definition-slot-description
				      def i))
		slots))
	(dolist (desc (cdr include))
	  (multiple-value-bind (name default-init type read-only)
	      (defstruct-parse-slot-option desc)
	    (let ((odesc (assoc name slots :test #'string=)))
	      (unless odesc
		(error "不正なスロット名です: ~A" name))
	      (setf (cadr odesc) default-init)
	      (cond ((eq type *option-not-specified*))
		    ((subtypep type (caddr odesc))
		     (setf (caddr odesc) type))
		    (t
		     (error "~Sは~Sのsubtypeではありません" type (caddr odesc))))
	      (cond ((eq read-only *option-not-specified*))
		    (read-only
		     (setf (cadddr odesc) t))
		    ((cadddr odesc)
		     (error "~Aはread-onlyです" name))))))
	(cond ((null (*structure-definition-type def))
	       (when type
		 (error ":typeオプションは指定できません")))
	      ((equal (*structure-definition-type def) type))
	      (t (error "不正な:typeオプションです: ~S" type)))
	(when (and (eq print-function *option-not-specified*)
		   (not type))
	  (setq print-function (*structure-definition-print-function def)))
	(setq include `(get ',(car include) 'structure-definition))))
    (cond (type
	   (unless (subtypep type 'sequence)
	     (error "不正な:typeオプションです: ~S" type))
	   (unless (eq print-function *option-not-specified*)
	     (error ":typeと:print-functionは同時に指定できません"))
	   (unless (or named
		       (null predicate)
		       (eq predicate *option-not-specified*))
	     (error ":namedでない構造体に:predicateは指定できません"))
	   (cond ((null initial-offset)
		  (setq initial-offset 0))
		 ((and (*fixnump initial-offset)
		       (>= initial-offset 0)))
		 (t
		  (error "不正な:initial-offset引数です: ~S" initial-offset))))
	  (t
	   (when initial-offset
	     (error ":initial-offsetは:typeがなければ指定できません"))))
    (when (eq print-function *option-not-specified*)
      (setq print-function nil))
    (when default-constructor
      (push (intern (concatenate 'string "make-" (string name))) constructors))
    (when (eq copier *option-not-specified*)
      (setq copier (intern (concatenate 'string "copy-" (string name)))))
    (when (eq predicate *option-not-specified*)
      (setq predicate (intern (concatenate 'string (string name) "-p"))))
    (values conc-name constructors copier predicate
	    include slots print-function type named initial-offset)))

(defun defstruct-parse-slot-descriptions (name slot-desc slots named initial-offset)
  (let (offset)
    (when initial-offset
      (when slots
	(setq initial-offset (+ initial-offset 1 (nth 4 (car (last slots))))))
      (setq offset initial-offset)
      (when named
	(setq slots (nconc slots (list (list *named-slot* name t t offset))))
	(incf offset)))
    (dolist (desc slot-desc (values slots initial-offset))
      (let ((d (multiple-value-list (defstruct-parse-slot-option desc))))
	(setf (car d) (intern (symbol-name (car d)) "keyword"))
	(when (assoc (car d) slots :test #'string=)
	  (error "スロット~Aが重複しています" (car d)))
	(when offset
	  (nconc d (list offset))
	  (incf offset))
	(setq slots (nconc slots (list d)))))))

(defun defstruct-parse-lambda-list (lambda-list slots)
  (let ((lambda-keys '(&optional &rest &key &aux))
	(vars '())
	(r '())
	tem
	v)
    (while (setq v (pop lambda-list))
      (if (member v lambda-keys :test #'eq)
	  (return))
      (push v r)
      (push v vars))
    (pop lambda-keys)
    (when (eq v '&optional)
      (push v r)
      (while (setq v (pop lambda-list))
	(cond ((member v lambda-keys :test #'eq)
	       (return))
	      ((symbolp v)
	       (setq tem (assoc v slots :test #'string=))
	       (cond ((or (null tem)
			  (eq (cadr tem) *option-not-specified*))
		      (push v r)
		      (push v vars))
		     (t
		      (push (list v (cadr tem)) r)
		      (push v vars))))
	      ((consp v)
	       (setq tem (assoc (car v) slots :test #'string=))
	       (cond ((endp (cdr v))
		      (cond ((or (null tem)
				 (eq (cadr tem) *option-not-specified*))
			     (push (car v) r)
			     (push (car v) vars))
			    (t
			     (push (list (car v) (cadr tem)) r)
			     (push (car v) vars))))
		     (t
		      (push v r)
		      (push (car v) vars)
		      (unless (endp (cddr v))
			(push (caddr v) vars)))))
	      (t
	       (error 'type-error :datum v :expected-type '(or symbol cons))))))
    (pop lambda-keys)
    (when (eq v '&rest)
      (push v r)
      (setq v (pop lambda-list))
      (push v r)
      (push v vars))
    (pop lambda-keys)
    (when (eq v '&key)
      (push v r)
      (while (setq v (pop lambda-list))
	(cond ((member v lambda-keys :test #'eq)
	       (return))
	      ((symbolp v)
	       (setq tem (assoc v slots :test #'string=))
	       (cond ((or (null tem)
			  (eq (cadr tem) *option-not-specified*))
		      (push v r)
		      (push v vars))
		     (t
		      (push (list v (cadr tem)) r)
		      (push v vars))))
	      ((consp v)
	       (cond ((symbolp (car v))
		      (setq tem (assoc (car v) slots :test #'string=))
		      (cond ((endp (cdr v))
			     (cond ((or (null tem)
					(eq (cadr tem) *option-not-specified*))
				    (push (car v) r)
				    (push (car v) vars))
				   (t
				    (push (list (car v) (cadr tem)) r)
				    (push (car v) vars))))
			    (t
			     (push v r)
			     (push (car v) vars))))
		     ((consp (car v))
		      (setq tem (assoc (cadar v) slots :test #'string=))
		      (cond ((endp (cdr v))
			     (cond ((or (null tem)
					(eq (cadr tem) *option-not-specified*))
				    (push (car v) r)
				    (push (cadar v) vars))
				   (t
				    (push (list (car v) (cadr tem)) r)
				    (push (cadar v) vars))))
			    (t
			     (push v r)
			     (push (cadar v) vars))))
		     (t
		      (error 'type-error
			:expected-type '(or symbol cons)
			:datum (car v))))
	       (unless (endp (cddr v))
		 (push (caddr v) vars)))
	      (t
	       (error 'type-error
		 :expected-type '(or symbol cons)
		 :datum v)))))
    (when (eq v '&aux)
      (push v r)
      (while (setq v (pop lambda-list))
	(push v r)
	(cond ((symbolp v)
	       (push v vars))
	      ((consp v)
	       (push (car v) vars))
	      (t
	       (error 'type-error
		 :expected-type '(or symbol cons)
		 :datum v)))))
    (values (nreverse r) vars)))

(defun create-constructors (def constructors slots)
  (let ((forms '())
	(object (gensym)))
    (dolist (c constructors)
      (cond ((symbolp c)
	     (push `(defun ,c
		      (&key ,@(mapcar #'(lambda (desc)
					  (let ((name (intern (symbol-name (car desc))))
						(init (cadr desc)))
					    (if (eq init *option-not-specified*)
						name
					      (list name init))))
				      slots))
		      (let ((,object (*make-structure-data ,def)))
			,@(let ((inits '())
				(i 0))
			    (dolist (desc slots inits)
			      (push `(*set-index-slot-value ,object ,i
							    ,(intern (symbol-name (car desc))))
				    inits)
			      (incf i)))
			,object))
		   forms))
	    (t
	     (multiple-value-bind (lambda-list init-vars)
		 (defstruct-parse-lambda-list (cadr c) slots)
	       (push `(defun ,(car c) ,lambda-list
			(let ((,object (*make-structure-data ,def)))
			  ,@(let ((inits '()))
			      (dolist (desc slots inits)
				(let ((v (member (car desc) init-vars :test #'string=)))
				  (cond (v
					 (push `(*set-slot-value ,object ,(car desc) ,(car v))
					       inits))
					((not (eq (cadr desc) *option-not-specified*))
					 (push `(*set-slot-value ,object ,(car desc) ,(cadr desc))
					       inits))))))
			  ,object))
		     forms)))))
    forms))

(defun create-accessors (conc-name slots)
  (let ((forms '())
	(i 0))
    (dolist (desc slots forms)
      (let ((name (intern (concatenate 'string conc-name (symbol-name (car desc))))))
	(push `(defun ,name (x)
		 (*index-slot-value x ,i))
	      forms)
	(when (or (eq (cadddr desc) *option-not-specified*)
		  (null (cadddr desc)))
	  (push `(defsetf ,name (x) (y)
		   `(*set-index-slot-value ,x ,,i ,y))
		forms)))
      (incf i))))

(defun create-print-function (print-function)
  (when print-function
    (labels ((rec (pf)
	       (cond ((si:*closurep pf)
		      ;; 親から継承した場合
		      pf)
		     ((symbolp pf)
		      ;; :print-function foo の場合
		      `',pf)
		     ((eq (safe-car pf) 'lambda)
		      ;; :print-function (lambda (o s p) ...) の場合
		      `#',pf)
		     ((eq (safe-car pf) 'function)
		      ;; :print-function #'foo または
		      ;; :print-function #'(lambda (o s p) ...) の場合
		      ;; 警告を出して #' を外す
		      (warn ":print-functionに関数が指定されました。~%~S の代わりに ~S を指定してください"
			    print-function (cadr pf))
		      (rec (cadr pf)))
		     (t
		      (error "不正な:print-functionです: ~S" print-function)))))
      (rec print-function))))

(defun define-structure (name doc conc-name constructors copier predicate
			 include slots print-function)
  (let ((def (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,def (*make-structure-definition ',name nil ,include
					       ',constructors
					       ,(create-print-function print-function)
					       nil ',slots)))
	 (setf (get ',name 'structure-definition) ,def)
	 (setf (get ',name 'lisp::structure-documentation) ,doc)
	 ,@(create-constructors def constructors slots)
	 ,(when copier
	    `(setf (symbol-function ',copier) #'*copy-structure-data))
	 ,(when predicate
	    `(defun ,predicate (x)
	       (typep x ',name)))
	 ,@(create-accessors conc-name slots)
	 ,def))))

(defun create-typed-constructors (constructors slots type length)
  (let ((forms '())
	(object (gensym)))
    (dolist (c constructors)
      (cond ((symbolp c)
	     (push `(defun ,c
		      (&key ,@(mapcan
			       #'(lambda (desc)
				   (unless (eq (car desc) *named-slot*)
				     (let ((name (intern (symbol-name (car desc))))
					   (init (cadr desc)))
				       (if (eq init *option-not-specified*)
					   (list name)
					 (list (list name init))))))
			       slots))
		      (let ((,object (make-sequence ',type ,length)))
			,@(mapcar
			   #'(lambda (desc)
			       `(setf (elt ,object ,(nth 4 desc))
				      ,(if (eq (car desc) *named-slot*)
					   `',(cadr desc)
					 (intern (symbol-name (car desc))))))
			   slots)
			,object))
		   forms))
	    (t
	     (multiple-value-bind (lambda-list init-vars)
		 (defstruct-parse-lambda-list (cadr c) slots)
	       (push `(defun ,(car c) ,lambda-list
			(let ((,object (make-sequence ',type ,length)))
			  ,@(mapcan
			     #'(lambda (desc)
				 (if (eq (car desc) *named-slot*)
				     `((setf (elt ,object ,(nth 4 desc)) ',(cadr desc)))
				   (let ((v (member (car desc) init-vars :test #'string=)))
				     (when v
				       `((setf (elt ,object ,(nth 4 desc)) ,(car v)))))))
			     slots)
			  ,object))
		     forms)))))
    forms))

(defun create-typed-accessors (conc-name slots)
  (let ((forms '()))
    (dolist (desc slots forms)
      (unless (eq (car desc) *named-slot*)
	(let ((name (intern (concatenate 'string conc-name (symbol-name (car desc))))))
	  (push `(defun ,name (x)
		   (elt x ,(nth 4 desc)))
		forms)
	  (when (or (eq (cadddr desc) *option-not-specified*)
		    (null (cadddr desc)))
	    (push `(defsetf ,name (x) (y)
		     `(*set-elt ,x ,,(nth 4 desc) ,y))
		  forms)))))))

(defun define-typed-structure (name doc conc-name constructors copier predicate
			       include slots type named initial-offset)
  (let ((def (gensym))
	(length (if slots
		    (1+ (nth 4 (car (last slots))))
		  0)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,def (*make-structure-definition ',name ',type ,include
					       ',constructors nil
					       ,named ',slots)))
	 (setf (get ',name 'structure-definition) ,def)
	 (setf (get ',name 'lisp::structure-documentation) ,doc)
	 ,@(create-typed-constructors constructors slots type length)
	 ,(when copier
	    `(setf (symbol-function ',copier) #'copy-seq))
	 ,(when predicate
	    `(defun ,predicate (x)
	       (and (typep x ',type)
		    (>= (length x) ,length)
		    (eq (elt x ,initial-offset) ',name))))
	 ,@(create-typed-accessors conc-name slots)
	 ,def))))

(defun defstruct-helper (name-and-options rest)
  (let ((doc nil)
	(slot-desc rest)
	(name name-and-options)
	(options '()))
    (when (stringp (car slot-desc))
      (setq doc (pop slot-desc)))
    (when (consp name-and-options)
      (setq name (car name-and-options))
      (setq options (cdr name-and-options)))
    (when (or (get name 'type-predicate)
	      (get name 'type-specifier-list))
      (error "~Aは型指定子です" name))
    (let ((def (get name 'structure-definition)))
      (when (and def
		 (*structure-definition-p def)
		 (*structure-definition-read-only-p def))
	(error "~Aの再定義はできません" name)))
    (multiple-value-bind (conc-name constructors copier
			  predicate include slots print-function type
			  named initial-offset)
	(defstruct-parse-options name options)
      (multiple-value-setq (slots initial-offset)
	(defstruct-parse-slot-descriptions name slot-desc slots
					   named initial-offset))
      (if type
	  (define-typed-structure name doc conc-name constructors copier
				  predicate include slots
				  type named initial-offset)
	(define-structure name doc conc-name constructors copier predicate
			  include slots print-function)))))

(defmacro defstruct (name-and-options &rest rest)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defstruct-helper name-and-options rest)))

(defun *structure-reader (name args)
  (let ((def (get name 'structure-definition)))
    (unless def
      (error "構造体ではありません: ~S" name))
    (dolist (x (*structure-definition-constructors def))
      (when (atom x)
	(return-from *structure-reader (apply x args))))
    (error "コンストラクタがありません: ~S" name)))
