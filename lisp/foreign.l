;;; -*- Mode: Lisp; Package: FOREIGN -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "foreign")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "foreign")
    (defpackage "foreign" (:nicknames "c"))))

(in-package "foreign")

(export '(expand-c-type define-c-type *define-c-type))
(export '(define-dll-entry *define-dll-entry
	  expand-c-vaargs c-vaargs
	  defun-c-callable *defun-c-callable
	  define-c-struct *define-c-struct
	  c-struct-size-of c-struct-offset-of
	  define *define *define-c-macro
	  *c-typedef-auto-export* *c-structure-packing-align*
	  c-size-of last-win32-error
	  ))

(defsetf si:unpack-int8 si:pack-int8)
(defsetf si:unpack-uint8 si:pack-uint8)
(defsetf si:unpack-int16 si:pack-int16)
(defsetf si:unpack-uint16 si:pack-uint16)
(defsetf si:unpack-int32 si:pack-int32)
(defsetf si:unpack-uint32 si:pack-uint32)
(defsetf si:unpack-int64 si:pack-int64)
(defsetf si:unpack-uint64 si:pack-uint64)
(defsetf si:unpack-float si:pack-float)
(defsetf si:unpack-double si:pack-double)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get ':double 'c-type-specifier)

    (defstruct (c-type-definition
		(:conc-name "ctypedef-"))
      type     ; 型をあらわすシンボル
      size     ; 型のサイズ
      slots    ; 構造体ならc-structure-slotのリスト
      accessor ; アクセサ
    )

    (defstruct (c-structure-slot
		(:conc-name cslot-))
      name     ; スロット名
      type     ; 型(c-type-definition)
      offset   ; 構造体の先頭からのオフセット
      size     ; バイト数
    )

    (dolist (x '((:void 0 nil)
		 (:int8 1 si:unpack-int8)
		 (:uint8 1 si:unpack-uint8)
		 (:int16 2 si:unpack-int16)
		 (:uint16 2 si:unpack-uint16)
		 (:int32 4 si:unpack-int32)
		 (:uint32 4 si:unpack-uint32)
		 (:int64 8 si:unpack-int64)
		 (:uint64 8 si:unpack-uint64)
		 (:float 4 si:unpack-float)
		 (:double 8 si:unpack-double)))
      (setf (get (car x) 'c-type-specifier)
	    (make-c-type-definition :type (car x) :size (cadr x)
				    :accessor (caddr x)))))

  (defvar *c-structure-packing-align* 8)

  (defvar *c-typedef-auto-export* nil)

  (defun c-typespec-p (type)
    (cond ((symbolp type)
	   (let ((x (get type 'c-type-specifier)))
	     (when (c-type-definition-p x)
	       x)))
	  ((listp type)
	   (and (null (cddr type))
		(c-typespec-p (car type))
		(eq (cadr type) '*)
		(get ':uint32 'c-type-specifier)))))

  (defun expand-c-type (type)
    (let ((x (c-typespec-p type)))
      (unless x
	(error "~Aは型指定子ではありません" type))
      x))

  (defun **define-c-type (type decl export)
    (let ((x (get decl 'c-type-specifier)))
      (when x
	(unless (eq x (expand-c-type type))
	  (error "~Aはすでに宣言されています" decl))
	(return-from **define-c-type nil)))
    (setf (get decl 'c-type-specifier)
	  (expand-c-type type))
    (when export
      (export decl)))
)

(defmacro define-c-type (type decl)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (**define-c-type ',type ',decl *c-typedef-auto-export*)))

(defmacro *define-c-type (type decl)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (**define-c-type ',type ',decl t)))

(*define-c-type :void void)
(*define-c-type :int8 char)
(*define-c-type :int16 short)
(*define-c-type :int32 int)
(*define-c-type :int32 long)
(*define-c-type :int64 int64)
(*define-c-type :uint8 u_char)
(*define-c-type :uint16 u_short)
(*define-c-type :uint32 u_int)
(*define-c-type :uint32 u_long)
(*define-c-type :uint64 u_int64)
(*define-c-type :float float)
(*define-c-type :double double)

(*define-c-type u_short wchar_t)
(*define-c-type u_int size_t)
(*define-c-type long time_t)
(*define-c-type int ptrdiff_t)
(*define-c-type long clock_t)
(*define-c-type long off_t)

(*define-c-type long void*)
(*define-c-type void* char*)
(*define-c-type char* string)

(defun c-argument-type (type)
  (setq type (expand-c-type type))
  (when (zerop (ctypedef-size type))
    (error ":void型引数はサポートしていません"))
  (when (ctypedef-slots type)
    (error "構造体の値渡しはサポートしていません: ~A" (ctypedef-type type)))
  (ctypedef-type type))

(defun c-vaarg-p (types)
  (let ((rests (member '&rest types)))
    (when (and rests (cdr rests))
      (error "&restが最後の句ではありません"))
    (not (null rests))))

(defun c-argument-types (types)
  (mapcar #'c-argument-type (if (c-vaarg-p types) (butlast types) types)))

(defun expand-c-vaargs (vaargs)
  (mapcar #'(lambda (vaarg)
	      (unless (and (listp vaarg)
			   (= (length vaarg) 2))
		(error "不正な可変長引数です: ~S" vaargs))
	      (handler-case
		  (list (c-argument-type (car vaarg))
			(cadr vaarg))
		(error (c)
		  (error "不正な可変長引数です: ~S" vaargs))))
	  vaargs))

(defmacro c-vaargs (&rest vaargs)
  `(list
    ,@(mapcar #'(lambda (vaarg)
		  `(list ,@vaarg))
	      (expand-c-vaargs vaargs))))

(defun c-return-type (type)
  (setq type (expand-c-type type))
  (when (ctypedef-slots type)
    (error "構造体の戻り値はサポートしていません: ~A" (ctypedef-type type)))
  (ctypedef-type type))

(setf (get 'define-dll-entry 'ed:lisp-indent-hook) 'defun)
(setf (get '*define-dll-entry 'ed:lisp-indent-hook) 'defun)

(defmacro define-dll-entry (return-type name-and-options (&rest args) dll-name &optional export-name)
  (multiple-value-bind (name options)
      (parse-func-name-and-options name-and-options)
    (let ((convention (or (getf options :convention)
			  :stdcall)))
      (when (and (c-vaarg-p args)
		 (eq convention :stdcall))
	(error "stdcall呼び出し規約の場合、可変長引数は利用できません")))
    `(defun ,name (&rest args)
       (setf (symbol-function ',name)
	     (si:make-c-function (si:load-dll-module ,dll-name)
				 ',(or export-name (symbol-name name))
				 ',(c-argument-types args)
				 ',(c-return-type return-type)
				 :vaarg ,(c-vaarg-p args)
				 ,@options))
       (apply #',name args))))

(defmacro *define-dll-entry (return-type name (&rest args) dll-name &optional export-name)
  `(progn
     (define-dll-entry ,return-type ,name ,args ,dll-name ,export-name)
     (export ',name)))

(setf (get 'defun-c-callable 'ed:lisp-indent-hook) 'defun)
(setf (get '*defun-c-callable 'ed:lisp-indent-hook) 'defun)

(defmacro defun-c-callable (return-type name-and-options (&rest args) &body body)
  (multiple-value-bind (name options)
      (parse-func-name-and-options name-and-options)
    `(setf (symbol-function ',name)
	   (si:make-c-callable #'(lambda ,(mapcar #'cadr args)
				   (block ,name ,@body))
			       ',(mapcar #'(lambda (arg)
					     (c-argument-type (car arg)))
					 args)
			       ',(c-return-type return-type)
			       ,@options))))

(defmacro *defun-c-callable (return-type name (&rest args) &body body)
  `(progn
     (defun-c-callable ,return-type ,name ,args ,@body)
     (export ',name)))

(defun parse-func-name-and-options (name-and-options)
  (if (atom name-and-options)
      (values name-and-options nil)
    (values (car name-and-options) (cdr name-and-options))))

(defun make-c-struct-accessors (slotdef conc-name offset export)
  (mapcan #'(lambda (slot)
	      (let ((ac (intern (concatenate 'string conc-name
					     (string (cslot-name slot)))))
		    (type (cslot-type slot)))
		(if (= (cslot-size slot) (ctypedef-size type))
		    (let ((off (+ offset (cslot-offset slot))))
		      `((defun ,ac (#1=#:chunk)
			  (,(ctypedef-accessor type) #1# ,off))
			(defsetf ,ac (#1#) (#2=#:value)
			  `(setf (,',(ctypedef-accessor type) ,#1# ,,off) ,#2#))
			(if ,export
			    (export ',ac))))
		  (let ((off (cond ((zerop offset)
				    (if (= (ctypedef-size type) 1)
					'#3=#:index
				      `(* #3# ,(ctypedef-size type))))
				   (t
				    (if (= (ctypedef-size type) 1)
					`(+ ,offset #3#)
				      `(+ ,offset (* #3# ,(ctypedef-size type))))))))
		    `((defun ,ac (#1# #3#)
			(,(ctypedef-accessor type) #1# ,off))
		      (defsetf ,ac (#1# #3#) (#2#)
			`(setf (,',(ctypedef-accessor type) ,#1# ,,off) ,#2#))
		      (if ,export
			  (export ',ac)))))))
	  slotdef))

(defun define-c-struct-expand (name slots export)
  (check-type name symbol)
  (when (endp slots)
    (error "構造体のメンバがありません: ~A" name))
  (let ((offset 0)
	(struct-align nil))
    (let ((slotdef
	   (mapcar #'(lambda (slot)
		       (let (type pointerp dims name size)
			 (when (endp slot)
			   (error "不正なスロットの形式です"))
			 (setq type (expand-c-type (car slot)))
			 (pop slot)
			 (when (endp slot)
			   (error "不正なスロットの形式です"))
			 (when (eq (car slot) '*)
			   (setq type (expand-c-type ':int32))
			   (pop slot)
			   (when (endp slot)
			     (error "不正なスロットの形式です")))
			 (setq size (ctypedef-size type))
			 (unless struct-align
			   (setq struct-align (min size *c-structure-packing-align*)))
			 (when (zerop size)
			   (error ":void型は構造体のメンバには使用できません"))
			 (setq name (car slot))
			 (unless (symbolp name)
			   (error "スロット名はシンボルです: ~A" name))
			 (pop slot)
			 (unless (endp slot)
			   (setq dims (car slot))
			   (when (symbolp dims)
			     (setq dims (symbol-value dims)))
			   (unless (integerp dims)
			     (error "配列の長さは整数です: ~A" dims))
			   (when (<= dims 0)
			     (error "不正な配列の長さです: ~A" dims))
			   (unless (endp (cdr slot))
			     (error "スロットの引数が多すぎます")))
			 (let ((align (min size *c-structure-packing-align*)))
			   (setq offset (* (ceiling offset align) align))
			   (setq size (if dims (* dims size) size))
			   (prog1
			       (make-c-structure-slot :name name :type type
						      :offset offset :size size)
			     (incf offset size)))))
		   slots)))
      (setq offset (* (ceiling offset struct-align) struct-align))
      (let* ((sname (string name))
	     (constructor (intern (concatenate 'string "make-" sname)))
	     (predicate (intern (concatenate 'string sname "-p"))))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (when ,export
	     (export ',name)
	     (export ',constructor)
	     (export ',predicate))
	   ;; declare type
	   (setf (get ',name 'c-type-specifier)
		 (make-c-type-definition
		  :type ',name
		  :size ,offset
		  :slots (list ,@(mapcar
				  #'(lambda (slot)
				      `(make-c-structure-slot
					:name ',(cslot-name slot)
					:type (get ',(ctypedef-type (cslot-type slot))
						   'c-type-specifier)
					:offset ,(cslot-offset slot)
					:size ,(cslot-size slot)))
				  slotdef))
		  :accessor ',constructor))

	   ;; constructor and accessor
	   (defun ,constructor (&optional #2=#:chunk #3=#:offset)
	     (si:make-chunk ',name ,offset #2# #3#))
	   (defsetf ,constructor (#2# &optional #3#) (#4=#:value)
	     `(progn
		(si:copy-chunk ,#4# ,#2# nil nil ,#3#)
		,#4#))
	   ;; predicate
	   (defun ,predicate (#1=#:chunk)
	     (eq (si:chunk-type #1#) ',name))
	   ;; accessors
	   ,@(make-c-struct-accessors slotdef
				      (concatenate 'string sname "-")
				      0 export)
	   ',name)))))

(setf (get 'define-c-struct 'ed:lisp-indent-hook) 'defun)
(setf (get '*define-c-struct 'ed:lisp-indent-hook) 'defun)

(defmacro define-c-struct (name &rest slots)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-c-struct-expand name slots *c-typedef-auto-export*)))

(defmacro *define-c-struct (name &rest slots)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-c-struct-expand name slots t)))

(defmacro c-struct-size-of (struct &optional (slot nil sv))
  (let ((type (expand-c-type struct)))
    (unless (ctypedef-slots type)
      (error "~Aは構造体ではありません" type))
    (if sv
	(let ((slotdef (find slot (ctypedef-slots type)
			     :test #'eq :key #'cslot-name)))
	  (unless slotdef
	    (error "~Aは~Aのメンバではありません" slot type))
	  (cslot-size slotdef))
      (ctypedef-size type))))

(defmacro c-struct-offset-of (struct slot)
  (let ((type (expand-c-type struct)))
    (unless (ctypedef-slots type)
      (error "~Aは構造体ではありません" type))
    (let ((slotdef (find slot (ctypedef-slots type)
			 :test #'eq :key #'cslot-name)))
      (unless slotdef
	(error "~Aは~Aのメンバではありません" slot type))
      (cslot-offset slotdef))))

(setf (get 'define 'ed:lisp-indent-hook) 'defun)
(setf (get '*define 'ed:lisp-indent-hook) 'defun)

(defmacro define (sym val)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,sym ,val)
     (when *c-typedef-auto-export*
       (export ',sym))))

(defmacro *define (sym val)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,sym ,val)
     (export ',sym)))

(setf (get '*define-c-macro 'ed:lisp-indent-hook) 'defun)

(defmacro *define-c-macro (name (&rest lambda-list) &body body)
  `(progn
     (defmacro ,name ,lambda-list ,@body)
     (export ',name)))

(defmacro c-size-of (type)
  (ctypedef-size (expand-c-type type)))

(setf (symbol-function 'last-win32-error) #'si:*last-win32-error)
(setf (symbol-function 'set-last-win32-error) #'si:*set-last-win32-error)
(defsetf last-win32-error set-last-win32-error)

#|
struct foo
{
  int i;
  char *s;
  short a[32];
};

(define-c-struct foo
  (int i)
  (char * s)
  (short a 32))
|#
