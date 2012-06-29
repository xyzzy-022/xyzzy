;; need docstring support!
(deftest fix-previous-pseudo-frame-test ()
  "(previous-pseudo-frame) caused \"appframe not found\" error."
  (previous-pseudo-frame)
  => nil)

(defun make-test-package (&rest options)
  (let ((name (format nil "temporary-package-for-test-~D" (test-uid))))
    (apply #'make-package name options)))

(deftest fix-list-all-packages-test ()
  "(list-all-packages) should return 'a copy of' system package list."
  (let* ((pkg (make-test-package))
         (name (package-name pkg))
	 (pkglist (list-all-packages)))
    (delete pkg pkglist)
    (eq pkg (find-package name)))
  => t)

(let ((os-version (list (os-major-version) (os-minor-version)))
      (os-features '(:windows-8 :windows-7 :windows-vista :windows-xp :windows-2000)))
  ;; Windows 2000
  (when (equalp '(5 0) os-version)
    (deftest os-features-2000-test ()
      "*features* contains :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil nil nil nil t))
    (deftest os-platform-2000-test ()
      "(os-platform) should return :windows-2000"
      (os-platform)
      => windows-2000)
    (deftest user-config-path-and-dump-image-path-2000-test ()
      "(user-config-path) and (si:dump-image-path) should return w2k"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "w2k"
      => "w2k"))
  ;; Windows XP
  (when (equalp '(5 1) os-version)
    (deftest os-features-xp-test ()
      "*features* contains :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil nil nil t t))
    (deftest os-platform-xp-test ()
      "(os-platform) should return :windows-xp"
      (os-platform)
      => windows-xp)
    (deftest user-config-path-and-dump-image-path-xp-test ()
      "(user-config-path) and (si:dump-image-path) should return wxp"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  ;; Windows Vista
  (when (equalp '(6 0) os-version)
    (deftest os-features-vista-test ()
      "*features* contains :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil nil t t t))
    (deftest os-platform-vista-test ()
      "(os-platform) should return :windows-vista"
      (os-platform)
      => windows-vista)
    (deftest user-config-path-and-dump-image-path-vista-test ()
      "(user-config-path) and (si:dump-image-path) は wxp のまま。0.2.2.235 でもパスする"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  ;; Windows 7
  (when (equalp '(6 1) os-version)
    (deftest os-features-7-test ()
      "*features* contains :windows-7, :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil t t t t))
    (deftest os-platform-7-test ()
      "(os-platform) should return :windows-7"
      (os-platform)
      => windows-7)
    (deftest user-config-path-and-dump-image-path-7-test ()
      "(user-config-path) and (si:dump-image-path) は wxp のまま。0.2.2.235 でもパスする"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  ;; Windows 8
  (when (equalp '(6 2) os-version)
    (deftest os-features-8-test ()
      "*features* contains :windows-8, :windows-7, :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (t t t t t))
    (deftest os-platform-8-test ()
      "(os-platform) should return :windows-8"
      (os-platform)
      => windows-8)
    (deftest user-config-path-and-dump-image-path-8-test ()
      "(user-config-path) and (si:dump-image-path) は wxp のまま。0.2.2.235 でもパスする"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  )


(deftest lisp-implementation-type ()
  (lisp-implementation-type)
  => "xyzzy")

(deftest lisp-implementation-version ()
  (string= (software-version) (lisp-implementation-version))
  => t)


(deftest machine-instance ()
  (string= (machine-instance) (machine-name))
  => t)

(deftest machine-type ()
  (machine-type)
  => #+x86 "x86" #+x64 "x64" #+ia64 "IA64")

(deftest machine-version ()
  (string= (machine-version) (si:getenv "PROCESSOR_IDENTIFIER"))
  => t)


(deftest test-cl-char-Backspace ()
  "Backspace 文字が使えるか"
  (let ((c (read-from-string "#\\Backspace")))
    (values
     (char= c #\C-h)
     (char-code c)
     (char-name c)))
  => t
  => 8
  => "backspace")

(deftest test-cl-char-Tab ()
  "文字名の大文字小文字の区別はしないので、これは 0.2.2.235 でもパスする"
  (let ((c (read-from-string "#\\Tab")))
    (values
     (char= c #\TAB)
     (char-code c)
     (char-name c)))
  => t
  => 9
  => "horizontal_tabulation")

(deftest test-cl-char-Newline ()
  "Newline 文字が使えるか"
  (let ((c (read-from-string "#\\Newline")))
    (values
     (char= c #\LFD)
     (char-code c)
     (char-name c)))
  => t
  => 10
  => "line_feed")

(deftest test-cl-char-Linefeed ()
  "Linefeed 文字が使えるか"
  (let ((c (read-from-string "#\\Linefeed")))
    (values
     (char= c #\LFD)
     (char-code c)
     (char-name c)))
  => t
  => 10
  => "line_feed")

(deftest test-cl-char-Page ()
  "Page 文字が使えるか"
  (let ((c (read-from-string "#\\Page")))
    (values
     (char= c #\C-l)
     (char-code c)
     (char-name c)))
  => t
  => 12
  => "form_feed")

(deftest test-cl-char-Return ()
  "Return 文字が使えるか"
  (let ((c (read-from-string "#\\Return")))
    (values
     (char= c #\RET)
     (char-code c)
     (char-name c)))
  => t
  => 13
  => "carriage_return")

(deftest test-cl-char-Space ()
  "Space 文字が使えるか"
  (let ((c (read-from-string "#\\Space")))
    (values
     (char= c #\SPC)
     (char-code c)
     (char-name c)))
  => t
  => 32
  => "space")

(deftest test-cl-char-Rubout ()
  "Rubout 文字が使えるか"
  (let ((c (read-from-string "#\\Rubout")))
    (values
     (char= c #\DEL)
     (char-code c)
     (char-name c)))
  => t
  => 127
  => "delete")

(deftest test-coerce-to-package ()
  (let ((name (gensym "no-such-package-name")))
    (handler-case
        (lisp::coerce-to-package name)
      (simple-error (c)
        (equal `(,name) (simple-error-format-arguments c)))))
  => t)

(deftest test-defpackage-parse-options-1 ()
  (lisp::defpackage-parse-options
   :foo
   '((:shadowing-import-from :lisp :car)
     (:shadowing-import-from :lisp :cadr)
     (:shadow :caddr)))
  => nil
  => ("caddr")
  => (car cadr)
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil)

(deftest test-defpackage-parse-options-2 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :cadr)
         (:shadowing-import-from :lisp :car)
         (:shadow :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadow :shadowing-import-from "car"))

(deftest test-defpackage-parse-options-3 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :car)
         (:shadowing-import-from :lisp :car)
         (:shadow :caddr)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadowing-import-from :import-from "car"))

(deftest test-defpackage-parse-options-4 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :car)
         (:shadowing-import-from :lisp :cadr)
         (:shadow :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadow :import-from "car"))

(deftest test-defpackage-parse-options-5 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :cadr)
         (:shadowing-import-from :lisp :car)
         (:shadow :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadow :shadowing-import-from "car"))

(deftest test-defpackage-parse-options-6 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :cdr :car)
         (:shadowing-import-from :lisp :cddr :car)
         (:shadow :cdddr :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadow :shadowing-import-from "car"))

(deftest test-defpackage-parse-options-7 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :car)
         (:shadowing-import-from :lisp :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadowing-import-from :import-from "car"))

(deftest test-defpackage-parse-options-8 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :cdr)
         (:import-from :lisp :car)
         (:import-from :lisp :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:import-from :import-from "car"))

(deftest test-defpackage-parse-options-9 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:import-from :lisp :cdr)
         (:import-from :lisp :car)
         (:shadowing-import-from :lisp :cddr)
         (:shadowing-import-from :lisp :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadowing-import-from :import-from "car"))

(deftest test-defpackage-parse-options-10 ()
  (handler-case
      (lisp::defpackage-parse-options
       :foo
       '((:shadow :cdr :car :cddr)
         (:shadowing-import-from :lisp :cdddr)
         (:shadowing-import-from :lisp :car)))
    (simple-error (c)
      (simple-error-format-arguments c)))
  => (:shadow :shadowing-import-from "car"))

(deftest test-defpackage-parse-options-11 ()
  (lisp::defpackage-parse-options
   :foo
   '((:documentation "hogehoge")
     (:documentation "fugafuga")))
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil
  => "fugafuga")


(deftest test-defstruct-print-function-1 ()
  "print-function を事前に定義。0.2.2.235 でもパスする"
  (let* ((struct-name (gensym "struct-"))
         (print-function-name (gensym "print-function-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     `(defun ,print-function-name (o s p)
        (format s "#<test>")))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:print-function ,print-function-name))
                           '(a b c)))
    (format nil "~A" (eval `(,ctor))))
  => "#<test>")

(deftest test-defstruct-print-function-2 ()
  "print-function を lambda form で定義。0.2.2.235 でもパスする"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     `(let ((v 1))
        ,(si::defstruct-helper `(,struct-name
                                 (:print-function (lambda (o s p) (format s "#<test ~A>" v))))
                               '(a b c))))
    (format nil "~A" (eval `(,ctor))))
  => "#<test 1>")

(deftest test-defstruct-print-function-3 ()
  "print-function をクロージャで定義した場合、警告が出る"
  (let* ((struct-name (gensym "struct-"))
         (print-function-name (gensym "print-function-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     `(defun ,print-function-name (o s p)
        (format s "#<test>")))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:print-function #',print-function-name))
                           '(a b c)))
    (format nil "~A" (eval `(,ctor))))
  => "#<test>")

(deftest test-defstruct-print-function-4 ()
  "print-function をクロージャ (lambda form) で定義した場合、警告が出る"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     `(let ((v 1))
        ,(si::defstruct-helper `(,struct-name
                                 (:print-function #'(lambda (o s p) (format s "#<test ~A>" v))))
                               '(a b c))))
    (format nil "~A" (eval `(,ctor))))
  => "#<test 1>")

(deftest test-defstruct-print-function-5 ()
  "print-function を後で定義する場合"
  (let* ((struct-name (gensym "struct-"))
         (print-function-name (gensym "print-function-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:print-function ,print-function-name))
                           '(a b c)))
    (eval
     `(defun ,print-function-name (o s p)
        (format s "#<test>")))
    (format nil "~A" (eval `(,ctor))))
  => "#<test>")

(deftest test-defstruct-print-function-6 ()
  "print-function を再定義する場合"
  (let* ((struct-name (gensym "struct-"))
         (print-function-name (gensym "print-function-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     `(defun ,print-function-name (o s p)
        (format s "#<test>")))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:print-function ,print-function-name))
                           '(a b c)))
    (eval
     `(defun ,print-function-name (o s p)
        (format s "#<test2>")))
    (format nil "~A" (eval `(,ctor))))
  => "#<test2>")


(deftest test-defstruct-parent-print-function-1 ()
  "親の print-function を継承しない場合コンパイルできる。0.2.2.235 でもパスする"
  (let* ((struct-name1 (gensym "struct-"))
         (struct-name2 (gensym "struct-")))
    (eval
     (si::defstruct-helper `(,struct-name1
                             (:print-function (lambda (o s p) (format o "#<test>"))))
                           '(a b c)))
    (compile nil `(lambda ()
                    ,@(si::defstruct-helper `(,struct-name2
                                              (:print-function (lambda (o s p) (format o "#<test2>")))
                                              (:include ,struct-name1))
                                            '(d)))))
  => non-nil)

(deftest test-defstruct-parent-print-function-2 ()
  "親の print-function を継承する場合コンパイルできる (lambda form)"
  (let* ((struct-name1 (gensym "struct-"))
         (struct-name2 (gensym "struct-")))
    (declare (special struct-name1 struct-name2))
    (eval
     (si::defstruct-helper `(,struct-name1
                             (:print-function (lambda (o s p) (format o "#<test>"))))
                           '(a b c)))
    (compile nil `(lambda ()
                    ,@(si::defstruct-helper `(,struct-name2
                                              (:include ,struct-name1))
                                            '(d)))))
  => non-nil)

(deftest test-defstruct-parent-print-function-3 ()
  "親の print-function を継承する場合コンパイルできる (symbol)"
  (let* ((struct-name1 (gensym "struct-"))
         (struct-name2 (gensym "struct-"))
         (print-function-name1 (gensym "print-function-"))
         (ctor2 (intern (format nil "make-~A" struct-name2))))
    (declare (special struct-name1 struct-name2))
    (eval
     `(defun ,print-function-name1 (o s p)
        (format s "#<test>")))
    (eval
     (si::defstruct-helper `(,struct-name1
                             (:print-function ,print-function-name1))
                           '(a b c)))
    (compile nil `(lambda ()
                    ,@(si::defstruct-helper `(,struct-name2
                                              (:include ,struct-name1))
                                            '(d)))))
  => non-nil)


(deftest test-defstruct-default-constructor-1 ()
  ":constructor 指定がない場合デフォルトコンストラクタが作成される"
  (let* ((struct-name (gensym "struct-"))
         (default-ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name)
                           '(a b c)))
    (values
     (fboundp default-ctor)
     (typep (eval `(,default-ctor)) struct-name)))
  => t
  => t)

(deftest test-defstruct-default-constructor-2 ()
  ":constructor 指定がない場合デフォルトコンストラクタが作成される"
  (let* ((struct-name (gensym "struct-"))
         (default-ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor))
                           '(a b c)))
    (values
     (fboundp default-ctor)
     (typep (eval `(,default-ctor)) struct-name)))
  => t
  => t)

(deftest test-defstruct-default-constructor-3 ()
  "デフォルトコンストラクタと違う名前で :constructor 指定がある場合、
デフォルトコンストラクタは作成されない"
  (let* ((struct-name (gensym "struct-"))
         (default-ctor (intern (format nil "make-~A" struct-name)))
         (other-ctor (intern (format nil "make-~A-2" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,other-ctor))
                           '(a b c)))
    (values
     (fboundp default-ctor)
     (fboundp other-ctor)
     (typep (eval `(,other-ctor)) struct-name)))
  => nil
  => t
  => t)

(deftest test-defstruct-default-constructor-4 ()
  "デフォルトコンストラクタと同じ名前で :constructor 指定がある場合、
デフォルトコンストラクタは作成されない"
  (let* ((struct-name (gensym "struct-"))
         (default-ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,default-ctor (a b c)))
                           '(a b c)))
    (values
     (fboundp default-ctor)
     (typep (eval `(,default-ctor 1 2 3)) struct-name)))
  => t
  => t)


(deftest test-defstruct-constructor-arg-1 ()
  "コンストラクタで引数をすべて指定した場合。0.2.2.235 でもパスする"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A-2" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a b c)))
                           '(a b c)))
    (let ((o (eval `(,ctor 1 2 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c))))
  => (1 2 3))

(deftest test-defstruct-constructor-arg-2 ()
  "コンストラクタで引数を一部指定した場合"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A-2" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a c)))
                           '(a b c)))
    (let ((o (eval `(,ctor 1 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c))))
  => (1 nil 3))

(deftest test-defstruct-constructor-arg-3 ()
  "コンストラクタで引数を指定した場合 (&optional)"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A-2" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a &optional c (d 4))))
                           '(a b c d)))
    (let ((o (eval `(,ctor 1 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d))))
  => (1 nil 3 4))

(deftest test-defstruct-constructor-arg-4 ()
  "コンストラクタで引数を指定した場合 (&key)"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A-2" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a &key (c 3) d)))
                           '(a b c d)))
    (let ((o (eval `(,ctor 1 :d 4))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d))))
  => (1 nil 3 4))


(deftest test-defstruct-default-constructor-arg-1 ()
  "コンストラクタで引数をすべて指定した場合"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a b c)))
                           '(a b c)))
    (let ((o (eval `(,ctor 1 2 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c))))
  => (1 2 3))

(deftest test-defstruct-default-constructor-arg-2 ()
  "コンストラクタで引数を一部指定した場合"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a c)))
                           '(a b c)))
    (let ((o (eval `(,ctor 1 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c))))
  => (1 nil 3))

(deftest test-defstruct-default-constructor-arg-3 ()
  "コンストラクタで引数を指定した場合 (&optional)"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a &optional c (d 4))))
                           '(a b c d)))
    (let ((o (eval `(,ctor 1 3))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d))))
  => (1 nil 3 4))

(deftest test-defstruct-default-constructor-arg-4 ()
  "コンストラクタで引数を指定した場合 (&key)"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a &key (c 3) d)))
                           '(a b c d)))
    (let ((o (eval `(,ctor 1 :d 4))))
      (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d))))
  => (1 nil 3 4))


(deftest test-defstruct-constructor-with-slots-1 ()
  "コンストラクタを指定してもスロット定義の初期値が無視されない"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a b)))
                           '((a 1) (b 2) (c 3) (d 4))))
    (macrolet ((test (&rest args)
                 `(let ((o (eval `(,ctor ,,@args))))
                    (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d)))))
      (values (test 10 20)
              (test nil nil)
              )))
  => (10 20 3 4)
  => (nil nil 3 4))

(deftest test-defstruct-constructor-with-slots-2 ()
  "コンストラクタを指定してもスロット定義の初期値が無視されない"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (a b &optional (c 30))))
                           '((a 1) (b 2) (c 3) (d 4))))
    (macrolet ((test (&rest args)
                 `(let ((o (eval `(,ctor ,,@args))))
                    (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d)))))
      (values (test 10 20)
              (test 10 20 300)
              (test nil nil nil)
              )))
  => (10 20 30 4)
  => (10 20 300 4)
  => (nil nil nil 4))

(deftest test-defstruct-constructor-with-slots-3 ()
  "コンストラクタを指定してもスロット定義の初期値が無視されない"
  (let* ((struct-name (gensym "struct-"))
         (ctor (intern (format nil "make-~A" struct-name))))
    (eval
     (si::defstruct-helper `(,struct-name
                             (:constructor ,ctor (&optional (c 30))))
                           '(a b c (d 40))))
    (macrolet ((test (&rest args)
                 `(let ((o (eval `(,ctor ,,@args))))
                    (mapcar #'(lambda (slot) (si:*slot-value o slot)) '(:a :b :c :d)))))
      (values (test)
              (test 300)
              (test nil)
              )))
  => (nil nil 30 40)
  => (nil nil 300 40)
  => (nil nil nil 40))


(defmacro with-lispmode-bed (&body body)
  `(with-temp-buffer
     (lisp-mode)
     ,@body))

(defmacro user::smart-indentation-test (&whole whole &environment env vars &body body)
  `(prog ,vars ,@body))

(deftest lispmode-smart-indentation-for-macro-form-test ()
  "smart indentation for macro function symbol without
lisp-indent-hook property"
  (let ((*lisp-body-indent* 2)
	(*lisp-body-indention* 2))
    (with-lispmode-bed
      (setq *buffer-package* "user")
      (insert "(smart-indentation-test (a b)")
      (goto-char 23)
      #0=(lisp-newline-and-indent)
      (values #1=(current-column)
	      (and (goto-char (point-max)) #0# #1#))))
  => 4
  => 2)

(deftest fix-truename-1 ()
  (let ((path (merge-pathnames "drivers/etc/hosts" (get-system-directory))))
    (path-equal path (truename path)))
  => t)

(deftest fix-truename-2 ()
  (let ((path (merge-pathnames "drivers/etc" (get-system-directory))))
    (path-equal path (truename path)))
  => t)

(deftest fix-truename-3 ()
  (let ((path (merge-pathnames "drivers" (get-system-directory))))
    (path-equal path (truename path)))
  => t)

(deftest fix-truename-4 ()
  (let ((path (merge-pathnames "System32" (si:getenv "SystemRoot"))))
    (path-equal path (truename path)))
  => t)

#+wow64
(deftest fix-truename-5 ()
  (let ((path (merge-pathnames "SysWOW64" (si:getenv "SystemRoot"))))
    (path-equal path (truename path)))
  => t)

#+wow64
(deftest fix-truename-6 ()
  (let ((path (merge-pathnames "Sysnative" (si:getenv "SystemRoot"))))
    (path-equal path (truename path)))
  => t)

(deftest defpackage-documentation ()
  (let* ((pkg-name (gensym "pkg-")))
    (eval
     `(defpackage ,pkg-name
        (:documentation "test package")))
    (documentation (find-package pkg-name) t))
  => "test package")

(deftest defpackage-documentation-wrong-doc-type ()
  (let* ((pkg-name (gensym "pkg-")))
    (eval
     `(defpackage ,pkg-name
        (:documentation "test package")))
    (documentation (find-package pkg-name) nil))
  !! si::check-type-error)


(deftest hash-table-rehash-threshold-1 ()
  (values
   (hash-table-rehash-threshold (make-hash-table))
   (hash-table-rehash-threshold (make-hash-table :rehash-threshold 9/10))
   (hash-table-rehash-threshold (make-hash-table :rehash-threshold 0.9))
   (hash-table-rehash-threshold (make-hash-table :rehash-threshold 0.9d0))
   (hash-table-rehash-threshold (make-hash-table :rehash-threshold 1))
   (hash-table-rehash-threshold (make-hash-table :rehash-threshold 1.0)))
  => 0.8
  => 0.9
  => 0.9
  => 0.9
  => 1.0
  => 1.0)

(deftest hash-table-rehash-threshold-2 ()
  (let ((h (make-hash-table :rehash-threshold 5/10)))
    (dotimes (i 9)
      (setf (gethash i h) i))
    (values
     (list (hash-table-rehash-threshold h)
           (hash-table-count h)
           (hash-table-size h))
     (progn
       (setf (gethash :x h) :x)
       (list (hash-table-rehash-threshold h)
             (hash-table-count h)
             (hash-table-size h)))))
  => (0.5 9 17)
  => (0.5 10 47))

(deftest hash-table-rehash-threshold-3 ()
  (let ((h (make-hash-table :rehash-threshold 1)))
    (dotimes (i 17)
      (setf (gethash i h) i))
    (values
     (list (hash-table-rehash-threshold h)
           (hash-table-count h)
           (hash-table-size h))
     (progn
       (setf (gethash :x h) :x)
       (list (hash-table-rehash-threshold h)
             (hash-table-count h)
             (hash-table-size h)))))
  => (1.0 17 17)
  => (1.0 18 47))

(deftest hash-table-rehash-threshold-error-1 ()
  (make-hash-table :rehash-threshold -1)
  !! type-error)

(deftest hash-table-rehash-threshold-error-2 ()
  (make-hash-table :rehash-threshold 1.1)
  !! type-error)


(deftest hash-table-rehash-size-1 ()
  (values
   (hash-table-rehash-size (make-hash-table))
   (hash-table-rehash-size (make-hash-table :rehash-size 1))
   (hash-table-rehash-size (make-hash-table :rehash-size 2))
   (hash-table-rehash-size (make-hash-table :rehash-size 100))
   (hash-table-rehash-size (make-hash-table :rehash-size 1.1))
   (hash-table-rehash-size (make-hash-table :rehash-size 2.2)))
  => 1.5
  => 1
  => 2
  => 100
  => 1.1
  => 2.2)

(deftest hash-table-rehash-size-2 ()
  (let ((h (make-hash-table :rehash-size 2.0)))
    (dotimes (i 14)
      (setf (gethash i h) i))
    (values
     (list (hash-table-rehash-size h)
           (hash-table-count h)
           (hash-table-size h))
     (progn
       (setf (gethash :x h) :x)
       (list (hash-table-rehash-size h)
             (hash-table-count h)
             (hash-table-size h)))))
  => (2.0 14 17)
  => (2.0 15 47))

(deftest hash-table-rehash-size-3 ()
  (let ((h (make-hash-table :rehash-size 100)))
    (dotimes (i 14)
      (setf (gethash i h) i))
    (values
     (list (hash-table-rehash-size h)
           (hash-table-count h)
           (hash-table-size h))
     (progn
       (setf (gethash :x h) :x)
       (list (hash-table-rehash-size h)
             (hash-table-count h)
             (hash-table-size h)))))
  => (100 14 17)
  => (100 15 149))

(deftest hash-table-rehash-size-error-1 ()
  (make-hash-table :rehash-size -1)
  !! type-error)

(deftest hash-table-rehash-size-error-2 ()
  (make-hash-table :rehash-size 0)
  !! type-error)

(deftest hash-table-rehash-size-error-3 ()
  (make-hash-table :rehash-size 0.9)
  !! type-error)

(deftest hash-table-rehash-size-error-4 ()
  (make-hash-table :rehash-size -2.2)
  !! type-error)

(deftest copy-tree-circular-list-1 ()
  (let* ((a '#1=(#1#))
         (b (copy-tree a)))
    (values
     (eq a (car a))
     (eq b (car b))
     (not (eq a b))))
  => t
  => t
  => t)

(deftest copy-tree-circular-list-2 ()
  (let* ((a '#1=(1 2 3 #2=(4 5 6) #2# #1#))
         (b (copy-tree a)))
    (values
     (not (eql (butlast a) (butlast b)))
     (equal (butlast a) (butlast b))

     (not (eq (car (last a)) (car (last b))))
     (eq a (car (last a)))
     (eq b (car (last b)))))
  => t
  => t
  => t
  => t
  => t)


(deftest equal-circular-list-regression-test ()
  (values
   (equal nil nil)
   (equal nil "hoge")
   (equal 123 (+ 100 23))
   (equal "hoge" (copy-seq "hoge"))
   (equal '(1 2 (3 "foo")) (copy-tree '(1 2 (3 "foo")))))
  => t
  => nil
  => t
  => t
  => t)

(deftest equal-circular-list-1 ()
  (let* ((a '#1=(#1#))
         (b (copy-tree a)))
    (values
     (equal a b)
     (equal a nil)
     (equal a '#2=(#2# #2#))))
  => t
  => nil
  => nil)

(deftest equal-circular-list-2 ()
  (let ((a '#1=(1 2 3 #2=(4 5 6) #2# #1#))
        (b '#3=(1 2 3 (4 5 6) (4 5 6) #3#))
        (c '#4=(1 2 3 (4 5 6) "2" #4#)))
    (values
     (equal a b)
     (equal a c)
     (equal b c)))
  => t
  => nil
  => nil)


(deftest equalp-circular-list-regression-test ()
  (values
   (equalp nil nil)
   (equalp nil "hoge")
   (equalp 123 (+ 100 23))
   (equalp "hoge" "HOGE")
   (equalp '(1 2 (3 "foo")) (copy-tree '(1 2 (3 "FOO")))))
  => t
  => nil
  => t
  => t
  => t)

(deftest equalp-circular-list-1 ()
  (let* ((a '#1=(#1#))
         (b (copy-tree a)))
    (values
     (equalp a b)
     (equalp a nil)
     (equalp a '#2=(#2# #2#))))
  => t
  => nil
  => nil)

(deftest equalp-circular-list-2 ()
  (let ((a '#1=(1 2 3 #2=(4 5 6) #2# #1#))
        (b '#3=(1 2 3 (4 5 6) (4 5 6) #3#))
        (c '#4=(1 2 3 (4 5 6) "2" #4#)))
    (values
     (equalp a b)
     (equalp a c)
     (equalp b c)))
  => t
  => nil
  => nil)


(defun parse-body (body)
  (multiple-value-bind (decl body)
      (lisp::find-declaration body)
    (multiple-value-bind (intr body)
        (lisp::find-interactive body)
      (let ((doc (lisp::find-documentation body)))
        (values doc intr decl body)))))

(deftest find-declaration-none ()
  (parse-body
   '("not-doc-string"))
  => nil
  => nil
  => nil
  => ("not-doc-string"))

(deftest find-declaration-inteactive ()
  (parse-body
   '((interactive)))
  => nil
  => ((interactive))
  => nil
  => nil)

(deftest find-declaration-docstring-inteactive ()
  (parse-body
   '("doc-string"
     (interactive)
     t))
  => "doc-string"
  => ((interactive))
  => nil
  => ("doc-string" t))

(deftest find-declaration-inteactive-docstring ()
  (parse-body
   '((interactive)
     "doc-string"
     t))
  => "doc-string"
  => ((interactive))
  => nil
  => ("doc-string" t))

(deftest find-declaration-inteactive-none ()
  (parse-body
   '((interactive)
     "not-doc-string"))
  => nil
  => ((interactive))
  => nil
  => ("not-doc-string"))

(deftest find-declaration-inteactive-2 ()
  (parse-body
   '("not-doc-string"
     (interactive)))
  => nil
  => ((interactive))
  => nil
  => ("not-doc-string"))

(deftest find-declaration-declare-docstring ()
  (parse-body
   '((declare (special a))
     "doc-string"
     t))
  => "doc-string"
  => nil
  => ((declare (special a)))
  => ("doc-string" t))

(deftest find-declaration-docstring-declare ()
  (parse-body
   '("doc-string"
     (declare (special a))
     t))
  => "doc-string"
  => nil
  => ((declare (special a)))
  => ("doc-string" t))

(deftest find-declaration-declare-interactive ()
  (parse-body
   '((declare (special a))
     (interactive)
     t))
  => nil
  => ((interactive))
  => ((declare (special a)))
  => (t))

(deftest find-declaration-interactive-declare ()
  (parse-body
   '((interactive)
     (declare (special a))
     t))
  => nil
  => ((interactive))
  => ((declare (special a)))
  => (t))

(deftest find-declaration-docstring-interactive-declare ()
  (parse-body
   '("doc-string"
     (interactive)
     (declare (special a))
     t))
  => "doc-string"
  => ((interactive))
  => ((declare (special a)))
  => ("doc-string" t))

(deftest find-declaration-interactive-docstirng-declare ()
  (parse-body
   '((interactive)
     "doc-string"
     (declare (special a))
     t))
  => "doc-string"
  => ((interactive))
  => ((declare (special a)))
  => ("doc-string" t))

(deftest find-declaration-declare-interactive-declare ()
  (parse-body
   '((declare (special a))
     (interactive)
     (declare (special b))
     "not-doc-string"))
  => nil
  => ((interactive))
  => ((declare (special a) (special b)))
  => ("not-doc-string"))

(deftest find-declaration-declare-interactive-declare-docstring ()
  (parse-body
   '((declare (special a))
     (interactive)
     (declare (special b))
     "doc-string"
     t))
  => "doc-string"
  => ((interactive))
  => ((declare (special a) (special b)))
  => ("doc-string" t))

(deftest find-declaration-declare-interactive-declare-docstring-declare ()
  (parse-body
   '((declare (special a))
     (interactive)
     (declare (special b))
     "doc-string"
     (declare (special c))
     t))
  => "doc-string"
  => ((interactive))
  => ((declare (special a) (special b) (special c)))
  => ("doc-string" t))

(deftest find-declaration-declare-interctive-declare-docstring-let-declare ()
  (parse-body
   '((declare (special a))
     (interactive)
     (declare (special b))
     "doc-string"
     (let ((c 1))
       (declare (special c))
       t)))
  => "doc-string"
  => ((interactive))
  => ((declare (special a) (special b)))
  => ("doc-string" (let ((c 1)) (declare (special c)) t)))

(deftest find-declaration-extra-declare-1 ()
  (parse-body
   '((declare (special a))
     (interactive)
     (declare (special b))
     "not-doc-string"
     (declare (special c))))
  !! simple-error)

(deftest find-declaration-extra-declare-2 ()
  (parse-body
   '(t
     (declare (special a))))
  !! simple-error)

(deftest redefun-docstring ()
  (let ((sym (gensym)))
    (eval `(values
            (progn
              (defun ,sym () "docstring1" 123)
              (documentation ',sym 'function))
            (progn
              (defun ,sym () "docstring2" 123)
              (documentation ',sym 'function))
            (progn
              (defun ,sym () 123)
              (documentation ',sym 'function)))))
  => "docstring1"
  => "docstring2"
  => nil)

(deftest nth-value-1 ()
  (values
   (nth-value 0 (values 1 2 3))
   (nth-value 1 (values 1 2 3))
   (nth-value 2 (values 1 2 3))
   (nth-value 3 (values 1 2 3))
   (nth-value 4 (values 1 2 3))
   (nth-value 1000 (values 1 2 3)))
  => 1
  => 2
  => 3
  => nil
  => nil
  => nil)

(deftest nth-value-2 ()
  (values
   (nth-value 0 "hoge")
   (nth-value 1 "hoge")
   (nth-value 2 "hoge")
   (nth-value 3 "hoge")
   (nth-value 4 "hoge")
   (nth-value 1000 "hoge"))
  => "hoge"
  => nil
  => nil
  => nil
  => nil
  => nil)

(deftest nth-value-type-error ()
  (nth-value -1 (values 1 2 3))
  !! type-error)


(deftest rename-package--rename-all ()
  (let ((name1 (gensym))
        (name2 (gensym))
        (nick1 (gensym))
        (nick2 (gensym))
        (nick3 (gensym)))
    (let ((pkg (make-package name1 :nicknames (list nick1 nick2))))
      (rename-package name1 name2 (list nick3))
      (values
       (find-package name1)
       (find-package nick1)
       (find-package nick2)
       (equal (string name2) (package-name (find-package name2)))
       (equal (list (string nick3))
              (package-nicknames (find-package name2))))))
  => nil
  => nil
  => nil
  => t
  => t)

(deftest rename-package--add-nicknames ()
  (let ((name1 (gensym))
        (nick1 (gensym))
        (nick2 (gensym))
        (nick3 (gensym)))
    (let ((pkg (make-package name1 :nicknames (list nick1 nick2))))
      (rename-package name1 name1 (list nick1 nick2 nick3))
      (values
       (equal (string name1) (package-name (find-package name1)))
       (equal (mapcar #'string (list nick3 nick2 nick1))
              (package-nicknames (find-package name1))))))
  => t
  => t)

(deftest rename-package--remove-nicknames ()
  (let ((name1 (gensym))
        (nick1 (gensym))
        (nick2 (gensym)))
    (let ((pkg (make-package name1 :nicknames (list nick1 nick2))))
      (rename-package name1 name1 (list nick1))
      (values
       (find-package nick2)
       (equal (string name1) (package-name (find-package name1)))
       (equal (mapcar #'string (list nick1))
              (package-nicknames (find-package name1))))))
  => nil
  => t
  => t)

(deftest rename-package--swap-package-name-and-nickname ()
  (let ((name1 (gensym))
        (nick1 (gensym))
        (nick2 (gensym))
        (nick3 (gensym)))
    (let ((pkg (make-package name1 :nicknames (list nick1 nick2))))
      (rename-package name1 nick1 (list name1 nick2))
      (values
       (equal (string nick1) (package-name (find-package nick1)))
       (equal (mapcar #'string (list nick2 name1))
              (package-nicknames (find-package nick1))))))
  => t
  => t)

(deftest rename-package--package-name-conflict ()
  (let ((name1 (gensym))
        (name2 (gensym))
        (name3 (gensym))
        (nick1 (gensym))
        (nick2 (gensym))
        (nick3 (gensym)))
    (let ((pkg (make-package name1 :nicknames (list nick1))))
      (make-package name2)
      (make-package nick2)
      (flet ((test (conflict package new-name &optional new-nicknames)
               (handler-case
                   (rename-package package new-name new-nicknames)
                 (simple-package-error (c)
                   (equal (string conflict)
                          (simple-package-error-datum2 c))))))
        (values
         (test name2 name1 name2)
         (test nick2 name1 nick2)
         (test nick2 name1 name3 (list nick2))
         (test name2 name1 name3 (list nick1 name2))
         (test nick2 name1 name3 (list nick1 nick2 nick3))))))
  => t
  => t
  => t
  => t
  => t)


(defmacro macro-function-foo (a b)
  `(* ,a ,b))

(defmacro macro-function-bar (b c)
  `(* ,b ,c))

(deftest macro-function-environment ()
  (let (env)
    (macrolet ((macro-function-foo (a b)
                 `(+ ,a ,b))
               (macro-function-baz (a c)
                 `(+ ,a ,c))
               (env (&environment env)
                 env))
      (setf env (env)))
    (values
     (macro-function 'macro-function-foo)
     (macro-function 'macro-function-foo nil)
     (macro-function 'macro-function-foo env)
     (macro-function 'macro-function-foo '((macro-function-foo
                                            macro (a b) (block macro-function-foo (list '/ a b)))))
     (macro-function 'macro-function-bar)
     (macro-function 'macro-function-bar env)
     (macro-function 'macro-function-baz)
     (macro-function 'macro-function-baz env)
     ))
  => (macro (a b) (block macro-function-foo (list '* a b)))
  => (macro (a b) (block macro-function-foo (list '* a b)))
  => (macro (a b) (block macro-function-foo (list '+ a b)))
  => (macro (a b) (block macro-function-foo (list '/ a b)))
  => (macro (b c) (block macro-function-bar (list '* b c)))
  => (macro (b c) (block macro-function-bar (list '* b c)))
  => nil
  => (macro (a c) (block macro-function-baz (list '+ a c))))


(deftest locally-special-form-p ()
  (special-form-p 'locally)
  => non-nil)

(deftest locally-0 ()
  (values
   (locally)
   (locally t)
   (locally nil))
  => nil
  => t
  => nil)

(deftest locally-unbound-variable ()
  (values
   (handler-case
       (locally
	 locally-a)
     (unbound-variable (e)
       t))

   (handler-case
       (locally
	 (declare (special locally-a))
	 locally-a)
     (unbound-variable (e)
       t))

   (let ((locally-a 123))
     (locally
       (declare (special locally-a))
       t))

   (let ((locally-a 123))
     (locally
       locally-a))

   (handler-case
       (let ((locally-a 123))
	 (locally
	   (declare (special locally-a))
	   locally-a))
     (unbound-variable (e)
       t)))
  => t
  => t
  => t
  => 123
  => t)

(setq locally-b 0)
(deftest locally-toplev-setq ()
  (values
   (locally
     locally-b)

   (locally
     (declare (special locally-b))
     locally-b)

   (let ((locally-b 123))
     (locally
       locally-b))

   (let ((locally-b 123))
     (locally
       (declare (special locally-b))
       locally-b)))
  => 0
  => 0
  => 123
  => 0)

(defvar locally-c 0)
(deftest locally-defvar ()
  (values
   (locally
     locally-c)

   (locally
     (declare (special locally-c))
     locally-c)

   (let ((locally-c 123))
     (locally
       locally-c))

   (let ((locally-c 123))
     (locally
       (declare (special locally-c))
       locally-c)))
  => 0
  => 0
  => 123
  => 123)

(deftest locally-declare-special ()
  (values
   (let ((locally-d 0))
     (declare (special locally-d))
     (locally
       locally-d))

   (let ((locally-d 0))
     (declare (special locally-d))
     (locally
       (declare (special locally-d))
       locally-d))

   (let ((locally-d 0))
     (declare (special locally-d))
     (let ((locally-d 123))
       (locally
	 locally-d)))

   (let ((locally-d 0))
     (declare (special locally-d))
     (let ((locally-d 123))
       (locally
	 (declare (special locally-d))
	 locally-d))))
  => 0
  => 0
  => 123
  => 0)

(defvar-local locally-e 0)
(deftest locally-eefvar-local ()
  (values
   (locally
     locally-e)

   (locally
     (declare (special locally-e))
     locally-e)

   (let ((locally-e 123))
     (locally
       locally-e))

   (let ((locally-e 123))
     (locally
       (declare (special locally-e))
       locally-e))

   (with-temp-buffer
    (setq locally-e 1)
    (let ((locally-e 123))
      (locally
        (declare (special locally-e))
        locally-e))))
  => 0
  => 0
  => 123
  => 0
  => 1)


(defun locally-sample-function (y)  ;this y is regarded as special
  (declare (special y))
  (let ((y t))              ;this y is regarded as lexical
    (list y
	  (locally (declare (special y))
	    ;; this next y is regarded as special
	    y)
	  y)))

(deftest locally-clhs-1 ()
  (locally-sample-function nil)
  => (t nil t))


(deftest locally-0-compile ()
  (compile-and-eval
    (values
     (locally)
     (locally t)
     (locally nil)))
  => nil
  => t
  => nil)

(deftest locally-unbound-variable-compile ()
  (compile-and-eval
    (values
     (handler-case
	 (locally
	   locally-a)
       (unbound-variable (e)
	 t))

     (handler-case
	 (locally
	   (declare (special locally-a))
	   locally-a)
       (unbound-variable (e)
	 t))

     (let ((locally-a 123))
       (locally
	 (declare (special locally-a))
	 t))

     (let ((locally-a 123))
       (locally
	 locally-a))

     (handler-case
	 (let ((locally-a 123))
	   (locally
	     (declare (special locally-a))
	     locally-a))
       (unbound-variable (e)
	 t))))
  => t
  => t
  => t
  => 123
  => t)

(setq locally-b 0)
(deftest locally-toplev-setq-compile ()
  (compile-and-eval
    (values
     (locally
       locally-b)

     (locally
       (declare (special locally-b))
       locally-b)

     (let ((locally-b 123))
       (locally
	 locally-b))

     (let ((locally-b 123))
       (locally
	 (declare (special locally-b))
	 locally-b))))
  => 0
  => 0
  => 123
  => 0)

(defvar locally-c 0)
(deftest locally-defvar-compile ()
  (compile-and-eval
    (values
     (locally
       locally-c)

     (locally
       (declare (special locally-c))
       locally-c)

     (let ((locally-c 123))
       (locally
	 locally-c))

     (let ((locally-c 123))
       (locally
	 (declare (special locally-c))
	 locally-c))))
  => 0
  => 0
  => 123
  => 123)

(deftest locally-declare-special-compile ()
  (compile-and-eval
    (values
     (let ((locally-d 0))
       (declare (special locally-d))
       (locally
	 locally-d))

     (let ((locally-d 0))
       (declare (special locally-d))
       (locally
	 (declare (special locally-d))
	 locally-d))

     (let ((locally-d 0))
       (declare (special locally-d))
       (let ((locally-d 123))
	 (locally
	   locally-d)))

     (let ((locally-d 0))
       (declare (special locally-d))
       (let ((locally-d 123))
	 (locally
	   (declare (special locally-d))
	   locally-d)))))
  => 0
  => 0
  => 123
  => 0)

(defvar-local locally-e 0)
(deftest locally-eefvar-local-compile ()
  (compile-and-eval
    (values
     (locally
       locally-e)

     (locally
       (declare (special locally-e))
       locally-e)

     (let ((locally-e 123))
       (locally
         locally-e))

     (let ((locally-e 123))
       (locally
         (declare (special locally-e))
         locally-e))

     (with-temp-buffer
       (setq locally-e 1)
       (let ((locally-e 123))
         (locally
           (declare (special locally-e))
           locally-e)))))
  => 0
  => 0
  => 123
  => 0
  => 1)


(deftest locally-clhs-1-compile ()
  (compile-and-eval
    (let ((y nil))
      (declare (special y))
      (let ((y t))
	(list y
	      (locally (declare (special y))
		y)
	      y))))
  => (t nil t))
