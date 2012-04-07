(deftest fix-edict-lookup-word ()
  "単語境界で edict-lookup-word したらその単語を返す"
  (with-temp-buffer
    (insert "I  like   susi.")
    (flet ((test (point)
             (and (goto-char point)
                  (ed::edict-lookup-word #'buffer-substring))))
      (values (test 0) (test 1)
              (test 2) (test 3) (test 4) (test 7)
              (test 8))))
  => "I"
  => "I"
  => "like"
  => "like"
  => "like"
  => "like"
  => "susi")

(deftest fix-do-completion-dup ()
  "import したシンボルと export したシンボルが補完時に重複しないこと"
  (let ((pkg (make-package (gensym) :use :lisp)))
    (export 'caaaar pkg)
    (let ((*package* pkg))
      (*do-completion "caaaa" :function-name)))
  => "caaaar"
  => ("caaaar")
  => nil)

(deftest do-completion-package-name-1 ()
  "パッケージ名の補完ができること"
  (multiple-value-bind (result list prefix)
      (*do-completion "s" :function-name)
    (values
     (member "system" list :test #'string=)
     (member "sys" list :test #'string=)
     (member "si" list :test #'string=)))
  => non-nil
  => non-nil
  => non-nil)

(deftest do-completion-package-name-2 ()
  "パッケージ名の補完ができること"
  (multiple-value-bind (result list prefix)
      (*do-completion "e" :function-name)
    (values
     (member "editor" list :test #'string=)
     (member "ed" list :test #'string=)))
  => non-nil
  => non-nil)

(defmacro with-csharp-mode-bed (&body body)
  `(with-temp-buffer
     (csharp-mode)
     (make-local-variable 'csharp-indent-level)
     (setq csharp-indent-level 2)
     (make-local-variable 'csharp-continued-statement-offset)
     (setq csharp-continued-statement-offset 2)
     (make-local-variable 'csharp-argdecl-indent)
     (setq csharp-argdecl-indent 5)
     (make-local-variable 'csharp-brace-offset)
     (setq csharp-brace-offset 0)
     (make-local-variable 'csharp-brace-imaginary-offset)
     (setq csharp-brace-imaginary-offset 0)
     (make-local-variable 'csharp-label-offset)
     (setq csharp-label-offset -2)
     (make-local-variable 'csharp-comment-indent)
     (setq csharp-comment-indent 2)
     ,@body))

(defun csharp-region-indent-test (line)
  (insert line #\LFD)
  (previous-line)
  (goto-bol)
  (c-indent-line)
  (prog1
      (current-column)
    (forward-line)))

(deftest csharp-mode-calc-c-indent-for-region-directive-1 ()
  (with-csharp-mode-bed
    (values
     (csharp-region-indent-test "#region MyClass")
     (csharp-region-indent-test "class MyClass {")
     (csharp-region-indent-test   "#region Public Methods")
     (csharp-region-indent-test   "#region MyMethod")
     (csharp-region-indent-test   "public void MyMethod() {")
     (csharp-region-indent-test     "#region MyStatement")
     (csharp-region-indent-test     "return;")
     (csharp-region-indent-test     "#endregion // MyStatement")
     (csharp-region-indent-test   "}")
     (csharp-region-indent-test   "#endregion // MyMethod")
     (csharp-region-indent-test   "#endregion // Public Methods")
     (csharp-region-indent-test "}")
     (csharp-region-indent-test "#endregion // MyClass")))
  => 0
  => 0
  => 2
  => 2
  => 2
  => 4
  => 4
  => 4
  => 2
  => 2
  => 2
  => 0
  => 0)

(deftest csharp-mode-calc-c-indent-for-region-directive-2 ()
  (with-csharp-mode-bed
    (values
     (csharp-region-indent-test "class MyClass {")
     (csharp-region-indent-test "#undef DEBUG")
     (csharp-region-indent-test "#define DEBUG")
     (csharp-region-indent-test "#pragma warning disable 414, 3021")
     (csharp-region-indent-test   "# region MyMethod")
     (csharp-region-indent-test   "public int MyMethod() {")
     (csharp-region-indent-test "#if DEBUG")
     (csharp-region-indent-test     "#  region MyStatement")
     (csharp-region-indent-test "# define PI 3.14")
     (csharp-region-indent-test "# line 200")
     (csharp-region-indent-test     "return PI;")
     (csharp-region-indent-test "# line default")
     (csharp-region-indent-test     "#  endregion // MyStatement")
     (csharp-region-indent-test "#elif DEBUG2")
     (csharp-region-indent-test "# warn Not implemented yet.")
     (csharp-region-indent-test "#else")
     (csharp-region-indent-test "# error Not implemented yet.")
     (csharp-region-indent-test "#endif")
     (csharp-region-indent-test   "}")
     (csharp-region-indent-test   "# endregion // MyMethod")
     (csharp-region-indent-test "}")
     (csharp-region-indent-test "#endregion // MyClass")))
  => 0
  => 0
  => 0
  => 0
  => 2
  => 2
  => 0
  => 4
  => 0
  => 0
  => 4
  => 0
  => 4
  => 0
  => 0
  => 0
  => 0
  => 0
  => 2
  => 2
  => 0
  => 0)

(deftest csharp-mode-calc-c-indent-for-using-statement ()
  (with-csharp-mode-bed
    (values
     (csharp-region-indent-test "class MyClass {")
     (csharp-region-indent-test   "public int MyMethod() {")
     (csharp-region-indent-test     "using (StreamReader outFile = new StreamReader(outputFile.OpenRead()))")
     (csharp-region-indent-test     "using (StreamReader expFile = new StreamReader(expectedFile.OpenRead())) {")
     (csharp-region-indent-test       "outFile.Write(expFile.ReadToEnd());")
     (csharp-region-indent-test     "}")
     (csharp-region-indent-test   "}")
     (csharp-region-indent-test "}")))
  => 0
  => 2
  => 4
  => 4
  => 6
  => 4
  => 2
  => 0)

(deftest csharp-mode-calc-c-indent-for-using-directive ()
  (with-csharp-mode-bed
    (values
     (csharp-region-indent-test "namespace MyNS {")
     (csharp-region-indent-test   "using")
     (csharp-region-indent-test     "System.IO;")
     (csharp-region-indent-test "}")))
  => 0
  => 2
  => 4
  => 0)

(deftest detect-char-encoding-ascii ()
  (values
   (detect-char-encoding "")
   (detect-char-encoding "a")
   (detect-char-encoding "abc 123"))
  => nil
  => nil
  => nil)

(deftest detect-char-encoding-sjis ()
  (values
   (eq *encoding-sjis*
       (detect-char-encoding "あいう"))
   (eq *encoding-sjis*
       (with-input-from-string (s "あいう")
         (detect-char-encoding s))))
  => t
  => t)

(deftest detect-char-encoding-utf8 ()
  (values
   (eq *encoding-utf8*
       (detect-char-encoding (convert-encoding-from-internal *encoding-utf8* "初版")))
   (eq *encoding-utf8n*
       (detect-char-encoding (convert-encoding-from-internal *encoding-utf8n* "初版"))))
  => t
  => t)

(deftest detect-char-encoding-euc-jp ()
  (eq *encoding-euc-jp*
      (detect-char-encoding (convert-encoding-from-internal *encoding-euc-jp* "聞き出す")))
  => t)

(deftest detect-char-encoding-jis ()
  (eq *encoding-jis*
      (detect-char-encoding (convert-encoding-from-internal *encoding-jis* "聞き出す")))
  => t)


(deftest guess-char-encoding-ascii ()
  (let ((guess (guess-char-encoding "abc 123")))
    (values (length guess)
            (mapcar #'cdr guess)
            (mapcar #'char-encoding-name (mapcar #'car guess))))
  => 4
  => (1.0d0 1.0d0 1.0d0 1.0d0)
  => ("utf8n" "sjis" "euc-jp" "big5"))
