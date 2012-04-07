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

(defmacro with-c-mode-bed (&body body)
  `(with-temp-buffer
     (c-mode)
     (make-local-variable 'c-indent-level)
     (setq c-indent-level 2)
     (make-local-variable 'c-continued-statement-offset)
     (setq c-continued-statement-offset 2)
     (make-local-variable 'c-argdecl-indent)
     (setq c-argdecl-indent 5)
     (make-local-variable 'c-brace-offset)
     (setq c-brace-offset 0)
     (make-local-variable 'c-brace-imaginary-offset)
     (setq c-brace-imaginary-offset 0)
     (make-local-variable 'c-label-offset)
     (setq c-label-offset -2)
     (make-local-variable 'c-comment-indent)
     (setq c-comment-indent 2)
     (make-local-variable 'c-preprocessor-offset)
     (setq c-preprocessor-offset nil)
     ,@body))

(defmacro with-c++-mode-bed (&body body)
  `(with-temp-buffer
     (c++-mode)
     (make-local-variable 'c++-indent-level)
     (setq c++-indent-level 2)
     (make-local-variable 'c++-continued-statement-offset)
     (setq c++-continued-statement-offset 2)
     (make-local-variable 'c++-argdecl-indent)
     (setq c++-argdecl-indent 5)
     (make-local-variable 'c++-brace-offset)
     (setq c++-brace-offset 0)
     (make-local-variable 'c++-brace-imaginary-offset)
     (setq c++-brace-imaginary-offset 0)
     (make-local-variable 'c++-label-offset)
     (setq c++-label-offset -2)
     (make-local-variable 'c++-comment-indent)
     (setq c++-comment-indent 2)
     (make-local-variable 'c++-preprocessor-offset)
     (setq c++-preprocessor-offset nil)
     ,@body))

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
     (make-local-variable 'csharp-preprocessor-offset)
     (setq csharp-preprocessor-offset nil)
     ,@body))

(defun c-indent-test (line)
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
     (c-indent-test "#region MyClass")
     (c-indent-test "class MyClass {")
     (c-indent-test   "#region Public Methods")
     (c-indent-test   "#region MyMethod")
     (c-indent-test   "public void MyMethod() {")
     (c-indent-test     "#region MyStatement")
     (c-indent-test     "return;")
     (c-indent-test     "#endregion // MyStatement")
     (c-indent-test   "}")
     (c-indent-test   "#endregion // MyMethod")
     (c-indent-test   "#endregion // Public Methods")
     (c-indent-test "}")
     (c-indent-test "#endregion // MyClass")))
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
     (c-indent-test "class MyClass {")
     (c-indent-test "#undef DEBUG")
     (c-indent-test "#define DEBUG")
     (c-indent-test "#pragma warning disable 414, 3021")
     (c-indent-test   "# region MyMethod")
     (c-indent-test   "public int MyMethod() {")
     (c-indent-test "#if DEBUG")
     (c-indent-test     "#  region MyStatement")
     (c-indent-test "# define PI 3.14")
     (c-indent-test "# line 200")
     (c-indent-test     "return PI;")
     (c-indent-test "# line default")
     (c-indent-test     "#  endregion // MyStatement")
     (c-indent-test "#elif DEBUG2")
     (c-indent-test "# warn Not implemented yet.")
     (c-indent-test "#else")
     (c-indent-test "# error Not implemented yet.")
     (c-indent-test "#endif")
     (c-indent-test   "}")
     (c-indent-test   "# endregion // MyMethod")
     (c-indent-test "}")
     (c-indent-test "#endregion // MyClass")))
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
     (c-indent-test "class MyClass {")
     (c-indent-test   "public int MyMethod() {")
     (c-indent-test     "using (StreamReader outFile = new StreamReader(outputFile.OpenRead()))")
     (c-indent-test     "using (StreamReader expFile = new StreamReader(expectedFile.OpenRead())) {")
     (c-indent-test       "outFile.Write(expFile.ReadToEnd());")
     (c-indent-test     "}")
     (c-indent-test   "}")
     (c-indent-test "}")))
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
     (c-indent-test "namespace MyNS {")
     (c-indent-test   "using")
     (c-indent-test     "System.IO;")
     (c-indent-test "}")))
  => 0
  => 2
  => 4
  => 0)


(deftest csharp-preprocessor-offset-1 ()
  (with-csharp-mode-bed
    (setq csharp-preprocessor-offset 0)
    (values
     (c-indent-test "class MyClass {")
     (c-indent-test   "#define FOO")
     (c-indent-test   "public int MyMethod() {")
     (c-indent-test     "#define FOO")
     (c-indent-test     "using (StreamReader outFile = new StreamReader(outputFile.OpenRead()))")
     (c-indent-test     "using (StreamReader expFile = new StreamReader(expectedFile.OpenRead())) {")
     (c-indent-test       "#define FOO")
     (c-indent-test       "outFile.Write(expFile.ReadToEnd());")
     (c-indent-test     "}")
     (c-indent-test   "}")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 4
  => 4
  => 4
  => 6
  => 6
  => 4
  => 2
  => 0)

(deftest csharp-preprocessor-offset-2 ()
  (with-csharp-mode-bed
    (setq csharp-preprocessor-offset 2)
    (values
     (c-indent-test "class MyClass {")
     (c-indent-test     "#define FOO")
     (c-indent-test   "public int MyMethod() {")
     (c-indent-test       "#define FOO")
     (c-indent-test     "using (StreamReader outFile = new StreamReader(outputFile.OpenRead()))")
     (c-indent-test     "using (StreamReader expFile = new StreamReader(expectedFile.OpenRead())) {")
     (c-indent-test         "#define FOO")
     (c-indent-test       "outFile.Write(expFile.ReadToEnd());")
     (c-indent-test     "}")
     (c-indent-test   "}")
     (c-indent-test "}")))
  => 0
  => 4
  => 2
  => 6
  => 4
  => 4
  => 8
  => 6
  => 4
  => 2
  => 0)

(deftest c++-preprocessor-offset-1 ()
  (with-c++-mode-bed
    (setq c++-preprocessor-offset 0)
    (values
     (c-indent-test "class MyClass {")
     (c-indent-test   "#define FOO")
     (c-indent-test   "public int MyMethod() {")
     (c-indent-test     "#define FOO")
     (c-indent-test   "}")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 4
  => 2
  => 0)

(deftest c-preprocessor-offset-1 ()
  (with-c-mode-bed
    (setq c-preprocessor-offset 0)
    (values
     (c-indent-test "#define FOO")
     (c-indent-test "int main() {")
     (c-indent-test   "#define FOO")
     (c-indent-test "}")))
  => 0
  => 0
  => 2
  => 0)

(deftest c-preprocessor-offset-and-indent-cpp-directive ()
  (with-c-mode-bed
    (setq c-preprocessor-offset 0)
    (values
     (c-indent-test "int main() {")
     (c-indent-test   "#if FOO")
     (c-indent-test   "#if BAR")
     (c-indent-test   "#define BAZ")
     (c-indent-test   "#else")
     (c-indent-test   "#endif")
     (c-indent-test   "#else")
     (c-indent-test   "#endif")
     (c-indent-test "}")
     (progn
       (indent-cpp-directive)
       (buffer-substring (point-min) (point-max)))))
  => 0
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 0
  => "int main() {
  #if FOO
  # if BAR
  #  define BAZ
  # else
  # endif
  #else
  #endif
}
")


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
