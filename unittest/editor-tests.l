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

(defmacro with-java-mode-bed (&body body)
  `(with-temp-buffer
     (java-mode)
     (make-local-variable 'java-indent-level)
     (setq java-indent-level 2)
     (make-local-variable 'java-continued-statement-offset)
     (setq java-continued-statement-offset 2)
     (make-local-variable 'java-argdecl-indent)
     (setq java-argdecl-indent 5)
     (make-local-variable 'java-brace-offset)
     (setq java-brace-offset 0)
     (make-local-variable 'java-brace-imaginary-offset)
     (setq java-brace-imaginary-offset 0)
     (make-local-variable 'java-label-offset)
     (setq java-label-offset -2)
     (make-local-variable 'java-comment-indent)
     (setq java-comment-indent 2)
     (make-local-variable 'java-preprocessor-offset)
     (setq java-preprocessor-offset nil)
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


(deftest csharp-mode-enum-indent ()
  (with-csharp-mode-bed
    (values
     (c-indent-test "enum Color :")
     (c-indent-test   "byte {")
     (c-indent-test   "Red,")
     (c-indent-test   "Green,")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 2
  => 0)

(deftest csharp-mode-class-indent ()
  (with-csharp-mode-bed
    (values
     (c-indent-test "class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest csharp-mode-struct-indent ()
  (with-csharp-mode-bed
    (values
     (c-indent-test "struct Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest csharp-attribute-indent ()
  (with-csharp-mode-bed
    (values
     (c-indent-test "namespace Foo {")
     (c-indent-test   "[Serializable]")
     (c-indent-test   "class Foo {")
     (c-indent-test     "[Bar]")
     (c-indent-test     "int foo;")
     (c-indent-test     "[Bar]")
     (c-indent-test     "int Bar { get; set; }")
     (c-indent-test     "[Bar]")
     (c-indent-test     "[Baz]")
     (c-indent-test     "int foo() {")
     (c-indent-test       "// baz")
     (c-indent-test     "}")
     (c-indent-test   "}")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 4
  => 4
  => 4
  => 4
  => 4
  => 4
  => 4
  => 6
  => 4
  => 2
  => 0)


(deftest c++-mode-enum-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "enum Color :")
     (c-indent-test   "byte {")
     (c-indent-test   "Red,")
     (c-indent-test   "Green,")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 2
  => 0)

(deftest c++-mode-enum-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "enum class Color :")
     (c-indent-test   "byte {")
     (c-indent-test   "Red,")
     (c-indent-test   "Green,")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 2
  => 0)

(deftest c++-mode-enum-struct-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "enum struct Color :")
     (c-indent-test   "byte {")
     (c-indent-test   "Red,")
     (c-indent-test   "Green,")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 2
  => 0)

(deftest c++-mode-struct-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "struct Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++-mode-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-ref-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "ref class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-value-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "value class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-interface-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "interface class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-public-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "public class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-private-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "private class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-private-ref-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "private ref class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-public-interface-class-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "public interface class Foo :")
     (c-indent-test   "Bar {")
     (c-indent-test "}")))
  => 0
  => 2
  => 0)

(deftest c++/cli-attribute-indent ()
  (with-c++-mode-bed
    (values
     (c-indent-test "namespace Foo {")
     (c-indent-test   "[Serializable]")
     (c-indent-test   "class Foo {")
     (c-indent-test     "[Bar]")
     (c-indent-test     "int foo;")
     (c-indent-test     "[Bar]")
     (c-indent-test     "int bar(){}")
     (c-indent-test     "[Bar]")
     (c-indent-test     "[Baz]")
     (c-indent-test     "int baz(){}")
     (c-indent-test   "public:")
     (c-indent-test     "[Bar]")
     (c-indent-test     "[Baz]")
     (c-indent-test     "int foo2() {")
     (c-indent-test       "// baz")
     (c-indent-test     "}")
     (c-indent-test     "[Bar]")
     (c-indent-test     "[Baz]")
     (c-indent-test     "int bar2() {")
     (c-indent-test       "// baz")
     (c-indent-test     "}")
     (c-indent-test   "}")
     (c-indent-test "}")))
  => 0
  => 2
  => 2
  => 4
  => 4
  => 4
  => 4
  => 4
  => 4
  => 4
  => 2
  => 4
  => 4
  => 4
  => 6
  => 4
  => 4
  => 4
  => 4
  => 6
  => 4
  => 2
  => 0)

(deftest c++/cli-attribute-indent-2 ()
  (with-c++-mode-bed
   (values
    (c-indent-test "namespace Foo {")
    (c-indent-test   "[Serializable]")
    (c-indent-test   "class Foo {")
    (c-indent-test     "[Bar]")
    (c-indent-test     "int foo;")
    (c-indent-test     "[Bar]")
    (c-indent-test     "int foo;")))
  => 0
  => 2
  => 2
  => 4
  => 4
  => 4
  => 4)


(deftest java-annotation-indent-1 ()
  (with-java-mode-bed
   (values
    (c-indent-test "@Serializable")
    (c-indent-test "class Foo {")
    (c-indent-test   "@Test")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test     "// hoge")
    (c-indent-test   "}")
    (c-indent-test   "@Test()")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test   "}")
    (c-indent-test   "@Test({hoge = 1})")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test   "}")))
  => 0
  => 0
  => 2
  => 2
  => 4
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2)

(deftest java-annotation-indent-2 ()
  (with-java-mode-bed
   (values
    (c-indent-test "@Serializable")
    (c-indent-test "class Foo {")
    (c-indent-test   "@SuppressWarnings")
    (c-indent-test   "@Test")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test     "// hoge")
    (c-indent-test   "}")
    (c-indent-test   "@SuppressWarnings")
    (c-indent-test   "@Test()")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test   "}")
    (c-indent-test   "@SuppressWarnings")
    (c-indent-test   "@Test({hoge = 1})")
    (c-indent-test   "public int testFoo() {")
    (c-indent-test   "}")))
  => 0
  => 0
  => 2
  => 2
  => 2
  => 4
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2
  => 2)


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


(deftest chdir-0 ()
  (chdir (get-windows-directory))
  (values
   (chdir)
   (path-equal (user-homedir-pathname) (cwd)))
  => t
  => t)

(deftest chdir-1 ()
  (chdir (get-windows-directory))
  (values
   (chdir (si:system-root))
   (path-equal (si:system-root) (cwd)))
  => t
  => t)

(deftest chdir-2 ()
  (chdir (si:system-root))
  (values
   (chdir (get-windows-directory))
   (path-equal (get-windows-directory) (cwd)))
  => t
  => t)

(deftest chdir-3 ()
  "sysdep.curdir も変更されているか確認"
  (chdir (get-windows-directory))
  (values
   (chdir (get-system-directory))
   (progn
     ;; get-disk-usage は一時的にディレクトリを変更し、最後に sysdep.curdir に移動する
     (get-disk-usage (get-system-directory))
     (path-equal (get-system-directory) (cwd))))
  => t
  => t)

(deftest chdir-4 ()
  "Qdefault_dir も変更されているか確認"
  (chdir (get-windows-directory))
  (values
   (chdir (get-system-directory))
   (progn
     ;; call-process は一時的にディレクトリを変更し、最後に Qdefault_dir に移動する
     (call-process "cmd /c dir" :exec-directory (get-system-directory) :show :hide)
     (path-equal (get-system-directory) (cwd))))
  => t
  => t)

(deftest chdir-5 ()
  (chdir (get-system-directory))
  (values
   (chdir (get-windows-directory))
   (chdir (file-namestring (remove-trail-slash (get-system-directory))))
   (path-equal (get-system-directory) (cwd)))
  => t
  => t
  => t)

(deftest chdir-6 ()
  (chdir (get-windows-directory))
  (values
   (chdir (get-system-directory))
   (chdir "..")
   (chdir "..")
   (chdir ".."))
  => t
  => t
  => t
  => nil)

(deftest chdir-file-exists ()
  (with-temp-files (tmp)
    (chdir tmp))
  !! file-error)

(deftest chdir-path-not-found ()
  (chdir (format nil "path/not/found/~A" (gensym)))
  !! path-not-found)

(deftest kill-xyzzy-exit-code ()
  (flet ((run-xyzzy-and-kill (&optional (exit-code nil sp))
           (call-process (format nil "~A -e \"(kill-xyzzy ~A)\""
                                 (merge-pathnames "xyzzy.exe" (si:system-root))
                                 (if sp exit-code ""))
                         :show :minimize
                         :wait t)))
    (values
     (run-xyzzy-and-kill)
     (run-xyzzy-and-kill t)
     (run-xyzzy-and-kill nil)
     (run-xyzzy-and-kill 123)))
  => 0
  => 0
  => 1
  => 123)

(deftest kill-xyzzy-exit-code-error-float ()
  (kill-xyzzy 1.2)
  !! type-error)

(deftest kill-xyzzy-exit-code-error-symbol ()
  (kill-xyzzy 'foo)
  !! type-error)

(deftest kill-xyzzy-exit-code-error-string ()
  (kill-xyzzy "foo")
  !! type-error)


(deftest find-buffer-package ()
  (let ((ed-package (find-package :editor))
        (*find-buffer-package-hook* nil))
    (values
     (let ((*buffer-package* "editor"))
       (declare (special *buffer-package*))
       (eql ed-package (ed::find-buffer-package)))
     (let ((*buffer-package* "no such package"))
       (declare (special *buffer-package*))
       (eql *package* (ed::find-buffer-package)))
     (let ((*buffer-package* nil))
       (declare (special *buffer-package*))
       (eql *package* (ed::find-buffer-package)))
     (eql *package* (ed::find-buffer-package))
     ))
  => t
  => t
  => t
  => t)

(deftest find-buffer-package-hook ()
  (let ((*package* (find-package :user))
        (*buffer-package* nil))
    (declare (special *buffer-package*))
    (values
     (let ((*find-buffer-package-hook* #'(lambda () (error "hoge"))))
       (package-name (ed::find-buffer-package)))
     (let ((*find-buffer-package-hook* #'(lambda () (find-package :editor))))
       (package-name (ed::find-buffer-package)))
     (let ((*find-buffer-package-hook* #'(lambda () "editor")))
       (package-name (ed::find-buffer-package)))
     (let ((*find-buffer-package-hook* #'(lambda () "EdItOr")))
       (package-name (ed::find-buffer-package)))
     (let ((*find-buffer-package-hook* #'(lambda () :editor)))
       (package-name (ed::find-buffer-package)))
     ))
  => "user"
  => "editor"
  => "editor"
  => "editor"
  => "editor")

(deftest find-buffer-package-hook-list ()
  (let ((*package* (find-package :user))
        (*buffer-package* nil))
    (declare (special *buffer-package*))
    (values
     (let ((ed:*find-buffer-package-hook* (list #'(lambda () (find-package :editor))
                                                #'(lambda () (error "hoge")))))
       (package-name (ed::find-buffer-package)))
     (let ((ed:*find-buffer-package-hook* (list #'(lambda () nil)
                                                #'(lambda () (find-package :editor)))))
       (package-name (ed::find-buffer-package)))
     (let ((ed:*find-buffer-package-hook* (list #'(lambda () (error "hoge"))
                                                #'(lambda () (find-package :editor)))))
       (package-name (ed::find-buffer-package)))
     ))
  => "editor"
  => "editor"
  => "user")

(deftest lisp-search-in-package ()
  (let ((*package* (find-package :user))
        (*buffer-package* nil)
        (*find-buffer-package-hook* '(ed::lisp-search-in-package)))
    (declare (special *buffer-package*))
    (with-temp-buffer
      (insert "(in-package :editor)" #\LFD)
      (insert "(in-package   \"user\" )" #\LFD)
      (insert "(lisp:in-package \"cl-user\")" #\LFD)
      (insert "(common-lisp::in-package \"lisp\")" #\LFD)
      (insert "(common-lisp::in-package \"no-such-package\")" #\LFD)
      (flet ((test (n)
               (goto-line n)
               (goto-bol)
               (package-name (ed::find-buffer-package))))
        (values
         (test 1)
         (test 2)
         (test 3)
         (test 4)
         (test 5)
         ))))
  => "editor"
  => "user"
  => "common-lisp-user"
  => "lisp"
  => "user")

(require "foreign")
(deftest lisp-mode-eval-last-sexp (:compare equalp)
  (with-temp-buffer
    (flet ((insert-and-eval (sexp)
             (insert sexp #\LFD)
             (eval-last-sexp)))
      (values
       (handler-case
           (insert-and-eval "1\n>")
         (error (c)
           (si:*condition-string c)))
       (handler-case
           (insert-and-eval "1\n}")
         (error (c)
           (si:*condition-string c)))
       (insert-and-eval "(< 1 2)")
       (insert-and-eval "(string< 'a 'b)")
       (insert-and-eval "123")
       (insert-and-eval "#x123")
       (insert-and-eval "#o123")
       (insert-and-eval "#b111")
       (insert-and-eval "(list 1 2)")
       (insert-and-eval "#.(list 1 2)")
       (insert-and-eval "#(1 2)")
       (insert-and-eval "#3(1 2)")
       (insert-and-eval "#2A((1))")
       (insert-and-eval "#3A(((1)))")
       (insert-and-eval "#C(1 2)")
       (insert-and-eval "#S(foreign::c-type-definition type :int32 size 4 slots nil accessor system:unpack-int32)")
       (handler-case
           (insert-and-eval "#<function: car>")
         (reader-error (c)
           (list (format nil "~A" (reader-error-datum c)) (reader-error-arguments c))))
       (progn
         (insert-and-eval "(load-library \"ole\")")
         (insert-and-eval "(setf fso (ole-create-object \"Scripting.FileSystemObject\"))")
         (insert-and-eval "#{fso.BuildPath[\"foo\" \"bar\"]}"))
       )))
  => "変数が定義されていません: >"
  => "変数が定義されていません: }"
  => t
  => 0
  => 123
  => #x123
  => #o123
  => #b111
  => (1 2)
  => (1 2)
  => #(1 2)
  => #3(1 2)
  => #2A((1))
  => #3A(((1)))
  => #C(1 2)
  => #S(foreign::c-type-definition type :int32 size 4 slots nil accessor system:unpack-int32)
  => ("ディスパッチングマクロ副文字ではありません" (#\<))
  => "foo\\bar")

(deftest lisp-tags-find-target ()
  (with-temp-buffer
    (flet ((insert-and-find-target (str)
             (goto-char (point-max))
             (insert #\LFD str)
             (backward-char)
             (multiple-value-bind (class name functionp)
                 (ed::lisp-tags-find-target)
               name)))
      (values
       (insert-and-find-target "si:getenv")
       (insert-and-find-target "si::getenv")
       (insert-and-find-target "getenv"))))
  => "getenv"
  => "getenv"
  => "getenv")

(deftest xyzzy-ini-path ()
  (values
   (si:*builtin-function-p #'xyzzy-ini-path)
   (path-equal (merge-pathnames "xyzzy.ini" (user-config-path))
               (xyzzy-ini-path)))
  => t
  => t)

(deftest xyzzy-ini-path-startup-option ()
  (with-temp-files (ini-path)
    (path-equal ini-path
                (eval-in-another-xyzzy
                 '(xyzzy-ini-path)
                 :options (format nil "-ini \"~A\" -q" ini-path))))
  => t)

(deftest xyzzy-ini-path-XYZZYINIFILE ()
  (with-temp-files (ini-path)
    (path-equal ini-path
                (eval-in-another-xyzzy
                 '(xyzzy-ini-path)
                 :options "-q"
                 :environ `(("XYZZYINIFILE" . ,ini-path)))))
  => t)

(deftest xyzzy-ini-path-startup-option-and-XYZZYINIFILE ()
  (with-temp-files (ini-path1 ini-path2)
    (path-equal ini-path1
                (eval-in-another-xyzzy
                 '(xyzzy-ini-path)
                 :options (format nil "-ini \"~A\" -q" ini-path1)
                 :environ `(("XYZZYINIFILE" . ,ini-path2)))))
  => t)


(deftest user-config-path-startup-option ()
  (with-temp-dirs (config-path)
    (values
     (path-equal config-path
                 (eval-in-another-xyzzy
                  '(user-config-path)
                  :options (format nil "-config \"~A\" -q" config-path)))
     (delete-file (merge-pathnames "xyzzy.ini" config-path))))
  => t
  => t)

(deftest user-config-path-XYZZYCONFIGPATH ()
  (with-temp-dirs (config-path)
    (values
     (path-equal config-path
                 (eval-in-another-xyzzy
                  '(user-config-path)
                  :options "-q"
                  :environ `(("XYZZYCONFIGPATH" . ,config-path))))
     (delete-file (merge-pathnames "xyzzy.ini" config-path))))
  => t
  => t)

(deftest user-config-path-startup-option-and-XYZZYCONFIGPATH ()
  (with-temp-dirs (config-path1 config-path2)
    (values
     (path-equal config-path1
                 (eval-in-another-xyzzy
                  '(user-config-path)
                  :options (format nil "-config \"~A\" -q" config-path1)
                  :environ `(("XYZZYCONFIGPATH" . ,config-path2))))
     (delete-file (merge-pathnames "xyzzy.ini" config-path1))))
  => t
  => t)


(deftest image-startup-option ()
  (with-temp-files (image-path)
    (path-equal image-path
                (eval-in-another-xyzzy
                 '(si:dump-image-path)
                 :options (format nil "-image \"~A\" -q" image-path))))
  => t)


(deftest lisp-mode-indent-region-first-line ()
  (with-temp-buffer
    (insert "  (defvar foo 1)" #\LFD)
    (insert "  (defvar bar 2)" #\LFD)
    (lisp-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max)))
  => "(defvar foo 1)
(defvar bar 2)
")

(deftest c-mode-indent-region-first-line ()
  (with-temp-buffer
    (insert "  #define FOO 1" #\LFD)
    (insert "  #define BAR 2" #\LFD)
    (c-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max)))
  => "#define FOO 1
#define BAR 2
")

(deftest lisp-mode-save-buffer-package-1 ()
  (with-temp-buffer
    (setq *buffer-package* (find-package :editor))
    (lisp-mode)
    (setq *buffer-package* (find-package :system))
    (lisp-mode)
    (package-name *buffer-package*))
  => "system")

(deftest lisp-mode-save-buffer-package-2 ()
  (with-temp-buffer
    (setq *buffer-package* (find-package :editor))
    (lisp-mode)
    (setq *buffer-package* (find-package :system))
    (lisp-interaction-mode)
    (lisp-mode)
    (package-name *buffer-package*))
  => "system")

(deftest lisp-interaction-mode-save-buffer-package-1 ()
  (with-temp-buffer
    (setq *buffer-package* (find-package :editor))
    (lisp-interaction-mode)
    (setq *buffer-package* (find-package :system))
    (lisp-interaction-mode)
    (package-name *buffer-package*))
  => "system")

(deftest lisp-interaction-mode-save-buffer-package-2 ()
  (with-temp-buffer
    (setq *buffer-package* (find-package :editor))
    (lisp-interaction-mode)
    (setq *buffer-package* (find-package :system))
    (lisp-mode)
    (lisp-interaction-mode)
    (package-name *buffer-package*))
  => "system")

(deftest text-mode-does-not-save-buffer-package ()
  (with-temp-buffer
    (setq *buffer-package* (find-package :editor))
    (text-mode)
    (lisp-mode)
    *buffer-package*)
  => nil)

(deftest process-command-line--l ()
  (eval-in-another-xyzzy
   `(modulep "foreign")
   :options "-llisp/foreign.l")
  => non-nil)

(deftest process-command-line--load ()
  (eval-in-another-xyzzy
   `(modulep "foreign")
   :options "-load lisp/foreign.l")
  => non-nil)

(deftest process-command-line--I ()
  (eval-in-another-xyzzy
   `(member (merge-pathnames "unittest" (si:system-root)) *load-path*
            :test #'path-equal)
   :options "-Iunittest")
  => non-nil)


(deftest process-command-line--load-path ()
  (eval-in-another-xyzzy
   `(member (merge-pathnames "unittest" (si:system-root)) *load-path*
            :test #'path-equal)
   :options "-load-path unittest")
  => non-nil)

(deftest process-command-line--R ()
  (eval-in-another-xyzzy
   `(modulep "foreign")
   :options "-Rforeign")
  => non-nil)

(deftest process-command-line--require ()
  (eval-in-another-xyzzy
   `(modulep "foreign")
   :options "-require foreign")
  => non-nil)

(deftest process-command-line--work-dir ()
  (eval-in-another-xyzzy
   `(path-equal (cwd) (merge-pathnames "unittest" (si:system-root)))
   :options "-work-dir unittest")
  => t)

(deftest process-command-line--f ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-fview-mode")
  => t)

(deftest process-command-line--funcall ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-fview-mode")
  => t)

(deftest process-command-line--e ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-e(view-mode)")
  => t)

(deftest process-command-line--eval ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-eval (view-mode)")
  => t)

(deftest process-command-line--g ()
  (eval-in-another-xyzzy
   `(current-line-number)
   :options "unittest/editor-tests.l -g10")
  => 10)

(deftest process-command-line--go ()
  (eval-in-another-xyzzy
   `(current-line-number)
   :options "unittest/editor-tests.l -go 20")
  => 20)

(deftest process-command-line--c ()
  (eval-in-another-xyzzy
   `(current-column)
   :options "unittest/editor-tests.l -c10")
  => 10)

(deftest process-command-line--column ()
  (eval-in-another-xyzzy
   `(current-column)
   :options "unittest/editor-tests.l -column 20")
  => 20)

(deftest process-command-line--trace ()
  (eval-in-another-xyzzy
   `(not (null (find-buffer "*Trace Output*")))
   :options "-trace")
  => t)

(deftest process-command-line--kill ()
  (call-process (format nil "~A -kill -e \"(kill-xyzzy nil)\""
                        (merge-pathnames "xyzzy.exe" (si:system-root)))
                :show :minimize
                :wait t)
  => 0)

(deftest process-command-line--mailto ()
  (eval-in-another-xyzzy
   `(progn *test-result*)
   :options (format nil "-e \"~S\" -mailto mail@example.com"
                    '(setq *command-line-mailto-hook*
                      (lambda (arg)
                        (setq *test-result* arg)))))
  => "mail@example.com")

(deftest process-command-line--m ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-mview-mode unittest/editor-tests.l")
  => t)

(deftest process-command-line--mode ()
  (eval-in-another-xyzzy
   `(equal buffer-mode 'view-mode)
   :options "-mode view-mode unittest/editor-tests.l")
  => t)

(deftest process-command-line--p ()
  (eval-in-another-xyzzy
   `(progn *test-result*)
   :options (format nil "-e \"~S\" -punittest/editor-tests.l"
                    '(progn
                      (setq *print-option-show-dialog* t)
                      (defun print-selected-buffer-dialog ()
                        (setq *test-result* t)))))
  => t)

(deftest process-command-line--s ()
  (eval-in-another-xyzzy
   `(values
     (string= *test-result* (merge-pathnames "hoge.ssn" (si:system-root)))
     ed::*auto-save-session-file*)
   :options (format nil "-e \"~S\" -shoge.ssn"
                    '(defun load-session (file)
                      (setq *test-result* file))))
  => t
  => t)

(deftest process-command-line--S ()
  (eval-in-another-xyzzy
   `(values
     (string= *test-result* (merge-pathnames "hoge.ssn" (si:system-root)))
     ed::*auto-save-session-file*)
   :options (format nil "-e \"~S\" -Shoge.ssn"
                    '(defun load-session (file)
                      (setq *test-result* file))))
  => t
  => nil)

(deftest process-command-line--ro ()
  (eval-in-another-xyzzy
   `(progn buffer-read-only)
   :options "-ro unittest/editor-tests.l")
  => t)

(deftest process-command-line--rw ()
  (eval-in-another-xyzzy
   `(progn buffer-read-only)
   :options "-rw unittest/editor-tests.l")
  => nil)


(deftest char-encoding-type ()
  (values-list
   (mapcar #'(lambda (e)
               (list (char-encoding-name e)
                     (char-encoding-type e)))
           (list *encoding-auto*
                 *encoding-sjis*
                 *encoding-euc-jp*
                 *encoding-iso-2022-jp-2*
                 *encoding-big5*
                 *encoding-binary*
                 *encoding-utf5*
                 *encoding-utf7*
                 *encoding-utf8*
                 *encoding-utf8n*
                 *encoding-utf16*
                 *encoding-utf16le-bom*
                 *encoding-utf16be-bom*
                 *encoding-utf16be-big5*
                 *encoding-std-utf5*
                 *encoding-std-utf7*
                 *encoding-std-utf8*
                 *encoding-std-utf8n*
                 *encoding-std-utf16be*
                 *encoding-std-utf16le*
                 *encoding-std-utf16be-bom*
                 *encoding-std-utf16le-bom*
                 *encoding-windows-latin1*
                 *encoding-windows-turkish*
                 )))
  => ("auto" :auto-detect)
  => ("sjis" :sjis)
  => ("euc-jp" :iso2022)
  => ("iso2022jp2" :iso2022)
  => ("big5" :big5)
  => ("bin" :binary)
  => ("utf5" :utf5)
  => ("utf7" :utf7)
  => ("utf8" :utf8)
  => ("utf8n" :utf8)
  => ("utf16" :utf16)
  => ("utf16le-bom" :utf16)
  => ("utf16be-bom" :utf16)
  => ("utf16be-big5" :utf16)
  => ("std-utf5" :utf5)
  => ("std-utf7" :utf7)
  => ("std-utf8" :utf8)
  => ("std-utf8n" :utf8)
  => ("std-utf16be" :utf16)
  => ("std-utf16le" :utf16)
  => ("std-utf16be-bom" :utf16)
  => ("std-utf16le-bom" :utf16)
  => ("win-latin1" :windows-codepage)
  => ("win-turkish" :windows-codepage))

(deftest char-encoding-type-all ()
  (mapcar #'char-encoding-type *char-encoding-list*)
  => non-nil)

(deftest char-encoding-type-error-1 ()
  (char-encoding-type nil)
  !! type-error)

(deftest char-encoding-type-error-2 ()
  (char-encoding-type "utf-8")
  !! type-error)

(deftest char-encoding-type-error-3 ()
  (char-encoding-type 123)
  !! type-error)

(deftest char-encoding-signature ()
  (values-list
   (mapcar #'(lambda (e)
               (list (char-encoding-name e)
                     (char-encoding-signature e)))
           (list *encoding-auto*
                 *encoding-sjis*
                 *encoding-euc-jp*
                 *encoding-iso-2022-jp-2*
                 *encoding-big5*
                 *encoding-binary*
                 *encoding-utf5*
                 *encoding-utf7*
                 *encoding-utf8*
                 *encoding-utf8n*
                 *encoding-utf16*
                 *encoding-utf16le-bom*
                 *encoding-utf16be-bom*
                 *encoding-utf16be-big5*
                 *encoding-std-utf5*
                 *encoding-std-utf7*
                 *encoding-std-utf8*
                 *encoding-std-utf8n*
                 *encoding-std-utf16be*
                 *encoding-std-utf16le*
                 *encoding-std-utf16be-bom*
                 *encoding-std-utf16le-bom*
                 *encoding-windows-latin1*
                 *encoding-windows-turkish*
                 )))
  => ("auto" nil)
  => ("sjis" nil)
  => ("euc-jp" nil)
  => ("iso2022jp2" nil)
  => ("big5" nil)
  => ("bin" nil)
  => ("utf5" nil)
  => ("utf7" nil)
  => ("utf8" t)
  => ("utf8n" nil)
  => ("utf16" t)
  => ("utf16le-bom" t)
  => ("utf16be-bom" t)
  => ("utf16be-big5" nil)
  => ("std-utf5" nil)
  => ("std-utf7" nil)
  => ("std-utf8" t)
  => ("std-utf8n" nil)
  => ("std-utf16be" nil)
  => ("std-utf16le" nil)
  => ("std-utf16be-bom" t)
  => ("std-utf16le-bom" t)
  => ("win-latin1" nil)
  => ("win-turkish" nil))

(deftest char-encoding-signature-all ()
  (mapcar #'char-encoding-signature *char-encoding-list*)
  => non-nil)

(deftest char-encoding-signature-error-1 ()
  (char-encoding-signature nil)
  !! type-error)

(deftest char-encoding-signature-error-2 ()
  (char-encoding-signature "utf-8")
  !! type-error)

(deftest char-encoding-signature-error-3 ()
  (char-encoding-signature 123)
  !! type-error)


(defun find-file-auto-encoding-test0 (magic-comment encoding)
  (with-temp-files (file)
    (with-temp-buffer
      (insert magic-comment #\LFD)
      (insert "abc" #\LFD)
      (write-file file t nil encoding))
    (let ((b (ed::find-file-internal file)))
      (char-encoding-name (buffer-fileio-encoding b)))))

(defun find-file-auto-encoding-test (param encoding)
  (find-file-auto-encoding-test0 (format nil "; -*- encoding: ~A -*-" param)
                                 encoding))

(deftest find-file-auto-encoding-use-utf8n ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-encoding-test "utf-8" *encoding-utf8*)
     (find-file-auto-encoding-test "utf-8" *encoding-utf8n*)))
  => "utf8"
  => "utf8n")

(deftest find-file-auto-encoding-use-utf8n-wrong-encoding ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-encoding-test "utf-8" *encoding-sjis*)
     (find-file-auto-encoding-test "utf-8" *encoding-euc-jp*)))
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-encoding-use-utf8n-disable ()
  (let ((*find-file-auto-encoding-use-utf8n* nil))
    (values
     (find-file-auto-encoding-test "utf-8" *encoding-utf8*)
     (find-file-auto-encoding-test "utf-8" *encoding-utf8n*)))
  => "utf8"
  => "utf8")

(deftest find-file-auto-encoding-use-utf8n-changed ()
  (setf (gethash "utf-8" *mime-charset-name-hash-table*)
        *encoding-utf8n*)
  (unwind-protect
      (let ((*find-file-auto-encoding-use-utf8n* t))
        (values
         (find-file-auto-encoding-test "utf-8" *encoding-utf8*)
         (find-file-auto-encoding-test "utf-8" *encoding-utf8n*)))
    (setf (gethash "utf-8" *mime-charset-name-hash-table*)
          *encoding-utf8*))
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-encoding-use-utf8n-disable-changed ()
  (setf (gethash "utf-8" *mime-charset-name-hash-table*)
        *encoding-utf8n*)
  (unwind-protect
      (let ((*find-file-auto-encoding-use-utf8n* nil))
        (values
         (find-file-auto-encoding-test "utf-8" *encoding-utf8*)
         (find-file-auto-encoding-test "utf-8" *encoding-utf8n*)))
    (setf (gethash "utf-8" *mime-charset-name-hash-table*)
          *encoding-utf8*))
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-encoding-params ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-encoding-test0 ";; -*- Encoding: UTF-8 -*-" *encoding-utf8n*)
     (find-file-auto-encoding-test0 "#-*-Encoding:UTF-8-*-" *encoding-utf8n*)
     (find-file-auto-encoding-test0 ";; -*- coding: utf-8 -*-" *encoding-utf8n*)
     (find-file-auto-encoding-test0 "#-*-coding:utf-8-*-" *encoding-utf8n*)
     ))
  => "utf8n"
  => "utf8n"
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-encoding-params-not-supported ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-encoding-test0 "# vim:fileencoding=utf-8" *encoding-utf8n*)
     (find-file-auto-encoding-test0 "# vim:set fileencoding=utf-8 :" *encoding-utf8n*)
     (find-file-auto-encoding-test0 "#coding:utf-8" *encoding-utf8n*)
     ))
  => "sjis"
  => "sjis"
  => "sjis")


(defun find-file-auto-xml-encoding-test (param encoding)
  (find-file-auto-encoding-test0 (format nil "<?xml version=\"1.0\" encoding=\"~A\"?>" param)
                                 encoding))

(deftest find-file-auto-xml-encoding-use-utf8n ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8n*)))
  => "utf8"
  => "utf8n")

(deftest find-file-auto-xml-encoding-use-utf8n-wrong-encoding ()
  (let ((*find-file-auto-encoding-use-utf8n* t))
    (values
     (find-file-auto-xml-encoding-test "utf-8" *encoding-sjis*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-euc-jp*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf16*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf16be*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf16le*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf16be-bom*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf16le-bom*)
     ))
  => "utf8n"
  => "utf8n"
  => "utf8n"
  => "utf8n"
  => "utf8n"
  => "utf8n"
  => "utf8n")


(deftest find-file-auto-xml-encoding-use-utf8n-disable ()
  (let ((*find-file-auto-encoding-use-utf8n* nil))
    (values
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8*)
     (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8n*)))
  => "utf8"
  => "utf8")

(deftest find-file-auto-xml-encoding-use-utf8n-changed ()
  (setf (gethash "utf-8" *mime-charset-name-hash-table*)
        *encoding-utf8n*)
  (unwind-protect
      (let ((*find-file-auto-encoding-use-utf8n* t))
        (values
         (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8*)
         (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8n*)))
    (setf (gethash "utf-8" *mime-charset-name-hash-table*)
          *encoding-utf8*))
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-xml-encoding-use-utf8n-disable-changed ()
  (setf (gethash "utf-8" *mime-charset-name-hash-table*)
        *encoding-utf8n*)
  (unwind-protect
      (let ((*find-file-auto-encoding-use-utf8n* nil))
        (values
         (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8*)
         (find-file-auto-xml-encoding-test "utf-8" *encoding-utf8n*)))
    (setf (gethash "utf-8" *mime-charset-name-hash-table*)
          *encoding-utf8*))
  => "utf8n"
  => "utf8n")

(deftest find-file-auto-xml-encoding-use-UTF-16 ()
  (values
   (find-file-auto-xml-encoding-test "UTF-16" *encoding-sjis*)
   (find-file-auto-xml-encoding-test "UTf-16" *encoding-euc-jp*)
   (find-file-auto-xml-encoding-test "Utf-16" *encoding-utf8*)
   (find-file-auto-xml-encoding-test "utf-16" *encoding-utf16*)
   (find-file-auto-xml-encoding-test "utf-16" *encoding-utf16le*)
   (find-file-auto-xml-encoding-test "utf-16" *encoding-utf16be*)
   (find-file-auto-xml-encoding-test "utf-16" *encoding-utf16le-bom*)
   (find-file-auto-xml-encoding-test "utf-16" *encoding-utf16be-bom*))
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le"
  => "utf16be"
  => "utf16le-bom"
  => "utf16be-bom")

(deftest find-file-auto-xml-encoding-use-ISO-10646-UCS-2 ()
  (values
   (find-file-auto-xml-encoding-test "ISO-10646-UCS-2" *encoding-sjis*)
   (find-file-auto-xml-encoding-test "ISO-10646-ucs-2" *encoding-euc-jp*)
   (find-file-auto-xml-encoding-test "iso-10646-UCS-2" *encoding-utf8*)
   (find-file-auto-xml-encoding-test "iso-10646-ucs-2" *encoding-utf16*)
   (find-file-auto-xml-encoding-test "iso-10646-ucs-2" *encoding-utf16le*)
   (find-file-auto-xml-encoding-test "iso-10646-ucs-2" *encoding-utf16be*)
   (find-file-auto-xml-encoding-test "iso-10646-ucs-2" *encoding-utf16le-bom*)
   (find-file-auto-xml-encoding-test "iso-10646-ucs-2" *encoding-utf16be-bom*))
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le-bom"
  => "utf16le"
  => "utf16be"
  => "utf16le-bom"
  => "utf16be-bom")


(defun find-file-detect-xml-encoding-test (magic-comment)
  (with-temp-files (file)
    (with-temp-buffer
      (insert magic-comment #\LFD)
      (insert "abc" #\LFD)
      (ed::find-file-detect-xml-encoding)
      )))

(deftest test-find-file-scan-xml-declarations ()
  (values
   (find-file-detect-xml-encoding-test "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
   (find-file-detect-xml-encoding-test "<?xml version = \"1.0\" encoding = \"UTF-8\" ?>")
   (find-file-detect-xml-encoding-test "<?xml encoding=\"UTF-8\" version=\"1.0\"?>")
   (find-file-detect-xml-encoding-test "<?xml encoding=\"UTF-8\"?>")
   (find-file-detect-xml-encoding-test "<?xml encoding =\"UTF-8\"?>")
   (find-file-detect-xml-encoding-test "<?xml encoding = \"UTF-8\" ?>")
   (find-file-detect-xml-encoding-test "<?xml\tencoding\t=\t\"UTF-8\"\t?>")
   (find-file-detect-xml-encoding-test "<?xml\nencoding\n=\n\"UTF-8\"\n?>")
   )
  => "UTF-8"
  => "UTF-8"
  => "UTF-8"
  => "UTF-8"
  => "UTF-8"
  => "UTF-8"
  => "UTF-8"
  => "UTF-8")

(deftest test-find-file-scan-xml-declarations--invalid-declaration ()
  (values
   (find-file-detect-xml-encoding-test "xml encoding=\"UTF-8\"")
   (find-file-detect-xml-encoding-test "?xml encoding=\"UTF-8\"")
   (find-file-detect-xml-encoding-test "< ?xml encoding=\"UTF-8\"")
   (find-file-detect-xml-encoding-test "< ? xml encoding=\"UTF-8\"")
   (find-file-detect-xml-encoding-test "<?xml encoding=\"UTF-8\" ?")
   (find-file-detect-xml-encoding-test "<?xml encoding=\"UTF-8\" ? >")
   )
  => nil
  => nil
  => nil
  => nil
  => nil
  => nil)
