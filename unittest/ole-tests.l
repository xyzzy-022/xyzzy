(deftest ole-method-null-and-empty ()
  "ole-method‚ÅNull’l‚ÆEmpty’l‚ð“n‚·"
  (unwind-protect
      (let ((sc (ole-create-object "ScriptControl")))
        (ole-putprop sc 'Language "JScript")
        (ole-method sc 'AddCode "function f (a) { return '' + a; }")
        (values
         (ole-method sc 'Run "f" :null)
         (ole-method sc 'Run "f" :empty)))
    (gc))
  => "null"
  => "undefined")

(deftest fix-ole-getmethod-immediate-array ()
  (let ((xhr (ole-create-object "MSXML2.XMLHTTP")))
    (ole-method xhr 'open "GET" "https://github.com/xyzzy-022/xyzzy/" nil) ;; blocking!
    (ole-method xhr 'send nil)
    (ole-method xhr 'responseBody))
  => non-nil)
