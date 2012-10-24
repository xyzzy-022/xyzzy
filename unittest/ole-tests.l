(deftest ole-method-null-and-empty ()
  "ole-method‚ÅNull’l‚ÆEmpty’l‚ğ“n‚·"
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
