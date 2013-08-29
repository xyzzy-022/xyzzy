(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ole"))

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

(defmacro with-ie ((var) &body body)
  `(let ((,var (ole-create-object "InternetExplorer.Application")))
     (unwind-protect
         (progn ,@body)
       (ole-method ,var :Quit))))

(defmacro with-wbem ((var) &body body)
  `(let* ((#1=#:wbem (ole-create-object "WbemScripting.SWbemLocator"))
          (,var #{#1#.ConnectServer["."]}))
     (progn ,@body)))

(deftest test-ole-method*-args ()
  (with-ie (ie)
    (ole-method* ie :Navigate (list "http://www.google.co.jp/"
                                    0 "_self") nil)
    (while (/= (ole-method* ie :ReadyState) 4)
      (sit-for 0.1))
    (ole-method (ole-method* ie :Document) :Title))
  => "Google")

(deftest test-ole-method*-named-args ()
  (with-ie (ie)
    (ole-method* ie :Navigate nil (list :URL "http://www.google.co.jp/"
                                        :TargetFrameName "_self"))
    (while (/= (ole-method* ie :ReadyState) 4)
      (sit-for 0.1))
    (ole-method (ole-method* ie :Document) :Title))
  => "Google")

(deftest test-ole-reader-macro-no-args ()
  (values
   '#{worksheets.Add}
   '#{worksheets.Add[]}
   '#{xls.Worksheets.Add[]})
  => (ole-method* worksheets :Add nil nil)
  => (ole-method* worksheets :Add nil nil)
  => (ole-method* (ole-method* xls :Worksheets nil nil) :Add nil nil))

(deftest test-ole-reader-macro-args ()
  (values
   '#{worksheets.Add[worksheet]}
   '#{xls.Worksheets.Add[worksheet]}
   '#{xls.Worksheets.Add[:null worksheet]})
  => (ole-method* worksheets :Add (list worksheet) nil)
  => (ole-method* (ole-method* xls :Worksheets nil nil) :Add (list worksheet) nil)
  => (ole-method* (ole-method* xls :Worksheets nil nil) :Add (list :null worksheet) nil))

(deftest test-ole-reader-macro-named-args ()
  (values
   '#{worksheets.Add[:null worksheet {}]}
   '#{worksheets.Add[:null worksheet {:Count 2}]}
   '#{worksheets.Add[{:Count 2} worksheet]}
   '#{xls.Worksheets.Add[{:After worksheet} {:Count 2}]}
   '#{xls.Worksheets.Add[{:After worksheet :Count 2}]})
  => (ole-method* worksheets :Add (list :null worksheet) nil)
  => (ole-method* worksheets :Add (list :null worksheet) (list :Count 2))
  => (ole-method* worksheets :Add (list worksheet) (list :Count 2))
  => (ole-method* (ole-method* xls :Worksheets nil nil) :Add nil (list :After worksheet :Count 2))
  => (ole-method* (ole-method* xls :Worksheets nil nil) :Add nil (list :After worksheet :Count 2)))

(deftest fix-ole-event-sink-load-typelib ()
  (with-ie (ie)
    (let (download-complete)
      (ole-create-event-sink ie "DWebBrowserEvents2")
      (set-ole-event-handler ie "DownloadComplete"
                             #'(lambda () (setf download-complete t)))
      #{ie.Navigate["http://www.google.co.jp/"]}
      (while (/= #{ie.ReadyState} 4)
        (sit-for 0.1))
      download-complete))
  => t)

(deftest fix-ole-for-each ()
  (with-ie (ie)
    #{ie.Navigate["http://www.google.co.jp/"]}
    (while (/= #{ie.ReadyState} 4)
      (sit-for 0.1))
    (let ((c 0))
      (ole-for-each (e #{ie.Document.all})
        (incf c))
      (< 0 c)))
  => t)

(deftest fix-ole-for-each-2 ()
  (with-wbem (wbem)
    (let ((c 0))
      (ole-for-each (e #{wbem.ExecQuery["SELECT * FROM Win32_Process WHERE Name='xyzzy.exe'"]})
        (incf c))
      (< 0 c)))
  => t)

(deftest print-oledata ()
  (values
   (string-matchp "IFileSystem" (format nil "~S" (ole-create-object "Scripting.FileSystemObject")))
   (string-matchp (if (featurep :windows-8)
                      "IServerXMLHttpRequest"
                    "IXMLHttpRequest")
                  (format nil "~S" (ole-create-object "MSXML2.XMLHTTP"))))
  => non-nil
  => non-nil)
