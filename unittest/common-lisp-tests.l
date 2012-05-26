(deftest cl-package ()
  (values
   (find-package :cl)
   (find-package :common-lisp)
   (package-name :cl)
   (package-nicknames :cl)
   (mapcar 'package-name (package-use-list :cl))
   (member (find-package :cl) (list-all-packages)))
  => non-nil
  => non-nil
  => "common-lisp"
  => ("cl")
  => ("lisp")
  => non-nil)

(deftest cl-user-package ()
  (values
   (find-package :cl-user)
   (find-package :common-lisp-user)
   (package-name :cl-user)
   (package-nicknames :cl-user)
   (mapcar 'package-name (package-use-list :cl-user))
   (member (find-package :cl-user) (list-all-packages)))
  => non-nil
  => non-nil
  => "common-lisp-user"
  => ("cl-user")
  => ("common-lisp" "editor")
  => non-nil)

(deftest cl-format-tilde-A ()
  (values
   (lisp:format nil "~10@A" "hoge")
   (cl:format nil "~10@A" "hoge")
   (lisp:format nil "~10@A" :hoge)
   (cl:format nil "~10@A" :hoge))
  => "hoge      "
  => "      hoge"
  => "      hoge"
  => "      hoge")
