(deftest cl-package ()
  (values
   (find-package :common-lisp)
   (package-name :common-lisp)
   (package-nicknames :common-lisp)
   (mapcar 'package-name (package-use-list :common-lisp))
   (member (find-package :common-lisp) (list-all-packages)))
  => non-nil
  => "common-lisp"
  => nil
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
   (common-lisp:format nil "~10@A" "hoge")
   (lisp:format nil "~10@A" :hoge)
   (common-lisp:format nil "~10@A" :hoge))
  => "hoge      "
  => "      hoge"
  => "      hoge"
  => "      hoge")
