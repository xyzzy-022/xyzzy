(defun find-etc (file &rest args)
  (member file (apply #'directory (etc-path) args)
          :test #'string=))

(deftest directory--absolute ()
  (flet ((absolute-path-p (path)
           (string-equal (merge-pathnames path "c:/hoge")
                         (remove-trail-slash path))))
    (values
     (every #'absolute-path-p (directory (etc-path) :absolute t))
     (every (complement #'absolute-path-p) (directory (etc-path) :absolute nil))))
  => t
  => t)

(deftest directory--recursive ()
  (values
   (find-etc "CSS" :recursive t)
   (find-etc "CSS" :recursive nil)
   (find-etc "CSS3/line" :recursive t)
   (find-etc "CSS3/line" :recursive nil))
  => non-nil
  => non-nil
  => non-nil
  => nil)

(deftest directory--wild ()
  (values
   (every #'(lambda (path)
              (string= "exe" (pathname-type path)))
          (directory (etc-path) :wild "*.exe"))
   (sort (directory (etc-path) :wild "CS*") #'string<))
  => t
  => ("CSS" "CSS3/"))

(deftest directory--depth ()
  (values
   (find-etc "CSS3/line" :recursive t :depth 0)
   (find-etc "CSS3/line" :recursive t :depth 1)
   (find-etc "CSS3/line" :recursive t :depth 2))
  => non-nil
  => nil
  => non-nil)

(deftest directory--show-dots ()
  (values
   (find-etc "./" :show-dots t)
   (find-etc "../" :show-dots t)
   (find-etc "./" :show-dots nil)
   (find-etc "../" :show-dots nil))
  => non-nil
  => non-nil
  => nil
  => nil)

(deftest directory--file-only ()
  (values
   (find-etc "CSS3/" :file-only t)
   (find-etc "CSS3/" :file-only nil))
  => nil
  => non-nil)

(deftest directory--directory-only ()
  (values
   (find-etc "CSS" :directory-only t)
   (find-etc "CSS" :directory-only nil))
  => nil
  => non-nil)

(deftest directory--count ()
  (values
   (length (directory (etc-path) :count -1))
   (length (directory (etc-path) :count 0))
   (length (directory (etc-path) :count 3))
   (length (directory (etc-path) :count 8))
   (length (directory (etc-path) :count 10 :recursive nil))
   (length (directory (etc-path) :count 10 :recursive t)))
  => 0
  => 0
  => 3
  => 8
  => 10
  => 10)

(deftest directory--callback ()
  (let (args)
    (values
     (directory (etc-path) :callback #'(lambda (arg) (push arg args)))
     (directory (etc-path) :callback #'(lambda (arg) (push arg args))
                :recursive t :file-info t)
     (equal (reverse args)
            (append (directory (etc-path))
                    (directory (etc-path) :recursive t :file-info t)))))
  => nil
  => nil
  => t)

(deftest directory--file-info ()
  (equal (car (directory (etc-path) :file-info t :count 1 :absolute t))
         (let ((file (car (directory (etc-path) :file-info nil :count 1 :absolute t))))
           (cons file (get-file-info file))))
  => t)


(defparameter *directory-test-dirs*
  '("a/" "a/a/" "a/a/a/" "a/b/" "b/" "b/a/" "c/"))

(defmacro with-directory-test-dirs ((tmpdir) &body body)
  `(with-temp-dirs (,tmpdir)
     (dolist (sub *directory-test-dirs*)
       (create-directory (merge-pathnames sub ,tmpdir)))
     (unwind-protect
         (progn ,@body)
       (mapc 'delete-directory (directory ,tmpdir :recursive t :absolute t)))))

(deftest directory--test ()
  (with-directory-test-dirs (tmpdir)
    (flet ((test (&rest keys)
             (let (args)
               (apply #'directory tmpdir :test #'(lambda (arg) (push arg args))
                      keys)
               (sort args #'string<)))
           (to-abs (lst root)
             (mapcar #'(lambda (sub)
                         (append-trail-slash (merge-pathnames sub root)))
                     lst)))
      (values
       (test)
       (equal
        (test :absolute t)
        (to-abs '("a/" "b/" "c/") tmpdir))
       (test :recursive t)
       (equal
        (test :absolute t :recursive t)
        (to-abs *directory-test-dirs* tmpdir))
       )))
  => ("a/" "b/" "c/")
  => t
  => ("a/" "a/a/" "a/a/a/" "a/b/" "b/" "b/a/" "c/")
  => t)

(deftest directory--test-prune ()
  (with-directory-test-dirs (tmpdir)
    (flet ((test (fn)
             (sort (directory tmpdir :test fn :recursive t) #'string<)))
      (values
       (test #'(lambda (e)
                 (string-match "^b/" e)))
       (test #'(lambda (e)
                 (not (path-equal (merge-pathnames e tmpdir)
                                  (merge-pathnames "a/a" tmpdir)))))
       )))
  => ("b/" "b/a/")
  => ("a/" "a/b/" "b/" "b/a/" "c/"))

(deftest directory--test-file-info ()
  (with-directory-test-dirs (tmpdir)
    (let (args)
      (directory tmpdir :test #'(lambda (arg) (push arg args))
                 :file-info t :recursive t)
      (equal
       (mapcar #'cdr (sort args #'string< :key #'car))
       (mapcar #'(lambda (sub)
                   (get-file-info (merge-pathnames sub tmpdir)))
               *directory-test-dirs*))))
  => t)

(deftest directory--test-absolute ()
  (flet ((absolute-path-p (path)
           (string-equal (merge-pathnames path "c:/hoge")
                         (remove-trail-slash path)))
         (test (absolute)
           (let (args)
             (directory (etc-path) :test #'(lambda (arg) (push arg args))
                        :absolute absolute :recursive t)
             args)))
    (values
     (every #'absolute-path-p (test t))
     (every (complement #'absolute-path-p) (test nil))))
  => t
  => t)

(deftest directory--test-wild-1 ()
  (let (args)
    (values
     (sort (directory (etc-path) :wild "s*" :recursive t
                      :test #'(lambda (arg) (push arg args)))
           #'string<)
     (sort args #'string<)))
  => ("CSS3/speech" "Sql" "Sql-NonStd/" "Sql-NonStd/SQLServer")
  => ("CSS3/" "CSS3/speech" "Sql" "Sql-NonStd/" "Sql-NonStd/SQLServer"))

(deftest directory--test-wild-2 ()
  (let (args)
    (values
     (sort (directory (etc-path) :wild "Ora*" :recursive t
                      :test #'(lambda (arg) (push arg args)))
           #'string<)
     (sort args #'string<)))
  => ("Sql-NonStd/Oracle")
  => ("CSS3/" "Sql-NonStd/" "Sql-NonStd/Oracle"))
