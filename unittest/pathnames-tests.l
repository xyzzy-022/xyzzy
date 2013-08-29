(deftest shell-operation-invalid-arg-1 ()
  (si:*file-operation :copy nil nil)
  !! type-error)

(deftest shell-operation-invalid-arg-2 ()
  (si:*file-operation :copy 1 nil)
  !! type-error)

(deftest shell-operation-invalid-arg-3 ()
  (si:*file-operation :copy :hoge nil)
  !! type-error)

(deftest shell-operation-invalid-arg-4 ()
  (si:*file-operation :copy "c:/" nil)
  !! type-error)

(deftest shell-operation-invalid-arg-5 ()
  (si:*file-operation :copy "c:/" 1)
  !! type-error)

(deftest shell-operation-invalid-arg-6 ()
  (si:*file-operation :copy "c:/" :hoge)
  !! type-error)

(deftest shell-operation-invalid-arg-7 ()
  (si:*file-operation nil "c:/" "d:/")
  !! simple-program-error)

(deftest shell-operation-invalid-arg-8 ()
  (si:*file-operation :hoge "c:/" "d:/")
  !! simple-program-error)


(deftest shell-operation-copy ()
  (with-temp-files (file)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file))
    (si:*file-operation :copy file (concat file ".tmp") :no-ui t)
    (values
     (file-exist-p file)
     (with-temp-buffer
       (read-file (concat file ".tmp"))
       (buffer-substring (point-min) (point-max)))))
  => t
  => "abc\n")

(deftest shell-operation-move ()
  (with-temp-files (file)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file))
    (si:*file-operation :move file (concat file ".tmp") :no-ui t)
    (values
     (file-exist-p file)
     (with-temp-buffer
       (read-file (concat file ".tmp"))
       (buffer-substring (point-min) (point-max)))))
  => nil
  => "abc\n")

(deftest shell-operation-rename ()
  (with-temp-files (file)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file))
    (si:*file-operation :rename file (concat file ".tmp") :no-ui t)
    (values
     (file-exist-p file)
     (with-temp-buffer
       (read-file (concat file ".tmp"))
       (buffer-substring (point-min) (point-max)))))
  => nil
  => "abc\n")

(deftest shell-operation-delete ()
  (with-temp-files (file)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file))
    (values
     (file-exist-p file)
     (progn
       (si:*file-operation :delete file nil :no-ui t)
       (file-exist-p file))))
  => t
  => nil)


(deftest shell-operation-copy-multi ()
  (with-temp-files (file1 file2)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file1)
      (write-file file2))
    (with-temp-dirs (tmpdir)
      (let ((new-file1 (merge-pathnames (file-namestring file1) tmpdir))
            (new-file2 (merge-pathnames (file-namestring file2) tmpdir)))
        (unwind-protect
            (progn
              (si:*file-operation :copy
                                  (list file1 file2)
                                  tmpdir
                                  :no-ui t)
              (values
               (file-exist-p file1)
               (file-exist-p file2)
               (file-exist-p new-file1)
               (file-exist-p new-file2)
               (compare-file file1 new-file1)
               (compare-file file2 new-file2)
               ))
          (delete-file new-file1)
          (delete-file new-file2)))))
  => t
  => t
  => t
  => t
  => t
  => t)

(deftest shell-operation-move-multi ()
  (with-temp-files (file1 file2)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file1)
      (write-file file2))
    (with-temp-dirs (tmpdir)
      (let ((new-file1 (merge-pathnames (file-namestring file1) tmpdir))
            (new-file2 (merge-pathnames (file-namestring file2) tmpdir)))
        (unwind-protect
            (progn
              (si:*file-operation :move
                                  (list file1 file2)
                                  tmpdir
                                  :no-ui t)
              (values
               (file-exist-p file1)
               (file-exist-p file2)
               (file-exist-p new-file1)
               (file-exist-p new-file2)
               ))
          (delete-file new-file1)
          (delete-file new-file2)))))
  => nil
  => nil
  => t
  => t)

(deftest shell-operation-move-multi2 ()
  (with-temp-files (file1 file2)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file1)
      (write-file file2))
    (with-temp-dirs (tmpdir)
      (let ((new-file1 (merge-pathnames (file-namestring file1) tmpdir))
            (new-file2 (merge-pathnames (file-namestring file2) tmpdir)))
        (unwind-protect
            (progn
              (si:*file-operation :move
                                  (list file1 file2)
                                  (list new-file1 new-file2)
                                  :no-ui t)
              (values
               (file-exist-p file1)
               (file-exist-p file2)
               (file-exist-p new-file1)
               (file-exist-p new-file2)
               ))
          (delete-file new-file1)
          (delete-file new-file2)))))
  => nil
  => nil
  => t
  => t)

(deftest shell-operation-delete-multi ()
  (with-temp-files (file1 file2)
    (with-temp-buffer
      (insert "abc" #\LFD)
      (write-file file1)
      (write-file file2))
    (si:*file-operation :delete
                        (list file1 file2)
                        nil
                        :no-ui t)
    (values
     (file-exist-p file1)
     (file-exist-p file2)
     ))
  => nil
  => nil)
