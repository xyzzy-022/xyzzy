(deftest make-vector--fill-pointer-and-initial-contents ()
  (let ((v (make-array 3 :fill-pointer 2 :initial-contents '(0 1 2))))
    (values
     (fill-pointer v)
     (aref v 0)
     (aref v 1)
     (aref v 2)
     (format nil "~A" v)
     (setf (fill-pointer v) 3)
     (format nil "~A" v)
     ))
  => 2
  => 0
  => 1
  => 2
  => "#(0 1)"
  => 3
  => "#(0 1 2)")

(deftest make-string--fill-pointer-and-initial-contents ()
  (let ((v (make-array 3 :element-type 'character :fill-pointer 2
                       :initial-contents "abc")))
    (values
     (fill-pointer v)
     (aref v 0)
     (aref v 1)
     (aref v 2)
     (format nil "~A" v)
     (setf (fill-pointer v) 3)
     (format nil "~A" v)
     ))
  => 2
  => #\a
  => #\b
  => #\c
  => "ab"
  => 3
  => "abc")

(deftest remove-duplicates-nil-key (:compare equalp)
  (values
   (remove-duplicates '(1 2 1))
   (remove-duplicates '(1 2 1) :key nil)
   (remove-duplicates #(1 2 1))
   (remove-duplicates #(1 2 1) :key nil))
  => (2 1)
  => (2 1)
  => #(2 1)
  => #(2 1))

(deftest delete-duplicates-nil-key (:compare equalp)
  (values
   (delete-duplicates '(1 2 1))
   (delete-duplicates '(1 2 1) :key nil)
   (delete-duplicates #(1 2 1))
   (delete-duplicates #(1 2 1) :key nil))
  => (2 1)
  => (2 1)
  => #(2 1)
  => #(2 1))

(deftest substitute-nil-key ()
  (values
   (substitute #\a #\b "abcd")
   (substitute #\a #\b "abcd" :key nil))
  => "aacd"
  => "aacd")

(deftest substitute-if-nil-key ()
  (values
   (substitute-if nil #'oddp '(1 2 3 4))
   (substitute-if nil #'oddp '(1 2 3 4) :key nil))
  => (nil 2 nil 4)
  => (nil 2 nil 4))

(deftest substitute-if-not-nil-key ()
  (values
   (substitute-if-not nil #'oddp '(1 2 3 4))
   (substitute-if-not nil #'oddp '(1 2 3 4) :key nil))
  => (1 nil 3 nil)
  => (1 nil 3 nil))

(deftest nsubstitute-nil-key ()
  (values
   (nsubstitute #\b #\a "abcd")
   (nsubstitute #\b #\a "abcd" :key nil))
  => "bbcd"
  => "bbcd")

(deftest nsubstitute-if-nil-key ()
  (values
   (nsubstitute-if nil #'oddp '(1 2 3 4))
   (nsubstitute-if nil #'oddp '(1 2 3 4) :key nil))
  => (nil 2 nil 4)
  => (nil 2 nil 4))

(deftest nsubstitute-if-not-nil-key ()
  (values
   (nsubstitute-if-not nil #'oddp '(1 2 3 4))
   (nsubstitute-if-not nil #'oddp '(1 2 3 4) :key nil))
  => (1 nil 3 nil)
  => (1 nil 3 nil))

(deftest count-nil-key ()
  (values
   (count 1 '(1 2 3 4))
   (count 1 '(1 2 3 4) :key nil))
  => 1
  => 1)

(deftest count-if-nil-key ()
  (values
   (count-if #'oddp '(1 2 3 4))
   (count-if #'oddp '(1 2 3 4) :key nil))
  => 2
  => 2)

(deftest count-if-not-nil-key ()
  (values
   (count-if-not #'oddp '(1 2 3 4))
   (count-if-not #'oddp '(1 2 3 4) :key nil))
  => 2
  => 2)

(deftest mismatch-nil-key ()
  (values
   (mismatch '(1 3) '(1 2 3))
   (mismatch '(1 3) '(1 2 3) :key nil))
  => 1
  => 1)

(deftest merge-nil-key ()
  (values
   (merge 'list '(1 2 3 5 8) '(2 4 6) #'<)
   (merge 'list '(1 2 3 5 8) '(2 4 6) #'< :key nil)
   (merge 'string "AbCF" "aBcd" #'char-lessp)
   (merge 'string "AbCF" "aBcd" #'char-lessp :key nil))
  => (1 2 2 3 4 5 6 8)
  => (1 2 2 3 4 5 6 8)
  => "AabBCcdF"
  => "AabBCcdF")
