(deftest test-make-array-0 ()
  (equalp
   #()
   (make-array 0))
  => t)

(deftest test-make-array-1-1 ()
  (equalp
   #2A((nil))
   (make-array (list 1 1)))
  => t)

(deftest test-make-array-4-1 ()
  (equalp
   #2A((nil) (nil) (nil) (nil))
   (make-array (list 4 1)))
  => t)

(deftest test-make-array-4-2 ()
  (equalp
   #2A((nil nil) (nil nil) (nil nil) (nil nil))
   (make-array (list 4 2)))
  => t)


(deftest test-make-large-array-1 ()
  (make-array (list (expt 2 29) 1))
  !! program-error)

(deftest test-make-large-array-2 ()
  (make-array (list (expt 2 29) 2))
  !! program-error)

(deftest test-make-large-array-3 ()
  (make-array (list (expt 2 30) 1))
  !! program-error)

(deftest test-make-large-array-4 ()
  (make-array (list (- (expt 2 31) 1) 1))
  !! program-error)

(deftest test-make-large-array-5 ()
  (make-array (list (expt 2 31) 1))
  !! range-error)

(deftest test-make-large-array-6 ()
  (make-array (list (expt 2 64) 10))
  !! range-error)
