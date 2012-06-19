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
