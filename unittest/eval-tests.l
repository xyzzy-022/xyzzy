(defmacro with-too-few-arguments-datum (&body body)
  `(handler-case
       (progn ,@body)
     (too-few-arguments (e)
       (too-few-arguments-datum e))))

(deftest too-few-arguments-datum--special-form ()
  (values
   (with-too-few-arguments-datum (if))
   (with-too-few-arguments-datum (if 1)))
  => (if)
  => (if 1))

(deftest too-few-arguments-datum--macro ()
  (values
   (with-too-few-arguments-datum (when))
   (with-too-few-arguments-datum (prog2 1))
   (macrolet ((foo (a b)))
     (with-too-few-arguments-datum (foo 1))))
  => (when)
  => (prog2 1)
  => (foo 1))

(deftest too-few-arguments-datum--function ()
  (values
   (with-too-few-arguments-datum (car))
   (with-too-few-arguments-datum (nth 1))
   (flet ((foo (a b)))
     (with-too-few-arguments-datum (foo 1))))
  => (car)
  => (nth 1)
  => (foo 1))


(defmacro with-too-many-arguments-datum (&body body)
  `(handler-case
       (progn ,@body)
     (too-many-arguments (e)
       (too-many-arguments-datum e))))

(deftest too-many-arguments-datum--special-form ()
  (values
   (with-too-many-arguments-datum (if 1 2 3 4))
   (with-too-many-arguments-datum (if 1 2 3 4 5)))
  => (if 1 2 3 4)
  => (if 1 2 3 4 5))

(deftest too-many-arguments-datum--macro ()
  (values
   (with-too-many-arguments-datum (nth-value 1 2 3))
   (macrolet ((foo (a b)))
     (with-too-many-arguments-datum (foo 1 2 3))))
  => (nth-value 1 2 3)
  => (foo 1 2 3))

(deftest too-many-arguments-datum--function ()
  (values
   (with-too-many-arguments-datum (nth 1 2 3))
   (flet ((foo (a b)))
     (with-too-many-arguments-datum (foo 1 2 3))))
  => (nth 1 2 3)
  => (foo 1 2 3))


(deftest flet-function-name ()
  (values
   #1=(flet ((foo () 1))
        (flet ((bar () 2)
               (baz () 3))
          (mapcar #'si:*function-name (list #'foo #'bar #'baz))))
   (compile-and-eval
     #1#))
  => ((flet foo) (flet bar) (flet baz))
  => ((flet foo) (flet bar) (flet baz)))

(deftest labels-function-name ()
  (values
   #1=(labels ((foo () 1))
        (labels ((bar () 2)
                 (baz () 3))
          (mapcar #'si:*function-name (list #'foo #'bar #'baz))))
   (compile-and-eval
     #1#))
  => ((labels foo) (labels bar) (labels baz))
  => ((labels foo) (labels bar) (labels baz)))
