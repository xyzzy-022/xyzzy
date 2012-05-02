(require "foreign")

(c:define-dll-entry
  c:int
  EnumWindows ((c:void *)       ; lpEnumFunc
               (c:void *))      ; lParam
  "user32")

(c:defun-c-callable
  c:int
  enum-windows-proc
  ((c:int hwnd)
   ((c:void *) data))
  (push hwnd *enum-windows-result*)
  1)

(deftest defun-c-callable-stdcall ()
  (let ((*enum-windows-result* nil))
    (declare (special *enum-windows-result*))
    (EnumWindows #'enum-windows-proc 0)
    (member (get-window-handle) *enum-windows-result*))
  => non-nil)


(c:define-dll-entry
  c:void
  (qsort :convention :cdecl)
  ((c:void *)             ; base
   c:size_t               ; num
   c:size_t               ; width
   (c:void *))            ; compare
  "msvcrt")

(c:defun-c-callable
  c:int
  (int32-comparator :convention :cdecl) (((c:void *) elem1)
                                         ((c:void *) elem2))
  (let ((a (si:unpack-int32 (si:make-chunk 'int 4 nil elem1) 0))
        (b (si:unpack-int32 (si:make-chunk 'int 4 nil elem2) 0)))
    (cond ((= a b) 0)
          ((< a b) -1)
          (t 1))))

(deftest defun-c-callable-cdecl ()
  (flet ((unpack-int32-array (array n)
           (let (r)
             (dotimes (i n)
               (push (si:unpack-int32 array (* i 4)) r))
             (nreverse r))))
    (let* ((n 10)
           (array (si:make-chunk 'int[] (* n 4))))
      (dotimes (i n)
        (si:pack-int32 array (* i 4) (- n i)))
      (values (unpack-int32-array array n)
              (progn
                (qsort array n 4 #'int32-comparator)
                (unpack-int32-array array n)))))
  => (10 9 8 7 6 5 4 3 2 1)
  => (1 2 3 4 5 6 7 8 9 10))


(c:define-dll-entry
  c:int64
  _atoi64 ((c:void *))
  "msvcrt")

(deftest define-dll-entry-return-int64 ()
  (flet ((atoi (str)
           (_atoi64 (si:make-string-chunk str))))
    (values
     (atoi "0")
     (atoi "1")
     (atoi "-1")
     (atoi "4294967295")
     (atoi "9223372036854775807")))
  => 0
  => 1
  => -1
  => #xFFFFFFFF
  => #x7FFFFFFFFFFFFFFF)


(c:define-dll-entry
  (c:void *)
  _i64toa (c:int64 (c:void *) c:int)
  "msvcrt")

(deftest define-dll-entry-int64-arg ()
  (flet ((itoa (n)
           (let ((c (si:make-chunk nil 30)))
             (_i64toa n c 10)
             (si:unpack-string c 0))))
    (values
     (itoa 0)
     (itoa 1)
     (itoa -1)
     (itoa #xFFFFFFFF)
     (itoa #x7FFFFFFFFFFFFFFF)))
  => "0"
  => "1"
  => "-1"
  => "4294967295"
  => "9223372036854775807")
