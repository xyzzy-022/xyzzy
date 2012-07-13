(defun generate-random (n limit state)
  (let (nums)
    (dotimes (_ n)
      (push (random limit state) nums))
    nums))

(deftest random-limit ()
  (every #'(lambda (n) (and (<= 0 n) (< n 3)))
         (generate-random 1000 3 nil))
  => t)

(deftest random-type ()
  (values
   (si:*fixnump (random 1))
   (si:*bignump (random 100000000000000000000))
   (type-of (random 1.2))
   (type-of (random 1.2d3))
   (type-of (random 1/2)))
  => t
  => t
  => single-float
  => double-float
  => ratio)

(deftest random-state ()
  (let* ((s1 (make-random-state))
         (s2 (make-random-state t))
         (s3 (make-random-state s1)))
    (let ((r1 (generate-random 1000 100 s1))
          (r2 (generate-random 1000 100 s2))
          (r3 (generate-random 1000 100 s3)))
      (values
       (equal r1 r2)
       (equal r1 r3))))
  => nil
  => t)

(deftest random-state-seed ()
  "˜A‘±‚µ‚Ä random-state ‚ğ¶¬‚µ‚Ä‚àˆÙ‚È‚é seed ‚É‚È‚é‚±‚Æ"
  (let* ((s1 (make-random-state t))
         (s2 (make-random-state t))
         (s3 (make-random-state t)))
    (let ((r1 (generate-random 1000 100 s1))
          (r2 (generate-random 1000 100 s2))
          (r3 (generate-random 1000 100 s3)))
      (values
       (equal r1 r2)
       (equal r1 r3))))
  => nil
  => nil)
