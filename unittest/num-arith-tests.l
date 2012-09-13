(deftest test-number-negate-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (- shortint)
     (- longint)
     (- bignum)))
  => -2
  => -1073741824
  => -4294967296)

(deftest test-number-add-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (+ shortint shortint)
     (+ shortint longint)
     (+ shortint bignum)
     (+ longint shortint)
     (+ longint longint)
     (+ longint bignum)
     (+ bignum shortint)
     (+ bignum longint)
     (+ bignum bignum)
     ))
  => 4
  => 1073741826
  => 4294967298
  => 1073741826
  => 2147483648
  => 5368709120
  => 4294967298
  => 5368709120
  => 8589934592)

(deftest test-number-subtract-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (- shortint shortint)
     (- shortint longint)
     (- shortint bignum)
     (- longint shortint)
     (- longint longint)
     (- longint bignum)
     (- bignum shortint)
     (- bignum longint)
     (- bignum bignum)
     ))
  => 0
  => -1073741822
  => -4294967294
  => 1073741822
  => 0
  => -3221225472
  => 4294967294
  => 3221225472
  => 0)

(deftest test-number-multiply-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (* shortint shortint)
     (* shortint longint)
     (* shortint bignum)
     (* longint shortint)
     (* longint longint)
     (* longint bignum)
     (* bignum shortint)
     (* bignum longint)
     (* bignum bignum)
     ))
  => 4
  => 2147483648
  => 8589934592
  => 2147483648
  => 1152921504606846976
  => 4611686018427387904
  => 8589934592
  => 4611686018427387904
  => 18446744073709551616)

(deftest test-number-divide-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (/ shortint shortint)
     (/ shortint longint)
     (/ shortint bignum)
     (/ longint shortint)
     (/ longint longint)
     (/ longint bignum)
     (/ bignum shortint)
     (/ bignum longint)
     (/ bignum bignum)
     ))
  => 1
  => 1/536870912
  => 1/2147483648
  => 536870912
  => 1
  => 1/4
  => 2147483648
  => 4
  => 1)

(deftest test-fix-number-int64 ()
  (let ((shortint 2)
        (longint (expt 2 30))
        (bignum (expt 2 32)))
    (values
     (mod shortint -1)
     (mod longint -1)
     (mod bignum -1)
     ))
  => 0
  => 0
  => 0)
