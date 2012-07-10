(deftest sha-224 ()
  (values
   (si:sha-224 "")
   (si:sha-224 "The quick brown fox jumps over the lazy dog")
   (si:sha-224 "The quick brown fox jumps over the lazy dog."))
  => "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"
  => "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"
  => "619cba8e8e05826e9b8c519c0a5c68f4fb653e8a3d8aa04bb2c8cd4c")

(deftest sha-256 ()
  (values
   (si:sha-256 "")
   (si:sha-256 "The quick brown fox jumps over the lazy dog")
   (si:sha-256 "The quick brown fox jumps over the lazy dog."))
  => "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  => "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
  => "ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c")

(deftest sha-384 ()
  (values
   (si:sha-384 "")
   (si:sha-384 "The quick brown fox jumps over the lazy dog")
   (si:sha-384 "The quick brown fox jumps over the lazy dog."))
  => "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
  => "ca737f1014a48f4c0b6dd43cb177b0afd9e5169367544c494011e3317dbf9a509cb1e5dc1e85a941bbee3d7f2afbc9b1"
  => "ed892481d8272ca6df370bf706e4d7bc1b5739fa2177aae6c50e946678718fc67a7af2819a021c2fc34e91bdb63409d7")

(deftest sha-512 ()
  (values
   (si:sha-512 "")
   (si:sha-512 "The quick brown fox jumps over the lazy dog")
   (si:sha-512 "The quick brown fox jumps over the lazy dog."))
  => "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
  => "07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6"
  => "91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d12890cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed")


;;; RFC 4231 - Identifiers and Test Vectors for HMAC-SHA-224, HMAC-SHA-256,
;;;            HMAC-SHA-384, and HMAC-SHA-512
;;; http://tools.ietf.org/html/rfc4231

;;; 4.2.  Test Case 1

(deftest hmac-sha2-test-case-1 ()
  (let ((key (make-sequence 'string 20 :initial-element (code-char #x0b)))
        (data "Hi There"))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22"
  => "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
  => "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6"
  => "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854")

;;; 4.3.  Test Case 2

(deftest hmac-sha2-test-case-2 ()
  "Test with a key shorter than the length of the HMAC output."
  (let ((key "Jefe")
        (data "what do ya want for nothing?"))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44"
  => "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
  => "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649"
  => "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737")

;;;; 4.4.  Test Case 3

(deftest hmac-sha2-test-case-3 ()
  "Test with a combined length of key and data that is larger than 64
bytes (= block-size of SHA-224 and SHA-256)."
  (let ((key (make-sequence 'string 20 :initial-element (code-char #xaa)))
        (data (make-sequence 'string 50 :initial-element (code-char #xdd))))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "7fb3cb3588c6c1f6ffa9694d7d6ad2649365b0c1f65d69d1ec8333ea"
  => "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
  => "88062608d3e6ad8a0aa2ace014c8a86f0aa635d947ac9febe83ef4e55966144b2a5ab39dc13814b94e3ab6e101a34f27"
  => "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb")

;;; 4.5.  Test Case 4

(deftest hmac-sha2-test-case-4  ()
  "Test with a combined length of key and data that is larger than 64
bytes (= block-size of SHA-224 and SHA-256)."
  (let ((key (map 'string 'code-char '(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f #x10
                                       #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19)))
        (data (make-sequence 'string 50 :initial-element (code-char #xcd))))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a"
  => "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"
  => "3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb"
  => "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd")

;;; 4.6.  Test Case 5

;    Test with a truncation of output to 128 bits.

;;; 4.7.  Test Case 6

(deftest hmac-sha2-test-case-6  ()
  "Test with a key larger than 128 bytes (= block-size of SHA-384 and
SHA-512)."
  (let ((key (make-sequence 'string 131 :initial-element (code-char #xaa)))
        (data "Test Using Larger Than Block-Size Key - Hash Key First"))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "95e9a0db962095adaebe9b2d6f0dbce2d499f112f2d2b7273fa6870e"
  => "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
  => "4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952"
  => "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598")

;;; 4.8.  Test Case 7

(deftest hmac-sha2-test-case-7  ()
  "Test with a key and data that is larger than 128 bytes (= block-size
of SHA-384 and SHA-512)."
  (let ((key (make-sequence 'string 131 :initial-element (code-char #xaa)))
        (data "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."))
    (values (si:hmac-sha-224 key data)
            (si:hmac-sha-256 key data)
            (si:hmac-sha-384 key data)
            (si:hmac-sha-512 key data)))
  => "3a854166ac5d9f023f54d517d0b39dbd946770db9c2b95c9f6f565d1"
  => "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
  => "6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e"
  => "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58")


(defun hex-string-to-bin-string (hex-string)
  (let (r)
    (dotimes (i (/ (length hex-string) 2))
      (let ((s (* i 2)))
        (push (parse-integer hex-string :start s :end (+ 2 s) :radix 16) r)))
    (map 'string 'code-char (nreverse r))))

(deftest digest-method--binary-result ()
  (flet ((test (fn arg)
           (string= (hex-string-to-bin-string (funcall fn arg))
                    (funcall fn arg :binary t))))
    (values
     (test 'si:md5 "")
     (test 'si:md5 "foo")
     (test 'si:sha-1 "")
     (test 'si:sha-1 "foo")
     (test 'si:sha-224 "")
     (test 'si:sha-224 "foo")
     (test 'si:sha-256 "")
     (test 'si:sha-256 "foo")
     (test 'si:sha-384 "")
     (test 'si:sha-384 "foo")
     (test 'si:sha-512 "")
     (test 'si:sha-512 "foo")))
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t)

(deftest hmac-method--binary-result ()
  (flet ((test (fn key arg)
           (string= (hex-string-to-bin-string (funcall fn key arg))
                    (funcall fn key arg :binary t))))
    (values
     (test 'si:hmac-md5 "key" "")
     (test 'si:hmac-md5 "key" "foo")
     (test 'si:hmac-sha-1 "key" "")
     (test 'si:hmac-sha-1 "key" "foo")
     (test 'si:hmac-sha-224 "key" "")
     (test 'si:hmac-sha-224 "key" "foo")
     (test 'si:hmac-sha-256 "key" "")
     (test 'si:hmac-sha-256 "key" "foo")
     (test 'si:hmac-sha-384 "key" "")
     (test 'si:hmac-sha-384 "key" "foo")
     (test 'si:hmac-sha-512 "key" "")
     (test 'si:hmac-sha-512 "key" "foo")))
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t)

;;; package documentation

(deftest set-package-documentation-type-error-1 ()
  (si:*set-package-documentation (find-package :lisp) nil)
  !! type-error)

(deftest set-package-documentation-type-error-2 ()
  (si:*set-package-documentation (find-package :lisp) 123)
  !! type-error)

(deftest set-package-documentation-type-error-3 ()
  (si:*set-package-documentation (find-package :lisp) '(foo))
  !! type-error)

(deftest set-package-documentation-type-error-4 ()
  (si:*set-package-documentation (find-package :lisp) :foo)
  !! type-error)

(deftest set-package-documentation ()
  (let ((pkg (make-package (gensym))))
    (si:*set-package-documentation pkg "test package")
    (si:*package-documentation pkg))
  => "test package")

(deftest uuid-create-1 ()
  "戻り値の 1 つ目が文字列で 2 つ目がリストか"
  (multiple-value-bind (uuidstr uuidlst)
      (si:uuid-create)
    (values (typep uuidstr 'string)
            (typep uuidlst 'list)))
  => t
  => t)

(deftest uuid-create-1-seq ()
  "戻り値の 1 つ目が文字列で 2 つ目がリストか (:sequential)"
  (multiple-value-bind (uuidstr uuidlst)
      (si:uuid-create :sequential t)
    (values (typep uuidstr 'string)
            (typep uuidlst 'list)))
  => t
  => t)

(deftest uuid-create-2 ()
  "UUID文字列はRFC4122に従いすべて小文字か"
  (every #'(lambda (c)
             (or (digit-char-p c)
                 (lower-case-p c)
                 (char= c #\-)))
         (si:uuid-create))
  => t)

(deftest uuid-create-2-seq ()
  "UUID文字列はRFC4122に従いすべて小文字か (:sequential)"
  (every #'(lambda (c)
             (or (digit-char-p c)
                 (lower-case-p c)
                 (char= c #\-)))
         (si:uuid-create :sequential t))
  => t)

(deftest uuid-create-3 ()
  "文字列とリストの値は同じUUIDであるか"
  (multiple-value-bind (uuidstr uuidlst)
      (si:uuid-create)
    (string= uuidstr
             (apply 'format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~{~2,'0X~}"
                    uuidlst)))
  => t)

(deftest uuid-create-3-seq ()
  "文字列とリストの値は同じUUIDであるか (:sequential)"
  (multiple-value-bind (uuidstr uuidlst)
      (si:uuid-create :sequential t)
    (string= uuidstr
             (apply 'format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~{~2,'0X~}"
                    uuidlst)))
  => t)

(deftest uuid-create-4 ()
  ":sequential を指定しない場合、前回のUUIDからの差分は1より大きいか"
  (let ((uuid1 (cadr (multiple-value-list (si:uuid-create))))
        (uuid2 (cadr (multiple-value-list (si:uuid-create)))))
    (flet ((test (a b)
             (<= (abs (- a b)) 1)))
      (values (every #'test (butlast uuid1) (butlast uuid2))
              (every #'test (car (last uuid1)) (car (last uuid2))))))
  => nil
  => nil)

(deftest uuid-create-4-seq ()
  ":sequential を指定した場合、前回のUUIDからの差分は1か"
  (let ((uuid1 (cadr (multiple-value-list (si:uuid-create :sequential t))))
        (uuid2 (cadr (multiple-value-list (si:uuid-create :sequential t)))))
    (flet ((test (a b)
             (<= (abs (- a b)) 1)))
      (values (every #'test (butlast uuid1) (butlast uuid2))
              (every #'test (car (last uuid1)) (car (last uuid2))))))
  => t
  => t)

(deftest fix-file-stream-column-1 ()
  "ファイルの si:*stream-column が正しい値を返すか"
  (with-temp-files (out)
    (with-open-file (s out :direction :output)
      (values (si:*stream-column s)
              (progn (format s "aa") (si:*stream-column s))
              (progn (format s "b") (si:*stream-column s)))))
  => 0
  => 2
  => 3)

(deftest fix-file-stream-column-2 ()
  "ファイルの si:*stream-column が正しい値を返すか"
  (with-temp-files (out)
    (with-open-file (s out :direction :output)
      (values (si:*stream-column s)
              (progn (format s "aa\n") (si:*stream-column s))
              (progn (format s "b") (si:*stream-column s)))))
  => 0
  => 0
  => 1)

(deftest getpid ()
  "プロセスIDが取得できること"
  (integerp (si:getpid))
  => t)

(deftest octet-length-builtin-function-p ()
  (si:*builtin-function-p #'si:octet-length)
  => t)

(deftest octet-length-ascii ()
  (values
   (si:octet-length "")
   (si:octet-length " ")
   (si:octet-length "abc")
   (si:octet-length "" :encoding nil)
   (si:octet-length " " :encoding nil)
   (si:octet-length "abc" :encoding nil))
  => 0
  => 1
  => 3
  => 0
  => 1
  => 3)

(deftest octet-length-sjis ()
  (values
   (si:octet-length "あ")
   (si:octet-length "漢字")
   (si:octet-length "アイウ")
   (si:octet-length "あ" :encoding *encoding-sjis*)
   (si:octet-length "漢字" :encoding *encoding-sjis*)
   (si:octet-length "アイウ" :encoding *encoding-sjis*))
  => 2
  => 4
  => 6
  => 2
  => 4
  => 6)

(deftest octet-length-eucjp ()
  (values
   (si:octet-length (convert-encoding-from-internal *encoding-euc-jp* "あ"))
   (si:octet-length (convert-encoding-from-internal *encoding-euc-jp* "漢字"))
   (si:octet-length (convert-encoding-from-internal *encoding-euc-jp* (map-to-half-width-string "アイウ" :katakana t)))
   (si:octet-length "あ" :encoding *encoding-euc-jp*)
   (si:octet-length "漢字" :encoding *encoding-euc-jp*)
   (si:octet-length (map-to-half-width-string "アイウ" :katakana t) :encoding *encoding-euc-jp*))
  => 2
  => 4
  => 6
  => 2
  => 4
  => 6)

(deftest octet-length-utf8 ()
  (values
   (si:octet-length (convert-encoding-from-internal *encoding-utf8n* "あ"))
   (si:octet-length (convert-encoding-from-internal *encoding-utf8n* "漢字"))
   (si:octet-length (convert-encoding-from-internal *encoding-utf8n* "アイウ"))
   (si:octet-length "あ" :encoding *encoding-utf8n*)
   (si:octet-length "漢字" :encoding *encoding-utf8n*)
   (si:octet-length "アイウ" :encoding *encoding-utf8n*))
  => 3
  => 6
  => 9
  => 3
  => 6
  => 9)

(deftest octet-length-start-end ()
  (values
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :start 0)
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :start 1)
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :start 3)
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :start 2 :end 2)
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :start 2 :end 4)
   (si:octet-length "abcあいうアイウ" :encoding *encoding-utf8n* :end 4))
  => 21
  => 20
  => 18
  => 0
  => 4
  => 6)

(deftest octet-length-error-1 ()
  (si:octet-length nil)
  !! type-error)

(deftest octet-length-error-2 ()
  (si:octet-length "abc" :encoding *encoding-auto*)
  !! type-error)

(deftest octet-length-error-3 ()
  (si:octet-length "abc" :encoding "utf8")
  !! type-error)

(deftest octet-length-error-4 ()
  (si:octet-length "abc" :start -1)
  !! range-error)

(deftest octet-length-error-5 ()
  (si:octet-length "abc" :end -1)
  !! range-error)

(deftest octet-length-error-6 ()
  (si:octet-length "abc" :start 4)
  !! range-error)

(deftest octet-length-error-7 ()
  (si:octet-length "abc" :end 4)
  !! range-error)

(deftest chunkp ()
  (values
   (si:*chunkp (si:make-string-chunk ""))
   (si:*chunkp (si:make-chunk nil 0))
   (si:*chunkp nil)
   (si:*chunkp 0)
   (si:*chunkp ""))
  => t
  => t
  => nil
  => nil
  => nil)

(deftest chunk-typep ()
  (values
   (typep (si:make-string-chunk "") 'si:chunk)
   (typep (si:make-chunk nil 0) 'si:chunk)
   (typep nil 'si:chunk)
   (typep 0 'si:chunk)
   (typep "" 'si:chunk))
  => t
  => t
  => nil
  => nil
  => nil)

(deftest chunk-check-type-1 ()
  (values
   (check-type (si:make-string-chunk "") si:chunk)
   (check-type (si:make-chunk nil 0) si:chunk))
  => nil
  => nil)

(deftest chunk-check-type-2 ()
  (check-type "" si:chunk)
  !! type-error)

(deftest pack-unpack-int8 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-int8 c 0 #x12)
    (si:unpack-int8 c 0))
  => #x12)

(deftest pack-unpack-uint8 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-uint8 c 0 #xFF)
    (values
     (si:unpack-uint8 c 0)
     (si:unpack-int8 c 0)))
  => #xFF
  => -1)

(deftest pack-unpack-int8-overflow ()
  (values
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int8 c 0 #xFF)
     (si:unpack-int8 c 0))
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int8 c 0 #x80)
     (si:unpack-int8 c 0)))
  => -1
  => #x-80)

(deftest pack-unpack-int16 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-int16 c 0 #x1234)
    (si:unpack-int16 c 0))
  => #x1234)

(deftest pack-unpack-uint16 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-uint16 c 0 #xFFFF)
    (values
     (si:unpack-uint16 c 0)
     (si:unpack-int16 c 0)))
  => #xFFFF
  => -1)

(deftest pack-unpack-int16-overflow ()
  (values
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int16 c 0 #xFFFF)
     (si:unpack-int16 c 0))
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int16 c 0 #x8000)
     (si:unpack-int16 c 0)))
  => -1
  => #x-8000)

(deftest pack-unpack-int32 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-int32 c 0 #x12345678)
    (si:unpack-int32 c 0))
  => #x12345678)

(deftest pack-unpack-uint32 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-uint32 c 0 #xFFFFFFFF)
    (values
     (si:unpack-uint32 c 0)
     (si:unpack-int32 c 0)))
  => #xFFFFFFFF
  => -1)

(deftest pack-unpack-int32-overflow ()
  (values
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int32 c 0 #xFFFFFFFF)
     (si:unpack-int32 c 0))
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int32 c 0 #x80000000)
     (si:unpack-int32 c 0)))
  => -1
  => #x-80000000)

(deftest pack-unpack-float ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-float c 0 1.1111)
    (si:unpack-float c 0))
  => 1.1111)

(deftest pack-unpack-double ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-double c 0 1.1111d0)
    (si:unpack-double c 0))
  => 1.1111d0)

(deftest pack-unpack-string ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-string c 0 "hoge")
    (si:unpack-string c 0))
  => "hoge")



(deftest pack-unpack-int64 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-int64 c 0 #x1234567890ABCDEF)
    (si:unpack-int64 c 0))
  => #x1234567890ABCDEF)

(deftest pack-unpack-uint64 ()
  (let ((c (si:make-chunk nil 8)))
    (si:pack-uint64 c 0 #xFFFFFFFFFFFFFFFF)
    (values
     (si:unpack-uint64 c 0)
     (si:unpack-int64 c 0)))
  => #xFFFFFFFFFFFFFFFF
  => -1)

(deftest pack-unpack-int64-overflow ()
  (values
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int64 c 0 #xFFFFFFFFFFFFFFFF)
     (si:unpack-int64 c 0))
   (let ((c (si:make-chunk nil 8)))
     (si:pack-int64 c 0 #x8000000000000000)
     (si:unpack-int64 c 0)))
  => -1
  => #x-8000000000000000)

(deftest pack-unpack-int64-offset ()
  (let ((c (si:make-chunk nil 16)))
    (si:pack-int64 c 0 #x1234567890)
    (si:pack-int64 c 8 #x1234567891)
    (values
     (si:unpack-int64 c 0)
     (si:unpack-int64 c 8)))
  => #x1234567890
  => #x1234567891)

(deftest pack-unpack-uint64-offset ()
  (let ((c (si:make-chunk nil 16)))
    (si:pack-uint64 c 0 #xFFFFFFFFFFFFFFFF)
    (si:pack-uint64 c 8 #xFFFFFFFFFFFFFFFF)
    (values
     (si:unpack-uint64 c 0)
     (si:unpack-uint64 c 8)))
  => #xFFFFFFFFFFFFFFFF
  => #xFFFFFFFFFFFFFFFF)

(deftest pack-int64-range-error ()
  (let ((c (si:make-chunk nil 4)))
    (si:pack-int64 c 0 #x1234567890))
  !! range-error)

(deftest pack-uint64-range-error ()
  (let ((c (si:make-chunk nil 4)))
    (si:pack-uint64 c 0 #x1234567890))
  !! range-error)

(deftest unpack-int64-range-error ()
  (let ((c (si:make-chunk nil 4)))
    (si:unpack-uint64 c 0))
  !! range-error)

(deftest unpack-uint64-range-error ()
  (let ((c (si:make-chunk nil 4)))
    (si:unpack-uint64 c 0))
  !! range-error)


(deftest make-chunk-bad-ptr-1 ()
  (si:make-chunk nil 4 nil 0)
  => non-nil)

(deftest make-chunk-bad-ptr-2 ()
  (si:make-chunk nil 4 nil 123)
  => non-nil)

(deftest win32-exception-slots ()
  (handler-case
      (let ((chunk (si:make-chunk nil 100 nil 123)))
        (si:pack-int8 chunk 0 1))
    (win32-exception (e)
      (values (win32-exception-description e)
              (win32-exception-code e)
              (win32-exception-address e))))
  => "Access violation"
  => #xc0000005
  => non-nil)

(deftest pack/unpack-bad-ptr ()
  (macrolet ((test (&body body)
               `(handler-case
                    (progn
                      (progn ,@body)
                      nil)
                  (win32-exception (e)
                    t))))
    (let ((chunk (si:make-chunk nil 100 nil 123)))
      (values
       (test (si:pack-int8 chunk 0 1))
       (test (si:pack-int16 chunk 0 1))
       (test (si:pack-int32 chunk 0 1))
       (test (si:pack-int64 chunk 0 1))
       (test (si:pack-uint8 chunk 0 1))
       (test (si:pack-uint16 chunk 0 1))
       (test (si:pack-uint32 chunk 0 1))
       (test (si:pack-uint64 chunk 0 1))
       (test (si:pack-float chunk 0 1))
       (test (si:pack-double chunk 0 1))
       (test (si:pack-string chunk 0 "hoge"))
       (test (si:unpack-int8 chunk 0))
       (test (si:unpack-int16 chunk 0))
       (test (si:unpack-int32 chunk 0))
       (test (si:unpack-int64 chunk 0))
       (test (si:unpack-uint8 chunk 0))
       (test (si:unpack-uint16 chunk 0))
       (test (si:unpack-uint32 chunk 0))
       (test (si:unpack-uint64 chunk 0))
       (test (si:unpack-float chunk 0))
       (test (si:unpack-double chunk 0))
       (test (si:unpack-string chunk 0)))))
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t
  => t)

(deftest unpack-string-chunk ()
  (let* ((s (format nil "foo~Cbar" #\NUL))
         (c (si:make-string-chunk s)))
    (values
     (si:unpack-string c 0)
     (si:unpack-string c 0 7)
     (substitute #\- #\NUL (si:unpack-string c 0 7 nil))
     (substitute #\- #\NUL (si:unpack-string c 2 5 nil))
     (substitute #\- #\NUL (si:unpack-string c 0 nil nil))
     ))
  => "foo"
  => "foo"
  => "foo-bar"
  => "o-bar"
  => "foo-bar-")

(deftest unpack-string-pointer ()
  (let* ((s (format nil "foo~Cbar" #\NUL))
         (c (si:make-string-chunk s)))
    (values
     (si:unpack-string nil (si:chunk-data c))
     (si:unpack-string nil (si:chunk-data c) 7)
     (substitute #\- #\NUL (si:unpack-string nil (si:chunk-data c) 7 nil))
     (substitute #\- #\NUL (si:unpack-string nil (+ 2 (si:chunk-data c)) 5 nil))
     ))
  => "foo"
  => "foo"
  => "foo-bar"
  => "o-bar")

(deftest unpack-string-error-1 ()
  (let* ((s (format nil "foo~Cbar" #\NUL))
         (c (si:make-string-chunk s)))
    (si:unpack-string c -1))
  !! range-error)

(deftest unpack-string-error-2 ()
  (let* ((s (format nil "foo~Cbar" #\NUL))
         (c (si:make-string-chunk s)))
    (si:unpack-string nil (si:chunk-data c) -1 nil))
  !! range-error)

(deftest unpack-string-error-3 ()
  (let* ((s (format nil "foo~Cbar" #\NUL))
         (c (si:make-string-chunk s)))
    (si:unpack-string nil (si:chunk-data c) nil nil))
  !! range-error)

(deftest binhex-decode ()
  (si:binhex-decode ":#(4PFh3ZG(Kd!&4&@&3rN!3!N!8'!*!%&+PiHATkH3VEZ!!!:")
  => "xyzzy\n"
  => 1413830740
  => 1061109567
  => "test.txt")

(defun closure-variable-helper ()
  (let (a
        (b)
        (c 1)
        (d '(1 2 3))
        (buffer-read-only 4)
        (*kill-xyzzy-hook* 5))
    (si:closure-variable #'(lambda ()))))

(deftest closure-variable ()
  (closure-variable-helper)
  => ((*kill-xyzzy-hook* . 5)
      (buffer-read-only . 4)
      (d 1 2 3)
      (c . 1)
      (b)
      (a)))

(deftest list-builtin-packages-1 ()
  (values
   (length (si:list-builtin-packages))
   (member (find-package :lisp) (si:list-builtin-packages))
   (member (find-package :keyword) (si:list-builtin-packages))
   (member (find-package :editor) (si:list-builtin-packages))
   (member (find-package :system) (si:list-builtin-packages))
   (member (find-package :user) (si:list-builtin-packages))
   (member (find-package :common-lisp) (si:list-builtin-packages))
   (member (find-package :common-lisp-user) (si:list-builtin-packages)))
  => 7
  => non-nil
  => non-nil
  => non-nil
  => non-nil
  => non-nil
  => non-nil
  => non-nil)

(deftest list-builtin-packages-2 ()
  (let ((pkgname (gensym))
        (all-pkg-len (length (list-all-packages)))
        (builtin-pkg-len (length (si:list-builtin-packages))))
    (eval `(defpackage ,pkgname))
    (values
     (= all-pkg-len (length (list-all-packages)))
     (= builtin-pkg-len (length (si:list-builtin-packages)))))
  => nil
  => t)

(deftest builtin-package-p ()
  (values
   (si:*builtin-package-p nil)
   (si:*builtin-package-p t)
   (si:*builtin-package-p 123)
   (si:*builtin-package-p 1.1)
   (si:*builtin-package-p '(:lisp))
   (si:*builtin-package-p :lisp)
   (si:*builtin-package-p "editor")
   (si:*builtin-package-p 'system)
   (si:*builtin-package-p (find-package :user))
   (si:*builtin-package-p "cl-user"))
  => nil
  => nil
  => nil
  => nil
  => nil
  => t
  => t
  => t
  => t
  => t)

(defvar *data/hello.txt.gz* (merge-pathnames "data/hello.txt.gz" (directory-namestring *load-pathname*)))
(deftest inflate-stream ()
  (with-open-file (f *data/hello.txt.gz*
                     :encoding :binary)
    (si:inflate-stream f))
  => "xyzzy\r\n")
