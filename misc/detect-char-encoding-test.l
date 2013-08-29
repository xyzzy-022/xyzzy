#|
IPADIC の単語を detect-char-encoding で判別して正解率をテストする。

テスト実行前に rake -f mkwords.rake で IPADIC から単語ファイルを作成しておく必要がある。
|#

(defun detect-char-encoding-test (expected words-file)
  (long-operation
    (let (result line fail-words (n 0))
      (with-open-file (f words-file)
        (while (setf line (read-line f nil nil))
          (when (zerop (mod n 1000))
            (message "~A ~D" words-file n))
          (incf n)
          (let ((encoding (detect-char-encoding line)))
            (let ((e (assoc encoding result)))
              (if e
                  (incf (cdr e))
                (push (cons encoding 1) result)))
            (unless (eq encoding expected)
              (push line fail-words))
            )))
      (when fail-words
        (with-output-to-temp-buffer (words-file nil)
          (dolist (w fail-words)
            (format t "~@30A~{  ~A~}~%"
                    (convert-encoding-to-internal expected w)
                    (when (fboundp 'guess-char-encoding)
                      (mapcar #'(lambda (pair)
                                  (format nil "~10A (~4,2F)" (char-encoding-name (car pair)) (cdr pair)))
                              (guess-char-encoding w)))
                    ))))
      (with-output-to-buffer ((get-buffer-create "detect-char-encoding-test"))
        (let ((total (apply #'+ (mapcar #'cdr result)))
              (ok (cdr (assoc expected result))))
          (format t "~@30A  ~A/~A (~6,2F%)~%" words-file ok total (* 100.0 (/ ok total))))
        (refresh-screen)))))

(defun detect-char-encoding-test-euc-jp ()
  (interactive)
  (detect-char-encoding-test *encoding-euc-jp* "words.euc-jp.txt"))

(defun detect-char-encoding-test-sjis ()
  (interactive)
  (detect-char-encoding-test *encoding-sjis* "words.sjis.txt"))

(defun detect-char-encoding-test-utf8 ()
  (interactive)
  (detect-char-encoding-test *encoding-utf8n* "words.utf8.txt"))

(defun detect-char-encoding-test-jis ()
  (interactive)
  (detect-char-encoding-test *encoding-jis* "words.jis.txt"))

(defun detect-char-encoding-test-all ()
  (interactive)
  (detect-char-encoding-test-euc-jp)
  (detect-char-encoding-test-sjis)
  (detect-char-encoding-test-utf8)
  (detect-char-encoding-test-jis))
