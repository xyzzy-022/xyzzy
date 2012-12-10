(deftest test-print-debug-output-stream ()
  (string-match "debug-output-stream" (format nil "~A" *debug-output*))
  => non-nil)

(deftest test-output-debug-string-error-nil ()
  (si:output-debug-string nil)
  !! type-error)

(deftest test-output-debug-string-error-number ()
  (si:output-debug-string 123)
  !! type-error)

(deftest test-output-debug-string-error-symbol ()
  (si:output-debug-string :123)
  !! type-error)

(deftest write-debug-output-stream ()
  ;; メッセージの確認ができないので出力するだけ
  (si:output-debug-string "write-debug-output-stream")
  (si:output-debug-string "a")
  (si:output-debug-string "")
  (si:output-debug-string "write-debug-output-stream: ~A" :fuga)
  (format *debug-output* "write-debug-output-stream")
  (format *debug-output* "write-debug-output-stream: ~A" :fuga)
  (write-char #\a *debug-output*)
  (write-char #\NUL *debug-output*)
  (let ((*standard-output* *debug-output*))
    (format t "write-debug-output-stream"))
  nil
  => nil)

(deftest test-debug-output-stream-p ()
  (values
   (debug-output-stream-p *debug-output*)
   (debug-output-stream-p *status-window*)
   (let ((*standard-output* *debug-output*))
     (debug-output-stream-p *standard-output*)))
  => t
  => nil
  => t)
