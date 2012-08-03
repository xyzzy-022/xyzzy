(deftest test-buffer-substring--buffer ()
  (values
   #1=(with-temp-buffer
        (insert "hoge")
        (let ((b (selected-buffer)))
          (with-temp-buffer
            (insert "fuga")
            (list
             (buffer-substring 0 4)
             (buffer-substring 1 3 nil)
             (buffer-substring 0 4 b)
             (buffer-substring 1 3 b)
             (buffer-substring 3 2 (buffer-name b))
             ))))
   ;; buffer-substring は 2 引数の専用のバイトコードが用意されている。
   ;; 3 引数の場合は通常の関数呼び出しとしてコンパイルされる。
   (compile-and-eval
     #1#))
  => #2=("fuga" "ug" "hoge" "og" "g")
  => #2#)
