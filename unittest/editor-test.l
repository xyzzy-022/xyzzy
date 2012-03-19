(deftest fix-edict-lookup-word ()
  "’PŒê‹«ŠE‚Å edict-lookup-word ‚µ‚½‚ç‚»‚Ì’PŒê‚ğ•Ô‚·"
  (with-temp-buffer
    (insert "I  like   susi.")
    (flet ((test (point)
             (and (goto-char point)
                  (ed::edict-lookup-word #'buffer-substring))))
      (values (test 0) (test 1)
              (test 2) (test 3) (test 4) (test 7)
              (test 8))))
  => "I"
  => "I"
  => "like"
  => "like"
  => "like"
  => "like"
  => "susi")
