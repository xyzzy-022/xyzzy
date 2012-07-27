(defun execute-shell-command-string (command)
  (let ((b (create-new-buffer "*command*")))
    (unwind-protect
        (save-window-excursion
          (execute-shell-command command nil b)
          (set-buffer b)
          (buffer-substring (point-min) (point-max)))
      (delete-buffer b))))

(defun my-environ ()
  (mapcar #'(lambda (kv)
              (let ((pos (position #\= kv)))
                (cons (subseq kv 0 pos)
                      (subseq kv (1+ pos)))))
          (split-string (execute-shell-command-string "cmd.exe /c set") #\LFD)))

(deftest test-environ-1 ()
  (equal (sort (my-environ) #'string< :key 'car)
         (sort (si:environ) #'string< :key 'car))
  => t)

(deftest test-environ-2 ()
  (si:putenv "test" "hoge")
  (unwind-protect
      (equal (sort (my-environ) #'string< :key 'car)
             (sort (si:environ) #'string< :key 'car))
    (si:putenv "test" nil))
  => t)
