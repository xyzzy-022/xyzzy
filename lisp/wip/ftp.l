
(defconstant *ftp-preliminary* 1)
(defconstant *ftp-complete* 2)
(defconstant *ftp-continue* 3)
(defconstant *ftp-transient* 4)
(defconstant *ftp-error* 5)

(defconstant *ftp-type-ascii* "A")
(defconstant *ftp-type-image* "I")

(defconstant *telnet-DM* (code-char 242))
(defconstant *telnet-IP* (code-char 244))
(defconstant *telnet-WILL* (code-char 251))
(defconstant *telnet-WONT* (code-char 252))
(defconstant *telnet-DO* (code-char 253))
(defconstant *telnet-DONT* (code-char 254))
(defconstant *telnet-IAC* (code-char 255))

(defvar *ftp-passive-mode* nil)

(defstruct ftp
  stream
  reply-code
  reply-text
  (passive-mode *ftp-passive-mode*)
  (type *ftp-type-ascii*)
  (quit-sent nil))

(setf (get 'with-ftp-stream 'ed:lisp-indent-hook) 1)

(defmacro with-ftp-stream ((ftp host &optional (port "ftp")) &body body)
  `(let ((,ftp (ftp-connect ,host ,port)))
     (with-open-stream (#1=#:SO (ftp-stream ,ftp))
       (unwind-protect
	   (progn
	     (socket-stream-set-oob-inline #1# t)
	     ,@body)
	 (ftp-quit ,ftp)))))

(defun getpass (&optional (prompt "Password: "))
  (let ((pw (make-vector 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (minibuffer-prompt "~A~v@{~a~:*~}" prompt (length pw) #\*)
      (let ((c (read-char *keyboard*)))
	(case c
	  (#\RET
	   (return pw))
	  (#\C-g
	   (quit))
	  (#\C-q
	   (vector-push-extend (read-char *keyboard*) pw))
	  (#\C-h
	   (or (zerop (length pw))
	       (vector-pop pw)))
	  (t
	   (vector-push-extend c pw)))))))

(defun ftp-abort (ftp ds)
  (socket-stream-send-oob-data (ftp-stream ftp)
			       (format nil "~c~c~c" *telnet-IAC* *telnet-IP* *telnet-IAC*))
  (format (ftp-stream ftp) "~cABOR~%" *telnet-DM*)
  (princ "S: ABOR\n")
  (finish-output (ftp-stream ftp))
  (when ds
    (let ((s (make-vector 256 :element-type 'character :fill-pointer 0)))
      (while (read-into s ds nil))))
  (ftp-get-reply ftp)
  (and (= (ftp-reply-code ftp) 552)
       (ftp-get-reply ftp))
  (ftp-get-reply ftp))

(defun ftp-read-line (stream)
  (let ((r (make-vector 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((c (read-char stream nil)))
	(cond ((or (null c)
		   (eql c #\LFD))
	       (return (unless (zerop (length r)) r)))
	      ((eql c *telnet-IAC*)
	       (setq c (read-char stream nil))
	       (cond ((or (eql c *telnet-WILL*)
			  (eql c *telnet-WONT*))
		      (format stream "~c~c~c"
			      *telnet-IAC* *telnet-DONT*
			      (read-char stream nil *telnet-IAC*)))
		     ((or (eql c *telnet-DO*)
			  (eql c *telnet-DONT*))
		      (format stream "~c~c~c"
			      *telnet-IAC* *telnet-WONT*
			      (read-char stream nil *telnet-IAC*)))))
	      (t
	       (vector-push-extend c r)))))))

(defun ftp-get-reply (ftp &optional eof-ok)
  (let* ((code nil)
	 (r (let ((r (ftp-read-line (ftp-stream ftp))))
	      (format t "R: ~A~%" r)
	      (cond ((null r) r)
		    ((string-match "^[1-9][0-9][0-9] " r) r)
		    ((string-match "^[1-9][0-9][0-9]-" r)
		     (loop
		       (let ((x (ftp-read-line (ftp-stream ftp))))
			 (format t "R: ~A~%" x)
			 (unless x
			   (return nil))
			 (if (and (>= (length x) 4)
				  (string= x r :end1 3 :end2 3)
				  (char= (char x 3) #\SPC))
			     (return r)))))
		    (t
		     (setq code *ftp-error*)
		     r)))))
    (or r (setq r (if eof-ok
		      "221 Service closing control connection"
		    "421 Service not available, closing control connection")))
    (setf (ftp-reply-code ftp) (or (ignore-errors (parse-integer r :end 3)) -1))
    (setf (ftp-reply-text ftp) (substring r 4))
    (or code (parse-integer r :end 1))))

(defun ftp-command (ftp cmd &rest args)
  (format (ftp-stream ftp) "~?~%" cmd args)
  (if (and (>= (length cmd) 5)
	   (string= cmd "PASS " :end1 5))
      (princ "S: PASS *****\n")
    (format t "S: ~?~%" cmd args))
  (ftp-get-reply ftp (equal cmd "QUIT")))

(defun ftp-alt-command (ftp cmd1 cmd2 &rest args)
  (let ((r (apply #'ftp-command ftp cmd1 args)))
    (if (/= (ftp-reply-code ftp) 500)
	r
      (apply #'ftp-command ftp cmd2 args))))

(defun ftp-connect (host port)
  (let ((ftp (make-ftp)))
    (setf (ftp-stream ftp) (connect host port))
    (when (> (ftp-get-reply ftp) *ftp-complete*)
      (error "Connect failed"))
    ftp))

(defun ftp-login (ftp &optional user pass acct)
  (let ((af nil)
	r)
    (or user (setq user (read-string "Name: ")))
    (setq r (ftp-command ftp "USER ~A" user))
    (when (= r *ftp-continue*)
      (or pass (setq pass (getpass)))
      (setq r (ftp-command ftp "PASS ~A" pass)))
    (when (= r *ftp-continue*)
      (setq af t)
      (or acct (setq acct (getpass "Account: ")))
      (setq r (ftp-command ftp "ACCT ~A" acct)))
    (unless (= r *ftp-complete*)
      (error "Login failed"))
    (when (and (null af) acct)
      (ftp-command ftp "ACCT ~A"acct)))
  (values user pass acct))

(defun ftp-quit (ftp)
  (unless (ftp-quit-sent ftp)
    (ftp-command ftp "QUIT")
    (setf (ftp-quit-sent ftp) t))
  (close (ftp-stream ftp))
  t)

(defun ftp-set-type (ftp type)
  (when (= (ftp-command ftp "TYPE ~A" type) *ftp-complete*)
    (setf (ftp-type ftp) type)))

(defun ftp-ascii (ftp)
  (ftp-set-type ftp *ftp-type-ascii*))

(defun ftp-binary (ftp)
  (ftp-set-type ftp *ftp-type-image*))

(defun ftp-cd (ftp dir)
  (= (ftp-alt-command ftp "CWD ~A" "XCWD ~A" dir) *ftp-complete*))

(defun ftp-cdup (ftp)
  (= (ftp-alt-command ftp "CDUP" "XCUP") *ftp-complete*))

(defun ftp-chmod (ftp mode filename)
  (= (ftp-command ftp "SITE CHMOD ~A ~A" mode filename) *ftp-complete*))

(defun ftp-umask (ftp &optional umask)
  (= (ftp-command ftp "SITE UMASK~@[ ~A~]" umask) *ftp-complete*))

(defun ftp-delete (ftp filename)
  (= (ftp-command ftp "DELE ~A" filename) *ftp-complete*))

(defun ftp-mkdir (ftp dir)
  (= (ftp-alt-command ftp "MKD ~A" "XMKD ~A" dir) *ftp-complete*))

(defun ftp-mtime (ftp filename)
  (when (= (ftp-command ftp "MDTM ~A" filename) *ftp-complete*)
    (let ((text (ftp-reply-text ftp)))
      (and (string-match "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
			 text)
	   (encode-universal-time (parse-integer (substring text (match-beginning 6) (match-end 6)))
				  (parse-integer (substring text (match-beginning 5) (match-end 5)))
				  (parse-integer (substring text (match-beginning 4) (match-end 4)))
				  (parse-integer (substring text (match-beginning 3) (match-end 3)))
				  (parse-integer (substring text (match-beginning 2) (match-end 2)))
				  (parse-integer (substring text (match-beginning 1) (match-end 1)))
				  0)))))

(defun ftp-pwd (ftp)
  (and (= (ftp-alt-command ftp "PWD" "XPWD") *ftp-complete*)
       (string-match "\"\\([^\"]+\\)\"" (ftp-reply-text ftp))
       (substring (ftp-reply-text ftp) (match-beginning 1) (match-end 1))))

(defun ftp-system (ftp)
  (= (ftp-command ftp "SYST") *ftp-complete*))

(defun ftp-size (ftp filename)
  (and (= (ftp-command ftp "SIZE ~A" filename) *ftp-complete*)
       (string-match "[0-9]+" (ftp-reply-text ftp))
       (parse-integer (substring (ftp-reply-text ftp) (match-beginning 0) (match-end 0)))))

(defun ftp-rename (ftp from to)
  (and (= (ftp-command ftp "RNFR ~A" from) *ftp-continue*)
       (= (ftp-command ftp "RNTO ~A" to) *ftp-complete*)))

(defun ftp-init-data-stream (ftp)
  (if (ftp-passive-mode ftp)
      (progn
	(unless (= (ftp-command ftp "PASV") *ftp-complete*)
	  (error "Passive mode refused"))
	(let ((text (ftp-reply-text ftp)))
	  (unless (string-match "(\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\))"
				text)
	    (error "Passive mode address scan failure"))
	  (connect (concat (substring text (match-beginning 1) (match-end 1))
			   "." (substring text (match-beginning 2) (match-end 2))
			   "." (substring text (match-beginning 3) (match-end 3))
			   "." (substring text (match-beginning 4) (match-end 4)))
		   (+ (* (parse-integer (substring text (match-beginning 5) (match-end 5))) 256)
		      (parse-integer (substring text (match-beginning 6) (match-end 6)))))))
    (let ((abort t)
	  so)
      (unwind-protect
	  (progn
	    (setq so (make-listen-socket (socket-stream-local-address (ftp-stream ftp)) nil
					 :backlog 1))
	    (let ((addr (socket-stream-local-address so))
		  (port (socket-stream-local-port so)))
	      (unless (and (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" addr)
			   (= (ftp-command ftp "PORT ~A,~A,~A,~A,~A,~A"
					   (substring addr (match-beginning 1) (match-end 1))
					   (substring addr (match-beginning 2) (match-end 2))
					   (substring addr (match-beginning 3) (match-end 3))
					   (substring addr (match-beginning 4) (match-end 4))
					   (truncate port 256)
					   (rem port 256))
			      *ftp-complete*))
		(error "PORT command failed")))
	    (setq abort nil)
	    so)
	(when abort
	  (close so :abort t))))))

(defun ftp-connect-data-stream (ftp so)
  (if (ftp-passive-mode ftp)
      so
    (accept-connection so)))

(setf (get 'with-ftp-data-stream 'ed:lisp-indent-hook) 1)

(defmacro with-ftp-data-stream ((ftp so cmd &rest args) &body body)
  `(with-open-stream (#1=#:S (ftp-init-data-stream ,ftp))
     (when (= (funcall #'ftp-command ,ftp ,cmd ,@args) *ftp-preliminary*)
       (with-open-stream (,so (ftp-connect-data-stream ,ftp #1#))
	 (handler-case
	     (progn ,@body)
	   (error (c)
	     (ftp-abort ftp ,so)
	     (signal c))
	   (quit (c)
	     (ftp-abort ftp ,so)
	     (signal c)))))))

(defun ftp-list (ftp cmd pathname)
  (unless (eq (ftp-type ftp) *ftp-type-ascii*)
    (ftp-ascii ftp))
  (let ((list nil) r)
    (with-ftp-data-stream (ftp so "~A~@[ ~A~]" cmd pathname)
      (while (setq r (read-line so nil))
	(push r list)))
    (ftp-get-reply ftp)
    (nreverse list)))

(defun ftp-ls (ftp &optional pathname)
  (ftp-list ftp "LIST" pathname))

(defun ftp-nlist (ftp &optional pathname)
  (ftp-list ftp "NLST" pathname))

(defun ftp-copy-stream (is os)
  (let ((s (make-vector 4096 :element-type 'character :fill-pointer 0))
	(bytes 0))
    (while (read-into s is nil)
      (princ s os)
      (incf bytes (length s))
      (message "~d bytes" bytes))))

(defun ftp-get (ftp filename os)
  (unless (eq (ftp-type ftp) *ftp-type-image*)
    (ftp-binary ftp))
  (with-ftp-data-stream (ftp is "RETR ~A" filename)
    (set-stream-encoding is :binary)
    (set-stream-encoding os :binary)
    (ftp-copy-stream is os)))

(defun ftp-put (ftp is filename)
  (unless (eq (ftp-type ftp) *ftp-type-image*)
    (ftp-binary ftp))
  (with-ftp-data-stream (ftp os "STOR ~A" filename)
    (set-stream-encoding is :binary)
    (set-stream-encoding os :binary)
    (ftp-copy-stream is os)))
