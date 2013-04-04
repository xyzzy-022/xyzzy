;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "about")

(in-package "editor")

(export 'about-dialog)

(let ((dialog-template nil))
  (defun about-dialog ()
    (interactive)
    (unless dialog-template
      (setq dialog-template
	    `(dialog 0 0 211 148
		     (:caption "‚É‚Â‚¢‚Ä")
		     (:font 9 "MS UI Gothic")
		     (:control
		      (:button IDOK "‚¨‚¯" #x50010001 171 7 33 14)
		      (:static nil nil #x50020003 7 7 18 20)
		      (:link xyzzy ,(concat "\001xyzzy\002 version "
					    (software-version-display-string))
		       #x50020000 34 7 123 8)
		      (:static nil "Copyright (C) 1996-2005 Tetsuya Kamei"
		       #x50020000 34 19 132 8)
		      (:link org "Copyright (C) 2012-2013 \001xyzzy Project\002"
		       #x50020000 34 28 132 8)
		      (:static nil "Icon designed by Yoshinori Watanabe."
		       #x50020000 34 41 126 8)
		      (:static nil "RSA Data Security, Inc." #x50020000 34 53 83 8)
		      (:static nil "MD5 Message-Digest Algorithm." #x50020000 34 62 111 8)
		      (:listbox list nil #x50a10111 33 73 138 68)))))
    (dialog-box dialog-template
		(list (cons 'list
			    (long-operation
			      (mapcan #'(lambda (x)
					  (let ((version (archiver-dll-version x)))
					    (and version
						 (list (list (get x 'archiver-dll-file-name)
							     version)))))
				      *archiver-dll-list*))))
		'((xyzzy :url "http://xyzzy-022.github.com")
		  (org :url "https://github.com/xyzzy-022")
		  (list :column (28 10))))
    t))
