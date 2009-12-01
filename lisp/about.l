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
	    `(dialog 0 0 211 139
		     (:caption "‚É‚Â‚¢‚Ä")
		     (:font 9 "MS UI Gothic")
		     (:control
		      (:button IDOK "‚¨‚¯" #x50010001 171 7 33 14)
		      (:static nil nil #x50020003 7 7 18 20)
		      (:link xyzzy ,(concat "\001xyzzy\002 version "
					    (software-version-display-string))
		       #x50020000 34 7 123 8)
		      (:link mail "Copyright (C) 1996-2005 \001Tetsuya Kamei\002"
		       #x50020000 34 19 132 8)
		      (:static nil "Icon designed by Yoshinori Watanabe."
		       #x50020000 34 32 126 8)
		      (:static nil "RSA Data Security, Inc." #x50020000 34 44 83 8)
		      (:static nil "MD5 Message-Digest Algorithm." #x50020000 34 53 111 8)
		      (:listbox list nil #x50a10111 33 64 138 68)))))
    (dialog-box dialog-template
		(list (cons 'list
			    (long-operation
			      (mapcan #'(lambda (x)
					  (let ((version (archiver-dll-version x)))
					    (and version
						 (list (list (get x 'archiver-dll-file-name)
							     version)))))
				      *archiver-dll-list*))))
		'((xyzzy :url "http://www.jsdlab.co.jp/~kamei/")
		  (mail :url "mailto:kamei@jsdlab.co.jp")
		  (list :column (28 10))))
    t))
