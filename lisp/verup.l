;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "verup")

(in-package "editor")

(export 'version-up-xyzzy)

(defun version-up-xyzzy (filename)
  (interactive
      (let ((ignored-extensions
	     (remove ".lzh" ignored-extensions :test #'string-equal)))
	(list (read-exist-file-name "xyzzy LZH file: "))))
  (launch-application (format nil "\"~a\" e -a1m1x1 \"~a\" \"~a\""
			      (merge-pathnames "extract.exe" (etc-path))
			      filename
			      (append-trail-slash
			       (merge-pathnames ".." (si:system-root))))))

