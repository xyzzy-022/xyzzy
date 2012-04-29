;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "kwd")

(in-package "editor")

(export '(make-keyword-table load-keyword-file make-list-from-keyword-table
	  reload-keyword-file *keyword-load-path*))

(defvar *keyword-load-path* nil)

(or (boundp 'keyword-hash-table)
    (setq-default keyword-hash-table nil))

(defvar *keyword-translate-hash-table*
  (let ((x (make-hash-table)))
    (setf (gethash #\0 x) #x000400)
    (setf (gethash #\1 x) #x000600)
    (setf (gethash #\2 x) #x000800)
    (setf (gethash #\3 x) #x000a00)
    (setf (gethash #\4 x) #x000c00)
    (setf (gethash #\5 x) #x000e00)
    (setf (gethash #\6 x) #x000a02)
    (setf (gethash #\7 x) #x000c02)
    (setf (gethash #\8 x) #x000e02)
    (setf (gethash #\9 x) #x200000)
    (setf (gethash #\a x) #x200400)
    (setf (gethash #\b x) #x200600)
    (setf (gethash #\c x) #x200800)
    (setf (gethash #\d x) #x200a00)
    (setf (gethash #\e x) #x200c00)
    (setf (gethash #\f x) #x200e00)
    (setf (gethash #\g x) #x200a02)
    (setf (gethash #\h x) #x200c02)
    (setf (gethash #\i x) #x200e02)
    (setf (gethash #\S x) #x001a00)
    (setf (gethash #\T x) #x001c00)
    (setf (gethash #\C x) #x001e00)
    x))

(defun make-keyword-table (&optional icase size)
  (make-hash-table :test (if icase #'equalp #'equal) :size size))

(defun parse-keyword-file-attributes (num)
  (loop
    (forward-char 1)
    (let ((c (following-char)))
      (case (and c (char-downcase c))
	(#\b
	 (setq num (logior num #x200000)))
	(#\u
	 (setq num (logior num #x800000)))
	(#\s
	 (setq num (logior num #x1000000)))
	(#\l
	 (setq num (logior num 2)))
	(t
	 (return num))))))

(defun load-keyword-file (name &optional icase table condition)
  (if (listp name)
      (dolist (n name)
	(when n
	  (setf table (or (load-keyword-file-1 n icase table condition)
			  table))))
    (setf table (load-keyword-file-1 name icase table condition)))
  (when table
    (setf (gethash 'keyword-file table) name)
    (setf (gethash 'keyword-condition table) condition))
  table)

(defun load-keyword-file-1 (name &optional icase table condition loading)
  (let (filename)
    (dolist (d *keyword-load-path*
	       (unless (file-exist-p (setq filename
					   (merge-pathnames name (etc-path))))
		 (return-from load-keyword-file-1 nil)))
      (setq filename (merge-pathnames name d))
      (when (file-exist-p filename)
	(return)))
    (let ((num #x400)
	  (entity-ok 0)
	  (valid t)
	  (remove nil)
	  buffer)
      (unwind-protect
	  (save-excursion
	    (setq buffer (create-new-buffer "*load kwd*"))
	    (set-buffer buffer)
	    (setq filename (merge-pathnames filename (etc-path)))
	    (setq loading (cons filename loading))
	    (insert-file filename)
	    (unless table
	      (setq table (make-keyword-table icase (truncate (buffer-lines) 3/4))))
	    (and condition
		 (not (equal condition "*"))
		 (setq condition (compile-regexp (concat (regexp-quote condition)
							 "[ \t]*$")
						 t)))
	    (goto-char (point-min))
	    (while (cond ((looking-for ";")
			  (forward-char 1)
			  (cond ((looking-for "*+")
				 (forward-char 2)
				 (when (or (equal condition "*")
					   (match-keyword-condition condition))
				   (setq valid t remove nil)))
				((looking-for "*--")
				 (forward-char 3)
				 (when (match-keyword-condition condition)
				   (setq valid nil remove t)))
				((looking-for "*-")
				 (forward-char 2)
				 (when (match-keyword-condition condition)
				   (setq valid nil remove nil)))
				((not valid))
				((looking-at "@include[ \t]+\\(?:\"\\(.+\\)\"\\|<\\(.+\\)>\\)$")
				 (let ((include (if (match-string 1)
						    (merge-pathnames (match-string 1) (directory-namestring filename))
						  (merge-pathnames (match-string 2) (etc-path)))))
				   (cond ((not (file-exist-p include))
					  (warn "Keyword file not found: ~A" include))
					 ((member include loading :test #'path-equal)
					  (warn "Include recursion detected: ~S" (cons include loading)))
					 (t
					  (load-keyword-file-1 include icase table condition loading)))))
				((looking-for "*&")
				 (setq entity-ok 4)
				 (setq num (logior num 4)))
				((looking-for "*<")
				 (setq entity-ok 0)
				 (setq num (logandc2 num 4)))
				((looking-for "**")
				 (forward-char 2)
				 (let ((c (following-char)))
				   (when (and c (setq c (digit-char-p c 16)))
				     (setq num (+ (ash c 9) 1))
				     (forward-char 1)
				     (setq c (following-char))
				     (when (and c (setq c (digit-char-p c 16)))
				       (incf num (ash c 17))
				       (setq num (parse-keyword-file-attributes num)))))
				 (setq num (logior num entity-ok)))
				((looking-for "*")
				 (forward-char 1)
				 (let ((c (following-char)))
				   (when (and c (setq c (gethash c *keyword-translate-hash-table*)))
				     (setq num (parse-keyword-file-attributes c))))
				 (setq num (logior num entity-ok))))
			  (forward-line 1))
			 ((eolp)
			  (forward-char 1))
			 ((and (not valid) (not remove))
			  (forward-line 1))
			 (t
			  (let ((keyword (buffer-substring (point)
							   (progn
							     (goto-eol)
							     (point)))))
			    (cond ((and (not valid) remove)
				   (remhash keyword table))
				  (valid
				   (setf (gethash keyword table) num))))
			  (forward-char 1)))))
	(when buffer
	  (delete-buffer buffer)))))
  table)

(defun match-keyword-condition (condition)
  (or (progn
	(skip-chars-forward " \t")
	(eolp))
      (and condition
	   (looking-at condition))))

(defun make-list-from-keyword-table (hash)
  (let ((list nil))
    (when (hash-table-p hash)
      (maphash #'(lambda (x y) (and (stringp x) (push x list))) hash))
    list))

(defun reload-keyword-file (&optional append)
  (interactive "p")
  (or (hash-table-p keyword-hash-table)
      (plain-error "No keyword hash table"))
  (let ((condition (gethash 'keyword-condition keyword-hash-table)))
    (or (load-keyword-file (or (gethash 'keyword-file keyword-hash-table)
			       (plain-error "No keyword file name"))
			   (eq (hash-table-test keyword-hash-table) 'equal)
			   (progn
			     (or append (clrhash keyword-hash-table))
			     keyword-hash-table)
			   condition)
	(plain-error "Keyword file not found")))
  (refresh-screen t))
