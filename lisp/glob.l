;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "glob")

(in-package "editor")

(export '(glob glob-expand glob-expand-list glob-exec glob-common-length
	  glob-common-path glob-enumerator))

(defun glob-expand (path &key test)
  (let ((prefix ""))
    (setq path (namestring path))
    (when (string-match "^\\(.:\\|//[^/]+/[^/]+\\)?/" path)
      (setq prefix (subseq path 0 (match-end 0)))
      (setq path (subseq path (match-end 0))))
    (or (listp prefix)
	(setq prefix (list prefix)))
    (do ((p (split-string path #\/) (cdr p)))
	((or (null p) (null prefix))
	 prefix)
      (let ((dir (car p))
	    (r nil))
	(mapc #'(lambda (path)
		  (setq r (nconc r (mapcar #'(lambda (file)
					       (concat path file))
					   (directory path
						      :wild dir
						      :test test
						      :directory-only t)))))
	      prefix)
	(setq prefix r)))))

(defun glob-expand-list (list &key test)
  (apply #'append (mapcar #'(lambda (path)
			      (if (file-exist-p path)
				  (list (namestring path))
				(glob-expand path :test test)))
			  list)))

(defun glob-subseq (d l)
  (let ((slash (position #\/ d :from-end t)))
    (if slash
	(incf slash)
      (setq slash 0))
    (when (and (/= slash (length d))
	       (file-directory-p d))
      (setq d (concatenate 'string d "/"))
      (setq slash (length d)))
    (subseq d (min l slash) slash)))

(defun glob-exec (paths &rest rest &key absolute callback test)
  (cond ((null paths)
	 nil)
	(absolute
	 (if callback
	     (mapc #'(lambda (d) (apply #'directory d rest)) paths)
	   (apply #'append (mapcar #'(lambda (d) (apply #'directory d rest))
				   paths))))
	(t
	 (let ((l (glob-common-length paths)))
	   (if callback
	       (mapc #'(lambda (d)
			 (let ((base (glob-subseq d l)))
			   (apply #'directory d
				  :callback #'(lambda (name)
						(funcall callback
							 (concatenate 'string
								      base name)))
				  rest)))
		     paths)
	     (apply #'append
		    (mapcar #'(lambda (d)
				(let ((base (glob-subseq d l)))
				  (mapcar #'(lambda (name)
					      (concatenate 'string base name))
					  (apply #'directory d rest))))
			    paths)))))))

(defun glob (path &rest rest)
  (apply #'glob-exec (glob-expand path) rest))

(defun glob-common-length (dirs)
  (if dirs
      (let* ((s (car dirs))
	     (l (length s)))
	(dolist (x (cdr dirs) (if (string-match "/[^/]*$" s 0 l)
				  (1+ (match-beginning 0))
				0))
	  (setq l (min l (or (string-not-equal s x) l)))))
    0))

(defun glob-common-path (dirs)
  (subseq (car dirs) 0 (glob-common-length dirs)))

(defun glob-enumerator (dirlist wild recursive-p &optional empty-ok test)
  (let ((files nil))
    #'(lambda ()
	(loop
	  (when files
	    (return (pop files)))
	  (let* ((dir (pop dirlist))
		 (dot (equal dir ".")))
	    (unless dir
	      (return nil))
	    (setq files (directory dir :wild wild :file-only t :test test))
	    (let ((dirp (file-directory-p dir)))
	      (if dirp
		  (or (equal dir "")
		      (setq dir (append-trail-slash dir)))
		(let ((slash (position #\/ dir :from-end t)))
		  (setq dir (if slash (subseq dir 0 (1+ slash)) ""))))
	      (unless dot
		(setq files (mapcar #'(lambda (f) (concat dir f)) files)))
	      (when (and dirp recursive-p)
		(let ((dirs (directory dir :directory-only t :test test)))
		  (setq dirlist (nconc (if dot
					   dirs
					 (mapcar #'(lambda (f) (concat dir f)) dirs))
				       dirlist)))))
	    (when (and empty-ok (null files))
	      (return t)))))))
