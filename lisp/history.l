;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "history")

(in-package "editor")

(export '(*save-history* *history-file-name*
	  register-history-variable define-history-variable
	  unregister-history-variable
	  *save-resume-info*
	  *save-history-hook* *load-history-hook*))

(defvar *save-history* 200)

(defvar *history-file-name* nil)
(defvar *resume-file-name-long* ".xyzzy.resume.")
(defvar *resume-file-name-short* "xyzzyrsm.")
(defvar *save-history-hook* nil)
(defvar *load-history-hook* nil)
(defvar *history-file-loaded* nil)

(defvar *minibuffer-history-variables*
  '(*minibuffer-file-name-history*
    *minibuffer-directory-name-history*
    *minibuffer-search-string-history*
    *minibuffer-buffer-name-history*
    *minibuffer-symbol-name-history*
    *minibuffer-lisp-sexp-history*
    *minibuffer-execute-history*
    *minibuffer-complex-command-history*
    *minibuffer-default-history*))

(defvar *history-variable-list* '(*history-variable-list*))
(defvar *save-resume-info* nil)

(defun register-history-variable (var)
  (cond ((listp var)
	 (dolist (v var)
	   (when (symbolp v)
	     (pushnew v *history-variable-list* :test #'eq))))
	((symbolp var)
	 (pushnew var *history-variable-list* :test #'eq))))

(defun unregister-history-variable (var)
  (setq *history-variable-list*
	(delete var *history-variable-list* :test #'eq)))

(defmacro define-history-variable (name &optional (initial-value nil sv) doc)
  `(progn
     ,(if sv
	  `(defvar ,name ,initial-value ,doc)
	`(defvar ,name))
     (register-history-variable ',name)))

(defun history-file-name ()
  (or *history-file-name*
      (setq *history-file-name*
	    (merge-pathnames (if (file-system-supports-long-file-name-p (user-config-path))
				 ".xyzzy.history" "_xyzzy.his")
			     (user-config-path)))))

(defun resume-file-name-base ()
  (if (file-system-supports-long-file-name-p (user-config-path))
      *resume-file-name-long* *resume-file-name-short*))

(defun load-history-file ()
  (let ((vars *history-variable-list*))
    (load (history-file-name) :if-does-not-exist nil :verbose nil :print nil)
    (mapcar #'register-history-variable vars))
  (ignore-errors (run-hooks '*load-history-hook*))
  (setq *history-file-loaded* t)
  (when *save-resume-info*
    (let (tempfile)
      (unwind-protect
	  (progn
	    (setq tempfile (make-temp-file-name))
	    (dolist (file (directory (user-config-path)
				     :wild (concat (resume-file-name-base) "*")
				     :absolute t :file-only t))
	      (when (ignore-errors (rename-file file tempfile :if-exists :overwrite))
		(load tempfile :verbose nil :print nil)
		(return))))
	(and tempfile
	     (ignore-errors (delete-file tempfile)))))))

(defun load-history-symbol-packages (packages)
  (setq packages (remove-if #'find-package packages :key #'car))
  (mapc #'(lambda (package)
	    (make-package (car package)
			  :internal-size (getf (cdr package) :internal-size)
			  :external-size (getf (cdr package) :external-size)))
	packages)
  (mapc #'(lambda (package)
	    (let ((name (pop package)))
	      (unuse-package (package-use-list name) name)
	      (use-package (getf package :use-list) name)))
	packages))

(defvar *predefined-packages* (mapcar #'package-name (si:list-builtin-packages)))

(defun save-history-symbol-packages (stream vars)
  (let ((packages
	 (mapcar #'(lambda (package)
		     (list (package-name package)
			   :internal-size (length (si:*package-internal package))
			   :external-size (length (si:*package-external package))
			   :use-list (mapcar #'package-name (package-use-list package))))
		 (nset-difference (delete-duplicates (mapcar #'symbol-package vars))
				  (mapcar #'find-package *predefined-packages*)))))
    (when packages
      (write `(in-package ,(package-name *package*)) :stream stream :escape t :pretty t :readably t)
      (terpri stream)
      (write `(load-history-symbol-packages ',packages)
	     :stream stream :escape t :circle t :pretty t :readably t)
      (terpri stream))))

(defun save-history-file ()
  (when (and *history-file-loaded*
	     (integerp *save-history*))
    (ignore-errors (run-hooks '*save-history-hook*))
    (let ((*package* *package*))
      (with-open-file (s (history-file-name)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(flet ((put (var value)
		 (unless (eq *package* (symbol-package var))
		   (setq *package* (symbol-package var))
		   (write `(in-package ,(package-name *package*))
			  :stream s :escape t :pretty t :readably t)
		   (terpri s))
		 (write `(set-default ',var ',value)
			:stream s :escape t :circle t :pretty t :readably t)
		 (terpri s)))
	  (save-history-symbol-packages s *history-variable-list*)
	  (mapcar #'(lambda (var)
		      (let* ((his (symbol-value var))
			     (exceed (- (length his) *save-history*)))
			(if (> exceed 0)
			    (setq his (nbutlast his exceed)))
			(put var his)))
		  *minibuffer-history-variables*)
	  (mapcar #'(lambda (var)
		      (when (and (symbolp var)
				 (boundp var))
			(put var (default-value var))))
		  *history-variable-list*)))))
  (when *save-resume-info*
    (let ((base (resume-file-name-base)))
      (dotimes (x 1000)
	(with-open-file (s (merge-pathnames (format nil "~a~a" base x)
					    (user-config-path))
			   :direction :output
			   :if-exists nil
			   :if-does-not-exist :create)
	  (when s
	    (save-session-info s)
	    (return)))))))

(add-hook '*kill-xyzzy-hook* 'save-history-file)


(defun erase-registry-chunk-compat (key name)
  (let ((nkeys (read-registry key name)))
    (write-registry key name nil)
    (when (integerp nkeys)
      (dotimes (i nkeys)
	(write-registry key (format nil "~d:~a" i name) nil)))))

(defun read-registry-chunk-compat (key name)
  (let ((datum (read-registry key name)))
    (cond ((stringp datum)
	   datum)
	  ((integerp datum)
	   (do ((i (- datum 1) (- i 1))
		(r nil))
	       ((minusp i) (apply #'concat r))
	     (let ((d (read-registry key (format nil "~d:~a" i name))))
	       (unless (stringp d)
		 (erase-registry-chunk-compat key name)
		 (return))
	       (push d r))))
	  (t
	   (erase-registry-chunk-compat key name)))))

(defun load-history-file-compat ()
  (let* ((vars *history-variable-list*)
	 (history (read-registry-chunk-compat "Settings" "History")))
    (cond ((stringp history)
	   (with-input-from-string (s history)
	     (load s :verbose nil :print nil)))
	  (t
	   (let ((path (namestring (if (file-system-supports-long-file-name-p "~/")
				       "~/.xyzzy.history" "~/_xyzzy.his"))))
	     (load path :if-does-not-exist nil :verbose nil :print nil))))
    (mapcar #'register-history-variable vars))
  (save-history-file)
  (when (yes-or-no-p "バージョンアップ後、お初の起動ですね?
設定値をファイルに保存しました(たぶん)。
古いレジストリの内容を削除しますか?")
    (si::delete-registry-tree)))
