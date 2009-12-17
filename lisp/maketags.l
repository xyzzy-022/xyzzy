;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "maketags")

(in-package "editor")

(export '(make-tags-file *maketags-list* make-tags-file-dialog))

(defvar *maketags-list* nil)

(defun make-tags-file (dir &optional depth)
  (interactive "DMake TAGS file: \np")
  (when (and (interactive-p)
	     (eq *prefix-args* 'universal-argument))
    (setq depth 0))
  (long-operation
    (let ((output (append-trail-slash (namestring dir)))
	  (files nil))
      (dolist (l *maketags-list*)
	(setq files (nconc files
			   (mapcar #'(lambda (x) (cons (car l) x))
				   (sort (directory dir
						    :absolute t
						    :depth depth
						    :recursive (numberp depth)
						    :file-only t
						    :wild (cdr l))
					 #'string<)))))
      (save-excursion
	(let (temp-buffer)
	  (unwind-protect
	      (with-output-to-temp-buffer ("*MAKE-XTAGS*" nil)
		(setq temp-buffer (buffer-stream-buffer *standard-output*))
		(let ((total-files (length files))
		      (current-file 0)
		      (count 0)
		      (buffer nil))
		  (unwind-protect
		      (progn
			(setq buffer (create-new-buffer "XTAGS"))
			(set-buffer buffer)
			(dolist (file files)
			  (erase-buffer buffer)
			  (insert-file-contents (cdr file) t)
			  (setq current-file (1+ current-file))
			  (setq count (funcall (car file)
					       (if (eql (string-not-equal output (cdr file))
							(length output))
						   (substring (cdr file) (length output))
						 (cdr file))
					       count))
			  (message "Parsing ~d (~d of ~d files) ~a..."
				   count current-file total-files (cdr file))))
		    (and buffer (delete-buffer buffer))))
		(set-buffer temp-buffer)
		(goto-char (point-min))
		(perform-replace "[\n\t\f]" " " nil t nil t)
		(goto-char (point-min))
		(perform-replace "  +" " " nil t nil t)
		(goto-char (point-min))
		(perform-replace "\036" "\n" nil nil nil t)
		(write-file (merge-pathnames *tags-file-name* output))
		(when (numberp depth)
		  (mapc #'(lambda (d)
			    (unless (= (length d) (length output))
			      (erase-buffer temp-buffer)
			      (insert "#USE ")
			      (dotimes (i (count #\/ d :start (length output)))
				(insert "../"))
			      (insert "\n")
			      (write-file (merge-pathnames *tags-file-name* d) t)
			      (let ((buffer (find-tags-buffer d)))
				(and buffer (delete-buffer buffer)))))
			(delete-duplicates (mapcar #'(lambda (x)
						       (directory-namestring (cdr x)))
						   files)
					   :test #'string-equal))))
	    (and temp-buffer (delete-buffer temp-buffer))))
	(let ((buffer (find-tags-buffer output)))
	  (and buffer (delete-buffer buffer)))))
    t))

(defvar *make-tags-file-dialog-template*
  '(dialog 0 0 226 81
    (:caption "タグファイルを作る")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "ソースディレクトリ(&S):" #x50020000 7 7 58 8)
     (:edit src nil #x50810080 7 19 148 14)
     (:button subdir "ついでにサブディレクトリも(&U)" #x50010003 7 48 102 10)
     (:static nil "検索する階層の深さ(&D):" #x50020000 9 63 80 8)
     (:edit depth nil #x50810086 89 60 36 14)
     (:spin depthspin "Spin1" #x500000b6 119 55 9 14)
     (:static nil "(0は無制限)" #x50020000 129 63 48 8)
     (:button IDOK "OK" #x50010001 169 7 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 169 24 50 14)
     (:button ref "参照(&R)..." #x50010000 169 41 50 14))))

(define-history-variable *make-tags-file-depth* 0)
(define-history-variable *make-tags-file-subdir* nil)

(defun make-tags-file-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box *make-tags-file-dialog-template*
		  (list (cons 'src (or (and (file-exist-p (merge-pathnames *tags-file-name* (default-directory)))
					    (with-open-file (s (merge-pathnames *tags-file-name* (default-directory)))
					      (when (string-matchp "#USE \\(\\(../\\)+\\)$" (read-line s))
						(append-trail-slash (merge-pathnames (match-string 1) (default-directory))))))
				       (default-directory)))
			(cons 'subdir *make-tags-file-subdir*)
			(cons 'depthspin
			      (if *make-tags-file-subdir*
				  *make-tags-file-depth*
				0)))
		  '((src :non-null "ソースディレクトリを入力して" :enable (IDOK))
		    (depth :type integer :min 0 :max 10000
			   :range-error "階層の深さ0以上です"
			   :type-error "数値を入力してください")
		    (depthspin :min 0 :max 10000)
		    (ref :related src :directory-name-dialog (:title "参照"))))
    (when result
      (setq *make-tags-file-subdir* (cdr (assoc 'subdir data)))
      (setq *make-tags-file-depth* (cdr (assoc 'depth data)))
      (make-tags-file (cdr (assoc 'src data))
		      (and *make-tags-file-subdir*
			   *make-tags-file-depth*)))))
