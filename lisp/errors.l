;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "errors")

(in-package "editor")

(export '(first-error next-error *parse-errors-at-once* *highlight-error*
	  *error-window-size* *error-regexp-list*
	  *find-errors-find-buffers*))

(defvar *parse-errors-at-once* nil)
(defvar *highlight-error* t)

(defvar *error-list* nil)
(defvar *rerror-list* nil)
(defvar *max-error-list* 100)
(defvar *error-buffer* nil)
(defvar *last-error* nil)
(defvar *nerrors-found* nil)
(defvar *error-window-size* 4)
(defvar *find-errors-find-buffers* nil)

(defvar-local *find-error-scanner* nil)

(defun find-errors-get-buffer (name later)
  (setq name (string-trim " \t" name))
  (let ((file (substitute-string name "[/\\]+" "/"))
	buffer)
    (cond ((equal (car *last-error*) file)
	   (cdr *last-error*))
	  ((and later (null *parse-errors-at-once*))
	   t)
	  ((setq buffer (if (string-match "^<.+>$" name)
			    (find-buffer (subseq name 1 (- (length name) 1)))
			  (and (file-exist-p file)
			       (or (get-file-buffer file)
				   (save-excursion
				     (find-file file nil t)
				     (selected-buffer))))))
	   (setq *last-error* (cons file buffer))
	   buffer)
	  ((and *find-errors-find-buffers*
		(setq buffer (find-buffer name)))
	   (setq *last-error* (cons file buffer))
	   buffer)
	  (t nil))))

(defvar *error-regexp-list*
  (list
   (list (compile-regexp "^\\(.+\\)[ \t]*(\\([0-9]+\\))") 1 2)
   (list (compile-regexp "^\\(.+\\)[:：][ \t]*\\([0-9]+\\)\\([:： \t]\\|$\\)") 1 2)
   (list (compile-regexp "^[^ \n]+ \\(.+\\) \\([0-9]+\\):") 1 2)
   (list (compile-regexp "^[^ \n]+ [^ \n]+ \\(.+\\) \\([0-9]+\\):") 1 2)))

(defun find-errors (later)
  (let ((opoint (point))
	buffer file line file-regnum line-regnum)
    (goto-bol)
    (loop
      (dolist (x *error-regexp-list*)
	(let ((regexp (pop x)))
	  (when (looking-at regexp)
	    (let ((file-regnum (pop x))
		  (line-regnum (pop x)))
	      (setq line (parse-integer (match-string line-regnum)))
	      (setq file (match-string file-regnum))
	      (setq buffer (find-errors-get-buffer file later))
	      (while (and (not (bufferp buffer))
			  (string-match regexp file))
		(setq line (parse-integer (subseq file
						  (match-beginning line-regnum)
						  (match-end line-regnum))))
		(setq file (subseq file (match-beginning file-regnum)
				   (match-end file-regnum)))
		(setq buffer (find-errors-get-buffer file later))))
	    (when buffer
	      (unless (bufferp buffer)
		(return-from find-errors nil))
	      (save-excursion
		(set-buffer buffer)
		(goto-line line)
		(let ((marker (make-marker)))
		  (set-marker marker)
		  (return-from find-errors marker)))))))
      (unless (forward-line 1)
	(goto-char opoint)
	(return)))))

#|
(defvar *error-regexp*
  (compile-regexp
   "^[ \t]*\\(.+\\)[:：][ \t]*\\([0-9]+\\)\\([:： \t]\\|$\\)\\|^\\(.+\\)[ \t]*(\\([0-9]+\\))"))

(defun error-regexp-regnum ()
  (if (match-beginning 1) (values 1 2) (values 4 5)))

(defvar *error-regexp-regnum* #'error-regexp-regnum)

(defun find-errors (later)
  (let (buffer file line file-regnum line-regnum)
    (goto-bol)
    (while (scan-buffer *error-regexp*)
      (multiple-value-setq (file-regnum line-regnum)
	(funcall *error-regexp-regnum*))
      (setq file (match-string file-regnum))
      (setq line (parse-integer (match-string line-regnum)))
      (setq buffer (find-errors-get-buffer
		    (substitute-string file "[/\\]+" "/") later))
      (while (and (not (bufferp buffer))
		  (string-match *error-regexp* file))
	(multiple-value-setq (file-regnum line-regnum)
	  (funcall *error-regexp-regnum*))
	(setq line (parse-integer (subseq file
					  (match-beginning line-regnum)
					  (match-end line-regnum))))
	(setq file (subseq file (match-beginning file-regnum) (match-end file-regnum)))
	(setq buffer (find-errors-get-buffer file later)))
      (when buffer
	(unless (bufferp buffer)
	  (return))
	(save-excursion
	  (set-buffer buffer)
	  (goto-line line)
	  (let ((marker (make-marker)))
	    (set-marker marker)
	    (return marker))))
      (unless (forward-line 1)
	(return)))))
|#

(defun build-error-list-1 ()
  (long-operation
    (message "Parsing error messages...")
    (let (errors percent opercent (nerrors 0))
      (loop
	(let ((marker (find-errors errors)))
	  (or marker
	      (return))
	  (push (cons marker (current-line-number)) errors))
	(incf *nerrors-found*)
	(incf nerrors)
	(when (<= *max-error-list* nerrors)
	  (return))
	(setq percent (truncate (* 100 (point)) (point-max)))
	(unless (and opercent (eql percent opercent))
	  (setq opercent percent)
	  (message "Parsing error messages...~d (~d% of buffer)"
		   *nerrors-found* percent))
	(or (forward-line 1)
	    (return)))
      (setq *error-list* (nreverse errors)))
    (message "Parsing error messages...done")))

(defun build-error-list ()
  (setq *last-error* nil)
  (setq *nerrors-found* 0)
  (build-error-list-1)
  (setq *rerror-list* nil)
  (setq *error-buffer* (selected-buffer)))

(defun rebuild-error-list ()
  (message "Parsing error messages...")
  (let (errors)
    (save-excursion
      (set-buffer *error-buffer*)
      (goto-line (cdr (car *rerror-list*)))
      (forward-line 1)
      (build-error-list-1))))

(defun goto-error (next)
  (when (and next *error-list*)
    (setq *rerror-list* (cons (car *error-list*) *rerror-list*))
    (setq *error-list* (cdr *error-list*)))
  (when (and *rerror-list*
	     (null *error-list*))
    (rebuild-error-list))
  (when *error-list*
    (let ((error (car *error-list*)))
      (if (= (count-windows 'arg) 1)
	  (progn
	    (split-window *error-window-size*)
	    (set-buffer *error-buffer*))
	(pop-to-buffer *error-buffer*))
      (goto-line (cdr error))
      (reverse-region (progn
			(goto-eol)
			(point))
		      (progn
			(goto-bol)
			(point)))
      (recenter)
      (let ((scanner (and *highlight-error* *find-error-scanner*)))
	(other-window)
	(set-buffer (marker-buffer (car error)))
	(goto-marker (car error))
	(and scanner
	     (funcall scanner (save-excursion (goto-eol) (point)))
	     (eq *highlight-error* t)
	     (show-match)))
      (recenter)
      t)))

(defun first-error (&optional arg wrap)
  (interactive "p")
  (if (or (and arg (eq *error-buffer* (selected-buffer)))
	  wrap)
      (progn
	(dolist (x *rerror-list*)
	  (setq *error-list* (cons x *error-list*)))
	(setq *rerror-list* nil)
	(or wrap
	    (let ((l (current-line-number)))
	      (while *error-list*
		(let ((x (car *error-list*)))
		  (and (= (cdr x) l)
		       (return))
		  (setq *error-list* (cdr *error-list*))
		  (setq *rerror-list* (cons x *rerror-list*)))))))
    (build-error-list))
  (if (goto-error nil)
      t
    (progn
      (message "エラーはありません")
      nil)))

(defun next-error (&optional arg)
  (interactive "p")
  (if (or *error-list* arg)
      (if (if arg
	      (progn
		(when *rerror-list*
		  (setq *error-list* (cons (car *rerror-list*) *error-list*))
		  (setq *rerror-list* (cdr *rerror-list*)))
		(goto-error nil))
	    (goto-error t))
	  t
	(progn
	  (message "これ以上エラーはありません")
	  nil))
    (if (first-error nil t)
	(message "これ以上エラーはないっつってんだろうがこのバカチンが!"))))

(global-set-key #\F10 'first-error)
(global-set-key #\F11 'next-error)
(define-key ctl-x-map #\` 'next-error)
