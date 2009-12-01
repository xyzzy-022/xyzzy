;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "abbrev")

(in-package "editor")

(export '(*global-abbrev-table* *local-abbrev-table*
	  *abbrev-all-caps* *pre-abbrev-expand-hook*
	  *only-global-abbrevs* *abbrev-file-name*))
(export '(abbrev-mode define-abbrev define-global-abbrev define-mode-abbrev
	  define-abbrev-table expand-abbrev unexpand-abbrev
	  kill-all-abbrevs insert-abbrevs list-abbrevs
	  edit-abbrevs-mode edit-abbrevs edit-abbrevs-redefine
	  define-abbrevs read-abbrev-file quietly-read-abbrev-file
	  write-abbrev-file
	  add-mode-abbrev add-global-abbrev add-abbrev
	  inverse-add-mode-abbrev inverse-add-global-abbrev inverse-add-abbrev
	  abbrev-prefix-mark expand-region-abbrevs))

(defvar *global-abbrev-table* nil)
(defvar-local *local-abbrev-table* nil)
(defvar *abbrev-all-caps* nil)
(defvar *pre-abbrev-expand-hook* nil)
(defvar *only-global-abbrevs* nil)
(defvar *abbrev-file-name* "~/.abbrev_defs")
(defvar-local abbrev-mode nil)

(pushnew '(abbrev-mode . "Abbrev") *minor-mode-alist* :key #'car)
(defvar *abbrevs-changed* nil)
(defvar *save-abbrevs* nil)
(defvar *abbrev-table-name-list* nil)
(defvar-local *abbrev-prefix-location* nil)
(defvar-local *last-abbrev-text* nil)
(defvar-local *last-abbrev* nil)
(defvar-local *last-abbrev-point* nil)

(defun make-abbrev-table ()
  (make-hash-table :test #'equalp))

(defun clear-abbrev-table (table)
  (clrhash table))

(defun define-abbrev (table name exp &optional hook (count 0))
  (check-type name string)
  (and exp
       (check-type exp string))
  (check-type count integer)
  (let ((x (gethash name table)))
    (when (or (not (equal exp (car x)))
	      (not (equal hook (cadr x))))
      (setq *abbrevs-changed* t)))
  (if exp
      (setf (gethash name table) (list exp hook count))
    (remhash name table))
  name)

(defun define-global-abbrev (name exp)
  (define-abbrev *global-abbrev-table* name exp))

(defun define-mode-abbrev (name exp)
  (and (null *local-abbrev-table*)
       (error "現在のメジャーモードにゃabbrevテーブルがありません"))
  (define-abbrev *local-abbrev-table* name exp))

(defun abbrev-expansion (abbrev &optional table)
  (car (if table
	   (gethash abbrev table)
	 (or (and *local-abbrev-table*
		  (gethash abbrev *local-abbrev-table*))
	     (gethash abbrev *global-abbrev-table*)))))

(defun abbrev-expansion-region (from to &optional table)
  (car (if table
	   (gethash-region from to table)
	 (or (and *local-abbrev-table*
		  (gethash-region from to *local-abbrev-table*))
	     (gethash-region from to *global-abbrev-table*)))))

(defun define-abbrev-table (tabname &optional defs)
  (let ((table (or (and (boundp tabname)
			(symbol-value tabname))
		   (prog1
		       (set tabname (make-abbrev-table))
		     (push tabname *abbrev-table-name-list*)))))
    (mapc #'(lambda (x)
	      (apply #'define-abbrev table x))
	  defs)))

(define-abbrev-table '*global-abbrev-table*)

(defun insert-abbrev-table-description (name readable)
  (let ((table (symbol-value name)))
    (cond (readable
	   (format t "(~S)\n\n" name)
	   (maphash #'(lambda (name x)
			(format t "~S ~15,1T~S ~20,1T~S~@[ ~45,1T~S~]~%"
				name (caddr x) (car x) (cadr x)))
		    table)
	   (princ "\n\n"))
	  (t
	   (let ((*package* (symbol-package name)))
	     (format t "(in-package ~S)\n" (package-name (symbol-package name)))
	     (format t "(define-abbrev-table '~S '(\n" name)
	     (maphash #'(lambda (name x)
			  (format t "  (~S ~S ~S ~S)~%"
				  name (car x) (cadr x) (caddr x)))
		      table)
	     (princ "))\n\n"))))))

(defun expand-abbrev ()
  (interactive "*")
  (let (abbrev start end)
    (run-hooks '*pre-abbrev-expand-hook*)
    (save-excursion
      (let ((opoint (point)))
	(setq start (cond (*abbrev-prefix-location*
			   (goto-char *abbrev-prefix-location*)
			   (when (eql (following-char) #\-)
			     (delete-region (point) (+ (point) 1)))
			   (setq *abbrev-prefix-location* nil)
			   (point))
			  (t
			   (forward-word -1)
			   (point))))
	(setq end (min opoint
		       (progn
			 (forward-word 1)
			 (point))))
	(when (>= start end)
	  (return-from expand-abbrev nil))
	(setq abbrev (or (and *local-abbrev-table*
			      (gethash-region start end *local-abbrev-table*))
			 (gethash-region start end *global-abbrev-table*)))
	(unless abbrev
	  (return-from expand-abbrev nil))))
    (setq *last-abbrev-text* (buffer-substring start end))
    (delete-region start end)
    (goto-char start)
    (setq *last-abbrev* (car abbrev))
    (setq *last-abbrev-point* start)
    (incf (caddr abbrev))
    (insert (car abbrev))
    (cond ((string-match "^[A-Z]*$" *last-abbrev-text*)
	   (if (and (null *abbrev-all-caps*)
		    (save-excursion
		      (> (progn
			   (forward-word -1)
			   (point))
			 (progn
			   (goto-char start)
			   (forward-word 1)
			   (point)))))
	       (capitalize-region start (point))
	     (upcase-region start (point))))
	  ((string-match "[A-Z]" *last-abbrev-text*)
	   (let ((p (point)))
	     (goto-char start)
	     (capitalize-word 1)
	     (goto-char p))))
    (when (cadr abbrev)
      (funcall (cadr abbrev)))
    t))

(defun unexpand-abbrev ()
  (interactive "*")
  (cond ((or (null *last-abbrev-point*)
	     (< *last-abbrev-point* (point-min))
	     (> *last-abbrev-point* (point-max)))
	 nil)
	((and (stringp *last-abbrev-text*)
	      (stringp *last-abbrev*))
	 (save-excursion
	   (prog2
	       (goto-char *last-abbrev-point*)
	       (when (looking-for *last-abbrev*)
		 (delete-region (point) (+ (point) (length *last-abbrev*)))
		 (insert *last-abbrev-text*)
		 t)
	     (setq *last-abbrev-text* nil))))))

(defun abbrev-mode (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode 'abbrev-mode arg sv)
  (set-buffer-modified-p (buffer-modified-p)))

(defun kill-all-abbrevs ()
  (interactive)
  (mapc #'(lambda (x)
	    (clear-abbrev-table (symbol-value x)))
	*abbrev-table-name-list*)
  t)

(defun insert-abbrevs ()
  (interactive "*")
  (save-excursion
    (with-output-to-selected-buffer
      (mapc #'(lambda (x)
		(insert-abbrev-table-description x t))
	    *abbrev-table-name-list*)
      (set-mark))))

(defun prepare-abbrev-list-buffer ()
  (save-excursion
    (let ((buffer (get-buffer-create "*Abbrevs*")))
      (set-buffer buffer)
      (erase-buffer buffer)
      (with-output-to-selected-buffer
	(mapc #'(lambda (x)
		  (insert-abbrev-table-description x t))
	      *abbrev-table-name-list*))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (edit-abbrevs-mode)
      buffer)))

(defun list-abbrevs ()
  (interactive)
  (display-buffer (prepare-abbrev-list-buffer)))

(defun edit-abbrevs ()
  (interactive)
  (switch-to-buffer (prepare-abbrev-list-buffer)))

(defvar *edit-abbrevs-map* nil)
(unless *edit-abbrevs-map*
  (setq *edit-abbrevs-map* (make-sparse-keymap))
  (define-key *edit-abbrevs-map* '(#\C-x #\C-s) 'edit-abbrevs-redefine)
  (define-key *edit-abbrevs-map* '(#\C-c #\C-c) 'edit-abbrevs-redefine))

(defun edit-abbrevs-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'edit-abbrevs-mode)
  (setq mode-name "Edit Abbrevs")
  (use-keymap *edit-abbrevs-map*))

(defun edit-abbrevs-redefine ()
  (interactive)
  (define-abbrevs t)
  (set-buffer-modified-p nil))

(defun define-abbrevs (&optional arg)
  (interactive "p")
  (when arg
    (kill-all-abbrevs))
  (save-excursion
    (goto-char (point-min))
    (while (scan-buffer "^(" :tail t :regexp t)
      (let ((table (with-input-from-selected-buffer (read)))
	    (abbrevs nil))
	(forward-line 1)
	(while (progn
		 (forward-line 1)
		 (not (eolp)))
	  (let (name count exp hook)
	    (with-input-from-selected-buffer
	      (setq name (read) count (read) exp (read)))
	    (skip-chars-backward " \t\n\f")
	    (unless (eolp)
	      (with-input-from-selected-buffer
		(setq hook (read))
		(skip-chars-backward " \t\n\f")))
	    (push (list name exp hook count) abbrevs)))
	(define-abbrev-table table abbrevs))))
  t)

(defun read-abbrev-file (&optional file quietly)
  (interactive "fRead abbrev file: ")
  (or file
      (setq file *abbrev-file-name*))
  (load file :verbose (not quietly) :print (not quietly))
  (cond ((path-equal file *abbrev-file-name*)
	 (setq *save-abbrevs* t)
	 (setq *abbrevs-changed* nil))
	(t
	 (setq *abbrevs-changed* t)))
  t)

(defun quietly-read-abbrev-file (&optional file)
  (read-abbrev-file file t))

(defun write-abbrev-file (&optional file)
  (interactive "FWrite abbrev file: "
    :title0 "Write abbrev file"
    :default0 *abbrev-file-name*)
  (or file
      (setq file *abbrev-file-name*))
  (let (buffer)
    (unwind-protect
	(progn
	  (setq buffer (create-new-buffer "*write abbrev*"))
	  (save-excursion
	    (set-buffer buffer)
	    (with-output-to-selected-buffer
	      (mapc #'(lambda (x)
			(insert-abbrev-table-description x nil))
		    *abbrev-table-name-list*))
	    (write-region (point-min) (point-max) file)
	    (when (path-equal file *abbrev-file-name*)
	      (setq *abbrevs-changed* nil))))
      (when buffer
	(delete-buffer buffer)))
    t))

(defun save-abbrev-file ()
  (interactive)
  (and *save-abbrevs*
       *abbrevs-changed*
       (write-abbrev-file))
  t)

(add-hook '*kill-xyzzy-hook* 'save-abbrev-file)

(defun add-abbrev (table type arg)
  (let* ((exp (and (>= arg 0)
		   (buffer-substring
		    (point)
		    (if (= arg 0)
			(mark)
		      (save-excursion
			(forward-word (- arg))
			(point))))))
	 (name (read-string
		(format nil "~:[未定義にする~A略称~;\"~:*~A\"の~A略称~]: "
			exp type))))
    (when (or (null exp)
	      (null (abbrev-expansion name table))
	      (yes-or-no-p "\"~A\"は\"~A\"に定義されています。再定義する?"
			   name (abbrev-expansion name table)))
      (define-abbrev table (string-downcase name) exp))))

(defun add-mode-abbrev (&optional (arg 1))
  (interactive "p")
  (add-abbrev
   (if *only-global-abbrevs*
       *global-abbrev-table*
     (or *local-abbrev-table*
	 (error "モードごとのabbrevテーブルはありません")))
   "Mode" arg))

(defun add-global-abbrev (&optional (arg 1))
  (interactive "p")
  (add-abbrev *global-abbrev-table* "Global" arg))

(defun inverse-add-abbrev (table type arg)
  (let* (point
	 (name (save-excursion
		 (buffer-substring (progn
				     (forward-word (- arg))
				     (point))
				   (progn
				     (forward-word 1)
				     (setq point (point))))))
	 (exp (read-string (format nil "~A略称\"~A\"の展開: "
				   type name))))
    (when (or (not (abbrev-expansion name table))
	      (yes-or-no-p "\"~A\"は\"~A\"に定義されています。再定義する? "
			   name (abbrev-expansion name table)))
      (define-abbrev table (string-downcase name) exp)
      (goto-char point)
      (expand-abbrev))))

(defun inverse-add-mode-abbrev (&optional (arg 1))
  (interactive "p")
  (inverse-add-abbrev
   (if *only-global-abbrevs*
       *global-abbrev-table* 
     (or *local-abbrev-table*
	 (error "モードごとのabbrevテーブルはありません")))
   "Mode" arg))

(defun inverse-add-global-abbrev (&optional (arg 1))
  (interactive "p")
  (inverse-add-abbrev *global-abbrev-table* "Global" arg))

(defun abbrev-prefix-mark (&optional arg)
  (interactive "*p")
  (unless arg
    (expand-abbrev))
  (setq *abbrev-prefix-location* (point))
  (insert #\-))

(defun expand-region-abbrevs (from to &optional noquery)
  (interactive "*r\np")
  (or (< from to)
      (rotatef from to))
  (save-excursion
    (goto-char from)
    (let ((lim (- (point-max) to))
	  point)
      (unwind-protect
	  (while (and (not (eobp))
		      (progn
			(forward-word 1)
			(<= (point) (- (point-max) lim))))
	    (and (abbrev-expansion-region (save-excursion
					    (forward-word -1)
					    (setq point (point)))
					  (point))
		 (or noquery
		     (progn
		       (reverse-region point (point))
		       (refresh-screen)
		       (yes-no-or-cancel-p "展開しますか?")))
		 (expand-abbrev)))
	(clear-reverse-region))
      t)))

(define-key ctl-x-map #\+ 'add-global-abbrev)
(define-key ctl-x-map #\C-a 'add-mode-abbrev)
(define-key ctl-x-map #\- 'inverse-add-global-abbrev)
(define-key ctl-x-map #\C-h 'inverse-add-mode-abbrev)
(define-key esc-map #\' 'abbrev-prefix-mark)
(define-key ctl-x-map #\' 'expand-abbrev)
