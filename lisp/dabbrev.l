;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "dabbrev")

(in-package "editor")

(export '(dabbrev-expand dabbrev-popup
	  *dabbrevs-case-fold* *dabbrevs-no-undo*
	  *dabbrev-search-this-buffer-only*
	  *dabbrev-popup-this-buffer-only*))

(defvar-local *dabbrevs-case-fold* nil)
(defvar-local *last-dabbrevs-abbrev* nil)
(defvar-local *last-dabbrevs-text* nil)
(defvar-local *last-dabbrevs-text-location* nil)
(defvar-local *last-dabbrevs-location* nil)
(defvar-local *last-dabbrevs-last-buffer* nil)
(defvar-local *last-dabbrevs-direction* nil)
(defvar-local *dabbrevs-match-table* nil)
(defvar-local *dabbrevs-first-text* nil)
(defvar *dabbrevs-no-undo* nil)
(defvar *dabbrev-search-this-buffer-only* nil)
(defvar *dabbrev-popup-this-buffer-only* nil)

(defun dabbrev-find-expansion (abbrev count reverse no-dup case-fold match-table)
  (let (loc text)
    (dotimes (n count (values loc text))
      (unless (while (scan-buffer abbrev :reverse reverse :no-dup no-dup
				  :case-fold case-fold
				  :left-bound :symbol)
		(let* ((point (point))
		       (tail (save-excursion
			       (let ((end (match-end 0)))
				 (or (skip-syntax-spec-forward "w_")
				     (and (looking-at "\\sk\\|\\sj")
					  (forward-word 1)))
				 (when (< (point) end)
				   (goto-char end)
				   (or (skip-syntax-spec-forward "w_")
				       (and (looking-at "\\sk\\|\\sj")
					    (forward-word 1)))))
			       (point)))
		       (x (gethash-region point tail match-table)))
		  (when (or (null x) (= x point))
		    (setq loc point)
		    (setq text (buffer-substring loc tail))
		    (setf (gethash text match-table) loc)
		    (return t)))
		(setq no-dup t))
	(return nil)))))

(defun dabbrev-find (abbrev count reverse)
  (multiple-value-setq
      (*last-dabbrevs-text-location*
       *last-dabbrevs-text*)
    (dabbrev-find-expansion abbrev count reverse t
			    *dabbrevs-case-fold*
			    *dabbrevs-match-table*)))

(defun dabbrev-find-buffer (abbrev &optional buffer start)
  (unless *dabbrev-search-this-buffer-only*
    (let ((curbuf (selected-buffer))
	  (case-fold *dabbrevs-case-fold*)
	  (tab *dabbrevs-match-table*)
	  (syntax-table (syntax-table))
	  loc text)
      (unless buffer
	(setq buffer (or (and (minibuffer-window-p (selected-window))
			      (minibuffer-buffer curbuf))
			 (get-next-buffer curbuf nil nil t)))
	(setq start nil))
      (cond ((with-set-buffer
	       (with-interval-message (300)
		 (save-excursion
		   (while (not (eq buffer curbuf))
		     (let ((name (buffer-name buffer)))
		       (message "Searching (~A)..." name)
		       (set-buffer buffer)
		       (save-excursion
			 (goto-char (or start (point-min)))
			 (let ((osyntax-table (syntax-table)))
			   (unwind-protect
			       (progn
				 (use-syntax-table syntax-table nil t)
				 (multiple-value-setq (loc text)
				   (dabbrev-find-expansion
				    abbrev 1 nil start case-fold tab)))
			     (use-syntax-table osyntax-table nil t)))
			 (when loc
			   (message "Searching (~A)...found" name)
			   (return t))))
		     (setq buffer (get-next-buffer buffer nil nil t))
		     (setq start nil)))))
	     (setq *last-dabbrevs-text-location* loc)
	     (setq *last-dabbrevs-text* text)
	     buffer)
	    (t
	     (clear-message)
	     nil)))))

(defun dabbrev-start ()
  (let ((end (point))
	(start (progn
		 (or (skip-syntax-spec-backward "w_")
		     (and (save-excursion
			    (and (forward-char -1)
				 (looking-at "\\sk\\|\\sj")))
			  (forward-word -1)))
		 (point))))
    (cond ((and (= start end)
		(not (bolp))
		(forward-char -1)
		(looking-at "\\S "))
	   (goto-char end)
	   (forward-word -1)
	   (when (looking-at "\\sw\\|\\s_")
	     (forward-word 1)
	     (skip-syntax-spec-backward "w_"))
	   (point))
	  (t start))))

(defun dabbrev-expand (&optional arg)
  (interactive "*p")
  (let* ((end (point))
	 (start
	  (cond ((and *last-dabbrevs-location*
		      *last-dabbrevs-text*
		      (save-excursion
			(goto-char *last-dabbrevs-location*)
			(and (= (+ (point) (length *last-dabbrevs-text*)) end)
			     (looking-for *last-dabbrevs-text*))))
		 (goto-char *last-dabbrevs-location*)
		 *last-dabbrevs-location*)
		(t
		 (dabbrev-start))))
	 (lastloc *last-dabbrevs-location*)
	 (lasttext *last-dabbrevs-text*)
	 (direction arg)
	 (eq (if *dabbrevs-case-fold* #'equalp #'equal))
	 loc buffer abbrev)
    (setq *last-dabbrevs-location* nil)
    (when (= start end)
      (return-from dabbrev-expand nil))
    (cond ((and #|(eq *last-command* 'dabbrev-expand)|#
		(eql start lastloc)
		(= (- end start) (length *last-dabbrevs-text*))
		(looking-for *last-dabbrevs-text*))
	   (setq abbrev *last-dabbrevs-abbrev*)
	   (setq loc *last-dabbrevs-text-location*)
	   (setq buffer *last-dabbrevs-last-buffer*)
	   (and (null direction)
		(setq direction *last-dabbrevs-direction*)))
	  (t
	   (setq *dabbrevs-first-text* nil)
	   (setq lastloc nil)
	   (setq abbrev (buffer-substring start end))
	   (setq loc start)
	   (setq buffer nil)
	   (and (null direction)
		(setq direction 1))
	   (if *dabbrevs-match-table*
	       (clrhash *dabbrevs-match-table*)
	     (setq *dabbrevs-match-table*
		   (make-hash-table :test eq)))))
    (when buffer
      (unless (and (bufferp buffer)
		   (not (deleted-buffer-p buffer))
		   (setq buffer (dabbrev-find-buffer abbrev buffer loc)))
	(setq loc start)
	(setq buffer nil)
	(setq direction 1)
	(setq arg nil)))
    (unless buffer
      (goto-char loc)
      (if (plusp direction)
	  (cond ((or (dabbrev-find abbrev direction t) arg))
		((progn
		   (goto-char start)
		   (dabbrev-find abbrev 1 nil))
		 (setq direction 0))
		((setq buffer (dabbrev-find-buffer abbrev)))
		((= loc start))
		((progn
		   (goto-char start)
		   (dabbrev-find abbrev 1 t))
		 (setq direction 1)))
	(cond ((or (dabbrev-find abbrev (- 1 direction) nil) arg))
	      ((setq buffer (dabbrev-find-buffer abbrev)))
	      ((progn
		 (goto-char start)
		 (dabbrev-find abbrev 1 t))
	       (setq direction 1))
	      ((= loc start))
	      ((progn
		 (goto-char start)
		 (dabbrev-find abbrev 1 nil))
	       (setq direction 0)))))
    (unless *last-dabbrevs-text-location*
      (goto-char end)
      (plain-error "‚È‚¢‚æ‚ñ"))
    (cond ((funcall eq *last-dabbrevs-text* lasttext))
	  ((funcall eq *last-dabbrevs-text* *dabbrevs-first-text*)
	   (message "Wrapped"))
	  (*dabbrevs-first-text*)
	  (t (setq *dabbrevs-first-text* *last-dabbrevs-text*)))
    (setq *last-dabbrevs-direction* (if (plusp direction) 1 0))
    (setq *last-dabbrevs-abbrev* abbrev)
    (setq *last-dabbrevs-location* start)
    (setq *last-dabbrevs-last-buffer* buffer)
    (when (and *dabbrevs-no-undo*
	       (eq *last-command* 'dabbrev-expand))
      (clear-undo-boundary))
    (delete-region start end)
    (insert *last-dabbrevs-text*)
    t))

(defun dabbrev-find-all-expansion (abbrev case-fold match-table start matches)
  (let ((no-dup nil))
    (save-excursion
      (goto-char (point-min))
      (while (scan-buffer abbrev :case-fold case-fold
			  :no-dup no-dup :left-bound :symbol)
	(let ((from (point))
	      (to (save-excursion
		    (let ((end (match-end 0)))
		      (or (skip-syntax-spec-forward "w_")
			  (and (looking-at "\\sk\\|\\sj")
			       (forward-word 1)))
		      (when (< (point) end)
			(goto-char end)
			(or (skip-syntax-spec-forward "w_")
			    (and (looking-at "\\sk\\|\\sj")
				 (forward-word 1)))))
		    (point))))
	  (unless (or (eql from start)
		      (gethash-region from to match-table))
	    (let ((text (buffer-substring from to)))
	      (setf (gethash text match-table) from)
	      (push text matches)))
	  (setq no-dup t)))
      matches)))

(defun dabbrev-popup ()
  (interactive "*")
  (let* ((end (point))
	 (start (save-excursion (dabbrev-start))))
    (when (= start end)
      (return-from dabbrev-popup nil))
    (let* ((abbrev (buffer-substring start end))
	   (match-table (make-hash-table
			 :test (if *dabbrevs-case-fold* #'equalp #'equal)))
	   matches)
      (setf (gethash abbrev match-table) start)
      (setq matches (dabbrev-find-all-expansion abbrev *dabbrevs-case-fold*
						match-table start nil))
      (unless *dabbrev-popup-this-buffer-only*
	(let ((curbuf (selected-buffer))
	      (case-fold *dabbrevs-case-fold*)
	      (syntax-table (syntax-table)))
	  (with-set-buffer
	    (with-interval-message (300)
	      (save-excursion
		(dolist (buffer (buffer-list))
		  (unless (eq buffer curbuf)
		    (message "Searching (~A)..." (buffer-name buffer))
		    (set-buffer buffer)
		    (save-excursion
		      (let ((osyntax-table (syntax-table)))
			(unwind-protect
			    (progn
			      (use-syntax-table syntax-table nil t)
			      (setq matches (dabbrev-find-all-expansion
					     abbrev case-fold match-table nil matches)))
			  (use-syntax-table osyntax-table nil t))))))))))
	(clear-message))
      (if matches
	  (popup-completion-list (sort matches #'string-lessp) start end)
	(plain-error "‚È‚¢‚æ‚ñ")))))
