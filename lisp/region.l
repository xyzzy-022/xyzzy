;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "region")

(in-package "editor")

(export '(*kill-ring* *kill-ring-max*
	  yank yank-and-pop yank-pop
	  copy-region-as-kill kill-region append-next-kill
	  copy-region-to-clipboard kill-region-to-clipboard
	  yank-to-clipboard *sync-kill-ring-with-clipboard*
	  shift-region unshift-region
	  *quotation-prefix* quote-region))

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-pointer* nil)
(defvar *kill-ring-max* 16)
(defvar *last-yank-point* nil)
(defvar *quotation-prefix* "| ")

(defvar *sync-kill-ring-with-clipboard* nil)

(defun push-kill-ring (x ring max)
  (push x ring)
  (let ((l (length ring)))
    (when (> l max)
      (setq ring (nbutlast ring (- l max)))))
  ring)

(defun kill-new (x)
  (setq *kill-ring* (push-kill-ring (list x) *kill-ring* *kill-ring-max*))
  (setq *kill-ring-yank-pointer* *kill-ring*))

(defun kill-append (x front)
  (if *kill-ring*
      (setf (car *kill-ring*)
	    (if front
		(cons x (car *kill-ring*))
	      (nconc (car *kill-ring*) (list x))))
    (kill-new x)))

(defun get-kill-ring (ring pointer n nomove)
  (let* ((l (length ring))
	 (p (rem (+ (- l (length pointer)) n) l))
	 (x (nthcdr (if (minusp p) (+ p l) p) ring)))
    (unless nomove
      (setq pointer x))
    (values (car x) pointer)))

(defun current-kill (n &optional nomove)
  (when *sync-kill-ring-with-clipboard*
    (sync-clipboard-to-kill-ring))
  (unless *kill-ring*
    (plain-error "Kill ring is empty"))
  (multiple-value-setq
      (n *kill-ring-yank-pointer*)
    (get-kill-ring *kill-ring* *kill-ring-yank-pointer* n nomove)))

(defun yank-insert (arg rev)
  (if rev
      (goto-char (prog1
		     (point)
		   (apply #'insert (current-kill arg))
		   (setq *last-yank-point* (point))))
    (progn
      (setq *last-yank-point* (point))
      (apply #'insert (current-kill arg)))))

(defun pop-kill-ring (ring pointer)
  (cond ((eq ring pointer)
	 (pop ring)
	 (pop pointer))
	(t
	 (pop pointer)
	 (setf (cdr (nthcdr (- (length ring) (length pointer) 2) ring))
	       pointer)
	 (when (null pointer)
	   (setq pointer ring))))
  (values ring pointer))

(defun yank (&optional prefix (arg 0))
  (interactive "*P\np")
  (if (eq prefix 'universal-argument)
      (yank-insert 0 t)
    (yank-insert arg nil)))

(defun yank-and-pop (&optional prefix (arg 0))
  (interactive "*P\np")
  (yank prefix arg)
  (multiple-value-setq
      (*kill-ring* *kill-ring-yank-pointer*)
    (pop-kill-ring *kill-ring* *kill-ring-yank-pointer*))
  t)

(defun yank-pop (&optional (arg 1))
  (interactive "*p")
  (or (eq *last-command* 'yank)
      (plain-error "Previous command was not a yank"))
  (setq *this-command* 'yank)
  (delete-region (point) *last-yank-point*)
  (yank-insert arg (< (point) *last-yank-point*)))

(defun copy-region-as-kill (from to)
  (interactive "r")
  (setq *clipboard-newer-than-kill-ring-p* nil
	*kill-ring-newer-than-clipboard-p* t)
  (let ((text (buffer-substring from to)))
    (if (eq *last-command* 'kill-region)
	(kill-append text (> from to))
      (kill-new text)))
  (when (eq *sync-kill-ring-with-clipboard* :always)
    (sync-kill-ring-to-clipboard))
  (when (and (interactive-p)
	     (null *executing-macro*))
    (message "Region copied"))
  t)

(defun append-next-kill ()
  (interactive)
  (setq *last-command* 'kill-region)
  (setq *this-command* 'kill-region))

(defun kill-region (from to)
  (interactive "*r")
  (setq *this-command* 'kill-region)
  (copy-region-as-kill from to)
  (delete-region from to))

(defun copy-region-to-clipboard (from to)
  (interactive "r")
  (copy-to-clipboard (buffer-substring from to)))

(defun kill-region-to-clipboard (from to)
  (interactive "*r")
  (copy-region-to-clipboard from to)
  (delete-region from to))

(defun yank-to-clipboard (&optional (arg 1))
  (interactive)
  (copy-to-clipboard (apply #'concatenate 'string (nth (- arg 1) *kill-ring*))))

(defun quote-region (from to)
  (interactive "*r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (not (eobp))
      (insert *quotation-prefix*)
      (unless (forward-line 1)
	(return))))
  t)

(defun shift-region (start end &optional (column (tab-columns (selected-buffer))))
  (interactive "*r\np")
  (let ((rev nil))
    (cond ((= start end)
	   (return-from shift-region nil))
	  ((> start end)
	   (setq rev t)
	   (rotatef start end)))
    (save-restriction
      (narrow-to-region (progn
			  (goto-char start)
			  (goto-bol)
			  (point))
			(progn
			  (goto-char end)
			  (if (bolp)
			      (forward-char -1)
			    (goto-eol))
			  (point)))
      (goto-char (point-min))
      (cond ((plusp column)
	     (loop
	       (indent-to column)
	       (unless (forward-line 1)
		 (return))))
	    ((minusp column)
	     (setq column (- column))
	     (loop
	       (when (skip-chars-forward " \t")
		 (if (<= (current-column) column)
		     (delete-region (point)
				    (progn
				      (goto-bol)
				      (point)))
		   (backward-delete-char-untabify column)))
	       (unless (forward-line 1)
		 (return)))))
      (setq start (point-min) end (point-max)))
    (goto-char (if rev start end))
    (values start end)))

(defun unshift-region (start end &optional (column (tab-columns (selected-buffer))))
  (interactive "*r\np")
  (shift-region start end (- column)))

(defun current-kill-ring-data ()
  (let ((x (car *kill-ring*)))
    (when x
      (apply #'concatenate 'string x))))

(defun sync-kill-ring-to-clipboard ()
  (when *kill-ring-newer-than-clipboard-p*
    (let ((x (current-kill-ring-data)))
      (when (and x (string/= x (get-clipboard-data)))
	(copy-to-clipboard x)
	(setq *clipboard-newer-than-kill-ring-p* nil
	      *kill-ring-newer-than-clipboard-p* nil)))))

(defun sync-clipboard-to-kill-ring ()
  (when *clipboard-newer-than-kill-ring-p*
    (let ((x (get-clipboard-data)))
      (when (and x (string/= x (current-kill-ring-data)))
	(kill-new x)
	(setq *clipboard-newer-than-kill-ring-p* nil
	      *kill-ring-newer-than-clipboard-p* nil)))))

(defun sync-kill-ring-to-clipboard-hook ()
  (when *sync-kill-ring-with-clipboard*
    (sync-kill-ring-to-clipboard))
  t)
(add-hook '*deactivate-hook* 'sync-kill-ring-to-clipboard-hook)

(defun sync-clipboard-to-kill-ring-always-hook ()
  (when (eq *sync-kill-ring-with-clipboard* :always)
    (sync-clipboard-to-kill-ring))
  t)
(add-hook '*change-clipboard-hook* 'sync-clipboard-to-kill-ring-always-hook)

(global-set-key #\C-y 'yank)
(global-set-key #\M-y 'yank-pop)
(global-set-key #\C-w 'kill-region)
(define-key esc-map #\w 'copy-region-as-kill)
(define-key esc-map #\C-w 'append-next-kill)
