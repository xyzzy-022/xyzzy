;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "rectangl")

(in-package "editor")

(export '(*rectangle-kill-buffer* yank-rectangle yank-rectangle-as-region
	  append-rectangle overwrite-rectangle copy-rectangle
	  kill-rectangle clear-rectangle open-rectangle delete-rectangle
	  string-rectangle))

(defvar *rectangle-kill-buffer* nil)

(defun yank-rectangle ()
  (interactive "*")
  (let ((col (current-column))
	(first t))
    (dolist (x *rectangle-kill-buffer*)
      (unless (or first (forward-virtual-line 1))
	(goto-eol)
	(insert #\LFD))
      (let ((l (goto-virtual-column col)))
	(when (and (< l col)
		   (virtual-eolp))
	  (insert #\SPC (- col l))))
      (insert x)
      (setq first nil)))
  t)

(defun yank-rectangle-as-region ()
  (interactive "*")
  (dolist (x *rectangle-kill-buffer*)
    (insert x))
  t)

(defun append-rectangle ()
  (interactive "*")
  (let ((first t))
    (dolist (x *rectangle-kill-buffer*)
      (unless (or first (forward-virtual-line 1))
	(goto-eol)
	(insert #\LFD))
      (goto-virtual-eol)
      (insert x)
      (setq first nil)))
  t)

(defun overwrite-rectangle ()
  (interactive "*")
  (let ((col (current-column))
	(first t))
    (dolist (x *rectangle-kill-buffer*)
      (unless (or first (forward-virtual-line 1))
	(goto-eol)
	(insert #\LFD))
      (let ((l (goto-virtual-column col)))
	(when (and (< l col)
		   (virtual-eolp))
	  (insert #\SPC (- col l))))
      (delete-region (point)
		     (progn
		       (goto-virtual-column
			(count-column x (current-virtual-column) (selected-buffer)))
		       (point)))
      (insert x)
      (setq first nil)))
  t)

(defun operate-on-rectangle (p1 p2 c1 c2 &key copy delete clear open string)
  (let ((delete-p (or delete clear string))
	col1 col2 from to buffer goal)
    (when (and (= p1 p2)
	       (not string))
      (return-from operate-on-rectangle t))
    (save-excursion
      (if (< p1 p2)
	  (setq from p1 to p2)
	(setq from p2 to p1))
      (setq p1 (or c1
		   (progn
		     (goto-char from)
		     (current-virtual-column))))
      (setq p2 (or c2
		   (progn
		     (goto-char to)
		     (current-virtual-column))))
      (when (and (= p1 p2)
		 (not string))
	(return-from operate-on-rectangle t))
      (if (< p1 p2)
	  (setq col1 p1 col2 p2)
	(setq col1 p2 col2 p1))
      (save-restriction
	(narrow-to-region from to)
	(goto-char from)
	(loop
	  (unless (or (>= (goto-virtual-column col1) col1)
		      (virtual-eolp))
	    (forward-char 1))
	  (cond (open
		 (let ((l (current-virtual-column)))
		   (when (< l col2)
		     (insert #\SPC (- col2 l)))))
		(t
		 (setq p1 (point))
		 (goto-virtual-column col2)
		 (setq p2 (point))
		 (setq goal (current-virtual-column))
		 (or (<= p1 p2)
		     (setq p2 p1))
		 (when copy
		   (push (buffer-substring p1 p2) buffer))
		 (when delete-p
		   (delete-region p1 p2))
		 (if string
		     (insert string)
		   (if clear
		       (let ((l (current-virtual-column)))
			 (when (< l goal)
			   (insert #\SPC (- goal l))))))))
	  (unless (forward-virtual-line 1)
	    (return)))
	(when copy
	  (setq *rectangle-kill-buffer* (nreverse buffer)))))))

(defun copy-rectangle (p1 p2)
  (interactive "r")
  (operate-on-rectangle p1 p2 nil nil :copy t))

(defun kill-rectangle (p1 p2)
  (interactive "*r")
  (operate-on-rectangle p1 p2 nil nil :copy t :delete t))

(defun clear-rectangle (p1 p2)
  (interactive "*r")
  (operate-on-rectangle p1 p2 nil nil :copy t :clear t))

(defun open-rectangle (p1 p2)
  (interactive "*r")
  (operate-on-rectangle p1 p2 nil nil :open t))

(defun delete-rectangle (p1 p2)
  (interactive "*r")
  (operate-on-rectangle p1 p2 nil nil :delete t))

(defun string-rectangle (p1 p2 string)
  (interactive "*r\nsString rectangle: ")
  (operate-on-rectangle p1 p2 nil nil :string string))
