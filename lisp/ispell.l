;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "ispell")

(in-package "editor")

(export '(*ispell-command* *ispell-option* *ispell-spelling-error-attributes*
	  *ispell-possibilities-window-height* ispell-buffer ispell-region))

(defvar *ispell-command* "ispell.exe")
(defvar *ispell-option* nil)
(defvar *ispell-spelling-error-attributes* '(:foreground 1 :underline t))
(defvar *ispell-possibilities-window-height* 6)

(defvar-local *ispell-local-dictionary* nil)

(defun ispell-parse-output (output beg end)
  (save-excursion
    (let ((result nil)
	  (dic *ispell-local-dictionary*)
	  tem)
      (unwind-protect
	  (progn
	    (setq tem (create-new-buffer " *ispell temp*"))
	    (set-buffer tem)
	    (insert-file-contents output)
	    (goto-char (point-min))
	    (loop
	      (let ((beg (point)))
		(or (skip-chars-forward "^:")
		    (return))
		(push (cons (buffer-substring beg (point))
			    (split-string (buffer-substring (1+ (point))
							    (progn (goto-eol) (point)))
					  #\,))
		      result)
		(forward-char 1)))
	    (delete-if #'(lambda (x) (member x dic :test #'equal)) result :key #'car))
	(and tem (delete-buffer tem))))))

(defun ispell-get-spelling-errors (beg end)
  (save-excursion
    (let (temp stdout)
      (unwind-protect
	  (progn
	    (setq temp (make-temp-file-name))
	    (setq stdout (make-temp-file-name))
	    (write-region beg end temp nil *encoding-euc-jp*)
	    (unless (zerop (call-process (concat *ispell-command* " -ll " *ispell-option*)
					 :input temp
					 :output stdout
					 :error nil
					 :show :hide
					 :environ (and (not (si:getenv "HOME"))
						       (list (cons "HOME" (user-homedir-pathname))))
					 :wait t))
	      (error "Cannot spawn ispell"))
	    (ispell-parse-output stdout beg end))
	(and temp (delete-file temp :if-does-not-exist :skip))
	(and stdout (delete-file stdout :if-does-not-exist :skip))))))

(defun ispell-add-to-private-dictionary (word)
  (let (buffer file)
    (unwind-protect
	(progn
	  (save-excursion
	    (setq buffer (create-new-buffer "*ispell temp*"))
	    (set-buffer buffer)
	    (insert "*" word)
	    (setq file (make-temp-file-name))
	    (write-file file t))
	  (unless (zerop (call-process (concat *ispell-command* " -a " *ispell-option*)
				       :input file
				       :output nil
				       :show :hide
				       :environ (and (not (si:getenv "HOME"))
						     (list (cons "HOME" (user-homedir-pathname))))
				       :wait t))
	    (warn "Add to private dictionary failed.")))
      (and buffer (delete-buffer buffer))
      (and file (delete-file file :if-does-not-exist :skip)))))

(defun ispell-highlight-spelling-errors (beg end list)
  (dolist (r list)
    (goto-char beg)
    (while (scan-buffer (car r) :tail t :left-bound t :right-bound t :limit end)
      (apply #'set-text-attribute
	     (match-beginning 0) (match-end 0) (cons 'ispell r)
	     *ispell-spelling-error-attributes*))))

(defun ispell-clear-highlight ()
  (delete-text-attributes 'ispell :key #'(lambda (x) (and (consp x) (car x)))))

(defun ispell-show-possibilities (r)
  (let ((buffer (selected-buffer)))
    (with-output-to-temp-buffer ("*ispell possibilities*"
				 *ispell-possibilities-window-height*)
      (if (null r)
	  (princ "No possibilities.\n")
	(let* ((maxl (apply #'max (mapcar #'length r)))
	       (w (+ maxl 6))
	       (n (min 8 (max 1 (truncate (window-columns) w))))
	       (i 1))
	  (dolist (x r)
	    (format t "~3d ~@va  " i maxl x)
	    (if (zerop (rem i n))
		(terpri))
	    (incf i))
	  (if (/= (rem i n) 1)
	    (terpri)))))
    (pop-to-buffer buffer)))

(defun ispell-accept-word (word)
  (delete-text-attributes word
			  :key #'(lambda (x)
				   (and (consp x)
					(eq (car x) 'ispell)
					(cadr x)))
			  :test #'equal))

(defun ispell-accept-this-session (word)
  (ispell-accept-word word)
  (pushnew word *ispell-local-dictionary* :test #'equal))

(defun ispell-accept-and-add-to-private-dictionary (word)
  (ispell-accept-word word)
  (ispell-add-to-private-dictionary word))

(defun ispell-accept-and-add-lowercase-to-private-dictionary (word)
  (ispell-accept-word word)
  (ispell-add-to-private-dictionary (string-downcase word)))

(defun ispell-replace-word (word beg end)
  (delete-region beg end)
  (insert word)
  (undo-boundary))

(defun ispell-help ()
  (save-excursion
    (let (buffer)
      (unwind-protect
	  (progn
	    (setq buffer (get-buffer-create "*ispell help*"))
	    (set-buffer buffer)
	    (erase-buffer buffer)
	    (insert "
Commands are:

SPC       Accept the word this time only.
<Number>  Replace with one of the suggested words.
i         Accept the word, and put it in your private dictionary.
a         Accept the word for the rest of this session.
r         Replace the misspelled word completely.
u         Accept and add lowercase version to private dictionary.
q         Quit
")
	    (refresh-screen)
	    (minibuffer-prompt "Type any key to continue: ")
	    (read-char))
	(and buffer (delete-buffer buffer))))))

(defun ispell-correct-word ()
  (multiple-value-bind (beg end r)
      (find-text-attribute 'ispell :key #'(lambda (x) (and (consp x) (car x))) :start (point))
    (unless beg
      (return-from ispell-correct-word nil))
    (delete-text-attributes r :test #'eq)
    (goto-char end)
    (when (pos-not-visible-in-window-p beg)
      (recenter))
    (reverse-region beg end)
    (unwind-protect
	(progn
	  (ispell-show-possibilities (cddr r))
	  (refresh-screen)
	  (loop
	    (minibuffer-prompt "Ispell(? for help): ")
	    (let ((c (read-char)))
	      (cond ((or (eql c #\q)
			 (eql c #\C-g))
		     (quit))
		    ((eql c #\SPC)
		     (return t))
		    ((eql c #\a)
		     (ispell-accept-this-session (cadr r))
		     (return t))
		    ((eql c #\i)
		     (ispell-accept-and-add-to-private-dictionary (cadr r))
		     (return t))
		    ((eql c #\u)
		     (ispell-accept-and-add-lowercase-to-private-dictionary (cadr r))
		     (return t))
		    ((eql c #\r)
		     (let ((s (handler-case (read-string "Replace with: ") (quit))))
		       (when (plusp (length s))
			 (ispell-replace-word s beg end)
			 (setq end (point))
			 (long-operation
			   (ispell-highlight-spelling-errors
			    beg end (ispell-get-spelling-errors beg end)))
			 (goto-char beg)
			 (return t))))
		    ((digit-char-p c)
		     (let ((choice
			    (if (cddr r)
				(let* ((n (digit-char-p c))
				       (l (length (cddr r)))
				       (m (truncate (+ 1 (log l 10)))))
				  (loop
				    (cond ((zerop n))
					  ((> n l)
					   (return nil))
					  ((= (truncate (+ 1 (log n 10))) m)
					   (return (nth (- n 1) (cddr r)))))
				    (minibuffer-prompt "Choice: ~a" n)
				    (setq c (read-char))
				    (cond ((eql c #\C-h)
					   (setq n (truncate n 10)))
					  ((digit-char-p c)
					   (setq n (+ (* n 10) (digit-char-p c))))
					  ((or (eql c #\RET)
					       (eql c #\LFD))
					   (return (and (plusp n) (nth (- n 1) (cddr r)))))
					  (t
					   (ding))))))))
		       (when choice
			 (ispell-replace-word choice beg end)
			 (return t))
		       (ding)))
		    ((eq c #\?)
		     (ispell-help))
		    (t
		     (ding))))))
      (clear-reverse-region))))

(defun ispell-region (beg end)
  (interactive "r")
  (ispell-clear-highlight)
  (if (= beg end)
      (return-from ispell-region nil))
  (if (> beg end)
      (rotatef beg end))
  (unwind-protect
      (save-window-excursion
	(delete-other-windows)
	(long-operation
	  (ispell-highlight-spelling-errors beg end (ispell-get-spelling-errors beg end))
	  (goto-char beg))
	(while (ispell-correct-word))
	(message "Spell checking done"))
    (let ((buffer (find-buffer "*ispell possibilities*")))
      (and buffer (delete-buffer buffer)))
    (ispell-clear-highlight))
  t)

(defun ispell-buffer ()
  (interactive)
  (ispell-region (point-min) (point-max)))
