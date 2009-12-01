;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "javafns")

(in-package "editor")

(defvar *scan-javafunc-class-regexp*
  (compile-regexp "\\(^\\|[ \t]\\)\
\\(\\(abstract\\|public\\|final\\)[ \t\n\f]+\\)*\
class[ \t\r\n\f]+\\([A-Za-z_][A-Za-z0-9_]*\\)\\|\\(/[/*]\\)\\|\\(['\"]\\)"))

(defvar *scan-javafunc-method-regexp*
  (compile-regexp "\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n\f]*(\\|\\(/[/*]\\)\\|\\(['\"]\\)"))

(defvar *scan-javafunc-keyword-regexp*
  (compile-regexp
   "if\\|for\\|while\\|switch\\|return\\|catch\\|new\\|synchronized"))

(defun scan-javafunc-skip-white ()
  (loop
    (skip-white-forward)
    (unless (skip-chars-forward "\n")
      (return))))

(defun scan-javafunc-find-semi-or-braces ()
  (while (not (eobp))
    (scan-javafunc-skip-white)
    (cond ((looking-at "[];})]")
	   (return nil))
	  ((looking-for "{")
	   (return t))
	  (t
	   (or (forward-sexp 1 t)
	       (forward-char 1))))))

(defun scan-java-function-1 (fn count class)
  (when class
    (goto-char (point-min))
    (while (scan-buffer *scan-javafunc-method-regexp*)
      (cond ((match-beginning 1)
	     (let ((nameb (match-beginning 1))
		   (namee (match-end 1))
		   (end (match-end 0))
		   argb arge)
	       (goto-char nameb)
	       (cond ((and (looking-at *scan-javafunc-keyword-regexp*)
			   (= (match-end 0) namee))
		      (goto-char end))
		     (t
		      (goto-char (- end 1))
		      (setq argb (point))
		      (when (forward-sexp 1 t)
			(setq arge (point))
			(when (scan-javafunc-find-semi-or-braces)
			  (funcall fn nameb class (buffer-substring nameb namee)
				   argb arge)
			  (incf count)))))))
	    (t
	     (let ((opoint (point)))
	       (if (match-beginning 2)
		   (skip-white-forward)
		 (forward-sexp 1 t))
	       (and (= opoint (point))
		    (not (forward-char 1))
		    (return)))))))
  count)

(defun scan-javafunc-scan-class-name (beg end)
  (when (scan-javafunc-find-semi-or-braces)
    (buffer-substring beg end)))

(defun scan-java-function (fn cfn count &optional class)
  (let ((beg (point-min))
	end next name)
    (goto-char beg)
    (while (scan-buffer *scan-javafunc-class-regexp* :tail t)
      (cond ((match-beginning 1)
	     (let ((b (match-beginning 4))
		   (e (match-end 4)))
	       (when (scan-javafunc-find-semi-or-braces)
		 (setq name (buffer-substring b e))
		 (funcall cfn b class name)
		 (setq end (point))
		 (unless (forward-sexp 1 t)
		   (if (scan-buffer "^}" :regexp t)
		       (forward-line -1)
		     (goto-char (point-max))))
		 (setq next (point))
		 (save-restriction
		   (narrow-to-region beg end)
		   (setq count (scan-java-function-1 fn count class)))
		 (save-restriction
		   (narrow-to-region end next)
		   (setq count (scan-java-function fn cfn count
						   (if class
						       (concat class "." name)
						     name)))
		   (setq beg next)))))
	    (t
	     (goto-char (match-beginning 0))
	     (if (match-beginning 5)
		 (scan-javafunc-skip-white)
	       (forward-sexp 1 t))
	     (when (= (point) (match-beginning 0))
	       (goto-char(match-end 0))))))
    (save-restriction
      (narrow-to-region beg (point-max))
      (setq count (scan-java-function-1 fn count class)))
    count))

(defun java-maketags (file count)
  (java-mode)
  (scan-java-function #'(lambda (point class name argb arge)
			  (format t "~A~A~A~D~A"
				  class name file
				  (save-excursion
				    (goto-char point)
				    (goto-bol)
				    (point))
				  (buffer-substring argb arge))
			  (do-events))
		      #'(lambda (point class name)
			  (format t "~A~A~A~D"
				  (if class class "") name file
				  (save-excursion
				    (goto-char point)
				    (goto-bol)
				    (point))))
		      count))

(defun java-build-summary-of-functions ()
  (let ((result nil))
    (goto-char (point-min))
    (scan-java-function #'(lambda (point class name argb arge)
			    (push (list (save-excursion
					  (goto-char point)
					  (current-line-number))
					(concat class "#" name
						(substitute-string
						 (buffer-substring argb arge)
						 "[ \t\n]+" " ")))
				  result))
			#'(lambda (point class name)
			    (push (list (save-excursion
					  (goto-char point)
					  (current-line-number))
					(if class (concat class "#" name) name))
				  result))
			0)
    (sort result #'< :key #'car)))
