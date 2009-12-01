;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "cfns")

(in-package "editor")

(defvar *scan-cfunc-mod-list-regexp*
  (compile-regexp
   "\\(const\\|throw\\|_THROW0\\|_THROW1\\|__STL_NOTHROW\\)\\([^A-Za-z0-9_]\\|$\\)"))

(defvar *scan-cfunc-keyword-regexp*
  (compile-regexp
   "if\\|for\\|while\\|switch\\|return\\|catch\\|new\\|delete\\|sizeof\\|__?declspec\\|__?except"))

(defvar *scan-cfunc-afx-macro-regexp*
  (compile-regexp
   "BEGIN_CONNECTION_MAP\\|BEGIN_CONNECTION_PART\\|BEGIN_DISPATCH_MAP\\|BEGIN_EVENTSINK_MAP\\|\
BEGIN_EVENT_MAP\\|BEGIN_INTERFACE_MAP\\|BEGIN_INTERFACE_PART\\|BEGIN_INTERFACE_PART_DERIVE\\|\
BEGIN_MESSAGE_MAP\\|BEGIN_OLECMD_MAP\\|BEGIN_OLEFACTORY\\|BEGIN_PARSE_MAP\\|BEGIN_PROPPAGEIDS\\|\
DECLARE_CONNECTION_MAP\\|DECLARE_DISPATCH_MAP\\|DECLARE_DYNAMIC\\|DECLARE_DYNCREATE\\|\
DECLARE_EVENTSINK_MAP\\|DECLARE_EVENT_MAP\\|DECLARE_INTERFACE_MAP\\|DECLARE_MESSAGE_MAP\\|\
DECLARE_OLECMD_MAP\\|DECLARE_OLECREATE\\|DECLARE_OLECREATE_EX\\|DECLARE_OLECTLTYPE\\|\
DECLARE_OLETYPELIB\\|DECLARE_PARSE_MAP\\|DECLARE_PROPPAGEIDS\\|DECLARE_SERIAL\\|\
END_CONNECTION_MAP\\|END_CONNECTION_PART\\|END_DISPATCH_MAP\\|END_EVENTSINK_MAP\\|\
END_EVENT_MAP\\|END_INTERFACE_MAP\\|END_INTERFACE_PART\\|END_MESSAGE_MAP\\|END_OLECMD_MAP\\|\
END_OLEFACTORY\\|END_PARSE_MAP\\|END_PROPPAGEIDS\\|IMPLEMENT_DYNAMIC\\|IMPLEMENT_DYNCREATE\\|\
IMPLEMENT_OLECREATE\\|IMPLEMENT_OLECREATE_EX\\|IMPLEMENT_OLECTLTYPE\\|IMPLEMENT_OLETYPELIB\\|\
IMPLEMENT_RUNTIMECLASS\\|IMPLEMENT_SERIAL\\|INIT_INTERFACE_PART\\|INIT_INTERFACE_PART_DERIVE\\|\
INTERFACE_AGGREGATE\\|INTERFACE_PART\\|STDMETHODIMP_\\|CATCH\\|AND_CATCH"))

(defvar *scan-cfunc-funcname-regexp*
  (compile-regexp "\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\r\n]*(\\|\\(\\_<typedef\\_>\\)\\|\\(/[/*]\\)\\|\\(['\"]\\)"))

(defvar *scan-cfunc-define-regexp*
  (compile-regexp "^[ \t]*#[ \t]*define[ \t]+\\([A-Za-z0-9_]+\\)("))

(defvar *scan-cfunc-class-regexp*
  (compile-regexp "\\(^\\|[ \t>]\\)\\(?:\\(?:static\\|extern\\|const\\|volatile\\)[ \t]+\\)*\\(class\\|struct\\|interface\\)[ \t\r\n\f]+\\([A-Za-z_][A-Za-z0-9_]*\\)?\\|\\(/[/*]\\)\\|\\(['\"]\\)"))

(defvar *scan-cfunc-comment-regexp*
  (compile-regexp "/[/*]"))

(defvar *scan-cfunc-functail-regexp* (compile-regexp "[A-Za-z_:{]"))
(defvar *scan-cfunc-const-regexp* (compile-regexp "const\\($\\|[^A-Za-z0-9_]\\)"))

(defun scan-cfunc-skip-white ()
  (loop
    (skip-white-forward)
    (unless (skip-chars-forward "\n")
      (unless (looking-for "#")
	(return))
      (while (and (progn
		    (goto-eol)
		    (looking-back "\\"))
		  (forward-line 1))))))

(defun scan-cfunc-in-macro-p ()
  (let ((opoint (point)))
    (goto-bol)
    (cond ((or (looking-back "\\\n")
	       (progn
		 (skip-chars-forward " \t")
		 (looking-for "#")))
	   (or (scan-buffer "^$\\|[^\\]$" :regexp t)
	       (goto-char (point-max)))
	   (forward-char 1)
	   t)
	  (t
	   (goto-char opoint)
	   nil))))

(defun scan-cfunc-find-brace ()
  (while (not (eobp))
    (scan-cfunc-skip-white)
    (cond ((looking-for ";")
	   (forward-char 1))
	  ((looking-for "{")
	   (return t))
	  (t
	   (or (forward-sexp 1 t)
	       (forward-char 1))))))

(defun scan-cfunc-get-class ()
  (let ((name nil))
    (loop
      (skip-chars-backward " \t\r\n\f")
      (unless (looking-back "::")
	(return name))
      (forward-char -2)
      (skip-chars-backward " \t\r\n\f")
      (when (looking-back ">")
	(let ((depth 1))
	  (loop
	    (forward-char -1)
	    (skip-chars-backward "^<>")
	    (cond ((looking-back ">")
		   (incf depth))
		  ((looking-back "<")
		   (decf depth)
		   (when (zerop depth)
		     (forward-char -1)
		     (return)))
		  (t
		   (return))))))
      (skip-chars-backward " \t\r\n\f")
      (let ((e (point)))
	(unless (skip-chars-backward "A-Za-z0-9_")
	  (return name))
	(setq name (if name
		       (concat (buffer-substring (point) e) "::" name)
		     (buffer-substring (point) e)))))))

(defun scan-cfunc-parse-arg (point)
  (save-excursion
    (goto-char point)
    (skip-chars-forward " \t\r\n\f")
    (if (looking-at *scan-cfunc-const-regexp*)
	(+ (point) 5)
      point)))

(defun scan-c-function-1 (fn cfn count in-class)
  (goto-char (point-min))
  (while (scan-buffer *scan-cfunc-funcname-regexp*)
    (cond ((match-beginning 1)
	   (unless (scan-cfunc-in-macro-p)
	     (let ((nameb (match-beginning 1))
		   (namee (match-end 1))
		   (end (match-end 0))
		   argb arge)
	       (cond ((or (and (looking-at *scan-cfunc-keyword-regexp*)
			       (= (match-end 0) namee))
			  (and (looking-at *scan-cfunc-afx-macro-regexp*)
			       (= (match-end 0) namee)))
		      (goto-char end))
		     (t
		      (goto-char (- end 1))
		      (setq argb (point))
		      (when (forward-sexp 1 t)
			(setq arge (point))
			(scan-cfunc-skip-white)
			(while (looking-at *scan-cfunc-mod-list-regexp*)
			  (goto-char (match-end 1))
			  (scan-cfunc-skip-white)
			  (when (looking-for "(")
			    (forward-sexp 1 t)
			    (scan-cfunc-skip-white)))
			(when (looking-at *scan-cfunc-functail-regexp*)
			  (let ((colon (looking-for ":"))
				class name)
			    (save-excursion
			      (goto-char nameb)
			      (when (looking-back "~")
				(forward-char -1)
				(setq nameb (point)))
			      (setq class (if in-class in-class (scan-cfunc-get-class))))
			    (setq name (buffer-substring nameb namee))
			    (when (or (not colon)
				      (and class
					   (string-match (concat "\\(^\\|:\\)" name "$") class)))
			      (funcall fn nameb class (buffer-substring nameb namee)
				       argb (if class (scan-cfunc-parse-arg arge) arge))
			      (incf count)
			      (when colon
				(scan-cfunc-find-brace)))))))))))
	  ((match-beginning 2)
	   (goto-char (match-end 2))
	   (when (forward-sexp 1 t)
	     (let ((opoint (point))
		   (end (progn
			  (skip-chars-forward "^;{}")
			  (point))))
	       (when (looking-for ";")
		 (goto-char opoint)
		 (while (< (point) end)
		   (skip-chars-forward " \t\r\n\f")
		   (cond ((looking-at "(\\**\\([A-Za-z0-9_]+\\)")
			  (funcall cfn opoint "typedef" in-class
				   (buffer-substring (match-beginning 1) (match-end 1)))
			  (while (and (forward-sexp 1 t)
				      (progn
					(skip-chars-forward " \t\r\n\f")
					(looking-at "[^;,{}]"))
				      (< (point) end)))
			  (or (looking-at "[;,]")
			      (return)))
			 ((looking-at "[[(]")
			  (or (forward-sexp 1 t)
			      (return)))
			 ((looking-for ";")
			  (return))
			 ((looking-for ",")
			  (or (forward-char 1)
			      (return)))
			 (t
			  (let ((p1 (point))
				(p2 (progn
				      (forward-sexp 1 t)
				      (point))))
			    (when (or (= p1 p2)
				      (> p2 end))
			      (return))
			    (goto-char p1)
			    (when (prog1
				      (scan-buffer "[A-Za-z0-9_]+" :regexp t :tail t :limit p2)
				    (goto-char p2))
			      (let ((b (match-beginning 0))
				    (e (match-end 0)))
				(unless (= b e)
				  (skip-chars-forward " \t\r\n\f")
				  (when (looking-at "[;,([]")
				    (funcall cfn opoint "typedef" in-class
					     (buffer-substring b e))))))))))))))
	  (t
	   (let ((opoint (point)))
	     (if (match-beginning 3)
		 (skip-white-forward)
	       (forward-sexp 1 t))
	     (and (= opoint (point))
		  (not (forward-char 1))
		  (return))))))
  (goto-char (point-min))
  (while (scan-buffer *scan-cfunc-define-regexp* :tail t)
    (let ((opoint (match-beginning 0)))
      (when (progn
	      (forward-char -1)
	      (setq argb (point))
	      (forward-sexp 1 t))
	(funcall fn opoint nil (match-string 1) argb (point))
	(incf count))))
  count)

(defun scan-cfunc-find-semi-or-brace ()
  (while (not (eobp))
    (scan-cfunc-skip-white)
    (cond ((looking-for ";")
	   (return nil))
	  ((looking-for "{")
	   (return t))
	  (t
	   (or (forward-sexp 1 t)
	       (forward-char 1))))))

(defun scan-cfunc-scan-class-name (beg end)
  (while (not (eobp))
    (scan-cfunc-skip-white)
    (cond ((looking-for "{")
	   (return (if beg (buffer-substring beg end) 't)))
	  ((looking-for ":")
	   (return (while (not (eobp))
		     (scan-cfunc-skip-white)
		     (cond ((looking-for ";")
			    (return nil))
			   ((looking-for "{")
			    (return (buffer-substring beg end)))
			   ((looking-at "[<>,]")
			    (forward-char 1))
			   (t
			    (or (forward-sexp 1 t)
				(forward-char 1)))))))
	  ((looking-for "(")
	   (or (forward-sexp 1 t)
	       (return nil)))
	  ((looking-at "[A-Za-z_]")
	   (setq beg (point))
	   (skip-chars-forward "A-Za-z0-9_")
	   (setq end (point)))
	  (t
	   (return nil)))))

(defun scan-cfunc-typedef-struct (cfn beg in-class)
  (when (save-excursion
	  (goto-char beg)
	  (skip-chars-backward " \t\n\r\f")
	  (skip-chars-backward "A-Za-z0-9_")
	  (looking-at "typedef[ \t\n\r\f]"))
    (let ((end (save-excursion
		 (skip-chars-forward "^;{}")
		 (point))))
      (while (scan-buffer "[A-Za-z0-9_]+" :regexp t :tail t :limit end)
	(funcall cfn beg "typedef" in-class
		 (buffer-substring (match-beginning 0) (match-end 0)))))))

(defun scan-c-function (fn cfn count &optional in-class)
  (let ((beg (point-min))
	end next name)
    (goto-char beg)
    (while (scan-buffer *scan-cfunc-class-regexp* :tail t)
      (cond ((match-beginning 1)
	     (unless (scan-cfunc-in-macro-p)
	       (let ((match-beg (match-beginning 1))
		     (class-beg (match-beginning 2))
		     (class-end (match-end 2)))
		 (setq name (scan-cfunc-scan-class-name (match-beginning 3)
							(match-end 3)))
		 (when name
		   (if (eq name 't)
		       (setq name "<__unnamed>")
		     (funcall cfn class-beg (buffer-substring class-beg class-end)
			      in-class name))
		   (setq end (point))
		   (cond ((forward-sexp 1 t)
			  (scan-cfunc-typedef-struct cfn match-beg in-class))
			 (t
			  (if (scan-buffer "^};" :regexp t)
			      (forward-line -1)
			    (goto-char (point-max)))))
		   (setq next (point))
		   (save-restriction
		     (narrow-to-region beg end)
		     (setq count (scan-c-function-1 fn cfn count in-class)))
		   (save-restriction
		     (narrow-to-region end next)
		     (setq count (scan-c-function fn cfn count
						  (if in-class (concat in-class "::" name) name)))
		     (setq beg next))))))
	    (t
	     (goto-char (match-beginning 0))
	     (if (match-beginning 4)
		 (skip-white-forward)
	       (forward-sexp 1 t))
	     (when (= (point) (match-beginning 0))
	       (goto-char(match-end 0))))))
    (save-restriction
      (narrow-to-region beg (point-max))
      (setq count (scan-c-function-1 fn cfn count in-class)))
    count))

(defun scan-cfunc-remove-comments ()
  (save-excursion
    (goto-char (point-min))
    (while (scan-buffer *scan-cfunc-comment-regexp*)
      (let ((beg (point))
	    (end (progn
		   (skip-white-forward)
		   (point))))
	(when (= beg end)
	  (return))
	(goto-char beg)
	(replace-buffer "." " " :regexp t :limit end)))))

(defun c-maketags (file count)
  (c++-mode)
  (scan-cfunc-remove-comments)
  (scan-c-function #'(lambda (point class name argb arge)
		       (format t "~A~A~A~D~A"
			       (if class class "") name file
			       (save-excursion
				 (goto-char point)
				 (goto-bol)
				 (point))
			       (buffer-substring argb arge))
		       (do-events))
		   #'(lambda (point key class name)
		       (format t "~A~A~A~D"
			       (if class class "") name file
			       (save-excursion
				 (goto-char point)
				 (goto-bol)
				 (point))))
		   count))

(defun c-build-summary-of-functions ()
  (let ((result nil))
    (let ((beg (point-min))
	  (end (point-max))
	  (contents (save-restriction
		      (widen)
		      (buffer-substring (point-min) (point-max))))
	  (mode buffer-mode)
	  buffer)
      (unwind-protect
	  (progn
	    (setq buffer (create-new-buffer "*cfns*"))
	    (set-buffer buffer)
	    (funcall mode)
	    (insert contents)
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (scan-c-function #'(lambda (point class name argb arge)
				 (push (list (save-excursion
					       (goto-char point)
					       (current-line-number))
					     (let ((args (substitute-string
							  (buffer-substring argb arge)
							  "[ \t\n]+" " ")))
					       (if class
						   (concat class "::" name args)
						 (concat name args))))
				       result))
			     #'(lambda (point key class name)
				 (push (list (save-excursion
					       (goto-char point)
					       (current-line-number))
					     (concat key " " class
						     (and class "::") name))
				       result))
			     0)
	    (sort result #'< :key #'car))
	(and buffer (delete-buffer buffer))))))
