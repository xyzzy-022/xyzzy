;;; -*- Mode: Lisp -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "help")

(in-package "lisp")

(export 'documentation)

(defconstant *documentation-type-list*
  '(variable function structure type setf))

(export *documentation-type-list*)

(defconstant *documentation-prop-list*
  '(variable-documentation
    function-documentation
    structure-documentation
    type-documentation
    setf-documentation))

(mapc #'(lambda (sym type)
	  (setf (get sym 'documentation-type) type))
      *documentation-type-list*
      *documentation-prop-list*)

(defvar *documentation-path* nil)

(defun documentation-path ()
  (or *documentation-path*
      (setq *documentation-path* (merge-pathnames "DOC" (ed:etc-path)))))

(defun snarf-documentation ()
  (create-directory (ed:etc-path) :if-exists :skip)
  (si:*snarf-documentation (documentation-path)
			   *documentation-prop-list*))

(defun documentation (symbol doc-type)
  (if (and (consp symbol)
	   (eq (car symbol) 'setf)
	   (eq doc-type 'function))
      (si:get-documentation-string (cadr symbol) 'setf-documentation
				   nil (documentation-path))
    (si:get-documentation-string symbol (get doc-type 'documentation-type)
				 nil (documentation-path))))

(in-package "editor")

(export '(apropos command-apropos *apropos-no-regexp*
	  view-lossage key-to-string lookup-key-command
	  describe-key describe-key-briefly
	  describe-function describe-variable))

(defvar *apropos-no-regexp* nil)

(defun print-key-sequence (keys &optional prefix)
  (unless prefix
    (setq prefix ""))
  (while keys
    (let ((first (car keys)))
      (if (consp first)
	  (print-key-sequence (cdr first)
			      (format nil "~A~:C " prefix (car first)))
	(progn
	  (let ((x (1+ (char-code first))))
	    (while (let ((tem (cadr keys)))
		     (and (characterp tem)
			  (eql x (char-code tem))))
	      (setq keys (cdr keys))
	      (setq x (1+ x))))
	  (format t "~A~:C" prefix first)
	  (unless (eql first (car keys))
	    (format t " .. ~A~:C" prefix (car keys))))))
    (setq keys (cdr keys))
    (when keys
      (princ ", "))))

(defun apropos-find-matches (pattern pred)
  (let ((regexp (compile-regexp pattern))
	(result nil))
    (cond ((and regexp pred)
	   (do-symbols (sym)
	     (and (funcall pred sym)
		  (string-match regexp (symbol-name sym))
		  (push sym result))))
	  (pred
	   (do-symbols (sym)
	     (and (funcall pred sym)
		  (push sym result))))
	  (regexp
	   (do-symbols (sym)
	     (and (string-match regexp (symbol-name sym))
		  (push sym result))))
	  (t
	   (do-symbols (sym)
	     (push sym result))))
    result))

(defun apropos (string &optional print-key pred)
  "指定した正規表現に一致するシンボルを表示します"
  (interactive "sApropos(Regexp): \np")
  (if *apropos-no-regexp*
      (setq string (regexp-quote string)))
  (long-operation
    (with-output-to-temp-buffer ("*Help*")
      (message "Building apropos list...")
      (let ((local-keymap (local-keymap))
	    (doc-path (lisp::documentation-path)))
	(dolist (sym (sort (apropos-find-matches string pred) #'string<))
	  (prin1 sym)
	  (when (and print-key (commandp sym))
	    (let ((keys (command-keys sym *global-keymap* local-keymap)))
	      (when keys
		(format t "~40T")
		(print-key-sequence keys))))
	  (terpri)
	  (let ((doc (si:get-documentation-string
		      sym 'lisp::function-documentation t doc-path)))
	    (when doc
	      (format t "  Function: ~A~%" doc)))
	  (let ((doc (si:get-documentation-string
		      sym 'lisp::variable-documentation t doc-path)))
	    (when doc
	      (format t "  Variable: ~A~%" doc)))))
      (message "Building apropos list...done"))))

(defun command-apropos (string)
  (interactive "sCommand apropos: ")
  (apropos string t #'commandp))

(defun view-lossage ()
  (interactive)
  (let ((keys (get-recent-keys)))
    (with-output-to-temp-buffer ("*Help*")
      (dotimes (x (length keys))
	(format t "~:C " (svref keys x))
	(when (> (si:*stream-column) 64)
	  (terpri)))))
  t)

(defun key-to-string (key &optional esc-to-meta)
  (if (and esc-to-meta
	   (consp key)
	   (eql (car key) #\ESC)
	   (= (length key) 2))
      (format nil "M-~:C" (cadr key))
    (format nil "~:[~:C~;~{~:C~#[~:; ~]~}~]" (consp key) key)))

(defun lookup-key-command (key)
  (let ((bound (mapcar #'(lambda (x)
			   (when (keymapp x)
			     (lookup-keymap x key)))
		       (append (list (current-selection-keymap))
			       (minor-mode-map)
			       (list (local-keymap))
			       (list *global-keymap*)))))
    (or (find-if-not #'keymapp bound)
	(find-if #'identity bound))))

(defun describe-key-briefly (key &optional arg)
  (interactive "kDescribe key briefly: \np")
  (let ((command (lookup-key-command key)))
    (setq key (key-to-string key))
    (cond ((null command)
	   (message "~a is not bound" key))
	  (arg
	   (save-excursion
	    (insert (format nil "~a (~a)" key command))))
	  (t
	   (message "~a runs the command ~A" key command)))))

(defun describe-key (key)
  (interactive "kDescribe key: ")
  (let ((command (lookup-key-command key)))
    (if command
	(with-output-to-temp-buffer ("*Help*")
	  (format t "~a runs the command ~A~%" (key-to-string key) command)
	  (let ((doc (and (symbolp command)
			  (documentation command 'function))))
	    (when doc
	      (format t "~%Documentation:~%~%~A~%" doc))))
      (message "~a is not bound" (key-to-string key)))))

(defun describe-function (symbol)
  (interactive "aDescribe function: ")
  (let ((def (symbol-function symbol))
	args)
    (cond ((si:*builtin-function-p def)
	   (setq args (get symbol 'si::builtin-function-argument)))
	  (t
	   (if (si:*closurep def)
	       (setq def (si:closure-body def)))
	   (when (or (eq (safe-car def) 'lambda)
		     (eq (safe-car def) 'macro))
	     (setq args (safe-cadr def)))))
    (with-output-to-temp-buffer ("*Help*")
      (format t "~s: " symbol)
      (cond ((stringp def)
	     (princ "a keyboard macro."))
	    ((special-form-p symbol)
	     (princ "a special form."))
	    ((si:*builtin-function-p def)
	     (if (macro-function symbol)
		 (princ "a built-in macro.")
	       (progn
		 (princ (if (commandp def) "an interactive " "a "))
		 (princ "built-in function."))))
	    ((si:*dll-function-p def)
	     (princ "a DLL function."))
	    ((si:*c-callable-p def)
	     (princ "a c-callable Lisp function."))
	    ((eq (safe-car def) 'lambda)
	     (let ((commandp (commandp def))
		   (autoloadp (autoload-function-p def)))
	       (princ (if (or commandp autoloadp) "an " "a "))
	       (and commandp (princ "interactive "))
	       (and autoloadp (princ "autoloaded ")))
	     (princ "Lisp function."))
	    ((eq (safe-car def) 'macro)
	     (princ (if (autoload-function-p def) "an autoloaded " "a "))
	     (princ "Lisp macro.")))
      (format t "~%~%~a" symbol)
      (if (stringp args)
	  (format t " ~a" args)
	(labels ((print-sexp (sexp)
			     (cond ((null sexp)
				    (princ "()"))
				   ((listp sexp)
				    (cond ((eq (car sexp) 'quote)
					   (princ "'")
					   (print-sexp (cadr sexp)))
					  ((eq (car sexp) 'function)
					   (princ "#'")
					   (print-sexp (cadr sexp)))
					  (t
					   (print-list sexp "(")
					   (princ ")"))))
				   ((member sexp lambda-list-keywords)
				    (format t "~a" sexp))
				   ((keywordp sexp)
				    (format t ":~:@(~a~)" sexp))
				   (t
				    (format t "~:@(~a~)" sexp))))
		 (print-list (args prefix)
			     (dolist (arg args)
			       (princ prefix)
			       (print-sexp arg)
			       (setq prefix " "))))
	  (print-list args " ")))
      (princ "\n\n")
      (let ((doc (documentation symbol 'function)))
	(princ (if doc doc "not documented."))
	(terpri))))
  t)

(defun describe-variable (symbol)
  (interactive "vDescribe variable: ")
  (with-output-to-temp-buffer ("*Help*")
    (format t "~s's value is " symbol)
    (if (boundp symbol)
	(prin1 (symbol-value symbol))
      (princ "void."))
    (terpri)
    (cond ((constantp symbol)
	   (format t "~s is constant.~%" symbol))
	  ((si:*specialp symbol)
	   (format t "~s is special.~%" symbol)))
    (terpri)
    (let ((doc (documentation symbol 'variable)))
      (princ (if doc doc "not documented."))
      (terpri)))
  t)
