;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "basic-mode")

(in-package "editor")

(export '(*basic-mode-hook* *basic-mode-map* basic-mode
	  *basic-mode-abbrev-table* *basic-mode-syntax-table*
	  *basic-indent-level* *basic-continued-line-offset*
	  *basic-label-offset* *basic-tab-always-indent*
	  *basic-auto-close* *basic-auto-cap* *basic-auto-correct-else-if*
	  *basic-auto-correct-then* *basic-auto-correct-operators*
	  *basic-comment-column*
	  *basic-popup-completion-list*
	  basic-indent-line basic-newline-and-indent
	  basic-electric-close basic-electric-cap
	  basic-forward-block basic-backward-block
	  basic-beginning-of-defun basic-end-of-defun
	  basic-correct-operators
	  basic-indent-sexp basic-completion))

(defvar *basic-indent-level* 2)
(defvar *basic-continued-line-offset* 2)
(defvar *basic-label-offset* -2)
(defvar *basic-tab-always-indent* t)
(defvar *basic-auto-close* t)
(defvar *basic-auto-cap* t)
(defvar *basic-auto-correct-then* t)
(defvar *basic-auto-correct-else-if* t)
(defvar *basic-auto-correct-operators* t)
(defvar *basic-mode-hook* nil)
(defvar *basic-comment-column* nil)
(defvar *basic-popup-completion-list* nil)

(defvar *basic-mode-map* nil)
(unless *basic-mode-map*
  (setq *basic-mode-map* (make-sparse-keymap))
  (define-key *basic-mode-map* #\TAB 'basic-indent-line)
  (define-key *basic-mode-map* #\RET 'basic-newline-and-indent)
  (define-key *basic-mode-map* #\SPC 'basic-electric-close)
  (define-key *basic-mode-map* #\# 'basic-electric-insert)
  (define-key *basic-mode-map* #\: 'basic-electric-insert)
  (define-key *basic-mode-map* #\( 'basic-electric-cap)
  (define-key *basic-mode-map* #\$ 'basic-electric-cap)
  (define-key *basic-mode-map* #\= 'basic-correct-operators)
  (define-key *basic-mode-map* #\& 'basic-correct-operators)
  (define-key *basic-mode-map* #\| 'basic-correct-operators)
  (define-key *basic-mode-map* '(#\ESC #\TAB) 'basic-completion)
  (define-key *basic-mode-map* #\C-M-q 'basic-indent-sexp)
  (define-key *basic-mode-map* #\C-h 'backward-delete-char-untabify-or-selection))

(defvar *basic-keyword-hash-table* nil)
(defvar *basic-keyword-file* "Basic")
(defvar *basic-keyword-cap-table* nil)
(defvar *basic-completion-list* nil)

(defvar *basic-mode-syntax-table* nil)
(unless *basic-mode-syntax-table*
  (setq *basic-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))
      ((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *basic-mode-syntax-table* c))))
  (set-syntax-string *basic-mode-syntax-table* #\")
  (set-syntax-symbol *basic-mode-syntax-table* #\_)
  (set-syntax-match *basic-mode-syntax-table* #\( #\))
  (set-syntax-start-comment *basic-mode-syntax-table* #\')
  (set-syntax-end-comment *basic-mode-syntax-table* #\LFD nil t))

(defvar *basic-mode-abbrev-table* nil)
(unless *basic-mode-abbrev-table*
  (define-abbrev-table '*basic-mode-abbrev-table*))

(defvar *basic-block-begin-regexp*
  (compile-regexp
   "\\(Begin\\|Do\\|For\\|If\\|Select\\|While\\|With\\|BeginProperty\\|\
\\(Public[ \t]+\\|Private[ \t]+\\)?\\(Type\\|Enum\\)\\|\
\\(Public[ \t]+\\|Private[ \t]+\\|Friend[ \t]+\\)?\\(Static[ \t]+\\)?\\(Function\\|Sub\\|Property\\)\\)\
\\([^A-Za-z0-9_]\\|$\\)" t))
(defvar *basic-block-end-regexp*
  (compile-regexp "\\(Loop\\|End\\|Next\\|Wend\\|EndProperty\\)\\([^A-Za-z0-9_\n]\\|$\\)" t))
(defvar *basic-block-mid-regexp*
  (compile-regexp "\\(ElseIf\\|Else\\|Case\\)\\([^A-Za-z0-9_\n]\\|$\\)" t))
(defvar *basic-label-regexp*
  (compile-regexp "\\([A-Za-z0-9_]\\|\\sk\\|\\sj\\)+[ \t]*:" t))
(defvar *basic-comment-regexp* "['#]")

(defvar *basic-procedure-regexp*
  (compile-regexp
   "^[ \t]*\\(\\(Public[ \t]+\\|Private[ \t]+\\|Friend[ \t]+\\)?\\(Static[ \t]+\\)?\
\\(Function\\|Sub\\|Property\\)\\|\
End[ \t]+\\(Function\\|Sub\\|Property\\)\\)[^A-Za-z0-9_]" t))

(defvar *basic-block-regexp*
  (compile-regexp
   "^[ \t]*\\(Begin\\|Do\\|For\\|If\\|Select\\|While\\|With\\|BeginProperty\\|\
Loop\\|End\\|Next\\|Wend\\|EndProperty\\|\
\\(Public[ \t]+\\|Private[ \t]+\\)?\\(Type\\|Enum\\)\\|\
\\(Public[ \t]+\\|Private[ \t]+\\|Friend[ \t]+\\)?\\(Static[ \t]+\\)?\\(Function\\|Sub\\|Property\\)\\)\
\\([^A-Za-z0-9_]\\|$\\)" t))

(defvar *basic-beginning-of-defun-noname-regexp*
  "^[ \t]*\\(Public[ \t]+\\|Private[ \t]+\\|Friend[ \t]+\\)?\\(Static[ \t]+\\|Declare[ \t]+\\)?\\(Function\\|Sub\\|Property\\)[ \t]+")

(defvar *basic-beginning-of-defun-regexp*
  (compile-regexp (concat *basic-beginning-of-defun-noname-regexp* "\\([A-Za-z0-9_]+\\)") t))

(defvar *basic-end-of-defun-regexp*
  (compile-regexp "^[ \t]*End[ \t]+\\(Function\\|Sub\\|Property\\)\\([ \t]+\\|$\\)" t))

(pushnew '(basic-maketags "*.bas" "*.frm" "*.mb" "*.cls") *maketags-list* :key #'car)

(defun basic-maketags (file count)
  (goto-char (point-min))
  (while (scan-buffer *basic-beginning-of-defun-regexp* :regexp t :case-fold t :tail t)
    (do-events)
    (format t "~A~A~D - ~A" (match-string 4) file (match-beginning 0) file)
    (incf count))
  count)

(defun tags-find-basic-point (class name functionp point)
  (goto-char (- point *jump-tag-limit*))
  (and (scan-buffer (concat *basic-beginning-of-defun-noname-regexp*
			    (regexp-quote name)
			    "[ \t]+")
		    :regexp t :case-fold t :limit (+ point *jump-tag-limit*))
       (point)))

(defun basic-build-summary-of-functions ()
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (scan-buffer *basic-beginning-of-defun-regexp* :regexp t :case-fold t :tail t)
	(push (list (current-line-number) (match-string 4)) result)))
    (nreverse result)))

(defun basic-beginning-of-defun (&optional (arg 1))
  (interactive "p")
  (cond ((plusp arg)
	 (dotimes (i arg t)
	   (unless (scan-buffer *basic-beginning-of-defun-regexp*
				:regexp t :case-fold t :reverse t :no-dup t)
	     (goto-char (point-min))
	     (return nil))))
	((minusp arg)
	 (dotimes (i (- arg) t)
	   (unless (scan-buffer *basic-end-of-defun-regexp* :regexp t :case-fold t)
	     (goto-char (point-max))
	     (return nil))
	   (or (forward-line 1)
	       (goto-eol))))))

(defun basic-end-of-defun (&optional (arg 1))
  (interactive "p")
  (basic-beginning-of-defun (- arg)))

(defun basic-continuation-line-p (&optional (n -3))
  (save-excursion
    (and (forward-char n)
	 (looking-at "\\(^\\|[^A-Za-z0-9_]\\)_"))))

(defun basic-eol-p ()
  (save-restriction
    (narrow-to-region (point) (progn (goto-eol) (point)))
    (goto-char (point-min))
    (skip-white-forward)
    (eolp)))

(defun basic-single-line-if-p ()
  (save-excursion
    (save-restriction
      (narrow-to-region
       (point)
       (progn
	 (loop
	   (unless (forward-line 1)
	     (goto-eol)
	     (return))
	   (unless (basic-continuation-line-p)
	     (return)))
	 (point)))
      (goto-char (point-min))
      (while (forward-sexp 1 t)
	(when (and (looking-back "Then" t)
		   (save-excursion
		     (= (- (point) 4) (progn (forward-sexp -1 t) (point)))))
	  (return (not (basic-eol-p))))))))

(defun basic-in-procedure-level-p ()
  (and (scan-buffer *basic-procedure-regexp* :regexp t :reverse t :case-fold t)
       (not (looking-for "End" t))))

(defun basic-in-comment-or-string-p ()
  (save-excursion
    (let ((end (point)))
      (goto-bol)
      (while (scan-buffer "['\"]" :regexp t :limit end)
	(when (or (looking-for "'")
		  (not (scan-buffer "\"" :limit end :tail t :no-dup t)))
	  (return t))))))

(defun basic-end-stmt-p ()
  (save-excursion
    (and (progn
	   (forward-char 3)
	   (basic-eol-p))
	 (basic-in-procedure-level-p))))

(defun calc-basic-indent ()
  (save-excursion
    (goto-bol)
    (when (looking-at "[ \t]*#")
      (return-from calc-basic-indent 0))
    (let* ((single-line-if-p nil)
	   (column
	    (save-excursion
	      (loop
		(or (forward-line -1)
		    (return 0))
		(unless (basic-continuation-line-p)
		  (skip-chars-forward " \t")
		  (cond ((eolp))
			((looking-at *basic-comment-regexp*))
			((looking-at *basic-label-regexp*))
			((looking-at *basic-block-begin-regexp* t)
			 (if (and (looking-for "If" t)
				  (basic-single-line-if-p))
			     (progn
			       (setq single-line-if-p t)
			       (return (current-column)))
			   (return (+ (current-column) *basic-indent-level*))))
			((looking-at *basic-block-end-regexp* t)
			 (return (current-column)))
			((looking-at *basic-block-mid-regexp* t)
			 (return (+ (current-column) *basic-indent-level*)))
			(t
			 (return (current-column)))))))))
      (goto-bol)
      (if (basic-continuation-line-p)
	  (progn
	    (if single-line-if-p
		(incf column *basic-indent-level*))
	    (incf column *basic-continued-line-offset*))
	(progn
	  (skip-chars-forward " \t")
	  (cond ((looking-at *basic-block-end-regexp* t)
		 (unless (and (looking-for "End" t)
			      (basic-end-stmt-p))
		   (decf column *basic-indent-level*)))
		((looking-at *basic-block-mid-regexp* t)
		 (decf column *basic-indent-level*))
		((looking-at *basic-label-regexp*)
		 (incf column *basic-label-offset*)))))
      (max column 0))))

(defun basic-indent-mid-or-end-block ()
  (when (save-excursion
	  (let ((point (point)))
	    (goto-bol)
	    (skip-chars-forward " \t")
	    (and (or (looking-at *basic-block-mid-regexp* t)
		     (looking-at *basic-block-end-regexp* t))
		 (= point (match-end 0)))))
    (basic-indent-line)))

(defun basic-indent-line ()
  (interactive "*")
  (if (or (not (interactive-p))
	  *basic-tab-always-indent*
	  (save-excursion
	    (skip-chars-backward " \t")
	    (bolp)))
      (smart-indentation (calc-basic-indent))
    (insert "\t"))
  t)

(defun basic-newline-and-indent (&optional (arg 1))
  (interactive "*p")
  (basic-auto-cap)
  (when *basic-auto-correct-then*
    (basic-correct-then))
  (delete-trailing-spaces)
  (let ((eol (point)))
    (insert #\LFD arg)
    (save-excursion
      (goto-char eol)
      (basic-indent-mid-or-end-block)))
  (basic-indent-line))

(defun basic-indent-sexp ()
  (interactive "*")
  (indent-region (point)
		 (save-excursion
		   (basic-forward-block)
		   (or (forward-line 1)
		       (goto-eol))
		   (point))))

(defun basic-electric-insert (&optional (arg 1))
  (interactive "*p")
  (self-insert-command arg)
  (unless (basic-in-comment-or-string-p)
    (basic-indent-line))
  t)

(defun basic-correct-then ()
  (let ((end (point)))
    (when (basic-continuation-line-p -2)
      (return-from basic-correct-then))
    (save-excursion
      (goto-bol)
      (while (and (basic-continuation-line-p)
		  (forward-line -1)))
      (skip-chars-forward " \t")
      (unless (looking-at "\\(If\\|ElseIf\\)\\([^A-Za-z0-9_]\\|$\\)" t)
	(return-from basic-correct-then))
      (save-restriction
	(narrow-to-region (point) end)
	(goto-char (point-min))
	(while (forward-sexp 1 t)
	  (when (and (looking-back "Then" t)
		     (save-excursion
		       (= (- (point) 4) (progn (forward-sexp -1 t) (point)))))
	    (return-from basic-correct-then)))
	(goto-char (point-min))
	(loop
	  (unless (scan-buffer "['\"]" :regexp t)
	    (goto-char (point-max))
	    (return))
	  (when (looking-for "'")
	    (return))
	  (unless (scan-buffer "\"" :tail t :no-dup t)
	    (return-from basic-correct-then)))
	(skip-chars-backward " \t")
	(insert " Then")))
    (goto-char (+ end 5))))

(defun basic-correct-operators (&optional (arg 1))
  (interactive "p")
  (if (and *basic-auto-correct-operators*
	   (eql *last-command-char* (preceding-char))
	   (not (basic-in-comment-or-string-p)))
      (case *last-command-char*
	(#\=)
	(#\&
	 (delete-region (point) (1- (point)))
	 (insert "And"))
	(#\|
	 (delete-region (point) (1- (point)))
	 (insert "Or"))
	(t
	 (self-insert-command arg)))
    (self-insert-command arg)))

(defun basic-ident-equal (i)
  (and (looking-for i t)
       (save-excursion
	 (forward-char (length i))
	 (looking-at "[^A-Za-z0-9_]\\|$"))))

(defun basic-stmt-type ()
  (while (progn
	   (skip-chars-forward " \t")
	   (looking-at "Public\\|Private\\|Friend\\|Static" t))
    (goto-char (match-end 0)))
  (cond ((looking-for "Do" t) (values 'do t))
	((looking-for "Loop" t) 'do)
	((looking-for "For" t) (values 'for t))
	((looking-for "Next" t) 'for)
	((looking-for "While" t) (values 'while t))
	((looking-for "Wend" t) 'while)
	((looking-for "BeginProperty" t) (values 'begin-property t))
	((looking-for "EndProperty" t) 'begin-property)
	((looking-for "If" t)
	 (unless (basic-single-line-if-p)
	   (values 'if t)))
	((looking-for "Select" t) (values 'select t))
	((looking-for "With" t) (values 'with t))
	((looking-for "Type" t) (values 'type t))
	((looking-for "Enum" t) (values 'enum t))
	((looking-for "Function" t) (values 'function t))
	((looking-for "Sub" t) (values 'sub t))
	((looking-for "Property") (values 'property t))
	((looking-for "Begin" t) (values 'begin t))
	((looking-for "End" t)
	 (unless (basic-end-stmt-p)
	   (forward-char 3)
	   (skip-chars-forward " \t")
	   (cond ((basic-ident-equal "If") 'if)
		 ((basic-ident-equal "Select") 'select)
		 ((basic-ident-equal "With") 'with)
		 ((basic-ident-equal "Type") 'type)
		 ((basic-ident-equal "Enum") 'enum)
		 ((basic-ident-equal "Function") 'function)
		 ((basic-ident-equal "Sub") 'sub)
		 ((basic-ident-equal "Property") 'property)
		 ((basic-eol-p) 'begin)
		 (t nil))))
	(t nil)))

(defun basic-forward-block-1 (match)
  (unless (while (scan-buffer *basic-block-regexp* :regexp t :case-fold t)
	    (multiple-value-bind (type open)
		(basic-stmt-type)
	      (cond ((null type))
		    (open
		     (unless (forward-line 1)
		       (return nil))
		     (basic-forward-block-1 type))
		    ((eq type match)
		     (return t))
		    (t
		     (return nil)))
	      (unless (forward-line 1)
		(return nil))))
    (error "Unmatched ~a" match))
  match)

(defun basic-forward-block ()
  (interactive)
  (let ((point (save-excursion
		 (goto-bol)
		 (when (looking-at *basic-block-regexp* t)
		   (multiple-value-bind (type open)
		       (basic-stmt-type)
		     (when (and open (forward-line 1))
		       (basic-forward-block-1 type)
		       (point)))))))
    (when point
      (goto-char point)
      (goto-bol)
      (skip-chars-forward " \t")
      t)))

(defun basic-backward-block-1 (match)
  (unless (while (scan-buffer *basic-block-regexp* :regexp t :case-fold t :reverse t)
	    (multiple-value-bind (type open)
		(basic-stmt-type)
	      (cond ((null type))
		    ((not open)
		     (unless (forward-line -1)
		       (return nil))
		     (basic-backward-block-1 type))
		    ((eq type match)
		     (return t))
		    ((eq match 't)
		     (setq match type)
		     (return t))
		    (t
		     (return nil))))
	    (unless (forward-line -1)
	      (return nil)))
    (error "Unmatched ~a" match))
  match)

(defun basic-backward-block ()
  (interactive)
  (let ((point (save-excursion
		 (goto-bol)
		 (when (looking-at *basic-block-regexp* t)
		   (multiple-value-bind (type open)
		       (basic-stmt-type)
		     (when (and type (not open) (forward-line -1))
		       (basic-backward-block-1 type)
		       (point)))))))
    (when point
      (goto-char point)
      (goto-bol)
      (skip-chars-forward " \t")
      t)))

(defun basic-auto-cap ()
  (when (and *basic-auto-cap*
	     (hash-table-p *basic-keyword-cap-table*))
    (save-excursion
      (let ((end (point)))
	(when (skip-chars-backward "A-Za-z0-9_")
	  (let ((kwd (gethash-region (point) end *basic-keyword-cap-table*)))
	    (when (and kwd
		       (= (length kwd) (- end (point)))
		       (not (looking-for kwd)))
	      (insert kwd)
	      (delete-region (point) (+ (point) (length kwd))))))))))

(defun basic-electric-cap (&optional (arg 1))
  (interactive "*p")
  (unless (basic-in-comment-or-string-p)
    (basic-auto-cap))
  (self-insert-command arg))

(defun basic-electric-close (&optional (arg 1))
  (interactive "*p")
  (if (basic-in-comment-or-string-p)
      (self-insert-command arg)
    (progn
      (basic-auto-cap)
      (if (and *basic-auto-close*
	       (save-excursion
		 (let ((point (point)))
		   (goto-bol)
		   (skip-chars-forward " \t")
		   (and (looking-for "End" t)
			(= (+ (point) 3) point)))))
	  (let (open)
	    (case (save-excursion
		    (and (forward-line -1)
			 (prog1
			     (basic-backward-block-1 't)
			   (setq open (point)))))
	      (if (insert " If"))
	      (select (insert " Select"))
	      (with (insert " With"))
	      (type (insert " Type"))
	      (enum (insert " Enum"))
	      (function (insert " Function"))
	      (sub (insert " Sub"))
	      (property (insert " Property"))
	      (begin)
	      (do (delete-region (point) (- (point) 3))
		  (insert "Loop"))
	      (for (delete-region (point) (- (point) 3))
		   (insert "Next"))
	      (while (delete-region (point) (- (point) 3))
		(insert "Wend"))
	      (begin-property (delete-region (point) (- (point) 3))
			      (insert "EndProperty"))
	      (t
	       (error "Unmatched End")))
	    (basic-indent-line)
	    (save-excursion
	      (goto-char open)
	      (show-matched-parenthesis)))
	(progn
	  (basic-correct-else-if)
	  (self-insert-command arg)
	  (basic-indent-mid-or-end-block)))))
  t)

(defun basic-correct-else-if ()
  (save-excursion
    (let ((point (point)))
      (goto-bol)
      (skip-chars-forward " \t")
      (when (and (looking-at "Else[ \t]+If" t)
		 (= point (match-end 0)))
	(delete-region (+ (match-beginning 0) 4) (- (match-end 0) 2))))))

(defun basic-completion ()
  (interactive)
  (or *basic-completion-list*
      (setq *basic-completion-list* (make-list-from-keyword-table *basic-keyword-hash-table*))
      (return-from basic-completion nil))
  (let ((opoint (point)))
    (when (skip-chars-backward "A-Za-z0-9_")
      (let ((from (point)))
	(goto-char opoint)
	(do-completion from opoint :list-ignore-case *basic-completion-list*
		       nil nil *basic-popup-completion-list*)))))

(defun basic-comment-indent ()
  (generic-comment-indent "'" #'calc-basic-indent))

(defun basic-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'basic-mode)
  (setq mode-name "basic")
  (use-keymap *basic-mode-map*)
  (use-syntax-table *basic-mode-syntax-table*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'basic-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'mode-specific-beginning-of-defun)
  (setq mode-specific-beginning-of-defun 'basic-beginning-of-defun)
  (make-local-variable 'mode-specific-end-of-defun)
  (setq mode-specific-end-of-defun 'basic-end-of-defun)
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'tags-find-target-simple)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'tags-find-basic-point)
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'basic-build-summary-of-functions)
  (make-local-variable 'tags-case-insensitive)
  (setq tags-case-insensitive t)
  (and *basic-keyword-file*
       (null *basic-keyword-hash-table*)
       (setq *basic-keyword-hash-table*
	     (load-keyword-file *basic-keyword-file* t)))
  (when *basic-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *basic-keyword-hash-table*)
    (unless *basic-keyword-cap-table*
      (setq *basic-keyword-cap-table* (make-hash-table :test #'equalp :size
						       (hash-table-size *basic-keyword-hash-table*)))
      (maphash #'(lambda (key val)
		   (setf (gethash key *basic-keyword-cap-table*) key))
	       *basic-keyword-hash-table*)))
  (setq *local-abbrev-table* *basic-mode-abbrev-table*)
  (setq comment-start "' ")
  (setq comment-end "")
  (setq comment-start-skip "'[ \t]*")
  (setq comment-indent-function 'basic-comment-indent)
  (when *basic-comment-column*
    (setq comment-column *basic-comment-column*))
  (run-hooks '*basic-mode-hook*))

#|
Do [{While | Until} condition]
  [statements]
  [Exit Do]
  [statements]
Loop

Do
  [statements]
  [Exit Do]
  [statements]
Loop [{While | Until} condition]

[Public | Private] Enum name
  membername [= constantexpression]
  membername [= constantexpression]
  ...
End Enum

For Each element In group
  [statements]
  [Exit For]
  [statements]
Next [element]

For counter = start To end [Step step]
  [statements]
  [Exit For]
  [statements]
Next [counter]

[Public | Private | Friend] [Static] Function name [(arglist)] [As type]
  [statements]
  [name = expression]
  [Exit Function]
  [statements]
  [name = expression]
End Function

If condition Then [statements][Else elsestatements]

If condition Then
  [statements]
[ElseIf condition-n Then
  [elseifstatements] ...
[Else
  [elsestatements]]
End If

Select Case testexpression
[Case expressionlist-n
  [statements-n]] ...
[Case Else
  [elsestatements]]
End Select

[Private | Public | Friend] [Static] Sub name [(arglist)]
  [statements]
  [Exit Sub]
  [statements]
End Sub

[Private | Public] Type varname
  elementname [([subscripts])] As type
  [elementname [([subscripts])] As type]
  ...
End Type

While condition
  [statements]
Wend

With object
  [statements]
End With

[Public | Private | Friend] [Static] Property Get name [(arglist)] [As type]
  [statements]
  [name = expression]
  [Exit Property]
  [statements]
  [name = expression]
End Property

[Public | Private | Friend] [Static] Property Let name ([arglist,] value)
  [statements]
  [Exit Property]
  [statements]
End Property

[Public | Private | Friend] [Static] Property Set name ([arglist,] reference)
  [statements]
  [Exit Property]
  [statements]
End Property

Begin name
  ...
End

BeginProperty name
  ...
EndProperty

|#
