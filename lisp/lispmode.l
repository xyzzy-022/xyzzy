;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "lispmode")

(in-package "editor")

(export '(*lisp-mode-hook* *lisp-interaction-mode-hook* *lisp-indent-offset*
	  *find-buffer-package-hook*
	  *lisp-body-indention* *lisp-body-indent* lisp-complete-symbol
	  lisp-indent-hook lisp-paren-imaginary-offset
	  lisp-indent-line lisp-newline-and-indent
	  indent-region indent-sexp eval-region eval-buffer eval-last-sexp
	  eval-print-last-sexp eval-defun lisp-mode lisp-interaction-mode
	  lisp-electric-close *lisp-tab-always-indent*
	  *lisp-indent-tabs-mode* *lisp-comment-column*
	  *lisp-popup-completion-list*
	  *kill-buffer-kills-scratch*))

(defvar *lisp-mode-hook* nil)
(defvar *lisp-popup-completion-list* nil)
(defvar *lisp-interaction-mode-hook* nil)
(defvar *find-buffer-package-hook* nil)
(defvar *lisp-indent-offset* nil)
(defvar *lisp-body-indention* 2)
(defvar *lisp-body-indent* 2)
(defvar *lisp-comment-column* nil)

(defvar lisp-paren-imaginary-offset 0)
(defvar lisp-indent-close-paren nil)

(defvar *lisp-tab-always-indent* t)

(defvar *lisp-indent-tabs-mode* t)

(setf (get 'lambda 'lisp-indent-hook) 'defun)
(setf (get 'interactive 'lisp-indent-hook) 1)

(setf (get 'progn 'lisp-indent-hook) 0)
(setf (get 'locally 'lisp-indent-hook) 0)
(setf (get 'if 'lisp-indent-hook) 2)
(setf (get 'let 'lisp-indent-hook) 1)
(setf (get 'let* 'lisp-indent-hook) 1)
(setf (get 'catch 'lisp-indent-hook) 1)
(setf (get 'unwind-protect 'lisp-indent-hook) 1)
(setf (get 'case 'lisp-indent-hook) 1)
(setf (get 'tagbody 'lisp-indent-hook) 0)
(setf (get 'block 'lisp-indent-hook) 1)
(setf (get 'save-excursion 'lisp-indent-hook) 0)
(setf (get 'save-window-excursion 'lisp-indent-hook) 0)
(setf (get 'save-restriction 'lisp-indent-hook) 0)
(setf (get 'return-from 'lisp-indent-hook) 1)
(setf (get 'multiple-value-call 'lisp-indent-hook) 1)
(setf (get 'multiple-value-bind 'lisp-indent-hook) 2)
(setf (get 'multiple-value-setq 'lisp-indent-hook) 1)
(setf (get 'eval-when 'lisp-indent-hook) 1)
(setf (get 'flet 'lisp-indent-hook) 1)
(setf (get 'labels 'lisp-indent-hook) 1)
(setf (get 'macrolet 'lisp-indent-hook) 1)

(setf (get 'flet 'lisp-indent-flet) t)
(setf (get 'labels 'lisp-indent-flet) t)
(setf (get 'macrolet 'lisp-indent-flet) t)

(setf (get 'defmacro 'lisp-indent-hook) 'defun)
(setf (get 'defun 'lisp-indent-hook) 'defun)
(setf (get 'defvar 'lisp-indent-hook) 'defun)
(setf (get 'defparameter 'lisp-indent-hook) 'defun)
(setf (get 'defconstant 'lisp-indent-hook) 'defun)
(setf (get 'deftype 'lisp-indent-hook) 'defun)
(setf (get 'defstruct 'lisp-indent-hook) 'defun)
(setf (get 'define-condition 'lisp-indent-hook) 'defun)
(setf (get 'when 'lisp-indent-hook) 1)
(setf (get 'unless 'lisp-indent-hook) 1)
(setf (get 'loop 'lisp-indent-hook) 0)
(setf (get '*loop 'lisp-indent-hook) 0)
(setf (get 'prog1 'lisp-indent-hook) 1)
(setf (get 'prog2 'lisp-indent-hook) 2)
(setf (get 'do* 'lisp-indent-hook) 2)
(setf (get 'do 'lisp-indent-hook) 2)
(setf (get 'dolist 'lisp-indent-hook) 1)
(setf (get 'dotimes 'lisp-indent-hook) 1)
(setf (get 'while 'lisp-indent-hook) 1)
(setf (get 'prog 'lisp-indent-hook) 1)
(setf (get 'prog* 'lisp-indent-hook) 1)

(setf (get 'defsetf 'lisp-indent-hook) 3)
(setf (get 'define-setf-method 'lisp-indent-hook) 2)
(setf (get 'defpackage 'lisp-indent-hook) 1)

(setf (get 'do-symbols 'lisp-indent-hook) 1)
(setf (get 'do-external-symbols 'lisp-indent-hook) 1)
(setf (get 'do-all-symbols 'lisp-indent-hook) 1)
(setf (get 'with-package-iterator 'lisp-indent-hook) 1)
(setf (get 'with-hash-table-iterator 'lisp-indent-hook) 1)

(defvar *lisp-keyword-hash-table* nil)
(defvar *lisp-keyword-file* "Lisp")

(defvar *lisp-mode-syntax-table* nil)
(unless *lisp-mode-syntax-table*
  (setq *lisp-mode-syntax-table* (make-syntax-table))
  (dotimes (x 127)
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-symbol *lisp-mode-syntax-table* c))))
  (set-syntax-whitespace *lisp-mode-syntax-table* #\SPC)
  (set-syntax-whitespace *lisp-mode-syntax-table* #\TAB)
  (set-syntax-whitespace *lisp-mode-syntax-table* #\C-l)
  (set-syntax-whitespace *lisp-mode-syntax-table* #\RET)
  (set-syntax-start-comment *lisp-mode-syntax-table* #\; nil)
  (set-syntax-end-comment *lisp-mode-syntax-table* #\LFD nil t)
  (set-syntax-string *lisp-mode-syntax-table* #\")
  (set-syntax-quote *lisp-mode-syntax-table* #\#)
  (set-syntax-quote *lisp-mode-syntax-table* #\')
  (set-syntax-quote *lisp-mode-syntax-table* #\,)
  (set-syntax-quote *lisp-mode-syntax-table* #\`)
  (set-syntax-match *lisp-mode-syntax-table* #\( #\))
  (set-syntax-escape *lisp-mode-syntax-table* #\\)
  (set-syntax-start-multi-comment *lisp-mode-syntax-table* "#|")
  (set-syntax-end-multi-comment *lisp-mode-syntax-table* "|#"))

(defvar *lisp-mode-map* nil)
(unless *lisp-mode-map*
  (setq *lisp-mode-map* (make-sparse-keymap))
  (define-key *lisp-mode-map* #\C-M-x 'eval-defun)
  (define-key *lisp-mode-map* #\TAB 'lisp-indent-line)
  (define-key *lisp-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *lisp-mode-map* #\RET 'lisp-newline-and-indent)
  (define-key *lisp-mode-map* #\C-M-q 'indent-sexp)
  (define-key *lisp-mode-map* #\) 'lisp-electric-close))

(defvar *lisp-interaction-mode-map* nil)
(unless *lisp-interaction-mode-map*
  (setq *lisp-interaction-mode-map* (make-sparse-keymap))
  (define-key *lisp-interaction-mode-map* #\C-M-x 'eval-defun)
  (define-key *lisp-interaction-mode-map* #\LFD 'eval-print-last-sexp)
  (define-key *lisp-interaction-mode-map* #\TAB 'lisp-indent-line)
  (define-key *lisp-interaction-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *lisp-interaction-mode-map* #\RET 'lisp-newline-and-indent)
  (define-key *lisp-interaction-mode-map* #\C-M-q 'indent-sexp)
  (define-key *lisp-interaction-mode-map* #\) 'lisp-electric-close))

(defvar *lisp-mode-abbrev-table* nil)
(unless *lisp-mode-abbrev-table*
  (define-abbrev-table '*lisp-mode-abbrev-table*))

(setq-default *buffer-package* nil)
(make-variable-buffer-local '*buffer-package*)

(defun calc-lisp-indent-current-symbol (package)
  (skip-syntax-spec-forward " ")
  (if (looking-for ":")
      (values nil nil t)
    (multiple-value-bind (symbol found maybe-symbol)
	(lookup-symbol (point)
		       (progn
			 (loop
			   (or (skip-syntax-spec-forward "w_")
			       (skip-chars-forward "#")
			       (return)))
			 (point))
		       package)
      (values symbol (and found maybe-symbol)))))

(defun calc-lisp-indent-method (package)
  (multiple-value-bind (symbol found pkg-marker-p)
      (calc-lisp-indent-current-symbol package)
    (when pkg-marker-p
      (return-from calc-lisp-indent-method 'package-marker))
    (when found
      (or (let ((method (get symbol 'lisp-indent-handler)))
	    (and method
		 (save-excursion
		   (and (up-list -1 t)
			(forward-list -1 t)
			(up-list -1 t)
			(progn
			  (forward-char 1)
			  (multiple-value-bind (symbol found)
			      (calc-lisp-indent-current-symbol package)
			    (eq symbol 'handler-case)))))
		 method))
	  (get symbol 'lisp-indent-hook)
	  (let ((args (cadr (macro-function symbol))))
	    (when (consp args)
	      (let ((p (position '&body args))
		    (ep (position '&environment args)))
		(when p
		  (and (eq (car args) '&whole) (decf p 2))
		  (and ep (< ep p) (decf p 2))
		  p))))))))

(defvar *lisp-search-in-package-regexp*
  (compile-regexp
   "^(\\(?:\\(?:lisp\\|cl\\|common-lisp\\)::?\\)?in-package\\([^)]+\\))"))

(defun lisp-search-in-package ()
  (ignore-errors
    (protect-match-data
      (save-excursion
	(when (or (scan-buffer *lisp-search-in-package-regexp* :reverse t)
		  (scan-buffer *lisp-search-in-package-regexp* :reverse nil))
	  (let (*read-eval*)
	    (find-package (read-from-string (match-string 1)))))))))

(defun find-buffer-package ()
  (or (and (stringp *buffer-package*)
	   (find-package *buffer-package*))
      (let ((result (ignore-errors
		      (run-hook-with-args-until-success
		       '*find-buffer-package-hook*))))
	(cond ((packagep result) result)
	      ((or (symbolp result)
		   (stringp result))
	       (find-package
		(find (string result)
		      (mapcan
		       #'(lambda (p)
			   (copy-list
			    (cons (package-name p)
				  (package-nicknames p))))
		       (list-all-packages))
		      :test #'equalp)))))
      *package*))

(defun calc-lisp-indent (opoint)
  (protect-match-data
    (let ((begin-paren (and lisp-indent-close-paren
			    (looking-at "[ \t]*)"))))
      (goto-bol)
      (when (and (looking-at "\\s(")
		 (forward-char -1))
	(skip-white-backward)
	(forward-char 1))
      (or (up-list -1 t)
	  (return-from calc-lisp-indent 0))
      (cond (begin-paren
	     (+ (current-column) lisp-paren-imaginary-offset))
	    ((or (looking-back "#")
		 (and (not (looking-back "#'"))
		      (looking-back "'")))
	     (+ (current-column) 1))
	    (t
	     (let ((package (find-buffer-package)))
	       (when (save-excursion
		       (when (and (up-list -1 t)
				  (looking-for "((")
				  (up-list -1 t))
			 (forward-char 1)
			 (multiple-value-bind (symbol found)
			     (calc-lisp-indent-current-symbol package)
			   (and found (get symbol 'lisp-indent-flet)))))
		 (return-from calc-lisp-indent (+ (current-column) *lisp-body-indention*)))
	       (let ((column (progn
			       (forward-char 1)
			       (current-column)))
		     (method (calc-lisp-indent-method package)))
		 (cond ((eq method 'package-marker) column)
		       ((numberp method)
			(let ((count -1))
			  (while (< (point) opoint)
			    (skip-white-forward)
			    (setq count (+ count 1))
			    (or (forward-sexp 1 t)
				(return)))
			  (+ column -1 (if (< count method)
					   (* *lisp-body-indent* 2)
					 *lisp-body-indent*))))
		       (method
			(+ column -1 *lisp-body-indention*))
		       (t (skip-chars-forward " \t")
			  (if (or (eolp) (looking-for ";"))
			      (if *lisp-indent-offset*
				  (+ column *lisp-indent-offset*)
				column)
			    (current-column)))))))))))

(defun lisp-indent-line ()
  (interactive "*")
  (if (or (not (interactive-p))
	  *lisp-tab-always-indent*
	  (save-excursion
	    (skip-chars-backward " \t")
	    (bolp)))
	(smart-indentation (save-excursion
			     (goto-bol)
			     (if (protect-match-data
				   (looking-at "[ \t]*;;;"))
				 0
			       (max 0 (calc-lisp-indent (point))))))
    (insert "\t"))
  t)

(defun lisp-comment-indent ()
  (save-excursion
    (let ((opoint (point)))
      (goto-bol)
      (skip-chars-forward " \t")
      (cond ((looking-for ";;;")
	     0)
	    ((or (looking-for ";")
		 (eolp))
	     (calc-lisp-indent (point)))
	    ((zerop comment-column)
	     0)
	    (t
	     (goto-char opoint)
	     (skip-chars-backward " \t")
	     (max (1+ (current-column)) comment-column))))))

(defun lisp-newline-and-indent (&optional (arg 1))
  (interactive "*p")
  (delete-trailing-spaces)
  (insert #\LFD arg)
  (lisp-indent-line))

(defun indent-region (from to)
  (interactive "*r")
  (if (> from to)
      (rotatef from to))
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) to)
      (goto-char from)
      (goto-eol)
      (delete-trailing-spaces)
      (loop
	(goto-eol)
	(delete-trailing-spaces)
	(unless (bolp)
	  (funcall mode-specific-indent-command))
	(unless (forward-line 1)
	  (return))
	)))
  t)

(defun indent-sexp ()
  (interactive "*")
  (indent-region (point)
		 (save-excursion
		   (forward-sexp)
		   (point))))

(defmacro eval-region-print (stream &body body)
  `(let ((*print-circle* t)
	 (#1=#:stream ,stream))
     (cond ((bufferp #1#)
	    (let ((obuffer (selected-buffer)))
	      (unwind-protect
		  (progn
		    (set-buffer #1#)
		    (with-output-to-selected-buffer
		      (fresh-line)
		      ,@body))
		(set-buffer obuffer))))
	   ((null #1#)
	    ,@body)
	   (t
	    (when (and (buffer-stream-p #1#)
		       (eq (selected-buffer) (buffer-stream-buffer #1#))
		       (> (point) (buffer-stream-point #1#)))
	      (buffer-stream-set-point #1# (point)))
	    (let ((*standard-output* #1#))
	      (fresh-line)
	      ,@body)))))

(defun eval-region (from to &optional stream)
  (interactive "r")
  (let ((result nil)
	(*package* (find-buffer-package)))
    (when (> from to)
      (rotatef from to))
    (let ((s (make-buffer-stream (selected-buffer) from to)))
      (handler-case
	  (while (< (buffer-stream-point s) to)
	    (let ((form (read s nil '#1=#:eof)))
	      (when (eq form '#1#)
		(return))
	      (setq result (multiple-value-list (eval form)))))
	((or error reader-error quit) (c)
	 (when (<= (buffer-stream-point s) to)
	   (goto-char (buffer-stream-point s))
	   (when stream
	     (eval-region-print stream
	       (format t "~A~%" c)))
	   (let ((si:*trace-on-error* nil))
	     (error c))))))
    (eval-region-print stream
      (if (null stream)
	  (message "~s" (car result))
	(format t "~{~S~%~}" result)))
    (values-list result)))

(defun eval-buffer (buffer)
  (interactive "bEval Buffer: ")
  (save-excursion
    (set-buffer buffer)
    (eval-region (point-min) (point-max))))

(defun eval-last-sexp (&optional stream)
  (interactive "p")
  (let ((syntab (syntax-table)))
    (let (from to)
      (cond ((selection-start-end (start end)
	       (setq from start to end)))
	    (t
	     (use-syntax-table *lisp-mode-syntax-table*)
	     (unwind-protect
		 (let ((opoint (point)))
		   (flet ((goto-reader-macro (open-char close-char)
			    (skip-chars-backward " \r\t\f\n")
			    (when (char= (preceding-char) close-char)
			      (let ((nest 0))
				(while (backward-char)
				  (when (char= (following-char) close-char)
				    (incf nest))
				  (when (char= (following-char) open-char)
				    (decf nest))
				  (when (or (< 3 nest) (zerop nest))
				    (return)))
				(when (and (zerop nest) (char= (preceding-char) #\#))
				  (backward-char)
				  (while (char= (preceding-char) #\')
				    (backward-char))
				  (return-from goto-reader-macro t))
				;; not reader macro
				(goto-char opoint)
				nil))))
		     (cond ((or (goto-reader-macro #\< #\>)
				(goto-reader-macro #\{ #\}))
			    (setq from (point) to opoint))
			   (t
			    (setq from (progn
					 (backward-sexp)
					 (if (and (save-excursion
						    (scan-buffer "#[0-9]*[ACS=]?"
								 :reverse t :regexp t :case-fold t))
						  (= (point) (match-end 0)))
					     (match-beginning 0)
					   (point)))
				  to (progn
				       (forward-sexp)
				       (point))))))
		   (goto-char (max opoint to)))
	       (use-syntax-table syntab))))
      (save-excursion
	(eval-region from to
		     (cond ((or (streamp stream)
				(bufferp stream))
			    stream)
			   (stream
			    (selected-buffer))
			   (t nil)))))))

(defun eval-print-last-sexp ()
  (interactive "*")
  (with-output-to-selected-buffer
    (let ((*standard-input* *cooked-mode-input-stream*))
      (when (fresh-line)
	(goto-char (buffer-stream-point *standard-output*))
	(refresh-screen))
      (eval-last-sexp *standard-output*))))

(defun eval-defun (&optional (arg nil f))
  (interactive "p")
  (save-excursion
    (let ((opoint (point)))
      (end-of-defun)
      (let ((to (point))
	    (from (progn
		    (beginning-of-defun)
		    (point))))
	(end-of-defun)
	(goto-char (if (< (point) opoint) opoint to))
	(eval-region from (point) (if f (selected-buffer) nil))))))

(defvar *lisp-character-name-list*
  (let ((x '(("TAB" . #\TAB) ("SPC" . #\SPC) ("LFD" . #\LFD)
	     ("RET" . #\RET) ("ESC" . #\ESC) ("DEL" . #\DEL)
	     ("NUL" . #\NUL) ("PageUp" . #\PageUp) ("PageDown" . #\PageDown)
	     ("End" . #\End) ("Home" . #\Home) ("Left" . #\Left)
	     ("Up" . #\Up) ("Right" . #\Right) ("Down" . #\Down)
	     ("Pause" . #\Pause) ("MouseMove" . #\MouseMove) ("Scroll" . #\Scroll)
	     ("Apps" . #\Apps) ("Insert" . #\Insert) ("Delete" . #\Delete)
	     ("Help" . #\Help)
	     ("Backspace" . #\Backspace) ("Newline" . #\Newline)
	     ("Linefeed" . #\Linefeed) ("Page" . #\Page)
	     ("Return" . #\Return) ("Space" . #\Space)
	     ("Rubout" . #\Rubout)
	     ("F1" . #\F1) ("F2" . #\F2) ("F3" . #\F3) ("F4" . #\F4)
	     ("F5" . #\F5) ("F6" . #\F6) ("F7" . #\F7) ("F8" . #\F8)
	     ("F9" . #\F9) ("F10" . #\F10) ("F11" . #\F11) ("F12" . #\F12)
	     ("F13" . #\F13) ("F14" . #\F14) ("F15" . #\F15) ("F16" . #\F16)
	     ("F17" . #\F17) ("F18" . #\F18) ("F19" . #\F19) ("F20" . #\F20)
	     ("F21" . #\F21) ("F22" . #\F22) ("F23" . #\F23) ("F24" . #\F24)
	     ("LBtnDown" . #\LBtnDown) ("LBtnUp" . #\LBtnUp) ("LBtnMove" . #\LBtnMove)
	     ("RBtnDown" . #\RBtnDown) ("RBtnUp" . #\RBtnUp) ("RBtnMove" . #\RBtnMove)
	     ("MBtnDown" . #\MBtnDown) ("MBtnUp" . #\MBtnUp) ("MBtnMove" . #\MBtnMove)
	     ("XBtn1Down" . #\XBtn1Down) ("XBtn1Up" . #\XBtn1Up) ("XBtn1Move" . #\XBtn1Move)
	     ("XBtn2Down" . #\XBtn2Down) ("XBtn2Up" . #\XBtn2Up) ("XBtn2Move" . #\XBtn2Move))))
    (maphash #'(lambda (a b)
		 (setq x (acons a b x)))
	     si:*character-name-hash-table*)
    (sort x #'(lambda (x y)
		(string< (car x) (car y))))))

(defun lisp-complete-symbol ()
  (interactive "*")
  (let ((otab (syntax-table)))
    (use-syntax-table *lisp-mode-syntax-table*)
    (unwind-protect
	(protect-match-data
	  (let* ((list nil)
		 (end (point))
		 (start (progn
			  (when (skip-syntax-spec-backward "w_")
			    (skip-syntax-spec-forward "'"))
			  (point)))
		 (type (cond ((looking-back "#\\")
			      (setq list *lisp-character-name-list*)
			      :list)
			     ((and (looking-back "(")
				   (progn
				     (forward-char -2)
				     (not (looking-at "[('#]"))))
			      :function-name)
			     (t
			      :non-trivial-symbol-name))))
	    (goto-char end)
	    (when (<= start end)
	      (do-completion start end type list nil nil
			     *lisp-popup-completion-list*))))
      (use-syntax-table otab))))

(defun lisp-electric-close (&optional (arg 1))
  (interactive "*p")
  (self-insert-command arg)
  (lisp-indent-line)
  (save-excursion
    (forward-char -1)
    (and (goto-matched-parenthesis)
	 (show-matched-parenthesis)))
  t)

(define-key esc-map #\TAB 'lisp-complete-symbol)

(define-key ctl-x-map #\C-e 'eval-last-sexp)

(autoload 'lisp-build-summary-of-functions "lispfns" nil)
(autoload 'lisp-maketags "lispfns" nil)
(pushnew '(lisp-maketags "*.l" "*.lisp") *maketags-list* :key #'car)

(defun lisp-tags-find-target ()
  (let* ((opoint (point))
	 (name (buffer-substring (progn
				   (skip-syntax-spec-forward "w_")
				   (point))
				 (progn
				   (skip-syntax-spec-backward "w_")
				   (point))))
	 (functionp (or (and (not (looking-back "#("))
			     (looking-back "("))
			(looking-back "#'"))))
    (goto-char opoint)
    (setq name (substitute-string name "\\`[^:]+::?" ""))
    (values nil name functionp)))

(defun lisp-tags-find-point (class name functionp point)
  (goto-char (- point *jump-tag-limit*))
  (and (scan-buffer (concat (if functionp
				"(def\\(un\\|macro\\)"
			      "(def\\(var\\|parameter\\|constant\\)")
			    "[ \t\n\f]+"
			    (regexp-quote name)
			    "[ \t\n\f]")
		    :regexp t :limit (+ point *jump-tag-limit*))
       (point)))

(defun lisp-mode ()
  (interactive)
  (let ((buffer-package *buffer-package*))
    (kill-all-local-variables)
    (setq *buffer-package* buffer-package))
  (setq buffer-mode 'lisp-mode)
  (setq mode-name "Lisp")
  (use-keymap *lisp-mode-map*)
  (use-syntax-table *lisp-mode-syntax-table*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'lisp-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *lisp-indent-tabs-mode*)
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'lisp-tags-find-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'lisp-tags-find-point)
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'lisp-build-summary-of-functions)
  (and *lisp-keyword-file*
       (null *lisp-keyword-hash-table*)
       (setq *lisp-keyword-hash-table*
	     (load-keyword-file *lisp-keyword-file*)))
  (when *lisp-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *lisp-keyword-hash-table*))
  (setq *local-abbrev-table* *lisp-mode-abbrev-table*)
  (setq comment-start "; ")
  (setq comment-end "")
  (setq comment-start-skip ";+[ \t]*")
  (setq comment-indent-function 'lisp-comment-indent)
  (when *lisp-comment-column*
    (setq comment-column *lisp-comment-column*))
  (run-hooks '*lisp-mode-hook*))

(defun lisp-interaction-mode ()
  (interactive)
  (let ((*lisp-mode-hook* nil))
    (lisp-mode))
  (setq buffer-mode 'lisp-interaction-mode)
  (setq mode-name "Lisp Interaction")
  (use-keymap *lisp-interaction-mode-map*)
  (unless (file-visited-p)
    (make-local-variable 'need-not-save)
    (setq need-not-save t)
    (make-local-variable 'auto-save)
    (setq auto-save nil))
  (make-local-variable '*find-buffer-package-hook*)
  (add-hook '*find-buffer-package-hook* 'lisp-search-in-package)
  (run-hooks '*lisp-interaction-mode-hook*))

(defvar *kill-buffer-kills-scratch* nil)

(defun kill-scratch-hook ()
  (cond (*kill-buffer-kills-scratch*
	 t)
	((and (eq buffer-mode 'lisp-interaction-mode)
	      (equal (buffer-name (selected-buffer)) "*scratch*"))
	 (erase-buffer (selected-buffer))
	 nil)
	(t t)))

(add-hook '*query-kill-buffer-hook* 'kill-scratch-hook)
