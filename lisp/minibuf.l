;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "minibuf")

(in-package "editor")

(export '(*minibuffer-maximum-history-count* *enable-recursive-minibuffers*
	  *minibuffer-enable-add-to-menu* add-history
	  *minibuffer-file-name-history*
	  *minibuffer-directory-name-history*
	  *minibuffer-search-string-history*
	  *minibuffer-buffer-name-history*
	  *minibuffer-symbol-name-history*
	  *minibuffer-lisp-sexp-history*
	  *minibuffer-execute-history*
	  *minibuffer-default-history*
	  *minibuffer-complex-command-history*
	  *minibuffer-popup-completion-list*
	  history-ignore-case
	  minibuffer-search-history-forward minibuffer-search-history-backward
	  minibuffer-next-line-or-search-history
	  minibuffer-previous-line-or-search-history
	  minibuffer-history-forward minibuffer-history-backward
	  minibuffer-next-line-or-history
	  minibuffer-previous-line-or-history
	  minibuffer-complete minibuffer-complete-word minibuffer-exit
	  minibuffer-complete-and-exit minibuffer-copy-word
	  minibuffer-scroll-other-window
	  minibuffer-convert-slash-to-backslash-region
	  minibuffer-convert-backslash-to-slash-region
	  minibuffer-local-map minibuffer-local-completion-map
	  minibuffer-local-command-line-map
	  minibuffer-local-must-match-map))

(defvar *minibuffer-maximum-history-count* 200)
(defvar *minibuffer-popup-completion-list* nil)

(defvar *enable-recursive-minibuffers* nil)

(defvar *minibuffer-enable-add-to-menu* t)

(defvar *minibuffer-file-name-history* nil)
(setf (get '*minibuffer-file-name-history* 'history-ignore-case) t)
(defvar *minibuffer-directory-name-history* nil)
(setf (get '*minibuffer-directory-name-history* 'history-ignore-case) t)
(defvar *minibuffer-search-string-history* nil)
(defvar *minibuffer-buffer-name-history* nil)
(defvar *minibuffer-symbol-name-history* nil)
(defvar *minibuffer-lisp-sexp-history* nil)
(defvar *minibuffer-execute-history* nil)
(defvar *minibuffer-default-history* nil)
(defvar *minibuffer-complex-command-history* nil)

(make-variable-buffer-local 'minibuffer-last-input)
(make-variable-buffer-local 'minibuffer-last-history)
(make-variable-buffer-local 'minibuffer-current-history-number)
(make-variable-buffer-local 'minibuffer-buffer-point)
(make-variable-buffer-local 'minibuffer-current-history)
(make-variable-buffer-local 'minibuffer-current-history-variable)

(add-hook '*enter-minibuffer-hook* 'prologue-minibuffer)
(add-hook '*exit-minibuffer-hook* 'epilogue-minibuffer)

(mapc #'(lambda (x)
	  (setf (get (car x) 'minibuffer-history-variable) (cdr x)))
      '((:file-name . *minibuffer-file-name-history*)
	(:directory-name . *minibuffer-directory-name-history*)
	(search . *minibuffer-search-string-history*)
	(:buffer-name . *minibuffer-buffer-name-history*)
	(:symbol-name . *minibuffer-symbol-name-history*)
	(:lisp-sexp . *minibuffer-lisp-sexp-history*)
	(execute . *minibuffer-execute-history*)))

(mapc #'(lambda (x)
	  (setf (get x 'minibuffer-reject-newline) t))
      '(*minibuffer-file-name-history*
	*minibuffer-directory-name-history*
	*minibuffer-buffer-name-history*
	*minibuffer-symbol-name-history*
	*minibuffer-execute-history*))

(setf (get '*minibuffer-file-name-history* 'add-lru-menu-function)
      'add-file-history-to-menu)

(defun minibuffer-initialize (buffer history point)
  (save-excursion
    (set-buffer buffer)
    (setq minibuffer-last-input "")
    (setq minibuffer-last-history "")
    (setq minibuffer-current-history-number -1)
    (setq minibuffer-buffer-point point)
    (setq minibuffer-current-history-variable
	  (or (and history (symbolp history)
		   (get history 'minibuffer-history-variable))
	      '*minibuffer-default-history*))
    (setq minibuffer-current-history
	  (symbol-value minibuffer-current-history-variable))))

(defun prologue-minibuffer (buffer history)
  (let ((point (point)))
    (with-set-buffer
      (minibuffer-initialize buffer history point))))

(defun minibuffer-add-history (buffer contents)
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (when (and (stringp contents)
	       (string/= contents ""))
      (setq minibuffer-current-history
	    (cons contents (delete contents minibuffer-current-history :test
				   (if (get minibuffer-current-history-variable
					    'history-ignore-case)
				       #'equalp #'equal))))
      (let ((exceed (- (length minibuffer-current-history)
		       *minibuffer-maximum-history-count*)))
	(if (> exceed 0)
	    (setq minibuffer-current-history
		  (nbutlast minibuffer-current-history exceed))))
      (set minibuffer-current-history-variable
	   minibuffer-current-history)
      (when *minibuffer-enable-add-to-menu*
	(let ((f (get minibuffer-current-history-variable
		      'add-lru-menu-function)))
	  (and f (funcall f)))))))

(defun epilogue-minibuffer (buffer contents)
  (with-set-buffer
    (minibuffer-add-history buffer contents)))

(defun add-history (item var)
  (let ((minibuffer-current-history (symbol-value var))
	(minibuffer-current-history-variable var))
    (declare (special minibuffer-current-history minibuffer-current-history-variable))
    (minibuffer-add-history nil item)))

(defun minibuffer-search-history (direct &optional (search t))
  (let ((line (if (pre-selection-p) ""
		(buffer-substring (point-min) (point-max)))))
    (when (not (equal minibuffer-last-history line))
      (setq minibuffer-current-history-number -1)
      (setq minibuffer-last-history "")
      (setq minibuffer-last-input line))
    (let ((length (length minibuffer-last-input))
	  (number minibuffer-current-history-number)
	  (compare (if search
		       (if (get minibuffer-current-history-variable 'history-ignore-case)
			   #'string-not-equal #'string/=)
		     #'(lambda (x y) nil)))
	  history
	  match)
      (loop
	(setq number (if (eq direct 'down)
			 (1- number)
		       (1+ number)))
	(if (= number -1)
	    (setq history minibuffer-last-input)
	  (progn
	    (when (minusp number)
	      (setq minibuffer-current-history-number -1)
	      (plain-error))
	    (setq history (nth number minibuffer-current-history))
	    (unless history
	      (or (zerop minibuffer-current-history-number)
		  (setq minibuffer-current-history-number
			(1- (length minibuffer-current-history))))
	      (plain-error))))
	(setq match (funcall compare history minibuffer-last-input))
	(when (or (null match)
		  (= match length))
	  (return)))
      (setq minibuffer-current-history-number number)
      (setq minibuffer-last-history history)
      (delete-region (point-min) (point-max))
      (insert history)
      (when (eq direct 'down)
	(goto-char (point-min))
	(goto-eol))
      t)))

(defun minibuffer-search-history-forward ()
  (interactive)
  (minibuffer-search-history 'down))

(defun minibuffer-search-history-backward ()
  (interactive)
  (minibuffer-search-history 'up))

(defun minibuffer-next-line-or-search-history ()
  (interactive)
  (unless (next-line 1)
    (minibuffer-search-history-forward)))

(defun minibuffer-previous-line-or-search-history ()
  (interactive)
  (unless (previous-line 1)
    (minibuffer-search-history-backward)))

(defun minibuffer-history-forward ()
  (interactive)
  (minibuffer-search-history 'down nil))

(defun minibuffer-history-backward ()
  (interactive)
  (minibuffer-search-history 'up nil))

(defun minibuffer-next-line-or-history ()
  (interactive)
  (unless (next-line 1)
    (minibuffer-history-forward)))

(defun minibuffer-previous-line-or-history ()
  (interactive)
  (unless (previous-line 1)
    (minibuffer-history-backward)))

(defun minibuffer-complete (&optional word)
  (interactive "*p")
  (do-completion (point-min) (point-max)
		 (minibuffer-completion-type)
		 (minibuffer-completion-list)
		 word
		 *last-command-char*
		 *minibuffer-popup-completion-list*)
  (setq *this-command* 'minibuffer-complete))

(defun minibuffer-complete-word ()
  (interactive "*")
  (minibuffer-complete t))

(defun minibuffer-exit-check ()
  (let ((type (minibuffer-completion-type)))
    (case type
      (:symbol-name
       (when (/= (point-min) (point-max))
	 (exit-recursive-edit)))
      (:exist-buffer-name
       (if (and (= (point-min) (point-max))
		(equal (minibuffer-default) ""))
	   (exit-recursive-edit)
	 t))
      ((:file-name :exist-file-name :file-name-list :directory-name)
       (let ((contents (buffer-substring (point-min) (point-max))))
	 (if (string-match "[/\\]\\.$" contents)
	     (progn
	       (delete-region (point-min) (point-max))
	       (insert (cwd))
	       (return-from minibuffer-exit-check nil)))
	 (let ((file (namestring contents)))
	   (and (eq type :directory-name)
		(file-directory-p file)
		(progn
		  (delete-region (point-min) (point-max))
		  (insert file)
		  (exit-recursive-edit)))
	   (if (or (file-directory-p file)
		   (wild-pathname-p file))
	       (progn
		 (toggle-ime nil)
		 (multiple-value-bind (files result)
		     (filer file (eq type :file-name-list)
			    (minibuffer-dialog-title))
		   (unless result
		     (quit-recursive-edit t))
		   (delete-region (point-min) (point-max))
		   (cond ((stringp files)
			  (setq files (namestring files))
			  (insert files))
			 ((consp files)
			  (insert (car files))
			  (let ((*minibuffer-enable-add-to-menu* nil))
			    (dolist (x (cdr files))
			      (minibuffer-add-history nil x)))))
		   (exit-recursive-edit files)))
	     t))))
      (:integer
       (if (ignore-errors (parse-integer (buffer-substring (point-min) (point-max))
					 :junk-allowed t))
	   t
	 (progn
	   (delete-region (point-min) (point-max))
	   nil)))
      (t t))))

(defun minibuffer-newline (&optional (arg 1))
  (interactive "*")
  (let ((v minibuffer-current-history-variable))
    (if (and (symbolp v) (get v 'minibuffer-reject-newline))
	(ding)
      (let ((*last-command-char* #\LFD))
	(self-insert-command arg)))))

(defun minibuffer-exit ()
  (interactive)
  (and (minibuffer-exit-check)
       (exit-recursive-edit)))

(defun minibuffer-complete-and-exit ()
  (interactive "*")
  (minibuffer-exit-check)
  (let ((status (do-completion-internal (point-min) (point-max)
					(minibuffer-completion-type)
					(minibuffer-completion-list)
					t nil *minibuffer-popup-completion-list*)))
    (if (or (eq status :solo-match)
	    (eq status :not-unique))
	(exit-recursive-edit)
      (completion-message status))))

(defun minibuffer-copy-word ()
  (interactive)
  (let ((point minibuffer-buffer-point)
	(buffer (minibuffer-buffer))
	word)
    (with-set-buffer
      (save-excursion
	(set-buffer buffer)
	(goto-char point)
	(unless (eobp)
	  (setq word
		(buffer-substring point
				  (progn
				    (if (eolp)
					(forward-word 1)
				      (let ((lnum (current-line-number)))
					(forward-word 1)
					(or (= lnum (current-line-number))
					    (progn
					      (goto-line lnum)
					      (goto-eol)))))
				    (point))))
	  (setq point (point)))))
    (when word
      (setq minibuffer-buffer-point point)
      (when (pre-selection-p)
	(delete-region (selection-mark) (selection-point))
	(stop-selection))
      (insert word))))

(defun minibuffer-scroll-other-window ()
  (interactive)
  (let ((w (let ((buffer (find-buffer " *Completion*")))
	     (and buffer (get-buffer-window buffer)))))
    (if w
	(let ((ow (selected-window)))
	  (unwind-protect
	      (progn
		(set-window w)
		(if (pos-visible-in-window-p (point-max))
		    (goto-char (point-min))
		  (next-page)))
	    (set-window ow)))
      (scroll-other-window))))

(defun minibuffer-replace-chars (r1 r2 c1 c2)
  (save-restriction
    (narrow-to-region r1 r2)
    (goto-char (point-min))
    (replace-buffer c1 c2)
    t))

(defun minibuffer-convert-slash-to-backslash-region (from to)
  (interactive "*r")
  (minibuffer-replace-chars from to "/" "\\"))

(defun minibuffer-convert-backslash-to-slash-region (from to)
  (interactive "*r")
  (minibuffer-replace-chars from to "\\" "/"))

(defun minibuffer-define-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap #\Up 'minibuffer-previous-line-or-search-history)
    (define-key keymap #\C-p 'minibuffer-previous-line-or-search-history)
    (define-key keymap #\M-p 'minibuffer-previous-line-or-search-history)
    (define-key keymap #\Down 'minibuffer-next-line-or-search-history)
    (define-key keymap #\C-n 'minibuffer-next-line-or-search-history)
    (define-key keymap #\M-n 'minibuffer-next-line-or-search-history)
    (define-key keymap #\C-Up 'minibuffer-previous-line-or-history)
    (define-key keymap #\C-Down 'minibuffer-next-line-or-history)
    (define-key keymap #\Home 'minibuffer-copy-word)
    (define-key keymap #\RET 'minibuffer-exit)
    (define-key keymap #\LFD 'minibuffer-newline)
    (define-key keymap #\C-M-v 'minibuffer-scroll-other-window)
    (define-key keymap #\C-g 'quit-recursive-edit)
    (define-key keymap '(#\ESC #\ESC) 'quit-recursive-edit)
    (define-key keymap '(#\C-c #\\) 'minibuffer-convert-slash-to-backslash-region)
    (define-key keymap '(#\C-c #\/) 'minibuffer-convert-backslash-to-slash-region)
    keymap))

(defvar minibuffer-local-map nil)
(unless minibuffer-local-map
  (setq minibuffer-local-map (minibuffer-define-keymap)))

(defvar minibuffer-local-completion-map nil)
(unless minibuffer-local-completion-map
  (setq minibuffer-local-completion-map (minibuffer-define-keymap))
  (define-key minibuffer-local-completion-map #\TAB 'minibuffer-complete)
  (define-key minibuffer-local-completion-map #\SPC 'minibuffer-complete-word))

(defvar minibuffer-local-must-match-map nil)
(unless minibuffer-local-must-match-map
  (setq minibuffer-local-must-match-map (minibuffer-define-keymap))
  (define-key minibuffer-local-must-match-map #\RET 'minibuffer-complete-and-exit)
  (define-key minibuffer-local-must-match-map #\TAB 'minibuffer-complete)
  (define-key minibuffer-local-must-match-map #\SPC 'minibuffer-complete-word))

(defvar minibuffer-local-command-line-map nil)
(unless minibuffer-local-command-line-map
  (setq minibuffer-local-command-line-map (minibuffer-define-keymap))
  (define-key minibuffer-local-command-line-map #\TAB 'minibuffer-complete))
