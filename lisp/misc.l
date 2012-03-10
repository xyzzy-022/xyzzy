;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "misc")

(in-package "editor")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-output-to-temp-buffer with-output-to-selected-buffer
	    with-output-to-buffer with-input-from-buffer
	    with-input-from-selected-buffer
	    with-set-buffer with-selected-window
	    with-interval-message
	    *cooked-mode-input-stream*
	    defvar-local setq-default
	    msgbox message minibuffer-message
	    minibuffer-prompt clear-minibuffer-message
	    add-hook delete-hook plain-error simple-warn
	    clear-message long-operation
	    execute-extended-command call-interactively
	    concat vconcat char-name *etc-path* etc-path cd trap-errors
	    setup-temp-buffer toggle-trace-on-error
	    *use-global-ime* *default-load-path*)))

(defvar *use-global-ime* nil)

(setf (get 'with-output-to-buffer 'lisp-indent-hook) 1)

(defmacro with-output-to-buffer ((buffer &optional point) &rest body)
  `(let ((*standard-output* (make-buffer-stream ,buffer ,point)))
     ,@body))

(defun with-selected-buffer-adjust-point (buffer stream)
  (if (eq (selected-buffer) buffer)
      (goto-char (buffer-stream-point stream))
    (save-excursion
      (set-buffer buffer)
      (goto-char (buffer-stream-point stream)))))

(setf (get 'with-output-to-selected-buffer 'lisp-indent-hook) 0)

(defmacro with-output-to-selected-buffer (&rest body)
  `(let ((#1=#:buffer (selected-buffer)))
     (with-output-to-buffer (#1# (point))
       (let ((#2=#:stream *standard-output*))
	 (unwind-protect
	     (progn ,@body)
	   (with-selected-buffer-adjust-point #1# #2#))))))

(defun setup-temp-buffer (buffer)
  (with-set-buffer
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (setq need-not-save t)
      (setq kept-undo-information nil)
      (setq auto-save nil))))

(setf (get 'with-output-to-temp-buffer 'lisp-indent-hook) 1)

(defmacro with-output-to-temp-buffer ((bufname &optional (popup t) vert-p)
				      &rest body)
  `(let ((#1=#:buffer (or (find-buffer ,bufname)
			  (create-new-buffer ,bufname))))
     (erase-buffer #1#)
     (setup-temp-buffer #1#)
     ,@(if popup `((pop-to-buffer #1# ,popup ,vert-p)))
     (with-output-to-buffer (#1#)
       (prog1
	   (progn ,@body)
	 (set-buffer-modified-p nil #1#)))))

(setf (get 'with-input-from-buffer 'lisp-indent-hook) 1)

(defmacro with-input-from-buffer ((buffer &optional point eob) &rest body)
  `(let* ((*standard-input* (make-buffer-stream ,buffer ,point ,eob)))
     (progn ,@body)))

(setf (get 'with-input-from-selected-buffer 'lisp-indent-hook) 0)

(defmacro with-input-from-selected-buffer (&rest body)
  `(let ((#1=#:buffer (selected-buffer)))
     (with-input-from-buffer (#1# (point))
       (let ((#2=#:stream *standard-input*))
	 (prog1
	     (progn ,@body)
	   (with-selected-buffer-adjust-point #1# #2#))))))

(setf (get 'with-set-buffer 'lisp-indent-hook) 0)

(defmacro with-set-buffer (&body body)
  `(let ((#1=#:mini (minibuffer-window-p (selected-window))))
     (unwind-protect
	 (progn
	   (and #1# (other-window))
	   ,@body)
       (and #1# (set-window (minibuffer-window))))))

(setf (get 'with-selected-window 'lisp-indent-hook) 0)

(defmacro with-selected-window (&body body)
  `(let ((#1=#:owindow (selected-window)))
     (unwind-protect
	 (progn ,@body)
       (set-window #1#))))

(setf (get 'with-interval-message 'lisp-indent-hook) 1)

(defmacro with-interval-message ((interval) &body body)
  `(let ((#1=#:last-tick (+ (get-internal-real-time) ,interval))
	 (#2=#:tick))
     (flet ((message (&rest args)
	      (setq #2# (get-internal-real-time))
	      (when (> #2# #1#)
		(setq #1# (+ #2# ,interval))
		(apply #'message args))))
       ,@body)))

(defun cooked-mode-tty-input ()
  (if (buffer-stream-p *standard-output*)
      (with-set-buffer
	(let* ((s *standard-output*)
	       (buf (buffer-stream-buffer s))
	       (start (buffer-stream-point s))
	       (selected-p (eq (selected-buffer) buf))
	       (refresh-p (get-buffer-window buf))
	       (eolp nil))
	  (loop
	    (when refresh-p
	      (and selected-p
		   (goto-char (buffer-stream-point s)))
	      (refresh-screen))
	    (and eolp
		 (return))
	    (let ((c (read-char *keyboard*)))
	      (case c
		(#\C-h
		 (let ((point (buffer-stream-point s)))
		   (when (> point start)
		     (save-excursion
		       (set-buffer buf)
		       (delete-region point (- point 1))))))
		(#\C-d
		 (when (= (buffer-stream-point s) start)
		   (return-from cooked-mode-tty-input nil))
		 (return))
		(#\C-c
		 (quit))
		(t
		 (and (eql c #\RET)
		      (setq c #\LFD))
		 (setq eolp (eq c #\LFD))
		 (write-char c)))))
	  (save-excursion
	    (set-buffer buf)
	    (buffer-substring start (buffer-stream-point s)))))
    (read-char *keyboard*)))

(defvar *cooked-mode-input-stream* nil)

(setf (get 'long-operation 'lisp-indent-hook) 0)

(defmacro long-operation (&rest body)
  `(prog2
       (begin-wait-cursor)
       (progn ,@body)
     (end-wait-cursor)))

(defun msgbox (fmt &rest args)
  (message-box (apply #'format nil fmt args) nil '(:ok :information)))

(defun message (fmt &rest args)
  (terpri *status-window*)
  (apply #'format *status-window* fmt args)
  (terpri *status-window*)
  t)

(defun clear-message ()
  (princ "\n \n" *status-window*))

(defun minibuffer-message (fmt &rest args)
  (si:*minibuffer-message (apply #'format nil fmt args) nil))

(defun minibuffer-prompt (fmt &rest args)
  (si:*minibuffer-message (apply #'format nil fmt args) t))

(defun clear-minibuffer-message ()
  (si:*minibuffer-message nil))

(defun add-hook (hook fn &optional append)
  (or (boundp hook)
      (set hook nil))
  (let ((x (symbol-value hook)))
    (if (or (not (listp x))
	    (eq (car x) 'lambda))
	(set hook (list x))))
  (unless (member fn (symbol-value hook) :test #'equal)
    (set hook
	 (if append
	     (nconc (symbol-value hook) (list fn))
	   (cons fn (symbol-value hook))))))

(defun delete-hook (hook fn)
  (if (boundp hook)
      (let ((x (symbol-value hook)))
	(cond ((equal x fn)
	       (set hook nil))
	      ((listp x)
	       (set hook (remove fn x :test #'equal)))))))

(setf (get 'trap-errors 'lisp-indent-hook) 0)

(defmacro trap-errors (&body body)
  `(handler-case
       (progn ,@body)
     (error (c) (si:*print-condition c) nil)
     (quit (c) (si:*print-condition c) nil)))

(defun plain-error (&rest rest)
  (cond ((null rest)
	 (error 'plain-error))
	((stringp (car rest))
	 (error 'plain-error
	   :format-string (car rest)
	   :format-arguments (cdr rest)))
	(t
	 (apply #'error rest))))

(defun simple-warn (&rest rest)
  (cond ((null rest)
	 (error 'simple-warning))
	((stringp (car rest))
	 (error 'simple-warning
	   :format-string (car rest)
	   :format-arguments (cdr rest)))
	(t
	 (apply #'error rest))))

(defun concat (&rest seq)
  (apply #'concatenate 'string seq))

(defun vconcat (&rest seq)
  (apply #'concatenate 'vector seq))

(defun char-name (c)
  (car (rassoc (char-unicode c) si:*character-name-alist*)))

(defmacro defvar-local (name initial-value &optional doc)
  `(progn
     ,(if doc
	  `(si:*putprop ',name ,doc 'lisp::variable-documentation))
     (or (boundp ',name)
	 (set-default ',name ,initial-value))
     (make-variable-buffer-local ',name)
     ',name))

(defmacro setq-default (var val)
  `(set-default ',var ,val))

(defun setup-trace-output-buffer ()
  (unless (and (buffer-stream-p *error-output*)
	       (ignore-errors (buffer-stream-buffer *error-output*)))
    (let ((buffer (get-buffer-create "*Trace Output*")))
      (setup-temp-buffer buffer)
      (setq *error-output* (make-buffer-stream buffer)))))

(defun toggle-trace-on-error (&optional (arg nil svar))
  (interactive "p")
  (toggle-mode 'si:*trace-on-error* arg svar)
  (when si:*trace-on-error*
    (setup-trace-output-buffer))
  t)

(defvar *match-data-pool* (let ((r nil))
			    (dotimes (x 5 r)
			      (push (make-vector 21) r))))

(defmacro protect-match-data (&body body)
  `(let* ((#1=#:match-data (match-data (car *match-data-pool*)))
	  (*match-data-pool* (cdr *match-data-pool*)))
     (unwind-protect
	 (progn ,@body)
       (store-match-data #1#))))

(setf (get 'protect-match-data 'lisp-indent-hook) 0)

(export '(fundamental-mode *fundamental-mode-hook*))

(defvar *fundamental-mode-hook* nil)

(defvar *fundamental-mode-syntax-table* (make-syntax-table))
(defvar *fundamental-mode-map* (make-sparse-keymap))

(defun fundamental-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Fundamental")
  (setq buffer-mode 'fundamental-mode)
  (use-keymap *fundamental-mode-map*)
  (use-syntax-table *fundamental-mode-syntax-table*)
  (make-local-variable 'highlight-keyword)
  (setq highlight-keyword nil)
  (run-hooks '*fundamental-mode-hook*))

(setq-default mode-name "Fundamental")
(setq-default buffer-mode 'fundamental-mode)

(defvar *last-complex-command* nil)
(defvar *last-complex-command-history* nil)

(defun record-complex-command (command args)
  (setq *last-complex-command* command)
  (setq *last-complex-command-history*
	(format nil "~S" (cons command
			       (mapcar #'(lambda (x)
					   (cond ((or (consp x)
						      (symbolp x))
						  (list 'quote x))
						 ((bufferp x)
						  (buffer-name x))
						 (t x)))
				       args))))
  (add-history *last-complex-command-history*
	       '*minibuffer-complex-command-history*))

(defun execute-extended-command (command &optional raw arg)
  (interactive "0CM-x: \nP\np")
  (let ((*prefix-args* raw)
	(*prefix-value* arg))
    (setq *last-complex-command* nil)
    (setq *this-command* command)
    (command-execute command (and (interactive-p)
				  (not *executing-macro*)
				  #'record-complex-command))))

(defun autoload-complex-command (command raw arg)
  (let ((*prefix-args* raw)
	(*prefix-value* arg)
	(hook nil))
    (when (and (eq *last-complex-command* command)
	       (eq *last-complex-command-history*
		   (car *minibuffer-complex-command-history*)))
      (pop *minibuffer-complex-command-history*)
      (setq hook #'record-complex-command))
    (setq *last-complex-command* nil)
    (command-execute command hook)))

(setf (symbol-function 'call-interactively) #'command-execute)

(defvar *etc-path* nil)

(defun etc-path ()
  *etc-path*)

(defun cd (directory)
  (interactive "Dcd: ")
  (set-default-directory directory))

(defvar *default-load-path* nil)

(defun init-misc-objects ()
  (setq *etc-path* (merge-pathnames "etc" (si:system-root)))
  (setq *cooked-mode-input-stream*
	(make-general-input-stream #'cooked-mode-tty-input nil
				   #'(lambda () (listen *keyboard*))))
  (mapc #'(lambda (x)
	    (pushnew x *load-path* :test #'equalp))
	*default-load-path*))

(defun init-misc-options ()
  (set-tab-columns *tab-columns*)
  (set-kinsoku-mode *kinsoku-mode*)
  (set-kinsoku-chars *kinsoku-bol-chars* *kinsoku-eol-chars*)
  (set-kinsoku-extend-limit *kinsoku-extend-limit*)
  (set-kinsoku-shorten-limit *kinsoku-shorten-limit*)
  (when (and *use-global-ime*
	     (not (ignore-errors (enable-global-ime t))))
    (warn "Global IMEは使用できません")))

(export 'require-mouse-move)
(defun require-mouse-move (f) f)

(in-package "lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(autoload autoload-function-p load-library load-file
	    mc-load-file mc-load-library mc-autoload
	    y-or-n-p yes-or-no-p no-or-yes-p yes-no-or-cancel-p
	    step)))

(defun load-and-go (fn file interactive-p macrop args)
  (let ((ofn (symbol-function fn)))
    (si:*load-library file)
    (when (eq ofn (symbol-function fn))
      (error "オートロードが失敗しました"))
    (cond (macrop
	   (macroexpand (cons fn args)))
	  (interactive-p
	   (ed::autoload-complex-command fn (car args) (cadr args)))
	  (t
	   (apply fn args)))))

(defun autoload (fn file &optional commandp macrop)
  (unless (fboundp fn)
    (eval (cond (macrop
		 `(defmacro ,fn (&rest args)
		    (load-and-go ',fn ,file nil t args)))
		(commandp
		 `(defun ,fn (&rest args)
		    (ed:interactive "P\np")
		    (load-and-go ',fn ,file (ed:interactive-p) nil args)))
		(t
		 `(defun ,fn (&rest args)
		    (load-and-go ',fn ,file nil nil args)))))))

(defun mc-load-and-go (fn file interactive-p macrop args encoding)
  (let ((ofn (symbol-function fn)))
    (mc-load-library file :encoding encoding)
    (when (eq ofn (symbol-function fn))
      (error "オートロードが失敗しました"))
    (cond (macrop
	   (macroexpand (cons fn args)))
	  (interactive-p
	   (ed::autoload-complex-command fn (car args) (cadr args)))
	  (t
	   (apply fn args)))))

(defun mc-autoload (fn file &optional commandp macrop encoding)
  (unless (fboundp fn)
    (eval (cond (macrop
		 `(defmacro ,fn (&rest args)
		    (mc-load-and-go ',fn ,file nil t args ,encoding)))
		(commandp
		 `(defun ,fn (&rest args)
		    (ed:interactive "P\np")
		    (mc-load-and-go ',fn ,file (ed:interactive-p) nil args ,encoding)))
		(t
		 `(defun ,fn (&rest args)
		    (mc-load-and-go ',fn ,file nil nil args ,encoding)))))))

(defun autoload-function-p (def)
  (when (symbolp def)
    (setq def (symbol-function def)))
  (when (si:*closurep def)
    (setq def (si:closure-body def)))
  (when (cond ((eq (safe-car def) 'lambda)
	       (setq def (safe-cddr def))
	       (when (eq (safe-caar def) 'ed::interactive)
		 (setq def (cdr def)))
	       def)
	      ((eq (safe-car def) 'macro)
	       (setq def (safe-cddr def)))
	      (t nil))
    (setq def (safe-car def))
    (and (eq (safe-car def) 'block)
	 (let ((fn (safe-caar (safe-cddr def))))
	   (or (eq fn 'load-and-go)
	       (eq fn 'mc-load-and-go))))))

(defun yes-or-no-p (fmt &rest args)
  (eq (ed:message-box (apply #'format nil fmt args) nil '(:yes-no :question :button1))
      :yes))

(defun no-or-yes-p (fmt &rest args)
  (eq (ed:message-box (apply #'format nil fmt args) nil '(:yes-no :question :button2))
      :yes))

(defun yes-no-or-cancel-p (fmt &rest args)
  (let ((r (ed:message-box (apply #'format nil fmt args) nil
			   '(:yes-no-cancel :question :button1))))
    (if (eq r :cancel)
	(quit)
      (eq r :yes))))

(defun y-or-n-p (fmt &rest args)
  (ed:minibuffer-prompt "~? (y or n) " fmt args)
  (loop
    (case (read-char ed:*keyboard*)
      ((#\Y #\y)
       (return t))
      ((#\N #\n)
       (return nil))
      (#\C-g
       (quit)))))

(defun load-library (filename &rest rest)
  (ed:interactive "sLoad library: ")
  (apply #'si:*load-library filename rest))

(defun load-file (filename)
  (ed:interactive
      (let ((ed:ignored-extensions
	     (remove ".lc" ed:ignored-extensions :test #'string-equal)))
	(list (ed:read-exist-file-name "Load file: "))))
  (load filename))

(defun mc-load-file (filename &optional encoding)
  (ed:interactive
      (list (let ((ed:ignored-extensions
		   (remove ".lc" ed:ignored-extensions :test #'string-equal)))
	      (ed:read-exist-file-name "Load file: "))
	    (and ed:*prefix-value*
		 (ed:read-char-encoding "Encoding: "))))
  (unless (file-exist-p filename)
    (error 'file-not-found
      :datum "ファイルが見つかりません"
      :pathname filename))
  (let (buffer
	(loading (if (ed:string-matchp "\\.lc$" filename)
		     "loading" "LOADING")))
    (unwind-protect
	(ed:save-excursion
	  (setq buffer (ed:create-new-buffer " *load file*"))
	  (ed:set-buffer buffer)
	  (let ((ed:*expected-fileio-encoding*
		 (or encoding
		     (ed::find-file-auto-encoding filename)
		     ed:*expected-fileio-encoding*)))
	    (declare (special ed:*expected-fileio-encoding*))
	    (ed:message "~A ~A..." loading filename)
	    (ed:insert-file-contents filename)
	    (load (ed:make-buffer-stream buffer) :verbose nil)
	    (ed:message "~A ~A...done" loading filename)))
      (when buffer
	(ed:delete-buffer buffer)))))

(defun mc-load-library (filename &key encoding no-suffix (if-does-not-exist :error))
  (ed:interactive "sLoad library: ")
  (let ((path (if no-suffix
		  filename
		(find-load-path filename))))
    (cond ((and path (file-exist-p path))
	   (mc-load-file path encoding))
	  ((not (eq if-does-not-exist ':error))
	   nil)
	  (t
	   (error 'file-not-found
	     :datum "ファイルが見つかりません"
	     :pathname filename)))))

(defvar *bypass-step* nil)

(defmacro step (form &environment env)
  `(let ((*bypass-step* nil))
     (evalhook ',form nil #'step-apply-hook ',env)))

(defun step-apply-hook (fn args)
  (let ((values (multiple-value-list
		 (applyhook fn args nil (and (not *bypass-step*) #'step-apply-hook)))))
    (when (and (not *bypass-step*)
	       (eq (ed:message-box (format nil "Function: ~S~%Args:~{ ~S~}~%Value:~{ ~S~}"
					   fn args values)
				   nil '(:ok-cancel :information))
		   ':cancel))
      (setq *bypass-step* t))
    (values-list values)))
