;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "session")

(in-package "editor")

(export '(save-session load-session open-session-dialog save-session-dialog
	  *default-session-directory* toggle-session-auto-save
	  close-session close-session-dialog
	  *buffer-info-variable-list*))

(define-history-variable *session-file-history* nil)
(setf (get '*session-file-history* 'minibuffer-history-variable)
      '*session-file-history*)
(setf (get '*session-file-history* 'add-lru-menu-function)
      'add-session-history-to-menu)
(setf (get '*session-file-history* 'history-ignore-case) t)

(defvar *default-session-directory* nil)
(defvar *current-session-file-name* nil)
(define-history-variable *auto-save-session-file* nil)

(defvar *buffer-info-variable-list*
  '(buffer-read-only))

(defun winconf-to-readable-winconf (conf)
  (list 'window-configuration
	(if (windowp (cadr conf))
	    (position (cadr conf) (caddr conf) :key #'car)
	  (cadr conf))
	(mapcar #'(lambda (x)
		    (mapcar #'(lambda (x)
				(cond ((windowp x) nil)
				      ((markerp x)
				       (marker-point x))
				      ((bufferp x)
				       (unless (deleted-buffer-p x)
					 (cons (get-buffer-file-name x)
					       (buffer-name x))))
				      (t x)))
			    x))
		(caddr conf))
	(cadddr conf)))

(defun readable-winconf-to-winconf (conf)
  (list 'window-configuration
	(cadr conf)
	(mapc #'(lambda (x)
		  (let ((f (cadr x)))
		    (when f
		      (setf (cadr x)
			    (or (and (car f) (get-file-buffer (car f)))
				(find-buffer (cdr f)))))))
	      (caddr conf))
	(cadddr conf)))

(defun list-buffer-info ()
  (save-excursion
    (let ((info nil))
      (mapc #'(lambda (buffer)
		(when (file-visited-p buffer)
		  (set-buffer buffer)
		  (push (list (get-buffer-file-name buffer)
			      (point)
			      (cons buffer-mode
				    (mapcan #'(lambda (mode)
						(let ((var (and (consp mode) (car mode))))
						  (and (symbolp var)
						       (boundp var)
						       (symbol-value var)
						       (list var))))
					    *minor-mode-alist*))
			      (mapcar #'(lambda (var)
					  (and (symbolp var)
					       (local-variable-p var)
					       (cons var (symbol-value var))))
				      *buffer-info-variable-list*))
			info)))
	    (buffer-list :buffer-bar-order t))
      (nreverse info))))

(defun restore-buffer-info (info)
  (let ((obuffer (selected-buffer)))
    (mapc #'(lambda (i)
	      (let ((file (pop i)))
		(when (file-exist-p file)
		  (handler-case
		      (let ((point (pop i))
			    (mode (pop i))
			    (minor nil))
			(when (listp mode)
			  (setq minor (cdr mode))
			  (setq mode (car mode)))
			(let ((*find-file-auto-mode-function* mode))
			  (find-file file))
			(goto-char point)
			(mapc #'(lambda (f) (and (fboundp f) (funcall f))) minor)
			(mapc #'(lambda (x)
				  (when (and (car x) (symbolp (car x)))
				    (make-local-variable (car x))
				    (set (car x) (cdr x))))
			      (pop i)))
		    (file-error (c)
		      (si:*print-condition c))))))
	  info)
    (set-buffer obuffer)))

(defun save-session-info (s)
  (save-current-pseudo-frame)
  (let ((finfo (mapcan #'(lambda (frame)
			   (when (pseudo-frame-save-p frame)
			     (list (list (pseudo-frame-name frame)
					 (winconf-to-readable-winconf
					  (pseudo-frame-winconf frame))
					 (eq frame *current-pseudo-frame*)))))
		       (list-pseudo-frames)))
	(binfo (list-buffer-info)))
    (princ ";;; xyzzy session file.
;;; This file is generated automatically. do not edit.\n" s)
    (write `(in-package ,(package-name *package*)) :stream s :escape t :pretty t :readably t)
    (terpri s)
    (write `(restore-session ',binfo ',finfo) :stream s :escape t :pretty t :readably t)
    (terpri s)))

(defun write-session-file (filename)
  (with-open-file (s filename
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (save-session-info s)
    t))

(defun session-directory ()
  (or *default-session-directory*
      (and *current-session-file-name*
	   (directory-namestring *current-session-file-name*))))

(defun save-session (filename &optional nowarn)
  (interactive "FSave session: "
    :title0 "Save session" :history0 '*session-file-history*
    :default0 (session-directory))
  (setq filename (namestring filename))
  (unless (or nowarn
	      (not (file-exist-p filename))
	      (yes-or-no-p "~A~%はすでに存在します。上書きしますか?"
			   filename))
    (quit))
  (write-session-file filename)
  (setq *current-session-file-name* filename))

(defun restore-session (binfo finfo)
  (save-current-pseudo-frame)
  (restore-buffer-info binfo)
  (let ((current nil))
    (mapc #'(lambda (info)
	      (let ((frame (or (find-pseudo-frame (car info))
			       (create-pseudo-frame-1 (car info) t))))
		(setf (pseudo-frame-winconf frame)
		      (readable-winconf-to-winconf (cadr info)))
		(when (caddr info)
		  (setq current frame))))
	  finfo)
    (when current
      (setq *current-pseudo-frame* nil)
      (select-pseudo-frame current))))

(defun close-session (&optional noask (create-new t) (fn #'write-file))
  (interactive "p")
  (save-some-buffers (not noask) fn)
  (auto-save-session-file)
  (let ((scratch (find-buffer "*scratch*")))
    (dolist (buffer (buffer-list))
      (or (eq buffer scratch)
	  (delete-buffer buffer))))
  (delete-all-pseudo-frames create-new)
  (delete-other-windows)
  (setq *current-session-file-name* nil)
  (clear-message))

(defun close-session-dialog (&optional noask)
  (interactive "p")
  (close-session noask t #'save-buffer-dialog))

(defun load-session (filename &optional not-close)
  (interactive "fLoad session: \np"
    :title0 "Load session" :history0 '*session-file-history*
    :default0 (session-directory))
  (setq filename (namestring filename))
  (with-open-file (s filename
		     :direction :input
		     :if-does-not-exist :error)
    (unless (equal (read-line s nil) ";;; xyzzy session file.")
      (error "セッションファイルではありません")))
  (unwind-protect
      (progn
	(or not-close
	    (close-session nil nil (if (interactive-p)
				       #'write-file #'save-buffer-dialog)))
	(load filename :verbose nil :print nil)
	(setq *current-session-file-name* filename))
    (init-pseudo-frame))
  t)

(defvar *session-dialog-filter*
  '(("セッションファイル(*.ssn)" . "*.ssn")
    ("すべてのファイル(*.*)" . "*.*")))

(defun open-session-dialog ()
  (interactive)
  (let ((file (file-name-dialog :title "セッションを開く"
				:initial-directory (session-directory)
				:filter *session-dialog-filter*
				:extension "ssn")))
    (when file
      (load-session file)
      (add-history file '*session-file-history*)
      t)))

(defun save-session-dialog ()
  (interactive)
  (let ((file (file-name-dialog :save t
				:title "セッションの保存"
				:initial-directory (session-directory)
				:filter *session-dialog-filter*
				:extension "ssn"
				:overwrite t :hide-read-only t)))
    (when file
      (write-session-file file)
      (setq *current-session-file-name* file)
      (add-history file '*session-file-history*)
      t)))

(defun toggle-session-auto-save (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode '*auto-save-session-file* arg sv))

(defun update-session-auto-save-ui ()
  (cond ((null *current-session-file-name*) :disable)
	(*auto-save-session-file* :check)
	(t t)))

(defun auto-save-session-file ()
  (when (and *auto-save-session-file*
	     *current-session-file-name*)
    (ignore-errors
     (write-session-file *current-session-file-name*))))

(add-hook '*save-history-hook* #'auto-save-session-file)

(defun add-session-history-to-menu ()
  (when (menup *app-menu*)
    (add-lru-menu *recent-session-popup-menu*
		  *session-file-history*
		  #'(lambda (filename)
		      #'(lambda ()
			  (interactive)
			  (load-session filename)
			  (add-history filename '*session-file-history*))))))

(defun restore-resume-info (info conf)
  (when *save-resume-info*
    (restore-buffer-info info)
    (when conf
      (ignore-errors
       (set-window-configuration
	(readable-winconf-to-winconf conf))))))

(defun save-resume-info (s)
  (when *save-resume-info*
    (let ((conf (winconf-to-readable-winconf
		 (current-window-configuration)))
	  (info (list-buffer-info)))
      (write `(in-package ,(package-name *package*)) :stream s :escape t :pretty t :readably t)
      (terpri s)
      (write `(ignore-errors (restore-resume-info ',info ',conf))
	     :stream s :escape t :pretty t :readably t)
      (terpri s))))

(define-key ctl-x-6-map #\f 'load-session)
(define-key ctl-x-6-map #\s 'save-session)
(define-key ctl-x-6-map #\k 'close-session)
(define-key ctl-x-6-map #\F 'open-session-dialog)
(define-key ctl-x-6-map #\S 'save-session-dialog)
(define-key ctl-x-6-map #\K 'close-session-dialog)
