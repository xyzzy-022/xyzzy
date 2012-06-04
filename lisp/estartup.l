;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "estartup")

(in-package "editor")

(export '(*inhibit-history-load* *pre-startup-hook* *post-startup-hook*
	  *command-line-mailto-hook* *print-option-show-dialog*
	  *process-command-line-hook* *loading-user-initial-file*
	  *command-line-args-base-directory*
	  ))

(defvar *inhibit-history-load* nil)
(defvar *pre-startup-hook* nil)
(defvar *post-startup-hook* nil)
(defvar *command-line-mailto-hook* nil)
(defvar *print-option-show-dialog* t)
(defvar *process-command-line-hook* nil)
(defvar *loading-user-initial-file* nil)
(defvar *command-line-args-base-directory* nil)

(defun parse-command-line (arg short long warn)
  (if (or (equal short arg)
	  (equal long arg))
      (if warn
	  (if si:*command-line-args*
	      (pop si:*command-line-args*)
	    (progn
	      (warn "No ~As given for `~A'" warn arg)
	      (throw 'warn nil)))
	t)
    (when (and short warn)
      (let ((l (string/= short arg)))
	(when (= l (length short))
	  (subseq arg l))))))

(defun process-command-line-1 (dir waitobj)
  (let ((mode nil)
	(ro nil)
	(*command-line-args-base-directory* dir))
    (catch 'warn
      (while si:*command-line-args*
	(let ((arg (pop si:*command-line-args*))
	      opt)
	  (cond ((setq opt (parse-command-line arg "-I" "-load-path" 'dir))
		 (push (merge-pathnames opt dir) *load-path*))
		((setq opt (parse-command-line arg "-l" "-load" 'file))
		 (load opt))
		((setq opt (parse-command-line arg "-R" "-require" 'module))
		 (require opt))
		((setq opt (parse-command-line arg nil "-work-dir" 'dir))
		 (chdir opt))
		((setq opt (parse-command-line arg "-f" "-funcall" 'function))
		 (funcall (read-from-string opt)))
		((setq opt (parse-command-line arg "-e" "-eval" 'string))
		 (eval (read-from-string opt)))
		((setq opt (parse-command-line arg "-g" "-go" 'number))
		 (goto-line (parse-integer opt)))
		((setq opt (parse-command-line arg "-c" "-column" 'number))
		 (goto-column (parse-integer opt)))
		((equal "-trace" arg)
		 (toggle-trace-on-error t))
		((equal "-kill" arg)
		 (when si:*accept-kill-xyzzy*
		   (kill-xyzzy)))
		((setq opt (parse-command-line arg nil "-mailto" 'args))
		 (when *command-line-mailto-hook*
		   (trap-errors
		     (funcall *command-line-mailto-hook* opt))))
		((setq opt (parse-command-line arg "-m" "-mode" 'mode))
		 (setq mode (find-symbol opt)))
		((setq opt (parse-command-line arg "-p" nil 'file))
		 (let ((filename (merge-pathnames opt dir)))
		   (find-file filename)
		   (cond (*print-option-show-dialog*
			  (refresh-screen 'foo)
			  (print-selected-buffer-dialog))
			 (t
			  (print-selected-buffer))))
		 (and si:*accept-kill-xyzzy*
		      (or si:*command-line-args*
			  (kill-xyzzy))))
		((setq opt (parse-command-line arg "-s" nil 'file))
		 (trap-errors
		   (load-session (merge-pathnames opt dir))
		   (setq *auto-save-session-file* t)))
		((setq opt (parse-command-line arg "-S" nil 'file))
		 (trap-errors
		   (load-session (merge-pathnames opt dir))
		   (setq *auto-save-session-file* nil)))
		((equal "-ro" arg)
		 (setq ro t))
		((equal "-rw" arg)
		 (setq ro nil))
		((run-hook-with-args-until-success
		  '*process-command-line-hook* arg))
		(t
		 (let* ((filename (merge-pathnames arg dir))
			(*find-file-auto-mode-function* mode))
		   (if ro
		       (find-file-read-only filename)
		     (find-file filename))
		   (add-history filename '*minibuffer-file-name-history*)
		   (when waitobj
		     (si:*add-wait-object waitobj))))))))))

(defun process-command-line ()
  (process-command-line-1 (cwd) nil))

(defun *xyzzycli-helper (dir args)
  (si:*show-window-foreground)
  (let ((si:*command-line-args*
	 (mapcan #'(lambda (arg)
		     (let ((wild (ignore-errors (merge-pathnames arg dir))))
		       (or (and wild
				(wild-pathname-p wild)
				(directory wild :absolute t))
			   (list arg))))
		 args))
	waitobj)
    (when (equal (car si:*command-line-args*) "-wait")
      (setq waitobj (si:*create-wait-object))
      (pop si:*command-line-args*))
    (handler-case
	(with-set-buffer
	  (process-command-line-1 dir waitobj))
      (error (c) (si:*print-condition c))
      (quit (c) (si:*print-condition c)))
    waitobj))

(defun integer-version (version)
  (do ((i 0 (+ i 1))
       (l (split-string version #\.) (cdr l))
       (v 0 (+ (* v 65536) (if (and l (string-match "^[0-9]+$" (car l)))
			       (parse-integer (car l)) 0))))
      ((>= i 5) v)))

(defun older-version-p (version)
  (let ((v (integer-version version)))
    (and (or (not (stringp *last-xyzzy-version*))
	     (< (integer-version *last-xyzzy-version*) v))
	 (>= (integer-version (software-version)) v))))

(defun keep-compatibility ()
  (define-history-variable *last-xyzzy-version* nil)
  (setq *last-xyzzy-version* (software-version)))

(defun startup ()
  (trap-errors (init-misc-objects))
  (trap-errors (init-pseudo-frame))
  (trap-errors (run-hooks '*pre-startup-hook*))
  (let (no-init-file
	(cl (car si:*command-line-args*)))
    (when (or (equal cl "-q")
	      (equal cl "-no-init-file"))
      (setq no-init-file t)
      (pop si:*command-line-args*))
    (when *convert-registry-to-file-p*
      (ignore-errors (load-history-file-compat)))
    (unless no-init-file
      (trap-errors
	(let ((*loading-user-initial-file* t))
	  (load (namestring (if (file-system-supports-long-file-name-p "~/")
				"~/.xyzzy" "~/_xyzzy"))
		:if-does-not-exist nil :verbose nil :print nil)))
      (unless *inhibit-history-load*
	(ignore-errors (load-history-file))))
    (ignore-errors (keep-compatibility))
    (ignore-errors (init-app-menus))
    (ignore-errors (init-misc-options))
    (trap-errors
      (let ((scratch (find-buffer "*scratch*")))
	(when scratch
	  (save-excursion
	    (set-buffer scratch)
	    (funcall *initial-buffer-mode*)))))
    (trap-errors (process-command-line))
    (trap-errors (run-hooks '*post-startup-hook*))))
