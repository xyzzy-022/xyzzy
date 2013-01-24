;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "process")

(in-package "editor")

(export '(*shell* *eshell* *shell-command-option* *shell-chdir-option-alist*
	  execute-shell-command
	  launch-application run-console run-admin-console pipe-command
	  filter-buffer filter-region execute-subprocess kill-subprocess
	  command-output-mode *command-output-mode-hook*
	  command-output-send-input command-output-alternate-send-input
	  process-send-y process-send-n execute-region))

(let ((shell (if (featurep :windows-nt) "cmd.exe" "command.com")))
  (defvar *shell* shell)
  (defvar *eshell* shell))

(defvar *shell-command-option* "/c")
(defvar *shell-chdir-option-alist*
  '(("cmd.exe" . "/k cd /d \"~A\"")
    ("powershell.exe" . "-NoExit -Command \"cd '~A'\"")
    ))

(defvar *launch-app-directory* nil)

(defun launch-application (app)
  (interactive "e% " :history0 'execute)
  (call-process app :no-std-handles t :show :show
		:exec-directory *launch-app-directory*))

(defun run-console ()
  (interactive)
  (launch-application (or *eshell* *shell*)))

(defun run-admin-console ()
  (interactive)
  (let ((path (si:search-path (or *eshell* *shell*) nil ".exe")))
    (unless path
      (error "shellが見つかりません: ~A" (or *eshell* *shell*)))
    (let* ((exec-dir (map-slash-to-backslash (default-directory)))
	   (params (let ((fmt (cdr (assoc (file-namestring path) *shell-chdir-option-alist*
					  :test #'string-equal))))
		     (when fmt
		       (format nil fmt exec-dir)))))
      (shell-execute path exec-dir params :verb :runas))))

(defun shell-command-line (cmd dir)
  (when (and (not (featurep :windows-nt))
	     (string-match "\\(^\\|[\\/]\\)command\\(\\.com\\)?$" *shell*))
    (let ((sdir (get-short-path-name (append-trail-slash dir))))
      (when (>= (length sdir) 67)
	(return-from shell-command-line
	  (values (format nil "~A ~A ~A -- ~A ~A ~A ~A"
			  *shell* *shell-command-option*
			  (map-slash-to-backslash
			   (get-short-path-name
			    (merge-pathnames "xyzzyenv.exe" (si:system-root))))
			  sdir *shell* *shell-command-option* cmd)
		  (si:system-root))))))
  (values (format nil "~A ~A ~A" *shell* *shell-command-option* cmd)
	  (if (pathname-host dir)
	      (get-windows-directory)
	    dir)))

(defun execute-shell-command (command &optional infile output environ directory)
  (let (outfile)
    (unless directory
      (setq directory (default-directory)))
    (unwind-protect
	(prog2
	    (when output
	      (setq outfile (make-temp-file-name)))
	    (multiple-value-bind (cmdline dir)
		(shell-command-line command directory)
	      (call-process cmdline
			    :input infile
			    :output outfile
			    :exec-directory dir
			    :environ environ
			    :show :minimize
			    :wait t))
	  (when output
	    (let (new)
	      (unless (bufferp output)
		(setq output (or (find-buffer output)
				 (progn
				   (setq new t)
				   (create-new-buffer output)))))
	      (erase-buffer output)
	      (update-visited-file-modtime)
	      (pop-to-buffer output t)
	      (insert-file-contents outfile)
	      (and new
		   (setq need-not-save t))
	      (set-buffer-modified-p nil)
	      (set-default-directory directory))))
      (when outfile
	(delete-file outfile)))))

(defun pipe-command (cmd)
  (interactive "e@ " :history0 'execute)
  (execute-shell-command cmd nil "*Shell output*"))

#|
(defun filter-buffer (cmd)
  (interactive "*e# " :history0 'execute)
  (when (and (need-buffer-save-p (selected-buffer))
	     (not (no-or-yes-p "バッファが変更されています。変更内容を破棄しますか?")))
    (return-from filter-buffer nil))
  (let ((tem (make-temp-file-name)))
    (write-file tem t)
    (unwind-protect
	(execute-shell-command cmd tem (selected-buffer))
      (delete-file tem))))
|#

(defun filter-buffer (command)
  (interactive "*e# " :history0 'execute)
  (filter-region command (point-min) (point-max)))

(defun filter-region (command &optional start end)
  (interactive "*e| \nr" :history0 'execute)
  (let (infile outfile)
    (when (and start end)
      (setq infile (make-temp-file-name))
      (write-region start end infile nil
		    (let ((encoding (buffer-fileio-encoding)))
		      (or (cdr (assoc encoding *char-encoding-bom-alist*))
			  encoding))))
    (setq outfile (make-temp-file-name))
    (unwind-protect
	(prog1
	    (multiple-value-bind (cmdline dir)
		(shell-command-line command (default-directory))
	      (call-process cmdline
			    :input infile
			    :output outfile
			    :exec-directory dir
			    :show :minimize
			    :wait t))
	  (when (and start end)
	    (delete-region start end))
	  (insert-file-contents outfile))
      (when infile
	(delete-file infile))
      (delete-file outfile))))


(defvar *command-output-mode-hook* nil)
(defvar *command-output-mode-map* nil)
(unless *command-output-mode-map*
  (setq *command-output-mode-map* (make-sparse-keymap))
  (define-key *command-output-mode-map* #\RET 'command-output-send-input)
  (define-key *command-output-mode-map* #\C-j 'command-output-alternate-send-input))

(defun command-output-mode ()
  (kill-all-local-variables)
  (setq mode-name "Command Output")
  (setq buffer-mode 'command-output-mode)
  (use-keymap *command-output-mode-map*)
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information nil)
  (run-hooks '*command-output-mode-hook*))

(defun command-output-send-input (&optional (eol "\n"))
  (interactive)
  (let ((process (buffer-process (selected-buffer))))
    (if (and process
	     (process-marker process)
	     (>= (point) (marker-point (process-marker process))))
	(let* ((start (marker-point (process-marker process)))
	       (end (progn (goto-eol) (point)))
	       (string (buffer-substring start end)))
	  (delete-region start end)
	  (process-send-string process (concatenate 'string string eol)))
      (insert "\n"))))

(defun command-output-alternate-send-input ()
  (interactive)
  (command-output-send-input "\r"))

(defun execute-subprocess (cmd &optional (arg nil f) bufname environ directory)
  (interactive "e& \np" :history0 'execute)
  (unless bufname
    (setq bufname "*Command Output*"))
  (unless directory
    (setq directory (default-directory)))
  (let ((buffer
	 (save-excursion
	   (let* ((buffer (switch-to-buffer bufname))
		  (proc (buffer-process buffer)))
	     (when (and proc (eq (process-status proc) :run))
	       (cond (f
		      (do ((x 0 (1+ x)))
			  ((progn
			     (setq buffer (switch-to-buffer (format nil "~A(~D)"
								    bufname x)))
			     (setq proc (buffer-process buffer))
			     (not (and proc (eq (process-status proc) :run)))))))
		     (t
		      (if (yes-or-no-p "すでにサブプロセスを実行中です。死なす?")
			  (signal-process proc)
			(return-from execute-subprocess nil))
		      (sit-for 1))))
	     (set-default-directory directory buffer)
	     (erase-buffer buffer)
	     (command-output-mode)
	     buffer))))
    (multiple-value-bind (cmdline dir)
	(shell-command-line cmd directory)
      (make-process cmdline :environ environ :output buffer :exec-directory dir))
    (pop-to-buffer buffer t)
    buffer))

(defun execute-region (from to)
  (interactive "r")
  (let (batch
	(delete t))
    (unwind-protect
	(progn
	  (setq batch (make-temp-file-name nil "bat"))
	  (write-region from to batch)
	  (let ((buffer (execute-subprocess
			 (map-slash-to-backslash batch))))
	    (set-process-sentinel (buffer-process buffer)
				  #'(lambda (proc)
				      (delete-file batch)))
	    (setq delete nil)))
      (and delete batch
	   (delete-file batch)))
    t))

(defun kill-subprocess ()
  (interactive)
  (let ((proc (buffer-process (selected-buffer))))
    (if (and proc (eq (process-status proc) :run))
	(signal-process proc)
      (error "サブプロセスはないで"))))

(defun process-send-y-or-n (s)
  (let ((proc (buffer-process (selected-buffer))))
    (if (and proc (eq (process-status proc) :run))
	(process-send-string proc s)
      (error "サブプロセスはないで"))))

(defun process-send-y ()
  (interactive)
  (process-send-y-or-n "y\n"))

(defun process-send-n ()
  (interactive)
  (process-send-y-or-n "n\n"))

(define-key ctl-x-map #\c 'run-console)
(define-key ctl-x-map #\% 'launch-application)
(define-key ctl-x-map #\@ 'pipe-command)
(define-key ctl-x-map #\# 'filter-buffer)
(define-key ctl-x-map #\| 'filter-region)
(define-key ctl-x-map #\& 'execute-subprocess)
(define-key spec-map #\C-k 'kill-subprocess)

