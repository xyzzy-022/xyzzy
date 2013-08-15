;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "buffer")

(in-package "editor")

(export '(not-modified toggle-read-only get-buffer-create
	  switch-to-buffer rename
	  select-buffer kill-buffer kill-selected-buffer
	  kill-all-buffers
	  *query-kill-buffer-hook*
	  revert-buffer-function revert-buffer
	  insert-buffer-substring insert-buffer
	  verify-buffers-file-modtime switch-to-buffer-other-window
	  get-alternate-file-buffer
	  next-buffer previous-buffer
	  *next-buffer-in-tab-order*
	  *buffer-bar-context-menu*
	  *buffer-bar-context-menu-buffer*
	  emacs-interactive-exist-buffer-name
	  emacs-interactive-buffer-name))

(defvar *directory-buffer-color-alist* nil)
(defvar *next-buffer-in-tab-order* nil)
(defvar *query-kill-buffer-hook* nil)

(defvar-local verify-visited-file-modtime t)

(defun find-buffer-colors (buffer)
  (unless (string= (buffer-name buffer) " " :end1 1)
    (let* ((dir (default-directory buffer))
	   (colors (assoc-if #'(lambda (x)
				 (cond ((atom x)
					(path-equal dir x))
				       ((cdr x) nil)
				       (t
					(path-equal dir (car x)))))
			     *directory-buffer-color-alist*)))
      (unless colors
	(dolist (x *directory-buffer-color-alist*)
	  (and (consp (car x)) (cdar x)
	       (sub-directory-p dir (caar x))
	       (or (null colors)
		   (sub-directory-p (caar x) (caar colors)))
	       (setq colors x))))
      colors)))

(add-hook '*create-buffer-hook* 'create-buffer-hook)

(defun create-buffer-hook (buffer)
  (with-set-buffer
    (save-excursion
      (set-buffer buffer)
      (funcall *default-buffer-mode*)))
  (let ((colors (cdr (find-buffer-colors buffer))))
    (when colors
      (set-buffer-colors colors buffer))))

(defun not-modified ()
  (interactive)
  (set-buffer-modified-p nil)
  (when (and (interactive-p)
	     (null *executing-macro*))
    (message "変更フラグをクリアしました")))

(defun toggle-read-only (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode 'buffer-read-only arg sv))

(defun get-buffer-create (name)
  (or (find-buffer name)
      (create-new-buffer name)))

(defun switch-to-buffer (buffer &optional nowarn)
  (interactive "BSwitch to buffer: " :default0 (other-buffer))
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create buffer)))
  (set-buffer buffer)
  (or nowarn
      (null verify-visited-file-modtime)
      (verify-visited-file-modtime buffer)
      (and (or (eq verify-visited-file-modtime ':auto)
	       (yes-or-no-p "誰かが勝手にファイルを書き換えています。\nディスクから読み直す?"))
	   (revert-buffer)))
  buffer)

(defun switch-to-buffer-other-window (buffer &optional nowarn)
  (interactive "BSwitch to buffer other window: " :default0 (other-buffer))
  (setq buffer (save-excursion (switch-to-buffer buffer nowarn)))
  (when (minibuffer-window-p (selected-window))
    (other-window))
  (when (= (count-windows) 1)
    (split-window))
  (set-window (next-window (selected-window) 'foo))
  (set-buffer buffer)
  buffer)

(defun select-buffer ()
  (interactive)
  (let ((buffer (buffer-selector)))
    (and buffer (switch-to-buffer buffer))))

(defun rename (filename &optional nowarn)
  (interactive "Frename: " :title0 "ファイル名の変更")
  (setq filename (namestring filename))
  (when (or (not (file-exist-p filename))
	    nowarn
	    (no-or-yes-p "~aは既に存在します。リネームしますか?" filename))
    (set-buffer-file-name filename)
    (rename-buffer (file-namestring filename))
    (when need-not-save
      (kill-local-variable 'need-not-save))
    (update-visited-file-modtime)
    (set-buffer-modified-p t)))

(defun kill-buffer (buffer)
  (interactive "bKill buffer: " :default0 (selected-buffer))
  (unless (or (not (with-set-buffer
		     (save-excursion
		       (set-buffer buffer)
		       (run-hook-with-args-while-success '*query-kill-buffer-hook*))))
	      (and (need-buffer-save-p buffer)
		   (not (no-or-yes-p "~sは変更されています。変更内容を破棄しますか?" buffer))))
    (delete-buffer buffer)))

(defun kill-selected-buffer ()
  (interactive)
  (kill-buffer (selected-buffer)))

(defun kill-all-buffers (&optional not-verbose)
  (interactive "p")
  (save-some-buffers (not not-verbose))
  (dolist (buffer (buffer-list) t)
    (unless (string= (buffer-name buffer) " " :end1 1)
      (kill-buffer buffer))))

(defun revert-buffer-normal ()
  (let ((opoint (point)))
    (or (file-visited-p)
        (error "ファイル名がありません。"))
    (when (let ((buffer-read-only nil))
	    (declare (special buffer-read-only))
	    (let* ((filename (get-buffer-file-name))
		   (nlines (read-file filename)))
	      (find-file-lock-file)
	      (find-file-notice filename nlines nil))
	    buffer-read-only)
      (setq buffer-read-only t))
    (goto-char opoint)))

(defun revert-buffer (&optional encoding)
  (interactive "0zEncoding: ")
  (let ((filename (get-buffer-file-name))
	(buffer nil)
	(obuffer (selected-buffer)))
    (when (and filename
	       (not encoding)
	       (not *find-file-no-auto-encoding*))
      (unwind-protect
	  (with-set-buffer
	    (setq buffer (create-new-buffer "*revert-buffer*"))
	    (set-buffer buffer)
	    (let ((enc (find-file-auto-encoding filename)))
	      (when enc
		(setq encoding enc))))
	(when buffer
	  (delete-buffer buffer))
	(set-buffer obuffer))))
  (let ((*expected-fileio-encoding* (or encoding *expected-fileio-encoding*)))
    (declare (special *expected-fileio-encoding*))
    (if revert-buffer-function
	(funcall revert-buffer-function)
      (revert-buffer-normal))))

(defvar-local revert-buffer-function #'revert-buffer-normal)

(defun insert-buffer-substring (buffer &optional from to)
  (insert (save-excursion
            (set-buffer buffer)
	    (buffer-substring (or from (point-min))
			      (or to (point-max))))))

(defun insert-buffer (buffer)
  (interactive "BInsert buffer: " :default0 (other-buffer))
  (let ((opoint (point)))
    (insert-buffer-substring buffer)
    (set-mark-command)
    (goto-char opoint)))

(defun next-buffer ()
  (interactive)
  (let* ((cur (selected-buffer))
	 (next (get-next-buffer cur nil *next-buffer-in-tab-order*)))
    (unless (eq cur next)
      (set-buffer next))))

(defun previous-buffer ()
  (interactive)
  (let* ((cur (selected-buffer))
	 (prev (get-next-buffer cur t *next-buffer-in-tab-order*)))
    (unless (eq cur prev)
      (set-buffer prev))))

(defun verify-buffers-file-modtime ()
  (let ((no-query nil))
    (with-set-buffer
      (dolist (buffer (buffer-list) t)
	(or (null (get-buffer-file-name buffer))
	    (null (buffer-local-value buffer 'verify-visited-file-modtime))
	    (verify-visited-file-modtime buffer)
	    (save-excursion
	      (set-buffer buffer)
	      (when (cond ((eq no-query :yes) t)
			  ((eq no-query :no) nil)
			  ((eq verify-visited-file-modtime ':auto) t)
			  (t
			   (refresh-screen)
			   (case (message-box
				  (format nil "~A\n\n誰かが勝手にファイルを変更しています。ディスクから読み直しますか?"
					  (get-buffer-file-name buffer))
				  nil '(:yes-no-cancel :question :button1)
				  :button4 "いちいちうざい(&A)")
			     (:yes t)
			     (:no nil)
			     (:button4 (setq no-query :yes) t)
			     (t (setq no-query :no) nil))))
		(update-visited-file-modtime)
		(revert-buffer))))))))

(add-hook '*activate-hook* 'verify-buffers-file-modtime)

(defun get-alternate-file-buffer (filename &key (test #'equalp))
  (enum-buffers #'(lambda (buffer)
		    (let ((name (get-buffer-alternate-file-name buffer)))
		      (and name (funcall test filename name) buffer)))))

(defvar *buffer-bar-context-menu* nil)
(defvar *buffer-bar-context-menu-buffer* nil)

(defun default-buffer-bar-context-menu-handler (buffer)
  (when buffer
    (setq *buffer-bar-context-menu-buffer* buffer)
    *buffer-bar-context-menu*))

(setq-default *buffer-bar-context-menu-handler*
	      #'default-buffer-bar-context-menu-handler)

(setq-default *buffer-bar-hook* #'set-buffer)

(defun emacs-read-buffer-name (prompt default fn)
  (when (bufferp default)
    (setq default (buffer-name default)))
  (let ((buffer (funcall fn (format nil "~A(default ~A) "
				    prompt default)
			 :default "")))
    (if (or (null buffer)
	    (equal buffer ""))
	(setq buffer default))
    (list (if (bufferp buffer)
	      buffer
	    (or (find-buffer buffer) buffer)))))

(defun emacs-interactive-exist-buffer-name (prompt default &rest rest)
  (emacs-read-buffer-name prompt (or default (selected-buffer))
			  #'read-exist-buffer-name))

(defun emacs-interactive-buffer-name (prompt default &rest rest)
  (emacs-read-buffer-name prompt (or default (other-buffer))
			  #'read-buffer-name))

#|
;; 以下を .xyzzy に書いておけば emacs 風のバッファ名指定ができる。
(pushnew '(#\B . emacs-interactive-buffer-name)
	 *interactive-specifier-alist* :test #'equal)
(pushnew '(#\b . emacs-interactive-exist-buffer-name)
	 *interactive-specifier-alist* :test #'equal)
|#

(global-set-key #\Home 'next-buffer)
(global-set-key #\S-Home 'previous-buffer)
(define-key esc-map #\~ 'not-modified)
(define-key ctl-x-map #\C-q 'toggle-read-only)
(define-key ctl-x-map #\b 'switch-to-buffer)
(define-key ctl-x-map #\k 'kill-buffer)
(define-key ctl-x-map #\C-n 'rename)
(global-set-key #\F2 'select-buffer)
(define-key ctl-x-4-map #\b 'switch-to-buffer-other-window)
