;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "files")

(in-package "editor")

(export '(*find-file-hooks* *find-file-file-not-found-hook*
	  *before-find-file-hook* *auto-mode-parameter-alist*
	  find-file find-other-file
	  read-file insert-file save-some-buffers save-buffers-kill-xyzzy
	  write-file append-file new-file save-all-buffers
	  save-all-buffers-kill-xyzzy lock-file exclusive-lock-file
	  find-file-other-window create-shortcut-to-desktop
	  *find-other-file-requires-file-name*
	  create-shortcut find-file-in-archive
	  *default-write-file-directory* emacs-write-file
	  *auto-encoding-alist* *find-file-no-auto-encoding*
	  *find-file-auto-encoding-use-utf8n*
	  find-file-read-only *find-file-read-only-hook*
	  *auto-mode-last-buffer* *find-file-auto-mode-function*))

;; files

(defvar *find-file-hooks* nil)
(defvar *find-file-file-not-found-hook* nil)
(defvar *before-find-file-hook* nil)
(defvar *find-file-read-only-hook* nil)
(defvar-local lock-file nil)
(defvar-local exclusive-lock-file nil)
(defvar *find-other-file-requires-file-name* nil)
(defvar *auto-mode-parameter-alist*
  '(("mode" . find-file-auto-mode)
    ("package" . find-file-buffer-package)))
(defvar *default-write-file-directory* nil)

(defvar *auto-encoding-alist* nil)
(defvar *find-file-no-auto-encoding* nil)
(defvar *find-file-auto-encoding-use-utf8n* t)
(defvar *auto-mode-last-buffer* nil)
(defvar *find-file-auto-mode-function* nil)

(defun find-file-lock-file ()
  (when (and lock-file
	     (not (eq lock-file :edit)))
    (handler-case
	(when (eq (lock-file) :shared)
	  (setq buffer-read-only t))
      (file-error (c)
	(setq buffer-read-only t)
	(si:*print-condition c)))))

(defun find-file-verify (buffer filename nowarn encoding eol-code nomsg)
  (if (or nowarn
	  (verify-visited-file-modtime buffer)
	  (cond ((not (file-exist-p filename))
		 (error "ファイルが削除されています" filename))
		((yes-or-no-p (if (buffer-modified-p buffer)
				  "誰かが勝手にファイルを変更しています。\n変更内容を破棄してディスクから読みなおしますか?"
				"誰かが勝手にファイルを変更しています。\nディスクから読みなおしますか?"))
		 (save-excursion
		   (set-buffer buffer)
		   (when eol-code
		     (set-buffer-eol-code eol-code))
		   (revert-buffer encoding)
		   nil))
		(t t)))
      (or nomsg
	  (message "Old buffer"))))

(defun find-file-notice (filename nlines nomsg)
  (cond ((file-writable-p filename)
	 (unless nomsg
	   (if buffer-read-only
	       (message "~aは他のプロセスが上書き禁止で開いています。~d行読み込みました"
			filename nlines)
	     (message "~d行読み込みました" nlines))))
	(t
	 (setq buffer-read-only t)
	 (or nomsg
	     (message "~aは書き込み禁止ファイルです。~d行読み込みました"
		      filename nlines)))))

(defun find-file-readin (filename nowarn nomsg)
  (when buffer-read-only
    (setq buffer-read-only nil))
  (cond ((file-exist-p filename)
	 (or nomsg (message "Reading file..."))
	 (multiple-value-bind (nlines noerror)
	     (handler-case
		 (insert-file-contents filename t)
	       (file-error (c)
		 (delete-buffer (selected-buffer))
		 (error c)))
	   (unless noerror
	     (set-buffer-truncated-p t)
	     (or nowarn
		 (message-box (format nil "~aはメモリが足りなくて全部読み込めませんでした。"
				      filename)
			      nil '(:exclamation))))
	   (find-file-lock-file)
	   (find-file-notice filename nlines nomsg))
	 t)
	(t
	 (clear-visited-file-modtime)
	 (or nomsg
	     (message "新しいファイルです"))
	 nil)))

(defun find-file-scan-params ()
  (save-excursion
    (let (beg end (params nil))
      (goto-char (point-min))
      (when (and (when (scan-buffer "-*-" :tail t :limit (+ (point) 1000))
		   (skip-chars-forward " \t")
		   (setq beg (point)))
		 (when (scan-buffer "-*-" :limit (save-excursion (goto-eol) (point)))
		   (skip-chars-backward " \t")
		   (setq end (point))))
	(goto-char beg)
	(while (scan-buffer "[ \t\n]*\\([^ \t\n:]+\\)[ \t\n]*:[ \t\n]*\\([^ \t\n;]*\\)[ \t\n]*;?"
			    :regexp t :tail t :limit end)
	  (push (cons (match-string 1) (match-string 2)) params))
	(if params
	    (nreverse params)
	  (list (cons "mode" (buffer-substring beg end))))))))

(setf (get 'find-file-auto-mode 'find-file-auto-mode) t)

(defun find-file-auto-mode (string)
  (let (mode)
    (when (or (and (setq mode (find-symbol (concat string "-mode")))
		   (fboundp mode))
	      (and (setq mode (find-symbol (concat (string-downcase string) "-mode")))
		   (fboundp mode)))
      (funcall mode)
      t)))

(defun find-file-buffer-package (string)
  (setq *buffer-package* (string-downcase string)))

(defun find-file-process-params (params last-buffer)
  (let ((auto-mode nil))
    (declare (special auto-mode))
    (when (and (symbolp *find-file-auto-mode-function*)
	       (fboundp *find-file-auto-mode-function*))
      (funcall *find-file-auto-mode-function*)
      (setq auto-mode t))
    (mapc #'(lambda (x)
	      (let ((f (cdr (assoc (car x) *auto-mode-parameter-alist*
				   :test #'string-equal))))
		(when f
		  (unless (and auto-mode (get f 'find-file-auto-mode))
		    (when (and (funcall f (cdr x))
			       (get f 'find-file-auto-mode))
		      (setq auto-mode t))))))
	  params)
    (unless auto-mode
      (let ((filename (get-buffer-file-name))
	    (*auto-mode-last-buffer* last-buffer))
	(dolist (x *auto-mode-alist*)
	  (when (string-matchp (car x) filename)
	    (funcall (cdr x))
	    (return)))))))

(defun find-file-auto-encoding (filename)
  (unwind-protect
      (let ((encoding
	     (progn
	       (ignore-errors (insert-file-contents filename t nil 4096))
	       (goto-char (point-min))
	       (dolist (x *auto-encoding-alist* nil)
		 (when (string-matchp (car x) filename)
		   (return (funcall (cdr x))))))))
	(when (stringp encoding)
	  (setq encoding (gethash encoding
				  *mime-charset-name-hash-table*)))
	(if (char-encoding-p encoding)
	    encoding
	  (progn
	    (setq encoding (find-file-detect-auto-encoding))
	    (when (stringp encoding)
	      (setq encoding (gethash encoding *mime-charset-name-hash-table*)))
	    (when (and encoding *find-file-auto-encoding-use-utf8n*)
	      (flet ((utf8-encoding-p (x)
		       (and (eq (char-encoding-type x) :utf8)
			    (char-encoding-signature x))))
		(when (and (utf8-encoding-p encoding)
			   (not (utf8-encoding-p (buffer-fileio-encoding))))
		  (setq encoding (cdr (assoc encoding *char-encoding-bom-alist*))))))
	    encoding)))
    (erase-buffer (selected-buffer))))

(defun find-file-detect-auto-encoding ()
  (or (cdr (assoc "\\`\\(en\\)?coding\\'"
		  (find-file-scan-params)
		  :test #'string-matchp))
      (find-file-detect-xml-encoding)
      ))

(defun find-file-detect-xml-encoding ()
  (flet ((scan-buffer-utf16 (string &key big-endian-p)
	   (and (scan-buffer (format nil "~{~C~}"
				     (let* ((chars (coerce string 'list))
					    (nulls (make-list (length chars) :initial-element #\NUL)))
				       (if big-endian-p
					   (mapcan #'list nulls chars)
					 (mapcan #'list chars nulls)))))
		(evenp (point)))))
    (save-excursion
      (goto-char (point-min))
      (let ((be (save-excursion (scan-buffer-utf16 "<?xml" :big-endian-p t)))
	    (le (save-excursion (scan-buffer-utf16 "<?xml" :big-endian-p nil)))
	    beg end)
	(when (or be le)
	  (save-excursion
	    (replace-buffer (format nil "~C" #\NUL) "")))
	(when (and (when (scan-buffer "<?xml" :limit (+ (point) 1000))
		     (setq beg (point)))
		   (when (scan-buffer "?>" :limit (+ (point) 1000))
		     (setq end (point))))
	  (goto-char beg)
	  (when (scan-buffer "encoding[ \t\n]*=[ \t\n]*['\"]\\(.+?\\)['\"]"
			     :regexp t :tail t :limit end)
	    (let ((encoding (match-string 1)))
	      (if (or (string-equal encoding "UTF-16")
		      (string-equal encoding "ISO-10646-UCS-2"))
		  (cond (be *encoding-utf16be*)
			(le *encoding-utf16le*)
			((eq (char-encoding-type (buffer-fileio-encoding)) :utf16)
			 (buffer-fileio-encoding))
			(t *encoding-utf16*))
		encoding))))))))

(defun find-file-internal (filename &optional nowarn encoding eol-code nomsg read-only)
  (let ((r (run-hook-with-args-until-success
	    '*before-find-file-hook* filename)))
    (when r (return-from find-file-internal r)))
  (setq filename (namestring filename))
  (if (file-exist-p filename)
      (setq filename (truename filename)))
  (when (special-file-p filename)
    (error "~aはデバイスファイルです" filename))
  (when (file-directory-p filename)
    (error "~aはディレクトリです" filename))
  (unless (valid-path-p filename)
    (error "~a\nファイルまたはディレクトリが見つかりません" filename))
  (let ((buffer (get-file-buffer filename)))
    (if buffer
	(unwind-protect
	    (find-file-verify buffer filename nowarn encoding eol-code nomsg)
	  (when read-only
	    (save-excursion
	      (set-buffer buffer)
	      (setq buffer-read-only t))))
      (save-excursion
	(let ((last-buffer (selected-buffer))
	      (params nil)
	      file-exists)
	  (setq buffer (create-file-buffer filename))
	  (set-buffer buffer)
	  (when eol-code
	    (set-buffer-eol-code eol-code))
	  (or encoding *find-file-no-auto-encoding*
	      (setq encoding (find-file-auto-encoding filename)))
	  (let ((*expected-fileio-encoding*
		 (or encoding *expected-fileio-encoding*)))
	    (declare (special *expected-fileio-encoding*))
	    (setq file-exists (find-file-readin filename nowarn nomsg))
	    (when file-exists
	      (setq params (find-file-scan-params))))
	  (when read-only
	    (setq buffer-read-only t))
	  (when (or file-exists
		    (null (run-hook-with-args-until-success
			   '*find-file-file-not-found-hook* nil)))
	    (find-file-process-params params last-buffer)
	    (run-hooks '*find-file-hooks*)))))
    buffer))

(defun find-file (filename &optional encoding nomsg)
  (interactive "lFind file: \n0zEncoding: " :title0 "Find file")
  (long-operation
    (if (listp filename)
	(dolist (file filename t)
	  (find-file file encoding nomsg))
      (set-buffer (find-file-internal filename nil encoding nil nomsg)))))

(defun find-other-file (filename &optional encoding nomsg)
  (interactive "FFind other file: \n0zEncoding: "
    :title0 "Find other file"
    :default0 (and *find-other-file-requires-file-name*
		   (get-buffer-file-name)))
  (when (kill-buffer (selected-buffer))
    (find-file filename encoding nomsg)))

(defun find-file-other-window (filename &optional encoding nomsg)
  (interactive "lFind file other window:  \n0zEncoding: "
    :title0 "Find file other window")
  (switch-to-buffer-other-window (save-excursion
				   (find-file filename encoding nomsg)
				   (selected-buffer))
				 t))

(defun find-file-read-only (filename &optional encoding nomsg)
  (interactive "lFind file read-only: \n0zEncoding: " :title0 "Find file read-only")
  (long-operation
    (cond ((listp filename)
	   (dolist (file filename t)
	     (find-file-read-only file encoding nomsg)))
	  (t
	   (set-buffer (find-file-internal filename nil encoding nil nomsg t))
	   (run-hooks '*find-file-read-only-hook*)))))

(defun read-file (filename &optional encoding nomsg)
  (interactive "fRead file:  \n0zEncoding: " :title0 "Read file")
  (let ((*expected-fileio-encoding* (or encoding *expected-fileio-encoding*)))
    (declare (special *expected-fileio-encoding*))
    (when buffer-read-only
      (error "書き込み禁止バッファです"))
    (setq filename (namestring filename))
    (when (special-file-p filename)
      (error "~aはデバイスファイルです" filename))
    (when (file-directory-p filename)
      (error "~aはディレクトリです" filename))
    (unless (and (valid-path-p filename)
		 (file-exist-p filename))
      (error "~a\nファイルまたはディレクトリが見つかりません" filename))
    (when (and (need-buffer-save-p (selected-buffer))
	       (not (no-or-yes-p "バッファが変更されています。変更内容を破棄しますか?")))
      (return-from read-file nil))
    (erase-buffer (selected-buffer))
    (or nomsg
	(message "Reading file..."))
    (multiple-value-bind (nlines noerror)
	(insert-file-contents filename t)
      (unless noerror
	(warn "~a\nメモリが足りなくてすべて読み込めませんでした" filename)
	(set-buffer-truncated-p t))
      (or nomsg
	  (message "~d行読み込みました" nlines))
      nlines)))

(defun insert-file (filename &optional encoding)
  (interactive "*fInsert file: \n0zEncoding: " :title0 "Insert file")
  (let ((*expected-fileio-encoding* (or encoding *expected-fileio-encoding*)))
    (declare (special *expected-fileio-encoding*))
    (setq filename (namestring filename))
    (when (special-file-p filename)
      (error "~aはデバイスファイルです" filename))
    (when (file-directory-p filename)
      (error "~aはディレクトリです" filename))
    (unless (and (valid-path-p filename)
		 (file-exist-p filename))
      (error "~a\nファイルまたはディレクトリが見つかりません" filename))
    (insert-file-contents filename)))

(defun save-some-buffers (&optional verbose (fn #'write-file))
  (interactive "p")
  (long-operation
    (let ((count 0))
      (save-excursion
	(dolist (buffer (buffer-list))
	  (when (need-buffer-save-p buffer)
	    (set-buffer buffer)
	    (if (not (file-visited-p))
		(progn
		  (refresh-screen)
		  (if (yes-no-or-cancel-p "~aをセーブしますか?" buffer)
		      (when (call-interactively fn)
			(setq count (1+ count)))
		    (not-modified)))
	      (when (or (not verbose)
			(progn
			  (refresh-screen)
			  (yes-no-or-cancel-p "~a~%をセーブしますか?"
					      (get-buffer-file-name))))
		(save-buffer)
		(setq count (1+ count)))))))
      (message "~d個のファイルをセーブしました" count))))

(defun save-buffers-kill-xyzzy (&optional verbose)
  (interactive "p")
  (save-some-buffers verbose)
  (kill-xyzzy))

(defun write-file (filename &optional nomsg append char-encoding eol-code)
  (interactive "FWrite file: "
    :title0 "Write File" :default0 *default-write-file-directory*)
  (setq filename (namestring filename))
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename append char-encoding eol-code)
    (unless nomsg
      (message "Wrote ~a" filename)))
  t)

(defun emacs-write-file (filename)
  (interactive "FWrite file: "
    :title0 "Write File" :default0 *default-write-file-directory*)
  (and (rename filename)
       (save-buffer)))

(defun append-file (filename &optional nomsg char-encoding eol-code)
  (interactive "FAppend file: " :title0 "Append File")
  (write-file filename t t char-encoding eol-code)
  (unless nomsg
    (message "Append to ~a" filename))
  t)

(defun new-file ()
  (interactive)
  (switch-to-buffer (create-new-buffer "*untitled*")))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers nil #'save-as-dialog))

(defun save-all-buffers-kill-xyzzy ()
  (interactive)
  (save-all-buffers)
  (kill-xyzzy))

(defun create-shortcut (src dest &rest rest)
  (apply #'*create-shortcut
	 src
	 (merge-pathnames (concat (file-namestring (string-right-trim "/\\" src))
				  ".lnk")
			  dest)
	 rest))

(defun create-shortcut-to-desktop (path &rest rest)
  (apply #'create-shortcut path
	 (get-special-folder-location :desktop)
	 rest))

(defun find-file-in-archive (arcname filename)
  (let (dir file)
    (unwind-protect
	(let* ((name (concat arcname ":" filename))
	       (bufname (if (string-match "[/\\]\\([^/\\]+\\)$" filename)
			    (subseq filename (match-beginning 1) (match-end 1))
			  filename))
	       (buffer (get-alternate-file-buffer name)))
	  (cond (buffer
		 (set-buffer buffer)
		 (message "Out buffer"))
		(t
		 (setq dir (make-temp-file-name nil nil nil t))
		 (setq file (merge-pathnames (substitute-string filename "^[/\\]" "") dir))
		 (extract-archive arcname dir filename)
		 (unless (file-exist-p file)
		   (error "~a~%なんか知らんけど解凍できませんでした" file))
		 (prog1
		     (find-file file)
		   (set-default-directory (directory-namestring arcname))
		   (rename-buffer bufname)
		   (set-buffer-file-name nil)
		   (set-buffer-alternate-file-name name)))))
      (when dir
	(directory dir
		   :absolute t :recursive t
		   :callback #'(lambda (f)
				 (if (file-directory-p f)
				     (delete-directory f :if-access-denied :force)
				   (delete-file f :if-access-denied :force))))
	(delete-directory dir :if-does-not-exist :skip :if-access-denied :force)))))

(define-key ctl-x-map #\C-f 'find-file)
(define-key ctl-x-map #\C-s 'save-buffer)
(define-key ctl-x-map #\s 'save-some-buffers)
(define-key ctl-x-map #\C-v 'find-other-file)
(define-key ctl-x-map #\C-r 'read-file)
(define-key ctl-x-map #\i 'insert-file)
(define-key ctl-x-map #\C-w 'write-file)
(define-key ctl-x-4-map #\f 'find-file-other-window)

