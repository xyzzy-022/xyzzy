;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(require "glob")

(provide "grepd")

(in-package "editor")

(export '(grep-dialog *grep-directory-name-hook*
	  *grep-ignored-directories*
	  *grepd-hook* *before-grepd-hook*))

(defvar *last-grepd-regexp* "")
(defvar *regexp-search* nil)
(defvar *understand-escape-sequences* nil)
(defvar *grepd-hook* nil)
(defvar *before-grepd-hook* nil)

(define-history-variable *grep-directory-history* nil)
(define-history-variable *grep-file-history* nil)
(define-history-variable *grep-case-fold-search* *case-fold-search*)
(define-history-variable *grep-word-search* *word-search*)
(define-history-variable *grep-regexp-search* *regexp-search*)
(define-history-variable *grep-subdir* nil)
(define-history-variable *grep-name-only* nil)
(define-history-variable *grep-understand-escape-sequences* *understand-escape-sequences*)
(define-history-variable *grep-async* nil)

(defvar *grep-directory-name-hook* nil)

(defvar *grep-ignored-directories*
  '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))

(defun grep-not-ignored-directories-p (path)
  (not (and (file-directory-p path)
	    (member (pathname-name path) *grep-ignored-directories*
		    :test #'string-equal))))

(defun scan-files-1 (file pattern buffer scanner)
  (let ((file-buffer (get-file-buffer file)))
    (if file-buffer
	(set-buffer file-buffer)
      (progn
	(set-buffer buffer)
	(erase-buffer buffer)
	(ignore-errors (insert-file-contents file))))
    (save-excursion
      (goto-char (point-min))
      (message "scanning (~A)" file)
      (cond (*grep-name-only*
	     (when (funcall scanner)
	       (princ file)
	       (terpri)
	       t))
	    (t
	     (grep-scan-file file scanner))))))

(defun scan-files-setup (dir buffer temp scanner)
  (pop-to-buffer buffer t)
  (set-buffer temp)
  (set-default-directory dir)
  (set-buffer buffer)
  (erase-buffer buffer)
  (set-buffer-fold-width nil)
  (setq buffer-read-only nil)
  (setq need-not-save t)
  (setq kept-undo-information nil)
  (setq auto-save nil)
  (set-default-directory dir)
  (make-local-variable '*find-error-scanner*)
  (setq *find-error-scanner* scanner))

(defun scan-files-sync (pattern buffer temp scanner file dir)
  (unwind-protect
      (let ((dirlist (if (consp dir) (glob-expand-list dir) (glob-expand dir))))
	(unless dirlist
	  (error "~A~%ディレクトリが見つかりません" dir))
	(scan-files-setup (glob-common-path dirlist)
			  buffer temp scanner)
	(run-hook-with-args '*before-grepd-hook* pattern)
	(refresh-screen)
	(with-output-to-selected-buffer
	  (let ((last-tick (get-internal-real-time))
		(found nil)
		tick)
	    (glob-exec dirlist
		       :recursive *grep-subdir*
		       :file-only t :wild file
		       :test 'grep-not-ignored-directories-p
		       :callback
		       #'(lambda (f)
			   (when (scan-files-1 f pattern temp scanner)
			     (setq found t))
			   (when found
			     (setq tick (get-internal-real-time))
			     (when (> (- tick last-tick) 500)
			       (setq last-tick tick)
			       (set-buffer buffer)
			       (goto-char (buffer-stream-point *standard-output*))
			       (refresh-screen)
			       (setq found nil)))
			   (do-events))))))
    (delete-buffer temp)
    (set-buffer buffer)
    (goto-char (point-min)))
  (set-buffer-modified-p nil)
  (run-hooks '*grepd-hook*)
  (message "done"))

(or (boundp 'async-grep-mode)
    (setq-default async-grep-mode nil))
(or (boundp 'async-grep-status)
    (setq-default async-grep-status nil))
(or (boundp 'async-grep-timer-callback)
    (setq-default async-grep-timer-callback nil))
(pushnew '(async-grep-mode . async-grep-status)
	 *minor-mode-alist* :key #'car)

(defvar *async-grep-mode-map* nil)
(unless *async-grep-mode-map*
  (setq *async-grep-mode-map* (make-sparse-keymap))
  (define-key *async-grep-mode-map* #\C-g 'stop-async-grep))

(defun async-grep-mode ()
  (kill-all-local-variables)
  (setq buffer-mode 'async-grep-mode)
  (setq mode-name "Grep")
  (use-keymap *async-grep-mode-map*)
  (make-local-variable 'async-grep-timer-callback)
  (setq async-grep-timer-callback nil)
  (make-local-variable 'async-grep-temp-buffer)
  (setq async-grep-temp-buffer nil)
  (make-local-variable 'async-grep-last-update)
  (setq async-grep-last-update (get-internal-real-time))
  (make-local-variable 'async-grep-mode)
  (setq async-grep-mode nil)
  (make-local-variable 'async-grep-status))

(defun stop-async-grep ()
  (interactive)
  (when async-grep-timer-callback
    (set-buffer-modified-p nil)
    (setq async-grep-status "done")
    (update-mode-line (selected-buffer))
    (stop-timer async-grep-timer-callback)
    (setq async-grep-timer-callback nil)
    (when async-grep-temp-buffer
      (delete-buffer async-grep-temp-buffer)
      (setq async-grep-temp-buffer nil))
    (run-hooks '*grepd-hook*)
    (message "done"))
  t)

(defun grep-async-scanner (pattern buffer temp scanner glob)
  #'(lambda ()
      (let ((update (get-buffer-window buffer)))
	(with-set-buffer
	  (save-excursion
	    (set-buffer buffer)
	    (if (null async-grep-timer-callback)
		(setq update nil)
	      (let ((file (funcall glob))
		    (timer async-grep-timer-callback)
		    (last-tick async-grep-last-update))
		(cond ((stringp file)
		       (with-output-to-buffer (buffer (point-max))
			 (cond ((not (scan-files-1 file pattern temp scanner))
				(setq update nil))
			       (update
				(let ((tick (get-internal-real-time)))
				  (cond ((> (- tick last-tick) 500)
					 (set-buffer buffer)
					 (setq async-grep-last-update tick))
					(t
					 (setq update nil))))))
			 (start-timer 0 timer t)))
		      (file
		       (setq update nil)
		       (start-timer 0 timer t))
		      (t
		       (stop-async-grep)))))))
	(and update (refresh-screen)))))

(defun scan-files-async (pattern buffer temp scanner file dir)
  (let* ((dirlist (if (consp dir) (glob-expand-list dir) (glob-expand dir)))
	 (commonl (glob-common-length dirlist)))
    (unless dirlist
      (error "~A~%ディレクトリが見つかりません" dir))
    (scan-files-setup (subseq (car dirlist) 0 commonl)
		      buffer temp scanner)
    (async-grep-mode)
    (run-hook-with-args '*before-grepd-hook*)
    (setq async-grep-temp-buffer temp)
    (setq async-grep-timer-callback
	  (grep-async-scanner pattern buffer temp scanner
			      (glob-enumerator (mapcar #'(lambda (x)
							   (subseq x commonl))
						       dirlist)
					       file *grep-subdir* t 'grep-not-ignored-directories-p)))
    (start-timer 0 async-grep-timer-callback t)
    (setq async-grep-mode t)
    (setq async-grep-status "running")))

(defun scan-files (pattern file dir &optional async)
  (long-operation
    (let ((buffer (get-buffer-create "*grep*")))
      (when (save-excursion
	      (set-buffer buffer)
	      async-grep-timer-callback)
	(if (yes-or-no-p "grepはすでに動作中です。死なす?")
	    (save-excursion
	      (set-buffer buffer)
	      (stop-async-grep))
	  (quit)))
      (when *grep-regexp-search*
	(setq pattern (compile-regexp pattern *grep-case-fold-search*)))
      (let ((temp (create-new-buffer "*grep temp*"))
	    (scanner (grep-scanner pattern *grep-case-fold-search*
				   *grep-word-search*)))
	(if async
	    (scan-files-async pattern buffer temp scanner file dir)
	  (scan-files-sync pattern buffer temp scanner file dir))))))

(defun grep-dialog-1 (template dirlist)
  (interactive)
  (let ((pattern (selection-start-end (start end)
		   (buffer-substring start end)))
	(no-dirs (if dirlist
		     (dolist (d dirlist ':disable)
		       (if (file-directory-p d)
			   (return))))))
    (multiple-value-bind (result data)
	(dialog-box template
		    (list (cons 'pat pattern)
			  (cons 'pat *minibuffer-search-string-history*)
			  (cons 'file (or (car *grep-file-history*) "*"))
			  (cons 'file *grep-file-history*)
			  (cons 'dir
				(if dirlist
				    nil
				  (let ((dir (and *grep-directory-name-hook*
						  (funcall *grep-directory-name-hook*))))
				    (if (stringp dir)
					dir
				      (default-directory)))))
			  (cons 'dir *grep-directory-history*)
			  (cons 'case-fold (cfs2dialog *grep-case-fold-search*))
			  (cons 'word *grep-word-search*)
			  (cons 'regexp *grep-regexp-search*)
			  (cons 'escseq *grep-understand-escape-sequences*)
			  (cons 'subdir *grep-subdir*)
			  (cons 'async *grep-async*)
			  (cons 'name *grep-name-only*))
		    `((file :disable ,no-dirs)
		      (file-static :disable ,no-dirs)
		      (subdir :disable ,no-dirs)
		      (word :disable (regexp))
		      (pat :non-null "検索文字列を入力して" :enable (IDOK))
		      (ref :related dir :directory-name-dialog (:title "参照"))))
      (when result
	(let ((pattern (cdr (assoc 'pat data)))
	      (file (or (cdr (assoc 'file data)) "*"))
	      (dir (or dirlist (namestring (or (cdr (assoc 'dir data))
					       (default-directory))))))
	  (when pattern
	    (si:*activate-toplevel)
	    (add-history pattern '*minibuffer-search-string-history*)
	    (or no-dirs (add-history file '*grep-file-history*))
	    (or dirlist (add-history dir '*grep-directory-history*))
	    (setq *grep-case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	    (setq *grep-word-search* (cdr (assoc 'word data)))
	    (setq *grep-regexp-search* (cdr (assoc 'regexp data)))
	    (setq *grep-understand-escape-sequences* (cdr (assoc 'escseq data)))
	    (when *grep-understand-escape-sequences*
	      (setq pattern (decode-escape-sequence pattern *grep-regexp-search*)))
	    (or no-dirs (setq *grep-subdir* (cdr (assoc 'subdir data))))
	    (setq *grep-async* (cdr (assoc 'async data)))
	    (setq *grep-name-only* (cdr (assoc 'name data)))
	    (scan-files pattern (split-string file #\; t " ")
			(or dirlist dir) *grep-async*)
	    t))))))

(defun grep-dialog ()
  (interactive)
  (grep-dialog-1 '(dialog 0 0 271 157
		   (:caption "Grep")
		   (:font 9 "MS UI Gothic")
		   (:control
		    (:static nil "パターン(&P):" #x50020000 7 10 42 8)
		    (:combobox pat nil #x50210042 51 8 157 96)
		    (:static nil "ファイル名(&F):" #x50020000 7 27 42 8)
		    (:combobox file nil #x50210042 51 25 157 96)
		    (:static nil "ディレクトリ(&D):" #x50020000 7 45 42 8)
		    (:combobox dir nil #x50210042 51 42 157 96)
		    (:button case-fold "大文字小文字を区別する(&C)" #x50010006 51 61 105 10)
		    (:button word "単語単位で検索する(&W)" #x50010003 51 74 92 10)
		    (:button regexp "正規表現(&E)" #x50010003 51 87 58 10)
		    (:button escseq "エスケープシーケンスを理解しろ(&Y)" #x50010003 51 100 106 10)
		    (:button subdir "ついでにサブディレクトリも(&U)" #x50010003 51 113 95 10)
		    (:button async "非同期でgrep(&A)" #x50010003 51 126 65 10)
		    (:button name "ファイル名だけ出力(&O)" #x50010003 51 139 87 10)
		    (:button IDOK "検索(&S)" #x50010001 214 7 50 14)
		    (:button IDCANCEL "キャンセル" #x50010000 214 24 50 14)
		    (:button ref "参照(&R)..." #x50010000 214 41 50 14)))
		 nil))
