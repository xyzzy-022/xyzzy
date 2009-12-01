;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "niftylog")

(in-package "editor")

(export '(view-nifty-log log-jump-to-article log-next-article
	  log-previous-article log-next-unread log-previous-unread
	  log-next-page log-previous-page log-read-article
	  log-unread-article log-read-thread log-unread-thread
	  log-mouse-left-press
	  *log-summary-mode-hook* *log-article-mode-hook*
	  log-summary-mode log-article-mode
	  ))

(defvar *log-menu*
  (let ((menu (copy-menu-items *app-menu* (create-menu))))
    (insert-popup-menu menu (get-menu-position menu 'search)
		       (define-popup-menu
			 (:item nil "メッセージを読んだことにする(&R)"
			  'log-read-article)
			 (:item nil "メッセージを読んでないことにする(&N)"
			  'log-unread-article)
			 (:item nil "スレッドを読んだことにする(&T)"
			  'log-read-thread)
			 (:item nil "スレッドを読んでないことにする(&H)"
			  'log-unread-thread)
			 (:item nil "ニュースグループを読んだことにする(&G)")
			 (:item nil "ニュースグループを読んでないことにする(&P)"))
		       "メッセージ(&M)")
    menu))

(defvar *nifty-forum-header-regexp-1*
  (compile-regexp
   "^- [A-Z0-9 ]*MES(\\([ 0-9][0-9]\\)):.+ [0-9][0-9]/[0-9][0-9]/[0-9][0-9] -$"))

(defvar *nifty-forum-header-regexp-2*
  (compile-regexp
   "^－\\(会議室\\([ 0-9][0-9]\\)  .+\\)発言数 :[0-9]+ 未読 :[0-9]+－"))

(defvar *nifty-article-header-regexp*
  (compile-regexp
   "[0-9][0-9][0-9]+/[0-9][0-9][0-9]+ +[A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9][0-9]  "))

(defvar *nifty-article-header-regexp-lead*
  "\\([A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9][0-9]  .+\\)\n(")

(defvar *nifty-article-header-regexp-trail*
  ")   [0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]")

(defvar *newsgroup-list* nil)

(defconstant *log-read-mark* "\n\037\022\037\n")

(defvar *nifty-log-summary-buffer* "*Nifty summary*")

(defmacro log-number (x) `(car ,x))
(defmacro log-marker-start (x) `(cadr ,x))
(defmacro log-marker-end (x) `(caddr ,x))
(defmacro log-filename (x) `(cadddr ,x))
(defmacro log-header (x) `(nth 4 ,x))
(defmacro log-id (x) `(nth 5 ,x))
(defmacro log-handle (x) `(nth 6 ,x))
(defmacro log-title (x) `(nth 7 ,x))
(defmacro log-response (x) `(nth 8 ,x))
(defmacro log-parent (x) `(nth 9 ,x))
(defmacro log-child (x) `(nth 10 ,x))
(defmacro log-read (x) `(nth 11 ,x))

(defun log-find-newsgroups ()
  (let ((newsgroups '()))
    (goto-char (point-min))
    (while (scan-buffer *nifty-forum-header-regexp-1*)
      (let ((point (point)))
	(push (list (buffer-substring (+ point 2)
				      (progn
					(goto-eol)
					(forward-char -11)
					(point)))
		    (match-string 1)
		    point)
	      newsgroups)))
    (goto-char (point-min))
    (while (scan-buffer *nifty-forum-header-regexp-2*)
      (let ((point (point)))
	(push (list (match-string 1)
		    (match-string 2)
		    point)
	      newsgroups))
      (forward-line 1))
    (sort newsgroups #'> :key #'caddr)))

(defun log-add-newsgroup (newsgroup)
  (car (or (member newsgroup *newsgroup-list* :test #'string= :key #'car)
	   (progn
	     (push (list newsgroup nil nil nil) *newsgroup-list*)
	     *newsgroup-list*))))

(defun log-find-articles ()
  (save-excursion
    (let ((filename (get-buffer-file-name (selected-buffer)))
	  (limit (point-max)))
      (dolist (newsgroup (log-find-newsgroups))
	(let* ((ng (log-add-newsgroup (car newsgroup)))
	       (ndigits (caddr ng))
	       (start (caddr newsgroup)))
	  (goto-char start)
	  (forward-line 1)
	  (when (scan-buffer *nifty-article-header-regexp*)
	    (unless ndigits
	      (setf (caddr ng) (setq ndigits (if (looking-at "[0-9][0-9][0-9]/") 3 5))))
	    (let ((digits (if (= ndigits 3)
			      "[0-9][0-9][0-9]"
			    "[0-9][0-9][0-9][0-9][0-9]")))
	      (let ((regex (compile-regexp (concat "^\\(" digits "\\)"
						   (buffer-substring (+ (point) ndigits)
								     (+ (point) (* ndigits 2) 1))
						   (if (= ndigits 3) "   " " ")
						   *nifty-article-header-regexp-lead*
						   (cadr newsgroup)
						   *nifty-article-header-regexp-trail*)))
		    (comment (compile-regexp (concat "  \\(" digits "\\)へのコメント")))
		    (last nil))
		(while (scan-buffer regex :limit limit)
		  (let* ((read-mark (when (looking-back *log-read-mark*)
				      (forward-char (- (length *log-read-mark*)))
				      t))
			 (cur (list (parse-integer (match-string 1))
				    (set-marker (make-marker))
				    nil
				    filename
				    nil
				    (buffer-substring (progn
							(goto-char (match-beginning 2))
							(point))
						      (progn
							(forward-char 8)
							(point)))
				    (buffer-substring (progn
							(forward-char 2)
							(point))
						      (progn
							(goto-column (+ (current-column) 16))
							(point)))
				    (buffer-substring (progn
							(forward-char 1)
							(point))
						      (progn
							(goto-eol)
							(point)))
				    (when (progn
					    (goto-char (match-end 0))
					    (looking-at comment))
				      (parse-integer (match-string 1)))
				    nil
				    nil
				    read-mark)))
		    (when last
		      (setf (log-marker-end last) (log-marker-start cur)))
		    (setq last cur)
		    (push cur (cadr ng))))
		(when last
		  (setf (log-marker-end last) (set-marker (make-marker) limit)))))
	    (setq limit start)))))))

(defun log-chain-threads ()
  (dolist (newsgroup *newsgroup-list*)
    (let ((summary (sort (cadr newsgroup) #'> :key #'car)))
      (do ((header summary (cdr header)))
	  ((null header))
	(let* ((h (car header))
	       (re (log-response h)))
	  (when re
	    (let ((parent (member re (cdr header) :test #'= :key #'car)))
	      (when (and parent
			 (null (member parent (log-child h) :test #'eq)))
		(setq parent (car parent))
		(setf (log-parent h) parent)
		(push h (log-child parent)))))))
      (setf (cadr newsgroup) (nreverse summary)))))

(defun log-print-summary (headers ndigits level)
  (dolist (header headers)
    (unless (and (zerop level)
		 (log-parent header))
      (setf (log-header header) (buffer-stream-point *standard-output*))
      (format t "~:[ ~;*~] ~V,'0D ~A ~A ~@[~V,1@T┗~]~A~%"
	      (log-read header)
	      ndigits (log-number header) (log-id header) (log-handle header)
	      (unless (zerop level) level) (log-title header))
      (when (log-child header)
	(log-print-summary (log-child header) ndigits (+ level 1))))))

(defun log-process-files (files)
  (long-operation
    (let ((buffer (switch-to-buffer *nifty-log-summary-buffer*)))
      (dolist (f files)
	(message "reading ~A..." f)
	(find-file f nil t)
	(widen)
	(log-article-mode)
	(setq log-summary-buffer buffer)
	(message "proccessing ~A..." f)
	(do-events)
	(log-find-articles)
	(do-events))
      (message "finding threads...")
      (log-chain-threads)
      (message "formatting summary...")
      (setq *newsgroup-list* (sort *newsgroup-list* #'string< :key #'car))
      (set-buffer buffer)
      (erase-buffer buffer)
      (log-summary-mode)
      (setq log-article-files files)
      (let ((buffer-read-only nil))
	(declare (special buffer-read-only))
	(with-output-to-selected-buffer
	  (dolist (newsgroup *newsgroup-list*)
	    (format t "~A~%" (car newsgroup))
	    (log-print-summary (cadr newsgroup) (caddr newsgroup) 0)
	    (setf (cadddr newsgroup) (buffer-stream-point *standard-output*)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (delete-other-windows)
      (log-next-unread))
    (message "done")
    t))

(defun view-nifty-log (files)
  (interactive "lログファイルの読み込み: " :title0 "ログファイルの読み込み")
  (setq *newsgroup-list* nil)
  (log-process-files (if (listp files) files (list files))))

(defun log-revert ()
  (view-nifty-log log-article-files))

(defun log-lookup-summary (point)
  (car (member point (cadar (member point *newsgroup-list* :test #'< :key #'cadddr))
	       :key #'(lambda (x) (log-header x)))))

(defun log-set-read-mark (read goto recenter)
  (goto-bol)
  (let ((m (log-lookup-summary (point))))
    (when m
      (cond (read
	     (unless (looking-for "*")
	       (let ((buffer-read-only nil))
		 (declare (special buffer-read-only))
		 (delete-char 1)
		 (insert #\*)
		 (forward-char -1))))
	    (t
	     (when (looking-for "*")
	       (let ((buffer-read-only nil))
		 (declare (special buffer-read-only))
		 (delete-char 1)
		 (insert #\SPC)
		 (forward-char -1)))))
      (setf (log-read m) read)
      (when goto
	(reverse-region (progn
			  (goto-eol)
			  (point))
			(progn
			  (goto-bol)
			  (point)))
	(when recenter
	  (recenter)))
      (pop-to-buffer (marker-buffer (log-marker-start m))
		     (- (truncate (* 2 (screen-height)) 3)))
      (widen)
      (goto-marker (log-marker-start m))
      (cond (read
	     (if (looking-for *log-read-mark*)
		 (forward-char (length *log-read-mark*))
	       (insert *log-read-mark*)))
	    (t
	     (when (looking-for *log-read-mark*)
	       (delete-char (length *log-read-mark*)))))
      (when goto
	(recenter 0))
      (narrow-to-region (point) (log-marker-end m))
      m)))

(defun log-jump-to-article ()
  (interactive)
  (log-set-read-mark t t (not (interactive-p))))

(defun log-next-article (&optional (arg 1))
  (interactive)
  (setq arg (if (minusp arg) -1 1))
  (let ((obuffer (selected-buffer)))
    (when (eq buffer-mode 'log-article-mode)
      (pop-to-buffer log-summary-buffer))
    (when (get-selection-type)
      (goto-char (selection-mark)))
    (cond ((while (forward-line arg)
	     (when (log-jump-to-article)
	       (return t)))
	   t)
	  (t
	   (unless (eq obuffer (selected-buffer))
	     (pop-to-buffer obuffer))
	   nil))))

(defun log-previous-article ()
  (interactive)
  (log-next-article -1))

(defun log-next-unread (&optional (arg 1))
  (interactive)
  (let ((obuffer (selected-buffer)))
    (when (eq buffer-mode 'log-article-mode)
      (pop-to-buffer log-summary-buffer))
    (goto-bol)
    (cond ((scan-buffer "^ " :regexp t :reverse (minusp arg) :no-dup t)
	   (log-jump-to-article))
	  (t
	   (goto-char (if (minusp arg) (point-min) (point-max)))
	   (unless (eq obuffer (selected-buffer))
	     (pop-to-buffer obuffer))
	   nil))))

(defun log-previous-unread ()
  (interactive)
  (log-next-unread -1))

(defun log-next-page (&optional (arg 1))
  (interactive)
  (if (pos-visible-in-window-p (if (minusp arg)
				   (point-min)
				 (point-max)))
      (log-next-unread arg)
    (next-page arg)))

(defun log-previous-page ()
  (interactive)
  (log-next-page -1))

(defun log-read-article (&optional (read t))
  (interactive)
  (save-excursion
    (save-window-excursion
      (when (eq buffer-mode 'log-article-mode)
	(pop-to-buffer log-summary-buffer))
      (when (get-selection-type)
	(goto-char (selection-mark)))
      (log-set-read-mark read nil t))))

(defun log-unread-article ()
  (interactive)
  (log-read-article nil))

(defun log-read-thread (&optional (read t))
  (interactive)
  (save-excursion
    (save-window-excursion
      (when (eq buffer-mode 'log-article-mode)
	(pop-to-buffer log-summary-buffer))
      (when (get-selection-type)
	(goto-char (selection-mark)))
      (while (progn
	       (goto-column 34)
	       (and (looking-at " +┗")
		    (forward-line -1))))
      (while (progn
	       (save-excursion
		 (log-set-read-mark read nil t))
	       (and (forward-line 1)
		    (progn
		      (goto-column 34)
		      (looking-at " +┗")))))
      t)))

(defun log-unread-thread ()
  (interactive)
  (log-read-thread nil))

(defun log-mouse-left-press ()
  (interactive)
  (let ((owindow (selected-window)))
    (set-window *last-mouse-window*)
    (cond ((eq buffer-mode 'log-summary-mode)
	   (goto-last-mouse-point)
	   (if (> *last-mouse-click-count* 1)
	       (log-set-read-mark t t nil))
	   t)
	  (t
	   (set-window owindow)
	   (mouse-left-press)))))

(defvar *log-summary-mode-hook* nil)
(defvar *log-article-mode-hook* nil)

(defun log-summary-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'log-summary-mode)
  (setq mode-name "Log summary")
  (use-keymap *log-summary-mode-map*)
  (set-buffer-fold-width nil)
  (setq buffer-read-only t)
  (make-local-variable 'kept-undo-information)
  (setq kept-undo-information nil)
  (make-local-variable 'need-not-save)
  (setq need-not-save t)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (make-local-variable 'log-article-files)
  (setq log-article-files nil)
  (use-local-menu *log-menu*)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function #'log-revert)
  (run-hooks '*log-summary-mode-hook*))

(defun log-article-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'log-article-mode)
  (setq mode-name "Log article")
  (setq hide-restricted-region t)
  (make-local-variable 'log-summary-buffer)
  (setq log-summary-buffer nil)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (use-keymap *log-article-mode-map*)
  (use-local-menu *log-menu*)
  (run-hooks '*log-article-mode-hook*))

(defvar *log-summary-mode-map* nil)

(unless *log-summary-mode-map*
  (setq *log-summary-mode-map* (make-sparse-keymap))
  (define-key *log-summary-mode-map* #\RET 'log-jump-to-article)
  (define-key *log-summary-mode-map* #\LBtnDown 'log-mouse-left-press)
  (define-key *log-summary-mode-map* #\LBtnMove 'mouse-nop)
  (define-key *log-summary-mode-map* #\LBtnUp 'mouse-nop)
)

(defvar *log-article-mode-map* nil)

(unless *log-article-mode-map*
  (setq *log-article-mode-map* (make-sparse-keymap))
  (define-key *log-article-mode-map* #\PageDown 'log-next-page)
  (define-key *log-article-mode-map* #\PageUp 'log-previous-page)
  (define-key *log-article-mode-map* #\C-PageDown 'log-next-article)
  (define-key *log-article-mode-map* #\C-PageUp 'log-previous-article)
  (define-key *log-article-mode-map* #\C-z 'log-previous-page)
  (define-key *log-article-mode-map* #\C-v 'log-next-page)
  (define-key *log-article-mode-map* '(#\C-c #\C-v) 'log-next-article)
  (define-key *log-article-mode-map* '(#\C-c #\C-z) 'log-previous-article)
  (define-key *log-article-mode-map* #\LBtnDown 'log-mouse-left-press)
)

(defun log-find-current-summary ()
  (let ((point (point))
	(buffer (selected-buffer)))
    (dolist (newsgroup *newsgroup-list*)
      (let ((header (member-if #'(lambda (x)
				   (let ((marker (log-marker-start x)))
				     (and (eq buffer (marker-buffer marker))
					  (< (marker-point marker)
					     point
					     (marker-point (log-marker-end x))))))
			       (cadr newsgroup))))
	(when header
	  (return (car header)))))))

(defun log-goto-current-summary ()
  (let ((header (log-find-current-summary)))
    (when header
      (pop-to-buffer log-summary-buffer)
      (goto-char (log-header header)))))

