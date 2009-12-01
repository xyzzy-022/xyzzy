;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "isearch")

(in-package "editor")

(export '(isearch-forward isearch-backward *isearch-smart-case*
	  *isearch-map* isearch-repeat-forward isearch-repeat-backward
	  isearch-abort isearch-search-or-exit isearch-exit
	  isearch-search-history-backward
	  isearch-search-history-forward
	  isearch-yank-word isearch-yank
	  isearch-paste-from-clipboard
	  isearch-quote-char isearch-self-insert
	  *isearch-scanner-hook*))

(defvar *isearch-map* nil)
(unless *isearch-map*
  (setq *isearch-map* (make-sparse-keymap))
  (define-key *isearch-map* #\C-s 'isearch-repeat-forward)
  (define-key *isearch-map* #\C-r 'isearch-repeat-backward)
  (define-key *isearch-map* #\C-h 'isearch-delete-char)
  (define-key *isearch-map* #\DEL 'isearch-delete-char)
  (define-key *isearch-map* #\Delete 'isearch-delete-char)
  (define-key *isearch-map* #\C-g 'isearch-abort)
  (define-key *isearch-map* #\ESC 'isearch-search-or-exit)
  (define-key *isearch-map* #\Home 'isearch-search-or-exit)
  (define-key *isearch-map* #\C-q 'isearch-quote-char)
  (define-key *isearch-map* #\Up 'isearch-search-history-backward)
  (define-key *isearch-map* #\Down 'isearch-search-history-forward)
  (define-key *isearch-map* #\C-w 'isearch-yank-word)
  (define-key *isearch-map* #\C-y 'isearch-yank)
  (define-key *isearch-map* #\S-Insert 'isearch-paste-from-clipboard)
  (define-key *isearch-map* #\C-v 'isearch-paste-or-exit)
  (define-key *isearch-map* #\RET 'isearch-exit)
  (define-key *isearch-map* #\LFD 'isearch-self-insert))

(defvar *isearch-history* nil)
(register-history-variable '*isearch-history*)

(defvar *isearch-smart-case* nil)

(defvar *isearch-current-string* nil)
(defvar *isearch-current-match-string* nil)
(defvar *isearch-command-char* nil)
(defvar *isearch-this-command* nil)
(defvar *isearch-last-command* nil)
(defvar *isearch-last-match* nil)
(defvar *isearch-direction* nil)
(defvar *isearch-wrapped* nil)
(defvar *isearch-status* nil)
(defvar *isearch-no-clear* nil)
(defvar *isearch-last-history* nil)
(defvar *isearch-history-index* nil)
(defvar *isearch-history-last-string* nil)
(defvar *isearch-regexp* nil)
(defvar *isearch-scanner-hook* nil)

(defun isearch-scanner (string no-dup)
  (let ((*isearch-regexp* nil)
	(pattern string))
    (when *isearch-scanner-hook*
      (setq pattern (funcall *isearch-scanner-hook* pattern)))
    (setq *isearch-current-string* string)
    (setq *isearch-last-match*
	  (if (and (scan-buffer pattern
				:reverse (not *isearch-direction*)
				:no-dup no-dup
				:regexp *isearch-regexp*
				:case-fold *case-fold-search*)
		   (setq *isearch-current-match-string* (match-string 0)))
	      t
	    (progn
	      (setq *isearch-current-match-string* nil)
	      (ding)
	      nil)))))

(defun isearch-push-status ()
  (push (list (point) *isearch-direction* *isearch-last-match*
	      *isearch-current-string* *isearch-wrapped*)
	*isearch-status*))

(defun isearch-pop-status ()
  (let ((x (pop *isearch-status*)))
    (goto-char (first x))
    (setq *isearch-direction* (second x)
	  *isearch-last-match* (third x)
	  *isearch-current-string* (fourth x)
	  *isearch-wrapped* (fifth x))))

(defun isearch-repeat-forward ()
  (when (string= *isearch-current-string* "")
    (setq *isearch-current-string* *last-search-string*))
  (let ((last-dir *isearch-direction*)
	(no-wrap t))
    (setq *isearch-direction* (eq *isearch-this-command* 'isearch-repeat-forward))
    (unless (string= *isearch-current-string* "")
      (isearch-push-status)
      (or *isearch-last-match*
	  (not (eq last-dir *isearch-direction*))
	  (progn
	    (setq *isearch-wrapped* t no-wrap nil)
	    (goto-char (if *isearch-direction* (point-min) (point-max)))))
      (isearch-scanner *isearch-current-string* no-wrap)
      (and *isearch-wrapped*
	   (not *isearch-last-match*)
	   (goto-char (caar *isearch-status*))))))

(setf (symbol-function 'isearch-repeat-backward) #'isearch-repeat-forward)

(defun isearch-delete-char ()
  (if (or (eq *isearch-last-command* 'isearch-search-history-forward)
	  (not *isearch-status*))
      (let ((l (length *isearch-current-string*)))
	(if (zerop l)
	    (ding)
	  (setq *isearch-current-string* (subseq *isearch-current-string* 0 (- l 1))))
	(setq *isearch-this-command* 'isearch-search-history-forward))
    (isearch-pop-status)))

(defun isearch-abort ()
  (let ((last *isearch-last-match*)
	(curstr *isearch-current-string*))
    (loop
      (unless *isearch-status*
	(unless (string= curstr "")
	  (setq *last-search-string* curstr))
	(quit))
      (and (not last)
	   *isearch-last-match*
	   (return))
      (isearch-pop-status))))

(defun isearch-search-or-exit ()
  (if *isearch-status*
      (isearch-exit)
    (progn
      (setq *isearch-no-clear* t)
      (clear-reverse-region)
      (throw 'isearch-exit (call-interactively
			    (if *isearch-direction*
				'search-forward
			      'search-backward))))))

(defun isearch-exit ()
  (unless (string= *isearch-current-string* "")
    (setq *last-search-string* *isearch-current-string*))
  (throw 'isearch-exit t))

(defun isearch-search-history-backward ()
  (if (eq *isearch-last-history* *isearch-current-string*)
      (setq *isearch-history-index*
	    (min (max 0 *isearch-history-index*)
		 (1- (length *isearch-history*))))
    (progn
      (setq *isearch-history-index* -1)
      (setq *isearch-history-last-string* *isearch-current-string*)))
  (let ((l (length *isearch-history-last-string*)))
    (loop
      (if (eq *isearch-this-command* 'isearch-search-history-backward)
	  (incf *isearch-history-index*)
	(decf *isearch-history-index*))
      (let ((s (unless (minusp *isearch-history-index*)
		 (nth *isearch-history-index* *isearch-history*))))
	(unless s
	  (ding)
	  (return))
	(let ((match (if *case-fold-search*
			 (string-not-equal s *isearch-history-last-string*)
		       (string/= s *isearch-history-last-string*))))
	  (when (or (null match)
		    (= match l))
	    (setq *isearch-last-history* s)
	    (setq *isearch-current-string* s)
	    (return))))))
  (setq *isearch-this-command* 'isearch-search-history-forward))

(setf (symbol-function 'isearch-search-history-forward)
      #'isearch-search-history-backward)

(defun isearch-yank-word ()
  (let* ((start (+ (point)
		   (if (and *isearch-last-match*
			    (not (eq *isearch-last-command*
				     'isearch-search-history-forward)))
		       (length *isearch-current-string*)
		     0)))
	 (end (save-excursion
		(goto-char start)
		(forward-word)
		(point)))
	 (next-word (buffer-substring start end)))
    (if (and *isearch-smart-case*
	     (eq *case-fold-search* :smart)
	     (not (string-match "[A-Z]" *isearch-current-string*)))
	(setq next-word (string-downcase next-word)))
    (isearch-push-status)
    (isearch-scanner (concat *isearch-current-string* next-word) nil)))

(defun isearch-yank ()
  (let ((x (current-kill 0)))
    (if (and *isearch-smart-case*
	     (eq *case-fold-search* :smart)
	     (not (string-match "[A-Z]" *isearch-current-string*)))
	(setq x (mapcar #'string-downcase x)))
    (isearch-push-status)
    (isearch-scanner (apply #'concat *isearch-current-string* x) nil)))

(defun isearch-paste-from-clipboard ()
  (let ((x (get-clipboard-data)))
    (when x
      (isearch-push-status)
      (isearch-scanner (concat *isearch-current-string* x) nil))))

(defun isearch-quote-char ()
  (let ((c (quote-char-read)))
    (isearch-push-status)
    (isearch-scanner (concat *isearch-current-string* (string c)) nil)))

(defun isearch-paste-or-exit ()
  (cond ((eq (lookup-key-command *isearch-command-char*) 'paste-from-clipboard)
	 (isearch-paste-from-clipboard))
	(t
	 (unread-char *isearch-command-char* *keyboard*)
	 (isearch-exit))))

(defun isearch-self-insert (&optional (ch *isearch-command-char*))
  (unless (or (eq *isearch-this-command* 'isearch-self-insert)
	      (graphic-char-p ch))
    (unread-char ch *keyboard*)
    (isearch-exit))
  (isearch-push-status)
  (isearch-scanner (concat *isearch-current-string* (string ch)) nil))

(defun isearch-forward (&optional reverse)
  (interactive)
  (let ((*isearch-current-string* "")
	(*isearch-current-match-string* nil)
	(*isearch-last-match* t)
	(*isearch-no-clear* nil)
	(*isearch-history-index* -1)
	(*isearch-last-history* nil)
	(*isearch-history-last-string* nil)
	(*isearch-this-command* nil)
	(*isearch-last-command* nil)
	(*isearch-wrapped* nil)
	(*isearch-status* nil)
	(*isearch-direction* (not reverse)))
    (setq *last-search-regexp-p* nil)
    (unwind-protect
	(catch 'isearch-exit
	  (loop
	    (reverse-region (point)
			    (+ (point)
			       (if (and *isearch-last-match*
					(not (eq *isearch-last-command*
						 'isearch-search-history-forward)))
				   (max (length *isearch-current-match-string*) 1)
				 1))
			    t)
	    (refresh-screen)
	    (minibuffer-prompt "~:[Failing ~;~]~:[~;Wrapped ~]I-search~:[ backward~;~]: ~a"
			       *isearch-last-match* *isearch-wrapped*
			       *isearch-direction* *isearch-current-string*)
	    (let ((*isearch-command-char* (read-char *keyboard*)))
	      (setq *isearch-this-command* (lookup-keymap *isearch-map*
							  *isearch-command-char* t))
	      (if *isearch-this-command*
		  (funcall *isearch-this-command*)
		(isearch-self-insert *isearch-command-char*))
	      (setq *isearch-last-command* *isearch-this-command*))))
      (unless *isearch-no-clear*
	(clear-reverse-region)
	(add-history *isearch-current-string* '*isearch-history*)))))

(defun isearch-backward ()
  (interactive)
  (isearch-forward t))

(global-set-key #\C-s 'isearch-forward)
(global-set-key #\C-r 'isearch-backward)
