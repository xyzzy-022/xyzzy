;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "grep")

(in-package "editor")

(export '(*last-fgrep-pattern* *last-grep-regexp* grep fgrep
	  *grep-highlight-match* *grep-hook* *before-grep-hook*))

(defvar *last-fgrep-pattern* "")
(defvar *last-grep-regexp* "")
(defvar *grep-highlight-match* '(:bold t))
(defvar *grep-hook* nil)
(defvar *before-grep-hook* nil)

(defun fgrep (pattern &optional arg)
  (interactive "sfgrep: \np" :default0 *last-fgrep-pattern* :history0 'search)
  (setq *last-fgrep-pattern* pattern)
  (grep1 pattern arg))

(defun grep (regexp &optional arg)
  (interactive "sgrep: \np" :default0 *last-grep-regexp* :history0 'search)
  (setq *last-grep-regexp* regexp)
  (grep1 (compile-regexp regexp *case-fold-search*) arg))

(defun grep-scanner (pattern case-fold word-search)
  #'(lambda (&optional limit)
      (scan-buffer pattern
		   :case-fold case-fold
		   :left-bound word-search
		   :right-bound word-search
		   :limit limit)))

(defun grep-scan-file (file scanner)
  (let ((found nil))
    (while (funcall scanner)
      (setq found t)
      (let ((beg (progn (goto-bol) (point)))
	    (end (progn (goto-eol) (point))))
	(format t "~A:~D:~A~%" file (current-line-number) (buffer-substring beg end))
	(when (and *grep-highlight-match*
		   (buffer-stream-p *standard-output*))
	  (save-excursion
	    (set-buffer (buffer-stream-buffer *standard-output*))
	    (let ((p (- (buffer-stream-point *standard-output*) end 1)))
	      (apply #'set-text-attribute (+ (match-beginning 0) p) (+ (match-end 0) p)
		     'grep *grep-highlight-match*)))))
      (or (forward-char 1)
	  (return)))
    found))

(defun grep1 (pattern arg)
  (long-operation
    (and arg
	 (setq arg (selected-buffer)))
    (with-output-to-temp-buffer ("*compilation*")
      (let ((sbuffer (buffer-stream-buffer *standard-output*))
	    (scanner (grep-scanner pattern *case-fold-search* *word-search*)))
	(save-excursion
	  (set-buffer sbuffer)
	  (make-local-variable '*find-error-scanner*)
	  (setq *find-error-scanner* scanner)
	  (set-buffer-fold-width nil sbuffer)
	  (run-hook-with-args '*before-grep-hook* pattern)
	  (dolist (buffer (if arg (list arg) (buffer-list)))
	    (let ((bufname (buffer-name buffer)))
	      (unless (or (eq buffer sbuffer)
			  (string-match "^ " bufname))
		(message "Scanning (~A)" bufname)
		(set-buffer buffer)
		(save-excursion
		  (goto-char (point-min))
		  (grep-scan-file (concatenate 'string "<" bufname ">") scanner))))))))
    (run-hooks '*grep-hook*)
    (message "completed.")
    t))
