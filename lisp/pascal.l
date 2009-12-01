;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "pascal")

(in-package "editor")

(export '(*pascal-mode-hook* pascal-mode *pascal-keyword-file*
	  pascal-electric-rbrace pascal-goto-matched-parenthesis
	  *pascal-indent-tabs-mode*))

(defvar *pascal-mode-hook* nil)

(defvar *pascal-indent-tabs-mode* nil)

(defvar *pascal-keyword-hash-table* nil)
(defvar *pascal-keyword-file* "Pascal")

(defvar *pascal-mode-syntax-table* nil)
(unless *pascal-mode-syntax-table*
  (setq *pascal-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *pascal-mode-syntax-table* c))))
  (set-syntax-string *pascal-mode-syntax-table* #\")
  (set-syntax-string *pascal-mode-syntax-table* #\')
  ;(set-syntax-escape *pascal-mode-syntax-table* #\\)
  (set-syntax-symbol *pascal-mode-syntax-table* #\_)
  (set-syntax-match *pascal-mode-syntax-table* #\( #\))
  (set-syntax-match *pascal-mode-syntax-table* #\[ #\])
  (set-syntax-start-comment *pascal-mode-syntax-table* #\{)
  (set-syntax-end-comment *pascal-mode-syntax-table* #\})
  (set-syntax-start-multi-comment *pascal-mode-syntax-table* "(*")
  (set-syntax-end-multi-comment *pascal-mode-syntax-table* "*)"))

(defvar *pascal-mode-map* nil)
(unless *pascal-mode-map*
  (setq *pascal-mode-map* (make-sparse-keymap))
  (define-key *pascal-mode-map* #\} 'pascal-electric-rbrace)
  (define-key *pascal-mode-map* '(#\ESC #\]) 'pascal-goto-matched-parenthesis))

(defvar *pascal-mode-abbrev-table* nil)
(unless *pascal-mode-abbrev-table*
  (define-abbrev-table '*pascal-mode-abbrev-table*))

(defun pascal-electric-rbrace (&optional (arg 1))
  (interactive "*p")
  (self-insert-command arg)
  (and (interactive-p)
       (not *executing-macro*)
       (save-excursion
	 (and (scan-buffer "{" :reverse t)
	      (show-matched-parenthesis))))
  t)

(defun pascal-goto-matched-parenthesis ()
  (interactive)
  (cond ((looking-at "[][()]")
	 (goto-matched-parenthesis))
	((looking-for "}")
	 (unless (scan-buffer "{" :reverse t)
	   (message "ˆê’v‚·‚éŠ‡ŒÊ‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")))
	((looking-for "{")
	 (unless (scan-buffer "}")
	   (message "ˆê’v‚·‚éŠ‡ŒÊ‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")))
	(t
	 (let ((pos (save-excursion
		      (let ((tail (progn
				    (skip-syntax-spec-forward "w_")
				    (point))))
			(skip-syntax-spec-backward "w_")
			(cond ((and (looking-for "begin" t)
				    (= (- tail (point)) 5))
			       (goto-char tail)
			       (if (forward-identifier "end" "begin" t)
				   (point)
				 (progn
				   (message "ˆê’v‚·‚éend‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")
				   nil)))
			      ((and (looking-for "end" t)
				    (= (- tail (point)) 3))
			       (forward-char -1)
			       (if (backward-identifier "begin" "end" t)
				   (point)
				 (progn
				   (message "ˆê’v‚·‚ébegin‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")
				   nil))))))))
	   (when pos
	     (goto-char pos))))))

(defun pascal-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Pascal")
  (setq buffer-mode 'pascal-mode)
  (use-syntax-table *pascal-mode-syntax-table*)
  (use-keymap *pascal-mode-map*)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *pascal-indent-tabs-mode*)
  (and *pascal-keyword-file*
       (null *pascal-keyword-hash-table*)
       (setq *pascal-keyword-hash-table*
	     (load-keyword-file *pascal-keyword-file* t)))
  (when *pascal-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *pascal-keyword-hash-table*))
  (setq *local-abbrev-table* *pascal-mode-abbrev-table*)
  (run-hooks '*pascal-mode-hook*))
