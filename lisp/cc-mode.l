;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "cc-mode")

(in-package "editor")

(export '(c++-mode *default-c-mode* *c++-comment-column*
	  *c++-mode-hook* *c++-keyword-file* *c++-indent-tabs-mode*
	  c++-indent-level c++-continued-statement-offset
	  c++-argdecl-indent c++-brace-offset c++-brace-imaginary-offset
	  c++-label-offset c++-preprocessor-offset c++-comment-indent
	  detect-c-or-c++-mode))

(defvar *c++-mode-hook* nil)

(unless (boundp 'c++-indent-level)
  (setq c++-indent-level 2)
  (setq c++-continued-statement-offset 2)
  (setq c++-argdecl-indent 5)
  (setq c++-brace-offset 0)
  (setq c++-brace-imaginary-offset 0)
  (setq c++-label-offset -2)
  (setq c++-preprocessor-offset nil)
  (setq c++-comment-indent 2))

(defvar *c++-keyword-hash-table* nil)
(defvar *c++-keyword-file* "C++")

(defvar *c++-indent-tabs-mode* nil)
(defvar *c++-comment-column* nil)

(defvar *c++-mode-syntax-table* nil)
(unless *c++-mode-syntax-table*
  (setq *c++-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *c++-mode-syntax-table* c))))
  (set-syntax-option *c++-mode-syntax-table*
		     (+ *syntax-option-c-preprocessor*
			*syntax-option-indent-c++*
			*syntax-option-indent-c++/cli*))
  (set-syntax-string *c++-mode-syntax-table* #\")
  (set-syntax-string *c++-mode-syntax-table* #\')
  (set-syntax-escape *c++-mode-syntax-table* #\\)
  (set-syntax-symbol *c++-mode-syntax-table* #\_)
  (set-syntax-symbol *c++-mode-syntax-table* #\#)
  (set-syntax-match *c++-mode-syntax-table* #\( #\))
  (set-syntax-match *c++-mode-syntax-table* #\{ #\})
  (set-syntax-match *c++-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *c++-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *c++-mode-syntax-table* "*/")
  (set-syntax-start-c++-comment *c++-mode-syntax-table* #\/)
  (set-syntax-end-c++-comment *c++-mode-syntax-table* #\LFD))

(defvar *c++-mode-map* nil)
(unless *c++-mode-map*
  (setq *c++-mode-map* (make-sparse-keymap))
  (define-key *c++-mode-map* #\{ 'c-electric-insert)
  (define-key *c++-mode-map* #\: 'c-electric-insert)
  (define-key *c++-mode-map* #\# 'c-electric-insert)
  (define-key *c++-mode-map* #\} 'c-electric-close)
  (define-key *c++-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *c++-mode-map* #\TAB 'c-indent-line)
  (define-key *c++-mode-map* #\C-M-q 'indent-sexp)
  (define-key *c++-mode-map* #\RET 'c-newline-and-indent))

(defvar *c++-mode-abbrev-table* nil)
(unless *c++-mode-abbrev-table*
  (define-abbrev-table '*c++-mode-abbrev-table*))

(autoload 'c-build-summary-of-functions "cfns" nil)

(defun c++-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "C++")
  (setq buffer-mode 'c++-mode)
  (use-syntax-table *c++-mode-syntax-table*)
  (use-keymap *c++-mode-map*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'c-indent-line)
  (make-local-variable 'c-comment-indent-variable)
  (setq c-comment-indent-variable 'c++-comment-indent)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *c++-indent-tabs-mode*)
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'c-tags-find-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'c-tags-find-point)
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'c-build-summary-of-functions)
  (and *c++-keyword-file*
       (null *c++-keyword-hash-table*)
       (setq *c++-keyword-hash-table*
	     (load-keyword-file *c++-keyword-file*)))
  (when *c++-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *c++-keyword-hash-table*))
  (setq *local-abbrev-table* *c++-mode-abbrev-table*)
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "/\\(\\*+\\|/\\)[ \t]*")
  (setq comment-indent-function 'c-comment-indent)
  (when *c++-comment-column*
    (setq comment-column *c++-comment-column*))
  (run-hooks '*c++-mode-hook*))

(defvar *default-c-mode* 'c-mode)

(defun detect-c-or-c++-mode ()
  (interactive)
  (let ((mode (save-excursion
		(goto-char (point-min))
		(cond ((or (scan-buffer "//" :limit 3000)
			   (scan-buffer "\\(^\\|[^A-Za-z0-9_]\\)class\\($\\|[^A-Za-z0-9_]\\)"
					:regexp t :limit 3000)
			   (scan-buffer "\\(^\\|[^A-Za-z0-9_]\\)\\(public\\|private\\|protected\\)[ \t\n\f]*:"
					:regexp t :limit 3000))
		       'c++-mode)
		      ((bufferp *auto-mode-last-buffer*)
		       (set-buffer *auto-mode-last-buffer*)
		       (and (boundp 'buffer-mode)
			    (or (eq buffer-mode 'c-mode)
				(eq buffer-mode 'c++-mode))
			    buffer-mode))))))
    (funcall (or mode *default-c-mode*))))

(export 'decode-c-mode)
(setf (symbol-function 'decode-c-mode) #'detect-c-or-c++-mode)
