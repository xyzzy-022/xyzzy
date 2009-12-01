;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "java")

(in-package "editor")

(export '(java-mode *java-keyword-file* *java-mode-hook*
	  *java-mode-syntax-table* *java-mode-map*
	  *java-mode-abbrev-table* *java-indent-tabs-mode*
	  *java-comment-column*
	  java-indent-level java-continued-statement-offset
	  java-argdecl-indent java-brace-offset java-brace-imaginary-offset
	  java-label-offset java-comment-indent))

(defvar *java-mode-hook* nil)

(unless (boundp 'java-indent-level)
  (setq java-indent-level 2)
  (setq java-continued-statement-offset 2)
  (setq java-argdecl-indent 5)
  (setq java-brace-offset 0)
  (setq java-brace-imaginary-offset 0)
  (setq java-label-offset -2)
  (setq java-comment-indent 2))

(defvar *java-indent-tabs-mode* nil)
(defvar *java-comment-column* nil)

(defvar *java-keyword-hash-table* nil)
(defvar *java-keyword-file* "java")

(defvar *java-mode-syntax-table* nil)
(unless *java-mode-syntax-table*
  (setq *java-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))
      ((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *java-mode-syntax-table* c))))
  (set-syntax-option *java-mode-syntax-table*
		     *syntax-option-indent-java*)
  (set-syntax-string *java-mode-syntax-table* #\")
  (set-syntax-string *java-mode-syntax-table* #\')
  (set-syntax-escape *java-mode-syntax-table* #\\)
  (set-syntax-symbol *java-mode-syntax-table* #\_)
  (set-syntax-symbol *java-mode-syntax-table* #\#)
  (set-syntax-match *java-mode-syntax-table* #\( #\))
  (set-syntax-match *java-mode-syntax-table* #\{ #\})
  (set-syntax-match *java-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *java-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *java-mode-syntax-table* "*/")
  (set-syntax-start-c++-comment *java-mode-syntax-table* #\/)
  (set-syntax-end-c++-comment *java-mode-syntax-table* #\LFD))

(defvar *java-mode-map* nil)
(unless *java-mode-map*
  (setq *java-mode-map* (make-sparse-keymap))
  (define-key *java-mode-map* #\{ 'c-electric-insert)
  (define-key *java-mode-map* #\: 'c-electric-insert)
  (define-key *java-mode-map* #\} 'c-electric-close)
  (define-key *java-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *java-mode-map* #\TAB 'c-indent-line)
  (define-key *java-mode-map* #\C-M-q 'indent-sexp)
  (define-key *java-mode-map* #\RET 'c-newline-and-indent))

(defvar *java-mode-abbrev-table* nil)
(unless *java-mode-abbrev-table*
  (define-abbrev-table '*java-mode-abbrev-table*))

(autoload 'java-build-summary-of-functions "javafns" nil)

(defun java-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Java")
  (setq buffer-mode 'java-mode)
  (use-syntax-table *java-mode-syntax-table*)
  (use-keymap *java-mode-map*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'c-indent-line)
  (make-local-variable 'c-comment-indent-variable)
  (setq c-comment-indent-variable 'java-comment-indent)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *java-indent-tabs-mode*)
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'c-tags-find-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'c-tags-find-point)
  (make-local-variable 'tags-class-method-separator)
  (setq tags-class-method-separator "#")
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'java-build-summary-of-functions)
  (and *java-keyword-file*
       (null *java-keyword-hash-table*)
       (setq *java-keyword-hash-table*
	     (load-keyword-file *java-keyword-file*)))
  (when *java-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *java-keyword-hash-table*))
  (setq *local-abbrev-table* *java-mode-abbrev-table*)
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "/\\(\\*+\\|/\\)[ \t]*")
  (setq comment-indent-function 'c-comment-indent)
  (when *java-comment-column*
    (setq comment-column *java-comment-column*))
  (run-hooks '*java-mode-hook*))
