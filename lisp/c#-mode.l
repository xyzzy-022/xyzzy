;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "c#-mode")

(in-package "editor")

(export '(csharp-mode *csharp-comment-column* *csharp-mode-hook*
	  *csharp-keyword-file* *csharp-indent-tabs-mode*
	  csharp-indent-level csharp-continued-statement-offset
	  csharp-argdecl-indent csharp-brace-offset csharp-brace-imaginary-offset
	  csharp-label-offset csharp-preprocessor-offset csharp-comment-indent))

(defvar *csharp-mode-hook* nil)

(unless (boundp 'csharp-indent-level)
  (setq csharp-indent-level 2)
  (setq csharp-continued-statement-offset 2)
  (setq csharp-argdecl-indent 5)
  (setq csharp-brace-offset 0)
  (setq csharp-brace-imaginary-offset 0)
  (setq csharp-label-offset -2)
  (setq csharp-preprocessor-offset nil)
  (setq csharp-comment-indent 2))

(defvar *csharp-keyword-hash-table* nil)
(defvar *csharp-keyword-file* "C#")

(defvar *csharp-indent-tabs-mode* nil)
(defvar *csharp-comment-column* nil)

(defvar *csharp-mode-syntax-table* nil)
(unless *csharp-mode-syntax-table*
  (setq *csharp-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *csharp-mode-syntax-table* c))))
  (set-syntax-option *csharp-mode-syntax-table*
		     (+ *syntax-option-c-preprocessor*
			*syntax-option-indent-csharp*))
  (set-syntax-string *csharp-mode-syntax-table* #\")
  (set-syntax-string *csharp-mode-syntax-table* #\')
  (set-syntax-escape *csharp-mode-syntax-table* #\\)
  (set-syntax-symbol *csharp-mode-syntax-table* #\_)
  (set-syntax-symbol *csharp-mode-syntax-table* #\#)
  (set-syntax-symbol *csharp-mode-syntax-table* #\@)
  (set-syntax-match *csharp-mode-syntax-table* #\( #\))
  (set-syntax-match *csharp-mode-syntax-table* #\{ #\})
  (set-syntax-match *csharp-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *csharp-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *csharp-mode-syntax-table* "*/")
  (set-syntax-start-c++-comment *csharp-mode-syntax-table* #\/)
  (set-syntax-end-c++-comment *csharp-mode-syntax-table* #\LFD))

(defvar *csharp-mode-map* nil)
(unless *csharp-mode-map*
  (setq *csharp-mode-map* (make-sparse-keymap))
  (define-key *csharp-mode-map* #\{ 'c-electric-insert)
  (define-key *csharp-mode-map* #\# 'c-electric-insert)
  (define-key *csharp-mode-map* #\: 'c-electric-insert)
  (define-key *csharp-mode-map* #\} 'c-electric-close)
  (define-key *csharp-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *csharp-mode-map* #\TAB 'c-indent-line)
  (define-key *csharp-mode-map* #\C-M-q 'indent-sexp)
  (define-key *csharp-mode-map* #\RET 'c-newline-and-indent))

(defvar *csharp-mode-abbrev-table* nil)
(unless *csharp-mode-abbrev-table*
  (define-abbrev-table '*csharp-mode-abbrev-table*))

(autoload 'c-build-summary-of-functions "cfns" nil)

(defun csharp-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "C#")
  (setq buffer-mode 'csharp-mode)
  (use-syntax-table *csharp-mode-syntax-table*)
  (use-keymap *csharp-mode-map*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'c-indent-line)
  (make-local-variable 'c-comment-indent-variable)
  (setq c-comment-indent-variable 'csharp-comment-indent)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *csharp-indent-tabs-mode*)
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'c-tags-find-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'c-tags-find-point)
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'c-build-summary-of-functions)
  (and *csharp-keyword-file*
       (null *csharp-keyword-hash-table*)
       (setq *csharp-keyword-hash-table*
	     (load-keyword-file *csharp-keyword-file*)))
  (when *csharp-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *csharp-keyword-hash-table*))
  (setq *local-abbrev-table* *csharp-mode-abbrev-table*)
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "/\\(\\*+\\|/\\)[ \t]*")
  (setq comment-indent-function 'c-comment-indent)
  (when *csharp-comment-column*
    (setq comment-column *csharp-comment-column*))
  (run-hooks '*csharp-mode-hook*))

