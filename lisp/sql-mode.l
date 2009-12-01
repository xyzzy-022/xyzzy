;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;
;;; written by Henmi

(provide "sql-mode")

(in-package "editor")

(export '(*sql-mode-hook* sql-mode *sql-keyword-file*))

(defvar *sql-mode-hook* nil)

(defvar *sql-keyword-hash-table* nil)
(defvar *sql-keyword-file* "SQL")

(defvar *sql-mode-syntax-table* nil)
(unless *sql-mode-syntax-table*
  (setq *sql-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *sql-mode-syntax-table* c))))
  (set-syntax-string *sql-mode-syntax-table* #\")
  (set-syntax-string *sql-mode-syntax-table* #\')
  (set-syntax-escape *sql-mode-syntax-table* #\\)
  (set-syntax-symbol *sql-mode-syntax-table* #\_)
  (set-syntax-symbol *sql-mode-syntax-table* #\#)
  (set-syntax-match *sql-mode-syntax-table* #\( #\))
  (set-syntax-match *sql-mode-syntax-table* #\{ #\})
  (set-syntax-match *sql-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *sql-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *sql-mode-syntax-table* "*/"))

(defvar *sql-mode-map* nil)
(unless *sql-mode-map*
  (setq *sql-mode-map* (make-sparse-keymap))
)

(defvar *sql-mode-abbrev-table* nil)
(unless *sql-mode-abbrev-table*
  (define-abbrev-table '*sql-mode-abbrev-table*))

(defun sql-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "SQL")
  (setq buffer-mode 'sql-mode)
  (use-syntax-table *sql-mode-syntax-table*)
  (use-keymap *sql-mode-map*)
  (and *sql-keyword-file*
       (null *sql-keyword-hash-table*)
       (setq *sql-keyword-hash-table*
	     (load-keyword-file *sql-keyword-file* t)))
  (when *sql-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *sql-keyword-hash-table*))
  (setq *local-abbrev-table* *sql-mode-abbrev-table*)
  (run-hooks '*sql-mode-hook*))
