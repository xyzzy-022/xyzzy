;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "htmlmode")
(require "html-kwd")

(in-package "editor")

(export '(*html-mode-hook* *html-mode-map* html-mode *html-comment-column*))

(defvar *html-mode-hook* nil)
(defvar *html-comment-column* 0)

(defvar *html-mode-syntax-table* nil)
(unless *html-mode-syntax-table*
  (setq *html-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *html-mode-syntax-table* c))))
  (set-syntax-symbol-prefix *html-mode-syntax-table* #\&)
  (set-syntax-symbol *html-mode-syntax-table* #\-)
  (set-syntax-symbol *html-mode-syntax-table* #\:)
  (set-syntax-string *html-mode-syntax-table* #\")
  (set-syntax-tag *html-mode-syntax-table* #\< #\>)
  (set-syntax-symbol *html-mode-syntax-table* #\/))

(defvar *html-mode-map* nil)
(unless *html-mode-map*
  (setq *html-mode-map* (make-sparse-keymap)))

(defvar *html-mode-abbrev-table* nil)
(unless *html-mode-abbrev-table*
  (define-abbrev-table '*html-mode-abbrev-table*))

(defun html-comment-indent ()
  (save-excursion
    (cond ((looking-for "<!--")
	   (current-column))
	  (t
	   (goto-eol)
	   (skip-chars-backward " \t")
	   (if (bolp)
	       0
	     (max (current-column) comment-column))))))

(defun html-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'html-mode)
  (setq mode-name "HTML")
  (use-keymap *html-mode-map*)
  (use-syntax-table *html-mode-syntax-table*)
  (html-load-keyword-file nil)
  (setq *local-abbrev-table* *html-mode-abbrev-table*)
  (setq comment-start "<!-- ")
  (setq comment-end " -->")
  (setq comment-start-skip "<!--[ \t]*")
  (setq comment-indent-function 'html-comment-indent)
  (when *html-comment-column*
    (setq comment-column *html-comment-column*))
  (run-hooks '*html-mode-hook*))
