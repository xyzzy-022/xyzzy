;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "LaTeX")

(in-package "editor")

(export '(*LaTeX-mode-hook* LaTeX-mode latex-mode
	  LaTeX-self-insert *LaTeX-ime-control* *LaTeX-keyword-file*
	  *LaTeX-keywords* LaTeX-complete-keyword
	  *LaTeX-popup-completion-list*))

(defvar *LaTeX-mode-hook* nil)
(defvar *LaTeX-popup-completion-list* nil)

(defvar *LaTeX-keyword-hash-table* nil)
(defvar *LaTeX-keyword-file* "TeX")

(defvar *LaTeX-ime-control* t)

(defvar *LaTeX-keywords* nil)

(defvar *LaTeX-mode-syntax-table* nil)
(unless *LaTeX-mode-syntax-table*
  (setq *LaTeX-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alpha-char-p c)
	(set-syntax-punctuation *LaTeX-mode-syntax-table* c))))
  (set-syntax-math *LaTeX-mode-syntax-table* #\$)
  (set-syntax-escape *LaTeX-mode-syntax-table* #\\)
  (set-syntax-symbol *LaTeX-mode-syntax-table* #\*)
  (set-syntax-symbol *LaTeX-mode-syntax-table* #\@)
  (set-syntax-symbol *LaTeX-mode-syntax-table* #\')
  (set-syntax-tag *LaTeX-mode-syntax-table* #\{ #\})
  (set-syntax-match *LaTeX-mode-syntax-table* #\[ #\])
  (set-syntax-start-comment *LaTeX-mode-syntax-table* #\% t)
  (set-syntax-end-comment *LaTeX-mode-syntax-table* #\LFD t t))

(defvar *LaTeX-mode-map* nil)
(unless *LaTeX-mode-map*
  (setq *LaTeX-mode-map* (make-sparse-keymap))
  (define-key *LaTeX-mode-map* '(#\ESC #\TAB) 'LaTeX-complete-keyword)
  (define-key *LaTeX-mode-map* #\\ 'LaTeX-self-insert)
  (define-key *LaTeX-mode-map* #\$ 'LaTeX-self-insert))

(defvar *LaTeX-mode-abbrev-table* nil)
(unless *LaTeX-mode-abbrev-table*
  (define-abbrev-table '*LaTeX-mode-abbrev-table*))

(defun LaTeX-self-insert (&optional (arg 1))
  (interactive "*p")
  (when *LaTeX-ime-control*
    (toggle-ime nil))
  (if (interactive-p)
      (call-interactively 'self-insert-command)
    (self-insert-command *last-command-char* arg)))

(defun LaTeX-complete-keyword ()
  (interactive "*")
  (or *LaTeX-keywords*
      (setq *LaTeX-keywords* (make-list-from-keyword-table *LaTeX-keyword-hash-table*))
      (return-from LaTeX-complete-keyword nil))
  (let ((opoint (point)))
    (skip-syntax-spec-backward "w_")
    (let ((from (1- (point))))
      (when (prog1
		(looking-back "\\")
	      (goto-char opoint))
	(do-completion from opoint :list *LaTeX-keywords* nil nil
		       *LaTeX-popup-completion-list*)))))

(defun LaTeX-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "LaTeX")
  (setq buffer-mode 'LaTeX-mode)
  (use-syntax-table *LaTeX-mode-syntax-table*)
  (use-keymap *LaTeX-mode-map*)
  (and *LaTeX-keyword-file*
       (null *LaTeX-keyword-hash-table*)
       (setq *LaTeX-keyword-hash-table*
	     (load-keyword-file *LaTeX-keyword-file*)))
  (when *LaTeX-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *LaTeX-keyword-hash-table*))
  (setq *local-abbrev-table* *LaTeX-mode-abbrev-table*)
  (run-hooks '*LaTeX-mode-hook*))

(setf (symbol-function 'latex-mode) #'LaTeX-mode)
