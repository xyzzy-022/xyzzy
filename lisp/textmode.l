;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "textmode")

(in-package "editor")

(export '(*text-mode-hook* *text-mode-map* *tab-stop-list*
	  tab-to-tab-stop center-region center-line center-paragraph
	  text-mode *text-mode-syntax-table*))

(defvar *text-mode-hook* nil)

(defvar *tab-stop-list* '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120 128))

(defvar *text-mode-map* nil)
(unless *text-mode-map*
  (setq *text-mode-map* (make-sparse-keymap))
  ;(define-key *text-mode-map* #\TAB 'tab-to-tab-stop)
  (define-key *text-mode-map* #\M-S 'center-paragraph)
  ;(define-key *text-mode-map* #\M-s 'center-line)
)

(defvar *text-mode-syntax-table* nil)
(unless *text-mode-syntax-table*
  (setq *text-mode-syntax-table* (make-syntax-table))
  (set-syntax-symbol *text-mode-syntax-table* #\&)
  (set-syntax-symbol *text-mode-syntax-table* #\*)
  (set-syntax-symbol *text-mode-syntax-table* #\+)
  (set-syntax-symbol *text-mode-syntax-table* #\-)
  (set-syntax-symbol *text-mode-syntax-table* #\/)
  (set-syntax-symbol *text-mode-syntax-table* #\<)
  (set-syntax-symbol *text-mode-syntax-table* #\=)
  (set-syntax-symbol *text-mode-syntax-table* #\>)
  (set-syntax-symbol *text-mode-syntax-table* #\_)
  (set-syntax-symbol *text-mode-syntax-table* #\|))

(defvar *text-mode-abbrev-table* nil)
(unless *text-mode-abbrev-table*
  (define-abbrev-table '*text-mode-abbrev-table*))

(defun tab-to-tab-stop ()
  (interactive "*")
  (let ((tab (member (current-column) *tab-stop-list* :test #'<)))
    (if tab
	(indent-to (car tab))
      (let ((*last-command-char* #\TAB))
	(self-insert-command)))))

(defun center-region (from to)
  (interactive "*r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (not (eobp))
      (delete-region (point)
		     (progn
		       (skip-chars-forward " \t@")
		       (point)))
      (goto-eol)
      (delete-region (point)
		     (progn
		       (skip-chars-backward " \t@")
		       (point)))
      (let ((n (truncate (- fill-column (current-column)) 2)))
	(when (plusp n)
	  (goto-bol)
	  (or (eolp)
	      (indent-to n))))
      (unless (forward-line 1)
	(return))))
  t)

(defun center-line (&optional (arg 1))
  (interactive "*p")
  (center-region (point)
		 (progn
		   (forward-line arg)
		   (point))))

(defun center-paragraph ()
  (interactive "*")
  (center-region (progn
		   (forward-paragraph)
		   (point))
		 (progn
		   (backward-paragraph)
		   (point))))

(defun text-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'text-mode)
  (setq mode-name "Text")
  (use-keymap *text-mode-map*)
  (use-syntax-table *text-mode-syntax-table*)
  (setq *local-abbrev-table* *text-mode-abbrev-table*)
  (make-local-variable 'highlight-keyword)
  (setq highlight-keyword nil)
  (run-hooks '*text-mode-hook*))
