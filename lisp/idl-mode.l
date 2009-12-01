;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "idl-mode")

(in-package "editor")

(export '(*idl-mode-hook* *idl-mode-map* idl-mode))

(defvar *idl-mode-hook* nil)

(defvar *idl-mode-map* nil)
(unless *idl-mode-map*
  (setq *idl-mode-map* (make-sparse-keymap)))

(defvar *idl-keyword-hash-table* nil)
(defvar *idl-keyword-file* "idl")

(defun idl-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'idl-mode)
  (setq mode-name "idl")
  (use-keymap *idl-mode-map*)
  (use-syntax-table *c++-mode-syntax-table*)
  (and *idl-keyword-file*
       (null *idl-keyword-hash-table*)
       (setq *idl-keyword-hash-table*
	     (load-keyword-file *idl-keyword-file*)))
  (when *idl-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *idl-keyword-hash-table*))
  (run-hooks '*idl-mode-hook*))
