;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "viewmode")

(in-package "editor")

(export '(*view-mode-hook* *view-mode-map* view-mode))

(defvar *view-mode-hook* nil)

(defvar *view-mode-map* nil)
(unless *view-mode-map*
  (setq *view-mode-map* (make-sparse-keymap))
  (define-key *view-mode-map* #\SPC 'next-page)
  (define-key *view-mode-map* #\C-h 'previous-page))

(defun view-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'view-mode)
  (setq mode-name "View")
  (use-keymap *view-mode-map*)
  (use-syntax-table *text-mode-syntax-table*)
  (setq buffer-read-only t)
  (run-hooks '*view-mode-hook*))
