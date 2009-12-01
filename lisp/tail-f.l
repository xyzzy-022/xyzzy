;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "tail-f")

(in-package "editor")

(export '(tail-f *tail-f-mode-map* *tail-f-mode-hook*))

(defvar *tail-f-mode-map* (make-sparse-keymap))
(defvar *tail-f-mode-hook* nil)

(defun tail-f (filename)
  (interactive "ftail-f: " :title0 "tail-f")
  (set-buffer (tail-f-internal filename)))

(defun tail-f-internal (filename)
  (setq filename (namestring filename))
  (let ((buffer (tail-f-find-buffer filename)))
    (unless buffer
      (save-excursion
	(setq buffer (create-new-buffer "*tail-f*"))
	(set-buffer buffer)
	(tail-f-mode filename))
      (make-process (format nil "\"~a\" \"~a\""
			    (merge-pathnames "tail-f.exe" (etc-path))
			    filename)
		    :output buffer
		    :exec-directory (default-directory buffer)))
    buffer))

(defun tail-f-mode (filename)
  (kill-all-local-variables)
  (setq mode-name "tail-f")
  (setq buffer-mode 'tail-f-mode)
  (use-keymap *tail-f-mode-map*)
  (set-buffer-alternate-file-name filename)
  (set-default-directory (directory-namestring filename))
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information nil)
  (setq verify-visited-file-modtime nil)
  (make-local-variable '*before-delete-buffer-hook*)
  (add-hook '*before-delete-buffer-hook* 'tail-f-kill-buffer-hook)
  (run-hooks '*tail-f-mode-hook*))

(defun tail-f-find-buffer (filename)
  (enum-buffers #'(lambda (buffer)
		    (and (eq (buffer-local-value buffer 'buffer-mode)
			     'tail-f-mode)
			 (let ((name (get-buffer-alternate-file-name buffer)))
			   (and name (equalp filename name) buffer))))))

(defun tail-f-kill-buffer-hook (buffer)
  (let ((proc (buffer-process buffer)))
    (when (and proc (eq (process-status proc) :run))
      (signal-process proc)
      (dotimes (x 20)
	(or (eq (process-status proc) :run)
	    (return))
	(sit-for .1))))
  t)
