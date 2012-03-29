;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "shell")

(in-package "editor")

(export '(*shell-mode-hook* *shell-prompt-regexp* *shell-mode-map*
	  *shell-ret* *shell-echo* shell shell-send-input shell-send-interrupt
	  shell-alternate-send-input))

(defvar *shell-mode-hook* nil)
(defvar *shell-prompt-regexp* "^[^#$%>?\n]*[#$%>?] *")
(defvar-local *shell-ret* "\n")
(defvar-local *shell-echo* nil)

(defvar *shell-mode-map* nil)
(unless *shell-mode-map*
  (setq *shell-mode-map* (make-sparse-keymap))
  (define-key *shell-mode-map* #\RET 'shell-send-input)
  (define-key *shell-mode-map* #\C-j 'shell-alternate-send-input)
  (define-key *shell-mode-map* '(#\C-c #\C-c) 'shell-send-interrupt))

(defun shell-mode ()
  (setq mode-name "Shell")
  (setq buffer-mode 'shell-mode)
  (use-keymap *shell-mode-map*)
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information nil)
  (set-buffer-fold-width 80)
  (cond ((string-matchp *eshell* "command.com$")
	 (setq *shell-ret* "\r" *shell-echo* t))
	((string-matchp *eshell* "cmd.exe$")
	 (setq *shell-ret* "\n" *shell-echo* t))
	(t
	 (setq *shell-ret* "\n" *shell-echo* nil)))
  (run-hooks '*shell-mode-hook*))

(defun shell ()
  (interactive)
  (set-buffer (get-buffer-create "*Shell*"))
  (let ((proc (buffer-process (selected-buffer))))
    (and proc (eq (process-status proc) ':run)
	 (return-from shell t)))
  (goto-char (point-max))
  (shell-mode)
  (make-process *eshell* :output (selected-buffer)))

;;;
;;; This code is loosely based on version 0.1 by Yutaka Oiwa <oiwa@is.s.u-tokyo.ac.jp>.
;;;

(defun shell-send-input ()
  (interactive)
  (let ((process (buffer-process (selected-buffer)))
	start end prompt)
    (when (and process
	       (eq (process-status process) :run))
      (cond ((>= (point) (marker-point (process-marker process)))
	     (setq start (marker-point (process-marker process)))
	     (setq end (progn (goto-eol) (point))))
	    ((save-excursion
	      (goto-bol)
	      (looking-at *shell-prompt-regexp*))
	     (setq start (match-end 0))
	     (setq end (progn (goto-eol) (point)))
	     (setq prompt (match-string 0)))
	    (t
	     (return-from shell-send-input nil)))
      (let ((cmd (buffer-substring start end)))
	(cond ((eobp)
	       (if *shell-echo*
		   (delete-region start end)
		 (insert "\n")))
	      (t
	       (goto-char (point-max))
	       (or (bolp)
		   (insert "\n"))
	       (and prompt (insert prompt))
	       (unless *shell-echo*
		 (insert cmd "\n"))))
	(set-marker (process-marker process))
	(process-send-string process (concatenate 'string cmd *shell-ret*))))))

(defun shell-alternate-send-input ()
  (interactive)
  (let ((*shell-ret* (if (equal *shell-ret* "\n") "\r" "\n")))
    (declare (special *shell-ret*))
    (shell-send-input)))

(setf (symbol-function 'shell-send-interrupt) #'kill-subprocess)
