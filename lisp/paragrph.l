;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "paragrph")

(in-package "editor")

(export '(paragraph-ignore-fill-prefix fill-prefix paragraph-start paragraph-separate
	  forward-paragraph backward-paragraph kill-paragraph backward-kill-paragraph
	  mark-paragraph transpose-paragraphs selection-paragraph))

(or (boundp 'paragraph-ignore-fill-prefix)
    (setq paragraph-ignore-fill-prefix nil))

(or (boundp 'fill-prefix)
    (setq fill-prefix nil))

(defvar-local paragraph-start "^[ \t\f\nÅ@]")
(defvar-local paragraph-separate "^[ \t\fÅ@]*$")

(defun forward-paragraph (&optional (arg 1))
  (interactive "p")
  (let* ((fill-prefix-regexp (and fill-prefix
				  (string/= fill-prefix "")
				  (null paragraph-ignore-fill-prefix)
				  (regexp-quote fill-prefix)))
	 (paragraph-separate (compile-regexp (if fill-prefix-regexp
						 (concat paragraph-separate
							 "\\|^"
							 fill-prefix-regexp "[ \tÅ@]*$")
					       paragraph-separate))))
    (or (zerop arg)
	(goto-bol))
    (if (plusp arg)
	(if fill-prefix-regexp
	    (dotimes (x arg)
	      (while (looking-at paragraph-separate)
		(unless (forward-line 1)
		  (goto-eol)
		  (return-from forward-paragraph nil)))
	      (unless (forward-line 1)
		(goto-eol)
		(return-from forward-paragraph t))
	      (while (and (not (looking-at paragraph-separate))
			  (looking-for fill-prefix))
		(unless (forward-line)
		  (goto-eol)
		  (return-from forward-paragraph t))))
	  (dotimes (x arg)
	    (while (looking-at paragraph-separate)
	      (unless (forward-line 1)
		(goto-eol)
		(return-from forward-paragraph nil)))
	    (unless (forward-line 1)
	      (goto-eol)
	      (return-from forward-paragraph t))
	    (while (and (not (looking-at paragraph-separate))
			(not (looking-at paragraph-start)))
	      (unless (forward-line 1)
		(goto-eol)
		(return-from forward-paragraph t)))))
      (if fill-prefix-regexp
	  (dotimes (x (- arg))
	    (unless (looking-for fill-prefix)
	      (if (bolp)
		  (forward-line -1)
		(goto-bol)))
	    (while (looking-at paragraph-separate)
	      (unless (forward-line -1)
		(return-from forward-paragraph nil)))
	    (while (and (not (looking-at paragraph-separate))
			(looking-for fill-prefix))
	      (unless (forward-line -1)
		(return-from forward-paragraph t))))
	(dotimes (x (- arg))
	  (if (looking-at paragraph-start)
	      (forward-line -1)
	    (goto-bol))
	  (while (looking-at paragraph-separate)
	    (unless (forward-line -1)
	      (return-from forward-paragraph nil)))
	  (while (not (looking-at paragraph-start))
	    (unless (forward-line -1)
	      (return-from forward-paragraph t)))))))
  t)

(defun backward-paragraph (&optional (arg 1))
  (interactive "p")
  (forward-paragraph (- arg)))

(defun kill-paragraph (&optional (arg 1))
  (interactive "*p")
  (kill-region (point)
	       (progn
		 (forward-paragraph arg)
		 (point))))

(defun backward-kill-paragraph (&optional (arg 1))
  (interactive "*p")
  (kill-region (point)
	       (progn
		 (backward-paragraph arg)
		 (point))))

(defun mark-paragraph ()
  (interactive)
  (forward-paragraph)
  (set-mark-command)
  (backward-paragraph))

(defun transpose-paragraphs (&optional (arg 1))
  (interactive "*p")
  (transpose-region 'forward-paragraph arg))

(defun selection-paragraph ()
  (interactive)
  (forward-paragraph)
  (start-selection 2 t)
  (backward-paragraph))

(global-set-key #\M-h 'mark-paragraph)
(global-set-key #\M-} 'forward-paragraph)
(global-set-key #\M-{ 'backward-paragraph)
