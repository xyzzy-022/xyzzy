;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "fill")

(in-package "editor")

(export '(fill-column *auto-fill-hook fill-prefix auto-fill-hook
	  auto-fill-mode set-fill-column set-fill-prefix
	  fill-region-as-paragraph fill-paragraph fill-region
	  fill-region-hook))

(defvar-local fill-column 72)
(defvar-local fill-prefix nil)

(or (boundp 'auto-fill-hook)
    (setq auto-fill-hook nil))

(or (boundp 'fill-region-hook)
    (setq fill-region-hook nil))

(defconstant *ascii-chars-range* "\041-\176")
(defvar *ascii-chars-regexp* (compile-regexp "[\041-\176]"))

(setq-default *auto-fill-hook 'do-auto-fill)

(defun auto-fill-mode (&optional (arg nil sarg))
  (interactive "p")
  (setq auto-fill
	(cond ((null sarg)
	       (null auto-fill))
	      ((numberp arg)
	       (plusp arg))
	      (t arg))))

(defun do-auto-fill (c)
  (unless (plusp fill-column)
    (return-from do-auto-fill nil))
  (when (and (or (member c '(#\SPC #\TAB #\LFD))
		 (and (or (kanji-char-p c)
			  (kana-char-p c))
		      (not (listen *keyboard*))))
	     (>= (current-column) fill-column))
    (when (= (save-excursion
	       (kinsoku-goto-column fill-column *fill-kinsoku-mode*)
	       (when (eolp)
		 (return-from do-auto-fill nil))
	       (forward-char 1)
	       (prog1
		   (point)
		 (insert #\LFD)))
	     (point))
      (forward-char 1))
    (when mode-specific-indent-command
      (save-excursion
	(goto-bol)
	(funcall mode-specific-indent-command))
      (when (bolp)
	(skip-chars-forward " \t")))
    (when fill-prefix
      (if (bolp)
	  (insert fill-prefix)
	(save-excursion
	  (goto-bol)
	  (insert fill-prefix))))
    (run-hooks 'auto-fill-hook)))

(defun fill-region-as-paragraph (from to)
  (interactive "*r")
  (save-restriction
    (narrow-to-region from to)
    (unless (plusp fill-column)
      (goto-char (point-max))
      (return-from fill-region-as-paragraph nil))
    (goto-char (point-min))
    (while (progn
	     (goto-eol)
	     (forward-char 1)
	     (not (eobp)))
      (delete-region (if (and fill-prefix
			      (looking-for fill-prefix))
			 (+ (point) (length fill-prefix))
		       (point))
		     (progn
		       (forward-char -1)
		       (point)))
      (when (and (or (looking-at "[[({0-9A-Za-z\"']")
		     (extended-alphabet-char-p (following-char)))
		 (prog2
		     (forward-char -1)
		     (or (looking-at "[]})0-9A-Za-z,.?!\"']")
			 (extended-alphabet-char-p (following-char)))
		   (forward-char 1)))
	(insert #\SPC)))
    (goto-char (point-min))
    (loop
      (let ((opoint (point)))
	(kinsoku-goto-column fill-column *fill-kinsoku-mode*)
	(when (eolp)
	  (forward-char 1)
	  (return))
	(forward-char 1)
	(when (= opoint (point))
	  (forward-char 1))
	(insert #\LFD)
	(when fill-prefix
	  (insert fill-prefix))
	(run-hooks 'fill-region-hook))))
  t)

(defun fill-paragraph ()
  (interactive "*")
  (when (forward-paragraph 1)
    (forward-paragraph -1))
  (let ((sep (compile-regexp (if (and fill-prefix
				      (string/= fill-prefix ""))
				 (concat paragraph-separate
					 "\\|^"
					 (regexp-quote fill-prefix)
					 "[ \t　]*$")
			       paragraph-separate))))
    (while (and (looking-at sep)
		(forward-line 1)))
    (fill-region-as-paragraph (point)
			      (progn
				(or (forward-paragraph 1)
				    (goto-char (point-max)))
				(point)))))

(defun fill-region (from to)
  (interactive "*r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (not (eobp))
      (goto-bol)
      (fill-paragraph)
      (goto-eol))))

(defun set-fill-column (&optional (col (current-column)))
  (interactive "p")
  (unless (<= 4 col 30000)
    (error "フィルカラムは4以上30000以下です"))
  (setq fill-column col)
  (message "フィルカラムは~dです" fill-column))

(defun set-fill-prefix ()
  (interactive)
  (cond ((bolp)
	 (setq fill-prefix nil)
	 (message "fill prefixをクリアしました"))
	(t
	 (setq fill-prefix (buffer-substring (point)
					     (save-excursion
					       (goto-bol)
					       (point))))
	 (message "fill prefixは\"~a\"です" fill-prefix)))
  t)

(define-key esc-map #\q 'fill-paragraph)
(define-key ctl-x-map #\. 'set-fill-prefix)
(define-key ctl-x-map #\f 'set-fill-column)

