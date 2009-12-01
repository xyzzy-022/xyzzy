;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "gresreg")

(in-package "editor")

(export '(*query-gresreg-map* gresreg re-gresreg query-gresreg
	  query-gresreg-regexp))

(defvar *last-gresreg-string* "")
(defvar *last-gresreg-replace-string* "")
(defvar *last-gresreg-regexp* "")
(defvar *last-gresreg-replace-regexp* "")

(defvar *query-gresreg-map* nil)
(unless *query-gresreg-map*
  (setq *query-gresreg-map* (copy-keymap *query-replace-map*))
  (define-key *query-gresreg-map* #\C-g '(throw . quit)))

(defun gresreg (pattern replacement)
  (interactive "sfgresreg: \nswith: "
    :default0 *last-gresreg-string* :history0 'search
    :default1 *last-gresreg-replace-string* :history1 'search)
  (setq *last-gresreg-string* pattern)
  (setq *last-gresreg-replace-string* replacement)
  (gresreg1 pattern replacement nil nil))

(defun re-gresreg (pattern replacement)
  (interactive "sgresreg: \nswith: "
    :default0 *last-gresreg-regexp* :history0 'search
    :default1 *last-gresreg-replace-regexp* :history1 'search)
  (setq *last-gresreg-regexp* pattern)
  (setq *last-gresreg-replace-regexp* replacement)
  (gresreg1 pattern replacement nil t))

(defun query-gresreg (pattern replacement)
  (interactive "squery fgresreg: \nswith: "
    :default0 *last-gresreg-string* :history0 'search
    :default1 *last-gresreg-replace-string* :history1 'search)
  (setq *last-gresreg-string* pattern)
  (setq *last-gresreg-replace-string* replacement)
  (gresreg1 pattern replacement t nil))

(defun query-gresreg-regexp (pattern replacement)
  (interactive "squery gresreg: \nswith: "
    :default0 *last-gresreg-regexp* :history0 'search
    :default1 *last-gresreg-replace-regexp* :history1 'search)
  (setq *last-gresreg-regexp* pattern)
  (setq *last-gresreg-replace-regexp* replacement)
  (gresreg1 pattern replacement t t))

(defun gresreg1 (pattern replacement query regexpp)
  (save-excursion
    (let ((count 0)
	  (*query-replace-map* *query-gresreg-map*))
      (declare (special *query-replace-map*))
      (when (catch 'quit
	      (dolist (buffer (buffer-list) nil)
		(let ((bufname (buffer-name buffer)))
		  (unless (string-match "^ " bufname)
		    (set-buffer buffer)
		    (unless buffer-read-only
		      (save-excursion
			(goto-char (point-min))
			(setq count (+ count
				       (perform-replace pattern replacement
							query regexpp nil t)))))))))
	(quit))
      (message "~DŒÂ‚Ì•¶Žš—ñ‚ð’uŠ·‚µ‚Ü‚µ‚½" count))))

