;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "encdec")

(in-package "editor")

(export '(base64-decode-region base64-decode-region-to-file
	  uudecode-region uudecode-region-to-file
	  quoted-printable-decode-region
	  quoted-printable-decode-region-to-file
	  url-encoding-decode-region
	  binhex-decode-region binhex-decode-region-to-file
	  decode-mime-header decode-mime-header-region
	  decode-mime-field rfc822-detect-header))

(defun text-decode-region (decode from to)
  (or (< from to)
      (rotatef from to))
  (insert (with-output-to-string (s)
	    (with-input-from-buffer ((selected-buffer) from to)
	      (funcall decode nil s))
	    (delete-region from to))))

(defun text-decode-region-to-file (decode filename from to)
  (or (< from to)
      (rotatef from to))
  (with-open-file (s filename
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :encoding :binary)
    (with-input-from-buffer ((selected-buffer) from to)
      (funcall decode nil s))))

(defun base64-decode-region (from to)
  (interactive "*r")
  (text-decode-region #'si:base64-decode from to))

(defun base64-decode-region-to-file (filename from to)
  (interactive "Ffilename: \nr")
  (text-decode-region-to-file #'si:base64-decode filename from to))

(defun uudecode-region (from to)
  (interactive "*r")
  (text-decode-region #'si:uudecode from to))

(defun uudecode-region-to-file (filename from to)
  (interactive "Ffilename: \nr")
  (text-decode-region-to-file #'si:uudecode filename from to))

(defun quoted-printable-decode-region (from to)
  (interactive "*r")
  (text-decode-region #'si:quoted-printable-decode from to))

(defun quoted-printable-decode-region-to-file (filename from to)
  (interactive "Ffilename: \nr")
  (text-decode-region-to-file #'si:quoted-printable-decode filename from to))

(defun url-encoding-decode-region (from to)
  (interactive "*r")
  (text-decode-region #'si:www-url-decode from to))

(defun binhex-decode-region (from to)
  (interactive "*r")
  (text-decode-region #'si:binhex-decode from to))

(defun binhex-decode-region-to-file (filename from to)
  (interactive "Ffilename: \nr")
  (text-decode-region-to-file #'si:binhex-decode filename from to))

(defconstant *rfc822-field-regexp*
  (compile-regexp "[^\000-\040:\177-\377]+:"))

(defconstant *rfc822-LWSP-regexp*
  (compile-regexp "[\040\011]"))

(defmacro rfc822-field-p ()
  '(looking-at *rfc822-field-regexp*))

(defmacro rfc822-LWSP-char-p ()
  '(looking-at *rfc822-LWSP-regexp*))

(defconstant *rfc2047-encoded-word-regexp*
  (compile-regexp "=\\?\\([-a-z0-9_]+\\)\\(\\*[A-Za-z]+\\)?\\?\\([bq]\\)\\?" t))

(defvar *decode-mime-header-cleanup*
  (list '("\037\036" . "")
	'("(\036" . "(")
	'("\037)" . ")")
	'("\"\036" . "\"")
	'("\037\"" . "\"")
	(cons (compile-regexp "\037$") "")))

(defun rfc822-detect-header ()
  (save-excursion
    (goto-bol)
    (let ((match nil))
      (loop
	(cond ((rfc822-field-p)
	       (setq match (point)))
	      ((rfc822-LWSP-char-p))
	      (t
	       (return)))
	(unless (forward-line -1)
	  (return)))
      (when match
	(goto-char match)
	(values match
		(loop
		  (cond ((rfc822-field-p))
			((rfc822-LWSP-char-p))
			(t
			 (return (point))))
		  (unless (forward-line 1)
		    (return (point)))))))))

(defun decode-mime-field (beg end &optional no-unfolding)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((mod nil)
	  (white-chars (if no-unfolding " \t" " \t\n"))
	  charset
	  encoding
	  decoded)
      (while (scan-buffer *rfc2047-encoded-word-regexp* :tail t)
	(setq charset (gethash (match-string 1) *mime-charset-name-hash-table*))
	(when charset
	  (setq encoding (match-string 3))
	  (setq beg (match-beginning 0))
	  (setq end (match-end 0))
	  (when (scan-buffer "?=" :limit (save-excursion (goto-eol) (point)))
	    (insert "\036"
		    (prog1
			(convert-encoding-to-internal charset
						      (if (string-equal encoding "b")
							  (si:base64-decode
							   (buffer-substring end (point)))
							(si:quoted-printable-decode
							 (buffer-substring end (point)) nil t)))
		      (delete-region (progn
				       (forward-char 2)
				       (skip-chars-forward white-chars)
				       (point))
				     (progn
				       (goto-char beg)
				       (skip-chars-backward white-chars)
				       (point))))
		    "\037")
	    (setq mod t))))
      (when mod
	(mapc #'(lambda (x)
		  (goto-char (point-min))
		  (replace-buffer (car x) (cdr x)))
	      *decode-mime-header-cleanup*)
	(goto-char (point-min))
	(replace-buffer (if no-unfolding "[\036\037]" "[\036\037\n]") " " :regexp t))
      (goto-char (point-max)))))

(defun decode-mime-header-region (beg end)
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (loop
      (let ((beg (point)))
	(loop
	  (unless (forward-line 1)
	    (goto-eol)
	    (return))
	  (unless (rfc822-LWSP-char-p)
	    (forward-char -1)
	    (return)))
	(decode-mime-field beg (point))
	(unless (forward-char 1)
	  (return))))))

(defun decode-mime-header ()
  (interactive "*")
  (save-excursion
    (multiple-value-bind (beg end)
	(rfc822-detect-header)
      (when beg
	(decode-mime-header-region beg end)
	t))))
