;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "html-kwd")

(in-package "editor")

(export '(*html-default-doctype* *html-keyword-file* *html5-keyword-file* *html-doctypes*
	  *html-ignore-doctype-declaration*
	  html-find-doctype html-current-doctype html-load-keyword-file))

(defvar *html-default-doctype* "HTML4.01 Frameset")
(defvar *html-keyword-file* "HTML")
(defvar *html5-keyword-file* "HTML5")
(defvar *html-ignore-doctype-declaration* nil)

(defvar *html-doctypes*
  '(("HTML1.0"
     nil
     "HTML[ \t\r\n]*1\\.?0")
    ("HTML2.0"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//IETF//DTD[ \t\r\n]+HTML\\([ \t\r\n]+2\\.0.*\\)?//EN[\"']"
     "HTML[ \t\r\n]*2\\.?0")
    ("HTML2.x"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//IETF//DTD[ \t\r\n]+HTML[ \t\r\n]+i18n.*//EN[\"']"
     "i18n")
    ("HTML3.0"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//IETF//DTD[ \t\r\n]+HTML[ \t\r\n]+3\\.0.*//EN[\"']"
     "HTML[ \t\r\n]*3\\.?0")
    ("HTML3.2"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+3\\.2.*//EN[\"']"
     "HTML[ \t\r\n]*3\\.?2")
    ("HTML4.0 Strict"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.0//EN[\"']"
     "HTML[ \t\r\n]*4\\.?0\\([^1]\\|$\\)")
    ("HTML4.0 Transitional"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.0[ \t\r\n]+Transitional//EN[\"']"
     nil)
    ("HTML4.0 Frameset"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.0[ \t\r\n]+Frameset//EN[\"']"
     nil)
    ("HTML4.0 Mobile"
     "HTML[ \t\r\n]+SYSTEM[ \t\r\n]+[\"']html40-mobile.dtd[\"']"
     nil)
    ("HTML4.01 Strict"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.01//EN[\"']"
     "HTML[ \t\r\n]*4\\.?01")
    ("HTML4.01 Transitional"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.01[ \t\r\n]+Transitional//EN[\"']"
     nil)
    ("HTML4.01 Frameset"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+HTML[ \t\r\n]+4\\.01[ \t\r\n]+Frameset//EN[\"']"
     nil)
    ("XHTML1.0 Strict"
     "html[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+XHTML[ \t\r\n]+1\\.0\\([ \t\r\n]+Strict\\)?//EN[\"']"
     "XHTML")
    ("XHTML1.0 Transitional"
     "html[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+XHTML[ \t\r\n]+1\\.0[ \t\r\n]+Transitional//EN[\"']"
     nil)
    ("XHTML1.0 Frameset"
     "html[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+XHTML[ \t\r\n]+1\\.0[ \t\r\n]+Frameset//EN[\"']"
     nil)
    ("XHTML1.1"
     "html[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+XHTML[ \t\r\n]+1\\.1//EN[\"']"
     nil)
    ("XHTML Basic"
     "html[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+XHTML[ \t\r\n]+Basic[ \t\r\n]+1\\.0//EN[\"']"
     nil)
    ("ISO/IEC 15445"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']ISO/IEC[ \t\r\n]+15445:2000//DTD[ \t\r\n]+\\(HTML\\|HyperText[ \t\r\n]+Markup[ \t\r\n]+Language\\)//EN[\"']"
     "15445")
    ("ISO/IEC 15445 Preparation"
     "Pre-HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']ISO/IEC[ \t\r\n]+15445:2000//DTD[ \t\r\n]+\\(HTML\\|HyperText[ \t\r\n]+Markup[ \t\r\n]+Language\\)//EN[\"']"
     nil)
    ("Mozilla2.0"
     nil
     "\\(Netscape\\|Navigator\\|Mozilla\\)[^0-9]*2")
    ("Mozilla3.0"
     nil
     "\\(Netscape\\|Navigator\\|Mozilla\\)[^0-9]*3")
    ("Mozilla4.0"
     nil
     "\\(Netscape\\|Navigator\\|Communicator\\|Mozilla\\)[^0-9]*\\([^23]\\|$\\)")
    ("MSIE3.0beta"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//Microsoft//DTD[ \t\r\n]+Internet[ \t\r\n]+Explorer[ \t\r\n]+3\\.0[ \t\r\n]+HTML//EN[\"']"
     nil)
    ("MSIE3.0"
     nil
     "\\(Internet Explorer\\|MSIE\\)[^0-9]*3")
    ("MSIE4.0"
     nil
     "\\(Internet Explorer\\|MSIE\\)[^0-9]*4")
    ("MSIE5.0"
     nil
     "\\(Internet Explorer\\|MSIE\\)[^0-9]*5\\(\\.?0\\)?$")
    ("MSIE5.5"
     nil
     "\\(Internet Explorer\\|MSIE\\)[^0-9]*5\\.?5")
    ("WebExplorer1.1"
     "HTML[ \t\r\n]+PUBLIC[ \t\r\n]+[\"']-//W3C//DTD[ \t\r\n]+Compact[ \t\r\n]+HTML[ \t\r\n]+1.0[ \t\r\n]+Draft//EN[\"']"
     nil)
    ("Compact HTML"
     nil
     "Compact\\([ \t\r\n]+HTML\\)?")
    ("iMode1.0"
     nil
     "\\(DoCoMo\\|iMode\\)[^0-9]*\\(1\\|$\\)")
    ("iMode2.0"
     nil
     "\\(DoCoMo\\|iMode\\)[^0-9]*2")
    ("iMode3.0"
     nil
     "\\(DoCoMo\\|iMode\\)[^0-9]*3")
    ("J-SkyWeb"
     nil
     "J-?Sky\\(Web\\)?")
    ("J-SkyWeb Station"
     nil
     "J-?Sky\\(Web\\)?[ \t\r\n]*Station")
    ("doti1.0"
     nil
     "\\(doti\\)[^0-9]*\\(1\\|$\\)")
    ("HTML5.0"
     "html"
     nil)
    ))

(defvar *html-keyword-hash-list* nil)

(defun html-find-doctype ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\r\n")
    (when (looking-at "<\\?xml[^>]+>")
      (goto-char (match-end 0))
      (skip-chars-forward " \t\r\n"))
    (when (looking-at "<!DOCTYPE[ \t\r\n]+" t)
      (save-restriction
	(narrow-to-region (match-end 0)
			  (progn
			    (scan-buffer ">")
			    (point)))
	(goto-char (point-min))
	(dolist (x *html-doctypes*)
	  (when (and (cadr x)
		     (looking-at (cadr x) t))
	    (return-from html-find-doctype (car x))))
	(unless (= (point-min) (point-max))
	  (dolist (x *html-doctypes*)
	    (when (and (caddr x)
		       (progn
			 (goto-char (point-min))
			 (scan-buffer (caddr x) :case-fold t)))
	      (return-from html-find-doctype (car x)))))))
    nil))

(defun html-current-doctype ()
  (and (hash-table-p keyword-hash-table)
       (gethash 'keyword-condition keyword-hash-table)))

(defun html-keyword-file (doctype)
  (cond ((string-match "HTML5" doctype)
	 *html5-keyword-file*)
	(t
	 *html-keyword-file*)))

(defun html-load-keyword-file (doctype)
  (interactive
      (list (completing-read "doctype: "
			     (mapcar #'car *html-doctypes*)
			     :case-fold t
			     :default (html-find-doctype)
			     :must-match t)))
  (when *html-keyword-file*
    (or doctype
	(setq doctype (or (and (not *html-ignore-doctype-declaration*)
			       (html-find-doctype))
			  *html-default-doctype*)))
    (let* ((hash (cdr (assoc doctype *html-keyword-hash-list*
			     :test #'string=))))
      (unless hash
	(setq hash (load-keyword-file (html-keyword-file doctype)
				      (not (string-match "XHTML" doctype))
				      nil doctype))
	(when hash
	  (push (cons doctype hash) *html-keyword-hash-list*)))
      (when hash
	(setq html-highlight-mode t)
	(make-local-variable 'keyword-hash-table)
	(setq keyword-hash-table hash)
	(refresh-screen t)))))
