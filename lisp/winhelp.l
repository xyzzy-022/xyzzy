;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "winhelp")

(in-package "editor")

(export '(run-winhelp kill-winhelp show-winhelp find-winhelp
	  *winhelp-path* *winhelp-directory* *winhelp-index-files*
	  run-info-viewer *info-viewer-title* *info-viewer-file-name*
	  show-info-viewer show-html-help *html-help-path*))

(defvar *winhelp-path-list* nil)

(defvar *winhelp-path* nil)

(defvar *winhelp-directory* nil)
(defvar *winhelp-index-files* nil)

(defconstant *reg-info-viewer-titles*
  "Software\\Microsoft\\InfoViewer\\Titles\\")

(defvar *info-viewer-file-name* nil)
(defvar *info-viewer-title* nil)

(defvar *html-help-path* nil)

(defun run-winhelp (path &optional topic)
  (setq path (truename path))
  (when (*run-winhelp path topic)
    (pushnew path *winhelp-path-list* :test #'string-equal)
    t))

(defun kill-winhelp (path)
  (setq path (truename path))
  (when (*kill-winhelp path)
    (delete path *winhelp-path-list* :test #'string-equal)
    t))

(defun kill-all-winhelp ()
  (dolist (f *winhelp-path-list*)
    (*kill-winhelp f)))

(add-hook '*kill-xyzzy-hook* 'kill-all-winhelp)

(defun run-info-viewer (title kwd)
  (long-operation
    (let ((chan (handler-case (dde-initiate "MSIN" title)
		  (dde-no-conv ()
		    (let ((r (concat *reg-info-viewer-titles* title)))
		      (call-process
		       (concat
			(or *info-viewer-file-name*
			    (let ((viewer (read-registry r "Exe"
							 :current-user)))
			      (unless viewer
				(error "ÉrÉÖÅ[ÉèÇ™íËã`Ç≥ÇÍÇƒÇ¢Ç‹ÇπÇÒ"))
			      (setq *info-viewer-file-name* viewer)
			      viewer))
			" "
			(read-registry r "Path" :current-user)
			title)
		       :show :show :wait 60))
		    (dde-initiate "MSIN" title)))))
      (unwind-protect
	  (handler-case (dde-execute chan (format nil "KeywordLookup(`~A')"
						  (if kwd kwd "")))
	    (dde-timeout () nil))
	(dde-terminate chan)))))

(defun get-winhelp-topic ()
  (or (selection-start-end (start end)
	(buffer-substring start end))
      (save-excursion
	(buffer-substring (progn
			    (skip-syntax-spec-forward "w_")
			    (point))
			  (progn
			    (skip-syntax-spec-backward "w_")
			    (point))))))

(defun show-winhelp ()
  (interactive)
  (run-winhelp *winhelp-path*
	       (let ((topic (get-winhelp-topic)))
		 (and (not (equal topic "")) topic))))

(defun find-winhelp ()
  (interactive)
  (multiple-value-bind (file topic)
      (find-winhelp-path (mapcar #'(lambda (x)
				     (merge-pathnames x *winhelp-directory*))
				 *winhelp-index-files*)
			 (get-winhelp-topic))
    (when file
      (run-winhelp (merge-pathnames file *winhelp-directory*)
		   (and (not (equal topic "")) topic)))))

(defun show-info-viewer ()
  (interactive)
  (run-info-viewer *info-viewer-title*
		   (let ((topic (get-winhelp-topic)))
		     (and (not (equal topic "")) topic))))

(defun show-html-help ()
  (interactive)
  (html-help *html-help-path* (get-winhelp-topic)))

(global-set-key #\F1 'find-winhelp)
(global-set-key #\C-F1 'show-winhelp)
(global-set-key #\F3 'show-info-viewer)
