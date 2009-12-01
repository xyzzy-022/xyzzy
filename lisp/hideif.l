;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "hideif")

(in-package "editor")

(export '(show-ifdef hide-ifdef
	  *hide-ifdef-cpp* *hide-ifdef-cpp-flags*
	  *hide-ifdef-c++-regexp*
	  *hide-ifdef-opt-file*
	  *hide-ifdef-use-grep*
	  *hide-ifdef-cpp-error-regexp*))

(defvar *hide-ifdef-cpp* "cl -E")
(defvar *hide-ifdef-cpp-flags* "")
(defvar *hide-ifdef-c++-regexp* "\\.cpp$\\|\\.cxx$\\|\\.cc$")
(defvar *hide-ifdef-opt-file* ".hide-if")
(defvar *hide-ifdef-use-grep* nil)
(defvar *hide-ifdef-cpp-error-regexp*
  "^\\([A-Za-z]:\\)?[^\t\n:]+: *[0-9]+:.*$\\|^[^\t\n]+ *([0-9]+).*$")

(defun hide-ifdef-mark-directive ()
  (let ((end (point-max)))
    (loop
      (goto-char end)
      (unless (scan-buffer "^[ \t]*#[ \t]*\\(if\\|el\\|end\\)"
			   :reverse t :regexp t :no-dup t)
	(goto-char (point-min))
	(insert (format nil "!!!0-~d!!!~%" end))
	(return))
      (let ((beg (point)))
	(while (and (looking-at "\\(\\|.*[^\\]\\)\\(\\\\\\\\\\)*\\\\$")
		    (forward-line 1)
		    (< (point) end)))
	(while (and (forward-line 1)
		    (<= (point) end))
	  (unless (eq (parse-point-syntax (point)) ':comment)
	    (insert (format nil "!!!~d-~d!!!~%" (point) end))
	    (return)))
	(setq end beg)))))

(defun hide-ifdef-read-option ()
  (when (file-exist-p *hide-ifdef-opt-file*)
    (erase-buffer (selected-buffer))
    (insert-file-contents *hide-ifdef-opt-file*)
    (goto-char (point-min))
    (replace-buffer "\n" " ")
    (buffer-substring (point-min) (point-max))))

(defun hide-ifdef-run-cpp (obuffer)
  (let ((buffer (selected-buffer))
	temp)
    (unwind-protect
	(save-window-excursion
	  (setq temp (make-temp-file-name
		      nil
		      (if (string-matchp *hide-ifdef-c++-regexp*
					 (or (get-buffer-file-name)
					     (buffer-name obuffer)))
			  "cpp" "c")
		      "."))
	  (write-file temp t)
	  (let ((opt (hide-ifdef-read-option)))
	    (set-buffer obuffer)
	    (unless (zerop (execute-shell-command
			    (concat *hide-ifdef-cpp* " "
				    *hide-ifdef-cpp-flags* " "
				    opt " "
				    (file-namestring temp)
				    (if *hide-ifdef-use-grep*
					" | egrep \"^!!!\""))
			    nil buffer))
	      (error "~A" (if (save-excursion
				(set-buffer buffer)
				(goto-char (point-min))
				(scan-buffer *hide-ifdef-cpp-error-regexp* :regexp t))
			      (match-string 0)
			    "‰½‚©’m‚ç‚ñ‚¯‚Çcpp‚ªƒGƒ‰[")))))
      (when temp
	(delete-file temp)))))

(defun hide-ifdef ()
  (interactive)
  (long-operation
    (let ((obuffer (selected-buffer))
	  buffer)
      (save-restriction
	(widen)
	(unwind-protect
	    (let (range)
	      (save-excursion
		(setq buffer (create-new-buffer "*hide ifdef*"))
		(set-buffer buffer)
		(use-syntax-table (syntax-table obuffer))
		(set-default-directory (default-directory obuffer))
		(insert-buffer-substring obuffer)
		(hide-ifdef-mark-directive)
		(hide-ifdef-run-cpp obuffer)
		(goto-char (point-min))
		(while (scan-buffer "^!!!\\([0-9]+\\)-\\([0-9]+\\)!!!\015?$"
				    :regexp t :tail t)
		  (push (cons (parse-integer (match-string 1))
			      (parse-integer (match-string 2)))
			range)))
	      (setq range (nreverse range))
	      (delete-text-attributes 'hide-ifdef)
	      (let ((last (cdar range)))
		(mapc #'(lambda (x)
			  (set-text-attribute last (car x) 'hide-ifdef :foreground 15)
			  (setq last (cdr x)))
		      (cdr range))))
	  (when buffer
	    (delete-buffer buffer)))))))

(defun show-ifdef ()
  (interactive)
  (delete-text-attributes 'hide-ifdef))
