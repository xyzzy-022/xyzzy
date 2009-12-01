;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "tags")

(in-package "editor")

(export '(*jump-tag-limit* *tags-file-name* jump-tag
	  jump-tag-other-window back-tag-jump direct-tag-jump
	  *tags-push-position* tags-find-target tags-find-point))

(defvar *jump-tag-limit* 3000)

(defvar *tags-file-name* "XTAGS")

(defvar *tags-push-position* t)

(defvar-local tags-find-target nil)
(defvar-local tags-find-point nil)
(defvar-local tags-case-insensitive nil)
(defvar-local tags-completion-list nil)

(defvar-local tags-use-buffer nil)
(defvar-local tags-class-method-separator "::")

(defun tags-find-target-simple ()
  (values nil
	  (save-excursion
	    (buffer-substring (progn
				(skip-syntax-spec-forward "w_")
				(point))
			      (progn
				(skip-syntax-spec-backward "w_")
				(point))))
	  t))

(defun find-tags-buffer (&optional dir)
  (or dir
      (setq dir (default-directory)))
  (setq dir (append-trail-slash dir))
  (save-excursion
    (dolist (buffer (find-name-buffer " *TAGS*"))
      (set-buffer buffer)
      (when (equalp dir (default-directory))
	(return buffer)))))

(defun create-tags-buffer (&optional dir)
  (or dir
      (setq dir (default-directory)))
  (setq dir (append-trail-slash dir))
  (let ((file (merge-pathnames *tags-file-name* dir)))
    (unless (file-exist-p file)
      (error "~Aが見つかりません" *tags-file-name*))
    (let ((buffer (create-new-buffer " *TAGS*")))
      (save-excursion
	(set-buffer buffer)
	(setq need-not-save t)
	(setq kept-undo-information nil)
	(setq lock-file nil)
	(insert-file-contents file t)
	(setq buffer-read-only t)
	(set-default-directory dir)
	(setq tags-use-buffer nil))
      buffer)))

(defun tags-setup-buffer ()
  (let ((buffer (or (find-tags-buffer)
		    (create-tags-buffer))))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (when (looking-at "#USE \\(\\(../\\)+\\)$")
	(let ((d (namestring (match-string 1))))
	  (setq buffer (or (find-tags-buffer d)
			   (create-tags-buffer d)))
	  (setq tags-use-buffer buffer)
	  (set-buffer buffer)
	  (goto-char (point-min)))))
    buffer))

(defun make-select-tags-entries (list)
  (sort
   (mapcar #'(lambda (x)
	       (goto-char x)
	       (goto-bol)
	       (cons
		(let (class sep name args file)
		  (unless (looking-at "[]")
		    (setq class (buffer-substring (point)
						  (progn (skip-chars-forward "^") (point)))
			  sep tags-class-method-separator))
		  (setq name (buffer-substring (1+ (point))
					       (progn (skip-chars-forward "^") (point)))
			file (progn
			       (forward-char 1)
			       (buffer-substring (point)
						 (progn (skip-chars-forward "^") (point))))
			args (progn
			       (skip-chars-forward "^\n")
			       (looking-for "")
			       (buffer-substring (1+ (point)) (progn (goto-eol) (point)))))
		  (concat class sep name " " args " - " file))
		x))
	   list)
   #'(lambda (x y)
       (string< (car x) (car y)))))

(defvar *select-tags-entry-dialog-template*
  '(dialog 0 0 254 107
    (:caption "タグジャンプ")
    (:font 9 "MS UI Gothic")
    (:control
     (:listbox list nil #x50b10001 2 3 188 103)
     (:button IDOK "移動(&G)" #x50030001 198 7 52 14)
     (:button IDCANCEL "キャンセル" #x50030000 198 27 52 14))))

(defun select-tags-entry-dialog (list buffer)
  (let ((entries (make-select-tags-entries list)))
    (multiple-value-bind (result data)
	(save-excursion
	  (set-buffer buffer)
	  (dialog-box *select-tags-entry-dialog-template*
		      (list (cons 'list (mapcar #'car entries))
			    '(list . 0))
		      '((list :index t :must-match t :enable (IDOK)))))
      (when result
	(cdr (nth (cdr (assoc 'list data)) entries))))))

(defun tags-looking-entry-type ()
  (cond ((looking-for "") 'function)
	((looking-for "") 'structure)
	(t nil)))

(defun tags-find-entry-1 (class name type case-insensitive)
  (let* ((sep (cond ((eq type 'structure) "")
		    ((null type) "")
		    (t "")))
	 (ntarget (concat sep name ""))
	 (ctarget (if class (concat class sep)))
	 found)
    (goto-char (point-min))
    (while (scan-buffer ntarget :case-fold case-insensitive)
      (when (or (null ctarget)
		(progn
		  (goto-bol)
		  (looking-for ctarget case-insensitive)))
	(push (point) found))
      (unless (forward-line 1)
	(return)))
    found))

(defun tags-find-entry (class name type)
  (let ((obuffer (selected-buffer))
	(case-insensitive tags-case-insensitive)
	(buffer (tags-setup-buffer))
	(tags-class-method-separator tags-class-method-separator))
    (declare (special tags-class-method-separator))
    (save-excursion
      (set-buffer buffer)
      (let* ((typel (if (consp type) type (list type)))
	     (found (or (mapcan #'(lambda (type)
				    (tags-find-entry-1 class name type case-insensitive))
				typel)
			(mapcan #'(lambda (type)
				    (tags-find-entry-1 class name type case-insensitive))
				(remove-if #'(lambda (x) (member x typel))
					   '(structure function nil)))
			(and class
			     (mapcan #'(lambda (type)
					 (tags-find-entry-1 nil name type case-insensitive))
				     '(structure function nil))))))
	(cond ((null found)
	       (message "~:[~*~;~:*~A~A~]~Aが見つかりません"
			class tags-class-method-separator name)
	       (return-from tags-find-entry nil))
	      ((null (cdr found))
	       (setq found (car found)))
	      (t
	       (setq found (select-tags-entry-dialog (nreverse found) obuffer))
	       (unless found
		 (return-from tags-find-entry nil))))
	(goto-char found)
	(skip-chars-forward "^")
	(setq type (tags-looking-entry-type))
	(skip-chars-forward "^")
	(forward-char 1)
	(values (namestring (buffer-substring (point)
					      (progn
						(skip-chars-forward "^")
						(point))))
		(parse-integer (buffer-substring (1+ (point))
						 (progn
						   (skip-chars-forward "^\n")
						   (point))))
		type)))))

(defun jump-tag-1 (file class name point functionp &optional other-window)
  (when *tags-push-position*
    (or *gmark-keep-column*
	(goto-bol))
    (global-mark-add))
  (if other-window
      (find-file-other-window file)
    (find-file file))
  (goto-char (or (funcall tags-find-point class name functionp point)
		 point))
  (goto-bol))

(defun jump-tag (&optional other-window)
  (interactive)
  (long-operation
    (when (and tags-find-target tags-find-point)
      (let ((opoint (point)))
	(multiple-value-bind (class name functionp)
	    (funcall tags-find-target)
	  (multiple-value-bind (file point functionp)
	      (tags-find-entry class name functionp)
	    (when file
	      (goto-char opoint)
	      (jump-tag-1 file class name point functionp other-window)
	      t)))))))

(defun jump-tag-other-window ()
  (interactive)
  (jump-tag t))

(defun back-tag-jump ()
  (interactive)
  (loop
    (let ((marker (car *global-mark-list*)))
      (or marker
	  (plain-error "No last tag jump"))
      (when (marker-buffer marker)
	(global-mark-goto marker)
	(global-mark-delete marker)
	(return t))
      (global-mark-delete marker))))

(defun tags-make-completion-list ()
  (long-operation
    (let ((r nil) caption class name args file point type)
      (while (not (eobp))
	(cond ((looking-at "[]")
	       (setq type (tags-looking-entry-type))
	       (setq class nil)
	       (setq name (buffer-substring (1+ (point))
					    (progn
					      (skip-chars-forward "^")
					      (point)))))
	      (t
	       (setq class (buffer-substring (point)
					     (progn
					       (skip-chars-forward "^")
					       (point))))
	       (setq type (tags-looking-entry-type))
	       (setq name (buffer-substring (1+ (point))
					    (progn
					      (skip-chars-forward "^")
					      (point))))))
	(forward-char 1)
	(setq file (namestring (buffer-substring (point)
						 (progn
						   (skip-chars-forward "^")
						   (point)))))
	(setq point (parse-integer (buffer-substring (1+ (point))
						     (progn
						       (skip-chars-forward "^\n")
						       (point)))))
	(when (looking-for "")
	  (setq args (buffer-substring (1+ (point))
				       (progn
					 (goto-eol)
					 (point)))))
	(push (list (if args
			(if class
			    (concat class tags-class-method-separator name " " args)
			  (concat name " " args))
		      (if class
			  (concat class tags-class-method-separator name)
			name))
		    file class name point type) r)
	(or (forward-line 1)
	    (return)))
      r)))

(defun direct-tag-jump ()
  (interactive)
  (let ((buffer (tags-setup-buffer))
	(tags-class-method-separator tags-class-method-separator)
	list entry req)
    (declare (special tags-class-method-separator))
    (save-excursion
      (set-buffer buffer)
      (unless tags-completion-list
	(setq tags-completion-list (tags-make-completion-list)))
      (setq list tags-completion-list))
    (setq req (completing-read "Tag jump: " list
			       :case-fold tags-case-insensitive
			       :must-match t))
    (setq entry (find req list :key #'car :test #'equal))
    (unless entry
      (error "~A~%not found" req))
    (apply #'jump-tag-1 (cdr entry))))

(global-set-key #\F4 'jump-tag)
(global-set-key #\S-F4 'back-tag-jump)

