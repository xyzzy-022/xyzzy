;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "diff")

(in-package "editor")

(export '(*diff-command-name* *diff-command-option*
	  *diff-old-file-foreground* *diff-old-file-background*
	  *diff-new-file-foreground* *diff-new-file-background*
	  *diff-merge-foreground* *diff-merge-background*
	  *diff-split-vertically* *diff-mode-hook* *diff-mode-map*
	  diff diff-forward diff-backward
	  diff-merge diff-merge-reverse diff-undo diff-quit))

(defvar *diff-command-name* "diff")
(defvar *diff-command-option* "-a")

(defvar *diff-output-regexp*
  (compile-regexp "^\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?$"))

(defvar *diff-old-file-foreground* 0)
(defvar *diff-old-file-background* 11)
(defvar *diff-new-file-foreground* 0)
(defvar *diff-new-file-background* 9)
(defvar *diff-merge-foreground* 0)
(defvar *diff-merge-background* 15)
(defvar *diff-split-vertically* t)

(defvar *diff-mode-hook* nil)
(defvar *diff-mode-map* nil)

(defvar *diff-old-buffer-name* "*diff old*")
(defvar *diff-new-buffer-name* "*diff new*")

(unless *diff-mode-map*
  (setq *diff-mode-map* (make-sparse-keymap))
  (define-key *diff-mode-map* #\M-Down 'diff-forward)
  (define-key *diff-mode-map* #\M-Up 'diff-backward)
  (define-key *diff-mode-map* #\q 'diff-quit)
  (define-key *diff-mode-map* #\n 'diff-forward)
  (define-key *diff-mode-map* #\p 'diff-backward)
  (define-key *diff-mode-map* #\m 'diff-merge)
  (define-key *diff-mode-map* #\r 'diff-merge-reverse)
  (define-key *diff-mode-map* #\u 'diff-undo))

(defvar-local diff-buffer nil)
(defvar-local diff-new-file-p nil)

(defun diff-color (tag c l1 l2 f fg bg)
  (let (p1 p2)
    (if f
	(setq p1 (progn
		   (goto-line l1)
		   (or (zerop l1)
		       (forward-line 1)
		       (goto-eol))
		   (point))
	      p2 p1)
      (setq p1 (progn
		 (goto-line l1)
		 (point))
	    p2 (progn
		 (goto-line l2)
		 (unless (forward-line 1)
		   (goto-eol)
		   (setq f t))
		 (point))))
    (set-text-attribute p1 p2 tag
			:foreground fg :background bg
			:prefix c :extend (not f))))

(defun diff-color-old (x)
  (let ((c (cadr x))
	(y (caddr x)))
    (diff-color x c (car y) (cdr y) (eql c #\+)
		*diff-old-file-foreground*
		*diff-old-file-background*)))

(defun diff-color-new (x)
  (let ((c (cadr x))
	(y (cadddr x)))
    (diff-color x c (car y) (cdr y) (eql c #\-)
		*diff-new-file-foreground*
		*diff-new-file-background*)))

(defun diff-code (x)
  (case x
    (#\a #\+)
    (#\c #\!)
    (t #\-)))

(defun diff-toggle-code (x)
  (case x
    (#\+ #\-)
    (#\- #\+)
    (t x)))

(defun diff-scan (buffer)
  (let ((diffs '()))
    (save-excursion
      (set-buffer buffer)
      (cond ((eobp)
	     (return-from diff-scan nil))
	    ((not (looking-at *diff-output-regexp*))
	     (error (buffer-substring (point) (progn (goto-eol) (point))))))
      (while (scan-buffer *diff-output-regexp* :tail t)
	(push (list 'diff
		    (diff-code (schar (match-string 4) 0))
		    (let ((x (parse-integer (match-string 1)))
			  (y (match-string 3)))
		      (cons x (if y (parse-integer y) x)))
		    (let ((x (parse-integer (match-string 5)))
			  (y (match-string 7)))
		      (cons x (if y (parse-integer y) x))))
	      diffs)))
    diffs))

(defun diff-set-buffer (buffer)
  (pop-to-buffer buffer t *diff-split-vertically*))

(defun diff-update (p1 p2)
  (let ((l2 (progn
	      (diff-set-buffer diff-buffer)
	      (goto-char p2)
	      (recenter)
	      (get-window-line)))
	(l1 (progn
	      (diff-set-buffer diff-buffer)
	      (goto-char p1)
	      (recenter)
	      (get-window-line))))
    (cond ((< l1 l2)
	   (diff-set-buffer diff-buffer)
	   (recenter l1)
	   (diff-set-buffer diff-buffer))
	  ((> l1 l2)
	   (recenter l2)))))

(defun diff-view (diffs old-buffer new-buffer)
  (delete-other-windows)
  (diff-update (progn
		 (set-buffer old-buffer)
		 (delete-text-attributes 'diff :key #'safe-car)
		 (mapc #'diff-color-old diffs)
		 (point-min))
	       (progn
		 (set-buffer new-buffer)
		 (delete-text-attributes 'diff :key #'safe-car)
		 (mapc #'diff-color-new diffs)
		 (point-min))))

(defun diff-set-modified ()
  (set-buffer-modified-p
   (find-text-attribute-if #'(lambda (x)
			       (and (consp x)
				    (eq (car x) 'diff)
				    (consp (cdr x))
				    (consp (cadr x)))))))

(defun diff-merge-1 (old-buffer p1 p2 new-buffer q1 q2 tag ope reverse)
  (save-excursion
    (let ((new-tag (list 'diff (cons ope reverse) nil)))
      (set-buffer old-buffer)
      (delete-text-attributes tag)
      (let ((text (save-excursion
		    (set-buffer new-buffer)
		    (delete-text-attributes tag)
		    (set-text-attribute q1 q2 new-tag
					:foreground *diff-merge-foreground*
					:background *diff-merge-background*
					:prefix ope :extend t)
		    (buffer-substring q1 q2)))
	    (buffer-read-only nil))
	(declare (special buffer-read-only))
	(goto-char p1)
	(set-text-attribute (point)
			    (progn
			      (unless (eq ope #\+)
				(setf (caddr new-tag) (buffer-substring p1 p2))
				(delete-region p1 p2))
			      (unless (eq ope #\-)
				(insert text))
			      (point))
			    new-tag
			    :foreground *diff-merge-foreground*
			    :background *diff-merge-background*
			    :prefix ope :extend t)))))

(defun diff-undo-1 (old-buffer p1 p2 new-buffer q1 q2 tag ope reverse)
  (save-excursion
    (let ((new-tag (list 'diff ope))
	  ofg obg nfg nbg)
      (if reverse
	  (setq nfg *diff-old-file-foreground*
		nbg *diff-old-file-background*
		ofg *diff-new-file-foreground*
		obg *diff-new-file-background*)
	(setq ofg *diff-old-file-foreground*
	      obg *diff-old-file-background*
	      nfg *diff-new-file-foreground*
	      nbg *diff-new-file-background*))
      (set-buffer new-buffer)
      (delete-text-attributes tag)
      (set-text-attribute q1 q2 new-tag
			  :foreground nfg
			  :background nbg
			  :prefix ope :extend t)
      (set-buffer old-buffer)
      (delete-text-attributes tag)
      (let ((text (caddr tag))
	    (buffer-read-only nil))
	(declare (special buffer-read-only))
	(goto-char p1)
	(set-text-attribute (point)
			    (progn
			      (delete-region p1 p2)
			      (and text (insert text))
			      (point))
			    new-tag
			    :foreground ofg
			    :background obg
			    :prefix ope :extend t)
	(diff-set-modified)))))

(defun diff-modify (undo-p reverse)
  (interactive "p")
  (multiple-value-bind (p1 p2 tag)
      (find-text-attribute-point (save-excursion
				   (goto-bol)
				   (point)))
    (let* ((buffer (selected-buffer))
	   (old-buffer buffer)
	   (new-buffer diff-buffer)
	   q1 q2)
      (unless (and p1
		   (if undo-p (consp (cadr tag)) (not (consp (cadr tag))))
		   (save-excursion
		     (set-buffer diff-buffer)
		     (multiple-value-setq (q1 q2)
		       (find-text-attribute tag))))
	(plain-error "‚È‚¢‚Á‚·"))
      (when diff-new-file-p
	(rotatef old-buffer new-buffer)
	(rotatef p1 q1)
	(rotatef p2 q2))
      (cond (undo-p
	     (let ((ope (caadr tag))
		   (reverse (cdadr tag)))
	       (if reverse
		   (diff-undo-1 new-buffer q1 q2 old-buffer p1 p2 tag
				(diff-toggle-code ope) reverse)
		 (diff-undo-1 old-buffer p1 p2 new-buffer q1 q2 tag ope reverse))))
	    (reverse
	     (diff-merge-1 new-buffer q1 q2 old-buffer p1 p2 tag
			   (diff-toggle-code (cadr tag)) t))
	    (t
	     (diff-merge-1 old-buffer p1 p2 new-buffer q1 q2 tag (cadr tag) nil)))
      t)))

(defun diff-mode (buffer file other-buffer mode new-file-p)
  (save-excursion
    (set-buffer buffer)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer buffer)
    (insert-file-contents file t)
    (setq buffer-read-only t)
    (set-buffer-file-name file)
    (and mode (funcall mode))
    (setq buffer-mode 'diff-mode)
    (setq mode-name "Diff")
    (setq buffer-read-only t)
    (setq diff-buffer other-buffer)
    (setq diff-new-file-p new-file-p)
    (make-local-variable 'kept-undo-information)
    (setq kept-undo-information nil)
    (use-keymap *diff-mode-map*)
    (run-hooks '*diff-mode-hook*)))

(defun diff (new-file old-file)
  (interactive "fdiff(new file): \nfdiff(old file): ")
  (setq new-file (namestring new-file))
  (setq old-file (namestring old-file))
  (diff-quit)
  (let ((buffer nil))
    (unwind-protect
	(progn
	  (setq buffer (create-new-buffer "*diff*"))
	  (save-window-excursion
	    (execute-shell-command (format nil "~A ~A \"~A\" \"~A\""
					   *diff-command-name*
					   *diff-command-option*
					   old-file new-file)
				   nil buffer))
	  (let ((diffs (diff-scan buffer)))
	    (if diffs
		(let ((old-buffer (get-buffer-create *diff-old-buffer-name*))
		      (new-buffer (get-buffer-create *diff-new-buffer-name*))
		      (mode (cdr (or (assoc-if #'(lambda (x)
						   (string-matchp x new-file))
					       *auto-mode-alist*)
				     (assoc-if #'(lambda (x)
						   (string-matchp x old-file))
					       *auto-mode-alist*)))))
		  (diff-mode old-buffer old-file new-buffer mode nil)
		  (diff-mode new-buffer new-file old-buffer mode t)
		  (diff-view diffs old-buffer new-buffer))
	      (msgbox "ˆá‚¢‚Í‚ ‚è‚Ü‚¹‚ñ"))))
      (and buffer
	   (delete-buffer buffer))))
  t)

(defun diff-forward (&optional reverse)
  (interactive "p")
  (multiple-value-bind (p1 p2 tag)
      (if reverse
	  (find-text-attribute 'diff :end (- (point) 1) :key #'safe-car :from-end t)
	(find-text-attribute 'diff :start (+ (point) 1) :key #'safe-car))
    (unless (and p1
		 (save-excursion
		   (set-buffer diff-buffer)
		   (setq p2 (find-text-attribute tag))))
      (plain-error "‚È‚¢‚Á‚·"))
    (diff-update p1 p2)))

(defun diff-backward ()
  (interactive)
  (diff-forward t))

(defun diff-merge (&optional reverse)
  (interactive "p")
  (diff-modify nil reverse))

(defun diff-merge-reverse ()
  (interactive)
  (diff-merge t))

(defun diff-undo ()
  (interactive)
  (diff-modify t nil))

(defun diff-quit ()
  (interactive)
  (when (prog1
	    diff-buffer
	  (and (find-buffer *diff-old-buffer-name*)
	       (or (kill-buffer *diff-old-buffer-name*)
		   (quit)))
	  (and (find-buffer *diff-new-buffer-name*)
	       (or (kill-buffer *diff-new-buffer-name*)
		   (quit))))
    (delete-other-windows))
  t)
