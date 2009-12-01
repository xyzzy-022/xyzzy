;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "keymap")

(in-package "editor")

(export '(global-set-key local-set-key undefine-key global-unset-key local-unset-key
	  ctl-x-map ctl-x-prefix esc-map meta-prefix
	  spec-map specific-command-prefix ctl-x-4-map ctl-x-4-prefix
	  copy-keymap describe-bindings *full-keymap-length*
	  substitute-key-definition))

(defvar ctl-x-map (make-keymap))
(setf (symbol-function 'ctl-x-prefix) ctl-x-map)

(defvar esc-map (make-keymap))
(setf (symbol-function 'meta-prefix) esc-map)

(defvar spec-map (make-sparse-keymap))
(setf (symbol-function 'specific-command-prefix) spec-map)

(defvar ctl-x-4-map (make-sparse-keymap))
(setf (symbol-function 'ctl-x-4-prefix) ctl-x-4-map)

(defun copy-keymap (map)
  (if (consp map)
      (copy-tree map)
    (copy-seq map)))

(defun global-set-key (key command)
  (interactive "kGlobal set key: \nCcommand: ")
  (define-key *global-keymap* key command))

(defun local-set-key (key command)
  (interactive "kLocal set key: \nCcommand: ")
  (define-key (local-keymap) key command))

(defun undefine-key (keymap key)
  (loop
    (setq keymap (keymapp keymap))
    (unless keymap
      (return-from undefine-key nil))
    (if (consp key)
	(if (consp (cdr key))
	    (progn
	      (setq keymap (lookup-keymap keymap (car key)))
	      (pop key))
	  (setq key (car key)))
      (let ((c (set-meta-bit key nil)))
	(when (char= c key)
	  (return))
	(setq keymap (lookup-keymap keymap #\ESC))
	(setq key c))))
  (if (vectorp keymap)
      (setf (elt keymap (*keymap-char-index key)) nil)
    (setf (cdr keymap)
	  (delete key (cdr keymap) :key #'car)))
  t)

(defun global-unset-key (key)
  (interactive "kGlobal unset key: ")
  (undefine-key *global-keymap* key))

(defun local-unset-key (key)
  (interactive "kLocal unset key: ")
  (undefine-key (local-keymap) key))

(defvar *full-keymap-length* 380)

(defun lookup-keymap-index (keymap shadow index)
  (let* ((c (*keymap-index-char index))
	 (bound (lookup-keymap keymap c)))
    (when (and bound (notany #'(lambda (x) (lookup-keymap x c)) shadow))
      bound)))

(defun describe-bindings-1 (prefix keymap shadow)
  (setq keymap (keymapp keymap))
  (when keymap
    (setq shadow (mapcan #'(lambda (x)
			     (setq x (keymapp x))
			     (and x (list x)))
			 shadow))
    (do ((index 0)
	 (nl nil))
	((>= index *full-keymap-length*))
      (let ((nindex (1+ index))
	    (bound (lookup-keymap-index keymap shadow index)))
	(when bound
	  (if (>= index 128)
	      (when nl
		(terpri)
		(setq nl nil))
	    (unless nl
	      (terpri)
	      (setq nl t)))
	  (format t "~A~:C" prefix (*keymap-index-char index))
	  (cond ((symbolp bound)
		 (while (and (< nindex *full-keymap-length*)
			     (eq bound (lookup-keymap-index keymap shadow nindex)))
		   (setq nindex (1+ nindex)))
		 (when (/= index (1- nindex))
		   (format t " ... ~A~:C" prefix (*keymap-index-char (1- nindex))))
		 (format t "~19T ~S~%" bound))
		((keymapp bound)
		 (format t "~19T Prefix Command~%"))
		(t
		 (format t "~19T Command~%"))))
	(setq index nindex)))
    (dotimes (index *full-keymap-length*)
      (let* ((c (*keymap-index-char index))
	     (x (lookup-keymap keymap c)))
	(when (keymapp x)
	  (let ((y (mapcar #'(lambda (x) (lookup-keymap x c)) shadow)))
	    (when (notany #'(lambda (x) (and x (not (keymapp x)))) y)
	      (describe-bindings-1 (format nil "~A~:C " prefix c) x y))))))))

(defun describe-bindings ()
  (interactive)
  (let ((local (local-keymap))
	(global *global-keymap*)
	(minor (minor-mode-map))
	(shadow nil))
    (long-operation
      (message "Building binding list...")
      (with-output-to-temp-buffer ("*Help*")
	(when minor
	  (format t "Minor Mode Bindings:~%key~20Tbinding~%---~20T-------")
	  (mapc #'(lambda (x)
		    (describe-bindings-1 "" x shadow)
		    (push x shadow))
		minor))
	(format t "Local Bindings:~%key~20Tbinding~%---~20T-------")
	(describe-bindings-1 "" local shadow)
	(push local shadow)
	(format t "~%Global Bindings:~%key~20Tbinding~%---~20T-------")
	(describe-bindings-1 "" global shadow)
	(goto-char 0))
      (message "Building binding list...done"))))

(defun read-key-sequence (local global minor-maps &optional prompt)
  (do ((result)
       (keymap (append (list (current-selection-keymap)) minor-maps
		       (list local) (list global))))
      ((progn
	 (when prompt
	   (minibuffer-prompt "~a~{~:c ~}" prompt result))
	 (notany #'keymapp keymap))
       (if (consp (cdr result))
	   result
	 (car result)))
    (let ((c (read-char *keyboard*)))
      (setq result (nconc result (list c)))
      (setq keymap (mapcan #'(lambda (x)
			       (when (keymapp x)
				 (let ((y (lookup-keymap x c)))
				   (and y (list y)))))
			   keymap)))))

(defun interactive-read-key-sequence (prompt &rest rest)
  (list (read-key-sequence (local-keymap) *global-keymap*
			   (minor-mode-map) prompt)))

(pushnew '(#\k . interactive-read-key-sequence)
	 *interactive-specifier-alist* :test #'equal)

(defun expand-command-keys (keys &optional prefix)
  (let (result)
    (dolist (x keys)
      (if (consp x)
	  (setq result
		(nconc result (expand-command-keys (cdr x)
						   (cons (car x) prefix))))
	(setq result (cons (if prefix (reverse (cons x prefix)) x) result))))
    result))

(defun substitute-key-definition (olddef newdef &optional (keymap *global-keymap*) (okeymap keymap))
  (dolist (x (expand-command-keys (command-keys olddef okeymap nil)))
    (define-key keymap x newdef))
  t)
