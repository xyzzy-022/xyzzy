;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "pframe")

(in-package "editor")

(export '(find-pseudo-frame new-pseudo-frame select-pseudo-frame
	  other-pseudo-frame delete-pseudo-frame ctl-x-6-prefix
	  ctl-x-6-map next-pseudo-frame previous-pseudo-frame
	  delete-all-pseudo-frames rename-pseudo-frame
	  selected-pseudo-frame *select-pseudo-frame-hook*
	  switch-pseudo-frame pseudo-frame-selector
	  *next-pseudo-frame-in-tab-order*
	  *pseudo-frame-bar-context-menu*
	  *pseudo-frame-bar-context-menu-frame*
	  *pseudo-frame-bar-context-menu-handler*
	  ))

(defstruct
  (pseudo-frame (:constructor pseudo-frame-constructor
		 (name winconf save-p selected-fn deleted-fn)))
  name winconf save-p selected-fn deleted-fn)

(defvar *pseudo-frame-counter* 1)
(defvar *current-pseudo-frame* nil)
(defvar *pseudo-frame-list* nil)
(defvar *select-pseudo-frame-hook* nil)
(defvar *next-pseudo-frame-in-tab-order* nil)

(defun selected-pseudo-frame ()
  *current-pseudo-frame*)

(defun list-pseudo-frames (&optional (tab-order *next-pseudo-frame-in-tab-order*))
  (if tab-order
      (tab-bar-list-items 'pseudo-frame-bar)
    *pseudo-frame-list*))

(defun save-current-pseudo-frame ()
  (when *current-pseudo-frame*
    (setf (pseudo-frame-winconf *current-pseudo-frame*)
	  (current-window-configuration))))

(defun find-pseudo-frame (name)
  (find name *pseudo-frame-list*
	:key #'pseudo-frame-name :test #'string=))

(defun pseudo-frame-check-minibuffer ()
  (when (window-buffer (minibuffer-window))
    (when (tool-bar-exist-p 'pseudo-frame-bar)
      (tab-bar-select-item 'pseudo-frame-bar *current-pseudo-frame*))
    (plain-error "ミニバッファ表示中はできません")))

(defun unique-pseudo-frame-name ()
  (let ((i *pseudo-frame-counter*))
    (loop
      (let ((name (format nil "Frame ~D" i)))
	(or (find-pseudo-frame name)
	    (return (values name i))))
      (incf i))))

(defun make-pseudo-frame-name ()
  (multiple-value-bind (def counter)
      (unique-pseudo-frame-name)
    (let ((name (if *prefix-args*
		    (read-string "New frame: " :default def)
		  "")))
      (when (or (string= name "")
		(string= name def))
	(setq name def)
	(setq *pseudo-frame-counter* (+ counter 1)))
      name)))

(defun create-pseudo-frame-1 (name &optional save-p selected-fn deleted-fn)
  (let ((frame (pseudo-frame-constructor name nil save-p selected-fn deleted-fn)))
    (setq *pseudo-frame-list* (nconc *pseudo-frame-list* (list frame)))
    (when (tool-bar-exist-p 'pseudo-frame-bar)
      (tab-bar-add-item 'pseudo-frame-bar frame name nil
			'call-pseudo-frame-bar-context-menu-handler))
    frame))

(defun create-pseudo-frame (name &optional save-p selected-fn deleted-fn)
  (pseudo-frame-check-minibuffer)
  (when (find-pseudo-frame name)
    (error "~Aはすでに存在します" name))
  (save-current-pseudo-frame)
  (delete-other-windows)
  (let ((frame (create-pseudo-frame-1 name save-p selected-fn deleted-fn)))
    (setq *current-pseudo-frame* frame)
    (when (tool-bar-exist-p 'pseudo-frame-bar)
      (tab-bar-select-item 'pseudo-frame-bar frame))
    (run-hooks '*select-pseudo-frame-hook*)
    frame))

(defun new-pseudo-frame (name &optional save-p selected-fn deleted-fn)
  (interactive (list (progn
		       (pseudo-frame-check-minibuffer)
		       (make-pseudo-frame-name))
		     't))
  (create-pseudo-frame name save-p selected-fn deleted-fn))

(defun select-pseudo-frame (frame)
  (unless (eq frame *current-pseudo-frame*)
    (pseudo-frame-check-minibuffer)
    (save-current-pseudo-frame)
    (set-window-configuration (pseudo-frame-winconf frame))
    (setq *current-pseudo-frame* frame)
    (when (tool-bar-exist-p 'pseudo-frame-bar)
      (tab-bar-select-item 'pseudo-frame-bar frame))
    (and (pseudo-frame-selected-fn frame)
	 (funcall (pseudo-frame-selected-fn frame)))
    (run-hooks '*select-pseudo-frame-hook*)))

(defun switch-pseudo-frame (name)
  (interactive (list (completing-read "Frame: "
				      (mapcar #'pseudo-frame-name *pseudo-frame-list*)
				      :must-match t)))
  (select-pseudo-frame (or (find-pseudo-frame name)
			   (error "~aはフレームではありません" name))))

(defun pseudo-frame-selector ()
  (interactive)
  (multiple-value-bind (result data)
      (let ((l (sort (mapcar #'pseudo-frame-name *pseudo-frame-list*) #'string<)))
	(dialog-box '(dialog 0 0 153 115
		      (:caption "フレーム選択")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:listbox list nil #x50a10101 7 7 85 101)
		       (:button IDOK "OK" #x50010001 96 7 50 14)
		       (:button IDCANCEL "キャンセル" #x50010000 96 24 50 14)))
		    `((list . ,l)
		      (list . ,(position (pseudo-frame-name (selected-pseudo-frame)) l :test #'string=)))
		    nil))
    (when result
      (switch-pseudo-frame (cdr (assoc 'list data))))))

(defun other-pseudo-frame ()
  (interactive)
  (let* ((frames (list-pseudo-frames))
	 (frame (or (cadr (member (pseudo-frame-name *current-pseudo-frame*)
				  frames
				  :key #'pseudo-frame-name :test #'string=))
		    (car frames))))
    (unless (eq frame *current-pseudo-frame*)
      (select-pseudo-frame frame)
      t)))

(setf (symbol-function 'next-pseudo-frame) #'other-pseudo-frame)

(defun previous-pseudo-frame ()
  (interactive)
  (let* ((frames (list-pseudo-frames))
	 (frame (let ((n (position (pseudo-frame-name *current-pseudo-frame*)
				   frames
				   :key #'pseudo-frame-name :test #'string=)))
		  (if (and n (plusp n))
		      (nth (- n 1) frames)
		    (car (last frames))))))
    (unless (eq frame *current-pseudo-frame*)
      (select-pseudo-frame frame)
      t)))

(defun delete-pseudo-frame (&optional (frame *current-pseudo-frame*))
  (interactive)
  (pseudo-frame-check-minibuffer)
  (and (pseudo-frame-deleted-fn frame)
       (funcall (pseudo-frame-deleted-fn frame)))
  (when (eq frame *current-pseudo-frame*)
    (other-pseudo-frame))
  (setq *pseudo-frame-list* (delete (pseudo-frame-name frame) *pseudo-frame-list*
				    :key #'pseudo-frame-name :test #'string=))
  (when (tool-bar-exist-p 'pseudo-frame-bar)
    (tab-bar-delete-item 'pseudo-frame-bar frame))
  (unless *pseudo-frame-list*
    (init-pseudo-frame))
  t)

(defun set-pseudo-frame-name (frame name)
  (when (find-pseudo-frame name)
    (error "~Aはすでに存在します" name))
  (when (and (tool-bar-exist-p 'pseudo-frame-bar)
	     (tab-bar-modify-item 'pseudo-frame-bar frame name))
    (setf (pseudo-frame-name frame) name)))

(defun rename-pseudo-frame (name)
  (interactive "sRename frame: ")
  (unless (zerop (length name))
    (set-pseudo-frame-name *current-pseudo-frame* name)))

(defun delete-all-pseudo-frames (&optional (create-new t))
  (let ((tb (tool-bar-exist-p 'pseudo-frame-bar)))
    (mapc #'(lambda (frame)
	      (and (pseudo-frame-deleted-fn frame)
		   (funcall (pseudo-frame-deleted-fn frame)))
	      (and tb (tab-bar-delete-item 'pseudo-frame-bar frame)))
	  *pseudo-frame-list*))
  (setq *pseudo-frame-list* nil)
  (when create-new
    (init-pseudo-frame))
  t)

(defun pseudo-frame-bar ()
  (create-tab-bar 'pseudo-frame-bar #'(lambda (frame)
					(select-pseudo-frame frame)
					(refresh-screen 'foo)))
  (mapc #'(lambda (frame)
	    (tab-bar-add-item 'pseudo-frame-bar frame (pseudo-frame-name frame) nil
			      'call-pseudo-frame-bar-context-menu-handler))
	*pseudo-frame-list*)
  (tab-bar-select-item 'pseudo-frame-bar *current-pseudo-frame*))

(defun init-pseudo-frame ()
  (unless *pseudo-frame-list*
    (setq *current-pseudo-frame* nil)
    (setq *pseudo-frame-counter* 1)
    (new-pseudo-frame (let ((*prefix-args* nil))
			(make-pseudo-frame-name))
		      't)))

(defun new-pseudo-frame-menu-update ()
  (if (window-buffer (minibuffer-window))
      :disable))

(defun pseudo-frame-menu-update ()
  (if (or (null (cdr *pseudo-frame-list*))
	  (window-buffer (minibuffer-window)))
      :disable))

(defun buffer-in-any-pseudo-frame-p (buffer)
  (member-if #'(lambda (frame)
		 (unless (eq frame *current-pseudo-frame*)
		   (member-if #'(lambda (x)
				  (member buffer x :test #'eq))
			      (caddr (pseudo-frame-winconf frame)))))
	     *pseudo-frame-list*))

(defvar *pseudo-frame-bar-context-menu* nil)
(defvar *pseudo-frame-bar-context-menu-frame* nil)

(defun default-pseudo-frame-bar-context-menu-handler (frame)
  (when frame
    (setq *pseudo-frame-bar-context-menu-frame* frame)
    *pseudo-frame-bar-context-menu*))

(defvar *pseudo-frame-bar-context-menu-handler*
  #'default-pseudo-frame-bar-context-menu-handler)

(defun call-pseudo-frame-bar-context-menu-handler (frame)
  (when *pseudo-frame-bar-context-menu-handler*
    (funcall *pseudo-frame-bar-context-menu-handler* frame)))

(define-command-bar 'pseudo-frame-bar "フレーム(&F)")

(defvar ctl-x-6-map (make-sparse-keymap))
(setf (symbol-function 'ctl-x-6-prefix) ctl-x-6-map)

(global-set-key '(#\C-x #\6) 'ctl-x-6-prefix)
(define-key ctl-x-6-map #\0 'delete-pseudo-frame)
(define-key ctl-x-6-map #\2 'new-pseudo-frame)
(define-key ctl-x-6-map #\o 'other-pseudo-frame)
(define-key ctl-x-6-map #\p 'previous-pseudo-frame)
