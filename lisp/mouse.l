;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "mouse")

(in-package "editor")

(export '(goto-last-mouse-point mouse-left-press mouse-left-motion
	  mouse-ctl-left-motion mouse-ctl-left-press mouse-nop
	  mouse-menu-popup apps-popup))

(defun goto-last-mouse-point (&optional ex)
  (goto-virtual-line *last-mouse-line*)
  (goto-virtual-column *last-mouse-column*)
  (and ex
       (buffer-fold-column)
       (not (eolp))
       (>= *last-mouse-column* (current-line-columns))
       (forward-char 1)))

(defun drag-selection ()
  (and *enable-D&D-edit*
       (selection-start-end (start end)
	 (let ((point (save-excursion
			(goto-last-mouse-point)
			(point)))
	       (buffer (selected-buffer)))
	   (when (> start end)
	     (rotatef start end))
	   (when (and (<= start point)
		      (< point end))
	     (start-selection 2 nil end)
	     (goto-char start)
	     (fix-selection-point)
	     (multiple-value-bind (effect window)
		 (drag-region start end)
	       (when (and (eq effect :move)
			  (eq (selected-buffer) buffer))
		 (save-excursion
		   (delete-region (selection-point) (selection-mark))))
	       (when (windowp window)
		 (set-window window)))
	     (stop-selection)
	     (cancel-mouse-event)
	     t)))))

(defun mouse-selection-forward-word ()
  (cond ((skip-syntax-spec-forward "w_"))
	((word-char-p (following-char))
	 (forward-char 1))
	(t (return-from mouse-selection-forward-word nil)))
  (loop
    (while (and (word-char-p (following-char))
		(forward-char 1)))
    (unless (skip-syntax-spec-forward "w_")
      (return t))))

(defun mouse-selection-backward-word ()
  (cond ((skip-syntax-spec-backward "w_"))
	((word-char-p (preceding-char))
	 (forward-char -1))
	(t (return-from mouse-selection-backward-word nil)))
  (loop
    (while (and (word-char-p (preceding-char))
		(forward-char -1)))
    (unless (skip-syntax-spec-backward "w_")
      (return t))))

(defun mouse-selection-start-end (point click)
  (save-excursion
    (case click
      (1 (values point point))
      (2 (goto-char point)
	 (cond ((mouse-selection-forward-word)
		(values (point) (progn (mouse-selection-backward-word) (point))))
	       ((skip-syntax-spec-forward " ")
		(values (point) point))
	       ((skip-syntax-spec-forward "^jkw_ ")
		(values (point) point))
	       (t
		(values (progn (forward-word 1) (point))
			(progn (forward-word -1) (point))))))
      (3 (goto-char point)
	 (cond ((forward-virtual-line 1)
		(values (point) (progn (forward-virtual-line -1) (point))))
	       (t
		(values (progn (goto-virtual-eol) (point))
			(progn (goto-virtual-bol) (point))))))
      (4 (goto-char point)
	 (if (forward-paragraph 1)
	     (values (point) (progn (forward-paragraph -1) (point)))
	   (values point point)))
      (5 (values (point-min) (point-max))))))

(defvar *mouse-selection-begin* nil)
(defvar *mouse-selection-end* nil)

(defun mouse-left-press ()
  (interactive)
  (setq *mouse-selection-begin* nil)
  (let ((click (mod *last-mouse-click-count* 6)))
    (if (= click 1)
	(unless (drag-selection)
	  (stop-selection)
	  (if (eq (selected-window) *last-mouse-window*)
	      (progn
		(goto-last-mouse-point)
		(setq *mouse-selection-begin* (point)
		      *mouse-selection-end* (point)))
	    (progn
	      (set-window *last-mouse-window*)
	      (cancel-mouse-event))))
      (multiple-value-bind (beg end)
	  (mouse-selection-start-end
	   (save-excursion (goto-last-mouse-point) (point)) click)
	(when beg
	  (or (> beg end)
	      (rotatef beg end))
	  (setq *mouse-selection-begin* end)
	  (setq *mouse-selection-end* beg)
	  (goto-char end)
	  (start-selection 2 t)
	  (goto-char beg))))))

(defun mouse-left-motion ()
  (interactive)
  (cond ((get-selection-type)
	 (let ((click (mod *last-mouse-click-count* 6))
	       (obeg (selection-mark))
	       (oend (selection-point)))
	   (multiple-value-bind (beg end)
	       (mouse-selection-start-end
		(progn (goto-last-mouse-point t) (point)) click)
	     (if beg
		 (progn
		   (and (> beg end)
			(rotatef beg end))
		   (if (>= oend obeg)
		       (setq beg obeg)
		     (setq end beg beg obeg))
		   (when *mouse-selection-begin*
		     (if (< beg end)
			 (setq beg (min beg *mouse-selection-begin*)
			       end (max end *mouse-selection-end*))
		       (setq beg (max beg *mouse-selection-end*)
			     end (min end *mouse-selection-begin*))))
		   (goto-char beg)
		   (start-selection 2 t)
		   (goto-char end))
	       (continue-pre-selection)))))
	(t
	 (start-selection 2 t)
	 (goto-last-mouse-point)))
  (let ((o (pos-not-visible-in-window-p (point))))
    (and o (scroll-window o))))

(defun mouse-shift-left-press ()
  (interactive)
  (if (eq (selected-window) *last-mouse-window*)
      (mouse-left-motion)
    (progn
      (set-window *last-mouse-window*)
      (cancel-mouse-event))))

(defun mouse-ctl-left-motion ()
  (interactive)
  (cond ((get-selection-type)
	 (continue-pre-selection)
	 (set-selection-type 3 t)
	 (goto-last-mouse-point)
	 (set-goal-column (max 0 *last-mouse-column*)))
	(t
	 (start-selection 3 t)
	 (goto-last-mouse-point)
	 (set-goal-column (max 0 *last-mouse-column*))))
  (let ((o (pos-not-visible-in-window-p (point))))
    (and o (scroll-window o))))

(defun mouse-ctl-left-press ()
  (interactive)
  (unless (drag-selection)
    (cond ((eq (selected-window) *last-mouse-window*)
	   (goto-last-mouse-point)
	   (start-selection 3 t)
	   (set-goal-column (max 0 *last-mouse-column*)))
	  (t
	   (set-window *last-mouse-window*)
	   (cancel-mouse-event))))
  t)

(defvar *url-scheme-regexp*
  (compile-regexp
   "\\(http\\|https\\|ftp\\|news\\|nntp\\|wais\\|telnet\\|mailto\\|gopher\\):"))

(defun mouse-menu-popup (&optional apps)
  (interactive)
  (delete-menu *app-popup-menu* 'url)
  (delete-menu *app-popup-menu* 'url-sep)
  (set-window *last-mouse-window*)
  (continue-pre-selection)
  (let (menu url)
    (cond ((eql (get-selection-type) 3)
	   (setq menu *app-rectangle-popup-menu*))
	  ((and (eql (get-selection-type) 2)
		(save-excursion
		  (goto-char (min (selection-point)
				  (selection-mark)))
		  (looking-at *url-scheme-regexp*)))
	   (setq url (buffer-substring (selection-point)
				       (selection-mark)))
	   (setq menu *app-popup-menu*))
	  (t
	   (setq menu *app-popup-menu*)
	   (save-excursion
	     (goto-last-mouse-point)
	     (skip-chars-backward "-a-zA-Z0-9_/~.#@%?&=;+(),'$!*:")
	     (when (looking-at *url-scheme-regexp*)
	       (setq url (buffer-substring (point)
					   (progn
					     (goto-char (match-end 0))
					     (skip-chars-forward "-a-zA-Z0-9_/~.#@%?&=;+(),'$!*:")
					     (point))))))))
    (when url
      (add-menu-separator menu 'url-sep)
      (add-menu-item menu 'url
		     (concatenate 'string (abbreviate-display-string url 40 t)
				  " ‚ğŠJ‚­")
		     #'(lambda ()
			 (interactive)
			 (shell-execute url t))))
    (track-popup-menu menu (or apps :button2))))

(defun apps-popup ()
  (interactive)
  (let ((*last-mouse-window* (selected-window))
	(*last-mouse-line* (current-virtual-line-number))
	(*last-mouse-column* (current-virtual-column)))
    (mouse-menu-popup t)))

(defun mouse-nop ()
  (interactive)
  (continue-pre-selection))

(defun default-drag-and-drop-hook (window files)
  (set-window window)
  (if (minibuffer-window-p window)
      (progn
	(selection-start-end (start end)
	  (delete-region start end))
	(insert (format nil "~{~A~^ ~}" files)))
    (progn
      (find-file files)
      (mapc #'(lambda (f)
		(setq f (namestring f))
		(if (file-exist-p f)
		    (setq f (truename f)))
		(add-history f '*minibuffer-file-name-history*))
	    files))))

(setq *drag-and-drop-hook* #'default-drag-and-drop-hook)

(defun mouse-wheel-scroll (window n lines)
  (or lines
      (setq lines (window-lines window)))
  (with-selected-window
    (set-window window)
    (scroll-window (* n lines))))

(or (boundp 'mouse-wheel-handler)
    (setq-default mouse-wheel-handler #'mouse-wheel-scroll))

(global-set-key #\LBtnDown 'mouse-left-press)
(global-set-key #\LBtnMove 'mouse-left-motion)
(global-set-key #\LBtnUp 'mouse-nop)
(global-set-key #\C-LBtnDown 'mouse-ctl-left-press)
(global-set-key #\C-LBtnMove 'mouse-ctl-left-motion)
(global-set-key #\C-LBtnUp 'mouse-nop)
(global-set-key #\S-LBtnDown 'mouse-shift-left-press)
(global-set-key #\S-LBtnMove 'mouse-left-motion)
(global-set-key #\S-LBtnUp 'mouse-nop)
(global-set-key #\RBtnDown 'mouse-nop)
(global-set-key #\RBtnUp 'mouse-menu-popup)
(global-set-key #\Apps 'apps-popup)
(global-set-key #\MBtnDown 'begin-auto-scroll)
