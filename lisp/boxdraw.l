;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "boxdraw")

(in-package "editor")

(export '(*box-drawings-line-type* *box-drawings-thin-line* *box-drawings-thick-line*
	  box-drawings-right box-drawings-left
	  box-drawings-up box-drawings-down
	  box-drawings-erase-right box-drawings-erase-left
	  box-drawings-erase-up box-drawings-erase-down
	  box-drawings-thin-line box-drawings-thick-line
	  box-drawings-toggle-line-type
	  box-drawings-mode *box-drawings-mode*))

(defvar-local *box-drawings-mode* nil)
(defvar *box-drawings-mode-map* nil)

(unless *box-drawings-mode-map*
  (setq *box-drawings-mode-map* (make-sparse-keymap))
  (define-key *box-drawings-mode-map* #\C-Right 'box-drawings-right)
  (define-key *box-drawings-mode-map* #\C-Left 'box-drawings-left)
  (define-key *box-drawings-mode-map* #\C-Up 'box-drawings-up)
  (define-key *box-drawings-mode-map* #\C-Down 'box-drawings-down)
  (define-key *box-drawings-mode-map* #\C-S-Right 'box-drawings-erase-right)
  (define-key *box-drawings-mode-map* #\C-S-Left 'box-drawings-erase-left)
  (define-key *box-drawings-mode-map* #\C-S-Up 'box-drawings-erase-up)
  (define-key *box-drawings-mode-map* #\C-S-Down 'box-drawings-erase-down)
  (define-key *box-drawings-mode-map* #\C-M-Right 'box-drawings-right-arrow)
  (define-key *box-drawings-mode-map* #\C-M-Left 'box-drawings-left-arrow)
  (define-key *box-drawings-mode-map* #\C-M-Up 'box-drawings-up-arrow)
  (define-key *box-drawings-mode-map* #\C-M-Down 'box-drawings-down-arrow))

(defvar *box-drawings-line-type* '*box-drawings-thin-line*)

;;      4              64
;;     Ñ†             Ñ´
;; 1 Ñü  Ñü 2    16 Ñ™  Ñ™ 32
;;     Ñ†             Ñ´
;;      8              128

(defconstant *box-drawings-w* 1)
(defconstant *box-drawings-e* 2)
(defconstant *box-drawings-n* 4)
(defconstant *box-drawings-s* 8)
(defconstant *box-drawings-ww* 16)
(defconstant *box-drawings-ee* 32)
(defconstant *box-drawings-nn* 64)
(defconstant *box-drawings-ss* 128)
(defconstant *box-drawings-w-mask* (+ *box-drawings-w* *box-drawings-ww*))
(defconstant *box-drawings-e-mask* (+ *box-drawings-e* *box-drawings-ee*))
(defconstant *box-drawings-n-mask* (+ *box-drawings-n* *box-drawings-nn*))
(defconstant *box-drawings-s-mask* (+ *box-drawings-s* *box-drawings-ss*))
(defconstant *box-drawings-masks-list*
  (list *box-drawings-w-mask*
	*box-drawings-e-mask*
	*box-drawings-n-mask*
	*box-drawings-s-mask*))

(defconstant *box-drawings-chars*
  '((#\Ñü . #b00000011)
    (#\Ñ† . #b00001100)
    (#\Ñ° . #b00001010)
    (#\Ñ¢ . #b00001001)
    (#\Ñ£ . #b00000101)
    (#\Ñ§ . #b00000110)
    (#\Ñ• . #b00001110)
    (#\Ñ¶ . #b00001011)
    (#\Ñß . #b00001101)
    (#\Ñ® . #b00000111)
    (#\Ñ© . #b00001111)
    (#\Ñ™ . #b00110000)
    (#\Ñ´ . #b11000000)
    (#\Ñ¨ . #b10100000)
    (#\Ñ≠ . #b10010000)
    (#\ÑÆ . #b01010000)
    (#\ÑØ . #b01100000)
    (#\Ñ∞ . #b11100000)
    (#\Ñ± . #b10110000)
    (#\Ñ≤ . #b11010000)
    (#\Ñ≥ . #b01110000)
    (#\Ñ¥ . #b11110000)
    (#\Ñµ . #b11000010)
    (#\Ñ∂ . #b00111000)
    (#\Ñ∑ . #b11000001)
    (#\Ñ∏ . #b00110100)
    (#\Ñπ . #b00111100)
    (#\Ñ∫ . #b00101100)
    (#\Ñª . #b10000011)
    (#\Ñº . #b00011100)
    (#\ÑΩ . #b01000011)
    (#\Ñæ . #b11000011)

    (#\Ñü . #b00000001)
    (#\Ñü . #b00000010)
    (#\Ñ† . #b00000100)
    (#\Ñ† . #b00001000)
    (#\Ñ™ . #b00010000)
    (#\Ñ™ . #b00100000)
    (#\Ñ´ . #b01000000)
    (#\Ñ´ . #b10000000)

    (#\Å® . #b00000011)
    (#\Å© . #b00000011)
    (#\Å™ . #b00001100)
    (#\Å´ . #b00001100)))

(defun box-drawings-following-char ()
  (or (cdr (assoc (following-char) *box-drawings-chars*)) 0))

(defun box-drawings-index-char (index)
  (car (rassoc index *box-drawings-chars*)))

(defun box-drawings-line-type (x)
  (setq x (logand x #xf))
  (if (eq *box-drawings-line-type* '*box-drawings-thin-line*) x (ash x 4)))

(defun box-drawings-opposite-char (x)
  (ash x (if (zerop (logand x #x55)) -1 1)))

(defun box-drawings-opposite-mask (x)
  (logand x (if (eq *box-drawings-line-type* '*box-drawings-thin-line*) #xf0 #xf)))

(defun box-drawings-flap (x)
  (logior x (ash x -4) (ash x 4)))

(defun box-drawings-goto-column (goal adjust-p)
  (let ((current (goto-virtual-column goal)))
    (cond ((= current goal) t)
	  ((null adjust-p) nil)
	  ((virtual-eolp)
	   (insert #\SPC (- goal current)))
	  (t
	   (overwrite-char #\SPC)
	   (goto-virtual-column goal)))))

(defun box-drawings-next-line (arg adjust-p)
  (let ((goal (current-virtual-column)))
    (cond ((forward-virtual-line arg))
	  ((null adjust-p)
	   (return-from box-drawings-next-line nil))
	  ((plusp arg)
	   (goto-eol)
	   (insert #\LFD arg))
	  ((minusp arg)
	   (goto-bol)
	   (insert #\LFD (- arg))
	   (forward-line arg)))
    (box-drawings-goto-column goal adjust-p)))

(defun box-drawings-forward-char (arg adjust-p)
  (let ((goal (+ (current-virtual-column) (* arg 2))))
    (unless (or (minusp goal)
		(and (buffer-fold-column)
		     (> goal (- (buffer-fold-column) 1))))
      (box-drawings-goto-column goal adjust-p))))

(defun box-drawings-east-char ()
  (save-excursion
    (if (box-drawings-forward-char 1 nil)
	(logand (box-drawings-following-char)
		*box-drawings-w-mask*)
      0)))

(defun box-drawings-west-char ()
  (save-excursion
    (if (box-drawings-forward-char -1 nil)
	(logand (box-drawings-following-char)
		*box-drawings-e-mask*)
      0)))

(defun box-drawings-north-char ()
  (save-excursion
    (if (box-drawings-next-line -1 nil)
	(logand (box-drawings-following-char)
		*box-drawings-s-mask*)
      0)))

(defun box-drawings-south-char ()
  (save-excursion
    (if (box-drawings-next-line 1 nil)
	(logand (box-drawings-following-char)
		*box-drawings-n-mask*)
      0)))

(defun box-drawings-char (i)
  (or (box-drawings-index-char i)
      (progn
	(mapc #'(lambda (mask)
		  (if (= (logand i mask) mask)
		      (setq i (logandc2 i (box-drawings-opposite-mask mask)))))
	      *box-drawings-masks-list*)
	(box-drawings-index-char i))
      (progn
	(or (zerop (logand i *box-drawings-e-mask*))
	    (zerop (logand i *box-drawings-w-mask*))
	    (let ((x (logand i (+ *box-drawings-e-mask* *box-drawings-w-mask*))))
	      (or (= x (+ *box-drawings-e* *box-drawings-w*))
		  (= x (+ *box-drawings-ee* *box-drawings-ww*))))
	    (setq i (logior (logandc2 i (+ *box-drawings-e-mask* *box-drawings-w-mask*))
			    (box-drawings-line-type (+ *box-drawings-e* *box-drawings-w*)))))
	(or (zerop (logand i *box-drawings-s-mask*))
	    (zerop (logand i *box-drawings-n-mask*))
	    (let ((x (logand i (+ *box-drawings-s-mask* *box-drawings-n-mask*))))
	      (or (= x (+ *box-drawings-s* *box-drawings-n*))
		  (= x (+ *box-drawings-ss* *box-drawings-nn*))))
	    (setq i (logior (logandc2 i (+ *box-drawings-s-mask* *box-drawings-n-mask*))
			    (box-drawings-line-type (+ *box-drawings-s* *box-drawings-n*)))))
	(box-drawings-index-char i))
      (box-drawings-index-char (box-drawings-line-type (box-drawings-flap i)))))

(defun box-drawings-write (i)
  (save-excursion
    (let ((c (box-drawings-char i)))
      (when c
	(overwrite-char c)))))

(defun box-drawings-write-last (x)
  (let* ((oi (box-drawings-following-char))
	 (i oi))
    (unless (zerop i)
      (cond ((eq *last-command* 'box-drawings-right)
	     (when (zerop (box-drawings-east-char))
	       (setq i (logandc2 i *box-drawings-e-mask*))))
	    ((eq *last-command* 'box-drawings-left)
	     (when (zerop (box-drawings-west-char))
	       (setq i (logandc2 i *box-drawings-w-mask*))))
	    ((eq *last-command* 'box-drawings-up)
	     (when (zerop (box-drawings-north-char))
	       (setq i (logandc2 i *box-drawings-n-mask*))))
	    ((eq *last-command* 'box-drawings-down)
	     (when (zerop (box-drawings-south-char))
	       (setq i (logandc2 i *box-drawings-s-mask*))))))
    (setq i (logior i (box-drawings-line-type x)))
    (unless (= i oi)
      (box-drawings-write i))
    (not (zerop oi))))

(defun box-drawings-write-this (x)
  (box-drawings-write (logior (box-drawings-opposite-char (box-drawings-east-char))
			      (box-drawings-opposite-char (box-drawings-west-char))
			      (box-drawings-opposite-char (box-drawings-north-char))
			      (box-drawings-opposite-char (box-drawings-south-char))
			      (box-drawings-line-type x))))

(defun box-drawings-erase (x)
  (let* ((i (box-drawings-following-char)))
    (unless (zerop i)
      (setq i (logandc2 i (box-drawings-flap x)))
      (let ((c (if (or (zerop (logandc2 i *box-drawings-e-mask*))
		       (zerop (logandc2 i *box-drawings-w-mask*))
		       (zerop (logandc2 i *box-drawings-s-mask*))
		       (zerop (logandc2 i *box-drawings-n-mask*)))
		   nil
		 (box-drawings-char i))))
	(save-excursion
	  (overwrite-char (if c c #\SPC)))))))

(defun box-drawings-right ()
  (interactive "*")
  (and (box-drawings-write-last *box-drawings-e*)
       (box-drawings-forward-char 1 t)
       (box-drawings-write-this *box-drawings-w*))
  t)

(defun box-drawings-left ()
  (interactive "*")
  (and (box-drawings-write-last *box-drawings-w*)
       (box-drawings-forward-char -1 t)
       (box-drawings-write-this *box-drawings-e*))
  t)

(defun box-drawings-up ()
  (interactive "*")
  (and (box-drawings-write-last *box-drawings-n*)
       (box-drawings-next-line -1 t)
       (box-drawings-write-this *box-drawings-s*))
  t)

(defun box-drawings-down ()
  (interactive "*")
  (and (box-drawings-write-last *box-drawings-s*)
       (box-drawings-next-line 1 t)
       (box-drawings-write-this *box-drawings-n*))
  t)

(defun box-drawings-erase-right ()
  (interactive "*")
  (box-drawings-erase *box-drawings-e*)
  (when (box-drawings-forward-char 1 nil)
    (box-drawings-erase *box-drawings-w*))
  t)

(defun box-drawings-erase-left ()
  (interactive "*")
  (box-drawings-erase *box-drawings-w*)
  (when (box-drawings-forward-char -1 nil)
    (box-drawings-erase *box-drawings-e*)))

(defun box-drawings-erase-up ()
  (interactive "*")
  (box-drawings-erase *box-drawings-n*)
  (when (box-drawings-next-line -1 nil)
    (box-drawings-erase *box-drawings-s*)))

(defun box-drawings-erase-down ()
  (interactive "*")
  (box-drawings-erase *box-drawings-s*)
  (when (box-drawings-next-line 1 nil)
    (box-drawings-erase *box-drawings-n*)))

(defun box-drawings-right-arrow ()
  (interactive "*")
  (save-excursion
    (overwrite-char #\Å®)))

(defun box-drawings-left-arrow ()
  (interactive "*")
  (save-excursion
    (overwrite-char #\Å©)))

(defun box-drawings-up-arrow ()
  (interactive "*")
  (save-excursion
    (overwrite-char #\Å™)))

(defun box-drawings-down-arrow ()
  (interactive "*")
  (save-excursion
    (overwrite-char #\Å´)))

(defun box-drawings-thin-line ()
  (interactive)
  (setq *box-drawings-line-type* '*box-drawings-thin-line*))

(defun box-drawings-thick-line ()
  (interactive)
  (setq *box-drawings-line-type* '*box-drawings-thick-line*))

(defun box-drawings-toggle-line-type ()
  (interactive)
  (setq *box-drawings-line-type*
	(if (eq *box-drawings-line-type* '*box-drawings-thick-line*)
	    '*box-drawings-thin-line*
	  '*box-drawings-thick-line*))
  t)

(defun box-drawings-mode (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode '*box-drawings-mode* arg sv)
  (update-mode-line t)
  (if *box-drawings-mode*
      (set-minor-mode-map *box-drawings-mode-map*)
    (unset-minor-mode-map *box-drawings-mode-map*))
  t)

(pushnew '(*box-drawings-mode* . "BoxDraw") *minor-mode-alist* :key #'car)
