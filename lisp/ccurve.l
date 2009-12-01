;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "ccurve")

(in-package "editor")

(defvar *curve-x* nil)
(defvar *curve-y* nil)

(defun curve-screen (buffer)
  (switch-to-buffer buffer)
  (set-buffer-fold-width nil)
  (erase-buffer (selected-buffer))
  (setq need-not-save t)
  (setq auto-save nil)
  (setq kept-undo-information nil))

(defun curve-plot ()
  (goto-line *curve-y*)
  (unless (= (current-line-number) *curve-y*)
    (goto-char (point-max))
    (insert #\LFD (- *curve-y* (current-line-number))))
  (goto-column *curve-x*)
  (unless (= (current-column) *curve-x*)
    (goto-eol)
    (insert #\SPC (- *curve-x* (current-column))))
  (unless (looking-for "*")
    (insert #\*)
    (unless (eolp)
      (delete-char)))
  (refresh-screen))

(defun curve-line (n)
  (curve-plot)
  (case n
    (0 (setq *curve-y* (- *curve-y* 1)))
    (1 (setq *curve-x* (- *curve-x* 1))
       (setq *curve-y* (- *curve-y* 1)))
    (2 (setq *curve-x* (- *curve-x* 1)))
    (3 (setq *curve-x* (- *curve-x* 1))
       (setq *curve-y* (+ *curve-y* 1)))
    (4 (setq *curve-y* (+ *curve-y* 1)))
    (5 (setq *curve-x* (+ *curve-x* 1))
       (setq *curve-y* (+ *curve-y* 1)))
    (6 (setq *curve-x* (+ *curve-x* 1)))
    (7 (setq *curve-x* (+ *curve-x* 1))
       (setq *curve-y* (- *curve-y* 1))))
  (curve-plot))

(defmacro curve-plus-angle (n)
  `(if (= ,n 7) 0 (1+ ,n)))

(defmacro curve-minus-angle (n)
  `(if (zerop ,n) 7 (1- ,n)))

(defun ccurve (level angle)
  (if (zerop level)
      (curve-line angle)
    (progn
      (ccurve (- level 1) (curve-plus-angle angle))
      (ccurve (- level 1) (curve-minus-angle angle)))))

(defun dcurve (level angle flag)
  (if (zerop level)
      (curve-line angle)
    (if flag
	(progn
	  (dcurve (- level 1) (curve-plus-angle angle) t)
	  (dcurve (- level 1) (curve-minus-angle angle) nil))
      (progn
	(dcurve (- level 1) (curve-minus-angle angle) t)
	(dcurve (- level 1) (curve-plus-angle angle) nil)))))

(defun c-curve (&optional (level 8))
  (interactive "p")
  (long-operation
    (curve-screen "*C curve*")
    (let ((l (1- (truncate (+ level 1) 2))))
      (setq *curve-x* 1)
      (setq *curve-y* 2)
      (dotimes (x l)
	(setq *curve-x* (+ (* *curve-x* 2) 1))
	(setq *curve-y* (+ (* *curve-y* 2) 1))))
    (setq *curve-x* (+ *curve-x* 1))
    (setq *curve-y* (+ *curve-y* 1))
    (ccurve level 0)
    (goto-char (point-min)))
  t)

(defun dragon-curve (&optional (level 8))
  (interactive "p")
  (long-operation
    (curve-screen "*Dragon curve*")
    (let ((l (1- (truncate (+ level 1) 2))))
      (setq *curve-x* 1)
      (setq *curve-y* 2)
      (dotimes (x l)
	(setq *curve-x* (+ (* *curve-x* 2) 1))
	(setq *curve-y* (+ (* *curve-y* 2) 1))))
    (setq *curve-x* (+ *curve-x* 1))
    (setq *curve-y* (+ *curve-y* 1))
    (dcurve level 0 nil)
    (goto-char (point-min)))
  t)
