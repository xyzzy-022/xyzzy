;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "sexp")

(in-package "editor")

(export '(mark-sexp transpose-sexps kill-sexp backward-kill-sexp beginning-of-defun
	  end-of-defun mark-defun backward-sexp backward-list backward-up-list
	  mode-specific-beginning-of-defun mode-specific-end-of-defun))

(or (boundp 'mode-specific-beginning-of-defun)
    (setq mode-specific-beginning-of-defun nil))
(or (boundp 'mode-specific-end-of-defun)
    (setq mode-specific-end-of-defun nil))

(defun mark-sexp (&optional (arg 1))
  (interactive "p")
  (save-excursion
    (forward-sexp arg)
    (set-mark-command)))

(defun transpose-sexps (&optional (arg 1))
  (interactive "*p")
  (transpose-region 'forward-sexp arg))

(defun kill-sexp (&optional (arg 1))
  (interactive "*p")
  (kill-region (point)
	       (progn
		 (forward-sexp arg)
		 (point))))

(defun backward-kill-sexp (&optional (arg 1))
  (interactive "*p")
  (kill-sexp (- arg)))

(defun beginning-of-defun (&optional (arg 1))
  (interactive "p")
  (protect-match-data
    (if mode-specific-beginning-of-defun
	(funcall mode-specific-beginning-of-defun arg)
      (let ((dir t))
	(when (minusp arg)
	  (setq dir nil)
	  (setq arg (- arg)))
	(dotimes (x arg)
	  (unless (scan-buffer "^\\s(" :regexp t :reverse dir :no-dup t)
	    (goto-char (point-min))
	    (return)))))
    t))

(defun end-of-defun (&optional (arg 1))
  (interactive "p")
  (protect-match-data
    (if mode-specific-end-of-defun
	(funcall mode-specific-end-of-defun arg)
      (progn
	(when (plusp arg)
	  (if (backward-up-list 1 t)
	      (or (looking-at "^\\s(")
		  (beginning-of-defun))
	    (or (scan-buffer "^\\s(" :regexp t)
		(goto-char (point-max))))
	  (setq arg (1- arg)))
	(dotimes (x arg)
	  (unless (scan-buffer "^\\s(" :regexp t :no-dup t)
	    (return)))
	(and (forward-sexp 1 t)
	     (forward-line 1))))))

(defun mark-defun ()
  (interactive)
  (end-of-defun)
  (set-mark-command)
  (beginning-of-defun))

(defun backward-sexp (&optional (arg 1) noerror)
  (interactive "p")
  (forward-sexp (- arg) noerror))

(defun backward-list (&optional (arg 1) noerror)
  (interactive "p")
  (forward-list (- arg) noerror))

(defun backward-up-list (&optional (arg 1) noerror)
  (interactive "p")
  (up-list (- arg) noerror))

(define-key esc-map #\C-@ 'mark-sexp)
(define-key esc-map #\C-a 'beginning-of-defun)
(define-key esc-map #\C-b 'backward-sexp)
(define-key esc-map #\C-d 'down-list)
(define-key esc-map #\C-e 'end-of-defun)
(define-key esc-map #\C-f 'forward-sexp)
(define-key esc-map #\C-n 'forward-list)
(define-key esc-map #\C-p 'backward-list)
(define-key esc-map #\C-t 'transpose-sexps)
(define-key esc-map #\C-u 'backward-up-list)
