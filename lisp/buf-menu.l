;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "buf-menu")

(in-package "editor")

(export '(*buffer-menu-mode-hook* list-buffers buffer-menu
	  buffer-menu-this-window buffer-menu-1-window buffer-menu-save
	  buffer-menu-delete buffer-menu-delete-backward
	  buffer-menu-not-modified buffer-menu-unmark
	  buffer-menu-execute buffer-menu-mode))

(defvar *buffer-menu-mode-hook* nil)

(defun list-buffers (&optional file-only)
  (interactive "p")
  (let ((selected (selected-buffer)))
    (with-output-to-temp-buffer ("*Buffer List*")
      (let ((tmp (selected-buffer)))
	(princ " MR Buffer            Size    Line  Mode          File\n")
	(princ " -- ------            ----    ----  ----          ----\n")
	(dolist (buffer (buffer-list))
	  (when (not (eq buffer tmp))
	    (let ((bufname (buffer-name buffer))
		  (filename (get-buffer-file-name buffer)))
	      (unless (or (string= bufname " " :end1 1)
			  (and (null filename) file-only))
		(let ((size (buffer-size buffer))
		      (lines (buffer-lines buffer)))
		  (format t "~:[ ~;.~]~:[ ~;*~]~:[ ~;%~] ~A~VT ~D~VT ~D~35T ~A~:[~;~49T ~:*~A~]~%"
			  (eq buffer selected)
			  (buffer-modified-p buffer)
			  (buffer-local-value buffer 'buffer-read-only)
			  bufname
			  (- 24 (if (zerop size) 0 (floor (log size 10)))) size
			  (- 32 (if (zerop lines) 0 (floor (log lines 10)))) lines
			  (buffer-local-value buffer 'mode-name)
			  filename)))))))
      (goto-char (point-min))
      (buffer-menu-mode))))

(defun buffer-menu ()
  (interactive)
  (list-buffers)
  (goto-line 3))

(defun buffer-menu-buffer-exist-p ()
  (goto-column 1)
  (when (or (looking-at "[-M]")
	    (eobp))
    (plain-error "バッファ名がありません"))
  t)

(defun buffer-menu-buffname ()
  (buffer-menu-buffer-exist-p)
  (buffer-substring (progn
		      (goto-column 4)
		      (point))
		    (progn
		      (or (scan-buffer " +[0-9]+ +[0-9]+ +[^ 0-9]"
				       :regexp t
				       :limit (save-excursion (goto-eol) (point)))
			  (scan-buffer " +[0-9]+ +[0-9]+ " :regexp t))
		      (point))))

(defun buffer-menu-this-window ()
  (interactive)
  (set-buffer (buffer-menu-buffname)))

(defun buffer-menu-1-window ()
  (interactive)
  (set-buffer (buffer-menu-buffname))
  (delete-other-windows))

(defun buffer-menu-other-window ()
  (interactive)
  (switch-to-buffer-other-window (buffer-menu-buffname)))

(defun buffer-menu-save ()
  (interactive)
  (buffer-menu-buffer-exist-p)
  (goto-column 1)
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (insert #\S)
    (delete-char 1))
  (forward-line 1))

(defun buffer-menu-delete ()
  (interactive)
  (buffer-menu-buffer-exist-p)
  (goto-column 2)
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (insert #\D)
    (delete-char 1))
  (forward-line 1))

(defun buffer-menu-delete-backward ()
  (interactive)
  (buffer-menu-delete)
  (forward-line -2)
  (when (looking-at " [-M]")
    (forward-line 1))
  t)

(defun buffer-menu-not-modified ()
  (interactive)
  (set-buffer-modified-p nil (buffer-menu-buffname))
  (goto-column 1)
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (insert #\SPC)
    (delete-char 1))
  (forward-line 1))

(defun buffer-menu-unmark ()
  (interactive)
  (let* ((buffer (buffer-menu-buffname))
	 (read-only (buffer-local-value buffer 'buffer-read-only)))
    (goto-column 1)
    (let ((buffer-read-only nil))
      (declare (special buffer-read-only))
      (insert (if (buffer-modified-p buffer) #\* #\SPC))
      (insert (if read-only #\% #\SPC))
      (delete-char 2))
    (forward-line 1)))

(defun buffer-menu-execute ()
  (interactive)
  (goto-char (point-min))
  (while (scan-buffer "^.S" :regexp t)
    (save-excursion
      (set-buffer (buffer-menu-buffname))
      (save-buffer))
    (forward-line 1))
  (goto-char (point-min))
  (while (scan-buffer "^..D" :regexp t)
    (delete-buffer (buffer-menu-buffname))
    (forward-line 1))
  (buffer-menu))

(defun buffer-menu-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'buffer-menu-mode)
  (setq mode-name "Buffer menu")
  (use-keymap *buffer-menu-mode-map*)
  (setq buffer-read-only t)
  (set-buffer-fold-width nil)
  (make-local-variable 'kept-undo-information)
  (setq kept-undo-information nil)
  (make-local-variable 'need-not-save)
  (setq need-not-save t)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function #'list-buffers)
  (run-hooks '*buffer-menu-mode-hook*))

(defvar *buffer-menu-mode-map* nil)
(unless *buffer-menu-mode-map*
  (setq *buffer-menu-mode-map* (make-sparse-keymap))
  (define-key *buffer-menu-mode-map* #\q 'buffer-menu-1-window)
  (define-key *buffer-menu-mode-map* #\1 'buffer-menu-1-window)
  (define-key *buffer-menu-mode-map* #\f 'buffer-menu-this-window)
  (define-key *buffer-menu-mode-map* #\s 'buffer-menu-save)
  (define-key *buffer-menu-mode-map* #\d 'buffer-menu-delete)
  (define-key *buffer-menu-mode-map* #\k 'buffer-menu-delete)
  (define-key *buffer-menu-mode-map* #\o 'buffer-menu-other-window)
  (define-key *buffer-menu-mode-map* #\C-d 'buffer-menu-delete-backward)
  (define-key *buffer-menu-mode-map* #\C-k 'buffer-menu-delete)
  (define-key *buffer-menu-mode-map* #\x 'buffer-menu-execute)
  (define-key *buffer-menu-mode-map* #\SPC 'next-line)
  (define-key *buffer-menu-mode-map* #\~ 'buffer-menu-not-modified)
  (define-key *buffer-menu-mode-map* #\u 'buffer-menu-unmark))

(define-key ctl-x-map #\C-b 'list-buffers)
