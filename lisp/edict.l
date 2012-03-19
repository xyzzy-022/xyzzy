;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "edict")

(in-package "editor")

(export '(*edict-dictionary-path*
	  lookup-e2j-dictionary-word lookup-idiom-dictionary-word
	  lookup-j2e-dictionary-word lookup-reading-dictionary-word
	  lookup-e2j-dictionary-selection lookup-idiom-dictionary-selection
	  lookup-j2e-dictionary-selection lookup-reading-dictionary-selection
	  lookup-e2j-dictionary lookup-idiom-dictionary
	  lookup-j2e-dictionary lookup-reading-dictionary
	  *edict-conjugation-regexp* edict-mode edict-mouse-lookup))

(defvar-local edict-mode nil)

(defvar *edict-conjugation-regexp*
  '(("^\\(.+\\)s$" . "\\1")
    ("^\\(.+\\)es$" . "\\1")
    ("^\\(.+\\)ies$" . "\\1y")
    ("^\\(.+\\)ves$" . "\\1f")
    ("^\\(.+\\)ed$" . "\\1")
    ("^\\(.+\\)ed$" . "\\1e")
    ("^\\(.+\\)\\(.\\)\\2ed$" . "\\1\\2")
    ("^\\(.+\\)ied$" . "\\1y")
    ("^\\(.+\\)ing$" . "\\1")
    ("^\\(.+\\)ing$" . "\\1e")
    ("^\\(.+\\)\\(.\\)\\2ing$" . "\\1\\2")
    ("^\\(.+\\)ying$" . "\\1ie")
    ("^\\(.+\\)er$" . "\\1")
    ("^\\(.+\\)er$" . "\\1e")
    ("^\\(.+\\)\\(.\\)\\2er$" . "\\1\\2")
    ("^\\(.+\\)ier$" . "\\1y")
    ("^\\(.+\\)est$" . "\\1")
    ("^\\(.+\\)est$" . "\\1e")
    ("^\\(.+\\)\\(.\\)\\2est$" . "\\1\\2")
    ("^\\(.+\\)iest$" . "\\1y")
    ("^\\(.+\\)al$" . "\\1")
    ("^\\(.+\\)ly$" . "\\1")
    ("^\\(.+\\)ally$" . "\\1")))

(defun edict-lookup-word (fn)
  (apply fn (save-excursion
              (list (flet ((end-of-word-p (&optional (pt (point)))
			     (and (syntax-word-p (char-before pt))
				  (not (syntax-word-p (char-after pt))))))
		      (unless (end-of-word-p)
			(forward-word 1))
		      (point))
		    (progn
		      (forward-word -1)
		      (point))))))

(defun lookup-e2j-dictionary-word ()
  (interactive)
  (edict-lookup-word #'lookup-e2j-dictionary))

(defun lookup-idiom-dictionary-word ()
  (interactive)
  (edict-lookup-word #'lookup-idiom-dictionary))

(defun lookup-j2e-dictionary-word ()
  (interactive)
  (edict-lookup-word #'lookup-j2e-dictionary))

(defun lookup-reading-dictionary-word ()
  (interactive)
  (edict-lookup-word #'lookup-reading-dictionary))

(defun edict-lookup-selection (fn)
  (multiple-value-bind (start end)
      (get-selection-start-end)
    (if start
	(funcall fn start end)
      (edict-lookup-word fn))))

(defun lookup-e2j-dictionary-selection ()
  (interactive)
  (edict-lookup-selection #'lookup-e2j-dictionary))

(defun lookup-idiom-dictionary-selection ()
  (interactive)
  (edict-lookup-selection #'lookup-idiom-dictionary))

(defun lookup-j2e-dictionary-selection ()
  (interactive)
  (edict-lookup-selection #'lookup-j2e-dictionary))

(defun lookup-reading-dictionary-selection ()
  (interactive)
  (edict-lookup-selection #'lookup-reading-dictionary))

(defun lookup-e2j-dictionary (from to)
  (interactive "r")
  (edict-analogize-conjugation from to))

;;;(defun lookup-e2j-dictionary (from to)
;;;  (interactive "r")
;;;  (edict-lookup-dictionary from to "xyzzye2j" nil))

(defun lookup-idiom-dictionary (from to)
  (interactive "r")
  (edict-lookup-dictionary from to "xyzzyidi" "xyzzye2j"))

(defun lookup-j2e-dictionary (from to)
  (interactive "r")
  (edict-lookup-dictionary from to "xyzzyj2e" nil))

(defun lookup-reading-dictionary (from to)
  (interactive "r")
  (edict-lookup-dictionary from to "xyzzyjrd" nil))

(defun edict-lookup-dictionary (from to dic dic2)
  (let* ((s (buffer-substring from to))
	 (r (lookup-dictionary *edict-dictionary-path* "xyzzydic" dic s)))
    (unless r
      (plain-error "“o˜^‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ: ~A" s))
    (let ((buffer (selected-buffer)))
      (with-output-to-temp-buffer ("*dictionary*" t)
	(long-operation
	  (format t "~A:~%~%" s)
	  (if dic2
	      (dolist (x (sort r #'string<))
		(format t "~A~%" x)
		(format t "  ~{~A~^,~}~%~%"
			(lookup-dictionary *edict-dictionary-path* "xyzzydic" dic2 x)))
	    (format t "  ~{~A~^,~}~%" r))
	  (set-buffer "*dictionary*")
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))))
      (pop-to-buffer buffer)))
  t)

(defun edict-analogize-conjugation-1 (string rl)
  (mapc #'(lambda (x)
	    (when (string-matchp (car x) string)
	      (let* ((s (string-replace-match string (cdr x)))
		     (r (lookup-dictionary *edict-dictionary-path*
					   "xyzzydic" "xyzzye2j" s)))
		(when r
		  (push (cons s r) rl)))))
	*edict-conjugation-regexp*)
  rl)

(defun edict-analogize-conjugation (from to)
  (long-operation
    (let ((string (buffer-substring from to))
	  (rl nil))
      (when (setq r (lookup-dictionary *edict-dictionary-path*
				       "xyzzydic" "xyzzye2j" string))
	(push (cons string r) rl))
      (setq rl (edict-analogize-conjugation-1 string rl))
      (unless rl
	(plain-error "“o˜^‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ: ~A" string))
      (let ((buffer (selected-buffer)))
	(with-output-to-temp-buffer ("*dictionary*" t)
	  (dolist (x (nreverse rl))
	    (format t "~A:~%~%  ~{~A~^,~}~%~%" (car x) (cdr x))))
	(set-buffer "*dictionary*")
	(fill-region (point-min) (point-max))
	(goto-char (point-min))
	(pop-to-buffer buffer)))
    t))

(defvar-local edict-mode nil)
(defvar *edict-mode-map*
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap #\MouseMove 'edict-mouse-lookup)
    (define-key keymap #\C-MouseMove 'edict-mouse-lookup-conjugation)
    keymap))

(defvar *edict-last-range-begin* nil)
(defvar *edict-last-range-end* nil)

(defun edict-mode (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode 'edict-mode arg sv)
  (if edict-mode
      (set-minor-mode-map *edict-mode-map*)
    (unset-minor-mode-map *edict-mode-map*))
  (setq *edict-last-range-begin* nil)
  (update-mode-line t))

(defun edict-mouse-lookup (&optional conjugation)
  (interactive)
  (continue-pre-selection)
  (when edict-mode
    (with-selected-window
      (set-window *last-mouse-window*)
      (save-excursion
	(goto-last-mouse-point)
	(cond ((eolp)
	       (setq *edict-last-range-begin* nil))
	      ((and *edict-last-range-begin*
		    (<= *edict-last-range-begin* (point))
		    (< (point) *edict-last-range-end*))
	       (continue-popup))
	      (t
	       (let* ((end (progn
			     (skip-chars-forward "A-Za-z")
			     (point)))
		      (beg (progn
			     (skip-chars-backward "A-Za-z")
			     (point)))
		      s r)
		 (setq *edict-last-range-begin* beg)
		 (setq *edict-last-range-end* end)
		 (if (= beg end)
		     (return-from edict-mouse-lookup nil))
		 (setq s (buffer-substring beg end))
		 (handler-case
		     (setq r (lookup-dictionary *edict-dictionary-path*
						"xyzzydic" "xyzzye2j" s))
		   (error (e)
		     (edict-mode nil)
		     (error e)))
		 (if (or conjugation (not r))
		     (progn
		       (setq r (edict-analogize-conjugation-1
				s (if r (acons s r nil) nil)))
		       (if r
			   (popup-string (with-output-to-string (so)
					   (do ((x (nreverse r) (cdr x)))
					       ((null x))
					     (format so "~A:~%~%  ~{~A~^, ~}~@[~%~%~]"
						     (caar x) (cdar x) (cdr x))))
					 beg)
			 (popup-string "Œ©‚Â‚©‚è‚Ü‚Ö‚ñ" beg)))
		   (popup-string (format nil "~A:~%~%~{~A~^, ~}" s r) beg)))))))))

(defun edict-mouse-lookup-conjugation ()
  (interactive)
  (edict-mouse-lookup t))

(pushnew '(edict-mode . "Edict") *minor-mode-alist* :key #'car)

(global-set-key '(#\C-c #\e) 'lookup-e2j-dictionary-selection)
(global-set-key '(#\C-c #\i) 'lookup-idiom-dictionary-selection)
(global-set-key '(#\C-c #\j) 'lookup-j2e-dictionary-selection)
(global-set-key '(#\C-c #\r) 'lookup-reading-dictionary-selection)
