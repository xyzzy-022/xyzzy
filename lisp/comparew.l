;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "comparew")

(in-package "editor")

(export 'compare-windows)

(defun compare-windows ()
  (interactive)
  (let* ((w1 (selected-window))
	 (w2 (next-window w1 'arg))
	 buf1 buf2
	 start1 start2
	 end1 end2)
    (when (eq w1 w2)
      (return-from compare-windows nil))
    (unwind-protect
	(progn
	  (setq buf1 (selected-buffer) start1 (point) end1 (point-max))
	  (set-window w2)
	  (setq buf2 (selected-buffer) start2 (point) end2 (point-max))
	  (let ((diff (compare-buffer-substrings buf1 start1 end1 buf2 start2 end2)))
	    (setq diff (if (zerop diff)
			   (min (- end1 start1) (- end2 start2))
			 (- (abs diff) 1)))
	    (goto-char (+ start2 diff))
	    (set-window w1)
	    (goto-char (+ start1 diff))
	    t))
      (set-window w1))))
