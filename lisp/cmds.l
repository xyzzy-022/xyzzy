;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "cmds")

(in-package "editor")

(export '(backward-line next-line previous-line next-line-add-newlines
	  backward-virtual-line next-virtual-line previous-virtual-line
	  backward-char backward-word next-word previous-word
	  goto-last-modified-line
	  delete-char delete-backward-char backward-delete-char-untabify
	  backward-delete-char-untabify-or-selection
	  delete-char-or-selection delete-backward-char-or-selection
	  mark-word kill-word backward-kill-word kill-line open-line
	  end-of-line beginning-of-line end-of-buffer beginning-of-buffer
	  delete-horizontal-spaces just-one-space delete-trailing-spaces
	  delete-indentation split-line back-to-indentation delete-blank-lines
	  default-input-function self-insert-command *auto-fill-hook newline
	  zap-to-char overwrite-mode toggle-over point-marker
	  set-mark-command exchange-point-and-mark
	  upcase-word downcase-word capitalize-word
	  transpose-chars transpose-words transpose-lines transpose-region
	  untabify tabify indent-relative newline-and-indent
	  fast-scroll-up fast-scroll-down scroll-up-both-window
	  scroll-down-both-window scroll-other-window scroll-up-other-window
	  scroll-down-other-window scroll-left scroll-right
	  eval-expression set-buffer-package set-variable
	  digit-argument negative-argument universal-argument
	  quote-char undefined what-cursor-position *executing-macro*
	  repeat-complex-command overwrite-char
	  indent-for-comment comment-start comment-end
	  comment-indent-function comment-column
	  comment-start-skip comment-start-skip-function
	  *smart-indentation* smart-indentation
	  mark-whole-buffer end-of-virtual-line beginning-of-virtual-line
	  *show-matched-parenthesis*
	  set-text-color clear-all-text-colors
	 ))

(defvar *executing-macro* nil)

(defun backward-line (&optional (n 1))
  (interactive "p")
  (forward-line (- n)))

(or (boundp 'next-line-add-newlines)
    (setq next-line-add-newlines nil))

(defun next-line-1 (n line-mover column-mover)
  (let ((goal-column (goal-column))
	(moved (funcall line-mover n)))
    (cond ((and next-line-add-newlines
		(plusp n)
		(not (eql moved n)))
	   (goto-char (point-max))
	   (insert #\LFD (- n (or moved 0)))
	   (set-goal-column goal-column))
	  (moved
	   (funcall column-mover goal-column)
	   (set-goal-column goal-column))
	  (t nil))))

(defun next-line (&optional (n 1))
  (interactive "p")
  (next-line-1 n #'forward-line #'goto-column))

(defun previous-line (&optional (n 1))
  (interactive "p")
  (next-line (- n)))

(defun backward-virtual-line (&optional (n 1))
  (interactive "p")
  (forward-virtual-line (- n)))

(defun next-virtual-line (&optional (n 1))
  (interactive "p")
  (next-line-1 n #'forward-virtual-line #'goto-virtual-column))

(defun previous-virtual-line (&optional (n 1))
  (interactive "p")
  (next-virtual-line (- n)))

(defun backward-char (&optional (n 1))
  (interactive "p")
  (forward-char (- n)))

(defun backward-word (&optional (n 1))
  (interactive "p")
  (forward-word (- n)))

(defun next-word (&optional (arg 1))
  (interactive "p")
  (if (plusp arg)
      (let ((opoint (point)))
	(when (forward-word 1)
	  (forward-word -1)
	  (when (> (point) opoint)
	    (decf arg))
	  (and (plusp arg)
	       (forward-word arg)
	       (forward-word 1)
	       (forward-word -1))
	  t))
    (forward-word arg)))

(defun previous-word (&optional (arg 1))
  (interactive "p")
  (next-word (- arg)))

(defun delete-char (&optional (n 1))
  (interactive "*p")
  (delete-region (point)
		 (progn
		   (forward-char n)
		   (point))))

(defun delete-backward-char (&optional (n 1))
  (interactive "*p")
  (delete-char (- n)))

(defun delete-char-or-selection (&optional (n 1))
  (interactive "*p")
  (if (pre-selection-p)
      (delete-selection)
    (delete-char n)))

(defun delete-backward-char-or-selection (&optional (n 1))
  (interactive "*p")
  (delete-char-or-selection (- n)))

(defun backward-delete-char-untabify (&optional (arg 1))
  (interactive "*p")
  (delete-backward-char
   (save-excursion
     (dotimes (x arg x)
       (if (eql (preceding-char) #\TAB)
	   (let ((col (current-column)))
	     (forward-char -1)
	     (insert #\SPC (- col (current-column)))
	     (delete-char 1)))
       (or (forward-char -1)
	   (return x))))))

(defun backward-delete-char-untabify-or-selection (&optional (n 1))
  (interactive "*p")
  (if (pre-selection-p)
      (delete-selection)
    (backward-delete-char-untabify n)))

(defun mark-word (&optional (arg 1))
  (interactive "p")
  (save-excursion
    (forward-word arg)
    (set-mark-command)))

(defun kill-word (&optional (arg 1))
  (interactive "*p")
  (kill-region (point)
	       (progn
		 (forward-word arg)
		 (point))))

(defun backward-kill-word (&optional (arg 1))
  (interactive "*p")
  (kill-word (- arg)))

(defun kill-line (&optional lines)
  (interactive "*p")
  (kill-region (point)
	       (progn
		 (cond ((null lines)
			(if (eolp)
			    (forward-line 1)
			  (goto-eol)))
		       ((zerop lines)
			(if (bolp)
			    (forward-line -1)
			  (goto-bol)))
		       (t
			(forward-line lines)))
		 (point))))

(defun open-line (&optional (arg 1))
  (interactive "*p")
  (insert #\LFD arg)
  (forward-char (- arg)))

(defun end-of-line ()
  (interactive)
  (goto-eol))

(defun beginning-of-line ()
  (interactive)
  (goto-bol))

(defun end-of-virtual-line ()
  (interactive)
  (goto-virtual-eol))

(defun beginning-of-virtual-line ()
  (interactive)
  (goto-virtual-bol))

(defun end-of-buffer ()
  (interactive)
  (set-mark-command)
  (goto-char (point-max)))

(defun beginning-of-buffer ()
  (interactive)
  (set-mark-command)
  (goto-char (point-min)))

(defun delete-horizontal-spaces ()
  (interactive "*")
  (delete-region (progn
		   (skip-chars-backward " \t")
		   (point))
		 (progn
		   (skip-chars-forward " \t")
		   (point))))

(defun just-one-space ()
  (interactive "*")
  (delete-horizontal-spaces)
  (insert #\SPC))

(defun delete-trailing-spaces ()
  (delete-region (point)
		 (progn
		   (skip-chars-backward " \t")
		   (point))))

(defun delete-indentation (&optional arg)
  (interactive "*p")
  (if arg
      (forward-line 1)
    (goto-bol))
  (delete-backward-char)
  (delete-horizontal-spaces)
  (protect-match-data
    (unless (or (bolp)
		(looking-at "\\s)")
		(save-excursion
		  (forward-char -1)
		  (looking-at "\\s(\\|\\s'\\|\\(\\sk\\|\\sj\\)\\{2\\}")))
      (insert #\SPC)))
  t)

(defvar *smart-indentation* t)

(defun smart-indentation (column)
  (let ((opoint (point)) oend beg end)
    (save-excursion
      (setq beg (progn (goto-bol) (point)))
      (setq end (progn (skip-chars-forward " \t") (point)))
      (setq oend end)
      (when (or (/= column (current-column))
		(null *smart-indentation*)
		(and (eq *smart-indentation* 't)
		     (/= (if indent-tabs-mode
			     (apply #'+ (multiple-value-list
					 (truncate column (tab-columns
							   (selected-buffer)))))
			   column)
			 (- end beg))))
	(delete-region beg end)
	(indent-to column)
	(setq end (point))))
    (and (<= opoint oend) (goto-char end))
    t))

(defun split-line ()
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((column (current-column))
	(opoint (point)))
    (insert #\LFD)
    (indent-to column)
    (goto-char opoint)))

(defun back-to-indentation ()
  (interactive)
  (goto-bol)
  (skip-chars-forward " \t")
  t)

(defun delete-blank-lines ()
  (interactive "*")
  (protect-match-data
    (save-excursion
      (goto-bol)
      (or (looking-at "^[ \t]*$")
	  (forward-line 1))
      (let ((lines 0)
	    (opoint (point))
	    from)
	(while (looking-at "^[ \t]*$")
	  (setq lines (1+ lines))
	  (unless (forward-line 1)
	    (goto-eol)
	    (return)))
	(setq from (point))
	(goto-char opoint)
	(while (forward-line -1)
	  (unless (looking-at "^[ \t]*$")
	    (forward-line 1)
	    (return))
	  (setq lines (1+ lines)))
	(unless (zerop lines)
	  (delete-region (point) from)
	  (and (/= lines 1)
	       (open-line)))))))

(defun zap-to-char (c &optional (n 1))
  (interactive "cZap to char: \np")
  (setq c (string c))
  (protect-match-data
    (kill-region (point)
		 (dotimes (x n (point))
		   (unless (scan-buffer c :tail t)
		     (return (point-max)))))))

(defun toggle-mode (var arg sv)
  (set var (if sv
	       (if (integerp arg)
		   (not (minusp arg))
		 arg)
	     (null (symbol-value var)))))

(defun overwrite-mode (&optional (arg nil sv))
  (interactive "p")
  (toggle-mode 'overwrite-mode arg sv))

(setf (symbol-function 'toggle-over) #'overwrite-mode)

(defun overwrite-char (c &optional (arg 1))
  (let ((tab-columns (tab-columns (selected-buffer))))
    (dotimes (x arg)
      (cond ((char= c #\LFD)
	     (unless (forward-virtual-line 1)
	       (goto-eol)
	       (insert c)))
	    ((char= c #\TAB)
	     (let ((goal (* (1+ (truncate (current-virtual-column)
					  tab-columns))
			    tab-columns)))
	       (goto-virtual-column goal)
	       (and (virtual-eolp)
		    (< (current-virtual-column) goal)
		    (insert c))))
	    (t
	     (let ((column (char-columns c)))
	       (while (and (plusp column)
			   (not (eolp)))
		 (let ((c (following-char)))
		   (cond ((char= c #\TAB)
			  (let ((nchars (- tab-columns
					   (rem (current-virtual-column)
						tab-columns)
					   column)))
			    (delete-char 1)
			    (when (> nchars 0)
			      (insert #\SPC nchars)
			      (forward-char (- nchars)))
			    (setq column (- nchars))))
			 ((= (char-columns c) 2)
			  (delete-char 1)
			  (when (= column 1)
			    (insert #\SPC 1)
			    (forward-char -1))
			  (setq column (- column 2)))
			 (t
			  (delete-char 1)
			  (setq column (1- column)))))))
	     (insert c))))
    t))

(defvar *show-matched-parenthesis* t)

(defun show-matched-parenthesis ()
  (when *show-matched-parenthesis*
    (if (pos-visible-in-window-p (point))
	(sit-for .5)
      (let ((bol (progn (goto-virtual-bol) (point))))
	(message "~A" (buffer-substring bol
					(min (+ bol 100)
					     (progn
					       (goto-virtual-eol)
					       (point)))))))))

(or (boundp '*auto-fill-hook)
    (setq-default *auto-fill-hook nil))

(or (boundp 'default-input-function)
    (setq default-input-function 'self-insert-command))

(defun self-insert-command (&optional (arg 1))
  (interactive "*p")
  (when (pre-selection-p)
    (delete-region (selection-mark) (selection-point))
    (stop-selection))
  (and abbrev-mode
       (not (bobp))
       (not (syntax-word-p *last-command-char*))
       (syntax-word-p (preceding-char))
       (expand-abbrev))
  (if overwrite-mode
      (overwrite-char *last-command-char* arg)
    (insert *last-command-char* arg))
  (and (interactive-p)
       (not *executing-macro*)
       (or (syntax-close-p *last-command-char*)
	   (syntax-close-tag-p *last-command-char*)
	   (syntax-math-p *last-command-char*))
       (save-excursion
	 (forward-char -1)
	 (and (goto-matched-parenthesis)
	      (show-matched-parenthesis))))
  (and auto-fill *auto-fill-hook
       (funcall *auto-fill-hook *last-command-char*))
  t)

(defun newline (&optional (arg 1))
  (interactive "*p")
  (let ((*last-command-char* #\LFD))
    (self-insert-command arg)))

(defun set-mark-command ()
  (interactive)
  (set-mark)
  (unless *executing-macro*
    (message "Mark set"))
  t)

(defun mark-whole-buffer (&optional arg)
  (interactive "p")
  (cond (arg
	 (goto-char (point-min))
	 (set-mark-command)
	 (goto-char (point-max)))
	(t
	 (goto-char (point-max))
	 (set-mark-command)
	 (goto-char (point-min)))))

(defun point-marker ()
  (let ((marker (make-marker)))
    (set-marker marker)
    marker))

(defun exchange-point-and-mark ()
  (interactive)
  (let ((mark (mark)))
    (set-mark)
    (goto-char mark)))

(defun upcase-word (&optional (arg 1))
  (interactive "*p")
  (upcase-region (point)
		 (progn
		   (forward-word arg)
		   (point))))

(defun downcase-word (&optional (arg 1))
  (interactive "*p")
  (downcase-region (point)
		   (progn
		     (forward-word arg)
		     (point))))

(defun capitalize-word (&optional (arg 1))
  (interactive "*p")
  (capitalize-region (point)
		     (progn
		       (forward-word arg)
		       (point))))

(defun transpose-chars (&optional (arg 1 f))
  (interactive "*p")
  (and (null f)
       (eolp)
       (forward-char -1))
  (transpose-region #'forward-char arg))

(defun transpose-words (&optional (arg 1))
  (interactive "*p")
  (transpose-region #'forward-word arg))

(defun transpose-lines (&optional (arg 1))
  (interactive "*p")
  (transpose-region #'forward-line arg))

(defun transpose-region (mover arg)
  (let (start1 end1 start2 end2)
    (funcall mover -1)
    (setq start1 (point))
    (funcall mover 1)
    (setq end1 (point))
    (cond ((zerop arg)
	   (goto-char (mark))
	   (funcall mover -1)
	   (setq start2 (point))
	   (funcall mover)
	   (setq end2 (point))
	   (if (< start1 start2)
	       (transpose-region-1 start1 end1 start2 end2 t)
	     (transpose-region-1 start2 end2 start1 end1 nil)))
	  ((plusp arg)
	   (funcall mover arg)
	   (setq end2 (point))
	   (funcall mover -1)
	   (setq start2 (point))
	   (transpose-region-1 start1 end1 start2 end2 t))
	  (t (funcall mover (1- arg))
	     (setq start2 (point))
	     (funcall mover 1)
	     (setq end2 (point))
	     (transpose-region-1 start2 end2 start1 end1 nil)))))

(defun transpose-region-1 (start1 end1 start2 end2 dir)
  (and (> end1 start2)
       (plain-error "ŒðŠ·‚Å‚«‚Ü‚¹‚ñ"))
  (let ((string1 (buffer-substring start1 end1))
	(string2 (buffer-substring start2 end2)))
    (delete-region start2 end2)
    (goto-char start2)
    (insert string1)
    (if dir
	(save-excursion
	  (delete-region start1 end1)
	  (goto-char start1)
	  (insert string2))
      (progn
	(delete-region start1 end1)
	(goto-char start1)
	(insert string2)))))

(defun goto-last-modified-line ()
  (interactive)
  (let ((x (last-modified-point)))
    (when x
      (goto-char x)
      (goto-virtual-bol))))

(defun untabify (from to)
  (interactive "*r")
  (let ((tab-columns (tab-columns (selected-buffer))))
    (save-excursion
      (save-restriction
	(narrow-to-region from to)
	(goto-char (point-min))
	(while (scan-buffer "\t")
	  (insert #\SPC (- tab-columns (rem (current-column) tab-columns)))
	  (delete-char)))))
  t)

(defun tabify (from to)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let ((indent-tabs-mode t)
	    (tab-columns (tab-columns (selected-buffer)))
	    columnb columne)
	(declare (special indent-tabs-mode))
	(while (scan-buffer "[ \t][ \t]+" :regexp t)
	  (setq columnb (current-column))
	  (goto-char (match-end 0))
	  (setq columne (current-column))
	  (when (/= (truncate columnb tab-columns)
		    (truncate columne tab-columns))
	    (delete-region (match-beginning 0) (match-end 0))
	    (indent-to columne))))))
  t)

(defun indent-relative ()
  (interactive "*")
  (delete-horizontal-spaces)
  (indent-to (save-excursion
	       (goto-bol)
	       (skip-chars-backward " \t\n")
	       (goto-bol)
	       (skip-chars-forward " \t")
	       (if (eolp) 0 (current-column))))
  t)

(defun newline-and-indent (&optional (arg 1))
  (interactive "*p")
  (insert #\LFD arg)
  (indent-relative))

(defun fast-scroll-up ()
  (interactive)
  (scroll-window 2))

(defun fast-scroll-down ()
  (interactive)
  (scroll-window -2))

(defun scroll-up-both-window ()
  (interactive)
  (other-window 1)
  (scroll-window 2)
  (other-window -1)
  (scroll-window 2))

(defun scroll-down-both-window ()
  (interactive)
  (other-window 1)
  (scroll-window -2)
  (other-window -1)
  (scroll-window -2))

(defun scroll-other-window (&optional arg)
  (interactive "p")
  (other-window 1)
  (if arg
      (previous-page)
    (next-page))
  (other-window -1))

(defun scroll-up-other-window (&optional (arg 1))
  (interactive "p")
  (other-window 1)
  (scroll-window arg)
  (other-window -1))

(defun scroll-down-other-window (&optional (arg 1))
  (interactive "p")
  (other-window 1)
  (scroll-window (- arg))
  (other-window -1))

(defun scroll-left (&optional arg)
  (interactive "p")
  (scroll-window-horizontally (if arg
				  arg
				(max (- (window-columns) 2) 1))))

(defun scroll-right (&optional arg)
  (interactive "p")
  (scroll-window-horizontally (- (if arg
				     arg
				   (max (- (window-columns) 2) 1)))))

(defun eval-expression (x)
  (interactive "xEval: ")
  (message "~S" (eval x)))

(defun set-buffer-package (package-name)
  (interactive
      (list (completing-read
	     "Package: " #1=(mapcar #'package-name (list-all-packages))
	     :case-fold t
	     :default (or *buffer-package* "user")
	     :must-match t)))
  (setf *buffer-package*
	(find package-name #1#
	      :test #'string-equal)))

(defun set-variable (var)
  (interactive "vSet variable: ")
  (if (and (symbolp var)
	   (boundp var))
      (set var (if (stringp (symbol-value var))
		   (read-string "Value: ")
		 (read-sexp "Value: ")))
    (error "No such variable")))

(defun process-prefix-args (raw arg sep)
  (reset-prefix-args raw arg)
  (unless sep
    (let ((neg (minusp arg)))
      (if neg
	  (setq arg (- arg)))
      (loop
	(let* ((cc (read-char *keyboard*))
	       (c (set-meta-bit cc nil)))
	  (cond ((digit-char-p c)
		 (setq arg (if (eq raw 'digit-argument)
			       (+ (* arg 10) (digit-char-p c))
			     (digit-char-p c)))
		 (setq raw 'digit-argument))
		((and (not (eq raw 'digit-argument))
		      (char= c #\-))
		 (when (eq raw 'universal-argument)
		   (setq arg 1))
		 (setq raw 'negative-argument)
		 (setq neg (not neg)))
	      (t (unread-char cc *keyboard*)
		 (return)))
	  (setq *last-command-char* cc)
	  (set-next-prefix-args 'digit-argument (if neg (- arg) arg) cc))))
    (when (eq *last-command* 'kill-region)
      (setq *this-command* 'kill-region)))
  t)

(defconstant ctrl-digit-char-hash
  (let ((hash (make-hash-table :size 10)))
    (setf (gethash #\C-0 hash) 0)
    (setf (gethash #\C-1 hash) 1)
    (setf (gethash #\C-2 hash) 2)
    (setf (gethash #\C-3 hash) 3)
    (setf (gethash #\C-4 hash) 4)
    (setf (gethash #\C-5 hash) 5)
    (setf (gethash #\C-6 hash) 6)
    (setf (gethash #\C-7 hash) 7)
    (setf (gethash #\C-8 hash) 8)
    (setf (gethash #\C-9 hash) 9)
    hash))

(defun ctrl-digit-char-p (c)
  (gethash c ctrl-digit-char-hash))

(defun digit-argument (raw &optional arg)
  (interactive "P\np")
  (let* ((c (set-meta-bit *last-command-char* nil))
	 (n (or (digit-char-p c)
		(ctrl-digit-char-p c)
		0)))
    (process-prefix-args 'digit-argument
			 (if (or (null arg)
				 (eq raw 'universal-argument))
			     n (+ (* arg 10) n))
			 nil)))

(defun negative-argument (raw &optional arg)
  (interactive "P\np")
  (if (or (null arg)
	  (eq raw 'universal-argument))
      (process-prefix-args 'negative-argument -1 nil)
    (process-prefix-args 'digit-argument (- arg) nil)))

(defun universal-argument (raw &optional arg)
  (interactive "P\np")
  (cond ((null arg)
	 (process-prefix-args 'universal-argument 4 nil))
	((eq raw 'universal-argument)
	 (process-prefix-args raw (* arg 4) nil))
	(t
	 (process-prefix-args raw arg t))))

(defun read-quoted-char-prompt (l code prefix)
  (unless *executing-macro*
    (if code
	(minibuffer-prompt "Quote char: ~a~V,'0x" prefix l code)
      (minibuffer-prompt "Quote char: ~a" prefix))))

(defun quote-char-read-hex (maxl prefix &optional internal-p)
  (let ((i 0)
	(code nil))
    (while (< i maxl)
      (read-quoted-char-prompt i code prefix)
      (let ((c (read-char *keyboard*)))
	(case c
	  (#\RET
	   (unless (zerop i)
	     (return)))
	  (#\C-h
	   (when (plusp i)
	     (decf i)
	     (setq code (if (zerop i) nil (truncate code 16)))))
	  (t
	   (setq c (digit-char-p c 16))
	   (unless c
	     (quit))
	   (setq code (if code (+ (* code 16) c) c))
	   (incf i)
	   (and internal-p
		(= i 2)
		(not (dbc-first-byte-p code))
		(return))))))
    code))

(defun quote-char-read ()
  (read-quoted-char-prompt nil nil "")
  (let (code)
    (or (let ((char (read-char *keyboard*)))
	  (cond ((eql char #\u)
		 (read-quoted-char-prompt nil nil "U+")
		 (setq char (read-char *keyboard*))
		 (let ((code (cond ((eql char #\u)
				    (quote-char-read-hex 6 "U+"))
				   (t
				    (unread-char char *keyboard*)
				    (quote-char-read-hex 4 "U+")))))
		   (setq char (unicode-char code))
		   (unless char
		     (error "U+~4,'0x‚Í”ÍˆÍŠO‚Å‚·" code))
		   char))
		((eql char #\j)
		 (iso-code-char (setq code (quote-char-read-hex 4 "JIS X 0208: ")) :jisx0208))
		((eql char #\x)
		 (iso-code-char (setq code (quote-char-read-hex 4 "JIS X 0212: ")) :jisx0212))
		((eql char #\g)
		 (iso-code-char (setq code (quote-char-read-hex 4 "GB 2312: ")) :gb2312))
		((eql char #\k)
		 (iso-code-char (setq code (quote-char-read-hex 4 "KS C 5601: ")) :ksc5601))
		((eql char #\B)
		 (iso-code-char (setq code (quote-char-read-hex 4 "Big-5: ")) :big5))
		((eql char #\i)
		 (read-quoted-char-prompt nil nil "ISO 8859-")
		 (case (read-char *keyboard*)
		   (#\1 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-1: ")) :iso8859-1))
		   (#\2 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-2: ")) :iso8859-2))
		   (#\3 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-3: ")) :iso8859-3))
		   (#\4 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-4: ")) :iso8859-4))
		   (#\5 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-5: ")) :iso8859-5))
		   (#\7 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-7: ")) :iso8859-7))
		   (#\9 (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-9: ")) :iso8859-9))
		   ((\#0 #\A #\a) (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-10: ")) :iso8859-10))
		   ((#\D #\d) (iso-code-char (setq code (quote-char-read-hex 2 "ISO 8859-13: ")) :iso8859-13))
		   (t (quit))))
		((digit-char-p char 16)
		 (unread-char char *keyboard*)
		 (code-char (quote-char-read-hex 4 "" t)))
		(t
		 char)))
	(error "~x‚Í”ÍˆÍŠO‚Å‚·" code))))

(defun quote-char (&optional (arg 1))
  (interactive "*p")
  (let ((char (quote-char-read)))
    (if (characterp char)
	(let ((*last-command-char* char)
	      (abbrev-mode nil))
	  (declare (special abbrev-mode))
	  (self-insert-command arg))
      (let ((abbrev-mode nil))
	(declare (special abbrev-mode))
	(dotimes (x arg)
	  (let ((*last-command-char* (char char 0)))
	    (self-insert-command))
	  (let ((*last-command-char* (char char 1)))
	    (self-insert-command))))))
  (unless *executing-macro*
    (clear-message))
  t)

(defun undefined ()
  (interactive)
  (ding))

(defun quit ()
  (interactive)
  (error 'quit))

(defun what-cursor-position ()
  (interactive)
  (let* ((point (point))
	 (begin (point-min))
	 (end (point-max))
	 (size (buffer-size))
	 (ratio (truncate (* point 100) (max size 1)))
	 (column (current-column))
	 (char (if (= point end)
		   ""
		 (let ((c (following-char)))
		   (multiple-value-bind (code charset)
		       (iso-char-code c)
		   (format nil "Char: ~:c (0x~x~@[ / U+~4,'0X~]~@[ / 0x~2,'0X[~A]~])  "
			   c (char-code c) (char-unicode c) code charset
		   ))))))
    (if (or (/= begin 0) (/= end size))
	(message "~Apoint=~D of ~D(~D%) <~D - ~D>  column ~D"
		 char point size ratio begin end column)
      (message "~Apoint=~D of ~D(~D%)  column ~D"
	       char point size ratio column))))

(defun repeat-complex-command (sexp)
  (interactive
      (list
       (let ((*minibuffer-default-history* *minibuffer-complex-command-history*))
	 (prog1
	     (read-from-string (read-string "Redo: "
					    :default (car *minibuffer-complex-command-history*)))
	   (setq *minibuffer-complex-command-history* *minibuffer-default-history*)))))
  (eval sexp))

(defvar-local comment-start nil)
(defvar-local comment-end nil)
(defvar-local comment-start-skip nil)
(defvar-local comment-start-skip-function nil)
(defvar-local comment-indent-function nil)
(defvar-local comment-column 32)

(defun indent-for-comment ()
  (interactive "*")
  (unless comment-start
    (plain-error "No comment syntax defined"))
  (let (match-pos)
    (goto-bol)
    (if comment-start-skip-function
	(setq match-pos (funcall comment-start-skip-function))
      (let ((eol (save-excursion (goto-eol) (point))))
	(while (and (scan-buffer comment-start-skip
				 :regexp t :tail t :limit eol)
		    (or (eq (parse-point-syntax (point)) :string)
			(progn
			  (setq match-pos (point))
			  (save-restriction
			    (narrow-to-region (match-beginning 0) (point))
			    (skip-syntax-spec-backward " ")
			    (skip-syntax-spec-backward "^ "))
			  nil))))))
    (unless match-pos
      (goto-eol))
    (let ((opoint (point))
	  (indent (save-excursion (funcall comment-indent-function))))
      (unless (= (current-column) indent)
	(delete-horizontal-spaces)
	(indent-to indent))
      (if match-pos
	  (goto-char (+ match-pos (- (point) opoint)))
	(progn
	  (insert comment-start)
	  (save-excursion
	    (insert comment-end)))))))

(defun generic-comment-indent (comment calc-indent)
  (save-excursion
    (let ((opoint (point)))
      (goto-bol)
      (cond ((looking-for comment)
	     0)
	    (t
	     (skip-chars-forward " \t")
	     (cond ((or (looking-for comment)
			(eolp))
		    (funcall calc-indent))
		   ((zerop comment-column)
		    0)
		   (t
		    (goto-char opoint)
		    (skip-chars-backward " \t")
		    (max (1+ (current-column)) comment-column))))))))

(let ((tag '#:text-color))
  (defun set-text-color (from to &optional foreground background char extend-p)
    (set-text-attribute from to tag
			:foreground foreground
			:background background
			:prefix char :extend extend-p))
  (defun clear-all-text-colors ()
    (delete-text-attributes tag :test #'eq)))

(global-set-key #\C-@ 'set-mark-command)
(global-set-key #\C-a 'beginning-of-line)
(global-set-key #\C-b 'backward-char)
(global-set-key #\C-c 'specific-command-prefix)
(global-set-key #\C-d 'delete-char-or-selection)
(global-set-key #\C-e 'end-of-line)
(global-set-key #\C-f 'forward-char)
(global-set-key #\C-g 'quit)
(global-set-key #\C-h 'delete-backward-char-or-selection)
(global-set-key #\TAB 'self-insert-command)
(global-set-key #\LFD 'newline-and-indent)
(global-set-key #\C-k 'kill-line)
(global-set-key #\RET 'newline)
(global-set-key #\C-n 'next-virtual-line)
(global-set-key #\C-o 'open-line)
(global-set-key #\C-p 'previous-virtual-line)
(global-set-key #\C-q 'quote-char)
(global-set-key #\C-t 'transpose-chars)
(global-set-key #\C-u 'universal-argument)
(global-set-key #\C-x 'ctl-x-prefix)
(global-set-key #\ESC 'meta-prefix)
(global-set-key #\C-\\ 'undo)
(global-set-key #\C-_ 'redo)

(do ((c #x20 (+ c 1)))
    ((> c #x7e))
  (global-set-key (code-char c) 'self-insert-command))

(global-set-key #\Down 'next-virtual-line)
(global-set-key #\Up 'previous-virtual-line)
(global-set-key #\Left 'backward-char)
(global-set-key #\Right 'forward-char)
(global-set-key #\Delete 'delete-char-or-selection)
(global-set-key #\End 'undo)
(global-set-key #\S-End 'redo)
(global-set-key #\F12 'redo)
(global-set-key #\C-Down 'fast-scroll-up)
(global-set-key #\C-Up 'fast-scroll-down)
(global-set-key #\C-Left 'beginning-of-line)
(global-set-key #\C-Right 'end-of-line)
(global-set-key #\S-C-Down 'scroll-up-both-window)
(global-set-key #\S-C-Up 'scroll-down-both-window)
(global-set-key #\Insert 'toggle-over)

(define-key esc-map #\C-o 'split-line)
(define-key esc-map #\C-h 'backward-kill-word)
(define-key esc-map #\C-v 'scroll-other-window)
(define-key esc-map #\ESC 'eval-expression)
(define-key esc-map #\SPC 'set-mark-command)
(define-key esc-map #\- 'negative-argument)
(define-key esc-map #\/ 'just-one-space)
(define-key esc-map #\0 'digit-argument)
(define-key esc-map #\1 'digit-argument)
(define-key esc-map #\2 'digit-argument)
(define-key esc-map #\3 'digit-argument)
(define-key esc-map #\4 'digit-argument)
(define-key esc-map #\5 'digit-argument)
(define-key esc-map #\6 'digit-argument)
(define-key esc-map #\7 'digit-argument)
(define-key esc-map #\8 'digit-argument)
(define-key esc-map #\9 'digit-argument)
(define-key esc-map #\; 'indent-for-comment)
(define-key esc-map #\< 'beginning-of-buffer)
(define-key esc-map #\> 'end-of-buffer)
(define-key esc-map #\@ 'mark-word)
(define-key esc-map #\\ 'delete-horizontal-spaces)
(define-key esc-map #\^ 'delete-indentation)
(define-key esc-map #\b 'backward-word)
(define-key esc-map #\c 'capitalize-word)
(define-key esc-map #\d 'kill-word)
(define-key esc-map #\f 'forward-word)
(define-key esc-map #\g 'goto-line)
(define-key esc-map #\h 'kill-word)
(define-key esc-map #\l 'downcase-word)
(define-key esc-map #\m 'back-to-indentation)
(define-key esc-map #\p 'repeat-complex-command)
(define-key esc-map #\t 'transpose-words)
(define-key esc-map #\u 'upcase-word)
(define-key esc-map #\x 'execute-extended-command)
(define-key esc-map #\z 'zap-to-char)
(define-key esc-map #\] 'goto-matched-parenthesis)

(define-key ctl-x-map #\C-c 'kill-xyzzy)
(define-key ctl-x-map #\C-l 'downcase-region)
(define-key ctl-x-map #\C-o 'delete-blank-lines)
(define-key ctl-x-map #\C-t 'transpose-lines)
(define-key ctl-x-map #\C-u 'upcase-region)
(define-key ctl-x-map #\C-x 'exchange-point-and-mark)
(define-key ctl-x-map #\4 'ctl-x-4-prefix)
(define-key ctl-x-map #\< 'scroll-left)
(define-key ctl-x-map #\> 'scroll-right)
(define-key ctl-x-map #\? 'describe-key-briefly)
(define-key ctl-x-map #\a 'set-variable)
(define-key ctl-x-map #\n 'narrow-to-region)
(define-key ctl-x-map #\w 'widen)
(define-key ctl-x-map #\= 'what-cursor-position)

