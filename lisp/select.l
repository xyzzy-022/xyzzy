;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "select")

(in-package "editor")

(export '(*selection-ring* *selection-ring-max* *shodat-copy-mode*
	  start-selection-as-line start-selection-as-region
	  selection-forward-char selection-backward-char
	  selection-next-line selection-previous-line
	  selection-next-virtual-line selection-previous-virtual-line
	  selection-end-of-line selection-beginning-of-line
	  selection-end-of-virtual-line
	  selection-beginning-of-virtual-line
	  selection-end-of-buffer selection-beginning-of-buffer
	  selection-forward-word selection-backward-word
	  selection-next-word selection-previous-word
	  selection-next-page selection-previous-page
	  copy-selection kill-selection
	  delete-selection yank-selection yank-selection-and-pop
	  yank-rectangle-selection operate-on-rectangle-selection
	  copy-rectangle-selection kill-rectangle-selection
	  delete-rectangle-selection string-rectangle-selection
	  clear-rectangle-selection open-rectangle-selection
	  selection-whole-buffer selection-start-end
	  copy-selection-to-clipboard kill-selection-to-clipboard
	  copy-rectangle-selection-to-clipboard
	  kill-rectangle-selection-to-clipboard
	  paste-rectangle-from-clipboard
	  paste-from-clipboard
	  shift-selection unshift-selection
	  upcase-selection downcase-selection capitalize-selection
	  tabify-selection untabify-selection
	  map-all-to-half-width-selection
	  map-to-half-width-selection
	  map-ascii-to-half-width-selection
	  map-hiragana-to-half-width-selection
	  map-katakana-to-half-width-selection
	  map-ascii-and-hiragana-to-half-width-selection
	  map-ascii-and-katakana-to-half-width-selection
	  map-greek-to-half-width-selection
	  map-cyrillic-to-half-width-selection
	  map-all-to-full-width-hiragana-selection
	  map-all-to-full-width-katakana-selection
	  map-to-full-width-hiragana-selection
	  map-to-full-width-katakana-selection
	  map-ascii-to-full-width-selection
	  map-kana-to-full-width-hiragana-selection
	  map-kana-to-full-width-katakana-selection
	  map-greek-to-full-width-selection
	  map-cyrillic-to-full-width-selection
	  map-char-encoding-selection map-jis-selection
	  map-euc-selection))

(defvar *selection-ring* nil)
(defvar *selection-ring-max* 16)
(defvar *selection-ring-yank-pointer* nil)
(defvar *shodat-copy-mode* nil)

(defun get-selection-start-end ()
  (let (start end)
    (save-excursion
      (case (get-selection-type)
	(1
	 (setq start (progn
		       (goto-char (selection-point))
		       (goto-virtual-bol)
		       (point)))
	 (setq end (progn
		     (goto-char (selection-mark))
		     (goto-virtual-bol)
		     (point))))
	(2
	 (setq start (selection-point))
	 (setq end (selection-mark)))))
    (stop-selection)
    (values start end)))

(setf (get 'selection-start-end 'lisp-indent-hook) 1)

(defmacro selection-start-end ((start end) &body body)
  `(multiple-value-bind (,start ,end)
       (get-selection-start-end)
     (when ,start
       ,@body)))

(defun start-selection-as-line ()
  (interactive)
  (if (get-selection-type)
      (stop-selection)
    (start-selection 1)))

(defun start-selection-as-region ()
  (interactive)
  (case (get-selection-type)
    (1 (stop-selection))
    (2 (set-selection-type 3))
    (3 (set-selection-type 2))
    (t (start-selection 2))))

(defun begin-selection ()
  (if (pre-selection-p)
      (continue-pre-selection)
    (start-selection 2 t)))

(defun selection-forward-char (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (forward-char arg))

(defun selection-backward-char (&optional (arg 1))
  (interactive "p")
  (selection-forward-char (- arg)))

(defun selection-next-line (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (next-line arg))

(defun selection-previous-line (&optional (arg 1))
  (interactive "p")
  (selection-next-line (- arg)))

(defun selection-next-virtual-line (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (next-virtual-line arg))

(defun selection-previous-virtual-line (&optional (arg 1))
  (interactive "p")
  (selection-next-virtual-line (- arg)))

(defun selection-end-of-line ()
  (interactive "p")
  (begin-selection)
  (goto-eol))

(defun selection-beginning-of-line ()
  (interactive "p")
  (begin-selection)
  (goto-bol))

(defun selection-end-of-virtual-line ()
  (interactive "p")
  (begin-selection)
  (goto-virtual-eol)
  (or (eolp)
      (forward-char 1))
  t)

(defun selection-beginning-of-virtual-line ()
  (interactive "p")
  (begin-selection)
  (goto-virtual-bol))

(defun selection-end-of-buffer ()
  (interactive "p")
  (begin-selection)
  (goto-char (point-max)))

(defun selection-beginning-of-buffer ()
  (interactive "p")
  (begin-selection)
  (goto-char (point-min)))

(defun selection-forward-word (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (forward-word arg))

(defun selection-backward-word (&optional (arg 1))
  (interactive "p")
  (selection-forward-word (- arg)))

(defun selection-next-word (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (next-word arg))

(defun selection-previous-word (&optional (arg 1))
  (interactive "p")
  (selection-next-word (- arg)))

(defun selection-next-page (&optional (arg 1))
  (interactive "p")
  (begin-selection)
  (next-page arg))

(defun selection-previous-page (&optional (arg 1))
  (interactive "p")
  (selection-next-page (- arg)))

(defun selection-new (type string)
  (setq *selection-ring* (push-kill-ring (cons type string)
					 *selection-ring*
					 *selection-ring-max*))
  (setq *selection-ring-yank-pointer* *selection-ring*))

(defun current-selection (n &optional nomove)
  (when *selection-ring*
    (multiple-value-setq
	(n *selection-ring-yank-pointer*)
      (get-kill-ring *selection-ring* *selection-ring-yank-pointer* n nomove))))

(defun copy-selection ()
  (interactive)
  (let ((type (get-selection-type)))
    (if (eql type 3)
	(copy-rectangle-selection)
      (selection-start-end (start end)
	(selection-new type (buffer-substring start end)))))
  t)

(defun kill-selection ()
  (interactive "*")
  (let ((type (get-selection-type)))
    (if (eql type 3)
	(kill-rectangle-selection)
      (selection-start-end (start end)
	(selection-new type (buffer-substring start end))
	(delete-region start end))))
  t)

(defun delete-selection ()
  (interactive "*")
  (let ((type (get-selection-type)))
    (if (eql type 3)
	(delete-rectangle-selection)
      (selection-start-end (start end)
	(delete-region start end))))
  t)

(defun yank-selection (&optional (arg 0))
  (interactive "*p")
  (let ((buffer (current-selection arg))
	mover)
    (case (car buffer)
      (1 (setq mover #'goto-virtual-bol))
      (2 nil)
      (t (return-from yank-selection nil)))
    (selection-start-end (start end)
      (delete-region start end))
    (when mover
      (funcall mover))
    (insert (cdr buffer))))

(defun yank-selection-and-pop (&optional (arg 0))
  (interactive "*p")
  (when (yank-selection arg)
    (multiple-value-setq
	(*selection-ring* *selection-ring-yank-pointer*)
      (pop-kill-ring *selection-ring* *selection-ring-yank-pointer*))
    t))

(defun yank-rectangle-selection (&optional arg)
  (interactive "*p")
  (while (null arg)
    (message "F6: Cancel  F7: Append  F8: Overwrite  F9: Insert  F10: Insert as region")
    (case (read-char)
      ((#\F6 #\C-g)
       (clear-message)
       (return-from yank-rectangle-selection nil))
      (#\F7 (setq arg 1))
      (#\F8 (setq arg 2))
      (#\F9 (setq arg 3))
      (#\F10 (setq arg 4))))
  (case arg
    (1 (append-rectangle))
    (2 (overwrite-rectangle))
    (3 (yank-rectangle))
    (4 (yank-rectangle-as-region))))

(defvar *dont-clear-selection* nil)

(defun operate-on-rectangle-selection (&key copy delete clear open string)
  (when (eql (get-selection-type) 3)
    (save-excursion
      (operate-on-rectangle (selection-point)
			    (selection-mark)
			    (goal-column)
			    (progn
			      (goto-char (selection-mark))
			      (current-virtual-column))
			    :copy copy
			    :delete delete
			    :clear clear
			    :open open
			    :string string))
    (or *dont-clear-selection*
	(stop-selection)))
  t)

(defun copy-rectangle-selection ()
  (interactive)
  (operate-on-rectangle-selection :copy t))

(defun kill-rectangle-selection ()
  (interactive "*")
  (operate-on-rectangle-selection :copy t :delete t))

(defun delete-rectangle-selection ()
  (interactive "*")
  (operate-on-rectangle-selection :delete t))

(defun clear-rectangle-selection ()
  (interactive "*")
  (operate-on-rectangle-selection :copy t :clear t))

(defun open-rectangle-selection ()
  (interactive "*")
  (operate-on-rectangle-selection :open t))

(defun string-rectangle-selection (string)
  (interactive "*sString rectangle: ")
  (operate-on-rectangle-selection :string string))

(defun rectangle-selection-to-clipboard (fn)
  (let ((*rectangle-kill-buffer* nil))
    (funcall fn)
    (copy-to-clipboard (format nil "~{~A~%~}" *rectangle-kill-buffer*))))

(defun copy-rectangle-selection-to-clipboard ()
  (interactive)
  (if (and *shodat-copy-mode*
	   (pre-selection-p))
      (let ((*dont-clear-selection* t))
	(rectangle-selection-to-clipboard #'copy-rectangle-selection)
	(continue-pre-selection))
    (rectangle-selection-to-clipboard #'copy-rectangle-selection))
  t)

(defun kill-rectangle-selection-to-clipboard ()
  (interactive "*")
  (rectangle-selection-to-clipboard #'kill-rectangle-selection))

(defun paste-rectangle-from-clipboard ()
  (interactive "*")
  (let ((string (get-clipboard-data)))
    (when string
      (stop-selection)
      (let ((*rectangle-kill-buffer* (split-string string #\LFD t)))
	(yank-rectangle)))))

(defun selection-whole-buffer ()
  (interactive)
  (start-selection 2 t (point-max))
  (goto-char (point-min)))

(defun copy-selection-to-clipboard ()
  (interactive)
  (cond ((eql (get-selection-type) 3)
	 (copy-rectangle-selection-to-clipboard))
	((and *shodat-copy-mode*
	      (pre-selection-p))
	 (let ((type (get-selection-type)))
	   (selection-start-end (start end)
	     (copy-region-to-clipboard start end)
	     (start-selection type t end))))
	(t
	 (selection-start-end (start end)
	   (copy-region-to-clipboard start end))))
  t)

(defun kill-selection-to-clipboard ()
  (interactive "*")
  (if (eql (get-selection-type) 3)
      (kill-rectangle-selection-to-clipboard)
    (selection-start-end (start end)
      (kill-region-to-clipboard start end)))
  t)

(defun paste-from-clipboard ()
  (interactive "*")
  (let ((string (get-clipboard-data)))
    (when string
      (selection-start-end (start end)
	(delete-region start end))
      (insert string))))

(defun paste-from-clipboard-helper ()
  (when (paste-from-clipboard)
    (refresh-screen)))

(add-hook 'si:*paste-hook* 'paste-from-clipboard-helper)

(defun shift-selection (&optional (column (tab-columns (selected-buffer))))
  (interactive "*p")
  (let ((pre (pre-selection-p)))
    (selection-start-end (start end)
      (multiple-value-setq (start end)
	(shift-region start end column))
      (when start
	(goto-char end)
	(forward-line 1)
	(start-selection 1 pre)
	(goto-char start)))))

(defun unshift-selection (&optional (column (tab-columns (selected-buffer))))
  (interactive "*p")
  (shift-selection (- column)))

(defun map-selection (fn)
  (let ((type (get-selection-type)))
    (when type
      (let ((pre (pre-selection-p)))
	(selection-start-end (start end)
	  (save-restriction
	    (narrow-to-region start end)
	    (funcall fn start end)
	    (cond ((< start end)
		   (goto-char (point-min))
		   (start-selection type pre (point-max)))
		  (t
		   (goto-char (point-max))
		   (start-selection type pre (point-min))))))))
    t))

(defun upcase-selection ()
  (interactive "*")
  (map-selection #'upcase-region))

(defun downcase-selection ()
  (interactive "*")
  (map-selection #'downcase-region))

(defun capitalize-selection ()
  (interactive "*")
  (map-selection #'capitalize-region))

(defun tabify-selection ()
  (interactive "*")
  (map-selection #'tabify))

(defun untabify-selection ()
  (interactive "*")
  (map-selection #'untabify))

(defun map-all-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii t
					       :hiragana t :katakana t
					       :greek t :cyrillic t))))

(defun map-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii t
					       :hiragana t :katakana t))))

(defun map-ascii-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii t
					       :hiragana nil :katakana nil))))

(defun map-hiragana-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii nil
					       :hiragana t :katakana nil))))

(defun map-katakana-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii nil
					       :hiragana nil :katakana t))))

(defun map-ascii-and-hiragana-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii t
					       :hiragana t :katakana nil))))

(defun map-ascii-and-katakana-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :ascii t
					       :hiragana nil :katakana t))))

(defun map-greek-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :greek t))))

(defun map-cyrillic-to-half-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-half-width-region start end :cyrillic t))))

(defun map-all-to-full-width-hiragana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii t
					       :hiragana t :katakana nil
					       :greek t :cyrillic t))))

(defun map-all-to-full-width-katakana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii t
					       :hiragana nil :katakana t
					       :greek t :cyrillic t))))

(defun map-to-full-width-hiragana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii t
					       :hiragana t :katakana nil))))

(defun map-to-full-width-katakana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii t
					       :hiragana nil :katakana t))))

(defun map-ascii-to-full-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii t
					       :hiragana nil :katakana nil))))

(defun map-kana-to-full-width-hiragana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii nil
					       :hiragana t :katakana nil))))

(defun map-kana-to-full-width-katakana-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :ascii nil
					       :hiragana nil :katakana t))))

(defun map-greek-to-full-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :greek t))))

(defun map-cyrillic-to-full-width-selection ()
  (interactive "*")
  (map-selection #'(lambda (start end)
		     (map-to-full-width-region start end :cyrillic t))))

(defun map-char-encoding-selection (&optional encoding)
  (interactive "*0ZEncoding: ")
  (map-selection #'(lambda (b e) (map-char-encoding-region b e encoding))))

(defun map-jis-selection ()
  (interactive "*")
  (map-selection #'map-jis-region))

(defun map-euc-selection ()
  (interactive "*")
  (map-selection #'map-euc-region))

(global-set-key #\F6 'start-selection-as-line)
(global-set-key #\S-F6 'start-selection-as-region)
(global-set-key #\F7 'kill-selection)
(global-set-key #\F8 'copy-selection)
(global-set-key #\F9 'yank-selection)
(global-set-key #\S-F9 'yank-rectangle-selection)

(global-set-key #\S-Left 'selection-backward-char)
(global-set-key #\S-Right 'selection-forward-char)
(global-set-key #\S-Up 'selection-previous-virtual-line)
(global-set-key #\S-Down 'selection-next-virtual-line)

(global-set-key #\C-Delete 'kill-selection-to-clipboard)
(global-set-key #\C-Insert 'copy-selection-to-clipboard)
(global-set-key #\S-Insert 'paste-from-clipboard)
(global-set-key #\S-C-Insert 'paste-rectangle-from-clipboard)

;(global-set-key #\C-Left 'backward-word)
;(global-set-key #\C-Right 'forward-word)
;(global-set-key #\S-End 'selection-end-of-line)
;(global-set-key #\S-Home 'selection-beginning-of-line)
(global-set-key #\S-C-Left 'selection-backward-word)
(global-set-key #\S-C-Right 'selection-forward-word)
(global-set-key #\S-PageUp 'selection-previous-page)
(global-set-key #\S-PageDown 'selection-next-page)
