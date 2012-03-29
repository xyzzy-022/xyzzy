;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "kanji")

(in-package "editor")

(export '(map-char-encoding-region map-sjis-region map-euc-region
	  map-jis-region
	  map-utf-8-region map-utf-7-region map-ucs-2-region
	  change-fileio-encoding change-default-fileio-encoding
	  change-eol-code change-default-eol-code
	  change-clipboard-encoding
	  *kanji-map* kanji-prefix
	  delete-last-ime-composition rewind-ime-composition
	  ime-push-composition-string ime-register-word-dialog
	  *enable-ime-reconvert*))

(defvar *enable-ime-reconvert* t)

(defun map-char-encoding-region (p1 p2 &optional encoding)
  (interactive "*r\n0ZEncoding: ")
  (let ((string (buffer-substring p1 p2)))
    (or encoding (setq encoding (detect-char-encoding string)))
    (cond (encoding
	   (delete-region p1 p2)
	   (insert (convert-encoding-to-internal encoding string)))
	  ((interactive-p)
	   (message "エンコーディングが判定できません")))
    t))

(defun map-sjis-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-sjis-to-internal string))))

(defun map-euc-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-euc-to-internal string))))

(defun map-jis-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-jis-to-internal string))))

(defun map-utf-8-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-utf-8-to-internal string))))

(defun map-utf-7-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-utf-7-to-internal string))))

(defun map-ucs-2-region (p1 p2)
  (interactive "*r")
  (let ((string (buffer-substring p1 p2)))
    (delete-region p1 p2)
    (insert (map-ucs-2-to-internal string))))

(defun change-fileio-encoding (encoding)
  (interactive "ZEncoding: ")
  (set-buffer-fileio-encoding encoding))

(defun change-default-fileio-encoding (encoding)
  (interactive "ZEncoding: ")
  (setq *default-fileio-encoding* encoding))

(defun change-eol-code (&optional arg)
  (interactive "p")
  (set-buffer-eol-code (cond (arg arg)
			     ((= (buffer-eol-code) 2) 0)
			     (t (+ (buffer-eol-code) 1)))))

(defun change-default-eol-code (&optional arg)
  (interactive "p")
  (setq *default-eol-code* (cond (arg arg)
				 ((= *default-eol-code* 2) 0)
				 (t (+ *default-eol-code* 1)))))

(defun change-clipboard-encoding (encoding)
  (interactive "ZClipboard encoding: ")
  (setq *clipboard-char-encoding* encoding))

(defvar *kanji-map* nil)
(unless *kanji-map*
  (setq *kanji-map* (make-sparse-keymap))
  (setf (symbol-function 'kanji-prefix) *kanji-map*)
  (define-key ctl-x-map #\C-k 'kanji-prefix)
  (define-key *kanji-map* #\f 'change-fileio-encoding)
  (define-key *kanji-map* #\F 'change-default-fileio-encoding)
  (define-key *kanji-map* #\n 'change-eol-code)
  (define-key *kanji-map* #\N 'change-default-eol-code)
  (define-key *kanji-map* #\c 'change-clipboard-encoding))

(defun rewind-ime-composition (&optional nodelete pop)
  (interactive "p")
  (let ((read (cond ((selection-start-end (start end)
		       (prog1
			   (buffer-substring start end)
			 (unless nodelete
			   (delete-region start end)))))
		    (t
		     (let ((d (if pop
				  (pop-ime-composition-string)
				(get-ime-composition-string))))
		       (unless d
			 (plain-error "まだ変換してないでしょ?"))
		       (when (and (not nodelete)
				  (looking-back (car d)))
			 (delete-region (point)
					(progn
					  (forward-char (- (length (car d))))
					  (point))))
		       (cdr d))))))
    (toggle-ime t)
    (set-ime-read-string read)))

(defun delete-last-ime-composition ()
  (interactive)
  (let ((d (pop-ime-composition-string)))
    (unless d
      (plain-error "まだ変換してないでしょ?"))
    (when (looking-back (car d))
      (delete-region (point)
		     (progn
		       (forward-char (- (length (car d))))
		       (point))))))

(defun ime-push-composition-string (&optional nodelete)
  (interactive "p")
  (let ((read (save-excursion
		(cond ((selection-start-end (start end)
			 (prog1
			     (buffer-substring start end)
			   (unless nodelete
			     (delete-region start end)))))
		      (t
		       (let ((point (point))
			     (space (and (looking-back " ")
					 (forward-char -1))))
			 (unless (skip-chars-backward "A-Za-z-")
			   (return-from ime-push-composition-string nil))
			 (prog1
			     (buffer-substring (point)
					       (if space
						   (1- point)
						 point))
			   (unless nodelete
			     (delete-region (point) point)))))))))
    (toggle-ime t)
    (set-ime-read-string read)))

(defun ime-register-word-dialog ()
  (interactive)
  (let ((read "")
	(word ""))
    (cond ((selection-start-end (start end)
	     (setq word (buffer-substring start end))))
	  (t
	   (let ((d (get-ime-composition-string)))
	     (when d
	       (setq word (car d))
	       (setq read (map-to-full-width-string (cdr d)
						    :ascii t :hiragana t))))))
    (*ime-register-word-dialog word read)))

(defun ime-reconvert-helper ()
  (when *enable-ime-reconvert*
    (let (start1 end1 start2 end2)
      (multiple-value-setq (start2 end2)
	(get-selection-start-end))
      (save-excursion
	(unless start2
	  (forward-word 1)
	  (setq end2 (point))
	  (forward-word -1)
	  (setq start2 (point)))
	(when (> start2 end2)
	  (rotatef start2 end2))
	(when (eql (char-before end2) #\LFD)
	  (decf end2))
	(setq start1 (max (progn
			    (goto-char start2)
			    (forward-word -1)
			    (point))
			  (progn
			    (goto-char start2)
			    (goto-bol)
			    (point))))
	(setq end1 (min (progn
			  (goto-char end2)
			  (forward-word 1)
			  (point))
			(progn
			  (goto-char end2)
			  (goto-eol)
			  (point)))))
      (goto-char start2)
      (start-selection 2 t end2)
      (refresh-screen)
      (values start1 end1 start2 end2))))

(setq *ime-reconvert-helper* #'ime-reconvert-helper)

(defun ime-documentfeed-helper ()
  (save-excursion
    (let ((curr (point)))
      (goto-bol)
      (values (buffer-substring (point) curr)
	      (buffer-substring (point) (progn (goto-eol) (point)))))))

(setq *ime-documentfeed-helper* #'ime-documentfeed-helper)

(define-key spec-map #\C-c 'rewind-ime-composition)
(define-key spec-map #\C-d 'delete-last-ime-composition)
(define-key spec-map #\C-r 'ime-register-word-dialog)
(define-key spec-map #\C-p 'ime-push-composition-string)
