;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "search")

(in-package "editor")

(export '(search-forward search-backward
	  search-forward-again search-backward-again
	  re-search-forward re-search-backward
	  re-search-forward-again re-search-backward-again
	  repeat-forward-search repeat-backward-search
	  perform-replace
	  replace-string query-replace replace-regexp query-replace-regexp
	  *case-fold-search* *word-search* *wrap-search*
	  *select-match* *reverse-match*
	  *query-replace-map* act skip automatic quit recenter quit-and-back undo
	  count-matches
	  *show-match-hook*))

(defvar *last-search-string* "")
(defvar *last-replace-string* "")
(defvar *last-search-regexp* "")
(defvar *last-replace-regexp* "")
(defvar *last-search-regexp-p* nil)
(defvar *last-search-p* nil)
(defvar *case-fold-search* ':smart)
(defvar *word-search* nil)
(defvar *wrap-search* t)
(defvar *reverse-match* t)
(defvar *select-match* nil)
(defvar *show-match-hook* nil)

(register-history-variable
 '(*case-fold-search* *word-search* *wrap-search* *reverse-match* *select-match*))

(defvar *no-wrap-search-on-kbd-macro* t)

(defun show-match ()
  (cond (*reverse-match*
	 (reverse-region (match-beginning 0) (match-end 0) t))
	(*select-match*
	 (goto-char (match-end 0))
	 (start-selection 2 t)
	 (goto-char (match-beginning 0)))))

(defun hide-match ()
  (cond (*reverse-match*
	 (clear-reverse-region))
	(*select-match*
	 (stop-selection))))

(defun search-command (pattern reverse regexp interactive-p &optional noerror nomsg)
  (let ((no-dup (equal pattern (if regexp
				   *last-search-regexp*
				 *last-search-string*)))
	(wrapped 'nil))
    (when interactive-p
      (setq *last-search-p* t)
      (setq *last-search-regexp-p* regexp)
      (if regexp
	  (setq *last-search-regexp* pattern)
	(setq *last-search-string* pattern))
      (or *executing-macro*
	  nomsg
	  (message "Searching...")))
    (cond ((or (scan-buffer pattern
			    :regexp regexp
			    :no-dup no-dup
			    :case-fold *case-fold-search*
			    :reverse reverse
			    :left-bound *word-search*
			    :right-bound *word-search*)
	       (and interactive-p
		    *wrap-search*
		    (or (not *executing-macro*)
			(not *no-wrap-search-on-kbd-macro*))
		    (let ((opoint (point)))
		      (goto-char (if reverse (point-max) (point-min)))
		      (setq wrapped
			    (or (scan-buffer pattern
					     :regexp regexp
					     :no-dup nil
					     :case-fold *case-fold-search*
					     :reverse reverse
					     :left-bound *word-search*
					     :right-bound *word-search*
					     :limit (if reverse
							(1+ opoint)
						      opoint))
				(progn
				  (goto-char opoint)
				  nil))))))
	   (when (and interactive-p
		      (null *executing-macro*))
	     (or nomsg
		 (message "Searching...done~A" (if wrapped "(Wrapped)" "")))
	     (or (and *show-match-hook*
		      (run-hook-with-args-while-success '*show-match-hook*))
		 (show-match)))
	   t)
	  (noerror
	   nil)
	  (t
	   (plain-error "文字列が見つかりません")))))

(defun search-forward (pattern &optional noerror)
  (interactive "sSearch forward: "
    :default0 *last-search-string* :history0 'search)
  (search-command pattern nil nil (interactive-p) noerror))

(defun search-backward (pattern &optional noerror)
  (interactive "sSearch backward: "
    :default0 *last-search-string* :history0 'search)
  (search-command pattern t nil (interactive-p) noerror))

(defun search-forward-again ()
  (interactive)
  (when (equal *last-search-string* "")
    (plain-error "No last search"))
  (search-command *last-search-string* nil nil (interactive-p) nil))

(defun search-backward-again ()
  (interactive)
  (when (equal *last-search-string* "")
    (plain-error "No last search"))
  (search-command *last-search-string* t nil (interactive-p) nil))

(defun re-search-forward (regexp &optional noerror)
  (interactive "sRe-search forward: "
    :default0 *last-search-regexp* :history0 'search)
  (search-command regexp nil t (interactive-p) noerror))

(defun re-search-backward (regexp &optional noerror)
  (interactive "sRe-search backward: "
    :default0 *last-search-regexp* :history0 'search)
  (search-command regexp t t (interactive-p) noerror))

(defun re-search-forward-again ()
  (interactive)
  (when (equal *last-search-regexp* "")
    (plain-error "No last search"))
  (search-command *last-search-regexp* nil t (interactive-p) nil))

(defun re-search-backward-again ()
  (interactive)
  (when (equal *last-search-regexp* "")
    (plain-error "No last search"))
  (search-command *last-search-regexp* t t (interactive-p) nil))

(defun repeat-forward-search ()
  (interactive)
  (if (interactive-p)
      (call-interactively (if *last-search-regexp-p*
			      're-search-forward-again
			    'search-forward-again))
    (if *last-search-regexp-p*
	(re-search-forward-again)
      (search-forward-again))))

(defun repeat-backward-search ()
  (interactive)
  (if (interactive-p)
      (call-interactively (if *last-search-regexp-p*
			      're-search-backward-again
			    'search-backward-again))
    (if *last-search-regexp-p*
	(re-search-backward-again)
      (search-backward-again))))

(defvar *query-replace-map* nil)
(unless *query-replace-map*
  (setq *query-replace-map* (make-sparse-keymap))
  (define-key *query-replace-map* #\SPC 'act)
  (define-key *query-replace-map* #\y 'act)
  (define-key *query-replace-map* #\C-d 'skip)
  (define-key *query-replace-map* #\Delete 'skip)
  (define-key *query-replace-map* #\C-h 'skip)
  (define-key *query-replace-map* #\n 'skip)
  (define-key *query-replace-map* #\! 'automatic)
  (define-key *query-replace-map* #\ESC 'quit)
  (define-key *query-replace-map* #\q 'quit)
  (define-key *query-replace-map* #\C-g 'quit)
  (define-key *query-replace-map* #\C-l 'recenter)
  (define-key *query-replace-map* #\. 'quit-and-back)
  (define-key *query-replace-map* #\^ 'undo)
  (define-key *query-replace-map* #\u 'undo))

(defun perform-replace (pattern replacement query regexp interactive-p noerror)
  (let ((literal (null regexp))
	(count 0)
	(nodup nil)
	(opoint (point))
	(not-found t)
        (def nil)
	(last-match nil)
	(last-match-char nil)
	(undo-bound (or interactive-p query)))
    (undo-boundary)
    (when interactive-p
      (if regexp
	  (setq *last-search-regexp* pattern
		*last-replace-regexp* replacement)
	(setq *last-search-string* pattern
	      *last-replace-string* replacement))
      (when query
	(toggle-ime nil)))
    (if regexp
	(setq pattern (compile-regexp pattern *case-fold-search*)))
    (loop
      (unless query
	(long-operation
	  (setq count (+ count
			 (replace-buffer pattern
					 replacement
					 :regexp regexp
					 :no-dup nodup
					 :last-match (cons last-match last-match-char)
					 :case-fold *case-fold-search*
					 :left-bound *word-search*
					 :right-bound *word-search*
					 :literal literal))))
	(when not-found
	  (setq not-found (zerop count)))
	(return))
      (unless (scan-buffer pattern
			   :regexp regexp
			   :no-dup nodup
			   :last-match (cons last-match last-match-char)
			   :case-fold *case-fold-search*
			   :left-bound *word-search*
			   :right-bound *word-search*)
	(return))
      (if (and (eql (match-beginning 0) (match-end 0))
	       (eql last-match (match-beginning 0)))
	  (setq nodup t)
	(progn
	  (setq not-found nil)
	  (show-match)
	  (unless (pos-visible-in-window-p (point))
	    (recenter))
	  (refresh-screen)
	  (message (if (eq def 'help)
		       "(y)置換, (n)スキップ, (!)残り全部, (u)アンドゥ, (C-g)中止, (.)中止して戻る"
		     "置換する?"))
	  (setq def (lookup-keymap *query-replace-map* (read-char *keyboard*) t))
	  (when (cond ((eq def 'act)
		       t)
		      ((eq def 'skip)
		       (setq nodup t)
		       nil)
		      ((eq def 'automatic)
		       (setq query nil)
		       t)
		      ((eq def 'undo)
		       (and (plusp count)
			    (undo)
			    (setq count (1- count)))
		       (setq nodup nil)
		       nil)
		      ((eq def 'quit-and-back)
		       (goto-char opoint)
		       (setq def 'quit)
		       (return))
		      ((eq def 'quit)
		       (return))
		      ((eq def 'recenter)
		       (recenter)
		       (setq nodup nil)
		       nil)
		      ((and (consp def)
			    (eq (car def) 'throw))
		       (throw (cdr def) count))
		      (t (setq def 'help)
			 (setq nodup nil)
			 nil))
	    (setq last-match-char (unless (= (match-end 0) (point-min))
				    (char-before (match-end 0))))
	    (unless (replace-match replacement :literal literal)
	      (return))
	    (setq last-match (point))
	    (setq nodup (= (match-beginning 0) (match-end 0)))
	    (when undo-bound
	      (undo-boundary))
	    (setq count (1+ count)))))
      (when (eobp)
	(return)))
    (hide-match)
    (unless noerror
      (and (eq def 'quit)
	   (quit))
      (and not-found
	   (error "文字列が見つかりません")))
    (when (and interactive-p
	       (null *executing-macro*))
      (message "~d個置換しました" count))
    count))

(defun replace-string (pattern replacement &optional noerror)
  (interactive "*sReplace string: \nswith: "
    :default0 *last-search-string* :history0 'search
    :default1 *last-replace-string* :history1 'search)
  (perform-replace pattern replacement nil nil (interactive-p) noerror))

(defun query-replace (pattern replacement &optional noerror)
  (interactive "*sQuery replace: \nswith: "
    :default0 *last-search-string* :history0 'search
    :default1 *last-replace-string* :history1 'search)
  (perform-replace pattern replacement t nil (interactive-p) noerror))

(defun replace-regexp (regexp replacement &optional noerror)
  (interactive "*sReplace regexp: \nswith: "
    :default0 *last-search-regexp* :history0 'search
    :default1 *last-replace-regexp* :history1 'search)
  (perform-replace regexp replacement nil t (interactive-p) noerror))

(defun query-replace-regexp (regexp replacement &optional noerror)
  (interactive "*sQuery replace regexp: \nswith: "
    :default0 *last-search-regexp* :history0 'search
    :default1 *last-replace-regexp* :history1 'search)
  (perform-replace regexp replacement t t (interactive-p) noerror))

(defun count-matches (regexp &optional literal nomsg)
  (interactive "sHow many matches for (regexp): "
    :default0 *last-search-regexp* :history0 'search)
  (when (interactive-p)
    (set (if literal '*last-search-string* '*last-search-regexp*) regexp))
  (unless literal
    (setq regexp (compile-regexp regexp *case-fold-search*)))
  (setq literal (not literal))
  (let ((count 0)
	(f nil))
    (save-excursion
      (while (scan-buffer regexp
			  :regexp literal
			  :case-fold *case-fold-search*
			  :left-bound *word-search*
			  :right-bound *word-search*
			  :no-dup f)
	(incf count)
	(setq f t)))
    (or nomsg
	(message "~d個の文字列があります" count))
    count))

(defun cfs2dialog (x)
  (cond ((null x) t)
	((eq x :smart) :disable)
	(t nil)))

(defun dialog2cfs (x)
  (cond ((null x) t)
	((eq x :disable) :smart)
	(t nil)))

(global-set-key #\C-s 'search-forward)
(global-set-key #\C-r 'search-backward)
(global-set-key #\F5 'repeat-forward-search)
(global-set-key #\S-F5 'repeat-backward-search)
(define-key esc-map #\s 're-search-forward)
(define-key esc-map #\C-s 're-search-backward)
(global-set-key #\M-r 'replace-string)
(global-set-key #\C-M-r 'query-replace)
