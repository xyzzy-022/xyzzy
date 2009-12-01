;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "csearch")

(in-package "editor")

(export '(search-forward-continuously search-backward-continuously
	  re-search-forward-continuously re-search-backward-continuously
	  *continuous-search-map* next quit backward forward recenter))

(defvar *continuous-search-map* nil)
(unless *continuous-search-map*
  (setq *continuous-search-map* (make-sparse-keymap))
  (define-key *continuous-search-map* #\RET 'next)
  (define-key *continuous-search-map* #\LFD 'next)
  (define-key *continuous-search-map* #\SPC 'next)
  (define-key *continuous-search-map* #\C-g 'quit)
  (define-key *continuous-search-map* #\ESC 'quit)
  (define-key *continuous-search-map* #\r 'backward)
  (define-key *continuous-search-map* #\C-r 'backward)
  (define-key *continuous-search-map* #\s 'forward)
  (define-key *continuous-search-map* #\C-s 'forward)
  (define-key *continuous-search-map* #\C-l 'recenter))

(defun search-continuously (pattern reverse regexp interactive-p noerror)
  (while (search-command pattern reverse regexp interactive-p t)
    (unless (pos-visible-in-window-p (point))
      (recenter))
    (refresh-screen)
    (loop
      (let ((def (lookup-keymap *continuous-search-map* (read-char *keyboard*) t)))
	(cond ((eq def 'next)
	       (return))
	      ((eq def 'quit)
	       (hide-match)
	       (quit))
	      ((eq def 'backward)
	       (setq reverse t)
	       (return))
	      ((eq def 'forward)
	       (setq reverse nil)
	       (return))
	      ((eq def 'recenter)
	       (recenter)
	       (refresh-screen))
	      (t
	       (message "(RET)Œp‘±, (C-g)’†Ž~, (s)‘O‚Ö, (r)Œã‚ë‚Ö"))))))
  (hide-match)
  (or noerror
      (plain-error "•¶Žš—ñ‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ")))

(defun search-forward-continuously (pattern &optional noerror)
  (interactive "sSearch forward: "
    :default0 *last-search-string* :history0 'search)
  (search-continuously pattern nil nil (interactive-p) noerror))

(defun search-backward-continuously (pattern &optional noerror)
  (interactive "sSearch backward: "
    :default0 *last-search-string* :history0 'search)
  (search-continuously pattern t nil (interactive-p) noerror))

(defun re-search-forward-continuously (regexp &optional noerror)
  (interactive "sRe-search forward: "
    :default0 *last-search-regexp* :history0 'search)
  (search-continuously regexp nil t (interactive-p) noerror))

(defun re-search-backward-continuously (regexp &optional noerror)
  (interactive "sRe-search backward: "
    :default0 *last-search-regexp* :history0 'search)
  (search-continuously regexp t t (interactive-p) noerror))
