;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

;;; Author: 逸見さん(henmi@synform.co.jp)

(provide "css-mode")

(in-package "editor")

(export '(*css-mode-hook* *css-mode-map* css-mode
	  *css-popup-completion-list*))

;; -------------

(defvar *css-mode-hook* nil)
(defvar *css-popup-completion-list* nil)

(defvar *css-mode-map* nil)
(unless *css-mode-map*
  (setq *css-mode-map* (make-sparse-keymap))
  (define-key *css-mode-map* #\M-TAB 'css-completion)
)

(defvar *css-keyword-hash-table* nil)
(defvar *css-keyword-file* "css")

(defvar *css-completion-list* nil)

(defvar *css-mode-syntax-table* nil)
(unless *css-mode-syntax-table*
  (setq *css-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *css-mode-syntax-table* c))))
;  (set-syntax-option *css-mode-syntax-table* 1)
  (set-syntax-string *css-mode-syntax-table* #\")
  (set-syntax-string *css-mode-syntax-table* #\')
;  (set-syntax-escape *css-mode-syntax-table* #\\)
  (set-syntax-symbol *css-mode-syntax-table* #\_)
  (set-syntax-symbol *css-mode-syntax-table* #\-)  ;追加
  (set-syntax-symbol *css-mode-syntax-table* #\#)
  (set-syntax-match *css-mode-syntax-table* #\( #\))
  (set-syntax-match *css-mode-syntax-table* #\{ #\})
  (set-syntax-match *css-mode-syntax-table* #\[ #\])
  (set-syntax-start-multi-comment *css-mode-syntax-table* "/*")
  (set-syntax-end-multi-comment *css-mode-syntax-table* "*/"))

(defun css-completion ()
  (interactive)
  (or *css-completion-list*
      (setq *css-completion-list* (make-list-from-keyword-table *css-keyword-hash-table*))
      (return-from css-completion nil))
  (let ((opoint (point)))
    (when (skip-syntax-spec-backward "w_")
      (let ((from (point)))
	(goto-char opoint)
	(do-completion from opoint :list-ignore-case *css-completion-list*
		       nil nil *css-popup-completion-list*)))))

(defun css-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'css-mode)
  (setq mode-name "css")
  (use-keymap *css-mode-map*)
  (use-syntax-table *css-mode-syntax-table*)
#|  ;; タグ・ジャンプ
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'tags-find-css-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'tags-find-css-point) |#
  ;; キーワードファイル
  (and *css-keyword-file*
       (null *css-keyword-hash-table*)
       (setq *css-keyword-hash-table*
	     (load-keyword-file *css-keyword-file* t))) ;;990608 tを追加
  (when *css-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *css-keyword-hash-table*))
  (run-hooks '*css-mode-hook*))

