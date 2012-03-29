;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

;;; Author: 逸見さん(henmi@synform.co.jp)

(provide "css-mode")

(in-package "editor")

(export '(*css-mode-hook* *css-mode-map*
	  css-mode css2-mode css3-mode
	  *css-popup-completion-list*
	  *css-level*
	  *css-keyword-file*
	  *css3-keyword-files*
	  ))

;; -------------

(defvar *css-mode-hook* nil)
(defvar *css-popup-completion-list* nil)

(defvar *css-mode-map* nil)
(unless *css-mode-map*
  (setq *css-mode-map* (make-sparse-keymap))
  (define-key *css-mode-map* #\M-TAB 'css-completion)
)

(defvar *css-level* 3)
(defvar *css-keyword-hash-table-by-level* (make-hash-table))
(defvar *css-completion-list-by-level* (make-hash-table))

(defvar *css-keyword-file* "CSS")
(defvar *css3-keyword-files*
  '(;; CSS3 modules
    "CSS3/2d-transforms"
    "CSS3/3d-transforms"
    "CSS3/animations"
    "CSS3/backgrounds-and-borders"
    "CSS3/basic-box"
    "CSS3/basic-user-interface"
    "CSS3/behavioral-extensions"
    "CSS3/color"
    "CSS3/flexible-box-layout"
    "CSS3/fonts"
    "CSS3/generated-and-replaced-content"
    "CSS3/hyperlinks"
    "CSS3/layout"
    "CSS3/line"
    "CSS3/lists"
    "CSS3/marquee"
    "CSS3/multi-column"
    "CSS3/paged-media"
    "CSS3/presentation-levels"
    "CSS3/ruby"
    "CSS3/speech"
    "CSS3/tables"
    "CSS3/text"
    "CSS3/transitions"
    ;; vendor extensions
    "CSS3/microsoft-extensions"
    "CSS3/mozilla-extensions"
    "CSS3/webkit-extensions"
    ;; HTML elements
    "CSS3/html4-elements"
    "CSS3/html5-elements"
    ))
(defvar *css-keyword-file-symbol-by-level*
  (let ((h (make-hash-table)))
    (setf (gethash 2 h) '*css-keyword-file*)
    (setf (gethash 3 h) '*css3-keyword-files*)
    h))

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

(defmacro css-keyword-hash-table ()
  `(gethash *css-level* *css-keyword-hash-table-by-level*))

(defmacro css-completion-list ()
  `(gethash *css-level* *css-completion-list-by-level*))

(defun css-keyword-file ()
  (let ((sym (gethash *css-level* *css-keyword-file-symbol-by-level*)))
    (when sym
      (symbol-value sym))))

(defun css-mode-name ()
  (format nil "CSS~A" *css-level*))

(defun css-completion ()
  (interactive)
  (or (css-completion-list)
      (setf (css-completion-list) (make-list-from-keyword-table (css-keyword-hash-table)))
      (return-from css-completion nil))
  (let ((opoint (point)))
    (when (skip-syntax-spec-backward "w_")
      (let ((from (point)))
	(goto-char opoint)
	(do-completion from opoint :list-ignore-case (css-completion-list)
		       nil nil *css-popup-completion-list*)))))

(defun css-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'css-mode)
  (setq mode-name (css-mode-name))
  (use-keymap *css-mode-map*)
  (use-syntax-table *css-mode-syntax-table*)
#|  ;; タグ・ジャンプ
  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'tags-find-css-target)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'tags-find-css-point) |#
  ;; キーワードファイル
  (and (css-keyword-file)
       (null (css-keyword-hash-table))
       (setf (css-keyword-hash-table)
	     (load-keyword-file (css-keyword-file) t)))
  (when (css-keyword-hash-table)
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table (css-keyword-hash-table)))
  (run-hooks '*css-mode-hook*))

(defun css2-mode ()
  (interactive)
  (setf *css-level* 2)
  (css-mode))

(defun css3-mode ()
  (interactive)
  (setf *css-level* 3)
  (css-mode))
