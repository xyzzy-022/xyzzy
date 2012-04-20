;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "kbdmacro")

(in-package "editor")

(export '(start-kbd-macro end-kbd-macro call-last-kbd-macro
	  name-last-kbd-macro insert-kbd-macro
	  save-last-kbd-macro load-kbd-macro
	  save-kbd-macro-to-file
	  *last-kbd-macro*))

(defvar *last-kbd-macro* nil)

(defun start-kbd-macro (&optional arg)
  (interactive "p")
  (when (kbd-macro-saving-p)
    (error "キーボードマクロの定義中です"))
  (start-save-kbd-macro)
  (when (and arg *last-kbd-macro*)
    (command-execute *last-kbd-macro*)))

(defun end-kbd-macro (&optional arg)
  (interactive "p")
  (unless (kbd-macro-saving-p)
    (error "キーボードマクロは定義していません"))
  (setq *last-kbd-macro* (stop-save-kbd-macro))
  (when (zerop (length *last-kbd-macro*))
    (setq *last-kbd-macro* nil))
  (when (and arg (/= arg 1))
    (call-last-kbd-macro (if (zerop arg) 0 (- arg 1)))))

(defun call-last-kbd-macro (&optional (arg 1))
  (interactive "p")
  (when *executing-macro*
    (error "キーボードマクロはすでに実行中です"))
  (when (kbd-macro-saving-p)
    (error "キーボードマクロの定義中です"))
  (unless *last-kbd-macro*
    (error "キーボードマクロは未定義です"))
  (let ((*executing-macro* t)
	(*prefix-value* arg))
    (command-execute *last-kbd-macro*)))

(defun name-last-kbd-macro (name)
  (interactive "sマクロにつける名前を入れて: ")
  (unless *last-kbd-macro*
    (error "キーボードマクロは定義されていません"))
  (when (zerop (length name))
    (error "名前を入れてって言ったっしょ"))
  (let ((symbol (intern name "user")))
    (when (fboundp symbol)
      (let ((def (symbol-function symbol)))
	(unless (stringp def)
	  (error "~aはキーボードマクロでないものが定義されています" name))
	(unless (y-or-n-p "~aのキーボードマクロ定義を置き換えますか?" name)
	  (return-from name-last-kbd-macro nil))))
    (setf (symbol-function symbol) *last-kbd-macro*)))

(defun insert-kbd-macro (command)
  (interactive "*CInsert kbd macro: ")
  (unless (fboundp command)
    (error "~aはコマンドが定義されていません" command))
  (let ((def (symbol-function command)))
    (unless (stringp def)
      (error "~aはキーボードマクロでないものが定義されています" command))
    (insert (format nil "(setf (symbol-function '~S)\n      ~S)\n"
		    command def))))

(define-history-variable *saved-kbd-macro-alist* nil)

(defun save-last-kbd-macro ()
  (interactive)
  (when *last-kbd-macro*
    (multiple-value-bind (result data)
	(dialog-box
	 '(dialog 0 0 187 135
	   (:caption "キーボードマクロの保存")
	   (:font 9 "MS UI Gothic")
	   (:control
	    (:combobox combo nil #x50210501 7 7 119 121)
	    (:button IDOK "OK" #x50010001 130 7 50 14)
	    (:button IDCANCEL "キャンセル" #x50010000 130 24 50 14)))
	 (list (cons 'combo (mapcar #'car *saved-kbd-macro-alist*)))
	 '((combo :non-null t :enable (IDOK))))
      (when result
	(let ((name (cdr (assoc 'combo data))))
	  (when name
	    (pushnew (cons name *last-kbd-macro*)
		     *saved-kbd-macro-alist*
		     :test #'string= :key #'car)))))))

(defun load-kbd-macro ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box
       '(dialog 0 0 187 135
	 (:caption "キーボードマクロの読み込み")
	 (:font 9 "MS UI Gothic")
	 (:control
	  (:listbox list nil #x50a10113 7 7 119 121)
	  (:button load "読み込み(&L)" #x50010001 130 7 50 14)
	  (:button exec "実行(&E)" #x50010000 130 24 50 14)
	  (:button remove "削除(&D)" #x50010000 130 41 50 14)
	  (:button IDCANCEL "キャンセル" #x50010000 130 58 50 14)))
       (list (cons 'list *saved-kbd-macro-alist*))
       '((list :must-match t :enable (load exec remove))))
    (let ((selected (cdr (assoc 'list data))))
      (when selected
	(cond ((eq result 'load)
	       (setq *last-kbd-macro* (cdr selected)))
	      ((eq result 'exec)
	       (command-execute (cdr selected)))
	      ((eq result 'remove)
	       (setq *saved-kbd-macro-alist*
		     (delete selected *saved-kbd-macro-alist* :test #'eq))))))))

(defun save-kbd-macro-to-file ()
  (interactive)
  (let ((filename (file-name-dialog :title "キーボードマクロを保存"
				    :filter '(("Lispファイル(*.l;*.lisp)" . "*.l;*.lisp")
					      ("すべてのファイル(*.*)" . "*.*")))))
    (when filename
      (with-open-file (s filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(format s ";;; This file contains keyboard macro definition.~%")
	(format-date s ";;;   %d %b %Y %H:%M:%S %Z\n\n")
	(format s "(setq editor::*saved-kbd-macro-alist*~%")
	(format s "  '(~{~S~^~%    ~}))~%" *saved-kbd-macro-alist*)))))

(define-key ctl-x-map #\( 'start-kbd-macro)
(define-key ctl-x-map #\) 'end-kbd-macro)
(define-key ctl-x-map #\e 'call-last-kbd-macro)
