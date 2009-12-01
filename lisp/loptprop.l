;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "loptprop")

(in-package "editor")

(export '(add-local-option-prop-page local-option-property-sheet))

(defvar *local-option-prop-page* '(color-page))
(defvar *local-option-prop-page-no* nil)

(defun add-local-option-prop-page (ident tmpl init handlers result)
  (setq *local-option-prop-page* (delete ident *local-option-prop-page* :test #'eq
					 :key #'(lambda (x) (if (consp x) (car x) nil))))
  (setf (get ident 'local-prop-result) result)
  (push (list ident tmpl init handlers) *local-option-prop-page*))

(defun local-option-property-sheet ()
  (interactive)
  (let (sheet result)
    (dolist (page *local-option-prop-page*)
      (cond ((symbolp page)
	     (push page sheet))
	    (t
	     (push (list (car page)
			 (cadr page)
			 (let ((init (caddr page)))
			   (when init
			     (funcall init)))
			 (cadddr page))
		   sheet))))
    (let* ((dir (default-directory))
	   (colors (assoc-if #'(lambda (x)
				 (path-equal (if (consp x) (car x) x) dir))
			     *directory-buffer-color-alist*))
	   (*color-page-enable-dir-p* (cdr colors))
	   (*color-page-enable-subdir-p* (and colors (consp (car colors)) (cdar colors))))
      (multiple-value-setq (result *local-option-prop-page-no*)
	(property-sheet sheet "ローカル設定" *local-option-prop-page-no*))
      (dolist (r result)
	(let ((f (get (car r) 'local-prop-result)))
	  (when f
	    (funcall f (cdr r))))))))

(define-history-variable *directory-buffer-color-alist* nil)

(setq *change-buffer-colors-hook*
      #'(lambda (buffer colors subdirp)
	  (let ((dir (default-directory buffer)))
	    (setq *directory-buffer-color-alist*
		  (delete-if #'(lambda (x)
				 (path-equal (if (consp x) (car x) x) dir))
			     *directory-buffer-color-alist*
			     :key #'car))
	    (when (or colors (cdr (find-buffer-colors buffer)))
	      (push (cons (cons dir subdirp) colors) *directory-buffer-color-alist*))
	    (unless colors
	      (setq *directory-buffer-color-alist*
		    (delete-if #'(lambda (x)
				   (unless (cdr x)
				     (let ((c nil))
				       (dolist (y *directory-buffer-color-alist*)
					 (and (not (eq x y)) (consp (car y)) (cdar y)
					      (sub-directory-p (caar x) (caar y))
					      (or (null c)
						  (sub-directory-p (caar y) (caar c)))
					      (setq c y)))
				       (or (null c) (null (cdr c))))))
			       *directory-buffer-color-alist*)))
	    (dolist (buffer (buffer-list))
	      (let ((colors (find-buffer-colors buffer)))
		(when colors
		  (set-buffer-colors (cdr colors) buffer)))))))

(defun set-buffer-local (var val)
  (unless (equal val (buffer-local-value (selected-buffer) var))
    (make-local-variable var)
    (set var val)))

(add-local-option-prop-page
 'backup-file
 *backup-prop-page-template*
 #'(lambda ()
     (list (cons 'backup make-backup-files)
	   (cons 'numbered (and version-control (not (eq version-control 'never))))
	   (cons 'ifexist (null version-control))
	   (cons 'never (eq version-control 'never))
	   (cons 'overspin kept-old-versions)
	   (cons 'nverspin kept-new-versions)
	   (cons 'pack pack-backup-file-name)
	   (cons 'always make-backup-file-always)))
 *backup-prop-page-handlers*
 #'(lambda (data)
     (set-buffer-local 'make-backup-files (cdr (assoc 'backup data)))
     (when make-backup-files
       (set-buffer-local 'version-control (cond ((cdr (assoc 'numbered data))
						 t)
						((cdr (assoc 'ifexist data))
						 nil)
						(t
						 'never)))
       (set-buffer-local 'kept-old-versions (cdr (assoc 'oldver data)))
       (set-buffer-local 'kept-new-versions (cdr (assoc 'newver data)))
       (set-buffer-local 'pack-backup-file-name (cdr (assoc 'pack data)))
       (set-buffer-local 'make-backup-file-always (cdr (assoc 'always data))))))

(add-local-option-prop-page
 'lock
 '(dialog 0 0 163 113
   (:caption "排他制御")
   (:control
    (:button nil "排他制御" #x50000007 7 7 149 60)
    (:button no "しない(&N)" #x50030009 15 22 43 10)
    (:button yes "する(&Y)" #x50000009 15 36 55 10)
    (:button edit "編集してるときだけ(&E)" #x50000009 15 50 79 10)
    (:button way "ロックの方法" #x50000007 7 71 149 35)
    (:button share "共有ロック(&S)" #x50030009 15 87 55 10)
    (:button exclusive "排他ロック(&E)" #x50000009 84 87 55 10)))
 #'(lambda ()
     (list (cons 'no (null lock-file))
	   (cons 'yes (and lock-file (not (eq lock-file :edit))))
	   (cons 'edit (eq lock-file :edit))
	   (cons 'share (null exclusive-lock-file))
	   (cons 'exclusive exclusive-lock-file)))
 '((no :disable (way share exclusive))
   (yes :enable (way share exclusive))
   (edit :enable (way share exclusive)))
 #'(lambda (data)
     (let* ((lock (cond ((cdr (assoc 'yes data))
			 t)
			((cdr (assoc 'edit data))
			 :edit)
			(t nil)))
	    (ex (if lock
		    (cdr (assoc 'exclusive data))
		  exclusive-lock-file)))
       (unless (and (eq lock lock-file)
		    (eq ex exclusive-lock-file))
	 (setq lock-file lock)
	 (when lock-file
	   (setq exclusive-lock-file ex))
	 (unlock-file)
	 (when (cond ((eq lock-file :edit)
		      (buffer-modified-p))
		     (lock-file t)
		     (t nil))
	   (handler-case
	       (when (eq (lock-file) :shared)
		 (setq buffer-read-only t)
		 (message "~aは他のプロセスが上書き禁止で開いています"
			  (get-buffer-file-name)))
	     (file-error (c)
	       (setq buffer-read-only t)
	       (si:*print-condition c))))))))

(add-local-option-prop-page
 'misc
 '(dialog 0 0 213 165
   (:caption "もろもろ")
   (:control
    (:static nil "文字エンコーディング(&E):" #x50020000 8 7 71 8)
    (:combobox encoding nil #x50210003 7 18 112 139)
    (:button nil "改行コード" #x50000007 7 37 56 60)
    (:button dos "CR+LF(&D)" #x50030009 11 50 45 10)
    (:button unix "LF(&X)" #x50000009 11 66 32 10)
    (:button mac "CR(&Y)" #x50000009 11 82 34 10)
    (:button nil "タブの幅" #x50000007 66 37 63 60)
    (:button deftab "デフォルト(&O)" #x50030009 71 49 55 10)
    (:button localtab "ローカル(&L):" #x50000009 71 65 50 10)
    (:edit tab nil #x50830086 80 78 40 14)
    (:spin tabspin nil #x500000b6 114 85 10 14)
    (:button nil "テキストの折り返し" #x50000007 133 21 75 76)
    (:button foldno "しない(&F)" #x50030009 137 34 42 10)
    (:button foldwin "ウィンドウの幅(&W)" #x50000009 137 49 67 10)
    (:button foldcol "位置指定(&C):" #x50000009 137 65 54 10)
    (:edit column nil #x50810086 147 78 40 14)
    (:spin colspin nil #x500000b6 183 77 9 14)
    (:static nil "アンドゥの回数(&U):" #x50020000 9 106 60 8)
    (:edit undo nil #x50810086 69 102 39 14)
    (:spin undospin nil #x500000b6 106 102 9 14)
    (:button autosave "自動セーブする(&A)" #x50010003 7 123 77 10)
    (:button readonly "書き込み禁止(&R)" #x50010003 106 123 77 10)
    (:button newline "行番号は表示行(&N)" #x50010003 7 136 86 10)
    (:button indenttab "インデントにタブを使う(&T)" #x50010003 106 136 88 10)
    (:button noneedsave "保存してなくても文句を言わない(&V)" #x50010003 7 149 133 10)))
 #'(lambda ()
     (let ((eol (buffer-eol-code)))
       (multiple-value-bind (tab localtab)
	   (tab-columns (selected-buffer))
	 (list (cons 'encoding (cdr *char-encoding-list*))
	       (cons 'encoding (or (position (buffer-fileio-encoding)
					     (cdr *char-encoding-list*))
				   t))
	       (cons 'unix (eql eol *eol-lf*))
	       (cons 'dos (eql eol *eol-crlf*))
	       (cons 'mac (eql eol *eol-cr*))
	       (cons 'deftab (null localtab))
	       (cons 'localtab localtab)
	       (cons 'tabspin tab)
	       (cons 'foldno (null (buffer-fold-width)))
	       (cons 'foldwin (eq (buffer-fold-width) 't))
	       (cons 'foldcol (integerp (buffer-fold-width)))
	       (cons 'colspin (if (integerp (buffer-fold-width))
				  (buffer-fold-width)
				buffer-fold-width))
	       (cons 'newline (buffer-line-number-mode))
	       (cons 'undospin kept-undo-information)
	       (cons 'autosave auto-save)
	       (cons 'readonly buffer-read-only)
	       (cons 'noneedsave need-not-save)
	       (cons 'indenttab indent-tabs-mode)))))
 '((encoding :index t)
   (undospin :min 0 :max 10000)
   (undo :min 0 :range-error "0以上を指定してください"
	 :type integer :type-error "数値を入力してください")
   (colspin :min 4 :max 30000)
   (column :type integer :min 4 :max 30000
	   :range-error "折り返しは4から30000です"
	   :type-error "数値を入力してください")
   (tabspin :min 1 :max 32)
   (tab :type integer :min 1 :max 32
	:range-error "1から32を指定してください"
	:type-error "数値を入力してください"))
 #'(lambda (data)
     (let ((n (cdr (assoc 'encoding data))))
       (when n
	 (set-buffer-fileio-encoding (nth n (cdr *char-encoding-list*)))))
     (set-buffer-eol-code (cond ((cdr (assoc 'unix data)) *eol-lf*)
				((cdr (assoc 'dos data)) *eol-crlf*)
				(t *eol-cr*)))
     (let ((x (cond ((cdr (assoc 'foldno data)) 'nil)
		    ((cdr (assoc 'foldwin data)) 't)
		    (t (cdr (assoc 'column data))))))
       (unless (eql x (buffer-fold-width))
	 (set-buffer-fold-width x)
	 (when (integerp (buffer-fold-width))
	   (setq buffer-fold-width (buffer-fold-width)))))
     (let ((x (cdr (assoc 'newline data))))
       (or (eq x (buffer-line-number-mode))
	   (set-buffer-line-number-mode x)))
     (set-tab-columns (and (cdr (assoc 'localtab data))
			   (cdr (assoc 'tab data)))
		      (selected-buffer))
     (set-buffer-local 'auto-save (cdr (assoc 'autosave data)))
     (set-buffer-local 'buffer-read-only (cdr (assoc 'readonly data)))
     (set-buffer-local 'need-not-save (cdr (assoc 'noneedsave data)))
     (set-buffer-local 'kept-undo-information (cdr (assoc 'undo data)))
     (set-buffer-local 'indent-tabs-mode (cdr (assoc 'indenttab data)))))

