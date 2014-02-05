;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "filer")

(in-package "editor")

(export '(*filer-path-masks* filer-keymap *filer-directories*
	  filer-backward-line filer-backward-page filer-chdir
	  filer-up-directory filer-goto-root
	  filer-toggle-mark-files filer-mark-all-files
	  filer-ok filer-cancel
	  filer-toggle-mark-and-forward-line filer-ok filer-cancel
	  filer-change-drive filer-create-directory
	  filer-scroll-left-or-left-window
	  filer-scroll-right-or-right-window
	  filer-scroll-or-left-window-or-up
	  filer-scroll-or-right-window-or-up
	  filer-set-path-mask filer-mark-path-mask
	  filer-jump-directory filer-execute-application
	  filer-copy filer-copy-as filer-move filer-delete filer-modify-attributes
	  filer-extract-archive filer-create-archive
	  filer-extract-archive-to-current filer-search
	  filer-shell-execute filer-rename open-filer
	  filer-sync-with-primary filer-sync-with-secondary
	  filer-create-shortcut-to-desktop filer-create-shortcut
	  filer-input-file-name filer-list-archive
	  filer-load-file filer-sort-items filer-compare-file
	  filer-eject-media filer-search-name filer-view-file
	  *filer-guide-text* filer-file-property
	  filer-send-to *filer-send-to-list*
	  *filer-modal* *filer-follow-links*
	  filer-grep filer-gresreg filer-byte-compile
	  filer-close-modal
	  filer-isearch-forward filer-isearch-backward
	  *filer-use-recycle-bin*
	  *filer-use-shell-file-operation*
	  filer-delete-mask *filer-delete-mask-list*))

(setq *filer-guide-text*
      '("A:属性  C:コピー  D:削除  E:解凍  F:検索  J:DIR移動  \
K:mkdir  L:ドライブ  M:移動  N:ファイル名  O:圧縮  P:DIR??"
	"R:名前変更  S:DIR??  T:ショートカット  U:DIRサイズ  V:リスト  X:実行  .:マスク  \
/:マーク  *:load  Home:トグル  S-Home:クリア  End:リロード  F3:実行"))

(defvar *filer-path-masks*
  '(("すべてのファイル" "*")
    ("ドキュメント" "*.doc" "*.txt" "*.man" "*readme*")
    ("C/C++" "*.c" "*.h" "*.cpp" "*.cxx" "*.cc" "*.inl")
    ("Lisp" "*.l" "*.el" "*.lsp" "*.lisp")
    ("Visual Basic" "*.bas" "*.frm" "*.vbp")
    ("実行ファイル" "*.exe" "*.com" "*.bat" "*.pif")
    ("アーカイブ" . ("*.lzh" "*.zip" "*.arj" "*.tar" "*.bz2" "*.gz"
		 "*.Z" "*.tbz" "*.taz" "*.tgz" "*.cab" "*.rar"
		 "*.gza" "*.bza" "*.yz1" "*.7z" "*.lzma" "*.xz"))
    ("バックアップ" "*~" "*.bak")
    ("自動セーブ" "#*#")))

(defvar *filer-directories*
  '(("msdev/include" . "c:/msdev/include")
    ("MFC/src" . "c:/msdev/mfc/src")
    ("MFC/include" . "c:/msdev/mfc/include")
    ("NT" . "c:/winnt40")
    ("tmp" . "d:/tmp")))

;; 設定方法は *filer-directories* とだいたい同じ
(defvar *filer-send-to-list* nil)

(defvar *filer-delete-mask-list*
  '(("バックアップ" "*~" "*.bak")
    ("自動セーブ" "#*#")))

(define-history-variable *filer-archive-directory-history* nil)

(define-history-variable *filer-search-directory-history* nil)
(define-history-variable *filer-search-pattern-history* nil)

(define-history-variable *filer-directory-history* nil)

(defvar *filer-retrieve-icon* nil)
(defvar *filer-query-before-process* nil)
(defvar *filer-query-delete-precisely* nil)
(defvar *filer-ask-same-name-default* :skip)
(defvar *filer-ask-same-name-all-default* nil)
(defvar *filer-ask-same-name-make-default* t)
(defvar *filer-delete-non-empty-directory* :force)
(defvar *filer-delete-read-only-files* nil)
(defvar *filer-follow-links* t)
(defvar *filer-use-recycle-bin* nil)
(defvar *filer-use-shell-file-operation* nil)
(defvar *filer-delete-mask* nil)
(define-history-variable *filer-delete-mask-history* nil)

(defvar *filer-modal* t)

(defun filer-backward-line (&optional (arg 1))
  (filer-forward-line (- arg)))

(defun filer-backward-page (&optional (arg 1))
  (filer-forward-page (- arg)))

(defun filer-chdir (dir &optional s)
  (when (filer-set-directory dir s)
    (filer-reload nil s)
    (add-history (filer-get-directory s)
		 '*filer-directory-history*)))

(defun filer-up-directory ()
  (filer-chdir ".."))

(defun filer-goto-root ()
  (filer-chdir "/"))

(defun filer-toggle-mark-and-forward-line ()
  (filer-toggle-mark)
  (filer-forward-line 1))

(defun filer-toggle-mark-files ()
  (filer-toggle-all-marks t))

(defun filer-mark-all-files ()
  (filer-mark-all t))

(defun filer-focus ()
  (if (filer-left-window-p)
      (filer-left-window)
    (filer-right-window)))

(defun filer-readin-1 (file)
  (if (listp file)
      (mapc #'filer-readin-1 file)
    (unless (file-directory-p file)
      (find-file file)
      (add-history file '*minibuffer-file-name-history*)))
  t)

(defun filer-readin (file)
  (filer-readin-1 file)
  (si:*activate-toplevel))

(defun filer-open-in-archive (arcname filename)
  (find-file-in-archive arcname filename)
  (si:*activate-toplevel))

(defun filer-read-link (file)
  (or (and (pathname-match-p file "*.lnk")
	   (ignore-errors (resolve-shortcut file)))
      file))

(defun filer-ok ()
  (let ((file (filer-get-text)))
    (cond (file
	   (filer-set-text "")
	   (setq file (merge-pathnames file (filer-get-directory)))
	   (when *filer-follow-links*
	     (setq file (filer-read-link file)))
	   (cond ((wild-pathname-p file)
		  (filer-focus)
		  (filer-set-directory (directory-namestring file))
		  (filer-reload (file-namestring file)))
		 ((file-directory-p file)
		  (filer-focus)
		  (filer-chdir file))
		 (t
		  (check-valid-pathname file)
		  (if (filer-modal-p)
		      (filer-close file)
		    (filer-readin file)))))
	  (t
	   (let ((files (filer-get-mark-files t)))
	     (cond (files
		    (when *filer-follow-links*
		      (setq files (mapcar #'filer-read-link files)))
		    (if (filer-modal-p)
			(filer-close files)
		      (filer-readin files)))
		   ((and (setq files (filer-get-mark-files nil))
			 (null (cdr files)))
		    (filer-chdir (car files)))
		   ((filer-current-file-dot-dot-p)
		    (filer-up-directory))
		   ((setq files (filer-get-current-file))
		    (when *filer-follow-links*
		      (setq files (filer-read-link files)))
		    (if (file-directory-p files)
			(filer-chdir files)
		      (if (filer-modal-p)
			  (filer-close files)
			(filer-readin files))))
		   (t nil)))))))

(defun filer-cancel ()
  (filer-close nil))

(defun filer-change-drive ()
  (let ((drive (drive-dialog (filer-get-drive))))
    (when drive
      (let ((path (merge-pathnames (format nil "~A:" drive)
				   (filer-get-directory))))
	(filer-chdir (if (file-directory-p path)
			 path
		       (format nil "~A:/" drive)))))))

(defun filer-scroll-left-or-left-window ()
  (if (filer-dual-window-p)
      (filer-left-window)
    (filer-scroll-left)))

(defun filer-scroll-right-or-right-window ()
  (if (filer-dual-window-p)
      (filer-right-window)
    (filer-scroll-right)))

(defun filer-scroll-or-left-window-or-up ()
  (cond ((filer-dual-window-p)
	 (if (filer-left-window-p)
	     (filer-up-directory)
	   (filer-left-window)))
	((filer-scroll-left))
	(t (filer-up-directory))))

(defun filer-scroll-or-right-window-or-up ()
  (if (filer-dual-window-p)
      (if (not (filer-left-window-p))
	  (filer-up-directory)
	(filer-right-window))
    (filer-scroll-right)))

(defvar *filer-path-mask-dialog-template*
  '(dialog 0 0 219 135
    (:caption "パスマスク")
    (:font 9 "MS UI Gothic")
    (:control
     (:listbox list nil #x50a10111 7 7 150 104)
     (:static nil "マスク(&M):" #x50020000 7 118 27 8)
     (:edit mask nil #x50810080 38 115 119 14)
     (:button IDOK "OK" #x50010001 162 7 50 14)
     (:button both "両方の窓(&B)" #x50010000 162 24 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 162 41 50 14))))

(defun filer-set-path-mask ()
  (multiple-value-bind (result data)
      (let ((default '("デフォルト")))
	(dialog-box *filer-path-mask-dialog-template*
		    `((list . ,(cons default *filer-path-masks*))
		      (list . ,default))
		    `((list :column (50) :invalidate (mask))
		      (mask :invalidate (list))
		      ,@(unless (filer-dual-window-p)
			  '((both :disable :disable))))))
    (when result
      (let ((mask (let ((mask (cdr (assoc 'mask data))))
		    (or (and mask
			     (split-string mask #\; t " "))
			(cddr (assoc 'list data))))))
	(filer-set-file-mask mask)
	(filer-reload)
	(when (eq result 'both)
	  (filer-set-file-mask mask t)
	  (filer-reload nil t))))))

(defvar *filer-mark-mask-dialog-template*
  '(dialog 0 0 219 135
    (:caption "まとめてマーク")
    (:font 9 "MS UI Gothic")
    (:control
     (:listbox list nil #x50a10111 7 7 150 104)
     (:static nil "マスク(&M):" #x50020000 7 118 27 8)
     (:edit mask nil #x50810080 38 115 119 14)
     (:button IDOK "OK" #x50010001 162 7 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 162 24 50 14))))

(defun filer-mark-path-mask ()
  (multiple-value-bind (result data)
      (dialog-box *filer-mark-mask-dialog-template*
		  `((list . ,*filer-path-masks*))
		  '((list :column (50) :invalidate (mask))
		    (mask :invalidate (list))))
    (when result
      (let ((mask (cdr (assoc 'mask data))))
	(filer-mark-match-files
	 (or (and mask
		  (split-string mask #\; t " "))
	     (cddr (assoc 'list data))))))))

(defvar *filer-jump-directory-dialog-template*
  '(dialog 0 0 219 135
    (:caption "ディレクトリ移動")
    (:font 9 "MS UI Gothic")
    (:control
     (:listbox list nil #x50b10111 7 7 150 104)
     (:static nil "パス(&P):" #x50020000 7 118 22 8)
     (:edit path nil #x50810080 38 115 119 14)
     (:button IDOK "OK" #x50010001 162 7 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 162 24 50 14))))

(defun filer-jump-directory ()
  (multiple-value-bind (result data)
      (dialog-box *filer-jump-directory-dialog-template*
		  `((list ("(カレントディレクトリ)" . ,(default-directory))
			  ,@*filer-directories*
			  ,@(mapcar #'(lambda (x) (cons x x))
				    *filer-directory-history*)))
		  '((list :column (100) :invalidate (path))
		    (path :invalidate (list))))
    (when result
      (let ((path (string-trim " " (cdr (assoc 'path data)))))
	(when (zerop (length path))
	  (setq path (cddr (assoc 'list data))))
	(and path
	     (filer-chdir path))))))

(defun filer-execute-application ()
  (let ((*launch-app-directory* (filer-get-directory)))
    (launch-application-dialog
     (format nil "~{~A~^ ~}" (mapcar #'(lambda (x)
					 (map-slash-to-backslash
					  (if (find #\SPC x)
					      (concat "\"" x "\"")
					    x)))
				      (cons (filer-get-current-file)
					    (filer-get-mark-files)))))))

(defun filer-ask-same-name-file (from-file to-file op)
  (flet ((date (file)
	   (multiple-value-bind (sec min hour day mon year)
	       (decode-universal-time (file-write-time file))
	     (format nil "~4,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
		     year mon day hour min sec)))
	 (bytes (file)
	   (format nil "~dbyte~:p"
		   (file-length file))))
    (multiple-value-bind (result data)
	(dialog-box
	 '(dialog 0 0 235 153
	   (:caption "同名ファイルの確認")
	   (:font 9 "MS UI Gothic")
	   (:control
	    (:button text nil #x50000007 7 7 166 55)
	    (:button yes "する(&Y)" #x50000009 15 20 37 10)
	    (:button no "しない(&S)" #x50000009 15 32 42 10)
	    (:button newer "日付が新しければ(&N)" #x50000009 15 44 84 10)
	    (:button all "いちいち聞くな(&A)" #x50010003 15 67 66 10)
	    (:static from1 nil #x50020080 7 85 220 8)
	    (:static from2 nil #x50020080 13 95 214 8)
	    (:static nil "サイズ:" #x50020080 13 105 19 8)
	    (:static fromsize nil #x50020080 40 105 52 8)
	    (:static nil "更新日時:" #x50020080 113 105 30 8)
	    (:static fromdate nil #x50020080 149 105 60 8)
	    (:static to1 nil #x50020080 7 117 220 8)
	    (:static to2 nil #x50020080 13 126 214 8)
	    (:static nil "サイズ:" #x50020080 13 135 19 8)
	    (:static tosize nil #x50020080 40 135 52 8)
	    (:static nil "更新日時:" #x50020080 113 135 30 8)
	    (:static todate nil #x50020080 149 135 60 8)
	    (:button IDOK "OK" #x50010001 177 7 50 14)
	    (:button IDCANCEL "キャンセル" #x50010000 177 26 50 14)))
	 (list
	  (cons 'text (concat "同じ名前のファイルがすでに存在します。"
			      op "する?"))
	  (cons 'from1 (concat op "元:"))
	  (cons 'from2 from-file)
	  (cons 'fromsize (bytes from-file))
	  (cons 'fromdate (date from-file))
	  (cons 'to1 (concat op "先:"))
	  (cons 'to2 to-file)
	  (cons 'tosize (bytes to-file))
	  (cons 'todate (date to-file))
	  (cons 'yes (eq *filer-ask-same-name-default* :overwrite))
	  (cons 'no (eq *filer-ask-same-name-default* :skip))
	  (cons 'newer (eq *filer-ask-same-name-default* :newer))
	  (cons 'all *filer-ask-same-name-all-default*))
	 nil)
      (let ((r (cond ((null result)
		      (quit))
		     ((cdr (assoc 'yes data))
		      :overwrite)
		     ((cdr (assoc 'newer data))
		      :newer)
		     (t :skip)))
	    (all (cdr (assoc 'all data))))
	(when *filer-ask-same-name-make-default*
	  (setq *filer-ask-same-name-default* r)
	  (setq *filer-ask-same-name-all-default* all))
	(values r all)))))

(defun filer-ask-read-only-file (file op)
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 215 77
		    (:caption "書き込み禁止ファイルの確認")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "書込み禁止ファイルです。強制的に上書きしますか?" #x50020000 7 7 146 8)
		     (:static to nil #x50020080 7 31 144 8)
		     (:static file nil #x50020080 7 41 144 8)
		     (:button IDOK "はい(&Y)" #x50010000 153 7 55 14)
		     (:button IDCANCEL "いいえ(&N)" #x50010001 153 24 55 14)
		     (:button all "すべて上書き(&A)" #x50010000 153 41 55 14)))
		  (list (cons 'file file)
			(cons 'to (concat op "先:")))
		  nil)
    (cond ((eq result 'IDOK)
	   :force)
	  ((eq result 'all)
	   :all)
	  (t
	   :skip))))

(defun ask-file-error (e)
  (let ((answer (message-box (si:*condition-string e) nil
			     '(:abort-retry-ignore :exclamation))))
    (cond ((eq answer :retry)
	   t)
	  ((eq answer :ignore)
	   nil)
	  (t
	   (plain-error)))))

(defun filer-copy-file (from-file to-file)
  (let ((ie if-exists)
	(iad if-access-denied)
	all)
    (tagbody
      retry
      (message "~A から ~A へコピー中..." from-file to-file)
      (do-events)
      (handler-case (copy-file from-file to-file
			       :if-exists ie
			       :if-access-denied iad
			       :copy-attributes t)
	(file-exists ()
	  (multiple-value-setq (ie all)
	    (filer-ask-same-name-file from-file to-file "コピー"))
	  (when all
	    (setq if-exists ie))
	  (unless (eq ie ':skip)
	    (go retry)))
	(access-denied ()
	  (setq iad (filer-ask-read-only-file to-file "コピー"))
	  (when (eq iad ':all)
	    (setq iad ':force)
	    (setq if-access-denied ':force))
	  (unless (eq iad ':skip)
	    (go retry)))
	(file-error (e)
	  (when (ask-file-error e)
	    (go retry)))))))

(defun filer-do-copy (files to-dir dirl)
  (dolist (file files)
    (let ((name (substring file dirl)))
      (cond ((file-directory-p file)
	     (create-directory (concat to-dir name) :if-exists :skip)
	     (filer-do-copy (directory file :absolute t)
			    (concat to-dir name)
			    (length file)))
	    (t
	     (filer-copy-file file (concat to-dir name)))))))

(defun filer-do-copy-by-shell (files to-dir &optional dirl)
  (si:*file-operation :copy files to-dir
		      :no-confirm-mkdir t))

(defun filer-do-move (files to-dir dirl)
  (dolist (file files)
    (let ((name (substring file dirl)))
      (let ((to-file (concat to-dir name))
	    (ie if-exists)
	    (iad if-access-denied))
	(tagbody
	  retry
	  (message "~A から ~A へ移動中..." file to-file)
	  (do-events)
	  (handler-case (rename-file file to-file
				     :if-exists ie
				     :if-access-denied iad)
	    (file-exists ()
	      (let (all)
		(multiple-value-setq (ie all)
		  (filer-ask-same-name-file file to-file "移動"))
		(when all
		  (setq if-exists ie))
		(unless (eq ie ':skip)
		  (go retry))))
	    (access-denied ()
	      (cond ((file-directory-p file)
		     (create-directory to-file :if-exists :skip)
		     (filer-do-move (directory file :absolute t)
				    to-file (length file))
		     (ignore-errors (delete-directory file)))
		    (t
		     (setq iad (filer-ask-read-only-file to-file "移動"))
		     (when (eq iad ':all)
		       (setq iad ':force)
		       (setq if-access-denied ':force))
		     (unless (eq iad ':skip)
		       (go retry)))))
	    (file-error (e)
	      (when (ask-file-error e)
		(go retry)))))))))

(defun filer-do-move-by-shell (files to-dir &optional dirl)
  (si:*file-operation :move files to-dir
		      :no-confirm-mkdir t))

(defun filer-copy-1 (fn mv dest)
  (let ((if-exists :error)
	(if-access-denied :error))
    (declare (special if-exists if-access-denied))
    (long-operation
      (let (files
	    (from (filer-get-directory))
	    (to (or dest (filer-get-directory t))))
	(cond ((path-equal from to)
	       (or mv (filer-copy-as)))
	      (t
	       (when mv
		 (filer-subscribe-to-reload from t))
	       (filer-subscribe-to-reload to t)
	       (dolist (file (filer-get-mark-files))
		 (unless (sub-directory-p to file)
		   (push file files)))
	       (when (and files
			  (or (null *filer-query-before-process*)
			      (yes-or-no-p "選択されたファイルを~%~A~%に~Aしますかい?"
					   to (if mv "移動" "コピー"))))
		 (funcall fn (nreverse files) to (length from))))))
      (filer-clear-all-marks))
    (message "done.")))

(defun filer-copy (&optional dest)
  (filer-copy-1 (if *filer-use-shell-file-operation*
		    #'filer-do-copy-by-shell
		  #'filer-do-copy)
		nil dest))

(defun filer-move (&optional dest)
  (filer-copy-1 (if *filer-use-shell-file-operation*
		    #'filer-do-move-by-shell
		  #'filer-do-move)
		t dest))

(defun filer-get-copy-file-name (name)
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 215 71
		    (:caption "別名でコピー")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "新しいファイル名(&N):" #x50020000 7 38 55 8)
		     (:edit new nil #x50810080 7 48 144 14)
		     (:button IDOK "OK" #x50010001 153 7 55 14)
		     (:button IDCANCEL "キャンセル" #x50010000 153 24 55 14)
		     (:static nil "元のファイル名(&O):" #x50020000 7 7 50 8)
		     (:edit old nil #x50810880 7 17 144 14)))
		  (list (cons 'new (concat name "~"))
			(cons 'old name))
		  '((new :non-null "新しいファイル名を入力して" :enable (IDOK))))
    (when result
      (cdr (assoc 'new data)))))

(defun filer-copy-as ()
  (let ((old-file (filer-fetch-file))
	new-file)
    (when old-file
      (when (file-directory-p old-file)
	(error "ディレクトリはまだできてない"))
      (setq new-file (filer-get-copy-file-name (file-namestring old-file)))
      (when new-file
	(let ((if-exists :error)
	      (if-access-denied :error))
	  (declare (special if-exists if-access-denied))
	  (setq new-file (merge-pathnames new-file (filer-get-directory)))
	  (filer-subscribe-to-reload (directory-namestring new-file))
	  (if *filer-use-shell-file-operation*
	      (filer-do-copy-by-shell old-file new-file)
	    (filer-copy-file old-file new-file))
	  (message "done."))))))

(defun *filer-drag-and-drop-helper (effect files src dest)
  (let ((if-exists :error)
	(if-access-denied :error))
    (declare (special if-exists if-access-denied))
    (filer-subscribe-to-reload dest t)
    (cond ((eq effect :move)
	   (filer-subscribe-to-reload src t)
	   (filer-do-move files dest (length src)))
	  ((eq effect :copy)
	   (filer-do-copy files dest (length src)))
	  ((eq effect :link)
	   (dolist (file files)
	     (message "ショートカットの作成中: ~A..." file)
	     (create-shortcut file dest))))))

(defun filer-ask-delete-read-only-file (file)
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 219 79
		    (:caption "ファイル削除の確認")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:button IDCANCEL "いいえ(&N)" #x50010000 161 23 50 14)
		     (:button all "ねこそぎ(&A)" #x50010000 161 39 50 14)
		     (:button quit "中止(&Q)" #x50010000 161 55 50 14)
		     (:static nil "書込み禁止ファイルです。強制的に削除しますか?" #x50020000 7 7 144 8)
		     (:static nil "削除するファイル:" #x50020080 7 24 46 8)
		     (:static file nil #x50020080 7 35 150 8)
		     (:button IDOK "はい(&Y)" #x50010001 161 7 50 14)))
		  (list (cons 'file file))
		  nil)
    (cond ((eq result 'IDOK)
	   :force)
	  ((eq result 'all)
	   :all)
	  ((eq result 'quit)
	   (quit))
	  (t :skip))))

(defun filer-do-delete (files)
  (dolist (file files)
    (let ((iad if-access-denied))
      (tagbody
	retry
	(handler-case
	    (cond ((file-directory-p file)
		   (let ((files (directory file :absolute t)))
		     (cond ((null files)
			    (or *filer-delete-mask*
				(delete-directory file :if-access-denied iad)))
			   ((eq delete-non-empty-directory 'never))
			   ((or (eq delete-non-empty-directory :force)
				(yes-or-no-p "ディレクトリ~%~A~%を削除しますか?" file))
			    (when (eq delete-non-empty-directory :first-time)
			      (setq delete-non-empty-directory :force))
			    (filer-do-delete files)
			    (or *filer-delete-mask*
				(delete-directory file :if-access-denied iad))
			    (do-events))
			   (t
			    (when (eq delete-non-empty-directory :first-time)
			      (setq delete-non-empty-directory 'never))))))
		  (*filer-delete-mask*
		   (let ((name (file-namestring file)))
		     (dolist (mask *filer-delete-mask*)
		       (when (pathname-match-p name mask)
			 (message "~A を削除中..." file)
			 (delete-file file :if-access-denied iad
				      :recycle *filer-use-recycle-bin*)
			 (return)))))
		  (t
		   (message "~A を削除中..." file)
		   (delete-file file :if-access-denied iad
				:recycle *filer-use-recycle-bin*)))
	  (access-denied ()
	    (setq iad (filer-ask-delete-read-only-file file))
	    (when (eq iad ':all)
	      (setq iad ':force)
	      (setq if-access-denied ':force))
	    (unless (eq iad ':skip)
	      (go retry)))
	  (file-error (e)
	    (when (ask-file-error e)
	      (go retry))))))))

(defun filer-do-delete-by-shell (files)
  (si:*file-operation :delete files nil
		      :allow-undo *filer-use-recycle-bin*))

(defun filer-query-delete (files)
  (eq (dialog-box '(dialog 0 0 254 115
		    (:caption "削除の確認")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:button IDOK "はい(&Y)" #x50010001 197 7 50 14)
		     (:button IDCANCEL "いいえ(&N)" #x50010000 197 24 50 14)
		     (:static IDC_STATIC "以下のファイルを削除しますか?" #x50020000 7 7 85 8)
		     (:listbox list nil #x50a10101 7 17 186 91)))
		  (list (cons 'list files))
		  nil)
      'IDOK))

(defun filer-delete ()
  (long-operation
    (let ((marks (filer-get-mark-files)))
      (when (and marks
		 (or *filer-use-shell-file-operation*
		     (if *filer-query-delete-precisely*
			 (filer-query-delete marks)
		       (yes-or-no-p "~A" (concat "選択されたファイルを削除しまっせ"
						 (and *filer-delete-mask*
						      (filer-delete-mask-string *filer-delete-mask*
										"\n削除マスク: ")))))))
	(filer-subscribe-to-reload (filer-get-directory) t)
	(if *filer-use-shell-file-operation*
	    (filer-do-delete-by-shell marks)
	  (let ((if-access-denied (if *filer-delete-read-only-files*
				      :force :error))
		(delete-non-empty-directory
		 *filer-delete-non-empty-directory*))
	    (declare (special if-access-denied
			      delete-non-empty-directory))
	    (filer-do-delete marks)))
	(message "done.")))))

(defun filer-delete-mask-string (mask &optional (prefix ""))
  (format nil "~A~{~A~^;~}" prefix mask))

(defun filer-delete-mask ()
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 220 135
		    (:caption "削除マスク")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:listbox list nil #x50a10111 7 7 150 104)
		     (:static nil "マスク(&M):" #x50020000 7 118 27 8)
		     (:combobox mask nil #x50210002 38 115 119 88)
		     (:button IDOK "OK" #x50010001 162 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 162 24 50 14)))
		  `((list . ,*filer-delete-mask-list*)
		    (mask . ,*filer-delete-mask-history*)
		    (mask . ,(filer-delete-mask-string *filer-delete-mask*)))
		  '((list :column (50) :invalidate (mask))
		    (mask :invalidate (list))))
    (when result
      (setq *filer-delete-mask* (or (split-string (or (cdr (assoc 'mask data)) "") #\; nil " ")
				    (cddr (assoc 'list data))))
      (when *filer-delete-mask*
	(add-history (filer-delete-mask-string *filer-delete-mask*) '*filer-delete-mask-history*))
      t)))

(defconstant *file-attribute-readonly* 1)
(defconstant *file-attribute-hidden* 2)
(defconstant *file-attribute-system* 4)
(defconstant *file-attribute-directory* #x10)
(defconstant *file-attribute-archive* #x20)
(defconstant *file-attribute-compressed* #x800)

(defconstant *filer-attributes*
  '((readonly . *file-attribute-readonly*)
    (hidden . *file-attribute-hidden*)
    (system . *file-attribute-system*)
    (archive . *file-attribute-archive*)
    (compressed . *file-attribute-compressed*)))

(defvar *filer-attributes-dialog-template*
  '(dialog 0 0 275 113
    (:caption "ファイルの属性")
    (:font 9 "MS UI Gothic")
    (:control
     (:button nil "ファイルの属性(&B)" #x50000007 7 7 74 83)
     (:button readonly "読み取り専用(&R)" #x50010006 14 19 63 10)
     (:button hidden "隠しファイル(&H)" #x50010006 14 33 57 10)
     (:button archive "アーカイブ(&A)" #x50010006 14 47 51 10)
     (:button system "システム(&S)" #x50010006 14 61 46 10)
     (:button compressed "圧縮(&C)" #x50010006 14 75 39 10)
     (:button nil "更新日時(&M)" #x50000007 86 7 127 83)
     (:button date-nil "変更しない(&N)" #x50030009 93 20 55 10)
     (:button date-now "現在時刻(&W)" #x50000009 93 34 53 10)
     (:button date-0 "今日の&0時" #x50000009 93 48 46 10)
     (:button date-12 "今日の&12時" #x50000009 93 62 49 10)
     (:button date-spec "時刻指定(&P)" #x50000009 93 76 52 10)
     (:edit date nil #x50830080 147 73 62 14)
     (:button subdir "サブディレクトリのファイルも置き換える(&D)" #x50030003 7 96 123 10)
     (:button IDOK "OK" #x50010001 218 7 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 218 24 50 14))))

(defun filer-modify-attributes-dialog (and ior)
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (let ((init (list (cons 'date
			    (format nil "~4,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
				    year mon day hour min sec))
		      '(date-nil . t)))
	  (disables '((date-nil :disable (date))
		      (date-now :disable (date))
		      (date-0 :disable (date))
		      (date-12 :disable (date))
		      (date-spec :enable (date)))))
      (dolist (x *filer-attributes*)
	(let ((bit (symbol-value (cdr x))))
	  (setq init (acons (car x)
			    (cond ((logtest and bit)
				   t)
				  ((logtest ior bit)
				   :disable)
				  (t nil))
			    init))))
      (unless (logtest ior *file-attribute-directory*)
	(push '(subdir :disable :disable) disables))
      (unless (featurep :windows-nt)
	(push '(compressed :disable :disable) disables))
      (multiple-value-bind (result data)
	  (dialog-box *filer-attributes-dialog-template* init disables)
	(when result
	  (let ((on 0)
		(off 0))
	    (dolist (x *filer-attributes*)
	      (let ((d (cdr (assoc (car x) data))))
		(cond ((eq d ':disable))
		      (d
		       (setq on (logior on (symbol-value (cdr x)))))
		      (t
		       (setq off (logior off (symbol-value (cdr x))))))))
	    (values on off (cdr (assoc 'subdir data))
		    (cond ((cdr (assoc 'date-nil data))
			   nil)
			  ((cdr (assoc 'date-now data))
			   (encode-universal-time sec min hour day mon year))
			  ((cdr (assoc 'date-0 data))
			   (encode-universal-time 0 0 0 day mon year))
			  ((cdr (assoc 'date-12 data))
			   (encode-universal-time 0 0 12 day mon year))
			  (t
			   (parse-date-string (cdr (assoc 'date data))))))))))))

(defun filer-modify-file-attribute (file on off date)
  (message "~A..." file)
  (when on
    (tagbody
      retry
      (handler-case (modify-file-attributes file on off)
	(file-error (e)
	  (when (ask-file-error e)
	    (go retry))))))
  (when date
    (tagbody
      retry
      (handler-case (set-file-write-time file date)
	(file-error (e)
	  (when (ask-file-error e)
	    (go retry)))))))

(defun filer-modify-directory-attributes (files on off date)
  (dolist (f files)
    (filer-modify-file-attribute f on off date)
    (when (file-directory-p f)
      (filer-modify-directory-attributes (directory f :absolute t) on off date)
      (do-events))))

(defun filer-modify-attributes ()
  (let ((marks (or (filer-get-mark-files)
		   (let ((f (filer-get-current-file)))
		     (and f (list f))))))
    (when marks
      (let ((and #xfff)
	    (ior 0))
	(dolist (file marks)
	  (let ((x (get-file-attributes file)))
	    (setq and (logand and x))
	    (setq ior (logior ior x))))
	(multiple-value-bind (on off subdir date)
	    (filer-modify-attributes-dialog and ior)
	  (when on
	    (when (= on off 0)
	      (setq on nil))
	    (when (or on date)
	      (filer-subscribe-to-reload (filer-get-directory) t)
	      (if subdir
		  (filer-modify-directory-attributes marks on off date)
		(dolist (file marks)
		  (filer-modify-file-attribute file on off date))))
	    (message "done.")))))))

(defun filer-create-directory ()
  (let ((curdir (filer-get-directory)))
    (multiple-value-bind (result data)
	(dialog-box '(dialog 0 0 231 87
		      (:caption "ディレクトリの作成")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:static nil "作成するディレクトリ(&N):" #x50020000 7 38 64 8)
		       (:edit newdir nil #x50810080 7 47 162 14)
		       (:button move "新しいディレクトリに移動(&M)" #x50010003 7 70 90 10)
		       (:button IDOK "OK" #x50010001 174 7 50 14)
		       (:button IDCANCEL "キャンセル" #x50010000 174 24 50 14)
		       (:static nil "カレントディレクトリ(&C):" #x50020000 7 7 59 8)
		       (:edit curdir nil #x50810880 7 16 162 14)))
		    (list (cons 'curdir curdir)
			  '(move . t))
		    nil)
      (when result
	(let ((newdir (cdr (assoc 'newdir data))))
	  (when newdir
	    (setq newdir (merge-pathnames newdir curdir))
	    (filer-subscribe-to-reload curdir)
	    (create-directory newdir)
	    (when (cdr (assoc 'move data))
	      (filer-set-directory newdir))))))))

(defun filer-extract-archive (&optional (not-current t))
  (let ((files (filer-get-mark-files)))
    (when files
      (let ((dir (filer-get-directory not-current)))
	(filer-subscribe-to-reload dir t)
	(dolist (file files)
	  (when (or (null *filer-query-before-process*)
		    (yes-or-no-p "アーカイブファイル~%~A~%を~%~A~%に解凍しますか?"
				 file dir))
	    (extract-archive file dir)))
	(filer-clear-all-marks)))))

(defun filer-extract-archive-to-current ()
  (filer-extract-archive nil))

(defun filer-get-archive-name ()
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 251 95
		    (:caption "ファイルの圧縮")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "ファイル名(&N):" #x50020000 7 64 38 8)
		     (:edit name nil #x50810080 7 74 160 14)
		     (:button IDOK "OK" #x50010001 194 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 194 24 50 14)
		     (:static nil "作成するディレクトリ(&D):" #x50020000 8 7 64 8)
		     (:combobox dir nil #x50210102 8 18 160 85)
		     (:button ref "参照..." #x50010000 169 17 22 14)
		     (:static nil "基準ディレクトリ(&B):" #x50020000 7 35 53 8)
		     (:edit base nil #x50810080 7 45 160 14)
		     (:button baseref "参照..." #x50010000 169 45 22 14)))
		  (list (cons 'dir (filer-get-directory))
			(cons 'dir *filer-archive-directory-history*)
			(cons 'base (filer-get-directory)))
		  '((name :non-null "アーカイブファイル名を入力してください" :enable (IDOK))
		    (ref :related dir :directory-name-dialog (:title "参照"))
		    (baseref :related base :directory-name-dialog (:title "参照"))))
    (when result
      (let* ((dir (cdr (assoc 'dir data)))
	     (file (merge-pathnames (cdr (assoc 'name data)) dir)))
	(add-history dir '*filer-archive-directory-history*)
	(values (cond ((string-matchp "\\.lzh$" file) file)
		      ((string-matchp "\\.zip$" file) file)
		      ((string-matchp "\\.cab$" file) file)
		      ((string-matchp "\\.tar$" file) file)
		      ((string-matchp "\\.tar\\.gz$" file) file)
		      ((string-matchp "\\.tar\\.bz2$" file) file)
		      ((string-matchp "\\.tar\\.Z$" file) file)
		      ((string-matchp "\\.tar\\.lzma$" file) file)
		      ((string-matchp "\\.tar\\.xz$" file) file)
		      ((string-matchp "\\.tbz$" file) file)
		      ((string-matchp "\\.tgz$" file) file)
		      ((string-matchp "\\.taz$" file) file)
		      ((string-matchp "\\.bza$" file) file)
		      ((string-matchp "\\.gza$" file) file)
		      ((string-matchp "\\.yz1$" file) file)
		      ((string-matchp "\\.7z$" file) file)
		      ((string-matchp "\\.exe$" file) file)
		      (t (concat file ".lzh")))
		(merge-pathnames (cdr (assoc 'base data))
				 (filer-get-directory)))))))

(defun filer-create-archive ()
  (let ((marks (filer-get-mark-files)))
    (when marks
      (multiple-value-bind (arc base)
	  (filer-get-archive-name)
	(when arc
	  (filer-subscribe-to-reload (directory-namestring arc))
	  (let ((sfx (string-matchp "\\.exe$" arc)))
	    (when sfx
	      (setf (subseq arc (- (length arc) 3)) "lzh"))
	    (create-archive arc
			    (if (string-matchp "\\.cab$" arc)
				(mapcar #'(lambda (f)
					    (if (file-directory-p f)
						(merge-pathnames "*.*" f)
					      f))
					marks)
			      marks)
			    base)
	    (when sfx
	      (convert-to-SFX arc "-gw"))
	    (filer-clear-all-marks)))))))

(defun filer-fetch-file (&optional fo v)
  (let ((file (filer-get-mark-files fo v)))
    (cond ((null file)
	   (filer-get-current-file v))
	  ((cdr file)
	   (error "複数ファイルは指定でけん"))
	  (t
	   (car file)))))

(defun filer-list-archive ()
  (let ((file (filer-fetch-file t)))
    (when file
      (multiple-value-bind (result data)
	  (dialog-box
	   `(dialog 0 0 273 237
		    (:caption ,(concat (file-namestring file) "の中身"))
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:listbox list nil #x50b10911 7 7 259 202)
		     (:button IDOK "開く" #x50010001 158 216 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 216 216 50 14)))
	   (list (cons 'list
		       (long-operation
			 (mapcar #'(lambda (x)
				     (list (cadr x)
					   (format nil "~d" (caddr x))
					   (apply #'format
						  nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d"
						  (cadddr x))
					   (car x)))
				 (list-archive file)))))
	   '((list :column (6 -10 18 100) :must-match t :enable (IDOK))))
	(when result
	  (with-set-buffer
	    (when (filer-modal-p)
	      (filer-cancel))
	    (mapc #'(lambda (f)
		      (filer-open-in-archive file (nth 3 f)))
		  (cdr (assoc 'list data)))))))))

(defun filer-shell-execute ()
  (let ((file (filer-get-current-file)))
    (when (or (null *filer-query-before-process*)
	      (yes-or-no-p "~A~%を実行しますか?" file))
      (filer-forward-line 1)
      (shell-execute file (filer-get-directory)))))

(defun filer-file-property ()
  (let ((file (filer-fetch-file)))
    (when file
      (file-property file))))

(defun filer-get-rename-file (name)
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 209 67
		    (:caption "名前の変更")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "新しいファイル名(&N):" #x50020000 7 35 55 8)
		     (:edit new nil #x50810080 7 45 140 14)
		     (:button IDOK "OK" #x50010001 152 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 152 24 50 14)
		     (:static nil "元のファイル名(&O):" #x50020000 7 7 50 8)
		     (:edit old nil #x50810880 7 17 140 14)))
		  (list (cons 'new name)
			(cons 'old name))
		  '((new :non-null "新しいファイル名を入力して" :enable (IDOK))))
    (when result
      (cdr (assoc 'new data)))))

(define-history-variable *filer-rename-regexp-from-history* nil)
(define-history-variable *filer-rename-regexp-to-history* nil)

(defun filer-get-rename-pattern (files)
  (let ((dir (filer-get-directory))
	(from "")
	(to ""))
    (loop
      (multiple-value-bind (result data)
	  (dialog-box '(dialog 0 0 189 79
			(:caption "一気にリネーム")
			(:font 9 "MS UI Gothic")
			(:control
			 (:static nil "変更前ファイル名(正規表現)(&P):" #x50020000 7 7 90 8)
			 (:combobox from nil #x50210042 7 17 119 73)
			 (:static nil "変更後ファイル名(&R):" #x50020000 7 37 58 8)
			 (:combobox to nil #x50210042 7 47 119 73)
			 (:button IDOK "OK" #x50010001 132 7 50 14)
			 (:button IDCANCEL "キャンセル" #x50010000 132 24 50 14)))
		      (list (cons 'from *filer-rename-regexp-from-history*)
			    (cons 'to *filer-rename-regexp-to-history*)
			    (cons 'from from)
			    (cons 'to to))
		      '((to :non-null t :enable (IDOK))))
	(unless result
	  (return-from filer-get-rename-pattern))
	(setq from (cdr (assoc 'from data)))
	(setq to (cdr (assoc 'to data)))
	(add-history from '*filer-rename-regexp-from-history*)
	(add-history to '*filer-rename-regexp-to-history*)
	(let ((matches (mapcan #'(lambda (filename)
				   (let* ((name (file-namestring (remove-trail-slash filename))))
				     (when (string-matchp from name)
				       (let ((r (string-replace-match name to)))
					 (when (and r (string/= r ""))
					   (list (cons filename
						       (merge-pathnames r dir))))))))
			       files)))
	  (setq result (dialog-box
			'(dialog 0 0 235 151
			  (:caption "よろし?")
			  (:font 9 "MS UI Gothic")
			  (:control
			   (:listbox list nil #x50b10103 7 7 165 137)
			   (:button IDOK "OK" #x50010001 178 7 50 14)
			   (:button reinput "もいっかい(&R)" #x50010000 178 24 50 14)
			   (:button IDCANCEL "キャンセル" #x50010000 178 41 50 14)))
			(list (cons 'list
				    (mapcar #'(lambda (x)
						(format nil "~A -> ~A"
							(file-namestring (remove-trail-slash (car x)))
							(let ((l (string/= dir (cdr x))))
							  (if l (subseq (cdr x) l) (cdr x)))))
					    matches)))
			nil))
	  (cond ((eq result 'IDOK)
		 (return matches))
		((eq result 'reinput))
		(t
		 (return nil))))))))

(defun filer-rename ()
  (let ((old (filer-get-mark-files))
	(goto-new nil)
	files)
    (cond (old
	   (when (endp (cdr old))
	     (setq old (car old))))
	  (t
	   (when (filer-current-file-dot-dot-p)
	     (return-from filer-rename nil))
	   (setq old (filer-get-current-file))
	   (setq goto-new t)))
    (cond ((listp old)
	   (setq files (filer-get-rename-pattern old))
	   (unless files
	     (return-from filer-rename nil)))
	  (t
	   (let ((new (filer-get-rename-file (file-namestring (namestring old)))))
	     (unless new
	       (return-from filer-rename nil))
	     (setq files (acons old (merge-pathnames new (filer-get-directory)) nil)))))
    (filer-subscribe-to-reload (filer-get-directory))
    (mapc #'(lambda (x)
	      (message "~A から ~A へリネーム..." (car x) (cdr x))
	      (do-events)
	      (rename-file (car x) (cdr x))
	      (filer-subscribe-to-reload (directory-namestring (cdr x))))
	  files)
    (when goto-new
      (filer-demand-reload)
      (filer-goto-file (file-namestring (cdar files))))
    (message "done.")
    t))

(defun filer-sync-with-primary ()
  (filer-chdir (filer-get-directory) t))

(defun filer-sync-with-secondary ()
  (filer-chdir (filer-get-directory t)))

(defun filer-search ()
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 251 73
		    (:caption "ファイルの検索")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "ファイル名(&F):" #x50020000 7 28 37 8)
		     (:combobox name nil #x50210042 47 25 143 85)
		     (:button IDOK "OK" #x50010001 194 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 194 24 50 14)
		     (:button ref "参照(&R)..." #x50010000 194 41 50 14)
		     (:static nil "探す場所(&D):" #x50020000 7 11 39 8)
		     (:combobox dir nil #x50210002 47 7 143 85)))
		  (list (cons 'dir *filer-search-directory-history*)
			(cons 'name *filer-search-pattern-history*)
			(cons 'dir (filer-get-directory)))
		  '((ref :related dir :directory-name-dialog (:title "参照"))
		    (name :non-null "ファイル名を入力して" :enable (IDOK))))
    (when result
      (message "検索中...")
      (let ((pattern (cdr (assoc 'name data)))
	    (directory (merge-pathnames (cdr (assoc 'dir data))
					(filer-get-directory))))
	(add-history directory '*filer-search-directory-history*)
	(add-history pattern '*filer-search-pattern-history*)
	(let ((found (directory directory
				:absolute t :recursive t
				:wild (split-string pattern #\; t " "))))
	  (clear-message)
	  (cond (found
		 (multiple-value-setq (result data)
		   (dialog-box '(dialog 0 0 279 204
				 (:caption "検索結果")
				 (:font 9 "MS UI Gothic")
				 (:control
				  (:listbox list nil #x50b10111 7 7 265 176)
				  (:button copy "結果をコピー(&C)" #x50010000 168 187 50 14)
				  (:button IDOK "OK" #x50010001 222 187 50 14)))
			       (list (cons 'list (mapcar #'list found)))
			       '((list :column (256)))))
		 (cond ((eq result 'IDOK)
			(setq result (car (cdr (assoc 'list data))))
			(when result
			  (cond ((file-directory-p result)
				 (filer-chdir result))
				(t
				 (filer-chdir (directory-namestring result))
				 (filer-goto-file (file-namestring result))))))
		       ((eq result 'copy)
			(copy-to-clipboard (format nil "~{~A~%~}" found)))))
		(t
		 (error "~{~A~^, ~}:~%見つかりません" pattern))))))))

(defun filer-search-name ()
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 231 57
		    (:caption "ファイルを探すよ")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "ファイル名(&F):" #x50020000 7 7 37 8)
		     (:combobox name nil #x50210002 7 19 162 88)
		     (:button IDOK "OK" #x50010001 174 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 174 24 50 14)))
		  (list (cons 'name *minibuffer-file-name-history*))
		  '((name :non-null "ファイル名を入力して" :enable (IDOK))))
    (when result
      (let ((name (cdr (assoc 'name data))))
	(unless (zerop (length name))
	  (filer-goto-file name 'next nil 'arg))))))

(defun filer-create-shortcut-to-desktop ()
  (let ((files (filer-get-mark-files)))
    (when (and files
	       (or (null *filer-query-before-process*)
		   (yes-or-no-p "選択されたファイルのショートカットをデスクトップに作成しますか?")))
      (dolist (file files)
	(message "ショートカットの作成中: ~A..." file)
	(create-shortcut-to-desktop file))))
  (filer-clear-all-marks)
  (message "done."))

(defun filer-create-shortcut ()
  (let ((dest (filer-get-directory t))
	(files (filer-get-mark-files)))
    (when (and files
	       (or (null *filer-query-before-process*)
		   (yes-or-no-p "選択されたファイルのショートカットを~%~A~%に作成しますか?"
				dest)))
      (filer-subscribe-to-reload dest)
      (dolist (file files)
	(message "ショートカットの作成中: ~A..." file)
	(create-shortcut file dest)))
    (filer-clear-all-marks)
    (message "done.")))

(defun filer-input-file-name ()
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 231 57
		    (:caption "ファイル名")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:static nil "ファイル名(&F):" #x50020000 7 7 37 8)
		     (:combobox name nil #x50210002 7 19 162 88)
		     (:button IDOK "OK" #x50010001 174 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 174 24 50 14)))
		  (list (cons 'name *minibuffer-file-name-history*))
		  '((name :non-null "ファイル名を入力して" :enable (IDOK))))
    (when result
      (let ((name (cdr (assoc 'name data))))
	(unless (zerop (length name))
	  (filer-set-text name)
	  (filer-ok))))))

(defun filer-load-file ()
  (let ((files (or (filer-get-mark-files)
		   (let ((f (filer-get-current-file)))
		     (and f (list f))))))
    (when (and files
	       (or (null *filer-query-before-process*)
		   (if (null (cdr files))
		       (yes-or-no-p "~A~%をロードしますか?" (car files))
		     (yes-or-no-p "選択されたファイルをロードしますか?"))))
      (long-operation
	(mapc #'load files)))))

(defun filer-sort-items ()
  (multiple-value-bind (result data)
      (let* ((s (filer-get-sort-order))
	     (x (logand s 3)))
	(dialog-box '(dialog 0 0 143 147
		      (:caption "ファイルのソート")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:button nil "ソート方法" #x50000007 7 7 70 78)
		       (:button name "名前(&N)" #x50030009 13 21 39 10)
		       (:button size "サイズ(&S)" #x50000009 13 37 39 10)
		       (:button date "更新日時(&D)" #x50000009 13 53 53 10)
		       (:button ext "拡張子(&E)" #x50000009 13 69 45 10)
		       (:button reverse "降順でソート(&R)" #x50030003 7 91 63 10)
		       (:button icase "大文字小文字を区別する(&C)" #x50010003 7 104 105 10)
		       (:button idir "ディレクトリを特別扱いしない(&I)" #x50010003 7 117 101 10)
		       (:button num "数値を考慮する(&U)" #x50010003 7 130 78 10)
		       (:button IDOK "OK" #x50010001 86 7 50 14)
		       (:button both "両方の窓(&B)" #x50010000 86 24 50 14)
		       (:button IDCANCEL "キャンセル" #x50010000 86 41 50 14)))
		    (list (cons 'name (= x 0))
			  (cons 'size (= x 1))
			  (cons 'date (= x 2))
			  (cons 'ext (= x 3))
			  (cons 'reverse (logtest s 4))
			  (cons 'icase (logtest s 8))
			  (cons 'idir (logtest s 16))
			  (cons 'num (logtest s 32)))
		    (list (unless (filer-dual-window-p)
			    '(both :disable :disable)))))
    (when result
      (let ((x (cond ((cdr (assoc 'name data)) 0)
		     ((cdr (assoc 'size data)) 1)
		     ((cdr (assoc 'date data)) 2)
		     (t 3))))
	(and (cdr (assoc 'reverse data)) (incf x 4))
	(and (cdr (assoc 'icase data)) (incf x 8))
	(and (cdr (assoc 'idir data)) (incf x 16))
	(and (cdr (assoc 'num data)) (incf x 32))
	(filer-sort x)
	(when (eq result 'both)
	  (filer-sort x t))))))

(defun filer-compare-file ()
  (let ((file1 (filer-fetch-file nil nil))
	(file2 (filer-fetch-file nil t)))
    (unless (and file1 file2)
      (error "比較するファイルを指定して"))
    (msgbox "~A~%~A~%~%~:[ちゃう~;同じ~]"
	    file1 file2 (compare-file file1 file2))))

(defun filer-eject-media ()
  (long-operation
    (eject-media (svref (filer-get-directory) 0))))

(defun filer-view-file ()
  (let ((files (or (filer-get-mark-files t)
		   (let ((f (filer-get-current-file)))
		     (and f (list f))))))
    (when files
      (mapc #'(lambda (file)
		(unless (file-directory-p file)
		  (find-file file)
		  (setq buffer-read-only t)
		  (add-history file '*minibuffer-file-name-history*)))
	    files)
      (or (filer-modal-p)
	  (si:*activate-toplevel)))))

(defun filer-send-to ()
  (unless (zerop (filer-count-marks))
    (let* ((send-to (get-special-folder-location :send-to))
	   (dests (nconc (sort (directory send-to :file-only t :recursive t) #'string-lessp)
			 *filer-send-to-list*)))
      (multiple-value-bind (result data)
	  (dialog-box '(dialog 0 0 184 107
			(:caption "送っちゃうよ")
			(:font 9 "MS UI Gothic")
			(:control
			 (:listbox list nil #x50b10001 2 3 118 103)
			 (:button IDOK "送る" #x50030001 128 7 52 14)
			 (:button IDCANCEL "送らない" #x50030000 128 27 52 14)))
		      (list (cons 'list (mapcar #'(lambda (x)
						    (if (consp x)
							(car x)
						      (substitute-string x "\\.\\(lnk\\|pif\\)$" "")))
						dests))
			    '(list . 0))
		      '((list :index t :must-match t :enable (IDOK))))
	(when result
	  (let* ((selected (nth (cdr (assoc 'list data)) dests))
		 (dest (if (consp selected)
			   (cdr selected)
			 (merge-pathnames selected send-to)))
		 (link (truename (filer-read-link dest))))
	    (if (file-directory-p link)
		(filer-copy (append-trail-slash link))
	      (shell-execute dest nil
			     (format nil "~{\"~a\"~^ ~}"
				     (mapcar #'map-slash-to-backslash
					     (filer-get-mark-files)))))))))))

(defun filer-send-to-target ()
  (let* ((send-to (get-special-folder-location :send-to))
	 (dests (nconc (sort (directory send-to :file-only t :recursive t)
			     #'string-lessp)
		       *filer-send-to-list*)))
    (multiple-value-bind (result data)
	(dialog-box '(dialog 0 0 184 107
		      (:caption "送っちゃうよ")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:listbox list nil #x50b10001 2 3 118 103)
		       (:button IDOK "送る" #x50030001 128 7 52 14)
		       (:button IDCANCEL "送らない" #x50030000 128 27 52 14)))
		    (list (cons 'list (mapcar #'(lambda (x)
						  (if (consp x)
						      (car x)
						    (pathname-name x)))
					      dests))
			  '(list . 0))
		    '((list :index t :must-match t :enable (IDOK))))
      (when result
	(let ((selected (nth (cdr (assoc 'list data)) dests)))
	  (if (consp selected)
	      (cdr selected)
	    (merge-pathnames selected send-to)))))))

(defun filer-send-to ()
  (unless (zerop (filer-count-marks))
    (let ((dest (filer-send-to-target)))
      (when dest
	(setq dest (truename (ed::filer-read-link dest)))
	(if (file-directory-p dest)
	    (filer-copy (append-trail-slash dest))
	  (let ((files (filer-get-mark-files))
		(ext (pathname-type dest))
		(dir (filer-get-directory))
		clsid)
	    (when (and ext (not (string-equal ext "exe")))
	      (setq clsid (read-registry (concat "." ext) nil :classes-root))
	      (when clsid
		(setq clsid (read-registry (concat clsid "\\Shellex\\DropHandler")
					   nil :classes-root))))
	    (unless (and clsid
			 (ole-drop-files dest clsid dir
					 (mapcar #'(lambda (x)
						     (file-namestring
						      (remove-trail-slash x)))
						 files)))
	      (call-process
	       (format nil "~{~a~^ ~}"
		       (mapcar #'(lambda (x)
				   (map-slash-to-backslash
				    (remove-trail-slash
				     (get-short-path-name x))))
			       (cons dest files)))
	       :no-std-handles t :show :show
	       :exec-directory dir))))))))

(defun filer-grep ()
  (let ((files (filer-get-mark-files)))
    (when files
      (require "grepd")
      (grep-dialog-1 '(dialog 0 0 271 139
		       (:caption "Grep")
		       (:font 9 "MS UI Gothic")
		       (:control
			(:static nil "パターン(&P):" #x50020000 7 10 38 8)
			(:combobox pat nil #x50210042 51 8 157 96)
			(:static file-static "ファイル名(&F):" #x50020000 7 27 40 8)
			(:combobox file nil #x50210042 51 25 157 96)
			(:button case-fold "大文字小文字を区別する(&C)" #x50010006 51 44 105 10)
			(:button word "単語単位で検索する(&W)" #x50010003 51 57 92 10)
			(:button regexp "正規表現(&E)" #x50010003 51 70 58 10)
			(:button escseq "エスケープシーケンスを理解しろ(&Y)" #x50010003 51 83 106 10)
			(:button subdir "ついでにサブディレクトリも(&U)" #x50010003 51 96 95 10)
			(:button async "非同期でgrep(&A)" #x50010003 51 109 65 10)
			(:button name "ファイル名だけ出力(&O)" #x50010003 51 122 87 10)
			(:button IDOK "検索(&S)" #x50010001 214 7 50 14)
			(:button IDCANCEL "キャンセル" #x50010000 214 24 50 14)))
		     files))))

(defun filer-gresreg ()
  (let ((files (filer-get-mark-files)))
    (when files
      (require "gresregd")
      (gresreg-dialog-1 '(dialog 0 0 271 142
			  (:caption "Gresreg")
			  (:font 9 "MS UI Gothic")
			  (:control
			   (:static nil "検索(&S):" #x50020000 7 10 32 8)
			   (:combobox pat nil #x50210042 50 8 157 96)
			   (:static nil "置換(&R):" #x50020000 7 27 36 8)
			   (:combobox rep nil #x50210042 50 25 157 96)
			   (:static file-static "ファイル名(&F):" #x50020000 7 44 41 8)
			   (:combobox file nil #x50210042 50 42 157 96)
			   (:button case-fold "大文字小文字を区別する(&C)" #x50010006 50 60 113 10)
			   (:button word "単語単位で検索する(&W)" #x50010003 50 73 100 10)
			   (:button regexp "正規表現(&E)" #x50010003 50 86 62 10)
			   (:button escseq "エスケープシーケンスを理解しろ(&Y)" #x50010003 50 99 105 10)
			   (:button subdir "ついでにサブディレクトリも(&U)" #x50010003 50 112 103 10)
			   (:button save "勝手にセーブしろ(&V)" #x50010003 50 125 79 10)
			   (:button query "確認あり(&Q)" #x50010001 214 7 50 14)
			   (:button all "全て置換(&A)" #x50010000 214 24 50 14)
			   (:button IDCANCEL "キャンセル" #x50010000 214 42 50 14)
			   (:button ref "参照(&N)..." #x50010000 214 59 50 14)))
			files))))

(defun filer-byte-compile ()
  (let ((files (filer-get-mark-files)))
    (when (and files
	       (or (null *filer-query-before-process*)
		   (if (null (cdr files))
		       (yes-or-no-p "~A~%をコンパイルしますか?" (car files))
		     (yes-or-no-p "選択されたファイルをコンパイルしますか?"))))
      (filer-subscribe-to-reload (filer-get-directory))
      (mapc #'(lambda (f)
		(if (file-directory-p f)
		    (byte-recompile-directory f)
		  (byte-compile-file f)))
	    files))))

(defun filer-isearch-forward (&optional reverse)
  (let ((string (make-vector 16 :element-type 'character
			     :adjustable t :fill-pointer 0)))
    (loop
      (message "i-search~:[~; backward~]: ~a" reverse string)
      (let ((c (filer-read-char)))
	(cond ((graphic-char-p c)
	       (vector-push-extend c string)
	       (filer-goto-file string t nil 'arg))
	      ((eql c #\C-h)
	       (unless (zerop (length string))
		 (vector-pop string)))
	      (t
	       (let ((command (lookup-keymap filer-keymap c t)))
		 (cond ((eq command 'filer-isearch-forward)
			(filer-goto-file string 'next nil 'arg)
			(setq reverse nil))
		       ((eq command 'filer-isearch-backward)
			(filer-goto-file string 'next t 'arg)
			(setq reverse t))
		       (t
			(return))))))))
    (clear-message)))

(defun filer-isearch-backward ()
  (filer-isearch-forward t))

(defun filer-close-modal ()
  (if (filer-modal-p)
      (filer-cancel)
    (quit)))

(defun open-filer ()
  (interactive)
  (multiple-value-bind (files result)
      (let ((omode (get-ime-mode)))
	(unwind-protect
	    (filer nil t nil t (not *filer-modal*))
	  (and *filer-modal* (toggle-ime omode))))
    (when result
      (filer-readin-1 files))))

(define-key spec-map #\C-f 'open-filer)

(unless (boundp 'filer-keymap)
  (setq filer-keymap (make-keymap))
  (do ((c #x20 (+ c 1)))
      ((> c #x7e))
    (define-key filer-keymap (code-char c) 'filer-isearch))
  (define-key filer-keymap #\NUL 'filer-toggle-mark)
  (define-key filer-keymap #\C-b 'filer-scroll-or-left-window-or-up)
  (define-key filer-keymap #\C-f 'filer-scroll-or-right-window-or-up)
  (define-key filer-keymap #\C-g 'filer-close-modal)
  (define-key filer-keymap #\C-h 'filer-up-directory)
  (define-key filer-keymap #\TAB 'filer-swap-windows)
  (define-key filer-keymap #\RET 'filer-ok)
  (define-key filer-keymap #\C-n 'filer-forward-line)
  (define-key filer-keymap #\C-p 'filer-backward-line)
  (define-key filer-keymap #\C-r 'filer-isearch-backward)
  (define-key filer-keymap #\C-s 'filer-isearch-forward)
  (define-key filer-keymap #\C-u 'filer-calc-directory-byte-size)
  (define-key filer-keymap #\C-v 'filer-forward-page)
  (define-key filer-keymap #\C-z 'filer-backward-page)
  (define-key filer-keymap #\F5 'filer-toggle-mark-files)
  (define-key filer-keymap #\SPC 'filer-toggle-mark-and-forward-line)
  (define-key filer-keymap #\* 'filer-load-file)
  (define-key filer-keymap #\. 'filer-set-path-mask)
  (define-key filer-keymap #\/ 'filer-mark-path-mask)
  (define-key filer-keymap #\< 'filer-goto-bof)
  (define-key filer-keymap #\> 'filer-goto-eof)
  (define-key filer-keymap #\@ 'filer-copy-as)
  (define-key filer-keymap #\A 'filer-modify-attributes)
  (define-key filer-keymap #\B 'filer-byte-compile)
  (define-key filer-keymap #\C 'filer-copy)
  (define-key filer-keymap #\D 'filer-delete)
  (define-key filer-keymap #\E 'filer-extract-archive)
  (define-key filer-keymap #\F 'filer-search)
  (define-key filer-keymap #\G 'filer-search-name)
  (define-key filer-keymap #\I 'filer-delete-mask)
  (define-key filer-keymap #\J 'filer-jump-directory)
  (define-key filer-keymap #\K 'filer-create-directory)
  (define-key filer-keymap #\L 'filer-change-drive)
  (define-key filer-keymap #\M 'filer-move)
  (define-key filer-keymap #\N 'filer-input-file-name)
  (define-key filer-keymap #\O 'filer-create-archive)
  (define-key filer-keymap #\P 'filer-sync-with-primary)
  (define-key filer-keymap #\Q 'filer-cancel)
  (define-key filer-keymap #\R 'filer-rename)
  (define-key filer-keymap #\S 'filer-sync-with-secondary)
  (define-key filer-keymap #\T 'filer-create-shortcut-to-desktop)
  (define-key filer-keymap #\U 'filer-calc-directory-size)
  (define-key filer-keymap #\V 'filer-list-archive)
  (define-key filer-keymap #\W 'filer-viewer)
  (define-key filer-keymap #\X 'filer-shell-execute)
  (define-key filer-keymap #\Y 'filer-create-shortcut)
  (define-key filer-keymap #\Z 'filer-file-property)
  (define-key filer-keymap #\\ 'filer-goto-root)
  (define-key filer-keymap #\= 'filer-compare-file)
  (define-key filer-keymap #\^ 'filer-eject-media)
  (define-key filer-keymap #\] 'filer-send-to)
  (define-key filer-keymap #\M-g 'filer-grep)
  (define-key filer-keymap #\M-r 'filer-gresreg)
  (define-key filer-keymap #\M-v 'filer-view-file)
  (define-key filer-keymap #\Up 'filer-backward-line)
  (define-key filer-keymap #\Down 'filer-forward-line)
  (define-key filer-keymap #\Left 'filer-scroll-or-left-window-or-up)
  (define-key filer-keymap #\Right 'filer-scroll-or-right-window-or-up)
  (define-key filer-keymap #\PageUp 'filer-backward-page)
  (define-key filer-keymap #\PageDown 'filer-forward-page)
  (define-key filer-keymap #\Home 'filer-toggle-mark-files)
  (define-key filer-keymap #\S-Home 'filer-clear-all-marks)
  (define-key filer-keymap #\C-Home 'filer-toggle-all-marks)
  (define-key filer-keymap #\S-C-Home 'filer-mark-all-files)
  (define-key filer-keymap #\End 'filer-reload)
  (define-key filer-keymap #\F3 'filer-execute-application)
  (define-key filer-keymap #\F6 'filer-sort-items)

  (define-key filer-keymap #\C-1 #'(lambda () (filer-modify-column-width 0 1)))
  (define-key filer-keymap #\C-M-1 #'(lambda () (filer-modify-column-width 0 -1)))
  (define-key filer-keymap #\C-2 #'(lambda () (filer-modify-column-width 1 1)))
  (define-key filer-keymap #\C-M-2 #'(lambda () (filer-modify-column-width 1 -1)))
  (define-key filer-keymap #\C-3 #'(lambda () (filer-modify-column-width 2 1)))
  (define-key filer-keymap #\C-M-3 #'(lambda () (filer-modify-column-width 2 -1)))
  (define-key filer-keymap #\C-4 #'(lambda () (filer-modify-column-width 3 1)))
  (define-key filer-keymap #\C-M-4 #'(lambda () (filer-modify-column-width 3 -1)))

  (define-key filer-keymap #\Apps 'filer-context-menu)
  (define-key filer-keymap #\S-F10 'filer-context-menu)
)

