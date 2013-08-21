;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "dialogs")

(in-package "editor")

(export '(*regexp-search* *replace-whole-buffer*
	  *understand-escape-sequences*
	  search-dialog replace-dialog count-matches-dialog
	  goto-line-dialog open-file-dialog close-selected-buffer
	  close-and-open-file-dialog save-as-dialog
	  save-buffer-dialog insert-file-dialog rename-dialog
	  execute-subprocess-dialog pipe-command-dialog
	  launch-application-dialog
	  print-selected-buffer print-selected-buffer-dialog
	  control-code-dialog-box select-xyzzy
	  create-xyzzy-shortcut-dialog))

(defvar *regexp-search* nil)
(defvar *replace-whole-buffer* nil)
(define-history-variable *understand-escape-sequences* nil)

(defvar *last-search-string-pair* nil)
(defvar *last-replace-string-pair* nil)

(defvar *search-dialog-template*
  '(dialog 0 0 242 95
    (:caption "文字列の検索")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "検索(&S):" #x50020000 7 10 28 8)
     (:combobox search nil #x50210842 39 8 140 96)
     (:button case-fold "大文字小文字を区別する(&C)" #x50010006 39 27 96 10)
     (:button word "単語単位で検索する(&W)" #x50010003 39 40 83 10)
     (:button regexp "正規表現(&E)" #x50010003 39 53 52 10)
     (:button escseq "エスケープシーケンスを理解しろ(&Y)" #x50010003 39 66 107 10)
     (:button wrap "見つからなければ戻って検索(&V)" #x50010003 39 79 101 10)
     (:button backward "上検索(&P)" #x50010000 185 7 50 14)
     (:button forward "下検索(&N)" #x50010001 185 24 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 185 41 50 14))))

(defun search-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (let ((string (or (selection-start-end (start end)
			  (buffer-substring start end))
			(if *regexp-search*
			    *last-search-regexp*
			  *last-search-string*))))
	(dialog-box *search-dialog-template*
		    (list (cons 'search *minibuffer-search-string-history*)
			  (cons 'case-fold (cfs2dialog *case-fold-search*))
			  (cons 'word *word-search*)
			  (cons 'regexp *regexp-search*)
			  (cons 'search (if (eq (cdr *last-search-string-pair*)
						string)
					    (car *last-search-string-pair*)
					  string))
			  (cons 'escseq *understand-escape-sequences*)
			  (cons 'wrap *wrap-search*))
		    '((word :disable (regexp))
		      (search :non-null "検索文字列を入力して" :enable (forward backward)))))
    (when result
      (let ((string (cdr (assoc 'search data))))
	(when string
	  (add-history string '*minibuffer-search-string-history*)
	  (setq *case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	  (setq *word-search* (cdr (assoc 'word data)))
	  (setq *regexp-search* (cdr (assoc 'regexp data)))
	  (setq *wrap-search* (cdr (assoc 'wrap data)))
	  (setq *understand-escape-sequences* (cdr (assoc 'escseq data)))
	  (if *understand-escape-sequences*
	      (setq *last-search-string-pair*
		    (cons string
			  (setq string (decode-escape-sequence
					string *regexp-search*))))
	    (setq *last-search-string-pair* nil))
	  (search-command string (eq result 'backward)
			  (and (null *word-search*) *regexp-search*)
			  t nil))))))

(defvar *replace-dialog-template*
  '(dialog 0 0 242 115
    (:caption "文字列の置換")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "検索(&S):" #x50020000 7 10 32 8)
     (:combobox search nil #x50210842 39 7 140 96)
     (:static nil "置換(&R):" #x50020000 7 29 32 8)
     (:combobox replace nil #x50210842 39 26 140 96)
     (:button case-fold "大文字小文字を区別する(&C)" #x50010006 39 46 113 10)
     (:button word "単語単位で検索する(&W)" #x50010003 39 59 97 10)
     (:button regexp "正規表現(&E)" #x50010003 39 72 63 10)
     (:button escseq "エスケープシーケンスを理解しろ(&Y)" #x50010003 39 85 113 10)
     (:button whole "バッファの先頭から(&O)" #x50010003 39 98 89 10)
     (:button query "確認あり(&Q)" #x50010001 185 7 50 14)
     (:button all "全て置換(&A)" #x50010000 185 24 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 185 41 50 14))))

(defun replace-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (let ((search (or (selection-start-end (start end)
			  (buffer-substring start end))
			(if *regexp-search*
			    *last-search-regexp*
			  *last-search-string*)))
	    (replace (if *regexp-search*
			 *last-replace-regexp*
		       *last-replace-string*)))
	(dialog-box *replace-dialog-template*
		    (list (cons 'search *minibuffer-search-string-history*)
			  (cons 'replace *minibuffer-search-string-history*)
			  (cons 'case-fold (cfs2dialog *case-fold-search*))
			  (cons 'word *word-search*)
			  (cons 'regexp *regexp-search*)
			  (cons 'whole *replace-whole-buffer*)
			  (cons 'search
				(if (eq (cdr *last-search-string-pair*)
					search)
				    (car *last-search-string-pair*)
				  search))
			  (cons 'replace
				(if (eq (cdr *last-replace-string-pair*)
					replace)
				    (car *last-replace-string-pair*)
				  replace))
			  (cons 'escseq *understand-escape-sequences*))
		    '((word :disable (regexp))
		      (search :non-null "検索文字列を入力して" :enable (query all)))))
    (when result
      (let ((search (cdr (assoc 'search data)))
	    (replace (cdr (assoc 'replace data))))
	(when search
	  (add-history search '*minibuffer-search-string-history*)
	  (add-history replace '*minibuffer-search-string-history*)
	  (setq *case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	  (setq *word-search* (cdr (assoc 'word data)))
	  (setq *regexp-search* (cdr (assoc 'regexp data)))
	  (setq *understand-escape-sequences* (cdr (assoc 'escseq data)))
	  (if *understand-escape-sequences*
	      (progn
		(setq *last-search-string-pair*
		      (cons search
			    (setq search (decode-escape-sequence
					  search *regexp-search*))))
		(setq *last-replace-string-pair*
		      (cons replace
			    (setq replace (decode-escape-sequence
					   replace *regexp-search*)))))
	    (progn
	      (setq *last-search-string-pair* nil)
	      (setq *last-replace-string-pair* nil)))
	  (setq *replace-whole-buffer* (cdr (assoc 'whole data)))
	  (when *replace-whole-buffer*
	    (goto-char (point-min)))
	  (perform-replace search replace (eq result 'query)
			   (and (null *word-search*) *regexp-search*)
			   t nil))))))

(defvar *count-matches-dialog-template*
  '(dialog 0 0 242 84
    (:caption "一致する文字列を数える")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "検索(&S):" #x50020000 7 10 31 8)
     (:combobox search nil #x50210842 39 8 140 96)
     (:button case-fold "大文字小文字を区別する(&C)" #x50010006 39 27 113 10)
     (:button word "単語単位で検索する(&W)" #x50010003 39 40 100 10)
     (:button regexp "正規表現(&E)" #x50010003 39 53 55 10)
     (:button insert "結果を挿入する(&I)" #x50010003 39 66 105 10)
     (:button IDOK "検索(&P)" #x50010001 185 7 50 14)
     (:button IDCANCEL "キャンセル" #x50010000 185 24 50 14))))

(defun count-matches-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box *count-matches-dialog-template*
		  (list (cons 'search *minibuffer-search-string-history*)
			(cons 'case-fold (cfs2dialog *case-fold-search*))
			(cons 'word *word-search*)
			(cons 'regexp *regexp-search*)
			(cons 'search (or (selection-start-end (start end)
					    (buffer-substring start end))
					  (if *regexp-search*
					      *last-search-regexp*
					    *last-search-string*))))
		  '((word :disable (regexp))
		    (search :non-null "検索文字列を入力して" :enable (IDOK))))
    (when result
      (let ((string (cdr (assoc 'search data))))
	(when string
	  (add-history string '*minibuffer-search-string-history*)
	  (setq *case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	  (setq *word-search* (cdr (assoc 'word data)))
	  (setq *regexp-search* (cdr (assoc 'regexp data)))
	  (let ((count (count-matches string (or *word-search* (null *regexp-search*)) t)))
	    (if (cdr (assoc 'insert data))
		(insert (format nil "~D" count))
	      (if (zerop count)
		  (msgbox "文字列が見つかりません")
		(msgbox "~D個の文字列があります" count)))))))))

(defun goto-line-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box
       `(dialog 0 0 156 61
		(:caption "指定行へ移動")
		(:font 9 "MS UI Gothic")
		(:control
		 (:static nil "行番号(&L):" #x50020000 7 16 40 8)
		 (:edit line nil #x50810086 49 12 40 14)
		 (:button disp "表示行だす(&D)"
		  ,(if (integerp (buffer-fold-width)) #x50010003 #x58010003)
		  11 44 58 10)
		 (:button IDOK "移動(&G)" #x50010001 99 7 50 14)
		 (:button IDCANCEL "キャンセル" #x50010000 99 24 50 14)))
       '((disp . t))
       '((line :enable (IDOK) :type integer :min 1 :non-null t
	       :type-error "行番号を入力してしださい"
	       :range-error "1以上を指定してください")))
    (when result
      (if (cdr (assoc 'disp data))
	  (goto-virtual-line (cdr (assoc 'line data)))
	(goto-line (cdr (assoc 'line data)))))))

(defun open-file-dialog (&optional title (multiple t))
  (interactive)
  (multiple-value-bind (files index encoding eol-code)
      (file-name-dialog :title title :multiple multiple
			:filter *file-dialog-filter*
			:filter-index *file-dialog-filter-index*
			:char-encoding (or *expected-fileio-encoding* t)
			:eol-code *expected-eol-code*)
    (when files
      (setq *file-dialog-filter-index* index)
      (let ((*expected-eol-code* eol-code))
	(declare (special *expected-eol-code*))
	(when (eq encoding *encoding-auto*)
	  (setq encoding nil))
	(find-file files encoding))
      (if multiple
	  (dolist (f files t)
	    (add-history f '*minibuffer-file-name-history*))
	(add-history files '*minibuffer-file-name-history*))
      t)))

(defun close-and-open-file-dialog ()
  (interactive)
  (when (kill-buffer (selected-buffer))
    (open-file-dialog "閉じて開く" nil)))

(defun save-as-dialog ()
  (interactive)
  (multiple-value-bind (file index encoding eol-code)
      (file-name-dialog :save t
			:default (get-buffer-file-name)
			:filter *file-dialog-filter*
			:filter-index *file-dialog-filter-index*
			:overwrite t :hide-read-only t
			:char-encoding (buffer-fileio-encoding)
			:eol-code (buffer-eol-code))
    (when file
      (setq *file-dialog-filter-index* index)
      (rename file t)
      (set-buffer-fileio-encoding encoding)
      (set-buffer-eol-code eol-code)
      (save-buffer)
      (add-history file '*minibuffer-file-name-history*)
      t)))

(defun save-buffer-dialog ()
  (interactive)
  (if (file-visited-p)
      (save-buffer)
    (save-as-dialog)))

(defun close-selected-buffer ()
  (interactive)
  (cond ((not (need-buffer-save-p (selected-buffer)))
	 (kill-buffer (selected-buffer)))
	((yes-no-or-cancel-p "~aを保存しますか?" (selected-buffer))
	 (if (save-buffer-dialog)
	     (kill-buffer (selected-buffer))))
	(t
	 (not-modified)
	 (kill-buffer (selected-buffer)))))

(defun insert-file-dialog ()
  (interactive "*")
  (multiple-value-bind (file index encoding eol-code)
      (file-name-dialog :title "ファイルの挿入"
			:filter *file-dialog-filter*
			:filter-index *file-dialog-filter-index*
			:must-exist t
			:char-encoding (or *expected-fileio-encoding* t)
			:eol-code *expected-eol-code*)
    (when file
      (setq *file-dialog-filter-index* index)
      (let ((*expected-fileio-encoding* encoding)
	    (*expected-eol-code* eol-code))
	(declare (special *expected-fileio-encoding* *expected-eol-code*))
	(insert-file file)))))

(defun rename-dialog ()
  (interactive)
  (multiple-value-bind (file index)
      (file-name-dialog :title "名前の変更" :save t
			:filter *file-dialog-filter*
			:filter-index *file-dialog-filter-index*
			:overwrite t :hide-read-only t)
    (when file
      (setq *file-dialog-filter-index* index)
      (rename file t)
      (add-history file '*minibuffer-file-name-history*)
      t)))

(defun execute-process-dialog (fn title &optional init-cmd)
  (multiple-value-bind (result data)
      (dialog-box `(dialog 0 0 237 72
			   (:caption ,title)
			   (:font 9 "MS UI Gothic")
			   (:control
			    (:static nil "コマンドライン(&C):" #x50020000 7 7 48 8)
			    (:combobox cmd nil #x50210042 7 19 166 62)
			    (:button IDOK "OK" #x50010001 180 7 50 14)
			    (:button IDCANCEL "キャンセル" #x50010000 180 24 50 14)
			    (:button ref "参照(&R)..." #x50010000 180 41 50 14)))
		  (list (cons 'cmd *minibuffer-execute-history*)
			(cons 'cmd (or init-cmd "")))
		  '((cmd :non-null "実行するコマンドを入力してください" :enable (IDOK))
		    (ref :related cmd
			 :file-name-dialog (:title "参照"
					    :filter (("実行ファイル(*.exe;*.com;*.cmd;*.bat;*.pif)"
						      . "*.exe;*.com;*.cmd;*.bat;*.pif")
						     ("すべてのファイル(*.*)" . "*.*"))
					    :must-exist t))))
    (when result
      (let ((cmd (cdr (assoc 'cmd data))))
	(add-history cmd '*minibuffer-execute-history*)
	(funcall fn cmd)))))

(defun execute-subprocess-dialog (&optional init-cmd)
  (interactive)
  (execute-process-dialog #'execute-subprocess "非同期プログラムの実行" init-cmd))

(defun pipe-command-dialog (&optional init-cmd)
  (interactive)
  (execute-process-dialog #'pipe-command "コンソールプログラムの実行" init-cmd))

(defun launch-application-dialog (&optional init-cmd)
  (interactive)
  (execute-process-dialog #'launch-application "Windowsプログラムの実行" init-cmd))

(defun print-selected-buffer ()
  (interactive)
  (print-buffer (selected-buffer)))

(defun print-selected-buffer-dialog ()
  (interactive)
  (print-dialog (selected-buffer))
  (continue-pre-selection))

(defun control-code-dialog-box ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 186 162
		    (:caption "制御コードの入力")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:button IDOK "OK" #x50010001 129 7 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 129 24 50 14)
		     (:listbox list nil #x50b10111 7 7 116 148)))
		  '((list . ((#\C-@ "0" "NUL" "null")
			     (#\C-a "1" "SOH" "start of heading")
			     (#\C-b "2" "STX" "start of text")
			     (#\C-c "3" "ETX" "end of text")
			     (#\C-d "4" "EOT" "end of transmisson")
			     (#\C-e "5" "ENQ" "enquiry")
			     (#\C-f "6" "ACK" "acknowledge")
			     (#\C-g "7" "BEL" "bell")
			     (#\C-h "8" "BS" "backspace")
			     (#\C-i "9" "HT" "horizontal tabullation")
			     (#\C-j "10" "LF" "line feed")
			     (#\C-k "11" "VT" "vertical tabulation")
			     (#\C-l "12" "FF" "form feed")
			     (#\C-m "13" "CR" "carriage return")
			     (#\C-n "14" "SO" "shift out")
			     (#\C-o "15" "SI" "shift in")
			     (#\C-p "16" "DLE" "datalink escape")
			     (#\C-q "17" "DC1" "device control one")
			     (#\C-r "18" "DC2" "device control two")
			     (#\C-s "19" "DC3" "device control three")
			     (#\C-t "20" "DC4" "device control four")
			     (#\C-u "21" "NAK" "negative acknowledge")
			     (#\C-v "22" "SYN" "syncronous idle")
			     (#\C-w "23" "ETB" "end of transmission block")
			     (#\C-x "24" "CAN" "cancel")
			     (#\C-y "25" "EM" "end of medium")
			     (#\C-z "26" "SUB" "substitute")
			     (#\C-[ "27" "ESC" "escape")
			     (#\C-\\ "28" "IS4" "file separator")
			     (#\C-] "29" "IS3" "group separator")
			     (#\C-^ "30" "IS2" "record separator")
			     (#\C-_ "31" "IS1" "unit separator")
			     (#\C-? "127" "DEL" "delete"))))
		  '((list :column (0 -3 5 30) :must-match t :enable (IDOK))))
    (when result
      (insert (cadr (assoc 'list data))))))

(defun select-xyzzy ()
  (interactive)
  (let ((list (list-xyzzy-windows)))
    (multiple-value-bind (result data)
	(dialog-box '(dialog 0 0 219 131
		      (:caption "xyzzyの選択")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:listbox list nil #x50b10101 7 7 205 97)
		       (:button IDOK "OK" #x50010001 107 110 50 14)
		       (:button IDCANCEL "キャンセル" #x50010000 162 110 50 14)))
		    (list (cons 'list (mapcar #'cdr list)))
		    '((list :must-match t :enable (IDOK))))
      (when result
	(let ((x (find (cdr (assoc 'list data)) list :test #'equal :key #'cdr)))
	  (when x
	    (activate-xyzzy-window (car x))))))))

(defun create-xyzzy-shortcut-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (dialog-box `(dialog 0 0 200 120
		    (:caption "ショートカットの作成")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:button nil "プログラム:" #x50000007 7 7 186 30)
		     (:button xyzzycli-exe "xyzzycli.exe (&C)" #x50030009 12 20 80 10)
		     (:button xyzzy-exe "xyzzy.exe (&X)" #x50000009 102 20 80 10)
		     (:button nil "ショートカットの作成先:" #x50000007 7 46 186 45)
		     (:button :desktop "デスクトップ (&D)" #x50010003 12 59 80 10)
		     (:button :send-to "送る (&T)" #x50010003 102 59 80 10)
		     (:button :programs "プログラムメニュー (&P)" #x50010003 12 72 80 10)
		     ,@(unless (featurep :windows-7)
			 `((:button :start-menu "スタートメニュー (&S)" #x50010003 102 72 80 10)))
		     (:button IDOK "OK" #x50010001 88 100 50 14)
		     (:button IDCANCEL "キャンセル" #x50010000 144 100 50 14)))
		  '((xyzzycli-exe . t))
		  nil)
    (when result
      (let ((program (cond ((cdr (assoc 'xyzzycli-exe data))
			    "xyzzycli.exe")
			   ((cdr (assoc 'xyzzy-exe data))
			    "xyzzy.exe")))
	    dests)
	(when program
	  (dolist (k '(:desktop :start-menu :programs :send-to))
	    (let ((v (cdr (assoc k data))))
	      (when v
		(create-shortcut (merge-pathnames program (si:system-root))
				 (get-special-folder-location k)
				 :appid (si:*app-user-model-id))
		(push k dests)
		)))
	  (when dests
	    (message "~Aのショートカットを~{~A~^、~}に作成しました。" program dests))
	  )))))
