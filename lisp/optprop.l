;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "optprop")

(in-package "editor")

(export '(add-option-prop-page option-property-sheet))

(defvar *option-prop-page* '(font-page))
(defvar *option-prop-page-no* nil)

(defun add-option-prop-page (ident tmpl init handlers result)
  (setq *option-prop-page* (delete ident *option-prop-page* :test #'eq
				   :key #'(lambda (x) (if (consp x) (car x) nil))))
  (setf (get ident 'prop-result) result)
  (push (list ident tmpl init handlers) *option-prop-page*))

(defun option-property-sheet ()
  (interactive)
  (let (sheet result)
    (dolist (page *option-prop-page*)
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
    (multiple-value-setq (result *option-prop-page-no*)
      (property-sheet sheet "共通設定" *option-prop-page-no*))
    (dolist (r result)
      (let ((f (get (car r) 'prop-result)))
	(when f
	  (funcall f (cdr r)))))))

(add-option-prop-page
 'disp
 '(dialog 0 0 263 195
   (:caption "表示")
   (:control
    (:button nil "表示する?" #x50000007 4 3 181 79)
    (:button vscr "垂直スクロールバー(&V)" #x50010003 10 15 84 10)
    (:button hscr "水平スクロールバー(&H)" #x50010003 97 15 84 10)
    (:button lfd "改行(&L)" #x50010003 10 26 38 10)
    (:button htab "TAB(&T)" #x50010003 62 26 38 10)
    (:button zspc "全角スペース(&Z)" #x50010003 113 26 64 10)
    (:button linenum "行番号(&#)" #x50010003 10 37 45 10)
    (:button eof "EOF(&E)" #x50010003 62 37 37 10)
    (:button hspc "半角スペース(&Y)" #x50010003 113 37 64 10)
    (:button ruler "ルーラ(&+)" #x50010003 10 48 43 10)
    (:button fold "折り返し(&F)" #x50010003 62 48 49 10)
    (:button foldline "折り返しライン(&U)" #x50010003 113 48 66 10)
    (:button modeline "モード行(&M)" #x50010003 10 58 50 10)
    (:button cursor "行カーソル(&O)" #x50010003 62 58 57 10)
    (:button inverse "選択ウィンドウのモード行に色をつける(&J)" #x50010003 18 69 134 10)
    (:button nil "ファンクションキー" #x50000007 188 3 70 79)
    (:button fkeyn "表示しない(&N)" #x50020009 194 15 57 10)
    (:button fkey4 "&4個" #x50000009 194 31 26 10)
    (:button fkey5 "&5個" #x50000009 194 47 26 10)
    (:button fkey8 "&8個" #x50000009 194 63 26 10)
    (:button fkey10 "1&0個" #x50000009 226 31 30 10)
    (:button fkey12 "1&2個" #x50000009 226 47 30 10)
    (:button fkey15 "&15個" #x50000009 226 63 30 10)
    (:button nil "テキストの折り返し" #x50020007 4 85 109 55)
    (:button foldno "しない(&G)" #x50030009 9 98 43 10)
    (:button foldwin "ウィンドウの幅(&W)" #x50000009 9 111 67 10)
    (:button foldcol "位置指定(&C):" #x50000009 9 124 54 10)
    (:edit column nil #x50830086 69 122 40 14)
    (:spin colspin nil #x500000b6 101 122 9 14)
    (:static nil "挿入時のキャレット(&I):" #x50020000 125 89 64 8)
    (:combobox inscaret nil #x50210003 211 87 48 66)
    (:static nil "上書き時のキャレット(&B):" #x50020000 125 103 72 8)
    (:combobox ovwrtcaret nil #x50210003 211 101 48 66)
    (:static nil "マウスカーソル(&S):" #x50020000 125 117 55 8)
    (:combobox mousecur nil #x50210003 211 115 48 66)
    (:static nil "タブの幅(&X):" #x50020000 125 132 36 8)
    (:edit tab nil #x50810086 211 129 48 14)
    (:spin tabspin nil #x500000b6 250 135 7 14)
    (:button kwd "キーワードに色をつける(&K)" #x50010003 4 146 93 10)
    (:button newline "行番号は表示行(&D)" #x50010003 4 158 73 10)
    (:button caption "キャプションの表示順を逆にする(&R)" #x50010003 4 170 119 10)
    (:button hidecursor "マウスカーソルを消す(&@)" #x50010003 4 181 89 10)
    (:button blink "キャレットを点滅(&A)" #x50010003 125 146 73 10)
    (:button markline "マーク行の行番号を反転(&P)" #x50010003 125 158 97 10)
    (:button rev "ちゃんと反転する(&Q)" #x50010003 125 170 75 10)
    (:button horztext "タブバーを横書きにする(&*)" #x50010003 125 181 93 10)))
 #'(lambda ()
     (let ((flags (get-window-flags)))
       (list (cons 'vscr (not (zerop (logand flags *window-flag-vscroll-bar*))))
	     (cons 'hscr (not (zerop (logand flags *window-flag-hscroll-bar*))))
	     (cons 'lfd (not (zerop (logand flags *window-flag-newline*))))
	     (cons 'htab (not (zerop (logand flags *window-flag-tab*))))
	     (cons 'zspc (not (zerop (logand flags *window-flag-full-width-space*))))
	     (cons 'hspc (not (zerop (logand flags *window-flag-half-width-space*))))
	     (cons 'linenum (not (zerop (logand flags *window-flag-line-number*))))
	     (cons 'ruler (not (zerop (logand flags *window-flag-ruler*))))
	     (cons 'eof (not (zerop (logand flags *window-flag-eof*))))
	     (cons 'fold (not (zerop (logand flags *window-flag-fold-mark*))))
	     (cons 'foldline (not (zerop (logand flags *window-flag-fold-line*))))
	     (cons 'modeline (not (zerop (logand flags *window-flag-mode-line*))))
	     (cons 'cursor (not (zerop (logand flags *window-flag-cursor-line*))))
	     (cons 'rev (not (zerop (logand flags *window-flag-just-inverse*))))
	     (cons 'foldno (null (default-fold-width)))
	     (cons 'foldwin (eq (default-fold-width) 't))
	     (cons 'foldcol (integerp (default-fold-width)))
	     (cons 'colspin (if (integerp (default-fold-width))
				(default-fold-width)
			      (default-value 'buffer-fold-width)))
	     (cons 'newline (default-line-number-mode))
	     (cons 'inverse *inverse-mode-line*)
	     (cons 'kwd (default-value 'highlight-keyword))
	     (cons 'inscaret '("ブロック" "細いやつ" "1/2ブロック" "下線"))
	     (cons 'inscaret *normal-caret-shape*)
	     (cons 'ovwrtcaret '("ブロック" "細いやつ" "1/2ブロック" "下線"))
	     (cons 'ovwrtcaret *overwrite-caret-shape*)
	     (cons 'mousecur '("ふつ～の" "細いの"))
	     (cons 'mousecur (if (eq *cursor-shape* :arrow) 0 1))
	     (cons 'tabspin (tab-columns))
	     (cons (if (zerop (logand flags *window-flag-function-bar*))
		       'fkeyn
		     (case (number-of-function-bar-labels)
		       (4 'fkey4)
		       (5 'fkey5)
		       (8 'fkey8)
		       (10 'fkey10)
		       (12 'fkey12)
		       (t 'fkey15)))
		   t)
	     (cons 'caption *title-bar-text-order*)
	     (cons 'blink *blink-caret*)
	     (cons 'horztext *tab-bar-horizontal-text*)
	     (cons 'hidecursor *hide-mouse-cursor*)
	     (cons 'markline (default-value '*inverse-mark-line*)))))
 '((modeline :enable (inverse))
   (inscaret :index t)
   (ovwrtcaret :index t)
   (mousecur :index t)
   (tabspin :min 1 :max 32)
   (tab :type integer :min 1 :max 32
	:range-error "1から32を指定してください"
	:type-error "数値を入力してください")
   (colspin :min 4 :max 30000)
   (column :type integer :min 4 :max 30000
	   :range-error "折り返しは4から30000です"
	   :type-error "数値を入力してください"))
 #'(lambda (data)
     (let ((f 0))
       (if (cdr (assoc 'vscr data))
	   (setq f (+ f *window-flag-vscroll-bar*)))
       (if (cdr (assoc 'hscr data))
	   (setq f (+ f *window-flag-hscroll-bar*)))
       (if (cdr (assoc 'lfd data))
	   (setq f (+ f *window-flag-newline*)))
       (if (cdr (assoc 'htab data))
	   (setq f (+ f *window-flag-tab*)))
       (if (cdr (assoc 'zspc data))
	   (setq f (+ f *window-flag-full-width-space*)))
       (if (cdr (assoc 'hspc data))
	   (setq f (+ f *window-flag-half-width-space*)))
       (if (cdr (assoc 'linenum data))
	   (setq f (+ f *window-flag-line-number*)))
       (if (cdr (assoc 'ruler data))
	   (setq f (+ f *window-flag-ruler*)))
       (if (cdr (assoc 'eof data))
	   (setq f (+ f *window-flag-eof*)))
       (if (cdr (assoc 'fold data))
	   (setq f (+ f *window-flag-fold-mark*)))
       (if (cdr (assoc 'foldline data))
	   (setq f (+ f *window-flag-fold-line*)))
       (if (cdr (assoc 'modeline data))
	   (setq f (+ f *window-flag-mode-line*)))
       (if (cdr (assoc 'cursor data))
	   (setq f (+ f *window-flag-cursor-line*)))
       (if (cdr (assoc 'rev data))
	   (setq f (+ f *window-flag-just-inverse*)))
       (unless (cdr (assoc 'fkeyn data))
	 (setq f (+ f *window-flag-function-bar*))
	 (set-number-of-function-bar-labels
	  (cond ((cdr (assoc 'fkey4 data)) 4)
		((cdr (assoc 'fkey5 data)) 5)
		((cdr (assoc 'fkey8 data)) 8)
		((cdr (assoc 'fkey10 data)) 10)
		((cdr (assoc 'fkey12 data)) 12)
		(t 15))))
       (set-window-flags f))
     (when (cdr (assoc 'modeline data))
       (setq *inverse-mode-line* (cdr (assoc 'inverse data))))
     (setq-default highlight-keyword (cdr (assoc 'kwd data)))
     (setq *title-bar-text-order* (cdr (assoc 'caption data)))
     (set-default-fold-width (cond ((cdr (assoc 'foldno data)) 'nil)
				   ((cdr (assoc 'foldwin data)) 't)
				   (t (cdr (assoc 'column data)))))
     (when (integerp (default-fold-width))
       (setq-default buffer-fold-width (default-fold-width)))
     (set-default-line-number-mode (cdr (assoc 'newline data)))
     (set-tab-columns (setq *tab-columns* (cdr (assoc 'tab data))))
     (setq *normal-caret-shape* (cdr (assoc 'inscaret data)))
     (setq *overwrite-caret-shape* (cdr (assoc 'ovwrtcaret data)))
     (set-cursor (if (zerop (cdr (assoc 'mousecur data))) :arrow :ibeam))
     (setq *blink-caret* (cdr (assoc 'blink data)))
     (let ((x (cdr (assoc 'horztext data))))
       (unless (eq x *tab-bar-horizontal-text*)
	 (setq *tab-bar-horizontal-text* x)
	 (refresh-tool-bars)))
     (setq *hide-mouse-cursor* (cdr (assoc 'hidecursor data)))
     (setq *inverse-mark-line* (cdr (assoc 'markline data)))))

(register-history-variable
 '(*inverse-mode-line* *normal-caret-shape* *overwrite-caret-shape*
   *title-bar-text-order* highlight-keyword *blink-caret* *tab-columns*
   *inverse-mark-line* *cursor-shape* *hide-mouse-cursor*
   buffer-fold-width *tab-bar-horizontal-text*))

(defvar *backup-prop-page-template*
  '(dialog 0 0 194 165
	(:caption "バックアップファイル")
	(:control
	 (:button backup "バックアップファイルをつくる(&B)" #x50010003 7 7 104 10)
	 (:button bkframe "番号付きバックアップファイルの作り方" #x50000007 15 21 167 121)
	 (:button numbered "常に作る(&L)" #x50030009 23 37 62 10)
	 (:button ifexist "番号付きバックアップファイルがある場合のみ(&Y)" #x50000009 23 51 141 10)
	 (:button never "作らない(&R)" #x50000009 23 65 60 10)
	 (:static bk1 "古いバージョンをいくつ残す?(&O):" #x50020000 23 87 103 8)
	 (:edit oldver nil #x50810086 129 83 29 14)
	 (:spin overspin nil #x500000b6 156 83 10 14)
	 (:static bk2 "新しいバージョンをいくつ残す?(&N):" #x50020000 23 105 105 8)
	 (:edit newver nil #x50810086 129 101 29 14)
	 (:spin nverspin nil #x500000b6 157 101 10 14)
	 (:button pack "番号が最後までいったら勝手に詰める(&P)" #x50010003 24 123 153 10)
	 (:button always "ファイルを保存したら常にバックアップファイルを作る(&W)" #x50010003 15 148 171 10))))

(defvar *backup-prop-page-handlers*
  '((backup :enable (bkframe numbered ifexist never always
			     bk1 bk2 oldver newver overspin nverspin))
    (overspin :min 0 :max 50)
    (oldver :min 0 :max 50 :range-error "0から50を指定してください"
	    :type integer :type-error "数値を入力してください")
    (nverspin :min 1 :max 50)
    (newver :min 1 :max 50 :range-error "1から50を指定してください"
	    :type integer :type-error "数値を入力してください")))

(add-option-prop-page
 'backup-file
 *backup-prop-page-template*
 #'(lambda ()
     (let ((verctl (default-value 'version-control)))
       (list (cons 'backup (default-value 'make-backup-files))
	     (cons 'numbered (and verctl (not (eq verctl 'never))))
	     (cons 'ifexist (null verctl))
	     (cons 'never (eq verctl 'never))
	     (cons 'overspin (default-value 'kept-old-versions))
	     (cons 'nverspin (default-value 'kept-new-versions))
	     (cons 'pack (default-value 'pack-backup-file-name))
	     (cons 'always (default-value 'make-backup-file-always)))))
 *backup-prop-page-handlers*
 #'(lambda (data)
     (setq-default make-backup-files (cdr (assoc 'backup data)))
     (when (default-value 'make-backup-files)
       (setq-default version-control (cond ((cdr (assoc 'numbered data))
					    t)
					   ((cdr (assoc 'ifexist data))
					    nil)
					   (t
					    'never)))
       (setq-default kept-old-versions (cdr (assoc 'oldver data)))
       (setq-default kept-new-versions (cdr (assoc 'newver data)))
       (setq-default pack-backup-file-name (cdr (assoc 'pack data)))
       (setq-default make-backup-file-always (cdr (assoc 'always data))))))

(register-history-variable
 '(make-backup-files version-control kept-old-versions kept-new-versions
   make-backup-file-always pack-backup-file-name))

(add-option-prop-page
 'read-file
 '(dialog 0 0 242 163
   (:caption "読み込み")
   (:control
    (:static nil "文字エンコーディングの判定(&E)" #x50020000 7 7 92 8)
    (:listbox encoding nil #x50a10101 7 19 110 115)
    (:button nil "改行とEOFの解釈" #x50000007 124 7 71 81)
    (:button fauto "てきとーに(&P)" #x50030009 129 25 55 10)
    (:button dos "DOS風(&D)" #x50000009 129 41 45 10)
    (:button unix "UNIX風(&X)" #x50000009 129 57 46 10)
    (:button mac "MAC風(&C)" #x50000009 129 73 46 10)
    (:button mule "エンコーディングの自動判定でMule-UCSの変なUTF-8も勘弁しちゃる(&U)" #x50010003 7 146 226 10)))
 #'(lambda ()
     (let ((eol (default-value '*expected-eol-code*)))
       (list (cons 'encoding *char-encoding-list*)
	     (cons 'encoding (or (position (default-value '*expected-fileio-encoding*)
					   *char-encoding-list*)
				 t))
	     (cons 'unix (eql eol *eol-lf*))
	     (cons 'dos (eql eol *eol-crlf*))
	     (cons 'mac (eql eol *eol-cr*))
	     (cons 'fauto (eql eol *eol-guess*))
	     (cons 'mule *accept-mule-ucs-funny-utf8*))))
 '((encoding :index t))
 #'(lambda (data)
     (let ((n (cdr (assoc 'encoding data))))
       (when n
	 (setq-default *expected-fileio-encoding* (nth n *char-encoding-list*))))
     (setq-default *expected-eol-code* (cond ((cdr (assoc 'unix data)) *eol-lf*)
					     ((cdr (assoc 'dos data)) *eol-crlf*)
					     ((cdr (assoc 'mac data)) *eol-cr*)
					     (t *eol-guess*)))
     (setq *accept-mule-ucs-funny-utf8* (cdr (assoc 'mule data)))))

(register-history-variable '(*expected-fileio-encoding* *expected-eol-code*
			     *accept-mule-ucs-funny-utf8*))

(add-option-prop-page
 'lock
 '(dialog 0 0 163 113
   (:caption "排他制御")
   (:control
    (:button nil "排他制御" #x50000007 7 7 149 60)
    (:button no "しない(&N)" #x50030009 15 22 43 10)
    (:button open "開いたとき(&O)" #x50000009 15 36 55 10)
    (:button edit "編集してるときだけ(&E)" #x50000009 15 50 79 10)
    (:button way "ロックの方法" #x50000007 7 71 149 35)
    (:button share "共有ロック(&S)" #x50030009 15 87 55 10)
    (:button exclusive "排他ロック(&E)" #x50000009 84 87 55 10)))
 #'(lambda ()
     (let ((lock (default-value 'lock-file))
	   (mode (default-value 'exclusive-lock-file)))
       (list (cons 'no (null lock))
	     (cons 'open (and lock (not (eq lock :edit))))
	     (cons 'edit (eq lock :edit))
	     (cons 'share (null mode))
	     (cons 'exclusive mode))))
 '((no :disable (way share exclusive))
   (open :enable (way share exclusive))
   (edit :enable (way share exclusive)))
 #'(lambda (data)
     (setq-default lock-file (cond ((cdr (assoc 'open data))
				    t)
				   ((cdr (assoc 'edit data))
				    :edit)
				   (t nil)))
     (when (default-value 'lock-file)
       (setq-default exclusive-lock-file (cdr (assoc 'exclusive data))))))

(register-history-variable '(lock-file exclusive-lock-file))

(add-option-prop-page
 'auto-save
 '(dialog 0 0 190 59
   (:caption "自動セーブ")
   (:control
    (:button autosave "自動セーブする(&U)" #x50010003 7 7 71 10)
    (:static as1 "自動セーブするキーストロークの回数(&I):" #x50020000 17 25 118 8)
    (:edit interval nil #x50810086 139 21 33 14)
    (:spin intspin "" #x500000b6 163 22 10 14)
    (:static as2 "自動セーブの間隔(&T):" #x50020000 17 42 64 8)
    (:edit itimer nil #x50810086 139 39 33 14)
    (:spin timerspin "" #x500000b6 163 38 10 14)
    (:static as3 "秒" #x50020000 175 42 8 8)))
 #'(lambda ()
     (list (cons 'autosave (default-value 'auto-save))
	   (cons 'intspin *auto-save-interval*)
	   (cons 'timerspin *auto-save-interval-timer*)))
 '((interval :min 0 :range-error "0以上を指定してください"
	     :type integer :type-error "数値を入力してください")
   (intspin :min 0 :max 10000)
   (itimer :min 0 :range-error "0以上を指定してください"
	   :type integer :type-error "数値を入力してください")
   (timerspin :min 0 :max 10000)
   (autosave :enable (interval intspin itimer timerspin as1 as2 as3)))
 #'(lambda (data)
     (setq-default auto-save (cdr (assoc 'autosave data)))
     (when (default-value 'auto-save)
       (setq *auto-save-interval* (cdr (assoc 'interval data)))
       (setq *auto-save-interval-timer* (cdr (assoc 'itimer data))))))

(register-history-variable
 '(auto-save *auto-save-interval* *auto-save-interval-timer*))

(defconstant *buffer-list-sort-by-name* 0)
(defconstant *buffer-list-sort-by-size* 1)
(defconstant *buffer-list-sort-by-mode* 2)
(defconstant *buffer-list-sort-by-file* 3)

(add-option-prop-page
 'misc
 '(dialog 0 0 198 195
   (:caption "いろいろ")
   (:control
    (:button ime "複数キーストロークのIME制御(&I)" #x50010003 7 7 110 10)
    (:button saveime "バッファごとにIMEの状態を覚えとく(&V)" #x50010003 7 18 125 10)
    (:button mini "ミニバッファのIMEの状態を覚えとく(&F)" #x50010003 7 29 125 10)
    (:button gime "Global IMEを使う(&G)" #x50010003 7 40 74 10)
    (:button ime2k "MS-IME 2000でC-\\\\を入力したい(&E)" #x50010003 7 51 122 10)
    (:button reconvert "IMEの再変換をする(&O)" #x50010003 7 62 82 10)
    (:button alt "なんでALTでメニューが開かない?(&A)" #x50010003 7 73 123 10)
    (:button halfpage "ページスクロールは半画面ずつ(&P)" #x50010003 7 84 116 10)
    (:button sortc "バッファリストを作成順にソートする(&C)" #x50010003 7 95 126 10)
    (:button bsort "バッファリストを大文字小文字を区別しないでソートする(&B)" #x50010003 7 106 184 10)
    (:button save-sort-flags "バッファリストで指定したソート方法を保存する(&T)" #x50010003 7 117 184 10)
    (:button nil "バッファリストのソート方法のデフォルト" #x50000007 11 130 200 40)
    (:button sort-by-buffer-name "バッファ名(&W)" #x50030009 16 141 48 10)
    (:button sort-by-buffer-size "サイズ(&X)" #x50000009 66 141 42 10)
    (:button sort-by-buffer-mode "モード(&Y)" #x50000009 110 141 44 10)
    (:button sort-by-buffer-file "ファイル名(&Z)" #x50000009 156 141 50 10)
    (:button sort-reverse "降順でソートする(&R)" #x50010003 16 154 184 10)
    (:static nil "スクロールマージン(&R):" #x50020000 7 175 68 8)
    (:edit margin nil #x50810086 119 172 39 14)
    (:spin marginspin nil #x500000b6 158 171 9 14)
    (:static nil "アンドゥの回数(&U):" #x50020000 7 189 54 8)
    (:edit undo nil #x50810086 119 186 39 14)
    (:spin undospin nil #x500000b6 158 185 9 14)
    (:static nil "ヒストリの個数(&H):" #x50020000 7 203 53 8)
    (:edit hist nil #x50810086 119 200 39 14)
    (:spin histspin nil #x500000b6 158 199 9 14)
    (:static nil "保存するヒストリの個数(&S):" #x50020000 7 217 79 8)
    (:edit savehist nil #x50810086 119 214 39 14)
    (:spin savehistspin nil #x500000b6 158 214 9 14)
    (:static nil "メニューに表示するファイル履歴(&M):" #x50020000 7 231 106 8)
    (:edit menuhist nil #x50810086 119 228 39 14)
    (:spin menuhistspin nil #x500000b6 158 228 9 14)))
 #'(lambda ()
     (list (cons 'ime *ime-control*)
	   (cons 'saveime *save-buffer-ime-mode*)
	   (cons 'mini *minibuffer-save-ime-status*)
	   (cons 'gime *use-global-ime*)
	   (cons 'ime2k *ime-does-not-process-C-\\*)
	   (cons 'alt (null *enable-meta-key*))
	   (cons 'reconvert *enable-ime-reconvert*)
	   (cons 'halfpage *page-scroll-half-window*)
	   (cons 'bsort *buffer-list-sort-ignore-case*)
	   (cons 'sortc *sort-buffer-list-by-created-order*)
	   (cons 'sort-reverse *buffer-list-sort-reverse*)
	   (cons 'save-sort-flags *save-buffer-list-sort-flags*)
	   (cons 'sort-by-buffer-name (member *buffer-list-sort-type* '(nil *buffer-list-sort-by-name*)))
	   (cons 'sort-by-buffer-size (eq *buffer-list-sort-type* *buffer-list-sort-by-size*))
	   (cons 'sort-by-buffer-mode (eq *buffer-list-sort-type* *buffer-list-sort-by-mode*))
	   (cons 'sort-by-buffer-file (eq *buffer-list-sort-type* *buffer-list-sort-by-file*))
	   (cons 'histspin *minibuffer-maximum-history-count*)
	   (cons 'savehistspin *save-history*)
	   (cons 'menuhistspin *menu-history-max*)
	   (cons 'marginspin (default-value '*scroll-margin*))
	   (cons 'undospin (default-value 'kept-undo-information))))
 '((sortc :disable (bsort sort-reverse save-sort-flags
			  sort-by-buffer-name sort-by-buffer-size
			  sort-by-buffer-mode sort-by-buffer-file))
   (marginspin :min 0 :max 1000)
   (margin :min 0 :range-error "0以上を指定してください"
	   :type integer :type-error "数値を入力してください")
   (histspin :min 0 :max 10000)
   (hist :min 0 :range-error "0以上を指定してください"
	 :type integer :type-error "数値を入力してください")
   (savehistspin :min 0 :max 10000)
   (savehist :min 0 :range-error "0以上を指定してください"
	     :type integer :type-error "数値を入力してください")
   (menuhistspin :min 0 :max 16)
   (menuhist :min 0 :max 16 :range-error "0から16を指定してください"
	     :type integer :type-error "数値を入力してください")
   (undospin :min 0 :max 10000)
   (undo :min 0 :range-error "0以上を指定してください"
	 :type integer :type-error "数値を入力してください"))
 #'(lambda (data)
     (setq *ime-control* (cdr (assoc 'ime data)))
     (setq *save-buffer-ime-mode* (cdr (assoc 'saveime data)))
     (setq *minibuffer-save-ime-status* (cdr (assoc 'mini data)))
     (setq *use-global-ime* (cdr (assoc 'gime data)))
     (setq *ime-does-not-process-C-\\* (cdr (assoc 'ime2k data)))
     (unless (ignore-errors (enable-global-ime *use-global-ime*))
       (warn "Global IMEは使用できません"))
     (setq *enable-ime-reconvert* (cdr (assoc 'reconvert data)))
     (setq *enable-meta-key* (not (cdr (assoc 'alt data))))
     (setq *page-scroll-half-window* (cdr (assoc 'halfpage data)))
     (setq *buffer-list-sort-ignore-case* (cdr (assoc 'bsort data)))
     (setq *sort-buffer-list-by-created-order* (cdr (assoc 'sortc data)))
     (setq *buffer-list-sort-reverse* (cdr (assoc 'sort-reverse data)))
     (setq *save-buffer-list-sort-flags* (cdr (assoc 'save-sort-flags data)))
     (setq *buffer-list-sort-type*
	   (cond ((cdr (assoc 'sort-by-buffer-name data)) *buffer-list-sort-by-name*)
		 ((cdr (assoc 'sort-by-buffer-size data)) *buffer-list-sort-by-size*)
		 ((cdr (assoc 'sort-by-buffer-mode data)) *buffer-list-sort-by-mode*)
		 ((cdr (assoc 'sort-by-buffer-file data)) *buffer-list-sort-by-file*)
		 (t *buffer-list-sort-by-name*)))
     (setq *minibuffer-maximum-history-count* (cdr (assoc 'hist data)))
     (let ((margin (cdr (assoc 'margin data)))
	   (omargin (default-value '*scroll-margin*)))
       (setq-default *scroll-margin* margin)
       (unless (eql margin omargin)
	 (refresh-screen t)))
     (setq *save-history* (cdr (assoc 'savehist data)))
     (let ((o *menu-history-max*))
       (setq *menu-history-max* (cdr (assoc 'menuhist data)))
       (unless (eql o *menu-history-max*)
	 (add-file-history-to-menu)))
     (setq-default kept-undo-information (cdr (assoc 'undo data)))))

(register-history-variable '(*use-global-ime* *ime-does-not-process-C-\\*
			     *scroll-margin* *enable-ime-reconvert*
			     *minibuffer-save-ime-status*))

(add-option-prop-page
 'misc2
 '(dialog 0 0 235 248
   (:caption "さまざま")
   (:control
    (:button nil "検索での表示" #x50000007 7 7 207 30)
    (:button reverse "反転する(&R)" #x50030009 12 20 51 10)
    (:button select "選択する(&S)" #x50000009 75 20 51 10)
    (:button not "何もしない(&N)" #x50000009 142 20 55 10)
    (:button nowrap "キーボードマクロ実行中は戻って検索しない(&M)" #x50030003 7 41 153 10)
    (:button sync "削除バッファとクリップボードを同期する(&C)" #x50010003 7 54 173 10)
    (:button nil "同期の方法" #x50000007 14 67 200 30)
    (:button sync-harfway "中途半端に同期(&J)" #x50030009 19 80 80 10)
    (:button sync-always "完全に同期(&K)" #x50000009 100 80 80 10)
    (:button shodat "クリップボードへのコピーで範囲をクリアしない(&T)" #x50010003 7 101 157 10)
    (:button bra "[...] はワイルドカード(&I)" #x50010003 7 114 80 10)
    (:button DnD "D&&D編集する(&D)" #x50010003 7 127 65 10)
    (:button wheel "ホイールマウスに反応する(&H)" #x50010003 7 140 102 10)
    (:button resume "終了時の状態を保存する(&U)" #x50010003 7 153 98 10)
    (:button mild "エラーの表示をマイルドに(&L)" #x50010003 7 166 99 10)
    (:button rwinpos "起動時にウィンドウの位置を復元(&F)" #x50010003 7 179 119 10)
    (:button rwinsize "起動時にウィンドウサイズを復元(&G)" #x50010003 7 192 118 10)
    (:button swinpos "終了時にウィンドウの位置を保存(&P)" #x50010003 7 205 119 10)
    (:button swinsize "終了時にウィンドウサイズを保存(&Z)" #x50010003 7 218 118 10)
    (:button ssnapsize "エアロスナップで変更した場合も保存(&X)" #x50010003 14 231 118 10)
    (:button beepnever "いちいち音を鳴らすな(&B)" #x50010003 123 114 87 10)
    (:button errbeep "エラーのとき音を鳴らす(&E)" #x50010003 130 127 93 10)
    (:button warnbeep "警告のとき音を鳴らす(&W)" #x50010003 130 140 89 10)
    (:button vbell "ビシブルベル(&V)" #x50010003 130 153 63 10)))
 #'(lambda ()
     (list (cons 'resume *save-resume-info*)
	   (cons 'beepnever *beep-on-never*)
	   (cons 'errbeep *beep-on-error*)
	   (cons 'warnbeep *beep-on-warn*)
	   (cons 'vbell *visible-bell*)
	   (cons 'mild si:*report-simple-errors-mildly*)
	   (cons 'nowrap *no-wrap-search-on-kbd-macro*)
	   (cons 'shodat *shodat-copy-mode*)
	   (cons 'bra *brackets-is-wildcard-character*)
	   (cons 'DnD *enable-D&D-edit*)
	   (cons 'wheel *support-mouse-wheel*)
	   (cons 'reverse *reverse-match*)
	   (cons 'select (and *select-match* (null *reverse-match*)))
	   (cons 'sync *sync-kill-ring-with-clipboard*)
	   (cons 'sync-harfway (and *sync-kill-ring-with-clipboard*
				    (not (eq *sync-kill-ring-with-clipboard* :always))))
	   (cons 'sync-always (eq *sync-kill-ring-with-clipboard* :always))
	   (cons 'non (null (or *select-match* *reverse-match*)))
	   (cons 'swinpos *save-window-position*)
	   (cons 'swinsize *save-window-size*)
	   (cons 'ssnapsize *save-window-snap-size*)
	   (cons 'rwinpos *restore-window-position*)
	   (cons 'rwinsize *restore-window-size*)))
 '((swinsize :enable (ssnapsize))
   (sync :enable (sync-harfway sync-always))
   (beepnever :disable (errbeep warnbeep vbell)))
 #'(lambda (data)
     (cond ((cdr (assoc 'reverse data))
	    (setq *reverse-match* t)
	    (setq *select-match* nil))
	   ((cdr (assoc 'select data))
	    (setq *reverse-match* nil)
	    (setq *select-match* t))
	   (t
	    (setq *reverse-match* nil)
	    (setq *select-match* nil)))
     (setq *sync-kill-ring-with-clipboard*
	   (cond ((cdr (assoc 'sync-always data)) :always)
		 ((cdr (assoc 'sync data)) t)
		 (t nil)))
     (setq *no-wrap-search-on-kbd-macro* (cdr (assoc 'nowrap data)))
     (setq *save-resume-info* (cdr (assoc 'resume data)))
     (setq si:*report-simple-errors-mildly* (cdr (assoc 'mild data)))
     (setq *beep-on-never* (cdr (assoc 'beepnever data)))
     (unless *beep-on-never*
       (setq *visible-bell* (cdr (assoc 'vbell data)))
       (setq *beep-on-error* (cdr (assoc 'errbeep data)))
       (setq *beep-on-warn* (cdr (assoc 'warnbeep data))))
     (setq *shodat-copy-mode* (cdr (assoc 'shodat data)))
     (setq *brackets-is-wildcard-character* (cdr (assoc 'bra data)))
     (setq *enable-D&D-edit* (cdr (assoc 'DnD data)))
     (setq *support-mouse-wheel* (cdr (assoc 'wheel data)))
     (setq *save-window-position* (cdr (assoc 'swinpos data)))
     (setq *save-window-size* (cdr (assoc 'swinsize data)))
     (when *save-window-size*
       (setq *save-window-snap-size* (cdr (assoc 'ssnapsize data))))
     (setq *restore-window-position* (cdr (assoc 'rwinpos data)))
     (setq *restore-window-size* (cdr (assoc 'rwinsize data)))
   ))

(register-history-variable
 '(*ime-control* *enable-meta-key* *page-scroll-half-window*
   *minibuffer-maximum-history-count* *beep-on-error*
   *beep-on-never* *beep-on-warn* *save-history* *menu-history-max*
   kept-undo-information *buffer-list-sort-ignore-case*
   *sort-buffer-list-by-created-order*
   *buffer-list-sort-reverse*
   *last-buffer-list-sort-flags*
   *save-buffer-list-sort-flags*
   *buffer-list-sort-type*
   *brackets-is-wildcard-character*
   *save-resume-info* *enable-D&D-edit* *select-match* *reverse-match*
   *no-wrap-search-on-kbd-macro* *sync-kill-ring-with-clipboard*
   *shodat-copy-mode* *save-buffer-ime-mode*
   si:*report-simple-errors-mildly* *visible-bell*
   *support-mouse-wheel*))

(add-option-prop-page
 'kinsoku
 '(dialog 0 0 245 175
   (:caption "禁則処理")
   (:control
    (:button nil "表示" #x50000007 7 7 114 61)
    (:button kinsoku "禁則処理をする(&K)" #x50010003 13 18 83 10)
    (:button wordwrap "ワードラップする(&W)" #x50010003 13 30 75 10)
    (:button newline "改行文字をぶら下げる(&N)" #x50010003 13 42 103 10)
    (:button space "空白文字をぶら下げる(&H)" #x50010003 13 54 104 10)
    (:button nil "Fill" #x50000007 124 7 114 61)
    (:button fkinsoku "禁則処理をする(&I)" #x50010003 130 19 83 10)
    (:button fwordwrap "ワードラップする(&R)" #x50010003 130 31 75 10)
    (:button fnewline "改行文字をぶら下げる(&L)" #x50010003 130 43 103 10)
    (:button fspace "空白文字をぶら下げる(&T)" #x50010003 130 55 104 10)
    (:static nil "行頭禁則文字(&B):" #x50020000 7 74 65 8)
    (:edit bolchars nil #x50810080 7 86 188 14)
    (:static nil "行末禁則文字(&E):" #x50020000 7 105 64 8)
    (:edit eolchars nil #x50810080 7 116 188 14)
    (:static nil "ぶら下げる文字数(&X):" #x50020000 7 138 78 8)
    (:edit extend nil #x50810086 85 135 40 14)
    (:spin extendspin "Spin1" #x500000b6 131 134 9 14)
    (:static nil "追い出す文字数(&S):" #x50020000 7 156 72 8)
    (:edit shorten nil #x50810086 85 153 40 14)
    (:spin shortenspin "Spin1" #x500000b6 131 153 9 14)))
 #'(lambda ()
     (list (cons 'kinsoku (not (zerop (logand (kinsoku-mode) 8))))
	   (cons 'wordwrap (not (zerop (logand (kinsoku-mode) 4))))
	   (cons 'space (not (zerop (logand (kinsoku-mode) 2))))
	   (cons 'newline (not (zerop (logand (kinsoku-mode) 1))))
	   (cons 'fkinsoku (not (zerop (logand *fill-kinsoku-mode* 8))))
	   (cons 'fwordwrap (not (zerop (logand *fill-kinsoku-mode* 4))))
	   (cons 'fspace (not (zerop (logand *fill-kinsoku-mode* 2))))
	   (cons 'fnewline (not (zerop (logand *fill-kinsoku-mode* 1))))
	   (cons 'bolchars (kinsoku-bol-chars))
	   (cons 'eolchars (kinsoku-eol-chars))
	   (cons 'extendspin (kinsoku-extend-limit))
	   (cons 'shortenspin (kinsoku-shorten-limit))))
 '((extendspin :min 0 :max 16)
   (shortenspin :min 0 :max 16)
   (extend :type integer :min 0 :max 16
	   :range-error "ぶら下げる文字数は0から16です"
	   :type-error "数値を入力してください")
   (shorten :type integer :min 0 :max 16
	    :range-error "追い出す文字数は0から16です"
	    :type-error "数値を入力してください"))
 #'(lambda (data)
     (set-kinsoku-mode (+ (if (cdr (assoc 'kinsoku data)) 8 0)
			  (if (cdr (assoc 'wordwrap data)) 4 0)
			  (if (cdr (assoc 'space data)) 2 0)
			  (if (cdr (assoc 'newline data)) 1 0)))
     (set-kinsoku-chars (cdr (assoc 'bolchars data))
			(cdr (assoc 'eolchars data)))
     (set-kinsoku-extend-limit (cdr (assoc 'extend data)))
     (set-kinsoku-shorten-limit (cdr (assoc 'shorten data)))
     (setq *fill-kinsoku-mode* (+ (if (cdr (assoc 'fkinsoku data)) 8 0)
				  (if (cdr (assoc 'fwordwrap data)) 4 0)
				  (if (cdr (assoc 'fspace data)) 2 0)
				  (if (cdr (assoc 'fnewline data)) 1 0)))
     (setq *kinsoku-mode* (kinsoku-mode))
     (setq *kinsoku-bol-chars* (kinsoku-bol-chars))
     (setq *kinsoku-eol-chars* (kinsoku-eol-chars))
     (setq *kinsoku-extend-limit* (kinsoku-extend-limit))
     (setq *kinsoku-shorten-limit* (kinsoku-shorten-limit))))

(register-history-variable
 '(*kinsoku-mode* *kinsoku-bol-chars* *kinsoku-eol-chars*
   *kinsoku-extend-limit* *kinsoku-shorten-limit*
   *fill-kinsoku-mode*))

(add-option-prop-page
 'directory
 '(dialog 0 0 255 169
   (:caption "ディレクトリ")
   (:control
    (:button nil "MSVC2.xヘルプ" #x50000007 7 7 242 45)
    (:static nil "ディレクトリ(&D):" #x50020000 12 21 45 8)
    (:edit vc2dir nil #x50810480 82 18 130 14)
    (:button vc2ref "参照..." #x50010000 216 18 28 14)
    (:static nil "idxファイル(&I)" #x50020000 12 37 40 8)
    (:edit idx nil #x50810480 82 34 130 14)
    (:button nil "MSDN Info viewer" #x50000007 7 54 242 45)
    (:static nil "ビューワ(&V):" #x50020000 13 67 36 8)
    (:edit ivpath nil #x50810480 82 64 130 14)
    (:button ivref "参照..." #x50010000 216 64 28 14)
    (:static nil "mvbファイル(&M):" #x50020000 13 82 47 8)
    (:edit mvb nil #x50810480 82 79 130 14)
    (:button mvbref "参照..." #x50010000 216 79 28 14)
    (:button nil "HTMLヘルプ" #x50000007 7 101 242 27)
    (:static nil "COL/CHMファイル(&H):" #x50020000 13 113 65 8)
    (:edit html nil #x50810480 82 110 130 14)
    (:button htmlref "参照..." #x50010000 216 110 28 14)
    (:button nil "辞書" #x50000007 7 131 242 27)
    (:static nil "ディレクトリ(&C):" #x50020000 13 143 44 8)
    (:edit dic nil #x50810480 82 140 130 14)
    (:button dicref "参照..." #x50010000 216 140 28 14)))
 #'(lambda ()
     (list (cons 'vc2dir *winhelp-directory*)
	   (cons 'idx (format nil "~{~A~^;~}" *winhelp-index-files*))
	   (cons 'ivpath *info-viewer-file-name*)
	   (cons 'mvb *info-viewer-title*)
	   (cons 'html *html-help-path*)
	   (cons 'dic *edict-dictionary-path*)))
 '((vc2ref :related vc2dir :directory-name-dialog (:title "参照"))
   (ivref :related ivpath
	  :file-name-dialog (:title "参照"
			     :filter (("実行ファイル(*.exe)" . "*.exe")
				      ("すべてのファイル(*.*)" . "*.*"))
			     :must-exist t))
   (mvbref :related mvb
	   :file-name-dialog (:title "参照"
			      :filter (("mvbファイル(*.mvb)" . "*.mvb")
				       ("すべてのファイル(*.*)" . "*.*"))
			      :must-exist t))
   (htmlref :related html
	   :file-name-dialog (:title "参照"
			      :filter (("HTMLHELPファイル(*.chm;*.col)" . "*.chm;*.col")
				       ("すべてのファイル(*.*)" . "*.*"))
			      :must-exist t))
   (dicref :related dic :directory-name-dialog (:title "参照")))
 #'(lambda (data)
     (setq *winhelp-directory* (cdr (assoc 'vc2dir data)))
     (let ((f (cdr (assoc 'ivpath data))))
       (setq *info-viewer-file-name* (and f (not (string-match "^ *$" f)) f)))
     (let ((f (cdr (assoc 'mvb data))))
       (setq *info-viewer-title* (and f (file-namestring f))))
     (let ((f (cdr (assoc 'idx data))))
       (setq *winhelp-index-files* (and f (split-string f #\; t " "))))
     (setq *html-help-path* (cdr (assoc 'html data)))
     (setq *edict-dictionary-path* (cdr (assoc 'dic data)))))

(register-history-variable
 '(*winhelp-directory* *info-viewer-file-name* *info-viewer-title*
   *winhelp-index-files* *edict-dictionary-path*
   *html-help-path*))

(add-option-prop-page
 'filer
 '(dialog 0 0 243 252
   (:caption "ファイラ")
   (:control
    (:button icon "アイコンをファイルから取ってくる(&I)" #x50010003 7 7 116 10)
    (:button query "なんかする前に必ず確認する(&Q)" #x50010003 126 7 111 10)
    (:button savemask "マスクを保存する(&M)" #x50010003 7 19 76 10)
    (:button modeless "2画面ファイラをモードレスに(&L)" #x50010003 126 19 105 10)
    (:button comma "ファイルサイズをカンマ区切りで表示(&C)" #x50010003 7 31 130 10)
    (:button savesnapsize "エアロスナップでウィンドウサイズを変更した場合も保存(&X)" #x50010003 7 43 182 10)
    (:button hidden "隠しファイル、隠しフォルダ、または隠しドライブを表示しない(&H)" #x50010003 7 55 182 10)
    (:button system "保護されたオペレーティング システム ファイルを表示しない(&J)" #x50010003 7 67 182 10)
    (:button use-recycle-bin "ファイルの削除にゴミ箱を使用する(&G)" #x50010003 7 79 182 10)
    (:button shell-operation "ファイルの移動・コピー・削除をエクスプローラに任せる(&W)" #x50010003 7 91 182 10)
    (:button precious "ファイルを削除するときにもっと丁寧に確認して(&P)" #x50010003 7 103 159 10)
    (:button rdonly "書込み禁止ファイルを何も言わずに消す(&R)" #x50010003 7 115 142 10)
    (:button dosname "MS-DOSファイル名が重複したとき勝手にリネームする(&U)" #x50010003 7 127 182 10)
    (:button nil "同名ファイルの確認のデフォルト" #x50000007 7 143 176 49)
    (:button yes "する(&Y)" #x50030009 13 155 37 10)
    (:button no "しない(&S)" #x50000009 54 155 42 10)
    (:button newer "日付が新しければ(&N)" #x50000009 100 155 78 10)
    (:button all "いちいち聞くな(&A)" #x50030003 13 167 66 10)
    (:button makedefault "新しく選択されたものをデフォルトにする(&D)" #x50010003 13 179 140 10)
    (:button nil "中にファイルがあるディレクトリを削除するとき" #x50000007 7 197 188 51)
    (:button force "有無を言わさずに消す(&O)" #x50030009 13 209 90 10)
    (:button firsttime "最初の一回だけ確認する(&F)" #x50000009 13 221 98 10)
    (:button everytime "毎回確認する(&E)" #x50000009 13 233 65 10)))
 #'(lambda ()
     (list (cons 'icon *filer-retrieve-icon*)
	   (cons 'query *filer-query-before-process*)
	   (cons 'precious *filer-query-delete-precisely*)
	   (cons 'rdonly *filer-delete-read-only-files*)
	   (cons 'modeless (not *filer-modal*))
	   (cons 'comma *filer-format-comma*)
	   (cons 'savemask *filer-save-file-mask*)
	   (cons 'savesnapsize *filer-save-window-snap-size*)
	   (cons 'dosname *rename-alternate-file-name*)
	   (cons 'hidden (not *filer-show-hidden-files*))
	   (cons 'system (not *filer-show-system-files*))
	   (cons 'use-recycle-bin *filer-use-recycle-bin*)
	   (cons 'shell-operation *filer-use-shell-file-operation*)
	   (cons 'yes (eq *filer-ask-same-name-default* :overwrite))
	   (cons 'no (eq *filer-ask-same-name-default* :skip))
	   (cons 'newer (eq *filer-ask-same-name-default* :newer))
	   (cons 'all *filer-ask-same-name-all-default*)
	   (cons 'makedefault *filer-ask-same-name-make-default*)
	   (cons 'force (eq *filer-delete-non-empty-directory* :force))
	   (cons 'firsttime (eq *filer-delete-non-empty-directory* :first-time))
	   (cons 'everytime (eq *filer-delete-non-empty-directory* :every-time))))
 '((shell-operation :disable (precious rdonly dosname
			      yes no newer all makedefault
			      force firsttime everytime)))
 #'(lambda (data)
     (setq *filer-retrieve-icon* (cdr (assoc 'icon data)))
     (setq *filer-query-before-process* (cdr (assoc 'query data)))
     (setq *filer-query-delete-precisely* (cdr (assoc 'precious data)))
     (setq *filer-delete-read-only-files* (cdr (assoc 'rdonly data)))
     (setq *filer-save-file-mask* (cdr (assoc 'savemask data)))
     (setq *filer-save-window-snap-size* (cdr (assoc 'savesnapsize data)))
     (setq *rename-alternate-file-name* (cdr (assoc 'dosname data)))
     (setq *filer-show-hidden-files* (not (cdr (assoc 'hidden data))))
     (setq *filer-show-system-files* (not (cdr (assoc 'system data))))
     (setq *filer-use-recycle-bin* (cdr (assoc 'use-recycle-bin data)))
     (setq *filer-use-shell-file-operation* (cdr (assoc 'shell-operation data)))
     (setq *filer-modal* (not (cdr (assoc 'modeless data))))
     (setq *filer-format-comma* (cdr (assoc 'comma data)))
     (setq *filer-ask-same-name-default*
	   (cond ((cdr (assoc 'yes data))
		  :overwrite)
		 ((cdr (assoc 'no data))
		  :skip)
		 (t
		  :newer)))
     (setq *filer-ask-same-name-all-default* (cdr (assoc 'all data)))
     (setq *filer-ask-same-name-make-default* (cdr (assoc 'makedefault data)))
     (setq *filer-delete-non-empty-directory*
	   (cond ((cdr (assoc 'force data))
		  :force)
		 ((cdr (assoc 'firsttime data))
		  :first-time)
		 (t
		  :every-time)))
     (mapc (if *filer-save-file-mask*
	       #'register-history-variable
	     #'unregister-history-variable)
	   '(*filer-primary-file-mask* *filer-secondary-file-mask*
	     *filer-last-file-mask*))))

(defvar *filer-save-file-mask* nil)
(export '*filer-save-file-mask*)

(register-history-variable
 '(*filer-retrieve-icon* *filer-query-before-process*
   *filer-query-delete-precisely* *filer-ask-same-name-default*
   *filer-ask-same-name-all-default* *filer-ask-same-name-make-default*
   *filer-delete-read-only-files* *filer-delete-non-empty-directory*
   *filer-save-file-mask* *filer-modal* *filer-format-comma*
   *filer-save-window-snap-size*
   *rename-alternate-file-name*
   *filer-show-hidden-files* *filer-show-system-files*
   *filer-use-recycle-bin*
   *filer-use-shell-file-operation*
   ))

