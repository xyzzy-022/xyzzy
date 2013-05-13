;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "app-menu")

(in-package "editor")

(export '(*app-menu* *app-popup-menu* *menu-history-max*
	  *app-rectangle-popup-menu* *clipboard-char-encoding-popup-menu*
	  *dictionary-popup-menu* *paste-rectangle-popup-menu*
	  *command-bar-popup-menu*
	  make-char-encoding-popup-menu
	  add-file-history-to-menu *menu-display-length*
	  *init-app-menus-hook*))

(defvar *app-menu* nil)
(defvar *app-popup-menu* nil)
(defvar *app-rectangle-popup-menu* nil)
(defvar *clipboard-char-encoding-popup-menu* nil)
(defvar *dictionary-popup-menu* nil)
(defvar *paste-rectangle-popup-menu* nil)
(defvar *command-bar-popup-menu* nil)

(defvar *recent-file-popup-menu* nil)
(defvar *recent-session-popup-menu* nil)

(defvar *menu-history-max* 9)

(defvar *init-app-menus-hook* nil)

(defun init-app-menus ()

  (setq *clipboard-char-encoding-popup-menu*
	(make-char-encoding-popup-menu
	 #'(lambda (encoding)
	     (interactive)
	     (continue-pre-selection)
	     (setq *clipboard-char-encoding* encoding))
	 #'(lambda (encoding)
	     (and (eq *clipboard-char-encoding* encoding) :check))
	 *clipboard-char-encoding-list*))

  (setq *dictionary-popup-menu*
	(define-popup-menu ':dictionary-popup
	  (:item :tag-command "和英(&J)"
	   'lookup-j2e-dictionary-selection)
	  (:item :tag-command "読み(&R)"
	   'lookup-reading-dictionary-selection)
	  (:item :tag-command "英和(&E)"
	   'lookup-e2j-dictionary-selection)
	  (:item :tag-command "熟語(&I)"
	   'lookup-idiom-dictionary-selection)
	  (:sep :above-edict-mode)
	  (:item :tag-command "辞書引きモード(&D)"
	   'edict-mode
	   #'(lambda () (if edict-mode :check)))))

  (setq *paste-rectangle-popup-menu*
	(define-popup-menu ':paste-rectangle-popup
	  (:item :tag-command "挿入(&I)"
	   'yank-rectangle
	   #'(lambda () (unless *rectangle-kill-buffer* :disable)))
	  (:item :tag-command "上書き(&O)"
	   'overwrite-rectangle
	   #'(lambda () (unless *rectangle-kill-buffer* :disable)))
	  (:item :tag-command "行末へ追加(&A)"
	   'append-rectangle
	   #'(lambda () (unless *rectangle-kill-buffer* :disable)))
	  (:item :tag-command "文字列にして挿入(&R)"
	   'yank-rectangle-as-region
	   #'(lambda () (unless *rectangle-kill-buffer* :disable)))))

  (setq *app-popup-menu*
	(define-popup-menu
	  (:item :tag-command "切り取り(&T)"
	   'kill-selection-to-clipboard :modify-any-selection)
	  (:item :tag-command "コピー(&C)"
	   'copy-selection-to-clipboard :any-selection)
	  (:item :tag-command "貼り付け(&P)"
	   'paste-from-clipboard :clipboard)
	  (:item :tag-command "貼り付け - 矩形(&G)"
	   'paste-rectangle-from-clipboard :clipboard)
	  (:menu nil "クリップボードエンコーディング(&E)"
	   *clipboard-char-encoding-popup-menu*)
	  (:item :tag-command "削除(&D)"
	   'delete-selection :modify-any-selection)
	  (:item :tag-command "全てを選択(&A)"
	   'selection-whole-buffer)
	  (:sep :above-dictionary-popup-menu)
	  (:menu nil "辞書(&I)"
	   *dictionary-popup-menu*)
	  (:menu nil "矩形貼り付け(&R)"
	   *paste-rectangle-popup-menu*)))

  (setq *app-rectangle-popup-menu*
	(define-popup-menu
	  (:item :tag-command "切り取り(&T)"
	   'kill-rectangle-selection :modify-rectangle)
	  (:item :tag-command "コピー(&C)"
	   'copy-rectangle-selection :rectangle)
	  (:menu nil "貼り付け(&P)"
	   *paste-rectangle-popup-menu*)
	  (:item :tag-command "削除(&D)"
	   'delete-rectangle-selection :modify-rectangle)
	  (:item :tag-command "クリア(&R)"
	   'clear-rectangle-selection :modify-rectangle)
	  (:item :tag-command "空白を挿入(&O)"
	   'open-rectangle-selection :modify-rectangle)
	  (:item :tag-command "文字列で置き換え(&S)"
	   'string-rectangle-selection :modify-rectangle)))

  (or *command-bar-popup-menu*
      (setq *command-bar-popup-menu* (create-popup-menu ':command-bar-popup)))

  (or *recent-file-popup-menu*
      (setq *recent-file-popup-menu* (create-popup-menu ':recent-file-popup)))
  (or *recent-session-popup-menu*
      (setq *recent-session-popup-menu* (create-popup-menu ':recent-session-popup)))

  (setq *app-menu*
	(define-menu
	  (:popup 'file "ファイル(&F)"
	   (:item :tag-command "新規作成(&N)"
	    'new-file)
	   (:item :tag-command "開く(&O)..."
	    'open-file-dialog)
	   (:item :tag-command "閉じる(&C)"
	    'close-selected-buffer
	    #'(lambda ()
		(if (= (count-buffers) 1) :disable)))
	   (:item :tag-command "上書き保存(&S)"
	    'save-buffer-dialog :modified)
	   (:item :tag-command "名前を付けて保存(&A)..."
	    'save-as-dialog)
	   (:item :tag-command "ファイルの挿入(&I)..."
	    'insert-file-dialog)
	   (:item :tag-command "名前の変更(&R)..."
	    'rename-dialog)
	   (:item :tag-command "全て保存(&V)"
	    'save-all-buffers)
	   (:sep :above-session)
	   (:item :tag-command "セッションを開く(&D)..."
	    'open-session-dialog)
	   (:item :tag-command "セッションの保存(&W)..."
	    'save-session-dialog)
	   (:item :tag-command "セッションの自動保存(&T)"
	    'toggle-session-auto-save
	    'update-session-auto-save-ui)
	   (:item :tag-command "セッションを閉じる(&Y)"
	    'close-session-dialog)
	   (:sep :above-print)
	   (:item :tag-command "印刷(&P)..."
	    'print-selected-buffer-dialog)
	   (:sep :above-recent)
	   (:menu nil "最近使ったファイル(&F)"
	    *recent-file-popup-menu*)
	   (:menu nil "最近使ったセッション(&L)"
	    *recent-session-popup-menu*)
	   (:sep :above-kill-xyzzy)
	   (:item :tag-command "保存して終了(&E)"
	    'save-all-buffers-kill-xyzzy)
	   (:item :tag-command "終了(&X)"
	    'kill-xyzzy))
	  (:popup 'edit "編集(&E)"
	   (:item :tag-command "元に戻す(&U)"
	    'undo :undo)
	   (:item :tag-command "やっぱり元に戻さない(&R)"
	    'redo :redo)
	   (:sep :above-selection)
	   (:item :tag-command "切り取り(&T)"
	    'kill-selection-to-clipboard :modify-any-selection)
	   (:item :tag-command "コピー(&C)"
	    'copy-selection-to-clipboard :any-selection)
	   (:item :tag-command "貼り付け(&P)"
	    'paste-from-clipboard :clipboard)
	   (:item :tag-command "貼り付け - 矩形(&G)"
	    'paste-rectangle-from-clipboard :clipboard)
	   (:menu nil "クリップボードエンコーディング(&E)"
	    *clipboard-char-encoding-popup-menu*)
	   (:item :tag-command "削除(&L)"
	    'delete-selection :modify-any-selection)
	   (:item :tag-command "全てを選択(&S)"
	    'selection-whole-buffer)
	   (:sep :above-rectangle)
	   (:menu nil "矩形(&N)"
	    *app-rectangle-popup-menu*)
	   (:popup :convert-popup "変換(&V)"
	    (:item :tag-command "大文字(&U)"
	     'upcase-selection :selection)
	    (:item :tag-command "小文字(&L)"
	     'downcase-selection :selection)
	    (:item :tag-command "単語の先頭を大文字(&C)"
	     'capitalize-selection :selection)
	    (:sep :above-space-tab)
	    (:item :tag-command "スペース -> タブ(&T)"
	     'tabify-selection :selection)
	    (:item :tag-command "タブ -> スペース(&S)"
	     'untabify-selection :selection)
	    (:sep :above-full->half)
	    (:popup :full->half-popup "全角 -> 半角(&H)"
	     (:item :tag-command "すべて(&A)"
	      'map-all-to-half-width-selection :selection)
	     (:item :tag-command "英数・ひらがな・カタカナ(&S)"
	      'map-to-half-width-selection :selection)
	     (:item :tag-command "英数・ひらがな(&R)"
	      'map-ascii-and-hiragana-to-half-width-selection :selection)
	     (:item :tag-command "英数・カタカナ(&T)"
	      'map-ascii-and-katakana-to-half-width-selection :selection)
	     (:item :tag-command "英数(&N)"
	      'map-ascii-to-half-width-selection :selection)
	     (:item :tag-command "ひらがな(&H)"
	      'map-hiragana-to-half-width-selection :selection)
	     (:item :tag-command "カタカナ(&K)"
	      'map-katakana-to-half-width-selection :selection)
	     (:item :tag-command "ギリシア文字(&G)"
	      'map-greek-to-half-width-selection :selection)
	     (:item :tag-command "キリル文字(&C)"
	      'map-cyrillic-to-half-width-selection :selection))
	    (:popup :half->full-popup "半角 -> 全角(&Z)"
	     (:item :tag-command "すべて(ｶﾅ -> ひらがな)(&A)"
	      'map-all-to-full-width-hiragana-selection :selection)
	     (:item :tag-command "すべて(ｶﾅ -> カタカナ)(&L)"
	      'map-all-to-full-width-katakana-selection :selection)
	     (:item :tag-command "英数・ｶﾅ(ｶﾅ -> ひらがな)(&R)"
	      'map-to-full-width-hiragana-selection :selection)
	     (:item :tag-command "英数・ｶﾅ(ｶﾅ -> カタカナ)(&T)"
	      'map-to-full-width-katakana-selection :selection)
	     (:item :tag-command "英数(&N)"
	      'map-ascii-to-full-width-selection :selection)
	     (:item :tag-command "ｶﾅ -> ひらがな(&H)"
	      'map-kana-to-full-width-hiragana-selection :selection)
	     (:item :tag-command "ｶﾅ -> カタカナ(&K)"
	      'map-kana-to-full-width-katakana-selection :selection)
	     (:item :tag-command "ギリシア文字(&G)"
	      'map-greek-to-full-width-selection :selection)
	     (:item :tag-command "キリル文字(&C)"
	      'map-cyrillic-to-full-width-selection :selection))
	    (:popup :char-encoding-popup "漢字コード(&K)"
	     (:item :tag-command "自動判定(&A)"
	      'map-char-encoding-selection :selection)
	     (:item :tag-command "JISから(&J)"
	      'map-jis-selection :selection)
	     (:item :tag-command "EUCから(&E)"
	      'map-euc-selection :selection)))
	   (:popup :paragraph-popup "段落(&H)"
	    (:item :tag-command "前の段落(&B)"
	     'backward-paragraph)
	    (:item :tag-command "次の段落(&F)"
	     'forward-paragraph)
	    (:item :tag-command "選択(&S)"
	     'selection-paragraph)
	    (:item :tag-command "削除(&L)"
	     'kill-paragraph)
	    (:item :tag-command "入れ替え(&T)"
	     'transpose-paragraphs)
	    (:item :tag-command "詰め込み(&I)"
	     'fill-paragraph))
	   (:sep :above-not-modified)
	   (:item :tag-command "変更マークを消す(&M)"
	    'not-modified :modified)
	   (:item :tag-command "最初からやり直し(&O)"
	    'revert-buffer))
	  (:popup 'search "検索(&S)"
	   (:item :tag-command "検索(&F)..."
	    'search-dialog)
	   (:item :tag-command "次を検索(&N)"
	    'repeat-forward-search
	    #'(lambda () (or *last-search-p* :disable)))
	   (:item :tag-command "前を検索(&P)"
	    'repeat-backward-search
	    #'(lambda () (or *last-search-p* :disable)))
	   (:item :tag-command "置換(&R)..."
	    'replace-dialog)
	   (:item :tag-command "一致する文字列を数える(&C)..."
	    'count-matches-dialog)
	   (:sep :above-first-error)
	   (:item :tag-command "最初のタグ(&I)"
	    'first-error)
	   (:item :tag-command "次のタグ(&E)"
	    'next-error)
	   (:sep :above-goto-line)
	   (:item :tag-command "指定行(&J)..."
	    'goto-line-dialog)
	   (:item :tag-command "ファイルの先頭(&T)"
	    'beginning-of-buffer)
	   (:item :tag-command "ファイルの最後(&B)"
	    'end-of-buffer)
	   (:item :tag-command "マーク(&M)..."
	    'mark-dialog-box)
	   (:item :tag-command "関数の先頭(&D)"
	    'beginning-of-defun)
	   (:item :tag-command "関数の最後(&U)"
	    'end-of-defun)
	   (:sep :above-gresreg)
	   (:item :tag-command "Gresreg(&S)..."
	    'gresreg-dialog)
	   (:item :tag-command "Grep(&G)..."
	    'grep-dialog))
	  (:popup 'view "表示(&V)"
	   (:item :tag-command "折り返さない(&N)"
	    'set-buffer-fold-type-none
	    'set-buffer-fold-type-none-update)
	   (:item :tag-command "指定位置で折り返す(&C)"
	    'set-buffer-fold-type-column
	    'set-buffer-fold-type-column-update)
	   (:item :tag-command "ウィンドウ幅で折り返す(&W)"
	    'set-buffer-fold-type-window
	    'set-buffer-fold-type-window-update)
	   (:sep :above-command-bar)
	   (:menu nil "ツールバー(&Q)" *command-bar-popup-menu*)
	   (:sep :above-font-size)
	   (:popup nil "文字のサイズ(&X)"
	    (:item :tag-command "文字を大きく(&I)"
	     'increase-text-font-size)
	    (:item :tag-command "文字を小さく(&O)"
	     'decrease-text-font-size)
	    (:sep :above-set-text-font-x-large)
	    (:item :tag-command "最大(&G)"
	     #'(lambda () (interactive) (set-text-font-size :x-large))
	     #'(lambda () (if (eq (get-ascii-text-font-size) :x-large) :check)))
	    (:item :tag-command "大(&L)"
	     #'(lambda () (interactive) (set-text-font-size :large))
	     #'(lambda () (if (eq (get-ascii-text-font-size) :large) :check)))
	    (:item :tag-command "中(&M)"
	     #'(lambda () (interactive) (set-text-font-size :medium))
	     #'(lambda () (if (eq (get-ascii-text-font-size) :medium) :check)))
	    (:item :tag-command "小(&S)"
	     #'(lambda () (interactive) (set-text-font-size :small))
	     #'(lambda () (if (eq (get-ascii-text-font-size) :small) :check)))
	    (:item :tag-command "最小(&A)"
	     #'(lambda () (interactive) (set-text-font-size :x-small))
	     #'(lambda () (if (eq (get-ascii-text-font-size) :x-small) :check))))
	   (:sep :above-frame)
	   (:item :tag-command "新しいフレーム(&F)"
	    'new-pseudo-frame 'new-pseudo-frame-menu-update)
	   (:item :tag-command "現在のフレームを閉じる(&D)"
	    'delete-pseudo-frame 'pseudo-frame-menu-update)
	   (:item :tag-command "次のフレーム(&T)"
	    'other-pseudo-frame 'pseudo-frame-menu-update)
	   (:item :tag-command "前のフレーム(&R)"
	    'previous-pseudo-frame 'pseudo-frame-menu-update))
	  (:popup 'window "ウィンドウ(&W)"
	   (:item :tag-command "横に分割(&S)"
	    'split-window 'split-window-menu-update)
	   (:item :tag-command "縦に分割(&V)"
	    'split-window-vertically 'split-window-menu-update)
	   (:item :tag-command "他のウィンドウを閉じる(&O)"
	    'delete-other-windows 'close-window-menu-update)
	   (:item :tag-command "現在のウィンドウを閉じる(&C)"
	    'delete-window 'close-window-menu-update)
	   (:item :tag-command "次のウィンドウ(&N)"
	    'other-window 'move-window-menu-update)
	   (:item :tag-command "前のウィンドウ(&P)"
	    'move-previous-window 'move-window-menu-update)
	   (:sep :above-next-xyzzy)
	   (:item :tag-command "次のxyzzy(&X)"
	    'next-xyzzy-window
	    #'(lambda () (if (< (count-xyzzy-instance) 2) :disable)))
	   (:item :tag-command "前のxyzzy(&Y)"
	    'previous-xyzzy-window
	    #'(lambda () (if (< (count-xyzzy-instance) 2) :disable)))
	   (:item :tag-command "xyzzy選択(&Z)..."
	    'select-xyzzy
	    #'(lambda () (if (< (count-xyzzy-instance) 2) :disable)))
	   (:sep :above-select-buffer)
	   (:item :tag-command "バッファ選択(&B)..."
	    'select-buffer))

	  (:popup 'tools "ツール(&T)"
	   (:item :tag-command "コンソールプログラムを非同期で実行(&A)..."
	    'execute-subprocess-dialog)
	   (:item :tag-command "非同期プログラムを死なす(&K)"
	    'kill-subprocess
	    #'(lambda ()
		(let ((proc (buffer-process (selected-buffer))))
		  (unless (and proc
			       (eq (process-status proc) :run))
		    :disable))))
	   (:item :tag-command "コンソールプログラムの実行(&P)..."
	    'pipe-command-dialog)
	   (:item :tag-command "Windowsプログラムの実行(&W)..."
	    'launch-application-dialog)
	   (:item :tag-command (if (featurep :windows-nt)
				   "NTプロンプト(&D)"
				 "DOSプロンプト(&D)")
	    #'(lambda ()
		(interactive)
		(if (and (featurep :windows-vista)
			 (si:control-pressed)
			 (si:shift-pressed))
		    (run-admin-console)
		  (run-console))))
	   (:sep :above-kbd-macro)
	   (:item :tag-command "キーボードマクロ記録開始(&S)"
	    'start-kbd-macro)
	   (:item :tag-command "キーボードマクロ記録終了(&E)"
	    'end-kbd-macro
	    :end-macro)
	   (:item :tag-command "キーボードマクロ実行(&X)"
	    'call-last-kbd-macro
	    #'(lambda () (if (null *last-kbd-macro*) :disable)))
	   (:popup :kbd-macro-popup "キーボードマクロおまけ(&O)"
	    (:item :tag-command "保存(&V)..."
	     'save-last-kbd-macro
	     #'(lambda () (if (null *last-kbd-macro*) :disable)))
	    (:item :tag-command "読み込み(&L)..."
	     'load-kbd-macro)
	    (:item :tag-command "ファイルへ保存(&S)..."
	     'save-kbd-macro-to-file))
	   (:sep :above-property-sheet)
	   (:item :tag-command "共通設定(&C)..."
	    'option-property-sheet)
	   (:item :tag-command "ローカル設定(&L)..."
	    'local-option-property-sheet)
	   (:sep :above-box-drawings)
	   (:item :tag-command "罫線モード(&R)"
	    'box-drawings-mode
	    #'(lambda () (if *box-drawings-mode* :check)))
	   (:item :tag-command "太い罫線(&T)"
	    'box-drawings-toggle-line-type
	    #'(lambda ()
		(values (unless *box-drawings-mode*
			  :disable)
			(if (eq *box-drawings-line-type* '*box-drawings-thick-line*)
			    :check))))
	   (:sep :above-insert-date)
	   (:item :tag-command "日付と時刻の挿入(&M)..."
	    'insert-date-string)
	   (:item :tag-command "ショートカットの作成(&N)..."
	    'create-xyzzy-shortcut-dialog)
	   (:item :tag-command "ファイラ(&F)..."
	    'open-filer)
	   (:menu nil "辞書(&I)"
	    *dictionary-popup-menu*)
	   (:sep :above-make-tags)
	   (:item :tag-command "TAGSファイルの作成(&G)..."
	    'make-tags-file-dialog)
	   (:item :tag-command "タグジャンプ(&J)"
	    'jump-tag
	    #'(lambda () (unless (and tags-find-target tags-find-point) :disable)))
	   (:item :tag-command "ダイレクトタグジャンプ(&U)"
	    'direct-tag-jump
	    #'(lambda () (unless (and tags-find-target tags-find-point) :disable)))
	   (:item :tag-command "バックタグジャンプ(&B)"
	    'back-tag-jump
	    #'(lambda () (if (null *global-mark-list*) :disable)))
	   (:sep :|above-(>_<)|)
	   (:popup :|(>_<)-popup| "(>&_<)"
	    (:item :tag-command "ニフティのログを読む(&N)"
	     'view-nifty-log)
	    (:item :tag-command "電卓(&T)"
	     'calc)
	    (:item :tag-command "カレンダー(&A)"
	     'calendar)
	    (:item :tag-command "ハノイの塔(&H)"
	     'hanoi)
	    (:item :tag-command "五目(&G)"
	     'gomoku)
	    (:item :tag-command "ライフ(&L)"
	     'life)
	    (:item :tag-command "C曲線(&C)"
	     'c-curve)
	    (:item :tag-command "ドラゴン曲線(&R)"
	     'dragon-curve)))
	  (:popup 'help "ヘルプ(&?)"
	   (:item :tag-command "キー割り当て一覧(&K)"
	    'describe-bindings)
	   (:item :tag-command "最近入力したキー(&L)"
	    'view-lossage)
	   (:sep :above-about)
	   (:item :tag-command "について(&A)..."
	    'about-dialog))))

  (setq *buffer-bar-context-menu*
	(define-popup-menu
	  (:item 'close "閉じる(&C)"
	   #'(lambda ()
	       (interactive)
	       (kill-buffer *buffer-bar-context-menu-buffer*)))))

  (setq *pseudo-frame-bar-context-menu*
	(define-popup-menu
	  (:item 'close "閉じる(&C)"
	   #'(lambda ()
	       (interactive)
	       (delete-pseudo-frame *pseudo-frame-bar-context-menu-frame*)))))

  (run-hooks '*init-app-menus-hook*)
  (set-menu *app-menu*)
  (add-file-history-to-menu)
  (add-session-history-to-menu))

(defun make-char-encoding-popup-menu (command &optional init (list *char-encoding-list*))
  (labels ((define-menu (list)
	     (let ((menu (create-popup-menu)))
	       (mapc #'(lambda (encoding)
			 (if (consp encoding)
			     (add-popup-menu menu
					     (define-menu (cdr encoding))
					     (car encoding))
			   (add-menu-item menu nil
					  (char-encoding-display-name encoding)
					  #'(lambda ()
					      (interactive)
					      (funcall command encoding))
					  (and init #'(lambda ()
							(funcall init encoding))))))
		     list)
	       menu)))
    (define-menu
      (if (eq list *char-encoding-list*)
	*char-encoding-tree*
	(labels ((make-tree (tree list)
		   (mapcan #'(lambda (encoding)
			       (if (consp encoding)
				   (let ((l (make-tree (cdr encoding) list)))
				     (and l (list (cons (car encoding) l))))
				 (and (find encoding list :test #'eq)
				      (list encoding))))
			   tree)))
	  (make-tree
	   *char-encoding-tree*
	   (mapcar #'(lambda (encoding)
		       (if (symbolp encoding) (symbol-value encoding) encoding))
	     list)))))))
#|
(defun menu-display-name (path maxlen)
  (when (<= (length path) maxlen)
    (return-from menu-display-name path))
  (let* ((unc )
	 (paths (split-string path #\/))
	 (dev (cond ((eql (string/= path "//") 2)
		     (prog1
			 (concat "//" (car paths) "/" (cadr paths))
		       (setq paths (cddr paths))))
		    (t
		     (pop paths))))
	 (name (progn
		 (setq paths (nreverse paths))
		 (pop paths)))
	 (len (+ (length dev) (length name) 5)))
    (unless name
      (return-from menu-display-name path))
    (when (> len maxlen)
      (return-from menu-display-name name))
    (do* ((npaths nil)
	  (l (length (car paths)) (length (car paths))))
	((> (setq len (+ len l 1)) maxlen)
	 (format nil "~A/.../~{~A/~}~A" dev npaths name))
      (push (pop paths) npaths))))
|#

(defvar *menu-display-length* 40)

(defun add-lru-menu (menu files fn)
  (when (menup menu)
    (while (delete-menu menu 0 t))
    (do ((file files (cdr file))
	 (count 1))
	((or (null file)
	     (> count *menu-history-max*))
	 t)
      (unless (wild-pathname-p (car file))
	(let ((filename (namestring (car file))))
	  (insert-menu-item menu (1- count) nil
			    (format nil "&~:(~35r~): ~a"
				    (rem count 36)
				    (quote-string
				     (abbreviate-display-string
				      filename *menu-display-length* t)
				     #\& #\&))
			    (funcall fn filename)))
	(incf count)))))

(defun add-file-history-to-menu ()
  (when (menup *app-menu*)
    (add-lru-menu *recent-file-popup-menu*
		  *minibuffer-file-name-history*
		  #'(lambda (filename)
		      #'(lambda ()
			  (interactive)
			  (find-file filename)
			  (add-history filename '*minibuffer-file-name-history*))))))
