;;; -*- Mode: Lisp -*-
;;;
;;; This file is part of xyzzy.
;;;

;;;
;;; 電信八号用のメール読みモード
;;;
;;; 1 使う前の準備
;;;
;;;   (require "den8view") を初期化ファイルのどこかに書いておく。
;;; 電八側ではなにもする必要が無い。
;;;
;;; 1.1 必須項目
;;;
;;; *den8-directory*
;;;   フォルダがあるディレクトリを設定する。
;;;
;;; *den8-folder-list*
;;;   フォルダのリストを絶対パスまたは*den8-directory*からの相対パスで設
;;; 定する。`.fld'は書いちゃだめ。すべて絶対パスで指定するなら*den8-directory*
;;; は設定しなくても構わない。
;;;
;;; 例) c:/Den8の下に各フォルダがある場合
;;;   (setq *den8-directory* "c:/Den8")
;;;   (setq *den8-folder-list* '("IN" "Den8club"))
;;;
;;; 1.2 お好みで
;;;
;;; *den8-view-scroll-ratio*
;;;   スクロールする量をウィンドウの行数に対する比で指定する。デフォルト
;;; は2/3。整数でも分数でも小数でも構わない。負数でも絶対値が1より大きく
;;; ても、そうしたいのならしても構わない。
;;;
;;; *den8-view-smooth-scroll*
;;;   nilでなければ、スクロールするときにズリズリ動く。デフォルトはnil。
;;;
;;; *den8-maintenance-x-status*
;;;   nilでなければ、メールを表示したときにX-StatusをRECVからREADに書き
;;; 換える。デフォルトはt。
;;;
;;; *den8-default-thread-mode*
;;;   nilでなければ立ち上がったときにスレッド表示する。デフォルトはnil。
;;; `t'で切り替えられるようにしてみた。
;;;
;;; *den8-thread-by-subject*
;;;   nilでなければ、スレッドをつなげるときにIn-Reply-Toがなくても勘でつ
;;; なぐ。
;;;
;;; *den8-jdate-path*
;;;   Jdate.exeをフルパスで指定する。指定されていれば、返信メールを作る際に
;;;   Jdateを使う。
;;;
;;; 2 使い方
;;;
;;;   M-x den8-viewer RET で、設定が正しければメールが表示される。
;;;
;;; ・SPC, Backspace(C-h)
;;;   スクロールしたり、前後のメールを表示する。
;;; ・p, n
;;;   フォルダを前後に移動する。移動する順序は*den8-folder-list*で指定し
;;;  た通り。
;;; ・m
;;;   要返信マークをつける。
;;; ・C-p, C-n
;;;   前後のメールを表示する(スクロールはしない)。
;;; ・g
;;;   新しいメールを取ってくる。
;;; ・f
;;;   フォルダの選択。
;;; ・t
;;;   スレッド表示のモードを切り替え。
;;; ・q
;;;   フォルダの再読み込み。
;;; ・r
;;;   カレントのメールに返事を書く。送信は C-c C-c
;;;   手を抜いているので、r した時点で返信元のX-Statusを変えてしまう。
;;; ・s
;;;   次のステータス
;;; ・S
;;;   前のステータス
;;;
;;; 3 一覧のステータス
;;;
;;;   以下のようにしてみましたが、文句があればどーぞ
;;;
;;;   (空白) : 未読
;;;  *       : 読んだ
;;;  R       : 要返信
;;;  r       : 返信済み
;;;  w       : まだ送ってない
;;;  s       : 送った
;;;

;;;
;;; 履歴
;;;
;;; Tue Feb 24 1998
;;; ・排他をする設定になっているとき空のファイルを作って電八君に怒られる
;;;   場合があったのをFIX。
;;;
;;; Wed Feb 25 1998
;;; ・キャッシュの同期の仕方が分かった。存在するメール番号でUpdateをかけ
;;;   るとキャッシュがフラッシュされるっぽい(^^;)
;;;
;;; Fri Feb 27 1998
;;; ・den8-view-modeのinteractiveをやめた。
;;; ・一覧を付けてみた。
;;; ・folderを選択できるようにした。
;;; ・なんも考えずに作ったら、コードがぐちゃぐちゃになってしまった。

(provide "den8view")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "foreign"))

(defvar *den8-view-scroll-ratio* 2/3)
(defvar *den8-view-smooth-scroll* nil)
(defvar *den8-maintenance-x-status* t)
(defvar *den8-default-thread-mode* nil)
(defvar *den8-summary-window-ratio* 1/4)
(defvar *den8-thread-by-subject* t)

(defvar *den8-directory* nil)
(defvar *den8-folder-list* nil)

(defvar *den8-view-mode-hook* nil)

(defconstant *den8-dde-service* "Den8")
(defconstant *den8-dde-topic* "Command")

(defvar *den8-jdate-path* nil)

(defvar *den8-xyzzy-version* t)

(defvar *den8-quote-regexp* "^[ \t　]*[|>＞]\\|^[A-Za-z0-9]+>")
(defvar *den8-quote-forecolor* 12)
(defvar *den8-quote-backcolor* nil)

(defconstant *den8-x-status-recv* '("RECV" 1 #\SPC))
(defconstant *den8-x-status-read* '("READ" 2 #\*))
(defconstant *den8-x-status-reply* '("RPLY" 3 #\R))
(defconstant *den8-x-status-replied* '("RPLD" 4 #\r))
(defconstant *den8-x-status-write* '("WRIT" 5 #\w))
(defconstant *den8-x-status-sent* '("SENT" 6 #\s))

(defconstant *den8-next-x-status*
  (vector
   nil
   *den8-x-status-read*
   *den8-x-status-reply*
   *den8-x-status-replied*
   *den8-x-status-recv*
   *den8-x-status-sent*
   *den8-x-status-write*))

(defconstant *den8-previous-x-status*
  (vector nil
	  *den8-x-status-replied*
	  *den8-x-status-recv*
	  *den8-x-status-read*
	  *den8-x-status-reply*
	  *den8-x-status-sent*
	  *den8-x-status-write*))

(defconstant *den8-print-summary-format*
  "~[ ~; ~;*~;R~;r~;w~;s~:;E~] ~4,'0D \
~D/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d ~20@A ~@[~V,1@T┗~]~A~%")

(defstruct
  (den8msg
   (:constructor create-den8msg (mailno status time size from/to msgid
				 in-reply-to subject)))
  mailno status time size from/to msgid in-reply-to subject
  xsubject re-subject parent child)

(defvar *den8-view-mode-map* nil)
(defvar *den8-summary-mode-map* nil)

(unless *den8-view-mode-map*
  (setq *den8-view-mode-map* (make-sparse-keymap))
  (define-key *den8-view-mode-map* #\SPC 'den8-next-page-or-message)
  (define-key *den8-view-mode-map* #\TAB 'other-window)
  (define-key *den8-view-mode-map* #\C-h 'den8-previous-page-or-message)
  (define-key *den8-view-mode-map* #\C-n 'den8-next-message)
  (define-key *den8-view-mode-map* #\C-p 'den8-previous-message)
  ;(define-key *den8-view-mode-map* #\Down 'den8-scroll-up)
  ;(define-key *den8-view-mode-map* #\Up 'den8-scroll-down)
  (define-key *den8-view-mode-map* #\t 'den8-toggle-thread-mode)
  (define-key *den8-view-mode-map* #\m 'den8-mark-reply)
  (define-key *den8-view-mode-map* #\n 'den8-next-folder)
  (define-key *den8-view-mode-map* #\p 'den8-previous-folder)
  (define-key *den8-view-mode-map* #\q 'den8-refresh-folder)
  (define-key *den8-view-mode-map* #\f 'den8-select-folder)
  (define-key *den8-view-mode-map* #\g 'den8-receive)
  (define-key *den8-view-mode-map* #\s 'den8-next-x-status)
  (define-key *den8-view-mode-map* #\S 'den8-previous-x-status)
  (define-key *den8-view-mode-map* #\LBtnDown 'den8-mouse-left-press)
  (define-key *den8-view-mode-map* #\r 'den8-reply)
  (define-key *den8-view-mode-map* #\R 'den8-recover)
  (unless *den8-summary-mode-map*
    (setq *den8-summary-mode-map* (copy-keymap *den8-view-mode-map*))
    (define-key *den8-summary-mode-map* #\RET 'den8-summary-jump-to-mail)
    (define-key *den8-summary-mode-map* #\LBtnDown 'den8-mouse-left-press)
    (define-key *den8-summary-mode-map* #\LBtnMove 'mouse-nop)
    (define-key *den8-summary-mode-map* #\LBtnUp 'mouse-nop))
  (define-key *den8-view-mode-map* #\C-s 'den8-search-forward)
  (define-key *den8-view-mode-map* #\C-r 'den8-search-backward)
  (define-key *den8-view-mode-map* #\M-s 'den8-re-search-forward)
  (define-key *den8-view-mode-map* #\C-M-s 'den8-re-search-backward)
  (define-key *den8-view-mode-map* #\F5 'den8-search-forward-again)
  (define-key *den8-view-mode-map* #\S-F5 'den8-search-backward-again))

(defun den8-next-page (&optional (n 1))
  (interactive "p")
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (unless (cond ((plusp n)
		 (pos-visible-in-window-p (point-max)))
		((< (point) den8-point-min)
		 t)
		(t
		 (pos-visible-in-window-p den8-point-min)))
    (let ((nlines (max (truncate (* (window-height)
				    *den8-view-scroll-ratio*))
		       1)))
      (if *den8-view-smooth-scroll*
	  (dotimes (x nlines t)
	    (scroll-window (if (plusp n) 1 -1))
	    (refresh-screen))
	(scroll-window (if (plusp n) nlines (- nlines))))
      t)))

(defun den8-previous-page (&optional (n 1))
  (interactive "p")
  (den8-next-page (- n)))

(defun den8-message (n regexp)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (when den8-mail-number
    (let ((mail den8-mail-number))
      (save-excursion
	(set-buffer den8-summary-buffer)
	(den8-summary-goto-mail mail)
	(unless (zerop n)
	  (if (scan-buffer regexp :regexp t :reverse (minusp n) :no-dup t)
	      (setq mail (parse-integer (buffer-substring
					 (match-beginning 1)
					 (match-end 1))))
	    (plain-error "~:[前~;次~]のメールはありません" (plusp n)))))
      (setq den8-mail-number mail)
      (den8-read-mail (format nil "~A/~4,'0d.TXT"
			      den8-folder-name den8-mail-number)))
    t))

(defun den8-next-message (&optional (n 1))
  (interactive "p")
  (den8-message n "^. \\([0-9][0-9][0-9][0-9]\\) "))

(defun den8-previous-message (&optional (n 1))
  (interactive "p")
  (den8-next-message (- n)))

(defun den8-next-unread (&optional (n 1))
  (interactive "p")
  (den8-message n "^  \\([0-9][0-9][0-9][0-9]\\) "))

(defun den8-previous-unread (&optional (n 1))
  (interactive "p")
  (den8-next-unread (- n)))

(defun den8-next-folder (&optional (n 1))
  (interactive "p")
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (loop
    (cond ((minusp n)
	   (when (zerop den8-folder-index)
	     (plain-error "前のフォルダはありません"))
	   (decf den8-folder-index 1))
	  ((zerop n)
	   (unless *den8-folder-list*
	     (error "フォルダが定義されていません"))
	   (setq n 1))
	  (t
	   (when (>= den8-folder-index (- (length *den8-folder-list*) 1))
	     (plain-error "次のフォルダはありません"))
	   (incf den8-folder-index 1)))
    (when (den8-init-folder)
      (return)))
  (if (> den8-mail-number den8-max-mail-number)
      (den8-read-mail nil)
    (den8-next-message 0)))

(defun den8-previous-folder (&optional (n 1))
  (interactive "p")
  (den8-next-folder (- n)))

(defun den8-refresh-folder ()
  (interactive)
  (den8-next-folder 0))

(defun den8-next-page-or-message (&optional (n 1))
  (interactive "p")
  (or (den8-next-page n)
      (den8-next-message (if (plusp n) 1 -1))))

(defun den8-previous-page-or-message (&optional (n 1))
  (interactive "p")
  (den8-next-page-or-message (- n)))

(defun den8-scroll-up ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (scroll-window 1))

(defun den8-scroll-down ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (scroll-window -1))

(defun den8-mark-reply ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (den8-modify-x-status nil *den8-x-status-reply*))

(defun den8-next-x-status (&optional arg)
  (interactive "p")
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (let ((status (save-excursion
		  (let ((mail den8-mail-number))
		    (set-buffer den8-summary-buffer)
		    (let ((x (find mail den8-summary :key #'den8msg-mailno)))
		      (when x
			(den8msg-status x)))))))
    (and (integerp status)
	 (< 0 status (length *den8-next-x-status*))
	 (den8-modify-x-status nil (elt (if arg
					    *den8-previous-x-status*
					  *den8-next-x-status*)
					status)))))

(defun den8-previous-x-status (&optional arg)
  (interactive "p")
  (den8-next-x-status (not arg)))

(defun den8-receive ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (den8-dde-execute "[Receive]")
  (si:*activate-toplevel)
  (den8-refresh-folder))

(defun den8-toggle-thread-mode ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (let ((mail den8-mail-number)
	(folder den8-folder-short-name))
    (den8-popup-summary-buffer)
    (setq den8-thread-mode (not den8-thread-mode))
    (den8-format-summary)
    (den8-summary-show-mail folder mail))
  (pop-to-buffer den8-article-buffer))

(defvar *den8-dde-channel* nil)

(defun den8-dde-execute (data)
  (if *den8-dde-channel*
      (dde-execute *den8-dde-channel* data)
    (let ((chan (dde-initiate *den8-dde-service* *den8-dde-topic*)))
      (dde-execute chan data)
      (dde-terminate chan))))

(defun den8-update-mail (folder mail)
  (den8-dde-execute (format nil "[Update(~A.fld,~:[~4,'0D~;~A~])]"
			    folder (stringp mail) mail)))

(defun den8-modify-x-status (from to)
  (when den8-mail-number
    (save-excursion
      (goto-char (point-min))
      (when (scan-buffer
	     (if from
		 (concatenate 'string
			      "^X-Status[ \t]*:[ \t]*" (car from) "[ \t]*$")
	       "^X-Status[ \t]*:.*$")
	     :regexp t :limit den8-point-min)
	(let ((buffer-read-only nil))
	  (declare (special buffer-read-only))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert "X-Status: " (car to))
	  (incf den8-point-min (+ (- (match-beginning 0) (match-end 0))
				  10 (length (car to))))
	  (save-buffer)
	  (let ((mail den8-mail-number))
	    (save-excursion
	      (set-buffer den8-summary-buffer)
	      (when (den8-summary-goto-mail mail)
		(delete-char 1)
		(insert (caddr to))
		(let ((x (find mail den8-summary :key #'den8msg-mailno)))
		  (when x
		    (setf (den8msg-status x) (cadr to)))))))
	  (den8-update-mail den8-folder-name den8-mail-number))))))

(defun den8-parse-header ()
  (goto-char (point-min))
  (unless (scan-buffer "\n--------\n")
    (setq den8-point-min (point-min))
    (return-from den8-parse-header))
  (forward-line 2)
  (setq den8-point-min (point))
  (recenter 1))

(defun den8-read-mail (file)
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (delete-region (point-min) (point-max))
    (when file
      (insert-file-contents file t)))
  (set-buffer-file-name file)
  (setq mode-line-format
	(format nil "--%*- %b (%M) [%k:%l] %P Folder: ~A  Mail: ~:[~;~4,'0D~]"
		den8-folder-short-name file den8-mail-number))
  (den8-parse-header)
  (when (and *den8-quote-regexp*
	     (or *den8-quote-forecolor*
		 *den8-quote-backcolor*))
    (save-excursion
      (while (scan-buffer *den8-quote-regexp* :regexp t)
	(set-text-color (point) (progn (goto-eol) (point))
			*den8-quote-forecolor*
			*den8-quote-backcolor*)
	(or (forward-line 1)
	    (return)))))
  (when *den8-maintenance-x-status*
    (den8-modify-x-status *den8-x-status-recv* *den8-x-status-read*))
  (let ((mail den8-mail-number)
	(folder den8-folder-short-name))
    (den8-popup-summary-buffer)
    (den8-summary-show-mail folder mail))
  (pop-to-buffer den8-article-buffer))

(c:define-c-struct den8-folder-info
  (c:int min)
  (c:int max)
  (c:int unread))

(let ((dll (merge-pathnames "den8util.dll" (etc-path))))
  (c:define-dll-entry c:int den8-folder-info ((c:char *)(c:void *))
    dll "_den8_folder_info@8")
  (c:define-dll-entry c:int parse-den8-cache ((c:char *) (c:void *))
    dll "_parse_den8_cache@8")
  (c:define-dll-entry c:int find-den8-msgid ((c:char *) (c:char *))
    dll "_find_den8_msgid@8")
  (c:define-dll-entry c:int den8-flush-cache ()
    dll "_den8_flush_cache@0")
  (c:define-dll-entry c:int den8-cache-dirty-p ((c:char *))
    dll "_den8_cache_dirty_p@4"))

(defun den8-sync-cache (folder cache)
  (unless (zerop (den8-cache-dirty-p (si:make-string-chunk cache)))
    (let ((file (directory folder
			   :wild "[0-9][0-9][0-9][0-9].TXT"
			   :file-only t
			   :count 1)))
      (when file
	(message "Updating cache(~A)..." folder)
	(den8-update-mail folder (subseq (car file) 0 4))
	(message "Updating cache(~A)...done" folder)))))

(defun den8-init-folder ()
  (let* ((folder (merge-pathnames (nth den8-folder-index
				       *den8-folder-list*)
				  *den8-directory*))
	 (cache (concatenate 'string folder ".fld")))
    (unless (file-directory-p folder)
      (msgbox "~A~%フォルダではありません" folder)
      (return-from den8-init-folder nil))
    (message "Flushing cache...")
    (den8-flush-cache)
    (setq folder (map-slash-to-backslash folder))
    (den8-sync-cache folder cache)
    (unless (file-exist-p cache)
      (msgbox "~A~%ファイルが見つかりません" cache)
      (return-from den8-init-folder nil))
    (let ((xinfo (make-den8-folder-info))
	  (no-unread nil))
      (when (zerop (den8-folder-info (si:make-string-chunk cache) xinfo))
	(msgbox "~A~%電八君のキャッシュファイルではなさそうです" cache)
	(return-from den8-init-folder nil))
      (setq den8-min-mail-number (den8-folder-info-min xinfo))
      (setq den8-max-mail-number (den8-folder-info-max xinfo))
      (setq den8-mail-number (den8-folder-info-unread xinfo))
      (when (minusp den8-mail-number)
	(setq den8-mail-number (+ den8-max-mail-number 1))
	(setq no-unread t))
      (setq den8-folder-name folder)
      (setq den8-folder-short-name (car (last (split-string folder #\\))))
      (when (or (null den8-folder-short-name)
		(string-match "^.:$" den8-folder-short-name))
	(setq den8-folder-short-name ""))
      (save-excursion
	(set-buffer den8-summary-buffer)
	(setq den8-summary (den8-build-summary cache))
	(den8-format-summary))
      (when no-unread
	(message "未読メールはないような気がします"))
      t)))

(defun den8-view-mode ()
  (kill-all-local-variables)
  (setq buffer-mode 'den8-view-mode)
  (setq mode-name "Den8 View")
  (use-keymap *den8-view-mode-map*)
  (use-syntax-table *text-mode-syntax-table*)
  (setq buffer-read-only t)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (make-local-variable 'make-backup-files)
  (setq make-backup-files nil)
  (make-local-variable 'den8-point-min)
  (make-local-variable 'den8-folder-index)
  (setq den8-folder-index 0)
  (make-local-variable 'den8-folder-name)
  (setq den8-folder-name nil)
  (make-local-variable 'den8-folder-short-name)
  (setq den8-folder-short-name nil)
  (make-local-variable 'lock-file)
  (setq lock-file nil)
  (make-local-variable 'need-not-save)
  (setq need-not-save t)
  (make-local-variable 'kept-undo-information)
  (setq kept-undo-information nil)
  (make-local-variable 'mode-line-format)
  (make-local-variable 'den8-max-mail-number)
  (make-local-variable 'den8-min-mail-number)
  (make-local-variable 'den8-mail-number)
  (make-local-variable 'den8-folders)
  (make-local-variable 'den8-summary-buffer)
  (run-hooks '*den8-view-mode-hook*))

(defun den8-summary-mode (view)
  (kill-all-local-variables)
  (setq buffer-mode 'den8-summary-mode)
  (setq mode-name "Den8 Summary")
  (use-keymap *den8-summary-mode-map*)
  (setq buffer-read-only t)
  (set-local-window-flags (selected-buffer)
			  *window-flag-line-number* nil)
  (set-buffer-fold-width nil)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (make-local-variable 'need-not-save)
  (setq need-not-save t)
  (make-local-variable 'kept-undo-information)
  (setq kept-undo-information nil)
  (make-local-variable 'den8-article-buffer)
  (setq den8-article-buffer view)
  (make-local-variable 'den8-thread-mode)
  (setq den8-thread-mode *den8-default-thread-mode*)
  (make-local-variable 'den8-summary)
  (make-local-variable 'mode-line-format))

(defun den8-viewer ()
  (interactive)
  (let ((view (switch-to-buffer "*Den8 View*")))
    (den8-view-mode)
    (let ((sum (switch-to-buffer "*Den8 Summary*")))
      (den8-summary-mode view)
      (set-buffer view)
      (setq den8-summary-buffer sum)))
  (delete-other-windows)
  (den8-popup-summary-buffer)
  (pop-to-buffer den8-article-buffer)
  (den8-next-folder 0))

(let (callback
      (tmbase (encode-universal-time 0 0 0 1 1 1970 0)))
  (c:defun-c-callable c:void parse-den8-cache-callback ((c:int mailno)
							(c:int status)
							(c:u_int time)
							(c:u_int size)
							(c:int from/to)
							(c:int msgid)
							(c:int in-reply-to)
							(c:int subject))
    (funcall callback
	     mailno status (+ tmbase time) size
	     (si:unpack-string (si:make-chunk nil 500 nil from/to) 0)
	     (si:unpack-string (si:make-chunk nil 500 nil msgid) 0)
	     (si:unpack-string (si:make-chunk nil 500 nil in-reply-to) 0)
	     (si:unpack-string (si:make-chunk nil 500 nil subject) 0)))

  (defun list-den8-cache (cache fn)
    (setq callback fn)
    (not (zerop (parse-den8-cache (si:make-string-chunk cache)
				  #'parse-den8-cache-callback)))))

(defun den8-find-thread-by-subject (sum)
  (message "Finding thread by Subject...")
  (let ((hash (make-hash-table :size (truncate (* 3 (length sum)) 2) :test #'equal)))
    (mapc #'(lambda (x)
	      (setf (den8msg-xsubject x)
		    (if (and (string-matchp
			      "^\\(\\(\\[[^][]+]\\|\\(Re^?[0-9]*:\\)\\|\\(Re\\[[0-9]+]:\\)\\) *\\)+"
			      (den8msg-subject x))
			     (< (match-end 0) (length (den8msg-subject x))))
			(progn
			  (setf (den8msg-re-subject x)
				(or (match-beginning 3) (match-beginning 4)))
			  (subseq (den8msg-subject x) (match-end 0)))
		      (den8msg-subject x)))
	      (setf (gethash (den8msg-xsubject x) hash) x))
	  sum)
    (mapc #'(lambda (x)
	      (when (den8msg-re-subject x)
		(unless (or (den8msg-parent x)
			    (zerop (length (den8msg-xsubject x))))
		  (let ((parent (gethash (den8msg-xsubject x) hash)))
		    (when (and parent
			       (not (eq parent x)))
		      (setf (den8msg-parent x) parent)
		      (push x (den8msg-child parent)))))))
	  sum)))

(defun den8-build-summary (cache)
  (let ((sum nil))
    (message "Reading cache...")
    (list-den8-cache cache
		     #'(lambda (&rest x)
			 (push (apply #'create-den8msg x) sum)))
    (let ((hash (make-hash-table :size (truncate (* 3 (length sum)) 2) :test #'equal)))
      (message "Finding thread...")
      (mapc #'(lambda (x)
		(setf (gethash (den8msg-msgid x) hash) x))
	    sum)
      (mapc #'(lambda (x)
		(unless (zerop (length (den8msg-in-reply-to x)))
		  (let ((parent (gethash (den8msg-in-reply-to x) hash)))
		    (when parent
		      (setf (den8msg-parent x) parent)
		      (push x (den8msg-child parent))))))
	    sum)
      (when *den8-thread-by-subject*
	(den8-find-thread-by-subject sum))
      (mapc #'(lambda (x)
		(setf (den8msg-child x)
		      (sort (den8msg-child x) #'< :key #'den8msg-time)))
	    sum)
      (nreverse sum))))

(defun den8-print-thread (l depth)
  (mapc #'(lambda (x)
	    (unless (and (zerop depth)
			 (den8msg-parent x))
	      (multiple-value-bind (sec min hour day mon year)
		  (decode-universal-time (den8msg-time x))
		(format t *den8-print-summary-format*
			(den8msg-status x)
			(den8msg-mailno x)
			year mon day hour min sec
			(den8msg-from/to x)
			(unless (zerop depth) (+ 2 depth))
			(den8msg-subject x)))
	      (when (den8msg-child x)
		(den8-print-thread (den8msg-child x) (+ depth 1)))))
	l))

(defun den8-print-nothread (l)
  (mapc #'(lambda (x)
	    (multiple-value-bind (sec min hour day mon year)
		(decode-universal-time (den8msg-time x))
	      (format t *den8-print-summary-format*
		      (den8msg-status x)
		      (den8msg-mailno x)
		      year mon day hour min sec
		      (den8msg-from/to x)
		      nil
		      (den8msg-subject x))))
	l))

(defun den8-format-summary ()
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (message "Formatting summary...")
    (erase-buffer (selected-buffer))
    (with-output-to-selected-buffer
      (if den8-thread-mode
	  (den8-print-thread den8-summary 0)
	(den8-print-nothread den8-summary)))
    (message "Formatting summary...done")))

(defun den8-summary-goto-mail (mail)
  (let ((regexp (format nil "^. ~4,'0D " mail)))
    (cond ((or (progn
		 (goto-bol)
		 (looking-at regexp))
	       (progn
		 (goto-char (point-min))
		 (scan-buffer regexp :regexp t)))
	   t)
	  (t
	   (goto-char (point-max))
	   nil))))

(defvar *den8-summary-no-recenter* nil)

(defun den8-summary-show-mail (folder mail)
  (cond ((den8-summary-goto-mail mail)
	 (reverse-region (progn
			   (goto-eol)
			   (point))
			 (progn
			   (goto-bol)
			   (point)))
	 (unless *den8-summary-no-recenter*
	   (recenter)))
	(t
	 (setq mail nil)
	 (clear-reverse-region)))
  (setq mode-line-format
	(format nil "--%*- %b (%M)   Folder: ~A  Mail: ~@[~4,'0D~]  ~:[~;(Thread)~]"
		folder mail den8-thread-mode)))

(defun den8-popup-summary-buffer ()
  (pop-to-buffer den8-summary-buffer
		 (truncate (* (screen-height) *den8-summary-window-ratio*))))

(defun den8-summary-jump-to-mail ()
  (interactive)
  (let ((mail (save-excursion
		(goto-bol)
		(and (looking-at "^. \\([0-9][0-9][0-9][0-9]\\) ")
		     (parse-integer (buffer-substring (match-beginning 1)
						      (match-end 1)))))))
    (when mail
      (let ((obuffer (selected-buffer)))
	(pop-to-buffer den8-article-buffer)
	(setq den8-mail-number mail)
	(let ((*den8-summary-no-recenter* t))
	  (den8-next-message 0))
	(pop-to-buffer obuffer)))))

(defun den8-select-folder ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (message "Flushing cache...")
  (den8-flush-cache)
  (let ((folders (let ((xinfo (make-den8-folder-info))
		       (*den8-dde-channel* (dde-initiate *den8-dde-service*
							 *den8-dde-topic*)))
		   (prog1
		       (mapcan #'(lambda (x)
				   (let* ((folder (merge-pathnames x *den8-directory*))
					  (cache (concatenate 'string folder ".fld")))
				     (den8-sync-cache (map-slash-to-backslash folder) cache)
				     (cond ((zerop (den8-folder-info
						    (si:make-string-chunk cache)
						    xinfo))
					    nil)
					   ((minusp (den8-folder-info-unread xinfo))
					    (list (list " " x)))
					   (t
					    (list (list "*" x))))))
			       *den8-folder-list*)
		     (dde-terminate *den8-dde-channel*)))))
    (multiple-value-bind (result data)
	(dialog-box '(dialog 0 0 186 162
		      (:caption "フォルダの選択")
		      (:font 9 "MS UI Gothic")
		      (:control
		       (:listbox list nil #x50b10111 7 7 116 148)
		       (:button IDOK "OK" #x50010001 129 7 50 14)
		       (:button IDCANCEL "キャンセル" #x50010000 129 24 50 14)))
		    (list (cons 'list folders)
			  (cons 'list (nth den8-folder-index folders)))
		    '((list :column (1 30) :must-match t :enable (IDOK))))
      (when result
	(setq result (position (caddr (assoc 'list data))
			       *den8-folder-list* :test #'eq))
	(when result
	  (setq den8-folder-index result)
	  (den8-refresh-folder))))))

(defun den8-mouse-left-press ()
  (interactive)
  (let ((owindow (selected-window)))
    (set-window *last-mouse-window*)
    (cond ((eq buffer-mode 'den8-summary-mode)
	   (goto-last-mouse-point)
	   (if (> *last-mouse-click-count* 1)
	       (den8-summary-jump-to-mail))
	   t)
	  (t
	   (set-window owindow)
	   (mouse-left-press)))))

(defun den8-reply ()
  (interactive)
  (when (eq buffer-mode 'den8-summary-mode)
    (pop-to-buffer den8-article-buffer))
  (when den8-mail-number
    (let (temp)
      (unwind-protect
	  (progn
	    (setq temp (make-temp-file-name "_den"))
	    (den8-dde-execute (format nil "[Reply(~A.fld,~4,'0D,~A)]"
				      (map-slash-to-backslash den8-folder-name)
				      den8-mail-number
				      (map-slash-to-backslash temp)))
	    (when *den8-jdate-path*
	      (call-process (concat "\"" (merge-pathnames "jdwrap.exe" (etc-path))
				    "\" \"" *den8-jdate-path* "\" \"" temp "\"")
			    :wait t))
	    (den8-modify-x-status nil *den8-x-status-replied*)
	    (set-buffer (create-new-buffer "*Den8 Draft*"))
	    (insert-file-contents temp t)
	    (when *den8-xyzzy-version*
	      (goto-char (point-min))
	      (insert "X-Yzzy-Version: " (software-version) "\n")
	      (goto-char (point-min))
	      (set-buffer-modified-p nil))
	    (den8-draft-mode))
	(when temp
	  (delete-file temp))))))

(defvar *den8-draft-mode-hook* nil)
(defvar *den8-draft-mode-map* nil)
(unless *den8-draft-mode-map*
  (setq *den8-draft-mode-map* (make-sparse-keymap))
  (define-key *den8-draft-mode-map* '(#\C-c #\C-c) 'den8-draft-send))

(defvar *den8-draft-recover-file* nil)

(defun den8-kill-xyzzy-hook ()
  (when *den8-draft-recover-file*
    (delete-file *den8-draft-recover-file* :if-does-not-exist :skip)
    (setq *den8-draft-recover-file* nil)))

(defun den8-delete-buffer-hook (buffer)
  (save-excursion
    (set-buffer buffer)
    (cond (*den8-draft-sent*
	   t)
	  (t
	   (refresh-screen)
	   (case (message-box "メールを送信しますか?" nil
			      '(:yes-no-cancel :question :button1))
	     (:yes (den8-draft-send t))
	     (:no t)
	     (t nil))))))

(add-hook '*kill-xyzzy-hook* 'den8-kill-xyzzy-hook)

(defun den8-recover ()
  (interactive)
  (unless (and *den8-draft-recover-file*
	       (file-exist-p *den8-draft-recover-file*))
    (error "No recover file"))
  (set-buffer (create-new-buffer "*Den8 Draft*"))
  (insert-file-contents *den8-draft-recover-file* t)
  (den8-draft-mode)
  (delete-file *den8-draft-recover-file*)
  (setq *den8-draft-recover-file* nil))

(defun den8-draft-send (&optional no-delete)
  (interactive "p")
  (let ((opoint (point)))
    (goto-char (point-min))
    (when (re-search-forward "[\xa0-\xdf\X8540-\X889e\Xeb40-\Xffff]+" t)
      (ed::show-match)
      (error "腐れ文字があるので送信でけん"))
    (goto-char opoint))
  (unless *den8-draft-recover-file*
    (setq *den8-draft-recover-file* (make-temp-file-name)))
  (let ((file (make-temp-file-name)))
    (write-file file t)
    (copy-file file *den8-draft-recover-file* :if-exists :overwrite)
    (den8-dde-execute (concat "[Send(" (map-slash-to-backslash file) ")]")))
  (si:*activate-toplevel)
  (set-buffer-modified-p nil)
  (setq *den8-draft-sent* t)
  (or no-delete
      (kill-buffer (selected-buffer)))
  t)

(defun den8-draft-mode ()
  (kill-all-local-variables)
  (setq buffer-mode 'den8-draft-mode)
  (setq mode-name "Den8 Draft")
  (use-keymap *den8-draft-mode-map*)
  (use-syntax-table *text-mode-syntax-table*)
  (make-local-variable '*den8-draft-sent*)
  (setq *den8-draft-sent* nil)
  (make-local-variable '*delete-buffer-hook*)
  (add-hook '*delete-buffer-hook* 'den8-delete-buffer-hook)
  (run-hooks '*den8-draft-mode-hook*))

(defun den8-scan-buffer (pattern reverse regexp interactive-p)
  (long-operation
    (let ((mail den8-mail-number)
	  (*wrap-search* nil))
      (when (ed::search-command pattern reverse regexp interactive-p t)
	(return-from den8-scan-buffer t))
      (setq mail (save-excursion
		   (let ((buffer nil)
			 (folder den8-folder-name)
			 (sum-buffer den8-summary-buffer))
		     (unwind-protect
			 (progn
			   (setq buffer (create-new-buffer "*Den8 Scan*"))
			   (loop
			     (setq mail (progn
					  (set-buffer sum-buffer)
					  (save-excursion
					    (and (if (den8-summary-goto-mail mail)
						     (forward-line (if reverse -1 1))
						   (forward-line -1))
						 (ignore-errors
						  (parse-integer (buffer-substring
								  (+ (point) 2)
								  (+ (point) 6))))))))
			     (unless mail
			       (plain-error "文字列が見つかりません"))
			     (set-buffer buffer)
			     (erase-buffer buffer)
			     (message "Searching ~4,'0d.TXT..." mail)
			     (insert-file-contents
			      (format nil "~A/~4,'0d.TXT" folder mail))
			     (goto-char (if reverse (point-max) (point-min)))
			     (when (ed::search-command pattern reverse regexp
						       interactive-p t t)
			       (return mail))))
		       (when buffer
			 (delete-buffer buffer))))))
      (when mail
	(setq den8-mail-number mail)
	(den8-read-mail (format nil "~A/~4,'0d.TXT"
				den8-folder-name den8-mail-number))
	(goto-char (if reverse (point-max) (point-min)))
	(ed::search-command pattern reverse regexp interactive-p t)))))

(defun den8-search-forward (pattern)
  (interactive "sSearch forward: "
    :default0 ed::*last-search-string* :history0 'search)
  (den8-scan-buffer pattern nil nil (interactive-p)))

(defun den8-search-backward (pattern)
  (interactive "sSearch backward: "
    :default0 ed::*last-search-string* :history0 'search)
  (den8-scan-buffer pattern t nil (interactive-p)))

(defun den8-search-forward-again ()
  (interactive)
  (let ((s (if ed::*last-search-regexp-p*
	       ed::*last-search-regexp*
	     ed::*last-search-string*)))
    (when (equal s "")
      (plain-error "No last search"))
    (den8-scan-buffer s nil ed::*last-search-regexp-p* (interactive-p))))

(defun den8-search-backward-again ()
  (interactive)
  (let ((s (if ed::*last-search-regexp-p*
	       ed::*last-search-regexp*
	     ed::*last-search-string*)))
    (when (equal s "")
      (plain-error "No last search"))
    (den8-scan-buffer s t ed::*last-search-regexp-p* (interactive-p))))

(defun den8-re-search-forward (regexp)
  (interactive "sRe-search forward: "
    :default0 ed::*last-search-regexp* :history0 'search)
  (den8-scan-buffer regexp nil t (interactive-p)))

(defun den8-re-search-backward (regexp)
  (interactive "sRe-search backward: "
    :default0 ed::*last-search-regexp* :history0 'search)
  (den8-scan-buffer regexp t t (interactive-p)))

