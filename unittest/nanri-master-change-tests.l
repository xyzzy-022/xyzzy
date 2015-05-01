;;; -*- mode: lisp -*-

;; 2011-12-21  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* lisp : fix saving session file.
;; 	ヒストリファイルと同様にセッションファイルも書き込み時に *print-length*
;; 	などの影響を受けないよう修正。
;; 
;; 	(progn
;; 	  (ed::write-session-file "0ssn.l")
;; 	  (let ((*print-circle* t)
;; 	        (*print-pretty* nil)
;; 	        (*print-length* 1)
;; 	        (*print-level* 1))
;; 	    (ed::write-session-file "1ssn.l")))
;;
(deftest fix-saving-session-file ()
  "セッションファイルを保存時に *print-length* などの影響を受けないように修正。"
  (with-temp-files (ssn)
    (block test
      (with-fake-functions ((write (object &key pretty readably escape)
                              (unless (or (stringp object)
                                          (and pretty readably escape))
                                (return-from test nil))))
        (let ((*print-circle* t)
              (*print-pretty* nil)
              (*print-length* 1)
              (*print-level* 1))
          (ed::write-session-file ssn))
        t)))
  => non-nil)

;; 	* lisp : fix saving history-file.
;; 	ヒストリファイル書き込み時に *print-length* などの影響を受けるのを修正。
;; 
;; 	(progn
;; 	  (let ((*history-file-name* "0hist.l"))
;; 	    (ed::save-history-file))
;; 	  (let ((*history-file-name* "1hist.l")
;; 	        (*print-circle* t)
;; 	        (*print-pretty* nil)
;; 	        (*print-length* 1)
;; 	        (*print-level* 1))
;; 	    (ed::save-history-file)))
;;
(deftest fix-saving-history-file ()
  "ヒストリファイル書き込み時に *print-length* などの影響を受けるのを修正。"
  ;; NOTE: xyzzy.exe -q で起動してるとヒストリファイルを使わない感じになっちゃうので
  (unless ed::*history-file-loaded*
    (trap-errors (ed::load-history-file)))
  (with-temp-files (histfile)
    (block test
      (with-fake-functions ((write (object &key pretty readably escape)
                              (unless (and pretty readably escape)
                                (return-from test nil))))
        (let ((*history-file-name* histfile)
              (*print-circle* t)
              (*print-pretty* nil)
              (*print-readably* nil)
              (*print-length* 1)
              (*print-level* 1))
          (ed::save-history-file))
        t)))
  => non-nil)

;; 2011-12-20  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix equalp for hash-table.
;; 	equalp に hash-table を渡すと多値が返ってくる問題を修正。
;; 	https://gist.github.com/1480046
;; 
(deftest equalp-for-hash-table ()
  "equalp に hash-table を渡すと多値がが選ってくる問題を修正。"
  (let ((a (make-hash-table))
        (b (make-hash-table)))
    (setf (gethash 1 a) "foo"
          (gethash 1 b) "foo")
    (equalp a b))
  => t)

;; 2011-06-17  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix sxhash.
;; 	sxhash の値が equalp 用の値になっているが equal 用の値になるように修正。
;; 	(= (sxhash (list "ABC")) (sxhash (list "ABC"))) => t
;; 	(= (sxhash (list "abc")) (sxhash (list "ABC"))) => nil
;;
;; NOTE: これは元からオッケー
; (deftest sxhash-fix-00 ()
;   (= (sxhash (list "ABC")) (sxhash (list "ABC")))
;   => t)
 
(deftest sxhash-fix-01 ()
  "sxhash の値が equalp 用の値になっていたが equal 用の値に修正。"
  (= (sxhash (list "abc")) (sxhash (list "ABC")))
  => nil)

;; 2011-06-15  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* lisp : fix compiling lambda form.
;; 	lambda form をコンパイルするとレキシカル変数を参照できなくなっていた問
;; 	題を修正。
;; 	https://gist.github.com/998414
;; 	https://gist.github.com/1000096
;; 
(deftest fix-compiling-lambda-form ()
  "lambda form をコンパイルするとレキシカル変数を参照できなくなっていた問題を修正。"
  (compile-and-eval
    (let ((a 1))
      ((lambda () a))))
  => 1)

;; 2011-06-14  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* lisp : fix typo in lisp/compile.l.
;; 	関数名のミスを修正。
;; 	https://twitter.com/#!/bowbow99/status/80332901014704128
;;
(deftest fix-typo-in-lisp/compile.l ()
  "lisp/compile.l の関数名 typo を修正（s/nineth/ninth/）。"
  (progn
    (require "compile")
    (functionp (get 'ninth (find-symbol "optimize-form" :compiler))))
  => non-nil)

;; 2011-01-30  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix abbreviate-display-string.
;; 	abbreviate-display-string で必要以上に文字列が省略されることがあるのを
;; 	修正。以下のようになる場合があった。
;; 	(abbreviate-display-string "D:/src/cmigemo-1.3c-MIT/src/wordlist.h" 40 t)
;; 	=> "D:/..."
;; 
;; 	string.cc の 1109 行目、以下のようになっていた。
;; 	  strcpy (le, "...");
;; 	  strcpy (le + 3, rb);
;; 	はじめの strcpy で '\0' が書き込まれ、その位置がちょうど rb と一致した
;; 	ときに発生した。
;;
(deftest fix-abbreviate-display-string ()
  "abbreviate-display-string で必要以上に文字列が省略されることがあるのを修正。"
  (abbreviate-display-string "D:/src/cmigemo-1.3c-MIT/src/wordlist.h" 40 t)
  => "D:/.../cmigemo-1.3c-MIT/src/wordlist.h")


;; 2011-01-21  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* lisp : fix pipe-command.
;; 	「plink name@example.jp ls」と plink を利用しようとすると
;; 	「Unable to read from standard input: ハンドルが無効です。」
;; 	というエラーになるのを修正。
;; 
;; 	execute-shell-command の第二引数に文字列・nil以外の引数を渡すとファイル
;; 	名が空のファイルを開こうとするので、エラーになっていた模様。
;; 
;; NOTE: 変更の内容をよく理解してないが、変更されてるかだけチェック。
(deftest fix-pipe-command ()
  "pipe-command が execute-shell-command に渡す引数が間違ってたのを修正（たぶん）"
  (with-fake-functions ((execute-shell-command (a b c) b))
    (pipe-command "duh.."))
  => nil)

;; 	* src : fix wheel icon.
;; 	なんとなくホイールボタンをクリックしたときのアイコンをいじってみた。
;; 
;; NOTE: テスト不要かと。無理だし。

;; 	* src : fix multiple binding of special variables in let, let*.
;; 	http://d.hatena.ne.jp/bowbow99/20101127/1290818576
;; 	let や let* で同一のスペシャル変数に対して複数回の束縛を作ったときの値
;; 	がおかしいのを修正。
;; 	コンパイルしたときの動作は修正できていない。
;; 	(progn
;; 	  (defparameter *special* :global)
;; 	  (let ((*special* :local-1)
;; 	        (*special* :local-2))
;; 	    *special*))
;; 	=> :local-2
;; 
;; 	レキシカルな環境には逆順に登録されるので、スペシャル変数に値を設定する
;; 	前に順序を元に戻している。
;; 
(deftest fix-multiple-binding-of-special-variable-in-let/let*-00 ()
  "let で同一のスペシャル変数に対して複数回の束縛を作ったときの値
がおかしいのを修正。"
  (progn
    (defparameter *special* :global)
    (let ((*special* :local-1)
          (*special* :local-2))
      *special*))
  => :local-2)

(deftest fix-multiple-binding-of-special-variable-in-let/let*-01 ()
  "let* で同一のスペシャル変数に対して複数回の束縛を作ったときの値
がおかしいのを修正。"
  (progn
    (defparameter *special* :global)
    (let* ((*special* :local-1)
           (*special* :local-2))
      *special*))
  => :local-2)

;; 2011-01-05  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix let, let*.
;; 	http://d.hatena.ne.jp/bowbow99/20101127/1290818576
;; 	コンパイルしたときは値が変わることはないが、インタプリタと同様に逆順で
;; 	値をリストアするように修正。
;;
;; NOTE: これは元から壊れてない（挙動は変わってない）
; (deftest fix-let/let*-multiple-bind-for-special-variable-in-compiled-code ()
;   (progn
;     (defparameter *special* :global)
;     (funcall
;      (compile nil
;               ;; NOTE: need empty lexenv to compile
;               (eval '(lambda ()
;                       (let ((*special* :local-1)
;                             (*special* :local-2))
;                         *special*)))))
;     *special*)
;   => :global)

;; 2011-01-04  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix special variables restore at the end of let, let*.
;; 	http://d.hatena.ne.jp/bowbow99/20101127/1290818576
;; 	let や let* で同一のスペシャル変数に対して複数回の束縛を作ると、スコー
;; 	プが外れたときに値が書き換わってしまうのを修正。
;; 	(progn
;; 	  (defparameter *special* :global)
;; 	  (let ((*special* :local-1)
;; 	        (*special* :local-2))
;; 	    *special*)
;; 	  *special*)
;; 	=> :global
;; 
;; 	値を保存するとき(src/eval.cc 347行目辺り)と逆順に値をリストアするように
;; 	した。
;; 
(deftest fix-special-variables-restore-at-the-end-of-let/let*-00 ()
  "let で同一のスペシャル変数に対して複数回の束縛を作ると、スコー
プが外れたときに値が書き換わってしまうのを修正。"
  (progn
    (defparameter *special* :global)
    (let ((*special* :local-1)
          (*special* :local-2))
      *special*)
    *special*)
  => :global)

(deftest fix-special-variables-restore-at-the-end-of-let/let*-01 ()
  "let* で同一のスペシャル変数に対して複数回の束縛を作ると、スコー
プが外れたときに値が書き換わってしまうのを修正。"
  (progn
    (defparameter *special* :global)
    (let* ((*special* :local-1)
           (*special* :local-2))
      *special*)
    *special*)
  => :global)

;; 2010-12-14  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix eol-code of zero size file.
;; 	ファイルサイズが 0 のときの (正確には改行がないファイルのときの) 改行コー
;; 	ドが常に CRLF になるのを修正し *default-eol-code* に基づいた改行コード
;; 	になるようにする。
;; 	xyzzy Part16 577
;;
(deftest fix-eol-code-of-zero-size-file ()
  "ファイルサイズが 0 のときの (正確には改行がないファイルのときの) 改行コー
ドが常に CRLF になるのを修正し *default-eol-code* に基づいた改行コード
になるようにする。"
  (with-temp-files (name)
    (let (buffer)
      (with-open-file (out name :direction :output :if-does-not-exist :create))
      (unwind-protect
          (let ((*default-eol-code* *eol-lf*))
            (declare (special *default-eol-code*))
            (setq buffer (ed::find-file-internal name))
            (buffer-eol-code buffer))
        (delete-buffer buffer))))
  => 0)

;; 2010-12-13  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix DLL loading (hhctrl.ocx).
;; 	JVNVU#707943: Windows プログラムの DLL 読み込みに脆弱性
;; 	http://jvn.jp/cert/JVNVU707943/index.html
;; 	の修正の続き。
;; 	hhctrl.ocx のロードは静的変数の初期化時に行われるため、先の修正の効果が
;; 	及ばない。よって、ロードのタイミングを関数 html-help 呼び出し時に変更。
;; 
;; 	* src : fix DLL loading.
;; 	JVNVU#707943: Windows プログラムの DLL 読み込みに脆弱性
;; 	http://jvn.jp/cert/JVNVU707943/index.html
;; 	の修正。
;; 	統合アーカイバの DLL と WSOCK32.DLL と ctl3d32.dll が対象。
;; 	foreign:define-dll-entry (si:load-dll-module) で指定する DLL も対象だが、
;; 	こちらはフルパスで指定するのが望ましいだろう。
;; 
;; TODO: テキトーな dll を読み込んでテストする？？

;; 2010-12-12  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : update zlib(version 1.2.5).
;; 	zlibをバージョン1.2.5にアップデート。
;; 
;; 	* lisp : support lzma and xz in filer.
;; 	lzma および xz の圧縮と展開をファイラから利用できるように修正。
;; 
(deftest lzma-support-in-filer-00 ()
  "lzma の圧縮と展開をファイラから利用できるように修正。"
  (member "*.lzma" (cdr (assoc "アーカイブ" ed:*filer-path-masks* :test #'string=))
          :test #'string=)
  => non-nil)

(deftest lzma-support-in-filer-01 ()
  (with-fake-functions ((dialog-box (&rest args)
                          (values t '((ed::name . "foo.tar.lzma")
                                      (ed::dir . "C:/dir")
                                      (ed::base . "dir"))))
                        (ed::add-history (&rest args) t)
                        (ed::filer-get-directory () "C:/"))
    (ed::filer-get-archive-name))
  => "C:/dir/foo.tar.lzma"
  => "C:/dir")

(deftest xz-support-in-filer-00 ()
  "xz の圧縮と展開をファイラから利用できるように修正。"
  (member "*.xz" (cdr (assoc "アーカイブ" ed:*filer-path-masks* :test #'string=))
          :test #'string=)
  => non-nil)

(deftest xz-support-in-filer-01 ()
  (with-fake-functions ((dialog-box (&rest args)
                          (values t '((ed::name . "foo.tar.xz")
                                      (ed::dir . "C:/dir")
                                      (ed::base . "dir"))))
                        (ed::add-history (&rest args) t)
                        (ed::filer-get-directory () "C:/"))
    (ed::filer-get-archive-name))
  => "C:/dir/foo.tar.xz"
  => "C:/dir")

;; 	* src : support lzma and xz.
;; 	tar32.dll Ver2.35 から利用できるようになった lzma および xz の圧縮と展
;; 	開に対応。
;;

;; 2010-12-11  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix hash-table.
;; 	rehash 時に不要なハッシュオブジェクトのエントリーを NULL に設定している
;; 	ので、サイズも 0 に設定する。hash-table のマークフェーズ(data.ccの613行
;; 	目あたり)で NULL を参照するのを防止する。
;; 	以下のような hash-table が rehash した後の GC のタイミングで落ちる現象
;; 	がおそらく解消すると思われる。
;; 	http://twitter.com/miyamuko/status/14229437114
;; 	http://d.hatena.ne.jp/knenet/20091223/1261578863
;; 
;; FIXME: 「空でない環境で定義された関数はコンパイルできません」
; (deftest fix-hash-table-and-gc-crash ()
;   (with-another-xyzzy (:options "-q")
;     (defun xn ()
;       (let ((tb (make-hash-table)))
;         (dotimes (a 10000 :done)
;           (let* ((p (f))
;                  (n (gethash p tb)))
;             (setf (gethash p tb) (if n (1+ n) 1))))))
;     (defun f ()
;       (let ((p 0))
;         (dotimes (- 20 p)
;           (if (< 0.4 (random 1.0)) (incf p)))))
;     (progn
;       (compile 'xn)
;       (compile 'f)
;       (xn)))
;   => :done)

;; 2010-12-10  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix (cdr '#1='#1#) printing.
;; 	http://blog.bugyo.tk/lyrical/archives/610
;; 	http://blog.practical-scheme.net/gauche/20100609-curious-circular-list
;; 	「(cdr '#1='#1#)」の評価結果の印字でスタックオーバーフローするのを修正。
;; 	'#1='#1#              => #1='#1#
;; 	(cdr '#1='#1#)        => #1=((quote . #1#))
;; 	(cdr '#1=(quote #1#)) => #1=((quote . #1#))
;; 	(cdr '#1=(q #1#))     => #1=((q . #1#))
;;
(deftest |fix (cdr '#1='#1#) printing| ()
  "「(cdr '#1='#1#)」の評価結果の印字でスタックオーバーフローするのを修正。"
  (eval-in-another-xyzzy
   '(with-output-to-string (*standard-output*)
     (write (cdr '#1='#1#) :circle t))
   :options "-q")
  => "#1=((quote . #1#))")

;; 2010-12-01  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix macroexpand.
;; 	http://twitter.com/bowbow99/status/9887141152694272
;; 	二番目の戻り値が返っていなかったのを修正。
;; 	(macroexpand '(push 1 x))
;; 	=> (setq x (cons 1 x))
;; 	=> t
;; 	(macroexpand '(setq x (cons 1 x)))
;; 	=> (setq x (cons 1 x))
;; 	=> nil
;; 
(deftest fix-macroexpand-00 ()
  "macroexpand で2番目の戻り値が返っていなかったのを修正: 展開した場合 t"
  (macroexpand '(push 1 x))
  => (setq x (cons 1 x))
  => t)

(deftest fix-macroexpand-01 ()
  "macroexpand で2番目の戻り値が返っていなかったのを修正: 展開しない場合 nil"
  (macroexpand '(setq x (cons 1 x)))
  => (setq x (cons 1 x))
  => nil)

;; 	* src : fix flet, labels, macrolet.
;; 	http://twitter.com/bowbow99/status/8011887295856640
;; 	引数のチェックがおかしかったのを修正。
;; 	(flet () 3)     => 3
;; 	(flet ())       => nil
;; 	(labels () 3)   => 3
;; 	(labels ())     => nil
;; 	(macrolet () 3) => 3
;; 	(macrolet ())   => nil
;; 
(deftest fix-flet/labels/macrolet-00 ()
  "flet, labels, macrolet の BODY を空にするとエラーになってたのを修正: flet"
  (flet ()) => nil)

(deftest fix-flet/labels/macrolet-01 ()
  "flet, labels, macrolet の BODY を空にするとエラーになってたのを修正: labels"
  (labels ()) => nil)

(deftest fix-flet/labels/macrolet-02 ()
  "flet, labels, macrolet の BODY を空にするとエラーになってたのを修正: macrolet"
  (macrolet ()) => nil)

;; 	* src : add "listp" Macro.
;; 	オブジェクトが cons である、又は nil であるかどうか調べる listp マクロ
;; 	を追加。
;; 

;; 2010-11-09  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix type-check in list-length.
;; 	http://twitter.com/bowbow99/status/1742537278623744
;; 	(list-length :foo)
;; 	-> 不正なデータ型です: :foo: list
;; 	(list-length '(a . b))
;; 	-> 不正なデータ型です: b: list
;; 
;; 	引数にdotted listを渡したときのエラーメッセージがいまいちだが、
;; 	とりあえずそのまま。
;; 
(deftest fix-type-check-in-list-length-00 ()
  "list-length の型チェック。"
  (list-length :foo)
  !! type-error)

(deftest fix-type-check-in-list-length-01 ()
  "list-length の型チェック。ドットリストもダメ。"
  (list-length '(a . b))
  !! type-error)

;; 	* src : fix nthcdr given dotted list.
;; 	http://twitter.com/bowbow99/status/1746294087352320
;; 	(nthcdr 0 '()) => nil
;; 	(nthcdr 3 '()) => nil
;; 	(nthcdr 0 '(a b c)) => (a b c)
;; 	(nthcdr 2 '(a b c)) => (c)
;; 	(nthcdr 4 '(a b c)) => nil
;; 	(nthcdr 1 '(0 . 1)) => 1
;; 	(nthcdr 3 '(0 . 1)) => nil
;; 
;; 	最後のケースはエラーにすべきだけど、とりあえずそのまま。
;;

;; NOTE: 05 以外は元からちゃんと動く
; (deftest fix-nthcdr-given-dotted-list-00 ()
;   (nthcdr 0 '()) => nil)

; (deftest fix-nthcdr-given-dotted-list-01 ()
;   (nthcdr 3 '()) => nil)

; (deftest fix-nthcdr-given-dotted-list-02 ()
;   (nthcdr 0 '(a b c)) => (a b c))

; (deftest fix-nthcdr-given-dotted-list-03 ()
;   (nthcdr 2 '(a b c)) => (c))

; (deftest fix-nthcdr-given-dotted-list-04 ()
;   (nthcdr 4 '(a b c)) => nil)

(deftest fix-nthcdr-given-dotted-list-05 ()
  "nthcdr をドットリストに対応させる。"
  (nthcdr 1 '(0 . 1)) => 1)

; (deftest fix-nthcdr-given-dotted-list-06 ()
;   (nthcdr 3 '(0 . 1)) => nil)


;; 	* src : fix single-float-epsilon, single-float-negative-epsilon.
;; 	http://twitter.com/TwilightClover/status/26251344464
;; 	http://gist.github.com/608561
;; 	http://gist.github.com/608467
;; 	(dolist (sym '((double-float-epsilon t)
;; 	               (double-float-negative-epsilon nil)
;; 	               (long-float-epsilon t)
;; 	               (long-float-negative-epsilon nil)
;; 	               (short-float-epsilon t)
;; 	               (short-float-negative-epsilon nil)
;; 	               (single-float-epsilon t)
;; 	               (single-float-negative-epsilon nil)))
;; 	  (let ((epsilon (symbol-value (car sym)))
;; 	        (positive (cadr sym)))
;; 	    (format t "~30S = ~@30S, test = ~S~%"
;; 	            (car sym)
;; 	            epsilon
;; 	            (if positive
;; 	                (not (= (float 1 epsilon) (+ (float 1 epsilon) epsilon)))
;; 	              (not (= (float 1 epsilon) (- (float 1 epsilon) epsilon)))))
;; 	    ))
;; 	double-float-epsilon           =          2.220446049250313d-16, test = t
;; 	double-float-negative-epsilon  =          1.110223024625157d-16, test = t
;; 	long-float-epsilon             =          2.220446049250313d-16, test = t
;; 	long-float-negative-epsilon    =          1.110223024625157d-16, test = t
;; 	short-float-epsilon            =                    1.192093e-7, test = t
;; 	short-float-negative-epsilon   =                    5.960464e-8, test = t
;; 	single-float-epsilon           =                    1.192093e-7, test = t
;; 	single-float-negative-epsilon  =                    5.960464e-8, test = t
;; 	nil
;; 
(deftest fix-single-float-eplilon ()
  "single-float-epsilon とその仲間たちの値が、倍精度のそれと同じになってしまってたのを修正。"
  (dolist (sym '((double-float-epsilon t)
                 (double-float-negative-epsilon nil)
                 (long-float-epsilon t)
                 (long-float-negative-epsilon nil)
                 (short-float-epsilon t)
                 (short-float-negative-epsilon nil)
                 (single-float-epsilon t)
                 (single-float-negative-epsilon nil)))
    (let ((epsilon (symbol-value (car sym)))
          (positive (cadr sym)))
      (format t "~30S = ~@30S, test = ~S~%"
              (car sym)
              epsilon
              (if positive
                  (not (= (float 1 epsilon) (+ (float 1 epsilon) epsilon)))
                (not (= (float 1 epsilon) (- (float 1 epsilon) epsilon)))))
      ))
  >> double-float-epsilon           =          2.220446049250313d-16, test = t
  >> double-float-negative-epsilon  =          1.110223024625157d-16, test = t
  >> long-float-epsilon             =          2.220446049250313d-16, test = t
  >> long-float-negative-epsilon    =          1.110223024625157d-16, test = t
  >> short-float-epsilon            =                    1.192093e-7, test = t
  >> short-float-negative-epsilon   =                    5.960464e-8, test = t
  >> single-float-epsilon           =                    1.192093e-7, test = t
  >> single-float-negative-epsilon  =                    5.960464e-8, test = t
  >> 
  => nil
  )

;; 2010-09-30  MIYAMUKO Katsuyuki  <miyamuko@gmail.com>
;; 
;; 	* src : add deleted-window-p
;; 	削除された Window なら t を返す。
;; 	deleted-buffer-p の Window バージョン。
;; 	http://twitter.com/bowbow99/status/25935896653
;;

;; NOTE: 自前で用意してた場合 pass してしまう。
(deftest add-deleted-window-p-00 ()
  "削除された window なら t を返す deleted-window-p を追加。
関数かどうかチェック。"
  (fboundp 'deleted-window-p)
  => non-nil)

;; NOTE: 自前で用意してた場合 pass してしまう。
(deftest add-deleted-window-p-01 ()
  "削除された window なら t を返す deleted-window-p を追加。
削除された window に t を返すかチェック。"
  (let ((current (selected-window))
        (new     (save-window-excursion
                   (split-window -1)
                   (selected-window))))
    (deleted-window-p new))
  => non-nil)

(deftest add-deleted-window-p-02 ()
  "削除された window なら t を返す deleted-window-p を追加。
ビルトイン関数かどうかチェック。"
  (si:*builtin-function-p (symbol-function 'deleted-window-p))
  => non-nil)

;; 2010-09-12  MIYAMUKO Katsuyuki  <miyamuko@gmail.com>
;; 
;; 	* src : add optional value parameter to si:putenv.
;; 	引数を環境変数名とオプショナルな値をとるように変更。
;; 	値を省略した場合や nil を指定した場合は環境変数を削除する。
;; 	戻り値は環境変数の設定に成功した場合は設定した値を返し、
;; 	環境変数を削除した場合または設定に失敗した場合は nil を返すように変更。
;; 	(si:putenv "FOO" "bar")
;; 	=> "bar"
;; 	(si:putenv "FOO")
;; 	=> nil
;; 	(si:putenv "=FOO" "bar")
;; 	=> nil
;; 
(deftest add-putenv-00 ()
  "関数 si:putenv を追加＆引数を (VARNAME &optional VALUE) に"
  (multiple-value-bind (sym class)
      (find-symbol "putenv" :system)
    class)
  => :external)

(deftest add-putend-01 ()
  "関数 si:putenv を追加＆引数を (VARNAME &optional VALUE) に"
  (fboundp (find-symbol "putenv" :system))
  => non-nil)

(deftest add-putenv-02 ()
  "関数 si:putenv を追加＆引数を (VARNAME &optional VALUE) に"
  (funcall (find-symbol "putenv" :system) "FOO" "bar")
  => "bar")

(deftest add-putenv-03 ()
  "関数 si:putenv を追加＆引数を (VARNAME &optional VALUE) に"
  (progn
    (funcall (find-symbol "putenv" :system) "FOO" "baz")
    (si:getenv "FOO"))
  => "baz")

(deftest add-putenv-04 ()
  "関数 si:putenv を追加＆引数を (VARNAME &optional VALUE) に"
  (values
   (funcall (find-symbol "putenv" :system) "FOO" nil)
   (si:getenv "FOO"))
  => nil
  => nil)

;; 2010-09-11  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : remove redefinition.
;; 	Microsoft Visual C++ 2010 では wingdi.h がインクルードされて、
;; 	tagWCRANGE と tagGLYPHSET が定義されるようなので gen-fontrange.cc での
;; 	定義を削除。
;; 	とりあえずVC2010でのみ削除。
;; 

;; 	* src : remove pragma optimize.
;; 	「internal compiler error」対策とのコメントがあるが、Microsoft Visual
;; 	C++ 2010 ではエラーが発生しないので削除。
;; 	とりあえずVC2010でのみ削除。
;; 

;; 	* src : add si:putenv.
;; 	si:putenv を追加。
;; 	via http://d.hatena.ne.jp/miyamuko/20100910/xyzzy_putenv
;; 
;; NOTE: その後変更されてるんでそっちでテスト

;; 2010-08-31  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix call-process.
;; 	call-processの標準入力・標準出力等の指定がうまく設定されない現象がおき
;; 	ていたのを修正。
;;
;; TODO: command prompt が一瞬表示されるのうざい
;; FIXME: 0.2.2.235 で pass してしまう。どういう場合に上手く設定されないのかわからん。
; (deftest fix-call-process-input/output ()
;   (labels ((make-file (name &optional contents)
;              (with-open-file (out name
;                                   :direction :output
;                                   :if-does-not-exist :create)
;                (when (princ contents out)))
;              name))
;     (let ((infile (make-file (make-temp-file-name) "hello"))
;           (outfile (make-file (make-temp-file-name) "")))
;       (call-process "sort" :input infile :output outfile :wait t)
;       (unwind-protect
;           (with-open-file (in outfile :direction :input)
;             (read in))
;         (delete-file infile)
;         (delete-file outfile))))
;   => hello
;   )

;; 2010-07-14  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix format "~T".
;; 	「~colnum,colincT」のとき、「colnum+k*colinc」を満たす最小の正の整数kで
;; 	桁を求めるように修正。
;; 	(format nil "~0,1T")
;; 	=> " "
;; 
(deftest fix-format-T ()
  (format t "~0,1Tx")
  >>  x
  => nil)

;; 	* src : fox format "~VT" in the case of being provided '() as a prefix parameter.
;; 	Vに引数としてnilを渡したときの動作を修正。
;; 	(format t "~VA" nil 'x)
;; 	== (format t "~A" 'x)
;; 
(deftest fix-format-VT ()
  (format t "~VA" nil 'x)
  >> x
  => nil)

;; 	* src : fix for VC2010.
;; 	Microsoft Visual C++ 2010 でのビルド用に修正。
;; 	Makefileの修正のみ。
;; 

;; 2010-04-04  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : add repl.
;; 	コマンドプロンプトを表示するバージョンxyzzyrpl.exeを作成できるようにし
;; 	た。「nmake repl」で作成できる。
;; 
;; 	;;; 簡易REPL
;; 	(defun repl ()
;; 	  (interactive)
;; 	  (let ((*standard-input* *terminal-io*)
;; 	        (*standard-output* *terminal-io*))
;; 	    (loop
;; 	      (format t "~%> ")
;; 	      (format t "~A" (eval (read *standard-input*))))))
;; 
;; NOTE: merge されてない（xyzzy Part17 >>593 http://toro.2ch.net/test/read.cgi/win/1303662374/593）

;; 2010-03-01  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix "*load-pathname*" initialization.
;; 	ダンプ作成時にロードしたファイル名がそのままダンプファイルに保存される
;; 	ようなので、起動時にも初期化するように修正。
;; 
(deftest fix-*load-pathname*-from-dump-image ()
  "ダンプ作成時にロードしたファイル名がそのままダンプファイルに保存される
ようなので、起動時にも初期化するように修正。"
  ;; FIXME: ダンプイメージがまだ無い場合 pass ってしまう。
  (eval-in-another-xyzzy
   `*load-pathname*
   :options "-q")
  => nil
  )

;; 	* src : fix format "~F".
;; 	formatの"~F"の第一パラメータwを指定しないと第三パラメータkが機能しない
;; 	のを修正。
;; 
;; 	(dotimes (i 10)
;; 	  (format t "~,,VF~%" i pi))
;; 	3.141592653589793
;; 	31.41592653589793
;; 	314.1592653589793
;; 	3141.592653589793
;; 	31415.92653589793
;; 	314159.2653589793
;; 	3141592.653589793
;; 	31415926.53589793
;; 	314159265.3589793
;; 	3141592653.589793
;; 	nil
;; 
;; 	(do ((i -9 (1+ i)))
;; 	    ((> i 9))
;; 	  (format t "~,,VF~%" i pi))
;; 	-0.000000009
;; 	-0.00000008
;; 	-0.0000007
;; 	-0.000006
;; 	-0.00005
;; 	-0.0004
;; 	-0.003
;; 	-0.02
;; 	-0.1
;; 	0.0
;; 	10.0
;; 	200.0
;; 	3000.0
;; 	40000.0
;; 	500000.0
;; 	6000000.0
;; 	70000000.0
;; 	800000000.0
;; 	9000000000.0
;; 	nil
;;
(deftest fix-format-F-01 ()
  (dotimes (i 10)
    (format t "~,,VF~%" i pi))
  >> 3.141592653589793
  >> 31.41592653589793
  >> 314.1592653589793
  >> 3141.592653589793
  >> 31415.92653589793
  >> 314159.2653589793
  >> 3141592.653589793
  >> 31415926.53589793
  >> 314159265.3589793
  >> 3141592653.589793
  >> 
  => nil
  )


(deftest fix-format-F-02 ()
  (do ((i -9 (1+ i)))
      ((> i 9))
    (format t "~,,VF~%" i i))
  >> -0.000000009
  >> -0.00000008
  >> -0.0000007
  >> -0.000006
  >> -0.00005
  >> -0.0004
  >> -0.003
  >> -0.02
  >> -0.1
  >> 0.0
  >> 10.0
  >> 200.0
  >> 3000.0
  >> 40000.0
  >> 500000.0
  >> 6000000.0
  >> 70000000.0
  >> 800000000.0
  >> 9000000000.0
  >> 
  => nil
  )

;; 2010-02-21  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix applyhook.
;; 	applyhook, *applyhook* をローカル関数に対応させた。
;; 	call_applyhookの第一引数にクロージャを渡すように変更した。
;; 
(deftest fix-applyhook-00 ()
  "applyhook, *applyhook* をローカル関数に対応させた。"
  (let ((*applyhook* (lambda (fn args)
                       (functionp fn))))
    (car 1))
  => non-nil)

;; 	* src : add "*read-eval*".
;; 	スペシャル変数*read-eval*が機能するようにした。
;; 	*read-eval*の出力への影響の理解が怪しい。
;; 	*read-eval*がnilの場合はencodingをunreadable-objectとして出力するようし
;; 	ているが、自信なし。
;; 
(deftest add-*read-eval*-00 ()
  "スペシャル変数*read-eval*が機能するようにした。"
  (si:*specialp '*read-eval*)
  => non-nil)

(deftest add-*read-eval*-01 ()
  "スペシャル変数*read-eval*が機能するようにした。"
  (let ((*read-eval* nil))
    (read-from-string "#.(+ 1 2)"))
  !! reader-error)

;; TODO: 出力への影響？

;; 2010-01-23  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix for VC2008.
;; 	Microsoft Visual C++ 2008 でのビルド用に修正。
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?Memo%2F%A5%BD%A1%BC%A5%B9%A4%AB%A4%E9%A5%D3%A5%EB%A5%C9%A4%B7%A4%C6%A4%DF%A4%EB3
;; 
;; NOTE: テストはいらんと思うけど、commit 見当たらない。

;; 	* src : add get-buffer-colors.
;; 	関数get-buffer-colorsを追加。
;; 	set-buffer-colorsと対となる現在のバッファの色設定を取得する関数。
;;
(deftest add-get-buffer-colors-00 ()
  "関数get-buffer-colorsを追加。"
  (fboundp 'get-buffer-colors)
  => non-nil)

;; NOTE: NANRI さんからの頂き物; thx
(deftest add-get-buffer-colors-01 ()
  "関数get-buffer-colorsは現在のバッファの色設定を返す。"
  (flet ((make-tmp-colors ()
           (let ((l (list #xffffff #x000000)))
             (dotimes (i 16)
               (push (* i #x111111) l))
             (make-vector 18 :initial-contents (nreverse l)))))
    (let ((colors (make-tmp-colors))
          (buf (create-new-buffer "test get-buffer-colors")))
      (save-excursion
        (with-set-buffer
          (set-buffer buf)
          (unwind-protect
              (progn
                (set-buffer-colors colors)
                (equalp colors (get-buffer-colors buf)))
            (delete-buffer buf))))))
  => non-nil)

;; 2010-01-22  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix listen.
;; 	EOFのときにnilを返すように修正。
;; 	分かる範囲ということでstring-streamとfile-streamだけ。
;; 
(deftest fix-listen-for-string-stream ()
  "EOFのときにnilを返すように修正（string-stream）"
  (let ((stream (make-string-input-stream "")))
    (listen stream))
  => nil)

(deftest fix-listen-for-file-stream ()
  "EOFのときにnilを返すように修正（file-stream）"
  (with-temp-files (file)
    (with-open-file (in file :direction :input)
      (listen in)))
  => nil)

;; 2010-01-11  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : fix save-window-excursion.
;; 	(save-window-excursion (values 1 2 3))
;; 	で正しく多値が返らない場合があるのを修正。
;; 	xyzzy Part11 8 http://www.bookshelf.jp/2ch/win/1085301777.html
;;
;; 	WindowConfigurationのデストラクタ内でLispのコードが実行されるので、
;; 	WindowConfigurationのデストラクタを実行したあとで、多値を元の値に戻
;; 	す必要がある。
;; 
;; FIXME: 未修正でも正しく多値が返ることもあるみたいなので、その時は pass してしまう
(deftest fix-save-window-excursion ()
  "正しく多値が返らない場合があるのを修正"
  (save-window-excursion (values 1 2 3))
  => 1
  => 2
  => 3)

;; 	* lisp : fix long-operation.
;; 	long-operationの戻り値がprog1相当になっているのをprogn相当になるよ
;; 	うに修正。
;; 	xyzzy Part10 470
;; 
(deftest fix-long-operation ()
  "long-operationの戻り値がprog1相当になっているのをprogn相当になるよ
うに修正。"
  (long-operation
    1
    2)
  => 2)

;; 2009-12-26  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : update zlib.
;; 	zlibをバージョン1.2.3にアップデート。
;; 

;; 2009-12-17  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* lisp : modify tags-setup-buffer.
;; 	XTAGSファイルを探す位置を少々変更。
;; 
;; NOTE: merge されてない（xyzzy Part17 >>593 http://toro.2ch.net/test/read.cgi/win/1303662374/593）

;; 	* lisp : modify edict-lookup-word.
;; 	辞書を引く単語をカーソルの前方に変更。
;; 
;; NOTE: merge されてない（xyzzy Part17 >>593 http://toro.2ch.net/test/read.cgi/win/1303662374/593）

;; 	* lisp : modify start point in buffer-menu.
;; 	buffer-menuを実行したときのカーソル位置を現在のバッファの位置に変更。
;;
;; NOTE: merge されてない（xyzzy Part17 >>593 http://toro.2ch.net/test/read.cgi/win/1303662374/593）
; (deftest modify-start-point-in-buffer-menu ()
;   (let ((buffers '()))
;     (do ((i 0 (1+ i)))
;         ((>= i 4))
;       (push (create-new-buffer "*Test Example*") buffers))
;     (prog1
;         (save-window-excursion
;           (with-set-buffer
;             (set-buffer (car buffers))
;             (buffer-menu)
;             (and (bolp)
;                  (looking-at "^\\."))))
;       (dolist (b buffers)
;         (delete-buffer b)))))

;; 	* src : add key parameter ":show" to make-process.
;; 	make-processにキーパラメータ :show を追加する。
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?patch%2F12
;; 
;; FIXME: テストする方法が思いつかない

;; 2009-12-16  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : support IMR_DOCUMENTFEED.
;; 	via http://fixdap.com/p/xyzzy/7376/
;; 	以下で動作を確認。
;; 	さいた　　　　　あたたかい
;; 	布を　　　　　　牛乳
;; 	花が　　　　　　春
;; 	時間を　　　　　
;; 
;; FIXME: テストする方法が思いつかない

;; 	* src : fix set-buffer-modified-p.
;; 	(set-buffer-modified-p nil) をした時に以前更新なし状態だった点が更
;; 	新無し状態のままなのを修正。
;;
;; TODO: 変更の内容がよくわからんので後回し
; (deftest fix-set-buffer-modified-p ()
;   (let ((buffer (create-new-buffer "*Example*")))
;     (set-buffer-modified-p nil buffer)
;     (buffer-modified-p buffer))
;   => nil)

;; 	* src : fix inverse-cursor-line.
;; 	(setq inverse-cursor-line t)
;; 	の状態にすると行カーソルと折り返し線の交点が反転したまま残ってしま
;; 	うのを修正。
;;
;; FIXME: テストする方法が思いつかない

;; 2009-12-09  NANRI Masaoki  <southly@gmail.com>
;; 
;; 	* src : add "*brackets-is-wildcard-character*" to history-variable.
;; 	共通設定の「[...]はワイルドカード」の設定が保存されないので、対応す
;; 	る変数をヒストリに追加。
;;
(deftest add-*brackets-is-wildcard-character*-to-history-variable ()
  "共通設定の「[...]はワイルドカード」の設定が保存されないので、対応す
る変数をヒストリに追加。"
  (member '*brackets-is-wildcard-character* ed::*history-variable-list*)
  => non-nil)

;; 	* src : fix dump hash-table.
;; 	シンボルがキーのハッシュテーブルをダンプファイルに入れた場合、
;; 	gethashで値がとれない問題があった。ただし、maphashで全要素を取得す
;; 	ることはできた。
;; 	---- $XYZZY/site-lisp/siteinit.l の内容
;; 	(defparameter *hash* (make-hash-table))
;; 	(dolist (i '((aaa . abab) (bbb . bcbc) (ccc . cdcd)))
;; 	  (setf (gethash (car i) *hash*) (cdr i)))
;; 	---- ダンプファイルをロードしたあとに評価
;; 	(gethash 'aaa *hash*)
;; 	=>nil ; 本来は abab
;; 	=>nil ;        t
;; 
;; 	(maphash #'(lambda (key val) (format t "~S: ~S~%" key val)) *hash*)
;; 	aaa: abab
;; 	bbb: bcbc
;; 	ccc: cdcd
;; 	=>nil
;; 	----
;; 	シンボルのハッシュ値はオブジェクトのアドレスを元に計算しているが、
;; 	ダンプファイル作成時とダンプファイルロード後ではシンボルオブジェク
;; 	トのアドレスが異なり、同一のハッシュ値にならないのが原因。
;; 
;; 	ダンプファイルをロードしたあとにrehashするように修正。
;;

;; FIXME: ダミーの site-lisp/siteinit.l を用意してやらにゃならんらしい。
; (deftest fix-dump-hash-table ()
;   (progn
;     (with-another-xyzzy (:options "-q")
;       (defparameter *hash* (make-hash-table))
;       (dolist (i '((aaa . abab) (bbb . bcbc) (ccc . cdcd)))
;         (setf (gethash (car i) *hash*) (cdr i)))
;       (dump-xyzzy (merge-pathnames "dump-image-for-test.wxp" (si:system-root))))
;     (let ((image-name (merge-pathnames "dump-image-for-test.wxp" (si:system-root))))
;       (unless (file-exist-p image-name)
;         (error "Failed to create dump-image."))
;       (multiple-value-prog1
;           (with-another-xyzzy (:options (format nil "-image ~S -q" image-name))
;             (gethash 'aaa *hash*))
;         ;(delete-file image-name)
;         )))
;   => abab
;   => t)

;; 	* lisp : fix defpackage.
;; 	defpackageマクロで:exportオプションを指定できない不具合を修正。
;; 	[xyzzy:09241]
;; 
(deftest fix-depackage-export ()
  "defpackageマクロで:exportオプションを指定できない不具合を修正。"
  (defpackage :example
    (:export #:foo))
  == (eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((package (or (find-package "example")
                           (make-package "example"))))
         (export (mapcar #'(lambda (lisp::x)
                             (intern lisp::x package))
                   '("foo"))
                 package)
         package)))

;; 	* lisp : fix shell-alternate-send-input.
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?patch%2F15
;; 

(require "shell")

(deftest fix-shell-alternate-send-input ()
  "shell-alternate-send-input は改行コードをスイッチするように修正"
  (labels ((mocked-shell-send-input ()
             *shell-ret*))
    (let ((original-shell-send-input #'shell-send-input))
      (setf (symbol-function 'shell-send-input) #'mocked-shell-send-input)
      (unwind-protect
          (let ((*shell-ret* "\n"))
            (shell-alternate-send-input))
        (setf (symbol-function 'shell-send-input) original-shell-send-input))))
  => "\r")

;; 	* lisp : fix calendar.
;; 	カレンダーの祝日を現行法にあわせる。
;; 	[xyzzy:09224]
;; 
(require "calendar")

(defun calendar-japanese-holiday-spec (year month day)
  (let ((v (make-vector 33)))
    (ed::calendar-japanese-holiday year month v)
    (svref v day)))

(deftest fix-calendar-01 ()
  "calendar の祝日を現行法に合わせる"
  (calendar-japanese-holiday-spec 2007 4 29)
  => "昭和の日")

(deftest fix-calendar-02 ()
  "calendar の祝日を現行法に合わせる"
  (calendar-japanese-holiday-spec 2007 5 4)
  => "みどりの日")

(deftest fix-calendar-03 ()
  "calendar の祝日を現行法に合わせる"
  (calendar-japanese-holiday-spec 2008 5 6)
  => "振替休日")

(deftest fix-calendar-04 ()
  "calendar の祝日を現行法に合わせる"
  (calendar-japanese-holiday-spec 2016 8 11)
  => "山の日")

;; 	* lisp : fix typo in lisp/timestmp.l.
;; 	コメントのtypoを修正。
;; 	[xyzzy:09158]
;;

;; 	* lisp : fix typo in lisp/encoding.l.
;; 	provideのtypoを修正。
;; 	[xyzzy:09158]
;;
(deftest fix-typo-in-lisp/encoding.l ()
  "lisp/encoding.l で provide しているモジュール名の typo を修正"
  (modulep "encoding")
  => non-nil)

;; 	* lisp : fix typo in lisp/css-mode.l.
;; 	provideのtypoを修正。
;; 	[xyzzy:09158]
;;
(deftest fix-typo-in-lisp/css-mode.l ()
  "lisp/css-mode.l で provide しているモジュール名の typo を修正"
  (progn
    (require "css-mode")
    (and (modulep "css-mode")
         (not (modulep "cssmode"))))
  => non-nil)

;; 	* lisp : fix typo in lisp/builtin.l.
;; 	主に引数のtypoを修正。
;; 	[xyzzy:09158]
;;
(deftest fix-typo-in-lisp/builtin--sort ()
  "builtin.l の typo を修正: sort"
  (get 'sort 'si::builtin-function-argument #1='#:undefined)
  => (sequence lisp::predicate &key :key))

(deftest fix-typo-in-lisp/builtin--rename-file ()
  "builtin.l の typo を修正: rename-file"
  (get 'rename-file 'si::builtin-function-argument #1='#:undefined)
  => (lisp::file lisp::new-name &key :if-exists :if-access-denied))

(deftest fix-typo-in-lisp/builtin--copy-file ()
  "builtin.l の typo を修正: copy-file"
  (get 'copy-file 'si::builtin-function-argument #1='#:undefined)
  => (lisp::file lisp::new-file &key :if-exists :copy-attributes :if-access-denied))

(deftest fix-typo-in-lisp/builtin--dump-image-path ()
  "builtin.l の typo を修正: si:dump-image-path"
  (get 'system:dump-image-path 'si::builtin-function-argument #1='#:undefined)
  => nil)

(deftest fix-typo-in-lisp/builtin--char-before ()
  "builtin.l の typo を修正: char-before"
  (get 'char-before 'si::builtin-function-argument #1='#:undefined)
  => (point &optional editor::not-used))

(deftest fix-typo-in-lisp/builtin--file-name-dialog ()
  "builtin.l の typo を修正: file-name-dialog"
  (get 'file-name-dialog 'si::builtin-function-argument #1='#:undefined)
  => (&key :char-encoding :eol-code :filter :filter-index :title :default :extension :save :multiple :must-exist :explorer :hide-read-only :initial-directory))

(deftest fix-typo-in-lisp/builtin--directory-name-dialog ()
  "builtin.l の typo を修正: directory-name-dialog"
  (get 'directory-name-dialog 'si::builtin-function-argument #1='#:undefined)
  => (&key :title :default))

(deftest fix-typo-in-lisp/builtin--make-iso2022-encoding ()
  "builtin.l の typo を修正: make-iso2022-ecnoding"
  (get 'make-iso2022-encoding 'si::builtin-function-argument #1='#:undefined)
  => (editor::name editor::display-name &key :g0 :g1 :g2 :g3 :ascii-eol :ascii-control :7bits :locking-shift :short-form :no-escape :cjk :use-cns11643 :vender))

(deftest fix-typo-in-lisp/builtin--make-utf16-encoding ()
  "builtin.l の typo を修正: make-utf16-encoding"
  (get 'make-utf16-encoding 'si::builtin-function-argument #1='#:undefined)
  => (editor::name editor::display-name &key :signature :byte-order :windows :cjk))

(deftest fix-typo-in-lisp/builtin--make-utf8-encoding ()
  "builtin.l の typo を修正: make-utf8-encoding"
  (get 'make-utf8-encoding 'si::builtin-function-argument #1='#:undefined)
  => (editor::name editor::display-name &key :signature :windows :cjk))

(deftest fix-typo-in-lisp/builtin--make-utf7-encoding ()
  "builtin.l の typo を修正: make-utf7-encoding"
  (get 'make-utf7-encoding 'si::builtin-function-argument #1='#:undefined)
  => (editor::name editor::display-name &key :windows :imap4 :white :set-o :cjk))

(deftest fix-typo-in-lisp/builtin--make-utf5-encoding ()
  "builtin.l の typo を修正: make-utf5-encoding"
  (get 'make-utf5-encoding 'si::builtin-function-argument #1='#:undefined)
  => (editor::name editor::display-name &key :windows :imap4 :white :set-o :cjk))

(deftest fix-typo-in-lisp/builtin--parse-char-encoding-string ()
  "builtin.l の typo を修正: parse-char-encoding-string"
  (get 'parse-char-encoding-string 'si::builtin-function-argument #1='#:undefined)
  => (editor::accept-char-encoding string &key :start :end))

;; 	* src : fix hash-table rehash size.
;; 	ハッシュテーブルの要素数が80000を越える辺りから、ハッシュテーブルの
;; 	サイズの増加量が100に固定される。そのため頻繁にrehashされパフォーマ
;; 	ンスが低下していた。
;; 	rehashするときは最低でも前のサイズの1.5倍になるようにした。
;;
;; NOTE: やたらと時間掛かるんで保留
;; Ref: https://gist.github.com/227683

;; 	* src : fix ole-method.
;; 	ole-method で文字列を渡すと余計な 0x00 が付く問題
;; 	via http://fixdap.com/p/xyzzy/8379/
;; 	via http://d.hatena.ne.jp/miyamuko/20080304/xyzzy_ole_method_bug
;;
;; NOTE: miyamuko さんからの頂き物; thx
(deftest fix-ole-method ()
  "ole-method で文字列を渡すと余計な 0x00 が付く問題"
  (flet ((ole-create-file (content)
           (with-temp-files (tmp)
             (let* ((fso (ole-create-object "Scripting.FileSystemObject"))
                    (file (ole-method fso 'OpenTextFile tmp 2)))
               (unwind-protect
                   (ole-method file 'Write content)
                 (ole-method file 'Close))
               (file-length tmp)))))
    (values (ole-create-file "")
            (ole-create-file "f")
            (ole-create-file "foo")))
  => 0
  => 1
  => 3)


;; 	* src : fix for FFI.
;; 	戻り値が double, float の C 関数を呼ぶとクラッシュする
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?patch%2F25
;; 	via http://d.hatena.ne.jp/miyamuko/20070509/p1
;;
(deftest fix-for-FFI-c-function-return-doubl/float-00 ()
  "戻り値が double, float の C 関数を呼ぶとクラッシュする"
  (eval-in-another-xyzzy
    `(labels ((c (name) (find-symbol name :c)))
       (require "foreign")
       (eval `(,(c "define-dll-entry")
               ,(c "double")
               strtod (,(c "char*") (,(c "char*") *))
               "msvcrt"))
       (strtod (si:make-string-chunk "123") 0))
    :options "-q" :timeout 6)
  => 123.0d0
  )


;; 	* src : fix start-timer.
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?patch%2F21
;; 

;; NOTE: タイマーからバックグラウンドでテスト実行すると sleep-for しても
;; start-timer した関数が実行されないので返ってこなくなる。のでタイムアウト
;; させてる。
(deftest fix-start-timer ()
  (let ((n 0)
        (start (get-internal-real-time))
        thunk
        (finish nil))
    (setf thunk (lambda ()
                  (when (>= (incf n) 3)
                    (stop-timer thunk)
                    (setf finish (get-internal-real-time)))))
    (start-timer 0.5 thunk)
    (while (not finish)
      (sleep-for 0.2)
      (when (> (- (get-internal-real-time) start) 6000)
        (error "fix-start-timer test timeout.")))
    (> (- finish start) 1400))
  => non-nil)

;; 	* src : fix format "~G".
;; 	[xyzzy:09204]
;;
;; 	http://hie.s64.xrea.com/xyzzy/ml/msg09203.html
(deftest fix-format-G ()
  (format t "~10g" 1.23456d+38)
  >> 123456000000000000000000000000000000000.0    
  => nil)

;; 	* src : fix format "~E".
;; 	[xyzzy:09204]
;;
;; 	http://hie.s64.xrea.com/xyzzy/ml/msg09202.html
(deftest fix-format-E-00 ()
  (format t "~E" 123.45)
  >> 1.2345e+2
  => nil)

(deftest fix-format-E-01 ()
  (format t "~0E" 123.45)
  >> 1.2345e+2
  => nil)

;; 	* src : fix format "~F".
;; 	[xyzzy:09204]
;;
;; 	http://hie.s64.xrea.com/xyzzy/ml/msg09202.html
(deftest fix-format-F-00 ()
  (format t "~@F" 123.45)
  >> +123.45
  => nil)

;; 	* src : fix format "~R".
;; 	[xyzzy:09198] http://hie.s64.xrea.com/xyzzy/ml/msg09197.html
;;
(deftest fix-format-R ()
  (format t "~16,10,'*,'-,2:R" #x123abc)
  >> **12-3a-bc
  => nil)

;; 	* src : fix for VC2005.
;; 	Microsoft Visual C++ 2005 でビルドできるように、こまごまと修正。
;; 	http://xyzzy.s53.xrea.com/wiki/index.php?patch%2F13
;; 

;(when (yes-or-no-p "今すぐテストする？")
;  (test-changes))
;; ;;; nanri-master-change-tests.l ends here.
