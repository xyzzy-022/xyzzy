xyzzy リリースノート
====================

  * バージョン: 0.2.2.239
  * リリース日: 2012-05-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

xyzzy 0.2.2.239 では主にファイラのフォント設定、
FFI の改善を行なっています。


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.239 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * ファイラに最大化ボタンを付けました (x022235, #297)

  * ファイラのフォントを設定する関数を追加しました (x022235, #57)

    ```lisp
    ;; ファイラのフォントサイズを設定
    (add-hook '*init-app-menus-hook*
              #'(lambda ()
                  (set-filer-font :face "メイリオ" :size 14)))
    ```

    ※共通設定から設定することはまだ出来ません。今後対応予定です。

  * メニューの最大数を増やしました (xyzzy-17-638, #223)

    今まではメニューの項目数が 1024 個を超えると「メニュー項目が多すぎます」という
    エラーになっていましたが、今後は約 32,000 個のメニューを作成可能となります。

    ※マルチフレーム版から取り込みました。

  * C のキーワードファイルを更新しました (x022235, #279)

    C11 で追加されたキーワードなど、以下のキーワードを追加しました。

    ```
    _Alignas
    _Alignof
    _Atomic
    _Generic
    _Noreturn
    _Static_assert
    _Thread_local
    __func__
    __DATE__
    __FILE__
    __LINE__
    __TIME__
    __VA_ARGS__
    inline
    ```

  * Perl のキーワードファイルを更新しました (x022235, #277)

    Perl 5.16 で追加されたキーワードなど、以下のキーワードを追加しました。

    ```
    q
    x
    fc
    evalbytes
    __SUB__
    ```

  * コマンドライン オプションを追加しました (x022235, #172)

    * `-r, -require MODULE`
      * 指定したモジュールを `*load-path*` から探して読み込む
    * `-I, -load-path PATH`
      * 指定したパスを `*load-path*` に追加する
    * `-work-dir DIR`
      * 指定したディレクトリに移動する
    * `-trace`
      * `(toggle-trace-on-error t)` する
    * `-kill`
      * xyzzy を終了する
      * これ以降に指定されたオプションは無視される


バグ修正
--------

  * `indent-region` が最初の行をインデントするようにしました (x022235, #247)


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * `*process-command-line-hook*` から xyzzycli.exe のワーキングディレクトリを取得できるようにしました (x022235, #257)

    xyzzycli.exe のワーキングディレクトリは `*command-line-args-base-directory*` で取得可能です。
    この変数は `*process-command-line-hook*` を実行している時のみ値が設定されます。

    xyzzy.exe のワーキングディレクトリと異なる場所から xyzzycli.exe を使って相対パス指定でファイルを開くと、
    `*process-command-line-hook*` には相対パスのファイルパスのみが渡され、
    xyzzycli.exe のワーキングディレクトリが不明なためファイルのパスがフックからは分からない問題を修正しました。

    ```lisp
    (add-hook '*process-command-line-hook*
              #'(lambda (file)
                  (msgbox "~A" (merge-pathnames file *command-line-args-base-directory*))))
    ```

  * `nth-value` を追加しました (x022235, #292)

    ```lisp
    (nth-value 1 (values 1 2 3))
    => 2
    ```

  * builtin パッケージの一覧を返す関数を追加しました (x022235, #290)

    ```lisp
    (si:list-builtin-packages)
    => (#<package: lisp> #<package: system> #<package: keyword> #<package: user>
        #<package: editor> #<package: common-lisp> #<package: common-lisp-user>)

    (si:*builtin-package-p :system)
    t
    ```

  * `C-u M-x apropos` で全パッケージから `apropos` 出来るようにしました (x022235, #287)

  * `apropos` の結果バッファを `lisp-interaction-mode` にしました (x022235, #287)

  * `apropos` の結果バッファでパッケージ名を出力するようにしました (x022235, #287)

    `M-x apropos` で `defun` を検索した結果。

    ```
    ;; In lisp package
    defun

    ;; In editor package
    beginning-of-defun
    end-of-defun
    eval-defun
    mark-defun
    mode-specific-beginning-of-defun
    mode-specific-end-of-defun
    ```

    `C-u M-x apropos` で `defun` を検索した結果。

    ```
    ;; In lisp package
    defun

    ;; In system package
    si::defun-builtin
    si::defun-builtin-1

    ;; In editor package
    beginning-of-defun                      ESC C-a
    end-of-defun                            ESC C-e
    eval-defun                              ESC C-x
    mark-defun
    mode-specific-beginning-of-defun
    mode-specific-end-of-defun

    ;; In foreign package
    c:*defun-c-callable
    c:defun-c-callable
    ```

    参考: 0.2.2.238 で `M-x apropos` で `defun` を検索した結果。

    ```
    beginning-of-defun
    defun
    end-of-defun
    eval-defun
    mark-defun
    mode-specific-beginning-of-defun
    mode-specific-end-of-defun
    ```

  * `ed::find-file-internal` などパッケージ名がついたシンボルでタグジャンプできるようにしました (x022235, #286)

  * xyzzy.ini のパス取得関数を追加しました (xyzzy-17-638, #280)

    ```lisp
    (xyzzy-ini-path)
    => "C:/Tools/xyzzy/usr/Administrator/wxp/xyzzy.ini"
    ```

    ※マルチフレーム版から取り込みました。

  * xyzzy 終了時の exit status を指定できるようにしました (x022235, #278)

    ```lisp
    (kill-xyzzy t)   ; EXIT_SUCCESS
    (kill-xyzzy nil) ; EXIT_FAILURE
    (kill-xyzzy 123) ; exit コード指定
    ```

  * 現在のパッケージで `eval-region`, `eval-print-last-sexp` するようにしました (snmsts, #260)

    以下の順序でパッケージを決定します。
    1. `*buffer-package*` の値
    2. `*find-buffer-package-hook*` の評価結果

    `*find-buffer-package-hook*` にはデフォルトで `in-package` で指定した
    パッケージを返す関数が登録されています。
    評価時のパッケージをカスタマイズしたい場合はこのフックを利用してください。
    フックは `run-hook-with-args-until-success` で実行されます。引数はありません。

    注意: lpp をインストールしていると `eval-region`, `eval-print-last-sexp` が
    上書きされるので注意してください。

  * chunk の印字形式に情報を追加しました (x022235, #243)

    以下の様な形式で印字されます。

    ```lisp
    (si:make-chunk :int32[] (* 4 2))
    => #<chunk :type :int32[] :size 8 191191356>
    ```

  * `si:chunk` 型を定義しました (x022235, #242)

    `check-type` や `typep` で chunk かどうかをチェックできます。

    ```lisp
    (setf c (si:make-chunk nil 4))
    => #<chunk :type nil :size 4 191190700>

    (typep c 'si:chunk)
    => t			; 0.2.2.238 では => nil

    (check-type c si:chunk)
    => nil			; 0.2.2.238 では => `c'の値`#<chunk 183636044>'はsystem:chunkではありません

    (setf c "hoge")
    => "hoge"

    (check-type c si:chunk)
    => `c'の値`"hoge"'はsystem:chunkではありません
    ```

  * pack/unpack 時に発生した例外を処理するようにしました (x022235, #251)

    chunk を pack/unpack するときに不正なポインタを参照した場合に、
    xyzzy ごとクラッシュしていた問題を修正しました。

    ```lisp
    (setf c (si:make-chunk nil 4 nil 0))
    => #<chunk :type nil :size 4 191192204>

    (handler-case
        (si:unpack-int32 c 0)
      (win32-exception (e)
        (format nil "~A" e)))
    => "Win32例外が発生しました: Access violation (c0000005) at 0x1016ba6"
    ```

    注意: `win32-exception` は `serious-condition` のサブコンディションなので、
    `ignore-errors` では補足できません。

  * FFI 呼び出し時に発生した例外を処理するようにしました (x022235, #250)

    `c:define-dll-entry` で定義した関数を呼び出した時に引数を間違えた場合や
    DLL 内部のバグなどで xyzzy ごとクラッシュしていた問題を修正しました。

    ```lisp
    (c:define-dll-entry
      c:int
      GetModuleHandleA ((c:void *))
      "kernel32")
    => GetModuleHandleA

    (handler-case
        (GetModuleHandleA 32)
      (win32-exception (e)
        (format nil "~A" e)))
    => "Win32例外が発生しました: Access violation (c0000005) at 0x772cf7da"
    ```

    注意: `win32-exception` は `serious-condition` のサブコンディションなので、
    `ignore-errors` では補足できません。

  * pack/unpack で 64 bit 整数に対応しました (x022235, #52)

    `si:pack-int64`、`si:pack-uint64`、`si:unpack-int64`、`si:unpack-uint64`
    関数を追加しました。

    また、`c:int64` および `c:uint64` 型を定義しました。

    ```lisp
    (setf c (si:make-chunk nil 8))
    => #<chunk :type nil :size 8 191180684>

    (si:pack-int64 c 0 9876543210)
    => 9876543210

    (si:unpack-int64 c 0)
    => 9876543210

    (c:define-dll-entry
      c:int64
      (_atoi64 :convention :cdecl) ((c:void *))
      "msvcrt")
    => _atoi64

    (_atoi64 (si:make-string-chunk "9876543210"))
    => 9876543210
    ```

  * cdecl 呼び出し規約に対応しました (x022235, #51)

    `c:define-dll-entry` と `c:defun-c-callable` で呼び出し規約を指定できるようにしました。
    関数名を `(NAME :convention CONVENTION)` という形式のリストで指定します。
    `CONVENTION` には `:cdecl` か `:stdcall` を指定できます。
    `CONVENTION` を指定しない場合、または従来通り関数名だけを指定する場合は
    `:stdcall` として扱います。

    コールバックを cdecl で呼び出す関数に `c:defun-c-callable` で作成した関数を指定すると
    クラッシュしていた問題を修正しました。

    ```lisp
    (c:define-dll-entry
      c:void
      (qsort :convention :cdecl) ; cdecl 呼び出し
      ((c:void *)                ; base
       c:size_t                  ; num
       c:size_t                  ; width
       (c:void *))               ; compare
      "msvcrt")
    => qsort

    (c:defun-c-callable
      c:int
      (int32-comparator :convention :cdecl) ; cdecl 呼び出し
      (((c:void *) elem1)
       ((c:void *) elem2))
      (let ((a (si:unpack-int32 (si:make-chunk 'int 4 nil elem1) 0))
            (b (si:unpack-int32 (si:make-chunk 'int 4 nil elem2) 0)))
        (cond ((= a b) 0)
              ((< a b) -1)
              (t 1))))
    => int32-comparator

    (flet ((unpack-int32-array (array n)
             (let (r)
               (dotimes (i n)
                 (push (si:unpack-int32 array (* i 4)) r))
               (nreverse r))))
      (let* ((n 10)
             (array (si:make-chunk 'int[] (* n 4))))
        (dotimes (i n)
          (si:pack-int32 array (* i 4) (- n i)))
        (values (unpack-int32-array array n)
                (progn
                  (qsort array n 4 #'int32-comparator)
                  (unpack-int32-array array n)))))
    => (10 9 8 7 6 5 4 3 2 1)
    => (1 2 3 4 5 6 7 8 9 10)
    ```

  * Win32 API 呼び出し時の `GetLastError` を取得する関数を追加しました (x022235, #50)

    FFI 呼び出し後に GetLastError の結果を自動的に保存するようにしました。
    次の FFI 呼び出しをするまで、保存されたエラーコードを `c:last-win32-error` で
    取得できます。
    また、`c:last-win32-error` に `setf` することでエラーコードを設定できます。

    ```lisp
    (c:define-dll-entry
      c:int
      MultiByteToWideChar (c:u_int         ; code page
                           c:u_long        ; character-type options
                           (c:void *)      ; string to map
                           c:int           ; number of bytes in string
                           (c:void *)      ; wide-character buffer
                           c:int)          ; size of buffer
      "kernel32")
    => MultiByteToWideChar

    (let ((a (si:make-string-chunk "abc"))
          (u (si:make-chunk nil 9)))
      (si:clear-chunk u)
         (setf (c:last-win32-error) 0)
         (MultiByteToWideChar 932932 0
                              a (si:chunk-size a)
                              u (si:chunk-size u))
         (c:last-win32-error))
    => 87 ; ERROR_INVALID_PARAMETER
    ```


xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * `si:binhex-decode` でクラッシュしていた問題を修正しました (x022235, #45)

  * 循環リストを `copy-tree` するとクラッシュしていた問題を修正しました (x022235, #191)

    ```lisp
    (copy-tree '#1=(#1#))
    => #1=(#1#)

    (copy-tree '(#1=(1 2 3 #1#) #1#))
    => (#1=(1 2 3 #1#) #2=(1 2 3 #2#))

    (copy-tree '(#1=(1 2 3) #1#))
    => ((1 2 3) (1 2 3))
    ```

  * 循環リストを `equal` や `equalp` で比較するとクラッシュしていた問題を修正 (x022235, #255)

    ```lisp
    (equal '#1=(#1# 1 2 3) '(#1# 1 2 3))
    => t

    (equal '#1=(#1# 1 2 3) '(#1# 1 2))
    => nil
    ```

  * スペシャル変数をレキシカルに束縛すると `si:closure-variable` で値が取れない問題を修正しました (x022235, #274)

    ```lisp
    (defvar *hoge* 123)
    => *hoge*

    (let ((*hoge* 234)
          (fuga 345))
      (si:closure-variable #'(lambda ())))
    => ((fuga . 345) (*hoge* . 234)) ; 0.2.2.238 では => ((fuga . 345) *hoge*)
    ```

  * 関数定義を上書きしても docstring が削除されない問題を修正しました (x022235, #263)

    ```lisp
    (defun foo ()
      "foo doc"
      1)
    => foo

    (documentation 'foo 'function)
    => "foo doc"

    (defun foo ()
      1)
    => foo

    (documentation 'foo 'function)
    => nil			; 0.2.2.238 では => "foo doc"
    ```

  * 一部のリーダマクロが `eval-last-sexp` で評価できない問題を修正しました (x022235, #262)

    ```lisp
    #5(1 2 3)
    => #(1 2 3 3 3)		; 0.2.2.238 では => 不正な関数です: 1

    (setf fso (ole-create-object "Scripting.FileSystemObject"))
    => #<oledata 85135436>

    #{fso.BuildPath["foo" "bar"]}
    => "foo\\bar"		; 0.2.2.238 では => 変数が定義されていません: ]}
    ```

    注意: lpp をインストールしていると `eval-region`, `eval-print-last-sexp` が
    上書きされるので注意してください。

  * `M-x lisp-interaction-mode`、`M-x lisp-mode` で `*buffer-package*` が消えないようにしました (x022235, #261)

    lisp ソースファイルを開いたあとに一時的に `lisp-interaction-mode` にした場合に
    `*buffer-package*` の設定が消えてしまいインデントなどが正しく動作しなくなっていた問題を修正しました。

  * docstring と declare を同時に使うとエラーになる問題を修正しました (x022235, #259)

    ```lisp
    (defun foo ()
      (+ x y))
    => foo

    (defun bar (x y)
      "foo doc"
      (declare (special x y))
      (foo))
    => bar

    (bar 1 2)
    => 3			; 0.2.2.238 では => 関数が定義されていません: declare
    ```

  * `*read-eval*` のリファレンスのタイポを修正しました (southly, #253)

  * `refresh-screen` が多値を返す問題を修正しました (x022235, #219)

  * builtin.l に抜けている関数を追加しました (x022235, #285)

  * debug 版でダンプファイルがある状態で起動するとassert fail する問題を修正しました (mumurik, #266)

  * debug 版でダンプ作成時に assert fail する問題を修正しました (mumurik, #265)


Common Lisp との互換性向上
--------------------------

  * `common-lisp` および `common-lisp-user` パッケージを追加しました (x022235, #88)

    注意: とりあえず追加しただけでまだ実用できるレベルではありません。

  * format ~n@A 書式のバグを修正しました (x022235, #246)

    このバグの修正は影響範囲が大きいので `lisp:format` の挙動は修正されません。
    代わりに `common-lisp:format` を利用してください。

    ```lisp
    (format nil "~30@A" "foo")
    => "foo                           "

    (common-lisp:format nil "~30@A" "foo")
    => "                           foo"
    ```

  * `*readtable*` を元の値で束縛してから load するようにしました (x022235, #64)

    load 中に `*readtable*` を変更したとしても他のファイルの load には影響を与えません。

    ただし、`*readtable*` が参照しているリードテーブル自体はコピーされないので、
    独自のリーダーマクロを利用したい場合は以下のように `copy-readtable` してから
    `*readtable*` の値を変更してください。

    ```lisp
    (defun enable-hogehoge-syntax ()
      (setq *readtable* (copy-readtable))
      (set-dispatch-macro-character #\# #\? #'hogehoge-reader))
    ```

注意事項
--------

  * NetInstaller で入手可能な以下のパッケージは 0.2.2.239 では
    本体に同梱しています。インストールすると古いファイルで上書きされるので
    インストールしないようにしてください。

    ```
    keyword file                       2007.12.25     2007/12/25 01:27
    reference.xml                      2007.12.25     2007/12/25 01:23
    ```

  * NetInstaller で入手可能な以下のパッケージの一部の機能は 0.2.2.239 では
    本体に同梱しています。インストールするときは注意してください。

    ```
    lpp                                2008.05.05     2008/05/05 09:16
    ```


既知の問題
----------

  * (setf (symbol-function)) がシンボル名を返す (#269)
  * ASCII 以外のサイズはお任せ時に日本語が縦長で表示される (#241)
  * software-type, software-version が CL と異なる (#169)
  * :typeがlistかvectorで:namedじゃない構造体でtypepがおかしい (#138)
  * ローカル関数で (setf READER) (#137)
  * dualウィンドウモードでfilerのディレクトリ指定が動かない (#130)
  * c-modeでマクロの継続行のインデントがおかしい (#127)
  * クリップボードにコピーするとxyzzyが固まる場合がある (#113)
  * C-u 999 C-g 後にメニュー操作でエラー (#111)
  * Vista 以降で再変換 (C-c C-c) が動作しない (#101)
  * ole で responseBody, responseStream を取得できない (#68)
  * ole-for-each で ie.Document.all の IEnum を取得できない (#67)
  * ole-create-event-sink に TypeLib のファイル名を明示的に指定しないとエラーになる (#66)
  * 巨大な文字列に対する正規表現マッチがすごい遅い (#65)
  * setf の最適化に bug (#63)
  * handler-case で :no-error を指定してコンパイルするとエラー (#62)
  * labels の lambda-list 内の init-form で同じ labels 式で定義したローカル関数を呼び出してると、コンパイルで挙動が変わる (#61)
  * multiframe: 画面端の折り返しがウィンドウ単位でちゃんと動くようにする変更を取り込む (#25)

`(provide "xyzzy-0.2.2.239")`


  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
