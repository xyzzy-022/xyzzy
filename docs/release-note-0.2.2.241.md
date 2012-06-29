xyzzy リリースノート
====================

  * バージョン: 0.2.2.241
  * リリース日: 2012-06-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

xyzzy 0.2.2.241 では主にバグ修正と Common Lisp との互換性の向上を行なっています。


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.241 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * `coding` マジックコメントをサポートしました (x022235, #288)

    ファイルの先頭に `-*- coding: utf-8 -*-` などと書いてある場合に
    指定されたエンコーディングでファイルを開くようにしました。

  * マジックコメントが utf-8 の場合に utf-8n で開く (x022235, #307)

    ファイルの先頭に `-*- coding: utf-8 -*-` や `-*- encoding: utf-8 -*-` などと書いてある場合に
    utf-8n (BOM なしの UTF-8) としてファイルを開くようにしました。

    ファイルの実際のエンコーディングが utf-8 (BOM 付の UTF-8) だった場合は
    utf-8 として開きます。

    なお、従来の動作 (常に utf-8 として開く) に戻したい場合は以下のように設定してください。

    ```lisp
    ;; encoding: utf-8 の場合に BOM 付き UTF-8 で開く (0.2.2.240 までの動作に戻す)
    (setf *find-file-auto-encoding-use-utf8n* nil)
    ```

  * zlib 1.2.7 へ更新しました (x022235, #296)

  * DEP を有効にしました (x022235, #175)


バグ修正
--------

  * 0.2.2.239 で `-ro`, `-rw` オプションが使えなくなっていた問題を修正しました (x022235, sabimaru, #306)

  * Windows 7 でウィンドウの復元位置がずれる問題を修正しました (x022235, #328)


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * 文字エンコードの情報を取得する関数を追加しました (x022235, #307)

    * `char-encoding-type`
    * `char-encoding-signature`

    ```lisp
    (char-encoding-type *encoding-utf8*)
    => :utf8

    (char-encoding-signature *encoding-utf8*)
    => t

    (char-encoding-type *encoding-utf8n*)
    => :utf8

    (char-encoding-signature *encoding-utf8n*)
    => nil
    ```

  * FFI: 可変長引数に対応しました (x022235, #294)

    `c:define-dll-entry` の引数宣言の最後に `&rest` を追加し、
    呼び出し時に `(c:c-vaargs (型1 引数1) (型2 引数2) ...)` という形式で
    可変長引数を指定してください。

    ```lisp
    (require "foreign")

    (c:define-dll-entry
      c:string
      (sqlite3_mprintf :convention :cdecl) (c:string &rest) ; 可変長引数を取る場合 &rest を指定
      "sqlite3.dll")

    (c:define-dll-entry
      c:void
      (sqlite3_free :convention :cdecl) ((c:void *))
      "sqlite3.dll")

    (defun make-sql (text date)
      (let ((ptr (sqlite3_mprintf (si:make-string-chunk "INSERT INTO table VALUES ('%q', %d)")
                                  (c:c-vaargs ; 可変長引数の指定
                                   (c:string (si:make-string-chunk text))
                                   (c:int date)))))
        (when (not (zerop ptr))
          (unwind-protect
              (si:unpack-string nil ptr)
            (sqlite3_free ptr)))))

    (make-sql "It's a happy day!" 31)
    => "INSERT INTO table VALUES ('It''s a happy day!', 31)"
    ```

  * FFI: `c:c-size-of` を追加しました (x022235, #313)

    ```lisp
    (require "foreign")
    (require "wip/winapi")

    (c:c-size-of c:int)
    => 4

    (c:c-size-of c:int64)
    => 8

    (c:c-struct-size-of winapi:RECT)
    => 16
    ```

  * `si:unpack-string` で C のポインタを指定可能としました (x022235, #311)

    `si:make-chunk` と同じように `CHUNK` 引数が `nil` の場合は、
    `OFFSET` をポインタとみなすようにしました。

    ```lisp
    (require "foreign")

    (c:define-dll-entry
      c:string
      strerror (c:int)
      "msvcrt")

    (strerror 2)
    => 41155912

    (si:unpack-string nil (strerror 2))
    => "No such file or directory"
    ```

  * `locally` 特殊形式を追加しました (x022235, #271)

    [Common Lisp の locally] を追加しました。

    ```lisp
    (defun sample-function (y)  ;this y is regarded as special
      (declare (special y))
      (let ((y t))              ;this y is regarded as lexical
        (list y
              (locally (declare (special y))
                ;; this next y is regarded as special
                y))))
    => sample-function

    (sample-function nil)
    => (t nil)
    ```

  * `reduce` に `:key` パラメータを追加しました (x022235, youz, #322)

    ```lisp
    (reduce #'+ '((orange . 3) (apple . 5) (grape . 2))
            :key #'cdr)
    => 10
    ```

  * `macro-function` に `environment` 引数を追加しました (youz, #326)

    ```lisp
    (macrolet ((foo (a b)
                 `(+ ,a ,b))
               (env (&environment env)
                 env))
      (setf env (env)))
    => #<environment-object 134287448>

    (macro-function 'foo)
    => nil

    (macro-function 'foo env)
    => (macro (a b) (block foo (list '+ a b)))
    ```

xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * `rename-package` で自分自身のニックネームを変更できない問題を修正しました (x022235, snmsts, #302)

    ```lisp
    (rename-package :common-lisp :common-lisp '(:cl))
    => #<package: common-lisp>  ; 0.2.2.240 では => 同じ名前のパッケージが存在します: "common-lisp"

    (package-nicknames :cl)
    => ("cl")
    ```

  * `:key` に `nil` を指定するとエラーになる関数があったのを修正しました (x022235, #324)

    ```lisp
    (substitute 2 1 '(1 2 3 2 1) :key nil)
    => (2 2 3 2 2)              ; 0.2.2.240 では => 関数が定義されていません: nil
    ```

  * `fill-pointer` を持つ `vector` を `:initial-contents` で正しく初期化出来ない問題を修正しました (x022235, youz, #325)

    ```lisp
    (setf v (make-array 3 :fill-pointer 2 :initial-contents '(1 2 3)))
    => #(1 2)

    (setf (fill-pointer v) 3)
    => 3

    v
    => #(1 2 3)                 ; 0.2.2.240 では => #(1 2 nil)
    ```

  * `int64` の範囲を超える場合、`si:unpack-int64` の結果がおかしい問題を修正しました (x022235, #305)

  * builtin パッケージを削除すると `si:list-builtin-packages` が `#<package anonymous>`
    を返す問題を修正しました (x022235, #303)


その他
------

  * Windows SDK 7.1 がなくてもビルドできるようになりました (x022235, #314)


注意事項
--------

  * NetInstaller で入手可能な以下のパッケージは 0.2.2.241 では
    本体に同梱しています。インストールすると古いファイルで上書きされるので
    インストールしないようにしてください。

    ```
    keyword file                       2007.12.25     2007/12/25 01:27
    reference.xml                      2007.12.25     2007/12/25 01:23
    ```

  * NetInstaller で入手可能な以下のパッケージの一部の機能は 0.2.2.241 では
    本体に同梱しています。インストールするときは注意してください。

    ```
    lpp                                2008.05.05     2008/05/05 09:16
    ```


既知の問題
----------

  * マクロ・特殊形式の引数間違い時のエラーメッセージが意味不明 (#329)
  * ミニバッファで補完時に落ちる場合がある (#321)
  * macro-function が CL と異なる (#320)
  * multiple-value-bind、multiple-value-setq の macro-function が nil (#319)
  * special-form-p が特殊形式以外にも t を返す (#318)
  * call-process :wait t でコマンドが固まると xyzzy も固まる (#316)
  * make-process の :show パラメータが動作しない (#315)
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


`(provide "xyzzy-0.2.2.241")`


  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
  [Common Lisp の locally]: http://clhs.lisp.se/Body/s_locall.htm
