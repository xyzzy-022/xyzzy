xyzzy リリースノート
====================

  * バージョン: 0.2.2.243
  * リリース日: 2012-08-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

xyzzy 0.2.2.243 ではファイラのソート方法の改善などを行なっています。


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.243 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * ファイラで数値を考慮したソートをエクスプローラと合わせました (x022235, #347)

    xyzzy 0.2.2.242 までは以下のようなソート結果になっていましたが、

    ```
    a3.txt
    a02.txt
    a10.txt
    a001.txt
    ```

    これをエクスプローラとあわせて以下のようになるようにしました。

    ```
    a001.txt
    a02.txt
    a3.txt
    a10.txt
    ```


バグ修正
--------

  * なし


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * 環境変数の一覧を取得できるようにしました (x022235, #344)

    ```lisp
    (si:environ)
    (("ALLUSERSPROFILE" . "C:\\ProgramData")
     ("APPDATA" . "C:\\Users\\xyzzy\\AppData\\Roaming")
     ("CommonProgramFiles" . "C:\\Program Files\\Common Files")
     ...)
    ```

  * `buffer-substring` に optional な buffer 引数を追加しました (x022235, #345)

  * `si:getenv` を `setf` 可能にしました (x022235, #346)

  * 以下の型の typespec を追加しました (x022235, #352)

    `check-type` などでチェックできます。

    * `ed:wait-object`
    * `ed:char-encoding`
    * `si:structure-definition`
    * `si:dll-module`
    * `si:c-function`
    * `si:c-callable`
    * `si:environment`

  * `si:*dll-module-p` を追加しました (x022235, #352)


xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * なし


Common Lisp との互換性向上
--------------------------

  * なし


その他
------

  * なし


既知の問題
----------

  * macro-function が CL と異なる (#320)
  * multiple-value-bind、multiple-value-setq の macro-function が nil (#319)
  * special-form-p が特殊形式以外にも t を返す (#318)
  * call-process :wait t でコマンドが固まると xyzzy も固まる (#316)
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

`(provide "xyzzy-0.2.2.243")`
