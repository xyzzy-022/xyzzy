xyzzy リリースノート
====================

  * バージョン: 0.2.2.245
  * リリース日: 2012-10-29
  * ホームページ: <http://xyzzy-022.github.com>


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.245 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * `ole-method` の引数に `:null` を指定すると Null 値 (VT_NULL) を渡すようになりました (youz, #362)


バグ修正
--------

  * `ole-method` で配列型のデータを取得できない問題を修正しました (mumurik, #68)

    ```lisp
    (let ((xhr (ole-create-object "MSXML2.XMLHTTP")))
      (ole-method xhr 'Open "get" "https://github.com/xyzzy-022/xyzzy" nil)
      (ole-method xhr 'Send nil)
      (convert-encoding-to-internal
       *encoding-utf8n*
       (map 'string 'code-char
            (ole-getprop xhr 'responseBody))))
    => "<!DOCTYPE html>..."   ; 0.2.2.244 では => 変数の種類が間違っています。
    ```

  * Windows 8 で共通設定のフォントタブが表示されない問題を修正しました (yasuoohno, x022235, #363)


既知の問題
----------

  * 2^30 個の配列を作ろうとするとクラッシュ (#356)
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
  * ole-for-each で ie.Document.all の IEnum を取得できない (#67)
  * ole-create-event-sink に TypeLib のファイル名を明示的に指定しないとエラーになる (#66)
  * 巨大な文字列に対する正規表現マッチがすごい遅い (#65)
  * setf の最適化に bug (#63)
  * handler-case で :no-error を指定してコンパイルするとエラー (#62)
  * labels の lambda-list 内の init-form で同じ labels 式で定義したローカル関数を呼び出してると、コンパイルで挙動が変わる (#61)
  * multiframe: 画面端の折り返しがウィンドウ単位でちゃんと動くようにする変更を取り込む (#25)

`(provide "xyzzy-0.2.2.245")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
  [complete+]: http://white.s151.xrea.com/wiki/index.php?script%2Fcomplete%2B
