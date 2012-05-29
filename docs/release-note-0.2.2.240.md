xyzzy リリースノート
====================

  * バージョン: 0.2.2.240
  * リリース日: 2012-05-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

0.2.2.239 で common-lisp パッケージのニックネームと
changelogmemo パッケージのニックネームが衝突していた問題を修正した
hotfix リリースです。

0.2.2.239 のリリースノートは以下を参照してください。

  * <http://xyzzy-022.github.com/xyzzy/2012/05/29/xyzzy-0_2_2_239-release-note/>


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.240 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


バグ修正
--------

  * `common-lisp` パッケージから `cl` というニックネームを削除しました (x022235, #300)


注意事項
--------

  * NetInstaller で入手可能な以下のパッケージは 0.2.2.240 では
    本体に同梱しています。インストールすると古いファイルで上書きされるので
    インストールしないようにしてください。

    ```
    keyword file                       2007.12.25     2007/12/25 01:27
    reference.xml                      2007.12.25     2007/12/25 01:23
    ```

  * NetInstaller で入手可能な以下のパッケージの一部の機能は 0.2.2.240 では
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

`(provide "xyzzy-0.2.2.240")`


  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
