xyzzy リリースノート
====================

  * バージョン: 0.2.2.252
  * リリース日: 2013-11-29
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
  2. 0.2.2.252 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * エアロスナップでファイラのサイズを変更した場合もウィンドウサイズを記憶するようにしました (wkoiking, x022235, #406)

    共通設定→ファイラで設定します（メインのウィンドウとは別設定）。

    ![aerosnap-filer](https://f.cloud.github.com/assets/1522408/1645488/ae1fd1f8-5901-11e3-8b2f-42d2fe3f74fd.png)


既知の問題
----------

  * <https://github.com/xyzzy-022/xyzzy/issues?labels=bug&state=open>


`(provide "xyzzy-0.2.2.252")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
