xyzzy リリースノート
====================

  * バージョン: 0.2.2.250
  * リリース日: 2013-05-29
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
  2. 0.2.2.250 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * フレームバーにコンテキストメニューを追加しました (x022235, #389)

    デフォルトでは閉じるメニューのみあります。
    以下の変数を利用することでバッファバーのようにメニューを拡張できます。

    * `*pseudo-frame-bar-context-menu*`
    * `*pseudo-frame-bar-context-menu-frame*`
    * `*pseudo-frame-bar-context-menu-handler*`

  * 削除バッファとクリップボードを完全に同期するオプションを追加しました (x022235, #161)

    ツール→共通設定→さまざまで設定出来ます。

    ![クリップボードの同期設定画面](https://f.cloud.github.com/assets/1522408/572539/8abdd8b8-c79d-11e2-9ee1-1d119db291a4.png)

    または `*sync-kill-ring-with-clipboard*` 変数で設定します。

    * `t` なら中途半端に同期
    * `:always` なら完全に同期
    * `nil` なら同期しない

    完全に同期を選ぶとクリップボードにコピーした場合にすぐに削除バッファに追加されます。
    また、削除バッファにコピーするとすぐにクリップボードにも反映されます。


バグ修正
--------

  * Windows 8 でローカル設定の表示色タブが表示されない問題を修正しました (x022235, #392)

    @heavencondition さんが報告してくれました。

  * クリップボードと削除バッファの同期が止まる場合があったのを修正しました (x022235, #390)

    行儀の悪いアプリケーションやアプリケーションが強制終了された場合にクリップボードの同期が
    止まってしまう場合があったのを修正しました。


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * クリップボードが変更された時に実行するフックを追加しました (x022235, #161)

    `*sync-kill-ring-with-clipboard*` 変数の値にかかわらずクリップボードにコピーされた時点で
    `*change-clipboard-hook*` フックは実行されます。
    フックの引数はありません。

    ```lisp
    (add-hook '*change-clipboard-hook*
              #'(lambda ()
                  (si:output-debug-string (get-clipboard-data))))
    ```

既知の問題
----------

  * <https://github.com/xyzzy-022/xyzzy/issues?labels=bug&state=open>


`(provide "xyzzy-0.2.2.250")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
