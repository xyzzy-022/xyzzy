xyzzy リリースノート
====================

  * バージョン: 0.2.2.246
  * リリース日: 2012-12-29
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
  2. 0.2.2.246 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * 管理者権限でコマンドプロンプトを起動できるようにしました (x022235, #330)

    Ctrl+Shift を押しながら [ツール]-[NT プロンプト] を実行すると
    管理者権限でコマンドプロンプトを開きます。

    また、`M-x run-admin-console` でも実行できます。

  * ファイラでシステムファイルと隠しファイルを非表示に設定できるようにしました (akawaguc, x022235, #368)

    共通設定または `*filer-show-hidden-files*` と `*filer-show-system-files*` で設定できます。

    ![filer-setting]

バグ修正
--------

  * 0.2.2.244 以降 [ac-mode] のポップアップが逆順に表示される問題を修正しました (x022235, #367)

xyzzy Lisp 開発者向け機能追加
-----------------------------

  * デバッガへの出力 API を追加しました (x022235, #366)

    以下の関数および変数を追加しました。

    * `si:output-debug-string FMT &rest ARGS`
    * `*debug-output*` ストリーム
    * `debug-output-stream-p`

    出力したデバッグメッセージは [DebugView] で確認できます。

    ![debug-output]

  * `oledata` の印字形式に COM の型名を表示するようにしました (x022235, #370)

    ```lisp
    (require "ole")
    (setf ie (ole-create-object "InternetExplorer.Application"))
    => #<oledata: IWebBrowser2 56234988>
    #{ie.Navigate["http://www.google.co.jp/"]}
    #{ie.document}
    => #<oledata: JScriptTypeInfo 56234972>
    #{ie.document.GetElementsByTagName["div"]}
    => #<oledata: DispHTMLElementCollection 56234940>
    #{ie.document.GetElementsByTagName["div"].Item[0]}
    => #<oledata: DispHTMLDivElement 56234876>
    ```

  * OLE メソッドを名前付き引数を指定できるようになりました (x022235, #361)

    以下の関数および変数を追加しました。

    * `ole-method* OBJECT PROPERTY ARGS NAMED-ARGS`

    ```lisp
    (let ((ie (ole-create-object "InternetExplorer.Application")))
      (ole-putprop ie :Visible t)
      (ole-method* ie :Navigate
                   (list "http://www.google.co.jp/")
                   (list :Headers "Referer: http://www.google.co.jp/")))
    ```

    リーダーマクロを利用して以下のように記述することもできます。

    ```lisp
    (require "ole")
    (let ((ie (ole-create-object "InternetExplorer.Application")))
      (setf #{ie.Visible} t)
      #{ie.Navigate["http://www.google.co.jp/" {:Headers "Referer: http://www.google.co.jp/"}]})
    ```

  * `shell-execute` で動詞を指定できるようになりました (x022235, #330)

    ```lisp
    ;; 管理者権限でコマンドプロンプトを起動
    (shell-execute "c:/Windows/System32/cmd.exe" nil nil
                   :verb :runas)
    ```

  * キーの押下状態を取得する関数を追加しました (x022235, #330)

    * `si:get-key-state VKEY`
    * `si:control-pressed`
    * `si:meta-pressed`
    * `si:shift-pressed`

    キーの押下状態及びトグル状態を多値で返します。

  * 特定のパスからファイルを探す関数を追加しました (x022235, #330)

    * `si:search-path FILE &optional PATH EXT`

    例:

    ```lisp
    (si:search-path "cmd.exe")
    ;=> "C:/Windows/system32/cmd.exe"

    (si:search-path "bash" nil ".exe")
    ;=> "c:/cygwin/bin/bash.exe"
    ```

xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * `ole-create-event-sink` に TypeLib のファイル名を明示的に指定しないとエラーになる問題を修正しました (x022235, #66)

    ```lisp
    (let ((ie (ole-create-object "InternetExplorer.Application")))
      (ole-create-event-sink ie "DWebBrowserEvents2")
      (set-ole-event-handler ie "DownloadComplete"
                             #'(lambda ()
                                 (msgbox "Download complete!")))
      #{ie.Navigate["http://www.google.co.jp/"]})
    ```

  * `ole-for-each` で ie.document.all の IEnum を取得できない問題を修正しました (x022235, #67)

    ```lisp
    (let ((ie (ole-create-object "InternetExplorer.Application")))
      (setf #{ie.Visible} t)
      #{ie.Navigate["http://www.google.co.jp/"]}
      (while (/= #{ie.ReadyState} 4)
        (sit-for 0.1))
      (ole-for-each (div #{ie.document.GetElementsByTagName["div"]})
        (princ #{div.InnerText}))
      #{ie.Quit})
    ```

  * 2^30 個の配列を作ろうとするとクラッシュ問題を修正しました (x022235, #356)

その他
------

  * OLE 関連のリファレンスを追加しました (x022235, #369)

既知の問題
----------

  * <https://github.com/xyzzy-022/xyzzy/issues?labels=bug&state=open>


`(provide "xyzzy-0.2.2.246")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
  [ac-mode]: http://white.s151.xrea.com/wiki/index.php?script%2Fac-mode
  [filer-setting]: https://f.cloud.github.com/assets/1522408/34233/2f998aac-5107-11e2-8452-409b0467a0b2.png
  [debug-output]: https://f.cloud.github.com/assets/1522408/34249/4fe0a358-5108-11e2-9b8d-aee4e49f290e.png
  [DebugView]: http://technet.microsoft.com/ja-jp/sysinternals/bb896647.aspx
