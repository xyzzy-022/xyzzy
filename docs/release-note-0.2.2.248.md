xyzzy リリースノート
====================

  * バージョン: 0.2.2.248
  * リリース日: 2013-03-29
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
  2. 0.2.2.248 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * なし


バグ修正
--------

  * Windows XP ではタイトルバーに「管理者：」を表示しないようにしました。 (x022235, #378)


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * SSL クライアントをサポートしました。(x022235, #53)

    `connect` 関数の `:ssl` キーワード引数に `non-nil` を指定すると SSL ストリームを作成します。
    また、`:ssl-verify-mode` キーワード引数で証明書の検証モードを指定可能できます (`:none` と `:peer`)。

    Proxy 越しの HTTPS や FTPS など、通信の途中から SSL 通信を行いたい場合には
    `ssl-do-handshake` を利用してください。

    SSL クライアントのみサポートしています。
    サーバソケット (`make-listen-socket`) では SSL はサポートしていません。

    例:

        ;;; https://www.google.co.jp/ を取得してみる。
        (with-open-stream (stream (connect "www.google.co.jp" "https" :ssl t))
          (format stream "GET / HTTP/1.1\n")
          (format stream "Host: www.google.co.jp\n\n")
          (let (line)
            (while (setq line (read-line stream nil))
              (insert line "\n"))))

        ;;; Proxy 経由で https://www.google.co.jp/ を取得してみる。
        ;;; 最初は Proxy サーバと普通に通信するので ssl-do-handshake を利用。
        (let ((proxy-host "proxy.example.com")
              (proxy-port 8080)
              (host "www.google.co.jp")
              (port 443))
          (with-open-stream (stream (connect proxy-host proxy-port))
            (format stream "CONNECT ~A:~A HTTP/1.0~%~%" host port)
            (let (line)
              (setq line (read-line stream))
              (unless (string-match "^HTTP/[0-9]\.[0-9] 200" line)
                (plain-error "CONNECT failed: ~S" line))
              (ssl-do-handshake stream host)
              (format stream "GET / HTTP/1.0~%")
              (format stream "Host: ~A~%~%" host)
              (while (setq line (read-line stream nil))
                (insert line "\n")))))


xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * なし

既知の問題
----------

  * <https://github.com/xyzzy-022/xyzzy/issues?labels=bug&state=open>


`(provide "xyzzy-0.2.2.248")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
