xyzzy リリースノート
====================

  * バージョン: 0.2.2.244
  * リリース日: 2012-09-29
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
  2. 0.2.2.244 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * 補完候補の表示順を指定できるようにしました (x022235, #354)

    `*print-completion-sort-function*` で比較関数を指定できます。

    既に自然順でソートされた補完候補を渡す場合に、勝手に辞書順でソートしなおされると
    困る場合などに `nil` を指定します。

    ```lisp
    (let ((*print-completion-sort-function* nil))
      (completing-read "No: " '("2:ばなな" "4:りんご" "30:すもも" "100:みかん")))
    ```

    ※[complete+] を利用している場合はこの変数は無視されます (`print-completion+-list` を書き換える必要があります)。

  * ファイル保存時に一時ファイルを作成しないオプションを追加しました (x022235, #358)

    `file-precious-flag` を `nil` に設定することで一時ファイルを作成しないようになります。

    仕様は Emacs の `file-precious-flag` とあわせてありますが、Emacs とは異なりデフォルト値は t です。

    > Variable: file-precious-flag

    > この変数がnil以外ならば、 save-bufferは保存処理中の入出力エラーに備えて対処する。 つまり、目的の名前のファイルにではなく一時的な名前の新規ファイルに書き出し、 エラーがないことを確認してから目的の名前に改名する。 これにより、不正なファイルに起因する問題からディスク容量の不足といった 問題を回避できる。

    > 副作用として、バックアップも必然的にコピーして行う。 see section 25.1.2 改名によるバックアップかコピーによるバックアップか。 それと同時に、大事な（precious）ファイルとして保存すると、 読者が保存したファイルと別のファイル名とのあいだの ハードリンクをつねに切ってしまう。

    > 特定のバッファではこの変数にnil以外のバッファローカルな値を 指定するモードもある。

    [GNU Emacs Lispリファレンスマニュアル: Saving Buffers](http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_372.html#SEC373)

xyzzy Lisp 開発者向け機能追加
-----------------------------

  * なし

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
  * ole で responseBody, responseStream を取得できない (#68)
  * ole-for-each で ie.Document.all の IEnum を取得できない (#67)
  * ole-create-event-sink に TypeLib のファイル名を明示的に指定しないとエラーになる (#66)
  * 巨大な文字列に対する正規表現マッチがすごい遅い (#65)
  * setf の最適化に bug (#63)
  * handler-case で :no-error を指定してコンパイルするとエラー (#62)
  * labels の lambda-list 内の init-form で同じ labels 式で定義したローカル関数を呼び出してると、コンパイルで挙動が変わる (#61)
  * multiframe: 画面端の折り返しがウィンドウ単位でちゃんと動くようにする変更を取り込む (#25)

`(provide "xyzzy-0.2.2.243")`

  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
  [complete+]: http://white.s151.xrea.com/wiki/index.php?script%2Fcomplete%2B
