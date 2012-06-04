xyzzy README
============

概要
----

xyzzy は Windows XP 以降で動作する日本語テキストエディタのようなものです。
以下のような特徴があります。

  * テキストファイルを読んだり書いたりできる
  * 文字を入力したり削除したりできる
  * 回数制限のある Undo/Redo
  * Windows の標準を無視したユーザインターフェース
  * Visual Basic アンライクな拡張言語
  * 物覚えが悪いと使えないファイラを装備(かといって物覚えがよければ使えるってわけでもない)
  * ややインテリマウス対応(かも)
  * その他、作者が使ったこともない機能の数々


インストール
------------

アーカイブをディレクトリつきで展開すればできあがりです。
インストーラなどという気の利いたものはないので、ファイルの関連付けなどをしたい場合は自分でやってください。

ショートカットは「ツール」→「ショートカットの作成...」でできます。


アンインストール
----------------

当然のことながらアンインストーラはないので、以下のものを手で削除してください。

  * アーカイブを展開したディレクトリを丸ごと
  * ショートカットやファイルの関連付けなど


操作方法など
------------

普通のテキストエディタとして使う分には、これといって特殊な操作は必要ありません。


コマンドラインオプション
------------------------

起動時の引数は以下の形式を受け付けます。

<b>xyzzy</b>
  [<b>-image</b> `dump-file`]
  [<b>-config</b> `config-directory`]
  [<b>-ini</b> `ini-file`]
  [<b>-q</b>|<b>-no-init-file</b>]
  \(`other-option`|`file`)`*`

<b>-image</b>、<b>-config</b> または <b>-ini</b >を指定する場合は、それ以外のオプションより前になければなりません。
また、 <b>-q</b> または <b>-no-init-file</b> を指定する場合は <b>-image</b>、<b>-config</b> および <b>-ini</b> 以外の
オプションの先頭になければなりません。

それぞれのオプションの詳細は以下の通りです。

  * <b>-image</b> `dump-file`

    ダンプイメージのファイル名を `dump-file` に変更します。
    指定されない場合、実行ファイルの拡張子を .wxp に変更したファイル名を使用します。

  * <b>-config</b> `config-directory`

    自動的に作成される設定ファイルを置くディレクトリを `config-directory` に変更します。
    指定されない場合、$XYZZY/usr/`username`/wxp/に置かれます。
    環境変数 `XYZZYCONFIGPATH` で指定することも可能です。

  * <b>-ini</b> `ini-file`

    自動的に作成される設定ファイルのファイル名を `ini-file`に変更します。
    指定されない場合は xyzzy.ini となります。

  * <b>-q</b>
  * <b>-no-init-file</b>

    ユーザ初期化ファイルをロードしません。

  * <b>-l</b> `file`
  * <b>-load</b> `file`

    `file` を <b>load</b> します。

  * <b>-I</b> `dir`
  * <b>-load-path</b> `dir`

    `dir` を <b>*load-path*</b> の先頭に追加します。

  * <b>-R</b> `module`
  * <b>-require</b> `module`

    `module` を <b>require</b> します。

  * <b>-work-dir</b> `dir`

    xyzzy の作業ディレクトリを `dir` に変更します。

  * <b>-f</b> `fn`
  * <b>-funcall</b> `fn`

    `fn` を <b>funcall</b> します。

  * <b>-e</b> `sexp`
  * <b>-eval</b> `sexp`

    `sexp` を評価します。

  * <b>-g</b> `linenum`
  * <b>-go</b> `linenum`

    直前に指定したファイルの `linenum` 行目に移動します。

  * <b>-c</b> `column`
  * <b>-column</b> `column`

    直前に指定したファイルの `column` 桁に移動します。

  * <b>-trace</b>

    <b>*Trace Output*</b> バッファを有効にし、エラー発生時にスタックトレースが
    出力されるようにます。

  * <b>-kill</b>

    xyzzy を終了します。これ以降の引数は無視されます。

  * <b>-p</b> `filename`

    `filename`を印刷して終了します。

  * <b>-s</b> `session-file`

    `session-file` をセッションファイルとして読み込み、セッションの自動保存を有効にします。

  * <b>-S</b> `session-file`

    `session-file` をセッションファイルとして読み込み、セッションの自動保存を無効にします。

  * <b>-m</b> `mode`
  * <b>-mode</b> `mode`

    以降に指定された `file` のモードを `mode` にします。

  * <b>-ro</b>

    以降に指定された `file` を書込み禁止モードで読み込みます。

  * <b>-rw</b>

    以降に指定された `file` を書込み可能モードで読み込みます (<b>-ro</b> を取り消します)。

  * <b>-mailto</b> `mailto`

    `mailto` を引数にして `*command-line-mailto-hook*` を呼びます。

  * `file`

    `file`を読み込みます。


xyzzycli について
----------------

xyzzycli は、すでに動作している xyzzy にファイルを読ませたり読ませなかったりということができます。
xyzzy が動作していない場合は勝手に起動します。
起動時の引数は以下の形式を受け付けます。

<b>xyzzycli</b>
  [<b>-image</b> `dump-file`]
  [<b>-config</b> `config-directory`]
  [<b>-ini</b> `ini-file`]
  [<b>-q</b>|<b>-no-init-file</b>]
  [<b>-wait</b>]
  \(`other-option`|`file`)`*`

<b>-image</b>、<b>-config</b>、<b>-ini</b>、<b>-q</b> および <b>-no-init-file</b> を除くコマンドラインの先頭に
<b>-wait</b> を指定すると、以降に指定したファイルのバッファが削除されるのを待ちます。

その他のコマンドラインオプションは、xyzzy のオプションと同じものを指定することができます。
xyzzy がすでに動作している場合、<b>-image</b>、<b>-config</b>、<b>-ini</b>、<b>-q</b> および <b>-no-init-file</b> は無視されます。

なお、コマンドプロンプト上で <b>-wait</b> を指定する場合、`start /wait xyzzycli -wait ...` のように
起動する必要があります。


使用条件など
------------

[LICENSE](https://github.com/xyzzy-022/xyzzy/blob/develop/LICENSE) を参照してください。
