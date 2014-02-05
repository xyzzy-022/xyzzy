# xyzzy

xyzzy は[亀井哲弥氏](http://www.jsdlab.co.jp/~kamei/) が開発した、Common Lisp っぽい言語で拡張可能な
Emacs っぽいテキストエディタのようなものです。
2ch とか Twitter とか五目並べができたり、テキストファイルの読み書きができます。

現在は亀井氏に変わり有志によって開発が継続しています。


----

## xyzzy 0.2.2 系列とは

xyzzy 0.2.2 系列は以下の観点を重視したバージョンです。

  * xyzzy 0.2.2.235 との後方互換性
    * xyzzy は過去に作られた資産のうちメンテされていないものが結構ありますが、
      これらを修正せずに動作できることが絶対条件です
    * そのためにはたとえバグでも直さないことがあります
  * Common Lisp との互換性を重視
    * ただし、0.2.2.235 との互換性が保たれる場合のみ
    * CL と動作が違う部分で過去の資産に影響を与えなさそうなものは修正する
    * CL と動作が違う部分で過去の資産に影響を与えるものは、
      common-lisp パッケージまたは common-lisp-user パッケージで定義する
  * 安定性
    * 十分テストしてから粛々とリリースする

以下の観点については特に重視しません。

  * 前方互換性
    * 新規 API に依存したアプリが 0.2.2.235 で動かないのは当たりまえ
    * 前方互換性を気にしていると前に進めない
  * multiframe 版や unicode 版との互換性
    * そもそも上記派生バージョンは 0.2.2.235 と互換性がないから
    * multiframe 版や unicode 版のうち互換性に影響を与えない修正は取り込む

他の派生版との違いは以下のとおりです。

  * multiframe 版と比べて、一つの xyzzy で画面をいっぱい開いたり出来ません。
    その代わり 0.2.2.235 と互換性があります。
  * unicode 版と比べて、ユニコードへの対応が中途半端です。
    その代わり 0.2.2.235 と互換性があります。

用語:

  * 後方互換性 = 0.2.2.235 以前に書かれたものが 0.2.2.236 以降でも動作すること
  * 前方互換性 = 0.2.2.236 以降に書かれたものが 0.2.2.235 でも動作すること


----

## 開発

https://github.com/xyzzy はすでにアカウントが取られていたので https://github.com/xyzzy-022
という GitHub Organization で行います。

開発したい人は Organization に追加するので、とりあえず Pull Request を送ってください。

  * ブランチモデルは [A successful Git branching model] に従います
    * topic ブランチ、develop ブランチ、release ブランチ、master ブランチ
    * topic ブランチで開発して develop ブランチの `merge --no-ff`
    * Fast Forward な merge だと問題があった時に revert するのが面倒なので、かならず `--no-ff` でマージコミットを生成する
    * master ブランチは常にリリース可能な状態に置く
  * チケット・ドリブン
    * とりあえず気づいたことは GitHub Issues に登録しておく
    * いきなり Pull Request でもよい
    * すでにあるチケットを実装したなら Pull Request 時に元のチケットの番号を記載する
  * チケットの仕方
    * てきとーに
  * ChangeLog は書かない
    * merge 時にコンフリクトしまくってめんどくさい
    * チケットに書けばいい
  * コミットログの書き方
    * Git 流 (メールみたいなやつ) に従うが日本語で良い
    * エンコーディングは UTF-8 で
  * コーディングルール
    * 基本的には元々の亀井さんのコードに合わせます
      * misc/mode-settings.l を読み込んでおいてください
    * ただし、以下の点は変えます
      * ヘッダファイルのプロトタイプ宣言で引数名はちゃんと書く
      * C++ スタイルのキャストを利用 (static_cast, const_cast, reinterpret_cast, dynamic_cast)
        * <http://msdn.microsoft.com/ja-jp/library/cc440191(v=vs.71).aspx>
        * <http://msdn.microsoft.com/ja-jp/library/cc440192(v=vs.71).aspx>
      * XML ドキュメントを記述
        * <http://msdn.microsoft.com/ja-jp/library/ms177227.aspx>
  * push または Pull Request する前に以下を確認
    * `build.bat` と `build.bat Debug` が警告なしでビルドできること
    * `run-tests-all.bat` がパスすること
  * .gitconfig に以下を設定しておく

    ```ini
    [i18n]
            commitencoding = utf-8
            logoutputencoding = utf-8
    [gui]
            encoding = utf-8
    ```

  [A successful Git branching model]: http://keijinsonyaban.blogspot.com/2010/10/successful-git-branching-model.html

----

## リリース

  * 毎月、肉の日リリース
  * バージョン番号は 0.2.2.x の x 部分をリリースのたびにインクリメント
    * 修正の大小にかかわらず常に 1 づつ増やしていく
    * 0.2.2.65535 まで使える


----

## サポート OS

  * Windows XP SP3 以降をサポート


----

## ビルド方法

### 環境

  * Visual Studio Express 2013 for Windows Desktop
    * <http://www.microsoft.com/ja-jp/download/details.aspx?id=40787>
  * msysGit
    * <http://code.google.com/p/msysgit/>
  * 7zip
    * <http://www.7-zip.org/>
    * リリース作業時のみ
    * 7za.exe をパスの通ったところに置く

### 手順

 1. build.bat
    * デバッグ版は build.bat Debug
 2. bytecompile.bat
 3. ぽけーと待つ
 4. できあがり
 5. run-tests.bat でユニットテストを実行

----

## ライセンス

MIT ライセンスです。
詳細は LICENSE ファイルを参照のこと。
