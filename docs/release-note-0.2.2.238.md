xyzzy リリースノート
====================

  * バージョン: 0.2.2.238
  * リリース日: 2012-04-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

xyzzy 0.2.2.238 では主に文字コード自動判別周りの改善、
メジャーモードのインデント処理のバグ修正、
フォントサイズの変更 API の追加を行なっています。


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.238 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * 文字コード判定を改善しました (x022235, #108)

    文字コードの判定に [libguess] を利用するようにしました。
    また、ファイルの先頭から 256K バイトを見て文字コードを判定するようにしました
    (0.2.2.235 では 64K バイト)。

    文字コードの判定方法および判定に利用するバッファサイズは以下の変数で指定できます。

    ```lisp
    ;; 文字コード判定処理のデフォルト設定
    (setq *detect-char-encoding-mode* :libguess)
    (setq *detect-char-encoding-buffer-size* #x40000)
    ```

    IPA 辞書に含まれる 32 万個の単語を利用して自動判別の精度を評価した結果は
    以下のとおりです。

    ```
    encoding   | 0.2.2.238               | 0.2.2.235
    -----------+-------------------------+------------------------
    JIS        | 325871/325871 (100.00%) | 325871/325871 (100.00%)
    Shift_JIS  | 324646/325839 ( 99.63%) | 325839/325839 (100.00%)
    EUC-JP     | 323407/325871 ( 99.24%) | 195228/325871 ( 59.91%)
    UTF-8      | 319157/325871 ( 97.94%) | 149699/325871 ( 45.94%)

    ※正解数/単語数 (正解率)
    ```

    Shift_JIS の判定精度が 0.2.2.235 と比べて若干落ちていますが、
    UTF-8 と EUC-JP の判定精度が大幅に改善していることが分かります。

    ただ、半角カタカナを含む Shift_JIS のファイルの場合 EUC-JP と誤認することが
    多くなっています。

    その場合、以下の設定をすることで 0.2.2.235 と同じ動作に変更できます。

    ```lisp
    ;; 文字コード判定処理を 0.2.2.235 と同じにする
    (setq *detect-char-encoding-mode* :xyzzy)
    (setq *detect-char-encoding-buffer-size* #x10000)
    ```

  * クリップボード・エンコーディングに自動判別を追加しました (x022235, #73)

    デフォルトは Shift_JIS のままなので、自動判別を有効にしたい場合は
    以下の設定を行なってください。

    ```lisp
    ;; クリップボードエンコーディングを自動判別
    (setq *clipboard-char-encoding* *encoding-auto*)
    ```

  * フォントを変更する API を追加しました (x022235, #110)

    * `ed:increase-text-font-size`
    * `ed:decrease-text-font-size`
    * `ed:set-text-fontset`
    * `ed:get-text-fontset`

    以下の設定で Ctrl +, Ctrl - でフォントサイズを変更できます。

    ```lisp
    ;; Ctrl +, - でフォントサイズ変更
    (global-set-key #\C-+ 'increase-text-font-size)
    (global-set-key #\C-- 'decrease-text-font-size)
    ```

    また、表示メニュー → 文字のサイズから変更できます。

    この API は xyzzy 全体のフォントを指定するものです。
    バッファまたはウィンドウごとにフォントを指定する API は今後追加予定です。

  * `grep` 前に実行されるフックを追加しました (x022235, #205)

    * `ed:*before-grep-hook*`
    * `ed:*before-grepd-hook*`

    `grep` または `grep-dialog` 実行時に検索キーワード (string または regex) を
    引数に指定してこのフックを呼び出します。

    例えば以下の設定をすることで `grep` 途中に `Ctrl-g` で停止しても
    [grep-mode] が有効になります。

    ```lisp
    ;; grep-mode
    (require "grep-mode")
    (add-hook '*grep-hook* 'ed::grep-mode)
    (add-hook '*grepd-hook* 'ed::grep-mode)

    (defun grep-mode-for-before-hook (pattern)
      (ed::grep-mode)
      (toggle-read-only nil))
    (add-hook '*before-grep-hook* 'grep-mode-for-before-hook)
    (add-hook '*before-grepd-hook* 'grep-mode-for-before-hook)
    ```

  * xyzzy のワーキングディレクトリを変更する API を追加しました (x022235, #168)

    * `ed:chdir`

    関連付けたファイルをダブルクリックして xyzzy を新規に起動すると、
    そのファイルがあるディレクトリが xyzzy のワーキングディレクトリとなるため、
    xyzzy を起動したままそのディレクトリを削除しようとすると
    「別のプログラムがこのファイルを開いているので、操作を完了できません」
    というエラーになります。

    以下の設定をすることで常に xyzzy.exe のある場所をワーキングディレクトリと
    することが出来ます。

    ```lisp
    ;; xyzzy 起動時に $XYZZY に移動
    (defun chdir-to-system-root ()
      (chdir (si:system-root)))
    (add-hook '*post-startup-hook* 'chdir-to-system-root)
    ```

  * キーワードファイルのカスタマイズを機能を強化しました (x022235, #165)

    キーワードファイルの中から他のキーワードファイルを読み込む機能、
    および定義済みのキーワードを削除する機能を追加しました。

    例えば etc/C# から LINQ のクエリ式のキーワードを削除して、#region と #endregion
    の色を変えたい場合は以下のように設定します。

    まず、カスタマイズしたキーワードファイルを置く場所を
    `*keyword-load-path*` に設定します。

    ```lisp
    ;; $XYZZY/keywords にカスタマイズしたキーワードファイルを置く
    (pushnew (merge-pathnames "keywords" (si:system-root))
             *keyword-load-path* :test #'string=)
    ```

    次に、$XYZZY/keywords/C# に以下の内容のファイルを置きます。

    ```
    ;;;; C# のキーワードファイルのカスタマイズ
    ;;
    :: #reigon と #endregion はキーワード番号0の色を利用
    ;; LINQ のクエリ式のキーワードを削除
    ;; http://msdn.microsoft.com/en-us/library/bb310804.aspx

    ;; etc/C# の読み込み
    ;@include <C#>

    ;; 色の変更
    ;*0
    #region
    #endregion

    ;; キーワードの削除
    ;*--
    from
    where
    select
    group
    into
    orderby
    join
    let
    in
    on
    equals
    by
    ascending
    descending
    ```

    基本的に etc/ 配下は直接修正せずに、この方法で差分だけを記述したキーワードファイルを
    別途用意することをおすすめします（アップデートが楽になります）。


バグ修正
--------

  * java-mode: アノテーションのインデントに対応しました (x022235, #225)

    ```java
    public class MyTest {
      @Test
      public static void testFoo() { // ここがインデントされないように修正
      }
    }
    ```

  * c#-mode: ネストした using のインデントに対応しました (x022235, #196)

    ```c#
    public class Foo {
      public void Bar() {
        using (FileStream f = new FileStream("test.txt", FileMode.Read))
        using (StreamReader s = new StreamReader(f)) // ここがインデントされないように修正
        {
        }
      }
    }
    ```

  * c#-mode: #region, #endregion のインデントに対応しました (x022235, #192)

    ```c#
    public class Foo {
      #region    // ここがインデントされないように修正
      public void Bar() {
      }
      #endregion // 同上
    }
    ```

  * c#-mode: #if 〜 #endif の中に #endregion があるとインデントがおかしい問題を修正しました (x022235, #206)

    ```c#
    #if FOO
    #region hoge
    #endregion
    #else
    #endif
    // ここでインデントすると「'else'が妙な場所にあります」というエラーになっていたのを修正
    ```

  * c#-mode, c++-mode: 属性のインデントがおかしい問題を修正しました (x022235, #226)

    ```c#
    class AnimalTypeTestClass {
      [AnimalType(Animal.Dog)]
      public void DogMethod() {}

      [AnimalType(Animal.Cat)]
      public void CatMethod() {}  // ここがインデントされないように修正

      [Conditional("DEBUG")]
      [AnimalType(Animal.Bird)]   // 同上
      public void BirdMethod() {} // 同上
    }
    ```

  * c#-mode, c++-mode: enum のインデント対応しました (x022235, #207)

    ```c#
    enum E :
      unsigned int { // ここがインデントされるように修正
      Val1, Val2,
    }
    ```

  * c++-mode: C++/CLI に対応しました (x022235, #195)

    ```c#
    using namespace System;

    [Serializable]
    public ref class MyClass
      : MyBase {   // ここがインデントされるように修正
    private:
      [NonSerialized]
      int m_nData; // ここがインデントされないように修正
    };
    ```

  * c-mode, c++-mode, c#-mode: マクロのインデントに対応しました (x022235, #194)

    * `ed:c-preprocessor-offset`
    * `ed:c++-preprocessor-offset`
    * `ed:csharp-preprocessor-offset`

    整数値を指定すると通常のインデント位置からのオフセットとして解釈します。
    0 の場合は通常のインデント位置と同じ場所にインデントします。

    上記変数に `nil` を指定すると従来通りインデントはしません (デフォルト)。

    ```c
    int main() {
      #if DEBUG  // (setq c-preprocessor-offset 0) の場合のインデント位置
      #if _WIN32 // 同上
      hoge;
      #endif     // 同上
      #endif     // 同上
    }
    ```

  * cmd.exe 実行時にワーキングディレクトリが UNC パスの場合は
    c:\Windows で起動するようにしました  (x022235, #209)

    これにより UNC パス上のファイルを `M-x diff` した場合にエラーになっていた問題が
    修正されました。

  * BOM 付き UTF8 ファイルを `C-x |` でフィルタすると BOM が挿入される問題を修正しました (x022235, #197)

  * クリップボードエンコーディングが sjis, utf-16 以外の場合に xyzzy でコピーして
    メモ帳などに貼り付けると文字化けする問題を修正しました (x022235, #213)

  * ウィンドウ位置がディスプレイを 1px でも上にはみ出ると前回終了時の位置が復元されない
    問題を修正しました (x022235, #212)

  * フォントサイズを大きくするとミニバッファのサイズがウィンドウサイズを越える問題を修正しました (x022235, #220)

  * フォントサイズを小さくしてもミニバッファのサイズが元に戻らない問題を修正しました (x022235, #218)

  * 巨大なファイルの `first-error` が遅い問題を修正しました (x022235, #100)

  * C++ キーワードのタイポを修正しました (x022235, DeaR, #187)


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * 拡張子 .lisp をサポートしました (x022235, #177)

    バイトコンパイルした場合は .lc ファイルが出来ます。
    ロード順は .lc → .l → .lisp です。

  * hashtable の印字形式に情報を追加しました (x022235, #228)

    以下の様な形式で印字されます。

    ```lisp
    #<hashtable :test eql :size 0/17 12590184>
    ```

  * 文字列のバイトサイズを取得する API を追加しました (x022235, #48)

    * `si:octet-length`

    エンコーディングを指定して文字列のバイトサイズを求めることが出来ます。

    ```lisp
    (si:octet-length "abcあいう")
    => 9
    (si:octet-length "abcあいう" :encoding *encoding-utf8n*)
    => 12
    ```

  * ハッシュテーブルの `:rehash-size` に float を指定できるようにしました (x022235, #55)

    float を指定した場合「現在のハッシュテーブルのサイズ * rehash-size」で
    rehash 後のハッシュテーブルの目安サイズを求めます。

    ```lisp
    ;; rehash 時にハッシュテーブルのサイズを 2 倍にする
    (make-hash-table :rehash-size 2.0)
    ```

    デフォルトは 1.5 です。
    また、1.5 より小さい値を指定した場合は 1.5 として扱います。

  * ハッシュテーブルの `:rehash-threshold` を指定できるようにしました (x022235, #55)

    ```lisp
    ;; ハッシュテーブルの 90% を利用した時点で rehash
    (make-hash-table :rehash-threshold 0.9)
    ```

    デフォルトは 0.8 です。

    `:size`、`:rehash-threshold`、`:rehash-size` をデータの特性に合わせて
    うまく指定することで rehash の回数を削減できます。

    ```lisp
    (defun print-hash-table-rehash (&key rehash-threshold rehash-size)
      (let* ((h (make-hash-table :rehash-threshold rehash-threshold
                                 :rehash-size rehash-size))
             (prev-size (hash-table-size h)))
        (dotimes (i 10000)
          (setf (gethash i h) i)
          (let* ((size (hash-table-size h))
                 (count (hash-table-count h))
                 (prev-count (1- count)))
            (when (/= prev-size size)
              (format t "~5D/~5D (~5,2F) => ~5D/~5D (~5,2F)  x ~4,2F~%"
                      prev-count prev-size (/ prev-count prev-size)
                      count size (/ count size)
                      (/ size prev-size))
              (setf prev-size size)
              )))))
    => print-hash-table-rehash

    (print-hash-table-rehash :rehash-threshold 0.8 :rehash-size 1.5)
       14/   17 ( 0.82) =>    15/   47 ( 0.32)  x 2.76
       38/   47 ( 0.81) =>    39/  101 ( 0.39)  x 2.15
       81/  101 ( 0.80) =>    82/  199 ( 0.41)  x 1.97
      160/  199 ( 0.80) =>   161/  307 ( 0.52)  x 1.54
      246/  307 ( 0.80) =>   247/  499 ( 0.49)  x 1.63
      400/  499 ( 0.80) =>   401/  797 ( 0.50)  x 1.60
      638/  797 ( 0.80) =>   639/ 1499 ( 0.43)  x 1.88
     1200/ 1499 ( 0.80) =>  1201/ 2999 ( 0.40)  x 2.00
     2400/ 2999 ( 0.80) =>  2401/ 4999 ( 0.48)  x 1.67
     4000/ 4999 ( 0.80) =>  4001/ 8009 ( 0.50)  x 1.60
     6408/ 8009 ( 0.80) =>  6409/19997 ( 0.32)  x 2.50
    => nil

    (print-hash-table-rehash :rehash-threshold 1.0 :rehash-size 3.0)
       17/   17 ( 1.00) =>    18/  101 ( 0.18)  x 5.94
      101/  101 ( 1.00) =>   102/  307 ( 0.33)  x 3.04
      307/  307 ( 1.00) =>   308/  997 ( 0.31)  x 3.25
      997/  997 ( 1.00) =>   998/ 2999 ( 0.33)  x 3.01
     2999/ 2999 ( 1.00) =>  3000/ 8999 ( 0.33)  x 3.00
     8999/ 8999 ( 1.00) =>  9000/29989 ( 0.30)  x 3.33
    => nil
    ```


xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * lisp-mode: Common Lisp 互換文字を補完できない問題を修正しました (x022235, #227)

  * lisp-mode: マクロ名に # が含まれるとインデントがおかしい問題を修正しました (x022235, #193)

    ```lisp
    (with-c#-mode ()
      ;; ここのインデントを修正
      )
    ```


注意事項
--------

  * NetInstaller で入手可能な以下のパッケージは 0.2.2.238 では
    本体に同梱しています。インストールすると古いファイルで上書きされるので
    インストールしないようにしてください。

    ```
    keyword file                       2007.12.25     2007/12/25 01:27  | 2007.12.25     2007/12/25 01:27
    reference.xml                      2007.12.25     2007/12/25 01:23  | 2007.12.25     2007/12/25 01:23
    ```

既知の問題
----------

  * format の `~n@A` 書式がバグっている

    このバグの修正は影響範囲が大きいので修正されません。

    ```lisp
    (format nil "~10@A" "hoge")
    "hoge      "                   ; 0.2.2.235, 0.2.2.236
    "      hoge"                   ; 本来の仕様
    ```

  * ASCII 以外のサイズはお任せ時に日本語が縦長で表示される (#241)
  * refresh-screen が多値を返す (#219)
  * 循環リストを copy-tree すると落ちる (#191)
  * software-type, software-version が CL と異なる (#169)
  * NULL 文字をインクリメンタル検索しようとすると落ちる (#152)
  * :typeがlistかvectorで:namedじゃない構造体でtypepがおかしい (#138)
  * ローカル関数で (setf READER) (#137)
  * dualウィンドウモードでfilerのディレクトリ指定が動かない (#130)
  * c-modeでマクロの継続行のインデントがおかしい (#127)
  * クリップボードにコピーするとxyzzyが固まる場合がある (#113)
  * C-u 999 C-g 後にメニュー操作でエラー (#111)
  * Vista 以降で再変換 (C-c C-c) が動作しない (#101)
  * ファイル保存時にパーミッションを保存 (#96)
  * ole で responseBody, responseStream を取得できない (#68)
  * ole-for-each で ie.Document.all の IEnum を取得できない (#67)
  * ole-create-event-sink に TypeLib のファイル名を明示的に指定しないとエラーになる (#66)
  * 巨大な文字列に対する正規表現マッチがすごい遅い (#65)
  * load 時の `*readtable*` がファイルローカルではない (#64)
  * setf の最適化に bug (#63)
  * handler-case で :no-error を指定してコンパイルするとエラー (#62)
  * labels の lambda-list 内の init-form で同じ labels 式で定義したローカル関数を呼び出してると、コンパイルで挙動が変わる (#61)
  * si:binhex-decode で落ちる (#45)
  * multiframe: 画面端の折り返しがウィンドウ単位でちゃんと動くようにする変更を取り込む (#25)
  * siteinit.l が sjis 以外で書かれていた場合に対応 (#11)
  * multiframe: cpp-syntax の修正を取り込む (#10)

`(provide "xyzzy-0.2.2.238")`


  [libguess]: http://www.honeyplanet.jp/download.html#libguess
  [grep-mode]: http://nazoking.s31.xrea.com:8080/k/grep-mode.l
  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
