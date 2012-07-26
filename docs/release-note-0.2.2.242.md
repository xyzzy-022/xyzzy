xyzzy リリースノート
====================

  * バージョン: 0.2.2.242
  * リリース日: 2012-07-29
  * ホームページ: <http://xyzzy-022.github.com>


はじめに
--------

xyzzy 0.2.2.242 では grep-dialog などでの無視ディレクトリの指定や、
complete+ 利用時に時々クラッシュする問題の修正などを行なっています。


インストール
------------

インストーラはありませんので zip を展開するだけです。
インストールから初期設定までは以下を参照してください。

  * [QuickTour - XyzzyWiki]


アップデート
------------

以下の手順で 0.2.2.235 からアップデートしてください。

  1. 0.2.2.235 のバックアップ取得
  2. 0.2.2.242 を上書き
  3. $XYZZY/html を削除 ($XYZZY/docs/old に移動しています)
  4. xyzzy.wxp を削除
  5. xyzzy.exe 起動

lisp/ 配下や etc/ 配下をカスタマイズしている場合は
上書き後に再度カスタマイズをお願いします。


機能追加
--------

  * `grep-dialog`, `gresreg-dialog` で無視ディレクトリを指定できるようになりました (x022235, #198)

    再帰的にファイルを検索する時に無視したいディレクトリ名を
    `ed:*grep-ignored-directories*` で指定します。

    デフォルトでは以下が設定されています。

    ```lisp
    "SCCS", "RCS", "CVS", "MCVS", ".svn", ".git", ".hg", ".bzr",
    "_MTN", "_darcs", "{arch}"
    ```

    追加で tmp/ を無視したい場合は以下のようにします。

    ```lisp
    (require "grepd")
    (pushnew "tmp" *grep-ignored-directories* :test #'string-equal)
    ```

  * 非表示バッファ選択時にもタイトルバーを変更するようにしました (x022235, #336)

  * ファイルを開くときに XML のエンコード宣言にしたがってエンコードを決定するようにしました (x022235, #339)


バグ修正
--------

  * ミニバッファでファイル名の補完時に時々クラッシュする問題を修正しました (x022235, #321)

    Vista 以降で [complete+] を利用していると高確率で発生していた問題です。
    complete+ の問題ではなく xyzzy 本体の問題でした。

  * etc/extract.exe がウィルス感染ファイルであると誤認識される問題を修正しました (x022235, #342)


xyzzy Lisp 開発者向け機能追加
-----------------------------

  * ハッシュダイジェストをバイナリ値で取得できるようにしました (x022235, #47)

    Amazon Web Services や OAuth などは HMAC-SHA-256 のバイナリ値を
    base64 でエンコードしたものが必要になります。

    今までは `si:hmac-sha-256` などが返した 16 進文字列からバイナリ値を
    得るために無駄な再変換が必要でしたが、以下の関数に `:binary` 引数を
    指定することでバイナリ値を直接生成できるようにしました。

    - md5
    - sha-1
    - sha-224
    - sha-256
    - sha-384
    - sha-512
    - hmac-md5
    - hmac-sha-1
    - hmac-sha-224
    - hmac-sha-256
    - hmac-sha-384
    - hmac-sha-512

    `:binary` 引数が `non-nil` なら計算結果をバイナリ値を文字列で返します。
    `nil` の場合は従来通り 16 進文字列に変換して返します。

    ```lisp
    (setq sha256bin (map 'list 'char-code (si:sha-256 "xyzzy" :binary t)))
    => (24 72 88 160 15 215 151 31 129 8 72 38 110 188 236 238 94 139 105
        151 44 95 250 237 98 47 94 224 120 103 26 237)

    (format nil "~{~2,'0X~}" sha256bin)
    => "184858a00fd7971f810848266ebcecee5e8b69972c5ffaed622f5ee078671aed"

    (si:sha-256 "xyzzy")
    => "184858a00fd7971f810848266ebcecee5e8b69972c5ffaed622f5ee078671aed"
    ```

  * 乱数の生成アルゴリズムをメルセンヌ・ツイスターに変更しました (x022235, #142)

    実装には松本眞氏と斎藤睦夫氏によって開発された [Double precision SIMD-oriented Fast Mersenne Twister (dSFMT)]
    を利用しています (周期はビルド時に 2^19937-1 で固定)。

    参考: [良い乱数・悪い乱数]

  * `directory` 関数に `:test` 引数を追加しました (x022235, #198)

    見つかったファイルを引数にして `:test` で指定した関数を `funcall` します。
    関数が `nil` を返した場合、そのファイルは無視します。
    ディレクトリの場合はディレクトリ配下のファイルをすべて無視します
    (そのディレクトリ配下の探索自体を行いません)。

    ```lisp
    ;; .git ディレクトリを無視したい場合
    (directory "." :test #'(lambda (path)
                             (not (and (file-directory-p path)
                                       (string-equal ".git" (pathname-name path)))))
               :absolute t :recursive t)
    ```

    備考: [CL の directory] 関数には `:test` 引数はありませんが、
    もともと xyzzy の `directory` 関数は CL とは大きく異なるため追加しました。
    この仕様は [CL-FAD の walk-directory] 関数を参考にしました。

  * `flet`, `labels` で作成した局所関数に印字用の名前を付けました (youz, x022235, #323)

    `*Trace Output*` バッファでも名前付きで出力されるためデバッグがしやすくなりました。

    ```lisp
    (flet ((foo ()))
      (labels ((bar ()))
        (list #'foo #'bar)))
    ;=> (#<lexical-closure: (flet foo)> #<lexical-closure: (labels bar)>)
    ;   0.2.2.241 では (#<lexical-closure: (anonymous)> #<lexical-closure: (anonymous)>)
    ```


xyzzy Lisp 開発者向けバグ修正
-----------------------------

  * `random-state` をほぼ同時に作成すると同じ乱数が生成される問題を修正しました (x022235, #142)

    乱数の seed を時刻から [CryptGenRandom] で生成した乱数に変更しました。

    ```lisp
    (let ((s1 (make-random-state t))
          (s2 (make-random-state t)))
      (dotimes (_ 10)
        (format t "~S~%" (list (random 100 s1) (random 100 s2)))))
    (48 63)
    (53 47)
    (32 70)
    (83 27)
    (36 25)
    (99 81)
    (54 22)
    (36 82)
    (63 35)
    (67 14)

    ; 0.2.2.241 では以下のように同じ乱数が生成される
    (72 72)
    (44 44)
    (92 92)
    (54 54)
    (89 89)
    (72 72)
    (22 22)
    (93 93)
    (19 19)
    (72 72)
    ```

  * `make-process` の `:show` パラメータが動作しない問題を修正しました (x022235, #315)

    `make-process` は xyzzyenv.exe 経由で指定したコマンドを実行しますが、
    `:show` パラメータが指定したコマンドではなく xyzzyenv.exe に反映されていました。

    ```lisp
    (make-process "notepad" :show :maximize)
    ;=> メモ帳が最大化して起動
    ;   0.2.2.241 では xyzzyenv.exe が最大化して起動
    ```

    また、`ed:*xyzzyenv-show-flag*` で xyzzyenv.exe の表示方法を指定することができます。

    ```lisp
    ; xyzzyenv.exe はタスクバーに表示しない
    (let ((*xyzzyenv-show-flag* :hide))
      (make-process "notepad" :show :maximize))
    ```

  * マクロ・特殊形式の引数間違い時のエラーメッセージを改善しました (x022235, #329)

    ```lisp
    (if 1 2 3 4)
    ;=> 引数が多すぎます: (if 1 2 3 4)
    ;   0.2.2.241 では「引数が多すぎます: (#1=#:|| . #1#)」

    (defmacro foo (a b)
      `(+ ,a ,b))
    (foo 1)
    ;=> 引数が少なすぎます: (foo 1)
    ;   0.2.2.241 では「引数が少なすぎます: (#1=#:|| . #1#)」
    ```

  * `*scratch*` バッファで評価すると `*Trace Output*` に警告が出力される問題を修正しました (x022235, #334)

  * `reduce` の `:from-end` パラメータに t を指定した時の不具合を修正しました (youz, #335)

    0.2.2.241 でのデグレードでした。

その他
------

  * マルチコア環境でのビルド速度を改善しました (x022235, #341)


既知の問題
----------

  * macro-function が CL と異なる (#320)
  * multiple-value-bind、multiple-value-setq の macro-function が nil (#319)
  * special-form-p が特殊形式以外にも t を返す (#318)
  * call-process :wait t でコマンドが固まると xyzzy も固まる (#316)
  * (setf (symbol-function)) がシンボル名を返す (#269)
  * ASCII 以外のサイズはお任せ時に日本語が縦長で表示される (#242)
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


`(provide "xyzzy-0.2.2.242")`


  [QuickTour - XyzzyWiki]: http://xyzzy.s53.xrea.com/wiki/index.php?QuickTour
  [complete+]: http://white.s151.xrea.com/wiki/index.php?script%2Fcomplete%2B
  [良い乱数・悪い乱数]: http://www001.upp.so-net.ne.jp/isaku/rand.html
  [Double precision SIMD-oriented Fast Mersenne Twister (dSFMT)]: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index-jp.html
  [CL の directory]: http://clhs.lisp.se/Body/f_dir.htm
  [CL-FAD の walk-directory]: http://weitz.de/cl-fad/#walk-directory
  [CryptGenRandom]: http://msdn.microsoft.com/en-us/library/windows/desktop/aa379942.aspx
