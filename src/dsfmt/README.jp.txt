 =================================================================
 dSFMT ver. 2.1.1
 2011.11.10

 double precision SIMD oriented Fast Mersenne Twister(dSFMT)
 based on IEEE 754 floating point format.

 Mutsuo Saito (Hiroshima University) and
 Makoto Matsumoto (Hiroshima University)

 Copyright (C) 2007, 2008, 2009 Mutsuo Saito, Makoto Matsumoto and
 Hiroshima University.
 Copyright (C) 2011 Mutsuo Saito, Makoto Matsumoto, Hiroshima
 University and The University of Tokyo.
 All rights reserved.

 The (modified) BSD License is applied to this software, see
 LICENSE.txt
 =================================================================

 ドキュメントは、たとえ英語が文法的に正しくない場合でも、英語版が正式な
 ものです。

 dSFMT2.0 および 2.1 はdSFMT1.x とは疑似乱数生成の漸化式が違います。つまり、
 dSFMT1.xと同じ初期値を与えても異なる疑似乱数列を生成します。

 doxygen によって生成されたドキュメント（英語）が、html ディレクトリの
 下にあるので参照してください。

 このプログラムは IEEE754 形式の浮動小数点数を使用しているシステムでの
 み動作します。

 このプログラムは、C言語の構造体を使用しています。したがって、それをう
 まく使えば、スレッド毎に別々の疑似乱数列を利用することができます。しか
 し、dSFMT.cのコンパイル時と異なるDSFMT_MEXPをユーザープログラムのコン
 パイル時に指定すると、問題が発生します。（スピードを犠牲にして、パラメー
 タを全部構造体に入れるという方法もありますが、ここではスピードを優先し
 ました）

 とりあえずテストプログラムを走らせてみたいという人は、html ディレクト
 リの下のhowto-compile.html（英語）をみて下さい。CPU がビッグエンディア
 ンの場合は、注意が必要です。事前定義プリプロセッサマクロによって、エン
 ディアンの判定をしていますが、明示的に DSFMT_BIG_ENDIAN マクロを定義し
 た方が安全だと思います。

 ソースコードを変更したりしなかったりしてこのプログラムを再配布したい人
 は、LICENSE.txt（英語）を読んで下さい。

 ファイルに変更を加えて再配布する場合は、どうか、配布物の中にあなたの連
 絡先を書いておいて、問題があったときは、私たちではなく、あなたにまず連
 絡するように利用者に伝えて下さい。

