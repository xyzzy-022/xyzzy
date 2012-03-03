## usage

- M-x makelc --- ~/lispフォルダ以下の*.lを再コンパイル
- (makelc:compile-files dirname) --- 指定ディレクトリ以下の*.lを再コンパイル
- (makelc:compile-package :packagename) --- 指定パッケージ内で定義されている未コンパイルの関数をコンパイル

