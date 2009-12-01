;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

#|

バックアップファイルの作成先を好みの場所に変更します。
(require "backup") をした上で、以下の変数を好みの値に設定してください。


*backup-directory*
バックアップファイルを作成するディレクトリを指定する。

*hierarchic-backup-directory*
バックアップディレクトリの階層を掘るか?

*auto-save-to-backup-directory*
nilでなければ、自動セーブファイルをここで指定するバックアップディレクト
リに保存する(デフォルトはnil)


*backup-directory*に設定できる値は以下の通り。

 1 nil (デフォルト)
 2 文字列
 3 リスト
 4 関数

ちゃんとした説明は面倒なので省略。例を見て理解して。

1 (setq *backup-directory* nil)

  保存するファイルと同じディレクトリにバックアップファイルが作成される。
  つまり、従来通り。

2.1 (setq *backup-directory* "c:/backup-files/")
    (setq *hierarchic-backup-directory* nil)

  この場合、すべてのバックアップファイルは c:/backup-files/ の下に作成される。

2.2 (setq *backup-directory* "c:/backup-files/")
    (setq *hierarchic-backup-directory* t)

  c:/foo/bar.txt のバックアップファイルは c:/backup-files/c/foo/
  c:/foo/bar/baz.txt のバックアップファイルは c:/backup-files/c/foo/bar/
  d:/foo/bar.txt のバックアップファイルは c:/backup-files/d/foo/
  //foo/bar/baz/qux.txt のバックアップファイルは c:/backup-files/foo/bar/baz/
  にそれぞれ作成される。

2.3 (setq *backup-directory* "BACKUP")

  *backup-directory* が '/' を含まない場合、
  c:/foo/bar.txt のバックアップファイルは c:/foo/BACKUP/
  c:/foo/bar/baz.txt のバックアップファイルは c:/foo/bar/BACKUP/
  にそれぞれ作成される。

  *hierarchic-backup-directory* の値は関係ない。

  (setq *backup-directory* "../BACKUP") のような場合は、'/' を含んでいる
  ので 2.1 または 2.2 となる。

3.1 (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))
    (setq *hierarchic-backup-directory* nil)

  c:/foo/bar.txt, c:/foo/bar/baz.txt 等、c:/foo/ で始まるファイルは、すべて
  c:/backup-files-c/ の下に作成され、
  d:/bar/baz.txt, d:/bar/baz/qux.txt 等、d:/bar/ で始まるファイルは、すべて
  c:/backup-files-d/ の下に作成される。
  それ以外のファイルは、保存するファイルと同じディレクトリに作成される。

3.2 (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))
    (setq *hierarchic-backup-directory* t)

  c:/foo/bar.txt のバックアップファイルは c:/backup-files-c/
, c:/foo/bar/baz.txt のバックアップファイルは c:/backup-files-c/bar/
  d:/bar/baz.txt のバックアップファイルは c:/backup-files-d/
  d:/bar/baz/qux.txt のバックアップファイルは c:/backup-files-d/baz/
  にそれぞれ作成される。
  それ以外のファイルは、保存するファイルと同じディレクトリに作成される。

3.3 3.1, 3.2 共通

3.3.1 リストに一致しないファイルのバックアップ先を指定する場合は、

    (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")
	    ("" . "c:/common-backup/")))

  とすれば OK。

3.3.2 ディレクトリを表わす場合は、末尾に '/' を付ける。

  ("c:/foo" . "c:/backup-files/")

  と指定した場合、c:/foo/bar.txt だけでなく、c:/food/drink.txt にも、
  c:/foot/ball.txt にもマッチすることに注意。
  ただし、c:/fool.txt にはマッチしない。

3.3.3 リストは左から評価される。

    (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-foo/")
	    ("c:/foo/bar/" . "c:/backup-files-fubar/")))

  の場合、c:/foo/bar/baz.txt は c:/foo/ にマッチする。

    (setq *backup-directory*
	  '(("c:/foo/bar/" . "c:/backup-files-fubar/")
	    ("c:/foo/" . "c:/backup-files-foo/")))

  とするのが吉。

  同様に、

    (setq *backup-directory*
	  '(("" . "c:/common-backup/")
	    ("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))

  は、すべて c:/common-backup/ にバックアップされてまう。

3.3.4 バックアップ先が nil なら、保存するファイルと同じディレクトリに作
      成される。

    (setq *backup-directory*
	  '(("c:/foo/")
	    ("d:/bar/" . "c:/backup-files-d/")
	    ("" . "c:/common-backup/")))

この場合、c:/foo/ にマッチするファイルのバックアップファイルは、同じディ
レクトリに作成される。

4 関数
  ファイル名を引数とする funcall 可能な関数でなければならない。
  戻り値が nil の場合は、保存するファイルと同じディレクトリに作成される。
  戻り値が nil でない場合、文字列でなければならず、2 と同様に処理される。

注1) パスの区切りに '\' は受け付けない。必ず '/' を使用すること。
注2) 間違った設定をすると、ファイルのセーブができなくなるので気をつけてね。
     もし、どうしようもなくなったら、

     (setq *backup-directory* nil)

     を評価するか、write-file 等で逃げる。

|#

(provide "backup")

(in-package "editor")

(export '(*backup-directory* *hierarchic-backup-directory*))

(or (boundp '*backup-directory*)
    (setq *backup-directory* nil))

(or (boundp '*hierarchic-backup-directory*)
    (setq *hierarchic-backup-directory* nil))

(or (boundp '*auto-save-to-backup-directory*)
    (setq *auto-save-to-backup-directory* nil))

(defun make-backup-directory-hook (filename)
  (let* ((dir (directory-namestring filename))
	 (templ nil)
	 (backup (cond ((null *backup-directory*)
			(return-from make-backup-directory-hook nil))
		       ((stringp *backup-directory*)
			*backup-directory*)
		       ((consp *backup-directory*)
			(dolist (x *backup-directory*
				   (return-from make-backup-directory-hook nil))
			  (setq templ (car x))
			  (when (and (>= (length dir) (length templ))
				     (string-equal templ dir :end2 (length templ)))
			    (when (null (cdr x))
			      (return-from make-backup-directory-hook nil))
			    (return (cdr x)))))
		       (t
			(or (funcall *backup-directory* filename)
			    (return-from make-backup-directory-hook nil))))))
    (when (prog1
	      (and *hierarchic-backup-directory*
		   (find #\/ backup))
	    (setq backup (append-trail-slash (merge-pathnames backup dir))))
      (setq backup (format nil "~A~{~A/~}" backup
			   (split-string (subseq dir (length templ)) "/:"))))
    (create-directory backup :if-exists :skip)
    backup))

(setq *make-backup-filename-hook* #'make-backup-directory-hook)
