;; -*- mode:lisp; package:makelc -*-
;;
;;; Usage:
;;
;; - M-x makelc:makelc
;;   compiler パッケージと lisp パッケージの関数をコンパイル後、
;;   $XYZZY/lisp フォルダ以下の更新された*.l を再コンパイルしてリロード
;;
;; - C-u M-x makelc:makelc
;;   compiler パッケージと lisp パッケージの関数をコンパイル後、
;;   $XYZZY/lisp フォルダ以下の*.l を再コンパイルしてリロード
;;
;; - (makelc:makelc-and-exit &optional force-recompile)
;;   compiler パッケージと lisp パッケージの関数をコンパイル後、
;;   $XYZZY/lisp フォルダ以下の*.l を再コンパイルして xyzzy を終了
;;   force-recompile が non-nil の場合は常に再コンパイル
;;
;; - (makelc:compile-files &key dir force-recompile)
;;   指定ディレクトリ以下の更新された*.l を再コンパイル
;;   force-recompile が non-nil の場合は常に再コンパイル
;;
;; - (makelc:compile-package :packagename)
;;   指定パッケージ内で定義されている未コンパイルの関数をコンパイル
;;
;; - (makelc:reload-files)
;;   lisp/配下のモジュールをすべて再読み込み
;;
;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "compile"))

(defpackage :makelc
  (:use :lisp :ed))

(in-package :makelc)
(export '(compile-package
	  compile-files
	  reload-files
          makelc
          makelc-and-exit
	  ))

(defmacro time (form)
  `(let ((start (get-internal-real-time))
	 (result (multiple-value-list ,form)))
     (format t "~&~V@{-~}~%~D msec~%~%" 30 (- (get-internal-real-time) start))
     (values-list result)))

(defun not-compiled-functions (pkg)
  (let ((fns nil)
	(p (find-package pkg)))
    (do-symbols (s p)
      (when (and (fboundp s)
		 (not (macro-function s))
		 (eq p (symbol-package s)))
	(let ((f (symbol-function s)))
	  (when (not (or (si:*builtin-function-p f)
			 (compiled-function-p f)
			 (not (functionp f))
			 (and (si:*closurep f)
			      (si:closure-variable f))))
	    (push s fns)))))
    (nreverse fns)))

(defun compile-package (pkg)
  (format t "compiling <~A> ...~%" pkg)
  (time
   (dolist (f (not-compiled-functions pkg))
     (format t "~A ..." f)
     (goto-char (point-max))
     (refresh-screen)
     (handler-case
	 (progn
	   (compile f)
	   (princ "ok"))
       (error (c)
	 (format t "error~%*****~%~A~%*****"
		 (si:*condition-string c))))
     (terpri))))

(defun compile-files (&key (dir (merge-pathnames "lisp" (si:system-root)))
			   force-recompile)
  (let ((files (directory dir :absolute t :wild "*.l" :recursive t)))
    (format t "compiling ~D files ...~%" (length files))
    (time
     (dolist (src files)
       (let ((bin (compile-file-pathname src)))
	 (when (or force-recompile
		   (file-newer-than-file-p src bin))
           (message "Compiling ~A" src)
	   (byte-compile-file src)
	   (refresh)
	   ))))))

(defun reload-files ()
  (labels ((lisp-module-p (module)
	     (file-exist-p (merge-pathnames (format nil "lisp/~A.l" module)
					    (si:system-root)))))
    (let ((lisp-modules (remove-if-not #'lisp-module-p *modules*)))
      (setf *modules* (remove-if #'lisp-module-p *modules*))
      (format t "reloading ~D modules ...~%" (length lisp-modules))
      (time
       (dolist (module lisp-modules)
	 (load-library module)
	 (refresh)
	 )))))

(defun refresh ()
  (goto-char (point-max))
  (refresh-screen))

(defun makelc (&optional (force-recompile *prefix-args*))
  (interactive)
  (let ((buf (create-new-buffer "*compile log*")))
    (set-buffer buf)
    (setup-temp-buffer buf)
    (with-output-to-buffer (buf)
      (compile-package :compiler)
      (compile-package :lisp)
      (compile-files :force-recompile force-recompile)
      (reload-files)
      (format t "DONE.~%")
      (refresh)
      (let ((dump (si:dump-image-path)))
	(when (file-exist-p dump)
	  (delete-file dump)))
      )))

(defun makelc-and-exit (&optional (force-recompile *prefix-args*))
  (handler-case
      (progn
        (makelc force-recompile)
        (kill-xyzzy t))
    (condition (e)
      (msgbox "~A" e)
      (kill-xyzzy nil))))


(provide "makelc")
