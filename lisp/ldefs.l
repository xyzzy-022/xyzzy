;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "ldefs")

(in-package "lisp")

(export '(** *** // /// ++ +++))

(export 'compile-file)
(autoload 'compile-file "compile")

(export 'byte-compile-file)
(autoload 'byte-compile-file "compile" t)

(export 'byte-recompile-directory)
(autoload 'byte-recompile-directory "compile" t)

(export 'compile)
(autoload 'compile "compile")

(export 'mc-compile-file)
(autoload 'mc-compile-file "compile")

(export 'mc-byte-compile-file)
(autoload 'mc-byte-compile-file "compile" t)

(export 'mc-byte-recompile-directory)
(autoload 'mc-byte-recompile-directory "compile" t)

(export 'defstruct)
(autoload 'defstruct "struct" nil t)
(unless (and (fboundp 'si:*structure-reader)
	     (not (si:*builtin-function-p #'si:*structure-reader)))
  (fmakunbound 'si:*structure-reader)
  (autoload 'si:*structure-reader "struct"))
