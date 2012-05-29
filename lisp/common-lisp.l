;;; -*- Mode: Lisp; Package: COMMON-LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "common-lisp")

(in-package "common-lisp")

(export '(nil))

(do-external-symbols (lspsym "lisp")
  (multiple-value-bind (sym state)
      (find-symbol (string lspsym) "common-lisp")
    (case state
      (:inherited (export sym "common-lisp"))
      (:external (shadow sym "common-lisp"))
      )))
