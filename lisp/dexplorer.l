;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "dexplorer")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ole"))

(in-package "editor")

(export '(show-dexplorer *dexplorer-collection* *dexplorer-filter*))

(defvar *dexplorer-object* nil)
(defvar *dexplorer-help* nil)
(defvar *dexplorer-collection* "ms-help://ms.vscc")
(defvar *dexplorer-filter* "Visual Studio .NET Combined Help Collection")

(defun show-dexplorer ()
  (interactive)
  (let ((keyword (get-winhelp-topic)))
    (when (equal keyword "")
      (setq keyword " "))
    (unless *dexplorer-object*
      (setq *dexplorer-object* (ole-create-object "DExplore.AppObj"))
      (setq *dexplorer-help* #{*dexplorer-object*.Help})
      #{*dexplorer-help*.SetCollection [ *dexplorer-collection* *dexplorer-filter* ]})
    #{*dexplorer-help*.DisplayTopicFromKeyword [ keyword ]}
    ;#{*dexplorer-help*.SyncIndex [ keyword 0 ]}
    t))
