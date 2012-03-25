;;;; Generate WiX XML Source, from which we eventually generate the .MSI

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; XML generation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "cmu_loop"))

(defpackage :wxsgen
  (:use :lisp :editor))

(in-package :wxsgen)

(export '(update-xyzzy-wxs
          ))

(defvar *product-id* "64737C3A-D311-46C3-3A7C-736411D3C346"
  "メジャーアップグレード時に変更する")

(defvar *upgrade-code* "A12EDB2B-4F0A-4D46-2BDB-2EA10A4F464D"
  "アップグレード可能かどうかを調べるための ID。
基本的に変更しない。")

(defvar *application-name* (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
(defvar *application-version* (lisp-implementation-version))
(defvar *application-manufacturer* "xyzzy Development Team")
(defvar *application-properties*
  `(("WIXUI_INSTALLDIR" . "INSTALLDIR")
    ("ARPCONTACT" . "@x022235")
    ("ARPPRODUCTICON" . "xyzzy.ico")
    ("ARPHELPLINK" . "http://xyzzy-022.github.com")
    ))
(defvar *application-shortcuts*
  '("xyzzy.exe" "xyzzycli.exe"))


(defparameter *xyzzy-source-root* (si:system-root))
(defparameter *xyzzy-wxs-path* (merge-pathnames "installer/xyzzy.gen.wxs" *xyzzy-source-root*))
(defparameter *xyzzy-wxl-path* (merge-pathnames "installer/xyzzy.wxs.l" *xyzzy-source-root*))
(defparameter *xyzzy-components* '("etc" "lisp" "html" "reference"))
(defparameter *ignored-files* (list (si:dump-image-path)
                                    (merge-pathnames "DOC" *etc-path*)))
(defparameter *components* nil)
(defparameter *id->guid-alist* nil)


(defun update-xyzzy-wxs ()
  (interactive)
  (with-output-to-temp-buffer ("wxsgen")
    (when (file-exist-p *xyzzy-wxl-path*)
      (format t "Reading ~A..." *xyzzy-wxl-path*)
      (refresh-screen)
      (update-guid-mapping (read-xyzzy-wxs *xyzzy-wxl-path*))
      (format t "done~%"))
    (format t "Generating ~A..." *xyzzy-wxs-path*)
    (refresh-screen)
    (let ((wxs (make-wxs-sexp)))
      (write-sexp *xyzzy-wxl-path* wxs)
      (write-xml *xyzzy-wxs-path* wxs))
    (format t "done~%")
    ))

(defun make-wxs-sexp ()
  ;; both :INVERT and :PRESERVE could be used here, but this seemed
  ;; better at the time
  `(:Wix (:xmlns "http://schemas.microsoft.com/wix/2006/wi")
    (:Product (:Id ,*product-id*
               :UpgradeCode ,*upgrade-code*
               :Name ,*application-name*
               :Version ,*application-version*
               :Manufacturer ,*application-manufacturer*
               :Language 1041)
     (:Package (:Id "*" ;; ID 自動生成
                :Manufacturer ,*application-manufacturer*
                :InstallerVersion 200
                :Compressed "yes"))
     (:Media (:Id 1
              :Cabinet "xyzzy.cab"
              :EmbedCab "yes"))
     (:Directory (:Id "TARGETDIR"
                  :Name "SourceDir")
      (:Directory (:Id "ProgramMenuFolder"
                   :Name "PMFolder")
       (:Directory (:Id "ApplicationProgramsFolder"
                    :Name "xyzzy")))
      (:Directory (:Id "ProgramFilesFolder"
                   :Name "PFiles")
       (:Directory (:Id "BaseFolder"
                    :Name "xyzzy")
        (:Directory (:Id "INSTALLDIR")
         ,@(collect-all-components)))))
     (:DirectoryRef (:Id "ApplicationProgramsFolder")
      ,(collect-all-shortcuts))
     (:Feature (:Id "Minimal"
                :ConfigurableDirectory "INSTALLDIR"
                :Level 1)
      ,@(ref-all-components))
     ,@(application-properties)
     (:Icon (:Id "xyzzy.ico" :SourceFile "src/res/xyzzy4.ico"))
     (:Icon (:Id "xyzzycli.ico" :SourceFile "src/res/xyzzy3.ico"))
     (:UIRef (:Id "WixUI_InstallDir")))))

(defun write-sexp (pathname sexp)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (print-sexp sexp stream)))

(defun print-sexp (sexp &optional (stream *standard-output*))
  (walk-sexp sexp
             #'(lambda (tag attributes children indent)
                 (fresh-line stream)
                 (format stream "~VT(~S~@[ ~S~]~@[)~]"
                         indent tag attributes (not children)))
             #'(lambda (tag attributes children indent)
                 (when children
                   (format stream ")")))))

(defun write-xml (pathname sexp)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "<?xml version='1.0' encoding='utf-8'?>~%")
    (print-xml sexp stream)))

(defun print-xml (sexp &optional (stream *standard-output*))
  (walk-sexp sexp
             #'(lambda (tag attributes children indent)
                 (format stream "~VT<~A~{ ~A='~A'~}~@[/~]>~%"
                         indent tag attributes (not children)))
             #'(lambda (tag attributes children indent)
                 (when children
                   (format stream "~VT</~A>~%" indent tag)))))

(defun walk-sexp (sexp onbegin onend &optional (indent 0))
  (let  ((tag (car sexp))
         (attributes (cadr sexp))
         (children (cddr sexp)))
    (when onbegin
      (funcall onbegin tag attributes children indent))
    (dolist (child children)
      (unless (listp child)
        (error "Malformed child: ~S in ~S" child children))
      (walk-sexp child onbegin onend (+ indent 2)))
    (when (and children onend)
      (funcall onend tag attributes children indent))))

(defun read-xyzzy-wxs (path)
  (with-open-file (stream path)
    (read stream)))

(defun update-guid-mapping (sexp)
  (walk-sexp sexp
             #'(lambda (tag attributes children indent)
                 (let ((id (getf attributes :Id))
                       (guid (getf attributes :Guid)))
                   (put-id->guid-mapping id guid)))
             nil))


(defun application-properties ()
  (loop for (id . value) in *application-properties*
    collect `(:Property (:Id ,id :Value ,value))))

(defun collect-all-shortcuts ()
  (let ((component-id (component-id "shortcut")))
    `(:Component (:Id ,component-id
                  :Guid ,(get-guid component-id))
      ,@(loop for exe in *application-shortcuts*
          collect (collect-1-shortcut exe))
      (:RemoveFolder (:Id "ApplicationProgramsFolder" :On "uninstall"))
      (:RegistryValue (:Root "HKCU" :Key "Software\\xyzzy"
                       :Name "Installed" :Type "integer" :Value "1" :KeyPath "yes")))))

(defun collect-1-shortcut (path)
  (let* ((filename (file-namestring path))
         (basename (pathname-name filename))
         (lnk (format nil "~A.lnk" basename)))
    `(:Shortcut (:Id ,(shortcut-id lnk)
                 :Name ,basename
                 :Target ,(format nil "[INSTALLDIR]~A" filename)
                 :WorkingDirectory "INSTALLDIR"
                 :Icon ,(format nil "~A.ico" basename))
      (:ShortcutProperty (:Key "System.AppUserModel.ID"
                          :Value "xyzzy")))))

(defun collect-all-components ()
  (append
   (loop for exe in (directory *xyzzy-source-root* :wild "*.exe")
     collect (collect-exe-component exe))
   (loop for dir in *xyzzy-components*
     append (collect-components (merge-pathnames dir *xyzzy-source-root*)))))

(defun collect-exe-component (exe)
  (let ((id (component-id exe)))
    `(:Component (:Id ,id
                  :Guid ,(get-guid id)
                  :DiskId 1)
      (:File (:Id ,(file-id id)
              ,@(file-names exe)
              :Source ,(enough-namestring exe *xyzzy-source-root*)
              :KeyPath "yes")))))

(defun collect-components (root)
  (list (collect-1-component root)))

(defun collect-1-component (root)
  (let ((id (component-id root)))
    `(:Directory (:Id ,(directory-id root)
                   ,@(directory-names root))
      (:Component (:Id ,id
                    :Guid ,(get-guid id)
                    :DiskId 1)
       ,@(loop for file in (directory root :absolute t :file-only t)
           when (and (or (pathname-name file) (pathname-type file))
                     (not (member file *ignored-files* :test #'path-equal)))
           collect `(:File (:Id ,(file-id file)
                             ,@(file-names file)
                            :Source ,(enough-namestring file *xyzzy-source-root*)))))
      ,@(loop for subdir in (directory root :absolute t :directory-only t)
          append (collect-components subdir)))))

(defun ref-all-components ()
  (prog1
      (mapcar (lambda (id)
                `(:ComponentRef (:Id ,id)))
              *components*)
    (setf *components* nil)))

(defun component-id (pathname)
  (let ((id (id (format nil "Component_~A" (enough-namestring pathname *xyzzy-source-root*)))))
    (push id *components*)
    id))


(defun shortcut-id (name)
  (id (format nil "Shortcut_~A" (enough-namestring name *xyzzy-source-root*))))

(defun directory-id (name)
  (id (format nil "Directory_~A" (enough-namestring name *xyzzy-source-root*))))

(defun directory-names (pathname)
  `(:Name ,(file-namestring (remove-trail-slash pathname))))

(defun file-id (pathname)
  (id (format nil "File_~A" (enough-namestring pathname *xyzzy-source-root*))))

(defun file-names (pathname)
  `(:Name ,(file-namestring pathname)))

(defun enough-namestring (pathname &optional defaults)
  (let* ((pathname (map-backslash-to-slash pathname))
         (prefix (append-trail-slash (merge-pathnames "" defaults)))
         (prefix-len (length prefix)))
    (if (and (< prefix-len (length pathname))
             (string-equal prefix (substring pathname 0 prefix-len)))
        (substring pathname prefix-len)
      pathname)))

(defun id (string)
  ;; Mangle a string till it can be used as an Id. A-Z, a-z, 0-9, and
  ;; _ are ok, nothing else is.
  (substitute-string string "[^a-zA-Z0-9._]" "_"))

(defun get-guid (id)
  (or (cdr (assoc id *id->guid-alist* :test #'string=))
      (let ((guid (make-guid)))
        (put-id->guid-mapping id guid)
        guid)))

(defun put-id->guid-mapping (id guid)
  (when (and id guid)
    (setf *id->guid-alist* (acons id guid *id->guid-alist*))))

(defun make-guid ()
  (si:uuid-create))


(provide "gen-wxs")
