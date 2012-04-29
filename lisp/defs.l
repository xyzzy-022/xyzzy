;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "defs")

(in-package "editor")

(export '(mode-line-format mode-specific-indent-command *auto-mode-alist*
	  show-dots ignored-extensions insert-default-directory
	  mode-specific-indent-command *default-buffer-mode*
	  *caret-shape-block* *caret-shape-thin*
	  *caret-shape-half-block* *caret-shape-underline*
	  *initial-buffer-mode* *tab-columns*
	  *kinsoku-mode* *kinsoku-bol-chars* *kinsoku-eol-chars*
	  *kinsoku-extend-limit* *kinsoku-shorten-limit*
	  *fill-kinsoku-mode*
	  *file-dialog-filter* *file-dialog-filter-index*
	  *syntax-option-c-preprocessor* *syntax-option-indent-c++*
	  *syntax-option-indent-java* *syntax-option-column-comment-char*
	  *syntax-option-indent-c++/cli*
	  ))

(or (boundp 'mode-line-format)
    (setq-default mode-line-format "--%*- %b (%M) [%k:%l] %P %f"))

(defvar *auto-mode-alist* '(("\\.c$" . c-mode)
			    ("\\.h$" . detect-c-or-c++-mode)
			    ("\\.cc$" . c++-mode)
			    ("\\.cxx$" . c++-mode)
			    ("\\.cpp$" . c++-mode)
			    ("\\.hxx$" . c++-mode)
			    ("\\.hpp$" . c++-mode)
			    ("\\.inl$" . c++-mode)
			    ("\\.l$" . lisp-mode)
			    ("\\.lisp$" . lisp-mode)
			    ("\\.txt$" . text-mode)
			    ("\\.doc$" . text-mode)
			    ("\\.log$" . text-mode)
			    ("\\.html?$" . html-mode)
			    ("\\.pas$" . pascal-mode)
			    ("\\.tex$" . LaTeX-mode)
			    ("\\.java$" . java-mode)
			    ("\\.sql$" . sql-mode)
			    ("\\.idl$" . idl-mode)
			    ("\\.pl$" . perl-mode)
			    ("\\.pm$" . perl-mode)
			    ("\\.cgi$" . perl-mode)
			    ("\\.bas$" . basic-mode)
			    ("\\.mb$" . basic-mode)
			    ("\\.frm$" . basic-mode)
			    ("\\.cls$" . basic-mode)
			    ("\\.css$" . css-mode)
			    ("\\.cs$" . csharp-mode)
			   ))

(defvar show-dots nil)

(defvar ignored-extensions
  '("~" ".lc" ".obj" ".lib" ".exe" ".com" ".lzh" ".dll" ".ocx" ".fnt" ".dct"
    ".bmp" ".hlp" ".fon" ".fot" ".ttf" ".wri" ".pif" ".grp" ".wav"))

(or (boundp 'insert-default-directory)
    (setq insert-default-directory t))

(or (boundp 'mode-specific-indent-command)
    (setq mode-specific-indent-command nil))

(defvar *default-buffer-mode* 'fundamental-mode)

(or *initial-buffer-mode*
    (setq *initial-buffer-mode* 'lisp-interaction-mode))

(or *minor-mode-alist*
    (setq *minor-mode-alist*
	  '((auto-fill . "Fill")
	    (overwrite-mode . "Ovrwt"))))

(setq *ime-control* t)
(setq *drag-and-drop-auto-activate* t)

(defvar *file-dialog-filter*
  '(("ÉeÉLÉXÉgÉtÉ@ÉCÉã(*.txt;*.doc;*.log)" . "*.txt;*.doc;*.log")
    ("C/C++(*.c;*.cpp;*.cxx;*.h)" . "*.c;*.cpp;*.cxx;*.cc;*.h;*.hxx")
    ("Lisp(*.l;*.lisp)" . "*.l;*.lisp")
    ("HTML(*.htm;*.html)" . "*.htm;*.html")
    ("Ç∑Ç◊ÇƒÇÃÉtÉ@ÉCÉã(*.*)" . "*.*")))

(defvar *file-dialog-filter-index* 5)

(or *protected-local-variables*
    (setq *protected-local-variables*
	  '(kept-undo-information
	    need-not-save
	    buffer-read-only
	    auto-save
	    lock-file
	    abbrev-mode)))

(defvar *tab-columns* 8)

(defvar *kinsoku-mode* 15)
(defvar *kinsoku-bol-chars*
  "!),.:;?]}°£§•ﬁﬂÅAÅBÅCÅDÅEÅFÅGÅHÅIÅJÅKÅLÅRÅSÅTÅUÅXÅfÅhÅjÅlÅnÅpÅrÅtÅvÅxÅzÅÑÅ‚")
(defvar *kinsoku-eol-chars*
  "([{¢ÅMÅeÅgÅiÅkÅmÅoÅqÅsÅuÅwÅyÅÉÅ·")
(defvar *kinsoku-extend-limit* 3)
(defvar *kinsoku-shorten-limit* 10)
(defvar *fill-kinsoku-mode* 15)

(or (boundp '*inverse-mark-line*)
    (setq-default *inverse-mark-line* nil))

(defconstant *caret-shape-block* 0)
(defconstant *caret-shape-thin* 1)
(defconstant *caret-shape-half-block* 2)
(defconstant *caret-shape-underline* 3)

(or *normal-caret-shape*
    (setq *normal-caret-shape* *caret-shape-block*))
(or *overwrite-caret-shape*
    (setq *overwrite-caret-shape* *caret-shape-half-block*))

(or *std-control-up-char*
    (setq *std-control-up-char* #\C-p))
(or *std-control-down-char*
    (setq *std-control-down-char* #\C-n))
(or *std-control-default-char*
    (setq *std-control-default-char* #\C-m))
;; *std-control-prior-char*
;; *std-control-next-char*

(defconstant *syntax-option-c-preprocessor* 1)
(defconstant *syntax-option-indent-c++* 2)
(defconstant *syntax-option-indent-java* 4)
(defconstant *syntax-option-indent-csharp* 8)
(defconstant *syntax-option-column-comment-char* 16)
(defconstant *syntax-option-indent-c++/cli* 32)

(setq-default display-newline-char t)
(setq-default display-first-tab-char t)
(setq-default display-rest-tab-char t)

(setq-default inverse-cursor-line nil)

(export '(*archiver-dll-list* archiver-dll-file-name))

(defconstant *archiver-dll-list*
  '(:7-zip :bga32 :cab32 :ish32 :tar32 :unarj32 :ungca32
    :unlha32 :unrar32 :unzip32 :yz1 :zip32j))

(mapc #'(lambda (x)
	  (setf (get (car x) 'archiver-dll-file-name) (cdr x)))
      '((:ish32 . "ISH32.DLL")
	(:tar32 . "tar32.dll")
	(:unlha32 . "UNLHA32.DLL")
	(:unarj32 . "UNARJ32J.DLL")
	(:unzip32 . "UNZIP32.DLL")
	(:zip32j . "ZIP32J.DLL")
	(:cab32 . "cab32.dll")
	(:unrar32 . "unrar32.dll")
	(:bga32 . "Bga32.dll")
	(:yz1 . "Yz1.dll")
	(:ungca32 . "UnGCA32.dll")
	(:7-zip . "7-zip32.dll")))

(export '(make-tags-file make-tags-file-dialog *maketags-list*))
(autoload 'make-tags-file "maketags" t)
(autoload 'make-tags-file-dialog "maketags" t)
(defvar *maketags-list* nil)

(export 'hanoi)
(autoload 'hanoi "hanoi" t)

(export '(life life-mode))
(autoload 'life "life" t)
(autoload 'life-mode "life" t)

(export '(search-dialog replace-dialog count-matches-dialog
	  goto-line-dialog open-file-dialog
	  close-selected-buffer
	  close-and-open-file-dialog save-as-dialog
	  save-buffer-dialog insert-file-dialog rename-dialog
	  execute-subprocess-dialog pipe-command-dialog
	  launch-application-dialog print-selected-buffer-dialog
	  control-code-dialog-box select-xyzzy))

(autoload 'search-dialog "dialogs" t)
(autoload 'replace-dialog "dialogs" t)
(autoload 'count-matches-dialog "dialogs" t)
(autoload 'goto-line-dialog "dialogs" t)
(autoload 'open-file-dialog "dialogs" t)
(autoload 'close-and-open-file-dialog "dialogs" t)
(autoload 'save-as-dialog "dialogs" t)
(autoload 'close-selected-buffer "dialogs" t)
(autoload 'save-buffer-dialog "dialogs" t)
(autoload 'insert-file-dialog "dialogs" t)
(autoload 'rename-dialog "dialogs" t)
(autoload 'execute-subprocess-dialog "dialogs" t)
(autoload 'pipe-command-dialog "dialogs" t)
(autoload 'launch-application-dialog "dialogs" t)
(autoload 'print-selected-buffer-dialog "dialogs" t)
(autoload 'print-selected-buffer "dialogs" t)
(autoload 'control-code-dialog-box "dialogs" t)
(autoload 'select-xyzzy "dialogs" t)
(autoload 'create-xyzzy-shortcut-dialog "dialogs" t)

(export 'grep-dialog)
(autoload 'grep-dialog "grepd" t)

(export '(c-curve dragon-curve))
(autoload 'c-curve "ccurve" t)
(autoload 'dragon-curve "ccurve" t)

(export 'gomoku)
(autoload 'gomoku "gomoku" t)

(export 'view-nifty-log)
(autoload 'view-nifty-log "niftylog" t)

(export '(gresreg re-gresreg query-gresreg query-gresreg-regexp))
(autoload 'gresreg "gresreg" t)
(autoload 're-gresreg "gresreg" t)
(autoload 'query-gresreg "gresreg" t)
(autoload 'query-gresreg-regexp "gresreg" t)

(export 'gresreg-dialog)
(autoload 'gresreg-dialog "gresregd" t)

(export 'calendar)
(autoload 'calendar "calendar" t)

(export 'calc)
(autoload 'calc "calc" t)

(export 'html-mode)
(autoload 'html-mode "htmlmode" t)

(export 'pascal-mode)
(autoload 'pascal-mode "pascal" t)

(export 'LaTeX-mode)
(autoload 'LaTeX-mode "LaTeX" t)

(export 'java-mode)
(autoload 'java-mode "java" t)
(autoload 'java-maketags "javafns")
(pushnew '(java-maketags "*.java") *maketags-list* :key #'car)

(export 'sql-mode)
(autoload 'sql-mode "sql-mode" t)

(export 'idl-mode)
(autoload 'idl-mode "idl-mode" t)

(export 'view-mode)
(autoload 'view-mode "viewmode" t)

(export '(dabbrev-expand *dabbrevs-case-fold* *dabbrevs-no-undo*))
(defvar-local *dabbrevs-case-fold* nil)
(defvar *dabbrevs-no-undo* nil)
(autoload 'dabbrev-expand "dabbrev" t)
(autoload 'dabbrev-popup "dabbrev" t)
(define-key ctl-x-map #\/ 'dabbrev-expand)
(define-key ctl-x-map #\\ 'dabbrev-popup)

(export 'shell)
(autoload 'shell "shell" t)

(export 'hide-ifdef)
(autoload 'hide-ifdef "hideif" t)

(export 'perl-mode)
(autoload 'perl-mode "perl" t)
(autoload 'perl-maketags "perl")
(pushnew '(perl-maketags "*.pl" "*.pm" "*.cgi") *maketags-list* :key #'car)

(export 'basic-mode)
(autoload 'basic-mode "basic-mode" t)
(autoload 'basic-maketags "basic-mode")
(pushnew '(basic-maketags "*.bas" "*.frm" "*.mb") *maketags-list* :key #'car)

(export 'csharp-mode)
(autoload 'csharp-mode "c#-mode" t)

(export 'css-mode)
(autoload 'css-mode "css-mode" t)

(export 'list-function)
(autoload 'list-function "listfn" t)

(export '(hh-open hh-keyword-call hh-sensitive-call *hh-help-file-list*))
(autoload 'hh-open "hh-call" t)
(autoload 'hh-keyword-call "hh-call" t)
(autoload 'hh-sensitive-call "hh-call" t)

(export '(*ispell-command* *ispell-option* ispell-buffer ispell-region))
(autoload 'ispell-buffer "ispell" t)
(autoload 'ispell-region "ispell" t)

(export 'compare-windows)
(autoload 'compare-windows "comparew" t)

(export 'version-up-xyzzy)
(autoload 'version-up-xyzzy "verup" t)

(export 'diff)
(autoload 'diff "diff" t)

(export 'tail-f)
(autoload 'tail-f "tail-f" t)

(export 'show-dexplorer)
(autoload 'show-dexplorer "dexplorer" t)
