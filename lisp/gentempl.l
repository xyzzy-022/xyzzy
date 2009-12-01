;;; NAME
;;;	  convert-dialog-template, convert-dialog-template-buffer
;;;
;;; SYNOPSIS
;;;	  リソーススクリプト(*.rc)から xyzzy の dialog-box で使用できる
;;;	形式のコードをでっち上げます。
;;;
;;; DESCRIPTION
;;;	  Developper Studio 等のダイアログエディタで *.rc ファイルを作り、
;;;	IDD_... DIALOG ... END の部分を切り出してこいつに食わせると、
;;;	dialog-box の形式のリストをバッファに出力します。
;;;
;;; BUGS
;;;	  作りがてけとーなので、それなりにしか動かない。
;;;

(in-package "editor")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(convert-dialog-template convert-dialog-template-buffer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "gentempl")
    (defpackage "gentempl" (:use "lisp" "editor"))))

(in-package "gentempl")

(defconstant *keywords*
  '((DIALOG :dialog)
    (PRELOAD :preload)
    (LOADONCALL :loadoncall)
    (FIXED :fixed)
    (MOVEABLE :moveable)
    (DISCARDABLE :discardable)
    (STYLE :style)
    (CAPTION :caption)
    (FONT :font)
    (BEGIN :begin)
    (END :end)
    ({ :begin)
    (} :end)
    (CONTROL :control)
    (COMBOBOX :combobox 1342177280 nil)      ; WS_VISIBLE | WS_CHILD
    (EDITTEXT :edit 1350631424 nil)          ; WS_VISIBLE | WS_CHILD | WS_BORDER | WS_TABSTOP
    (LISTBOX :listbox 1350565889 nil)        ; WS_VISIBLE | WS_CHILD | WS_BORDER | LBS_NOTIFY
    (CHECKBOX :button 1342242818 t)          ; WS_VISIBLE | WS_CHILD | WS_TABSTOP | BS_CHECKBOX
    (CTEXT :static 1342308353 t)             ; WS_VISIBLE | WS_CHILD | WS_GROUP | SS_CENTER
    (DEFPUSHBUTTON :button 1342242817 t)     ; WS_VISIBLE | WS_CHILD | WS_TABSTOP | BS_DEFPUSHBUTTON
    (GROUPBOX :button 1342177287 t)          ; WS_VISIBLE | WS_CHILD | BS_GROUPBOX
    (LTEXT :static 1342308352 t)             ; WS_VISIBLE | WS_CHILD | WS_GROUP | SS_LEFT
    (PUSHBUTTON :button 1342242816 t)        ; WS_VISIBLE | WS_CHILD | WS_TABSTOP | BS_PUSHBUTTON
    (RADIOBUTTON :button 1342177284 t)       ; WS_VISIBLE | WS_CHILD | BS_RADIOBUTTON
    (RTEXT :static 1342308354 t)             ; WS_VISIBLE | WS_CHILD | WS_GROUP | SS_RIGHT
    (ICON :static 1342308355 t)              ; WS_VISIBLE | WS_CHILD | WS_GROUP | SS_ICON
    (SCROLLBAR :scrollbar 1342177280 nil)))  ; WS_VISIBLE | WS_CHILD | SBS_HORZ

(defconstant *styles*
  '((DS_ABSALIGN . 1)
    (DS_SYSMODAL . 2)
    (DS_LOCALEDIT . 32)
    (DS_SETFONT . 64)
    (DS_MODALFRAME . 128)
    (DS_NOIDLEMSG . 256)
    (WS_OVERLAPPED . 0)
    (WS_POPUP . -2147483648)
    (WS_CHILD . 1073741824)
    (WS_CLIPSIBLINGS . 67108864)
    (WS_CLIPCHILDREN . 33554432)
    (WS_VISIBLE . 268435456)
    (WS_DISABLED . 134217728)
    (WS_MINIMIZE . 536870912)
    (WS_MAXIMIZE . 16777216)
    (WS_CAPTION . 12582912)
    (WS_BORDER . 8388608)
    (WS_DLGFRAME . 4194304)
    (WS_VSCROLL . 2097152)
    (WS_HSCROLL . 1048576)
    (WS_SYSMENU . 524288)
    (WS_THICKFRAME . 262144)
    (WS_MINIMIZEBOX . 131072)
    (WS_MAXIMIZEBOX . 65536)
    (WS_OVERLAPPEDWINDOW . 13565952)
    (WS_POPUPWINDOW . -2138570752)
    (WS_CHILDWINDOW . 1073741824)
    (WS_GROUP . 131072)
    (WS_TABSTOP . 65536)
    (SS_LEFT . 0)
    (SS_CENTER . 1)
    (SS_RIGHT . 2)
    (SS_ICON . 3)
    (SS_BLACKRECT . 4)
    (SS_GRAYRECT . 5)
    (SS_WHITERECT . 6)
    (SS_BLACKFRAME . 7)
    (SS_GRAYFRAME . 8)
    (SS_WHITEFRAME . 9)
    (SS_SIMPLE . 11)
    (SS_LEFTNOWORDWRAP . 12)
    (SS_OWNERDRAW . 13)
    (SS_ENHMETAFILE . 15)
    (SS_ETCHEDHORZ . 16)
    (SS_ETCHEDVERT . 17)
    (SS_ETCHEDFRAME . 18)
    (SS_NOPREFIX . 128)
    (SS_CENTERIMAGE . 512)
    (SS_RIGHTJUST . 1024)
    (SS_REALSIZEIMAGE . 2048)
    (SS_SUNKEN . 4096)
    (BS_PUSHBUTTON . 0)
    (BS_DEFPUSHBUTTON . 1)
    (BS_CHECKBOX . 2)
    (BS_AUTOCHECKBOX . 3)
    (BS_RADIOBUTTON . 4)
    (BS_3STATE . 5)
    (BS_AUTO3STATE . 6)
    (BS_GROUPBOX . 7)
    (BS_USERBUTTON . 8)
    (BS_AUTORADIOBUTTON . 9)
    (BS_OWNERDRAW . 11)
    (BS_LEFTTEXT . 32)
    (ES_LEFT . 0)
    (ES_CENTER . 1)
    (ES_RIGHT . 2)
    (ES_MULTILINE . 4)
    (ES_UPPERCASE . 8)
    (ES_LOWERCASE . 16)
    (ES_PASSWORD . 32)
    (ES_AUTOVSCROLL . 64)
    (ES_AUTOHSCROLL . 128)
    (ES_NOHIDESEL . 256)
    (ES_OEMCONVERT . 1024)
    (ES_READONLY . 2048)
    (ES_WANTRETURN . 4096)
    (LBS_NOTIFY . 1)
    (LBS_SORT . 2)
    (LBS_NOREDRAW . 4)
    (LBS_MULTIPLESEL . 8)
    (LBS_OWNERDRAWFIXED . 16)
    (LBS_OWNERDRAWVARIABLE . 32)
    (LBS_HASSTRINGS . 64)
    (LBS_USETABSTOPS . 128)
    (LBS_NOINTEGRALHEIGHT . 256)
    (LBS_MULTICOLUMN . 512)
    (LBS_WANTKEYBOARDINPUT . 1024)
    (LBS_EXTENDEDSEL . 2048)
    (LBS_DISABLENOSCROLL . 4096)
    (LBS_STANDARD . 10485763)
    (CBS_SIMPLE . 1)
    (CBS_DROPDOWN . 2)
    (CBS_DROPDOWNLIST . 3)
    (CBS_OWNERDRAWFIXED . 16)
    (CBS_OWNERDRAWVARIABLE . 32)
    (CBS_AUTOHSCROLL . 64)
    (CBS_OEMCONVERT . 128)
    (CBS_SORT . 256)
    (CBS_HASSTRINGS . 512)
    (CBS_NOINTEGRALHEIGHT . 1024)
    (CBS_DISABLENOSCROLL . 2048)
    (SBS_HORZ . 0)
    (SBS_VERT . 1)
    (SBS_TOPALIGN . 2)
    (SBS_LEFTALIGN . 2)
    (SBS_BOTTOMALIGN . 4)
    (SBS_RIGHTALIGN . 4)
    (SBS_SIZEBOXTOPLEFTALIGN . 2)
    (SBS_SIZEBOXBOTTOMRIGHTALIGN . 4)
    (SBS_SIZEBOX . 8)
    (UDS_WRAP . 1)
    (UDS_SETBUDDYINT . 2)
    (UDS_ALIGNRIGHT . 4)
    (UDS_ALIGNLEFT . 8)
    (UDS_AUTOBUDDY . 16)
    (UDS_ARROWKEYS . 32)
    (UDS_HORZ . 64)
    (UDS_NOTHOUSANDS . 128)))

(defconstant *known-class*
  '(("button" . :button)
    ("edit" . :edit)
    ("static" . :static)
    ("listbox" . :listbox)
    ("scrollbar" . :scrollbar)
    ("combobox" . :combobox)
    ("msctls_updown32" . :spin)))

(defconstant DS_SETFONT (cdr (assoc 'DS_SETFONT *styles*)))
(defconstant WS_VISIBLE\|WS_CHILD (+ (cdr (assoc 'WS_VISIBLE *styles*))
				     (cdr (assoc 'WS_CHILD *styles*))))

(defvar *line-number* nil)
(defvar *file-name* nil)

(defun getc ()
  (let ((c (read-char nil nil nil)))
    (when (eql c #\LFD)
      (incf *line-number*))
    c))

(defun ungetc (c)
  (when (eql c #\LFD)
    (decf *line-number*))
  (unread-char c))

(defun parse-error (&rest args)
  (error (concatenate 'string
		      (format nil "~a: ~d: " *file-name* *line-number*)
		      (apply #'format nil args)
		      ".")))

(defun skip-c-comment ()
  (loop
    (loop
      (let ((c (getc)))
	(unless c
	  (parse-error "Unterminated comment"))
	(when (char= c #\*)
	  (return))))
    (loop
      (let ((c (getc)))
	(unless c
	  (parse-error "Unterminated comment"))
	(when (char= c #\/)
	  (return-from skip-c-comment c))
	(unless (char= c #\*)
	  (return))))))

(defun skip-white ()
  (let ((lc nil)
	(cpp nil))
    (loop
      (let ((c (getc)))
	(case c
	  ((#\SPC #\RET #\TAB #\C-l))
	  (#\LFD
	   (unless (eql lc #\\)
	     (setq cpp nil)))
	  (#\/
	   (setq c (getc))
	   (case c
	     (#\*
	      (setq c (skip-c-comment)))
	     (#\/
	      (loop
		(setq lc c)
		(setq c (getc))
		(unless c
		  (return-from skip-white nil))
		(when (and (char= c #\LFD)
			   (char/= lc #\\))
		  (return)))
	      (setq cpp nil))
	     (t (ungetc c))))
	  (#\#
	   (setq cpp t))
	  (nil
	   (return-from skip-white nil))
	  (t
	   (unless cpp
	     (return c))))
	(setq lc c)))))

(defun extract-string ()
  (let ((string (make-vector 32 :element-type 'character
			     :fill-pointer 0 :adjustable t)))
    (loop
      (let ((c (getc)))
	(unless c
	  (parse-error "Unexpected EOF"))
	(when (char= c #\")
	  (setq c (getc))
	  (unless (char= c #\")
	    (unless (char= c #\,)
	      (ungetc c))
	    (return)))
	(vector-push-extend c string)))
    string))

(let (last-token)
  (defun read-token ()
    (when last-token
      (return-from read-token (prog1
				  last-token
				(setq last-token nil))))
    (let ((c (skip-white)))
      (unless c
	(return-from read-token nil))
      (when (char= c #\")
	(return-from read-token (extract-string)))
      (let ((string (make-vector 32 :element-type 'character
				 :fill-pointer 0 :adjustable t)))
	(loop
	  (vector-push-extend c string)
	  (setq c (getc))
	  (when (member c '(nil #\SPC #\LFD #\RET #\TAB #\C-l #\,))
	    (return))
	  (unless (or (alphanumericp c)
		      (member c '(#\_ #\-)))
	    (ungetc c)
	    (return)))
	(if (string-match "^[0-9-]" string)
	    (parse-integer string)
	  (intern string)))))

  (defun unread-token (x)
    (setq last-token x))
  )

(defun expect-keyword (&optional token)
  (or token
      (setq token (read-token)))
  (let ((x (assoc token *keywords* :test #'eq)))
    (unless x
      (parse-error "Syntax error: keyword expected: ~S" token))
    x))

(defun expect-integer ()
  (let ((x (read-token)))
    (unless (integerp x)
      (parse-error "Syntax error: integer expected: ~S" x))
    x))

(defun expect-string ()
  (let ((x (read-token)))
    (unless (stringp x)
      (parse-error "Syntax error: string expected: ~S" x))
    x))

(defun extract-class ()
  (let* ((s (expect-string))
	 (class (assoc s *known-class* :test #'string-equal)))
    (unless class
      (parse-error "Unknown class: ~A" s))
    (cdr class)))

(defun extract-style (style empty-ok)
  (let ((not nil))
    (loop
      (let ((token (read-token)))
	(cond ((integerp token)
	       (setq style (if not
			       (logand style (lognot token))
			     (logior style token)))
	       (setq not nil)
	       (setq empty-ok nil))
	      ((symbolp token)
	       (let ((x (assoc token *styles* :test #'eq)))
		 (cond (x
			(if not
			    (setq style (logand style (lognot (cdr x))))
			  (setq style (logior style (cdr x))))
			(setq not nil))
		       ((eq token 'NOT)
			(setq not t))
		       (empty-ok
			(unread-token token)
			(return))
		       (t
			(parse-error "Unknown style: " token))))
	       (setq empty-ok nil))
	      (t
	       (when empty-ok
		 (unread-token token)
		 (return))
	       (parse-error "Syntax error: style expected: ~S" token)))
	(unless not
	  (setq token (read-token))
	  (unless (eq token '\|)
	    (unread-token token)
	    (return)))))
    style))

(defun extract-dialog ()
  (let ((dialog-id (read-token)))
    (unless dialog-id
      (return-from extract-dialog nil))
    (unless (eq (car (expect-keyword)) 'DIALOG)
      (parse-error "Syntax error: DIALOG expected"))
    (let ((x 0)
	  (y 0)
	  (w 0)
	  (h 0)
	  (style 0)
	  (caption nil)
	  size
	  (font nil))
      (loop
	(let ((token (read-token)))
	  (cond ((stringp token)
		 (parse-error "Unexpected string"))
		((integerp token)
		 (setq x token)
		 (setq y (expect-integer))
		 (setq w (expect-integer))
		 (setq h (expect-integer))
		 (return))
		((symbolp token))
		(t
		 (parse-error "Unexpected EOF")))))
      (loop
	(let ((i (car (expect-keyword))))
	  (cond ((eq i 'STYLE)
		 (setq style (extract-style 0 nil)))
		((eq i 'CAPTION)
		 (setq caption (expect-string)))
		((eq i 'FONT)
		 (setq size (expect-integer))
		 (setq font (expect-string)))
		((or (eq i 'BEGIN)
		     (eq i '{))
		 (return))
		(t
		 (parse-error "Syntax error: dialog options expected: ~S" i)))))
      (setq style (if font
		      (logior style DS_SETFONT)
		    (logand style (lognot DS_SETFONT))))
      (format t ";; ~A~%" dialog-id)
      (format t "(dialog ~d ~d ~d ~d" x y w h)
      (when caption
	(format t "~%  (:caption ~S)" caption))
      (when font
	(format t "~%  (:font ~d ~S)" size font))))
  t)

(defun extract-control ()
  (format t "~%  (:control")
  (loop
    (let ((token (read-token)))
      (cond ((null token)
	     (parse-error "Unexpected EOF"))
	    ((symbolp token)
	     (let (style text id class x y w h)
	       (setq token (expect-keyword token))
	       (cond ((eq (cadr token) :end)
		      (return))
		     ((eq (car token) 'CONTROL)
		      (setq style WS_VISIBLE\|WS_CHILD)
		      (setq text (expect-string))
		      (setq id (read-token))
		      (setq class (extract-class))
		      (setq style (extract-style style nil))
		      (setq x (expect-integer))
		      (setq y (expect-integer))
		      (setq w (expect-integer))
		      (setq h (expect-integer)))
		     ((= (length token) 4)
		      (setq class (cadr token))
		      (setq style (caddr token))
		      (when (cadddr token)
			(setq text (expect-string)))
		      (setq id (read-token))
		      (setq x (expect-integer))
		      (setq y (expect-integer))
		      (setq w (expect-integer))
		      (setq h (expect-integer))
		      (setq style (extract-style style t)))
		     (t
		      (parse-error "Unexpected: ~A" token)))
	       (when (or (eq id 'IDC_STATIC)
			 (eql id -1))
		 (setq id nil))
	       (format t "~%    (~S ~A " class id)
	       (if (and text
			(not (zerop (length text))))
		   (format t "~S" text)
		 (format t "nil"))
	       (format t " #x~8,'0x ~d ~d ~d ~d)" style x y w h)))
	    (t
	     (parse-error "Syntax error: control expected: ~S" token)))))
  (format t "))~%~%"))

(defun convert-dialog-template-1 (name)
  (with-output-to-temp-buffer ("*dialog template*")
    (let ((*package* (find-package "gentempl"))
	  (*file-name* name)
	  (*line-number* 1))
      (while (extract-dialog)
	(extract-control)))))

(defun convert-dialog-template (filename)
  (interactive "fmake dialog template: ")
  (with-open-file (s filename :direction :input :if-does-not-exist :error)
    (let ((*standard-input* s))
      (convert-dialog-template-1 filename))))

(defun convert-dialog-template-buffer (buffer)
  (interactive "bmake dialog template: ")
  (with-input-from-buffer (buffer)
    (convert-dialog-template-1 (buffer-name buffer))))
