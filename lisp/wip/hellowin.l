;;; -*- Mode: Lisp; Package: WIN-USER -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "wip/winapi"))

(in-package "win-user")

(defconstant hello-windows-class "HelloWindowsClass")

(defun on-paint (hwnd hdc)
  (let* ((r (make-RECT))
	 (sz (make-SIZE))
	 (text (si:make-string-chunk "はろ～ ういんど～ず"))
	 (l (- (si:chunk-size text) 1))
	 ofont)
    (let ((lf (make-LOGFONT)))
      ;; memset (&lf, 0, sizeof lf)
      (si:clear-chunk lf)
      (setf (LOGFONT-lfHeight lf) 64)
      (setf (LOGFONT-lfCharSet lf) SHIFTJIS_CHARSET)
      ;; strcpy (lf.lfFaceName, "...")
      (si:pack-string lf (c-struct-offset-of LOGFONT winapi::lfFaceName)
		      "ＭＳ Ｐゴシック")
      (setq ofont (SelectObject hdc (CreateFontIndirect lf))))
    (GetClientRect hwnd r)
    (GetTextExtentPoint32 hdc text l sz)
    (ExtTextOut hdc
		(truncate (- (RECT-right r) (SIZE-cx sz)) 2)
		(truncate (- (RECT-bottom r) (SIZE-cy sz)) 2)
		0 0 text l 0)
    (DeleteObject (SelectObject hdc ofont))))

;; 複数回評価すると、以前に RegisterClass した WndProc が GC されてなく
;; なってしまう。
;; 再定義する場合は UnregisterClass するか、xyzzy を立ちあげ直す。
(unless (fboundp 'hello-windows-wndproc)
  (defun-c-callable LRESULT hello-windows-wndproc
    ((HWND hwnd) (UINT msg) (WPARAM wparam) (LPARAM lparam))
    (cond ((= msg WM_NCDESTROY)
	   (setq break-loop t))
	  ((= msg WM_PAINT)
	   (let* ((ps (make-PAINTSTRUCT))
		  (hdc (BeginPaint hwnd ps)))
	     (on-paint hwnd hdc)
	     (EndPaint hwnd ps))
	   (return-from hello-windows-wndproc 0))
    )
    (DefWindowProc hwnd msg wparam lparam)))

(defun user::hello-windows ()
  (let ((wc (make-WNDCLASS)))
    (setf (WNDCLASS-style wc) (logior CS_HREDRAW CS_VREDRAW))
    (setf (WNDCLASS-lpfnWndProc wc) #'hello-windows-wndproc)
    (setf (WNDCLASS-cbClsExtra wc) 0)
    (setf (WNDCLASS-cbWndExtra wc) 0)
    (setf (WNDCLASS-hInstance wc) (GetModuleHandle 0))
    (setf (WNDCLASS-hIcon wc) (LoadIcon 0 (MAKEINTRESOURCE IDI_APPLICATION)))
    (setf (WNDCLASS-hCursor wc) (LoadCursor 0 (MAKEINTRESOURCE IDC_ARROW)))
    (setf (WNDCLASS-hbrBackground wc) (+ 1 COLOR_WINDOW))
    (setf (WNDCLASS-lpszMenuName wc) 0)
    (setf (WNDCLASS-lpszClassName wc) (si:make-string-chunk hello-windows-class))
    (RegisterClass wc))

  (let ((hwnd (CreateWindow (si:make-string-chunk hello-windows-class)
			    (si:make-string-chunk "Hello Windows")
			    WS_OVERLAPPEDWINDOW
			    CW_USEDEFAULT CW_USEDEFAULT
			    CW_USEDEFAULT CW_USEDEFAULT
			    0 0 (GetModuleHandle 0) 0)))
    (ShowWindow hwnd SW_SHOW)
    (UpdateWindow hwnd)
    (let ((msg (make-MSG))
	  (break-loop nil))
      (declare (special break-loop))
      #|
      ;; ここでメインループをまわしてもいいんだが、
      ;; editor側で何も出来なくなってしまうので
      (while (not break-loop)
	(GetMessage msg 0 0 0)
	(TranslateMessage msg)
	(DispatchMessage msg))
      |#
    )))

