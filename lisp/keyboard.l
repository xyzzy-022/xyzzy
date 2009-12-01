;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "keyboard")

(in-package "editor")

(export '(set-extended-key-translate-table
	  unset-extended-key-translate-table
	  exkey-backspace exkey-S-backspace exkey-C-backspace
	  exkey-S-C-backspace exkey-M-backspace exkey-S-M-backspace
	  exkey-C-M-backspace exkey-S-C-M-backspace
	  exkey-tab exkey-S-tab exkey-C-tab exkey-S-C-tab
	  exkey-return exkey-S-return exkey-C-return exkey-S-C-return
	  exkey-M-return exkey-S-M-return exkey-C-M-return exkey-S-C-M-return
	  exkey-escape exkey-S-escape
	  exkey-zenkaku exkey-S-zenkaku exkey-C-zenkaku
	  exkey-S-C-zenkaku exkey-S-M-zenkaku
	  exkey-space exkey-S-space exkey-C-space exkey-S-C-space
	  exkey-M-space exkey-S-M-space exkey-C-M-space exkey-S-C-M-space))

(unless *kbd-translate-table*
  (setq *kbd-translate-table* (make-vector 128))
  (dotimes (x 128)
    (setf (svref *kbd-translate-table* x) (code-char x))))

(let ()
  (defconstant exkey-backspace 0)
  (defconstant exkey-S-backspace 1)
  (defconstant exkey-C-backspace 2)
  (defconstant exkey-S-C-backspace 3)
  (defconstant exkey-M-backspace 4)
  (defconstant exkey-S-M-backspace 5)
  (defconstant exkey-C-M-backspace 6)
  (defconstant exkey-S-C-M-backspace 7)
  (defconstant exkey-tab 8)
  (defconstant exkey-S-tab 9)
  (defconstant exkey-C-tab 10)
  (defconstant exkey-S-C-tab 11)
  (defconstant exkey-return 16)
  (defconstant exkey-S-return 17)
  (defconstant exkey-C-return 18)
  (defconstant exkey-S-C-return 19)
  (defconstant exkey-M-return 20)
  (defconstant exkey-S-M-return 21)
  (defconstant exkey-C-M-return 22)
  (defconstant exkey-S-C-M-return 23)
  (defconstant exkey-escape 24)
  (defconstant exkey-S-escape 25)
  (defconstant exkey-zenkaku 32)
  (defconstant exkey-S-zenkaku 33)
  (defconstant exkey-C-zenkaku 34)
  (defconstant exkey-S-C-zenkaku 35)
  (defconstant exkey-S-M-zenkaku 37)
  (defconstant exkey-space 40)
  (defconstant exkey-S-space 41)
  (defconstant exkey-C-space 42)
  (defconstant exkey-S-C-space 43)
  (defconstant exkey-M-space 44)
  (defconstant exkey-S-M-space 45)
  (defconstant exkey-C-M-space 46)
  (defconstant exkey-S-C-M-space 47))

(or *extended-key-translate-table*
    (setq *extended-key-translate-table* (make-vector 48)))

(defun set-extended-key-translate-table (key c)
  (setf (svref *extended-key-translate-table* key) c))

(defun unset-extended-key-translate-table (key)
  (set-extended-key-translate-table key nil))

(set-extended-key-translate-table exkey-tab #\TAB)
(set-extended-key-translate-table exkey-backspace #\C-h)
(set-extended-key-translate-table exkey-M-backspace #\C-M-h)
(set-extended-key-translate-table exkey-return #\RET)
(set-extended-key-translate-table exkey-C-return #\LFD)
(set-extended-key-translate-table exkey-escape #\ESC)
(set-extended-key-translate-table exkey-space #\SPC)
(set-extended-key-translate-table exkey-S-space #\SPC)
(set-extended-key-translate-table exkey-C-space #\NUL)
(set-extended-key-translate-table exkey-S-C-space #\NUL)
(set-extended-key-translate-table exkey-M-space #\M-SPC)
(set-extended-key-translate-table exkey-S-M-space #\M-SPC)
(set-extended-key-translate-table exkey-C-M-space #\M-NUL)
(set-extended-key-translate-table exkey-S-C-M-space #\M-NUL)

(unless *keyboard-layout-list*
  (setq *keyboard-layout-list*
	'((#x11 * *encoding-sjis*)		; Japanese
	  (#x12 * *encoding-euc-kr*)		; Korean
	  (#x04 1 *encoding-big5*)		; Chinese (Traditional)
	  (#x04 2 *encoding-euc-gb*)		; Chinese (Simplified)
	  (#x04 3 *encoding-big5*)		; Chinese (Hong Kong) ?
	  (#x04 4 *encoding-euc-gb*)		; Chinese (Singapore) ?
	  (#x1a 2 *encoding-windows-latin2*)	; Croatian
	  (#x05 * *encoding-windows-latin2*)	; Czech
	  (#x0e * *encoding-windows-latin2*)	; Hungarian
	  (#x15 * *encoding-windows-latin2*)	; Polish
	  (#x18 * *encoding-windows-latin2*)	; Romanian
	  (#x1b * *encoding-windows-latin2*)	; Slovak
	  (#x24 * *encoding-windows-latin2*)	; Slovenian
	  (#x23 * *encoding-windows-cyrillic*)	; Belarusian ?
	  (#x02 * *encoding-windows-cyrillic*)	; Bulgarian
	  (#x19 * *encoding-windows-cyrillic*)	; Russian
	  (#x1a 3 *encoding-windows-cyrillic*)	; Serbian
	  (#x22 * *encoding-windows-cyrillic*)	; Ukrainian
	  (#x08 * *encoding-windows-greek*)	; Greek
	  (#x1f * *encoding-windows-turkish*)	; Turkish
	  (#x25 * *encoding-windows-baltic*)	; Estonian
	  (#x26 * *encoding-windows-baltic*)	; Latvian
	  (#x27 * *encoding-windows-baltic*)	; Lithuanian
	  (#x37 * nil)				; Georgian

	  ;(#x36 * *encoding-windows-latin1*)	; Afrikaans
	  ;(#x1c * *encoding-windows-latin1*)	; Albanian
	  ;(#x2d * *encoding-windows-latin1*)	; Basque
	  ;(#x03 * *encoding-windows-latin1*)	; Catalan
	  ;(#x06 * *encoding-windows-latin1*)	; Danish
	  ;(#x13 * *encoding-windows-latin1*)	; Dutch
	  ;(#x09 * *encoding-windows-latin1*)	; English
	  ;(#x38 * *encoding-windows-latin1*)	; Faeroese
	  ;(#x0b * *encoding-windows-latin1*)	; Finnish
	  ;(#x0c * *encoding-windows-latin1*)	; French
	  ;(#x07 * *encoding-windows-latin1*)	; German
	  ;(#x0f * *encoding-windows-latin1*)	; Icelandic
	  ;(#x10 * *encoding-windows-latin1*)	; Italian
	  ;(#x14 * *encoding-windows-latin1*)	; Norwegian
	  ;(#x16 * *encoding-windows-latin1*)	; Portuguese
	  ;(#x0a * *encoding-windows-latin1*)	; Spanish
	  ;(#x1d * *encoding-windows-latin1*)	; Swedish
	  ;(#x01 * *encoding-windows-arabic*)	; Arabic
	  ;(#x0d * *encoding-windows-hebrew*)	; Hebrew
	  ;(#x2a * *encoding-windows-vietnamese*)	; Vietnamese
	  ;(#x29 * ?)				; Farsi
	  ;(#x21 * ?)				; Indonesian
	  ;(#x1e * ?)				; Thai
	 )))
