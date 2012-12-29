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


(in-package "system")

(export '(control-pressed
	  meta-pressed
	  shift-pressed

	  *vk-lbutton*
	  *vk-rbutton*
	  *vk-cancel*
	  *vk-mbutton*
	  *vk-xbutton1*
	  *vk-xbutton2*
	  *vk-back*
	  *vk-tab*
	  *vk-clear*
	  *vk-return*
	  *vk-shift*
	  *vk-control*
	  *vk-menu*
	  *vk-pause*
	  *vk-capital*
	  *vk-kana*
	  *vk-hangul*
	  *vk-junja*
	  *vk-final*
	  *vk-hanja*
	  *vk-kanji*
	  *vk-escape*
	  *vk-convert*
	  *vk-nonconvert*
	  *vk-accept*
	  *vk-modechange*
	  *vk-space*
	  *vk-prior*
	  *vk-next*
	  *vk-end*
	  *vk-home*
	  *vk-left*
	  *vk-up*
	  *vk-right*
	  *vk-down*
	  *vk-select*
	  *vk-print*
	  *vk-execute*
	  *vk-snapshot*
	  *vk-insert*
	  *vk-delete*
	  *vk-help*
	  *vk-0*
	  *vk-1*
	  *vk-2*
	  *vk-3*
	  *vk-4*
	  *vk-5*
	  *vk-6*
	  *vk-7*
	  *vk-8*
	  *vk-9*
	  *vk-a*
	  *vk-b*
	  *vk-c*
	  *vk-d*
	  *vk-e*
	  *vk-f*
	  *vk-g*
	  *vk-h*
	  *vk-i*
	  *vk-j*
	  *vk-k*
	  *vk-l*
	  *vk-m*
	  *vk-n*
	  *vk-o*
	  *vk-p*
	  *vk-q*
	  *vk-r*
	  *vk-s*
	  *vk-t*
	  *vk-u*
	  *vk-v*
	  *vk-w*
	  *vk-x*
	  *vk-y*
	  *vk-z*
	  *vk-lwin*
	  *vk-rwin*
	  *vk-apps*
	  *vk-sleep*
	  *vk-numpad0*
	  *vk-numpad1*
	  *vk-numpad2*
	  *vk-numpad3*
	  *vk-numpad4*
	  *vk-numpad5*
	  *vk-numpad6*
	  *vk-numpad7*
	  *vk-numpad8*
	  *vk-numpad9*
	  *vk-multiply*
	  *vk-add*
	  *vk-separator*
	  *vk-subtract*
	  *vk-decimal*
	  *vk-divide*
	  *vk-f1*
	  *vk-f2*
	  *vk-f3*
	  *vk-f4*
	  *vk-f5*
	  *vk-f6*
	  *vk-f7*
	  *vk-f8*
	  *vk-f9*
	  *vk-f10*
	  *vk-f11*
	  *vk-f12*
	  *vk-f13*
	  *vk-f14*
	  *vk-f15*
	  *vk-f16*
	  *vk-f17*
	  *vk-f18*
	  *vk-f19*
	  *vk-f20*
	  *vk-f21*
	  *vk-f22*
	  *vk-f23*
	  *vk-f24*
	  *vk-numlock*
	  *vk-scroll*
	  *vk-oem-nec-equal*
	  *vk-oem-fj-jisho*
	  *vk-oem-fj-masshou*
	  *vk-oem-fj-touroku*
	  *vk-oem-fj-loya*
	  *vk-oem-fj-roya*
	  *vk-lshift*
	  *vk-rshift*
	  *vk-lcontrol*
	  *vk-rcontrol*
	  *vk-lmenu*
	  *vk-rmenu*
	  *vk-browser-back*
	  *vk-browser-forward*
	  *vk-browser-refresh*
	  *vk-browser-stop*
	  *vk-browser-search*
	  *vk-browser-favorites*
	  *vk-browser-home*
	  *vk-volume-mute*
	  *vk-volume-down*
	  *vk-volume-up*
	  *vk-media-next-track*
	  *vk-media-prev-track*
	  *vk-media-stop*
	  *vk-media-play-pause*
	  *vk-launch-mail*
	  *vk-launch-media-select*
	  *vk-launch-app1*
	  *vk-launch-app2*
	  *vk-oem-1*
	  *vk-oem-plus*
	  *vk-oem-comma*
	  *vk-oem-minus*
	  *vk-oem-period*
	  *vk-oem-2*
	  *vk-oem-3*
	  *vk-oem-4*
	  *vk-oem-5*
	  *vk-oem-6*
	  *vk-oem-7*
	  *vk-oem-8*
	  *vk-oem-ax*
	  *vk-oem-102*
	  *vk-ico-help*
	  *vk-ico-00*
	  *vk-processkey*
	  *vk-ico-clear*
	  *vk-packet*
	  *vk-oem-reset*
	  *vk-oem-jump*
	  *vk-oem-pa1*
	  *vk-oem-pa2*
	  *vk-oem-pa3*
	  *vk-oem-wsctrl*
	  *vk-oem-cusel*
	  *vk-oem-attn*
	  *vk-oem-finish*
	  *vk-oem-copy*
	  *vk-oem-auto*
	  *vk-oem-enlw*
	  *vk-oem-backtab*
	  *vk-attn*
	  *vk-crsel*
	  *vk-exsel*
	  *vk-ereof*
	  *vk-play*
	  *vk-zoom*
	  *vk-noname*
	  *vk-pa1*
	  *vk-oem-clear*
	  ))

;;
;; Virtual Keys, Standard Set
;;

(defconstant *vk-lbutton*        #x01)
(defconstant *vk-rbutton*        #x02)
(defconstant *vk-cancel*         #x03)
(defconstant *vk-mbutton*        #x04)    ; NOT contiguous with L & RBUTTON

(defconstant *vk-xbutton1*       #x05)    ; NOT contiguous with L & RBUTTON
(defconstant *vk-xbutton2*       #x06)    ; NOT contiguous with L & RBUTTON

;;
;; #x07 : unassigned
;;

(defconstant *vk-back*           #x08)
(defconstant *vk-tab*            #x09)

;;
;; #x0A - #x0B : reserved
;;

(defconstant *vk-clear*          #x0C)
(defconstant *vk-return*         #x0D)

(defconstant *vk-shift*          #x10)
(defconstant *vk-control*        #x11)
(defconstant *vk-menu*           #x12)
(defconstant *vk-pause*          #x13)
(defconstant *vk-capital*        #x14)

(defconstant *vk-kana*           #x15)
(defconstant *vk-hangul*         #x15)
(defconstant *vk-junja*          #x17)
(defconstant *vk-final*          #x18)
(defconstant *vk-hanja*          #x19)
(defconstant *vk-kanji*          #x19)

(defconstant *vk-escape*         #x1B)

(defconstant *vk-convert*        #x1C)
(defconstant *vk-nonconvert*     #x1D)
(defconstant *vk-accept*         #x1E)
(defconstant *vk-modechange*     #x1F)

(defconstant *vk-space*          #x20)
(defconstant *vk-prior*          #x21)
(defconstant *vk-next*           #x22)
(defconstant *vk-end*            #x23)
(defconstant *vk-home*           #x24)
(defconstant *vk-left*           #x25)
(defconstant *vk-up*             #x26)
(defconstant *vk-right*          #x27)
(defconstant *vk-down*           #x28)
(defconstant *vk-select*         #x29)
(defconstant *vk-print*          #x2A)
(defconstant *vk-execute*        #x2B)
(defconstant *vk-snapshot*       #x2C)
(defconstant *vk-insert*         #x2D)
(defconstant *vk-delete*         #x2E)
(defconstant *vk-help*           #x2F)

;;
;; *vk-0* - *vk-9* are the same as ASCII '0' - '9' (#x30 - #x39)
(defconstant *vk-0*              #x30)
(defconstant *vk-1*              #x31)
(defconstant *vk-2*              #x32)
(defconstant *vk-3*              #x33)
(defconstant *vk-4*              #x34)
(defconstant *vk-5*              #x35)
(defconstant *vk-6*              #x36)
(defconstant *vk-7*              #x37)
(defconstant *vk-8*              #x38)
(defconstant *vk-9*              #x39)

;; #x40 : unassigned

;; *vk-a* - *vk-z* are the same as ASCII 'A' - 'Z' (#x41 - #x5A)
(defconstant *vk-a*              #x41)
(defconstant *vk-b*              #x42)
(defconstant *vk-c*              #x43)
(defconstant *vk-d*              #x44)
(defconstant *vk-e*              #x45)
(defconstant *vk-f*              #x46)
(defconstant *vk-g*              #x47)
(defconstant *vk-h*              #x48)
(defconstant *vk-i*              #x49)
(defconstant *vk-j*              #x4A)
(defconstant *vk-k*              #x4B)
(defconstant *vk-l*              #x4C)
(defconstant *vk-m*              #x4D)
(defconstant *vk-n*              #x4E)
(defconstant *vk-o*              #x4F)
(defconstant *vk-p*              #x50)
(defconstant *vk-q*              #x51)
(defconstant *vk-r*              #x52)
(defconstant *vk-s*              #x53)
(defconstant *vk-t*              #x54)
(defconstant *vk-u*              #x55)
(defconstant *vk-v*              #x56)
(defconstant *vk-w*              #x57)
(defconstant *vk-x*              #x58)
(defconstant *vk-y*              #x59)
(defconstant *vk-z*              #x5A)

(defconstant *vk-lwin*           #x5B)
(defconstant *vk-rwin*           #x5C)
(defconstant *vk-apps*           #x5D)

;;
;; #x5E : reserved
;;

(defconstant *vk-sleep*          #x5F)

(defconstant *vk-numpad0*        #x60)
(defconstant *vk-numpad1*        #x61)
(defconstant *vk-numpad2*        #x62)
(defconstant *vk-numpad3*        #x63)
(defconstant *vk-numpad4*        #x64)
(defconstant *vk-numpad5*        #x65)
(defconstant *vk-numpad6*        #x66)
(defconstant *vk-numpad7*        #x67)
(defconstant *vk-numpad8*        #x68)
(defconstant *vk-numpad9*        #x69)
(defconstant *vk-multiply*       #x6A)
(defconstant *vk-add*            #x6B)
(defconstant *vk-separator*      #x6C)
(defconstant *vk-subtract*       #x6D)
(defconstant *vk-decimal*        #x6E)
(defconstant *vk-divide*         #x6F)
(defconstant *vk-f1*             #x70)
(defconstant *vk-f2*             #x71)
(defconstant *vk-f3*             #x72)
(defconstant *vk-f4*             #x73)
(defconstant *vk-f5*             #x74)
(defconstant *vk-f6*             #x75)
(defconstant *vk-f7*             #x76)
(defconstant *vk-f8*             #x77)
(defconstant *vk-f9*             #x78)
(defconstant *vk-f10*            #x79)
(defconstant *vk-f11*            #x7A)
(defconstant *vk-f12*            #x7B)
(defconstant *vk-f13*            #x7C)
(defconstant *vk-f14*            #x7D)
(defconstant *vk-f15*            #x7E)
(defconstant *vk-f16*            #x7F)
(defconstant *vk-f17*            #x80)
(defconstant *vk-f18*            #x81)
(defconstant *vk-f19*            #x82)
(defconstant *vk-f20*            #x83)
(defconstant *vk-f21*            #x84)
(defconstant *vk-f22*            #x85)
(defconstant *vk-f23*            #x86)
(defconstant *vk-f24*            #x87)

;;
;; #x88 - #x8F : unassigned
;;

(defconstant *vk-numlock*        #x90)
(defconstant *vk-scroll*         #x91)

;;
;; NEC PC-9800 kbd definitions
;;
(defconstant *vk-oem-nec-equal*  #x92)   ; '=' key on numpad

;;
;; Fujitsu/OASYS kbd definitions
;;
(defconstant *vk-oem-fj-jisho*   #x92)   ; 'Dictionary' key
(defconstant *vk-oem-fj-masshou* #x93)   ; 'Unregister word' key
(defconstant *vk-oem-fj-touroku* #x94)   ; 'Register word' key
(defconstant *vk-oem-fj-loya*    #x95)   ; 'Left OYAYUBI' key
(defconstant *vk-oem-fj-roya*    #x96)   ; 'Right OYAYUBI' key

;;
;; #x97 - #x9F : unassigned
;;

;;
;; *vk-l** & *vk-r** - left and right Alt, Ctrl and Shift virtual keys.
;; Used only as parameters to GetAsyncKeyState() and GetKeyState().
;; No other API or message will distinguish left and right keys in this way.
;;
(defconstant *vk-lshift*         #xA0)
(defconstant *vk-rshift*         #xA1)
(defconstant *vk-lcontrol*       #xA2)
(defconstant *vk-rcontrol*       #xA3)
(defconstant *vk-lmenu*          #xA4)
(defconstant *vk-rmenu*          #xA5)

(defconstant *vk-browser-back*        #xA6)
(defconstant *vk-browser-forward*     #xA7)
(defconstant *vk-browser-refresh*     #xA8)
(defconstant *vk-browser-stop*        #xA9)
(defconstant *vk-browser-search*      #xAA)
(defconstant *vk-browser-favorites*   #xAB)
(defconstant *vk-browser-home*        #xAC)

(defconstant *vk-volume-mute*         #xAD)
(defconstant *vk-volume-down*         #xAE)
(defconstant *vk-volume-up*           #xAF)
(defconstant *vk-media-next-track*    #xB0)
(defconstant *vk-media-prev-track*    #xB1)
(defconstant *vk-media-stop*          #xB2)
(defconstant *vk-media-play-pause*    #xB3)
(defconstant *vk-launch-mail*         #xB4)
(defconstant *vk-launch-media-select* #xB5)
(defconstant *vk-launch-app1*         #xB6)
(defconstant *vk-launch-app2*         #xB7)

;;
;; #xB8 - #xB9 : reserved
;;

(defconstant *vk-oem-1*          #xBA)   ; ';:' for US
(defconstant *vk-oem-plus*       #xBB)   ; '+' any country
(defconstant *vk-oem-comma*      #xBC)   ; ',' any country
(defconstant *vk-oem-minus*      #xBD)   ; '-' any country
(defconstant *vk-oem-period*     #xBE)   ; '.' any country
(defconstant *vk-oem-2*          #xBF)   ; '/?' for US
(defconstant *vk-oem-3*          #xC0)   ; '`~' for US

;;
;; #xC1 - #xD7 : reserved
;;

;;
;; #xD8 - #xDA : unassigned
;;

(defconstant *vk-oem-4*          #xDB)  ;  '[{' for US
(defconstant *vk-oem-5*          #xDC)  ;  '\|' for US
(defconstant *vk-oem-6*          #xDD)  ;  ']}' for US
(defconstant *vk-oem-7*          #xDE)  ;  ''"' for US
(defconstant *vk-oem-8*          #xDF)

;;
;; #xE0 : reserved
;;

;;
;; Various extended or enhanced keyboards
;;
(defconstant *vk-oem-ax*         #xE1)  ;  'AX' key on Japanese AX kbd
(defconstant *vk-oem-102*        #xE2)  ;  "<>" or "\|" on RT 102-key kbd.
(defconstant *vk-ico-help*       #xE3)  ;  Help key on ICO
(defconstant *vk-ico-00*         #xE4)  ;  00 key on ICO
(defconstant *vk-processkey*     #xE5)
(defconstant *vk-ico-clear*      #xE6)
(defconstant *vk-packet*         #xE7)

;;
;; #xE8 : unassigned
;;

;;
;; Nokia/Ericsson definitions
;;
(defconstant *vk-oem-reset*      #xE9)
(defconstant *vk-oem-jump*       #xEA)
(defconstant *vk-oem-pa1*        #xEB)
(defconstant *vk-oem-pa2*        #xEC)
(defconstant *vk-oem-pa3*        #xED)
(defconstant *vk-oem-wsctrl*     #xEE)
(defconstant *vk-oem-cusel*      #xEF)
(defconstant *vk-oem-attn*       #xF0)
(defconstant *vk-oem-finish*     #xF1)
(defconstant *vk-oem-copy*       #xF2)
(defconstant *vk-oem-auto*       #xF3)
(defconstant *vk-oem-enlw*       #xF4)
(defconstant *vk-oem-backtab*    #xF5)

(defconstant *vk-attn*           #xF6)
(defconstant *vk-crsel*          #xF7)
(defconstant *vk-exsel*          #xF8)
(defconstant *vk-ereof*          #xF9)
(defconstant *vk-play*           #xFA)
(defconstant *vk-zoom*           #xFB)
(defconstant *vk-noname*         #xFC)
(defconstant *vk-pa1*            #xFD)
(defconstant *vk-oem-clear*      #xFE)

;;
;; #xFF : reserved
;;


(defun control-pressed ()
  (si:get-key-state si:*vk-control*))

(defun meta-pressed ()
  (si:get-key-state si:*vk-menu*))

(defun shift-pressed ()
  (si:get-key-state si:*vk-shift*))
