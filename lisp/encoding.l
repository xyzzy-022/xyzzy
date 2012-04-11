;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "encoding")

(in-package "editor")

(export '(*encoding-iso-8bit-jp* *encoding-iso-8bit-jp-open*
	  *encoding-iso-8bit-jp-necext* *encoding-iso-8bit-jp-ibmext*
	  *encoding-jis* *encoding-jis-open*
	  *encoding-jis-necext* *encoding-jis-ibmext*
	  *encoding-iso-2022-jp-2* *encoding-iso-2022-jp-2-open*
	  *encoding-iso-2022-jp-2-necext* *encoding-iso-2022-jp-2-ibmext*
	  *encoding-jis-sio* *encoding-jis8*
	  *encoding-big5* *encoding-binary*
	  *encoding-euc-jp* *encoding-euc-jp-open*
	  *encoding-euc-jp-necext* *encoding-euc-jp-ibmext*
	  *encoding-euc-gb* *encoding-iso-2022-cn* *encoding-iso-8bit-cn*
	  *encoding-euc-kr* *encoding-iso-2022-kr* *encoding-iso-8bit-kr*
	  *encoding-iso-8859-1* *encoding-iso-8859-2*
	  *encoding-iso-8859-3* *encoding-iso-8859-4* *encoding-iso-8859-5*
	  *encoding-iso-8859-7* *encoding-iso-8859-9* *encoding-iso-8859-10*
	  *encoding-iso-8859-13*
	  *encoding-windows-latin2* *encoding-windows-cyrillic* *encoding-windows-greek*
	  *encoding-windows-turkish* *encoding-windows-baltic* *encoding-utf16*
	  *encoding-utf16* *encoding-utf16le-bom* *encoding-utf16be-bom*
	  *encoding-utf16le* *encoding-utf16be* *encoding-utf8* *encoding-utf8n*
	  *encoding-utf7* *encoding-utf5* *encoding-imap4*
	  *encoding-std-utf16* *encoding-std-utf16le-bom* *encoding-std-utf16be-bom*
	  *encoding-std-utf16le* *encoding-std-utf16be* *encoding-std-utf8*
	  *encoding-std-utf8n* *encoding-std-utf7*
	  *encoding-std-utf5* *encoding-std-imap4*
	  *encoding-koi8r* *encoding-koi8u*
	  *encoding-utf16-kr* *encoding-utf16le-bom-kr* *encoding-utf16be-bom-kr*
	  *encoding-utf16le-kr* *encoding-utf16be-kr* *encoding-utf8-kr*
	  *encoding-utf8n-kr* *encoding-utf7-kr* *encoding-utf5-kr*
	  *encoding-imap4-kr*
	  *encoding-utf16-gb* *encoding-utf16le-bom-gb* *encoding-utf16be-bom-gb*
	  *encoding-utf16le-gb* *encoding-utf16be-gb* *encoding-utf8-gb*
	  *encoding-utf8n-gb* *encoding-utf7-gb* *encoding-utf5-gb*
	  *encoding-imap4-gb*
	  *encoding-utf16-big5* *encoding-utf16le-bom-big5* *encoding-utf16be-bom-big5*
	  *encoding-utf16le-big5* *encoding-utf16be-big5* *encoding-utf8-big5*
	  *encoding-utf8n-big5* *encoding-utf7-big5* *encoding-utf5-big5*
	  *encoding-imap4-big5*
	  *encoding-utf16-cn* *encoding-utf16le-bom-cn* *encoding-utf16be-bom-cn*
	  *encoding-utf16le-cn* *encoding-utf16be-cn* *encoding-utf8-cn*
	  *encoding-utf8n-cn* *encoding-utf7-cn* *encoding-utf5-cn*
	  *encoding-imap4-cn*
	  *encoding-default-utf16* *encoding-default-utf16le-bom*
	  *encoding-default-utf16be-bom* *encoding-default-utf16le*
	  *encoding-default-utf16be* *encoding-default-utf8*
	  *encoding-default-utf8n* *encoding-default-utf7*
	  *encoding-default-utf5* *encoding-default-imap4*
	  *eol-lf* *eol-crlf* *eol-cr* *eol-guess*
	  *char-encoding-tree*
	  *clipboard-char-encoding-list*
	  *character-set-alist* *mime-charset-name-hash-table*
	  insert-internal-char-table insert-unicode-char-table))

(defvar *encoding-euc-jp*
  (make-iso2022-encoding "euc-jp" "日本語(EUC-JP)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :no-escape t :cjk :jp))

(defvar *encoding-euc-jp-open*
  (make-iso2022-encoding "eucjp-open" "日本語(EUC-JP:OSF/JVC)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :no-escape t :cjk :jp :vender :osfjvc))

(defvar *encoding-euc-jp-necext*
  (make-iso2022-encoding "eucjp-necext" "日本語(EUC-JP:NEC-EXT)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :no-escape t :cjk :jp :vender :necext))

(defvar *encoding-euc-jp-ibmext*
  (make-iso2022-encoding "eucjp-ibmext" "日本語(EUC-JP:IBM-EXT)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :no-escape t :cjk :jp :vender :ibmext))

(defvar *encoding-iso-8bit-jp*
  (make-iso2022-encoding "iso-8bit-jp" "日本語(ISO 8bit with ESC)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :ascii-eol t :ascii-control t :short-form t))

(defvar *encoding-iso-8bit-jp-open*
  (make-iso2022-encoding "iso-8bit-jp-open" "日本語(ISO 8bit with ESC:OSF/JVC)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :ascii-eol t :ascii-control t :short-form t :vender :osfjvc))

(defvar *encoding-iso-8bit-jp-necext*
  (make-iso2022-encoding "iso-8bit-jp-necext" "日本語(ISO 8bit with ESC:NEC-EXT)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :ascii-eol t :ascii-control t :short-form t :vender :necext))

(defvar *encoding-iso-8bit-jp-ibmext*
  (make-iso2022-encoding "iso-8bit-jp-ibmext" "日本語(ISO 8bit with ESC:IBM-EXT)"
			 :g0 :us-ascii :g1 :jisx0208 :g2 :jisx0201-kana :g3 :jisx0212
			 :ascii-eol t :ascii-control t :short-form t :vender :ibmext))

(defvar *encoding-jis*
  (make-iso2022-encoding "jis" "日本語(ISO-2022-JP)"
			 :g0 :us-ascii :ascii-eol t :ascii-control t :7bits t
			 :short-form t :cjk :jp))

(defvar *encoding-jis-open*
  (make-iso2022-encoding "jis-open" "日本語(ISO-2022-JP:OSF/JVC)"
			 :g0 :us-ascii :ascii-eol t :ascii-control t :7bits t
			 :short-form t :cjk :jp :vender :osfjvc))

(defvar *encoding-jis-necext*
  (make-iso2022-encoding "jis-necext" "日本語(ISO-2022-JP:NEC-EXT)"
			 :g0 :us-ascii :ascii-eol t :ascii-control t :7bits t
			 :short-form t :cjk :jp :vender :necext))

(defvar *encoding-jis-ibmext*
  (make-iso2022-encoding "jis-ibmext" "日本語(ISO-2022-JP:IBM-EXT)"
			 :g0 :us-ascii :ascii-eol t :ascii-control t :7bits t
			 :short-form t :cjk :jp :vender :ibmext))

(defvar *encoding-iso-2022-jp-2*
  (make-iso2022-encoding "iso2022jp2" "日本語(ISO-2022-JP-2)"
			 :g0 :us-ascii :g2 '(nil :iso8859-1 :iso8859-7)
			 :ascii-eol t :ascii-control t :7bits t :short-form t))

(defvar *encoding-iso-2022-jp-2-open*
  (make-iso2022-encoding "iso2022jp2-open" "日本語(ISO-2022-JP-2:OSF/JVC)"
			 :g0 :us-ascii :g2 '(nil :iso8859-1 :iso8859-7)
			 :ascii-eol t :ascii-control t :7bits t
			 :short-form t :vender :osfjvc))

(defvar *encoding-iso-2022-jp-2-necext*
  (make-iso2022-encoding "iso2022jp2-necext" "日本語(ISO-2022-JP-2:NEC-EXT)"
			 :g0 :us-ascii :g2 '(nil :iso8859-1 :iso8859-7)
			 :ascii-eol t :ascii-control t :7bits t
			 :short-form t :vender :necext))

(defvar *encoding-iso-2022-jp-2-ibmext*
  (make-iso2022-encoding "iso2022jp2-ibmext" "日本語(ISO-2022-JP-2:IBM-EXT)"
			 :g0 :us-ascii :g2 '(nil :iso8859-1 :iso8859-7)
			 :ascii-eol t :ascii-control t :7bits t
			 :short-form t :vender :ibmext))

(defvar *encoding-jis-sio*
  (make-iso2022-encoding "jis-sio" "日本語(JIS - SO/SI)"
			 :g0 :us-ascii :g1 :jisx0201-kana :ascii-eol t :ascii-control t
			 :7bits t :locking-shift t :short-form t :cjk :jp))

(defvar *encoding-jis8*
  (make-iso2022-encoding "jis8" "日本語(JIS8)"
			 :g0 :us-ascii :g1 :jisx0201-kana :ascii-eol t :ascii-control t
			 :short-form t :cjk :jp))

(defvar *encoding-big5*
  (make-big5-encoding "big5" "中国語(Big-5)"))

(defvar *encoding-euc-gb*
  (make-iso2022-encoding "euc-gb" "中国語(GB2312)"
			 :g0 :us-ascii :g1 :gb2312 :no-escape t :cjk :cn-gb))

(defvar *encoding-iso-2022-cn*
  (make-iso2022-encoding "iso2022cn" "中国語(ISO-2022-CN)"
			 :g0 :us-ascii :g1 '(nil :gb2312 :cns11643-1)
			 :g2 '(nil :cns11643-2) :g3 '(nil)
			 :ascii-eol t :ascii-control nil
			 :7bits t :locking-shift t
			 :use-cns11643 t :cjk :cn))

(defvar *encoding-iso-8bit-cn*
  (make-iso2022-encoding "iso-8bit-cn" "中国語(ISO 8bit with ESC)"
			 :g0 :us-ascii :g1 :gb2312 :use-cns11643 t
			 :ascii-eol t :ascii-control t :short-form t))

(defvar *encoding-euc-kr*
  (make-iso2022-encoding "euc-kr" "韓国語(EUC-KR)"
			 :g0 :us-ascii :g1 :ksc5601 :no-escape t :cjk :kr))

(defvar *encoding-iso-2022-kr*
  (make-iso2022-encoding "iso2022kr" "韓国語(ISO-2022-KR)"
			 :g0 :us-ascii :g1 '(nil :ksc5601) :ascii-eol t :ascii-control t
			 :7bits t :locking-shift t :short-form t :cjk :kr))

(defvar *encoding-iso-8bit-kr*
  (make-iso2022-encoding "iso-8bit-kr" "韓国語(ISO 8bit with ESC)"
			 :g0 :us-ascii :g1 :ksc5601
			 :ascii-eol t :ascii-control t :short-form t))

(defvar *encoding-iso-8859-1*
  (make-iso8859-encoding "iso-latin1" "欧米(ISO-8859-1)" :iso8859-1))
(defvar *encoding-iso-8859-2*
  (make-iso8859-encoding "iso-latin2" "中欧(ISO-8859-2)" :iso8859-2))
(defvar *encoding-iso-8859-3*
  (make-iso8859-encoding "iso-latin3" "ISO-8859-3" :iso8859-3))
(defvar *encoding-iso-8859-4*
  (make-iso8859-encoding "iso-latin4" "ISO-8859-4" :iso8859-4))
(defvar *encoding-iso-8859-5*
  (make-iso8859-encoding "iso-cyrillic" "キリル文字(ISO-8859-5)" :iso8859-5))
(defvar *encoding-iso-8859-7*
  (make-iso8859-encoding "iso-greek" "ギリシア語(ISO-8859-7)" :iso8859-7))
(defvar *encoding-iso-8859-9*
  (make-iso8859-encoding "iso-latin5" "トルコ語(ISO-8859-9)" :iso8859-9))
(defvar *encoding-iso-8859-10*
  (make-iso8859-encoding "iso-latin6" "北欧(ISO-8859-10)" :iso8859-10))
(defvar *encoding-iso-8859-13*
  (make-iso8859-encoding "iso-baltic" "バルト言語(ISO-8859-13)" :iso8859-13))

(defvar *encoding-windows-latin1*
  (make-windows-codepage-encoding "win-latin1" "欧米(Windows 1252)" 1252))
(defvar *encoding-windows-latin2*
  (make-windows-codepage-encoding "win-latin2" "中欧(Windows 1250)" 1250))
(defvar *encoding-windows-cyrillic*
  (make-windows-codepage-encoding "win-cyrillic" "キリル文字(Windows 1251)" 1251))
(defvar *encoding-windows-greek*
  (make-windows-codepage-encoding "win-greek" "ギリシア語(Windows 1253)" 1253))
(defvar *encoding-windows-turkish*
  (make-windows-codepage-encoding "win-turkish" "トルコ語(Windows 1254)" 1254))
(defvar *encoding-windows-baltic*
  (make-windows-codepage-encoding "win-baltic" "バルト言語(Windows 1257)" 1257))

(defvar *encoding-koi8r*
  (make-windows-codepage-encoding "koi8r" "キリル文字(KOI8-R)" 878))
(defvar *encoding-koi8u*
  (make-windows-codepage-encoding "koi8u" "キリル文字(KOI8-U)" 100878))

(defvar *encoding-utf16*
  (make-utf16-encoding "utf16" "Unicode (UTF-16)"
		       :signature t :windows t :cjk :jp))
(defvar *encoding-utf16le-bom*
  (make-utf16-encoding "utf16le-bom" "Unicode (UTF-16LE-BOM)"
		       :signature t :windows t :byte-order :little-endian :cjk :jp))
(defvar *encoding-utf16be-bom*
  (make-utf16-encoding "utf16be-bom" "Unicode (UTF-16BE-BOM)"
		       :signature t :windows t :byte-order :big-endian :cjk :jp))
(defvar *encoding-utf16le*
  (make-utf16-encoding "utf16le" "Unicode (UTF-16LE)"
		       :signature nil :windows t :byte-order :little-endian :cjk :jp))
(defvar *encoding-utf16be*
  (make-utf16-encoding "utf16be" "Unicode (UTF-16BE)"
		       :signature nil :windows t :byte-order :big-endian :cjk :jp))
(defvar *encoding-utf8*
  (make-utf8-encoding "utf8" "Unicode (UTF-8)"
		      :signature t :windows t :cjk :jp))
(defvar *encoding-utf8n*
  (make-utf8-encoding "utf8n" "Unicode (UTF-8N)"
		      :signature nil :windows t :cjk :jp))
(defvar *encoding-utf7*
  (make-utf7-encoding "utf7" "Unicode (UTF-7)"
		      :signature nil :windows t :cjk :jp))
(defvar *encoding-utf5*
  (make-utf5-encoding "utf5" "Unicode (UTF-5)"
		      :signature nil :windows t :cjk :jp))
(defvar *encoding-imap4*
  (make-utf7-encoding "imap4" "IMAP4 mailbox"
		      :signature nil :windows t :imap4-mailbox-name t :cjk :jp))

(defvar *encoding-std-utf16*
  (make-utf16-encoding "std-utf16" "STD Unicode (UTF-16)"
		       :signature t :cjk :jp))
(defvar *encoding-std-utf16le-bom*
  (make-utf16-encoding "std-utf16le-bom" "STD Unicode (UTF-16LE-BOM)"
		       :signature t :byte-order :little-endian :cjk :jp))
(defvar *encoding-std-utf16be-bom*
  (make-utf16-encoding "std-utf16be-bom" "STD Unicode (UTF-16BE-BOM)"
		       :signature t :byte-order :big-endian :cjk :jp))
(defvar *encoding-std-utf16le*
  (make-utf16-encoding "std-utf16le" "STD Unicode (UTF-16LE)"
		       :byte-order :little-endian :cjk :jp))
(defvar *encoding-std-utf16be*
  (make-utf16-encoding "std-utf16be" "STD Unicode (UTF-16BE)"
		       :byte-order :big-endian :cjk :jp))
(defvar *encoding-std-utf8*
  (make-utf8-encoding "std-utf8" "STD Unicode (UTF-8)"
		      :signature t :cjk :jp))
(defvar *encoding-std-utf8n*
  (make-utf8-encoding "std-utf8n" "STD Unicode (UTF-8N)" :cjk :jp))
(defvar *encoding-std-utf7*
  (make-utf7-encoding "std-utf7" "STD Unicode (UTF-7)" :cjk :jp))
(defvar *encoding-std-utf5*
  (make-utf5-encoding "std-utf5" "STD Unicode (UTF-5)" :cjk :jp))
(defvar *encoding-std-imap4*
  (make-utf7-encoding "std-imap4" "STD IMAP4 mailbox"
		      :imap4-mailbox-name t :cjk :jp))

#|
(defvar *encoding-utf16*
  (make-utf16-encoding "utf16" "Unicode (UTF-16)"
		       :signature t :windows t))
(defvar *encoding-utf16le-bom*
  (make-utf16-encoding "utf16le-bom" "Unicode (UTF-16LE-BOM)"
		       :signature t :windows t :byte-order :little-endian))
(defvar *encoding-utf16be-bom*
  (make-utf16-encoding "utf16be-bom" "Unicode (UTF-16BE-BOM)"
		       :signature t :windows t :byte-order :big-endian))
(defvar *encoding-utf16le*
  (make-utf16-encoding "utf16le" "Unicode (UTF-16LE)"
		       :signature nil :windows t :byte-order :little-endian))
(defvar *encoding-utf16be*
  (make-utf16-encoding "utf16be" "Unicode (UTF-16BE)"
		       :signature nil :windows t :byte-order :big-endian))
(defvar *encoding-utf8*
  (make-utf8-encoding "utf8" "Unicode (UTF-8)"
		      :signature t :windows t))
(defvar *encoding-utf8n*
  (make-utf8-encoding "utf8n" "Unicode (UTF-8N)"
		      :signature nil :windows t))
(defvar *encoding-utf7*
  (make-utf7-encoding "utf7" "Unicode (UTF-7)"
		      :signature nil :windows t))
(defvar *encoding-utf5*
  (make-utf5-encoding "utf5" "Unicode (UTF-5)"
		      :signature nil :windows t))
(defvar *encoding-imap4*
  (make-utf7-encoding "imap4" "IMAP4 mailbox"
		      :signature nil :windows t :imap4-mailbox-name t))
|#

(defvar *encoding-utf16-kr*
  (make-utf16-encoding "utf16-kr" "Unicode (UTF-16) [KR]"
		       :signature t :windows t :cjk :kr))
(defvar *encoding-utf16le-bom-kr*
  (make-utf16-encoding "utf16le-bom-kr" "Unicode (UTF-16LE-BOM) [KR]"
		       :signature t :windows t :byte-order :little-endian :cjk :kr))
(defvar *encoding-utf16be-bom-kr*
  (make-utf16-encoding "utf16be-bom-kr" "Unicode (UTF-16BE-BOM) [KR]"
		       :signature t :windows t :byte-order :big-endian :cjk :kr))
(defvar *encoding-utf16le-kr*
  (make-utf16-encoding "utf16le-kr" "Unicode (UTF-16LE) [KR]"
		       :signature nil :windows t :byte-order :little-endian :cjk :kr))
(defvar *encoding-utf16be-kr*
  (make-utf16-encoding "utf16be-kr" "Unicode (UTF-16BE) [KR]"
		       :signature nil :windows t :byte-order :big-endian :cjk :kr))
(defvar *encoding-utf8-kr*
  (make-utf8-encoding "utf8-kr" "Unicode (UTF-8) [KR]"
		      :signature t :windows t :cjk :kr))
(defvar *encoding-utf8n-kr*
  (make-utf8-encoding "utf8n-kr" "Unicode (UTF-8N) [KR]"
		      :signature nil :windows t :cjk :kr))
(defvar *encoding-utf7-kr*
  (make-utf7-encoding "utf7-kr" "Unicode (UTF-7) [KR]"
		      :signature nil :windows t :cjk :kr))
(defvar *encoding-utf5-kr*
  (make-utf5-encoding "utf5-kr" "Unicode (UTF-5) [KR]"
		      :signature nil :windows t :cjk :kr))
(defvar *encoding-imap4-kr*
  (make-utf7-encoding "imap4-kr" "IMAP4 mailbox [KR]"
		      :signature nil :windows t :imap4-mailbox-name t :cjk :kr))

(defvar *encoding-utf16-gb*
  (make-utf16-encoding "utf16-gb" "Unicode (UTF-16) [GB]"
		       :signature t :windows t :cjk :cn-gb))
(defvar *encoding-utf16le-bom-gb*
  (make-utf16-encoding "utf16le-bom-gb" "Unicode (UTF-16LE-BOM) [GB]"
		       :signature t :windows t :byte-order :little-endian :cjk :cn-gb))
(defvar *encoding-utf16be-bom-gb*
  (make-utf16-encoding "utf16be-bom-gb" "Unicode (UTF-16BE-BOM) [GB]"
		       :signature t :windows t :byte-order :big-endian :cjk :cn-gb))
(defvar *encoding-utf16le-gb*
  (make-utf16-encoding "utf16le-gb" "Unicode (UTF-16LE) [GB]"
		       :signature nil :windows t :byte-order :little-endian :cjk :cn-gb))
(defvar *encoding-utf16be-gb*
  (make-utf16-encoding "utf16be-gb" "Unicode (UTF-16BE) [GB]"
		       :signature nil :windows t :byte-order :big-endian :cjk :cn-gb))
(defvar *encoding-utf8-gb*
  (make-utf8-encoding "utf8-gb" "Unicode (UTF-8) [GB]"
		      :signature t :windows t :cjk :cn-gb))
(defvar *encoding-utf8n-gb*
  (make-utf8-encoding "utf8n-gb" "Unicode (UTF-8N) [GB]"
		      :signature nil :windows t :cjk :cn-gb))
(defvar *encoding-utf7-gb*
  (make-utf7-encoding "utf7-gb" "Unicode (UTF-7) [GB]"
		      :signature nil :windows t :cjk :cn-gb))
(defvar *encoding-utf5-gb*
  (make-utf5-encoding "utf5-gb" "Unicode (UTF-5) [GB]"
		      :signature nil :windows t :cjk :cn-gb))
(defvar *encoding-imap4-gb*
  (make-utf7-encoding "imap4-gb" "IMAP4 mailbox [GB]"
		      :signature nil :windows t :imap4-mailbox-name t :cjk :cn-gb))

(defvar *encoding-utf16-big5*
  (make-utf16-encoding "utf16-big5" "Unicode (UTF-16) [Big5]"
		       :signature t :windows t :cjk :cn-big5))
(defvar *encoding-utf16le-bom-big5*
  (make-utf16-encoding "utf16le-bom-big5" "Unicode (UTF-16LE-BOM) [Big5]"
		       :signature t :windows t :byte-order :little-endian :cjk :cn-big5))
(defvar *encoding-utf16be-bom-big5*
  (make-utf16-encoding "utf16be-bom-big5" "Unicode (UTF-16BE-BOM) [Big5]"
		       :signature t :windows t :byte-order :big-endian :cjk :cn-big5))
(defvar *encoding-utf16le-big5*
  (make-utf16-encoding "utf16le-big5" "Unicode (UTF-16LE) [Big5]"
		       :signature nil :windows t :byte-order :little-endian :cjk :cn-big5))
(defvar *encoding-utf16be-big5*
  (make-utf16-encoding "utf16be-big5" "Unicode (UTF-16BE) [Big5]"
		       :signature nil :windows t :byte-order :big-endian :cjk :cn-big5))
(defvar *encoding-utf8-big5*
  (make-utf8-encoding "utf8-big5" "Unicode (UTF-8) [Big5]"
		      :signature t :windows t :cjk :cn-big5))
(defvar *encoding-utf8n-big5*
  (make-utf8-encoding "utf8n-big5" "Unicode (UTF-8N) [Big5]"
		      :signature nil :windows t :cjk :cn-big5))
(defvar *encoding-utf7-big5*
  (make-utf7-encoding "utf7-big5" "Unicode (UTF-7) [Big5]"
		      :signature nil :windows t :cjk :cn-big5))
(defvar *encoding-utf5-big5*
  (make-utf5-encoding "utf5-big5" "Unicode (UTF-5) [Big5]"
		      :signature nil :windows t :cjk :cn-big5))
(defvar *encoding-imap4-big5*
  (make-utf7-encoding "imap4-big5" "IMAP4 mailbox [Big5]"
		      :signature nil :windows t :imap4-mailbox-name t :cjk :cn-big5))

(defvar *encoding-utf16-cn*
  (make-utf16-encoding "utf16-cn" "Unicode (UTF-16) [CN]"
		       :signature t :windows t :cjk :cn))
(defvar *encoding-utf16le-bom-cn*
  (make-utf16-encoding "utf16le-bom-cn" "Unicode (UTF-16LE-BOM) [CN]"
		       :signature t :windows t :byte-order :little-endian :cjk :cn))
(defvar *encoding-utf16be-bom-cn*
  (make-utf16-encoding "utf16be-bom-cn" "Unicode (UTF-16BE-BOM) [CN]"
		       :signature t :windows t :byte-order :big-endian :cjk :cn))
(defvar *encoding-utf16le-cn*
  (make-utf16-encoding "utf16le-cn" "Unicode (UTF-16LE) [CN]"
		       :signature nil :windows t :byte-order :little-endian :cjk :cn))
(defvar *encoding-utf16be-cn*
  (make-utf16-encoding "utf16be-cn" "Unicode (UTF-16BE) [CN]"
		       :signature nil :windows t :byte-order :big-endian :cjk :cn))
(defvar *encoding-utf8-cn*
  (make-utf8-encoding "utf8-cn" "Unicode (UTF-8) [CN]"
		      :signature t :windows t :cjk :cn))
(defvar *encoding-utf8n-cn*
  (make-utf8-encoding "utf8n-cn" "Unicode (UTF-8N) [CN]"
		      :signature nil :windows t :cjk :cn))
(defvar *encoding-utf7-cn*
  (make-utf7-encoding "utf7-cn" "Unicode (UTF-7) [CN]"
		      :signature nil :windows t :cjk :cn))
(defvar *encoding-utf5-cn*
  (make-utf5-encoding "utf5-cn" "Unicode (UTF-5) [CN]"
		      :signature nil :windows t :cjk :cn))
(defvar *encoding-imap4-cn*
  (make-utf7-encoding "imap4-cn" "IMAP4 mailbox [CN]"
		      :signature nil :windows t :imap4-mailbox-name t :cjk :cn))

(defvar *encoding-binary*
  (make-binary-encoding "bin" "Binary"))

(defvar *encoding-default-iso-2022*
  (list *encoding-jis* *encoding-iso-2022-cn* *encoding-iso-2022-kr*
	*encoding-iso-2022-jp-2*))
(defvar *encoding-default-euc* *encoding-euc-jp*)

(or *default-fileio-encoding*
    (setq *default-fileio-encoding* *encoding-sjis*))
(or *expected-fileio-encoding*
    (setq *expected-fileio-encoding* *encoding-auto*))
(or *default-process-encoding*
    (setq *default-process-encoding* *encoding-sjis*))

(defvar *encoding-default-utf16* *encoding-utf16*)
(defvar *encoding-default-utf16le-bom* *encoding-utf16le-bom*)
(defvar *encoding-default-utf16be-bom* *encoding-utf16be-bom*)
(defvar *encoding-default-utf16le* *encoding-utf16le*)
(defvar *encoding-default-utf16be* *encoding-utf16be*)
(defvar *encoding-default-utf8* *encoding-utf8*)
(defvar *encoding-default-utf8n* *encoding-utf8n*)
(defvar *encoding-default-utf7* *encoding-utf7*)
(defvar *encoding-default-utf5* *encoding-utf5*)
(defvar *encoding-default-imap4* *encoding-imap4*)

(defvar *default-utf16-byte-order* :little-endian)

(unless *char-encoding-list*
  (setq *char-encoding-list*
	(list *encoding-auto*
	      *encoding-sjis*
	      *encoding-jis*
	      *encoding-euc-jp*
	      *encoding-iso-2022-jp-2*
	      *encoding-iso-8bit-jp*
	      *encoding-jis-open*
	      *encoding-euc-jp-open*
	      *encoding-iso-2022-jp-2-open*
	      *encoding-iso-8bit-jp-open*
	      *encoding-jis-necext*
	      *encoding-euc-jp-necext*
	      *encoding-iso-2022-jp-2-necext*
	      *encoding-iso-8bit-jp-necext*
	      *encoding-jis-ibmext*
	      *encoding-euc-jp-ibmext*
	      *encoding-iso-2022-jp-2-ibmext*
	      *encoding-iso-8bit-jp-ibmext*
	      *encoding-jis-sio*
	      *encoding-jis8*
	      *encoding-euc-gb*
	      *encoding-big5*
	      *encoding-iso-2022-cn*
	      *encoding-iso-8bit-cn*
	      *encoding-euc-kr*
	      *encoding-iso-2022-kr*
	      *encoding-iso-8bit-kr*
	      *encoding-iso-8859-1*
	      *encoding-windows-latin1*
	      *encoding-iso-8859-2*
	      *encoding-windows-latin2*
	      *encoding-iso-8859-3*
	      *encoding-iso-8859-4*
	      *encoding-iso-8859-5*
	      *encoding-koi8r*
	      *encoding-koi8u*
	      *encoding-windows-cyrillic*
	      *encoding-iso-8859-7*
	      *encoding-windows-greek*
	      *encoding-iso-8859-9*
	      *encoding-windows-turkish*
	      *encoding-iso-8859-10*
	      *encoding-iso-8859-13*
	      *encoding-windows-baltic*
	      *encoding-utf16*
	      *encoding-utf16le-bom*
	      *encoding-utf16be-bom*
	      *encoding-utf16le*
	      *encoding-utf16be*
	      *encoding-utf8*
	      *encoding-utf8n*
	      *encoding-utf7*
	      *encoding-utf5*
	      *encoding-std-utf16*
	      *encoding-std-utf16le-bom*
	      *encoding-std-utf16be-bom*
	      *encoding-std-utf16le*
	      *encoding-std-utf16be*
	      *encoding-std-utf8*
	      *encoding-std-utf8n*
	      *encoding-std-utf7*
	      *encoding-std-utf5*
	      *encoding-utf16-kr*
	      *encoding-utf16le-bom-kr*
	      *encoding-utf16be-bom-kr*
	      *encoding-utf16le-kr*
	      *encoding-utf16be-kr*
	      *encoding-utf8-kr*
	      *encoding-utf8n-kr*
	      *encoding-utf7-kr*
	      *encoding-utf5-kr*
	      *encoding-utf16-gb*
	      *encoding-utf16le-bom-gb*
	      *encoding-utf16be-bom-gb*
	      *encoding-utf16le-gb*
	      *encoding-utf16be-gb*
	      *encoding-utf8-gb*
	      *encoding-utf8n-gb*
	      *encoding-utf7-gb*
	      *encoding-utf5-gb*
	      *encoding-utf16-big5*
	      *encoding-utf16le-bom-big5*
	      *encoding-utf16be-bom-big5*
	      *encoding-utf16le-big5*
	      *encoding-utf16be-big5*
	      *encoding-utf8-big5*
	      *encoding-utf8n-big5*
	      *encoding-utf7-big5*
	      *encoding-utf5-big5*
	      *encoding-utf16-cn*
	      *encoding-utf16le-bom-cn*
	      *encoding-utf16be-bom-cn*
	      *encoding-utf16le-cn*
	      *encoding-utf16be-cn*
	      *encoding-utf8-cn*
	      *encoding-utf8n-cn*
	      *encoding-utf7-cn*
	      *encoding-utf5-cn*
	      *encoding-binary*
	)))

(defvar *char-encoding-bom-alist*
  `((,*encoding-utf8* . ,*encoding-utf8n*)
    (,*encoding-std-utf8* . ,*encoding-std-utf8n*)
    (,*encoding-utf8-kr* . ,*encoding-utf8n-kr*)
    (,*encoding-utf8-gb* . ,*encoding-utf8n-gb*)
    (,*encoding-utf8-big5* . ,*encoding-utf8n-big5*)
    (,*encoding-utf8-cn* . ,*encoding-utf8n-cn*)
    (,*encoding-utf16le* . ,*encoding-utf16le-bom*)
    (,*encoding-utf16be* . ,*encoding-utf16be-bom*)
    (,*encoding-std-utf16le* . ,*encoding-std-utf16le-bom*)
    (,*encoding-std-utf16be* . ,*encoding-std-utf16be-bom*)
    (,*encoding-utf16le-kr* . ,*encoding-utf16le-bom-kr*)
    (,*encoding-utf16be-kr* . ,*encoding-utf16be-bom-kr*)
    (,*encoding-utf16le-gb* . ,*encoding-utf16le-bom-gb*)
    (,*encoding-utf16be-gb* . ,*encoding-utf16be-bom-gb*)
    (,*encoding-utf16le-big5* . ,*encoding-utf16le-bom-big5*)
    (,*encoding-utf16be-big5* . ,*encoding-utf16be-bom-big5*)
    (,*encoding-utf16le-cn* . ,*encoding-utf16le-bom-cn*)
    (,*encoding-utf16be-cn* . ,*encoding-utf16be-bom-cn*)
    ))

(defvar *char-encoding-tree*
  `(,*encoding-auto*
    ("日本語" . (,*encoding-sjis*
		 ,*encoding-jis*
		 ,*encoding-euc-jp*
		 ,*encoding-iso-2022-jp-2*
		 ,*encoding-iso-8bit-jp*
		 ,*encoding-jis-open*
		 ,*encoding-euc-jp-open*
		 ,*encoding-iso-2022-jp-2-open*
		 ,*encoding-iso-8bit-jp-open*
		 ,*encoding-jis-necext*
		 ,*encoding-euc-jp-necext*
		 ,*encoding-iso-2022-jp-2-necext*
		 ,*encoding-iso-8bit-jp-necext*
		 ,*encoding-jis-ibmext*
		 ,*encoding-euc-jp-ibmext*
		 ,*encoding-iso-2022-jp-2-ibmext*
		 ,*encoding-iso-8bit-jp-ibmext*
		 ,*encoding-jis-sio*
		 ,*encoding-jis8*))
    ("中国語" . (,*encoding-euc-gb*
		 ,*encoding-big5*
		 ,*encoding-iso-2022-cn*
		 ,*encoding-iso-8bit-cn*
		 ("Unicode [GB]" . (,*encoding-utf16-gb*
				    ,*encoding-utf16le-bom-gb*
				    ,*encoding-utf16be-bom-gb*
				    ,*encoding-utf16le-gb*
				    ,*encoding-utf16be-gb*
				    ,*encoding-utf8-gb*
				    ,*encoding-utf8n-gb*
				    ,*encoding-utf7-gb*
				    ,*encoding-utf5-gb*))
		 ("Unicode [Big5]" . (,*encoding-utf16-big5*
				      ,*encoding-utf16le-bom-big5*
				      ,*encoding-utf16be-bom-big5*
				      ,*encoding-utf16le-big5*
				      ,*encoding-utf16be-big5*
				      ,*encoding-utf8-big5*
				      ,*encoding-utf8n-big5*
				      ,*encoding-utf7-big5*
				      ,*encoding-utf5-big5*))
		 ("Unicode [CN]" . (,*encoding-utf16-cn*
				    ,*encoding-utf16le-bom-cn*
				    ,*encoding-utf16be-bom-cn*
				    ,*encoding-utf16le-cn*
				    ,*encoding-utf16be-cn*
				    ,*encoding-utf8-cn*
				    ,*encoding-utf8n-cn*
				    ,*encoding-utf7-cn*
				    ,*encoding-utf5-cn*))))
    ("韓国語" . (,*encoding-euc-kr*
		 ,*encoding-iso-2022-kr*
		 ,*encoding-iso-8bit-kr*
		 ("Unicode" . (,*encoding-utf16-kr*
			       ,*encoding-utf16le-bom-kr*
			       ,*encoding-utf16be-bom-kr*
			       ,*encoding-utf16le-kr*
			       ,*encoding-utf16be-kr*
			       ,*encoding-utf8-kr*
			       ,*encoding-utf8n-kr*
			       ,*encoding-utf7-kr*
			       ,*encoding-utf5-kr*))))
    ("欧米" . (,*encoding-iso-8859-1*
	       ,*encoding-windows-latin1*
	       ,*encoding-iso-8859-2*
	       ,*encoding-windows-latin2*
	       ,*encoding-iso-8859-3*
	       ,*encoding-iso-8859-4*
	       ,*encoding-iso-8859-5*
	       ,*encoding-koi8r*
	       ,*encoding-koi8u*
	       ,*encoding-windows-cyrillic*
	       ,*encoding-iso-8859-7*
	       ,*encoding-windows-greek*
	       ,*encoding-iso-8859-9*
	       ,*encoding-windows-turkish*
	       ,*encoding-iso-8859-10*
	       ,*encoding-iso-8859-13*
	       ,*encoding-windows-baltic*))
    ("Unicode" . (,*encoding-utf16*
		  ,*encoding-utf16le-bom*
		  ,*encoding-utf16be-bom*
		  ,*encoding-utf16le*
		  ,*encoding-utf16be*
		  ,*encoding-utf8*
		  ,*encoding-utf8n*
		  ,*encoding-utf7*
		  ,*encoding-utf5*))
    ("STD Unicode" . (,*encoding-std-utf16*
		      ,*encoding-std-utf16le-bom*
		      ,*encoding-std-utf16be-bom*
		      ,*encoding-std-utf16le*
		      ,*encoding-std-utf16be*
		      ,*encoding-std-utf8*
		      ,*encoding-std-utf8n*
		      ,*encoding-std-utf7*
		      ,*encoding-std-utf5*))
    ,*encoding-binary*))

(or (boundp '*clipboard-char-encoding*)
    (setq-default *clipboard-char-encoding* *encoding-sjis*))

(defvar *clipboard-char-encoding-list*
  '(*encoding-auto*
    *encoding-sjis*
    *encoding-jis*
    *encoding-euc-jp*
    *encoding-iso-2022-jp-2*
    *encoding-iso-8bit-jp*
    *encoding-jis-open*
    *encoding-euc-jp-open*
    *encoding-iso-2022-jp-2-open*
    *encoding-iso-8bit-jp-open*
    *encoding-jis-necext*
    *encoding-euc-jp-necext*
    *encoding-iso-2022-jp-2-necext*
    *encoding-iso-8bit-jp-necext*
    *encoding-jis-ibmext*
    *encoding-euc-jp-ibmext*
    *encoding-iso-2022-jp-2-ibmext*
    *encoding-iso-8bit-jp-ibmext*
    *encoding-euc-gb*
    *encoding-big5*
    *encoding-iso-2022-cn*
    *encoding-iso-8bit-cn*
    *encoding-euc-kr*
    *encoding-iso-2022-kr*
    *encoding-iso-8bit-kr*
    *encoding-iso-8859-1*
    *encoding-windows-latin1*
    *encoding-iso-8859-2*
    *encoding-windows-latin2*
    *encoding-iso-8859-3*
    *encoding-iso-8859-4*
    *encoding-iso-8859-5*
    *encoding-koi8r*
    *encoding-koi8u*
    *encoding-windows-cyrillic*
    *encoding-iso-8859-7*
    *encoding-windows-greek*
    *encoding-iso-8859-9*
    *encoding-windows-turkish*
    *encoding-iso-8859-10*
    *encoding-iso-8859-13*
    *encoding-windows-baltic*
    *encoding-utf16*
    *encoding-utf8n*
    *encoding-utf7*
    *encoding-utf5*
    *encoding-std-utf16*
    *encoding-std-utf8n*
    *encoding-std-utf7*
    *encoding-std-utf5*
    *encoding-utf16-kr*
    *encoding-utf8n-kr*
    *encoding-utf7-kr*
    *encoding-utf5-kr*
    *encoding-utf16-gb*
    *encoding-utf8n-gb*
    *encoding-utf7-gb*
    *encoding-utf5-gb*
    *encoding-utf16-big5*
    *encoding-utf8n-big5*
    *encoding-utf7-big5*
    *encoding-utf5-big5*
    *encoding-utf16-cn*
    *encoding-utf8n-cn*
    *encoding-utf7-cn*
    *encoding-utf5-cn*
    *encoding-binary*))

(defvar *character-set-alist*
  '((*encoding-jis* "ISO-2022-JP" "csISO2022JP")
    (*encoding-iso-2022-jp-2* "ISO-2022-JP-1")
    (*encoding-iso-2022-jp-2* "ISO-2022-JP-2" "csISO2022JP2")
    (*encoding-sjis* "Shift_JIS" "MS_Kanji" "csShiftJIS"
		     "Windows-31J" "csWindows31J"
		     "shift-jis" "x-sjis" "x-ms-cp932")
    (*encoding-euc-jp* "EUC-JP" "Extended_UNIX_Code_Packed_Format_for_Japanese"
		       "csEUCPkdFmtJapanese" "x-euc-jp" "x-euc")
    (*encoding-jis-sio* "_ISO-2022-JP$SIO")
    (*encoding-jis* "_iso-2022-jp$ESC")
    (*encoding-utf8* "UTF-8" "UNICODE-1-1-UTF-8" "UNICODE-2-0-UTF-8" "X-UNICODE-2-0-UTF-8")
    (*encoding-utf7* "UTF-7" "UNICODE-1-1-UTF-7" "X-UNICODE-2-0-UTF-7" "csUnicode11UTF7")
    (*encoding-utf5* "UTF-5")
    (*encoding-utf16* "UNICODE-1-1" "csUnicode11" "ISO-10646-UCS-2" "csUnicode")
    (*encoding-big5* "Big5" "csBig5" "CN-Big5" "x-big5")
    (*encoding-euc-gb* "GB2312" "csGB2312" "chinese" "CN-GB" "csGB231280"
		       "csISO58GB231280" "GB_2312-80" "GB231280" "GB2312-80" "iso-ir-58")
    (*encoding-iso-2022-cn* "ISO-2022-CN")
    ;(*encoding-iso-2022-cn* "ISO-2022-CN-EXT")
    (*encoding-euc-kr* "EUC-KR" "csEUCKR")
    (*encoding-iso-2022-kr* "ISO-2022-KR" "csISO2022KR")
    (*encoding-iso-8859-1* "ISO-8859-1" "ISO_8859-1:1987" "iso-ir-100"
			   "ISO_8859-1" "latin1" "l1" "IBM819" "CP819" "csISOLatin1")
    (*encoding-iso-8859-2* "ISO-8859-2" "ISO_8859-2:1987" "iso-ir-101"
			   "ISO_8859-2" "latin2" "l2" "csISOLatin2")
    (*encoding-iso-8859-3* "ISO-8859-3" "ISO_8859-3:1988" "iso-ir-109"
			   "ISO_8859-3" "latin3" "l3" "csISOLatin3")
    (*encoding-iso-8859-4* "ISO-8859-4" "ISO_8859-4:1988" "iso-ir-110"
			   "ISO_8859-4" "latin4" "l4" "csISOLatin4")
    (*encoding-iso-8859-5* "ISO-8859-5" "ISO_8859-5:1988" "iso-ir-144"
			   "ISO_8859-5" "cyrillic" "csISOLatinCyrillic")
    (*encoding-iso-8859-7* "ISO-8859-7" "ISO_8859-7:1987" "iso-ir-126"
			   "ISO_8859-7" "ELOT_928" "ECMA-118"
			   "greek" "greek8" "csISOLatinGreek")
    (*encoding-iso-8859-9* "ISO-8859-9" "ISO_8859-9:1989" "iso-ir-148"
			   "ISO_8859-9" "latin5" "l5" "csISOLatin5")
    (*encoding-iso-8859-10* "ISO-8859-10" "latin6" "iso-ir-157"
			    "l6" "ISO_8859-10:1992" "csISOLatin6")
    (*encoding-iso-8859-13* "ISO-8859-13")
    (*encoding-koi8r* "KOI8-R" "csKOI8R" "koi" "koi8" "koi8r")
    (*encoding-koi8u* "KOI8-U" "koi8-ru")
    (*encoding-iso-8859-1* "US-ASCII" "ANSI_X3.4-1968" "iso-ir-6"
			   "ANSI_X3.4-1986" "ISO_646.irv:1991" "ASCII"
			   "ISO646-US" "us" "IBM367" "cp367" "csASCII")
    (*encoding-windows-latin1* "Windows-1252")
    (*encoding-windows-latin2* "Windows-1250" "x-cp1250")
    (*encoding-windows-cyrillic* "Windows-1251" "x-cp1251")
    (*encoding-windows-greek* "Windows-1253")
    (*encoding-windows-turkish* "Windows-1254")
    (*encoding-windows-baltic* "Windows-1257")))

(defvar *mime-charset-name-hash-table*
  (let ((hash (make-hash-table :test #'equalp)))
    (mapc #'(lambda (al)
	      (let ((encoding (symbol-value (car al))))
		(mapc #'(lambda (name)
			  (setf (gethash name hash) encoding))
		      (cdr al))))
	  *character-set-alist*)
    hash))

(defconstant *eol-lf* 0)
(defconstant *eol-crlf* 1)
(defconstant *eol-cr* 2)
(defconstant *eol-guess* 3)

(export '(map-sjis-to-internal map-euc-to-internal map-jis-to-internal
	  map-utf-8-to-internal map-utf-7-to-internal
	  map-utf-5-to-internal map-ucs-2-to-internal
	  map-internal-to-sjis map-internal-to-euc map-internal-to-jis
	  map-internal-to-utf-8 map-internal-to-utf-7
	  map-internal-to-utf-5 map-internal-to-ucs-2))

(defun map-sjis-to-internal (input-string-or-stream &optional output-stream)
  (convert-encoding-to-internal *encoding-sjis* input-string-or-stream output-stream))

(defun map-euc-to-internal (input-string-or-stream &optional output-stream)
  (convert-encoding-to-internal *encoding-euc-jp* input-string-or-stream output-stream))

(defun map-jis-to-internal (input-string-or-stream &optional output-stream)
  (convert-encoding-to-internal *encoding-jis* input-string-or-stream output-stream))

(defun map-utf-8-to-internal (input-string-or-stream &optional output-stream)
  (convert-encoding-to-internal *encoding-utf8* input-string-or-stream output-stream))

(defun map-utf-7-to-internal (input-string-or-stream &optional output-stream imap4-mailbox-name-p)
  (convert-encoding-to-internal (if imap4-mailbox-name-p *encoding-imap4* *encoding-utf7*)
				input-string-or-stream output-stream))

(defun map-utf-5-to-internal (input-string-or-stream &optional output-stream)
  (convert-encoding-to-internal *encoding-utf5* input-string-or-stream output-stream))

(defun map-ucs-2-to-internal (input-string-or-stream &optional output-stream big-endian-p)
  (convert-encoding-to-internal (if big-endian-p *encoding-utf16be* *encoding-utf16le*)
				input-string-or-stream output-stream))

(defun map-internal-to-sjis (input-string-or-stream &optional output-stream)
  (convert-encoding-from-internal *encoding-sjis* input-string-or-stream output-stream))

(defun map-internal-to-euc (input-string-or-stream &optional output-stream)
  (convert-encoding-from-internal *encoding-euc-jp* input-string-or-stream output-stream))

(defun map-internal-to-jis (input-string-or-stream &optional output-stream)
  (convert-encoding-from-internal *encoding-jis* input-string-or-stream output-stream))

(defun map-internal-to-utf-8 (input-string-or-stream &optional output-stream)
  (convert-encoding-from-internal *encoding-utf8n* input-string-or-stream output-stream))

(defun map-internal-to-utf-7 (input-string-or-stream
			      &optional output-stream
			      (direct-encode-white t)
			      (direct-encode-set-o t)
			      imap4-mailbox-name-p)
  (convert-encoding-from-internal (if imap4-mailbox-name-p *encoding-imap4* *encoding-utf7*)
				  input-string-or-stream output-stream))

(defun map-internal-to-utf-5 (input-string-or-stream &optional output-stream)
  (convert-encoding-from-internal *encoding-utf5* input-string-or-stream output-stream))

(defun map-internal-to-ucs-2 (input-string-or-stream &optional output-stream add-bom-p)
  (convert-encoding-from-internal (if add-bom-p *encoding-std-utf16le-bom* *encoding-std-utf16le*)
				  input-string-or-stream output-stream))

(export '(map-to-sjis map-jis-to-sjis map-euc-to-sjis map-utf-8-to-sjis))

(setf (symbol-function 'map-to-sjis) #'map-sjis-to-internal)
(setf (symbol-function 'map-jis-to-sjis) #'map-jis-to-internal)
(setf (symbol-function 'map-euc-to-sjis) #'map-euc-to-internal)
(setf (symbol-function 'map-utf-8-to-sjis) #'map-utf-8-to-internal)

(defun insert-char-table (fn)
  (save-excursion
    (do ((x 0 (+ x 32)))
	((>= x #x10000) t)
      (insert (format nil "~4,'0x: " x))
      (dotimes (y 32)
	(insert (funcall fn (+ x y))))
      (insert #\LFD))))

(defun insert-internal-char-table ()
  (interactive)
  (insert-char-table #'code-char))

(defun insert-unicode-char-table ()
  (interactive)
  (insert-char-table #'unicode-char))
