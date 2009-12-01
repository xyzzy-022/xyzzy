;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "iso8859-1")

(in-package "editor")

(defvar *iso-8859-1-map* nil)
(unless *iso-8859-1-map*
  (setq *iso-8859-1-map* (make-keymap))
  (define-key *iso-8859-1-map* #\SPC 'latin1-no-break-space)
  (define-key *iso-8859-1-map* #\! 'latin1-inverted-exclamation-mark)
  (define-key *iso-8859-1-map* '(#\" #\") 'latin1-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\A) 'latin1-A-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\E) 'latin1-E-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\I) 'latin1-I-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\O) 'latin1-O-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\U) 'latin1-U-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\a) 'latin1-a-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\e) 'latin1-e-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\i) 'latin1-i-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\o) 'latin1-o-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\u) 'latin1-u-diaeresis)
  (define-key *iso-8859-1-map* '(#\" #\y) 'latin1-y-diaeresis)
  (define-key *iso-8859-1-map* '(#\' #\') 'latin1-acute-accent)
  (define-key *iso-8859-1-map* '(#\' #\A) 'latin1-A-acute)
  (define-key *iso-8859-1-map* '(#\' #\E) 'latin1-E-acute)
  (define-key *iso-8859-1-map* '(#\' #\I) 'latin1-I-acute)
  (define-key *iso-8859-1-map* '(#\' #\O) 'latin1-O-acute)
  (define-key *iso-8859-1-map* '(#\' #\U) 'latin1-U-acute)
  (define-key *iso-8859-1-map* '(#\' #\Y) 'latin1-Y-acute)
  (define-key *iso-8859-1-map* '(#\' #\a) 'latin1-a-acute)
  (define-key *iso-8859-1-map* '(#\' #\e) 'latin1-e-acute)
  (define-key *iso-8859-1-map* '(#\' #\i) 'latin1-i-acute)
  (define-key *iso-8859-1-map* '(#\' #\o) 'latin1-o-acute)
  (define-key *iso-8859-1-map* '(#\' #\u) 'latin1-u-acute)
  (define-key *iso-8859-1-map* '(#\' #\y) 'latin1-y-acute)
  (define-key *iso-8859-1-map* #\$ 'latin1-currency-sign)
  (define-key *iso-8859-1-map* #\+ 'latin1-plus-minus-sign)
  (define-key *iso-8859-1-map* '(#\, #\,) 'latin1-cedilla)
  (define-key *iso-8859-1-map* '(#\, #\C) 'latin1-C-cedilla)
  (define-key *iso-8859-1-map* '(#\, #\c) 'latin1-c-cedilla)
  (define-key *iso-8859-1-map* #\- 'latin1-soft-hyphen)
  (define-key *iso-8859-1-map* #\. 'latin1-middle-dot)
  (define-key *iso-8859-1-map* '(#\/ #\/) 'latin1-division-sign)
  (define-key *iso-8859-1-map* '(#\/ #\O) 'latin1-O-stroke)
  (define-key *iso-8859-1-map* '(#\/ #\o) 'latin1-o-stroke)
  (define-key *iso-8859-1-map* '(#\1 #\/ #\2) 'latin1-1/2)
  (define-key *iso-8859-1-map* '(#\1 #\/ #\3) 'latin1-1/3)
  (define-key *iso-8859-1-map* '(#\1 #\/ #\4) 'latin1-1/4)
  (define-key *iso-8859-1-map* #\< 'latin1-<<)
  (define-key *iso-8859-1-map* #\= 'latin1-macron)
  (define-key *iso-8859-1-map* #\> 'latin1->>)
  (define-key *iso-8859-1-map* #\? 'latin1-inverted-question-mark)
  (define-key *iso-8859-1-map* #\A 'latin1-A-ring-above)
  (define-key *iso-8859-1-map* #\C 'latin1-copyright-sign)
  (define-key *iso-8859-1-map* #\D 'latin1-ETH)
  (define-key *iso-8859-1-map* #\E 'latin1-AE)
  (define-key *iso-8859-1-map* #\L 'latin1-pound-sign)
  (define-key *iso-8859-1-map* #\P 'latin1-pilcrow-sign)
  (define-key *iso-8859-1-map* #\R 'latin1-registered-sign)
  (define-key *iso-8859-1-map* #\S 'latin1-section-sign)
  (define-key *iso-8859-1-map* #\T 'latin1-THORN)
  (define-key *iso-8859-1-map* #\Y 'latin1-yen-sign)
  (define-key *iso-8859-1-map* '(#\^ #\1) 'latin1-superscript-one)
  (define-key *iso-8859-1-map* '(#\^ #\2) 'latin1-superscript-two)
  (define-key *iso-8859-1-map* '(#\^ #\3) 'latin1-superscript-three)
  (define-key *iso-8859-1-map* '(#\^ #\A) 'latin1-A-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\E) 'latin1-E-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\I) 'latin1-I-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\O) 'latin1-O-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\U) 'latin1-U-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\a) 'latin1-a-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\e) 'latin1-e-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\i) 'latin1-i-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\o) 'latin1-o-circumflex)
  (define-key *iso-8859-1-map* '(#\^ #\u) 'latin1-u-circumflex)
  (define-key *iso-8859-1-map* '(#\_ #\a) 'latin1-feminine-ordinal-indicator)
  (define-key *iso-8859-1-map* '(#\_ #\o) 'latin1-masculine-ordinal-indicator)
  (define-key *iso-8859-1-map* '(#\` #\A) 'latin1-A-grave)
  (define-key *iso-8859-1-map* '(#\` #\E) 'latin1-E-grave)
  (define-key *iso-8859-1-map* '(#\` #\I) 'latin1-I-grave)
  (define-key *iso-8859-1-map* '(#\` #\O) 'latin1-O-grave)
  (define-key *iso-8859-1-map* '(#\` #\U) 'latin1-U-grave)
  (define-key *iso-8859-1-map* '(#\` #\a) 'latin1-a-grave)
  (define-key *iso-8859-1-map* '(#\` #\e) 'latin1-e-grave)
  (define-key *iso-8859-1-map* '(#\` #\i) 'latin1-i-grave)
  (define-key *iso-8859-1-map* '(#\` #\o) 'latin1-o-grave)
  (define-key *iso-8859-1-map* '(#\` #\u) 'latin1-u-grave)
  (define-key *iso-8859-1-map* #\a 'latin1-a-ring-above)
  (define-key *iso-8859-1-map* #\c 'latin1-cent-sign)
  (define-key *iso-8859-1-map* #\d 'latin1-eth)
  (define-key *iso-8859-1-map* #\e 'latin1-ae)
  (define-key *iso-8859-1-map* #\o 'latin1-degree-sign)
  (define-key *iso-8859-1-map* #\s 'latin1-ss)
  (define-key *iso-8859-1-map* #\t 'latin1-thorn)
  (define-key *iso-8859-1-map* #\u 'latin1-micro-sign)
  (define-key *iso-8859-1-map* #\x 'latin1-multiplication-sign)
  (define-key *iso-8859-1-map* #\| 'latin1-broken-bar)
  (define-key *iso-8859-1-map* '(#\~ #\A) 'latin1-A-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\N) 'latin1-N-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\O) 'latin1-O-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\a) 'latin1-a-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\n) 'latin1-n-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\o) 'latin1-o-tilde)
  (define-key *iso-8859-1-map* '(#\~ #\~) 'latin1-not-sign)
  (define-key ctl-x-map #\8 *iso-8859-1-map*))

(let ((code #x20))
  (mapc #'(lambda (name)
	    (export name)
	    (setf (symbol-function name)
		  `(lambda ()
		     (interactive "*")
		     (insert ,(iso-code-char code :iso8859-1))))
	    (incf code))
	'(latin1-no-break-space
	  latin1-inverted-exclamation-mark
	  latin1-cent-sign
	  latin1-pound-sign
	  latin1-currency-sign
	  latin1-yen-sign
	  latin1-broken-bar
	  latin1-section-sign
	  latin1-diaeresis
	  latin1-copyright-sign
	  latin1-feminine-ordinal-indicator
	  latin1-<<
	  latin1-not-sign
	  latin1-soft-hyphen
	  latin1-registered-sign
	  latin1-macron
	  latin1-degree-sign
	  latin1-plus-minus-sign
	  latin1-superscript-two
	  latin1-superscript-three
	  latin1-acute-accent
	  latin1-micro-sign
	  latin1-pilcrow-sign
	  latin1-middle-dot
	  latin1-cedilla
	  latin1-superscript-one
	  latin1-masculine-ordinal-indicator
	  latin1->>
	  latin1-1/4
	  latin1-1/2
	  latin1-1/3
	  latin1-inverted-question-mark
	  latin1-A-grave
	  latin1-A-acute
	  latin1-A-circumflex
	  latin1-A-tilde
	  latin1-A-diaeresis
	  latin1-A-ring-above
	  latin1-AE
	  latin1-C-cedilla
	  latin1-E-grave
	  latin1-E-acute
	  latin1-E-circumflex
	  latin1-E-diaeresis
	  latin1-I-grave
	  latin1-I-acute
	  latin1-I-circumflex
	  latin1-I-diaeresis
	  latin1-ETH
	  latin1-N-tilde
	  latin1-O-grave
	  latin1-O-acute
	  latin1-O-circumflex
	  latin1-O-tilde
	  latin1-O-diaeresis
	  latin1-multiplication-sign
	  latin1-O-stroke
	  latin1-U-grave
	  latin1-U-acute
	  latin1-U-circumflex
	  latin1-U-diaeresis
	  latin1-Y-acute
	  latin1-THORN
	  latin1-ss
	  latin1-a-grave
	  latin1-a-acute
	  latin1-a-circumflex
	  latin1-a-tilde
	  latin1-a-diaeresis
	  latin1-a-ring-above
	  latin1-ae
	  latin1-c-cedilla
	  latin1-e-grave
	  latin1-e-acute
	  latin1-e-circumflex
	  latin1-e-diaeresis
	  latin1-i-grave
	  latin1-i-acute
	  latin1-i-circumflex
	  latin1-i-diaeresis
	  latin1-eth
	  latin1-n-tilde
	  latin1-o-grave
	  latin1-o-acute
	  latin1-o-circumflex
	  latin1-o-tilde
	  latin1-o-diaeresis
	  latin1-division-sign
	  latin1-o-stroke
	  latin1-u-grave
	  latin1-u-acute
	  latin1-u-circumflex
	  latin1-u-diaeresis
	  latin1-y-acute
	  latin1-thorn
	  latin1-y-diaeresis)))
