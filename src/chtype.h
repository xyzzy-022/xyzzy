// -*-C++-*-
#ifndef _chtype_h_
#define _chtype_h_

#include "cdecl.h"

#define CC_BEL 7
#define CC_BS 8
#define CC_HT 9
#define CC_TAB CC_HT
#define CC_NL 10
#define CC_LFD CC_NL
#define CC_VT 11
#define CC_FF 12
#define CC_CR 13
#define CC_RET CC_CR
#define CC_SO 14
#define CC_SI 15
#define CC_ESC 27
#define CC_SPC 32
#define CC_DEL 127
#define CC_SS2 142
#define CC_SS3 143

#define CC_META_BIT 0x8000

#define CCF_CHAR_MIN CCF_PRIOR
// See WINUSER.H
#define  CCF_PRIOR 0xff00
#define  CCF_NEXT 0xff01
#define  CCF_END 0xff02
#define  CCF_HOME 0xff03
#define  CCF_LEFT 0xff04
#define  CCF_UP 0xff05
#define  CCF_RIGHT 0xff06
#define  CCF_DOWN 0xff07
#define  CCF_SCROLL 0xff08
#define  CCF_MOUSEMOVE 0xff09
#define  CCF_PAUSE 0xff0a
#define  CCF_APPS 0xff0b
#define  CCF_INSERT 0xff0c
#define  CCF_DELETE 0xff0d
#define  CCF_HELP 0xff0e

#define  CCF_F1 0xff0f
#define  CCF_F2 0xff10
#define  CCF_F3 0xff11
#define  CCF_F4 0xff12
#define  CCF_F5 0xff13
#define  CCF_F6 0xff14
#define  CCF_F7 0xff15
#define  CCF_F8 0xff16
#define  CCF_F9 0xff17
#define  CCF_F10 0xff18
#define  CCF_F11 0xff19
#define  CCF_F12 0xff1a
#define  CCF_F13 0xff1b
#define  CCF_F14 0xff1c
#define  CCF_F15 0xff1d
#define  CCF_F16 0xff1e
#define  CCF_F17 0xff1f
#define  CCF_F18 0xff20
#define  CCF_F19 0xff21
#define  CCF_F20 0xff22
#define  CCF_F21 0xff23
#define  CCF_F22 0xff24
#define  CCF_F23 0xff25
#define  CCF_F24 0xff26
#define CCF_Fn_MAX CCF_F24

#define  CCF_LBTNDOWN 0xff27
#define  CCF_LBTNMOVE 0xff28
#define  CCF_LBTNUP 0xff29
#define  CCF_RBTNDOWN 0xff2a
#define  CCF_RBTNMOVE 0xff2b
#define  CCF_RBTNUP 0xff2c
#define  CCF_MBTNDOWN 0xff2d
#define  CCF_MBTNMOVE 0xff2e
#define  CCF_MBTNUP 0xff2f

#define CCF_CTLCHAR_MIN CCF_EXCLAM
#define  CCF_EXCLAM 0xff30
#define  CCF_DQUOTE 0xff31
#define  CCF_NUMBER 0xff32
#define  CCF_DOLLAR 0xff33
#define  CCF_PERCENT 0xff34
#define  CCF_AMPER 0xff35
#define  CCF_QUOTE 0xff36
#define  CCF_LPAREN 0xff37
#define  CCF_RPAREN 0xff38
#define CCF_CTLCHAR_MAX CCF_RPAREN

#define  CCF_ASTER 0xff70
#define  CCF_PLUS 0xff71
#define  CCF_COMMA 0xff72
#define  CCF_MINUS 0xff73
#define  CCF_DOT 0xff74
#define  CCF_SLASH 0xff75
#define  CCF_0 0xff76
#define  CCF_1 0xff77
#define  CCF_2 0xff78

#define  CCF_3 0xffb0
#define  CCF_4 0xffb1
#define  CCF_5 0xffb2
#define  CCF_6 0xffb3
#define  CCF_7 0xffb4
#define  CCF_8 0xffb5
#define  CCF_9 0xffb6
#define  CCF_COLON 0xffb7
#define  CCF_SEMI 0xffb8

#define  CCF_LT 0xfff0
#define  CCF_EQ 0xfff1
#define  CCF_GT 0xfff2
//#define  CCF_QUESTION  --> DEL
#define  CCF_BACKQ 0xfff3
#define  CCF_LBRACE 0xfff4
#define  CCF_VER 0xfff5
#define  CCF_RBRACE 0xfff6
#define  CCF_TILDE 0xfff7
#define  CCF_EMPTY_CHAR 0xfff8 // XXX

#define  CCF_XBTN1DOWN 0xff39
#define  CCF_XBTN1UP 0xff3a
#define  CCF_XBTN1MOVE 0xff3b
#define  CCF_XBTN2DOWN 0xff3c
#define  CCF_XBTN2UP 0xff3d
#define  CCF_XBTN2MOVE 0xff3e

#define CCF_CHAR_MAX CCF_XBTN2MOVE

#define CCF_CHAR_MASK 0x003f

#define NFUNCTION_KEYS (CCF_CHAR_MAX - CCF_CHAR_MIN + 1)

#define CCF_SHIFT_BIT 0x0040
#define CCF_CTRL_BIT 0x0080
#define CCF_FUNCTION_MASK 0xff00
#define CCF_META 0xfe00

#define LCHAR_MOUSE 0x10000
#define LCHAR_MENU 0x20000

#define _CTN 1
#define _CTU 2
#define _CTL 4
#define _CTK 8
#define _CTK1 0x10
#define _CTK2 0x20

#define UTF7_SET_D 1
#define UTF7_SET_O 2
#define UTF7_SET_B 4
#define UTF7_WHITE 8
#define UTF7_IMAP4_MAILBOX_NAME 16
#define UTF7_SHIFT_CHAR 32
#define UTF7_IMAP4_SHIFT_CHAR 64

#ifndef NOT_COMPILE_TIME

inline int
_char_type (int c)
{
  extern u_char char_type_table[];
  return (char_type_table + 1) [c];
}

inline int digit_char_p (int c) {return _char_type (c) & _CTN;}
inline int upper_char_p (int c) {return _char_type (c) & _CTU;}
inline int lower_char_p (int c) {return _char_type (c) & _CTL;}
inline int alpha_char_p (int c) {return _char_type (c) & (_CTL | _CTU);}
inline int alphanumericp (int c) {return _char_type (c) & (_CTL | _CTU | _CTN);}
inline int kana_char_p (int c) {return _char_type (c) & _CTK;}
inline int kanji_char_p (int c) {return _char_type (c) & _CTK1;}
inline int kanji2_char_p (int c) {return _char_type (c) & _CTK2;}
inline int SJISP (int c) {return kanji_char_p (c);}
inline int SJIS2P (int c) {return kanji2_char_p (c);}

inline int ascii_char_p (int c) {return u_int (c) < 128;}

inline int SBCP (Char c) {return c < 256;}
inline int DBCP (Char c) {return c >= 256;}
inline int digit_char_p (Char c)
  {return ascii_char_p (c) && digit_char_p (int (c));}
inline int upper_char_p (Char c)
  {return ascii_char_p (c) && upper_char_p (int (c));}
inline int lower_char_p (Char c)
  {return ascii_char_p (c) && lower_char_p (int (c));}
inline int alpha_char_p (Char c)
  {return ascii_char_p (c) && alpha_char_p (int (c));}
inline int alphanumericp (Char c)
  {return ascii_char_p (c) && alphanumericp (int (c));}
inline int kana_char_p (Char c)
  {return SBCP (c) && kana_char_p (int (c));}
inline int kanji_char_p (Char c)
  {return DBCP (c);}

inline int SBCP (lChar c)
  {return c < 256;}
inline int DBCP (lChar c)
  {return c >= 256 && c < CHAR_LIMIT;}
inline int digit_char_p (lChar c)
  {return ascii_char_p (c) && digit_char_p (int (c));}
inline int upper_char_p (lChar c)
  {return ascii_char_p (c) && upper_char_p (int (c));}
inline int lower_char_p (lChar c)
  {return ascii_char_p (c) && lower_char_p (int (c));}
inline int alpha_char_p (lChar c)
  {return ascii_char_p (c) && alpha_char_p (int (c));}
inline int alphanumericp (lChar c)
  {return ascii_char_p (c) && alphanumericp (int (c));}
inline int kana_char_p (lChar c)
  {return SBCP (c) && kana_char_p (int (c));}
inline int kanji_char_p (lChar c)
  {return DBCP (c);}

inline int
_char_downcase (int c)
{
  extern u_char char_translate_downcase_table[];
  return char_translate_downcase_table[c];
}

inline int
_char_upcase (int c)
{
  extern u_char char_translate_upcase_table[];
  return char_translate_upcase_table[c];
}

inline int
_char_transpose_case (int c)
{
  return c ^ 0x20;
}

inline int char_downcase (int c)
  {return ascii_char_p (c) ? _char_downcase (c) : c;}
inline int char_upcase (int c)
  {return ascii_char_p (c) ? _char_upcase (c) : c;}
inline int char_transpose_case (int c)
  {return alpha_char_p (c) ? _char_transpose_case (c) : c;}
inline Char char_downcase (Char c)
  {return ascii_char_p (c) ? Char (_char_downcase (c)) : c;}
inline Char char_upcase (Char c)
  {return ascii_char_p (c) ? Char (_char_upcase (c)) : c;}
inline Char char_transpose_case (Char c)
  {return alpha_char_p (c) ? (Char)_char_transpose_case (c) : c;}

inline int
_digit_char (int c)
{
  extern char char_numeric_table[];
  return char_numeric_table[c];
}

inline int
digit_char (int c)
{
  return ascii_char_p (c) ? _digit_char (c) : 36;
}

inline int
_digit_char_p (int c, int base)
{
  int n = _digit_char (c);
  return n < base ? n : -1;
}

inline int
digit_char_p (int c, int base)
{
  return ascii_char_p (c) ? _digit_char_p (c, base) : -1;
}

extern char upcase_digit_char[];
extern char downcase_digit_char[];

inline int
meta_char_p (Char c)
{
  return (c & CC_META_BIT && c <= CC_META_BIT + 127
          && ascii_char_p (c & ~CC_META_BIT));
}

inline Char
char_to_meta_char (Char c)
{
  return Char (c | CC_META_BIT);
}

inline Char
meta_char_to_char (Char c)
{
  return Char (c & ~CC_META_BIT);
}

inline Char
function_to_meta_function (Char c)
{
  return Char ((c & ~CCF_FUNCTION_MASK) | CCF_META);
}

inline Char
meta_function_to_function (Char c)
{
  return Char (c | CCF_FUNCTION_MASK);
}

inline int
function_char_p (Char c)
{
  if (c == CCF_EMPTY_CHAR)
    return 0;
  c &= ~(CCF_CTRL_BIT | CCF_SHIFT_BIT);
  return c >= CCF_CHAR_MIN && c <= CCF_CHAR_MAX;
}

inline int
meta_function_char_p (Char c)
{
  return ((c & CCF_FUNCTION_MASK) == CCF_META
          && function_char_p (meta_function_to_function (c)));
}

inline int
pseudo_ctlchar_p (Char c)
{
  c &= ~(CCF_CTRL_BIT | CCF_SHIFT_BIT);
  return c >= CCF_CTLCHAR_MIN && c <= CCF_CTLCHAR_MAX;
}

inline int
meta_pseudo_ctlchar_p (Char c)
{
  return ((c & CCF_FUNCTION_MASK) == CCF_META
          && pseudo_ctlchar_p (meta_function_to_function (c)));
}

inline int
base64_decode (int c)
{
  extern u_char base64_decode_table[];
  return c < 128 ? base64_decode_table[c] : 65;
}

inline int
imap4_base64_decode (int c)
{
  extern u_char imap4_base64_decode_table[];
  return c < 128 ? imap4_base64_decode_table[c] : 65;
}

inline u_char
utf7_set (int c)
{
  extern u_char utf7_set_table[];
  return utf7_set_table[c];
}

inline int
hqx_decode (int c)
{
  extern u_char hqx_decode_table[];
  return c < 128 ? hqx_decode_table[c] : 64;
}

extern u_char pseudo_char2ctl_table[];
extern u_char pseudo_ctl2char_table[];

#endif /* not NOT_COMPILE_TIME */

#endif
