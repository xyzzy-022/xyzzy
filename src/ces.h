// -*-C++-*-
#ifndef _ces_h_
#define _ces_h_

enum encoding_type
{
  encoding_auto_detect,
  encoding_sjis,
  encoding_big5,
  encoding_iso2022,
  encoding_iso2022_noesc,
  encoding_iso8859,
  encoding_windows_codepage,
  encoding_utf7,
  encoding_utf8,
  encoding_utf16,
  encoding_binary,
  encoding_utf5
};

#define ENCODING_ISO_ASCII_EOL     0x0001
#define ENCODING_ISO_ASCII_CTRL    0x0002
#define ENCODING_ISO_7BITS         0x0004
#define ENCODING_ISO_LOCKING_SHIFT 0x0008
#define ENCODING_ISO_SHORT_FORM    0x0010
#define ENCODING_ISO_USE_CNS11643  0x0020
#define ENCODING_ISO_VENDER_MASK   0x0300
#define ENCODING_ISO_VENDER_NIL    0x0000
#define ENCODING_ISO_VENDER_IBMEXT 0x0100
#define ENCODING_ISO_VENDER_NECEXT 0x0200
#define ENCODING_ISO_VENDER_OSFJVC 0x0300

#define ENCODING_LANG_NIL     0
#define ENCODING_LANG_JP      1
#define ENCODING_LANG_KR      2
#define ENCODING_LANG_CN      3
#define ENCODING_LANG_CN_GB   4
#define ENCODING_LANG_CN_BIG5 5
#define ENCODING_LANG_JP2     6

#define ENCODING_UTF_SIGNATURE 0x100
#define ENCODING_UTF_BE        0x200
#define ENCODING_UTF_LE        0x400
#define ENCODING_UTF_WINDOWS   0x800

class lchar_encoding: public lisp_object
{
public:
  encoding_type type;
  lisp name;
  lisp display_name;
  union
    {
      struct
        {
          int flags;
          u_int designatable[4];
          u_char cjk;
          u_char initial[4];
        } iso;
      struct
        {
          int flags;
          u_char cjk;
        } utf;
      int iso8859_charset;
      int windows_codepage;
    } u;
};

#define char_encoding_p(X) typep ((X), Tchar_encoding)

inline void
check_char_encoding (lisp x)
{
  check_type (x, Tchar_encoding, Qchar_encoding);
}

inline encoding_type &
xchar_encoding_type (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->type;
}

inline lisp &
xchar_encoding_name (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->name;
}

inline lisp &
xchar_encoding_display_name (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->display_name;
}

inline int &
xchar_encoding_iso_flags (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.iso.flags;
}

inline u_char *
xchar_encoding_iso_initial (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.iso.initial;
}

inline u_int *
xchar_encoding_iso_designatable (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.iso.designatable;
}

inline u_char &
xchar_encoding_iso_cjk (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.iso.cjk;
}

inline int &
xchar_encoding_utf_flags (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.utf.flags;
}

inline u_char &
xchar_encoding_utf_cjk (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.utf.cjk;
}

inline int &
xchar_encoding_iso8859_charset (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.iso8859_charset;
}

inline int &
xchar_encoding_windows_codepage (lisp x)
{
  assert (char_encoding_p (x));
  return ((lchar_encoding *)x)->u.windows_codepage;
}

#endif
