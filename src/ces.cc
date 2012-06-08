#include "stdafx.h"
#include "ed.h"

static const lisp *const encoding_type_key[] =
{
  &Kauto_detect,
  &Ksjis,
  &Kbig5,
  &Kiso2022,
  &Kiso2022, // iso2022_noesc ÇÃèÍçáÇÕ :iso2022 Çï‘Ç∑
  &Kiso8859,
  &Kwindows_codepage,
  &Kutf7,
  &Kutf8,
  &Kutf16,
  &Kbinary,
  &Kutf5,
};


static lisp
make_char_encoding (encoding_type type, lisp name, lisp display_name)
{
  lchar_encoding *p = ldata <lchar_encoding, Tchar_encoding>::lalloc ();
  p->type = type;
  p->name = name;
  p->display_name = display_name;
  bzero (&p->u, sizeof p->u);
  xsymbol_value (Vinternal_char_encoding_list) =
    xcons (p, xsymbol_value (Vinternal_char_encoding_list));
  return p;
}

static lisp
create_char_encoding (encoding_type type, lisp name, lisp display_name,
                      int param, int param2 = 0)
{
  check_string (name);
  check_string (display_name);

  for (lisp p = xsymbol_value (Vinternal_char_encoding_list); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (char_encoding_p (x)
          && xchar_encoding_type (x) == type
          && string_equal (name, xchar_encoding_name (x))
          && string_equal (display_name, xchar_encoding_display_name (x)))
        switch (type)
          {
          case encoding_auto_detect:
          case encoding_sjis:
          case encoding_big5:
          case encoding_binary:
            return x;

          case encoding_iso8859:
            if (xchar_encoding_iso8859_charset (x) == param)
              return x;
            break;

          case encoding_windows_codepage:
            if (xchar_encoding_windows_codepage (x) == param)
              return x;
            break;

          case encoding_utf5:
          case encoding_utf7:
          case encoding_utf8:
          case encoding_utf16:
            if (xchar_encoding_utf_flags (x) == param
                && xchar_encoding_utf_cjk (x) == param2)
              return x;
            break;
          }
    }

  lisp x = make_char_encoding (type, name, display_name);
  switch (type)
    {
    case encoding_auto_detect:
    case encoding_sjis:
    case encoding_big5:
    case encoding_binary:
      break;

    case encoding_iso8859:
      xchar_encoding_iso8859_charset (x) = param;
      break;

    case encoding_windows_codepage:
      xchar_encoding_windows_codepage (x) = param;
      break;

    case encoding_utf5:
    case encoding_utf7:
    case encoding_utf8:
    case encoding_utf16:
      xchar_encoding_utf_flags (x) = param;
      xchar_encoding_utf_cjk (x) = param2;
      break;
    }
  return x;
}

lisp
Fmake_auto_detect_encoding (lisp name, lisp display_name)
{
  return create_char_encoding (encoding_auto_detect, name, display_name, 0);
}

lisp
Fmake_sjis_encoding (lisp name, lisp display_name)
{
  return create_char_encoding (encoding_sjis, name, display_name, 0);
}

lisp
Fmake_big5_encoding (lisp name, lisp display_name)
{
  return create_char_encoding (encoding_big5, name, display_name, 0);
}

lisp
Fmake_binary_encoding (lisp name, lisp display_name)
{
  return create_char_encoding (encoding_binary, name, display_name, 0);
}

static const struct {lisp *obj; int ccs;} obj2ccs[] =
{
  {&Qnil, ccs_invalid},
  {&Kus_ascii, ccs_usascii},
  {&Kjisx0201_kana, ccs_jisx0201_kana},
  {&Kiso8859_1, ccs_iso8859_1},
  {&Kiso8859_2, ccs_iso8859_2},
  {&Kiso8859_3, ccs_iso8859_3},
  {&Kiso8859_4, ccs_iso8859_4},
  {&Kiso8859_5, ccs_iso8859_5},
  {&Kiso8859_7, ccs_iso8859_7},
  {&Kiso8859_9, ccs_iso8859_9},
  {&Kiso8859_10, ccs_iso8859_10},
  {&Kiso8859_13, ccs_iso8859_13},
  {&Kjisx0208, ccs_jisx0208},
  {&Kjisx0212, ccs_jisx0212},
  {&Kgb2312, ccs_gb2312},
  {&Kksc5601, ccs_ksc5601},
  {&Kbig5_1, ccs_big5_1},
  {&Kbig5_2, ccs_big5_2},
  {&Kcns11643_1, ccs_cns11643_1},
  {&Kcns11643_2, ccs_cns11643_2},
};

static int
to_charset (lisp lcharset)
{
  for (int i = 0; i < numberof (obj2ccs); i++)
    if (lcharset == *obj2ccs[i].obj)
      return obj2ccs[i].ccs;
  FEsimple_error (Eunknown_charset, lcharset);
  return 0;
}

static lisp
from_charset (int charset)
{
  for (int i = 0; i < numberof (obj2ccs); i++)
    if (charset == obj2ccs[i].ccs)
      return *obj2ccs[i].obj;
  assert (0);
  return Qnil;
}

int
to_lang (lisp lang)
{
  if (lang == Kjp)
    return ENCODING_LANG_JP;
  if (lang == Kjp2)
    return ENCODING_LANG_JP2;
  if (lang == Kkr)
    return ENCODING_LANG_KR;
  if (lang == Kcn)
    return ENCODING_LANG_CN;
  if (lang == Kcn_gb)
    return ENCODING_LANG_CN_GB;
  if (lang == Kcn_big5)
    return ENCODING_LANG_CN_BIG5;
  return ENCODING_LANG_NIL;
}

lisp
from_lang (int lang)
{
  switch (lang)
    {
    case ENCODING_LANG_JP:
      return Kjp;
    case ENCODING_LANG_JP2:
      return Kjp2;
    case ENCODING_LANG_KR:
      return Kkr;
    case ENCODING_LANG_CN:
      return Kcn;
    case ENCODING_LANG_CN_GB:
      return Kcn_gb;
    case ENCODING_LANG_CN_BIG5:
      return Kcn_big5;
    default:
      return Qnil;
    }
}

int
to_vender_code (lisp vender)
{
  if (vender == Kibmext)
    return ENCODING_ISO_VENDER_IBMEXT;
  if (vender == Knecext)
    return ENCODING_ISO_VENDER_NECEXT;
  if (vender == Kosfjvc)
    return ENCODING_ISO_VENDER_OSFJVC;
  return ENCODING_ISO_VENDER_NIL;
}

static lisp
from_vender_code (int code)
{
  switch (code)
    {
    case ENCODING_ISO_VENDER_IBMEXT:
      return Kibmext;
    case ENCODING_ISO_VENDER_NECEXT:
      return Knecext;
    case ENCODING_ISO_VENDER_OSFJVC:
      return Kosfjvc;
    default:
      return Qnil;
    }
}

static void
init_designation (u_char &initial, u_int &designatable, lisp lcharset)
{
  if (!consp (lcharset))
    {
      initial = to_charset (lcharset);
      designatable = u_int (-1);
    }
  else
    {
      designatable = 0;
      int n = 0;
      do
        {
          int ccs = to_charset (xcar (lcharset));
          if (!n)
            initial = ccs;
          if (ccs != ccs_invalid)
            designatable |= 1 << ccs;
          n++;
          lcharset = xcdr (lcharset);
          QUIT;
        }
      while (consp (lcharset));
    }
}

lisp
Fmake_iso2022_encoding (lisp name, lisp display_name, lisp keys)
{
  check_string (name);
  check_string (display_name);

  u_char initial[4];
  u_int designatable[4];
  init_designation (initial[0], designatable[0], find_keyword (Kg0, keys));
  init_designation (initial[1], designatable[1], find_keyword (Kg1, keys));
  init_designation (initial[2], designatable[2], find_keyword (Kg2, keys));
  init_designation (initial[3], designatable[3], find_keyword (Kg3, keys));
  if (initial[0] == ccs_invalid)
    initial[0] = ccs_usascii;

  int cjk = to_lang (find_keyword (Kcjk, keys));

  int flags = 0;
  if (find_keyword_bool (Kascii_eol, keys))
    flags |= ENCODING_ISO_ASCII_EOL;
  if (find_keyword_bool (Kascii_control, keys))
    flags |= ENCODING_ISO_ASCII_CTRL;
  if (find_keyword_bool (K7bits, keys))
    flags |= ENCODING_ISO_7BITS;
  if (find_keyword_bool (Klocking_shift, keys))
    flags |= ENCODING_ISO_LOCKING_SHIFT;
  if (find_keyword_bool (Kshort_form, keys))
    flags |= ENCODING_ISO_SHORT_FORM;
  if (find_keyword_bool (Kuse_cns11643, keys))
    flags |= ENCODING_ISO_USE_CNS11643;
  flags |= to_vender_code (find_keyword (Kvender, keys));

  encoding_type type = (find_keyword_bool (Kno_escape, keys)
                        ? encoding_iso2022_noesc
                        : encoding_iso2022);

  for (lisp p = xsymbol_value (Vinternal_char_encoding_list); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (char_encoding_p (x)
          && xchar_encoding_type (x) == type
          && string_equal (name, xchar_encoding_name (x))
          && string_equal (display_name, xchar_encoding_display_name (x))
          && xchar_encoding_iso_flags (x) == flags
          && xchar_encoding_iso_cjk (x) == cjk
          && !memcmp (xchar_encoding_iso_initial (x), initial, sizeof initial)
          && !memcmp (xchar_encoding_iso_designatable (x), designatable,
                      sizeof designatable))
        return x;
    }

  lisp ce = make_char_encoding (type, name, display_name);
  memcpy (xchar_encoding_iso_initial (ce), initial, sizeof initial);
  memcpy (xchar_encoding_iso_designatable (ce), designatable, sizeof designatable);
  xchar_encoding_iso_flags (ce) = flags;
  xchar_encoding_iso_cjk (ce) = cjk;
  return ce;
}

lisp
Fmake_iso8859_encoding (lisp name, lisp display_name, lisp lcharset)
{
  int charset = to_charset (lcharset);
  switch (charset)
    {
    default:
      FEsimple_error (Eunsupported_charset, lcharset);

    case ccs_iso8859_1:
    case ccs_iso8859_2:
    case ccs_iso8859_3:
    case ccs_iso8859_4:
    case ccs_iso8859_5:
    case ccs_iso8859_7:
    case ccs_iso8859_9:
    case ccs_iso8859_10:
    case ccs_iso8859_13:
      break;
    }

  return create_char_encoding (encoding_iso8859, name, display_name, charset);
}

lisp
Fmake_windows_codepage_encoding (lisp name, lisp display_name, lisp lcodepage)
{
  int codepage = fixnum_value (lcodepage);
  switch (codepage)
    {
    default:
      FEsimple_error (Eunsupported_codepage, lcodepage);

    case CP_JAPANESE:
    case CP_KOREAN:
    case CP_CN_TRADITIONAL:
    case CP_CN_SIMPLIFIED:
    case CP_LATIN1:
    case CP_LATIN2:
    case CP_CYRILLIC:
    case CP_GREEK:
    case CP_TURKISH:
    case CP_BALTIC:
    case CP_KOI8R:
    case CP_PSEUDO_KOI8U:
      break;
    }

  return create_char_encoding (encoding_windows_codepage, name, display_name, codepage);
}

static lisp
make_utf_encoding (encoding_type type, lisp name, lisp display_name, lisp keys)
{
  int flags = 0;

  if (find_keyword_bool (Ksignature, keys))
    flags |= ENCODING_UTF_SIGNATURE;
  if (find_keyword_bool (Kwindows, keys))
    flags |= ENCODING_UTF_WINDOWS;

  lisp byte_order = find_keyword (Kbyte_order, keys);
  if (byte_order == Kbig_endian)
    flags |= ENCODING_UTF_BE;
  else if (byte_order == Klittle_endian)
    flags |= ENCODING_UTF_LE;

  if (type == encoding_utf7)
    {
      if (find_keyword_bool (Kimap4_mailbox_name, keys))
        flags |= UTF7_IMAP4_MAILBOX_NAME;
      else
        {
          flags |= UTF7_SET_D;
          if (find_keyword_bool (Kdirect_encode_white, keys, Qt))
            flags |= UTF7_WHITE;
          if (find_keyword_bool (Kdirect_encode_set_o, keys, Qt))
            flags |= UTF7_SET_O;
        }
    }

  int cjk = to_lang (find_keyword (Kcjk, keys));
  switch (cjk)
    {
    default:
      flags |= ENCODING_UTF_WINDOWS;
      break;

    case ENCODING_LANG_JP:
    case ENCODING_LANG_JP2:
    case ENCODING_LANG_NIL:
      break;
    }

  return create_char_encoding (type, name, display_name, flags, cjk);
}

lisp
Fmake_utf5_encoding (lisp name, lisp display_name, lisp keys)
{
  return make_utf_encoding (encoding_utf5, name, display_name, keys);
}

lisp
Fmake_utf7_encoding (lisp name, lisp display_name, lisp keys)
{
  return make_utf_encoding (encoding_utf7, name, display_name, keys);
}

lisp
Fmake_utf8_encoding (lisp name, lisp display_name, lisp keys)
{
  return make_utf_encoding (encoding_utf8, name, display_name, keys);
}

lisp
Fmake_utf16_encoding (lisp name, lisp display_name, lisp keys)
{
  return make_utf_encoding (encoding_utf16, name, display_name, keys);
}

lisp
Fchar_encoding_name (lisp encoding)
{
  check_char_encoding (encoding);
  return xchar_encoding_name (encoding);
}

lisp
Fchar_encoding_display_name (lisp encoding)
{
  check_char_encoding (encoding);
  return xchar_encoding_display_name (encoding);
}

lisp
Fchar_encoding_type (lisp encoding)
{
  check_char_encoding (encoding);
  return *encoding_type_key[xchar_encoding_type (encoding)];
}

lisp
Fchar_encoding_signature (lisp encoding)
{
  check_char_encoding (encoding);
  switch (xchar_encoding_type (encoding))
    {
    case encoding_utf5:
    case encoding_utf7:
    case encoding_utf8:
    case encoding_utf16:
      return boole (xchar_encoding_utf_flags (encoding) & ENCODING_UTF_SIGNATURE);
    default:
      return Qnil;
    }
}

lisp
find_char_encoding (lisp name)
{
  for (lisp p = xsymbol_value (Vinternal_char_encoding_list); consp (p); p = xcdr (p))
    {
      lisp encoding = xcar (p);
      if (char_encoding_p (encoding)
          && string_equal (name, xchar_encoding_name (encoding)))
        return encoding;
    }
  return FEsimple_error (Eunknown_char_encoding, name);
}

void
init_char_encoding ()
{
  xsymbol_value (Vinternal_char_encoding_list) = Qnil;

  lisp ce;

  ce = make_char_encoding (encoding_auto_detect,
                           make_string ("auto"), make_string ("é©ìÆîªíË"));
  xsymbol_value (Qencoding_auto) = ce;
  xsymbol_value (Vencoding_auto) = ce;

  ce = Fmake_sjis_encoding (make_string ("sjis"), make_string ("ì˙ñ{åÍ(Shift_JIS)"));
  xsymbol_value (Qencoding_sjis) = ce;
  xsymbol_value (Vencoding_sjis) = ce;

  xsymbol_value (Vdefault_fileio_encoding) = xsymbol_value (Vencoding_sjis);
  xsymbol_value (Vexpected_fileio_encoding) = xsymbol_value (Vencoding_auto);
}

static lisp
make_designation (u_char initial, u_int designatable)
{
  if (designatable == u_int (-1))
    return from_charset (initial);

  lisp p = Qnil;
  for (int i = ccs_max - 1; i >= 0; i--)
    if (i != initial && designatable & (1 << i))
      p = xcons (from_charset (i), p);
  p = xcons (from_charset (initial), p);
  return xcons (Qquote, xcons (p, Qnil));
}

static lisp
utf_encoding_fn (lisp x)
{
  switch (xchar_encoding_type (x))
    {
    case encoding_utf5:
      return Smake_utf5_encoding;

    case encoding_utf7:
      return Smake_utf7_encoding;

    case encoding_utf8:
      return Smake_utf8_encoding;

    default:
      return Smake_utf16_encoding;
    }
}

lisp
make_char_encoding_constructor (lisp x)
{
  switch (xchar_encoding_type (x))
    {
    case encoding_auto_detect:
      return make_list (Smake_auto_detect_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        0);

    case encoding_sjis:
      return make_list (Smake_sjis_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        0);

    case encoding_big5:
      return make_list (Smake_big5_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        0);

    case encoding_binary:
      return make_list (Smake_binary_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        0);

    case encoding_iso2022:
    case encoding_iso2022_noesc:
      return make_list (Smake_iso2022_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        Kno_escape,
                        boole (xchar_encoding_type (x) == encoding_iso2022_noesc),
                        Kg0,
                        make_designation (xchar_encoding_iso_initial (x)[0],
                                          xchar_encoding_iso_designatable (x)[0]),
                        Kg1,
                        make_designation (xchar_encoding_iso_initial (x)[1],
                                          xchar_encoding_iso_designatable (x)[1]),
                        Kg2,
                        make_designation (xchar_encoding_iso_initial (x)[2],
                                          xchar_encoding_iso_designatable (x)[2]),
                        Kg3,
                        make_designation (xchar_encoding_iso_initial (x)[3],
                                          xchar_encoding_iso_designatable (x)[3]),
                        Kcjk,
                        from_lang (xchar_encoding_iso_cjk (x)),
                        Kascii_eol,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_ASCII_EOL),
                        Kascii_control,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_ASCII_CTRL),
                        K7bits,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_7BITS),
                        Klocking_shift,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_LOCKING_SHIFT),
                        Kshort_form,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_SHORT_FORM),
                        Kuse_cns11643,
                        boole (xchar_encoding_iso_flags (x) & ENCODING_ISO_USE_CNS11643),
                        Kvender,
                        from_vender_code (xchar_encoding_iso_flags (x) & ENCODING_ISO_VENDER_MASK),
                        0);

    case encoding_iso8859:
      return make_list (Smake_iso8859_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        from_charset (xchar_encoding_iso8859_charset (x)),
                        0);

    case encoding_windows_codepage:
      return make_list (Smake_windows_codepage_encoding,
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        make_fixnum (xchar_encoding_windows_codepage (x)),
                        0);

    case encoding_utf5:
    case encoding_utf7:
    case encoding_utf8:
    case encoding_utf16:
      return make_list (utf_encoding_fn (x),
                        xchar_encoding_name (x),
                        xchar_encoding_display_name (x),
                        Ksignature,
                        boole (xchar_encoding_utf_flags (x) & ENCODING_UTF_SIGNATURE),
                        Kwindows,
                        boole (xchar_encoding_utf_flags (x) & ENCODING_UTF_WINDOWS),
                        Kbyte_order,
                        (xchar_encoding_utf_flags (x) & ENCODING_UTF_BE
                         ? Kbig_endian
                         : (xchar_encoding_utf_flags (x) &  ENCODING_UTF_LE
                            ? Klittle_endian
                            : Qnil)),
                        Kcjk,
                        from_lang (xchar_encoding_utf_cjk (x)),
                        xchar_encoding_type (x) == encoding_utf7 ? Kimap4_mailbox_name : 0,
                        boole (xchar_encoding_utf_flags (x) & UTF7_IMAP4_MAILBOX_NAME),
                        Kdirect_encode_white,
                        boole (xchar_encoding_utf_flags (x) & UTF7_WHITE),
                        Kdirect_encode_set_o,
                        boole (xchar_encoding_utf_flags (x) & UTF7_SET_O),
                        0);

    default:
      assert (0);
      return Qnil;
    }
}

lisp
symbol_value_char_encoding (lisp x)
{
  x = xsymbol_value (x);
  if (char_encoding_p (x) && xchar_encoding_type (x) != encoding_auto_detect)
    return x;
  return xsymbol_value (Qencoding_sjis);
}

static int
parse_encoding_to_charset (lisp lcharset)
{
  if (lcharset == Kcp932)
    return ccs_pseudo_cp932;
  if (lcharset == Kbig5)
    return ccs_big5;
  return to_charset (lcharset);
}

static int
parse_encoding_init (lisp laccept)
{
  if (laccept == Qt)
    return -1;
  int accept = 0;
  for (; consp (laccept); laccept = xcdr (laccept))
    accept |= 1 << parse_encoding_to_charset (xcar (laccept));
  if (accept & (1 << ccs_pseudo_cp932))
    accept |= 1 << ccs_jisx0208;
  if (accept & (1 << ccs_big5_2))
    accept |= 1 << ccs_big5;
  return accept;
}

static lisp
parse_encoding_finish (int accept, int found)
{
  lisp r = Qnil;
  if (accept & (1 << ccs_pseudo_cp932) && found & (1 << ccs_jisx0208))
    {
      found &= ~(1 << ccs_jisx0208);
      r = xcons (Kcp932, r);
    }
  if (found & ((1 << ccs_big5_1) | (1 << ccs_big5_2)))
    {
      found &= ~((1 << ccs_big5_1) | (1 << ccs_big5_2));
      r = xcons (Kbig5, r);
    }
  for (int i = ccs_max - 1; i >= 0; i--)
    if (found & (1 << i))
      r = xcons (from_charset (i), r);
  return r;
}

static int
parse_encoding_exec (Char cc, int accept, int &found)
{
  if (cc != DEFCHAR)
    {
      cc = convert_ibmext (cc);
      if (cc == DEFCHAR)
        return 0;
    }

  int ccs = code_charset (cc);
  if (!(accept & (1 << ccs)))
    return 0;

  found |= 1 << ccs;
  switch (ccs)
    {
    default:
      return 0;

    case ccs_usascii:
      return 1;

    case ccs_iso8859_1:
    case ccs_iso8859_2:
    case ccs_iso8859_3:
    case ccs_iso8859_4:
    case ccs_iso8859_5:
    case ccs_iso8859_7:
    case ccs_iso8859_9:
    case ccs_iso8859_10:
    case ccs_iso8859_13:
      return (cc & 0x7f) >= 0x20;

    case ccs_jisx0201_kana:
      return kana_char_p (cc);

    case ccs_jisx0212:
      return cc <= CCS_JISX0212_MAX;

    case ccs_gb2312:
      return cc <= CCS_GB2312_MAX;

    case ccs_ksc5601:
      return cc <= CCS_KSC5601_MAX;

    case ccs_big5_1:
    case ccs_big5_2:
      return cc <= CCS_BIG5_MAX;

    case ccs_jisx0208:
      if (!(accept & (1 << ccs_pseudo_cp932))
          && ((cc >= 0x8540 && cc <= 0x889e) || cc >= 0xeb40))
        return 0;
      else
        {
          int c1 = cc >> 8, c2 = cc & 255;
          return SJISP (c1) && SJIS2P (c2);
        }
    }
}

lisp
Fparse_char_encoding_region (lisp laccept, lisp from, lisp to)
{
  int accept = parse_encoding_init (laccept);
  int found = 0;

  Buffer *bp = selected_buffer ();
  point_t p1 = bp->coerce_to_restricted_point (from);
  point_t p2 = bp->coerce_to_restricted_point (to);
  if (p1 > p2)
    swap (p1, p2);

  Point point;
  bp->set_point (point, p1);

  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (!parse_encoding_exec (c, accept, found))
        return make_fixnum (point.p_point);
      if (!bp->forward_char (point, 1))
        break;
    }
  return parse_encoding_finish (accept, found);
}

lisp
Fparse_char_encoding_string (lisp laccept, lisp string, lisp keys)
{
  int accept = parse_encoding_init (laccept);
  int found = 0;
  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  for (const Char *p = xstring_contents (string) + start,
       *const pe = xstring_contents (string) + end;
       p < pe; p++)
    if (!parse_encoding_exec (*p, accept, found))
      return make_fixnum (p - xstring_contents (string));
  return parse_encoding_finish (accept, found);
}
