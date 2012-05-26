#include "stdafx.h"
#include "ed.h"
#include "encoding.h"
#include "ibmext.h"
#include "utf2sjis.h"

u_char escseq_euckr[] = {ccs_usascii, ccs_ksc5601, ccs_invalid, ccs_invalid};
u_char escseq_eucgb[] = {ccs_usascii, ccs_gb2312, ccs_invalid, ccs_invalid};
u_int designatable_any[] = {u_int (-1), u_int (-1), u_int (-1), u_int (-1)};

const Char *
cjk_translate_table (int lang)
{
  switch (lang)
    {
    case ENCODING_LANG_JP:
    case ENCODING_LANG_JP2:
      return wc2cp932_table;

    case ENCODING_LANG_KR:
      init_wc2ksc5601_table ();
      return wc2ksc5601_table;

    case ENCODING_LANG_CN_GB:
      init_wc2gb2312_table ();
      return wc2gb2312_table;

    case ENCODING_LANG_CN_BIG5:
      init_wc2big5_table ();
      return wc2big5_table;

    case ENCODING_LANG_CN:
      init_wc2gb2312_table ();
      init_wc2big5_table ();
      return 0;

    default:
      return 0;
    }
}

int
xbuffered_read_stream::refill ()
{
  do
    {
      begin ();
      refill_internal ();
    }
  while (head () == tail () && head () != base ());
  return setbuf (head (), tail ());
}

void
sjis_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        break;
      if (SJISP (c1))
        {
          int c2 = s_in.get ();
          if (c2 != eof)
            c1 = (c1 << 8) + c2;
        }
      put (c1);
    }
}

void
fast_sjis_to_internal_stream::refill_internal ()
{
  const u_char *rs, *rse;
  Char *rd, *rde;
  s_in.begin_direct_input (rs, rse);
  begin_direct_output (rd, rde);

  const u_char *s = rs, *const se = rse;
  Char *d = rd, *const de = rde;
  for (; d < de && s < se; d++)
    {
      int c1 = *s++;
      if (SJISP (c1) && s < se)
        c1 = (c1 << 8) + *s++;
      *d = c1;
    }
  s_in.end_direct_input (s);
  end_direct_output (d);
}

Char
jisx0212_to_internal (int c1, int c2, int vender)
{
  if (vender == ENCODING_ISO_VENDER_OSFJVC && c1 >= 0x75)
    {
      c1 += 20;
      return (j2sh (c1, c2) << 8) | j2sl (c1, c2);
    }

  if (vender != ENCODING_ISO_VENDER_OSFJVC
      && vender != ENCODING_ISO_VENDER_IBMEXT)
    return jisx0212_to_int (c1, c2);

  if (c1 == 0x74)
    return ibmext_eucjp2sjis_table[c2 - (0x21 - (0x7f - 0x73))];
  if (c1 == 0x73 && c2 >= 0x73)
    return ibmext_eucjp2sjis_table[c2 - 0x73];
  Char cc = jisx0212_to_int (c1, c2);
  Char t = w2i (i2w (cc));
  if (t != Char (-1))
    return t;
  return cc;
}

iso2022_noesc_to_internal_stream::iso2022_noesc_to_internal_stream (xinput_stream <u_char> &in,
                                                                    const u_char *g,
                                                                    int flags)
     : xbuffered_read_stream (in),
       s_vender (vender_depend_code (flags & ENCODING_ISO_VENDER_MASK))
{
  memcpy (s_g, g, 4);
  init_cns11643_table ();
}

void
iso2022_noesc_to_internal_stream::to_internal (u_char ccs, int c1, int oc1)
{
  if (ccs_1byte_charset_p (ccs))
    put ((ccs_1byte_94_charset_p (ccs)
          ? c1 <= ' ' || c1 >= 0x7f : c1 < ' ')
         ? oc1 : (ccs << 7) | c1);
  else
    {
      if (c1 <= 0x20 || c1 >= 0x7f)
        put (oc1);
      else
        {
          int oc2 = s_in.get ();
          if (oc2 == eof)
            {
              put (oc1);
              return;
            }
          int c2 = oc2 & 127;
          if (c2 <= 0x20 || c2 >= 0x7f)
            {
              put (oc1);
              s_in.putback (oc2);
            }
          else
            switch (ccs)
              {
              case ccs_jisx0208:
                if (s_vender == ENCODING_ISO_VENDER_OSFJVC && c1 >= 0x75)
                  c1 += 10;
                put ((j2sh (c1, c2) << 8) | j2sl (c1, c2));
                break;

              case ccs_jisx0212:
                put (jisx0212_to_internal (c1, c2, s_vender));
                break;

              case ccs_gb2312:
                put (gb2312_to_int (c1, c2));
                break;

              case ccs_big5_1:
              case ccs_big5_2:
                mule_g2b (ccs, c1, c2);
                put (big5_to_int (c1, c2));
                break;

              case ccs_cns11643_1:
                {
                  Char cc = cns11643_1_to_internal[c1 * 94 + c2 - (0x21 * 94 + 0x21)];
                  if (cc != Char (-1))
                    put (cc);
                  else
                    {
                      put (oc1);
                      put (oc2);
                    }
                  break;
                }

              case ccs_cns11643_2:
                {
                  Char cc = cns11643_2_to_internal[c1 * 94 + c2 - (0x21 * 94 + 0x21)];
                  if (cc != Char (-1))
                    put (cc);
                  else
                    {
                      put (oc1);
                      put (oc2);
                    }
                  break;
                }

              case ccs_ksc5601:
                put (ksc5601_to_int (c1, c2));
                break;

              default:
                assert (0);
                put (oc1);
                put (oc2);
                break;
              }
        }
    }
}

void
iso2022_noesc_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      if (c < 128)
        to_internal (s_g[0], c, c);
      else if (c == CC_SS2)
        {
          if (s_g[2] == ccs_invalid)
            put (c);
          else
            {
              c = s_in.get ();
              if (c == eof)
                break;
              to_internal (s_g[2], c & 127, c);
            }
        }
      else if (c == CC_SS3)
        {
          if (s_g[3] == ccs_invalid)
            put (c);
          else
            {
              c = s_in.get ();
              if (c == eof)
                break;
              to_internal (s_g[3], c & 127, c);
            }
        }
      else
        to_internal (s_g[1], c & 127, c);
    }
}

int
iso2022_to_internal_stream::designate94 (u_char &g, int *cp)
{
  int c = s_in.get ();
  switch (c)
    {
    case 'B':
    case 'J':
      g = ccs_usascii;
      return 1;

    case 'I':
      g = ccs_jisx0201_kana;
      return 1;

    default:
      put (cp[0]);
      put (cp[1]);
      cp[0] = c;
      return 0;
    }
}

int
iso2022_to_internal_stream::designate96 (u_char &g, int *cp)
{
  int c = s_in.get ();
  switch (c)
    {
    case 'A':
      g = ccs_iso8859_1;
      return 1;

    case 'B':
      g = ccs_iso8859_2;
      return 1;

    case 'C':
      g = ccs_iso8859_3;
      return 1;

    case 'D':
      g = ccs_iso8859_4;
      return 1;

    case 'F':
      g = ccs_iso8859_7;
      return 1;

    case 'L':
      g = ccs_iso8859_5;
      return 1;

    case 'M':
      g = ccs_iso8859_9;
      return 1;

    case 'V':
      g = ccs_iso8859_10;
      return 1;

    case 'Y':
      g = ccs_iso8859_13;
      return 1;

    default:
      put (cp[0]);
      put (cp[1]);
      cp[0] = c;
      return 0;
    }
}

int
iso2022_to_internal_stream::designate94n (u_char &g, int *cp)
{
  int c = s_in.get ();
  switch (c)
    {
    case '@':
    case 'B':
      g = ccs_jisx0208;
      return 1;

    case 'D':
      g = ccs_jisx0212;
      return 1;

    case 'A':
      g = ccs_gb2312;
      return 1;

    case 'C':
      g = ccs_ksc5601;
      return 1;

    case 'G':
      g = ccs_cns11643_1;
      return 1;

    case 'H':
      g = ccs_cns11643_2;
      return 1;

    case '0':
      g = ccs_big5_1;
      return 1;

    case '1':
      g = ccs_big5_2;
      return 1;

    default:
      put (cp[0]);
      put (cp[1]);
      put (cp[2]);
      cp[0] = c;
      return 0;
    }
}

void
iso2022_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c[4];
      while (1)
        {
          c[0] = s_in.get ();
        again:
          if (c[0] != CC_ESC)
            break;
          s_ss = 0;

          c[1] = s_in.get ();
          switch (c[1])
            {
            case '$':
              c[2] = s_in.get ();
              switch (c[2])
                {
                case '@':
                case 'B':
                  s_g[0] = ccs_jisx0208;
                  continue;

                case 'A':
                  s_g[0] = ccs_gb2312;
                  continue;

                case '(':
                  if (designate94n (s_g[0], c))
                    continue;
                  break;

                case ')':
                  if (designate94n (s_g[1], c))
                    continue;
                  break;

                case '*':
                  if (designate94n (s_g[2], c))
                    continue;
                  break;

                case '+':
                  if (designate94n (s_g[3], c))
                    continue;
                  break;

                default:
                  put (c[0]);
                  put (c[1]);
                  c[0] = c[2];
                  break;
                }
              break;

            case '(':
              if (designate94 (s_g[0], c))
                continue;
              break;

            case ')':
              if (designate94 (s_g[1], c))
                continue;
              break;

            case '*':
              if (designate94 (s_g[2], c))
                continue;
              break;

            case '+':
              if (designate94 (s_g[3], c))
                continue;
              break;

            case ',': // Mule compatible
              if (designate96 (s_g[0], c))
                continue;
              break;

            case '-':
              if (designate96 (s_g[1], c))
                continue;
              break;

            case '.':
              if (designate96 (s_g[2], c))
                continue;
              break;

            case '/':
              if (designate96 (s_g[3], c))
                continue;
              break;

            case 'N': // SS2
              if (s_g[2] == ccs_invalid)
                goto wrong;
              s_ss = &s_g[2];
              continue;

            case 'O': // SS3
              if (s_g[3] == ccs_invalid)
                goto wrong;
              s_ss = &s_g[3];
              continue;

            case 'n': // LS2:
              if (s_g[2] == ccs_invalid)
                goto wrong;
              s_gl = &s_g[2];
              continue;

            case 'o': // LS3:
              if (s_g[3] == ccs_invalid)
                goto wrong;
              s_gl = &s_g[3];
              continue;

            case '~': // LS1R
              if (s_g[1] == ccs_invalid)
                goto wrong;
              s_gr = &s_g[1];
              continue;

            case '}': // LS2R
              if (s_g[2] == ccs_invalid)
                goto wrong;
              s_gr = &s_g[2];
              continue;

            case '|': // LS3R
              if (s_g[3] == ccs_invalid)
                goto wrong;
              s_gr = &s_g[3];
              continue;

            wrong:
              put (c[0]);
              c[0] = c[1];
              goto normal_char;

            default:
              s_in.putback (c[1]);
              goto normal_char;
            }

          if (c[0] == eof)
            return;
          if (room () > 0)
            goto again;
          s_in.putback (c[0]);
          return;
        }

    normal_char:
      int cc = c[0];
      if (cc == eof)
        break;

      switch (cc)
        {
        case CC_SS2:
          if (s_g[2] == ccs_invalid)
            break;
          s_ss = &s_g[2];
          continue;

        case CC_SS3:
          if (s_g[3] == ccs_invalid)
            break;
          s_ss = &s_g[3];
          continue;

        case CC_SI:
          if (s_g[1] == ccs_invalid)
            break;
          s_gl = &s_g[0];
          continue;

        case CC_SO:
          if (s_g[1] == ccs_invalid)
            break;
          s_gl = &s_g[1];
          continue;
        }

      int ccs;
      if (cc < 128)
        ccs = s_ss ? *s_ss : *s_gl;
      else
        {
          ccs = s_ss ? *s_ss : *s_gr;
          if (ccs != ccs_invalid)
            cc &= 127;
          else
            ccs = ccs_usascii;
        }
      s_ss = 0;
      to_internal (ccs, cc, c[0]);
    }
}

void
big5_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        break;
      if (c1 >= 0xa1 && c1 <= 0xf8 && c1 != 0xc8)
        {
          int c2 = s_in.get ();
          if (c2 >= 0x40 && c2 <= 0x7e || c2 >= 0xa1 && c2 <= 0xfe)
            c1 = big5_to_int (c1, c2);
          else
            s_in.putback (c2);
        }
      put (c1);
    }
}

void
binary_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      put (c);
    }
}

utf_to_internal_stream::putw_t
utf_to_internal_stream::per_lang_putw (int lang)
{
  switch (lang)
    {
    default:
    case ENCODING_LANG_JP:
    case ENCODING_LANG_JP2:
      return &putw_jp;

    case ENCODING_LANG_KR:
    case ENCODING_LANG_CN_GB:
    case ENCODING_LANG_CN_BIG5:
      return &putw_gen;

    case ENCODING_LANG_CN:
      return &putw_cn;
    }
}

void
utf_to_internal_stream::putw_jp (ucs2_t wc)
{
  if (s_has_bom < 0)
    {
      s_has_bom = wc == UNICODE_BOM;
      if (s_has_bom)
        return;
    }

  if (!(s_flags & ENCODING_UTF_WINDOWS))
    {
      int n = wc % numberof (utf_shiftjis2internal_hash);
      if (utf_shiftjis2internal_hash[n].wc == wc)
        {
          put (utf_shiftjis2internal_hash[n].cc);
          return;
        }
    }

  Char cc;
  if (s_to_full_width
      && (cc = wc2cp932 (wc)) != Char (-1)
      && !ccs_1byte_94_charset_p (code_charset (cc)))
    put (cc);
  else
    {
      cc = w2i (wc);
      if (cc != Char (-1))
        put (cc);
      else
        {
          put (utf16_ucs2_to_undef_pair_high (wc));
          put (utf16_ucs2_to_undef_pair_low (wc));
        }
    }
}

void
utf_to_internal_stream::putw_gen (ucs2_t wc)
{
  if (s_has_bom < 0)
    {
      s_has_bom = wc == UNICODE_BOM;
      if (s_has_bom)
        return;
    }

  Char cc = w2i (wc);
  if (cc != Char (-1))
    {
      if (!ccs_1byte_94_charset_p (code_charset (cc)))
        {
          Char t = s_cjk_translate[wc];
          if (t != Char (-1))
            cc = t;
        }
      put (cc);
    }
  else
    {
      put (utf16_ucs2_to_undef_pair_high (wc));
      put (utf16_ucs2_to_undef_pair_low (wc));
    }
}

void
utf_to_internal_stream::putw_cn (ucs2_t wc)
{
  if (s_has_bom < 0)
    {
      s_has_bom = wc == UNICODE_BOM;
      if (s_has_bom)
        return;
    }

  Char cc = w2i (wc);
  if (cc != Char (-1))
    {
      if (!ccs_1byte_94_charset_p (code_charset (cc)))
        {
          Char t = wc2gb2312_table[wc];
          if (t != Char (-1) || (t = wc2big5_table[wc]) != Char (-1))
            cc = t;
        }
      put (cc);
    }
  else
    {
      put (utf16_ucs2_to_undef_pair_high (wc));
      put (utf16_ucs2_to_undef_pair_low (wc));
    }
}

inline void
utf_to_internal_stream::putl (ucs4_t lc)
{
  if (lc < 0x10000)
    putw (ucs2_t (lc));
  else
    {
      putw (utf16_ucs4_to_pair_high (lc));
      putw (utf16_ucs4_to_pair_low (lc));
    }
}

void
utf16_to_internal_stream::refill_internal_le ()
{
  while (room () > 0)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        break;
      int c2 = s_in.get ();
      if (c2 == eof)
        break;
      putw ((c2 << 8) | c1);
    }
}

void
utf16_to_internal_stream::refill_internal_be ()
{
  while (room () > 0)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        break;
      int c2 = s_in.get ();
      if (c2 == eof)
        break;
      putw ((c1 << 8) | c2);
    }
}

void
utf16unknown_to_internal_stream::refill_internal ()
{
  if (!s_byte_order)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        return;
      int c2 = s_in.get ();
      if (c2 == eof)
        return;
      ucs2_t wc = (c1 << 8) | c2;
      if (wc == UNICODE_BOM)
        s_byte_order = ENCODING_UTF_BE;
      else if (wc == UNICODE_REVBOM)
        s_byte_order = ENCODING_UTF_LE;
      else
        {
          putw (wc);
          if (xsymbol_value (Vdefault_utf16_byte_order) == Kbig_endian)
            s_byte_order = ENCODING_UTF_BE;
          else
            s_byte_order = ENCODING_UTF_LE;
        }
    }

  if (s_byte_order == ENCODING_UTF_BE)
    refill_internal_be ();
  else
    refill_internal_le ();
}

u_char utf8_chtab[] =
{
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
  3,3,3,3,3,3,3,3,2,2,2,2,1,1,0,0,
};

u_char utf8_chmask[] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f};

void
utf8_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      u_char nbits = utf8_chtab[c];
      c &= utf8_chmask[nbits];
      switch (nbits)
        {
        case 7:
          putw (c);
          break;

        case 0:
        case 6:
          /* invalid code */
          break;

        default:
          {
            ucs4_t code = c;
            do
              {
                c = s_in.get ();
                if (c == eof)
                  return;
                code = (code << 6) | (c & 0x3f);
              }
            while (++nbits < 6);
            putl (code);
            break;
          }
        }
    }
}

utf7_to_internal_stream::utf7_to_internal_stream (xinput_stream <u_char> &in,
                                                  int flags, int lang)
     : utf_to_internal_stream (in, flags | ENCODING_UTF_SIGNATURE, lang),
       s_direct_encoding (1), s_cc (eof),
       s_imap4p (flags & UTF7_IMAP4_MAILBOX_NAME),
       s_shift_char (s_imap4p ? '&' : '+')
{
}

int
utf7_to_internal_stream::unicode_shifted_encoding ()
{
  u_char buf[8];
  int nchars;
  for (nchars = 0; nchars < sizeof buf; nchars++)
    {
      s_cc = s_in.get ();
      if (s_cc == eof)
        break;
      int b = s_imap4p ? imap4_base64_decode (s_cc) : base64_decode (s_cc);
      if (b >= 64)
        break;
      buf[nchars] = b;
    }

  int t = 0;
  int n = nchars & ~3;
  int i;
  for (i = 0; i < n; i += 4)
    {
      buf[t++] = (buf[i] << 2) | (buf[i + 1] >> 4);
      buf[t++] = (buf[i + 1] << 4) | (buf[i + 2] >> 2);
      buf[t++] = (buf[i + 2] << 6) | buf[i + 3];
    }

  switch (nchars & 3)
    {
    case 2:
      buf[t++] = (buf[i] << 2) | (buf[i + 1] >> 4);
      break;

    case 3:
      buf[t++] = (buf[i] << 2) | (buf[i + 1] >> 4);
      buf[t++] = (buf[i + 1] << 4) | (buf[i + 2] >> 2);
      break;
    }

  t &= ~1;
  for (i = 0; i < t; i += 2)
    putw ((buf[i] << 8) + buf[i + 1]);
  s_nbytes += t;
  s_direct_encoding = nchars < sizeof buf;
  if (s_cc == '-')
    {
      if (!s_nbytes)
        putw (s_shift_char);
      s_cc = s_in.get ();
    }
  return !s_direct_encoding;
}

void
utf7_to_internal_stream::refill_internal ()
{
  if (!s_direct_encoding)
    goto unicode_shifted_encoding;
  if (s_cc == eof)
    s_cc = s_in.get ();

  while (room () > 0)
    {
      if (s_cc == eof)
        break;
      if (s_cc != s_shift_char)
        {
          putw (s_cc);
          s_cc = s_in.get ();
        }
      else
        {
          s_nbytes = 0;
          s_direct_encoding = 0;
        unicode_shifted_encoding:
          while (room () > 0 && unicode_shifted_encoding ())
            ;
        }
    }
}

void
utf5_to_internal_stream::refill_internal ()
{
  while (1)
    {
      int c = s_in.get ();
      if (c == eof)
        return;
    nextchar:
      ucs4_t code = digit_char (c) - 16;
      if (code >= 16)
        continue;
      while (1)
        {
          c = s_in.get ();
          if (c == eof)
            {
              putl (code);
              return;
            }
          int n = digit_char (c);
          if (n >= 16)
            {
              putl (code);
              if (room () <= 0)
                {
                  s_in.putback (c);
                  return;
                }
              goto nextchar;
            }
          code = (code << 4) | n;
        }
    }
}

void
iso8859_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      if (c >= 0xa0)
        c = s_charset | (c & 127);
      put (c);
    }
}

void
windows_codepage_to_internal_stream::refill_internal ()
{
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      if (c >= 0x80 && s_translate[c - 0x80] != Char (-1))
        c = s_translate[c - 0x80];
      put (c);
    }
}

void
xwrite_stream::puteol ()
{
  if (s_eol == eol_crlf)
    {
      put ('\r');
      put ('\n');
    }
  else if (s_eol == eol_lf)
    put ('\n');
  else
    put ('\r');
  s_nlines++;
}

int
internal_to_sjis_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;

      Char cc = c;
      if (cc >= 0x80)
        {
          if (code_charset_bit (cc) & ccsf_possible_cp932)
            {
              cc = wc2cp932 (i2w (cc));
              if (cc == Char (-1))
                cc = DEFCHAR;
            }
          else if (code_charset_bit (cc) & ccsf_not_cp932)
            cc = DEFCHAR;

          if (DBCP (cc))
            {
              put (u_char (cc >> 8));
              put (u_char (cc));
              continue;
            }
        }

      if (cc == '\n')
        puteol ();
      else
        put (u_char (cc));
    }
  return finish ();
}

int
internal_to_big5_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;

      Char cc = wc2big5_table[i2w (c)];
      if (cc == Char (-1))
        cc = DEFCHAR;
      if (cc >= 0x100)
        {
          int c1, c2;
          int_to_big5 (cc, c1, c2);
          put (c1);
          put (c2);
        }
      else if (cc == '\n')
        puteol ();
      else
        put (u_char (cc));
    }
  return finish ();
}

int
internal_to_binary_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;

      if (c >= 0x100)
        {
          put (u_char (c >> 8));
          put (u_char (c));
        }
      else if (c == '\n')
        puteol ();
      else
        put (u_char (c));
    }
  return finish ();
}

Char
convert_ibmext (Char cc)
{
  if (cc <= 0xeffc)
    return cc;

  if (cc < 0xfa40 || cc > 0xfc4b)
    return DEFCHAR;

  int c2 = cc & 0xff;
  if (c2 < 0x40 || c2 == 0x7f || c2 > 0xfc)
    return DEFCHAR;
  if (c2 >= 0x80)
    c2--;
  return ibmext2internal_table[((cc >> 8) - 0xfa) * 188 + c2 - 0x40];
}

Char
convert_ibmext2necext (Char cc)
{
  if (cc <= 0xeffc)
    return cc;

  if (cc < 0xfa40 || cc > 0xfc4b)
    return DEFCHAR;

  int c2 = cc & 0xff;
  if (c2 < 0x40 || c2 == 0x7f || c2 > 0xfc)
    return DEFCHAR;
  if (c2 >= 0x80)
    c2--;
  return ibmext2necext_table[((cc >> 8) - 0xfa) * 188 + c2 - 0x40];
}

Char
convert_osfjvc (Char cc)
{
  if (cc < 0xecfc)
    return cc;

  if (cc >= 0xed40 && cc <= 0xeefc)
    {
      cc = w2i (i2w (cc));
      if (cc == Char (-1))
        return DEFCHAR;
    }

  if (cc < 0xfa40 || cc > 0xfc4b)
    return cc;

  int c2 = cc & 0xff;
  if (c2 < 0x40 || c2 == 0x7f || c2 > 0xfc)
    return DEFCHAR;
  if (c2 >= 0x80)
    c2--;
  return ibmext2internal_table[((cc >> 8) - 0xfa) * 188 + c2 - 0x40];
}

int
vender_depend_code (int vender)
{
  if (vender == ENCODING_ISO_VENDER_NIL)
    vender = to_vender_code (xsymbol_value (Vvender_depend_code_mapping));
  if (vender != ENCODING_ISO_VENDER_NIL)
    return vender;
  return ENCODING_ISO_VENDER_NECEXT;
}

vender_code_mapper_fn
select_vender_code_mapper (int vender)
{
  switch (vender_depend_code (vender))
    {
    default:
    case ENCODING_ISO_VENDER_IBMEXT:
      return convert_ibmext;

    case ENCODING_ISO_VENDER_NECEXT:
      return convert_ibmext2necext;

    case ENCODING_ISO_VENDER_OSFJVC:
      return convert_osfjvc;
    }
}

const internal_to_iso2022_stream::ccs_data internal_to_iso2022_stream::s_ccs_data[32] =
{
  {'B', ctype94},            // ccs_usascii
  {'I', ctype94},            // ccs_jisx0201_kana
  {'A', ctype96},            // ccs_iso8859_1
  {'B', ctype96},            // ccs_iso8859_2
  {'C', ctype96},            // ccs_iso8859_3
  {'D', ctype96},            // ccs_iso8859_4
  {'L', ctype96},            // ccs_iso8859_5
  {'F', ctype96},            // ccs_iso8859_7
  {'M', ctype96},            // ccs_iso8859_9
  {'V', ctype96},            // ccs_iso8859_10
  {'Y', ctype96},            // ccs_iso8859_13
  {'B', ctype94n, 1},        // ccs_jisx0208
  {'D', ctype94n},           // ccs_jisx0212
  {'A', ctype94n, 1},        // ccs_gb2312
  {'C', ctype94n},           // ccs_ksc5601
  {'0', ctype94n},           // ccs_big5_1
  {'1', ctype94n},           // ccs_big5_2
  {0},                       // ccs_utf16_undef_char_high
  {0},                       // ccs_utf16_undef_char_low
  {0},                       // ccs_utf16_surrogate_high
  {0},                       // ccs_utf16_surrogate_low
  {'G', ctype94n},           // ccs_cns11643_1
  {'H', ctype94n},           // ccs_cns11643_2
};

const char internal_to_iso2022_stream::s_inter94[] = {'(', ')', '*', '+'};
const char internal_to_iso2022_stream::s_inter96[] = {',', '-', '.', '/'};

int
internal_to_iso2022_stream::select_designation (int ccs) const
{
  if (ccs == ccs_usascii)
    return 0;
  for (int i = 0; i < 4; i++)
    if (s_initial[i] == ccs)
      return i;
  for (int i = 0; i < 4; i++)
    if (s_designatable[i] != u_int (-1)
        && s_designatable[i] & (1 << ccs))
      return i;
  for (int i = 0; i < 4; i++)
    if (s_designatable[i] == u_int (-1))
      return i;
  if (s_flags & ENCODING_ISO_LOCKING_SHIFT)
    return 1;
  if (s_ccs_data[ccs].ctype == ctype96)
    return 2;
  return 0;
}

internal_to_iso2022_stream::internal_to_iso2022_stream (xinput_stream <Char> &in,
                                                        eol_code eol,
                                                        int flags,
                                                        const u_char *initial,
                                                        const u_int *designatable,
                                                        int cjk)
     : xwrite_stream (in, eol), s_flags (flags), s_initial (initial),
       s_gl (&s_g[0]), s_gr (flags & ENCODING_ISO_7BITS ? 0 : &s_g[1]),
       s_designatable (designatable), s_cjk_translate (cjk_translate_table (cjk)),
       s_lang_cn (cjk == ENCODING_LANG_CN),
       s_vender_code_mapper (select_vender_code_mapper
                             (flags & ENCODING_ISO_VENDER_MASK))
{
  memcpy (s_g, s_initial, 4);
  for (int i = ccs_usascii; i < ccs_max; i++)
    s_designation[i] = select_designation (i);

  if (s_flags & ENCODING_ISO_USE_CNS11643)
    init_big5cns_table ();
}

void
internal_to_iso2022_stream::designate (int n, u_char ccs)
{
  if (s_g[n] != ccs)
    {
      put ('\033');
      switch (s_ccs_data[ccs].ctype)
        {
        case ctype94:
          put (s_inter94[n]);
          break;

        case ctype96:
          put (s_inter96[n]);
          break;

        case ctype94n:
          put ('$');
          if (n || !(s_flags & ENCODING_ISO_SHORT_FORM) || !s_ccs_data[ccs].fshort)
            put (s_inter94[n]);
          break;
        }
      put (s_ccs_data[ccs].final);
      s_g[n] = ccs;
    }
}

int
internal_to_iso2022_stream::designate (u_char ccs)
{
  int n = s_designation[ccs];
  if (s_g[n] != ccs)
    {
      if (s_gl != &s_g[0] && s_flags & ENCODING_ISO_ASCII_CTRL)
        {
          put (CC_SI);
          s_gl = &s_g[0];
        }
      designate (n, ccs);
    }

  switch (n)
    {
    default:
      if (s_gl != &s_g[0])
        {
          put (CC_SI);
          s_gl = &s_g[0];
        }
      return 0;

    case 1:
      if (s_gr == &s_g[1])
        return 0x80;
      if (s_gl != &s_g[1])
        {
          put (CC_SO);
          s_gl = &s_g[1];
        }
      return 0;

    case 2:
      if (s_gr)
        {
          put (CC_SS2);
          return 0x80;
        }
      else
        {
          put (CC_ESC);
          put ('N');
          return 0;
        }

    case 3:
      if (s_gr)
        {
          put (CC_SS3);
          return 0x80;
        }
      else
        {
          put (CC_ESC);
          put ('O');
          return 0;
        }
    }
}

int
internal_to_iso2022_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        {
          designate (ccs_usascii);
          break;
        }

      Char cc = c;
      u_int ccsf = code_charset_bit (cc);
      if (ccsf & (ccsf_utf16_surrogate | ccsf_utf16_undef_char))
        cc = DEFCHAR;
      else
        {
          if (s_lang_cn)
            {
              if (!(ccsf & (ccsf_gb2312 | ccsf_big5)))
                {
                  wchar_t wc = i2w (cc);
                  Char t = wc2gb2312_table[wc];
                  if (t != Char (-1) || (t = wc2big5_table[wc]) != Char (-1))
                    cc = t;
                }
            }
          else
            {
              if (s_cjk_translate)
                {
                  Char t = s_cjk_translate[i2w (cc)];
                  if (t != Char (-1))
                    cc = t;
                }
            }
          cc = (*s_vender_code_mapper)(cc);
        }

      int ccs = code_charset (cc);
      switch (ccs)
        {
          int c1, c2, f;
        case ccs_usascii:
          if (cc == '\n')
            {
              if (s_flags & ENCODING_ISO_ASCII_EOL)
                {
                  if (s_gl != &s_g[0])
                    {
                      put (CC_SI);
                      s_gl = &s_g[0];
                    }
                  for (int i = 0; i < 4; i++)
                    if (s_g[i] != s_initial[i])
                      {
                        if (s_initial[i] != ccs_invalid)
                          designate (i, s_initial[i]);
                        else
                          s_g[i] = s_initial[i];
                      }
                }
              puteol ();
              break;
            }
          /* fall thru... */
        usascii:
          if (s_flags & ENCODING_ISO_ASCII_CTRL || (cc > ' ' && cc < CC_DEL))
            designate (ccs_usascii);
          put (u_char (cc));
          break;

        case ccs_jisx0201_kana:
          if (cc <= 0xa0 || cc == 0xff)
            goto usascii;
          f = designate (ccs_jisx0201_kana);
          put (u_char (cc & 127 | f));
          break;

        case ccs_iso8859_1:
        case ccs_iso8859_2:
        case ccs_iso8859_3:
        case ccs_iso8859_4:
        case ccs_iso8859_5:
        case ccs_iso8859_7:
        case ccs_iso8859_9:
        case ccs_iso8859_10:
        case ccs_iso8859_13:
          cc &= 127;
          if (cc < ' ')
            {
              cc |= 0x80;
              goto usascii;
            }
          f = designate (ccs);
          put (u_char (cc | f));
          break;

        case ccs_jisx0212:
          if (cc > CCS_JISX0212_MAX)
            goto badchar;
          int_to_jisx0212 (cc, c1, c2);
          goto put94n;

        case ccs_gb2312:
          if (cc > CCS_GB2312_MAX)
            goto badchar;
          int_to_gb2312 (cc, c1, c2);
          goto put94n;

        case ccs_ksc5601:
          if (cc > CCS_KSC5601_MAX)
            goto badchar;
          int_to_ksc5601 (cc, c1, c2);
          goto put94n;

        case ccs_big5:
          if (cc > CCS_BIG5_MAX)
            goto badchar;
          if (s_flags & ENCODING_ISO_USE_CNS11643)
            {
              cc = big5cns_table[cc - CCS_BIG5_MIN];
              if (cc != Char (-1))
                {
                  switch (cc & 0x8080)
                    {
                    default:
                      ccs = ccs_gb2312;
                      break;

                    case BIG5CNS_CNS11643_1:
                      ccs = ccs_cns11643_1;
                      break;

                    case BIG5CNS_CNS11643_2:
                      ccs = ccs_cns11643_2;
                      break;
                    }
                  c1 = (cc >> 8) & 127;
                  c2 = cc & 127;
                  goto put94n;
                }
            }
          int_to_big5 (cc, c1, c2);
          mule_b2g (ccs, c1, c2);
          goto put94n;

        default:
          c1 = cc >> 8;
          c2 = cc & 255;
          if (!SJISP (c1) || !SJIS2P (c2))
            goto badchar;
          s2j (c1, c2);
          if (c1 >= 95 + 32)
            {
              if (c1 < 105 + 32)
                c1 -= 10;
              else if (c1 < 115 + 32)
                {
                  c1 -= 20;
                  ccs = ccs_jisx0212;
                }
              else
                goto badchar;
            }
        put94n:
          f = designate (ccs);
          put (c1 | f);
          put (c2 | f);
          break;

        badchar:
          designate (ccs_usascii);
          put ('?');
          break;
        }
    }
  return finish ();
}

int
internal_to_utf_stream::getw () const
{
  int c = s_in.get ();
  if (c == eof)
    return eof;

  Char cc = Char (c);

  if (!(s_flags & ENCODING_UTF_WINDOWS) && cc != Char (-1))
    {
      int n = cc % numberof (utf_internal2shiftjis_hash);
      if (utf_internal2shiftjis_hash[n].cc == cc)
        return utf_internal2shiftjis_hash[n].wc;
    }

  ucs2_t wc = i2w (cc);
  if (wc != ucs2_t (-1))
    return wc;
  if (utf16_undef_char_high_p (ucs2_t (cc)))
    {
      int c2 = s_in.get ();
      if (c2 != eof)
        {
          if (utf16_undef_char_low_p (ucs2_t (c2)))
            return utf16_undef_pair_to_ucs2 (ucs2_t (cc), ucs2_t (c2));
          s_in.putback (c2);
        }
    }
  return DEFCHAR;
}

int
internal_to_utf16le_stream::refill ()
{
  begin ();

  if (s_bom)
    {
      s_bom = 0;
      if (!s_in.eofp ())
        {
          put (u_char (UNICODE_BOM));
          put (u_char (UNICODE_BOM >> 8));
        }
    }

  while (room () > 0)
    {
      int c = getw ();
      if (c == eof)
        break;

      ucs2_t wc = ucs2_t (c);
      if (wc == '\n')
        {
          if (s_eol == eol_crlf)
            {
              put ('\r');
              put (0);
              put ('\n');
              put (0);
            }
          else if (s_eol == eol_lf)
            {
              put ('\n');
              put (0);
            }
          else
            {
              put ('\r');
              put (0);
            }
          s_nlines++;
        }
      else
        {
          put (u_char (wc));
          put (u_char (wc >> 8));
        }
    }
  return finish ();
}

int
internal_to_utf16be_stream::refill ()
{
  begin ();

  if (s_bom)
    {
      s_bom = 0;
      if (!s_in.eofp ())
        {
          put (u_char (UNICODE_BOM >> 8));
          put (u_char (UNICODE_BOM));
        }
    }

  while (room () > 0)
    {
      int c = getw ();
      if (c == eof)
        break;

      ucs2_t wc = ucs2_t (c);
      if (wc == '\n')
        {
          if (s_eol == eol_crlf)
            {
              put (0);
              put ('\r');
              put (0);
              put ('\n');
            }
          else if (s_eol == eol_lf)
            {
              put (0);
              put ('\n');
            }
          else
            {
              put (0);
              put ('\r');
            }
          s_nlines++;
        }
      else
        {
          put (u_char (wc >> 8));
          put (u_char (wc));
        }
    }
  return finish ();
}

int
internal_to_utf8_stream::refill ()
{
  begin ();

  if (s_bom)
    {
      s_bom = 0;
      if (!s_in.eofp ())
        {
          put (0xef);
          put (0xbb);
          put (0xbf);
        }
    }

  while (room () > 0)
    {
      int c = getw ();
      if (c == eof)
        break;

      ucs2_t wc = ucs2_t (c);
      ucs4_t lc = wc;
      if (utf16_surrogate_high_p (wc))
        {
          c = s_in.get ();
          if (utf16_surrogate_low_p (ucs2_t (c)))
            lc = utf16_pair_to_ucs4 (wc, ucs2_t (c));
          else
            s_in.putback (c);
        }

      if (lc < 0x80)
        {
          if (lc == '\n')
            puteol ();
          else
            put (u_char (lc));
        }
      else if (lc < 0x800)
        {
          put (u_char (0xc0 | ((lc >> 6) & 0x1f)));
          put (u_char (0x80 | (lc & 0x3f)));
        }
      else if (lc < 0x10000)
        {
          put (u_char (0xe0 | ((lc >> 12) & 0xf)));
          put (u_char (0x80 | ((lc >> 6) & 0x3f)));
          put (u_char (0x80 | (lc & 0x3f)));
        }
      else /* lc < 0x200000(0x110000) */
        {
          put (u_char (0xf0 | ((lc >> 18) & 7)));
          put (u_char (0x80 | ((lc >> 12) & 0x3f)));
          put (u_char (0x80 | ((lc >> 6) & 0x3f)));
          put (u_char (0x80 | (lc & 0x3f)));
        }
    }
  return finish ();
}

static const char b64chars[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char imap4_b64chars[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,";

internal_to_utf7_stream::internal_to_utf7_stream (xinput_stream <Char> &in,
                                                  eol_code eol, int flags)
     : internal_to_utf_stream (in, eol, flags & ~ENCODING_UTF_SIGNATURE),
       s_nb (0),
       s_nshift (0),
       s_accept (flags),
       s_imap4p (flags & UTF7_IMAP4_MAILBOX_NAME),
       s_shift_char (s_imap4p ? '&' : '+'),
       s_b64 (s_imap4p ? imap4_b64chars : b64chars)
{
  /* In IMAP4 modified UTF-7, "&" is always represented by "&-". */
  if (s_imap4p)
    s_accept |= UTF7_IMAP4_SHIFT_CHAR;
}

void
internal_to_utf7_stream::encode_b64 ()
{
  int n = s_nb - s_nb % 3;
  const u_char *b, *const be = s_b + n;
  for (b = s_b; b < be; b += 3)
    {
      put (s_b64[(b[0] >> 2) & 63]);
      put (s_b64[((b[0] << 4) | (b[1] >> 4)) & 63]);
      put (s_b64[((b[1] << 2) | (b[2] >> 6)) & 63]);
      put (s_b64[b[2] & 63]);
    }

  if (n != s_nb)
    switch (s_nb % 3)
      {
      case 1:
        put (s_b64[(b[0] >> 2) & 63]);
        put (s_b64[(b[0] << 4) & 63]);
        break;

      case 2:
        put (s_b64[(b[0] >> 2) & 63]);
        put (s_b64[((b[0] << 4) | (b[1] >> 4)) & 63]);
        put (s_b64[(b[1] << 2) & 63]);
        break;
      }
}

int
internal_to_utf7_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = getw ();
      if (c == eof)
        {
          if (s_nshift)
            {
              if (s_nshift != 1 || s_nb != 2 || s_b[1] != s_shift_char)
                encode_b64 ();
              put ('-');
              s_nshift = 0;
            }
          break;
        }

      ucs2_t wc = ucs2_t (c);
      if (wc < 0x80 && utf7_set (wc) & s_accept)
        {
          if (s_nb)
            {
              if (s_nshift == 1 && s_nb == 2 && s_b[1] == s_shift_char)
                put ('-');
              else
                {
                  encode_b64 ();
                  if (s_imap4p || wc == '-' || utf7_set (wc) & UTF7_SET_B)
                    put ('-');
                }
              s_nb = 0;
              s_nshift = 0;
            }
          if (wc == '\n')
            puteol ();
          else
            put (u_char (wc));
          if (wc == s_shift_char)
            put ('-');
        }
      else
        {
          if (!s_nshift)
            put (u_char (s_shift_char));
          if (s_nb == sizeof s_b)
            {
              encode_b64 ();
              s_nb = 0;
            }
          s_b[s_nb++] = wc >> 8;
          s_b[s_nb++] = u_char (wc);
          s_nshift++;
        }
    }
  return finish ();
}

int
internal_to_utf5_stream::refill ()
{
  begin ();

  while (room () > 0)
    {
      int c = getw ();
      if (c == eof)
        break;

      ucs2_t wc = ucs2_t (c);
      ucs4_t lc = wc;
      if (utf16_surrogate_high_p (wc))
        {
          c = s_in.get ();
          if (utf16_surrogate_low_p (ucs2_t (c)))
            lc = utf16_pair_to_ucs4 (wc, ucs2_t (c));
          else
            s_in.putback (c);
        }

      if (!lc)
        put ('G');
      else if (lc < 0x10000)
        {
          for (int i = 0;; i++, lc <<= 4)
            if (lc & 0xf000)
              {
                put ((upcase_digit_char + 16)[lc >> 12]);
                for (; i < 3; i++, lc <<= 4)
                  put (upcase_digit_char[(lc >> 8) & 15]);
                break;
              }
        }
      else
        {
          for (int i = 0;; i++, lc <<= 4)
            if (lc & 0xf0000000)
              {
                put ((upcase_digit_char + 16)[lc >> 28]);
                for (; i < 7; i++, lc <<= 4)
                  put (upcase_digit_char[(lc >> 24) & 15]);
                break;
              }
        }
    }
  return finish ();
}

const wc2int_hash &
internal_to_iso8859_stream::charset_hash (int ccs)
{
  switch (ccs)
    {
    default:
      assert (0);
    case ccs_iso8859_1:
      return wc2int_iso8859_1_hash;
    case ccs_iso8859_2:
      return wc2int_iso8859_2_hash;
    case ccs_iso8859_3:
      return wc2int_iso8859_3_hash;
    case ccs_iso8859_4:
      return wc2int_iso8859_4_hash;
    case ccs_iso8859_5:
      return wc2int_iso8859_5_hash;
    case ccs_iso8859_7:
      return wc2int_iso8859_7_hash;
    case ccs_iso8859_9:
      return wc2int_iso8859_9_hash;
    case ccs_iso8859_10:
      return wc2int_iso8859_10_hash;
    case ccs_iso8859_13:
      return wc2int_iso8859_13_hash;
    }
}

int
internal_to_iso8859_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;

      Char cc = c;
      if (cc >= 0xa0)
        {
          if (code_charset (cc) == s_charset)
            cc = int_to_iso8859 (cc);
          else
            {
              cc = lookup_wc2int_hash (s_hash, i2w (cc));
              cc = cc != Char (-1) ? int_to_iso8859 (cc) : DEFCHAR;
            }
          if (cc >= 0x80 && cc < 0xa0)
            cc = DEFCHAR;
        }

      if (cc == '\n')
        puteol ();
      else
        put (u_char (cc));
    }
  return finish ();
}

int
internal_to_windows_codepage_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;

      Char cc = c;
      if (cc >= 128)
        {
          cc = lookup_wc2int_hash (s_hash, i2w (cc));
          if (cc == Char (-1))
            cc = DEFCHAR;
        }

      if (cc == '\n')
        puteol ();
      else
        put (u_char (cc));
    }
  return finish ();
}

int
xdecode_stream::decode (int nchars, const u_char *i)
{
  if (!nchars)
    return eof;

  begin ();
  for (; nchars >= 3; i += 4, nchars -= 3)
    {
      put ((i[0] << 2) | (i[1] >> 4));
      put ((i[1] << 4) | (i[2] >> 2));
      put ((i[2] << 6) | i[3]);
    }
  if (nchars > 0)
    {
      put ((i[0] << 2) | (i[1] >> 4));
      if (nchars > 1)
        put ((i[1] << 4) | (i[2] >> 2));
    }
  return finish ();
}

int
xdecode_b64_stream::refill ()
{
  u_char buf[XDECODE_STREAM_BUFSIZE / 3 * 4];
  int nchars;
  for (nchars = 0; nchars < sizeof buf;)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      c = base64_decode (c);
      if (c < 64)
        buf[nchars++] = c;
      else if (c == 64 && nchars)
        break;
    }
  return decode (nchars * 3 / 4, buf);
}

int
xdecode_uu_stream::refill ()
{
  int c;
  do
    {
      c = s_in.get ();
      if (c == eof)
        return eof;
    }
  while (c == '\r' || c == '\n');

  int nchars = uudecode (c);
  u_char buf[63 / 3 * 4];
  int i;
  for (i = 0; i < sizeof buf; i++)
    {
      c = s_in.get ();
      if (c == eof || c == '\n')
        break;
      buf[i] = uudecode (c);
    }

  if (i == sizeof buf)
    do
      c = s_in.get ();
    while (c != eof && c != '\n');

  return decode (nchars, buf);
}

int
xdecode_qp_stream::refill ()
{
  int c1, c2, c3;
  begin ();
  c1 = s_in.get ();
  while (room () > 0)
    {
      if (c1 == eof)
        break;
      if (c1 != '=')
        put (s_underscore_to_space && c1 == '_' ? ' ' : c1);
      else
        {
          c2 = s_in.get ();
          if (c2 == '\r')
            {
              c3 = s_in.get ();
              if (c3 != '\n')
                {
                  put (c1);
                  put (c2);
                  c1 = c3;
                  continue;
                }
            }
          else if (c2 == '\n')
            ;
          else
            {
              if (c2 == eof || digit_char (c2) >= 16)
                {
                  put (c1);
                  c1 = c2;
                  continue;
                }
              c3 = s_in.get ();
              if (c3 == eof || digit_char (c3) >= 16)
                {
                  put (c1);
                  put (c2);
                  c1 = c3;
                  continue;
                }
              put ((digit_char (c2) << 4) | digit_char (c3));
            }
        }
      c1 = s_in.get ();
    }
  if (c1 != eof)
    s_in.putback (c1);
  return finish ();
}

int
xencode_b64_stream::refill ()
{
  u_char *b = s_buf, *const be = b + s_width;
  while (b < be)
    {
      int c1 = s_in.get ();
      if (c1 == eof)
        {
          if (b == s_buf)
            return eof;
          break;
        }
      int c2 = s_in.get ();
      if (c2 == eof)
        {
          *b++ = b64chars[(c1 >> 2) & 63];
          *b++ = b64chars[(c1 << 4) & 63];
          *b++ = '=';
          *b++ = '=';
          break;
        }
      int c3 = s_in.get ();
      if (c3 == eof)
        {
          *b++ = b64chars[(c1 >> 2) & 63];
          *b++ = b64chars[((c1 << 4) | (c2 >> 4)) & 63];
          *b++ = b64chars[(c2 << 2) & 63];
          *b++ = '=';
          break;
        }
      *b++ = b64chars[(c1 >> 2) & 63];
      *b++ = b64chars[((c1 << 4) | (c2 >> 4)) & 63];
      *b++ = b64chars[((c2 << 2) | (c3 >> 6)) & 63];
      *b++ = b64chars[c3 & 63];
    }
  if (s_fold_p)
    *b++ = '\n';
  return setbuf (s_buf, b);
}

int
xencode_uu_stream::refill ()
{
  if (s_eofp)
    return eof;

  u_char buf[BUFSIZE];
  int nchars;
  for (nchars = 0; nchars < sizeof buf; nchars++)
    {
      int c = s_in.get ();
      if (c == eof)
        {
          s_eofp = !nchars;
          break;
        }
      buf[nchars] = c;
    }

  u_char *b = s_buf;
  *b++ = uuencode (nchars);
  for (int i = 0; i < nchars; i += 3)
    {
      *b++ = uuencode ((buf[i] >> 2) & 63);
      *b++ = uuencode (((buf[i] << 4) | (buf[i + 1] >> 4)) & 63);
      *b++ = uuencode (((buf[i + 1] << 2) | (buf[i + 2] >> 6)) & 63);
      *b++ = uuencode (buf[i + 2] & 63);
    }
  *b++ = '\n';
  return setbuf (s_buf, b);
}

inline u_char *
xencode_qp_stream::encode (u_char *b, int c)
{
  *b++ = '=';
  *b++ = upcase_digit_char[c >> 4];
  *b++ = upcase_digit_char[c & 15];
  return b;
}

int
xencode_qp_stream::refill ()
{
  int c, c2;
  u_char *b = s_buf, *const be = b + LINESIZE;
  while (b < be)
    {
      c = s_in.get ();
      switch (c)
        {
        case eof:
          return setbuf (s_buf, b);

        case '\n':
          *b++ = c;
          return setbuf (s_buf, b);

        case ' ':
          if (s_space_to_underscore)
            {
              *b++ = '_';
              break;
            }
          goto white_space;

        case '\t':
          if (s_space_to_underscore)
            {
              b = encode (b, c);
              break;
            }
          /* fall thru... */
        white_space:
          c2 = s_in.get ();
          if (c2 == '\n')
            {
              b = encode (b, c);
              *b++ = c2;
              return setbuf (s_buf, b);
            }
          s_in.putback (c2);
          *b++ = c;
          break;

        case '_':
        case '?':
          if (s_space_to_underscore)
            b = encode (b, c);
          else
            *b++ = c;
          break;

        case '=':
          b = encode (b, c);
          break;

        default:
          if (c > ' ' && c < 0x7f)
            *b++ = c;
          else
            b = encode (b, c);
          break;
        }
    }
  *b++ = '=';
  *b++ = '\n';
  return setbuf (s_buf, b);
}

int
xdecode_url_stream::refill ()
{
  int c1, c2, c3;
  begin ();
  c1 = s_in.get ();
  while (room () > 0)
    {
      if (c1 == eof)
        break;
      if (c1 != '%')
        put (c1);
      else
        {
          c2 = s_in.get ();
          if (c2 == eof || digit_char (c2) >= 16)
            {
              put (c1);
              c1 = c2;
              continue;
            }
          c3 = s_in.get ();
          if (c3 == eof || digit_char (c3) >= 16)
            {
              put (c1);
              put (c2);
              c1 = c3;
              continue;
            }
          put ((digit_char (c2) << 4) | digit_char (c3));
        }
      c1 = s_in.get ();
    }
  if (c1 != eof)
    s_in.putback (c1);
  return finish ();
}

int
xencode_url_stream::refill ()
{
  begin ();
  while (room () > 0)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      if (s_literal[c])
        put (c);
      else
        {
          put ('%');
          put (upcase_digit_char[c >> 4]);
          put (upcase_digit_char[c & 15]);
        }
    }
  return finish ();
}

int
xdecode_hqx_stream::hqx7::refill ()
{
  u_char buf[XDECODE_STREAM_BUFSIZE / 3 * 4];
  int nchars;
  for (nchars = 0; nchars < sizeof buf;)
    {
      int c = s_in.get ();
      if (c == eof)
        break;
      c = hqx_decode (c);
      if (c < 64)
        buf[nchars++] = c;
    }
  return decode (nchars * 3 / 4, buf);
}

int
xdecode_hqx_stream::get ()
{
  if (s_rep > 0)
    {
      s_rep--;
      return s_cc;
    }

  int c = s_in.get ();
  if (c == eof)
    return c;
  if (c == s_marker)
    {
      s_rep = s_in.get ();
      if (s_rep == eof)
        {
          corrupted ();
          return eof;
        }
      if (!s_rep)
        {
          s_cc = eof;
          return c;
        }
      if (s_cc == eof || s_rep < 2)
        {
          corrupted ();
          return eof;
        }
      s_rep -= 2;
      c = s_cc;
    }
  else
    s_cc = c;
  return c;
}

int
xdecode_hqx_stream::read (u_short &x)
{
  int c1 = get ();
  if (c1 == eof)
    return 0;
  int c2 = get ();
  if (c2 == eof)
    return 0;
  x = (c1 << 8) | c2;
  return 1;
}

int
xdecode_hqx_stream::read (u_long &x)
{
  u_short s1, s2;
  if (!read (s1) || !read (s2))
    return 0;
  x = (s1 << 16) | s2;
  return 1;
}

xdecode_hqx_stream::xdecode_hqx_stream (xinput_stream <u_char> &in)
     : xdecode_stream (static_cast <xinput_stream <u_char> &> (s_hqx7)), s_hqx7 (in), s_corrupted (0),
       s_rest_bytes (0), s_cc (eof), s_rep (0)
{
  *s_name = 0;
  int l = get ();
  if (l < 1 || l > 63)
    {
      corrupted ();
      return;
    }
  int i;
  for (i = 0; i < l; i++)
    {
      int c = get ();
      if (c == eof)
        {
          corrupted ();
          return;
        }
      s_name[i] = c;
    }
  s_name[i] = 0;

  if ((s_version = get ()) == eof
      || !read (s_type)
      || !read (s_creator)
      || !read (s_flags)
      || !read (s_data_len)
      || !read (s_res_len)
      || !read (s_crc1))
    {
      corrupted ();
      return;
    }

  s_rest_bytes = s_data_len;
}

int
xdecode_hqx_stream::refill ()
{
  if (!s_rest_bytes)
    return eof;
  begin ();
  int nbytes = min ((u_long)room (), s_rest_bytes);
  for (int i = 0; i < nbytes; i++)
    {
      int c = get ();
      if (c == eof)
        {
          corrupted ();
          return eof;
        }
      put (c);
    }
  s_rest_bytes -= nbytes;
  return finish ();
}

encoding_input_stream_helper::encoding_input_stream_helper (lisp encoding,
                                                            xinput_stream <u_char> &in,
                                                            int fast_p)
{
  assert (char_encoding_p (encoding));
  switch (xchar_encoding_type (encoding))
    {
    default:
      assert (0);
    case encoding_sjis:
      if (fast_p)
        s_stream = new (&s_xbuf) fast_sjis_to_internal_stream (in);
      else
        s_stream = new (&s_xbuf) sjis_to_internal_stream (in);
      break;

    case encoding_big5:
      s_stream = new (&s_xbuf) big5_to_internal_stream (in);
      break;

    case encoding_binary:
      s_stream = new (&s_xbuf) binary_to_internal_stream (in);
      break;

    case encoding_iso2022:
      s_stream = new (&s_xbuf)
        iso2022_to_internal_stream (in, xchar_encoding_iso_initial (encoding),
                                    xchar_encoding_iso_flags (encoding));
      break;

    case encoding_iso2022_noesc:
      s_stream = new (&s_xbuf)
        iso2022_noesc_to_internal_stream (in, xchar_encoding_iso_initial (encoding),
                                          xchar_encoding_iso_flags (encoding));
      break;

    case encoding_iso8859:
      s_stream = new (&s_xbuf)
        iso8859_to_internal_stream (in, xchar_encoding_iso8859_charset (encoding));
      break;

    case encoding_windows_codepage:
      switch (xchar_encoding_windows_codepage (encoding))
        {
        case CP_JAPANESE:
          if (fast_p)
            s_stream = new (&s_xbuf) fast_sjis_to_internal_stream (in);
          else
            s_stream = new (&s_xbuf) sjis_to_internal_stream (in);
          break;

        case CP_KOREAN:
          s_stream = new (&s_xbuf) euckr_to_internal_stream (in);
          break;

        case CP_CN_TRADITIONAL:
          s_stream = new (&s_xbuf) big5_to_internal_stream (in);
          break;

        case CP_CN_SIMPLIFIED:
          s_stream = new (&s_xbuf) eucgb_to_internal_stream (in);
          break;

        default:
          assert (0);
        case CP_LATIN1:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_latin1_to_internal);
          break;

        case CP_LATIN2:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_latin2_to_internal);
          break;

        case CP_CYRILLIC:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_cyrillic_to_internal);
          break;

        case CP_GREEK:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_greek_to_internal);
          break;

        case CP_TURKISH:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_turkish_to_internal);
          break;

        case CP_BALTIC:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, windows_baltic_to_internal);
          break;

        case CP_KOI8R:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, koi8r_to_internal);
          break;

        case CP_PSEUDO_KOI8U:
          s_stream = new (&s_xbuf)
            windows_codepage_to_internal_stream (in, koi8u_to_internal);
          break;
        }
      break;

    case encoding_utf5:
      s_stream = new (&s_xbuf)
        utf5_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                 xchar_encoding_utf_cjk (encoding));
      break;

    case encoding_utf7:
      s_stream = new (&s_xbuf)
        utf7_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                 xchar_encoding_utf_cjk (encoding));
      break;

    case encoding_utf8:
      s_stream = new (&s_xbuf)
        utf8_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                 xchar_encoding_utf_cjk (encoding));
      break;

    case encoding_utf16:
      if (xchar_encoding_utf_flags (encoding) & ENCODING_UTF_BE)
        s_stream = new (&s_xbuf)
          utf16be_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                      xchar_encoding_utf_cjk (encoding));
      else if (xchar_encoding_utf_flags (encoding) & ENCODING_UTF_LE)
        s_stream = new (&s_xbuf)
          utf16le_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                      xchar_encoding_utf_cjk (encoding));
      else
        s_stream = new (&s_xbuf)
          utf16unknown_to_internal_stream (in, xchar_encoding_utf_flags (encoding),
                                           xchar_encoding_utf_cjk (encoding),
                                           s_byte_order);
      break;
    }
}

encoding_output_stream_helper::encoding_output_stream_helper (lisp encoding, xinput_stream <Char> &in,
                                                              eol_code eol)
{
  assert (char_encoding_p (encoding));
  switch (xchar_encoding_type (encoding))
    {
    default:
      assert (0);
    case encoding_sjis:
      s_stream = new (&s_xbuf) internal_to_sjis_stream (in, eol);
      break;

    case encoding_big5:
      s_stream = new (&s_xbuf) internal_to_big5_stream (in, eol);
      break;

    case encoding_binary:
      s_stream = new (&s_xbuf) internal_to_binary_stream (in, eol);
      break;

    case encoding_iso2022:
    case encoding_iso2022_noesc:
      s_stream = new (&s_xbuf)
        internal_to_iso2022_stream (in, eol,
                                    xchar_encoding_iso_flags (encoding),
                                    xchar_encoding_iso_initial (encoding),
                                    xchar_encoding_iso_designatable (encoding),
                                    xchar_encoding_iso_cjk (encoding));
      break;

    case encoding_iso8859:
      s_stream = new (&s_xbuf)
        internal_to_iso8859_stream (in, eol, xchar_encoding_iso8859_charset (encoding));
      break;

    case encoding_windows_codepage:
      switch (xchar_encoding_windows_codepage (encoding))
        {
        case CP_JAPANESE:
          s_stream = new (&s_xbuf) internal_to_sjis_stream (in, eol);
          break;

        case CP_KOREAN:
          s_stream = new (&s_xbuf) internal_to_euckr_stream (in, eol);
          break;

        case CP_CN_TRADITIONAL:
          s_stream = new (&s_xbuf) internal_to_big5_stream (in, eol);
          break;

        case CP_CN_SIMPLIFIED:
          s_stream = new (&s_xbuf) internal_to_eucgb_stream (in, eol);
          break;

        default:
          assert (0);
        case CP_LATIN1:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_latin1_hash);
          break;

        case CP_LATIN2:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_latin2_hash);
          break;

        case CP_CYRILLIC:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_cyrillic_hash);
          break;

        case CP_GREEK:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_greek_hash);
          break;

        case CP_TURKISH:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_turkish_hash);
          break;

        case CP_BALTIC:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_windows_baltic_hash);
          break;

        case CP_KOI8R:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_koi8r_hash);
          break;

        case CP_PSEUDO_KOI8U:
          s_stream = new (&s_xbuf)
            internal_to_windows_codepage_stream (in, eol, wc2int_koi8u_hash);
          break;
        }
      break;

    case encoding_utf5:
      s_stream = new (&s_xbuf)
        internal_to_utf5_stream (in, eol, xchar_encoding_utf_flags (encoding));
      break;

    case encoding_utf7:
      s_stream = new (&s_xbuf)
        internal_to_utf7_stream (in, eol, xchar_encoding_utf_flags (encoding));
      break;

    case encoding_utf8:
      s_stream = new (&s_xbuf)
        internal_to_utf8_stream (in, eol, xchar_encoding_utf_flags (encoding));
      break;

    case encoding_utf16:
      if (xchar_encoding_utf_flags (encoding) & ENCODING_UTF_LE)
        goto utf16le;
      if (xchar_encoding_utf_flags (encoding) & ENCODING_UTF_BE)
        goto utf16be;
      if (xsymbol_value (Vdefault_utf16_byte_order) == Kbig_endian)
        goto utf16be;
      goto utf16le;

    utf16be:
      s_stream = new (&s_xbuf)
        internal_to_utf16be_stream (in, eol, xchar_encoding_utf_flags (encoding));
      break;

    utf16le:
      s_stream = new (&s_xbuf)
        internal_to_utf16le_stream (in, eol, xchar_encoding_utf_flags (encoding));
      break;
    }
}
