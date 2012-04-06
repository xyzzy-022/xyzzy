#include "ed.h"
#include "StrBuf.h"
#include "safe_ptr.h"
#include "byte-stream.h"

int
check_kanji2 (const char *string, u_int off)
{
  const u_char *s0, *se, *s;
  for (s0 = (u_char *)string, se = s0 + off, s = se;
       s-- > s0 && SJISP (*s);)
    ;
  return !((se - s) & 1);
}

lisp
detect_char_encoding (const char *string, int size, int real_size)
{
  if (!string || size == 0)
    return Qnil;
  return Fdetect_char_encoding (make_string_simple (string, size));
}

lisp
Fdetect_char_encoding (lisp string)
{
#define score xcar
#define encoding xcdr
  // r == ((<score> . <encoding>) (<score> . <encoding>) ...)

  lisp r = Fguess_char_encoding (string);
  if (r == Qnil)
    return Qnil;

  if (xlist_length (r) == 1)
    return encoding (xcar (r));

  // 曖昧な場合は一番高いスコアのエンコーディングを返す。
  // 一番高いスコアが複数ある場合は nil を返す。
  lisp top = Qnil;
  for (; consp (r); r = xcdr (r))
    {
      lisp x = xcar (r);
      // sjis の半角カナだけだと big5 のスコアが高くなるので
      // 曖昧な場合は big5 は無視して判定する。
      if (xchar_encoding_type (encoding (x)) == encoding_big5)
          continue;

      if (top == Qnil || number_compare (score (xcar (top)), score (x)) < 0)
        top = xcons (x, Qnil);
      else if (number_compare (score (xcar (top)), score (x)) == 0)
        top = xcons (x, top);
    }

  size_t len = xlist_length (top);
  if (len == 1)
    return encoding (xcar (top));

  if (len == 2)
    {
      // sjis を utf-8 と誤認することはあまりないが、
      // utf-8 を sjis と誤認することが多いので、
      // sjis と utf-8 が同スコアの場合は utf-8 を優先する。
      lisp a = xcar (top);
      lisp b = Fcadr (top);
      if (xchar_encoding_type (encoding (a)) == encoding_sjis &&
          xchar_encoding_type (encoding (b)) == encoding_utf8)
        return encoding (b);
      if (xchar_encoding_type (encoding (b)) == encoding_sjis &&
          xchar_encoding_type (encoding (a)) == encoding_utf8)
        return encoding (a);
    }

  return Qnil;
#undef score
#undef encoding
}

lisp
Fconvert_encoding_to_internal (lisp encoding, lisp input, lisp output)
{
  check_char_encoding (encoding);
  if (xchar_encoding_type (encoding) == encoding_auto_detect)
    FEtype_error (encoding, Qchar_encoding);
  xstream_ibyte_helper is (input);
  xstream_oChar_helper os (output);
  encoding_input_stream_helper s (encoding, is);
  copy_xstream (s, os);
  return os.result ();
}

lisp
Fconvert_encoding_from_internal (lisp encoding, lisp input, lisp output)
{
  check_char_encoding (encoding);
  if (xchar_encoding_type (encoding) == encoding_auto_detect)
    FEtype_error (encoding, Qchar_encoding);
  xstream_iChar_helper is (input);
  xstream_obyte_helper os (output);
  encoding_output_stream_helper s (encoding, is, eol_noconv);
  copy_xstream (s, os);
  return os.result ();
}

#include "kanjitab.h"

struct to_half
{
  int min, max;
  const u_char *b;
};

static const to_half toh[] =
{
  {TO_HALF_WIDTH81_MIN, TO_HALF_WIDTH81_MAX, to_half_width_81},
  {TO_HALF_WIDTH82_MIN, TO_HALF_WIDTH82_MAX, to_half_width_82},
  {TO_HALF_WIDTH83_MIN, TO_HALF_WIDTH83_MAX, to_half_width_83},
};

static const to_half ssh[] =
{
  {VOICED_SOUND82_MIN, VOICED_SOUND82_MAX, voiced_sound_82},
  {VOICED_SOUND83_MIN, VOICED_SOUND83_MAX, voiced_sound_83},
};

#define CP932_GREEK_P(C) ((C) >= 0x839f && (C) <= 0x83d6)
#define CP932_CYRILLIC_P(C) ((C) >= 0x8440 && (C) <= 0x8491)

#define INTERNAL_GREEK_P(C) ((C) >= 0x3c1 && (C) <= 0x3f9)
#define INTERNAL_CYRILLIC_P(C) ((C) >= 0x321 && (C) <= 0x371 && (C) != 0x370)

#define ASC 1
#define HIRA 2
#define KATA 4
#define GREEK 8
#define CYRILLIC 16

static int
map_flags (lisp keys)
{
  int flags = 0;
  if (find_keyword_bool (Kascii, keys, 0))
    flags |= ASC;
  if (find_keyword_bool (Khiragana, keys, 0))
    flags |= HIRA;
  if (find_keyword_bool (Kkatakana, keys, 0))
    flags |= KATA;
  if (find_keyword_bool (Kgreek, keys, 0))
    flags |= GREEK;
  if (find_keyword_bool (Kcyrillic, keys, 0))
    flags |= CYRILLIC;
  return flags;
}

struct to_half_param
{
  int flags;
  Char fmin, fmax;
  Char hmin, hmax;
  to_half_param (lisp);
};

to_half_param::to_half_param (lisp keys)
{
  flags = map_flags (keys);
  if (!flags)
    return;

  switch (flags & (HIRA | KATA))
    {
    case HIRA:
      fmin = FULL_WIDTH_KATAKANA_MIN;
      fmax = FULL_WIDTH_KATAKANA_MAX;
      break;

    case KATA:
      fmin = FULL_WIDTH_HIRAGANA_MIN;
      fmax = FULL_WIDTH_HIRAGANA_MAX;
      break;

    case HIRA | KATA:
      fmin = 0xffff;
      fmax = 0;
      break;

    default:
      fmin = min (FULL_WIDTH_KATAKANA_MIN, FULL_WIDTH_HIRAGANA_MIN);
      fmax = max (FULL_WIDTH_KATAKANA_MAX, FULL_WIDTH_HIRAGANA_MAX);
      break;
    }

  if (flags & ASC)
    {
      hmin = 1;
      hmax = (flags & (HIRA | KATA)) ? 0xff : 0x7f;
    }
  else if (flags & (HIRA | KATA))
    {
      hmin = 0x80;
      hmax = 0xff;
    }
  else
    {
      hmin = 1;
      hmax = 0;
    }
}

lisp
Fmap_to_half_width_region (lisp from, lisp to, lisp keys)
{
  to_half_param thp (keys);
  if (!thp.flags)
    return Qnil;

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p2 = bp->prepare_modify_region (wp, from, to);
  if (p2 == -1)
    return Qt;
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;

  Point &point = wp->w_point;
  point_t p1 = point.p_point;
  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (c < thp.fmin || c > thp.fmax)
        for (int i = 0; i < numberof (toh); i++)
          if (c >= toh[i].min && c <= toh[i].max)
            {
              c = toh[i].b[c - toh[i].min];
              if (c >= thp.hmin && c <= thp.hmax)
                point.ch () = c;
              goto next;
            }
      if ((thp.flags & GREEK && CP932_GREEK_P (c))
          || (thp.flags & CYRILLIC && CP932_CYRILLIC_P (c)))
        {
          c = w2i (i2w (c));
          if (c != Char (-1))
            point.ch () = c;
        }
    next:
      if (!bp->forward_char (point, 1))
        break;
    }

  if (thp.flags & (HIRA | KATA))
    {
      int nconv = 0;
      while (bp->forward_char (point, -1) && point.p_point >= p1)
        {
          Char c = point.ch ();
          if (c < thp.fmin || c > thp.fmax)
            for (int i = 0; i < numberof (ssh); i++)
              if (c >= ssh[i].min && c <= ssh[i].max)
                {
                  c = ssh[i].b[c - ssh[i].min];
                  if (c)
                    {
                      //                                 ゛             ゜
                      point.ch () = c & 0x80 ? u_char (0xde) : u_char (0xdf);
                      c |= 0x80;
                      if (!bp->insert_chars_internal (point, &c, 1, 1))
                        {
                          bp->post_buffer_modified (Kmodify, point, p1, p2 + nconv);
                          FEstorage_error ();
                        }
                      bp->forward_char (point, -1);
                      nconv++;
                    }
                  break;
                }
        }
      bp->goto_char (point, p2 + nconv);
    }

  bp->post_buffer_modified (Kmodify, point, p1, point.p_point);
  return Qt;
}

static int
to_full_flags (lisp keys)
{
  int flags = map_flags (keys);
  if ((flags & (HIRA | KATA)) == (HIRA | KATA))
    FEprogram_error (Ehiragana_and_katakana_are_both_specified);
  return flags;
}

lisp
Fmap_to_full_width_region (lisp from, lisp to, lisp keys)
{
  int flags = to_full_flags (keys);
  if (!flags)
    return Qnil;

  const Char *tof = flags & HIRA ? to_fullhira_a1_df : to_fullkata_a1_df;

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p2 = bp->prepare_modify_region (wp, from, to);
  if (p2 == -1)
    return Qt;
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;

  Point &point = wp->w_point;
  point_t p1 = point.p_point;
  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (flags & (HIRA | KATA) && c >= 0xa1 && c <= 0xdf)
        point.ch () = tof[c - 0xa1];
      else if (flags & ASC && c >= 0x20 && c <= 0x7e)
        point.ch () = to_full_20_7e[c - 0x20];
      else if ((flags & GREEK && INTERNAL_GREEK_P (c))
               || (flags & CYRILLIC && INTERNAL_CYRILLIC_P (c)))
        {
          c = wc2cp932 (i2w (c));
          if (c != Char (-1))
            point.ch () = c;
        }
      if (!bp->forward_char (point, 1))
        break;
    }

  if (flags & (HIRA | KATA))
    {
      int nconv = 0;
      while (bp->forward_char (point, -1) && point.p_point >= p1)
        {
          Char c = point.ch (), c2;
          if (c != VOICED_SOUND_MARK && c != SEMI_VOICED_SOUND_MARK)
            continue;
          while (1)
            {
              if (!bp->forward_char (point, -1) || point.p_point < p1)
                goto done;
              c2 = point.ch ();
              if (c2 != VOICED_SOUND_MARK && c2 != SEMI_VOICED_SOUND_MARK)
                break;
              c = c2;
            }
          if (flags & HIRA)
            {
              if (c2 < TO_HALF_WIDTH82_MIN || c2 > TO_HALF_WIDTH82_MAX)
                continue;
              c2 = to_half_width_82[c2 - TO_HALF_WIDTH82_MIN];
              if (!c2)
                continue;
              if (c == VOICED_SOUND_MARK)
                {
                  if (c2 < 0xb6 || c2 > 0xce)
                    continue;
                  c2 = to_fullhira_voiced_b6_ce[c2 - 0xb6];
                  if (!c2)
                    continue;
                }
              else
                {
                  if (c2 < 0xca || c2 > 0xce)
                    continue;
                  c2 = to_fullhira_semi_voiced_ca_ce[c2 - 0xca];
                }
            }
          else
            {
              if (c2 < TO_HALF_WIDTH83_MIN || c2 > TO_HALF_WIDTH83_MAX)
                continue;
              c2 = to_half_width_83[c2 - TO_HALF_WIDTH83_MIN];
              if (!c2)
                continue;
              if (c == VOICED_SOUND_MARK)
                {
                  if (c2 < 0xb3 || c2 > 0xce)
                    continue;
                  c2 = to_fullkata_voiced_b3_ce[c2 - 0xb3];
                  if (!c2)
                    continue;
                }
              else
                {
                  if (c2 < 0xca || c2 > 0xce)
                    continue;
                  c2 = to_fullkata_semi_voiced_ca_ce[c2 - 0xca];
                }
            }
          if (!bp->delete_region_internal (point, point.p_point,
                                           point.p_point + 1))
            {
              bp->post_buffer_modified (Kmodify, point, p1, p2 + nconv);
              FEstorage_error ();
            }
          point.ch () = c2;
          nconv++;
        }
    done:
      bp->goto_char (point, p2 - nconv);
    }

  bp->post_buffer_modified (Kmodify, point, p1, point.p_point);
  return Qt;
}

lisp
Fmap_to_half_width_string (lisp string, lisp keys)
{
  check_string (string);

  to_half_param thp (keys);
  if (!thp.flags)
    return string;

  safe_ptr <Char> s0 (new Char [xstring_length (string) * 2]);
  bcopy (xstring_contents (string), s0, xstring_length (string));
  Char *s, *se;
  for (s = s0, se = s + xstring_length (string); s < se; s++)
    {
      Char c = *s;
      if (c < thp.fmin || c > thp.fmax)
        for (int i = 0; i < numberof (toh); i++)
          {
            if (c >= toh[i].min && c <= toh[i].max)
              {
                c = toh[i].b[c - toh[i].min];
                if (c >= thp.hmin && c <= thp.hmax)
                  *s = c;
                goto next;
              }
          }
      if ((thp.flags & GREEK && CP932_GREEK_P (c))
          || (thp.flags & CYRILLIC && CP932_CYRILLIC_P (c)))
        {
          c = w2i (i2w (c));
          if (c != Char (-1))
            *s = c;
        }
    next:;
    }

  if (!(thp.flags & (HIRA | KATA)))
    return make_string (s0, xstring_length (string));

  Char *de = s0 + xstring_length (string) * 2;
  Char *d = de;
  for (s = s0; se > s;)
    {
      Char c = *--se;
      *--d = c;
      if (c < thp.fmin || c > thp.fmax)
        for (int i = 0; i < numberof (ssh); i++)
          if (c >= ssh[i].min && c <= ssh[i].max)
            {
              c = ssh[i].b[c - ssh[i].min];
              if (c)
                {
                  *d = c & 0x80 ? u_char (0xde) : u_char (0xdf);
                  *--d = c | 0x80;
                }
              break;
            }
    }
  return make_string (d, de - d);
}

lisp
Fmap_to_full_width_string (lisp string, lisp keys)
{
  check_string (string);
  int flags = to_full_flags (keys);
  if (!flags)
    return string;

  const Char *tof = flags & HIRA ? to_fullhira_a1_df : to_fullkata_a1_df;

  safe_ptr <Char> s0 (new Char [xstring_length (string)]);
  bcopy (xstring_contents (string), s0, xstring_length (string));

  Char *s, *se;
  for (s = s0, se = s + xstring_length (string); s < se; s++)
    {
      Char c = *s;
      if (flags & (HIRA | KATA) && c >= 0xa1 && c <= 0xdf)
        *s = tof[c - 0xa1];
      else if (flags & ASC && c >= 0x20 && c <= 0x7e)
        *s = to_full_20_7e[c - 0x20];
      else if ((flags & GREEK && INTERNAL_GREEK_P (c))
               || (flags & CYRILLIC && INTERNAL_CYRILLIC_P (c)))
        {
          c = wc2cp932 (i2w (c));
          if (c != Char (-1))
            *s = c;
        }
    }

  if (!(flags & (HIRA | KATA)))
    return make_string (s0, xstring_length (string));

#define PAD 0xa1
  int npad = 0;
  for (s = s0; se > s;)
    {
      Char c = *--se, c2;
      if (c != VOICED_SOUND_MARK && c != SEMI_VOICED_SOUND_MARK)
        continue;
      while (1)
        {
          if (se == s)
            goto done;
          c2 = *--se;
          if (c2 != VOICED_SOUND_MARK && c2 != SEMI_VOICED_SOUND_MARK)
            break;
          c = c2;
        }
      if (flags & HIRA)
        {
          if (c2 < TO_HALF_WIDTH82_MIN || c2 > TO_HALF_WIDTH82_MAX)
            continue;
          c2 = to_half_width_82[c2 - TO_HALF_WIDTH82_MIN];
          if (!c2)
            continue;
          if (c == VOICED_SOUND_MARK)
            {
              if (c2 < 0xb6 || c2 > 0xce)
                continue;
              c2 = to_fullhira_voiced_b6_ce[c2 - 0xb6];
              if (!c2)
                continue;
            }
          else
            {
              if (c2 < 0xca || c2 > 0xce)
                continue;
              c2 = to_fullhira_semi_voiced_ca_ce[c2 - 0xca];
            }
        }
      else
        {
          if (c2 < TO_HALF_WIDTH83_MIN || c2 > TO_HALF_WIDTH83_MAX)
            continue;
          c2 = to_half_width_83[c2 - TO_HALF_WIDTH83_MIN];
          if (!c2)
            continue;
          if (c == VOICED_SOUND_MARK)
            {
              if (c2 < 0xb3 || c2 > 0xce)
                continue;
              c2 = to_fullkata_voiced_b3_ce[c2 - 0xb3];
              if (!c2)
                continue;
            }
          else
            {
              if (c2 < 0xca || c2 > 0xce)
                continue;
              c2 = to_fullkata_semi_voiced_ca_ce[c2 - 0xca];
            }
        }
      se[1] = PAD;
      *se = c2;
      npad++;
    }
done:

  if (!npad)
    return make_string (s0, xstring_length (string));

  Char *p, *q, *pe;
  for (p = s0, q = p, pe = p + xstring_length (string); p < pe;)
    {
      Char c = *p++;
      if (c != PAD)
        *q++ = c;
    }
  return make_string (s0, q - s0);
}
