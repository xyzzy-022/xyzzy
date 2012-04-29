#include "stdafx.h"
#include "ed.h"
#include "regex.h"
#include "StrBuf.h"

#define SF_CASE_FOLD 1
#define SF_NO_DUP 2
#define SF_REVERSE 4
#define SF_TAIL 8
#define SF_LWORD 16
#define SF_RWORD 32
#define SF_LSYMBOL 64
#define SF_RSYMBOL 128
#define SF_SMART_CASE_FOLD 256

static Regexp::sregs re_regs = {-1, {-1, -1}};
extern u_char char_no_translate_table[];
extern u_char char_translate_upcase_table[];

static inline void
clear_regs ()
{
  re_regs.nregs = -1;
}

static void
copy_regs0 (Regexp::sregs &d, const Regexp::sregs &s)
{
  d.nregs = s.nregs;
  for (int i = 0; i <= d.nregs; i++)
    {
      d.start[i] = s.start[i];
      d.end[i] = s.end[i];
    }
}

static void
check_regs ()
{
  int i;
  for (i = 0; i <= re_regs.nregs; i++)
    {
      if (re_regs.start[i] < 0)
        re_regs.end[i] = -1;
      else if (re_regs.end[i] < 0)
        re_regs.start[i] = -1;
      else
        {
          if (re_regs.end[i] < re_regs.start[i])
            re_regs.end[i] = re_regs.start[i];
          if (i)
            {
              if (re_regs.start[i] < re_regs.start[0])
                re_regs.start[i] = re_regs.start[0];
              if (re_regs.end[i] > re_regs.end[0])
                re_regs.end[i] = re_regs.end[0];
            }
        }
    }

  for (; i < MAX_REGS; i++)
    {
      re_regs.start[i] = -1;
      re_regs.end[i] = -1;
    }
}

static inline void
save_search (point_t start, point_t end)
{
  re_regs.nregs = 0;
  re_regs.start[0] = start;
  re_regs.end[0] = end;
  xsymbol_value (Vlast_match_string) = Qnil;
}

static inline void
save_match (const Regexp &re, lisp str)
{
  copy_regs0 (re_regs, re.re_regs);
  check_regs ();
  xsymbol_value (Vlast_match_string) = str;
}

static void
bm_compilef (int *BM, const Char *pattern, int patlen, int case_fold)
{
  for (int i = 0; i < 256; i++)
    BM[i] = patlen;

  for (int i = patlen - 1; i >= 0; i--)
    {
      Char cc = *pattern++;
      int c = DBCP (cc) ? cc >> 8 : cc;
      if (case_fold && alpha_char_p (cc))
        BM[_char_transpose_case (c)] = i;
      BM[c] = i;
    }
}

static void
bm_compileb (int *BM, const Char *pattern, int patlen, int case_fold)
{
  int i;
  for (i = 0; i < 256; i++)
    BM[i] = patlen;

  for (i = patlen - 1, pattern += patlen; i >= 0; i--)
    {
      Char cc = *--pattern;
      int c = DBCP (cc) ? cc >> 8 : cc;
      if (case_fold && alpha_char_p (cc))
        BM[_char_transpose_case (c)] = i;
      BM[c] = i;
    }
}

int
Buffer::word_bound (const Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  if (bobp (point) || eobp (point))
    return 1;
  Char c = point.ch ();
  if (!ascii_char_p (c) || xchar_syntax (tab, c) != SCword)
    return 1;
  c = point.prevch ();
  if (!ascii_char_p (c) || xchar_syntax (tab, c) != SCword)
    return 1;
  return 0;
}

int
Buffer::symbol_bound (const Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  if (bobp (point) || eobp (point))
    return 1;
  Char c = point.ch ();
  if (!ascii_char_p (c)
      || (xchar_syntax (tab, c) != SCword
          && xchar_syntax (tab, c) != SCsymbol))
    return 1;
  c = point.prevch ();
  if (!ascii_char_p (c)
      || (xchar_syntax (tab, c) != SCword
          && xchar_syntax (tab, c) != SCsymbol))
    return 1;
  return 0;
}

int
Buffer::bm_execf (Point &point, const Char *pattern, int patlen, const int *BM,
                  point_t limit, int flags) const
{
  const Char *pate = pattern + patlen - 1;
  Char c0 = (flags & SF_CASE_FOLD) ? char_upcase (*pate) : *pate;

  Chunk *cp = point.p_chunk;
  const Char *pe = cp->c_text + cp->c_used;
  const Char *p = cp->c_text + point.p_offset;

  Char c = *p;
  int delta = BM[DBCP (c) ? c >> 8 : c];

  while (1)
    {
      while (delta)
        {
          point.p_point += delta;
          if (point.p_point >= limit)
            return 0;
          while (pe - p <= delta)
            {
              delta -= pe - p;
              cp = cp->c_next;
              p = cp->c_text;
              pe = p + cp->c_used;
            }
          p += delta;
          c = *p;
          delta = BM[DBCP (c) ? c >> 8 : c];
        }

      if (((flags & SF_CASE_FOLD) ? char_upcase (c) : c) != c0)
        {
          delta = 1;
          continue;
        }

      point_t opoint (point.p_point);
      for (const Char *pat = pate; pat > pattern;)
        {
          point.p_point--;
          if (p == cp->c_text)
            {
              cp = cp->c_prev;
              p = pe = cp->c_text + cp->c_used;
            }
          c = *--p;
          Char c2 = *--pat;
          if ((flags & SF_CASE_FOLD)
              ? char_upcase (c) != char_upcase (c2)
              : c != c2)
            goto fail;
        }

      point.p_chunk = cp;
      point.p_offset = p - cp->c_text;

      if ((flags & SF_LWORD && !word_bound (point))
          || (flags & SF_LSYMBOL && !symbol_bound (point)))
        goto fail2;
      if (flags & (SF_RWORD | SF_RSYMBOL))
        {
          Point tem (point);
          goto_char (tem, opoint + 1);
          if (flags & SF_RWORD ? !word_bound (tem) : !symbol_bound (tem))
            goto fail2;
        }

      return 1;

    fail2:
      delta = opoint - point.p_point + 1;
      continue;

    fail:
      delta = max (BM[DBCP (c) ? c >> 8 : c], int (opoint - point.p_point + 1));
    }
}

int
Buffer::bm_execb (Point &point, const Char *pattern, int patlen, const int *BM,
                  point_t limit, int flags) const
{
  const Char *pate = pattern + patlen;
  Char c0 = (flags & SF_CASE_FOLD) ? char_upcase (*pattern) : *pattern;
  pattern++;

  Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  Char c = *p;

  int delta = BM[DBCP (c) ? c >> 8 : c];

  while (1)
    {
      while (delta)
        {
          point.p_point -= delta;
          if (point.p_point < limit)
            return 0;
          while (p - cp->c_text < delta)
            {
              delta -= p - cp->c_text;
              cp = cp->c_prev;
              p = pe = cp->c_text + cp->c_used;
            }
          p -= delta;
          c = *p;
          delta = BM[DBCP (c) ? c >> 8 : c];
        }

      if (((flags & SF_CASE_FOLD) ? char_upcase (c) : c) != c0)
        {
          delta = 1;
          continue;
        }

      point_t opoint (point.p_point);
      for (const Char *pat = pattern; pat < pate; pat++)
        {
          point.p_point++;
          if (++p == pe)
            {
              cp = cp->c_next;
              p = cp->c_text;
              pe = p + cp->c_used;
            }
          c = *p;
          Char c2 = *pat;
          if ((flags & SF_CASE_FOLD)
              ? char_upcase (c) != char_upcase (c2)
              : c != c2)
            goto fail;
        }

      point.p_chunk = cp;
      point.p_offset = p - cp->c_text;

      if (flags & (SF_RWORD | SF_RSYMBOL))
        {
          Point tem (point);
          forward_char (tem, 1);
          if (flags & SF_RWORD ? !word_bound (tem) : !symbol_bound (tem))
            goto fail2;
        }
      if (flags & (SF_LWORD | SF_LSYMBOL))
        {
          Point tem (point);
          goto_char (tem, opoint);
          if (flags & SF_LWORD ? !word_bound (tem) : !symbol_bound (tem))
            goto fail2;
        }

      return 1;

    fail2:
      delta = point.p_point - opoint + 1;
      continue;

    fail:
      delta = max (BM[DBCP (c) ? c >> 8 : c], int (point.p_point - opoint + 1));
    }
}

int
Buffer::scan_forward (Point &w_point, const Char *pattern, int patlen,
                      const int *BM, point_t limit, int flags) const
{
  int d = patlen - 1;
  if (flags & SF_NO_DUP)
    d++;
  if (w_point.p_point + d >= limit)
    return 0;

  Point point (w_point);
  forward_char (point, d);

  if (!bm_execf (point, pattern, patlen, BM, limit, flags))
    return 0;

  save_search (point.p_point, point.p_point + patlen);

  if (flags & SF_TAIL)
    forward_char (point, patlen);
  w_point = point;
  return 1;
}

int
Buffer::scan_backward (Point &w_point, const Char *pattern, int patlen,
                       const int *BM, point_t limit, int flags) const
{
  point_t t = w_point.p_point;
  if (flags & SF_NO_DUP)
    t--;
  t = min (t, point_t (b_contents.p2 - patlen));
  if (t < limit)
    return 0;

  Point point (w_point);
  goto_char (point, t);

  if (!bm_execb (point, pattern, patlen, BM, limit, flags))
    return 0;

  save_search (point.p_point - (patlen - 1), point.p_point + 1);

  if (flags & SF_TAIL)
    forward_char (point, 1);
  else
    forward_char (point, -(patlen - 1));
  w_point = point;
  return 1;
}

int
Buffer::re_scan_buffer (Point &w_point, lisp pattern, point_t limit,
                        point_t last_match, lChar last_match_char,
                        int flags) const
{
  Point point (w_point);
  if (flags & SF_REVERSE)
    {
      if (flags & SF_NO_DUP && !forward_char (point, -1))
        return 0;
      if (point.p_point < limit || point.p_point > b_contents.p2)
        return 0;
    }
  else
    {
      if (flags & SF_NO_DUP && !forward_char (point, 1))
        return 0;
      if (point.p_point < b_contents.p1 || point.p_point > limit)
        return 0;
    }

  if (flags & SF_SMART_CASE_FOLD
      && stringp (pattern)
      && Regexp::smart_case_fold_p (xstring_contents (pattern),
                                    xstring_length (pattern)))
    flags |= SF_CASE_FOLD;

  Regexp re ((flags & SF_CASE_FOLD
              ? char_translate_upcase_table
              : char_no_translate_table),
             xsyntax_table (lsyntax_table));

  if (regexpp (pattern))
    re.compile (pattern, 1);
  else
    re.compile (xstring_contents (pattern), xstring_length (pattern), 1);

  if (flags & SF_REVERSE
      ? re.search_backward (this, point, limit, b_contents.p2,
                            last_match, last_match_char)
      : re.search (this, point, b_contents.p1, limit,
                   last_match, last_match_char))
    {
      save_match (re, Qnil);
      goto_char (w_point, flags & SF_TAIL ? re_regs.end[0] : re_regs.start[0]);
      return 1;
    }
  return 0;
}

static int
scan_flags (lisp keys)
{
  int flags = 0;
  lisp x = find_keyword (Kcase_fold, keys);
  if (x == Ksmart)
    flags |= SF_SMART_CASE_FOLD;
  else if (x != Qnil)
    flags |= SF_CASE_FOLD;
  if (find_keyword_bool (Kno_dup, keys))
    flags |= SF_NO_DUP;
  if (find_keyword_bool (Kreverse, keys))
    flags |= SF_REVERSE;
  if (find_keyword_bool (Ktail, keys))
    flags |= SF_TAIL;
  x = find_keyword (Kleft_bound, keys);
  if (x == Ksymbol)
    flags |= SF_LSYMBOL;
  else if (x != Qnil)
    flags |= SF_LWORD;
  x = find_keyword (Kright_bound, keys);
  if (x == Ksymbol)
    flags |= SF_RSYMBOL;
  else if (x != Qnil)
    flags |= SF_RWORD;
  return flags;
}

static point_t
scan_limit (const Buffer *bp, int flags, lisp keys)
{
  lisp t = find_keyword (Klimit, keys);
  return (t != Qnil
          ? bp->coerce_to_restricted_point (t)
          : flags & SF_REVERSE ? bp->b_contents.p1 : bp->b_contents.p2);
}

static point_t
scan_last_match (const Buffer *bp, lisp keys, lChar &ch)
{
  lisp t = find_keyword (Klast_match, keys);
  if (t != Qnil)
    {
      check_cons (t);
      lisp lpoint = xcar (t);
      if (lpoint != Qnil)
        {
          lisp lch = xcdr (t);
          if (lch == Qnil)
            ch = lChar_EOF;
          else
            {
              check_char (lch);
              ch = xchar_code (lch);
            }
          return bp->coerce_to_restricted_point (lpoint);
        }
    }
  ch = lChar_EOF;
  return -1;
}

static int
smart_case_fold_string_p (lisp string)
{
  const Char *s = xstring_contents (string);
  const Char *const se = s + xstring_length (string);
  for (; s < se; s++)
    if (upper_char_p (*s))
      return 0;
  return 1;
}

lisp
Fscan_buffer (lisp pattern, lisp keys)
{
  int flags = scan_flags (keys);

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;

  point_t limit = scan_limit (bp, flags, keys);

  clear_regs ();

  if (!regexpp (pattern))
    {
      check_string (pattern);
      if (!xstring_length (pattern))
        return Qnil;
    }

  int result;
  if (find_keyword_bool (Kregexp, keys) || regexpp (pattern))
    {
      lChar last_match_char;
      point_t last_match = scan_last_match (bp, keys, last_match_char);
      result = bp->re_scan_buffer (wp->w_point, pattern, limit,
                                   last_match, last_match_char, flags);
    }
  else
    {
      int BM[256];

      if (flags & SF_SMART_CASE_FOLD
          && smart_case_fold_string_p (pattern))
        flags |= SF_CASE_FOLD;

      if (flags & SF_REVERSE)
        {
          bm_compileb (BM, xstring_contents (pattern), xstring_length (pattern),
                       flags & SF_CASE_FOLD);
          result = bp->scan_backward (wp->w_point, xstring_contents (pattern),
                                      xstring_length (pattern), BM, limit, flags);
        }
      else
        {
          bm_compilef (BM, xstring_contents (pattern), xstring_length (pattern),
                       flags & SF_CASE_FOLD);
          result = bp->scan_forward (wp->w_point, xstring_contents (pattern),
                                     xstring_length (pattern), BM, limit, flags);
        }
    }

  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;

  return boole (result);
}

static int
string_start_end (lisp string, lisp start, lisp end, int &offset, int &len)
{
  if (start && start != Qnil)
    {
      offset = fixnum_value (start);
      if (offset < 0 || offset > xstring_length (string))
        return 0;
    }
  else
    offset = 0;

  if (end && end != Qnil)
    {
      len = fixnum_value (end);
      if (len < offset || len > xstring_length (string))
        return 0;
    }
  else
    len = xstring_length (string);
  return 1;
}

static lisp
string_match (lisp regex, lisp string, lisp start, lisp end, const u_char *translate)
{
  check_string (string);

  clear_regs ();
  Regexp re (translate, xsyntax_table (selected_buffer ()->lsyntax_table));
  if (regexpp (regex))
    re.compile (regex, 1);
  else
    {
      check_string (regex);
      if (!xstring_length (regex))
        return Qnil;
      re.compile (xstring_contents (regex), xstring_length (regex), 1);
    }

  int offset, len;
  if (!string_start_end (string, start, end, offset, len))
    return Qnil;

  if (!re.search (xstring_contents (string), len, offset))
    return Qnil;
  save_match (re, string);
  return make_fixnum (re.re_regs.start[0]);
}

lisp
Fstring_match (lisp regexp, lisp string, lisp start, lisp end)
{
  return string_match (regexp, string, start, end, char_no_translate_table);
}

lisp
Fstring_matchp (lisp regexp, lisp string, lisp start, lisp end)
{
  return string_match (regexp, string, start, end, char_translate_upcase_table);
}

lisp
Fmatch_beginning (lisp regno)
{
  int r = fixnum_value (regno);
  if (r < 0 || r >= MAX_REGS)
    FErange_error (regno);
  if (r > re_regs.nregs || re_regs.start[r] < 0)
    return Qnil;
  return make_fixnum (re_regs.start[r]);
}

lisp
Fmatch_end (lisp regno)
{
  int r = fixnum_value (regno);
  if (r < 0 || r >= MAX_REGS)
    FErange_error (regno);
  if (r > re_regs.nregs || re_regs.start[r] < 0)
    return Qnil;
  return make_fixnum (re_regs.end[r]);
}

lisp
Fmatch_string (lisp regno)
{
  int r = fixnum_value (regno);
  if (r < 0 || r >= MAX_REGS)
    FErange_error (regno);
  if (r > re_regs.nregs || re_regs.start[r] < 0)
    return Qnil;
  lisp string = xsymbol_value (Vlast_match_string);
  if (string != Qnil)
    {
      int start = min ((int)re_regs.start[r], xstring_length (string));
      int end = max (start, min ((int)re_regs.end[r], xstring_length (string)));
      return make_string (xstring_contents (string) + start, end - start);
    }
  return selected_buffer ()->substring (re_regs.start[r], re_regs.end[r]);
}

lisp
Fcompile_regexp (lisp pattern, lisp case_fold)
{
  if (regexpp (pattern))
    return pattern;
  check_string (pattern);
  if (!xstring_length (pattern))
    return Qnil;

  Regexp re (((case_fold == Ksmart
               ? Regexp::smart_case_fold_p (xstring_contents (pattern),
                                            xstring_length (pattern))
               : case_fold && case_fold != Qnil)
              ? char_translate_upcase_table
              : char_no_translate_table),
             xsyntax_table (selected_buffer ()->lsyntax_table));
  re.compile (xstring_contents (pattern), xstring_length (pattern), 1);
  return re.make_regexp (pattern);
}

lisp
Fcompiled_regexp_source (lisp re)
{
  check_regexp (re);
  return xregexp_source (re);
}

lisp
Fcompiled_regexp_case_fold_p (lisp re)
{
  check_regexp (re);
  return boole (xregexp_flags (re) & lregexp::TRANSLATE);
}

lisp
Fregexp_quote (lisp string)
{
  check_string (string);

  int count = 0;
  const Char *p = xstring_contents (string);
  const Char *pe = p + xstring_length (string);
  while (p < pe)
    switch (*p++)
      {
      case '^':
      case '$':
      case '.':
      case '[':
      case '*':
      case '+':
      case '?':
      case '\\':
        count++;
        break;

      default:
        break;
      }

  if (!count)
    return string;

  p = xstring_contents (string);

  lisp string2 = make_string (xstring_length (string) + count);
  Char *p2 = xstring_contents (string2);

  while (p < pe)
    {
      Char c = *p++;
      switch (c)
        {
        case '^':
        case '$':
        case '.':
        case '[':
        case '*':
        case '+':
        case '?':
        case '\\':
          *p2++ = '\\';
          /* fall thru... */
        default:
          *p2++ = c;
          break;
        }
    }
  return string2;
}

lisp
Flooking_at (lisp regex, lisp case_fold)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;

  clear_regs ();
  Regexp re (((case_fold == Ksmart
               ? (stringp (regex)
                  && Regexp::smart_case_fold_p (xstring_contents (regex),
                                                xstring_length (regex)))
               : case_fold && case_fold != Qnil)
              ? char_translate_upcase_table
              : char_no_translate_table),
             xsyntax_table (bp->lsyntax_table));

  if (regexpp (regex))
    re.compile (regex, 0);
  else
    {
      check_string (regex);
      if (!xstring_length (regex))
        return Qnil;
      re.compile (xstring_contents (regex), xstring_length (regex), 0);
    }

  if (!re.match (bp, wp->w_point, bp->b_contents.p1, bp->b_contents.p2))
    return Qnil;
  save_match (re, Qnil);
  return Qt;
}

lisp
Flooking_for (lisp string, lisp case_fold)
{
  check_string (string);

  Window *wp = selected_window ();

  if (wp->w_point.p_point + xstring_length (string) > wp->w_bufp->b_contents.p2)
    return Qnil;

  const Chunk *cp = wp->w_point.p_chunk;
  const Char *p = cp->c_text + wp->w_point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  const Char *s = xstring_contents (string);
  const Char *se = s + xstring_length (string);

  int cf = (case_fold == Ksmart
            ? smart_case_fold_string_p (string)
            : case_fold && case_fold != Qnil);

  while (s < se)
    {
      if (p == pe)
        {
          cp = cp->c_next;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
      Char c1 = *s++;
      Char c2 = *p++;
      if (cf ? char_upcase (c1) != char_upcase (c2) : c1 != c2)
        return Qnil;
    }

  return Qt;
}

lisp
Flooking_back (lisp string, lisp case_fold)
{
  check_string (string);

  Window *wp = selected_window ();

  if (wp->w_point.p_point - xstring_length (string) < wp->w_bufp->b_contents.p1)
    return Qnil;

  const Chunk *cp = wp->w_point.p_chunk;
  const Char *p = cp->c_text + wp->w_point.p_offset;
  const Char *s = xstring_contents (string);
  const Char *se = s + xstring_length (string);

  int cf = (case_fold == Ksmart
            ? smart_case_fold_string_p (string)
            : case_fold && case_fold != Qnil);

  while (se > s)
    {
      if (p == cp->c_text)
        {
          cp = cp->c_prev;
          p = cp->c_text + cp->c_used;
        }
      Char c1 = *--se;
      Char c2 = *--p;
      if (cf ? char_upcase (c1) != char_upcase (c2) : c1 != c2)
        return Qnil;
    }

  return Qt;
}

static lisp
skip_chars (lisp chars, int dir)
{
  check_string (chars);

  u_long hi[(256 + sizeof (u_long) - 1) / sizeof (u_long)];
  bzero (hi, sizeof hi);

  u_long lo[256][(256 + sizeof (u_long) - 1) / sizeof (u_long)];

  const Char *p = xstring_contents (chars);
  const Char *pe = p + xstring_length (chars);

  if (p == pe)
    return Qnil;

  int not = 0;
  if (*p == '^')
    {
      not = 1;
      if (p == pe)
        return Qnil;
      p++;
    }

  while (p < pe)
    {
      Char c = *p++;
      if (p < pe - 1 && *p == '-')
        {
          Char c2 = p[1];
          p += 2;
          for (; c <= c2; c++)
            {
              int h = c >> 8;
              if (!bitisset (hi, h))
                {
                  bitset (hi, h);
                  bzero (lo[h], sizeof lo[h]);
                }
              bitset (lo[h], c & 255);
            }
        }
      else
        {
          int h = c >> 8;
          if (!bitisset (hi, h))
            {
              bitset (hi, h);
              bzero (lo[h], sizeof lo[h]);
            }
          bitset (lo[h], c & 255);
        }
    }

  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  Point &point = wp->w_point;

  if (dir < 0 && !bp->forward_char (point, -1))
    return Qnil;

  int nomatch = 1;

  if (!bp->eobp (point))
    while (1)
      {
        Char c = point.ch ();
        if (not == (bitisset (hi, c >> 8) && bitisset (lo[c >> 8], c & 255)))
          break;
        if (!bp->forward_char (point, dir) || bp->eobp (point))
          return Qt;
        nomatch = 0;
      }

  if (dir < 0)
    bp->forward_char (point, 1);
  return boole (!nomatch);
}

lisp
Fskip_chars_forward (lisp chars)
{
  return skip_chars (chars, 1);
}

lisp
Fskip_chars_backward (lisp chars)
{
  return skip_chars (chars, -1);
}

static lisp
skip_syntax_spec (lisp syntax_spec, int dir)
{
  u_char buf[SCmax];
  bzero (buf, sizeof buf);

  check_string (syntax_spec);
  const Char *p = xstring_contents (syntax_spec);
  const Char *pe = p + xstring_length (syntax_spec);
  if (p == pe)
    return Qnil;

  int not = 0;
  if (*p == '^')
    {
      not = 1;
      if (p == pe)
        return Qnil;
      p++;
    }

  while (p < pe)
    {
      Char c = *p++;
      if (!ascii_char_p (c) || syntax_spec_table[c] == -1)
        FEsimple_error (Einvalid_syntax_spec, syntax_spec);
      buf[syntax_spec_table[c]] = 1;
    }

  if (not)
    for (int i = 0; i < sizeof buf; i++)
      buf[i]--;

  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  const syntax_table *tab = xsyntax_table (bp->lsyntax_table);
  Point &point = wp->w_point;

  if (dir < 0 && !bp->forward_char (point, -1))
    return Qnil;

  int nomatch = 1;

  if (!bp->eobp (point))
    while (1)
      {
        Char c = point.ch ();
        if (c >= 256)
          c >>= 8;
        if (!buf[xchar_syntax (tab, c)])
          break;
        if (!bp->forward_char (point, dir) || bp->eobp (point))
          return Qt;
        nomatch = 0;
      }

  if (dir < 0)
    bp->forward_char (point, 1);
  return boole (!nomatch);
}

lisp
Fskip_syntax_spec_forward (lisp syntax_spec)
{
  return skip_syntax_spec (syntax_spec, 1);
}

lisp
Fskip_syntax_spec_backward (lisp syntax_spec)
{
  return skip_syntax_spec (syntax_spec, -1);
}

static int
re_tag_p (lisp string)
{
  for (const Char *p = xstring_contents (string), *pe = p + xstring_length (string) - 1;
       p < pe; p++)
    if (*p == '\\')
      return 1;
  return 0;
}

#define NOCASECONV     'E'
#define UPCASE_ONE     'u'
#define DOWNCASE_ONE   'l'
#define UPCASE_CHARS   'U'
#define DOWNCASE_CHARS 'L'

static void
case_conversion (int fconv, Point &from, const Point &end, Buffer *bp)
{
  switch (fconv)
    {
    case UPCASE_ONE:
      if (from.p_point < end.p_point)
        bp->upcase_region_internal (from, from.p_point + 1);
      break;

    case DOWNCASE_ONE:
      if (from.p_point < end.p_point)
        bp->downcase_region_internal (from, from.p_point + 1);
      break;

    case UPCASE_CHARS:
      bp->upcase_region_internal (from, end.p_point);
      break;

    case DOWNCASE_CHARS:
      bp->downcase_region_internal (from, end.p_point);
      break;
    }
}

static void
replace_match (Window *wp, lisp string, int literal)
{
  Buffer *bp = wp->w_bufp;
  if (literal)
    {
      int l = min (xstring_length (string), int (re_regs.end[0] - re_regs.start[0]));
      if (l)
        {
          bp->goto_char (wp->w_point, re_regs.start[0]);
          bp->overwrite_chars (wp, xstring_contents (string), l);
        }
      if (re_regs.start[0] + l != re_regs.end[0])
        bp->delete_region (wp, re_regs.start[0] + l, re_regs.end[0]);
      else if (l != xstring_length (string))
        {
          bp->goto_char (wp->w_point, re_regs.start[0] + l);
          bp->insert_chars (wp, xstring_contents (string) + l,
                            xstring_length (string) - l, 1);
        }
      else
        bp->goto_char (wp->w_point, re_regs.end[0]);
    }
  else
    {
      int l = (min (max (re_regs.end[0], bp->b_contents.p1), bp->b_contents.p2)
               - min (max (re_regs.start[0], bp->b_contents.p1), bp->b_contents.p2));
      Char *b = (Char *)alloca (sizeof (Char) * l);
      bp->substring (re_regs.start[0], l, b);
      bp->delete_region (wp, re_regs.start[0], re_regs.end[0]);

      const Char *p = xstring_contents (string);
      const Char *pe = p + xstring_length (string);
      const Char *p0 = p;
      int fconv = NOCASECONV;
      Point conv_point;

      while (p < pe)
        {
          if (*p++ == '\\')
            {
              bp->insert_chars (wp, p0, p - p0 - 1, 1);
              if (p == pe)
                break;
              Char c = *p++;
              switch (c)
                {
                case '&':
                  c = '0';
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                  c -= '0';
                  if (re_regs.start[c] >= 0)
                    bp->insert_chars (wp,
                                      b + min (int (re_regs.start[c] - re_regs.start[0]),
                                               l),
                                      min (int (re_regs.end[c] - re_regs.start[c]),
                                           l),
                                      1);
                  p0 = p;
                  break;

                case NOCASECONV:
                case UPCASE_ONE:
                case DOWNCASE_ONE:
                case UPCASE_CHARS:
                case DOWNCASE_CHARS:
                  case_conversion (fconv, conv_point, wp->w_point, bp);
                  fconv = c;
                  conv_point = wp->w_point;
                  p0 = p;
                  break;

                default:
                  p0 = p - 1;
                  break;
                }
            }
        }
      bp->insert_chars (wp, p0, p - p0, 1);
      case_conversion (fconv, conv_point, wp->w_point, bp);
    }
}

lisp
Freplace_match (lisp string, lisp keys)
{
  check_string (string);
  if (re_regs.start[0] < 0)
    return Qnil;
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  wp->w_bufp->check_read_only ();
  replace_match (wp, string, (find_keyword (Kliteral, keys) != Qnil
                              || !re_tag_p (string)));
  return Qt;
}

static lChar
match_end_before (Buffer *bp, const Point &point)
{
  Point p (point);
  bp->goto_char (p, re_regs.end[0]);
  if (bp->bobp (p))
    return lChar_EOF;
  return p.prevch ();
}

lisp
Freplace_buffer (lisp pattern, lisp replacement, lisp keys)
{
  int flags = scan_flags (keys) & ~SF_REVERSE;
  int once = find_keyword_bool (Konce, keys);
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t limit = scan_limit (bp, flags, keys);
  lChar last_match_char;
  point_t last_match = scan_last_match (bp, keys, last_match_char);
  clear_regs ();
  if (!regexpp (pattern))
    {
      check_string (pattern);
      if (!xstring_length (pattern))
        return make_fixnum (0);
    }
  check_string (replacement);
  int regexp = find_keyword_bool (Kregexp, keys) || regexpp (pattern);
  int literal = find_keyword (Kliteral, keys) != Qnil || !re_tag_p (replacement);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  wp->w_bufp->check_read_only ();

  int delete_regexp = 0;
  int BM[256];
  if (!regexp)
    {
      if (flags & SF_SMART_CASE_FOLD
          && smart_case_fold_string_p (pattern))
        flags |= SF_CASE_FOLD;
      bm_compilef (BM, xstring_contents (pattern), xstring_length (pattern),
                   flags & SF_CASE_FOLD);
    }
  else if (!regexpp (pattern))
    {
      pattern = Fcompile_regexp (pattern, (flags & SF_SMART_CASE_FOLD
                                           ? Ksmart : boole (flags & SF_CASE_FOLD)));
      delete_regexp = 1;
    }

  point_t last_pos = -1;
  int count = 0;
  while ((regexp
          ? bp->re_scan_buffer (wp->w_point, pattern, limit,
                                last_match, last_match_char, flags)
          : bp->scan_forward (wp->w_point, xstring_contents (pattern),
                              xstring_length (pattern), BM, limit, flags))
         && re_regs.start[0] >= 0)
    {
      if (re_regs.start[0] != re_regs.end[0]
          || last_match != re_regs.start[0])
        {
          long ochars = bp->b_nchars;
          last_match_char = match_end_before (bp, wp->w_point);
          replace_match (wp, replacement, literal);
          last_match = wp->w_point.p_point;
          limit += bp->b_nchars - ochars;
          count++;
          if (once)
            {
              last_pos = wp->w_point.p_point;
              bp->goto_eol (wp->w_point);
              flags |= SF_NO_DUP;
            }
          else if (re_regs.start[0] == re_regs.end[0])
            flags |= SF_NO_DUP;
          else
            flags &= ~SF_NO_DUP;
        }
      else
        flags |= SF_NO_DUP;
      clear_regs ();
      if (bp->eobp (wp->w_point))
        break;
      QUIT;
    }
  if (last_pos != -1)
    bp->goto_char (wp->w_point, last_pos);
  if (delete_regexp)
    destruct_regexp (pattern);
  return make_fixnum (count);
}

lisp
Fmatch_data (lisp v)
{
  if (re_regs.start[0] < 0)
    return Qnil;

  if (!v || v == Qnil)
    v = make_vector (2 * MAX_REGS + 1, Qnil);
  else
    {
      check_general_vector (v);
      if (xvector_length (v) != 2 * MAX_REGS + 1)
        FEtype_error (v, Smatch_data);
    }

  lisp *p = xvector_contents (v);
  for (int i = 0; i < MAX_REGS; i++)
    if (re_regs.start[i] >= 0)
      {
        *p++ = make_fixnum (re_regs.start[i]);
        *p++ = make_fixnum (re_regs.end[i]);
      }
    else
      {
        *p++ = Qnil;
        *p++ = Qnil;
      }
  *p = xsymbol_value (Vlast_match_string);
  return v;
}

lisp
Fstore_match_data (lisp data)
{
  if (data == Qnil)
    {
      clear_regs ();
      return Qnil;
    }

  check_general_vector (data);
  if (xvector_length (data) != 2 * MAX_REGS + 1)
    FEtype_error (data, Smatch_data);

  Regexp::sregs r;
  lisp *p = xvector_contents (data);
  for (int i = 0; i < MAX_REGS; i++, p += 2)
    if (*p == Qnil)
      r.start[i] = r.end[i] = -1;
    else
      {
        r.start[i] = fixnum_value (*p);
        r.end[i] = fixnum_value (p[1]);
      }
  r.nregs = MAX_REGS - 1;
  xsymbol_value (Vlast_match_string) = stringp (*p) ? *p : Qnil;
  copy_regs0 (re_regs, r);
  check_regs ();

  return Qt;
}

static void
ss_add (StrBuf &sb, Char cc, int &fconv)
{
  switch (fconv)
    {
    case UPCASE_ONE:
      sb.add (char_upcase (cc));
      fconv = NOCASECONV;
      break;

    case DOWNCASE_ONE:
      sb.add (char_downcase (cc));
      fconv = NOCASECONV;
      break;

    case UPCASE_CHARS:
      sb.add (char_upcase (cc));
      break;

    case DOWNCASE_CHARS:
      sb.add (char_downcase (cc));
      break;

    default:
      sb.add (cc);
      break;
    }
}

static void
substitute_string (StrBuf &sb, lisp string, lisp replacement)
{
  const Char *r = xstring_contents (replacement);
  const Char *const re = r + xstring_length (replacement);
  point_t l = xstring_length (string);
  int fconv = NOCASECONV;

  while (r < re)
    {
      Char c = *r++;
      if (c != '\\')
        ss_add (sb, c, fconv);
      else
        {
          if (r == re)
            break;
          c = *r++;
          switch (c)
            {
            case '&':
              c = '0';
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
              c -= '0';
              if (re_regs.start[c] >= 0)
                {
                  int start = min (re_regs.start[c], l);
                  int end = min (re_regs.end[c], l);
                  for (; start < end; start++)
                    ss_add (sb, xstring_contents (string)[start], fconv);
                }
              break;

            case NOCASECONV:
            case UPCASE_ONE:
            case DOWNCASE_ONE:
            case UPCASE_CHARS:
            case DOWNCASE_CHARS:
              fconv = c;
              break;

            default:
              ss_add (sb, c, fconv);
              break;
            }
        }
    }
}

lisp
Fsubstitute_string (lisp string, lisp pattern, lisp replacement, lisp keys)
{
  clear_regs ();

  lisp case_fold = find_keyword (Kcase_fold, keys);
  const u_char *const translate = ((case_fold == Ksmart
                                    ? (stringp (pattern)
                                       && Regexp::smart_case_fold_p (xstring_contents (pattern),
                                                                     xstring_length (pattern)))
                                    : case_fold != Qnil)
                                   ? char_translate_upcase_table
                                   : char_no_translate_table);

  check_string (string);
  check_string (replacement);

  Regexp re (translate, xsyntax_table (selected_buffer ()->lsyntax_table));
  if (regexpp (pattern))
    re.compile (pattern, 1);
  else
    {
      check_string (pattern);
      if (!xstring_length (pattern))
        return string;
      re.compile (xstring_contents (pattern), xstring_length (pattern), 1);
    }

  int offset, len;
  if (!string_start_end (string,
                         find_keyword (Kstart, keys, Qnil),
                         find_keyword (Kend, keys, Qnil),
                         offset, len))
    return Qnil;

  int count;
  lisp lcount = find_keyword (Kcount, keys, Qnil);
  if (lcount == Qnil)
    count = -1;
  else
    {
      count = fixnum_value (lcount);
      if (!count)
        return string;
    }

  lisp lskip = find_keyword (Kskip, keys, Qnil);
  int skip = lskip == Qnil ? 0 : fixnum_value (lskip);

  char tem[1024];
  StrBuf sb (tem, sizeof tem);
  if (offset)
    sb.add (xstring_contents (string), offset);

  int found = 0;
  while (re.search (xstring_contents (string), len, offset))
    {
      save_match (re, Qnil);
      if (skip > 0)
        {
          sb.add (xstring_contents (string) + offset, re_regs.end[0] - offset);
          skip--;
        }
      else
        {
          sb.add (xstring_contents (string) + offset, re_regs.start[0] - offset);
          substitute_string (sb, string, replacement);
          count--;
          found++;
        }
      offset = re_regs.end[0];
      if (re_regs.start[0] == re_regs.end[0])
        break;
      if (offset == len)
        break;
      if (!count)
        break;
      clear_regs ();
    }

  sb.add (xstring_contents (string) + offset, xstring_length (string) - offset);
  multiple_value::count () = 2;
  multiple_value::value (1) = make_fixnum (found);
  return sb.make_string ();
}

lisp
Fstring_replace_match (lisp string, lisp replacement)
{
  check_string (string);
  check_string (replacement);

  if (re_regs.start[0] < 0)
    return Qnil;

  char tem[1024];
  StrBuf sb (tem, sizeof tem);
  substitute_string (sb, string, replacement);
  return sb.make_string ();
}

lisp
Fstring_looking_at (lisp regex, lisp string, lisp keys)
{
  check_string (string);
  clear_regs ();

  lisp case_fold = find_keyword (Kcase_fold, keys);
  Regexp re (((case_fold == Ksmart
               ? (stringp (regex)
                  && Regexp::smart_case_fold_p (xstring_contents (regex),
                                                xstring_length (regex)))
               : case_fold != Qnil)
              ? char_translate_upcase_table
              : char_no_translate_table),
             xsyntax_table (selected_buffer ()->lsyntax_table));
  if (regexpp (regex))
    re.compile (regex, 0);
  else
    {
      check_string (regex);
      if (!xstring_length (regex))
        return Qnil;
      re.compile (xstring_contents (regex), xstring_length (regex), 0);
    }

  int offset, len;
  if (!string_start_end (string,
                         find_keyword (Kstart, keys, Qnil),
                         find_keyword (Kend, keys, Qnil),
                         offset, len))
    return Qnil;

  if (!re.match (xstring_contents (string), len, offset))
    return Qnil;
  save_match (re, string);
  return make_fixnum (re.re_regs.start[0]);
}

lisp
Fcompare_buffer_substrings (lisp buffer1, lisp start1, lisp end1,
                            lisp buffer2, lisp start2, lisp end2,
                            lisp case_fold)
{
  Buffer *bp1 = Buffer::coerce_to_buffer (buffer1);
  Buffer *bp2 = Buffer::coerce_to_buffer (buffer2);
  point_t p1 = bp1->coerce_to_restricted_point (start1);
  point_t pe1 = bp1->coerce_to_restricted_point (end1);
  point_t p2 = bp2->coerce_to_restricted_point (start2);
  point_t pe2 = bp2->coerce_to_restricted_point (end2);
  if (p1 > pe1)
    swap (p1, pe1);
  if (p2 > pe2)
    swap (p2, pe2);

  if (case_fold == Qnil)
    case_fold = 0;

  Point point1, point2;
  bp1->set_point (point1, p1);
  bp2->set_point (point2, p2);
  point_t end = p1 + min (pe1 - p1, pe2 - p2);
  int diff = 0;
  while (point1.p_point < end)
    {
      Char c1 = point1.ch ();
      Char c2 = point2.ch ();
      diff = case_fold ? char_upcase (c1) - char_upcase (c2) : c1 - c2;
      if (diff)
        break;
      if (!bp1->forward_char (point1, 1)
          || !bp2->forward_char (point2, 1))
        break;
    }

  if (!diff)
    diff = (pe1 - p1) - (pe2 - p2);
  return make_fixnum (diff < 0
                      ? -1 - (point1.p_point - p1)
                      : 1 + (point1.p_point - p1));
}
