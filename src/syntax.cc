#include "stdafx.h"
#include "ed.h"
#include "syntaxinfo.h"

char syntax_spec_table[128];

void
init_syntax_spec ()
{
  memset (syntax_spec_table, -1, sizeof syntax_spec_table);
  const char *p = " .()$\"<>/\\'_wkjx{}@";
  for (int i = 0; *p; i++, p++)
    syntax_spec_table[*p] = i;
}

lisp
Fmake_syntax_table ()
{
  lisp x = make_syntax_table ();
  syntax_table *p = (syntax_table *)xmalloc (sizeof *p);
  bzero (p, sizeof *p);
  p->comment_column = -1;
  xsyntax_table (x) = p;

  int i;
  for (i = 0; i <= ' '; i++)
    xchar_syntax (p, i) = SCjunk;
  xchar_syntax (p, ' ') = SCwhite;
  xchar_syntax (p, '\t') = SCwhite;
  xchar_syntax (p, '\n') = SCwhite;
  xchar_syntax (p, '\r') = SCwhite;
  xchar_syntax (p, '\f') = SCwhite;
  for (; i <= 0x7e; i++)
    xchar_syntax (p, i) = SCpunct;
  xchar_syntax (p, i++) = SCjunk; // 7f
  xchar_syntax (p, i++) = SCjunk; // 80
  for (; i < 0xa0; i++)
    xchar_syntax (p, i) = SCkanji;
  xchar_syntax (p, i++) = SCjunk; // a0
  for (; i <= 0xdf; i++)
    xchar_syntax (p, i) = SCkana;
  for (; i <= 0xfc; i++)
    xchar_syntax (p, i) = SCkanji;
  for (; i <= 0xff; i++)
    xchar_syntax (p, i) = SCjunk;
#if 0
  for (const char *s = "&*+-/<=>_|"; *s; s++)
    xchar_syntax (p, *s) = SCsymbol;
#endif
  for (i = '0'; i <= '9'; i++)
    xchar_syntax (p, i) = SCword;
  for (i = 'A'; i <= 'Z'; i++)
    xchar_syntax (p, i) = SCword;
  for (i = 'a'; i <= 'z'; i++)
    xchar_syntax (p, i) = SCword;
#if 0
  xchar_syntax (p, '(') = SCopen;
  xchar_syntax (p, '{') = SCopen;
  xchar_syntax (p, '[') = SCopen;
  xchar_syntax (p, ')') = SCclose;
  xchar_syntax (p, '}') = SCclose;
  xchar_syntax (p, ']') = SCclose;
  xchar_match (p, '(') = ')';
  xchar_match (p, '{') = '}';
  xchar_match (p, '[') = ']';
  xchar_match (p, ')') = '(';
  xchar_match (p, '}') = '{';
  xchar_match (p, ']') = '[';
#endif
  return x;
}

lisp
Fsyntax_table (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lsyntax_table;
}

lisp
Fuse_syntax_table (lisp table, lisp buffer, lisp invalidate)
{
  check_syntax_table (table);
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  bp->lsyntax_table = table;
  if (!invalidate || invalidate != Qnil)
    bp->invalidate_syntax ();
  return Qt;
}

lisp
Fcopy_syntax_table (lisp src, lisp dst)
{
  check_syntax_table (src);
  check_syntax_table (dst);
  memcpy (xsyntax_table (dst), xsyntax_table (src), sizeof *xsyntax_table (src));
  return dst;
}

static lisp
set_syntax (lisp table, lisp ch, int syntax)
{
  check_syntax_table (table);
  check_char (ch);
  Char c = xchar_code (ch);
  if (!ascii_char_p (c))
    return Qnil;
  xchar_syntax (xsyntax_table (table), c) = syntax;
  return Qt;
}

static lisp
set_syntax_multi_comment (lisp table, lisp string, int first, int second)
{
  check_syntax_table (table);
  check_string (string);
  if (xstring_length (string) != 2)
    return Qnil;
  syntax_table *tab = xsyntax_table (table);
  Char c1 = xstring_contents (string) [0];
  Char c2 = xstring_contents (string) [1];
  if (!ascii_char_p (c1) || !ascii_char_p (c2))
    return Qnil;
  int mask = ~(first | second);
  for (int i = 0; i <= 0x7f; i++)
    xchar_comment (tab, i) &= mask;
  xchar_comment (tab, c1) |= first;
  xchar_comment (tab, c2) |= second;
  return Qt;
}

static lisp
set_syntax_single_comment (lisp table, lisp ch, lisp ignore, lisp maybe, int syntax)
{
  check_syntax_table (table);
  check_char (ch);
  Char c = xchar_code (ch);
  if (!ascii_char_p (c))
    return Qnil;
  syntax_table *tab = xsyntax_table (table);
  xchar_syntax (tab, c) = syntax;
  if (ignore && ignore != Qnil)
    xchar_comment (tab, c) |= SFparse_sexp_ignore_comment;
  else
    xchar_comment (tab, c) &= ~SFparse_sexp_ignore_comment;
  if (maybe && maybe != Qnil)
    xchar_comment (tab, c) |= SFmaybe_comment_end;
  else
    xchar_comment (tab, c) &= ~SFmaybe_comment_end;
  return Qt;
}

static lisp
set_syntax_comment (lisp table, lisp ch, lisp ignore, int code, int clear)
{
  check_syntax_table (table);
  check_char (ch);
  Char c = xchar_code (ch);
  if (!ascii_char_p (c))
    return Qnil;
  syntax_table *tab = xsyntax_table (table);
  if (clear)
    for (int i = 0; i <= 0x7f; i++)
      xchar_comment (tab, i) &= ~code;
  xchar_comment (tab, c) |= code;
  if (ignore && ignore != Qnil)
    xchar_comment (tab, c) |= SFparse_sexp_ignore_comment;
  else
    xchar_comment (tab, c) &= ~SFparse_sexp_ignore_comment;
  return Qt;
}

lisp
Fset_syntax_whitespace (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCwhite);
}

lisp
Fset_syntax_punctuation (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCpunct);
}

static lisp
set_syntax_match (lisp table, lisp openc, lisp closec, int osyntax, int csyntax)
{
  check_syntax_table (table);
  check_char (openc);
  check_char (closec);
  Char o = xchar_code (openc);
  Char c = xchar_code (closec);
  if (!ascii_char_p (o) || !ascii_char_p (c))
    return Qnil;
  syntax_table *tab = xsyntax_table (table);
  xchar_syntax (tab, o) = osyntax;
  xchar_match (tab, o) = (u_char)c;
  xchar_syntax (tab, c) = csyntax;
  xchar_match (tab, c) = (u_char)o;
  return Qt;
}

lisp
Fset_syntax_match (lisp table, lisp openc, lisp closec)
{
  return set_syntax_match (table, openc, closec, SCopen, SCclose);
}

lisp
Fset_syntax_tag (lisp table, lisp openc, lisp closec)
{
  return set_syntax_match (table, openc, closec, SCtag_start, SCtag_end);
}

lisp
Fset_syntax_math (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCmath);
}

lisp
Fset_syntax_string (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCstring);
}

lisp
Fset_syntax_start_comment (lisp table, lisp ch, lisp ignore)
{
  return set_syntax_single_comment (table, ch, ignore, 0, SCcomment_start);
}

lisp
Fset_syntax_end_comment (lisp table, lisp ch, lisp ignore, lisp maybe)
{
  return set_syntax_single_comment (table, ch, ignore, maybe, SCcomment_end);
}

lisp
Fset_syntax_end_cplusplus_comment (lisp table, lisp ch, lisp ignore)
{
  return set_syntax_single_comment (table, ch, ignore, 0, SCcplusplus_comment_end);
}

lisp
Fset_syntax_escape (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCescape);
}

lisp
Fset_syntax_quote (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCquote);
}

lisp
Fset_syntax_symbol (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCsymbol);
}

lisp
Fset_syntax_symbol_prefix (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCsymbol_prefix);
}

lisp
Fset_syntax_word (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCword);
}

lisp
Fset_syntax_start_multi_comment (lisp table, lisp start)
{
  return set_syntax_multi_comment (table, start, SFcomment_start_first_char,
                                   SFcomment_start_second_char);
}

lisp
Fset_syntax_end_multi_comment (lisp table, lisp end)
{
  return set_syntax_multi_comment (table, end, SFcomment_end_first_char,
                                   SFcomment_end_second_char);
}

lisp
Fset_syntax_start_cplusplus_comment (lisp table, lisp ch, lisp ignore)
{
  return set_syntax_comment (table, ch, ignore, SFcplusplus_comment_char, 1);
}

lisp
Fset_syntax_start_column_comment (lisp table, lisp ch, lisp ignore)
{
  return set_syntax_comment (table, ch, ignore, SFcolumn_comment_char, 0);
}

lisp
Fset_syntax_junk (lisp table, lisp ch)
{
  return set_syntax (table, ch, SCjunk);
}

lisp
Fset_syntax_option (lisp table, lisp opt)
{
  check_syntax_table (table);
  xsyntax_table (table)->flags = fixnum_value (opt);
  return Qt;
}

lisp
Fset_syntax_comment_column (lisp table, lisp column)
{
  check_syntax_table (table);
  if (column == Qnil)
    xsyntax_table (table)->comment_column = -1;
  else
    {
      int n = fixnum_value (column);
      if (n < 0 || n > 80)
        FErange_error (column);
      xsyntax_table (table)->comment_column = n;
    }
  return Qt;
}

static lisp
syntaxp (lisp ch, lisp table, int syntax)
{
  check_char (ch);
  if (!table)
    table = selected_buffer ()->lsyntax_table;
  check_syntax_table (table);
  Char c = xchar_code (ch);
  if (!ascii_char_p (c))
    return Qnil;
  return boole (xchar_syntax (xsyntax_table (table), c) == syntax);
}

static lisp
syntax_multi_comment_p (lisp ch, lisp table, int syntax)
{
  check_char (ch);
  if (!table)
    table = selected_buffer ()->lsyntax_table;
  check_syntax_table (table);
  Char c = xchar_code (ch);
  if (!ascii_char_p (c))
    return Qnil;
  return boole (xchar_comment (xsyntax_table (table), c) & syntax);
}

lisp
Fsyntax_whitespace_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCwhite);
}

lisp
Fsyntax_punctuation_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCpunct);
}

lisp
Fsyntax_open_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCopen);
}

lisp
Fsyntax_close_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCclose);
}

lisp
Fsyntax_math_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCmath);
}

lisp
Fsyntax_string_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCstring);
}

lisp
Fsyntax_start_comment_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCcomment_start);
}

lisp
Fsyntax_end_comment_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCcomment_end);
}

lisp
Fsyntax_end_cplusplus_comment_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCcplusplus_comment_end);
}

lisp
Fsyntax_escape_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCescape);
}

lisp
Fsyntax_quote_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCquote);
}

lisp
Fsyntax_symbol_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCsymbol);
}

lisp
Fsyntax_symbol_prefix_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCsymbol_prefix);
}

lisp
Fsyntax_word_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCword);
}

lisp
Fsyntax_junk_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCjunk);
}

lisp
Fsyntax_open_tag_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCtag_start);
}

lisp
Fsyntax_close_tag_p (lisp ch, lisp table)
{
  return syntaxp (ch, table, SCtag_end);
}

lisp
Fsyntax_start_multi_comment_1_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcomment_start_first_char);
}

lisp
Fsyntax_start_multi_comment_2_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcomment_start_second_char);
}

lisp
Fsyntax_end_multi_comment_1_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcomment_end_first_char);
}

lisp
Fsyntax_end_multi_comment_2_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcomment_end_second_char);
}

lisp
Fsyntax_cplusplus_comment_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcplusplus_comment_char);
}

lisp
Fsyntax_start_column_comment_p (lisp ch, lisp table)
{
  return syntax_multi_comment_p (ch, table, SFcolumn_comment_char);
}

lisp
Fget_syntax_option (lisp table)
{
  check_syntax_table (table);
  return make_fixnum (xsyntax_table (table)->flags);
}

const word_state::category_range word_state::ws_range[] =
{
  {0x00c0, 0x01ff, WCword},              // LATIN LETTER
  {0x0250, 0x02af, WC2ipa},              // IPA EXTENSIONS
  {0x0386, 0x03ce, WCword, WC2greek},    // GREEK LETTER
  {0x0401, 0x04ff, WCword, WC2cyrillic}, // CYRILLIC LETTER
  {0x10a0, 0x10f0, WC2georgian},         // BASIC GEORGIAN & GEORGIAN EXTENDED
  {0x2500, 0x257f, WC2line},             // BOX DRAWINGS
  {0x3041, 0x3093, WC2hiragana},         // HIRAGANA LETTER
  {0x309b, 0x309c, WC2hiragana_or_katakana},  // KATAKANA-HIRAGANA (SEMI-)VOICED SOUND MARK
  {0x309d, 0x309e, WC2hiragana},         // HIRAGANA ITERATION MARK / HIRAGANA VOICED ITERATION MARK
  {0x30a1, 0x30f6, WC2katakana},         // KATAKANA LETTER
  {0x30fc, 0x30fc, WC2hiragana_or_katakana}, // KATAKANA-HIRAGANA PROLONGED SOUND MARK
  {0x30fd, 0x30fe, WC2katakana},         // KATAKANA ITERATION MARK / KATAKANA VOICED ITERATION MARK
  {0x4e00, 0x9fff, WC2kanji},            // CJK
  {0xac00, 0xd79f, WC2hangul},           // HANGUL
  {0xf900, 0xfaff, WC2kanji},            // CJK
  {0xff10, 0xff19, WC2alphanumeric},     // FULLWIDTH DIGIT
  {0xff21, 0xff3a, WC2alphanumeric},     // FULLWIDTH LATIN CAPITAL LETTER
  {0xff41, 0xff5a, WC2alphanumeric},     // FULLWIDTH LATIN SMALL LETTER
};

word_state::word_category
word_state::char_category (const syntax_table *tab, Char c)
{
  if (DBCP (c))
    {
      ucs2_t wc = i2w (c);
      for (int i = 0; i < numberof (ws_range); i++)
        {
          if (wc < ws_range[i].from)
            return WC2symbol;
          if (wc <= ws_range[i].to)
            return (!ws_range[i].cat2 || char_width (c) == 1
                    ? ws_range[i].cat1
                    : ws_range[i].cat2);
        }
      return WC2symbol;
    }
  else
    {
      switch (xchar_syntax (tab, c))
        {
        case SCword:
          return WCword;

        case SCkana:
          return WCkana;

        default:
          return WCnot_word;
        }
    }
}

int
word_state::forward (Char c)
{
  word_category last = ws_last;
  word_category cc = char_category (c);
  if (cc == WC2hiragana_or_katakana)
    {
      if (last == WC2hiragana || last == WC2katakana)
        cc = last;
      else if (last == WCfirst)
        last = cc;
    }
  else if (cc == WC2hiragana && (last == WC2kanji || last == WC2katakana))
    last = cc;
  else if (last == WC2hiragana_or_katakana
           && (cc == WC2hiragana || cc == WC2katakana))
    last = cc;
  ws_last = cc;
  if (cc == WCnot_word)
    return punct;
  return cc == last ? inword : not_inword;
}

int
word_state::backward (Char c)
{
  word_category last = ws_last;
  word_category cc = char_category (c);
  if (cc == WC2hiragana_or_katakana && (last == WC2hiragana || last == WC2katakana))
    cc = last;
  else if (last == WC2hiragana && (cc == WC2kanji || cc == WC2katakana))
    last = cc;
  else if ((cc == WC2hiragana || cc == WC2katakana)
           && last == WC2hiragana_or_katakana)
    last = cc;
  ws_last = cc;
  if (cc == WCnot_word)
    return punct;
  return cc == last ? inword : not_inword;
}

void
Buffer::upcase_region_internal (Point &point, point_t p2)
{
  point_t opoint = point.p_point;
  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (lower_char_p (c))
        point.ch () = _char_upcase (c);
      if (!forward_char (point, 1))
        break;
    }
  post_buffer_modified (Kmodify, point, opoint, point.p_point);
}

void
Buffer::downcase_region_internal (Point &point, point_t p2)
{
  point_t opoint = point.p_point;
  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (upper_char_p (c))
        point.ch () = _char_downcase (c);
      if (!forward_char (point, 1))
        break;
    }
  post_buffer_modified (Kmodify, point, opoint, point.p_point);
}

void
Buffer::capitalize_region_internal (Point &point, point_t p2)
{
  point_t opoint = point.p_point;
  int f = 1;
  while (point.p_point < p2)
    {
      Char c = point.ch ();
      if (alphanumericp (c))
        {
          if (f)
            point.ch () = char_upcase (c);
          else
            point.ch () = char_downcase (c);
          f = 0;
        }
      else
        f = 1;
      if (!forward_char (point, 1))
        break;
    }
  post_buffer_modified (Kmodify, point, opoint, point.p_point);
}

lisp
Fupcase_region (lisp from, lisp to)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p2 = bp->prepare_modify_region (wp, from, to);
  if (p2 != -1)
    {
      bp->upcase_region_internal (wp->w_point, p2);
      wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
    }
  return Qt;
}

lisp
Fdowncase_region (lisp from, lisp to)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p2 = bp->prepare_modify_region (wp, from, to);
  if (p2 != -1)
    {
      bp->downcase_region_internal (wp->w_point, p2);
      wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
    }
  return Qt;
}

lisp
Fcapitalize_region (lisp from, lisp to)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p2 = bp->prepare_modify_region (wp, from, to);
  if (p2 != -1)
    {
      bp->capitalize_region_internal (wp->w_point, p2);
      wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
    }
  return Qt;
}


/////////////////////////////////
#define skip_string_forward(point, term) \
  skip_string ((point), (term), 1)
#define skip_string_backward(point, term) \
  skip_string ((point), (term), -1)
#define skip_multi_chars_comment_forward(point) \
  skip_multi_chars_comment ((point), 1, \
                            SFcomment_end_first_char, \
                            SFcomment_end_second_char)
#define skip_multi_chars_comment_backward(point) \
  skip_multi_chars_comment ((point), -1, \
                            SFcomment_start_second_char, \
                            SFcomment_start_first_char)
#define forward_comment_p(point, f) \
  comment_char_p ((point), 1, (f))
#define backward_comment_p(point, f) \
  comment_char_p ((point), -1, (f))
#define forward_comment_start_p(point) \
  forward_comment_p ((point), SFcomment_start_second_char)
#define forward_comment_end_p(point) \
  forward_comment_p ((point), SFcomment_end_second_char)
#define backward_comment_start_p(point) \
  backward_comment_p ((point), SFcomment_start_first_char)
#define backward_comment_end_p(point) \
  backward_comment_p ((point), SFcomment_end_first_char)
#define forward_cplusplus_comment_p(point) \
  forward_comment_p ((point), SFcplusplus_comment_char)
#define backward_cplusplus_comment_p(point) \
  backward_comment_p ((point), SFcplusplus_comment_char)
#define skip_symbol_forward(point)  skip_symbol ((point), 1)
#define skip_symbol_backward(point) skip_symbol ((point), -1)
#define skip_single_char_comment_forward(point) \
  skip_single_char_comment ((point), 1, SCcomment_end)
#define skip_single_char_comment_backward(point) \
  skip_single_char_comment ((point), -1, SCcomment_start)

static int skip_over_newline;
static int skip_semi_colon;
static int skip_gt_or_lt;
static int skip_gt;
static int flag_fake_open_brace;
#define FLAG_PURE  1
#define FLAG_POUND 2
static int flag_pure;

static inline syntax_code
syntax (const syntax_table *tab, Char c)
{
  if (SBCP (c))
    return syntax_code (xchar_syntax (tab, c));
  return SCkanji;
}

static inline syntax_code
syntax (const syntax_table *tab, const Point &point)
{
  return syntax (tab, point.ch ());
}

int
Buffer::escaped_char_p (const Point &opoint, int in_string) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  Point point (opoint);
  if (!forward_char (point, -1))
    return 0;

  if (syntax (tab, point) == SCsymbol_prefix)
    {
      syntax_code sc = syntax (tab, opoint);
      if (sc == SCopen || sc == SCclose)
        return 0;
      if (!in_string && sc == SCstring)
        {
          Point p (point);
          goto_bol (p);
          while (p.p_point < point.p_point)
            {
              Char c = p.ch ();
              if (SBCP (c))
                {
                  if (xcomment_start_first_char_p (tab, c)
                      && forward_comment_start_p (p))
                    {
                      if (skip_multi_chars_comment_forward (p) || p.p_point > point.p_point)
                        return 0;
                    }
                  else
                    switch (xchar_syntax (tab, c))
                      {
                      case SCstring:
                        while (1)
                          {
                            if (!forward_char (p, 1) || p.p_point == point.p_point)
                              return 0;
                            if (p.ch () == c)
                              break;
                            if (syntax (tab, p) == SCescape
                                && (!forward_char (p, 1) || p.p_point == point.p_point))
                              return 0;
                          }
                        break;

                      case SCescape:
                      case SCsymbol_prefix:
                        if (!forward_char (p, 1) || p.p_point == point.p_point)
                          return 0;
                        break;

                      default:
                        break;
                      }
                }
              if (!forward_char (p, 1))
                break;
            }
          return 1;
        }
    }

  int n = 0;
  u_char prefix = in_string ? SCmax : SCsymbol_prefix;
  while (syntax (tab, point) == SCescape
         || syntax (tab, point) == prefix)
    {
      n++;
      if (!forward_char (point, -1))
        break;
    }
  return n & 1;
}

int
Buffer::skip_string (Point &point, Char term, int dir) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  while (1)
    {
      if (!forward_char (point, dir) || eobp (point))
        return Sin_string;
      Char c = point.ch ();
      if (c == term && !escaped_char_p (point, 1))
        return 0;
      if (syntax (tab, c) == SCopen && bolp (point))
        return Sin_string;
    }
}

int
Buffer::skip_multi_chars_comment (Point &point, int dir, int first, int second) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  while (1)
    {
      if (!forward_char (point, dir) || eobp (point))
        return Sunmatched_comment;
      Char c = point.ch ();
      while (SBCP (c) && xchar_comment (tab, c) & first)
        {
          if (!forward_char (point, dir) || eobp (point))
            return Sunmatched_comment;
          c = point.ch ();
          if (SBCP (c) && xchar_comment (tab, c) & second)
            return 0;
        }
    }
}

int
Buffer::comment_char_p (Point &point, int dir, int f) const
{
  if (!forward_char (point, dir) || eobp (point))
    return 0;
  Char c = point.ch ();
  if (SBCP (c) && xchar_comment (xsyntax_table (lsyntax_table), c) & f)
    return 1;
  forward_char (point, -dir);
  return 0;
}

int
Buffer::skip_single_char_comment (Point &point, int dir, syntax_code match) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  while (1)
    {
      if (!forward_char (point, dir) || eobp (point))
        return Sunmatched_comment;
      if (syntax (tab, point) == match)
        return 0;
    }
}

/* POINT.P_POINT ‚ÍŽg—p‚µ‚È‚¢ */
int
Buffer::column_comment_p (const syntax_table *tab, const Point &point) const
{
  if (tab->comment_column < 0)
    return 0;
  Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  for (int i = 0;; i++)
    {
      while (p == pe)
        {
          cp = cp->c_next;
          if (!cp)
            return 0;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
      Char c = *p++;
      if (c == '\n' || syntax (tab, c) == SCcomment_end)
        return 0;
      if (i == tab->comment_column)
        return (tab->flags & SYNTAX_OPT_COLUMN_CHAR
                ? ascii_char_p (c) && xcolumn_comment_char_p (tab, c)
                : c != ' ');
    }
}

int
Buffer::skip_maybe_comment (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  point_t opoint = point.p_point;
  goto_bol (point);
  if (tab->comment_column >= 0 && column_comment_p (tab, point))
    return 0;
  while (point.p_point < opoint)
    {
      Char c = point.ch ();
      if (SBCP (c))
        {
          if (xcomment_start_first_char_p (tab, c)
              && forward_comment_start_p (point))
            {
              if (skip_multi_chars_comment_forward (point) || point.p_point > opoint)
                {
                  goto_char (point, opoint);
                  return Sin_comment;
                }
            }
          else
            switch (xchar_syntax (tab, c))
              {
              case SCstring:
                if (Fparse_point_syntax (make_fixnum (point.p_point)) != Kstring)
                  while (1)
                    {
                      if (!forward_char (point, 1) || point.p_point == opoint)
                        return Sin_string;
                      if (point.ch () == c)
                        break;
                      if (syntax (tab, point) == SCescape
                          && (!forward_char (point, 1) || point.p_point == opoint))
                        return Sin_string;
                    }
                break;

              case SCcomment_start:
                return 0;

              case SCescape:
              case SCsymbol_prefix:
                if (!forward_char (point, 1) || point.p_point == opoint)
                  return 0;
                break;

              default:
                break;
              }
        }
      if (!forward_char (point, 1))
        break;
    }
  return 0;
}

int
Buffer::skip_cplusplus_comment_forward (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  while (1)
    {
      if (!forward_char (point, 1) || eobp (point))
        return Sunmatched_comment;
      if (syntax (tab, point) == SCcplusplus_comment_end)
        return 0;
    }
}

int
Buffer::skip_cplusplus_comment_backward (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  point_t opoint = point.p_point;
  goto_bol (point);
  while (point.p_point < opoint)
    {
      Char c = point.ch ();
      if (SBCP (c))
        {
          if (xcplusplus_comment_char_p (tab, c)
              && !xparse_sexp_ignore_comment_p (tab, c)
              && forward_cplusplus_comment_p (point))
            {
              forward_char (point, -1);
              return 0;
            }
          if (xcomment_start_first_char_p (tab, c)
              && forward_comment_start_p (point))
            {
              point_t comment_beg = point.p_point;
              if (skip_multi_chars_comment_forward (point) || point.p_point > opoint)
                {
                  goto_char (point, comment_beg);
                  return Sin_comment;
                }
            }
          else
            switch (xchar_syntax (tab, c))
              {
              case SCstring:
                while (1)
                  {
                    if (!forward_char (point, 1) || point.p_point == opoint)
                      return Sin_string;
                    if (point.ch () == c)
                      break;
                    if (syntax (tab, point) == SCescape
                        && (!forward_char (point, 1) || point.p_point == opoint))
                      return Sin_string;
                  }
                break;

              case SCescape:
              case SCsymbol_prefix:
                if (!forward_char (point, 1) || point.p_point == opoint)
                  return 0;
                break;

              default:
                break;
              }
        }
      if (!forward_char (point, 1))
        break;
    }
  return 0;
}

int
Buffer::goto_matched_open (Point &point, Char closec) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int depth = 1;
  int status;
  while (1)
    {
      if (!forward_char (point, -1))
        return Sunmatched_paren;

      Char c = point.ch ();
      if (!SBCP (c))
        continue;

      if (xcomment_start_second_char_p (tab, c)
          && backward_comment_start_p (point))
        return Sin_comment;
      if (xcomment_end_second_char_p (tab, c)
          && backward_comment_end_p (point))
        {
          status = skip_multi_chars_comment_backward (point);
          if (status)
            return status;
          continue;
        }
      if (xcplusplus_comment_char_p (tab, c)
          && !xparse_sexp_ignore_comment_p (tab, c)
          && backward_cplusplus_comment_p (point))
        return Sin_comment;

      switch (xchar_syntax (tab, c))
        {
        case SCopen:
        case SCtag_start:
          if (escaped_char_p (point))
            break;
          if (c == xchar_match (tab, closec))
            {
              if (!--depth)
                return 0;
            }
          else
            return Sunbalanced_paren;
          break;

        case SCclose:
        case SCtag_end:
          if (escaped_char_p (point))
            break;
          if (c == closec)
            depth++;
          else
            {
              status = goto_matched_open (point, c);
              if (status)
                return status;
            }
          break;

        case SCmath:
          if (escaped_char_p (point))
            break;
          if (c == closec)
            return 0;
          status = goto_matched_open (point, c);
          if (status)
            return status;
          break;

        case SCstring:
          if (escaped_char_p (point))
            break;
          status = skip_string_backward (point, c);
          if (status)
            return status;
          break;

        case SCcomment_start:
          if (escaped_char_p (point))
            break;
          if (xparse_sexp_ignore_comment_p (tab, c))
            break;
          return Sin_comment;

        case SCcomment_end:
          if (xparse_sexp_ignore_comment_p (tab, c))
            break;
          if (maybe_comment_end_p (tab, c))
            status = skip_maybe_comment (point);
          else
            status = skip_single_char_comment_backward (point);
          if (status)
            return status;
          break;

        case SCcplusplus_comment_end:
          if (xparse_sexp_ignore_comment_p (tab, c))
            break;
          status = skip_cplusplus_comment_backward (point);
          if (status && status != Sin_string)
            return status;
          break;

        default:
          break;
        }
    }
}

int
Buffer::goto_matched_close (Point &point, Char openc) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int depth = 1, status;
  Char prev_ch = 0;
  while (1)
    {
      if (tab->comment_column >= 0 && !eobp (point))
        prev_ch = point.ch ();
      if (!forward_char (point, 1) || eobp (point))
        return Sunmatched_paren;
      if (tab->comment_column >= 0 && prev_ch == '\n'
          && column_comment_p (tab, point))
        {
          status = skip_single_char_comment_forward (point);
          if (status)
            return status;
          continue;
        }
      Char c = point.ch ();
      if (!SBCP (c))
        continue;

      if (xcomment_end_first_char_p (tab, c)
          && forward_comment_end_p (point))
        return Sin_comment;
      if (xcomment_start_first_char_p (tab, c)
          && forward_comment_start_p (point))
        {
          status = skip_multi_chars_comment_forward (point);
          if (status)
            return status;
          continue;
        }
      if (xcplusplus_comment_char_p (tab, c)
          && !xparse_sexp_ignore_comment_p (tab, c)
          && forward_cplusplus_comment_p (point))
        {
          status = skip_cplusplus_comment_forward (point);
          if (status)
            return status;
          continue;
        }

      switch (xchar_syntax (tab, c))
        {
        case SCclose:
        case SCtag_end:
          if (escaped_char_p (point))
            break;
          if (c == xchar_match (tab, openc))
            {
              if (!--depth)
                return 0;
            }
          else
            return Sunbalanced_paren;
          break;

        case SCopen:
        case SCtag_start:
          if (escaped_char_p (point))
            break;
          if (c == openc)
            depth++;
          else
            {
              status = goto_matched_close (point, c);
              if (status)
                return status;
            }
          break;

        case SCmath:
          if (escaped_char_p (point))
            break;
          if (c == openc)
            return 0;
          status = goto_matched_close (point, c);
          if (status)
            return status;
          break;

        case SCstring:
          if (escaped_char_p (point))
            break;
          status = skip_string_forward (point, c);
          if (status)
            return status;
          break;

        case SCcomment_start:
          if (escaped_char_p (point))
            break;
          if (xparse_sexp_ignore_comment_p (tab, c))
            break;
          status = skip_single_char_comment_forward (point);
          if (status)
            return status;
          break;

        default:
          break;
        }
    }
}

int
Buffer::skip_symbol (Point &point, int dir) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  if (eobp (point))
    return 0;

  while (1)
    {
      switch (syntax (tab, point))
        {
        case SCword:
        case SCsymbol:
        case SCsymbol_prefix:
        case SCkanji:
        case SCkana:
        case SCescape:
        case SCquote:
          break;

        default:
          if (!escaped_char_p (point))
            {
              if (dir < 0)
                forward_char (point, 1);
              return 0;
            }
          break;
        }
      if (!forward_char (point, dir) || eobp (point))
        return 0;
    }
}

int
Buffer::skip_white_forward (Point &point, int flag) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int status;
  Char prev_ch = 0;

  if (eobp (point))
    return 0;
  if (tab->comment_column >= 0 && !bobp (point))
    prev_ch = point.prevch ();

  while (1)
    {
      if (tab->comment_column >= 0
          && prev_ch == '\n'
          && column_comment_p (tab, point))
        {
          status = skip_single_char_comment_forward (point);
          if (status)
            return status;
        }
      else
        {
          Char c = point.ch ();
          if (!SBCP (c))
            return 0;

          if (c == '<')
            {
              skip_gt_or_lt++;
              skip_gt = 1;
            }
          else if (c == '>')
            skip_gt_or_lt--;

          if (xcomment_start_first_char_p (tab, c)
              && forward_comment_start_p (point))
            {
              status = skip_multi_chars_comment_forward (point);
              if (status)
                return status;
              goto next;
            }
          if (xcomment_end_first_char_p (tab, c)
              && forward_comment_end_p (point))
            return Sin_comment;
          if (xcplusplus_comment_char_p (tab, c)
              && !xparse_sexp_ignore_comment_p (tab, c)
              && forward_cplusplus_comment_p (point))
            {
              status = skip_cplusplus_comment_forward (point);
              if (status)
                return status;
              goto next;
            }

          switch (xchar_syntax (tab, c))
            {
            case SCcomment_end:
              if (maybe_comment_end_p (tab, c))
                break;
              /* fall thru... */
            case SCpunct:
            case SCquote:
            case SCcplusplus_comment_end:
            case SCjunk:
              if (flag & FLAG_PURE)
                return 0;
              /* fall thru... */
            case SCwhite:
              break;

            case SCcomment_start:
              if (escaped_char_p (point))
                return 0;
              if (!xparse_sexp_ignore_comment_p (tab, c))
                {
                  status = skip_single_char_comment_forward (point);
                  if (status)
                    return status;
                }
              break;

            default:
              return 0;
            }
        }
    next:
      if (tab->comment_column >= 0 && !eobp (point))
        prev_ch = point.ch ();
      if (!forward_char (point, 1) || eobp (point))
        return Seob;
    }
}

int
Buffer::skip_white_backward (Point &point, int flag) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int status;

  if (bobp (point))
    return Sbob;

  if (eobp (point) && !forward_char (point, -1))
    return Sbob;

  flag_pure = 1;
  while (1)
    {
      Char c = point.ch ();
      if (!SBCP (c))
        return 0;
      if (c == ';' || c == '}')
        skip_semi_colon++;
      else if (c == '\n')
        skip_over_newline = 1;
      else if (c == '#' && flag & FLAG_POUND && bolp (point))
        {
          flag_fake_open_brace = 1;
          return 0;
        }

      if (xcomment_end_second_char_p (tab, c)
          && backward_comment_end_p (point))
        {
          status = skip_multi_chars_comment_backward (point);
          if (status)
            return status;
          goto next;
        }
      if (xcomment_start_second_char_p (tab, c)
          && backward_comment_start_p (point))
        return Sin_comment;

      if (xcplusplus_comment_char_p (tab, c)
          && !xparse_sexp_ignore_comment_p (tab, c)
          && backward_cplusplus_comment_p (point))
        return Sin_comment;

      switch (xchar_syntax (tab, c))
        {
        case SCescape:
          if (!escaped_char_p (point) && forward_char (point, 1))
            {
              c = eobp (point) ? 0 : point.ch ();
              forward_char (point, -1);
              if (c == '\n')
                break;
            }
          /* fall thru... */
        case SCpunct:
        case SCcomment_start:
        case SCquote:
        case SCjunk:
          if (flag & FLAG_PURE)
            return 0;
          flag_pure = 0;
          /* fall thru... */
        case SCwhite:
          break;

        case SCcomment_end:
          if (!xparse_sexp_ignore_comment_p (tab, c))
            {
              if (maybe_comment_end_p (tab, c))
                status = skip_maybe_comment (point);
              else
                status = skip_single_char_comment_backward (point);
              if (status)
                return status;
            }
          break;

        case SCcplusplus_comment_end:
          if (!xparse_sexp_ignore_comment_p (tab, c))
            {
              status = skip_cplusplus_comment_backward (point);
              if (status && status != Sin_string)
                return status;
            }
          break;

        default:
          return 0;
        }
    next:
      if (!forward_char (point, -1))
        return Sbob;
    }
}

int
Buffer::skip_sexp_forward (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int status;

  if (eobp (point))
    return Seob;

  while (1)
    {
      Char c = point.ch ();
      if (!SBCP (c))
        return skip_symbol_forward (point);

      switch (xchar_syntax (tab, c))
        {
        case SCclose:
        case SCtag_end:
          if (escaped_char_p (point))
            goto symbol;
          return Send_sexp;

        case SCopen:
        case SCtag_start:
        case SCmath:
          if (escaped_char_p (point))
            goto symbol;
          status = goto_matched_close (point, c);
          if (!status)
            forward_char (point, 1);
          return status;

        case SCstring:
          if (escaped_char_p (point))
            goto symbol;
          status = skip_string_forward (point, c);
          if (!status)
            forward_char (point, 1);
          return status;

        case SCword:
        case SCsymbol:
        case SCsymbol_prefix:
        case SCkanji:
        case SCkana:
        symbol:
          return skip_symbol_forward (point);

        case SCescape:
        case SCquote:
          break;

        default:
          return 0;
        }

      if (!forward_char (point, 1) || eobp (point))
        return Seob;
    }
}

int
Buffer::skip_sexp_backward (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);

  if (eobp (point) && !forward_char (point, -1))
    return Sbob;

  while (1)
    {
      Char c = point.ch ();
      if (!SBCP (c))
        return skip_symbol_backward (point);

      switch (xchar_syntax (tab, c))
        {
        case SCopen:
        case SCtag_start:
          if (escaped_char_p (point))
            goto symbol;
          return Send_sexp;

        case SCclose:
        case SCmath:
        case SCtag_end:
          if (escaped_char_p (point))
            goto symbol;
          return goto_matched_open (point, c);

        case SCstring:
          if (escaped_char_p (point))
            goto symbol;
          return skip_string_backward (point, c);

        case SCword:
        case SCsymbol:
        case SCsymbol_prefix:
        case SCkanji:
        case SCkana:
        symbol:
          return skip_symbol_backward (point);

        case SCescape:
        case SCquote:
          break;

        default:
          return 0;
        }

      if (!forward_char (point, -1))
        return Sbob;
    }
}

lisp
Fgoto_matched_parenthesis (lisp arg)
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  const syntax_table *tab = xsyntax_table (bp->lsyntax_table);
  Point &point = wp->w_point;
  if (!bp->eobp (point) && !bp->escaped_char_p (point))
    {
      Char c = point.ch ();
      if (!SBCP (c))
        return Qnil;
      point_t opoint = point.p_point;
      syntax_code syntax = syntax_code (xchar_syntax (tab, c));
      int status;
      if (syntax == SCclose || syntax == SCtag_end || (!arg && syntax == SCmath))
        status = bp->goto_matched_open (point, c);
      else if (syntax == SCopen || syntax == SCtag_start || syntax == SCmath)
        status = bp->goto_matched_close (point, c);
      else
        return Qnil;

      if (!status)
        return Qt;
      bp->goto_char (point, opoint);
      if (status == Sunbalanced_paren)
        format_message (Munbalanced_parenthesis);
      else if (status == Sunmatched_paren)
        format_message (Munmatched_parenthesis);
    }
  return Qnil;
}

static void
sexp_error (int status)
{
  if (status == Sunbalanced_paren)
    FEsimple_error (Munbalanced_parenthesis);
  if (status == Send_sexp || status == Sbob)
    FEsimple_error (Econtaining_expression_ends_prematurely);
  FEsimple_error (Munmatched_parenthesis);
}

lisp
Fforward_sexp (lisp arg, lisp noerror)
{
  int n = (!arg || arg == Qnil) ? 1 : fixnum_value (arg);
  if (!n)
    return Qnil;

  int (Buffer::*skip_white)(Point &, int) const;
  int (Buffer::*skip_sexp)(Point &) const;
  int dir;
  if (n > 0)
    {
      dir = 1;
      skip_white = &Buffer::skip_white_forward;
      skip_sexp = &Buffer::skip_sexp_forward;
    }
  else
    {
      dir = -1;
      n = -n;
      skip_white = &Buffer::skip_white_backward;
      skip_sexp = &Buffer::skip_sexp_backward;
    }

  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  Point &point = wp->w_point;
  point_t opoint = point.p_point;

  for (int i = 0; i < n; i++)
    {
      if (dir < 0 && !bp->forward_char (point, -1))
        return Qnil;

      int status;
      point_t before;
      do
        {
          before = point.p_point;
          status = (bp->*skip_white)(point, 0);
        }
      while (status == Sin_comment && point.p_point != before);
      if (status)
        return Qnil;

      status = (bp->*skip_sexp)(point);
      if (status)
        {
          if (status != Seob)
            {
              bp->goto_char (point, opoint);
              if (!noerror || noerror == Qnil)
                sexp_error (status);
            }
          return Qnil;
        }
    }

  if (dir < 0 && bp->forward_char (point, -1))
    {
      const syntax_table *tab = xsyntax_table (bp->lsyntax_table);
      while (syntax (tab, point) == SCquote)
        if (!bp->forward_char (point, -1))
          return Qt;
      bp->forward_char (point, 1);
    }
  return Qt;
}

lisp
Fforward_list (lisp arg, lisp noerror)
{
  int n = (!arg || arg == Qnil) ? 1 : fixnum_value (arg);
  if (!n)
    return Qnil;

  int (Buffer::*skip_white)(Point &, int) const;
  int (Buffer::*skip_sexp)(Point &) const;
  int dir, match1, match2;

  if (n > 0)
    {
      dir = 1;
      skip_white = &Buffer::skip_white_forward;
      skip_sexp = &Buffer::skip_sexp_forward;
      match1 = SCopen;
      match2 = SCtag_start;
    }
  else
    {
      dir = -1;
      n = -n;
      skip_white = &Buffer::skip_white_backward;
      skip_sexp = &Buffer::skip_sexp_backward;
      match1 = SCclose;
      match2 = SCtag_end;
    }

  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  const syntax_table *tab = xsyntax_table (bp->lsyntax_table);
  Point &point = wp->w_point;
  point_t opoint = point.p_point;

  for (int i = 0; i < n;)
    {
      if (dir < 0 && !bp->forward_char (point, -1))
        return Qnil;

      int status;
      point_t before;
      do
        {
          before = point.p_point;
          status = (bp->*skip_white)(point, 0);
        }
      while (status == Sin_comment && point.p_point != before);
      if (status)
        return Qnil;

      if (!bp->eobp (point) && (syntax (tab, point) == match1
                                || syntax (tab, point) == match2))
        i++;
      status = (bp->*skip_sexp)(point);
      if (status)
        {
          if (status != Seob)
            {
              bp->goto_char (point, opoint);
              if (!noerror || noerror == Qnil)
                sexp_error (status);
            }
          return Qnil;
        }
    }
  return Qt;
}

int
Buffer::up_down_list (Point &point, int n, int downp) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  int i = 0;
  int (Buffer::*skip_white)(Point &, int) const;
  int (Buffer::*skip_sexp)(Point &) const;
  int dir;

  if (n > 0)
    {
      dir = 1;
      skip_white = &Buffer::skip_white_forward;
      skip_sexp = &Buffer::skip_sexp_forward;
    }
  else
    {
      dir = -1;
      n = -n;
      skip_white = &Buffer::skip_white_backward;
      skip_sexp = &Buffer::skip_sexp_backward;
    }

  while (1)
    {
      if (dir < 0
          && ((!eobp (point) && bolp (point)
               && (syntax (tab, point) == SCopen
                   || syntax (tab, point) == SCtag_start))
              || !forward_char (point, -1)))
        return Send_sexp;

      int status;
      point_t before;
      do
        {
          before = point.p_point;
          status = (this->*skip_white)(point, 0);
        }
      while (status == Sin_comment && point.p_point != before);
      if (status && (dir > 0 || status != Sbob))
        return status;

      if (dir > 0 && !eobp (point)
          && (syntax (tab, point) == SCopen
              || syntax (tab, point) == SCtag_start))
        {
          if (downp)
            {
              forward_char (point, 1);
              if (++i == n)
                return 0;
            }
          else if (bolp (point))
            return Sunmatched_paren;
        }

      status = (this->*skip_sexp)(point);
      if (!downp && status == Send_sexp)
        {
          if (dir > 0)
            forward_char (point, 1);
          if (++i == n)
            return 0;
        }
      else if (status)
        return status;
      if (dir < 0 && bobp (point))
        return Sbob;
    }
}

static lisp
up_down_list (lisp arg, lisp noerror, int downp)
{
  int n = (!arg || arg == Qnil) ? 1 : fixnum_value (arg);
  if (!n)
    return Qnil;

  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  Point &point = wp->w_point;
  point_t opoint = point.p_point;
  int status = bp->up_down_list (point, n, downp);
  if (!status)
    return Qt;
  bp->goto_char (point, opoint);
  if (!noerror || noerror == Qnil)
    sexp_error (status);
  return Qnil;
}

lisp
Fup_list (lisp arg, lisp noerror)
{
  return up_down_list (arg, noerror, 0);
}

lisp
Fdown_list (lisp arg, lisp noerror)
{
  return up_down_list (arg, noerror, 1);
}

lisp
Fskip_white_forward ()
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  wp->w_bufp->skip_white_forward (wp->w_point, FLAG_PURE);
  return Qt;
}

lisp
Fskip_white_backward ()
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  wp->w_bufp->skip_white_backward (wp->w_point, FLAG_PURE);
  return Qt;
}

lisp
Fskip_token ()
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  Point &point = wp->w_point;

  if (bp->eobp (point))
    return Qnil;

  const syntax_table *tab = xsyntax_table (bp->lsyntax_table);

  Char c = point.ch ();
  if (!SBCP (c))
    bp->skip_symbol_forward (point);
  else
    switch (xchar_syntax (tab, c))
      {
      case SCstring:
        if (!bp->skip_string_forward (point, c))
          bp->forward_char (point, 1);
        break;

      case SCword:
      case SCsymbol:
      case SCsymbol_prefix:
      case SCkanji:
      case SCkana:
        bp->skip_symbol_forward (point);
        break;

      default:
        bp->forward_char (point, 1);
        break;
      }
  return Qt;
}

lisp
Findent_to (lisp arg)
{
  Char c;
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Buffer *bp = wp->w_bufp;
  bp->check_read_only ();
  Point &point = wp->w_point;

  int goal = fixnum_value (arg);

  int column = bp->point_column (point);
  if (column >= goal)
    return Qnil;

  if (symbol_value (Vindent_tabs_mode, bp) != Qnil)
    {
      int ntabs = goal / bp->b_tab_columns - column / bp->b_tab_columns;
      if (ntabs > 0)
        {
          column = goal / bp->b_tab_columns * bp->b_tab_columns;
          c = '\t';
          bp->insert_chars (wp, &c, 1, ntabs);
        }
    }

  int nspaces = goal - column;
  if (nspaces > 0)
    {
      c = ' ';
      bp->insert_chars (wp, &c, 1, nspaces);
    }
  return Qt;
}

lisp
Ftab_columns (lisp lbuffer)
{
  if (!lbuffer || lbuffer == Qnil)
    return make_fixnum (app.default_tab_columns);
  Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
  multiple_value::count () = 2;
  multiple_value::value (1) = boole (bp->b_local_tab_columns);
  return make_fixnum (bp->b_tab_columns);
}

lisp
Fset_tab_columns (lisp column, lisp lbuffer)
{
  if (!lbuffer || lbuffer == Qnil)
    {
      int n = fixnum_value (column);
      if (n < 1 || n > 32)
        FErange_error (column);
      if (app.default_tab_columns != n)
        {
          app.default_tab_columns = n;
          for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
            {
              bp->b_nfolded = -1;
              if (!bp->b_local_tab_columns)
                {
                  bp->b_tab_columns = n;
                  bp->fold_width_modified ();
                }
            }
          for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
            if (wp->w_bufp && !wp->w_bufp->b_local_tab_columns)
              wp->w_disp_flags |= Window::WDF_WINDOW;
        }
    }
  else
    {
      Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
      int n;
      if (column == Qnil)
        {
          bp->b_local_tab_columns = 0;
          n = app.default_tab_columns;
        }
      else
        {
          n = fixnum_value (column);
          if (n < 1 || n > 32)
            FErange_error (column);
          bp->b_local_tab_columns = 1;
        }
      if (bp->b_tab_columns != n)
        {
          bp->b_tab_columns = n;
          bp->fold_width_modified ();
          bp->refresh_buffer ();
        }
    }
  return Qt;
}

#define C_SYMBOL_MATCH_P(POINT, IDENT, JUNK_ALLOWED) \
  symbol_match_p ((POINT), (IDENT), sizeof (IDENT) - 1, (JUNK_ALLOWED))

int
Buffer::symbol_match_p (const Point &point, const char *s, int l,
                        int junk_allowed) const
{
  int rest = b_contents.p2 - point.p_point;
  if (rest < l)
    return 0;
  const Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  for (const u_char *t = (const u_char *)s, *te = t + l; t < te; t++)
    {
      if (*t != *p)
        return 0;
      if (++p == pe)
        {
          cp = cp->c_next;
          if (!cp)
            return 1;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
    }
  if (!rest || junk_allowed)
    return 1;
  syntax_code c = syntax (xsyntax_table (lsyntax_table), *p);
  return c != SCword && c != SCsymbol;
}

int
Buffer::symbol_match_p (const Point &point, const Char *s, int l, int igcase) const
{
  int rest = b_contents.p2 - point.p_point;
  if (rest < l)
    return 0;
  const Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  for (const Char *se = s + l; s < se; s++)
    {
      if (igcase ? char_upcase (*s) != char_upcase (*p) : *s != *p)
        return 0;
      if (++p == pe)
        {
          cp = cp->c_next;
          if (!cp)
            return 1;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
    }
  if (!rest)
    return 1;
  syntax_code c = syntax (xsyntax_table (lsyntax_table), *p);
  return c != SCword && c != SCsymbol;
}

int
Buffer::forward_identifier (Point &point, const Char *beg, int begl,
                            const Char *end, int endl, int igcase) const
{
  int depth = 1;
  while (1)
    {
      int n = skip_white_forward (point, 0);
      if (n)
        return 0;
      if (symbol_match_p (point, beg, begl, igcase))
        {
          if (!--depth)
            return 1;
        }
      else if (endl && symbol_match_p (point, end, endl, igcase))
        depth++;
      n = skip_sexp_forward (point);
      if (n)
        return 0;
      if (!forward_char (point, 1))
        return 0;
    }
}

int
Buffer::backward_identifier (Point &point, const Char *beg, int begl,
                             const Char *end, int endl, int igcase) const
{
  int depth = 1;
  while (1)
    {
      int n = skip_white_backward (point, 0);
      if (n)
        return 0;
      n = skip_sexp_backward (point);
      if (n)
        return 0;
      if (symbol_match_p (point, beg, begl, igcase))
        {
          if (!--depth)
            return 1;
        }
      else if (endl && symbol_match_p (point, end, endl, igcase))
        depth++;
      if (!forward_char (point, -1))
        return 0;
    }
}

lisp
Fforward_identifier (lisp beg, lisp end, lisp igcase)
{
  check_string (beg);
  if (!xstring_length (beg))
    return Qnil;
  if (end == Qnil)
    end = 0;
  else
    check_string (end);
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Point point (wp->w_point);
  if (!wp->w_bufp->forward_identifier (point,
                                       xstring_contents (beg),
                                       xstring_length (beg),
                                       end ? xstring_contents (end) : 0,
                                       end ? xstring_length (end) : 0,
                                       igcase != Qnil))
    return Qnil;
  wp->w_point = point;
  return Qt;
}

lisp
Fbackward_identifier (lisp beg, lisp end, lisp igcase)
{
  check_string (beg);
  if (!xstring_length (beg))
    return Qnil;
  if (end == Qnil)
    end = 0;
  else
    check_string (end);
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  Point point (wp->w_point);
  if (!wp->w_bufp->backward_identifier (point,
                                        xstring_contents (beg),
                                        xstring_length (beg),
                                        end ? xstring_contents (end) : 0,
                                        end ? xstring_length (end) : 0,
                                        igcase != Qnil))
    return Qnil;
  wp->w_point = point;
  return Qt;
}

#define Cindent (-1)
#define Csame (-2)
#define Cbrace (-3)
#define Ccontinue (-4)
#define Cargdecl (-5)
#define Cunmatch_else (-6)
#define Cunmatch_directive (-7)
#define Cenum (-8)
#define Cargdecl_colon (-9)
#define Cdefun (-10)
#define Cdefclass (-11)

static const char C_KWD_IF[] = "if";
static const char C_KWD_ELSE[] = "else";
static const char C_KWD_END[] = "end";
static const char C_KWD_EL[] = "el";
static const char C_KWD_DO[] = "do";
static const char C_KWD_TYPEDEF[] = "typedef";
static const char C_KWD_ENUM[] = "enum";
static const char C_KWD_CASE[] = "case";
static const char C_KWD_TEMPLATE[] = "template";
static const char C_KWD_CLASS[] = "class";
static const char C_KWD_STRUCT[] = "struct";
static const char C_KWD_INTERFACE[] = "interface";
static const char C_KWD_NAMESPACE[] = "namespace";
static const char C_KWD_THROW[] = "throw";
static const char C_KWD_THROWS[] = "throws";
static const char C_KWD_IMPLEMENTS[] = "implements";
static const char C_KWD_EXTENDS[] = "extends";
static const char C_KWD_INTERNAL[] = "internal";
static const char C_KWD_NEW[] = "new";
static const char C_KWD_SEALED[] = "sealed";
static const char C_KWD_EXTERN[] = "extern";
static const char C_KWD_REGION[] = "region";
static const char C_KWD_ENDREGION[] = "endregion";
static const char C_KWD_USING[] = "using";

#define C_KWD_LENGTH(KWD) (sizeof (KWD) - 1)
#define C_KWD_LENGTH_ENUM C_KWD_LENGTH (C_KWD_ENUM)
#define C_KWD_LENGTH_CLASS C_KWD_LENGTH (C_KWD_CLASS)
#define C_KWD_LENGTH_INTERFACE C_KWD_LENGTH (C_KWD_INTERFACE)
#define C_KWD_LENGTH_STRUCT C_KWD_LENGTH (C_KWD_STRUCT)
#define C_KWD_LENGTH_TEMPLATE C_KWD_LENGTH (C_KWD_TEMPLATE)
#define C_KWD_LENGTH_THROWS C_KWD_LENGTH (C_KWD_THROWS)
#define C_KWD_LENGTH_IMPLEMENTS C_KWD_LENGTH (C_KWD_IMPLEMENTS)
#define C_KWD_LENGTH_EXTENDS C_KWD_LENGTH (C_KWD_EXTENDS)
#define C_KWD_LENGTH_USING C_KWD_LENGTH (C_KWD_USING)

void
Buffer::skip_pure_white (Point &point) const
{
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  while (!eobp (point))
    {
      Char c = point.ch ();
      if (c == '\n' || syntax (tab, c) != SCwhite)
        break;
      forward_char (point, 1);
    }
}

int
Buffer::c_goto_if_directive (Point &point) const
{
  while (forward_line (point, -1))
    if (c_preprocessor_directive_p (point))
      {
        skip_pure_white (point);
        forward_char (point, 1);
        skip_pure_white (point);
        if (C_SYMBOL_MATCH_P (point, C_KWD_IF, 1))
          return 1;
        if (C_SYMBOL_MATCH_P (point, C_KWD_END, 1)
            && !C_SYMBOL_MATCH_P (point, C_KWD_ENDREGION, 1)
            && !c_goto_if_directive (point))
          return 0;
      }
  return 0;
}

int
Buffer::c_skip_white_backward (Point &point, int pure) const
{
  const int syntax_opt = xsyntax_table (lsyntax_table)->flags;
  const int opt_cpp = syntax_opt & SYNTAX_OPT_CPP;
  flag_fake_open_brace = 0;
  skip_over_newline = !eobp (point) && point.ch () == '\n';
  while (1)
    {
      int n = skip_white_backward (point, pure);
      if (n)
        return n;
      if (!skip_over_newline)
        return 0;

      Point opoint (point);

      goto_eol (point);
      forward_char (point, -1);
      if (point.ch () == '\\')
        {
          point = opoint;
          return 0;
        }

      flag_fake_open_brace = 0;
      goto_bol (point);
      if (opt_cpp && (c_preprocessor_directive_p (point) || csharp_region_directive_p (point, syntax_opt)))
        {
          skip_pure_white (point);
          forward_char (point, 1);
          skip_pure_white (point);
          if (C_SYMBOL_MATCH_P (point, C_KWD_EL, 1)
              && !c_goto_if_directive (point))
            return Cunmatch_directive;
          if (!forward_line (point, -1))
            return Sbob;
        }
      else
        {
          n = 0;
          while (forward_line (point, -1))
            {
              goto_eol (point);
              forward_char (point, -1);
              if (point.ch () != '\\')
                {
                  forward_char (point, 1);
                  forward_line (point, 1);
                  break;
                }
              n = 1;
            }
          if (!n)
            {
              point = opoint;
              return 0;
            }
          goto_bol (point);
          if (opt_cpp && !c_preprocessor_directive_p (point))
            {
              point = opoint;
              return 0;
            }
          if (!forward_char (point, -1))
            return Sbob;
        }
      goto_eol (point);
    }
}

int
Buffer::c_label_line_p (const Point &opoint) const
{
  if (eobp (opoint))
    return 0;
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  syntax_code c = syntax (tab, opoint);
  if (c != SCword && c != SCsymbol)
    return 0;

  if (C_SYMBOL_MATCH_P (opoint, C_KWD_CASE, 0))
    return 1;

  Point point (opoint);
  while (1)
    {
      forward_char (point, 1);
      if (eobp (point))
        break;
      c = syntax (tab, point);
      if (c != SCword && c != SCsymbol)
        break;
    }

  do
    {
      Char c = point.ch ();
      if (c == ':')
        {
          forward_char (point, 1);
          return eobp (point) || point.ch () != ':';
        }
      if (c == '\n' || syntax (tab, c) != SCwhite)
        break;
      forward_char (point, 1);
    }
  while (!eobp (point));
  return 0;
}

int
Buffer::first_char_p (const Point &opoint) const
{
  Point point (opoint);
  goto_bol (point);
  skip_pure_white (point);
  return point.p_point == opoint.p_point;
}

int
Buffer::c_goto_match_if (Point &point) const
{
  int n;
  int semi = 0;
  while (1)
    {
      if (!forward_char (point, -1))
        return Cunmatch_else;
      int osemi = skip_semi_colon;
      n = c_skip_white_backward (point, 0);
      if (n)
        break;
      if (osemi != skip_semi_colon)
        semi++;
      n = skip_sexp_backward (point);
      if (n)
        break;
      if (C_SYMBOL_MATCH_P (point, C_KWD_IF, 0))
        {
          semi--;
          if (semi < 0)
            return Cunmatch_else;
          if (semi > 0)
            continue;
          goto_bol (point);
          skip_pure_white (point);
          if (!eobp (point) && point.ch () == '{')
            {
              forward_char (point, 1);
              skip_pure_white (point);
            }
          return Csame;
        }
      if (C_SYMBOL_MATCH_P (point, C_KWD_ELSE, 0))
        {
          semi--;
          if (semi < 0)
            return Cunmatch_else;
          if (semi > 0)
            continue;
          n = c_goto_match_if (point);
          if (n != Csame)
            break;
        }
    }
  if (n == Send_sexp || n == Sbob)
    return Cunmatch_else;
  return n;
}

#if 0
static void
debug_print (const Buffer *bp, const char *s, const Point &p, int n)
{
  Point point (p);
  printf ("%s: ", s);
  for (int i = 0; i < n && !bp->eobp (point); i++, bp->forward_char (point, 1))
    putchar (point.ch ());
  printf ("\n");
  fflush (stdout);
}
#endif

int
Buffer::c_beginning_of_stmt (Point &point, int syntax_opt, Point *templ) const
{
  point_t opoint = point.p_point;
  Point stmt_start (point);
  int semi = skip_semi_colon;
  while (forward_char (point, -1)
         && !c_skip_white_backward (point, 0)
         && semi == skip_semi_colon
         && !skip_sexp_backward (point)
         && point.ch () != '}' && point.ch () != '{'
         && (syntax_opt & SYNTAX_OPT_JAVA
             || !first_char_p (point)
             || !c_label_line_p (point)))
    stmt_start = point;
  point = stmt_start;

  if (point.p_point <= point_t (opoint - C_KWD_LENGTH_TEMPLATE)
      && syntax_opt & SYNTAX_OPT_CPLUSPLUS
      && C_SYMBOL_MATCH_P (point, C_KWD_TEMPLATE, 0))
    {
      if (templ)
        *templ = point;
      forward_char (point, C_KWD_LENGTH_TEMPLATE);
      skip_gt = skip_gt_or_lt = 0;
      while (!skip_white_forward (point, 0)
             && skip_gt_or_lt > 0
             && point.p_point <= opoint
             && !skip_sexp_forward (point)
             && point.p_point <= opoint)
        ;
      return skip_gt && !skip_gt_or_lt;
    }
  return 0;
}

int
Buffer::c_class_decl_p (Point &point, const Point &opoint, int syntax_opt) const
{
#define DEFMODIFIER(X) {(X), sizeof (X) - 1}
  struct modifier {const char *kwd; int l;};
  static const modifier java_modifiers[] =
    {
      DEFMODIFIER ("public"),
      DEFMODIFIER ("abstruct"),
      DEFMODIFIER ("final"),
      {0},
    };
  static const modifier csharp_modifiers[] =
    {
      DEFMODIFIER ("public"),
      DEFMODIFIER ("protected"),
      DEFMODIFIER ("private"),
      DEFMODIFIER ("new"),
      DEFMODIFIER ("internal"),
      DEFMODIFIER ("abstract"),
      DEFMODIFIER ("sealed"),
      {0},
    };
  static const modifier cplusplus_cli_modifiers[] =
    {
      // C++/CLI
      // 12.4 Top-level type visibility
      DEFMODIFIER ("public"),
      DEFMODIFIER ("private"),
      // 19.1 Class definitions
      DEFMODIFIER ("ref"),
      DEFMODIFIER ("value"),
      DEFMODIFIER ("interface"),
      {0},
    };
  static const modifier c_modifiers[] =
    {
      DEFMODIFIER ("typedef"),
      DEFMODIFIER ("extern"),
      DEFMODIFIER ("static"),
      DEFMODIFIER ("const"),
      DEFMODIFIER ("volatile"),
      {0},
    };

  if (syntax_opt & (SYNTAX_OPT_CSHARP | SYNTAX_OPT_CPLUSPLUS_CLI)
      && point.ch () == '[')
    {
      skip_sexp_forward (point);
      skip_white_forward (point, 0);
    }

  const modifier *modifiers;
  if (syntax_opt & SYNTAX_OPT_JAVA)
    modifiers = java_modifiers;
  else if (syntax_opt & SYNTAX_OPT_CSHARP)
    modifiers = csharp_modifiers;
  else if (syntax_opt & SYNTAX_OPT_CPLUSPLUS_CLI)
    modifiers = cplusplus_cli_modifiers;
  else
    modifiers = c_modifiers;

  while (1)
    {
      if (point.p_point >= opoint.p_point)
        return 0;
      for (const modifier *p = modifiers;; p++)
        {
          if (!p->kwd)
            goto nomatch;
          if (symbol_match_p (point, p->kwd, p->l, 0))
            {
              forward_char (point, p->l);
              break;
            }
        }
      skip_pure_white (point);
    }
nomatch:

  if (C_SYMBOL_MATCH_P (point, C_KWD_CLASS, 0))
    return C_KWD_LENGTH_CLASS;
  if (C_SYMBOL_MATCH_P (point, C_KWD_INTERFACE, 0))
    return C_KWD_LENGTH_INTERFACE;
  if (syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_CSHARP)
      && C_SYMBOL_MATCH_P (point, C_KWD_STRUCT, 0))
    return C_KWD_LENGTH_STRUCT;
  if (syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_CSHARP)
      && C_SYMBOL_MATCH_P (point, C_KWD_ENUM, 0))
    return C_KWD_LENGTH_ENUM;
  return 0;
}

int
Buffer::c_in_class_p (const Point &opoint, int syntax_opt) const
{
  if (!(syntax_opt & (SYNTAX_OPT_CPLUSPLUS
                      | SYNTAX_OPT_JAVA
                      | SYNTAX_OPT_CSHARP)))
    return 0;

  Point point (opoint);
  if (bolp (point))
    forward_char (point, -1);
  if (up_down_list (point, -1, 0))
    return 0;
  c_beginning_of_stmt (point, syntax_opt);
  if (syntax_opt & SYNTAX_OPT_CPLUSPLUS
      && C_SYMBOL_MATCH_P (point, C_KWD_NAMESPACE, 0))
    return 1;
  return c_class_decl_p (point, opoint, syntax_opt);
}

int
Buffer::c_argdecl_p (Point &result_point, Point &colon_point,
                     const Point &start_point, const Point &curpos,
                     int in_class_p, int syntax_opt) const
{
  Point point (start_point);
  const syntax_table *tab = xsyntax_table (lsyntax_table);
  do
    {
      int semi = skip_semi_colon;
      c_skip_white_backward (point, 0);
      if (in_class_p && semi != skip_semi_colon)
        break;
      Char cc = point.ch ();
      if (cc == '}')
        break;
      Point brend (point);
      skip_sexp_backward (point);
      if (point.ch () == '{')
        break;
      if (cc == ')')
        {
          goto_bol (point);
          if (flag_pure && semi == skip_semi_colon)
            {
              if (in_class_p)
                skip_pure_white (point);
              Char cc = point.ch ();
              if (cc == '{')
                break;
              syntax_code c = syntax (tab, point);
              if (c == SCword || c == SCsymbol || (in_class_p && cc == '~'))
                {
                  Point beg (point);
                  if (in_class_p)
                    {
                      if (C_SYMBOL_MATCH_P (point, C_KWD_THROW, 0))
                        continue;
                      Point p (point);
                      forward_char (p, -1);
                      c_skip_white_backward (p, 1);
                      if (p.ch () == ',')
                        continue;
                    }
                  goto_bol (point);
                  if (!forward_char (point, -2) || point.ch () != '\\')
                    {
                      point = brend;
                      if (forward_line (point, 1) && point.p_point < curpos.p_point)
                        {
                          skip_pure_white (point);
                          int n = 0;
                          if (!eobp (point) && point.ch () == ':')
                            n = 1;
                          else if (syntax_opt & SYNTAX_OPT_JAVA
                                   && C_SYMBOL_MATCH_P (point, C_KWD_THROWS, 0))
                            n = C_KWD_LENGTH_THROWS;
                          if (n)
                            {
                              forward_char (point, n);
                              skip_pure_white (point);
                              colon_point = point;
                              result_point = beg;
                              return Cargdecl_colon;
                            }
                        }
                      result_point = beg;
                      if (syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_CSHARP))
                        return Cdefun;
                      return Cargdecl;
                    }
                  point = beg;
                }
            }
        }
    }
  while (forward_char (point, -1));
  return 0;
}

/*
CLASS:
  (abstract|public|final)* class NAME [extends SUPER] [implements INTERFACE, ...] {}
CONSTRUCTOR:
  [public|protected|private] NAME (ARGS...) [throws EXEPTION, ...] {}
ACCESS-METHOD:
  [public|protected|private|final|static|abstruct|native|synchronized] TYPE NAME (ARGS) [throws EXEPTION, ...] {}
STATIC-INITIALIZER:
  static {}
INTERFACE:
  (public|abstruct)* interface NAME [extends INTERFACE, ...] {}
 */

int
Buffer::c_check_class_decl (Point &result_point, Point &colon_point,
                            const Point &start_point,
                            int syntax_opt, Char cc) const
{
  if (!(syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_JAVA | SYNTAX_OPT_CSHARP)))
    return 0;

  Point point (start_point), templ;
  int has_templ = c_beginning_of_stmt (point, syntax_opt, &templ);
  if (point.p_point >= start_point.p_point)
    {
      if (has_templ)
        {
          point = templ;
          return Csame;
        }
      return 0;
    }

  Point stmt_beg (point);
  int class_len = c_class_decl_p (point, start_point, syntax_opt);
  if (!class_len)
    return 0;

  if (syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_CSHARP))
    {
      if (cc == ':')
        {
          result_point = stmt_beg;
          return Cdefclass;
        }
      if (cc != ',')
        {
          result_point = stmt_beg;
          goto_bol (result_point);
          skip_pure_white (result_point);
          return Cdefun;
        }
      forward_char (point, class_len);
      skip_pure_white (point);
      if (skip_white_forward (point, 0)
          || point.p_point >= start_point.p_point
          || skip_sexp_forward (point)
          || point.p_point >= start_point.p_point)
        return 0;
      skip_pure_white (point);
      if (point.p_point >= start_point.p_point || point.ch () != ':')
        return 0;
      forward_char (point, 1);
      skip_pure_white (point);
      if (eolp (point))
        {
          result_point = stmt_beg;
          return Cdefclass;
        }
      else
        {
          result_point = point;
          return Csame;
        }
    }
  else /*if (syntax_opt & SYNTAX_OPT_JAVA)*/
    {
      Point found;
      found.p_point = point_t (-1);
      int implements_found = 0;

      while (!skip_white_forward (point, 0)
             && point.p_point < start_point.p_point)
        {
          if (C_SYMBOL_MATCH_P (point, C_KWD_EXTENDS, 0))
            {
              found = point;
              forward_char (point, C_KWD_LENGTH_EXTENDS);
            }
          else if (C_SYMBOL_MATCH_P (point, C_KWD_IMPLEMENTS, 0))
            {
              found = point;
              forward_char (point, C_KWD_LENGTH_IMPLEMENTS);
              implements_found = 1;
              break;
            }
          else if (skip_sexp_forward (point))
            break;
        }
      if (found.p_point == point_t (-1))
        {
          result_point = stmt_beg;
          return Cdefclass;
        }

      point = found;
      skip_sexp_forward (point);
      skip_pure_white (point);
      if (eolp (point) && (point.p_point >= start_point.p_point
                           || cc == ','))
        {
          result_point = found;
          return Ccontinue;
        }

      if (cc != ',')
        {
          result_point = stmt_beg;
          if (implements_found)
            return Cdefun;
          colon_point = found;
          return Cargdecl_colon;
        }
      else
        {
          result_point = point;
          return Csame;
        }
    }

  return 0;
}

int
Buffer::c_check_throws (Point &stmt_beg, Point &throws,
                        const Point &start, int syntax_opt) const
{
  Point point (start);
  c_beginning_of_stmt (point, syntax_opt);
  stmt_beg = point;
  while (!skip_white_forward (point, 0)
         && point.p_point < start.p_point)
    {
      if (C_SYMBOL_MATCH_P (point, C_KWD_THROWS, 0))
        {
          forward_char (point, C_KWD_LENGTH_THROWS);
          skip_pure_white (point);
          throws = point;
          return 1;
        }
      else if (skip_sexp_forward (point))
        break;
    }
  return 0;
}

int
Buffer::c_check_extern_p (const Point &opoint) const
{
  Point point (opoint);
  return (forward_char (point, -1)
          && !skip_white_backward (point, 0)
          && point.ch () == '"'
          && !skip_sexp_backward (point)
          && forward_char (point, -1)
          && !skip_white_backward (point, 0)
          && !skip_sexp_backward (point)
          && C_SYMBOL_MATCH_P (point, C_KWD_EXTERN, 0));
}

int
Buffer::java_check_annotation_p (Point &point) const
{
  goto_bol (point);
  return (forward_char (point, -1)
          && !skip_white_backward (point, 0)
          && (point.ch () == ')' ?
              (!skip_sexp_backward (point)
               && forward_char (point, -1))
              : 1)
          && !skip_sexp_backward (point)
          && forward_char (point, -1)
          && (point.ch () == '@'));
}

int
Buffer::c_preprocessor_directive_p (const Point &opoint) const
{
  Point point (opoint);
  skip_pure_white (point);
  return (point.ch () == '#');
}

int
Buffer::csharp_region_directive_p (const Point &opoint, int syntax_opt) const
{
  if (!(syntax_opt & SYNTAX_OPT_CSHARP))
    return 0;

  Point point (opoint);

  skip_pure_white (point);
  if (point.ch () != '#')
    return 0;

  forward_char (point, 1);
  skip_pure_white (point);

  return (C_SYMBOL_MATCH_P (point, C_KWD_REGION, 1) ||
          C_SYMBOL_MATCH_P (point, C_KWD_ENDREGION, 1));
}

/*
using-statement:
  using   (    resource-acquisition   )    embedded-statement
resource-acquisition:
  local-variable-declaration
  expression
*/
int
Buffer::csharp_using_statement_p (const Point &opoint, int syntax_opt) const
{
  if (!(syntax_opt & SYNTAX_OPT_CSHARP))
    return 0;

  Point point (opoint);
  skip_pure_white (point);

  if (!C_SYMBOL_MATCH_P (point, C_KWD_USING, 0))
    return 0;

  forward_char (point, C_KWD_LENGTH_USING);
  skip_pure_white (point);
  if (point.ch () != '(')
    // Maybe using directive
    return 0;

  return 1;
}

int
Buffer::calc_c_indent (Point &point, Point &colon_point,
                       int syntax_opt) const
{
  const int opt_cpp = syntax_opt & SYNTAX_OPT_CPP ? FLAG_POUND : 0;
  goto_bol (point);
  const Point curpos (point);
  skip_pure_white (point);

  if (C_SYMBOL_MATCH_P (point, C_KWD_ELSE, 0))
    return c_goto_match_if (point);

  point = curpos;
  skip_semi_colon = 0;
  if (!forward_char (point, -1))
    return Sbob;

  int in_cpp = opt_cpp && !bobp (point) && point.prevch () == '\\';
  int result = c_skip_white_backward (point, FLAG_PURE);
  if (result)
    return result;

  Point tem;
  Point opos (point);
  const Char c = point.ch ();
  int term = c == ';' || c == '}' || c == ',';
  if (c == ';')
    forward_char (point, -1);
  else if (c == '}')
    skip_semi_colon--;

  int depth = 0;
  int nbraces = 0;

  while (1)
    {
      result = c_skip_white_backward (point, opt_cpp);
      if (result == Sbob)
        break;
      if (flag_fake_open_brace)
        {
          result = Send_sexp;
          goto fake_open_brace;
        }
      if (result)
        return result;

      if (point.ch () == '}')
        depth++;
      result = skip_sexp_backward (point);
      if (result == Sin_string)
        return result;

      if (point.ch () == '{')
        {
          nbraces++;
          if (!result)
            {
              tem = point;
              if (c == '}' && nbraces == 1)
                {
                  forward_char (point, -1);
                  c_skip_white_backward (point, 0);
                  goto_bol (point);
                  skip_pure_white (point);
                  if (!(syntax_opt & SYNTAX_OPT_JAVA)
                      && C_SYMBOL_MATCH_P (point, C_KWD_TYPEDEF, 0))
                    {
                      point = opos;
                      return Ccontinue;
                    }
                  point = tem;
                }
              goto_bol (point);
              skip_pure_white (point);
              if (bolp (point))
                {
                  if (depth != 1)
                    return Sunmatched_paren;
                  break;
                }
              point = tem;
            }

        fake_open_brace:
          if (!term && c != ':' && c != '{')
            {
              int status = Ccontinue;
              if (point_linenum (opos) == point_linenum (point))
                {
                  if (!flag_fake_open_brace)
                    goto_bol (point);
                }
              else
                {
                  if (result == Send_sexp && !flag_fake_open_brace
                      && !(syntax_opt & SYNTAX_OPT_JAVA))
                    for (int i = 0; i < 2; i++)
                      {
                        forward_char (point, -1);
                        skip_white_backward (point, FLAG_PURE);
                        skip_symbol_backward (point);
                        if (C_SYMBOL_MATCH_P (point, C_KWD_ENUM, 0))
                          {
                            status = Cenum;
                            break;
                          }
                      }

                  point = opos;
                  tem = opos;
                  do
                    {
                      c_skip_white_backward (point, 0);
                      if (point_linenum (tem) != point_linenum (point))
                        {
                          point = tem;
                          break;
                        }
                      skip_sexp_backward (point);
                      tem = point;
                    }
                  while (forward_char (point, -1));
                  goto_bol (point);
                }
              skip_pure_white (point);
              if (!result && bolp (point))
                break;

              if (status == Ccontinue)
                {
                  if (!in_cpp && c_in_class_p (curpos, syntax_opt))
                    {
                      result = c_argdecl_p (point, colon_point, opos,
                                            curpos, 1, syntax_opt);
                      if (result)
                        {
                          if (syntax_opt & SYNTAX_OPT_JAVA
                              && result == Cargdecl && c != ',')
                            {
                              tem = curpos;
                              if (forward_char (tem, -1)
                                  && !c_skip_white_backward (tem, 0)
                                  && !skip_sexp_backward (tem)
                                  && C_SYMBOL_MATCH_P (tem, C_KWD_THROWS, 0))
                                {
                                  forward_char (tem, C_KWD_LENGTH_THROWS);
                                  skip_pure_white (tem);
                                  point = tem;
                                  return Csame;
                                }
                              return Cdefun;
                            }
                          return result;
                        }
                    }
                  result = c_check_class_decl (point, colon_point,
                                               opos, syntax_opt, c);
                  if (result)
                    return result;
                  if (csharp_using_statement_p (point, syntax_opt))
                    return Csame;
                }
              return status;
            }

          if (result == Send_sexp)
            {
              if (c_check_extern_p (point))
                {
                  goto_bol (point);
                  return Csame;
                }
              int in_class = c_in_class_p (curpos, syntax_opt);
              if ((c == ','
                   || (c == ':'
                       && syntax_opt & (SYNTAX_OPT_CPLUSPLUS | SYNTAX_OPT_CSHARP)))
                  && in_class)
                {
                  if (c != ':')
                    {
                      if (syntax_opt & SYNTAX_OPT_JAVA
                          && c_check_throws (tem, point, curpos, syntax_opt))
                        return Csame;

                      result = c_argdecl_p (point, colon_point, opos,
                                            curpos, 1, syntax_opt);
                      if (result)
                        return result;
                    }
                  result = c_check_class_decl (point, colon_point,
                                               opos, syntax_opt, c);
                  if (result)
                    return result;
                }

              if (first_char_p (point) || flag_fake_open_brace)
                return Cindent;

              forward_char (point, -1);
              result = c_skip_white_backward (point, 0);
              if (result)
                return result;

              result = skip_sexp_backward (point);
              if (result && result != Send_sexp)
                return result;

              if (in_class)
                c_beginning_of_stmt (point, syntax_opt);
              else
                {
                  Point stmt_beg;
                  if (syntax_opt & SYNTAX_OPT_JAVA
                      && c_check_throws (stmt_beg, tem, point, syntax_opt))
                    point = stmt_beg;
                  else
                    {
                      goto_bol (point);
                      skip_pure_white (point);
                    }
                }
              return Cbrace;
            }
          else if (!result)
            {
              if (!depth--)
                {
                  while (1)
                    {
                      int semi = skip_semi_colon;
                      opos = point;
                      if (!forward_char (point, -1))
                        return Sbob;
                      result = c_skip_white_backward (point, 0);
                      if (result)
                        return result;
                      if (point.ch () != '}' && semi != skip_semi_colon)
                        {
                          point = opos;
                          return Csame;
                        }
                      result = skip_sexp_backward (point);
                      if (result)
                        return result == Send_sexp ? Cindent : result;
                    }
                }
            }
        }
      if (result)
        return result;

      if (c == ';' || c == '}')
        {
          if (C_SYMBOL_MATCH_P (point, C_KWD_DO, 0) && !--skip_semi_colon)
            {
              goto_bol (point);
              skip_pure_white (point);
              return Csame;
            }
          else if (C_SYMBOL_MATCH_P (point, C_KWD_ELSE, 0))
            skip_semi_colon--;
        }

      if (in_cpp && c_preprocessor_directive_p (point))
        {
          result = Send_sexp;
          goto fake_open_brace;
        }

      if (!forward_char (point, -1))
        break;
    }

  point = curpos;
  if (opt_cpp && forward_line (point, -1) && c_preprocessor_directive_p (point))
    {
      goto_eol (point);
      forward_char (point, -1);
      if (point.ch () == '\\')
        {
          goto_bol (point);
          return Ccontinue;
        }
    }

  result = c_argdecl_p (point, colon_point, opos,
                        curpos, 0, syntax_opt);
  if (result)
    return result;
  result = c_check_class_decl (point, colon_point,
                               opos, syntax_opt, c);
  if (result)
    return result;

  point = curpos;
  return /*c == '=' ? Ccontinue :*/ Csame;
}

lisp
Fcalc_c_indent ()
{
  enum
    {
      argdecl_indent,
      brace_imaginary_offset,
      brace_offset,
      comment_indent,
      continued_statement_offset,
      indent_level,
      label_offset,
      preprocessor_offset,
    };

  static const lisp *c_params[] =
    {
      &Vc_argdecl_indent,
      &Vc_brace_imaginary_offset,
      &Vc_brace_offset,
      &Vc_comment_indent,
      &Vc_continued_statement_offset,
      &Vc_indent_level,
      &Vc_label_offset,
      &Vc_preprocessor_offset,
    };
  static const lisp *cplusplus_params[] =
    {
      &Vcplusplus_argdecl_indent,
      &Vcplusplus_brace_imaginary_offset,
      &Vcplusplus_brace_offset,
      &Vcplusplus_comment_indent,
      &Vcplusplus_continued_statement_offset,
      &Vcplusplus_indent_level,
      &Vcplusplus_label_offset,
      &Vcplusplus_preprocessor_offset,
    };
  static const lisp *java_params[] =
    {
      &Vjava_argdecl_indent,
      &Vjava_brace_imaginary_offset,
      &Vjava_brace_offset,
      &Vjava_comment_indent,
      &Vjava_continued_statement_offset,
      &Vjava_indent_level,
      &Vjava_label_offset,
      &Vjava_preprocessor_offset,
    };
  static const lisp *csharp_params[] =
    {
      &Vcsharp_argdecl_indent,
      &Vcsharp_brace_imaginary_offset,
      &Vcsharp_brace_offset,
      &Vcsharp_comment_indent,
      &Vcsharp_continued_statement_offset,
      &Vcsharp_indent_level,
      &Vcsharp_label_offset,
      &Vcsharp_preprocessor_offset,
    };

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;

  const int syntax_opt = xsyntax_table (bp->lsyntax_table)->flags;
  Point point (wp->w_point), colon_point;
  int f = bp->calc_c_indent (point, colon_point, syntax_opt);
  if (f == Send_sexp)
    {
      bp->forward_char (point, 1);
      bp->skip_pure_white (point);
      f = Csame;

      if (bp->eobp (point) || point.ch () == '\n')
        {
          bp->goto_bol (point);
          bp->skip_pure_white (point);
          f = Cindent;
        }
    }
  else if (f == Ccontinue && syntax_opt & (SYNTAX_OPT_CSHARP | SYNTAX_OPT_CPLUSPLUS_CLI))
    {
      Point p (wp->w_point);
      bp->goto_bol (p);
      if (bp->forward_char (p, -1)
          && !bp->c_skip_white_backward (p, FLAG_PURE)
          && !bp->eobp (p) && p.ch () == ']'
          && !bp->skip_sexp_backward (p))
        {
          Point lbra (p);
          int r = (!bp->forward_char (p, -1)
                   ? Sbob : bp->c_skip_white_backward (p, FLAG_PURE));
          if (r == Sbob || (!r && (p.ch () == '{' || p.ch () == '}' || p.ch () == ']' || p.ch () == ';' ||
                                   ((syntax_opt & SYNTAX_OPT_CPLUSPLUS_CLI) && p.ch () == ':'))))
            {
              point = lbra;
              f = Csame;
            }
        }
    }
  else if (f == Ccontinue && syntax_opt & (SYNTAX_OPT_JAVA))
    {
      Point p (wp->w_point);
      if (bp->java_check_annotation_p (p))
        {
          point = p;
          f = Csame;
        }
    }

  long goal = bp->point_column (point);
  long column = goal;
  point = wp->w_point;

#define indent_param(X) symbol_value_as_integer (*params[(X)], bp)
#define indent_param_p(X) (symbol_value (*params[(X)], bp) != Qnil && \
                           symbol_value (*params[(X)], bp) != Qunbound)
  const lisp **const params = (syntax_opt & SYNTAX_OPT_CPLUSPLUS
                               ? cplusplus_params
                               : (syntax_opt & SYNTAX_OPT_JAVA
                                  ? java_params
                                  : (syntax_opt & SYNTAX_OPT_CSHARP
                                     ? csharp_params
                                     : c_params)));

  int indent = indent_param (indent_level);

  switch (f)
    {
    case Cbrace:
    case Cindent:
      if (!goal && !indent)
        goal = (indent_param (brace_offset)
                + indent_param (continued_statement_offset));
      else
        goal += indent;
      break;

    case Cdefclass:
    case Ccontinue:
    case Cenum:
      goal += indent_param (continued_statement_offset);
      break;

    case Csame:
      break;

    case Cargdecl_colon:
      goal = bp->point_column (colon_point);
      break;

    case Cdefun:
      if (goal)
        goal += indent_param (continued_statement_offset);
      break;

    case Cargdecl:
      goal += indent_param (argdecl_indent);
      break;

    case Sbob:
      goal = 0;
      break;

    case Sin_comment:
      bp->goto_eol (point);
      if (bp->first_char_p (point))
        {
          goal += indent_param (comment_indent);
          break;
        }
      /* fall thru... */
    case Sin_string:
      return Qt;

    case Sunbalanced_paren:
      format_message (Munbalanced_parenthesis);
      return Qnil;

    case Sunmatched_paren:
      format_message (Munmatched_parenthesis);
      return Qnil;

    case Cunmatch_else:
      format_message (Munbalanced_else);
      return Qnil;

    case Cunmatch_directive:
      format_message (Melse_not_within_a_conditional);
      return Qnil;

    default:
      return Qnil;
    }

  bp->goto_bol (point);
  bp->skip_pure_white (point);
  if (!bp->eobp (point))
    {
      switch (point.ch ())
        {
        case '}':
          if (f == Cenum)
            goal = column - indent;
          else
            goal -= indent;
          if (f == Cbrace || f == Cenum)
            goal += indent_param (brace_imaginary_offset);
          break;

        case '{':
          if (f == Cargdecl || f == Cargdecl_colon || f == Cdefclass)
            {
              goal = column;
              if (goal)
                goal += indent_param (continued_statement_offset);
            }
          if (f == Ccontinue || f == Cargdecl_colon
              || f == Cdefun || f == Cdefclass)
            goal += indent_param (brace_offset);
          break;

        case ':':
          if (f == Cdefun && syntax_opt & SYNTAX_OPT_CPLUSPLUS)
            goal = column + indent_param (argdecl_indent);
          break;

        case '#':
          if (syntax_opt & SYNTAX_OPT_CPP && !bp->csharp_region_directive_p (point, syntax_opt)) {
            if (indent_param_p (preprocessor_offset)) {
              goal += indent_param (preprocessor_offset);
            } else {
              bp->goto_bol (point);
              goal = 0;
            }
          }
          break;

        default:
          if (bp->c_label_line_p (point))
            goal += indent_param (label_offset);
          break;
        }
    }

  return make_fixnum (goal < 0 ? 0 : goal);
#undef indent_param
#undef indent_param_p
}

lisp
Frefresh_highlight_info (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  for (Chunk *cp = bp->b_chunkb; cp; cp = cp->c_next)
    cp->c_bstate = syntax_state::SS_INVALID;
  bp->refresh_buffer ();
  return Qt;
}

lisp
Fparse_point_syntax (lisp lpoint)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  no_restrictions nr (bp);
  syntax_info psi (bp,
                   symbol_value (Vparentheses_hash_table, bp),
                   symbol_value (Vhtml_highlight_mode, bp) != Qnil);

  u_char state[2];
  Point point (wp->w_point);
  if (lpoint && lpoint != Qnil)
    bp->goto_char (point, bp->coerce_to_point (lpoint));
  psi.point_syntax (point);
  state[0] = psi.si.ss_state;

  if (bp->eobp (point))
    state[1] = syntax_state::SS_INVALID;
  else
    {
      syntax_state::define_chunk (point.p_chunk);
      (psi.si.*syntax_state::update) (&point.ch ());
      state[1] = psi.si.ss_state;
    }

  switch (state[1])
    {
    case syntax_state::SS_STRING:
    case syntax_state::SS_ESC_STRING:
      switch (state[0])
        {
        case syntax_state::SS_STRING:
        case syntax_state::SS_ESC_STRING:
          return Kstring;

        case syntax_state::SS_TAG:
        case syntax_state::SS_TAG_FIRST:
        case syntax_state::SS_MCOMM2E:
          return Ktag;

        default:
          return Qnil;
        }
      break;

    case syntax_state::SS_SCOMMENT:
      return state[0] == syntax_state::SS_SCOMMENT ? Kcomment : Qnil;
      break;

    case syntax_state::SS_MCOMMENT:
    case syntax_state::SS_MCOMM1E:
    case syntax_state::SS_CPPCOMMENT:
    case syntax_state::SS_MCOMM2E:
      return Kcomment;

    case syntax_state::SS_MCOMM1S:
      if (state[0] == syntax_state::SS_TAG
          || state[0] == syntax_state::SS_TAG_FIRST
          || state[0] == syntax_state::SS_MCOMM2E)
        return Ktag;
      /* fall thru... */
    case syntax_state::SS_MCOMM1S_OR_CPPCOMM1:
    case syntax_state::SS_CPPCOMM1:
      return Qnil;

    case syntax_state::SS_NORMAL:
      switch (state[0])
        {
        case syntax_state::SS_STRING:
          return Kstring;

        case syntax_state::SS_SCOMMENT:
        case syntax_state::SS_MCOMM1E:
        case syntax_state::SS_CPPCOMMENT:
          return Kcomment;

        case syntax_state::SS_TAG:
        case syntax_state::SS_MCOMM2E:
          return Ktag;

        default:
          return Qnil;
        }
      break;

    case syntax_state::SS_TAG:
      switch (state[0])
        {
        case syntax_state::SS_STRING:
        case syntax_state::SS_ESC_STRING:
          return Kstring;

        default:
          return Ktag;
        }
      break;

    case syntax_state::SS_INVALID:
      switch (state[0])
        {
        case syntax_state::SS_SCOMMENT:
        case syntax_state::SS_CPPCOMMENT:
        case syntax_state::SS_MCOMMENT:
        case syntax_state::SS_MCOMM1E:
          return Kcomment;

        case syntax_state::SS_TAG:
        case syntax_state::SS_TAG_FIRST:
        case syntax_state::SS_MCOMM2E:
          return Ktag;

        case syntax_state::SS_STRING:
        case syntax_state::SS_ESC_STRING:
          return Kstring;

        default:
          return Qnil;
        }
      break;

    default:
      return Qnil;
    }
}


void
syntax_state::update_normal (const Char *p)
{
  assert (p >= ss_chunk->c_text);
  assert (p < ss_chunk->c_text + ss_chunk->c_used);

  Char c = *p;
  u_char sc, xcomm;
  if (SBCP (c))
    {
      sc = xchar_syntax (ss_tab, (u_char)c);
      xcomm = xchar_comment (ss_tab, (u_char)c);
    }
  else
    {
      sc = SCkanji;
      xcomm = 0;
    }

  switch (ss_state)
    {
    case SS_INVALID:
      ss_strch = 0;
    case SS_NORMAL:
    normal_char:
      if (xcomm & (SFcomment_start_first_char | SFcplusplus_comment_char))
        {
          if (!(xcomm & SFcplusplus_comment_char))
            ss_state = SS_MCOMM1S;
          else if (!(xcomm & SFcomment_start_first_char))
            ss_state = SS_CPPCOMM1;
          else
            ss_state = SS_MCOMM1S_OR_CPPCOMM1;
        }
      else
        switch (sc)
          {
          case SCstring:
            ss_state = SS_STRING;
            ss_strch = c;
            break;

          case SCcomment_start:
            ss_state = SS_SCOMMENT;
            break;

          case SCescape:
          case SCsymbol_prefix:
            ss_state = SS_ESC_NORMAL;
            break;

          default:
            ss_state = SS_NORMAL;
            break;
          }
      break;

    case SS_ESC_NORMAL:
      ss_state = SS_NORMAL;
      break;

    case SS_MCOMM1S_OR_CPPCOMM1:
      if (xcomm & SFcomment_start_second_char)
        ss_state = SS_MCOMMENT;
      else if (xcomm & SFcplusplus_comment_char)
        ss_state = SS_CPPCOMMENT;
      else
        goto normal_char;
      break;

    case SS_MCOMM1S:
      if (xcomm & SFcomment_start_second_char)
        ss_state = SS_MCOMMENT;
      else
        goto normal_char;
      break;

    case SS_CPPCOMM1:
      if (xcomm & SFcplusplus_comment_char)
        ss_state = SS_CPPCOMMENT;
      else
        goto normal_char;
      break;

    case SS_MCOMMENT:
      if (xcomm & SFcomment_end_first_char)
        ss_state = SS_MCOMM1E;
      break;

    case SS_MCOMM1E:
      if (xcomm & SFcomment_end_second_char)
        ss_state = SS_NORMAL;
      else if (!(xcomm & SFcomment_end_first_char))
        ss_state = SS_MCOMMENT;
      break;

    case SS_CPPCOMMENT:
      if (sc == SCcplusplus_comment_end)
        ss_state = SS_NORMAL;
      break;

    case SS_STRING:
      if (sc == SCstring && c == ss_strch)
        {
          ss_state = SS_NORMAL;
          ss_strch = 0;
        }
      else if (sc == SCescape)
        ss_state = SS_ESC_STRING;
      break;

    case SS_ESC_STRING:
      ss_state = SS_STRING;
      break;

    case SS_SCOMMENT:
      if (sc == SCcomment_end)
        ss_state = SS_NORMAL;
      break;
    }
}

void
syntax_state::update_column_comment (const Char *p)
{
  update_normal (p);
  if (p == ss_chunk->c_text)
    {
      Chunk *cp = ss_chunk->c_prev;
      if (cp && cp->c_text[cp->c_used - 1] != '\n')
        return;
    }
  else if (p[-1] != '\n')
    return;

  Point point;
  point.p_offset = p - ss_chunk->c_text;
  point.p_point = 0;
  point.p_chunk = ss_chunk;
  if (ss_bp->column_comment_p (ss_tab, point))
    ss_state = SS_SCOMMENT;
}

void
syntax_state::update_html (const Char *p)
{
  Char c = *p;
  u_char sc = SBCP (c) ? xchar_syntax (ss_tab, (u_char)c) : SCkanji;

  switch (ss_state)
    {
    case SS_INVALID:
      ss_state = SS_NORMAL;
      ss_strch = 0;
      /* fall thru... */
    case SS_NORMAL:
      if (sc == SCtag_start)
        ss_state = SS_TAG_FIRST;
      break;

    case SS_TAG_FIRST:
    case SS_MCOMM2E:
    normal_char:
      ss_state = SS_TAG;
      /* fall thru... */
    case SS_TAG:
      if (c == '-')
        ss_state = SS_MCOMM1S;
      else if (sc == SCstring)
        {
          ss_state = SS_STRING;
          ss_strch = c;
        }
      else if (sc == SCtag_end)
        ss_state = SS_NORMAL;
      break;

    case SS_STRING:
      if (sc == SCstring && c == ss_strch)
        {
          ss_state = SS_TAG;
          ss_strch = 0;
        }
      break;

    case SS_MCOMM1S:
      if (c == '-')
        ss_state = SS_MCOMMENT;
      else
        goto normal_char;
      break;

    case SS_MCOMMENT:
      if (c == '-')
        ss_state = SS_MCOMM1E;
      break;

    case SS_MCOMM1E:
      if (c == '-')
        ss_state = SS_MCOMM2E;
      else
        ss_state = SS_MCOMMENT;
      break;
    }
}

void
syntax_state::update_parentheses (const Char *p)
{
  Char c = *p;
  lChar c2;
  switch (ss_state)
    {
    case SS_INVALID:
      ss_strch = 0;
      ss_state = SS_NORMAL;
      /* fall thru... */
    case SS_NORMAL:
      c2 = Char_hash (c, ss_hashtab);
      if (c2 != lChar_EOF)
        {
          ss_state = SS_STRING;
          ss_strch = Char (c2);
        }
      break;

    case SS_STRING:
      if (c == ss_strch)
        {
          ss_state = SS_NORMAL;
          ss_strch = 0;
        }
      break;
    }
}

void
Buffer::invalidate_syntax () const
{
  for (Chunk *cp = b_chunkb; cp; cp = cp->c_next)
    cp->c_bstate = syntax_state::SS_INVALID;
  refresh_buffer ();
}

void
Chunk::parse_syntax ()
{
  syntax_state si;
  if (c_prev)
    {
      if (c_bstate == c_prev->c_estate
          && c_bstrch == c_prev->c_estrch)
        return;
      si.ss_state = c_prev->c_estate;
      si.ss_strch = c_prev->c_estrch;
    }
  else
    {
      if (c_bstate != syntax_state::SS_INVALID)
        return;
      si.ss_state = syntax_state::SS_NORMAL;
      si.ss_strch = 0;
    }
  c_bstate = si.ss_state;
  c_bstrch = si.ss_strch;
  syntax_state::define_chunk (this);
  for (const Char *p = c_text, *pe = p + c_used; p < pe; p++)
    (si.*syntax_state::update) (p);
  c_estate = si.ss_state;
  c_estrch = si.ss_strch;
}

void
Chunk::update_syntax (const syntax_info &psi)
{
  c_bstate = psi.bsi.ss_state;
  c_bstrch = psi.bsi.ss_strch;
  c_estate = psi.si.ss_state;
  c_estrch = psi.si.ss_strch;
}

syntax_info::syntax_info (Buffer *bp, lisp hashtab, int html)
     : point (0)
{
  syntax_state::ss_tab = xsyntax_table (bp->lsyntax_table);
  syntax_state::ss_bp = bp;
  if (!hash_table_p (hashtab))
    {
      if (html)
        {
          syntax_state::update = &syntax_state::update_html;
          syntax_state::ss_colors = syntax_state::ss_html_colors;
          syntax_state::ss_prev_colors = syntax_state::ss_html_prev_colors;
        }
      else
        {
          syntax_state::update = (syntax_state::ss_tab->comment_column >= 0
                                  ? &syntax_state::update_column_comment
                                  : &syntax_state::update_normal);
          syntax_state::ss_colors = syntax_state::ss_normal_colors;
          syntax_state::ss_prev_colors = syntax_state::ss_normal_prev_colors;
        }
      syntax_state::ss_hashtab = 0;
    }
  else
    {
      syntax_state::update = &syntax_state::update_parentheses;
      syntax_state::ss_colors = syntax_state::ss_normal_colors;
      syntax_state::ss_prev_colors = syntax_state::ss_normal_prev_colors;
      syntax_state::ss_hashtab = hashtab;
    }
}

void
syntax_info::point_syntax (const Point &goal)
{
  if (!goal.p_chunk)
    return;
  if (point > goal.p_point)
    {
      si.ss_state = syntax_state::SS_NORMAL;
      si.ss_strch = 0;
      bsi = si;
      point = 0;
    }
  if (goal.p_point == point)
    return;
  Point last (goal);
  no_restrictions nr (syntax_state::ss_bp);
  syntax_state::ss_bp->goto_char (last, point);
  if (last.p_chunk == goal.p_chunk)
    {
      bsi = si;
      syntax_state::define_chunk (last.p_chunk);
      for (const Char *p = last.p_chunk->c_text + last.p_offset,
           *pe = last.p_chunk->c_text + goal.p_offset; p < pe; p++)
        (si.*syntax_state::update) (p);
    }
  else
    {
      Chunk *cp = last.p_chunk;
      if (cp->c_prev
          ? (cp->c_bstate != cp->c_prev->c_estate
             || cp->c_estrch != cp->c_prev->c_estrch)
          : cp->c_bstate == syntax_state::SS_INVALID)
        {
          syntax_state::define_chunk (cp);
          for (const Char *p = cp->c_text + last.p_offset,
               *pe = cp->c_text + cp->c_used; p < pe; p++)
            (si.*syntax_state::update) (p);
          cp->update_syntax (*this);
        }
      cp = cp->c_next;
      for (; cp != goal.p_chunk; cp = cp->c_next)
        cp->parse_syntax ();
      si.ss_state = cp->c_prev->c_estate;
      si.ss_strch = cp->c_prev->c_estrch;
      bsi = si;
      syntax_state::define_chunk (cp);
      for (const Char *p = cp->c_text, *pe = p + goal.p_offset; p < pe; p++)
        (si.*syntax_state::update) (p);
    }
  point = goal.p_point;
}

u_int (*syntax_state::ss_colors)[SS_MAX];
u_int (*syntax_state::ss_prev_colors)[SS_MAX];
u_int syntax_state::ss_normal_colors[SS_MAX][SS_MAX];
u_int syntax_state::ss_normal_prev_colors[SS_MAX][SS_MAX];
u_int syntax_state::ss_html_colors[SS_MAX][SS_MAX];
u_int syntax_state::ss_html_prev_colors[SS_MAX][SS_MAX];
u_char syntax_state::ss_maybe_comment[SS_MAX];
lisp syntax_state::ss_hashtab;
const syntax_table *syntax_state::ss_tab;
Buffer *syntax_state::ss_bp;
Chunk *syntax_state::ss_chunk;
void (syntax_state::*syntax_state::update)(const Char *);

void
syntax_state::init_color_table ()
{
  int prev, cur, next;

  for (cur = SS_INVALID; cur < SS_MAX; cur++)
    {
      switch (cur)
        {
        case SS_MCOMMENT:
        case SS_MCOMM1E:
        case SS_CPPCOMMENT:
        case SS_SCOMMENT:
          next = GLYPH_COMMENT;
          break;

        case SS_STRING:
        case SS_ESC_STRING:
          next = GLYPH_STRING;
          break;

        default:
          next = KWD_OK;
          break;
        }
      for (prev = SS_INVALID; prev < SS_MAX; prev++)
        ss_normal_colors[prev][cur] = next;
    }

  ss_normal_colors[SS_MCOMM1E][SS_NORMAL] = GLYPH_COMMENT;

  for (prev = SS_INVALID; prev < SS_MAX; prev++)
    {
      switch (prev)
        {
        case SS_CPPCOMMENT:
        case SS_SCOMMENT:
          next = GLYPH_COMMENT;
          break;

        case SS_STRING:
          next = GLYPH_STRING;
          break;

        default:
          continue;
        }
      for (cur = SS_INVALID; cur < SS_MAX; cur++)
        ss_normal_colors[prev][cur] = next;
    }

  ss_normal_prev_colors[SS_MCOMM1S][SS_MCOMMENT] = GLYPH_COMMENT;
  ss_normal_prev_colors[SS_CPPCOMM1][SS_CPPCOMMENT] = GLYPH_COMMENT;
  ss_normal_prev_colors[SS_MCOMM1S_OR_CPPCOMM1][SS_MCOMMENT] = GLYPH_COMMENT;
  ss_normal_prev_colors[SS_MCOMM1S_OR_CPPCOMM1][SS_CPPCOMMENT] = GLYPH_COMMENT;

  for (cur = SS_INVALID; cur < SS_MAX; cur++)
    {
      switch (cur)
        {
        case SS_MCOMMENT:
        case SS_MCOMM1E:
        case SS_MCOMM2E:
          next = GLYPH_COMMENT;
          break;

        case SS_STRING:
          next = GLYPH_STRING | KWD2_OK;
          break;

        case SS_TAG:
        case SS_TAG_FIRST:
          next = GLYPH_TAG | KWD_OK | KWD2_OK;
          break;

        default:
          next = KWD2_OK;
          break;
        }
      for (prev = SS_INVALID; prev < SS_MAX; prev++)
        ss_html_colors[prev][cur] = next;
    }

  for (cur = SS_INVALID; cur < SS_MAX; cur++)
    if (cur != SS_MCOMM1S)
      {
        ss_html_colors[SS_TAG][cur] = u_int (GLYPH_TAG | KWD_OK | KWD2_OK);
        ss_html_colors[SS_TAG_FIRST][cur] = u_int (GLYPH_TAG | KWD_OK | KWD2_OK);
      }
  ss_html_colors[SS_MCOMM2E][SS_NORMAL] = u_int (GLYPH_TAG | KWD_OK | KWD2_OK);
  ss_html_colors[SS_MCOMM2E][SS_MCOMM1S] = u_int (GLYPH_TAG | KWD_OK | KWD2_OK);
  for (cur = SS_INVALID; cur < SS_MAX; cur++)
    ss_html_colors[SS_STRING][cur] = GLYPH_STRING | KWD2_OK;
  ss_html_colors[SS_TAG][SS_STRING] = GLYPH_STRING | KWD2_OK;
  ss_html_colors[SS_TAG_FIRST][SS_STRING] = GLYPH_STRING | KWD2_OK;

  ss_html_prev_colors[SS_MCOMM1S][SS_MCOMMENT] = GLYPH_COMMENT;

  ss_maybe_comment[SS_MCOMM1S] = 1;
  ss_maybe_comment[SS_CPPCOMM1] = 1;
  ss_maybe_comment[SS_MCOMM1S_OR_CPPCOMM1] = 1;
}
