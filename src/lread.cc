#include "stdafx.h"
#include "ed.h"
#include "StrBuf.h"

class Token: public StrBuf
{
  char buf[1018];
  int tk_escape;
  int tk_index;
public:
  int tk_colon;
  int tk_colon_index;

  Token ();
  void add_quote (lChar);
  void add_colon (lChar);
  void add (lChar);
  void quote ();
  void start ();
  lisp parse_integer (int);
  lisp parse_number ();
  static lisp parse_fraction (const Char *, int, int);
  int dot_token_p ();
};

inline
Token::Token ()
     : StrBuf (buf, sizeof buf), tk_escape (0), tk_index (0), tk_colon (0)
{
}

inline void
Token::quote ()
{
  tk_escape = 1;
}

inline void
Token::start ()
{
  empty ();
  tk_escape = 0;
  tk_index = 0;
  tk_colon = 0;
}

inline void
Token::add (lChar c)
{
  StrBuf::add (Char (c));
  tk_index++;
}

inline void
Token::add_quote (lChar c)
{
  add (c);
  quote ();
}

inline void
Token::add_colon (lChar c)
{
  switch (tk_colon)
    {
    case 0:
      tk_colon = 1;
      tk_colon_index = tk_index;
      break;

    case 1:
      if (tk_index == tk_colon_index + 1)
        tk_colon = 2;
      else
        tk_colon = -1;
      break;

    case 2:
      tk_colon = -1;
      break;

    default:
      break;
    }
  add (c);
}

lisp
Token::parse_fraction (const Char *p0, int l, int base)
{
  bignum_rep *r;
  Char *p = ato_bignum_rep (r, p0, l, base);
  lisp num = make_integer (r);
  l -= p - p0 + 1;
  return make_ratio (num, make_integer (ato_bignum_rep (p + 1, l, base)));
}

lisp
Token::parse_integer (int base)
{
  if (tk_escape || !sb_chunk)
    return 0;
  finish ();
  int l = length ();
  const Char *p;
  if (linear_p ())
    p = *this;
  else
    {
      Char *b = (Char *)alloca (sizeof (Char) * l);
      copy (b);
      p = b;
    }

  switch (parse_number_format (p, p + l, base))
    {
    case NF_INTEGER:
      return make_integer (ato_bignum_rep (p, l, base));

    case NF_FRACTION:
      return parse_fraction (p, l, base);

    default:
      return 0;
    }
}

static int
float_finite (float f)
{
  return _finite (f);
}

lisp
Token::parse_number ()
{
  if (tk_escape || !sb_chunk)
    return 0;
  finish ();
  int l = length ();
  const Char *p;
  if (linear_p ())
    p = *this;
  else
    {
      Char *b = (Char *)alloca (sizeof (Char) * l);
      copy (b);
      p = b;
    }

  int f = parse_number_format (p, p + l, 10);
  switch (f)
    {
    default:
      return 0;

    case NF_FLOAT_E:
      f = default_float_format ();
      /* fall thru... */
    case NF_FLOAT_S:
    case NF_FLOAT_F:
    case NF_FLOAT_D:
    case NF_FLOAT_L:
      {
        char *b0 = (char *)alloca (l + 1);
        char *b = b0, *be = b0 + l;
        while (b < be)
          {
            Char c = *p++;
            *b++ = alpha_char_p (c) ? 'e' : char (c);
          }
        *b = 0;
        errno = 0;
        double d = strtod (b0, 0);
        if (errno == ERANGE)
          return !d ? Qnil : Qt;
        if (f == NF_FLOAT_S || f == NF_FLOAT_F)
          {
            float f = float (d);
            if (!float_finite (f))
              return Qt;
            if (d && !f)
              return Qnil;
            return make_single_float (f);
          }
        else
          return make_double_float (d);
      }

    case NF_INTEGER:
    case NF_INTEGER_DOT:
      return make_integer (ato_bignum_rep (p, l, 10));

    case NF_FRACTION:
      return parse_fraction (p, l, 10);
    }
}

int
Token::dot_token_p ()
{
  if (tk_escape || !sb_chunk)
    return 0;
  finish ();
  for (const strbuf_chunk *cp = sb_chunk; cp; cp = cp->cdr)
    for (const Char *p = cp->contents, *pe = cp->used; p < pe; p++)
      if (*p != '.')
        return 0;
  return 1;
}

static void
clear_readtable (readtab_rep *r)
{
  readtab_rep *r0 = r;
  for (readtab_rep *const re = r + READTABLE_REP_SIZE; r < re; r++)
    if (r->disp)
      xfree (r->disp);
  xfree (r0);
}

lreadtable::~lreadtable ()
{
  if (rep)
    clear_readtable (rep);
}

static inline lreadtable *
make_readtable ()
{
  lreadtable *p = ldata <lreadtable, Treadtable>::lalloc ();
  p->rep = 0;
  p->rcase = RTC_PRESERVE;
  return p;
}

static readtab_rep *
alloc_readtab ()
{
  readtab_rep *const r =
    (readtab_rep *)xmalloc (sizeof (readtab_rep) * READTABLE_REP_SIZE);
  for (int i = 0; i < READTABLE_REP_SIZE; i++)
    {
      r[i].type = SCT_ILLEGAL;
      r[i].cfunc = 0;
      r[i].lfunc = Qunbound;
      r[i].disp = 0;
    }
  return r;
}

static disptab_rep *
alloc_disptab ()
{
  disptab_rep *const d =
    (disptab_rep *)xmalloc (sizeof (disptab_rep) * READTABLE_REP_SIZE);
  for (int i = 0; i < READTABLE_REP_SIZE; i++)
    {
      d[i].cfunc = 0;
      d[i].lfunc = Qunbound;
    }
  return d;
}

lisp
current_readtable ()
{
  lisp p = xsymbol_value (Vreadtable);
  if (!readtablep (p))
    {
      xsymbol_value (Vreadtable) = xsymbol_value (Vdefault_readtable);
      FEprogram_error (Ereadtable_value_is_not_readtable, p);
    }
  return p;
}

static lisp
reader_error1 (lisp stream, message_code e, lisp args, long linenum)
{
  lisp s = Qnil;
  if (streamp (stream))
    {
      if (file_stream_p (stream))
        s = xfile_stream_pathname (stream);
      else if (buffer_stream_p (stream))
        {
          lisp marker = xbuffer_stream_marker (stream);
          if (markerp (marker) && xmarker_buffer (marker))
            s = Fbuffer_name (xmarker_buffer (marker)->lbp);
        }
    }
  return FEreader_error (s, make_fixnum (linenum), make_message (e), args);
}

static inline lisp
reader_error (lisp stream, message_code e)
{
  return reader_error1 (stream, e, Qnil, stream_linenum (stream));
}

static inline lisp
reader_error (lisp stream, message_code e, lisp c)
{
  return reader_error1 (stream, e, xcons (c, Qnil), stream_linenum (stream));
}

static inline lisp
reader_error (lisp stream, message_code e, long linenum)
{
  return reader_error1 (stream, e, Qnil, linenum);
}

static inline int
read_suppress_p ()
{
  return xsymbol_value (Vread_suppress) != Qnil;
}

static inline int
read_eval_p ()
{
  return xsymbol_value (Vread_eval) != Qnil;
}

static void
multiple_escape (lisp stream, Token &token, lisp readtab)
{
  long olinenum = stream_linenum (stream);
  token.quote ();
  while (1)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        reader_error (stream, Eunexpected_eof, olinenum);

      switch (stdchar_type (xreadtable_rep (readtab), Char (c)))
        {
        case SCT_CONSTITUENT:
        case SCT_TERM_MACRO:
        case SCT_NON_TERM_MACRO:
        case SCT_WHITESPACE:
          token.add (c);
          break;

        case SCT_SINGLE_ESCAPE:
          c = readc_stream (stream);
          if (c == lChar_EOF)
            reader_error (stream, Eno_chars_following_single_escape);
          token.add (c);
          break;

        case SCT_MULTIPLE_ESCAPE:
          return;

        case SCT_ILLEGAL:
          reader_error (stream, Eillegal_character, make_char (Char (c)));
        }
    }
}

static inline lChar
translate (readtable_case rcase, lChar c)
{
  switch (rcase)
    {
    case RTC_UPCASE:
      return char_upcase (int (c));

    case RTC_DOWNCASE:
      return char_downcase (int (c));

    case RTC_PRESERVE:
    case RTC_INVERT:
    default:
      return c;
    }
}

static void
extended_token (lisp stream, Token &token, lisp readtab, lChar c)
{
  while (c != lChar_EOF)
    {
      switch (stdchar_type (xreadtable_rep (readtab), Char (c)))
        {
        case SCT_CONSTITUENT:
        case SCT_NON_TERM_MACRO:
          if (c == ':')
            token.add_colon (c);
          else
            token.add (translate (xreadtable_case (readtab), c));
          break;

        case SCT_SINGLE_ESCAPE:
          c = readc_stream (stream);
          if (c == lChar_EOF)
            reader_error (stream, Eno_chars_following_single_escape);
          token.add_quote (c);
          break;

        case SCT_MULTIPLE_ESCAPE:
          multiple_escape (stream, token, readtab);
          break;

        case SCT_ILLEGAL:
          reader_error (stream, Eillegal_character, make_char (Char (c)));

        case SCT_TERM_MACRO:
          unreadc_stream (c, stream);
          return;

        case SCT_WHITESPACE:
          if (xsymbol_value (Vreader_preserve_white) != Qnil)
            unreadc_stream (c, stream);
          return;
        }

      c = readc_stream (stream);
    }
}

static inline void
extended_token (lisp stream, Token &token, lisp readtab)
{
  extended_token (stream, token, readtab, readc_stream (stream));
}

static inline lisp
macro_reader (lisp stream, Char c, const readtab_rep *readtab)
{
  if (readtab[c].cfunc)
    return (*readtab[c].cfunc)(stream, c);
  if (readtab[c].lfunc == Qunbound)
    reader_error (stream, Eis_not_a_macro_char, make_char (c));
  lisp x = funcall_2 (readtab[c].lfunc, stream, make_char (c));
  if (!multiple_value::count ())
    x = 0;
  multiple_value::clear ();
  return x;
}

#define ACCEPT_EOF   1
#define ACCEPT_DOT   2

static lisp
lisp_parser (lisp stream, int flags = 0, lChar delim = lChar_EOF)
{
  Token token;
  while (1)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        {
          if (flags & ACCEPT_EOF)
            return Qeof;
          reader_error (stream, Eunexpected_eof);
        }
      if (c == delim)
        return Qclose_paren;

      lisp readtab = current_readtable ();
      switch (stdchar_type (xreadtable_rep (readtab), Char (c)))
        {
        default:
        case SCT_ILLEGAL:
          reader_error (stream, Eillegal_character, make_char (Char (c)));

        case SCT_WHITESPACE:
          break;

        case SCT_TERM_MACRO:
        case SCT_NON_TERM_MACRO:
          {
            lisp object = macro_reader (stream, Char (c), xreadtable_rep (readtab));
            if (object)
              return object;
            break;
          }

        case SCT_SINGLE_ESCAPE:
          c = readc_stream (stream);
          if (c == lChar_EOF)
            reader_error (stream, Eno_chars_following_single_escape);
          token.start ();
          token.add_quote (c);
          extended_token (stream, token, readtab);
          goto done;

        case SCT_CONSTITUENT:
          token.start ();
          if (c == ':')
            token.add_colon (c);
          else
            token.add (translate (xreadtable_case (readtab), c));
          extended_token (stream, token, readtab);
          goto done;

        case SCT_MULTIPLE_ESCAPE:
          token.start ();
          multiple_escape (stream, token, readtab);
          extended_token (stream, token, readtab);
          goto done;
        }
    }
done:
  if (read_suppress_p ())
    return Qnil;

  lisp x = token.parse_number ();
  if (x)
    {
      if (x == Qt)
        reader_error (stream, Efloating_point_overflow, token.make_string ());
      if (x == Qnil)
        reader_error (stream, Efloating_point_underflow, token.make_string ());
      return x;
    }

  if (token.dot_token_p ())
    {
      if (token.length () == 1 && flags & ACCEPT_DOT)
        return Qdot;
      reader_error (stream, Edot_only_token);
    }

  switch (token.tk_colon)
    {
    default:
      return reader_error (stream, Ebad_package_marker);

    case 0:
      return Fintern (token.make_string (), 0);

    case 1:
      {
        int l = token.length ();
        if (token.tk_colon_index == l - 1)
          reader_error (stream, Epackage_marker_appears_end_of_token);
        if (!token.tk_colon_index)
          return Fintern (token.make_substring (1, l), xsymbol_value (Vkeyword_package));
        lisp package = coerce_to_package (token.make_substring (0, token.tk_colon_index));
        lisp name = token.make_substring (token.tk_colon_index + 1, l);
        lisp symbol = Ffind_symbol (name, package);
        if (multiple_value::value (1) == Qnil)
          reader_error (stream, Eis_not_accessible_symbol, token.make_string ());
        if (multiple_value::value (1) != Kexternal)
          reader_error (stream, Eexternal_symbol_not_found, token.make_string ());
        return symbol;
      }

    case 2:
      {
        if (!token.tk_colon_index)
          reader_error (stream, Etwo_package_marker_appears_beginning_of_token);
        int l = token.length ();
        if (token.tk_colon_index == l - 2)
          reader_error (stream, Epackage_marker_appears_end_of_token);
        return Fintern (token.make_substring (token.tk_colon_index + 2, l),
                        coerce_to_package (token.make_substring (0, token.tk_colon_index)));
      }
    }
}

static lisp
read_delimited_list (lisp stream, Char delim, int &len)
{
  len = 0;
  lisp object = lisp_parser (stream, 0, delim);
  if (object == Qclose_paren)
    return Qnil;

  lisp list = xcons (object, Qnil);
  protect_gc gcpro (list);
  lisp p = list;
  while (1)
    {
      len++;
      object = lisp_parser (stream, 0, delim);
      if (object == Qclose_paren)
        break;
      xcdr (p) = xcons (object, Qnil);
      p = xcdr (p);
    }
  return list;
}

static int
read_escape_sequence (lisp stream, int val, int base, int nchars)
{
  int i;
  for (i = 0; i < nchars; i++)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        break;
      int n = digit_char (c);
      if (n >= base)
        {
          unreadc_stream (c, stream);
          break;
        }
      val = val * base + n;
    }
  if (!i && base == 16)
    reader_error (stream, Eno_following_hex_digits);
  return val;
}

static lisp
double_quote_reader (lisp stream, Char ch)
{
  Token token;
  token.start ();
  const readtab_rep *readtab = xreadtable_rep (current_readtable ());
  long olinenum = stream_linenum (stream);
  while (1)
    {
      lChar c = readc_stream (stream);
      if (c == ch)
        return token.make_string ();
      if (stdchar_single_escape_p (readtab, Char (c)))
        {
          c = readc_stream (stream);
          switch (c)
            {
            case '\n':
              continue;

            case 'a':
              c = CC_BEL;
              break;

            case 'b':
              c = CC_BS;
              break;

            case 'f':
              c = CC_FF;
              break;

            case 'n':
              c = CC_NL;
              break;

            case 'r':
              c = CC_CR;
              break;

            case 't':
              c = CC_HT;
              break;

            case 'v':
              c = CC_VT;
              break;

            case '?':
              c = CC_DEL;
              break;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
              c = read_escape_sequence (stream, c - '0', 8, 2);
              break;

            case 'x':
              c = read_escape_sequence (stream, 0, 16, 2);
              break;

            case 'X':
              c = read_escape_sequence (stream, 0, 16, 4);
              break;

            default:
              break;
            }
        }
      if (c == lChar_EOF)
        reader_error (stream, Eunterminated_string, olinenum);
      token.add (c);
    }
}

static inline lisp
ellipsis (lisp stream, lisp object)
{
  return xcons (object, xcons (lisp_parser (stream), Qnil));
}

static lisp
single_quote_reader (lisp stream, Char)
{
  return ellipsis (stream, Qquote);
}

static lisp
open_paren_reader (lisp stream, Char)
{
  lisp object = lisp_parser (stream, 0, ')');
  if (object == Qclose_paren)
    return Qnil;
  lisp list = xcons (object, Qnil);
  protect_gc gcpro (list);
  lisp p = list;
  while (1)
    {
      object = lisp_parser (stream, ACCEPT_DOT, ')');
      if (object == Qclose_paren)
        break;
      if (object == Qdot)
        {
          xcdr (p) = lisp_parser (stream);
          if (lisp_parser (stream, 0, ')') != Qclose_paren)
            reader_error (stream, Einvalid_dotted_list);
          break;
        }
      xcdr (p) = xcons (object, Qnil);
      p = xcdr (p);
    }
  return list;
}

static lisp
close_paren_reader (lisp stream, Char c)
{
  return reader_error (stream, Eunexpected_char, make_char (Char (c)));
}

static lisp
semicolon_reader (lisp stream, Char)
{
  lChar c;
  do
    c = readc_stream (stream);
  while (c != '\n' && c != lChar_EOF);
  return 0;
}

static lisp
comma_reader (lisp stream, Char)
{
  int ok = xsymbol_value (Vreader_in_backquote) != Qnil;
  lChar c = peekc_stream (stream);
  if (c == '@')
    {
      readc_stream (stream);
      if (!ok)
        reader_error (stream, Ecomma_atsign_is_not_in_backquote);
      return ellipsis (stream, Qcomma_atsign);
    }
  if (c == '.')
    {
      readc_stream (stream);
      if (!ok)
        reader_error (stream, Ecomma_dot_is_not_in_backquote);
      return ellipsis (stream, Qcomma_dot);
    }
  if (!ok)
    reader_error (stream, Ecomma_is_not_in_backquote);
  return ellipsis (stream, Qcomma);
}

static lisp
backquote_reader (lisp stream, Char)
{
  int in = xsymbol_value (Vreader_in_backquote) != Qnil;
  dynamic_bind dynb (Vreader_in_backquote, Qt);
  lisp form = ellipsis (stream, Qbackquote);
  protect_gc gcpro (form);
  if (!in)
    form = Feval (Fsi_bq_completely_process (form));
  return form;
}

struct dispmacro_param
{
  int value;
  int present;
};

static lisp
dispmacro_reader (lisp stream, Char ch)
{
  if (ch >= READTABLE_REP_SIZE)
    reader_error (stream, Emacro_char_out_of_range, make_char (ch));

  const readtab_rep *readtab = xreadtable_rep (current_readtable ());
  disptab_rep *disptab = readtab[ch].disp;
  if (!disptab)
    reader_error (stream, Eis_not_a_dispmacro_char, make_char (ch));

  dispmacro_param param;
  lChar c = readc_stream (stream);
  if (c >= '0' && c <= '9')
    {
      param.present = 1;
      param.value = c - '0';
      while (1)
        {
          c = readc_stream (stream);
          if (c < '0' || c > '9')
            break;
          param.value = param.value * 10 + c - '0';
        }
    }
  else
    param.present = 0;

  if (c == lChar_EOF)
    reader_error (stream, Eincomplete_dispatch_macro);
  if (c >= READTABLE_REP_SIZE)
    reader_error (stream, Edispmacro_sub_char_out_of_range, make_char (Char (c)));

  c = char_upcase (Char (c));
  if (disptab[c].cfunc)
    return (*disptab[c].cfunc)(stream, Char (c), param);
  if (disptab[c].lfunc == Qunbound)
    reader_error (stream, Eis_not_a_dispmacro_sub_char, make_char (Char (c)));
  lisp x = funcall_3 (disptab[c].lfunc, stream, make_char (Char (c)),
                      param.present ? make_fixnum (param.value) : Qnil);
  if (!multiple_value::count ())
    x = 0;
  multiple_value::clear ();
  return x;
}

static Char
parse_digit_char (lisp stream, Token &token, const Char *p, const Char *pe)
{
  int base;
  if (*p == 'x' || *p == 'X')
    {
      base = 16;
      p++;
    }
  else
    base = 8;

  int val = 0;
  for (; p < pe; p++)
    {
      int n = digit_char (*p);
      if (n >= base)
        {
          lisp name = token.make_string ();
          lisp hash = xsymbol_value (Vsi_character_name_hash_table);
          if (hash_table_p (hash))
            {
              lisp c = gethash (name, hash, Qnil);
              if (charp (c))
                return xchar_code (c);
            }
          reader_error (stream, Eunknown_char_name, name);
        }
      val = val * base + n;
      if (val >= CHAR_LIMIT)
        reader_error (stream, Echar_code_out_of_range, token.make_string ());
    }
  return val;
}

static lisp
number_backslash_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);

  Token token;
  token.start ();
  extended_token (stream, token, current_readtable (), '\\');

  if (read_suppress_p ())
    return Qnil;

  int l = token.length ();
  const Char *p = token;

  int ctl = 0, meta = 0, shift = 0;

  while (1)
    {
      int xl;
      Char bit = char_bit_name2Char (p, l, xl);
      if (!bit)
        break;
      switch (bit)
        {
        case CCF_CTRL_BIT:
          ctl = 1;
          break;

        case CCF_META:
          meta = 1;
          break;

        case CCF_SHIFT_BIT:
          shift = 1;
          break;

        default:
          assert (0);
          break;
        }
      p += xl;
      l -= xl;
    }

  if (!l)
    reader_error (stream, Einvalid_char_code_syntax, token.make_string ());

  Char c = function_char_name2Char (p, l);
  if (c != Char (-1))
    {
      if (ctl)
        c |= CCF_CTRL_BIT;
      if (shift)
        c |= CCF_SHIFT_BIT;
      if (meta)
        c = function_to_meta_function (c);
    }
  else if (l == 1 && ctl && *p < 128 && pseudo_char2ctl_table[*p])
    {
      if (shift)
        reader_error (stream, Ecannot_specify_shift_bit, token.make_string ());
      c = CCF_CHAR_MIN + pseudo_char2ctl_table[*p];
      if (meta)
        c = function_to_meta_function (c);
    }
  else
    {
      c = standard_char_name2Char (p, l);
      if (c == Char (-1))
        c = l == 1 ? *p : parse_digit_char (stream, token, p, p + l);

      if (shift)
        reader_error (stream, Ecannot_specify_shift_bit, token.make_string ());

      if (ctl)
        {
          if (c == '?')
            c = CC_DEL;
          else
            {
              c = char_upcase (c) - '@';
              if (c > ' ')
                reader_error (stream, Echar_code_out_of_range, token.make_string ());
            }
        }

      if (meta && ascii_char_p (c))
        c = char_to_meta_char (c);
    }

  return make_char (c);
}

static lisp
number_single_quote_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  return ellipsis (stream, Qfunction);
}

static lisp
number_open_paren_reader (lisp stream, Char, dispmacro_param &param)
{
  int l;
  lisp list = read_delimited_list (stream, ')', l);
  if (!l)
    {
      if (!read_suppress_p () && param.present && param.value)
        reader_error (stream, Eno_initializer_for_vector);
      return alloc_vector (0);
    }
  else
    {
      if (read_suppress_p () || !param.present)
        param.value = l;
      else if (param.value < l)
        reader_error (stream, Etoo_many_initializer_for_vector);
      return make_vector_from_list (list, param.value);
    }
}

static lisp
number_colon_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  Token token;
  token.start ();
  extended_token (stream, token, current_readtable ());
  if (read_suppress_p ())
    return Qnil;
  return Fmake_symbol (token.make_string ());
}

static lisp
number_dot_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_eval_p ())
    reader_error (stream, Enot_allow_evaluation);
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  lisp object = lisp_parser (stream);
  if (read_suppress_p ())
    return Qnil;
  object = Feval (object);
  multiple_value::clear ();
  return object;
}

static lisp
fixnum_reader (lisp stream, int base)
{
  Token token;
  token.start ();
  extended_token (stream, token, current_readtable ());
  if (read_suppress_p ())
    return Qnil;
  lisp x = token.parse_integer (base);
  if (!x)
    reader_error (stream, Ewrong_integer_format, token.make_string ());
  return x;
}

static lisp
number_B_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  return fixnum_reader (stream, 2);
}

static lisp
number_O_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  return fixnum_reader (stream, 8);
}

static lisp
number_X_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  return fixnum_reader (stream, 16);
}

static lisp
number_R_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && (!param.present || param.value < 2 || param.value > 36))
    reader_error (stream, Enumber_R_parameter_out_of_range);
  return fixnum_reader (stream, param.value);
}

static lisp
number_S_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);

  lisp object = lisp_parser (stream);
  if (read_suppress_p ())
    return Qnil;

  if (!consp (object))
    reader_error (stream, Einvalid_struct_syntax, object);

  lisp name = xcar (object);
  object = xcdr (object);
  int l = xlist_length (object);
  if (l & 1)
    reader_error (stream, Einvalid_struct_syntax, object);
  for (lisp p = object; consp (p); p = xcdr (xcdr (p)))
    {
      if (!symbolp (xcar (p)))
        reader_error (stream, Einvalid_slot_name, xcar (p));
      xcar (p) = Fintern (xsymbol_name (xcar (p)), xsymbol_value (Vkeyword_package));
    }

  if (name == Qrandom_state)
    return make_random_state (object);
  return funcall_2 (Ssi_structure_reader, name, object);
}

static lisp
number_C_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);

  lisp object = lisp_parser (stream);
  if (read_suppress_p ())
    return Qnil;

  if (xlist_length (object) != 2
      || !numberp (xcar (object))
      || !numberp (xcar (xcdr (object)))
      || xcdr (xcdr (object)) != Qnil)
    reader_error (stream, Einvalid_complex_syntax, object);

  return make_complex (xcar (object), xcar (xcdr (object)));
}

static void
count_dimensions (lisp stream, lisp object, int *dims, int rank)
{
  if (rank < 0)
    return;
  if (object != Qnil && !consp (object))
    reader_error (stream, Eno_initializer_for_array);

  int l = 0;
  for (; consp (object); object = xcdr (object), l++)
    count_dimensions (stream, xcar (object), dims, rank - 1);

  if (dims[rank] == -1)
    dims[rank] = l;
  else if (dims[rank] != l)
    reader_error (stream, Earray_initializer_count_mismatch);
}

static lisp *
store_array_initializer (lisp *b, lisp object, int rank)
{
  if (rank < 0)
    *b++ = object;
  else
    for (; consp (object); object = xcdr (object))
      b = store_array_initializer (b, xcar (object), rank - 1);
  return b;
}

static lisp
number_A_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && !param.present)
    reader_error (stream, Eno_rank_is_specified);

  lisp object = lisp_parser (stream);
  if (read_suppress_p ())
    return Qnil;

  int rank = param.value;
  if (rank < 0 || rank >= ARRAY_RANK_LIMIT)
    reader_error (stream, Earray_rank_too_large);

  int *dims = (int *)alloca (sizeof (int) * rank);
  for (int i = 0; i < rank; i++)
    dims[i] = -1;

  count_dimensions (stream, object, dims, rank - 1);

  lisp d = Qnil;
  for (int i = 0; i < rank; i++)
    d = xcons (make_fixnum (dims[i]), d);

  lisp array = Fsi_make_array (d, Qt, Qnil, Qnil, Qnil, Qnil);

  store_array_initializer ((lisp *)xbase_vector_contents (array),
                           object, rank - 1);

  return array;
}

#define UNREF_LABEL 0
#define REF_LABEL 1
#define MAKE_LABEL() make_long_int (UNREF_LABEL)
#define REF_LABEL_P(X) (xlong_int_value (X) == REF_LABEL)
#define UNREF_LABEL_P(X) \
  (long_int_p (X) && xlong_int_value (X) == UNREF_LABEL)
#define MAKE_LABEL_REF(X) (xlong_int_value (X) = REF_LABEL)

#define RECURSIVE_MARK 0

class patch_save
{
  void **place;
  void *value;
public:
  patch_save (void **p, void *v) : place (p), value (v) {*place = RECURSIVE_MARK;}
  ~patch_save () { *place = value; }
};

/*GENERIC_FUNCTION*/
static void
patch_number_equal (lisp p, lisp label, lisp object)
{
  if (immediatep (p))
    return;
  switch (object_typeof (p))
    {
    case Tcons:
      if (xcar (p) != RECURSIVE_MARK)
        {
          lisp a = xcar (p);
          lisp d = xcdr (p);
          patch_save save ((void **)&xcar (p), a == label ? object : a);
          if (a != label)
            patch_number_equal (a, label, object);
          if (d != label)
            patch_number_equal (d, label, object);
          else
            xcdr (p) = object;
        }
      break;

    case Tsimple_vector:
    case Tcomplex_vector:
    case Tarray:
      if (xbase_vector_contents (p) != RECURSIVE_MARK)
        {
          lisp *v0 = xvector_contents (p);
          patch_save save (&xbase_vector_contents (p), v0);
          for (lisp *v = v0, *ve = v0 + xvector_length (p); v < ve; v++)
            if (*v == label)
              *v = object;
            else
              patch_number_equal (*v, label, object);
        }
      break;
    }
}

static lisp
number_equal_reader (lisp stream, Char, dispmacro_param &param)
{
  if (read_suppress_p ())
    return Qnil;
  if (!param.present)
    reader_error (stream, Eno_label_for_number_equal);
  if (Fassoc (make_fixnum (param.value),
              xsymbol_value (Vreader_label_alist), Qnil) != Qnil)
    reader_error (stream, Elabel_number_redefined, make_fixnum (param.value));
  lisp label = MAKE_LABEL ();
  lisp x = xcons (make_fixnum (param.value), label);
  xsymbol_value (Vreader_label_alist) = xcons (x, xsymbol_value (Vreader_label_alist));
  lisp object = lisp_parser (stream);
  if (object == label)
    reader_error (stream, Elabel_reference_self, make_fixnum (param.value));
  xcdr (x) = object;
  if (REF_LABEL_P (label))
    patch_number_equal (object, label, object);
  return object;
}

static lisp
number_number_reader (lisp stream, Char, dispmacro_param &param)
{
  if (read_suppress_p ())
    return Qnil;
  if (!param.present)
    reader_error (stream, Eno_label_for_number_number);
  lisp x = Fassoc (make_fixnum (param.value), xsymbol_value (Vreader_label_alist), Qnil);
  if (x == Qnil)
    reader_error (stream, Eundefined_label, make_fixnum (param.value));
  assert (consp (x));
  x = xcdr (x);
  if (UNREF_LABEL_P (x))
    MAKE_LABEL_REF (x);
  return x;
}

static lisp
reader_featurep (lisp stream, lisp feature, lisp cond)
{
  if (symbolp (feature))
    return memq (feature, xsymbol_value (Vfeatures)) ? Qt : Qnil;

  QUIT;
  if (consp (feature))
    {
      lisp f = xcar (feature);
      feature = xcdr (feature);
      if (f == Knot)
        {
          if (!consp (feature))
            reader_error (stream, Etoo_few_arguments);
          return boole (reader_featurep (stream, xcar (feature), cond) == Qnil);
        }
      if (f == Kand)
        {
          for (; consp (feature); feature = xcdr (feature))
            if (reader_featurep (stream, xcar (feature), cond) == Qnil)
              return Qnil;
          return Qt;
        }
      if (f == Kor)
        {
          for (; consp (feature); feature = xcdr (feature))
            if (reader_featurep (stream, xcar (feature), cond) != Qnil)
              return Qt;
          return Qnil;
        }
    }

  if (cond == Qt)
    reader_error (stream, Ewrong_argument_for_number_plus, feature);
  else
    reader_error (stream, Ewrong_argument_for_number_minus, feature);
  return Qnil;
}

static lisp
reader_featurep (lisp stream, dispmacro_param &param, lisp cond)
{
  if (param.present)
    reader_error (stream, Einvalid_midfix_parameter);

  lisp feature;
  {
    dynamic_bind dynb1 (Vpackage, xsymbol_value (Vkeyword_package));
    dynamic_bind dynb2 (Vread_suppress, Qnil);
    feature = lisp_parser (stream);
  }

  if (reader_featurep (stream, feature, cond) == cond)
    return lisp_parser (stream);

  dynamic_bind dynb (Vread_suppress, Qt);
  lisp_parser (stream);
  return 0;
}

static lisp
number_plus_reader (lisp stream, Char, dispmacro_param &param)
{
  return reader_featurep (stream, param, Qt);
}

static lisp
number_minus_reader (lisp stream, Char, dispmacro_param &param)
{
  return reader_featurep (stream, param, Qnil);
}

static lisp
number_bar_reader (lisp stream, Char, dispmacro_param &param)
{
  if (!read_suppress_p () && param.present)
    reader_error (stream, Einvalid_midfix_parameter);
  long olinenum = stream_linenum (stream);
  int depth = 1;
  while (1)
    {
      lChar c = readc_stream (stream);
      switch (c)
        {
        case lChar_EOF:
          reader_error (stream, Eunterminated_comment, olinenum);

        case '|':
          if (peekc_stream (stream) == '#')
            {
              readc_stream (stream);
              if (!--depth)
                return 0;
            }
          break;

        case '#':
          if (peekc_stream (stream) == '|')
            {
              readc_stream (stream);
              depth++;
            }
          break;

        default:
          break;
        }
    }
}


static lisp
call_macro_reader (reader_macro_function fn, lisp stream, lisp cc)
{
  check_char (cc);
  lisp x = (*fn)(stream, xchar_code (cc));
  if (x)
    return x;
  multiple_value::count () = 0;
  return Qnil;
}

static lisp
call_dispmacro_reader (reader_dispmacro_function fn, lisp stream, lisp cc, lisp lparam)
{
  check_char (cc);
  dispmacro_param param;
  if (lparam == Qnil)
    param.present = 0;
  else
    {
      param.present = 1;
      param.value = fixnum_value (lparam);
    }
  lisp x = (*fn)(stream, xchar_code (cc), param);
  if (x)
    return x;
  multiple_value::count () = 0;
  return Qnil;
}

#define DEFUN_MACRO_READER(fn) \
  lisp CONCAT (Fsi_, fn) (lisp stream, lisp cc) \
  {return call_macro_reader (fn, stream, cc);}
#define DEFUN_DISPMACRO_READER(fn) \
  lisp CONCAT (Fsi_, fn) (lisp stream, lisp cc, lisp param) \
  {return call_dispmacro_reader (fn, stream, cc, param);}

DEFUN_MACRO_READER (double_quote_reader)
DEFUN_MACRO_READER (single_quote_reader)
DEFUN_MACRO_READER (open_paren_reader)
DEFUN_MACRO_READER (close_paren_reader)
DEFUN_MACRO_READER (semicolon_reader)
DEFUN_MACRO_READER (comma_reader)
DEFUN_MACRO_READER (backquote_reader)
DEFUN_MACRO_READER (dispmacro_reader)

DEFUN_DISPMACRO_READER (number_backslash_reader)
DEFUN_DISPMACRO_READER (number_single_quote_reader)
DEFUN_DISPMACRO_READER (number_open_paren_reader)
DEFUN_DISPMACRO_READER (number_colon_reader)
DEFUN_DISPMACRO_READER (number_dot_reader)
DEFUN_DISPMACRO_READER (number_B_reader)
DEFUN_DISPMACRO_READER (number_O_reader)
DEFUN_DISPMACRO_READER (number_X_reader)
DEFUN_DISPMACRO_READER (number_R_reader)
DEFUN_DISPMACRO_READER (number_S_reader)
DEFUN_DISPMACRO_READER (number_C_reader)
DEFUN_DISPMACRO_READER (number_A_reader)
DEFUN_DISPMACRO_READER (number_equal_reader)
DEFUN_DISPMACRO_READER (number_number_reader)
DEFUN_DISPMACRO_READER (number_plus_reader)
DEFUN_DISPMACRO_READER (number_minus_reader)
DEFUN_DISPMACRO_READER (number_bar_reader)

reader_dispmacro_function
get_reader_dispmacro_function (lisp fn)
{
  static const struct {reader_dispmacro_function f; lisp *s;} x[] =
    {
      {number_backslash_reader, &Ssi_number_backslash_reader},
      {number_single_quote_reader, &Ssi_number_single_quote_reader},
      {number_open_paren_reader, &Ssi_number_open_paren_reader},
      {number_colon_reader, &Ssi_number_colon_reader},
      {number_dot_reader, &Ssi_number_dot_reader},
      {number_B_reader, &Ssi_number_B_reader},
      {number_O_reader, &Ssi_number_O_reader},
      {number_X_reader, &Ssi_number_X_reader},
      {number_R_reader, &Ssi_number_R_reader},
      {number_S_reader, &Ssi_number_S_reader},
      {number_C_reader, &Ssi_number_C_reader},
      {number_A_reader, &Ssi_number_A_reader},
      {number_equal_reader, &Ssi_number_equal_reader},
      {number_number_reader, &Ssi_number_number_reader},
      {number_plus_reader, &Ssi_number_plus_reader},
      {number_minus_reader, &Ssi_number_minus_reader},
      {number_bar_reader, &Ssi_number_bar_reader},
    };
  for (int i = 0; i < numberof (x); i++)
    if (fn == *x[i].s)
      return x[i].f;
  return 0;
}

reader_macro_function
get_reader_macro_function (lisp fn)
{
  static const struct {reader_macro_function f; lisp *s;} x[] =
    {
      {double_quote_reader, &Ssi_double_quote_reader},
      {single_quote_reader, &Ssi_single_quote_reader},
      {open_paren_reader, &Ssi_open_paren_reader},
      {close_paren_reader, &Ssi_close_paren_reader},
      {semicolon_reader, &Ssi_semicolon_reader},
      {comma_reader, &Ssi_comma_reader},
      {backquote_reader, &Ssi_backquote_reader},
      {dispmacro_reader, &Ssi_dispmacro_reader},
    };
  for (int i = 0; i < numberof (x); i++)
    if (fn == *x[i].s)
      return x[i].f;
  return 0;
}

#define SET_MACRO(R, CH, FN, TYPE) \
  ((R)[(CH)].type = (TYPE), \
   (R)[(CH)].cfunc = (FN), \
   (R)[(CH)].lfunc = CONCAT (Ssi_, FN))

#define SET_TERM_MACRO(R, CH, FN) SET_MACRO (R, CH, FN, SCT_TERM_MACRO)
#define SET_NONTERM_MACRO(R, CH, FN) SET_MACRO (R, CH, FN, SCT_NON_TERM_MACRO)
#define SET_DISP_MACRO(R, CH, FN) \
  ((R)[(CH)].cfunc = (FN), \
   (R)[(CH)].lfunc = CONCAT (Ssi_, FN))

static void
default_readtable (readtab_rep *const r)
{
  int i;
  for (i = 0; i < ' '; i++)
    {
      r[i].type = SCT_ILLEGAL;
      r[i].cfunc = 0;
      r[i].lfunc = Qunbound;
      r[i].disp = 0;
    }
  for (; i < READTABLE_REP_SIZE; i++)
    {
      r[i].type = SCT_CONSTITUENT;
      r[i].cfunc = 0;
      r[i].lfunc = Qunbound;
      r[i].disp = 0;
    }

  r[CC_HT].type = SCT_WHITESPACE;
  r[CC_NL].type = SCT_WHITESPACE;
  r[CC_VT].type = SCT_WHITESPACE;
  r[CC_FF].type = SCT_WHITESPACE;
  r[CC_CR].type = SCT_WHITESPACE;
  r[CC_SPC].type = SCT_WHITESPACE;
  r['\\'].type = SCT_SINGLE_ESCAPE;
  r['|'].type = SCT_MULTIPLE_ESCAPE;

  SET_TERM_MACRO (r, '"', double_quote_reader);
  SET_TERM_MACRO (r, '\'', single_quote_reader);
  SET_TERM_MACRO (r, '(', open_paren_reader);
  SET_TERM_MACRO (r, ')', close_paren_reader);
  SET_TERM_MACRO (r, ',', comma_reader);
  SET_TERM_MACRO (r, ';', semicolon_reader);
  SET_TERM_MACRO (r, '`', backquote_reader);
  SET_NONTERM_MACRO (r, '#', dispmacro_reader);

  disptab_rep *const d = alloc_disptab ();
  r['#'].disp = d;

  SET_DISP_MACRO (d, '\\', number_backslash_reader);
  SET_DISP_MACRO (d, '\'', number_single_quote_reader);
  SET_DISP_MACRO (d, '(', number_open_paren_reader);
  SET_DISP_MACRO (d, ':', number_colon_reader);
  SET_DISP_MACRO (d, '.', number_dot_reader);
  SET_DISP_MACRO (d, 'B', number_B_reader);
  SET_DISP_MACRO (d, 'O', number_O_reader);
  SET_DISP_MACRO (d, 'X', number_X_reader);
  SET_DISP_MACRO (d, 'R', number_R_reader);
  SET_DISP_MACRO (d, 'S', number_S_reader);
  SET_DISP_MACRO (d, 'C', number_C_reader);
  SET_DISP_MACRO (d, 'A', number_A_reader);
  SET_DISP_MACRO (d, '=', number_equal_reader);
  SET_DISP_MACRO (d, '#', number_number_reader);
  SET_DISP_MACRO (d, '+', number_plus_reader);
  SET_DISP_MACRO (d, '-', number_minus_reader);
  SET_DISP_MACRO (d, '|', number_bar_reader);
}

void
init_readtable ()
{
  lisp readtab = make_readtable ();
  xreadtable_rep (readtab) = alloc_readtab ();
  default_readtable (xreadtable_rep (readtab));
  xsymbol_value (Vreadtable) = readtab;
  xsymbol_value (Vdefault_readtable) = readtab;
  xsymbol_value (Vstandard_readtable) = Fcopy_readtable (Qnil, Qnil);
}

static void
copy_readtable (const readtab_rep *oldr, readtab_rep *newr)
{
  memcpy (newr, oldr, sizeof *newr * READTABLE_REP_SIZE);
  readtab_rep *r = newr, *const e = r + READTABLE_REP_SIZE;
  for (; r < e; r++)
    r->disp = 0;
  for (; newr < e; oldr++, newr++)
    if (oldr->disp)
      {
        newr->disp = alloc_disptab ();
        memcpy (newr->disp, oldr->disp, sizeof *newr->disp * READTABLE_REP_SIZE);
      }
}

lisp
Fcopy_readtable (lisp from_readtable, lisp to_readtable)
{
  if (to_readtable == Qnil)
    to_readtable = 0;
  else if (to_readtable)
    check_readtable (to_readtable);

  if (!from_readtable)
    from_readtable = current_readtable ();
  else if (from_readtable != Qnil)
    check_readtable (from_readtable);

  lisp new_readtable = to_readtable ? to_readtable : make_readtable ();
  if (from_readtable != new_readtable)
    {
      readtab_rep *const newr = alloc_readtab ();
      try
        {
          if (from_readtable == Qnil)
            default_readtable (newr);
          else
            copy_readtable (xreadtable_rep (from_readtable), newr);
        }
      catch (nonlocal_jump &)
        {
          clear_readtable (newr);
          throw;
        }

      if (new_readtable == to_readtable)
        clear_readtable (xreadtable_rep (new_readtable));
      xreadtable_rep (new_readtable) = newr;
      xreadtable_case (new_readtable) = (from_readtable == Qnil
                                         ? RTC_PRESERVE
                                         : xreadtable_case (from_readtable));
    }
  return new_readtable;
}

lisp
Freadtable_case (lisp readtable)
{
  check_readtable (readtable);
  switch (xreadtable_case (readtable))
    {
    case RTC_UPCASE:
      return Kupcase;

    case RTC_DOWNCASE:
      return Kdowncase;

    case RTC_PRESERVE:
    default:
      return Kpreserve;

    case RTC_INVERT:
      return Kinvert;
    }
}

lisp
Fsi_set_readtable_case (lisp readtable, lisp lcase)
{
  check_readtable (readtable);
  if (lcase == Kupcase)
    xreadtable_case (readtable) = RTC_UPCASE;
  else if (lcase == Kdowncase)
    xreadtable_case (readtable) = RTC_DOWNCASE;
  else if (lcase == Kpreserve)
    xreadtable_case (readtable) = RTC_PRESERVE;
  else if (lcase == Kinvert)
    xreadtable_case (readtable) = RTC_INVERT;
  else
    FEprogram_error (Einvalid_readtable_case, lcase);
  return lcase;
}

static lisp
check_readtable (lisp readtable, int stdp)
{
  if (!readtable || readtable == Qnil)
    return stdp ? xsymbol_value (Vstandard_readtable) : current_readtable ();
  check_readtable (readtable);
  return readtable;
}

static Char
check_macro_char (lisp ch)
{
  check_char (ch);
  Char cc = xchar_code (ch);
  if (cc >= READTABLE_REP_SIZE)
    FEprogram_error (Emacro_char_out_of_range, ch);
  return cc;
}

lisp
Fset_syntax_from_char (lisp to_char, lisp from_char,
                       lisp to_readtable, lisp from_readtable)
{
  Char tc = check_macro_char (to_char);
  Char fc = check_macro_char (from_char);
  to_readtable = check_readtable (to_readtable, 0);
  from_readtable = check_readtable (from_readtable, 1);
  readtab_rep *fr = xreadtable_rep (from_readtable) + fc;
  readtab_rep *tr = xreadtable_rep (to_readtable) + tc;
  if (fr != tr)
    {
      disptab_rep *disp = 0;
      if (fr->disp)
        {
          disp = alloc_disptab ();
          memcpy (disp, fr->disp, sizeof *disp * READTABLE_REP_SIZE);
        }
      if (tr->disp)
        xfree (tr->disp);
      *tr = *fr;
      tr->disp = disp;
    }
  return Qt;
}

lisp
Fset_macro_character (lisp ch, lisp fn, lisp non_term_p, lisp readtable)
{
  Char cc = check_macro_char (ch);
  readtable = check_readtable (readtable, 0);
  readtab_rep *r = xreadtable_rep (readtable) + cc;
  r->type = non_term_p && non_term_p != Qnil ? SCT_NON_TERM_MACRO : SCT_TERM_MACRO;
  r->lfunc = fn;
  r->cfunc = get_reader_macro_function (fn);
  if (r->disp)
    {
      xfree (r->disp);
      r->disp = 0;
    }
  return Qt;
}

lisp
Fget_macro_character (lisp ch, lisp readtable)
{
  Char cc = check_macro_char (ch);
  readtable = check_readtable (readtable, 0);
  readtab_rep *r = xreadtable_rep (readtable) + cc;
  if (r->type == SCT_NON_TERM_MACRO)
    multiple_value::value (1) = Qt;
  else if (r->type == SCT_TERM_MACRO)
    multiple_value::value (1) = Qnil;
  else
    return Qnil;
  multiple_value::count () = 2;
  return r->lfunc == Qunbound ? Qnil : r->lfunc;
}

static Char
check_dispmacro_char (lisp ch)
{
  check_char (ch);
  Char cc = xchar_code (ch);
  if (cc >= READTABLE_REP_SIZE)
    FEprogram_error (Edispmacro_sub_char_out_of_range, ch);
  return cc;
}

lisp
Fmake_dispatch_macro_character (lisp ch, lisp non_term_p, lisp readtable)
{
  Char cc = check_dispmacro_char (ch);
  readtable = check_readtable (readtable, 0);
  readtab_rep *r = xreadtable_rep (readtable) + cc;
  if (!r->disp)
    r->disp = alloc_disptab ();
  r->type = non_term_p && non_term_p != Qnil ? SCT_NON_TERM_MACRO : SCT_TERM_MACRO;
  r->lfunc = Ssi_dispmacro_reader;
  r->cfunc = dispmacro_reader;
  return Qt;
}

lisp
Fset_dispatch_macro_character (lisp disp_char, lisp sub_char, lisp fn, lisp readtable)
{
  Char dc = check_macro_char (disp_char);
  Char sc = check_dispmacro_char (sub_char);
  readtable = check_readtable (readtable, 0);
  readtab_rep *r = xreadtable_rep (readtable) + dc;
  if (!r->disp)
    FEprogram_error (Eis_not_a_dispmacro_char, disp_char);
  if (digit_char_p (sc))
    FEprogram_error (Enumeric_dispmacro_sub_char, sub_char);
  sc = char_upcase (Char (sc));
  r->disp[sc].lfunc = fn;
  r->disp[sc].cfunc = get_reader_dispmacro_function (fn);
  return Qt;
}

lisp
Fget_dispatch_macro_character (lisp disp_char, lisp sub_char, lisp readtable)
{
  Char dc = check_macro_char (disp_char);
  Char sc = check_dispmacro_char (sub_char);
  readtable = check_readtable (readtable, 0);
  readtab_rep *r = xreadtable_rep (readtable) + dc;
  if (!r->disp)
    FEprogram_error (Eis_not_a_dispmacro_char, disp_char);
  if (digit_char_p (sc))
    return Qnil;
  sc = char_upcase (Char (sc));
  return r->disp[sc].lfunc == Qunbound ? Qnil : r->disp[sc].lfunc;
}

lisp
input_stream (lisp stream)
{
  if (!stream || stream == Qnil)
    stream = xsymbol_value (Vstandard_input);
  else if (stream == Qt)
    stream = xsymbol_value (Vterminal_io);
  check_stream (stream);
  return stream;
}

static lisp
end_of_file (lisp stream, lisp eof_error_p, lisp eof_value)
{
  if (eof_error_p != Qnil)
    FEend_of_file (stream);
  return eof_value ? eof_value : Qnil;
}

static lisp
parser (lisp stream, lisp recursive_p, int preserve_white)
{
  enable_quit eq;
  dynamic_bind dynb (Vreader_preserve_white,
                     xsymbol_value (Vreader_preserve_white));
  if (preserve_white)
    xsymbol_value (Vreader_preserve_white) = Qt;

  lisp object;
  if (recursive_p && recursive_p != Qnil)
    object = lisp_parser (stream, ACCEPT_EOF);
  else
    {
      dynamic_bind dynb1 (Vreader_in_backquote, Qnil);
      dynamic_bind dynb2 (Vreader_label_alist, Qnil);
      object = lisp_parser (stream, ACCEPT_EOF);
    }
  multiple_value::clear ();
  return object;
}

lisp
Fread (lisp stream, lisp eof_error_p, lisp eof_value, lisp recursive_p)
{
  stream = input_stream (stream);
  protect_gc gcpro (stream);
  lisp object = parser (stream, recursive_p, 0);
  if (object == Qeof)
    object = end_of_file (stream, eof_error_p, eof_value);
  return object;
}

lisp
Fread_preserving_whitespace (lisp stream, lisp eof_error_p,
                             lisp eof_value, lisp recursive_p)
{
  stream = input_stream (stream);
  protect_gc gcpro (stream);
  lisp object = parser (stream, recursive_p, 1);
  if (object == Qeof)
    object = end_of_file (stream, eof_error_p, eof_value);
  return object;
}

lisp
Fread_delimited_list (lisp ch, lisp stream, lisp recursive_p)
{
  check_char (ch);
  stream = input_stream (stream);
  protect_gc gcpro (stream);

  int len;
  lisp object;
  if (recursive_p && recursive_p != Qnil)
    object = read_delimited_list (stream, xchar_code (ch), len);
  else
    {
      dynamic_bind dynb1 (Vreader_in_backquote, Qnil);
      dynamic_bind dynb2 (Vreader_label_alist, Qnil);
      object = read_delimited_list (stream, xchar_code (ch), len);
    }
  multiple_value::clear ();
  return object;
}

static lisp
read_line (Token &t, lisp stream)
{
  while (1)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        return Qt;
      if (c == '\n')
        return Qnil;
      t.add (c);
    }
}

lisp
Fread_line (lisp stream, lisp eof_error_p, lisp eof_value, lisp /*recursive_p*/)
{
  stream = input_stream (stream);
  Token t;
  lisp r = read_line (t, stream);
  if (r == Qt && !t.length ())
    return end_of_file (stream, eof_error_p, eof_value);
  multiple_value::value (1) = r;
  multiple_value::count () = 2;
  return t.make_string ();
}

static void
check_read_string (lisp string)
{
  check_string (string);
  if (!complex_string_p (string) || !xarray_has_fillp (string))
    FEprogram_error (Evector_has_no_fill_pointer, string);
  xstring_length (string) = 0;
}

lisp
Fread_line_into (lisp string, lisp stream, lisp eof_error_p, lisp eof_value)
{
  check_read_string (string);
  if (!xarray_adjustable (string))
    FEprogram_error (Evector_is_not_adjustable, string);
  stream = input_stream (stream);
  Token t;
  lisp r = read_line (t, stream);
  if (r == Qt && !t.length ())
    return end_of_file (stream, eof_error_p, eof_value);
  multiple_value::value (1) = r;
  multiple_value::count () = 2;
  t.finish ();
  int l = t.length ();
  if (l > xstring_dimension (string))
    realloc_element (string, (l - xstring_dimension (string) + 127) & ~127, sizeof (Char));
  t.copy (xstring_contents (string));
  xstring_length (string) = l;
  return string;
}

lisp
Fread_into (lisp string, lisp stream, lisp eof_error_p, lisp eof_value, lisp max_length)
{
  check_read_string (string);
  stream = input_stream (stream);
  int l;
  if (!max_length || max_length == Qnil)
    l = xstring_dimension (string);
  else
    {
      l = fixnum_value (max_length);
      if (l <= 0 || l > xstring_dimension (string))
        FErange_error (max_length);
    }
  Char *p = xstring_contents (string), *const pe = p + l;
  for (; p < pe; p++)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        break;
      *p = Char (c);
    }
  xstring_length (string) = p - xstring_contents (string);
  if (!xstring_length (string) && xstring_dimension (string))
    return end_of_file (stream, eof_error_p, eof_value);
  return string;
}

lisp
Fread_as_string (lisp stream, lisp eof_error_p, lisp eof_value)
{
  stream = input_stream (stream);
  const readtab_rep *readtab = xreadtable_rep (current_readtable ());
  Token token;
  while (1)
    {
      lChar c = readc_stream (stream);
      if (c == lChar_EOF)
        {
          if (!token.length ())
            return end_of_file (stream, eof_error_p, eof_value);
          return token.make_string ();
        }

      switch (stdchar_type (readtab, Char (c)))
        {
        case SCT_CONSTITUENT:
        case SCT_ILLEGAL:
          token.add (c);
          break;

        case SCT_WHITESPACE:
          if (token.length ())
            return token.make_string ();
          break;

        default:
          unreadc_stream (c, stream);
          return token.length () ? token.make_string () : Qnil;
        }
    }
}

lisp
Fread_char (lisp stream, lisp eof_error_p, lisp eof_value, lisp /*recursive_p*/)
{
  stream = input_stream (stream);
  lChar c = readc_stream (stream);
  if (c == lChar_EOF)
    return end_of_file (stream, eof_error_p, eof_value);
  return make_char (Char (c));
}

lisp
Funread_char (lisp cc, lisp stream)
{
  check_char (cc);
  unreadc_stream (xchar_code (cc), input_stream (stream));
  return Qnil;
}

lisp
Fpeek_char (lisp peek_type, lisp stream, lisp eof_error_p, lisp eof_value, lisp /*recursive_p*/)
{
  stream = input_stream (stream);
  lChar c;
  if (!peek_type || peek_type == Qnil)
    c = peekc_stream (stream);
  else if (charp (peek_type))
    {
      lChar lc = xchar_code (peek_type);
      while (1)
        {
          c = peekc_stream (stream);
          if (c == lChar_EOF || c == lc)
            break;
          readc_stream (stream);
        }
    }
  else
    {
      const readtab_rep *readtab = xreadtable_rep (current_readtable ());
      while (1)
        {
          c = peekc_stream (stream);
          if (c == lChar_EOF || stdchar_type (readtab, Char (c)) != SCT_WHITESPACE)
            break;
          readc_stream (stream);
        }
    }
  if (c == lChar_EOF)
    return end_of_file (stream, eof_error_p, eof_value);
  return make_char (Char (c));
}

lisp
Flisten (lisp stream)
{
  return boole (listen_stream (input_stream (stream)));
}

lisp
Fread_char_no_hang (lisp stream, lisp eof_error_p, lisp eof_value, lisp /*recursive_p*/)
{
  stream = input_stream (stream);
  if (!listen_stream (stream))
    return Qnil;
  lChar c = readc_stream (stream);
  if (c == lChar_EOF)
    return end_of_file (stream, eof_error_p, eof_value);
  return make_char (Char (c));
}

lisp
Fclear_input (lisp stream)
{
  stream = input_stream (stream);
  while (listen_stream (stream) && readc_stream (stream) != lChar_EOF)
    ;
  return Qnil;
}

class with_open_file
{
  lisp stream;
public:
  with_open_file (lisp s) : stream (s) {}
  ~with_open_file ()
    {
      if (stream)
        Fclose (stream, 0);
    }
};

static lisp
load_file (lisp filename, lisp realname, lisp if_does_not_exist,
           int verbose, int print)
{
  lisp stream;
  if (streamp (filename))
    stream = filename;
  else
    {
      stream = create_file_stream (filename, Kinput, Qnil,
                                   if_does_not_exist, Kcanonical, 0);
      if (stream == Qnil)
        return Qnil;
    }

  Char buf[PATH_MAX * 2], *b = buf;
  if (verbose)
    {
      if (stringp (filename)
          && (xstring_length (filename) < 3
              || !streq (xstring_contents (filename) + xstring_length (filename) - 3,
                         3, ".lc")))
        b = a2w (b, "LOADING");
      else
        b = a2w (b, "loading");
      if (stringp (realname))
        {
          int l = min (PATH_MAX, xstring_length (realname));
          *b++ = ' ';
          bcopy (xstring_contents (realname), b, l);
          b += l;
        }
      b = a2w (b, "...\n");
      app.status_window.puts (buf, b - buf);
      b--;
    }

  dynamic_bind dynb1 (Vpackage, xsymbol_value (Vpackage));
  dynamic_bind dynb2 (Vload_pathname,
                      ((streamp (stream) && file_stream_p (stream))
                       ? xfile_stream_pathname (stream) : stream));
  dynamic_bind dynb3 (Vreadtable, xsymbol_value (Vreadtable));
  protect_gc gcpro (stream);

  with_open_file s (stream == filename ? 0 : stream);

  while (1)
    {
      lisp object = parser (stream, Qnil, 0);
      if (object == Qeof)
        break;
      object = Feval (object);
      if (print)
        {
          write_object (object, xsymbol_value (Vstandard_output), Qnil);
          Fterpri (xsymbol_value (Vstandard_output));
        }
    }

  if (verbose)
    {
      b = a2w (b, "done\n");
      app.status_window.puts (buf, b - buf);
    }

  return Qt;
}

lisp
Fload (lisp filename, lisp keys)
{
  return load_file (filename, filename,
                    find_keyword (Kif_does_not_exist, keys, Kerror),
                    find_keyword_bool (Kverbose, keys, xsymbol_value (Vload_verbose)),
                    find_keyword_bool (Kprint, keys, xsymbol_value (Vload_print)));
}

lisp
Fsi_load_library (lisp filename, lisp keys)
{
  lisp path;

  if (find_keyword_bool (Kno_suffix, keys))
    path = filename;
  else
    {
      path = Ffind_load_path (filename);
      if (path == Qnil)
        {
          if (find_keyword (Kif_does_not_exist, keys, Kerror) != Kerror)
            return Qnil;
          FEsimple_error (Efile_not_found, filename);
        }
    }
  return load_file (path, filename, Qnil, !find_keyword_bool (Kno_message, keys), 0);
}

