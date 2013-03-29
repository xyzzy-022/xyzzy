#include "stdafx.h"
#include "ed.h"
#include "wstream.h"
#include "sock.h"
#include "version.h"

char upcase_digit_char[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
char downcase_digit_char[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static int special_condition_report (wStream &, lisp);

struct fmt_float
{
  char buf[DBL_DIG + 16];
  char *b0;
  char *be;
  int sign;
  int exp;
  int fmt;
  fmt_float (lisp);
  void round (int);
  void roundf (int);
};

fmt_float::fmt_float (lisp lnumber)
{
  double number;
  int prec;
  if (double_float_p (lnumber))
    {
      number = xdouble_float_value (lnumber);
      prec = DBL_DIG + 1;
      fmt = NF_FLOAT_D;
    }
  else
    {
      number = coerce_to_double_float (lnumber);
      prec = FLT_DIG + 1;
      fmt = NF_FLOAT_F;
    }

  if (_finite (number))
    {
      char *buf1 = buf + 1;
      for (be = stpcpy (buf1, _ecvt (number, prec, &exp, &sign));
           be > buf1 && be[-1] == '0'; be--)
        ;
      b0 = buf1;
      *be = 0;
      exp--;
      sign = sign ? -1 : 1;
    }
  else
    {
      switch (_fpclass (number))
        {
        case _FPCLASS_NINF:
          strcpy (buf, "#<-Inf>");
          break;

        case _FPCLASS_PINF:
          strcpy (buf, "#<+Inf>");
          break;

        default:
          strcpy (buf, "#<NaN>");
          break;
        }
      b0 = 0;
    }
}

void
fmt_float::round (int d)
{
  if (d < 0 || d >= be - b0)
    return;
  be = b0 + d;
  char *b = be;
  if (*b >= '5')
    {
      b0[-1] = '0';
      while (1)
        {
          b--;
          if (*b == '9')
            be--;
          else
            {
              (*b)++;
              break;
            }
        }
      if (b < b0)
        {
          b0 = b;
          exp++;
        }
    }
  for (; be > b0 && be[-1] == '0'; be--)
    ;
  *be = 0;
  return;
}

inline void
fmt_float::roundf (int d)
{
  round (d + 1 + exp);
}

static char *
store_uint (char *b, u_int n)
{
  *--b = 0;
  do
    *--b = '0' + n % 10;
  while (n /= 10);
  return b;
}

struct circle_object
{
  enum
    {
      single = 1,
      shared = 1 << (BITS_PER_INT - 1),
      referenced = 0
    };
  lisp object;
  int f;
};

class print_circle
{
  struct circle_rep
    {
      enum {REPSIZE = 1024};
      circle_rep *next;
      int used;
      circle_object objs[REPSIZE];

      circle_rep () : next (0), used (0) {}
    };
  circle_rep initial_buf;

  circle_rep *currep;

  void remove_reps ();
  void setup1 (lisp object);
  void add_object (lisp object);
public:
  void reinit ();
  print_circle ();
  ~print_circle ();
  void setup (lisp object);
  circle_object *lookup (lisp object) const;
  int find (wStream &stream, lisp object, int need_dot) const;
};

inline
print_circle::print_circle ()
     : currep (&initial_buf)
{
}

inline
print_circle::~print_circle ()
{
  remove_reps ();
}

void
print_circle::reinit ()
{
  remove_reps ();
  initial_buf.used = 0;
  initial_buf.next = 0;
  currep = &initial_buf;
}

void
print_circle::remove_reps ()
{
  for (circle_rep *p = currep, *next; p; p = next)
    {
      next = p->next;
      if (p != &initial_buf)
        delete p;
    }
}

void
print_circle::add_object (lisp object)
{
  if (currep->used == circle_rep::REPSIZE)
    {
      circle_rep *p = new circle_rep;
      p->next = currep;
      currep = p;
    }
  currep->objs[currep->used].object = object;
  currep->objs[currep->used].f = circle_object::single;
  currep->used++;
}

circle_object *
print_circle::lookup (lisp object) const
{
  for (circle_rep *rep = currep; rep; rep = rep->next)
    for (circle_object *o = rep->objs, *oe = o + rep->used; o < oe; o++)
      if (o->object == object)
        return o;
  return 0;
}

/*GENERIC_FUNCTION*/
void
print_circle::setup1 (lisp object)
{
  while (1)
    {
      if (immediatep (object))
        return;
      int type = object_typeof (object);
      switch (type)
        {
        case Tlong_int:
        case Tclosure:
        case Tfunction:
        case Thash_table:
        case Tstream:
        case Tpackage:
        case Tsingle_float:
        case Tdouble_float:
        case Tcomplex:
        case Twindow:
        case Tbuffer:
        case Tmarker:
        case Tsyntax_table:
        case Tprocess:
        case Tregexp:
        case Twin32_menu:
        case Twin32_dde_handle:
        case Terror:
        case Tstruct_def:
        case Tchunk:
        case Tdll_module:
        case Tdll_function:
        case Tc_callable:
        case Toledata:
        case Treadtable:
        case Twait_object:
        case Tchar_encoding:
        case Tenvironment:
          return;

        case Tsymbol:
          if (xsymbol_package (object) != Qnil)
            return;
          break;
        }

      circle_object *p = lookup (object);
      if (p)
        {
          p->f = circle_object::shared;
          return;
        }

      add_object (object);

      if (typep (type, Tcons))
        {
          setup1 (xcar (object));
          object = xcdr (object);
        }
      else
        {
          if (typep (type, Tstruct_data))
            {
              for (const lisp *p = xstrdata_data (object),
                   *const pe = p + xstrdata_nslots (object);
                   p < pe; p++)
                setup1 (*p);
            }
          else
            {
              // general array and general vector
              if (object_type_bit_p (type, TAgeneral))
                for (const lisp *v = (const lisp *)xbase_vector_contents (object),
                     *ve = v + xvector_length (object);
                     v < ve; v++)
                  setup1 (*v);
            }
          return;
        }
    }
}

void
print_circle::setup (lisp object)
{
  setup1 (object);

  int number = 1;
  for (circle_rep *rep = currep; rep; rep = rep->next)
    {
      circle_object *p, *pe, *q;
      for (p = rep->objs, pe = p + rep->used, q = p;
           p < pe; p++)
        if (p->f == circle_object::shared)
          {
            q->object = p->object;
            q->f = circle_object::shared | number++;
            q++;
          }
      rep->used = q - rep->objs;
    }
}

int
print_circle::find (wStream &stream, lisp object, int need_dot) const
{
  circle_object *p = lookup (object);
  if (!p)
    return circle_object::single;

  if (need_dot)
    stream.add (". ");
  stream.add ('#');
  char buf[32];
  stream.add (store_uint (buf + sizeof buf, p->f & ~circle_object::shared));
  if (p->f & circle_object::shared)
    {
      stream.add ('=');
      p->f &= ~circle_object::shared;
      return circle_object::shared;
    }
  else
    {
      stream.add ('#');
      return circle_object::referenced;
    }
}

struct print_control
{
  int readably;
  int escape;
  int pretty;
  int base;
  int radix;
  int circle;
  int length;
  int level;

  print_circle *const pr_circle;

  print_control (print_circle &);
  print_control (int);
  print_control (print_circle &, lisp);

  void princ ();
  void prin1 ();

  void setup_circle (lisp object) const;
};

print_control::print_control (print_circle &pr_circle_)
     : pr_circle (&pr_circle_)
{
  readably = xsymbol_value (Vprint_readably) != Qnil;
  escape = xsymbol_value (Vprint_escape) != Qnil;
  pretty = xsymbol_value (Vprint_pretty) != Qnil;
  base = fixnum_value (xsymbol_value (Vprint_base));
  if (base < 2 || base > 36)
    base = 10;
  radix = xsymbol_value (Vprint_radix) != Qnil;
  circle = xsymbol_value (Vprint_circle) != Qnil;
  lisp x = xsymbol_value (Vprint_level);
  level = x == Qnil ? -1 : fixnum_value (x);
  x = xsymbol_value (Vprint_length);
  length = x == Qnil ? -1 : fixnum_value (x);
}

print_control::print_control (print_circle &pr_circle_, lisp keys)
     : pr_circle (&pr_circle_)
{
  readably = find_keyword_bool (Kreadably, keys, xsymbol_value (Vprint_readably));
  escape = find_keyword_bool (Kescape, keys, xsymbol_value (Vprint_escape));
  pretty = find_keyword_bool (Kpretty, keys, xsymbol_value (Vprint_pretty));
  base = find_keyword_int (Kbase, keys, xsymbol_value (Vprint_base));
  if (base < 2 || base > 36)
    base = 10;
  radix = find_keyword_bool (Kradix, keys, xsymbol_value (Vprint_radix));
  circle = find_keyword_bool (Kcircle, keys, xsymbol_value (Vprint_circle));
  lisp x = xsymbol_value (Vprint_level);
  level = find_keyword_int (Klevel, keys, x == Qnil ? -1 : fixnum_value (x));
  x = xsymbol_value (Vprint_length);
  length = find_keyword_int (Klength, keys, x == Qnil ? -1 : fixnum_value (x));
}

inline
print_control::print_control (int b)
     : pr_circle (0)
{
  base = b;
  radix = 0;
}

inline void
print_control::princ ()
{
  escape = 0;
  readably = 0;
}

inline void
print_control::prin1 ()
{
  escape = 1;
}

void
print_control::setup_circle (lisp object) const
{
  if (circle)
    {
      pr_circle->reinit ();
      pr_circle->setup (object);
    }
}

static void print_sexp (wStream &, const print_control &, lisp, int);

static void
print_object_address (wStream &stream, lisp object)
{
  char buf[32];
  char *b = store_uint (buf + sizeof buf, pointer_t (object));
  *--b = ' ';
  stream.add (b);
}

static void
print_unreadable_object (wStream &stream, lisp object, const char *string)
{
  stream.add ('#');
  stream.add ('<');
  stream.add (string);
  print_object_address (stream, object);
  stream.add ('>');
}

static int
print_list_pretty (wStream &stream, const print_control &pc, lisp object, int level)
{
  lisp x = xcar (object);
  object = xcdr (object);
  if (!consp (object) || xcdr (object) != Qnil)
    return 0;
  if (pc.circle && pc.pr_circle->lookup (object))
    return 0;
  if (x == Qquote)
    stream.add ('\'');
  else if (x == Qfunction)
    stream.add ("#'");
  else
    return 0;
  print_sexp (stream, pc, xcar (object), level);
  return 1;
}

static void
print_list (wStream &stream, const print_control &pc, lisp object, int level)
{
  if (pc.pretty && print_list_pretty (stream, pc, object, level))
    return;
  if (!pc.readably && pc.level >= 0 && level > pc.level)
    {
      stream.add ('#');
      return;
    }
  stream.add ('(');
  int braces = 1;
  int l = 0;
  while (1)
    {
      if (!pc.readably && pc.length >= 0 && l >= pc.length)
        {
          stream.add ("...");
          l = -1;
          break;
        }
      print_sexp (stream, pc, xcar (object), level);
      object = xcdr (object);
      if (!consp (object))
        break;
      l++;
      stream.add (' ');
      if (pc.circle)
        {
          int f = pc.pr_circle->find (stream, object, 1);
          if (f == circle_object::referenced)
            {
              object = Qnil;
              break;
            }
          if (f == circle_object::shared)
            {
              stream.add ('(');
              braces++;
            }
        }
      QUIT;
    }
  if (object != Qnil && l >= 0)
    {
      stream.add (" . ");
      print_sexp (stream, pc, object, level);
    }
  stream.fill (')', braces);
}

static int
symbol_name_need_escape_p (lisp readtab, const print_control &pc, lisp object)
{
  const Char *p0 = xstring_contents (object);
  const Char *pe = p0 + xstring_length (object);

  if (p0 == pe || stdchar_non_terminating_macro_p (xreadtable_rep (readtab), *p0))
    return 1;

  u_char ctype = 0;
  const Char *p;
  for (p = p0; p < pe; p++)
    {
      switch (stdchar_type (xreadtable_rep (readtab), *p))
        {
        case SCT_TERM_MACRO:
        case SCT_WHITESPACE:
          return 1;
        }
      if (ascii_char_p (*p))
        ctype |= _char_type (*p);
    }

  if (pc.escape)
    switch (xreadtable_case (readtab))
      {
      case RTC_UPCASE:
        if (ctype & _CTL)
          return 1;
        break;

      case RTC_DOWNCASE:
        if (ctype & _CTU)
          return 1;
        break;

      case RTC_PRESERVE:
      default:
        break;

      case RTC_INVERT:
        if (ctype & (_CTU | _CTL))
          return 1;
        break;
      }

  if (pc.base == 10)
    {
      if (parse_number_format (p0, pe, 10) != NF_BAD)
        return 1;
    }
  else
    {
      int f = parse_number_format (p0, pe, pc.base);
      if (f == NF_INTEGER || f == NF_FRACTION
          || parse_number_format (p0, pe, 10) & NF_FLOAT)
        return 1;
    }

  for (p = p0; p < pe; p++)
    if (*p != '.')
      break;
  if (p == pe)
    return 1;

  return 0;
}

static void
print_symbol_name (wStream &stream, const print_control &pc, lisp object)
{
  lisp readtab = current_readtable ();
  int escape = symbol_name_need_escape_p (readtab, pc, object);
  if (escape)
    stream.add ('|');

  for (const Char *p = xstring_contents (object), *pe = p + xstring_length (object);
       p < pe; p++)
    {
      switch (stdchar_type (xreadtable_rep (readtab), *p))
        {
        case SCT_ILLEGAL:
        case SCT_SINGLE_ESCAPE:
        case SCT_MULTIPLE_ESCAPE:
          stream.add ('\\');
          break;
        }
      stream.add (*p);
    }

  if (escape)
    stream.add ('|');
}

static void
print_symbol (wStream &stream, const print_control &pc, lisp object)
{
  lisp name = xsymbol_name (object);
  if (!pc.readably && !pc.escape)
    stream.add (xstring_contents (name), xstring_length (name));
  else
    {
      lisp package = xsymbol_package (object);
      if (package == Qnil)
        stream.add ("#:");
      else if (package == xsymbol_value (Vkeyword_package))
        stream.add (':');
      else if (Ffind_symbol (xsymbol_name (object), 0) != object
               || multiple_value::value (1) == Qnil)
        {
          if (xpackage_name (package) == Qnil)
            stream.add ('#:');
          else
            {
              print_symbol_name (stream, pc, xpackage_name (package));
              if (Ffind_symbol (xsymbol_name (object), package) != object)
                FEsimple_package_error (package,
                                        Ecannot_access_symbol_from_home_package,
                                        object);
              else if (multiple_value::value (1) == Kinternal)
                stream.add ("::");
              else if (multiple_value::value (1) == Kexternal)
                stream.add (':');
              else
                FEsimple_package_error (package,
                                        Ecannot_access_symbol_from_home_package,
                                        object);
            }
        }
      print_symbol_name (stream, pc, name);
      multiple_value::clear ();
    }
}

static int
print_integer_width (lisp linteger, int base)
{
  assert (base >= 2 && base <= 36);
  if (bignump (linteger))
    return xbignum_rep (linteger)->fmtwidth (base) + 32;
  else
    return BITS_PER_LONG + 32;
}

static inline int
print_integer_width (const print_control &pc, lisp linteger)
{
  return print_integer_width (linteger, pc.base);
}

static char *
print_integer (char *b, lisp linteger, int base, int sign, int radix)
{
  assert (base >= 2 && base <= 36);
  if (bignump (linteger))
    return xbignum_rep (linteger)->to_ascii (b, base, base == 10 && radix, sign,
                                             (radix
                                              ? upcase_digit_char
                                              : downcase_digit_char));
  *--b = 0;
  if (base == 10 && radix)
    *--b = '.';
  long value = fixnum_value (linteger);
  u_long v = value < 0 ? -value : value;
  const char *x = radix ? upcase_digit_char : downcase_digit_char;
  do
    *--b = x[v % base];
  while (v /= base);
  if (value < 0)
    *--b = '-';
  else if (sign)
    *--b = '+';
  return b;
}

static char *
print_base_spec (char *b, int base, int fractp)
{
  switch (base)
    {
    case 2:
      *--b = 'b';
      *--b = '#';
      break;

    case 8:
      *--b = 'o';
      *--b = '#';
      break;

    case 16:
      *--b = 'x';
      *--b = '#';
      break;

    case 10:
      if (!fractp)
        break;
      /* fall thru... */
    default:
      *--b = 'r';
      *--b = base % 10 + '0';
      if (base >= 10)
        *--b = base / 10 + '0';
      *--b = '#';
    }
  return b;
}

static char *
print_integer (char *b, const print_control &pc, lisp linteger, int sign)
{
  assert (pc.base >= 2 && pc.base <= 36);
  b = print_integer (b, linteger, pc.base, sign, pc.radix);
  if (pc.radix)
    b = print_base_spec (b, pc.base, 0);
  return b;
}

static void
print_integer (wStream &stream, const print_control &pc, lisp linteger)
{
  int w = print_integer_width (pc, linteger);
  char *b = (char *)alloca (w);
  stream.add (print_integer (b + w, pc, linteger, 0));
}

static void
print_fraction (wStream &stream, const print_control &pc, lisp object)
{
  int w = max (max (print_integer_width (pc, xfract_num (object)),
                    print_integer_width (pc, xfract_den (object))),
               10);
  char *b = (char *)alloca (w);
  if (pc.radix)
    stream.add (print_base_spec (b + w, pc.base, 1));
  stream.add (print_integer (b + w, xfract_num (object), pc.base, 0, pc.radix));
  stream.add ('/');
  stream.add (print_integer (b + w, xfract_den (object), pc.base, 0, pc.radix));
}

static void
print_flonum (wStream &stream, const print_control &, lisp lnumber)
{
  fmt_float f (lnumber);

  if (!f.b0)
    stream.add (f.buf);
  else if (f.b0 == f.be)
    {
      stream.add ("0.0");
      if (f.fmt != default_float_format ())
        {
          stream.add (f.fmt & ~NF_FLOAT);
          stream.add ('0');
        }
    }
  else
    {
      if (f.sign < 0)
        stream.add ('-');
      if (f.exp >= -3 && f.exp < 7)
        {
          if (f.exp < 0)
            {
              stream.add ('0');
              stream.add ('.');
              stream.fill ('0', -1 - f.exp);
              stream.add (f.b0);
            }
          else
            {
              char *b = f.b0;
              int i;
              for (i = -1; i < f.exp && *b; i++, b++)
                stream.add (*b);
              stream.fill ('0', f.exp - i);
              stream.add ('.');
              if (*b)
                stream.add (b);
              else
                stream.add ('0');
            }
          if (f.fmt != default_float_format ())
            {
              stream.add (f.fmt & ~NF_FLOAT);
              stream.add ('0');
            }
        }
      else
        {
          stream.add (*f.b0);
          stream.add ('.');
          if (f.b0[1])
            stream.add (f.b0 + 1);
          else
            stream.add ('0');
          if (f.fmt != default_float_format ())
            stream.add (f.fmt & ~NF_FLOAT);
          else
            stream.add ('e');
          char b[10];
          sprintf (b, "%d", f.exp);
          stream.add (b);
        }
    }
}

static void
print_complex (wStream &stream, const print_control &pc, lisp c)
{
  stream.add ("#C(");
  print_sexp (stream, pc, xcomplex_real (c), 0);
  stream.add (' ');
  print_sexp (stream, pc, xcomplex_imag (c), 0);
  stream.add (')');
}

static void
quote_char (wStream &stream, Char c)
{
  const readtab_rep *readtab = xreadtable_rep (current_readtable ());
  switch (stdchar_type (readtab, c))
    {
    case SCT_TERM_MACRO:
    case SCT_SINGLE_ESCAPE:
    case SCT_MULTIPLE_ESCAPE:
      stream.add ('\\');
      break;
    }
}

static void
print_char (wStream &stream, Char c, int escape)
{
  int meta = 0;

  if (meta_char_p (c))
    {
      meta = 1;
      c = meta_char_to_char (c);
    }
  else if (meta_function_char_p (c))
    {
      meta = 1;
      c = meta_function_to_function (c);
    }

  if (function_char_p (c))
    {
      if (pseudo_ctlchar_p (c))
        {
          stream.add ("C-");
          if (meta)
            stream.add ("M-");
          c = pseudo_ctl2char_table[c & 0xff];
          if (escape)
            quote_char (stream, c);
          stream.add (c);
        }
      else
        {
          if (c & CCF_SHIFT_BIT)
            stream.add ("S-");
          if (c & CCF_CTRL_BIT)
            stream.add ("C-");
          if (meta)
            stream.add ("M-");
          const char *p = function_Char2name (c & ~(CCF_SHIFT_BIT | CCF_CTRL_BIT));
          if (p)
            stream.add (p);
          else
            assert (0);
        }
    }
  else
    {
      const char *p = standard_Char2name (c);
      if (p)
        {
          if (meta)
            stream.add ("M-");
          stream.add (p);
        }
      else
        {
          int quote = 0;
          if (c < ' ')
            {
              stream.add ("C-");
              c = _char_downcase (c + '@');
              quote = 1;
            }
          else if (c == CC_DEL)
            {
              stream.add ("C-");
              c = '?';
            }
          if (meta)
            {
              stream.add ("M-");
              quote = 1;
            }

          if (DBCP (c))
            {
              if (!escape || SJISP (c >> 8))
                stream.add (c);
              else
                {
                  stream.add ('x');
                  stream.add (downcase_digit_char[(c >> 12) & 15]);
                  stream.add (downcase_digit_char[(c >> 8) & 15]);
                  stream.add (downcase_digit_char[(c >> 4) & 15]);
                  stream.add (downcase_digit_char[(c >> 0) & 15]);
                }
            }
          else
            {
              if (escape && SJISP (c))
                {
                  stream.add ('x');
                  stream.add (downcase_digit_char[(c >> 4) & 15]);
                  stream.add (downcase_digit_char[(c >> 0) & 15]);
                }
              else
                {
                  if (escape && quote)
                    quote_char (stream, c);
                  stream.add (c);
                }
            }
        }
    }
}

static void
print_char (wStream &stream, const print_control &pc, Char c)
{
  if (pc.readably || pc.escape)
    {
      stream.add ("#\\");
      print_char (stream, c, 1);
    }
  else
    stream.add (c);
}

static void
print_string (wStream &stream, const print_control &pc, lisp object)
{
  if (!pc.readably && !pc.escape)
    stream.add (xstring_contents (object), xstring_length (object));
  else
    {
      stream.add ('"');
      for (const Char *p = xstring_contents (object), *pe = p + xstring_length (object);
           p < pe; p++)
        {
          Char c = *p;
          if (DBCP (c))
            {
              if (SJISP (c >> 8))
                stream.add (c);
              else
                {
                  stream.add ('\\');
                  stream.add ('X');
                  stream.add (downcase_digit_char[(c >> 12) & 15]);
                  stream.add (downcase_digit_char[(c >> 8) & 15]);
                  stream.add (downcase_digit_char[(c >> 4) & 15]);
                  stream.add (downcase_digit_char[(c >> 0) & 15]);
                }
            }
          else
            {
              if (SJISP (c))
                {
                  stream.add ('\\');
                  stream.add ('x');
                  stream.add (downcase_digit_char[(c >> 4) & 15]);
                  stream.add (downcase_digit_char[(c >> 0) & 15]);
                }
              else
                {
                  if (c == '"' || c == '\\')
                    stream.add ('\\');
                  stream.add (c);
                }
            }
        }
      stream.add ('"');
    }
}

static void
print_vector (wStream &stream, const print_control &pc, lisp object, int level)
{
  if (!pc.readably && pc.level >= 0 && level > pc.level)
    {
      stream.add ('#');
      return;
    }
  const lisp *v = xvector_contents (object);
  int l = xvector_length (object);
  stream.add ("#(");
  if (l)
    {
      if (pc.readably || pc.length < 0 || l < pc.length)
        {
          for (int i = 1; i < l; i++, v++)
            {
              print_sexp (stream, pc, *v, level);
              stream.add (' ');
            }
          print_sexp (stream, pc, *v, level);
        }
      else
        {
          for (int i = 0; i < pc.length; i++, v++)
            {
              print_sexp (stream, pc, *v, level);
              stream.add (' ');
            }
          stream.add ("...");
        }
    }
  stream.add (')');
}

static void
print_empty_array (wStream &stream, const print_control &pc, lisp object)
{
  if (!pc.readably && !pc.escape)
    print_unreadable_object (stream, object, "array");
  else
    {
      stream.add ("#.(make-array '(");
      char buf[32];
      for (int i = 0; i < xarray_rank (object); i++)
        {
          if (i)
            stream.add (' ');
          stream.add (store_uint (buf + sizeof buf, xarray_dims (object) [i]));
        }
      stream.add ("))");
    }
}

static void
print_omitted_array (wStream &stream, const print_control &pc,
                     lisp object, int level)
{
  int total_size = xarray_total_size (object);
  int rank = xarray_rank (object);

  for (; rank && level > pc.level; level--)
    {
      rank--;
      total_size /= xarray_dims (object) [rank];
    }

  if (!rank)
    {
      stream.add ('#');
      return;
    }

  int *subscripts = (int *)alloca (sizeof (int) * rank);
  bzero (subscripts, sizeof (int) * rank);

  int dim_n = xarray_dims (object) [rank - 1];

  for (int i = 0; i < total_size; i++)
    {
      int j;
      for (j = rank - 1; j >= 0; j--)
        if (!subscripts[j])
          stream.add ('(');
        else
          break;

      int spc = 1;
      if (!pc.readably && pc.length >= 0 && i % dim_n >= pc.length)
        {
          if (i % dim_n == pc.length)
            stream.add ("...");
          spc = 0;
        }
      else
        stream.add ('#');
      for (j = rank - 1; j >= 0; j--)
        {
          subscripts[j]++;
          if (subscripts[j] < xarray_dims (object) [j])
            break;
          subscripts[j] = 0;
          stream.add (')');
          spc = 1;
        }
      if (j >= 0 && spc)
        stream.add (' ');
    }
}

/*GENERIC_FUNCTION:ARRAY*/
static void
print_array (wStream &stream, const print_control &pc,
             lisp object, int level)
{
  if (!xarray_total_size (object))
    {
      print_empty_array (stream, pc, object);
      return;
    }

  stream.add ('#');
  char buf[32];
  stream.add (store_uint (buf + sizeof buf, xarray_rank (object)));
  stream.add ('A');

  level += xarray_rank (object);
  if (!pc.readably && pc.level >= 0 && level > pc.level)
    {
      print_omitted_array (stream, pc, object, level);
      return;
    }

  int *subscripts = (int *)alloca (sizeof (int) * xarray_rank (object));
  bzero (subscripts, sizeof (int) * xarray_rank (object));

  int dim_n = (!xarray_rank (object) ? 1
               : xarray_dims (object) [xarray_rank (object) - 1]);

  int type = object_typeof (object);

  for (int i = 0; i < xarray_total_size (object); i++)
    {
      int j;
      for (j = xarray_rank (object) - 1; j >= 0; j--)
        if (!subscripts[j])
          stream.add ('(');
        else
          break;

      int spc = 1;
      if (!pc.readably && pc.length >= 0 && i % dim_n >= pc.length)
        {
          if (i % dim_n == pc.length)
            stream.add ("...");
          spc = 0;
        }
      else
        {
          switch (type)
            {
            case Tstring_array:
              print_char (stream, pc, xstring_array_contents (object) [i]);
              break;

            default:
              assert (0);
            case Tarray:
              print_sexp (stream, pc, xgeneral_array_contents (object) [i], level);
              break;
            }
        }

      for (j = xarray_rank (object) - 1; j >= 0; j--)
        {
          subscripts[j]++;
          if (subscripts[j] < xarray_dims (object) [j])
            break;
          subscripts[j] = 0;
          stream.add (')');
          spc = 1;
        }
      if (j >= 0 && spc)
        stream.add (' ');
    }
}

static inline void
print_message (wStream &stream, const print_control &, int code)
{
  stream.add (get_message_string (code));
}

static inline void
simple_print_string (wStream &stream, lisp string)
{
  if (stringp (string))
    stream.add (xstring_contents (string), xstring_length (string));
}

static void
print_function (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<function: ");
  simple_print_string (stream, xfunction_name (object));
  stream.add ('>');
}

static void
print_closure (wStream &stream, const print_control &pc, lisp object)
{
  stream.add ("#<lexical-closure: ");
  if (xclosure_name (object) == Qnil)
    stream.add ("(anonymous)");
  else
    print_sexp (stream, pc, xclosure_name (object), 0);
  stream.add ('>');
}

static inline void
print_hash_table (wStream &stream, const print_control &, lisp object)
{
  char buf[32];
  stream.add ("#<hashtable :test ");
  hash_test_proc test = xhash_table_test_fn (object);
  if (test == Feq)
    stream.add ("eq ");
  else if (test == Feql)
    stream.add ("eql ");
  else if (test == Fequal)
    stream.add ("equal ");
  else
    stream.add ("equalp ");
  stream.add (":size ");
  stream.add (store_uint (buf + sizeof buf, xhash_table_count (object)));
  stream.add ("/");
  stream.add (store_uint (buf + sizeof buf, xhash_table_size (object)));
  print_object_address (stream, object);
  stream.add (">");
}

static void
print_file_stream_pathname (wStream &stream, lisp object)
{
  if (stringp (xfile_stream_pathname (object)))
    simple_print_string (stream, xfile_stream_pathname (object));
  else
    stream.add ('-');
  stream.add ('>');
}

static void
print_stream (wStream &stream, const print_control &, lisp object)
{
  switch (xstream_type (object))
    {
    case st_file_input:
      stream.add ("#<file-input stream: ");
      print_file_stream_pathname (stream, object);
      break;

    case st_file_output:
      stream.add ("#<file-output stream: ");
      print_file_stream_pathname (stream, object);
      break;

    case st_file_io:
      stream.add ("#<file-io stream: ");
      print_file_stream_pathname (stream, object);
      break;

    case st_string_input:
      print_unreadable_object (stream, object, "string-input stream");
      break;

    case st_string_output:
      print_unreadable_object (stream, object, "string-output stream");
      break;

    case st_synonym:
      print_unreadable_object (stream, object, "synonym stream");
      break;

    case st_broadcast:
      print_unreadable_object (stream, object, "broadcast stream");
      break;

    case st_concatenated:
      print_unreadable_object (stream, object, "concatenated stream");
      break;

    case st_two_way:
      print_unreadable_object (stream, object, "two-way stream");
      break;

    case st_echo:
      print_unreadable_object (stream, object, "echo stream");
      break;

    case st_status:
      print_unreadable_object (stream, object, "status window stream");
      break;

    case st_buffer:
      print_unreadable_object (stream, object, "buffer stream");
      break;

    case st_keyboard:
      print_unreadable_object (stream, object, "keyboard stream");
      break;

    case st_wstream:
      print_unreadable_object (stream, object, "stream");
      break;

    case st_socket:
      print_unreadable_object (
        stream, object,
        (Fsocket_stream_ssl_p (object) == Qt) ? "ssl-socket-stream" : "socket-stream");
      break;

    case st_general_input:
      print_unreadable_object (stream, object, "general-input-stream");
      break;

    case st_general_output:
      print_unreadable_object (stream, object, "general-output-stream");
      break;

    case st_debug_output:
      print_unreadable_object (stream, object, "debug-output-stream");
      break;

    default:
      assert (0);
      break;
    }
}

static void
print_package (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<package: ");
  if (stringp (xpackage_name (object)))
    simple_print_string (stream, xpackage_name (object));
  else
    stream.add ("anonymous");
  stream.add ('>');
}

static void
print_error (wStream &stream, const print_control &, lisp object)
{
  if (xerror_type (object) == CRTL_ERROR)
    stream.add (strerror (xerror_number (object)));
  else
    {
      const char *s = sock::errmsg (xerror_number (object));
      if (s)
        stream.add (s);
      else
        {
          char buf[1024];
          static char *args[] = {"", "", "", "", 0,};
          if (!FormatMessage ((FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ARGUMENT_ARRAY
                               | FORMAT_MESSAGE_MAX_WIDTH_MASK),
                              0, xerror_number (object), GetUserDefaultLangID (),
                              buf, sizeof buf, args))
            *buf = 0;
          if (!*buf)
            wsprintf (buf, "Undocumented win32 error: %d", xerror_number (object));
          stream.add (buf);
        }
    }
}

static void
print_random_state (wStream &stream, const print_control &, lisp object)
{
  char buf[32];
  stream.add ("#S(random-state data #(");
  stream.add (store_uint (buf + sizeof buf, xrandom_state_object (object).index ()));
  for (int i = 0; i < Random::INDEX_MAX; i++)
    {
      stream.add (' ');
      stream.add (store_uint (buf + sizeof buf, xrandom_state_object (object).state (i)));
    }
  stream.add ("))");
}

static void
print_struct_def (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<structure-definition: ");
  simple_print_string (stream, xsymbol_name (xstrdef_name (object)));
  stream.add ('>');
}

class wstream_stream
{
  wStream *last;
public:
  wstream_stream (wStream &stream)
       : last (xwstream_stream_wstream (xsymbol_value (Vwstream_stream)))
    {
      xwstream_stream_wstream (xsymbol_value (Vwstream_stream)) = &stream;
    }
  ~wstream_stream ()
    {
      xwstream_stream_wstream (xsymbol_value (Vwstream_stream)) = last;
    }
};

static void
print_struct_data (wStream &stream, const print_control &pc,
                   lisp object, int level)
{
  lisp def = xstrdata_def (object);
  int condp = (Fsi_structure_subtypep (def, xsymbol_value (QCcondition)) != Qnil
               && xstrdef_report (def) != Qnil);

  if (pc.readably || pc.escape
      || (xstrdef_print_function (def) == Qnil && !condp))
    {
      stream.add ("#S(");
      print_sexp (stream, pc, xstrdef_name (def), level);
      int n = min (xstrdata_nslots (object), xstrdef_nslots (def));
      for (int i = 0; i < n; i++)
        {
          stream.add (' ');
          simple_print_string (stream, xsymbol_name (xstrdef_slotdesc (def) [i].name));
          stream.add (' ');
          print_sexp (stream, pc, xstrdata_data (object) [i], level);
        }
      stream.add (')');
    }
  else if (condp)
    {
      protect_gc gcpro (object);
      if (!special_condition_report (stream, object))
        {
          wstream_stream w (stream);
          funcall_2 (xstrdef_report (def), object, xsymbol_value (Vwstream_stream));
        }
    }
  else
    {
      wstream_stream w (stream);
      funcall_3 (xstrdef_print_function (def), object,
                 xsymbol_value (Vwstream_stream), make_fixnum (level));
    }
}

static inline void
print_window (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "window");
}

static void
print_buffer_name (wStream &stream, const Buffer *bp)
{
  simple_print_string (stream, bp->lbuffer_name);
  if (bp->b_version != 1)
    {
      char b[64];
      sprintf (b, "<%d>", bp->b_version);
      stream.add (b);
    }
}

static void
print_buffer (wStream &stream, const print_control &, lisp object)
{
  if (!xbuffer_bp (object))
    print_unreadable_object (stream, object, "deleted-buffer");
  else
    {
      stream.add ("#<buffer: ");
      print_buffer_name (stream, xbuffer_bp (object));
      stream.add ('>');
    }
}

static inline void
print_syntax_table (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "syntax-table");
}

static void
print_marker (wStream &stream, const print_control &, lisp object)
{
  if (!xmarker_buffer (object))
    print_unreadable_object (stream, object, "deleted-marker");
  else
    {
      stream.add ("#<marker: ");
      print_buffer_name (stream, xmarker_buffer (object));
      if (xmarker_point (object) == NO_MARK_SET)
        stream.add (": -");
      else
        {
          char b[64];
          sprintf (b, ": %u", xmarker_point (object));
          stream.add (b);
        }
      stream.add ('>');
    }
}

static inline void
print_regexp (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<compiled regular expression: ");
  simple_print_string (stream, xregexp_source (object));
  stream.add ('>');
}

static inline void
print_process (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "process");
}

static inline void
print_win32_menu (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "menu");
}

static inline void
print_win32_dde_handle (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "DDE handle");
}

static inline void
print_chunk (wStream &stream, const print_control &pc, lisp object)
{
  char buf[32];
  stream.add ("#<chunk :type ");
  print_symbol (stream, pc, xchunk_type (object));
  stream.add (" :size ");
  stream.add (store_uint (buf + sizeof buf, xchunk_size (object)));
  print_object_address (stream, object);
  stream.add (">");
}

static inline void
print_readtable (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "readtable");
}

static void
print_dll_module (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<DLL-module: ");
  simple_print_string (stream, xdll_module_name (object));
  stream.add ('>');
}

static void
print_dll_function (wStream &stream, const print_control &, lisp object)
{
  stream.add ("#<c-function: ");
  simple_print_string (stream, xdll_function_name (object));
  stream.add ('>');
}

static void
print_c_callable (wStream &stream, const print_control &pc, lisp object)
{
  stream.add ("#<c-callable: ");
  if (xc_callable_convention (object) == CALLING_CONVENTION_STDCALL)
    stream.add ("stdcall ");
  else
    stream.add ("cdecl ");
  print_sexp (stream, pc, xc_callable_function (object), 0);
  stream.add ('>');
}

static inline void
print_oledata (wStream &stream, const print_control &, lisp object)
{
  set_oledata_name (object);
  stream.add ("#<oledata:");
  if (xoledata_name (object) && xoledata_name (object) != Qnil)
    {
      stream.add (" ");
      simple_print_string (stream, xoledata_name (object));
    }
  print_object_address (stream, object);
  stream.add ('>');
}

static inline void
print_wait_object (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "wait-object");
}

static void
print_char_encoding (wStream &stream, const print_control &pc, lisp object)
{
  if ((pc.readably || pc.escape) && xsymbol_value (Vread_eval) != Qnil)
    {
      stream.add ("#.");
      object = make_char_encoding_constructor (object);
      protect_gc gcpro (object);
      print_sexp (stream, pc, object, 0);
    }
  else
    {
      stream.add ("#<char-encoding: ");
      simple_print_string (stream, xchar_encoding_name (object));
      stream.add ('>');
    }
}

static inline void
print_environment (wStream &stream, const print_control &, lisp object)
{
  print_unreadable_object (stream, object, "environment-object");
}

/*GENERIC_FUNCTION*/
static void
print_sexp (wStream &stream, const print_control &pc, lisp object, int level)
{
  assert (object);

  if (immediatep (object))
    {
      if (short_int_p (object))
        print_integer (stream, pc, object);
      else if (charp (object))
        print_char (stream, pc, xchar_code (object));
      else if (messagep (object))
        print_message (stream, pc, xmessage_code (object));
      else
        assert (0);
    }
  else
    {
      check_stack_overflow ();

      if (pc.circle
          && pc.pr_circle->find (stream, object, 0) == circle_object::referenced)
        return;

      switch (object_typeof (object))
        {
        case Tcons:
          print_list (stream, pc, object, level + 1);
          break;

        case Tsymbol:
          print_symbol (stream, pc, object);
          break;

        case Tlong_int:
        case Tbignum:
          print_integer (stream, pc, object);
          break;

        case Tfraction:
          print_fraction (stream, pc, object);
          break;

        case Tsingle_float:
          print_flonum (stream, pc, object);
          break;

        case Tdouble_float:
          print_flonum (stream, pc, object);
          break;

        case Tcomplex:
          print_complex (stream, pc, object);
          break;

        case Tsimple_string:
        case Tcomplex_string:
          print_string (stream, pc, object);
          break;

        case Tsimple_vector:
        case Tcomplex_vector:
          print_vector (stream, pc, object, level + 1);
          break;

        case Tarray:
        case Tstring_array:
          print_array (stream, pc, object, level + 1);
          break;

        case Tfunction:
          print_function (stream, pc, object);
          break;

        case Tclosure:
          print_closure (stream, pc, object);
          break;

        case Thash_table:
          print_hash_table (stream, pc, object);
          break;

        case Tstream:
          print_stream (stream, pc, object);
          break;

        case Tpackage:
          print_package (stream, pc, object);
          break;

        case Terror:
          print_error (stream, pc, object);
          break;

        case Trandom_state:
          print_random_state (stream, pc, object);
          break;

        case Tstruct_def:
          print_struct_def (stream, pc, object);
          break;

        case Tstruct_data:
          print_struct_data (stream, pc, object, level + 1);
          break;

        case Treadtable:
          print_readtable (stream, pc, object);
          break;

        case Twindow:
          print_window (stream, pc, object);
          break;

        case Tbuffer:
          print_buffer (stream, pc, object);
          break;

        case Tsyntax_table:
          print_syntax_table (stream, pc, object);
          break;

        case Tmarker:
          print_marker (stream, pc, object);
          break;

        case Tregexp:
          print_regexp (stream, pc, object);
          break;

        case Tprocess:
          print_process (stream, pc, object);
          break;

        case Twin32_menu:
          print_win32_menu (stream, pc, object);
          break;

        case Twin32_dde_handle:
          print_win32_dde_handle (stream, pc, object);
          break;

        case Tchunk:
          print_chunk (stream, pc, object);
          break;

        case Tdll_module:
          print_dll_module (stream, pc, object);
          break;

        case Tdll_function:
          print_dll_function (stream, pc, object);
          break;

        case Tc_callable:
          print_c_callable (stream, pc, object);
          break;

        case Toledata:
          print_oledata (stream, pc, object);
          break;

        case Twait_object:
          print_wait_object (stream, pc, object);
          break;

        case Tchar_encoding:
          print_char_encoding (stream, pc, object);
          break;

        case Tenvironment:
          print_environment (stream, pc, object);
          break;

        default:
          assert (0);
          break;
        }
    }
}

class Format
{
  enum {PARAM_MAX = 7};
  enum {FMT_INT, FMT_CHAR, FMT_NIL};
  struct
    {
      int type;
      int value;
    } param[PARAM_MAX];
  int nparams;
  int atsign;
  int colon;

  const Char *ctl;
  const Char *ctle;
  lisp *args;
  int nargs;
  int index;
  bool backward_compat_p;

  enum {IL_ILLEGAL, IL_EXIT, IL_NOT_EXIT};
  int in_loop;

  Char fetch (message_code = Eend_of_ctl_string);
  int args_left () const;
  lisp getarg ();
  int param_is_given (int = 0) const;
  void max_param (int) const;
  void not_colon () const;
  void not_atsign () const;
  void not_colon_atsign () const;
  void absolute_goto (int);
  void relative_goto (int);
  Char char_param (int, Char) const;
  int integer_param (int, int) const;
  int integer_param_min (int, int, int) const;
  int integer_param_minmax (int, int, int, int) const;
  void error (message_code) const;

  void s_exp (wStream &, Char);
  void integer (wStream &, lisp, int, int);
  void integer (wStream &, int);
  void radix (wStream &);
  void plural (wStream &);
  void character (wStream &);
  void fixed_format (wStream &);
  void exp_format (wStream &);
  void general_format (wStream &);
  void dollar_format (wStream &);
  void ampersand (wStream &);
  void percent (wStream &);
  void bar (wStream &);
  void tilde (wStream &);
  void newline (wStream &);
  void tabulate (wStream &);
  void asterisk (wStream &);
  void write (wStream &);
  void indirection (wStream &);
  void case_conversion (wStream &);
  void conditional (wStream &);
  void iteration (wStream &);
  void up_and_out (wStream &);

  Char colon_atsign (Char);
  Char prefix_parameters ();
  Format (const Char *, int, int, bool);
  void setarg (lisp *, lisp);
  void process (wStream &);

  friend class SaveCtlString;
  friend void format_internal (wStream &, const Char *, int, lisp, int, bool);
  friend lisp format (lisp, lisp, lisp, bool);
};

class SaveCtlString
{
  Format &fmt;
  const Char *ctl;
  const Char *ctle;
  SaveCtlString (Format &, const Char *, int);
  ~SaveCtlString ();
  friend Format;
};

class UpAndOut
{
  UpAndOut (int c) : colon (c) {}
public:
  int colon;
  friend Format;
};

inline
SaveCtlString::SaveCtlString (Format &f, const Char *p, int size)
     : fmt (f)
{
  ctl = fmt.ctl;
  ctle = fmt.ctle;
  fmt.ctl = p;
  fmt.ctle = p + size;
}

inline
SaveCtlString::~SaveCtlString ()
{
  fmt.ctl = ctl;
  fmt.ctle = ctle;
}

Format::Format (const Char *p, int size, int il, bool backward_compat_p)
     : backward_compat_p (backward_compat_p)
{
  ctl = p;
  ctle = p + size;
  index = 0;
  in_loop = il;
}

void
Format::setarg (lisp *v, lisp l)
{
  args = v;
  int i;
  for (i = 0; consp (l); l = xcdr (l), i++)
    *v++ = xcar (l);
  nargs = i;
}

inline void
Format::error (message_code e) const
{
  FEformat_error (e);
}

inline Char
Format::fetch (message_code e)
{
  if (ctl == ctle)
    error (e);
  return *ctl++;
}

inline int
Format::args_left () const
{
  return index < nargs;
}

inline lisp
Format::getarg ()
{
  if (!args_left ())
    error (Eformat_argument_exhausted);
  return args[index++];
}

inline int
Format::param_is_given (int n) const
{
  return n < nparams && param[n].type != FMT_NIL;
}

inline void
Format::max_param (int n) const
{
  if (nparams > n)
    error (Etoo_many_parameters);
}

inline void
Format::not_colon () const
{
  if (colon)
    error (Eillegal_colon);
}

inline void
Format::not_atsign () const
{
  if (atsign)
    error (Eillegal_atsign);
}

inline void
Format::not_colon_atsign () const
{
  if (colon && atsign)
    error (Eillegal_colon_atsign);
}

void
Format::absolute_goto (int n)
{
  if (n < 0)
    error (Ecannot_backup);
  if (n > nargs)
    error (Ecannot_goto);
  index = n;
}

inline void
Format::relative_goto (int n)
{
  absolute_goto (index + n);
}

Char
Format::char_param (int i, Char defalt) const
{
  assert (i >= 0 && i < PARAM_MAX);
  if (i >= nparams || param[i].type == FMT_NIL)
    return defalt;
  if (param[i].type != FMT_CHAR)
    error (Eillegal_parameter_type);
  return Char (param[i].value);
}

int
Format::integer_param (int i, int defalt) const
{
  assert (i >= 0 && i < PARAM_MAX);
  if (i >= nparams || param[i].type == FMT_NIL)
    return defalt;
  if (param[i].type != FMT_INT)
    error (Eillegal_parameter_type);
  return param[i].value;
}

int
Format::integer_param_min (int i, int defalt, int min) const
{
  int n = integer_param (i, defalt);
  if (n < min)
    error (Eparameter_out_of_range);
  return n;
}

int
Format::integer_param_minmax (int i, int defalt, int min, int max) const
{
  int n = integer_param (i, defalt);
  if (n < min || n > max)
    error (Eparameter_out_of_range);
  return n;
}

void
Format::s_exp (wStream &stream, Char c)
{
  max_param (4);
  int mincol = integer_param_min (0, 0, 0);
  int colinc = integer_param_min (1, 1, 1);
  int minpad = integer_param_min (2, 0, 0);
  Char padchar = char_param (3, ' ');

  int ocol = stream.columns ();
  wStream tem (ocol);

  lisp x = getarg ();
  if (colon && x == Qnil)
    tem.add ("()");
  else
    {
      print_circle circle;
      print_control pc (circle);
      if (c == 'a')
        pc.princ ();
      else
        pc.prin1 ();
      pc.setup_circle (x);
      print_sexp (tem, pc, x, 0);
    }

  int ncol;
  if (mincol)
    {
      int l = tem.columns () - ocol;
      ncol = max (l + minpad, mincol);
      ncol = (ncol + colinc - 1) / colinc * colinc - l;
    }
  else
    ncol = minpad;

  if (backward_compat_p && stringp (x))
    atsign = !atsign;
  if (!atsign)
    {
      stream.add (tem);
      stream.fill (padchar, ncol);
    }
  else
    {
      stream.fill (padchar, ncol);
      stream.add (tem);
    }
}

void
Format::integer (wStream &stream, lisp linteger, int base, int istart)
{
  max_param (4 + istart);
  int mincol = integer_param_min (istart, 0, 0);
  Char padchar = char_param (istart + 1, ' ');
  Char commachar = char_param (istart + 2, ',');
  int commaint = integer_param_min (istart + 3, 3, 1);

  print_control pc (base);
  int fmtw = print_integer_width (linteger, base);
  char *b0 = (char *)alloca (fmtw);
  char *be = b0 + fmtw;
  char *b = print_integer (be, pc, linteger, atsign);
  int l = be - b - 1;
  if (!colon)
    {
      if (mincol > l)
        stream.fill (padchar, mincol - l);
      stream.add (b);
    }
  else
    {
      l += (l - (digit_char_p (*b) ? 1 : 2)) / commaint;
      if (mincol > 0)
        stream.fill (padchar, mincol - l);
      if (!digit_char_p (*b))
        stream.add (*b++);
      stream.add (*b++);
      be--;
      while (b < be)
        {
          if (!((b - be) % commaint))
            stream.add (commachar);
          stream.add (*b++);
        }
    }
}

inline void
Format::integer (wStream &stream, int base)
{
  integer (stream, getarg (), base, 0);
}

static void
fmt_roman (wStream &stream, int n, Char one, Char five, Char ten)
{
  if (n <= 3)
    {
      for (int i = 0; i < n; i++)
        stream.add (one);
    }
  else if (n == 4)
    {
      stream.add (one);
      stream.add (five);
    }
  else if (n <= 8)
    {
      stream.add (five);
      for (int i = 5; i < n; i++)
        stream.add (one);
    }
  else
    {
      stream.add (one);
      stream.add (ten);
    }
}

static void
fmt_old_roman (wStream &stream, int n, Char one, Char five)
{
  if (n >= 5)
    {
      stream.add (five);
      n -= 5;
    }
  for (int i = 0; i < n; i++)
    stream.add (one);
}

static int
fmt_thousand (wStream &stream, int n, int o, int f)
{
  static const char *const numeral[] =
    {
      "zero", "one", "two", "three", "four",
      "five", "six", "seven", "eight", "nine",
      "ten", "eleven", "twelve", "thirteen", "fourteen",
      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
    };
  static const char *const numeral_10[] =
    {
      "twenty", "thirty", "forty", "fifty",
      "sixty", "seventy", "eighty", "ninety",
    };
  static const char *const ordinal[] =
    {
      "zeroth", "first", "second", "third", "fourth",
      "fifth", "sixth", "seventh", "eighth", "ninth",
      "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
      "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
    };
  static const char *const ordinal_10[] =
    {
      "twentieth", "thirtieth", "fortieth", "fiftieth",
      "sixtieth", "seventieth", "eightieth", "ninetieth",
    };

  if (n >= 100)
    {
      if (f)
        stream.add (' ');
      stream.add (numeral[n / 100]);
      stream.add (" hundred");
      n %= 100;
      if (!n)
        {
          if (o)
            stream.add ("th");
          return 1;
        }
      stream.add (" and");
      f = 1;
    }
  if (n >= 20)
    {
      if (f)
        stream.add (' ');
      stream.add ((o && !(n % 10) ? ordinal_10 : numeral_10) [n / 10 - 2]);
      n %= 10;
      if (!n)
        return 1;
      stream.add ('-');
      f = 0;
    }
  if (f)
    stream.add (' ');
  stream.add ((o ? ordinal : numeral) [n]);
  return 1;
}

static int
fmt_x_illion (wStream &stream, int n, int o, int f, int t)
{
  static const char *const big_numeral[] =
    {
      "thousand",
      "million",
    };

  if (n >= 1000)
    {
      f = fmt_x_illion (stream, n / 1000, o && !(n % 1000), f, t + 1);
      n %= 1000;
      if (n)
        {
          stream.add (',');
          f = 1;
        }
    }
  if (n)
    {
      f = fmt_thousand (stream, n, o, f);
      if (t)
        {
          if (f)
            stream.add (' ');
          stream.add (big_numeral[t - 1]);
          f = 1;
        }
    }
  return f;
}

void
Format::radix (wStream &stream)
{
  max_param (5);
  lisp linteger = getarg ();
  check_integer (linteger);
  if (nparams)
    {
      int r = integer_param (0, 10);
      if (r < 2 || r > 36)
        error (Efmt_r_requires_2_36);
      integer (stream, linteger, r, 1);
      return;
    }

  if (!bignump (linteger))
    {
      int n = fixnum_value (linteger);
      if (atsign)
        {
          if (colon)
            {
              if (n > 0 && n < 5000)
                {
                  fmt_old_roman (stream, n / 1000, 'M', 0);
                  fmt_old_roman (stream, n / 100 % 10, 'C', 'D');
                  fmt_old_roman (stream, n / 10 % 10, 'X', 'L');
                  fmt_old_roman (stream, n % 10, 'I', 'V');
                  return;
                }
            }
          else
            {
              if (n > 0 && n < 4000)
                {
                  fmt_roman (stream, n / 1000, 'M', 0, 0);
                  fmt_roman (stream, n / 100 % 10, 'C', 'D', 'M');
                  fmt_roman (stream, n / 10 % 10, 'X', 'L', 'C');
                  fmt_roman (stream, n % 10, 'I', 'V', 'X');
                  return;
                }
            }
        }
      else
        {
          int m = n < 0 ? -n : n;
          if (m < 1000000000)
            {
              if (!n)
                stream.add (colon ? "zeroth" : "zero");
              else
                {
                  if (n < 0)
                    stream.add ("minus ");
                  fmt_x_illion (stream, m, colon, 0, 0);
                }
              return;
            }
        }
    }

  print_control pc (10);
  print_integer (stream, pc, linteger);
}

void
Format::plural (wStream &stream)
{
  max_param (0);
  if (colon)
    relative_goto (-1);
  int f = Feql (getarg (), make_fixnum (1)) != Qnil;
  if (atsign)
    stream.add (f ? "y" : "ies");
  else if (!f)
    stream.add ('s');
}

void
Format::character (wStream &stream)
{
  max_param (0);
  lisp x = getarg ();
  check_char (x);
  Char c = xchar_code (x);
  if (colon)
    print_char (stream, c, 0);
  else if (atsign)
    {
      stream.add ("#\\");
      print_char (stream, c, 1);
    }
  else
    stream.add (c);
}

static int
fixed_fmt_width (int sign, int atsign, int exp, int d)
{
  int n = 0;
  if (sign < 0 || atsign)
    n++;
  if (exp < 0)
    n += 2 + d;
  else
    n += exp + 2 + d;
  return n;
}

void
Format::fixed_format (wStream &stream)
{
  max_param (5);
  not_colon ();
  int w = integer_param_min (0, 0, 0);
  int d = integer_param_min (1, 0, 0);
  int k = integer_param (2, 0);
  Char overflow = char_param (3, 0);
  Char padchar = char_param (4, ' ');
  lisp lnumber = getarg ();

  fmt_float f (lnumber);

  if (!f.b0)
    {
      if (param_is_given (0))
        {
          int l = strlen (f.buf);
          if (l > w && param_is_given (3))
            stream.fill (overflow, w);
          else
            {
              if (w > l)
                stream.fill (padchar, w - l);
              stream.add (f.buf);
            }
        }
      else
        stream.add (f.buf);
      return;
    }

  if (!param_is_given (0) && !param_is_given (1) && !param_is_given(2))
    {
      print_control pc (10);
      if (atsign && f.sign > 0) stream.add('+');
      print_flonum (stream, pc, lnumber);
      return;
    }

  f.exp += k;
  if (!param_is_given (0))
    {
      if (f.exp > 100 || f.exp < -100)
        {
          nparams = 0;
          atsign = 0;
          relative_goto (-1);
          exp_format (stream);
          return;
        }
      if (param_is_given (1))
        w = fixed_fmt_width (f.sign, atsign, f.exp, d);
      else
        {
          int n = 0;
          if (f.sign < 0 || atsign)
            n++;
          if (f.exp < 0)
            n += 1 - f.exp + max (f.be - f.b0, 1);
          else
            n += 1 + max (f.exp, f.be - f.b0);
          w = n;
        }
    }

  if (param_is_given (1))
    f.roundf (d);
  else
    {
      d = max (w - fixed_fmt_width (f.sign, atsign, f.exp, d), 0);
      d = max (1, min (d, f.be - f.b0 - f.exp - 1));
      f.roundf (d);
      d = max (1, min (d, f.be - f.b0 - f.exp - 1));
    }

  int no_lead_zero = 0;
  int m = fixed_fmt_width (f.sign, atsign, f.exp, d);
  if (m < w)
    stream.fill (padchar, w - m);
  else
    {
      if (m == w + 1)
        {
          if (f.exp < 0)
            {
              no_lead_zero = 1;
              m--;
            }
          else if (!param_is_given (1))
            {
              d--;
              m--;
            }
        }
      if (m > w && param_is_given (3))
        {
          stream.fill (overflow, w);
          return;
        }
    }

  if (f.sign < 0)
    stream.add ('-');
  else if (atsign)
    stream.add ('+');

  if (f.exp < 0)
    {
      if (!no_lead_zero)
        stream.add ('0');
      stream.add ('.');
      m = -1 - f.exp;
      if (m < d)
        {
          stream.fill ('0', m);
          for (char *b = f.b0; m < d && *b; m++, b++)
            stream.add (*b);
          stream.fill ('0', d - m);
        }
      else
        stream.fill ('0', d);
    }
  else
    {
      char *b = f.b0;
      int i;
      for (i = -1; i < f.exp && *b; i++, b++)
        stream.add (*b);
      stream.fill ('0', f.exp - i);
      stream.add ('.');
      for (m = 0; m < d && *b; m++, b++)
        stream.add (*b);
      stream.fill ('0', d - m);
    }
}

static int
exp_width (int exp)
{
  int n;
  for (n = 0; exp; n++, exp /= 10)
    ;
  return n ? n : 1;
}

void
Format::exp_format (wStream &stream)
{
  max_param (7);
  not_colon ();
  int w = integer_param_min (0, 0, 0);
  int d = integer_param_min (1, 0, 0);
  int e = integer_param_min (2, 0, 0);
  int k = integer_param (3, 1);
  Char overflow = char_param (4, 0);
  Char padchar = char_param (5, ' ');
  lisp lnumber = getarg ();
  fmt_float f (lnumber);
  Char expchar = char_param (6, (f.fmt != default_float_format ()
                                 ? f.fmt & ~NF_FLOAT : 'e'));

  if (!f.b0)
    {
      if (param_is_given (0))
        {
          int l = strlen (f.buf);
          if (l > w && param_is_given (4))
            stream.fill (overflow, w);
          else
            {
              if (w > l)
                stream.fill (padchar, w - l);
              stream.add (f.buf);
            }
        }
      else
        stream.add (f.buf);
      return;
    }

  if (!param_is_given (2))
    e = exp_width (f.exp - k + 1);

  if (!param_is_given (1))
    d = f.be - f.b0 - 1;
  if (k > 0)
    d = max (d, k - 1);
  else if (k < 0)
    d = max (d, -k + 1);

  int m = e + 2;
  if (f.sign < 0 || atsign)
    m++;
  m += d + 2;

  int no_lead_zero = k <= 0 && param_is_given (0) && m == w + 1;

  if (param_is_given (4))
    {
      if (param_is_given (0) && m > w && !no_lead_zero)
        {
          stream.fill (overflow, w);
          return;
        }
      if (param_is_given (2) && e < exp_width (f.exp - k + 1))
        {
          stream.fill (overflow, w);
          return;
        }
    }

  stream.fill (padchar, w - m);

  if (f.sign < 0)
    stream.add ('-');
  else if (atsign)
    stream.add ('+');

  if (!k)
    {
      if (!no_lead_zero)
        stream.add ('0');
      stream.add ('.');
      f.round (d);
      char *b = f.b0;
      int i;
      for (i = d; i > 0 && *b; i--, b++)
        stream.add (*b);
      stream.fill ('0', i);
    }
  else if (k > 0)
    {
      f.round (d + 1);
      char *b = f.b0;
      int i;
      for (i = k; i > 0 && *b; i--, b++)
        stream.add (*b);
      stream.fill ('0', i);
      stream.add ('.');
      for (i = d - k + 1; i > 0 && *b; i--, b++)
        stream.add (*b);
      stream.fill ('0', i);
    }
  else
    {
      f.round (d + k);
      if (!no_lead_zero)
        stream.add ('0');
      stream.add ('.');
      stream.fill ('0', -k);
      char *b = f.b0;
      int i;
      for (i = d + k; i > 0 && *b; i--, b++)
        stream.add (*b);
      stream.fill ('0', i);
    }
  stream.add (expchar);
  int n = f.exp - k + 1;
  if (n < 0)
    {
      stream.add ('-');
      n = -n;
    }
  else
    stream.add ('+');
  char buf[32];
  char *b = store_uint (buf + sizeof buf, n);
  e -= buf + sizeof buf - b - 1;
  for (; e > 0; e--)
    *--b = '0';
  stream.add (b);
}

void
Format::general_format (wStream &stream)
{
  max_param (7);
  not_colon ();
  lisp lnumber = getarg ();
  relative_goto (-1);

  fmt_float f (lnumber);
  if (!f.b0)
    {
      exp_format (stream);
      return;
    }

  int d = integer_param_min (1, 0, 0);
  int n = f.exp + 1;
  if (!param_is_given (1))
    {
      int l = f.be - f.b0;
      int q;
      if (f.exp < 0)
        q = 1 - f.exp + l;
      else if (f.exp >= l)
        q = f.exp + 2;
      else
        q = f.exp + 1;
      d = max (q, min (n, 7));
    }
  int dd = d - n;
  if (dd >= 0 && dd <= d)
    {
      int ee = param_is_given (2) ? integer_param_min (2, 0, 0) + 2 : 4;
      Char overflow = char_param (4, 0);
      Char padchar = char_param (5, ' ');
      if (param_is_given (0))
        param[0].value = integer_param_min (0, 0, 0) - ee;
      else
        param[0].type = FMT_NIL;
      param[1].type = FMT_INT;
      param[1].value = dd;
      param[2].type = FMT_NIL;
      if (param_is_given (4))
        {
          param[3].type = FMT_CHAR;
          param[3].value = overflow;
        }
      else
        param[3].type = FMT_NIL;
      param[4].type = FMT_CHAR;
      param[4].value = padchar;
      nparams = 5;
      fixed_format (stream);

      param[0].type = FMT_INT;
      param[0].value = ee;
      atsign = 1;
      nparams = 1;
      tabulate (stream);
    }
  else
    exp_format (stream);
}

void
Format::dollar_format (wStream &stream)
{
  max_param (4);
  lisp lnumber = getarg ();

  fmt_float f (lnumber);
  if (!f.b0)
    {
      relative_goto (-1);
      exp_format (stream);
      return;
    }

  int d = integer_param_min (0, 2, 0);
  int n = integer_param_min (1, 1, 0);
  int w = integer_param_min (2, 0, 0);
  Char padchar = char_param (3, ' ');

  f.roundf (d);

  int m = 1;
  if (f.sign < 0 || atsign)
    m++;
  m += max (n, f.exp >= 0 ? f.exp + 1 : 1);
  m += d;

  if (m >= w && m >= 100)
    {
      relative_goto (-1);
      nparams = 7;
      if (param_is_given (2))
        {
          param[0].type = FMT_INT;
          param[0].value = w;
        }
      else
        param[0].type = FMT_NIL;
      param[1].type = FMT_INT;
      param[1].value = d + n - 1;
      for (int i = 2; i < 6; i++)
        param[i].type = FMT_NIL;
      param[6].type = FMT_CHAR;
      param[6].value = padchar;
      exp_format (stream);
      return;
    }

  if (colon)
    {
      if (f.sign < 0)
        stream.add ('-');
      else if (atsign)
        stream.add ('+');
    }
  stream.fill (padchar, w - m);
  if (!colon)
    {
      if (f.sign < 0)
        stream.add ('-');
      else if (atsign)
        stream.add ('+');
    }

  char *b = f.b0;
  if (f.exp < 0)
    stream.fill ('0', n);
  else
    {
      stream.fill ('0', n - f.exp - 1);
      int i;
      for (i = -1; i < f.exp && *b; i++, b++)
        stream.add (*b);
      stream.fill ('0', f.exp - i);
    }
  stream.add ('.');
  int i;
  for (i = 0; i < d && *b; i++, b++)
    stream.add (*b);
  stream.fill ('0', d - i);
}

void
Format::percent (wStream &stream)
{
  max_param (1);
  not_colon ();
  not_atsign ();
  stream.fill ('\n', integer_param_min (0, 1, 0));
}

void
Format::ampersand (wStream &stream)
{
  max_param (1);
  not_colon ();
  not_atsign ();
  int n = integer_param_min (0, 1, 0);
  if (n)
    {
      if (stream.columns ())
        stream.add ('\n');
      stream.fill ('\n', n - 1);
    }
}

void
Format::bar (wStream &stream)
{
  max_param (1);
  not_colon ();
  not_atsign ();
  stream.fill ('\f', integer_param_min (0, 1, 0));
}

void
Format::tilde (wStream &stream)
{
  max_param (1);
  not_colon ();
  not_atsign ();
  stream.fill ('~', integer_param_min (0, 1, 0));
}

void
Format::newline (wStream &stream)
{
  max_param (0);
  not_colon_atsign ();
  if (atsign)
    stream.add ('\n');
  if (!colon)
    for (; ctl < ctle && (*ctl == ' ' || *ctl == '\t'); ctl++)
      ;
}

void
Format::tabulate (wStream &stream)
{
  max_param (2);
  not_colon ();
  not_colon_atsign ();
  if (!atsign)
    {
      int colnum = integer_param_min (0, 1, 0);
      int colinc = integer_param_min (1, 1, 0);
      int col = stream.columns ();
      if (col >= colnum)
        {
          if (!colinc)
            return;
          colnum += (col - colnum + colinc) / colinc * colinc;
        }
      stream.fill (' ', colnum - col);
    }
  else
    {
      int colrel = integer_param_min (0, 1, 0);
      int colinc = integer_param_min (1, 1, 1);
      int col = colinc - (stream.columns () + colrel) % colinc;
      if (col == colinc)
        col = 0;
      stream.fill (' ', col + colrel);
    }
}

void
Format::asterisk (wStream &stream)
{
  max_param (1);
  not_colon_atsign ();
  if (atsign)
    absolute_goto (integer_param (0, 0));
  else
    {
      int n = integer_param (0, 1);
      relative_goto (colon ? -n : n);
    }
}

void
Format::write (wStream &stream)
{
  lisp x = getarg ();
  print_circle circle;
  print_control pc (circle);
  pc.setup_circle (x);
  print_sexp (stream, pc, x, 0);
}

void
Format::indirection (wStream &stream)
{
  max_param (0);
  not_colon ();
  not_colon_atsign ();
  lisp string = getarg ();
  check_string (string);
  try
    {
      if (!atsign)
        format_internal (stream, xstring_contents (string), xstring_length (string),
                         getarg (), IL_ILLEGAL, backward_compat_p);
      else
        {
          SaveCtlString x (*this, xstring_contents (string), xstring_length (string));
          process (stream);
        }
    }
  catch (UpAndOut e)
    {
      if (e.colon)
        error (Eillegal_colon_circumflex);
    }
}

static const Char *
skip_ctl_string (const Char *p, const Char *pe, Char search, Char open)
{
  while (p < pe)
    {
      Char c = *p++;
      if (c != '~')
        continue;

      while (p < pe)
        {
          c = *p++;
          switch (c)
            {
            case '\'':
              if (p == pe)
                goto fail;
              p++;
              break;;

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
            case '+':
            case '-':
            case ',':
            case ':':
            case '@':
            case 'V':
            case 'v':
            case '#':
              break;

            case '[':
              p = skip_ctl_string (p, pe, ']', '[');
              goto done;

            case '{':
              p = skip_ctl_string (p, pe, '}', '{');
              goto done;

            case '(':
              p = skip_ctl_string (p, pe, ')', '(');
              goto done;

            case '<':
              p = skip_ctl_string (p, pe, '>', '<');
              goto done;

            case '}':
              if (open == '{')
                return p;
              goto fail;

            case ']':
              if (open == '[')
                return p;
              goto fail;

            case ')':
              if (open == '(')
                return p;
              goto fail;

            case '>':
              if (open == '<')
                return p;
              goto fail;

            case ';':
              if (search == ';')
                return p;
              goto done;

            default:
              goto done;
            }
        }
    done:
      ;
    }
fail:
  switch (open)
    {
    default:
    case '(':
      FEprogram_error (Eclose_paren_expected);

    case '{':
      FEprogram_error (Eclose_brace_expected);

    case '[':
      FEprogram_error (Eclose_bracket_expected);

    case '<':
      FEprogram_error (Egreater_than_expected);
    }
  return 0;
}

void
Format::case_conversion (wStream &stream)
{
  max_param (0);
  const Char *next = skip_ctl_string (ctl, ctle, ')', '(');
  const Char *p, *pe;
  for (p = ctl, pe = next - 1; *pe != '~'; pe--)
    ;

  ctl = next;
  int colon = Format::colon;
  int atsign = Format::atsign;

  wStream tem (stream.columns ());
  try
    {
      SaveCtlString x (*this, p, pe - p);
      process (tem);
    }
  catch (UpAndOut)
    {
      stream.case_conversion (tem, colon, atsign);
      throw;
    }
  stream.case_conversion (tem, colon, atsign);
}

void
Format::conditional (wStream &stream)
{
  not_colon_atsign ();
  const Char *next = skip_ctl_string (ctl, ctle, ']', '[');
  const Char *p, *pe;

  if (colon)
    {
      max_param (0);
      p = skip_ctl_string (ctl, next, ';', '[');
      pe = skip_ctl_string (p, next, ';', '[');
      if (pe != next)
        error (Etoo_many_conditional_form);
      if (getarg () == Qnil)
        {
          pe = p;
          p = ctl;
        }
      else
        pe = next;
    }
  else if (atsign)
    {
      max_param (0);
      p = skip_ctl_string (ctl, next, ';', '[');
      if (p != next)
        error (Etoo_many_conditional_form);
      if (getarg () == Qnil)
        goto nomatch;
      relative_goto (-1);
      p = ctl;
      pe = next;
    }
  else
    {
      max_param (1);
      int n;
      if (!param_is_given ())
        n = fixnum_value (getarg ());
      else
        n = integer_param (0, 0);
      const Char *match = 0, *matche;
      const Char *defalt = 0;
      p = ctl;
      for (int i = 0;; i++)
        {
          pe = skip_ctl_string (p, next, ';', '[');
          if (i == n)
            {
              match = p;
              matche = pe;
            }
          if (pe == next)
            {
              if (p[-1] == ';' && p[-2] == ':')
                defalt = p;
              break;
            }
          p = pe;
        }
      if (match)
        {
          p = match;
          pe = matche;
        }
      else if (defalt)
        {
          p = defalt;
          pe = next;
        }
      else
        goto nomatch;
    }

  for (pe--; *pe != '~'; pe--)
    ;
  {
    SaveCtlString x (*this, p, pe - p);
    process (stream);
  }

nomatch:
  ctl = next;
}

void
Format::iteration (wStream &stream)
{
  max_param (1);
  const Char *next = skip_ctl_string (ctl, ctle, '}', '{');
  const Char *p, *pe;
  for (p = ctl, pe = next - 1; *pe != '~'; pe--)
    ;
  int once_at_least = pe[1] == ':';
  if (p == pe)
    {
      lisp x = getarg ();
      check_string (x);
      p = xstring_contents (x);
      pe = p + xstring_length (x);
    }

  int limit = integer_param (0, INT_MAX);

  if (colon && atsign)
    {
      if (once_at_least)
        goto oal_1;
      while (args_left ())
        {
        oal_1:
          if (limit-- <= 0)
            break;
          try
            {
              lisp x = getarg ();
              format_internal (stream, p, pe - p, x, args_left () ? IL_NOT_EXIT : IL_EXIT,
                               backward_compat_p);
            }
          catch (UpAndOut e)
            {
              if (e.colon)
                break;
            }
        }
    }
  else if (colon)
    {
      lisp args = getarg ();
      if (once_at_least)
        goto oal_2;
      while (consp (args))
        {
        oal_2:
          if (limit-- <= 0)
            break;
          try
            {
              format_internal (stream, p, pe - p, xcar (args),
                               consp (xcdr (args)) ? IL_NOT_EXIT : IL_EXIT,
                               backward_compat_p);
            }
          catch (UpAndOut e)
            {
              if (e.colon)
                break;
            }
          args = xcdr (args);
        }
    }
  else if (atsign)
    {
      if (once_at_least)
        goto oal_3;
      while (args_left ())
        {
        oal_3:
          if (limit-- <= 0)
            break;
          try
            {
              SaveCtlString x (*this, p, pe - p);
              process (stream);
            }
          catch (UpAndOut e)
            {
              if (e.colon)
                error (Eillegal_colon_circumflex);
              break;
            }
          QUIT;
        }
    }
  else
    {
      lisp args = getarg ();
      lisp l = Flist_length (args);
      if (l == Qnil)
        error (Eargument_is_circle);
      Format f (p, pe - p, IL_ILLEGAL, backward_compat_p);
      lisp *v = (lisp *)alloca (sizeof (lisp) * fixnum_value (l));
      f.setarg (v, args);
      if (once_at_least)
        goto oal_4;
      while (f.args_left ())
        {
        oal_4:
          if (limit-- <= 0)
            break;
          try
            {
              f.ctl = p;
              f.ctle = pe;
              f.process (stream);
            }
          catch (UpAndOut)
            {
              break;
            }
          QUIT;
        }
    }

  ctl = next;
}

void
Format::up_and_out (wStream &)
{
  max_param (3);
  not_atsign ();

  if (colon && in_loop == IL_ILLEGAL)
    error (Eillegal_colon_circumflex);

  switch (nparams)
    {
    case 0:
      if (colon)
        {
          if (in_loop != IL_EXIT)
            return;
        }
      else
        {
          if (args_left ())
            return;
        }
      break;

    case 1:
      if (integer_param (0, 0))
        return;
      break;

    case 2:
      if (integer_param (0, 0) != integer_param (1, 1))
        return;
      break;

    case 3:
      {
        int i1 = integer_param (1, 1);
        if (integer_param (0, 0) > i1 || integer_param (2, 2) < i1)
          return;
        break;
      }
    }
  throw UpAndOut (colon);
}

Char
Format::colon_atsign (Char c)
{
  while (1)
    {
      if (c == ':')
        colon = 1;
      else if (c == '@')
        atsign = 1;
      else
        return c;
      c = fetch ();
    }
}

Char
Format::prefix_parameters ()
{
  Char c = colon_atsign (fetch ());
  while (1)
    {
      int sign = 0;
      if (c == '-' || c == '+')
        {
          sign = c == '-' ? -1 : 1;
          c = fetch (Esign_following_no_chars);
        }

      if (digit_char_p (c))
        {
          int n = 0;
          do
            {
              n = n * 10 + c - '0';
              c = fetch ();
            }
          while (digit_char_p (c));
          param[nparams].type = FMT_INT;
          param[nparams].value = sign < 0 ? -n : n;
          nparams++;
        }
      else
        {
          if (sign)
            error (Esign_following_not_digits);

          switch (c)
            {
            case '\'':
              param[nparams].type = FMT_CHAR;
              param[nparams].value = fetch (Equote_following_no_chars);
              nparams++;
              c = fetch ();
              break;

            case 'V':
            case 'v':
              {
                lisp x = getarg ();
                if (fixnump (x))
                  {
                    param[nparams].type = FMT_INT;
                    param[nparams].value = fixnum_value (x);
                    nparams++;
                  }
                else if (charp (x))
                  {
                    param[nparams].type = FMT_CHAR;
                    param[nparams].value = xchar_code (x);
                    nparams++;
                  }
                else if (x == Qnil)
                  {
                    param[nparams].type = FMT_NIL;
                    param[nparams].value = 0;
                    nparams++;
                  }
                else
                  error (Eillegal_fmt_V_parameter);
              }
              c = fetch ();
              break;

            case '#':
              param[nparams].type = FMT_INT;
              param[nparams].value = nargs - index;
              nparams++;
              c = fetch ();
              break;

            case ',':
              param[nparams++].type = FMT_NIL;
              break;
            }
        }

      if (c != ',')
        return colon_atsign (c);

      if (nparams == PARAM_MAX)
        error (Etoo_many_parameters);

      c = fetch ();
    }
}

void
Format::process (wStream &stream)
{
  while (ctl < ctle)
    {
      Char c = *ctl++;
      if (c != '~')
        {
          stream.add (c);
          continue;
        }

      colon = 0;
      atsign = 0;
      nparams = 0;
      c = char_downcase (prefix_parameters ());
      switch (c)
        {
        case 'a':
        case 's':
          s_exp (stream, c);
          break;

        case 'b':
          integer (stream, 2);
          break;

        case 'o':
          integer (stream, 8);
          break;

        case 'd':
          integer (stream, 10);
          break;

        case 'x':
          integer (stream, 16);
          break;

        case 'r':
          radix (stream);
          break;

        case 'p':
          plural (stream);
          break;

        case 'c':
          character (stream);
          break;

        case 'f':
          fixed_format (stream);
          break;

        case 'e':
          exp_format (stream);
          break;

        case 'g':
          general_format (stream);
          break;

        case '$':
          dollar_format (stream);
          break;

        case '%':
          percent (stream);
          break;

        case '&':
          ampersand (stream);
          break;

        case '|':
          bar (stream);
          break;

        case '~':
          tilde (stream);
          break;

        case '\n':
          newline (stream);
          break;

        case 't':
          tabulate (stream);
          break;

        case '*':
          asterisk (stream);
          break;

        case 'w':
          write (stream);
          break;

        case '?':
          indirection (stream);
          break;

        case '(':
          case_conversion (stream);
          break;

        case '[':
          conditional (stream);
          break;

        case '{':
          iteration (stream);
          break;

        case '^':
          up_and_out (stream);
          break;

        case '<':
          error (Ejustification_not_implemented);

        default:
          error (Eillegal_format_directive);
        }
    }
}

void
write_object (lisp object, lisp dest, lisp keys)
{
  check_stream (dest);
  wStreamsStream stream (dest);
  print_circle circle;
  print_control pc (circle, keys);
  pc.setup_circle (object);
  print_sexp (stream, pc, object, 0);
}

lisp
output_stream (lisp stream)
{
  if (!stream || stream == Qnil)
    stream = xsymbol_value (Vstandard_output);
  else if (stream == Qt)
    stream = xsymbol_value (Vterminal_io);
  check_stream (stream);
  return stream;
}

lisp
Fwrite (lisp object, lisp keys)
{
  write_object (object, output_stream (find_keyword (Kstream, keys, Qnil)), keys);
  return object;
}

lisp
Fwrite_char (lisp cc, lisp stream)
{
  check_char (cc);
  writec_stream (output_stream (stream), xchar_code (cc));
  return cc;
}

lisp
Fsi_stream_column (lisp stream)
{
  return make_fixnum (get_stream_column (output_stream (stream)));
}

lisp
Fsi_stream_line_number (lisp stream)
{
  return make_fixnum (stream_linenum (output_stream (stream)));
}

lisp
Fterpri (lisp stream)
{
  writec_stream (output_stream (stream), '\n');
  return Qnil;
}

lisp
Ffresh_line (lisp stream)
{
  stream = output_stream (stream);
  if (!get_stream_column (stream))
    return Qnil;
  writec_stream (stream, '\n');
  return Qt;
}

lisp
Ffinish_output (lisp stream)
{
  flush_stream (output_stream (stream));
  return Qnil;
}

lisp
Fwrite_to_string (lisp object, lisp keys)
{
  wStream stream;
  print_circle circle;
  print_control pc (circle, keys);
  pc.setup_circle (object);
  print_sexp (stream, pc, object, 0);
  return stream.make_string ();
}

void
format_internal (wStream &stream, const Char *p, int size, lisp args, int in_loop, bool backward_compat_p)
{
  lisp l = Flist_length (args);
  if (l == Qnil)
    FEprogram_error (Eargument_is_circle);
  Format f (p, size, in_loop, backward_compat_p);
  lisp *v = (lisp *)alloca (sizeof (lisp) * fixnum_value (l));
  f.setarg (v, args);
  f.process (stream);
}

inline lisp
format (lisp dest, lisp string, lisp args, bool backward_compat_p)
{
  check_string (string);
  if (dest == Qt)
    dest = xsymbol_value (Vstandard_output);
  else if (stringp (dest))
    dest = Fsi_make_string_output_stream_from_string (dest);

  if (dest == Qnil)
    {
      wStream stream;
      try
        {
          format_internal (stream, xstring_contents (string),
                           xstring_length (string), args,
                           Format::IL_ILLEGAL, backward_compat_p);
        }
      catch (UpAndOut)
        {
        }
      return stream.make_string ();
    }
  else
    {
      check_stream (dest);
      wStreamsStream stream (dest);
      try
        {
          format_internal (stream, xstring_contents (string),
                           xstring_length (string), args,
                           Format::IL_ILLEGAL, backward_compat_p);
        }
      catch (UpAndOut)
        {
        }
    }
  return Qnil;
}

lisp
Fcl_format (lisp dest, lisp string, lisp args)
{
  return format (dest, string, args, false);
}

lisp
Fformat (lisp dest, lisp string, lisp args)
{
  return format (dest, string, args, true);
}

void
ding (int x)
{
  if (xsymbol_value (Vbeep_on_never) == Qnil)
    {
      if (xsymbol_value (Vvisible_bell) != Qnil)
        {
          HWND hwnd = get_active_window ();
          RECT r;
          GetWindowRect (hwnd, &r);
          DWORD flags = DCX_WINDOW | DCX_CLIPSIBLINGS | DCX_CACHE;
          if (LockWindowUpdate (hwnd))
            flags |= DCX_LOCKWINDOWUPDATE;
          HDC hdc = GetDCEx (hwnd, 0, flags);
          PatBlt (hdc, 0, 0, r.right - r.left, r.bottom - r.top, DSTINVERT);
          GdiFlush ();
          Sleep (20);
          PatBlt (hdc, 0, 0, r.right - r.left, r.bottom - r.top, DSTINVERT);
          GdiFlush ();
          ReleaseDC (hwnd, hdc);
          if (flags & DCX_LOCKWINDOWUPDATE)
            LockWindowUpdate (0);
        }
      else
        MessageBeep (x);
    }
}

lisp
Fding ()
{
  ding (MB_OK);
  return Qnil;
}

struct msgbox_styles {int type, icon, def;};

static void
msgbox_style (msgbox_styles &mb, lisp styles)
{
  mb.type = -1;
  mb.icon = 0;
  mb.def = 0;

  if (styles)
    for (; consp (styles); styles = xcdr (styles))
      {
        lisp x = xcar (styles);
        if (x == Kok)
          mb.type = MB_OK;
        else if (x == Kok_cancel)
          mb.type = MB_OKCANCEL;
        else if (x == Kabort_retry_ignore)
          mb.type = MB_ABORTRETRYIGNORE;
        else if (x == Kyes_no_cancel)
          mb.type = MB_YESNOCANCEL;
        else if (x == Kyes_no)
          mb.type = MB_YESNO;
        else if (x == Kretry_cancel)
          mb.type = MB_RETRYCANCEL;
        else if (x == Khand)
          mb.icon = MB_ICONHAND;
        else if (x == Kquestion)
          mb.icon = MB_ICONQUESTION;
        else if (x == Kexclamation)
          mb.icon = MB_ICONEXCLAMATION;
        else if (x == Kasterisk)
          mb.icon = MB_ICONASTERISK;
        else if (x == Kinformation)
          mb.icon = MB_ICONINFORMATION;
        else if (x == Kstop)
          mb.icon = MB_ICONSTOP;
        else if (x == Kbutton1)
          mb.def = 0;
        else if (x == Kbutton2)
          mb.def = 1;
        else if (x == Kbutton3)
          mb.def = 2;
        else if (x == Kbutton4)
          mb.def = 3;
        else if (x == Kbutton5)
          mb.def = 4;
        QUIT;
      }
}

static lisp
msgbox_result (int x)
{
  switch (x)
    {
    case IDABORT:
      return Kabort;

    case IDCANCEL:
      return Kcancel;

    case IDIGNORE:
      return Kignore;

    case IDNO:
      return Kno;

    case IDOK:
      return Kok;

    case IDRETRY:
      return Kretry;

    case IDYES:
      return Kyes;

    case XMessageBox::IDBUTTON1:
      return Kbutton1;

    case XMessageBox::IDBUTTON2:
      return Kbutton2;

    case XMessageBox::IDBUTTON3:
      return Kbutton3;

    case XMessageBox::IDBUTTON4:
      return Kbutton4;

    case XMessageBox::IDBUTTON5:
      return Kbutton5;

    default:
      return Qnil;
    }
}

static void
msgbox_captions (lisp *lcaptions, lisp args)
{
  for (lisp l = args; consp (l); l = xcdr (l))
    {
      lisp key = xcar (l);
      if (!symbolp (key))
        FEinvalid_keyword_list (args);
      l = xcdr (l);
      if (!consp (l))
        FEinvalid_keyword_list (args);
      if (key == Kbutton1)
        lcaptions[0] = xcar (l);
      else if (key == Kbutton2)
        lcaptions[1] = xcar (l);
      else if (key == Kbutton3)
        lcaptions[2] = xcar (l);
      else if (key == Kbutton4)
        lcaptions[3] = xcar (l);
      else if (key == Kbutton5)
        lcaptions[4] = xcar (l);
    }
}

static int
count_crlf (const Char *p, const Char *pe)
{
  int l;
  for (l = 0; p < pe; p++)
    l += *p == '\n' ? 2 : 1;
  return l;
}

static void
copy_crlf (Char *b, const Char *p, const Char *pe)
{
  for (; p < pe; p++)
    if (*p == '\r')
      *b++ = ' ';
    else if (*p == '\n')
      {
        *b++ = '\r';
        *b++ = '\n';
      }
    else
      *b++ = *p;
}

lisp
Fmessage_box (lisp lmsg, lisp ltitle, lisp styles, lisp args)
{
  check_string (lmsg);
  int l = count_crlf (xstring_contents (lmsg),
                      xstring_contents (lmsg) + xstring_length (lmsg));
  char *msg = (char *)alloca (l * 2 + 3);
  copy_crlf ((Char *)msg + 1,
             xstring_contents (lmsg),
             xstring_contents (lmsg) + xstring_length (lmsg));
  w2s (msg, (Char *)msg + 1, l);

  const char *title;
  if (!ltitle || ltitle == Qnil)
    title = TitleBarString;
  else
    {
      check_string (ltitle);
      title = (char *)alloca (xstring_length (ltitle) * 2 + 1);
      w2s ((char *)title, ltitle);
    }

  msgbox_styles mb;
  msgbox_style (mb, styles);

  lisp lcaptions[5];
  const char *captions[numberof (lcaptions)];
  memset (lcaptions, 0, sizeof lcaptions);
  memset (captions, 0, sizeof captions);
  msgbox_captions (lcaptions, args);

  for (int i = 0; i < numberof (lcaptions); i++)
    {
      lisp x = lcaptions[i];
      if (x && x != Qnil)
        {
          check_string (x);
          char *s = (char *)alloca (xstring_length (x) * 2 + 1);
          w2s (s, x);
          captions[i] = s;
        }
    }

  return msgbox_result (MsgBoxEx (get_active_window (), msg, title,
                                  mb.type, mb.def, mb.icon, 1,
                                  captions, numberof (captions), 1,
                                  find_keyword_bool (Kno_wrap, args)));
}

static void
addobj (print_control &pc, wStream &stream, lisp x, int esc, int colon)
{
  if (x)
    {
      if (colon && stream.columns ())
        stream.add (": ");
      pc.escape = esc;
      pc.setup_circle (x);
      print_sexp (stream, pc, x, 0);
    }
}

static int
putmsg (wStream &stream, int msgboxp, int style, int beep)
{
  stream.finish ();
  int l = stream.length ();
  Char *b = (Char *)alloca (sizeof (Char) * l + sizeof (Char) + 1);
  stream.copy (b + 1);

  if (msgboxp)
    {
      w2s ((char *)b, b + 1, l);
      app.status_window.clear ();
      return MsgBox (get_active_window (), (char *)b, TitleBarString, style, beep);
    }
  else
    {
      app.status_window.puts (b + 1, l);
      app.status_window.putc ('\n');
      if (beep)
        Fding ();
      return 0;
    }
}

static int
special_condition_report (wStream &stream, lisp cc)
{
  dynamic_bind dynb (Vprint_circle, Qt);
  lisp def = xstrdata_def (cc);
  if (stringp (xstrdef_report (def))
      || messagep (xstrdef_report (def)))
    {
      print_circle circle;
      print_control pc (circle);
      pc.princ ();
      pc.length = 6;
      pc.level = 6;
      addobj (pc, stream, xstrdef_report (def), 0, 0);
      for (int i = 0; i < xstrdata_nslots (cc); i++)
        if (xstrdata_data (cc) [i] != Quninitialized)
          addobj (pc, stream, xstrdata_data (cc) [i], 1, 1);
      return 1;
    }
  else if (xstrdata_nslots (cc) == 2
           && xstrdef_nslots (def) == 2
           && xstrdef_slotdesc (def) [0].name == Kformat_string
           && xstrdef_slotdesc (def) [1].name == Kformat_arguments
           && (messagep (xstrdata_data (cc) [0])
               || errorp (xstrdata_data (cc) [0])))
    {
      print_circle circle;
      print_control pc (circle);
      pc.princ ();
      pc.length = 6;
      pc.level = 6;
      addobj (pc, stream, xstrdata_data (cc) [0], 0, 0);
      if (xstrdata_data (cc) [1] != Quninitialized)
        addobj (pc, stream, xstrdata_data (cc) [1], 1, 1);
      return 1;
    }
  else
    return 0;
}

class protect_infinite_recursive_call
{
  static int depth;
public:
  protect_infinite_recursive_call () {depth++;}
  ~protect_infinite_recursive_call () {depth--;}
  static int infinitep () {return depth > 100;}
};

int protect_infinite_recursive_call::depth;

static void
print_condition (wStream &stream, lisp cc)
{
  protect_infinite_recursive_call pirc;
  if (protect_infinite_recursive_call::infinitep ())
    return;
  protect_gc gcpro (cc);
  if (!special_condition_report (stream, cc))
    {
      dynamic_bind dynb (Vprint_circle, Qt);
      print_circle circle;
      print_control pc (circle);
      pc.princ ();
      pc.length = 6;
      pc.level = 6;
      pc.setup_circle (cc);
      print_sexp (stream, pc, cc, 0);
    }
}

void
print_condition (lisp cc)
{
  if (!struct_data_p (cc)
      || Fsi_structure_subtypep (xstrdata_def (cc),
                                 xsymbol_value (QCcondition)) == Qnil)
    {
      assert (0);
      return;
    }

  try
    {
      wStream stream;
      print_condition (stream, cc);
      putmsg (stream,
              (xstrdef_important_p (xstrdata_def (cc))
               && (xsymbol_value (Vsi_report_simple_errors_mildly) == Qnil
                   || (Fsi_structure_subtypep (xstrdata_def (cc),
                                               xsymbol_value (QCsimple_error)) == Qnil
                       && Fsi_structure_subtypep (xstrdata_def (cc),
                                                  xsymbol_value (QCread_only_buffer)) == Qnil))),
              MB_OK | MB_ICONEXCLAMATION,
              (Fsi_structure_subtypep (xstrdata_def (cc),
                                       xsymbol_value (QCwarning)) != Qnil
               ? xsymbol_value (Vbeep_on_warn) != Qnil
               : xsymbol_value (Vbeep_on_error) != Qnil));
    }
  catch (nonlocal_jump &)
    {
    }
}

void
print_condition (nonlocal_data *nld)
{
  if (nld->type != Qexit_this_level)
    print_condition (nld->id);
}

lisp
Fsi_condition_string (lisp cc)
{
  check_condition (cc);
  wStream stream;
  print_condition (stream, cc);
  stream.finish ();
  int l = stream.length ();
  Char *b = (Char *)alloca (sizeof (Char) * l);
  stream.copy (b);
  return make_string (b, l);
}

lisp
Fsi_print_condition (lisp cc)
{
  check_condition (cc);
  print_condition (cc);
  return Qnil;
}

static void
print_trace_object (wStreamsStream &stream, print_control &pc, lisp object)
{
  pc.setup_circle (object);
  print_sexp (stream, pc, object, 0);
}

void
print_stack_trace (lisp lstream, lisp cc)
{
  dynamic_bind dynb (Vprint_circle, Qt);
  print_circle circle;
  print_control pc (circle);
  pc.prin1 ();
  pc.readably = 0;
  pc.length = 3;
  pc.level = 4;

  wStreamsStream stream (lstream);

  int depth = 0;
  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    if (p->type != stack_trace::empty)
      depth++;

  lisp object = Qnil;
  protect_gc gcpro (object);

  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    {
      if (p->type == stack_trace::empty)
        continue;

      char buf[64];
      sprintf (buf, ">CALL STACK %2d: ", depth--);
      stream.add (buf);

      switch (p->type)
        {
        case stack_trace::special_form:
        case stack_trace::macro:
          if (p->fn == Ssi_byte_code)
            stream.add ("(system:*byte-code ...)\n");
          else
            {
              stream.add ('(');
              print_trace_object (stream, pc, p->fn);
              if (p->args[1] != Qnil)
                {
                  stream.add (' ');
                  print_trace_object (stream, pc, p->args[1]);
                }
              stream.add (')');
              stream.add ('\n');
            }
          break;

        case stack_trace::apply:
          stream.add ('(');
          print_trace_object (stream, pc, p->fn);
          if (p->args[0])
            {
              stream.add (' ');
              print_trace_object (stream, pc, p->args[0]);
              if (p->args[1])
                {
                  stream.add (' ');
                  print_trace_object (stream, pc, p->args[1]);
                }
            }
          else
            for (lisp q = p->args[1]; consp (q); q = xcdr (q))
              {
                stream.add (' ');
                print_trace_object (stream, pc, xcar (q));
              }
          stream.add (')');
          stream.add ('\n');
          break;

        case stack_trace::eval_args:
          stream.add ('(');
          print_sexp (stream, pc, p->fn, 0);
          stream.add (" calculating arguments...)\n");
          break;
        }
    }

  if (cc && cc != Qnil)
    {
      print_condition (stream, cc);
      stream.add ('\n');
    }
  stream.add ('\n');
}

lisp
Fstack_trace (lisp lstream, lisp cc)
{
  print_stack_trace ((lstream == Qt
                      ? xsymbol_value (Verror_output)
                      : output_stream (lstream)),
                     cc);
  return Qnil;
}

static int
domsg (int flags, int msgboxp, int warnp, lisp a1, lisp a2)
{
  suppress_gc sgc;
  int result = -1;
  try
    {
      dynamic_bind dynb (Vprint_circle, Qt);
      print_circle circle;
      print_control pc (circle);
      pc.princ ();
      pc.length = 6;
      pc.level = 6;
      wStream stream;

      addobj (pc, stream, a1, 0, 0);
      addobj (pc, stream, a2, 0, 1);

      result = putmsg (stream, msgboxp, flags, warnp && xsymbol_value (Vbeep_on_warn) != Qnil);
    }
  catch (nonlocal_jump &)
    {
    }
  return result;
}

int
msgbox (int flags, lisp a1, lisp a2)
{
  return domsg (flags, 1, 0, a1, a2);
}

void
message (lisp a1, lisp a2)
{
  domsg (0, 0, 0, a1, a2);
}

int
yes_or_no_p (lisp a1, lisp a2)
{
  return msgbox (MB_YESNO | MB_ICONQUESTION, a1, a2) == IDYES;
}

int
msgbox (int flags, message_code m, lisp a2)
{
  return msgbox (flags, make_message (m), a2);
}

void
message (message_code a1, lisp a2)
{
  message (make_message (a1), a2);
}

int
yes_or_no_p (message_code a1, lisp a2)
{
  return yes_or_no_p (make_message (a1), a2);
}

void
warn_msgbox (lisp a1, lisp a2)
{
  domsg (MB_OK | MB_ICONEXCLAMATION, 1, 1, a1, a2);
}

void
warn (lisp a1, lisp a2)
{
  domsg (0, 0, 1, a1, a2);
}

void
warn_msgbox (message_code m, lisp a2)
{
  warn_msgbox (make_message (m), a2);
}

void
warn (message_code a1, lisp a2)
{
  warn (make_message (a1), a2);
}

void
format_message (message_code m, ...)
{
  const char *fmt = get_message_string (m);
  va_list ap;
  va_start (ap, m);
  char buf[2048];
  vsprintf (buf, fmt, ap);
  va_end (ap);
  app.status_window.puts (buf, 1);
}

int
format_yes_or_no_p (message_code m, ...)
{
  const char *fmt = get_message_string (m);
  va_list ap;
  va_start (ap, m);
  char buf[2048];
  vsprintf (buf, fmt, ap);
  va_end (ap);
  return MsgBox (get_active_window (), buf, TitleBarString,
                 MB_YESNO | MB_ICONQUESTION, 1) == IDYES;
}

char *
print_key_sequence (char *b, char *be, Char c)
{
  wStream stream;
  print_char (stream, c, 0);
  stream.finish ();
  int l = stream.length ();
  Char *w = (Char *)alloca (sizeof (Char) * l);
  stream.copy (w);
  return w2s (b, be, w, l);
}
