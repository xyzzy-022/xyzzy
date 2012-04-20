#include "stdafx.h"
#include "ed.h"
#include "byte-stream.h"
#include "sequence.h"
#include "StrBuf.h"

int
update_column (int column, Char c)
{
  if (c == '\n')
    column = 0;
  else if (c == '\t')
    column = ((column + app.default_tab_columns)
              / app.default_tab_columns * app.default_tab_columns);
  else
    column += char_width (c);
  return column;
}

int
update_column (int column, const Char *s, int size)
{
  for (const Char *se = s + size; s < se; s++)
    column = update_column (column, *s);
  return column;
}

int
update_column (int column, Char c, int size)
{
  if (size)
    {
      if (c == '\n')
        column = 0;
      else if (c == '\t')
        column = ((column + app.default_tab_columns) / app.default_tab_columns
                  * app.default_tab_columns
                  + (size - 1) * app.default_tab_columns);
      else
        column += char_width (c) * size;
    }
  return column;
}

size_t
s2wl (const char *string)
{
  size_t l = 0;
  const u_char *s = (const u_char *)string;
  while (*s)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              s++;
              break;
            }
          l++;
          s += 2;
        }
      else
        s++;
    }
  return s - (const u_char *)string - l;
}

Char *
s2w (Char *b, size_t size, const char **string)
{
  Char *be = b + size;
  const u_char *s = (const u_char *)*string;
  while (b < be && *s)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              *b++ = *s++;
              break;
            }
          *b++ = (*s << 8) | s[1];
          s += 2;
        }
      else
        *b++ = *s++;
    }
  *string = (const char *)s;
  return b;
}

Char *
s2w (Char *b, const char *string)
{
  const u_char *s = (const u_char *)string;
  while (*s)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              *b = *s;
              break;
            }
          *b++ = (*s << 8) | s[1];
          s += 2;
        }
      else
        *b++ = *s++;
    }
  return b;
}

Char *
s2w (const char *string, size_t size)
{
  Char *b = (Char *)xmalloc (sizeof (Char) * size);
  s2w (b, string);
  return b;
}

void
a2w (Char *b, const char *string, size_t size)
{
  const u_char *s = (const u_char *)string;
  const u_char *se = s + size;
  while (s < se)
    *b++ = *s++;
}

Char *
a2w (Char *b, size_t size, const char **string)
{
  Char *be = b + size;
  const u_char *s = (const u_char *)*string;
  while (b < be && *s)
    *b++ = *s++;
  *string = (const char *)s;
  return b;
}

Char *
a2w (Char *b, const char *string)
{
  for (const u_char *s = (const u_char *)string; *s;)
    *b++ = *s++;
  return b;
}

Char *
a2w (const char *string, size_t size)
{
  Char *b = (Char *)xmalloc (sizeof (Char) * size);
  a2w (b, string, size);
  return b;
}

size_t
w2sl (const Char *s, size_t size)
{
  size_t l = 0;
  for (const Char *se = s + size; s < se; s++)
    if (DBCP (*s))
      l++;
  return size + l;
}

char *
w2s (char *b, const Char *s, size_t size)
{
  for (const Char *se = s + size; s < se; s++)
    {
      if (DBCP (*s))
        *b++ = *s >> 8;
      *b++ = char (*s);
    }
  *b = 0;
  return b;
}

char *
w2s (const Char *s, size_t size)
{
  char *b = (char *)xmalloc (w2sl (s, size) + 1);
  w2s (b, s, size);
  return b;
}

char *
w2s (char *b, char *be, const Char *s, size_t size)
{
  be--;
  for (const Char *se = s + size; s < se && b < be; s++)
    {
      if (DBCP (*s))
        {
          if (b == be - 1)
            break;
          *b++ = *s >> 8;
        }
      *b++ = char (*s);
    }
  *b = 0;
  return b;
}

char *
w2s_quote (char *b, char *be, const Char *s, size_t size, int qc, int qe)
{
  be--;
  for (const Char *se = s + size; s < se && b < be; s++)
    {
      if (DBCP (*s))
        {
          if (b == be - 1)
            break;
          *b++ = *s >> 8;
        }
      else if (*s == qc)
        {
          if (b == be - 1)
            break;
          *b++ = qe;
        }
      *b++ = char (*s);
    }
  *b = 0;
  return b;
}

size_t
s2wl (const char *string, const char *se, int zero_term)
{
  size_t l = 0;
  const u_char *s = (const u_char *)string;
  while (s < (const u_char *)se && (!zero_term || *s))
    {
      if (SJISP (*s))
        {
          if (s + 1 >= (const u_char *)se || (zero_term && !s[1]))
            {
              s++;
              break;
            }
          l++;
          s += 2;
        }
      else
        s++;
    }
  return s - (const u_char *)string - l;
}

Char *
s2w (Char *b, const char *string, const char *se, int zero_term)
{
  const u_char *s = (const u_char *)string;
  while (s < (const u_char *)se && (!zero_term || *s))
    {
      if (SJISP (*s))
        {
          if (s + 1 >= (const u_char *)se || (zero_term && !s[1]))
            {
              *b = *s;
              break;
            }
          *b++ = (*s << 8) | s[1];
          s += 2;
        }
      else
        *b++ = *s++;
    }
  return b;
}

void
w2s_chunk (char *b, char *be, const Char *s, size_t size)
{
  for (const Char *se = s + size; s < se && b < be; s++)
    {
      if (DBCP (*s))
        {
          if (b == be - 1)
            break;
          *b++ = *s >> 8;
        }
      *b++ = char (*s);
    }
  if (b < be)
    *b = 0;
}

lisp
make_string (const u_char *string)
{
  return make_string ((const char *)string);
}

lisp
make_string (const char *string)
{
  lisp p = make_simple_string ();
  size_t size = s2wl (string);
  xstring_contents (p) = s2w (string, size);
  xstring_length (p) = size;
  return p;
}

lisp
make_string (const char *string, size_t size)
{
  lisp p = make_simple_string ();
  Char *b = (Char *)xmalloc (size * sizeof (Char));
  xstring_contents (p) = b;
  xstring_length (p) = size;
  s2w (b, size, &string);
  return p;
}

lisp
make_string_simple (const char *string, size_t size)
{
  lisp p = make_simple_string ();
  xstring_contents (p) = a2w (string, size);
  xstring_length (p) = size;
  return p;
}

lisp
make_string (const Char *string, size_t size)
{
  lisp p = make_simple_string ();
  xstring_contents (p) = (Char *)xmemdup (string, size * sizeof (Char));
  xstring_length (p) = size;
  return p;
}

lisp
copy_string (lisp p)
{
  assert (stringp (p));
  return make_string (xstring_contents (p), xstring_length (p));
}

lisp
make_string (Char c, size_t size)
{
  lisp p = make_simple_string ();
  Char *d = (Char *)xmalloc (size * sizeof (Char));
  xstring_contents (p) = d;
  xstring_length (p) = size;
  bfill (d, int (size), c);
  return p;
}

lisp
make_complex_string (Char c, int fillp, int size, int adjustable)
{
  assert (fillp <= size);
  lisp p = make_complex_string ();
  Char *d = (Char *)xmalloc (size * sizeof (Char));
  xstring_contents (p) = d;
  xstring_length (p) = fillp >= 0 ? fillp : size;
  xstring_dimension (p) = size;
  xarray_adjustable (p) = adjustable;
  xarray_has_fillp (p) = fillp >= 0;
  bfill (d, size, c);
  return p;
}

lisp
make_string (size_t size)
{
  lisp p = make_simple_string ();
  xstring_contents (p) = (Char *)xmalloc (size * sizeof (Char));
  xstring_length (p) = size;
  return p;
}

lisp
make_string_from_list (lisp list)
{
  int l = 0;
  for (lisp x = list; consp (x); x = xcdr (x), l++)
    check_char (xcar (x));
  lisp string = make_string (l);
  Char *s = xstring_contents (string);
  for (lisp x = list; consp (x); x = xcdr (x))
    *s++ = xchar_code (xcar (x));
  return string;
}

lisp
make_string_from_vector (lisp vector)
{
  assert (general_vector_p (vector));
  lisp *p, *pe;
  for (p = xvector_contents (vector), pe = p + xvector_length (vector); p < pe; p++)
    check_char (*p);
  lisp string = make_string (xvector_length (vector));
  Char *s = xstring_contents (string);
  for (p = xvector_contents (vector); p < pe; p++)
    *s++ = xchar_code (*p);
  return string;
}

int
string_equalp (const Char *p1, int l1, const char *p2, int l2)
{
  if (l1 != l2)
    return 0;
  for (const Char *pe = p1 + l1; p1 < pe; p1++, p2++)
    if (char_upcase (*p1) != char_upcase (*p2))
      return 0;
  return 1;
}

int
string_equalp (const Char *p1, int l1, const Char *p2, int l2)
{
  if (l1 != l2)
    return 0;
  for (const Char *pe = p1 + l1; p1 < pe; p1++, p2++)
    if (char_upcase (*p1) != char_upcase (*p2))
      return 0;
  return 1;
}

int
string_equalp (lisp x, int xo, lisp y, int yo, int l)
{
  if (xo + l > xstring_length (x) || yo + l > xstring_length (y))
    return 0;
  return string_equalp (xstring_contents (x) + xo, l,
                        xstring_contents (y) + yo, l);
}

lisp
coerce_to_string (lisp x, int copy)
{
  if (immediatep (x))
    {
      if (charp (x))
        {
          Char c = xchar_code (x);
          return make_string (&c, 1);
        }
    }
  else
    {
      switch (object_typeof (x))
        {
        case Tsimple_string:
        case Tcomplex_string:
          return copy ? copy_string (x) : x;

        case Tsymbol:
          return copy ? Fcopy_string (xsymbol_name (x)) : xsymbol_name (x);

        case Tsimple_vector:
        case Tcomplex_vector:
          return make_string_from_vector (x);
        }
    }
  return FEtype_error (x, Qstring);
}

void
string_start_end (lisp string, int &start, int &end, lisp lstart, lisp lend)
{
  check_string (string);
  seq_start_end (xstring_length (string), start, end, lstart, lend);
}

lisp
Fcopy_string (lisp string)
{
  check_string (string);
  return copy_string (string);
}

lisp
Fchar (lisp string, lisp index)
{
  check_string (string);
  int i = fixnum_value (index);
  if (i < 0 || i >= xstring_length (string))
    FErange_error (index);
  return make_char (xstring_contents (string) [i]);
}

lisp
Fsi_set_char (lisp string, lisp index, lisp value)
{
  check_string (string);
  check_char (value);
  int i = fixnum_value (index);
  if (i < 0 || i >= xstring_length (string))
    FErange_error (index);
  xstring_contents (string) [i] = xchar_code (value);
  return value;
}

lisp
Fschar (lisp string, lisp index)
{
  check_simple_string (string);
  int i = fixnum_value (index);
  if (i < 0 || i >= xstring_length (string))
    FErange_error (index);
  return make_char (xstring_contents (string) [i]);
}

lisp
Fsi_set_schar (lisp string, lisp index, lisp value)
{
  check_simple_string (string);
  check_char (value);
  int i = fixnum_value (index);
  if (i < 0 || i >= xstring_length (string))
    FErange_error (index);
  xstring_contents (string) [i] = xchar_code (value);
  return value;
}

static const Char *
string_compare1 (lisp string1, lisp string2, lisp keys,
                 const Char *&p, const Char *&pe,
                 const Char *&q, const Char *&qe)
{
  string1 = coerce_to_string (string1, 0);
  int start1, end1;
  string_start_end (string1, start1, end1,
                    find_keyword (Kstart1, keys, make_fixnum (0)),
                    find_keyword (Kend1, keys, Qnil));

  string2 = coerce_to_string (string2, 0);
  int start2, end2;
  string_start_end (string2, start2, end2,
                    find_keyword (Kstart2, keys, make_fixnum (0)),
                    find_keyword (Kend2, keys, Qnil));

  p = xstring_contents (string1) + start1;
  pe = xstring_contents (string1) + end1;
  q = xstring_contents (string2) + start2;
  qe = xstring_contents (string2) + end2;

  return xstring_contents (string1);
}

static int
string_compare (lisp string1, lisp string2, lisp keys, int &l)
{
  const Char *p, *pe, *q, *qe;
  const Char *p0 = string_compare1 (string1, string2, keys, p, pe, q, qe);
  while (1)
    {
      if (p == pe)
        {
          l = p - p0;
          return q == qe ? 0 : -1;
        }
      if (q == qe)
        {
          l = p - p0;
          return 1;
        }
      if (*p != *q)
        {
          l = p - p0;
          return *p - *q;
        }
      p++;
      q++;
    }
}

static int
string_comparep (lisp string1, lisp string2, lisp keys, int &l)
{
  const Char *p, *pe, *q, *qe;
  const Char *p0 = string_compare1 (string1, string2, keys, p, pe, q, qe);
  while (1)
    {
      if (p == pe)
        {
          l = p - p0;
          return q == qe ? 0 : -1;
        }
      if (q == qe)
        {
          l = p - p0;
          return 1;
        }
      Char c1 = char_upcase (*p);
      Char c2 = char_upcase (*q);
      if (c1 != c2)
        {
          l = p - p0;
          return c1 - c2;
        }
      p++;
      q++;
    }
}

lisp
Fstring_equal (lisp x, lisp y, lisp keys)
{
  const Char *p, *pe, *q, *qe;
  string_compare1 (x, y, keys, p, pe, q, qe);
  return boole (pe - p == qe - q && !bcmp (p, q, pe - p));
}

lisp
Fstring_equalp (lisp x, lisp y, lisp keys)
{
  const Char *p, *pe, *q, *qe;
  string_compare1 (x, y, keys, p, pe, q, qe);
  return boole (string_equalp (p, pe - p, q, qe - q));
}

lisp
Fstring_not_equal (lisp x, lisp y, lisp keys)
{
  int l;
  return string_compare (x, y, keys, l) ? make_fixnum (l) : Qnil;
}

lisp
Fstring_not_equalp (lisp x, lisp y, lisp keys)
{
  int l;
  return string_comparep (x, y, keys, l) ? make_fixnum (l) : Qnil;
}

lisp
Fstring_less (lisp x, lisp y, lisp keys)
{
  int l;
  return string_compare (x, y, keys, l) < 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_lessp (lisp x, lisp y, lisp keys)
{
  int l;
  return string_comparep (x, y, keys, l) < 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_greater (lisp x, lisp y, lisp keys)
{
  int l;
  return string_compare (x, y, keys, l) > 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_greaterp (lisp x, lisp y, lisp keys)
{
  int l;
  return string_comparep (x, y, keys, l) > 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_not_greater (lisp x, lisp y, lisp keys)
{
  int l;
  return string_compare (x, y, keys, l) <= 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_not_greaterp (lisp x, lisp y, lisp keys)
{
  int l;
  return string_comparep (x, y, keys, l) <= 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_not_less (lisp x, lisp y, lisp keys)
{
  int l;
  return string_compare (x, y, keys, l) >= 0 ? make_fixnum (l) : Qnil;
}

lisp
Fstring_not_lessp (lisp x, lisp y, lisp keys)
{
  int l;
  return string_comparep (x, y, keys, l) >= 0 ? make_fixnum (l) : Qnil;
}

lisp
subseq_string (lisp string, lisp lstart, lisp lend)
{
  int start, end;
  string_start_end (string, start, end, lstart, lend);
  return make_string (xstring_contents (string) + start, end - start);
}

lisp
Fsubstring (lisp string, lisp lstart, lisp lend)
{
  check_string (string);
  int len = xstring_length (string);
  int start = fixnum_value (lstart);
  int end = lend && lend != Qnil ? fixnum_value (lend) : len;
  if (start < 0)
    start += len;
  if (end < 0)
    end += len;
  if (start < 0 || start > end)
    FErange_error (lstart);
  if (end > len)
    FErange_error (lend);
  return make_string (xstring_contents (string) + start, end - start);
}

static inline int
match_char_bag (Char c, lisp bag)
{
  assert (stringp (bag));
  for (const Char *p = xstring_contents (bag), *pe = p + xstring_length (bag);
       p < pe; p++)
    if (c == *p)
      return 1;
  return 0;
}

static const Char *
left_trim (const Char *p0, int l, lisp bag)
{
  const Char *p, *pe;
  for (p = p0, pe = p + l; p < pe; p++)
    if (!match_char_bag (*p, bag))
      break;
  return p;
}

static const Char *
right_trim (const Char *p0, int l, lisp bag)
{
  const Char *p;
  for (p = p0 + l; p > p0; p--)
    if (!match_char_bag (p[-1], bag))
      break;
  return p;
}

static inline int
left_trim (lisp string, lisp bag)
{
  assert (stringp (string));
  return (left_trim (xstring_contents (string), xstring_length (string), bag)
          - xstring_contents (string));
}

static inline int
right_trim (lisp string, lisp bag)
{
  assert (stringp (string));
  return (right_trim (xstring_contents (string), xstring_length (string), bag)
          - xstring_contents (string));
}

lisp
Fstring_left_trim (lisp char_bag, lisp string)
{
  string = Fstring (string);
  int start = left_trim (string, seq_to_string (char_bag));
  return !start ? string : subseq_string (string, make_fixnum (start), Qnil);
}

lisp
Fstring_right_trim (lisp char_bag, lisp string)
{
  string = Fstring (string);
  int end = right_trim (string, seq_to_string (char_bag));
  return (end == xstring_length (string)
          ? string
          : subseq_string (string, make_fixnum (0), make_fixnum (end)));
}

lisp
Fstring_trim (lisp char_bag, lisp string)
{
  string = Fstring (string);
  char_bag = seq_to_string (char_bag);
  int start = left_trim (string, char_bag);
  int end = right_trim (string, char_bag);
  if (start >= end)
    return make_string ("");
  return ((!start && end == xstring_length (string))
          ? string
          : subseq_string (string, make_fixnum (start), make_fixnum (end)));
}

lisp
Fstring_upcase (lisp string, lisp keys)
{
  return Fnstring_upcase (coerce_to_string (string, 1), keys);
}

lisp
Fstring_downcase (lisp string, lisp keys)
{
  return Fnstring_downcase (coerce_to_string (string, 1), keys);
}

lisp
Fstring_capitalize (lisp string, lisp keys)
{
  return Fnstring_capitalize (coerce_to_string (string, 1), keys);
}

lisp
Fnstring_upcase (lisp string, lisp keys)
{
  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  for (Char *p = xstring_contents (string) + start, *pe = xstring_contents (string) + end;
       p < pe; p++)
    *p = char_upcase (*p);
  return string;
}

lisp
Fnstring_downcase (lisp string, lisp keys)
{
  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  for (Char *p = xstring_contents (string) + start, *pe = xstring_contents (string) + end;
       p < pe; p++)
    *p = char_downcase (*p);
  return string;
}

lisp
Fnstring_capitalize (lisp string, lisp keys)
{
  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  int f = 1;
  for (Char *p = xstring_contents (string) + start, *pe = xstring_contents (string) + end;
       p < pe; p++)
    {
      if (alphanumericp (*p))
        {
          if (f)
            *p = char_upcase (*p);
          else
            *p = char_downcase (*p);
          f = 0;
        }
      else
        f = 1;
    }
  return string;
}

lisp
Fstring (lisp x)
{
  return coerce_to_string (x, symbolp (x));
}

static void
trim (const Char *&p0, const Char *&pe, lisp bag)
{
  if (p0 != pe)
    {
      p0 = left_trim (p0, pe - p0, bag);
      pe = right_trim (p0, pe - p0, bag);
    }
}

lisp
Fsplit_string (lisp string, lisp lsep, lisp ignore_empty, lisp char_bag)
{
  string = Fstring (string);
  if (!charp (lsep) && !stringp (lsep))
    FEtype_error (lsep, xsymbol_value (Qor_string_character));
  int empty_ok = ignore_empty && ignore_empty != Qnil;
  if (char_bag == Qnil)
    char_bag = 0;
  else if (char_bag)
    {
      char_bag = Fstring (char_bag);
      if (!xstring_length (char_bag))
        char_bag = 0;
    }

  const Char *p = xstring_contents (string);
  const Char *pe = p + xstring_length (string);
  if (p == pe)
    return Qnil;

  lisp result = Qnil;

  if (charp (lsep) || xstring_length (lsep) == 1)
    {
      Char sep = charp (lsep) ? xchar_code (lsep) : *xstring_contents (lsep);
      do
        {
          const Char *p0 = p;
          for (; p < pe && *p != sep; p++)
            ;
          const Char *pe = p;
          if (char_bag)
            trim (p0, pe, char_bag);
          if (p0 != pe || empty_ok)
            result = xcons (make_string (p0, pe - p0), result);
        }
      while (++p < pe);
    }
  else
    {
      if (!xstring_length (lsep))
        return char_bag ? Fstring_trim (char_bag, string) : string;
      do
        {
          const Char *p0 = p;
          for (; p < pe && !match_char_bag (*p, lsep); p++)
            ;
          const Char *pe = p;
          if (char_bag)
            trim (p0, pe, char_bag);
          if (p0 != pe || empty_ok)
            result = xcons (make_string (p0, pe - p0), result);
        }
      while (++p < pe);
    }
  return Fnreverse (result);
}

lisp
Fquote_string (lisp string, lisp search, lisp quote)
{
  check_string (string);
  const Char *s = xstring_contents (string);
  const Char *se = s + xstring_length (string);

  check_char (search);
  Char sch = xchar_code (search);

  check_char (quote);
  Char qch = xchar_code (quote);

  int count = 0;
  while (s < se)
    if (*s++ == sch)
      count++;

  if (!count)
    return string;

  s = xstring_contents (string);

  lisp string2 = make_string (xstring_length (string) + count);
  Char *d = xstring_contents (string2);

  while (s < se)
    {
      Char c = *s++;
      if (c == sch)
        *d++ = qch;
      *d++ = c;
    }
  return string2;
}

lisp
parse_integer (lisp string, int start, int &end, int radix, int junk_allowed)
{
  const Char *p = xstring_contents (string) + start;
  const Char *pe = xstring_contents (string) + end;

  if (junk_allowed)
    {
      const readtab_rep *readtab = xreadtable_rep (current_readtable ());
      for (; p < pe && stdchar_whitespace_p (readtab, *p); p++)
        ;
      for (; pe > p && stdchar_whitespace_p (readtab, pe[-1]); pe--)
        ;
    }

  if (p == pe)
    return Qnil;

  bignum_rep *rep;
  p = ato_bignum_rep (rep, p, pe - p, radix);
  end = p - xstring_contents (string);
  return p == pe ? make_integer (rep) : Qnil;
}

lisp
Fparse_integer (lisp string, lisp keys)
{
  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  lisp junk_allowed = find_keyword (Kjunk_allowed, keys, Qnil);
  int radix;
  lisp r = find_keyword (Kradix, keys);
  if (r == Qnil)
    radix = 10;
  else
    {
      radix = fixnum_value (r);
      if (radix < 2 || radix > 36)
        FErange_error (r);
    }
  lisp result = parse_integer (string, start, end, radix, junk_allowed != Qnil);
  if (result == Qnil && junk_allowed == Qnil)
    FEprogram_error (Einvalid_integer_format, string);
  multiple_value::count () = 2;
  multiple_value::value (1) = make_fixnum (end);
  return result;
}

int WINAPI
abbreviate_string (HDC hdc, char *buf, int maxpxl, int is_pathname)
{
  SIZE sz;
  int l = strlen (buf);
  GetTextExtentPoint32 (hdc, buf, l, &sz);
  if (sz.cx <= maxpxl)
    return 0;

  GetTextExtentPoint32 (hdc, "...", 3, &sz);
  maxpxl = (maxpxl - sz.cx);

  char *lb, *le;
  char *rb, *re;

  if (is_pathname)
    {
      lb = le = buf;
      re = buf + l;
      rb = find_last_slash (buf);
      if (rb)
        {
          GetTextExtentPoint32 (hdc, rb, re - rb, &sz);
          if (sz.cx > maxpxl)
            {
              rb++;
              goto trim_tail;
            }

          int pxl = sz.cx;
          int dev = 0;
          if (alpha_char_p (*lb & 255) && lb[1] == ':')
            dev = dir_separator_p (lb[2]) ? 3 : 2;
          else if (dir_separator_p (*lb) && dir_separator_p (lb[1]))
            {
              char *sl = find_slash (lb + 2);
              if (sl)
                sl = find_slash (sl + 1);
              if (sl && sl < rb)
                dev = sl - lb + 1;
            }
          if (dev)
            {
              GetTextExtentPoint32 (hdc, lb, dev, &sz);
              if (pxl + sz.cx > maxpxl)
                goto done;
              pxl += sz.cx;
              le = lb + dev;
            }

          while (rb > le)
            {
              char c = *rb;
              *rb = 0;
              char *slash = find_last_slash (buf);
              *rb = c;
              if (!slash)
                break;
              GetTextExtentPoint32 (hdc, slash, rb - slash, &sz);
              if (sz.cx + pxl > maxpxl)
                break;
              rb = slash;
              pxl += sz.cx;
            }
        }
      else
        {
          rb = buf;
        trim_tail:
          for (; re > rb; re = CharPrev (rb, re))
            {
              GetTextExtentPoint32 (hdc, rb, re - rb, &sz);
              if (sz.cx <= maxpxl)
                {
                  if (re - rb + 3 > l)
                    return 0;
                  *re = 0;
                  strcpy (stpcpy (buf, rb), "...");
                  return 1;
                }
            }
        }
    }
  else
    {
      maxpxl /= 2;
      for (lb = buf, le = buf + l / 2; le > lb; le = CharPrev (lb, le))
        {
          GetTextExtentPoint32 (hdc, lb, le - lb, &sz);
          if (sz.cx <= maxpxl)
            break;
        }
      for (rb = buf + l / 2, re = buf + l; rb < re; rb = CharNext (rb))
        {
          GetTextExtentPoint32 (hdc, rb, re - rb, &sz);
          if (sz.cx <= maxpxl)
            break;
        }
    }
done:
  if ((le - lb) + (re - rb) + 3 > l)
    return 0;

  for (int i = 0; i < 3; i++)
    le[i] = '.';
  strcpy (le + 3, rb);
  return 1;
}

static int
abbrev_string (char *buf, int maxl, int pathname_p)
{
  HDC hdc (GetDC (0));
  HGDIOBJ of (SelectObject (hdc, sysdep.ui_font ()));
  TEXTMETRIC tm;
  GetTextMetrics (hdc, &tm);
  int maxpxl = tm.tmAveCharWidth * maxl;
  int r = abbreviate_string (hdc, buf, maxpxl, pathname_p);
  SelectObject (hdc, of);
  ReleaseDC (0, hdc);
  return r;
}

lisp
Fabbreviate_display_string (lisp string, lisp maxlen, lisp pathname_p)
{
  check_string (string);
  int l = fixnum_value (maxlen);
  if (l <= 0)
    return make_string ("");
  char *buf = (char *)alloca (xstring_length (string) * 2 + 1);
  w2s (buf, string);
  if (!abbrev_string (buf, l, pathname_p && pathname_p != Qnil))
    return string;
  return make_string (buf);
}

lisp
Fabbreviate_string_column (lisp string, lisp column)
{
  check_string (string);
  int n = fixnum_value (column);
  const Char *const p0 = xstring_contents (string);
  const Char *const pe = p0 + xstring_length (string);
  const Char *p = p0;
  for (int c = 0; c < n && p < pe; p++)
    {
      c += char_width (*p);
      if (c > n)
        break;
    }
  return p == pe ? string : make_string (p0, p - p0);
}

static int
escseq_p (const Char *&p, const Char *pe)
{
  if (p == pe)
    return -1;

  switch (*p)
    {
    default:
      return -1;

    case 'f':
      p++;
      return CC_FF;

    case 'n':
      p++;
      return CC_NL;

    case 'r':
      p++;
      return CC_CR;

    case 't':
      p++;
      return CC_HT;

    case 'v':
      p++;
      return CC_VT;

    case 'x':
      pe = min (p + 3, pe);
      break;

    case 'X':
      pe = min (p + 5, pe);
      break;
    }

  const Char *p1 = p + 1;
  if (p1 == pe)
    return -1;
  int n = digit_char (*p1);
  if (n >= 16)
    return -1;
  for (p1++; p1 < pe; p1++)
    {
      int x = digit_char (*p1);
      if (x >= 16)
        break;
      n = n * 16 + x;
    }
  p = p1;
  return n;
}

lisp
Fdecode_escape_sequence (lisp string, lisp regexpp)
{
  check_string (string);
  const Char *p = xstring_contents (string);
  const Char *const pe = p + xstring_length (string);
  char tem[1024];
  StrBuf sb (tem, sizeof tem);
  int mod = 0;

  while (p < pe)
    {
      Char c = *p++;
      if (c == '\\' && p < pe)
        {
          if (*p == '\\')
            {
              p++;
              if (regexpp != Qnil)
                sb.add (c);
              else
                mod = 1;
            }
          else
            {
              int n = escseq_p (p, pe);
              if (n >= 0)
                {
                  c = n;
                  mod = 1;
                }
            }
        }
      sb.add (c);
    }

  return mod ? sb.make_string () : string;
}

lisp
Fsi_octet_length (lisp string, lisp keys)
{
  check_string (string);

  int start, end;
  string_start_end (string, start, end,
                    find_keyword (Kstart, keys, make_fixnum (0)),
                    find_keyword (Kend, keys, Qnil));
  lisp encoding = find_keyword (Kencoding, keys);
  if (encoding == Qnil)
    return make_fixnum (w2sl (xstring_contents (string) + start, end - start));

  check_char_encoding (encoding);
  if (xchar_encoding_type (encoding) == encoding_auto_detect)
    FEtype_error (encoding, Qchar_encoding);

  if (start != 0 || end != xstring_length (string))
    string = make_string (xstring_contents (string) + start, end - start);
  xstream_iChar_helper is (string);
  encoding_output_stream_helper s (encoding, is, eol_noconv);

  int r = 0;
  while (s->get () != xstream::eof)
    r++;
  return make_fixnum (r);
}
