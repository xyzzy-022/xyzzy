// -*-C++-*-
#ifndef _string_h_
# define _string_h_

/* string */

# include "array.h"

# define MAX_STRING_LENGTH (INT_MAX / sizeof (Char))

class lsimple_string: public lbase_vector
{
};

class lcomplex_string: public lbase_complex_vector
{
};

# define stringp(X) \
  object_type_mask_p ((X), TAvector | TAstring, TAvector | TAstring)
# define simple_string_p(X) typep ((X), Tsimple_string)
# define complex_string_p(X) typep ((X), Tcomplex_string)

inline void
check_string (lisp x)
{
  if (!stringp (x))
    FEtype_error (x, Qstring);
}

inline void
check_simple_string (lisp x)
{
  check_type (x, Tsimple_string, Qsimple_string);
}

# define xstring_length xvector_length
# define xstring_dimension xvector_dimension

inline Char *&
xstring_contents (lisp x)
{
  assert (stringp (x));
  return (Char *&)xbase_vector_contents (x);
}

inline lsimple_string *
make_simple_string ()
{
  lsimple_string *p = ldata <lsimple_string, Tsimple_string>::lalloc ();
  p->common_init ();
  return p;
}

inline lcomplex_string *
make_complex_string ()
{
  lcomplex_string *p = ldata <lcomplex_string, Tcomplex_string>::lalloc ();
  p->common_init ();
  return p;
}

inline int
string_equal (lisp x, lisp y)
{
  assert (stringp (x));
  assert (stringp (y));
  return (xstring_length (x) == xstring_length (y)
          && !bcmp (xstring_contents (x), xstring_contents (y),
                    xstring_length (x)));
}

int string_equalp (const Char *, int, const char *, int);
int string_equalp (const Char *, int, const Char *, int);

inline int
string_equalp (lisp x, lisp y)
{
  return string_equalp (xstring_contents (x), xstring_length (x),
                        xstring_contents (y), xstring_length (y));
}

int string_equalp (lisp, int, lisp, int, int);

lisp parse_integer (lisp, int, int &, int, int);

int update_column (int, Char);
int update_column (int, const Char *, int);
int update_column (int, Char, int);
size_t s2wl (const char *);
Char *s2w (Char *, size_t, const char **);
Char *s2w (Char *, const char *);
Char *s2w (const char *, size_t);
Char *a2w (Char *, size_t, const char **);
void a2w (Char *, const char *, size_t);
Char *a2w (Char *, const char *);
Char *a2w (const char *, size_t);
size_t w2sl (const Char *, size_t);
char *w2s (char *, const Char *, size_t);
char *w2s (const Char *, size_t);
char *w2s (char *, char *, const Char *, size_t);
char *w2s_quote (char *, char *, const Char *, size_t, int, int);

size_t s2wl (const char *string, const char *se, int zero_term);
Char *s2w (Char *b, const char *string, const char *se, int zero_term);
void w2s_chunk (char *, char *, const Char *, size_t);

ucs2_t *i2w (const Char *, int, ucs2_t *);
int i2wl (const Char *, int);

lisp coerce_to_string (lisp, int);

lisp make_string (const char *);
lisp make_string (const u_char *);
lisp make_string (const char *, size_t);
lisp make_string_simple (const char *, size_t);
lisp make_string (const Char *, size_t);
lisp make_string (Char, size_t);
lisp make_complex_string (Char, int, int, int);
lisp make_string_from_list (lisp);
lisp make_string_from_vector (lisp);
lisp make_string (size_t);
lisp copy_string (lisp);

void string_start_end (lisp, int &, int &, lisp, lisp);
lisp subseq_string (lisp, lisp, lisp);

u_int hashpjw (const Char *, int);
u_int ihashpjw (const Char *, int);

inline u_int
hashpjw (lisp string)
{
  assert (stringp (string));
  return hashpjw (xstring_contents (string), xstring_length (string));
}

inline u_int
ihashpjw (lisp string)
{
  assert (stringp (string));
  return ihashpjw (xstring_contents (string), xstring_length (string));
}

inline u_int
hashpjw (lisp string, u_int prime)
{
  return hashpjw (string) % prime;
}

inline u_int
ihashpjw (lisp string, u_int prime)
{
  return ihashpjw (string) % prime;
}

inline size_t
w2sl (lisp l)
{
  return w2sl (xstring_contents (l), xstring_length (l));
}

inline char *
w2s (char *b, lisp l)
{
  return w2s (b, xstring_contents (l), xstring_length (l));
}

inline char *
w2s (lisp l)
{
  return w2s (xstring_contents (l), xstring_length (l));
}

inline char *
w2s (char *b, char *be, lisp l)
{
  return w2s (b, be, xstring_contents (l), xstring_length (l));
}

inline char *
w2s_quote (char *b, char *be, lisp l, int qc, int qe)
{
  return w2s_quote (b, be, xstring_contents (l), xstring_length (l), qc, qe);
}

inline ucs2_t *
i2w (lisp x, ucs2_t *b)
{
  return i2w (xstring_contents (x), xstring_length (x), b);
}

inline int
i2wl (lisp x)
{
  return i2wl (xstring_contents (x), xstring_length (x));
}

#endif
