// -*-C++-*-
#ifndef _char_h_
# define _char_h_

# include "chtype.h"
# include "charset.h"

inline lisp
make_char (Char c)
{
  return make_immediate (Lchar, c);
}

inline int
charp (lisp x)
{
  return lowbits (pointer_t (x)) == Lchar;
}

inline Char
xchar_code (lisp x)
{
  assert (charp (x));
  return Char (ximmediate_data (x));
}

inline void
check_char (lisp x)
{
  if (!charp (x))
    FEtype_error (x, Qcharacter);
}

#endif
