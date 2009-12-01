// -*-C++-*-
#ifndef _vector_h_
# define _vector_h_

/* vector */

# include "array.h"

class lsimple_vector: public lbase_vector
{
};

class lcomplex_vector: public lbase_complex_vector
{
};

# define general_vector_p(X) \
  object_type_mask_p ((X), TAvector | TAgeneral, TAvector | TAgeneral)
# define simple_vector_p(X) typep ((X), Tsimple_vector)
# define complex_vector_p(X) typep ((X), Tcomplex_vector)

inline void
check_general_vector (lisp x)
{
  if (!general_vector_p (x))
    FEtype_error (x, Qvector);
}

inline void
check_simple_vector (lisp x)
{
  check_type (x, Tsimple_vector, Qsimple_vector);
}

inline lisp *&
xvector_contents (lisp x)
{
  assert (general_vector_p (x));
  return (lisp *&)xbase_vector_contents (x);
}

inline lsimple_vector *
make_simple_vector ()
{
  lsimple_vector *p = ldata <lsimple_vector, Tsimple_vector>::lalloc ();
  p->common_init ();
  return p;
}

inline lcomplex_vector *
make_complex_vector ()
{
  lcomplex_vector *p = ldata <lcomplex_vector, Tcomplex_vector>::lalloc ();
  p->common_init ();
  return p;
}

lisp alloc_vector (int);
lisp make_vector_from_list (lisp, int);
lisp make_vector (int, lisp);
lisp make_vector (int, int, lisp);
lisp copy_vector (lisp);
lisp subseq_vector (lisp, lisp, lisp);
int realloc_element (lisp, int, int);
u_int ihashpjw (const lisp *, const lisp *);

#endif
