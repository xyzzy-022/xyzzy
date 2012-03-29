// -*-C++-*-
#ifndef _cons_h_
# define _cons_h_

class lcons: public lisp_object
{
public:
  lisp car;
  lisp cdr;
};

# define consp(X) typep ((X), Tcons)
# define listp(X) ((X) == Qnil || consp (X))

inline void
check_cons (lisp x)
{
  check_type (x, Tcons, Qcons);
}

inline lisp &
xcar (lisp x)
{
  assert (consp (x));
  return ((lcons *)x)->car;
}

inline lisp &
xcdr (lisp x)
{
  assert (consp (x));
  return ((lcons *)x)->cdr;
}

inline lcons *
make_cons (lisp x = Qnil, lisp y = Qnil)
{
  lcons *p = ldata <lcons, Tcons>::lalloc ();
  p->car = x;
  p->cdr = y;
  return p;
}

inline lisp
xcons (lisp x, lisp y)
{
  return make_cons (x, y);
}

#endif
