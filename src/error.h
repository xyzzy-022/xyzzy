// -*-C++-*-
#ifndef _error_h_
# define _error_h_

enum {CRTL_ERROR, WIN32_ERROR};

class lerror: public lisp_object
{
public:
  int type;
  int number;
};

# define errorp(X) typep ((X), Terror)

inline int &
xerror_type (lisp x)
{
  assert (errorp (x));
  return ((lerror *)x)->type;
}

inline int &
xerror_number (lisp x)
{
  assert (errorp (x));
  return ((lerror *)x)->number;
}

inline lerror *
make_error (int type, int number)
{
  lerror *p = ldata <lerror, Terror>::lalloc ();
  p->type = type;
  p->number = number;
  return p;
}

#endif
