// -*-C++-*-
#ifndef _closure_h_
# define _closure_h_

# include "number.h"

class lclosure: public lisp_object
{
public:
  lisp body;
  lisp vars;
  lisp fns;
  lisp frame;
  lisp name;
};

# define closurep(X) typep ((X), Tclosure)

inline lisp &
xclosure_body (lisp x)
{
  assert (closurep (x));
  return ((lclosure *)x)->body;
}

inline lisp &
xclosure_vars (lisp x)
{
  assert (closurep (x));
  return ((lclosure *)x)->vars;
}

inline lisp &
xclosure_fns (lisp x)
{
  assert (closurep (x));
  return ((lclosure *)x)->fns;
}

inline lisp &
xclosure_frame (lisp x)
{
  assert (closurep (x));
  return ((lclosure *)x)->frame;
}

inline lisp &
xclosure_name (lisp x)
{
  assert (closurep (x));
  return ((lclosure *)x)->name;
}

lclosure *make_closure (lisp body, lisp vars, lisp fns, lisp frame);

# define FRAME_ACTIVE 0
# define FRAME_INACTIVE 1

inline int
active_frame_p (lisp x)
{
  return xlong_int_value (x) == FRAME_ACTIVE;
}

inline void
set_frame_inactive (lisp x)
{
  xlong_int_value (x) = FRAME_INACTIVE;
}

inline lisp
make_frame ()
{
  return make_long_int (FRAME_ACTIVE);
}

class dynamic_extent
{
  lisp frame;
public:
  dynamic_extent (lisp f) : frame (f)
    {
      assert (long_int_p (frame));
    }
  ~dynamic_extent ()
    {
      set_frame_inactive (frame);
    }
};

#endif
