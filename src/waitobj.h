// -*-C++-*-
#ifndef _waitobj_h_
# define _waitobj_h_

class lwait_object: public lisp_object
{
public:
  HANDLE hevent;
  int ref;

  ~lwait_object () {cleanup ();}
  void cleanup ();
};

# define wait_object_p(X) typep ((X), Twait_object)

inline void
check_wait_object (lisp x)
{
  check_type (x, Twait_object, Qwait_object);
}

inline HANDLE &
xwait_object_hevent (lisp x)
{
  assert (wait_object_p (x));
  return ((lwait_object *)x)->hevent;
}

inline int &
xwait_object_ref (lisp x)
{
  assert (wait_object_p (x));
  return ((lwait_object *)x)->ref;
}

inline lwait_object *
make_wait_object ()
{
  lwait_object *p = ldata <lwait_object, Twait_object>::lalloc ();
  p->hevent = 0;
  p->ref = 0;
  return p;
}

#endif
