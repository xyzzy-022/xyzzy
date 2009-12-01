#ifndef _trace_h_
# define _trace_h_

struct stack_trace
{
  enum
    {
      empty,
      special_form,
      macro,
      apply,
      eval_args
    };
  static stack_trace *stp;
  stack_trace *last;
  int type;
  lisp fn;
  lisp args[2];

  stack_trace ();
  stack_trace (int, lisp, lisp);
  stack_trace (int, lisp, lisp, lisp);
  ~stack_trace ();
  void set (int, lisp, lisp);
  void set (int, lisp, lisp, lisp);
  void drop ();
  void chain ();
};

inline void
stack_trace::chain ()
{
  last = stp;
  stp = this;
}

inline void
stack_trace::set (int t, lisp f, lisp a)
{
  type = t;
  fn = f;
  args[0] = 0;
  args[1] = a;
}

inline void
stack_trace::set (int t, lisp f, lisp a1, lisp a2)
{
  type = t;
  fn = f;
  args[0] = a1;
  args[1] = a2;
}

inline void
stack_trace::drop ()
{
  type = empty;
}

inline
stack_trace::stack_trace ()
{
  chain ();
  drop ();
}

inline
stack_trace::stack_trace (int t, lisp f, lisp a)
{
  chain ();
  set (t, f, a);
}

inline
stack_trace::stack_trace (int t, lisp f, lisp a1, lisp a2)
{
  chain ();
  set (t, f, a1, a2);
}

inline
stack_trace::~stack_trace ()
{
  stp = last;
}
#endif
