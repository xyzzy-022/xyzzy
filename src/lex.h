// -*-C++-*-
#ifndef _lex_h_
# define _lex_h_

class lex_env
{
  static lex_env *le;
  lex_env *last;
  void chain ();
public:
  lisp lex_var;
  lisp lex_frame;
  lisp lex_fns;
  lisp lex_ltail;
  lex_env ();
  lex_env (lisp, lisp, lisp);
  lex_env (const lex_env &);
  lex_env (lisp);
  ~lex_env ();
  void let (lisp, lex_env &);
  void bind (lisp, lisp);
  int set (lisp, lisp);
  lisp do_environment (lisp);
  void lambda_bind (lisp, lisp, lisp, int);
  void bind_frame (lisp, lisp, lisp);
  lisp search_frame (lisp, lisp);

  friend void gc_mark_object ();
};

inline void
lex_env::chain ()
{
  last = le;
  le = this;
}

inline
lex_env::lex_env ()
{
  lex_var = Qnil;
  lex_fns = Qnil;
  lex_frame = Qnil;
  lex_ltail = Qnil;
  chain ();
}

inline
lex_env::lex_env (lisp v, lisp f, lisp b)
{
  lex_var = v;
  lex_fns = f;
  lex_frame = b;
  lex_ltail = v;
  chain ();
}

inline
lex_env::lex_env (const lex_env &e)
{
  lex_var = e.lex_var;
  lex_fns = e.lex_fns;
  lex_frame = e.lex_frame;
  lex_ltail = e.lex_var;
  chain ();
}

inline
lex_env::~lex_env ()
{
  le = last;
}

inline void
lex_env::bind (lisp var, lisp val)
{
  check_symbol (var);
  if (constantp (var))
    FEmodify_constant (var);
  lex_var = xcons (xcons (var, val), lex_var);
}

// (TYPE TAG . FRAME)
inline void
lex_env::bind_frame (lisp type, lisp tag, lisp frame)
{
  lex_frame = xcons (xcons (type, xcons (tag, frame)), lex_frame);
}

class lenvironment: public lisp_object
{
public:
  lisp lvar;
  lisp lframe;
  lisp lfns;
};

#define environmentp(X) typep ((X), Tenvironment)

inline void
check_environment (lisp x)
{
  check_type (x, Tenvironment, Qenvironment);
}

inline lisp &
xenvironment_var (lisp x)
{
  assert (environmentp (x));
  return ((lenvironment *)x)->lvar;
}

inline lisp &
xenvironment_frame (lisp x)
{
  assert (environmentp (x));
  return ((lenvironment *)x)->lframe;
}

inline lisp &
xenvironment_fns (lisp x)
{
  assert (environmentp (x));
  return ((lenvironment *)x)->lfns;
}

inline lenvironment *
make_environment (const lex_env &e)
{
  lenvironment *p = ldata <lenvironment, Tenvironment>::lalloc ();
  p->lvar = e.lex_var;
  p->lframe = e.lex_frame;
  p->lfns = e.lex_fns;
  return p;
}

#endif
