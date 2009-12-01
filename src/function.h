// -*-C++-*-
#ifndef _function_h_
# define _function_h_

/* function */

# define FFspecial_form (1 << 0)
# define FFmacro        (1 << 1)
# define FFpseudo_macro (1 << 2)
# define FFneed_rest    (1 << 3)
# define FFeditor       (1 << 4)

# ifndef NOT_COMPILE_TIME

class lfunction: public lisp_object
{
public:
  lfunction_proc fn;
  lisp name;
  u_char flags;
  u_char nargs;
  u_char nopts;
  u_char interactive;
#ifdef DEBUG_GC
  struct lfns *tab;
#endif
};

# define functionp(X) typep ((X), Tfunction)

inline lfunction_proc &
xfunction_fn (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->fn;
}

inline lisp &
xfunction_name (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->name;
}

inline u_char &
xfunction_flags (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->flags;
}

inline u_char &
xfunction_nargs (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->nargs;
}

inline u_char &
xfunction_nopts (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->nopts;
}

inline u_char &
xfunction_interactive (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->interactive;
}

#ifdef DEBUG_GC
inline struct lfns *&
xfunction_tab (lisp x)
{
  assert (functionp (x));
  return ((lfunction *)x)->tab;
}
#endif

# define special_form_p(X) (xfunction_flags (X) & FFspecial_form)
# define real_special_form_p(X) \
  ((xfunction_flags (X) & (FFspecial_form | FFmacro | FFpseudo_macro)) == FFspecial_form)
# define macro_function_p(X) (xfunction_flags (X) & (FFmacro | FFpseudo_macro))
# define expand_macro_function_p(X) (xfunction_flags (X) & FFmacro)
# define editor_function_p(X) (xfunction_flags (X) & FFeditor)
# define need_rest_p(X) (xfunction_flags (X) & FFneed_rest)

inline lfunction *
make_function (lfunction_proc fn, lisp name, u_char flags,
               u_char nargs, u_char nopts, u_char interactive)
{
  lfunction *p = ldata <lfunction, Tfunction>::lalloc ();
  p->fn = fn;
  p->name = name;
  p->flags = flags;
  p->nargs = nargs;
  p->nopts = nopts;
  p->interactive = interactive;
  return p;
}

# endif /* not NOT_COMPILE_TIME */

#endif
