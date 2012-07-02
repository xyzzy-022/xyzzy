#include "stdafx.h"
#include "ed.h"
#include "lex.h"
#ifdef DEBUG_GC
#include "symtable.h"
#endif

static multiple_value_data default_mvalue = {1};
multiple_value_data *multiple_value::d = &default_mvalue;

stack_trace *stack_trace::stp;
#ifndef DEBUG
int gc_threshold = 150000;
#else
int gc_threshold = 1000;
#endif
long bignum_gc_threshold = 5 * 1024 * 1024;

class check_stack_depth
{
  char *limit;
public:
  check_stack_depth ();
  void check ();
};

check_stack_depth::check_stack_depth ()
{
  MEMORY_BASIC_INFORMATION mbi;
  VirtualQuery (&mbi, &mbi, sizeof mbi);
  limit = (char *)mbi.AllocationBase + 512 * 1024;
}

inline void
check_stack_depth::check ()
{
  char st;
  if (&st < limit)
    FEstack_overflow ();
}

static check_stack_depth stack_depth;

void
check_stack_overflow ()
{
  stack_depth.check ();
}

lclosure *
make_closure (lisp body, lisp vars, lisp fns, lisp frame)
{
  lclosure *p = ldata <lclosure, Tclosure>::lalloc ();
  p->body = body;
  p->vars = vars;
  p->fns = fns;
  p->frame = frame;
  p->name = Qnil;
  return p;
}

lisp
Fquote (lisp arg, lex_env &)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  if (xcdr (arg) != Qnil)
    FEtoo_many_arguments ();
  return xcar (arg);
}

lisp
Ffunction (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  if (xcdr (arg) != Qnil)
    FEtoo_many_arguments ();
  lisp fn = xcar (arg);
  if (!immediatep (fn))
    switch (object_typeof (fn))
      {
      case Tsymbol:
        {
          lisp f = assq (fn, lex.lex_fns);
          if (!f)
            return Fsymbol_function (fn);
          if (consp (f))
            {
              fn = xcdr (f);
              if (closurep (fn))
                return fn;
            }
          break;
        }

      case Tclosure:
        return fn;

      case Tfunction:
        if (special_form_p (fn))
          FEtype_error (fn, Qfunction);
        return fn;

      case Tcons:
        if (xcar (fn) == Qlambda)
          return make_closure (fn, lex.lex_var, lex.lex_fns, lex.lex_frame);
        break;

      case Tc_callable:
        return xc_callable_function (fn);
      }
  return FEinvalid_function (fn);
}

lisp
Fsetq (lisp arg, lex_env &lex)
{
  lisp val = Qnil;
  for (; consp (arg); arg = xcdr (arg))
    {
      lisp var = xcar (arg);
      check_symbol (var);
      if (constantp (var))
        FEmodify_constant (var);
      arg = xcdr (arg);
      if (!consp (arg))
        FEtoo_few_arguments ();
      val = eval (xcar (arg), lex);
      if (specialp (var) || !lex.set (var, val))
        set_globally (var, val, selected_buffer ());
    }
  multiple_value::clear ();
  return val;
}

lisp
Fset (lisp var, lisp val)
{
  check_symbol (var);
  if (constantp (var))
    FEmodify_constant (var);
  set_globally (var, val, selected_buffer ());
  return val;
}

lisp
Fsi_symbol_value (lisp symbol)
{
  check_symbol (symbol);
  return symbol_value (symbol, selected_buffer ());
}

lisp
Fsymbol_value (lisp symbol)
{
  check_symbol (symbol);
  lisp value = symbol_value (symbol, selected_buffer ());
  if (value == Qunbound)
    FEunbound_variable (symbol);
  return value;
}

lisp
Fdefault_value (lisp symbol)
{
  check_symbol (symbol);
  lisp value = xsymbol_value (symbol);
  if (value == Qunbound)
    FEunbound_variable (symbol);
  return value;
}

lisp
Fset_default (lisp var, lisp val)
{
  check_symbol (var);
  if (constantp (var))
    FEmodify_constant (var);
  xsymbol_value (var) = val;
  return val;
}

lisp
Fspecial_form_p (lisp symbol)
{
  check_symbol (symbol);
  lisp fn = xsymbol_function (symbol);
  if (functionp (fn) && special_form_p (fn) && !expand_macro_function_p (fn))
    return fn;
  return Qnil;
}

lisp
Fmacro_function (lisp symbol, lisp env)
{
  check_symbol (symbol);
  lisp fn = xsymbol_function (symbol);
  if (env)
    {
      lisp x = assq (symbol, environmentp (env) ? xenvironment_fns (env) : env);
      if (x && consp (x))
        fn = xcdr (x);
    }

  if ((functionp (fn) && expand_macro_function_p (fn))
      || (consp (fn) && xcar (fn) == Qmacro))
    return fn;
  return Qnil;
}

lisp
Fsymbol_function (lisp symbol)
{
  check_symbol (symbol);
  lisp fn = xsymbol_function (symbol);
  if (fn == Qunbound)
    FEundefined_function (symbol);
  return fn;
}

lisp
Fboundp (lisp x)
{
  check_symbol (x);
  return boole (symbol_value (x, selected_buffer ()) != Qunbound);
}

lisp
Ffboundp (lisp x)
{
  check_symbol (x);
  return boole (xsymbol_function (x) != Qunbound);
}

lisp
Fconstantp (lisp x)
{
  if (consp (x))
    return boole (xcar (x) == Qquote);
  if (symbolp (x))
    return boole (constantp (x));
  return Qt;
}

lisp
Fsi_specialp (lisp x)
{
  return boole (symbolp (x) && specialp (x));
}

lisp
Fsi_make_constant (lisp x)
{
  check_symbol (x);
  if (!constantp (x) && specialp (x))
    FEprogram_error (Eis_special_variable, x);
  xsymbol_flags (x) |= SFconstant | SFspecial;
  return x;
}

lisp
Fsi_make_special (lisp x)
{
  check_symbol (x);
  if (constantp (x))
    FEprogram_error (Eis_constant_variable, x);
  xsymbol_flags (x) |= SFspecial;
  return x;
}

class special_bind
{
  lisp *vec;
  char *flags;
  int n;

public:
  special_bind (lisp *, char *, int);
  ~special_bind ();
};

inline
special_bind::special_bind (lisp *v, char *f, int nv)
     : vec (v), flags (f), n (nv)
{
}

inline
special_bind::~special_bind ()
{
  for (int i = n - 2, j = n/2 - 1; i >= 0; i -= 2, j--)
    {
      assert (consp (vec[i]));
      assert (symbolp (xcar (vec[i])));
      assert (xcdr (vec[i]) == Qunbound);
      lisp sym = xcar (vec[i]);
      if (!flags[j])
        xsymbol_flags (sym) &= ~SFdynamic_bind;
      else
        assert (xsymbol_flags (sym) & SFdynamic_bind);
      xcdr (vec[i]) = xsymbol_value (sym);
      xsymbol_value (sym) = vec[i + 1];
    }
}

static lisp
declare_progn (lisp body, lex_env &lex, int can_doc)
{
  int nvars = 0;
  int nspecials = 0;

  for (lisp e = lex.lex_var; e != lex.lex_ltail; e = xcdr (e))
    {
      lisp x = xcar (e);
      if (consp (x) && symbolp (xcar (x))
          && specialp (xcar (x)) && xcdr (x) != Qunbound)
        nspecials++;
    }

  int doc = can_doc;
  lisp nbody;
  for (nbody = body; consp (nbody); nbody = xcdr (nbody))
    {
      lisp x = xcar (nbody);
      if (doc && stringp (x))
        {
          if (!consp (xcdr (nbody)))
            break;
          doc = 0;
          continue;
        }
      if (!consp (x) || xcar (x) != Qdeclare)
        break;
      for (x = xcdr (x); consp (x); x = xcdr (x))
        {
          lisp t = xcar (x);
          if (consp (t) && xcar (t) == Qspecial)
            for (t = xcdr (t); consp (t); t = xcdr (t))
              {
                lisp sym = xcar (t);
                if (symbolp (sym))
                  nvars++;
                QUIT;
              }
          QUIT;
        }
      QUIT;
    }

  if (!nspecials && !nvars)
    return Fprogn (nbody, lex);

  int nflags = (nspecials + nvars + sizeof (lisp) - 1) & ~(sizeof (lisp) - 1);
  char *oflags = (char *)alloca (2 * sizeof (lisp) * (nspecials + nvars) + nflags);
  lisp *save = (lisp *)(oflags + nflags);
  int i = 0, j = 0;

  if (nspecials)
    {
      lisp var = Qnil;
      for (lisp e = lex.lex_var; e != lex.lex_ltail; e = xcdr (e))
        var = xcons (xcar (e), var);
      for (lisp e = var; consp (e); e = xcdr (e))
        {
          lisp x = xcar (e);
          if (consp (x) && symbolp (xcar (x))
              && specialp (xcar (x)) && xcdr (x) != Qunbound)
            {
              lisp sym = xcar (x);
              oflags[j++] = xsymbol_flags (sym) & SFdynamic_bind;
              xsymbol_flags (sym) |= SFdynamic_bind;
              save[i++] = x;
              save[i++] = xsymbol_value (sym);
              xsymbol_value (sym) = xcdr (x);
              xcdr (x) = Qunbound;
            }
        }
    }

  if (nvars)
    {
      doc = can_doc;
      for (; consp (body); body = xcdr (body))
        {
          lisp x = xcar (body);
          if (doc && stringp (x))
            {
              if (!consp (xcdr (body)))
                break;
              doc = 0;
              continue;
            }
          if (!consp (x) || xcar (x) != Qdeclare)
            break;
          for (x = xcdr (x); consp (x); x = xcdr (x))
            {
              lisp t = xcar (x);
              if (consp (t) && xcar (t) == Qspecial)
                for (t = xcdr (t); consp (t); t = xcdr (t))
                  {
                    lisp sym = xcar (t);
                    if (symbolp (sym))
                      {
                        lisp x = assq (sym, lex.lex_var);
                        if (x && xcdr (x) != Qunbound)
                          {
                            oflags[j++] = xsymbol_flags (sym) & SFdynamic_bind;
                            xsymbol_flags (sym) |= SFdynamic_bind;
                            save[i++] = x;
                            save[i++] = xsymbol_value (sym);
                            xsymbol_value (sym) = xcdr (x);
                            xcdr (x) = Qunbound;
                          }
                      }
                  }
            }
        }
    }

  if (!i)
    return Fprogn (body, lex);

  assert (i <= 2 * (nvars + nspecials));
  protect_gc gcpro (save, i);
  special_bind sb (save, oflags, i);
  return Fprogn (body, lex);
}

static lisp
funcall_lambda (lisp sexp, lisp arglist, lex_env &olex)
{
  assert (consp (sexp) && xcar (sexp) == Qlambda);

  protect_gc gcpro (sexp);

  lisp f = xcdr (sexp);
  if (!consp (f))
    FEinvalid_lambda_list (Qnil);
  lisp lambda_list = xcar (f);
  lisp body = xcdr (f);
  lex_env nlex (olex);
  nlex.lambda_bind (lambda_list, arglist, Qnil, 0);
  lisp r = declare_progn (body, nlex, 1);
  if (bignum_allocated_bytes > bignum_gc_threshold * 2
      && !suppress_gc::gc_suppressed_p ())
    {
      save_multiple_value save_mvalue (r);
      gc (-1);
    }
  return r;
}

static inline lisp
call_special_form (lisp f, lisp arg, lex_env &lex)
{
  assert (special_form_p (f));
#ifdef DEBUG_GC
  MARK_FUNCALL (f);
#endif
  return ((lisp (*)(lisp, lex_env &))xfunction_fn (f))(arg, lex);
}

lisp
funcall_builtin (lisp f, lisp arglist)
{
  assert (functionp (f));
#ifdef _M_IX86
  int nargs = xfunction_nargs (f) + xfunction_nopts (f) + (need_rest_p (f) ? 1 : 0);
  lisp *stack = (lisp *)alloca (sizeof (lisp) * nargs);
  for (int i = xfunction_nargs (f); i > 0; i--)
    {
      if (!consp (arglist))
        FEtoo_few_arguments ();
      *stack++ = xcar (arglist);
      arglist = xcdr (arglist);
    }

  for (int i = xfunction_nopts (f); i > 0; i--)
    {
      if (!consp (arglist))
        {
          for (; i > 0; i--)
            *stack++ = 0;
          break;
        }
      *stack++ = xcar (arglist);
      arglist = xcdr (arglist);
    }

  if (need_rest_p (f))
    *stack = consp (arglist) ? arglist : Qnil;
  else if (consp (arglist))
    FEtoo_many_arguments ();

#ifdef DEBUG_GC
  MARK_FUNCALL (f);
#endif
  return lfunction_proc_0 (xfunction_fn (f))();
#else
# error "Not tested"
#endif
}

static lisp
eval_args (lisp args, lex_env &lex)
{
  if (!consp (args))
    return Qnil;
  lisp p = xcons (eval (xcar (args), lex), Qnil);
  lisp new_args = p;
  protect_gc gcpro (new_args);
  while (1)
    {
      args = xcdr (args);
      if (!consp (args))
        break;
      xcdr (p) = xcons (eval (xcar (args), lex), Qnil);
      p = xcdr (p);
    }
  multiple_value::clear ();
  return new_args;
}

static lisp
macroexpand (const lex_env &lex, lisp body, lisp arg)
{
  lex_env nlex (lex);
  nlex.lambda_bind (xcar (body), xcdr (arg), arg, 1);
  return declare_progn (xcdr (body), nlex, 1);
}

static int
call_evalhook (lisp form, const lex_env &lex, lisp &r)
{
  lisp hook = xsymbol_value (Vevalhook);
  if (hook == Qnil)
    return 0;
  if (xsymbol_value (Vbypass_evalhook) != Qnil)
    {
      xsymbol_value (Vbypass_evalhook) = Qnil;
      return 0;
    }
  protect_gc gcpro (form);
  dynamic_bind dynb (Vevalhook, Qnil);
  r = funcall_2 (hook, form, make_environment (lex));
  return 1;
}

static int
call_applyhook (lisp fn, lisp args, lisp &r)
{
  lisp hook = xsymbol_value (Vapplyhook);
  if (hook == Qnil)
    return 0;
  if (xsymbol_value (Vbypass_applyhook) != Qnil)
    {
      xsymbol_value (Vbypass_applyhook) = Qnil;
      return 0;
    }
  protect_gc gcpro1 (fn);
  protect_gc gcpro2 (args);
  dynamic_bind dynb (Vapplyhook, Qnil);
  r = funcall_2 (hook, fn, args);
  return 1;
}

lisp
eval (lisp arg, lex_env &lex)
{
  multiple_value::clear ();

  QUIT;

  lisp r;
  if (call_evalhook (arg, lex, r))
    return r;

  if (immediatep (arg))
    return arg;

  int type = object_typeof (arg);
  if (typep (type, Tsymbol))
    {
      if (!specialp (arg))
        {
          lisp x = assq (arg, lex.lex_var);
          if (x && xcdr (x) != Qunbound)
            return xcdr (x);
        }

      lisp x = symbol_value (arg, selected_buffer ());
      if (x == Qunbound)
        FEunbound_variable (arg);
      return x;
    }

  if (!typep (type, Tcons))
    return arg;

  stack_depth.check ();

  protect_gc gcpro1 (arg);
  lisp real_name = xcar (arg);
  lisp arglist = xcdr (arg);

  if ((ldataP::ld_nwasted > gc_threshold
       || bignum_allocated_bytes > bignum_gc_threshold)
      && !suppress_gc::gc_suppressed_p ())
    gc (-1);

  lisp f;
  if (!symbolp (real_name))
    f = real_name;
  else
    {
      lisp x = assq (real_name, lex.lex_fns);
      f = x && consp (x) ? xcdr (x) : xsymbol_function (real_name);
      if (f == Qunbound)
        FEundefined_function (real_name);
    }

  if (immediatep (f))
    FEinvalid_function (real_name);

  stack_trace trace;
  protect_gc gcpro2 (f);
  protect_gc gcpro3 (arglist);

  type = object_typeof (f);
  if (functionp (type))
    {
      if (special_form_p (f))
        {
          if (expand_macro_function_p (f))
            {
              trace.set (stack_trace::macro, real_name, arglist);
              f = call_special_form (f, arglist, lex);
#if 0
              trace.drop ();
#endif
              return eval (f, lex);
            }
          trace.set (stack_trace::special_form, real_name, arglist);
          return call_special_form (f, arglist, lex);
        }

      trace.set (stack_trace::eval_args, real_name, Qnil);
      arglist = eval_args (arglist, lex);
      if (call_applyhook (f, arglist, r))
        return r;
      trace.set (stack_trace::apply, real_name, arglist);
      return funcall_builtin (f, arglist);
    }

  if (closurep (type))
    {
      trace.set (stack_trace::eval_args, real_name, Qnil);
      arglist = eval_args (arglist, lex);
      if (call_applyhook (f, arglist, r))
        return r;
      lex_env nlex (xclosure_vars (f), xclosure_fns (f), xclosure_frame (f));
      trace.set (stack_trace::apply, real_name, arglist);
      return funcall_lambda (xclosure_body (f), arglist, nlex);
    }

  if (dll_function_p (type))
    {
      trace.set (stack_trace::eval_args, real_name, Qnil);
      arglist = eval_args (arglist, lex);
      if (call_applyhook (f, arglist, r))
        return r;
      trace.set (stack_trace::apply, real_name, arglist);
      return funcall_dll (f, arglist);
    }

  if (c_callable_p (type))
    {
      trace.set (stack_trace::eval_args, real_name, Qnil);
      arglist = eval_args (arglist, lex);
      if (call_applyhook (f, arglist, r))
        return r;
      trace.set (stack_trace::apply, real_name, arglist);
      return funcall_c_callable (f, arglist);
    }

  if (consp (type))
    {
      if (xcar (f) == Qlambda)
        {
          trace.set (stack_trace::eval_args, real_name, Qnil);
          arglist = eval_args (arglist, lex);
          if (call_applyhook (f, arglist, r))
            return r;
          trace.set (stack_trace::apply, real_name, arglist);
          return funcall_lambda (f, arglist, lex);
        }

      if (xcar (f) == Qmacro && f != real_name)
        {
          f = xcdr (f);
          if (!consp (f))
            FEbad_macro_form (real_name);
          trace.set (stack_trace::macro, real_name, arglist);
          f = macroexpand (lex, f, arg);
          return eval (f, lex);
        }
    }
  return FEinvalid_function (real_name);
}

lisp
Feval (lisp arg)
{
  lex_env lex;
  enable_quit eq;
  return eval (arg, lex);
}

lisp
Fevalhook (lisp form, lisp evalhook, lisp applyhook, lisp env)
{
  lex_env lex (env ? env : Qnil);
  dynamic_bind dynb1 (Vevalhook, evalhook);
  dynamic_bind dynb2 (Vapplyhook, applyhook);
  dynamic_bind dynb3 (Vbypass_evalhook, boole (evalhook != Qnil));
  dynamic_bind dynb4 (Vbypass_applyhook, Qnil);
  enable_quit eq;
  return eval (form, lex);
}

lisp
Ffuncall (lisp fn, lisp args)
{
  QUIT;

  stack_depth.check ();

  lisp r;
  if (call_applyhook (fn, args, r))
    return r;

  if (immediatep (fn))
    FEinvalid_function (fn);

  lisp f;
  int type = object_typeof (fn);
  if (!symbolp (type))
    f = fn;
  else
    {
      f = xsymbol_function (fn);
      if (f == Qunbound)
        FEundefined_function (fn);
      if (immediatep (f))
        FEinvalid_function (fn);
      type = object_typeof (f);
    }

  protect_gc gcpro1 (args);
  protect_gc gcpro2 (fn);

  if ((ldataP::ld_nwasted > gc_threshold
       || bignum_allocated_bytes > bignum_gc_threshold)
      && !suppress_gc::gc_suppressed_p ())
    gc (-1);

  enable_quit eq;

  multiple_value::clear ();

  stack_trace trace;
  if (functionp (type))
    {
      if (special_form_p (f))
        FEtype_error (fn, Qfunction);
      trace.set (stack_trace::apply, fn, args);
      return funcall_builtin (f, args);
    }

  if (closurep (type))
    {
      lex_env lex (xclosure_vars (f), xclosure_fns (f), xclosure_frame (f));
      trace.set (stack_trace::apply, fn, args);
      return funcall_lambda (xclosure_body (f), args, lex);
    }

  if (dll_function_p (type))
    {
      trace.set (stack_trace::apply, fn, args);
      return funcall_dll (f, args);
    }

  if (c_callable_p (type))
    {
      trace.set (stack_trace::apply, fn, args);
      return funcall_c_callable (f, args);
    }

  if (consp (type) && xcar (f) == Qlambda)
    {
      lex_env lex;
      trace.set (stack_trace::apply, fn, args);
      return funcall_lambda (f, args, lex);
    }

  return FEinvalid_function (fn);
}

lisp
Fapply (lisp fn, lisp args)
{
  args = Flist_star (args);
  protect_gc gcpro (args);
  return Ffuncall (fn, args);
}

lisp
Fapplyhook (lisp fn, lisp args, lisp evalhook, lisp applyhook)
{
  dynamic_bind dynb1 (Vevalhook, evalhook);
  dynamic_bind dynb2 (Vapplyhook, applyhook);
  dynamic_bind dynb3 (Vbypass_evalhook, Qnil);
  dynamic_bind dynb4 (Vbypass_applyhook, boole (applyhook != Qnil));
  enable_quit eq;
  return Ffuncall (fn, args);
}

lisp
funcall_1 (lisp fn, lisp arg)
{
  suppress_gc sgc;
  arg = xcons (arg, Qnil);
  protect_gc gcpro (arg);
  return Ffuncall (fn, arg);
}

lisp
funcall_2 (lisp fn, lisp arg1, lisp arg2)
{
  suppress_gc sgc;
  lisp args = xcons (arg1, xcons (arg2, Qnil));
  protect_gc gcpro (args);
  return Ffuncall (fn, args);
}

lisp
funcall_3 (lisp fn, lisp arg1, lisp arg2, lisp arg3)
{
  suppress_gc sgc;
  lisp args = xcons (arg1, xcons (arg2, xcons (arg3, Qnil)));
  protect_gc gcpro (args);
  return Ffuncall (fn, args);
}

lisp
funcall_4 (lisp fn, lisp arg1, lisp arg2, lisp arg3, lisp arg4)
{
  suppress_gc sgc;
  lisp args = xcons (arg1, xcons (arg2, xcons (arg3, xcons (arg4, Qnil))));
  protect_gc gcpro (args);
  return Ffuncall (fn, args);
}

lfunction_proc
fast_funcall_p (lisp fn, int nargs)
{
  if (symbolp (fn))
    fn = xsymbol_function (fn);
  return ((functionp (fn)
           && !special_form_p (fn)
           && !need_rest_p (fn)
           && xfunction_nargs (fn) + xfunction_nopts (fn) == nargs)
          ? (
#ifdef DEBUG_GC
            MARK_FUNCALL (fn),
#endif
            xfunction_fn (fn))
          : 0);
}

// vec = #(result-list call-argument-list arg1 arg2 ... argn)
#define MAP_VECSIZE(NARGS) ((NARGS) + 2)
#define MAP_RESULT(VEC) ((VEC)[0])
#define MAP_CALLARG(VEC) ((VEC)[1])
#define MAP_ARGS(VEC) (&(VEC)[2])

static int
map_count (lisp lists)
{
  int nargs, f_nil;
  for (nargs = 0, f_nil = 0; consp (lists); nargs++, lists = xcdr (lists))
    {
      QUIT;
      if (xcar (lists) == Qnil)
        f_nil = 1;
      else
        check_cons (xcar (lists));
    }
  if (f_nil)
    return 0;
  if (!nargs)
    FEtoo_few_arguments ();
  return nargs;
}

static void
map_first (lisp lists, lisp *vec)
{
  MAP_RESULT (vec) = Qnil;
  MAP_CALLARG (vec) = Qnil;
  for (lisp *v = MAP_ARGS (vec); consp (lists); v++, lists = xcdr (lists))
    {
      *v = xcar (lists);
      MAP_CALLARG (vec) = xcons (Qnil, MAP_CALLARG (vec));
    }
}

static int
map_next (lisp *vec, int nargs)
{
  for (lisp *v = MAP_ARGS (vec), *ve = v + nargs; v < ve; v++)
    {
      assert (consp (*v));
      *v = xcdr (*v);
      if (!consp (*v))
        {
          multiple_value::clear ();
          return 0;
        }
    }
  return 1;
}

static lisp
mapc_apply (lisp fn, lisp *vec, int nargs)
{
  lisp c = MAP_CALLARG (vec);
  for (lisp *v = MAP_ARGS (vec), *ve = v + nargs; v < ve; v++, c = xcdr (c))
    {
      check_cons (*v);
      check_cons (c);
      xcar (c) = xcar (*v);
    }
  return Ffuncall (fn, MAP_CALLARG (vec));
}

static lisp
mapl_apply (lisp fn, lisp *vec, int nargs)
{
  lisp c = MAP_CALLARG (vec);
  for (lisp *v = MAP_ARGS (vec), *ve = v + nargs; v < ve; v++, c = xcdr (c))
    {
      check_cons (c);
      xcar (c) = *v;
    }
  return Ffuncall (fn, MAP_CALLARG (vec));
}

lisp
Fmapcar (lisp fn, lisp lists)
{
  int nargs = map_count (lists);
  if (!nargs)
    return Qnil;
  lisp *vec = (lisp *)alloca (sizeof (lisp) * MAP_VECSIZE (nargs));
  protect_gc gcpro (vec, MAP_VECSIZE (nargs));
  map_first (lists, vec);
  do
    MAP_RESULT (vec) = xcons (mapc_apply (fn, vec, nargs), MAP_RESULT (vec));
  while (map_next (vec, nargs));
  return Fnreverse (MAP_RESULT (vec));
}

lisp
Fmaplist (lisp fn, lisp lists)
{
  int nargs = map_count (lists);
  if (!nargs)
    return Qnil;
  lisp *vec = (lisp *)alloca (sizeof (lisp) * MAP_VECSIZE (nargs));
  protect_gc gcpro (vec, MAP_VECSIZE (nargs));
  map_first (lists, vec);
  do
    MAP_RESULT (vec) = xcons (mapl_apply (fn, vec, nargs), MAP_RESULT (vec));
  while (map_next (vec, nargs));
  return Fnreverse (MAP_RESULT (vec));
}

lisp
Fmapc (lisp fn, lisp lists)
{
  int nargs = map_count (lists);
  if (nargs)
    {
      lisp *vec = (lisp *)alloca (sizeof (lisp) * MAP_VECSIZE (nargs));
      protect_gc gcpro (vec, MAP_VECSIZE (nargs));
      map_first (lists, vec);
      do
        mapc_apply (fn, vec, nargs);
      while (map_next (vec, nargs));
    }
  return Fcar (lists);
}

lisp
Fmapl (lisp fn, lisp lists)
{
  int nargs = map_count (lists);
  if (nargs)
    {
      lisp *vec = (lisp *)alloca (sizeof (lisp) * MAP_VECSIZE (nargs));
      protect_gc gcpro (vec, MAP_VECSIZE (nargs));
      map_first (lists, vec);
      do
        mapl_apply (fn, vec, nargs);
      while (map_next (vec, nargs));
    }
  return Fcar (lists);
}

lisp
Fprogn (lisp arg, lex_env &lex)
{
  lisp val = Qnil;
  multiple_value::clear ();
  for (; consp (arg); arg = xcdr (arg))
    {
      val = eval (xcar (arg), lex);
      QUIT;
    }
  return val;
}

lisp
Flocally (lisp arg, lex_env &olex)
{
  lex_env nlex (olex);
  for (lisp body = arg; consp (body); body = xcdr (body))
    {
      lisp x = xcar (body);
      if (!consp (x) || xcar (x) != Qdeclare)
        break;
      for (x = xcdr (x); consp (x); x = xcdr (x))
        {
          lisp t = xcar (x);
          if (consp (t) && xcar (t) == Qspecial)
            for (t = xcdr (t); consp (t); t = xcdr (t))
              {
                lisp sym = xcar (t);
                if (symbolp (sym))
                  nlex.bind (sym, symbol_value (sym, selected_buffer ()));
                QUIT;
              }
          QUIT;
        }
      QUIT;
    }

  return declare_progn (arg, nlex, 0);
}

lisp
Flet (lisp arg, lex_env &olex)
{
  if (!consp (arg))
    FEinvalid_variable_list (arg);
  lex_env nlex (olex);
  nlex.let (xcar (arg), olex);
  return declare_progn (xcdr (arg), nlex, 0);
}

lisp
Flet_star (lisp arg, lex_env &olex)
{
  if (!consp (arg))
    FEinvalid_variable_list (arg);
  lex_env nlex (olex);
  nlex.let (xcar (arg), nlex);
  return declare_progn (xcdr (arg), nlex, 0);
}

lisp
Fif (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp testf = xcar (arg);
  if (!consp (arg = xcdr (arg)))
    FEtoo_few_arguments ();
  lisp thenf = xcar (arg);
  lisp elsef = 0;
  if (consp (arg = xcdr (arg)))
    {
      elsef = xcar (arg);
      if (consp (xcdr (arg)))
        FEtoo_many_arguments ();
    }
  lisp f = eval (testf, lex);
  multiple_value::clear ();
  if (f == Qnil)
    return elsef ? eval (elsef, lex) : Qnil;
  return eval (thenf, lex);
}

lisp
Fblock (lisp arg, lex_env &olex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp name = xcar (arg);
  lisp body = xcdr (arg);
  check_symbol (name);
  lisp frame = make_frame ();
  dynamic_extent dyn (frame);
  lex_env nlex (olex);
  nlex.bind_frame (Qblock, name, frame);

  lisp result;
  try
    {
      result = Fprogn (body, nlex);
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type == Qblock && nld->tag == name && nld->id == frame)
        return nld->value;
      throw;
    }
  return result;
}

lisp
Freturn_from (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp name = xcar (arg);
  check_symbol (name);
  arg = xcdr (arg);
  lisp value = consp (arg) ? eval (xcar (arg), lex) : Qnil;

  lisp x = lex.search_frame (Qblock, name);
  if (x == Qnil)
    FEno_target_for (Sreturn_from, name);
  assert (consp (x));
  assert (consp (xcdr (x)));
  lisp frame = xcdr (xcdr (x));
  if (!active_frame_p (frame))
    FEtarget_missing (Sreturn_from, name);

  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qblock;
  nld->value = value;
  nld->tag = name;
  nld->id = frame;
  throw nonlocal_jump ();

  return Qnil;
}

lisp
Ftagbody (lisp arg, lex_env &olex)
{
  lisp frame = make_frame ();
  protect_gc gcpro (frame);
  dynamic_extent dyn (frame);
  lex_env nlex (olex);
  for (lisp x = arg; consp (x); x = xcdr (x))
    {
      lisp f = xcar (x);
      if (integerp (f) || symbolp (f))
        nlex.bind_frame (Qtagbody, f, frame);
      QUIT;
    }

  lisp begin = arg;
restart:
  try
    {
      for (lisp x = begin; consp (x); x = xcdr (x))
        {
          lisp f = xcar (x);
          if (!integerp (f) && !symbolp (f))
            eval (f, nlex);
          QUIT;
        }
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type == Qtagbody && nld->id == frame)
        {
          for (lisp x = arg; consp (x); x = xcdr (x))
            if (Feql (xcar (x), nld->tag) != Qnil)
              {
                begin = xcdr (x);
                goto restart;
              }
          assert (0);
        }
      throw;
    }
  multiple_value::clear ();
  return Qnil;
}

lisp
Fgo (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();

  lisp name = xcar (arg);
  if (!integerp (name) && !symbolp (name))
    FEtype_error (name, xsymbol_value (Qor_symbol_integer));

  lisp x = lex.search_frame (Qtagbody, name);
  if (x == Qnil)
    FEno_target_for (Sgo, name);
  assert (consp (x));
  assert (consp (xcdr (x)));
  lisp frame = xcdr (xcdr (x));
  if (!active_frame_p (frame))
    FEtarget_missing (Sgo, name);

  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qtagbody;
  nld->value = Qnil;
  nld->tag = name;
  nld->id = frame;
  throw nonlocal_jump ();

  return Qnil;
}

lisp
Funwind_protect (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp x;
  try
    {
      x = eval (xcar (arg), lex);
    }
  catch (nonlocal_jump &)
    {
      save_multiple_value save_mvalue (multiple_value::value (0));
      save_nonlocal_jump save_nlocal;
      try
        {
          Fprogn (xcdr (arg), lex);
        }
      catch (nonlocal_jump &)
        {
        }
      throw;
    }
  save_multiple_value save_mvalue (x);
  Fprogn (xcdr (arg), lex);
  return x;
}

lisp
Fcatch (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp tag = eval (xcar (arg), lex);
  protect_gc gcpro (tag);
  lisp result;
  try
    {
      result = Fprogn (xcdr (arg), lex);
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type == Qcatch && nld->tag == tag)
        return nld->value;
      throw;
    }
  return result;
}

lisp
Fthrow (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp tag = eval (xcar (arg), lex);
  protect_gc gcpro (tag);
  arg = xcdr (arg);
  if (!consp (arg))
    FEtoo_few_arguments ();
  if (consp (xcdr (arg)))
    FEtoo_many_arguments ();
  lisp result = eval (xcar (arg), lex);

  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qcatch;
  nld->value = result;
  nld->tag = tag;
  nld->id = Qnil;
  throw nonlocal_jump ();

  return Qnil;
}

lisp
Feval_when (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  if (memq (Kexecute, xcar (arg))
      || memq (Seval, xcar (arg)))
    return Fprogn (xcdr (arg), lex);
  return Qnil;
}

lisp
Fsi_fset (lisp name, lisp body)
{
  check_symbol (name);
  lisp odef = xsymbol_function (name);
  if (functionp (odef) && real_special_form_p (odef))
    FEmodify_constant (name);
  if (functionp (body) && real_special_form_p (body))
    FEprogram_error (Eis_special_form, body);
  xsymbol_function (name) = body;
  return name;
}

lisp
Fsi_function_name (lisp closure)
{
  if (!closurep (closure))
    FEtype_error (closure, Qclosure);
  return xclosure_name (closure);
}

lisp
Fsi_set_function_name (lisp closure, lisp name)
{
  if (!closurep (closure))
    FEtype_error (closure, Qclosure);
  xclosure_name (closure) = name;
  return closure;
}

static lisp
flet (lisp arg, lex_env &olex, lex_env &nlex, int macrop, lisp name_prefix)
{
  if (!consp (arg) || !listp (xcar (arg)) || !listp (xcdr (arg)))
    FEtoo_few_arguments ();
  lisp ofns = nlex.lex_fns;
  for (lisp defs = xcar (arg); consp (defs); defs = xcdr (defs))
    {
      lisp def = xcar (defs);
      if (!consp (def) || !consp (xcdr (def)))
        FEtoo_few_arguments ();
      lisp name = xcar (def);
      check_symbol (name);
      lisp body = xcdr (def);
      if (!consp (body) || (xcar (body) != Qnil && !consp (xcar (body))))
        FEinvalid_function (body);
      body = funcall_3 (Vsi_flet_helper, name, body, boole (macrop));
      if (!consp (body) || xcar (body) != (macrop ? Qmacro : Qlambda))
        FEinvalid_function (body);
      lisp fn = body;
      if (!macrop)
        {
          fn = make_closure (body,
                             olex.lex_var, olex.lex_fns,
                             olex.lex_frame);
          xclosure_name (fn) = make_list (name_prefix, name, 0);
        }
      nlex.lex_fns = xcons (xcons (name, fn), nlex.lex_fns);
      QUIT;
    }
  if (&olex == &nlex)
    for (lisp p = nlex.lex_fns; p != ofns; p = xcdr (p))
      xclosure_fns (xcdr (xcar (p))) = nlex.lex_fns;
  return declare_progn (xcdr (arg), nlex, 0);
}

lisp
Fflet (lisp arg, lex_env &olex)
{
  lex_env nlex (olex);
  return flet (arg, olex, nlex, 0, Sflet);
}

lisp
Flabels (lisp arg, lex_env &olex)
{
  lex_env nlex (olex);
  return flet (arg, nlex, nlex, 0, Slabels);
}

lisp
Fmacrolet (lisp arg, lex_env &olex)
{
  lex_env nlex (olex);
  return flet (arg, olex, nlex, 1, Qnil);
}

lisp
Fvalues_list (lisp list)
{
  int i;
  for (i = 0; consp (list); i++, list = xcdr (list))
    {
      if (i == MULTIPLE_VALUES_LIMIT)
        FEtoo_many_arguments ();
      multiple_value::value (i) = xcar (list);
    }
  multiple_value::count () = i;
  if (!i)
    multiple_value::value (0) = Qnil;
  return multiple_value::value (0);
}

lisp
Fmultiple_value_call (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp fn = eval (xcar (arg), lex);
  protect_gc gcpro1 (fn);
  arg = xcdr (arg);
  lisp result = Qnil;
  protect_gc gcpro2 (result);
  for (; consp (arg); arg = xcdr (arg))
    {
      multiple_value::value (0) = eval (xcar (arg), lex);
      for (int i = 0; i < multiple_value::count (); i++)
        result = xcons (multiple_value::value (i), result);
    }
  result = Fnreverse (result);
  if (fn == Qlist)
    return result;
  return Ffuncall (fn, result);
}

lisp
Fmultiple_value_prog1 (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp x = eval (xcar (arg), lex);
  multiple_value::value (0) = x;
  multiple_value_data save;
  save.count = multiple_value::count ();
  bcopy (multiple_value::data ()->values, save.values, save.count);
  protect_gc gcpro (save.values, save.count);
  Fprogn (xcdr (arg), lex);
  bcopy (save.values, multiple_value::data ()->values, save.count);
  multiple_value::count () = save.count;
  return x;
}

lisp
Fmultiple_value_setq (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp vars = xcar (arg);
  arg = xcdr (arg);
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp x = eval (xcar (arg), lex);
  multiple_value::value (0) = x;
  for (int i = 0; i < multiple_value::count (); i++, vars = xcdr (vars))
    {
      if (!consp (vars))
        break;
      lisp var = xcar (vars);
      check_symbol (var);
      if (constantp (var))
        FEmodify_constant (var);
      if (!lex.set (var, multiple_value::value (i)))
        set_globally (var, multiple_value::value (i), selected_buffer ());
    }
  for (; consp (vars); vars = xcdr (vars))
    {
      lisp var = xcar (vars);
      check_symbol (var);
      if (constantp (var))
        FEmodify_constant (var);
      if (!lex.set (var, Qnil))
        set_globally (var, Qnil, selected_buffer ());
      QUIT;
    }
  multiple_value::clear ();
  return x;
}

lisp
Fmultiple_value_bind (lisp arg, lex_env &olex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp vars = xcar (arg);
  arg = xcdr (arg);
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp values_form = xcar (arg);
  arg = xcdr (arg);

  lisp x = eval (values_form, olex);
  lex_env nlex (olex);
  multiple_value::value (0) = x;
  for (int i = 0; i < multiple_value::count (); i++, vars = xcdr (vars))
    {
      if (!consp (vars))
        break;
      nlex.bind (xcar (vars), multiple_value::value (i));
    }
  for (; consp (vars); vars = xcdr (vars))
    {
      nlex.bind (xcar (vars), Qnil);
      QUIT;
    }
  return declare_progn (arg, nlex, 0);
}

lisp
Fsi_find_in_environment (lisp form, lisp env)
{
  lisp symbol;
  return boole (consp (form)
                && symbolp (symbol = xcar (form))
                && assq (symbol,
                         (environmentp (env)
                          ? xenvironment_fns (env)
                          : env)));
}

lisp
Fmacroexpand_1 (lisp form, lisp env)
{
  lisp symbol;
  if (consp (form) && symbolp (symbol = xcar (form)))
    {
      lisp body = xsymbol_function (symbol);
      if (env)
        {
          lisp x = assq (symbol, environmentp (env) ? xenvironment_fns (env) : env);
          if (x && consp (x))
            body = xcdr (x);
        }
      else
        env = Qnil;

      if (functionp (body) && expand_macro_function_p (body))
        {
          lex_env lex (env);
          form = call_special_form (body, xcdr (form), lex);
          multiple_value::count () = 2;
          multiple_value::value (1) = Qt;
          return form;
        }
      else if (consp (body) && xcar (body) == Qmacro)
        {
          body = xcdr (body);
          if (consp (body))
            {
              form = macroexpand (lex_env (env), body, form);
              multiple_value::count () = 2;
              multiple_value::value (1) = Qt;
              return form;
            }
        }
    }
  multiple_value::count () = 2;
  multiple_value::value (1) = Qnil;
  return form;
}

lisp
Fmacroexpand (lisp arg, lisp env)
{
  protect_gc gcpro (arg);
  int n = 0;
  while (1)
    {
      arg = Fmacroexpand_1 (arg, env);
      if (multiple_value::value (1) == Qnil) break;
      n++;
    }
  multiple_value::count () = 2;
  multiple_value::value (1) = n > 0 ? Qt : Qnil;
  return arg;
}

lisp
Fsave_excursion (lisp arg, lex_env &lex)
{
  save_excursion se;
  lisp val = Fprogn (arg, lex);
  se.cleanup ();
  return val;
}

lisp
Fsave_restriction (lisp arg, lex_env &lex)
{
  save_restriction sr;
  return Fprogn (arg, lex);
}

lisp
Fsave_window_excursion (lisp arg, lex_env &lex)
{
  WindowConfiguration *wc = new WindowConfiguration;
  lisp x = Fprogn (arg, lex);
  multiple_value::value (0) = x;
  multiple_value_data save;
  save.count = multiple_value::count ();
  bcopy (multiple_value::data ()->values, save.values, save.count);
  protect_gc gcpro (save.values, save.count);
  delete wc;
  bcopy (save.values, multiple_value::data ()->values, save.count);
  multiple_value::count () = save.count;
  return x;
}

lisp
Fmakunbound (lisp symbol)
{
  check_symbol (symbol);
  xsymbol_value (symbol) = Qunbound;
  return symbol;
}

lisp
Ffmakunbound (lisp symbol)
{
  check_symbol (symbol);
  xsymbol_function (symbol) = Qunbound;
  return symbol;
}

lisp
Fidentity (lisp object)
{
  return object;
}

void
set_globally (lisp var, lisp val, Buffer *bp)
{
  assert (symbolp (var));
  if (bp && !(xsymbol_flags (var) & (SFdynamic_bind | SFspecial | SFconstant)))
    {
      if (xsymbol_flags (var) & SFbuffer_local)
        {
          lisp a = assq (var, bp->lvar);
          if (a)
            {
              xcdr (a) = val;
              return;
            }
        }
      if (xsymbol_flags (var) & SFmake_buffer_local)
        {
          bp->lvar = Facons (var, val, bp->lvar);
          xsymbol_flags (var) |= SFbuffer_local;
          return;
        }
    }
  xsymbol_value (var) = val;
}

lisp
symbol_value (lisp x, const Buffer *bp)
{
  assert (symbolp (x));
  if (bp && (xsymbol_flags (x) & (SFbuffer_local | SFdynamic_bind
                                  | SFspecial | SFconstant)) == SFbuffer_local)
    {
      lisp a = assq (x, bp->lvar);
      if (a)
        return xcdr (a);
    }
  return xsymbol_value (x);
}

int
symbol_value_as_integer (lisp x, const Buffer *bp)
{
  long n;
  return safe_fixnum_value (symbol_value (x, bp), &n) ? n : 0;
}

lisp
Fbuffer_local_value (lisp buffer, lisp symbol)
{
  check_symbol (symbol);
  lisp value = symbol_value (symbol, Buffer::coerce_to_buffer (buffer));
  if (value == Qunbound)
    FEunbound_variable (symbol);
  return value;
}

lisp
Fmake_variable_buffer_local (lisp symbol)
{
  check_symbol (symbol);
  if (constantp (symbol))
    FEmodify_constant (symbol);
  xsymbol_flags (symbol) |= SFmake_buffer_local;
  return Qt;
}

void
Buffer::make_local_variable (lisp symbol)
{
  if (!assq (symbol, lvar))
    {
      lisp val = symbol_value (symbol, this);
      lvar = Facons (symbol, val == Qunbound ? Qnil : val, lvar);
    }
  xsymbol_flags (symbol) |= SFbuffer_local;
}

lisp
Fmake_local_variable (lisp symbol)
{
  check_symbol (symbol);
  if (constantp (symbol))
    FEmodify_constant (symbol);
  selected_buffer ()->make_local_variable (symbol);
  return Qt;
}

void
Buffer::set_local_variable (lisp var, lisp val)
{
  make_local_variable (var);
  set_globally (var, val, this);
}

lisp
Fkill_all_local_variables ()
{
  Buffer *bp = selected_buffer ();
  lisp lv = bp->lvar;
  bp->lvar = Qnil;
  for (lisp p = xsymbol_value (Vprotected_local_variables);
       consp (p); p = xcdr (p))
    {
      lisp x = assq (xcar (p), lv);
      if (x)
        bp->set_local_variable (xcar (p), xcdr (x));
    }
  return Qt;
}

lisp
Fkill_local_variable (lisp var)
{
  Buffer *bp = selected_buffer ();
  return boole (delassq (var, &bp->lvar));
}

lisp
Flocal_variable_p (lisp var, lisp buffer)
{
  check_symbol (var);
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  return boole ((xsymbol_flags (var) & (SFbuffer_local | SFdynamic_bind
                                        | SFspecial | SFconstant)) == SFbuffer_local
                && assq (var, bp->lvar));
}

lisp
Finteractive (lisp, lex_env &)
{
  return Qnil;
}

static lisp
process_interactive_string (lisp fmt, lisp args)
{
  check_string (fmt);

  const Char *p = xstring_contents (fmt);
  const Char *pe = p + xstring_length (fmt);

  if (p == pe)
    return Qnil;

  lisp iargs = Qnil;
  lisp prefix_val = xsymbol_value (Vprefix_value);
  lisp prefix_args = xsymbol_value (Vprefix_args);

  int nargs = 0;
  int emacs = find_keyword_bool (Kemacs, args);
  lisp intr_alist = (emacs
                     ? Vemacs_interactive_specifier_alist
                     : Vinteractive_specifier_alist);

  protect_gc gcpro1 (iargs);
  protect_gc gcpro2 (fmt);
  protect_gc gcpro3 (prefix_val);
  protect_gc gcpro4 (prefix_args);

  if (*p == '*')
    {
      selected_buffer ()->check_read_only ();
      p++;
    }

  while (p < pe)
    {
      xsymbol_value (Vnext_prefix_args) = Qnil;
      xsymbol_value (Vnext_prefix_value) = Qnil;

      int opt_arg = -1;
      Char c = *p++;
      if (digit_char_p (c))
        {
          opt_arg = c - '0';
          if (p == pe)
            break;
          c = *p++;
        }

      const Char *p0;
      for (p0 = p; p < pe && *p != '\n'; p++)
        ;

      lisp al;
      for (al = xsymbol_value (intr_alist);
           consp (al); al = xcdr (al))
        {
          lisp x = xcar (al);
          if (consp (x))
            {
              lisp ch = xcar (x);
              if (charp (ch) && xchar_code (ch) == c)
                {
                  xsymbol_value (Vprefix_value) = prefix_val;
                  xsymbol_value (Vprefix_args) = prefix_args;
                  iargs = Fnreconc (Fnreverse (funcall_4 (xcdr (x),
                                                          make_string (p0, p - p0),
                                                          load_default (args, nargs),
                                                          load_history (args, nargs),
                                                          load_title (args, nargs))),
                                    iargs);
                  break;
                }
            }
        }

      if (!consp (al))
        {
          lisp v1 = 0, v2 = 0;

          switch (c)
            {
            case 'd':
              /* point */
              v1 = Fpoint ();
              break;

            case 'm':
              /* mark */
              v1 = Fmark (Qnil);
              break;

            case 'r':
              /* region */
              v1 = Fpoint ();
              v2 = Fmark (Qnil);
              if (emacs && fixnum_value (v1) < fixnum_value (v2))
                swap (v1, v2);
              break;

            case 'M':
              /* Selection mark */
              v1 = Fselection_mark ();
              break;

            case 'R':
              /* Selection region */
              v1 = Fselection_point ();
              v2 = Fselection_mark ();
              break;

            case 'p':
              /* prefix argument */
              if (prefix_val == Qnil)
                {
                  if (emacs)
                    v1 = make_fixnum (1);
                  break;
                }
              v1 = prefix_val;
              break;

            case 'P':
              /* Raw prefix argument */
              v1 = prefix_args;
              break;

            case 's':
              v1 = read_minibuffer (p0, p - p0, load_default (args, nargs), Qnil, Qnil,
                                    load_history (args, nargs), 0, 0, 0, Qnil, opt_arg);
              break;

            case 'a':
              v1 = complete_read (p0, p - p0, load_default (args, nargs),
                                  Kfunction_name, Qnil,
                                  load_history (args, nargs, Ksymbol_name),
                                  1, opt_arg);
              break;

            case 'C':
              v1 = complete_read (p0, p - p0, load_default (args, nargs),
                                  Kcommand_name, Qnil,
                                  load_history (args, nargs, Ksymbol_name),
                                  1, opt_arg);
              break;

            case 'S':
              v1 = complete_read (p0, p - p0, load_default (args, nargs),
                                  Ksymbol_name, Qnil,
                                  load_history (args, nargs, Ksymbol_name),
                                  1, opt_arg);
              break;

            case 'v':
              v1 = complete_read (p0, p - p0, load_default (args, nargs),
                                  Kvariable_name, Qnil,
                                  load_history (args, nargs, Ksymbol_name),
                                  1, opt_arg);
              break;

            case 'F':
              v1 = read_filename (p0, p - p0, Kfile_name,
                                  load_title (args, nargs),
                                  load_default (args, nargs),
                                  load_history (args, nargs));
              break;

            case 'l':
              v1 = read_filename (p0, p - p0, Kfile_name_list,
                                  load_title (args, nargs),
                                  load_default (args, nargs),
                                  load_history (args, nargs));
              break;

            case 'f':
              v1 = read_filename (p0, p - p0, Kexist_file_name,
                                  load_title (args, nargs),
                                  load_default (args, nargs),
                                  load_history (args, nargs));
              break;

            case 'D':
              v1 = read_filename (p0, p - p0, Kdirectory_name,
                                  load_title (args, nargs),
                                  load_default (args, nargs),
                                  load_history (args, nargs));
              break;

            case 'B':
            case 'b':
              {
                lisp def = load_default (args, nargs);
                if (def == Qnil)
                  def = c == 'B' ? Fother_buffer (0) : Fselected_buffer ();
                if (bufferp (def))
                  def = Fbuffer_name (def);
                v1 = complete_read (p0, p - p0, def,
                                    c == 'B' ? Kbuffer_name : Kexist_buffer_name,
                                    Qnil,
                                    load_history (args, nargs, Kbuffer_name),
                                    c != 'B', opt_arg);
                break;
              }

            case 'N':
              if (prefix_val != Qnil)
                {
                  v1 = prefix_val;
                  break;
                }
              /* fall thru... */
            case 'n':
              v1 = minibuffer_read_integer (p0, p - p0);
              break;

            case 'x':
            case 'X':
              v1 = funcall_1 (Vread_from_string,
                              read_minibuffer (p0, p - p0, Qnil, Klisp_sexp,
                                               Qnil, Klisp_sexp,
                                               0, 0, 0, Qnil, opt_arg));
              if (c == 'X')
                v1 = Feval (v1);
              break;

            case 'c':
              if (p != p0)
                Fsi_minibuffer_message (make_string (p0, p - p0), Qt);
              v1 = Fread_char (xsymbol_value (Qkeyboard), Qnil, Qnil, Qnil);
              if (p != p0)
                Fsi_minibuffer_message (Qnil, Qnil);
              break;

            case 'e':
              v1 = complete_read (p0, p - p0, load_default (args, nargs),
                                  Kcommand_line, Qnil, load_history (args, nargs),
                                  0, opt_arg);
              break;

            case 'z':
            case 'Z':
              if (!opt_arg && prefix_val == Qnil)
                {
                  v1 = Qnil;
                  break;
                }
              v1 = complete_read (p0, p - p0, Qnil,
                                  c == 'z' ? Kchar_encoding : Kexact_char_encoding,
                                  Qnil,
                                  load_history (args, nargs, Kchar_encoding),
                                  1, -1);
              break;

            default:
              FEprogram_error (Ebad_interactive_specifier, make_char (c));
            }

          if (v2)
            iargs = xcons (v2, iargs);
          if (v1)
            iargs = xcons (v1, iargs);
        }

      nargs++;
      if (p < pe && *p == '\n')
        p++;
    }

  return Fnreverse (iargs);
}

static lisp
find_interactive (lisp p)
{
  if (!consp (p))
    return 0;
  int doc = 0;
  for (p = xcdr (p); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (stringp (x))
        {
          if (doc)
            return 0;
          doc = 1;
          continue;
        }
      if (consp (x))
        {
          lisp t = xcar (x);
          if (t == Qdeclare)
            continue;
          if (t == Qinteractive)
            return xcdr (x);
        }
      break;
    }
  return 0;
}

lisp
Fcommand_execute (lisp command, lisp hook)
{
  enable_quit eq;

  lisp fn = command;
  if (symbolp (fn))
    {
      fn = xsymbol_function (fn);
      if (fn == Qunbound)
        FEundefined_function (command);
    }

  if (stringp (fn))
    return execute_string (fn);

  protect_gc gcpro1 (fn);

  lisp args;
  if (functionp (fn))
    {
      lisp intr = interactive_string (fn);
      if (!intr)
        FEprogram_error (Enot_a_command, command);
      if (intr == Qnil)
        args = Qnil;
      else
        args = process_interactive_string (intr, Qnil);
    }
  else
    {
      lisp f = closurep (fn) ? xclosure_body (fn) : fn;
      if (consp (f) && xcar (f) == Qlambda)
        {
          lisp intr = find_interactive (xcdr (f));
          if (!intr)
            FEprogram_error (Enot_a_command, command);
          if (!consp (intr))
            args = Qnil;
          else if (stringp (xcar (intr)))
            {
              lex_env lex;
              lisp eargs = eval_args (xcdr (intr), lex);
              protect_gc gcpro (eargs);
              args = process_interactive_string (xcar (intr), eargs);
            }
          else
            {
              args = Feval (xcar (intr));
              if (!consp (args) && args != Qnil)
                FEtype_error (args, Qlist);
            }
        }
      else
        FEprogram_error (Enot_a_command, command);
    }

  protect_gc gcpro2 (args);

  if (hook && hook != Qnil)
    funcall_2 (hook, command, args);

  return Ffuncall (fn, args);
}

lisp
Fcommandp (lisp object)
{
  if (symbolp (object))
    {
      object = xsymbol_function (object);
      if (object == Qunbound)
        return Qnil;
    }

  if (stringp (object))
    return Qt;

  if (functionp (object))
    return boole (interactive_string (object));

  if (closurep (object))
    object = xclosure_body (object);
  return boole (consp (object) && xcar (object) == Qlambda
                && find_interactive (xcdr (object)));
}

lisp
Finteractive_p ()
{
  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    if (p->type == stack_trace::apply)
      {
        lisp fn = p->fn;
        if (symbolp (fn))
          {
            fn = xsymbol_function (fn);
            if (!functionp (fn))
              return Qnil;
            if (xfunction_fn (fn) == lfunction_proc (Fcommand_execute))
              return Qt;
            if (xfunction_fn (fn) != lfunction_proc (Finteractive_p))
              return Qnil;
          }
      }
  return Qnil;
}

lisp
call_hook_nargs (lisp hook, lisp args, lisp expect)
{
  suppress_gc sgc;

  if (!consp (hook) || xcar (hook) == Qlambda)
    return Ffuncall (hook, args);

  lisp r;
  do
    {
      r = Ffuncall (xcar (hook), args);
      if (expect && (expect == Qnil ? r != Qnil : r == Qnil))
        return r;
      hook = xcdr (hook);
    }
  while (consp (hook));
  return r;
}

int
Buffer::check_hook (lisp symbol, lisp &hook) const
{
  check_symbol (symbol);
  hook = symbol_value (symbol, this);
  return hook != Qunbound && hook != Qnil;
}

lisp
Buffer::run_hook (lisp symbol) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  return call_hook_nargs (hook, Qnil, 0);
}

lisp
Buffer::run_hook (lisp symbol, lisp arg) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  arg = xcons (arg, Qnil);
  protect_gc gcpro (arg);
  return call_hook_nargs (hook, arg, 0);
}

lisp
Buffer::run_hook (lisp symbol, lisp arg1, lisp arg2) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  lisp args = xcons (arg1, xcons (arg2, Qnil));
  protect_gc gcpro (args);
  return call_hook_nargs (hook, args, 0);
}

lisp
Buffer::run_hook (lisp symbol, lisp arg1, lisp arg2, lisp arg3) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  lisp args = xcons (arg1, xcons (arg2, xcons (arg3, Qnil)));
  protect_gc gcpro (args);
  return call_hook_nargs (hook, args, 0);
}

lisp
Buffer::run_hook (lisp symbol, lisp arg1, lisp arg2,
                  lisp arg3, lisp arg4) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  lisp args = xcons (arg1, xcons (arg2, xcons (arg3, xcons (arg4, Qnil))));
  protect_gc gcpro (args);
  return call_hook_nargs (hook, args, 0);
}

lisp
Buffer::run_hook (lisp symbol, lisp arg1, lisp arg2,
                  lisp arg3, lisp arg4, lisp arg5) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  lisp args = xcons (arg1, xcons (arg2, xcons (arg3, xcons (arg4, xcons (arg5, Qnil)))));
  protect_gc gcpro (args);
  return call_hook_nargs (hook, args, 0);
}

lisp
Buffer::run_hook_while_success (lisp symbol) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qt;
  return call_hook_nargs (hook, Qnil, Qt);
}

lisp
Buffer::run_hook_while_success (lisp symbol, lisp arg) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qt;
  arg = xcons (arg, Qnil);
  protect_gc gcpro (arg);
  return call_hook_nargs (hook, arg, Qt);
}

lisp
Buffer::run_hook_until_success (lisp symbol) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  return call_hook_nargs (hook, Qnil, Qnil);
}

lisp
Buffer::run_hook_until_success (lisp symbol, lisp arg) const
{
  lisp hook;
  if (!check_hook (symbol, hook))
    return Qnil;
  arg = xcons (arg, Qnil);
  protect_gc gcpro (arg);
  return call_hook_nargs (hook, arg, Qnil);
}

lisp
Buffer::safe_run_hook (lisp symbol, int print) const
{
  try
    {
      return run_hook (symbol);
    }
  catch (nonlocal_jump &)
    {
      if (print)
        print_condition (nonlocal_jump::data ());
    }
  return Qnil;
}

lisp
Frun_hooks (lisp hooks)
{
  lisp r = Qnil;
  for (; consp (hooks); hooks = xcdr (hooks))
    r = selected_buffer ()->run_hook (xcar (hooks));
  return r;
}

lisp
Frun_hook_with_args (lisp symbol, lisp args)
{
  lisp hook;
  if (!selected_buffer ()->check_hook (symbol, hook))
    return Qnil;
  return call_hook_nargs (hook, args, 0);
}

lisp
Frun_hook_with_args_while_success (lisp symbol, lisp args)
{
  lisp hook;
  if (!selected_buffer ()->check_hook (symbol, hook))
    return Qt;
  return call_hook_nargs (hook, args, Qt);
}

lisp
Frun_hook_with_args_until_success (lisp symbol, lisp args)
{
  lisp hook;
  if (!selected_buffer ()->check_hook (symbol, hook))
    return Qnil;
  return call_hook_nargs (hook, args, Qnil);
}
