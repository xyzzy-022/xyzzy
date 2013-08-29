#include "stdafx.h"
#include "ed.h"
#include "lex.h"

int
lex_env::set (lisp var, lisp val)
{
  lisp x = assq (var, lex_var);
  if (x)
    {
      if (xcdr (x) == Qunbound)
        xsymbol_value (var) = val;
      else
        xcdr (x) = val;
      return 1;
    }
  return 0;
}

void
lex_env::let (lisp varlist, lex_env &eval_lex)
{
  if (varlist == Qnil)
    return;
  if (!consp (varlist))
    FEinvalid_variable_list (varlist);

  for (; consp (varlist); varlist = xcdr (varlist))
    {
      lisp var = xcar (varlist);
      if (consp (var))
        {
          lisp tem = xcdr (var);
          if (consp (tem))
            bind (xcar (var), eval (xcar (tem), eval_lex));
          else
            bind (xcar (var), Qnil);
        }
      else
        bind (var, Qnil);
    }
  multiple_value::clear ();
}

lisp
lex_env::do_environment (lisp lambda_list)
{
  lisp org_lambda_list = lambda_list;
  assert (consp (lambda_list));
  lambda_list = xcdr (lambda_list);
  if (!consp (lambda_list))
    FEinvalid_lambda_list (org_lambda_list);
  lisp var = xcar (lambda_list);
  if (!symbolp (var))
    FEinvalid_lambda_list (org_lambda_list);
  bind (var, make_environment (*this));
  return lambda_list;
}

void
lex_env::lambda_bind (lisp lambda_list, lisp params,
                      lisp whole, int macro_level)
{
  lisp org_lambda_list = lambda_list;
  if (macro_level == 1 && consp (lambda_list) && xcar (lambda_list) == Qenvironment)
    {
      lambda_list = xcdr (do_environment (lambda_list));
      macro_level++;
    }

  if (macro_level && consp (lambda_list) && xcar (lambda_list) == Qwhole)
    {
      lambda_list = xcdr (lambda_list);
      if (!consp (lambda_list))
        FEinvalid_lambda_list (org_lambda_list);
      lisp var = xcar (lambda_list);
      if (!symbolp (var))
        FEinvalid_lambda_list (org_lambda_list);
      bind (var, whole);
      lambda_list = xcdr (lambda_list);
    }

  if (lambda_list == Qnil)
    {
      if (consp (params))
        FEtoo_many_arguments ();
      return;
    }

  if (!consp (lambda_list))
    FEinvalid_lambda_list (org_lambda_list);

  while (1)
    {
      lisp var = xcar (lambda_list);
      if (macro_level == 1 && var == Qenvironment)
        {
          lambda_list = xcdr (do_environment (lambda_list));
          macro_level++;
        }
      else
        {
          if (symbolp (var))
            {
              if (lambda_key_p (var))
                break;
              if (!consp (params))
                FEtoo_few_arguments ();
              bind (var, xcar (params));
            }
          else if (macro_level && consp (var))
            {
              if (!consp (params))
                FEtoo_few_arguments ();
              lambda_bind (var, xcar (params), xcar (params), macro_level + 1);
            }
          else
            FEinvalid_lambda_list (org_lambda_list);

          lambda_list = xcdr (lambda_list);
          params = xcdr (params);
        }

      if (!consp (lambda_list))
        {
          if (macro_level && lambda_list != Qnil)
            bind (lambda_list, params);
          else if (consp (params))
            FEtoo_many_arguments ();
          return;
        }
    }

  if (xcar (lambda_list) == Qoptional)
    {
      while (1)
        {
          lambda_list = xcdr (lambda_list);
          if (!consp (lambda_list))
            {
              if (macro_level && lambda_list != Qnil)
                bind (lambda_list, params);
              else if (consp (params))
                FEtoo_many_arguments ();
              return;
            }

          lisp varlist = xcar (lambda_list);
          lisp var;
          lisp initform = Qnil;
          lisp svar = 0;

          if (symbolp (varlist))
            {
              if (lambda_key_p (varlist))
                {
                  if (macro_level == 1 && varlist == Qenvironment)
                    {
                      lambda_list = do_environment (lambda_list);
                      macro_level++;
                      continue;
                    }
                  break;
                }
              var = varlist;
            }
          else if (consp (varlist))
            {
              var = xcar (varlist);
              varlist = xcdr (varlist);
              if (consp (varlist))
                {
                  initform = xcar (varlist);
                  varlist = xcdr (varlist);
                  if (consp (varlist))
                    svar = xcar (varlist);
                }
            }
          else
            FEinvalid_lambda_list (org_lambda_list);

          if (consp (params))
            {
              if (macro_level && consp (var))
                lambda_bind (var, xcar (params), xcar (params), macro_level + 1);
              else
                bind (var, xcar (params));
              params = xcdr (params);
              if (svar)
                bind (svar, Qt);
            }
          else
            {
              if (macro_level && consp (var))
                lambda_bind (var, initform, initform, macro_level + 1);
              else
                bind (var, eval (initform, *this));
              multiple_value::clear ();
              if (svar)
                bind (svar, Qnil);
            }
        }
    }

  if (xcar (lambda_list) == Qrest || (macro_level && xcar (lambda_list) == Qbody))
    {
      lisp x = xcdr (lambda_list);
      if (!consp (x))
        FEinvalid_lambda_list (org_lambda_list);
      lambda_list = xcdr (x);
      x = xcar (x);
      if (macro_level && consp (x))
        lambda_bind (x, params, params, macro_level + 1);
      else
        bind (x, params);
    }

  if (macro_level == 1 && consp (lambda_list) && xcar (lambda_list) == Qenvironment)
    {
      lambda_list = xcdr (do_environment (lambda_list));
      macro_level++;
    }

  if (consp (lambda_list) && xcar (lambda_list) == Qkey)
    {
      for (lisp p = params; consp (p); p = xcdr (p))
        {
          check_symbol (xcar (p));
          p = xcdr (p);
          if (!consp (p))
            FEtoo_few_arguments ();
        }

      while (1)
        {
          lambda_list = xcdr (lambda_list);

          lisp keyword, var;
          lisp initform = Qnil, svar = 0;

          if (!consp (lambda_list))
            return;

          lisp varlist = xcar (lambda_list);
          if (symbolp (varlist))
            {
              if (lambda_key_p (varlist))
                {
                  if (macro_level == 1 && varlist == Qenvironment)
                    {
                      lambda_list = do_environment (lambda_list);
                      macro_level++;
                      continue;
                    }
                  break;
                }
              var = varlist;
              keyword = Fintern (xsymbol_name (var),
                                 xsymbol_value (Vkeyword_package));
            }
          else if (consp (varlist))
            {
              lisp x = xcar (varlist);
              if (consp (x))
                {
                  keyword = xcar (x);
                  check_symbol (keyword);
                  x = xcdr (x);
                  if (!consp (x))
                    FEtoo_few_arguments ();
                  var = xcar (x);
                }
              else
                {
                  check_symbol (x);
                  var = x;
                  keyword = Fintern (xsymbol_name (var),
                                     xsymbol_value (Vkeyword_package));
                }

              varlist = xcdr (varlist);
              if (consp (varlist))
                {
                  initform = xcar (varlist);
                  varlist = xcdr (varlist);
                  if (consp (varlist))
                    svar = xcar (varlist);
                }
            }
          else
            FEinvalid_lambda_list (org_lambda_list);

          lisp value = 0;
          for (lisp p = params; consp (p); p = xcdr (p))
            {
              lisp key = xcar (p);
              p = xcdr (p);
              if (key == keyword)
                {
                  value = xcar (p);
                  break;
                }
            }

          if (value)
            {
              bind (var, value);
              if (svar)
                bind (svar, Qt);
            }
          else
            {
              bind (var, eval (initform, *this));
              multiple_value::clear ();
              if (svar)
                bind (svar, Qnil);
            }
        }
    }

  if (consp (lambda_list))
    {
      if (xcar (lambda_list) == Qaux)
        let (xcdr (lambda_list), *this);
      else
        FEinvalid_lambda_list (org_lambda_list);
    }
}

lisp
lex_env::search_frame (lisp type, lisp tag)
{
  for (lisp p = lex_frame; consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      assert (consp (x));
      assert (consp (xcdr (x)));
      if (xcar (x) == type && xcar (xcdr (x)) == tag)
        return x;
    }
  return Qnil;
}

lex_env::lex_env (lisp o)
{
  if (environmentp (o))
    {
      lex_var = xenvironment_var (o);
      lex_fns = xenvironment_fns (o);
      lex_frame = xenvironment_frame (o);
    }
  else
    {
      lex_var = Qnil;
      lex_fns = consp (o) ? o : Qnil;
      lex_frame = Qnil;
    }
  lex_ltail = lex_var;
  chain ();
}


static lisp
check_closure (lisp x)
{
  lisp closure;
  if (!symbolp (x))
    closure = x;
  else
    {
      closure = xsymbol_function (x);
      if (closure == Qunbound)
        FEundefined_function (x);
    }
  if (!closurep (closure))
    FEtype_error (x, Qclosure);
  return closure;
}

lisp
Fsi_closure_variable (lisp closure)
{
  lisp vars = Fcopy_list (xclosure_vars (check_closure (closure)));
  for (lisp var = vars; consp (var); var = xcdr (var))
    {
      lisp v = xcar (var);
      if (!consp (v))
        {
          xcar (var) = Qnil;
          continue;
        }
      if (xcdr (v) == Qunbound)
        {
          if (xsymbol_flags (xcar (v)) & (SFdynamic_bind | SFspecial))
            xcar (var) = xcons (xcar (v), xsymbol_value (xcar (v)));
          else
            xcar (var) = xcar (v);
          continue;
        }
      xcar (var) = xcons (xcar (v), xcdr (v));
    }
  return vars;
}

lisp
Fsi_closure_function (lisp closure)
{
  return xclosure_fns (check_closure (closure));
}

lisp
Fsi_closure_frame (lisp closure)
{
  lisp envs = Fcopy_list (xclosure_frame (check_closure (closure)));
  for (lisp env = envs; consp (env); env = xcdr (env))
    {
      lisp e = xcar (env);
      assert (consp (e));
      assert (consp (xcdr (e)));
      xcar (env) = xcons (xcar (e),
                          xcons (xcar (xcdr (e)),
                                 xcons (boole (active_frame_p (xcdr (xcdr (e)))),
                                        Qnil)));
    }
  return envs;
}

lisp
Fsi_closure_body (lisp closure)
{
  return xclosure_body (check_closure (closure));
}

lisp
Fsi_closurep (lisp closure)
{
  return boole (closurep (closure));
}
