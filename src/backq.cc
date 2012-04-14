#include "stdafx.h"
#include "ed.h"

static lisp bq_process (lisp x);

static lisp
bq_mapcar_cadr (lisp list)
{
  if (list == Qnil)
    return Qnil;
  check_cons (list);
  lisp result = xcons (Fcadar (list), Qnil);
  lisp p = result;
  while (1)
    {
      list = xcdr (list);
      if (!consp (list))
        break;
      xcdr (p) = xcons (Fcadar (list), Qnil);
      p = xcdr (p);
    }
  return result;
}

static inline int
bq_splicing_frob (lisp x)
{
  return (consp (x)
          && (xcar (x) == Qcomma_atsign
              || xcar (x) == Qcomma_dot));
}

static inline int
bq_frob (lisp x)
{
  return (consp (x)
          && (xcar (x) == Qcomma
              || xcar (x) == Qcomma_atsign
              || xcar (x) == Qcomma_dot));
}

static lisp
bq_maptree (lisp (*fn)(lisp), lisp x)
{
  if (!consp (x))
    return fn (x);
  lisp a = fn (xcar (x));
  lisp d = bq_maptree (fn, xcdr (x));
  return ((Feql (a, xcar (x)) != Qnil && Feql (d, xcdr (x)) != Qnil)
          ? x
          : xcons (a, d));
}

static inline int
bq_null_or_quoted (lisp x)
{
  return x == Qnil || (consp (x) && xcar (x) == Qbq_quote);
}

static int
bq_every (int (*fn)(lisp), lisp list)
{
  for (; consp (list); list = xcdr (list))
    if (!fn (xcar (list)))
      return 0;
  return 1;
}

static int
bq_notany (int (*fn)(lisp), lisp list)
{
  for (; consp (list); list = xcdr (list))
    if (fn (xcar (list)))
      return 0;
  return 1;
}

static inline int
bq_quote_nil (lisp x)
{
  return (consp (x) && xcar (x) == Qbq_quote
          && consp (x = xcdr (x))
          && xcar (x) == Qnil
          && xcdr (x) == Qnil);
}

static lisp
bq_remove_tokens (lisp x)
{
  if (x == Qbq_list)
    return Qlist;
  if (x == Qbq_append)
    return Sappend;
  if (x == Qbq_nconc)
    return Snconc;
  if (x == Qbq_list_star)
    return Slist_star;
  if (x == Qbq_quote)
    return Qquote;
  if (!consp (x))
    return x;
  if (xcar (x) == Qbq_clobberable)
    return bq_remove_tokens (Fcadr (x));
  if (xcar (x) == Qbq_list_star
      && consp (Fcddr (x))
      && Fcdddr (x) == Qnil)
    return xcons (Qcons, bq_maptree (bq_remove_tokens, Fcdr (x)));
  return bq_maptree (bq_remove_tokens, x);
}

static lisp
bq_attach_conses (lisp items, lisp result)
{
  if (bq_every (bq_null_or_quoted, items)
      && bq_null_or_quoted (result))
    return list (Qbq_quote,
                 append (bq_mapcar_cadr (items), Fcadr (result)));
  if (result == Qnil || bq_quote_nil (result))
    return xcons (Qbq_list, items);
  if (consp (result)
      && (xcar (result) == Qbq_list
          || xcar (result) == Qbq_list_star))
    return xcons (xcar (result), append (items, xcdr (result)));
  return xcons (Qbq_list_star, append (items, list (result)));
}

static lisp
bq_attach_append (lisp op, lisp item, lisp result)
{
  if (bq_null_or_quoted (item) && bq_null_or_quoted (result))
    return list (Qbq_quote, append (Fcadr (item), Fcadr (result)));
  if (result == Qnil || bq_quote_nil (result))
    return bq_splicing_frob (item) ? list (op, item) : item;
  if (consp (result) && xcar (result) == op)
    return Flist_star (list (xcar (result), item, xcdr (result)));
  return list (op, item, result);
}

static lisp
bq_simplify_args (lisp x)
{
  lisp result = Qnil;
  for (lisp args = Freverse (Fcdr (x)); args != Qnil; args = Fcdr (args))
    {
      if (!consp (Fcar (args)))
        result = bq_attach_append (Qbq_append, Fcar (args), result);
      else if (Fcaar (args) == Qbq_list
               && bq_notany (bq_splicing_frob, Fcdar (args)))
        result = bq_attach_conses (Fcdar (args), result);
      else if (Fcaar (args) == Qbq_list_star
               && bq_notany (bq_splicing_frob, Fcdar (args)))
        result = bq_attach_conses (Freverse (Fcdr (Freverse (Fcdar (args)))),
                                   bq_attach_append (Qbq_append,
                                                     Fcar (Flast (Fcar (args), 0)),
                                                     result));
      else if (Fcaar (args) == Qbq_quote
               && consp (Fcadar (args))
               && !bq_frob (Fcadar (args))
               && Fcddar (args) == Qnil)
        result = bq_attach_conses (list (list (Qbq_quote,
                                               Fcaadar (args))),
                                   result);
      else if (Fcaar (args) == Qbq_clobberable)
        result = bq_attach_append (Qbq_nconc, Fcadar (args), result);
      else
        result = bq_attach_append (Qbq_append, Fcar (args), result);
    }
  return result;
}

static lisp
bq_simplify (lisp x)
{
  if (!consp (x))
    return x;
  if (xcar (x) != Qbq_quote)
    x = bq_maptree (bq_simplify, x);
  if (Fcar (x) != Qbq_append)
    return x;
  return bq_simplify_args (x);
}

static lisp
bq_bracket (lisp x)
{
  if (!consp (x))
    return list (Qbq_list, bq_process (x));
  if (xcar (x) == Qcomma)
    return list (Qbq_list, Fcar (xcdr (x)));
  if (xcar (x) == Qcomma_atsign)
    return Fcar (xcdr (x));
  if (xcar (x) == Qcomma_dot)
    return list (Qbq_clobberable, Fcar (xcdr (x)));
  return list (Qbq_list, bq_process (x));
}

static lisp
bq_process (lisp x)
{
  if (!consp (x))
    return list (Qbq_quote, x);
  if (xcar (x) == Qbackquote)
    return bq_process (Fsi_bq_completely_process (Fcar (xcdr (x))));
  if (xcar (x) == Qcomma)
    return Fcar (xcdr (x));
  if (xcar (x) == Qcomma_atsign)
    FEprogram_error (Ecomma_atsign_after_backquote);
  if (xcar (x) == Qcomma_dot)
    FEprogram_error (Ecomma_dot_after_backquote);
  lisp p, q;
  for (p = x, q = Qnil; consp (p); p = xcdr (p))
    {
      if (xcar (p) == Qcomma)
        {
          if (Fcdr (xcdr (p)) != Qnil)
            FEprogram_error (Emalformed_comma);
          return xcons (Qbq_append, Fnreconc (q, list (Fcar (xcdr (p)))));
        }
      if (xcar (p) == Qcomma_atsign)
        FEprogram_error (Edotted_comma_atsign);
      if (xcar (p) == Qcomma_dot)
        FEprogram_error (Edotted_comma_dot);
      q = xcons (bq_bracket (xcar (p)), q);
    }
  if (p == Qnil)
    return xcons (Qbq_append, Fnreverse (q));
  return xcons (Qbq_append,
                Fnreconc (q, list (list (Qbq_quote, p))));
}

lisp
Fsi_bq_completely_process (lisp x)
{
  lisp raw_result = bq_process (x);
  return bq_remove_tokens (xsymbol_value (Vsi_bq_simplify) == Qnil
                           ? raw_result
                           : bq_simplify (raw_result));
}
