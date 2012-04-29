#include "stdafx.h"
#include "ed.h"

lsymbol *
make_symbol (lisp name, u_int flags)
{
  lsymbol *p = ldata <lsymbol, Tsymbol>::lalloc ();
  p->flags = flags;
  p->value = Qunbound;
  p->fn = Qunbound;
  p->plist = Qnil;
  p->package = Qnil;
  p->name = name;
  return p;
}

lisp
Fget (lisp symbol, lisp indicator, lisp defalt)
{
  check_symbol (symbol);
  return Fgetf (xsymbol_plist (symbol), indicator, defalt);
}

static int
remf (lisp *p, lisp i)
{
  lisp cdr;
  for (; consp (*p); p = &xcdr (cdr))
    {
      cdr = xcdr (*p);
      if (!consp (cdr))
        break;
      if (xcar (*p) == i)
        {
          *p = xcdr (cdr);
          return 1;
        }
    }
  return 0;
}

lisp
Fremprop (lisp symbol, lisp indicator)
{
  check_symbol (symbol);
  return boole (remf (&xsymbol_plist (symbol), indicator));
}

lisp
Fsi_putprop (lisp symbol, lisp value, lisp indicator)
{
  check_symbol (symbol);
  xsymbol_plist (symbol) = Fsi_putf (xsymbol_plist (symbol), value, indicator);
  return value;
}

lisp
Fsymbol_plist (lisp symbol)
{
  check_symbol (symbol);
  return xsymbol_plist (symbol);
}

lisp
Fsi_set_symbol_plist (lisp symbol, lisp plist)
{
  check_symbol (symbol);
  xsymbol_plist (symbol) = plist;
  return plist;
}

lisp
Fgetf (lisp place, lisp indicator, lisp defalt)
{
  lisp cdr;
  for (lisp pl = place; consp (pl); pl = xcdr (cdr))
    {
      cdr = xcdr (pl);
      if (!consp (cdr))
        break;
      if (xcar (pl) == indicator)
        return xcar (cdr);
    }
  return defalt ? defalt : Qnil;
}

lisp
Fsi_remf (lisp place, lisp indicator)
{
  multiple_value::value (1) = boole (remf (&place, indicator));
  multiple_value::count () = 2;
  return place;
}

lisp
Fsi_putf (lisp place, lisp value, lisp indicator)
{
  lisp cdr;
  for (lisp pl = place; consp (pl); pl = xcdr (cdr))
    {
      cdr = xcdr (pl);
      if (!consp (cdr))
        break;
      if (xcar (pl) == indicator)
        {
          xcar (cdr) = value;
          return place;
        }
    }
  return xcons (indicator, xcons (value, place));
}

lisp
Fget_properties (lisp place, lisp indicator_list)
{
  lisp cdr;
  multiple_value::count () = 3;
  for (lisp pl = place; consp (pl); pl = xcdr (cdr))
    {
      cdr = xcdr (pl);
      if (!consp (cdr))
        break;
      for (lisp i = indicator_list; consp (i); i = xcdr (i))
        if (xcar (pl) == xcar (i))
          {
            multiple_value::value (1) = xcar (cdr);
            multiple_value::value (2) = pl;
            return xcar (i);
          }
    }
  multiple_value::value (1) = Qnil;
  multiple_value::value (2) = Qnil;
  return Qnil;
}

lisp
Fsymbol_name (lisp symbol)
{
  check_symbol (symbol);
  return xsymbol_name (symbol);
}

lisp
Fmake_symbol (lisp name)
{
  check_string (name);
  return make_symbol (name);
}

lisp
Fcopy_symbol (lisp symbol, lisp copy_props)
{
  check_symbol (symbol);
  lisp new_symbol = Fmake_symbol (xsymbol_name (symbol));
  if (copy_props && copy_props != Qnil)
    {
      xsymbol_value (new_symbol) = xsymbol_value (symbol);
      xsymbol_function (new_symbol) = xsymbol_function (symbol);
      xsymbol_plist (new_symbol) = xsymbol_plist (symbol);
    }
  return new_symbol;
}

lisp
Fsymbol_package (lisp symbol)
{
  check_symbol (symbol);
  return xsymbol_package (symbol);
}

lisp
Fkeywordp (lisp symbol)
{
  return boole (symbolp (symbol)
                && xsymbol_package (symbol) == xsymbol_value (Vkeyword_package));
}

