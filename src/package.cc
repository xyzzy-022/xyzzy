#include "stdafx.h"
#include "ed.h"

lisp
make_package (lisp name, lisp nicknames, int isize, int esize)
{
  lisp package = make_package ();
  xpackage_name (package) = name;
  xpackage_nicknames (package) = nicknames;
  xpackage_internal (package) = make_vector (isize, Qnil);
  xpackage_external (package) = make_vector (esize, Qnil);
  return package;
}

lisp
current_package ()
{
  lisp p = xsymbol_value (Vpackage);
  if (!packagep (p))
    {
      xsymbol_value (Vpackage) = xsymbol_value (Vuser_package);
      FEprogram_error (Epackage_value_is_not_package, p);
    }
  return p;
}

lisp
coerce_to_package (lisp name)
{
  if (!name)
    return current_package ();
  lisp p = Ffind_package (name);
  if (p == Qnil)
    FEsimple_package_error (Qnil, Eno_package, name);
  return p;
}

static lisp
coerce_to_package_name (lisp name)
{
  if (symbolp (name))
    name = xsymbol_name (name);
  else
    check_string (name);
  return name;
}

static lisp
coerce_to_package_nicknames (lisp p)
{
  if (p != Qnil)
    check_cons (p);
  lisp nicknames = Qnil;
  for (; consp (p); p = xcdr (p))
    {
      if (symbolp (xcar (p)))
        nicknames = xcons (xsymbol_name (xcar (p)), nicknames);
      else
        {
          check_string (xcar (p));
          nicknames = xcons (xcar (p), nicknames);
        }
    }
  return nicknames;
}

static lisp
lookup (u_int hash, lisp string, lisp vector)
{
  assert (general_vector_p (vector));
  assert (stringp (string));
  for (lisp p = xvector_contents (vector) [hash % u_int (xvector_length (vector))];
       consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (symbolp (x) && string_equal (string, xsymbol_name (x)))
        return x;
    }
  return 0;
}

static lisp
find_symbol (u_int hash, lisp string, lisp package)
{
  assert (stringp (string));
  assert (packagep (package));

  lisp symbol;
  lisp f = Qnil;
  if ((symbol = lookup (hash, string, xpackage_internal (package))))
    f = Kinternal;
  else if ((symbol = lookup (hash, string, xpackage_external (package))))
    f = Kexternal;
  else
    for (lisp p = xpackage_use_list (package); consp (p); p = xcdr (p))
      if (packagep (xcar (p))
          && (symbol = lookup (hash, string, xpackage_external (xcar (p)))))
        {
          f = Kinherited;
          break;
        }
  multiple_value::count () = 2;
  multiple_value::value (1) = f;
  return symbol;
}

static void
import_symbol (lisp symbol, lisp package)
{
  check_symbol (symbol);

  /* import signals a correctable error if any of the imported symbols has
     the same name as some distinct symbol already accessible in the package. */

  lisp name = xsymbol_name (symbol);
  u_int hash = hashpjw (name);
  lisp x = find_symbol (hash, name, package);
  if (x && x != symbol)
    FEsimple_package_error (package, Ecannot_import_because_name_conflict, symbol);

  /* If the symbol is already present in the importing package, import has
     no effect. */

  if (multiple_value::value (1) == Kexternal
      || multiple_value::value (1) == Kinternal)
    return;

  lisp vec = xpackage_internal (package);
  u_int h = hash % xvector_length (vec);
  xvector_contents (vec) [h] = xcons (symbol, xvector_contents (vec) [h]);

  /* X3J13 voted in June 1987 (IMPORT-SETF-SYMBOL-PACKAGE) to clarify that
     if any symbol to be imported has no home package then import sets the
     home package of the symbol to the package to which the symbol is being
     imported. */

  if (xsymbol_package (symbol) == Qnil)
    xsymbol_package (symbol) = package;
}

static void
export_symbol (lisp symbol, lisp package)
{
  check_symbol (symbol);
  lisp name = xsymbol_name (symbol);
  u_int hash = hashpjw (name);

  if (find_symbol (hash, name, package) != symbol)
    FEsimple_package_error (package, Eis_not_accessible_symbol, symbol);
  if (multiple_value::value (1) == Kexternal)
    return;
  int do_import = multiple_value::value (1) == Kinherited;

  for (lisp p = xpackage_used_by_list (package); consp (p); p = xcdr (p))
    {
      lisp x = find_symbol (hash, name, xcar (p));
      if (x && x != symbol && !memq (x, xpackage_shadowings (xcar (p))))
        FEsimple_package_error (xcar (p), Ecannot_export_because_name_conflict,
                                symbol);
    }

  if (do_import)
    import_symbol (symbol, package);

  lisp vec = xpackage_external (package);
  u_int h = hash % xvector_length (vec);
  xvector_contents (vec) [h] = xcons (symbol, xvector_contents (vec) [h]);

  vec = xpackage_internal (package);
  delq (symbol, &xvector_contents (vec) [hash % xvector_length (vec)]);
}

static void
unexport (lisp symbol, lisp package)
{
  check_symbol (symbol);
  lisp name = xsymbol_name (symbol);
  u_int hash = hashpjw (name);

  if (find_symbol (hash, name, package) != symbol)
    FEsimple_package_error (package, Eis_not_accessible_symbol, symbol);
  if (multiple_value::value (1) != Kexternal)
    return;

  lisp vec = xpackage_internal (package);
  u_int h = hash % xvector_length (vec);
  xvector_contents (vec) [h] = xcons (symbol, xvector_contents (vec) [h]);

  vec = xpackage_external (package);
  delq (symbol, &xvector_contents (vec) [hash % xvector_length (vec)]);
}

static void
shadowing_import (lisp symbol, lisp package)
{
  check_symbol (symbol);

  /* shadowing-import does name-conflict checking to the extent that
     it checks whether a distinct existing symbol with the same name
     is accessible. */

  int must_be_uninterned = 0;
  int insert = 1;
  lisp name = xsymbol_name (symbol);
  u_int hash = hashpjw (name);
  lisp x = find_symbol (hash, name, package);
  lisp sf = multiple_value::value (1);
  if (x && sf != Kinherited)
    {
      /* If so, it is shadowed by the new symbol, which implies that it
         must be uninterned if it was directly present in the package. */
      if (x != symbol)
        must_be_uninterned = 1;
      else
        insert = 0;
    }

  lisp shadow = (!memq (symbol, xpackage_shadowings (package))
                 ? xcons (symbol, xpackage_shadowings (package)) : 0);

  if (insert)
    {
      lisp vec = xpackage_internal (package);
      u_int h = hash % xvector_length (vec);
      xvector_contents (vec) [h] = xcons (symbol, xvector_contents (vec) [h]);
    }

  /* In addition to being imported, the symbol is placed on the
     shadowing-symbols list of package. */
  if (shadow)
    xpackage_shadowings (package) = shadow;

  if (must_be_uninterned)
    {
      lisp vec = (sf == Kinternal
                  ? xpackage_internal (package)
                  : xpackage_external (package));
      delq (x, &xvector_contents (vec) [hash % xvector_length (vec)]);
      delq (x, &xpackage_shadowings (package));
      if (xsymbol_package (x) == package)
        xsymbol_package (x) = Qnil;
    }
}

static void
shadow (lisp symbol, lisp package)
{
  /* The print name of each symbol is extracted, and the specified package
     is searched for a symbol of that name. */

  /* X3J13 voted in March 1988 (SHADOW-ALREADY-PRESENT) to change shadow to
     accept strings as well as symbols (a string in the symbols list being
     treated as a print name). */

  lisp name;
  if (symbolp (symbol))
    name = xsymbol_name (symbol);
  else if (stringp (symbol))
    name = symbol;
  else
    FEtype_error (symbol, xsymbol_value (Qor_symbol_string));

  u_int hash = hashpjw (name);
  int insert = 0;
  symbol = find_symbol (hash, name, package);
  if (symbol && multiple_value::value (1) != Kinherited)
    /* If such a symbol is present in this package (directly, not by
       inheritance), then nothing is done. */
    ;
  else
    {
      /* Otherwise, a new symbol is created with this print name, and it
         is inserted in the package as an internal symbol. */
      symbol = make_symbol (name);
      xsymbol_package (symbol) = package;
      insert = 1;
    }

  /* X3J13 voted in March 1988 (SHADOW-ALREADY-PRESENT) to clarify that if
    a symbol of specified name is already in the package but is not yet on
    the shadowing-symbols list for that package, then shadow does add it
    to the shadowing-symbols list rather than simply doing nothing. */

  lisp shadow = (!memq (symbol, xpackage_shadowings (package))
                 ? xcons (symbol, xpackage_shadowings (package)) : 0);

  if (insert)
    {
      lisp vec = xpackage_internal (package);
      u_int h = hash % xvector_length (vec);
      xvector_contents (vec) [h] = xcons (symbol, xvector_contents (vec) [h]);
    }

  if (shadow)
    xpackage_shadowings (package) = shadow;
}

static void
use_package (lisp use, lisp package)
{
  use = coerce_to_package (use);
  if (use == xsymbol_value (Vkeyword_package))
    FEsimple_package_error (package, Ecannot_use_keyword_package,
                            xsymbol_value (Vkeyword_package));
  if (use == package || memq (use, xpackage_use_list (package)))
    return;

  lisp vec = xpackage_external (use);
  for (lisp *v = xvector_contents (vec), *ve = v + xvector_length (vec);
       v < ve; v++)
    for (lisp p = *v; consp (p); p = xcdr (p))
      {
        lisp name = xsymbol_name (xcar (p));
        lisp x = find_symbol (hashpjw (name), name, package);
        if (x && x != xcar (p) && !memq (x, xpackage_shadowings (package)))
          FEsimple_package_error (use,
                                  Ecannot_use_because_name_conflict,
                                  xcar (p));
      }
  lisp used_by = xcons (package, xpackage_used_by_list (use));
  xpackage_use_list (package) = xcons (use, xpackage_use_list (package));
  xpackage_used_by_list (use) = used_by;
}

static void
unuse_package (lisp unuse, lisp package)
{
  unuse = coerce_to_package (unuse);
  delq (unuse, &xpackage_use_list (package));
  delq (package, &xpackage_used_by_list (unuse));
}

static int
find_prime (int n)
{
  static const int prime[] =
    {11, 53, 101, 211, 307, 401, 503, 601, 701, 809, 907, 1009,};
  int i;
  for (i = 0; i < numberof (prime); i++)
    if (prime[i] >= n)
      return prime[i];
  return prime[i - 1];
}

lisp
Fmake_package (lisp name, lisp keys)
{
  name = coerce_to_package_name (name);
  lisp nicknames = coerce_to_package_nicknames (find_keyword (Knicknames, keys));

  if (Ffind_package (name) != Qnil)
    FEsimple_package_error (Qnil, Epackage_already_exists, name);
  for (lisp p = nicknames; consp (p); p = xcdr (p))
    if (Ffind_package (xcar (p)) != Qnil)
      FEsimple_package_error (Qnil, Epackage_already_exists, xcar (p));

  int isize = find_prime (fixnum_value (find_keyword (Kinternal_size, keys,
                                                      make_fixnum (211))));
  int esize = find_prime (fixnum_value (find_keyword (Kexternal_size, keys,
                                                      make_fixnum (101))));
  lisp package = make_package (name, nicknames, isize, esize);
  lisp use = find_keyword (Kuse, keys);
  if (use == Qnil)
    use = xcons (xsymbol_value (Vlisp_package), Qnil);
  Fuse_package (use, package);
  xsymbol_value (Vpackage_list) = xcons (package, xsymbol_value (Vpackage_list));
  return package;
}

lisp
Ffind_package (lisp name)
{
  if (packagep (name))
    return name;
  name = coerce_to_package_name (name);
  for (lisp p = xsymbol_value (Vpackage_list); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (string_equal (xpackage_name (x), name))
        return x;
      for (lisp q = xpackage_nicknames (x); consp (q); q = xcdr (q))
        if (string_equal (xcar (q), name))
          return x;
    }
  return Qnil;
}

lisp
Fpackage_name (lisp package)
{
  return xpackage_name (coerce_to_package (package));
}

lisp
Fpackage_nicknames (lisp package)
{
  return xpackage_nicknames (coerce_to_package (package));
}

static lisp
find_other_package (lisp package, lisp name)
{
  lisp p = Ffind_package (name);
  return (p == package) ? Qnil : p;
}

lisp
Frename_package (lisp package, lisp new_name, lisp new_nicknames)
{
  package = coerce_to_package (package);
  new_name = coerce_to_package_name (new_name);
  new_nicknames = new_nicknames ? coerce_to_package_nicknames (new_nicknames) : Qnil;
  if (find_other_package (package, new_name) != Qnil)
    FEsimple_package_error (Qnil, Epackage_already_exists, new_name);
  for (lisp p = new_nicknames; consp (p); p = xcdr (p))
    if (find_other_package (package, xcar (p)) != Qnil)
      FEsimple_package_error (Qnil, Epackage_already_exists, xcar (p));
  xpackage_name (package) = new_name;
  xpackage_nicknames (package) = new_nicknames;
  return package;
}

lisp
Fpackage_use_list (lisp package)
{
  return xpackage_use_list (coerce_to_package (package));
}

lisp
Fpackage_used_by_list (lisp package)
{
  return xpackage_used_by_list (coerce_to_package (package));
}

lisp
Fpackage_shadowing_symbols (lisp package)
{
  return xpackage_shadowings (coerce_to_package (package));
}

lisp
Flist_all_packages ()
{
  return Fcopy_list (xsymbol_value (Vpackage_list));
}

lisp
Fsi_list_builtin_packages ()
{
  return Fcopy_list (xsymbol_value (Vbuiltin_package_list));
}

lisp
Fsi_builtin_package_p (lisp package)
{
  if (!(stringp (package) || symbolp (package) || packagep (package)))
    return Qnil;
  package = Ffind_package (package);
  return boole (memq (package, xsymbol_value (Vbuiltin_package_list)));
}

lisp
Fdelete_package (lisp package)
{
  package = coerce_to_package (package);
  if (consp (xpackage_used_by_list (package)))
    FEsimple_package_error (package, Epackage_is_used,
                            xpackage_used_by_list (package));
  if (xpackage_name (package) == Qnil)
    return Qnil;
  for (lisp p = xpackage_use_list (package); consp (p); p = xcdr (p))
    delq (package, &xpackage_used_by_list (xcar (p)));
  if (!delq (package, &xsymbol_value (Vpackage_list)))
    {
      assert (0);
      return Qnil;
    }
  delq (package, &xsymbol_value (Vbuiltin_package_list));
  xpackage_name (package) = Qnil;
  xpackage_nicknames (package) = Qnil;
  xpackage_documentation (package) = Qnil;
  return Qt;
}

lisp
Fintern (lisp string, lisp package)
{
  check_string (string);
  package = coerce_to_package (package);
  u_int hash = hashpjw (string);
  lisp symbol = find_symbol (hash, string, package);
  if (!symbol)
    {
      symbol = Fmake_symbol (string);
      lisp vec;
      if (package == xsymbol_value (Vkeyword_package))
        {
          xsymbol_value (symbol) = symbol;
          xsymbol_flags (symbol) |= SFconstant;
          vec = xpackage_external (package);
        }
      else
        vec = xpackage_internal (package);
      hash %= u_int (xvector_length (vec));
      xvector_contents (vec) [hash] = xcons (symbol, xvector_contents (vec) [hash]);
      xsymbol_package (symbol) = package;
    }
  return symbol;
}

lisp
Ffind_symbol (lisp string, lisp package)
{
  check_string (string);
  lisp symbol = find_symbol (hashpjw (string), string, coerce_to_package (package));
  return symbol ? symbol : Qnil;
}

lisp
Funintern (lisp symbol, lisp package)
{
  check_symbol (symbol);
  package = coerce_to_package (package);
  lisp string = xsymbol_name (symbol);
  u_int hash = hashpjw (string);
  lisp vec;
  if (lookup (hash, string, xpackage_internal (package)))
    vec = xpackage_internal (package);
  else if (lookup (hash, string, xpackage_external (package)))
    vec = xpackage_external (package);
  else
    return Qnil;

  if (memq (symbol, xpackage_shadowings (package)))
    {
      lisp x = 0;
      for (lisp p = xpackage_use_list (package); consp (p); p = xcdr (p))
        {
          lisp y = lookup (hash, string, xpackage_external (xcar (p)));
          if (y)
            {
              if (!x)
                x = y;
              else if (x != y)
                FEsimple_package_error (xcar (p),
                                        Eunable_to_unintern_shadowing_symbol,
                                        symbol);
            }
        }
      delq (symbol, &xpackage_shadowings (package));
    }

  delq (symbol, &xvector_contents (vec) [hash % xvector_length (vec)]);
  if (xsymbol_package (symbol) == package)
    xsymbol_package (symbol) = Qnil;
  return Qt;
}

lisp
Fexport (lisp symbols, lisp package)
{
  package = coerce_to_package (package);
  if (symbols != Qnil && !consp (symbols))
    export_symbol (symbols, package);
  else
    for (; consp (symbols); symbols = xcdr (symbols))
      export_symbol (xcar (symbols), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Funexport (lisp symbols, lisp package)
{
  package = coerce_to_package (package);
  if (package == xsymbol_value (Vkeyword_package))
    FEsimple_package_error (package, Eunable_to_unexport_keyword_package,
                            xsymbol_value (Vkeyword_package));
  if (symbols != Qnil && !consp (symbols))
    unexport (symbols, package);
  else
    for (; consp (symbols); symbols = xcdr (symbols))
      unexport (xcar (symbols), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Fimport (lisp symbols, lisp package)
{
  package = coerce_to_package (package);
  if (symbols != Qnil && !consp (symbols))
    import_symbol (symbols, package);
  else
    for (; consp (symbols); symbols = xcdr (symbols))
      import_symbol (xcar (symbols), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Fshadowing_import (lisp symbols, lisp package)
{
  package = coerce_to_package (package);
  if (symbols != Qnil && !consp (symbols))
    shadowing_import (symbols, package);
  else
    for (; consp (symbols); symbols = xcdr (symbols))
      shadowing_import (xcar (symbols), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Fshadow (lisp symbols, lisp package)
{
  package = coerce_to_package (package);
  if (symbols != Qnil && !consp (symbols))
    shadow (symbols, package);
  else
    for (; consp (symbols); symbols = xcdr (symbols))
      shadow (xcar (symbols), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Fuse_package (lisp packages_to_use, lisp package)
{
  package = coerce_to_package (package);
  if (packages_to_use != Qnil && !consp (packages_to_use))
    use_package (packages_to_use, package);
  else
    for (; consp (packages_to_use); packages_to_use = xcdr (packages_to_use))
      use_package (xcar (packages_to_use), package);
  multiple_value::clear ();
  return Qt;
}

lisp
Funuse_package (lisp packages_to_unuse, lisp package)
{
  package = coerce_to_package (package);
  if (packages_to_unuse != Qnil && !consp (packages_to_unuse))
    unuse_package (packages_to_unuse, package);
  else
    for (; consp (packages_to_unuse); packages_to_unuse = xcdr (packages_to_unuse))
      unuse_package (xcar (packages_to_unuse), package);
  return Qt;
}

lisp
Fsi_package_internal (lisp package)
{
  return xpackage_internal (coerce_to_package (package));
}

lisp
Fsi_package_external (lisp package)
{
  return xpackage_external (coerce_to_package (package));
}

lisp
Fsi_package_documentation (lisp package)
{
  return xpackage_documentation (coerce_to_package (package));
}

lisp
Fsi_set_package_documentation (lisp package, lisp documentation)
{
  check_string (documentation);
  xpackage_documentation (coerce_to_package (package)) = documentation;
  return documentation;
}

static lisp
lookup (u_int hash, const Char *s, int size, lisp vector)
{
  for (lisp p = xvector_contents (vector) [hash % u_int (xvector_length (vector))];
       consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (symbolp (x))
        {
          lisp name = xsymbol_name (x);
          if (xstring_length (name) == size && !bcmp (xstring_contents (name), s, size))
            return x;
        }
    }
  return 0;
}

void
maybe_symbol_string::parse (Char *&xb, int &xl)
{
  Char *b = xb;
  Char *be = b + xl;
  for (Char *colon = b; colon < be; colon++)
    if (*colon == ':')
      {
        if (colon == b)
          package = xsymbol_value (Vkeyword_package);
        else
          {
            temporary_string t ((Char *)b, colon - b);
            lisp pkg = Ffind_package (t.string ());
            if (pkg != Qnil)
              package = pkg;
          }

        pkge = colon;
        if (colon + 1 < be && colon[1] == ':')
          xb = colon + 2;
        else
          xb = colon + 1;
        xl = be - xb;
        break;
      }
}

lisp
Flookup_symbol (lisp from, lisp to, lisp package)
{
  package = coerce_to_package (package);

  Buffer *bp = selected_buffer ();
  point_t p1 = bp->coerce_to_restricted_point (from);
  point_t p2 = bp->coerce_to_restricted_point (to);
  if (p1 > p2)
    swap (p1, p2);

  int l = p2 - p1;
  Char *b = (Char *)alloca (sizeof *b * l);
  bp->substring (p1, l, b);

  maybe_symbol_string mss (package);
  mss.parse (b, l);
  package = mss.current_package ();

  u_int hash = hashpjw (b, l);
  lisp symbol;
  if (!(symbol = lookup (hash, b, l, xpackage_internal (package)))
      && (!(symbol = lookup (hash, b, l, xpackage_external (package)))))
    for (lisp p = xpackage_use_list (package); consp (p); p = xcdr (p))
      if (packagep (xcar (p))
          && (symbol = lookup (hash, b, l, xpackage_external (xcar (p)))))
        break;

  multiple_value::count () = 3;
  multiple_value::value (1) = symbol ? Qt : Qnil;
  multiple_value::value (2) = symbol ? Qt : Qnil;

  if (symbol)
    return symbol;

  const readtab_rep *readtab = xreadtable_rep (current_readtable ());
  for (const Char *p = b, *pe = p + l; p < pe; p++)
    if (stdchar_type (readtab, *p) != SCT_CONSTITUENT)
      return Qnil;

  multiple_value::value (2) = boole (parse_number_format (b, b + l, 10) == NF_BAD);
  return Qnil;
}

int
count_symbols (lisp vector)
{
  int size = 0;
  for (int i = xvector_length (vector) - 1; i >= 0; i--)
    for (lisp p = xvector_contents (vector) [i]; consp (p); p = xcdr (p))
      size++;
  return size;
}

lisp
Fsi_package_summary (lisp package)
{
  package = coerce_to_package (package);
  multiple_value::count () = 2;
  multiple_value::value (1) =
    make_fixnum (count_symbols (xpackage_external (package)));
  return make_fixnum (count_symbols (xpackage_internal (package)));
}
