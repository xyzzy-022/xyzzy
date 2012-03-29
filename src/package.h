// -*-C++-*-
#ifndef _package_h_
# define _package_h_

class lpackage: public lisp_object
{
public:
  lisp name;
  lisp nicknames;
  lisp use_list;
  lisp used_by_list;
  lisp shadowings;
  lisp internal;
  lisp external;
  lisp documentation;
};

# define packagep(X) typep ((X), Tpackage)

inline void
check_package (lisp x)
{
  check_type (x, Tpackage, Qpackage);
}

inline lisp &
xpackage_name (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->name;
}

inline lisp &
xpackage_nicknames (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->nicknames;
}

inline lisp &
xpackage_use_list (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->use_list;
}

inline lisp &
xpackage_used_by_list (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->used_by_list;
}

inline lisp &
xpackage_shadowings (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->shadowings;
}

inline lisp &
xpackage_internal (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->internal;
}

inline lisp &
xpackage_external (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->external;
}

inline lisp &
xpackage_documentation (lisp x)
{
  assert (packagep (x));
  return ((lpackage *)x)->documentation;
}

inline lpackage *
make_package ()
{
  lpackage *p = ldata <lpackage, Tpackage>::lalloc ();
  p->name = Qnil;
  p->nicknames = Qnil;
  p->use_list = Qnil;
  p->used_by_list = Qnil;
  p->shadowings = Qnil;
  p->internal = Qnil;
  p->external = Qnil;
  p->documentation = Qnil;
  return p;
}

lisp coerce_to_package (lisp);
lisp make_package (lisp, lisp, int = 211, int = 103);
int count_symbols (lisp vector);

class maybe_symbol_string
{
  lisp package;
  const Char *pkge;
public:
  maybe_symbol_string (lisp pkg) : package (pkg), pkge (0) {}
  lisp current_package () const {return package;}
  void parse (Char *&b, int &l);
  const Char *pkg_end () const {return pkge;}
};

#endif
