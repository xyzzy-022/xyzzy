#ifndef _structure_h_
# define _structure_h_

struct struct_slotdesc
{
  lisp name;
  lisp default_init;
  lisp type;
  lisp read_only;
  lisp offset;
};

class lstruct_def: public lisp_object
{
public:
  lisp name;
  lisp type;
  lisp includes;
  lisp constructors;
  lisp print_function;
  lisp report;
  int nslots;
  struct_slotdesc *slotdesc;
  char named;
  char read_only;
  char important;

  ~lstruct_def () {xfree (slotdesc);}
};

class lstruct_data: public lisp_object
{
public:
  lisp def;
  int nslots;
  lisp *data;

  ~lstruct_data () {xfree (data);}
};

#define struct_def_p(X) typep ((X), Tstruct_def)
#define struct_data_p(X) typep ((X), Tstruct_data)

inline void
check_struct_def (lisp x)
{
  check_type (x, Tstruct_def, Qsi_structure_definition);
}

inline void
check_struct_data (lisp x)
{
  check_type (x, Tstruct_data, Qstructure);
}

inline lisp &
xstrdef_name (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->name;
}

inline lisp &
xstrdef_type (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->type;
}

inline lisp &
xstrdef_includes (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->includes;
}

inline lisp &
xstrdef_constructors (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->constructors;
}

inline lisp &
xstrdef_print_function (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->print_function;
}

inline lisp &
xstrdef_report (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->report;
}

inline int &
xstrdef_nslots (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->nslots;
}

inline struct_slotdesc *&
xstrdef_slotdesc (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->slotdesc;
}

inline char &
xstrdef_named_p (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->named;
}

inline char &
xstrdef_read_only_p (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->read_only;
}

inline char &
xstrdef_important_p (lisp x)
{
  assert (struct_def_p (x));
  return ((lstruct_def *)x)->important;
}

inline lisp &
xstrdata_def (lisp x)
{
  assert (struct_data_p (x));
  return ((lstruct_data *)x)->def;
}

inline int &
xstrdata_nslots (lisp x)
{
  assert (struct_data_p (x));
  return ((lstruct_data *)x)->nslots;
}

inline lisp *&
xstrdata_data (lisp x)
{
  assert (struct_data_p (x));
  return ((lstruct_data *)x)->data;
}

inline lstruct_def *
make_struct_def ()
{
  lstruct_def *p = ldata <lstruct_def, Tstruct_def>::lalloc ();
  p->name = Qnil;
  p->type = Qnil;
  p->includes = Qnil;
  p->constructors = Qnil;
  p->print_function = Qnil;
  p->report = Qnil;
  p->nslots = 0;
  p->slotdesc = 0;
  return p;
}

inline lstruct_data *
make_struct_data ()
{
  lstruct_data *p = ldata <lstruct_data, Tstruct_data>::lalloc ();
  p->def = Qnil;
  p->nslots = 0;
  p->data = 0;
  return p;
}

int structure_equalp (lisp, lisp);

#endif
