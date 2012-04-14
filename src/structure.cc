#include "stdafx.h"
#include "ed.h"

static int
slot_index (lisp def, lisp slot)
{
  assert (struct_def_p (def));
  for (int i = 0; i < xstrdef_nslots (def); i++)
    if (xstrdef_slotdesc (def) [i].name == slot)
      return i;
  FEprogram_error (Einvalid_slot, slot);
  return 0;
}

lisp
Fsi_structure_definition_p (lisp def)
{
  return boole (struct_def_p (def));
}

lisp
Fsi_make_structure_definition (lisp name, lisp type, lisp include,
                               lisp constructors, lisp print_function,
                               lisp named, lisp slots,
                               lisp report, lisp important)
{
  check_symbol (name);
  if (include != Qnil)
    {
      check_struct_def (include);
      for (lisp x = xstrdef_includes (include); consp (x); x = xcdr (x))
        if (xstrdef_name (xcar (x)) == name)
          FEprogram_error (Estructure_includes_recursively, name);
    }

  int ooffset = -1;
  int nslots = 0;
  for (lisp sl = slots; consp (sl); sl = xcdr (sl))
    {
      lisp s = xcar (sl);
      int l = xlist_length (s);
      if (type == Qnil)
        {
          if (l != 4)
            FEprogram_error (Ewrong_slot_description, s);
        }
      else
        {
          if (l != 5)
            FEprogram_error (Ewrong_slot_description, s);
          lisp off = Fnth (make_fixnum (4), s);
          int n = fixnum_value (off);
          if (n <= ooffset)
            FErange_error (off);
          ooffset = n;
        }
      check_symbol (xcar (s));
      nslots++;
    }

  lisp def = make_struct_def ();
  xstrdef_name (def) = name;
  xstrdef_type (def) = type;
  xstrdef_includes (def) = (include == Qnil
                            ? Qnil
                            : xcons (include, xstrdef_includes (include)));
  xstrdef_constructors (def) = constructors;
  xstrdef_print_function (def) = print_function;
  xstrdef_report (def) = report ? report : Qnil;
  xstrdef_nslots (def) = nslots;
  xstrdef_slotdesc (def) = (struct_slotdesc *)xmalloc (sizeof (struct_slotdesc)
                                                       * nslots);
  xstrdef_named_p (def) = type != Qnil && named != Qnil;
  xstrdef_read_only_p (def) = 0;
  xstrdef_important_p (def) = important && important != Qnil;

  lisp sl = slots;
  for (int i = 0; i < nslots; i++, sl = xcdr (sl))
    {
      lisp s = xcar (sl);
      xstrdef_slotdesc (def) [i].name = xcar (s);
      s = xcdr (s);
      xstrdef_slotdesc (def) [i].default_init = xcar (s);
      s = xcdr (s);
      xstrdef_slotdesc (def) [i].type = xcar (s);
      s = xcdr (s);
      xstrdef_slotdesc (def) [i].read_only = xcar (s);
      s = xcdr (s);
      xstrdef_slotdesc (def) [i].offset = consp (s) ? xcar (s) : Qnil;
    }

  return def;
}

lisp
Fsi_structure_definition_name (lisp def)
{
  check_struct_def (def);
  return xstrdef_name (def);
}

lisp
Fsi_structure_definition_type (lisp def)
{
  check_struct_def (def);
  return xstrdef_type (def);
}

lisp
Fsi_structure_definition_constructors (lisp def)
{
  check_struct_def (def);
  return xstrdef_constructors (def);
}

lisp
Fsi_structure_definition_add_constructor (lisp def, lisp constructor)
{
  check_struct_def (def);
  xstrdef_constructors (def) = xcons (constructor,
                                      xstrdef_constructors (def));
  return Qt;
}

lisp
Fsi_structure_definition_print_function (lisp def)
{
  check_struct_def (def);
  return xstrdef_print_function (def);
}

lisp
Fsi_structure_definition_report (lisp def)
{
  check_struct_def (def);
  return xstrdef_report (def);
}

lisp
Fsi_structure_definition_nslots (lisp def)
{
  check_struct_def (def);
  return make_fixnum (xstrdef_nslots (def));
}

lisp
Fsi_structure_definition_slot_description (lisp def, lisp index)
{
  check_struct_def (def);
  int n = fixnum_value (index);
  if (n < 0 || n >= xstrdef_nslots (def))
    FErange_error (index);
  multiple_value::count () = xstrdef_type (def) == Qnil ? 4 : 5;
  multiple_value::value (4) = xstrdef_slotdesc (def) [n].offset;
  multiple_value::value (3) = xstrdef_slotdesc (def) [n].read_only;
  multiple_value::value (2) = xstrdef_slotdesc (def) [n].type;
  multiple_value::value (1) = xstrdef_slotdesc (def) [n].default_init;
  return xstrdef_slotdesc (def) [n].name;
}

lisp
Fsi_structure_definition_named_p (lisp def)
{
  check_struct_def (def);
  return boole (xstrdef_named_p (def));
}

lisp
Fsi_structure_definition_read_only_p (lisp def)
{
  check_struct_def (def);
  return boole (xstrdef_read_only_p (def));
}

lisp
Fsi_structure_definition_important_p (lisp def)
{
  check_struct_def (def);
  return boole (xstrdef_important_p (def));
}

lisp
Fsi_make_structure_data (lisp def)
{
  check_struct_def (def);
  if (xstrdef_type (def) != Qnil)
    FEprogram_error (Eis_typed_structure, def);
  lisp x = make_struct_data ();
  xstrdata_def (x) = def;
  xstrdata_data (x) = (lisp *)xmalloc (sizeof (lisp) * xstrdef_nslots (def));
  xstrdata_nslots (x) = xstrdef_nslots (def);
  for (int i = 0; i < xstrdata_nslots (x); i++)
    xstrdata_data (x) [i] = Qnil;
  return x;
}

lisp
Fsi_copy_structure_data (lisp src)
{
  check_struct_data (src);
  lisp dst = make_struct_data ();
  xstrdata_def (dst) = xstrdata_def (src);
  xstrdata_data (dst) = (lisp *)xmalloc (sizeof (lisp) * xstrdata_nslots (src));
  xstrdata_nslots (dst) = xstrdata_nslots (src);
  bcopy (xstrdata_data (src), xstrdata_data (dst), xstrdata_nslots (dst));
  return dst;
}

lisp
Fsi_structurep (lisp data)
{
  return boole (struct_data_p (data));
}

lisp
Fsi_structure_definition (lisp data)
{
  check_struct_data (data);
  return xstrdata_def (data);
}

lisp
Fsi_slot_index (lisp def, lisp slot)
{
  check_struct_def (def);
  return make_fixnum (slot_index (def, slot));
}

lisp
Fsi_slot_value (lisp data, lisp slot)
{
  check_struct_data (data);
  int n = slot_index (xstrdata_def (data), slot);
  if (n >= xstrdata_nslots (data))
    FEprogram_error (Einvalid_slot, slot);
  return xstrdata_data (data) [n];
}

lisp
Fsi_set_slot_value (lisp data, lisp slot, lisp value)
{
  check_struct_data (data);
  int n = slot_index (xstrdata_def (data), slot);
  if (n >= xstrdata_nslots (data))
    FEprogram_error (Einvalid_slot, slot);
  xstrdata_data (data) [n] = value;
  return value;
}

lisp
Fsi_index_slot_value (lisp data, lisp index)
{
  check_struct_data (data);
  int n = fixnum_value (index);
  if (n < 0 || n >= xstrdata_nslots (data))
    FEprogram_error (Einvalid_slot, index);
  return xstrdata_data (data) [n];
}

lisp
Fsi_set_index_slot_value (lisp data, lisp index, lisp value)
{
  check_struct_data (data);
  int n = fixnum_value (index);
  if (n < 0 || n >= xstrdata_nslots (data))
    FEprogram_error (Einvalid_slot, index);
  xstrdata_data (data) [n] = value;
  return value;
}

lisp
Fsi_structure_subtypep (lisp type1, lisp type2)
{
  if (!struct_def_p (type1) || !struct_def_p (type2))
    return Qnil;
  if (type1 == type2)
    return Qt;
  return boole (memq (type2, xstrdef_includes (type1)));
}

int
structure_equalp (lisp x, lisp y)
{
  assert (struct_data_p (x));
  assert (struct_data_p (y));
  if (xstrdata_def (x) != xstrdata_def (y))
    return 0;
  if (xstrdata_nslots (x) != xstrdata_nslots (y))
    return 0;
  for (int i = 0; i < xstrdata_nslots (x); i++)
    if (Fequalp (xstrdata_data (x) [i], xstrdata_data (y) [i]) == Qnil)
      return 0;
  return 1;
}

lisp
Fsi_structure_reader (lisp name, lisp args)
{
  return FEprogram_error (Eundefined_struct, name);
}
