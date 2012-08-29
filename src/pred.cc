#include "stdafx.h"
#include "ed.h"
#include "lex.h"

static int
compiled_function_p (lisp object)
{
  lisp body = xclosure_body (object);
  if (!consp (body) || xcar (body) != Qlambda
      || !consp (xcdr (body)))
    return 0;
  for (body = xcdr (body); consp (xcdr (body)); body = xcdr (body))
    QUIT;
  body = xcar (body);
  return consp (body) && xcar (body) == Ssi_byte_code;
}

/*GENERIC_FUNCTION*/
lisp
Ftype_of (lisp object)
{
  if (immediatep (object))
    {
      if (short_int_p (object))
        return Qinteger;
      else if (charp (object))
        return Qcharacter;
      else if (messagep (object))
        return Qmessage;
      else
        assert (0);
    }
  else
    {
      switch (object_typeof (object))
        {
        case Tarray:
        case Tstring_array:
          return Qarray;

        case Tcomplex_vector:
          return Qvector;

        case Tsimple_vector:
          return Qsimple_vector;

        case Tcomplex_string:
          return Qstring;

        case Tsimple_string:
          return Qsimple_string;

        case Tlong_int:
        case Tbignum:
          return Qinteger;

        case Tfraction:
          return Qratio;

        case Tsingle_float:
          return Qsingle_float;

        case Tdouble_float:
          return Qdouble_float;

        case Tcomplex:
          return Qcomplex;

        case Tcons:
          return Qcons;

        case Tsymbol:
          return Qsymbol;

        case Tclosure:
          return (compiled_function_p (object)
                  ? Qcompiled_function : Qfunction);

        case Tfunction:
          return Qcompiled_function;

        case Thash_table:
          return Qhash_table;

        case Tstream:
          return Qstream;

        case Tpackage:
          return Qpackage;

        case Trandom_state:
          return Qrandom_state;

        case Tstruct_def:
          return Qsi_structure_definition;

        case Tstruct_data:
          return Qstructure;

        case Treadtable:
          return Qreadtable;

        case Twindow:
          return Qwindow;

        case Tbuffer:
          return Qbuffer;

        case Tmarker:
          return Qmarker;

        case Tsyntax_table:
          return Qsyntax_table;

        case Tprocess:
          return Qprocess;

        case Tregexp:
          return Qregexp;

        case Twin32_menu:
          return Qmenu;

        case Twin32_dde_handle:
          return Qdde_handle;

        case Terror:
          return Qerror;

        case Tchunk:
          return Qsi_chunk;

        case Tdll_module:
          return Qsi_dll_module;

        case Tdll_function:
          return Qsi_c_function;

        case Tc_callable:
          return Qsi_c_callable;

        case Toledata:
          return Qoledata;

        case Twait_object:
          return Qwait_object;

        case Tchar_encoding:
          return Qchar_encoding;

        case Tenvironment:
          return Qsi_environment;

        default:
          assert (0);
          break;
        }
    }
  return Qnil;
}

lisp
Fnull (lisp object)
{
  return boole (object == Qnil);
}

lisp
Fsymbolp (lisp object)
{
  return boole (symbolp (object));
}

lisp
Fatom (lisp object)
{
  return boole (!consp (object));
}

lisp
Fconsp (lisp object)
{
  return boole (consp (object));
}

lisp
Flistp (lisp object)
{
  return boole (object == Qnil || consp (object));
}

lisp
Fnumberp (lisp object)
{
  return boole (numberp (object));
}

lisp
Fintegerp (lisp object)
{
  return boole (integerp (object));
}

lisp
Fsi_bignump (lisp object)
{
  return boole (bignump (object));
}

lisp
Fsi_fixnump (lisp object)
{
  return boole (fixnump (object));
}

lisp
Ffloatp (lisp object)
{
  return boole (floatp (object));
}

lisp
Fshort_float_p (lisp object)
{
  return boole (single_float_p (object));
}

lisp
Fsingle_float_p (lisp object)
{
  return boole (single_float_p (object));
}

lisp
Fdouble_float_p (lisp object)
{
  return boole (double_float_p (object));
}

lisp
Flong_float_p (lisp object)
{
  return boole (double_float_p (object));
}

lisp
Fsi_ratiop (lisp object)
{
  return boole (fractionp (object));
}

lisp
Frationalp (lisp object)
{
  return boole (rationalp (object));
}

lisp
Frealp (lisp object)
{
  return boole (realp (object));
}

lisp
Fcomplexp (lisp object)
{
  return boole (complexp (object));
}

lisp
Fcharacterp (lisp object)
{
  return boole (charp (object));
}

lisp
Fstringp (lisp object)
{
  return boole (stringp (object));
}

lisp
Fsimple_string_p (lisp object)
{
  return boole (simple_string_p (object));
}

lisp
Fvectorp (lisp object)
{
  return boole (common_vector_p (object));
}

lisp
Fsimple_vector_p (lisp object)
{
  return boole (simple_vector_p (object));
}

lisp
Farrayp (lisp object)
{
  return boole (common_array_p (object));
}

lisp
Fsi_simple_array_p (lisp object)
{
  if (base_simple_vector_p (object))
    return Qt;
  if (!base_array_p (object))
    return Qnil;
  return boole (xarray_displaced_to (object) == Qnil
                && !xarray_adjustable (object)
                && !xarray_has_fillp (object));
}

lisp
Ffunctionp (lisp object)
{
  return boole (functionp (object) || closurep (object)
                || dll_function_p (object)
                || c_callable_p (object));
}

lisp
Fcompiled_function_p (lisp object)
{
  return boole (functionp (object)
                || (closurep (object) && compiled_function_p (object))
                || dll_function_p (object)
                || c_callable_p (object));
}

lisp
Fsi_builtin_function_p (lisp object)
{
  return boole (functionp (object));
}

lisp
Fsi_dll_module_p (lisp object)
{
  return boole (dll_module_p (object));
}

lisp
Fsi_dll_function_p (lisp object)
{
  return boole (dll_function_p (object));
}

lisp
Fsi_c_callable_p (lisp object)
{
  return boole (c_callable_p (object));
}

lisp
Fpackagep (lisp object)
{
  return boole (packagep (object));
}

lisp
Fwindowp (lisp object)
{
  return boole (windowp (object));
}

lisp
Fbufferp (lisp object)
{
  return boole (bufferp (object));
}

lisp
Fsyntax_table_p (lisp object)
{
  return boole (syntax_table_p (object));
}

lisp
Freadtablep (lisp object)
{
  return boole (readtablep (object));
}

lisp
Fmarkerp (lisp object)
{
  return boole (markerp (object));
}

lisp
Fregexpp (lisp object)
{
  return boole (regexpp (object));
}

lisp
Fprocessp (lisp object)
{
  return boole (processp (object));
}

lisp
Fmenup (lisp object)
{
  return boole (win32_menu_p (object));
}

lisp
Fdde_handle_p (lisp object)
{
  return boole (win32_dde_handle_p (object));
}

lisp
Foledatap (lisp object)
{
  return boole (oledata_p (object));
}

lisp
Fwait_object_p (lisp object)
{
  return boole (wait_object_p (object));
}

lisp
Fchar_encoding_p (lisp object)
{
  return boole (char_encoding_p (object));
}

lisp
Fsi_environmentp (lisp object)
{
  return boole (environmentp (object));
}

lisp
Feq (lisp x, lisp y)
{
  return boole (x == y);
}

/*GENERIC_FUNCTION*/
lisp
Feql (lisp x, lisp y)
{
  if (x == y)
    return Qt;

  if (immediatep (x) || immediatep (y))
    return Qnil;

  if (object_typeof (x) != object_typeof (y))
    return Qnil;

  return boole (numberp (x) && !number_compare (x, y));
}

/*GENERIC_FUNCTION*/
static lisp
equal (lisp x, lisp y, lisp seen)
{
  if (x == y)
    return Qt;

  if (numberp (x))
    return Feql (x, y);

  if (immediatep (x) || immediatep (y))
    return Qnil;

  int xtype = object_typeof (x) & ~TAsimple;
  int ytype = object_typeof (y) & ~TAsimple;
  if (xtype != ytype)
    return Qnil;
  switch (xtype)
    {
    case Tcons:
      QUIT;
      if (lisp dup = assq (x, seen))
        return boole (xcdr (dup) == y);
      seen = xcons (xcons (x, y), seen);
      if (equal (xcar (x), xcar (y), seen) == Qnil)
        return Qnil;
      return equal (xcdr (x), xcdr (y), seen);

    case Tcomplex_string:
      return boole (string_equal (x, y));

    default:
      return Qnil;
    }
}

lisp
Fequal (lisp x, lisp y)
{
  return equal (x, y, Qnil);
}

static lisp
general_array_equalp (lisp *p1, int l1, lisp *p2, int l2)
{
  if (l1 != l2)
    return Qnil;
  for (lisp *pe = p1 + l1; p1 < pe; p1++, p2++)
    if (Fequalp (*p1, *p2) == Qnil)
      return Qnil;
  return Qt;
}

static lisp
string_array_equalp (const lisp *v, int vl, const Char *s, int sl)
{
  if (vl != sl)
    return Qnil;
  for (const lisp *ve = v + vl; v < ve; v++, s++)
    if (!charp (*v) || char_upcase (xchar_code (*v)) != char_upcase (*s))
      return Qnil;
  return Qt;
}

static int
array_rank_matchp (lisp x, lisp y)
{
  if (xarray_rank (x) != xarray_rank (y))
    return 0;
  for (int i = xarray_rank (x) - 1; i >= 0; i--)
    if (xarray_dims (x) [i] != xarray_dims (y) [i])
      return 0;
  return 1;
}

/*GENERIC_FUNCTION*/
static lisp
equalp (lisp x, lisp y, lisp seen)
{
  if (x == y)
    return Qt;

  if (numberp (x) && numberp (y))
    return boole (!number_compare (x, y));

  if (charp (x) && charp (y))
    return boole (char_upcase (xchar_code (x)) == char_upcase (xchar_code (y)));

  if (immediatep (x) || immediatep (y))
    return Qnil;

  int xtype = object_typeof (x);
  int ytype = object_typeof (y);

  if (base_vector_p (xtype))
    {
      if (!base_vector_p (ytype))
        return Qnil;
      if (common_vector_p (xtype) != common_vector_p (ytype))
        return Qnil;
      if (!common_vector_p (xtype) && !array_rank_matchp (x, y))
        return Qnil;

      xtype &= TAtype_mask;
      ytype &= TAtype_mask;
      switch (xtype)
        {
        case TAgeneral:
          switch (ytype)
            {
            case TAgeneral:
              return general_array_equalp ((lisp *)xbase_vector_contents (x),
                                           xvector_length (x),
                                           (lisp *)xbase_vector_contents (y),
                                           xvector_length (y));
            case TAstring:
              return string_array_equalp ((lisp *)xbase_vector_contents (x),
                                          xvector_length (x),
                                          (Char *)xbase_vector_contents (y),
                                          xvector_length (y));
            default:
              assert (0);
              return Qnil;
            }

        case TAstring:
          switch (ytype)
            {
            case TAgeneral:
              return string_array_equalp ((lisp *)xbase_vector_contents (y),
                                          xvector_length (y),
                                          (Char *)xbase_vector_contents (x),
                                          xvector_length (x));
            case TAstring:
              return boole (string_equalp ((Char *)xbase_vector_contents (x),
                                           xvector_length (x),
                                           (Char *)xbase_vector_contents (y),
                                           xvector_length (y)));
            default:
              assert (0);
              return Qnil;
            }

        default:
          assert (0);
          return Qnil;
        }
    }

  if (xtype != ytype)
    return Qnil;

  switch (xtype)
    {
    case Tcons:
      QUIT;
      if (lisp dup = assq (x, seen))
        return boole (xcdr (dup) == y);
      seen = xcons (xcons (x, y), seen);
      if (equalp (xcar (x), xcar (y), seen) == Qnil)
        return Qnil;
      return equalp (xcdr (x), xcdr (y), seen);

    case Thash_table:
      return boole (equalp ((lhash_table *)x, (lhash_table *)y));

    case Tstruct_data:
      return boole (structure_equalp (x, y));

    default:
      return Qnil;
    }
}

lisp
Fequalp (lisp x, lisp y)
{
  return equalp (x, y, Qnil);
}
