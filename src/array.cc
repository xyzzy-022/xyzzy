#include "stdafx.h"
#include "ed.h"

lbase_array::~lbase_array ()
{
  if (displaced_to != Qnil)
    {
      contents = 0;
      if (base_array_p (displaced_to))
        {
          int f = delq (this, &xarray_referenced_list (displaced_to));
          assert (f);
        }
    }
  for (lisp x = referenced_list; consp (x); x = xcdr (x))
    {
      lisp r = xcar (x);
      assert (xarray_displaced_to (r) == this);
      xarray_displaced_to (r) = Qnil;
      assert (xbase_vector_contents (r));
      xbase_vector_contents (r) = 0;
    }
  xfree (dims);
}

void
lbase_array::common_init ()
{
  lbase_vector::common_init ();
  dims = 0;
  displaced_to = Qnil;
  referenced_list = Qnil;
  rank = 0;
  adjustable = 0;
  has_fillp = 0;
}

void
check_array_type (lisp type)
{
  if (type != Qt && type != Qcharacter)
    FEbad_type_specifier (type);
}

Char
initial_char_elem (lisp init)
{
  if (init == Qnil)
    return Char (0);
  check_char (init);
  return xchar_code (init);
}

static int
vector_index (lisp indexes, int maxl)
{
  if (!consp (indexes))
    FEtoo_few_arguments ();
  if (consp (xcdr (indexes)))
    FEtoo_many_arguments ();
  int i = fixnum_value (xcar (indexes));
  if (i < 0 || i >= maxl)
    FErange_error (indexes);
  return i;
}

static int
array_index (lisp array, lisp indexes)
{
  int rank = xarray_rank (array);
  const int *dims = xarray_dims (array);
  int index = 0;
  for (int i = 0; i < rank; i++, dims++, indexes = xcdr (indexes))
    {
      if (!consp (indexes))
        FEtoo_few_arguments ();
      int n = fixnum_value (xcar (indexes));
      if (n < 0 || n >= *dims)
        FErange_error (xcar (indexes));
      index = index * *dims + n;
    }
  if (consp (indexes))
    FEtoo_many_arguments ();
  return index;
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Faref (lisp array, lisp indexes)
{
  if (pointerp (array))
    {
      switch (object_typeof (array))
        {
        case Tsimple_vector:
          return xvector_contents (array)
            [vector_index (indexes, xvector_length (array))];

        case Tcomplex_vector:
          return xvector_contents (array)
            [vector_index (indexes, xvector_dimension (array))];

        case Tsimple_string:
          return make_char (xstring_contents (array)
                            [vector_index (indexes, xstring_length (array))]);

        case Tcomplex_string:
          return make_char (xstring_contents (array)
                            [vector_index (indexes, xstring_dimension (array))]);

        case Tarray:
          return xgeneral_array_contents (array) [array_index (array, indexes)];

        case Tstring_array:
          return make_char (xstring_array_contents (array)
                            [array_index (array, indexes)]);
        }
    }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Fsi_aset (lisp array, lisp value, lisp indexes)
{
  if (pointerp (array))
    {
      switch (object_typeof (array))
        {
        case Tsimple_vector:
          xvector_contents (array)
            [vector_index (indexes, xvector_length (array))] = value;
          return value;

        case Tcomplex_vector:
          xvector_contents (array)
            [vector_index (indexes, xvector_dimension (array))] = value;
          return value;

        case Tsimple_string:
          check_char (value);
          xstring_contents (array)
            [vector_index (indexes, xstring_length (array))] = xchar_code (value);
          return value;

        case Tcomplex_string:
          check_char (value);
          xstring_contents (array)
            [vector_index (indexes, xstring_dimension (array))] = xchar_code (value);
          return value;

        case Tarray:
          xgeneral_array_contents (array) [array_index (array, indexes)] = value;
          return value;

        case Tstring_array:
          check_char (value);
          xstring_array_contents (array)
            [array_index (array, indexes)] = xchar_code (value);
          return value;
        }
    }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Farray_has_fill_pointer_p (lisp array)
{
  if (pointerp (array))
    switch (object_typeof (array))
      {
      case Tcomplex_vector:
      case Tcomplex_string:
        return boole (xarray_has_fillp (array));

      case Tsimple_vector:
      case Tsimple_string:
      case Tarray:
      case Tstring_array:
        return Qnil;
      }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Farray_total_size (lisp array)
{
  if (pointerp (array))
    switch (object_typeof (array))
      {
      case Tsimple_vector:
      case Tsimple_string:
        return make_fixnum (xvector_length (array));

      case Tcomplex_vector:
      case Tcomplex_string:
        return make_fixnum (xvector_dimension (array));

      case Tarray:
      case Tstring_array:
        return make_fixnum (xarray_total_size (array));
      }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
array_element_type (lisp array)
{
  if (pointerp (array))
    switch (object_typeof (array) & TAtype_mask)
      {
      case TAgeneral:
        return Qt;

      case TAstring:
        return Qcharacter;
      }
  return Qnil;
}

void
check_displace_type (lisp displaced_to, lisp element_type)
{
  if (!pointerp (displaced_to)
      || element_type != array_element_type (displaced_to))
    FEprogram_error (Eunmatch_array_type, displaced_to);
}

/*GENERIC_FUNCTION:ARRAY*/
static int
array_element_size (lisp array)
{
  switch (object_typeof (array) & TAtype_mask)
    {
    case TAgeneral:
      return sizeof (lisp);

    case TAstring:
      return sizeof (Char);

    default:
      assert (0);
      return 0;
    }
}

static void *
array_contents (lisp array, int offset)
{
  int size = array_element_size (array);
  return (void *)((char *)xbase_vector_contents (array) + size * offset);
}

lisp
displace_array (lisp new_array, int new_size, lisp element_type,
                lisp displaced_to, lisp displaced_index_offset)
{
  check_array (displaced_to);
  check_displace_type (displaced_to, element_type);
  int offset = (displaced_index_offset == Qnil
                ? 0 : fixnum_value (displaced_index_offset));
  if (offset < 0)
    FErange_error (displaced_index_offset);
  int dest_size = fixnum_value (Farray_total_size (displaced_to));
  if (offset > dest_size || offset + new_size > dest_size) // avoid overflow
    FEprogram_error (Edisplaced_to_array_too_small);

  if (base_array_p (displaced_to))
    xarray_referenced_list (displaced_to) =
      xcons (new_array, xarray_referenced_list (displaced_to));
  xbase_vector_contents (new_array) = array_contents (displaced_to, offset);
  xarray_displaced_to (new_array) = displaced_to;
  return new_array;
}

void
adjust_displace_array (lisp array, void *newp)
{
  for (lisp x = xarray_referenced_list (array); consp (x); x = xcdr (x))
    {
      lisp r = xcar (x);
      assert (xarray_displaced_to (r) == array);
      adjust_displace_array (r, newp);
      xbase_vector_contents (r) =
        (void *)((char *)newp
                 + ((char *)xbase_vector_contents (r)
                    - (char *)xbase_vector_contents (array)));
    }
}

static int
check_dimensions (lisp dims)
{
  long size = 1;
  for (lisp x = dims; consp (x); x = xcdr (x))
    {
      long n = fixnum_value (xcar (x));
      if (n < 0)
        FErange_error (xcar (x));
      int64_t li = int64_t (size) * int64_t (n);
      if (int64_t (LONG_MAX) < li)
        FEprogram_error (Earray_size_too_large, dims);
      size = long (li);
    }
  if (int64_t (LONG_MAX) < int64_t (size) * sizeof (lisp))
    FEprogram_error (Earray_size_too_large, dims);
  return size;
}

/*GENERIC_FUNCTION:ARRAY*/
static lisp
allocate_contents (lisp array, lisp initial_element)
{
  switch (object_typeof (array) & TAtype_mask)
    {
    case TAgeneral:
      xgeneral_array_contents (array) =
        (lisp *)xmalloc (sizeof (lisp) * xarray_total_size (array));
      bfill (xgeneral_array_contents (array), xarray_total_size (array),
             initial_element);
      break;

    case TAstring:
      xstring_array_contents (array) =
        (Char *)xmalloc (sizeof (Char) * xarray_total_size (array));
      bfill (xstring_array_contents (array), xarray_total_size (array),
             initial_char_elem (initial_element));
      break;

    default:
      assert (0);
      break;
    }
  return array;
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Fsi_make_array (lisp dimensions, lisp element_type,
                lisp initial_element, lisp adjustable,
                lisp displaced_to, lisp displaced_index_offset)
{
  int rank = xlist_length (dimensions);
  if (rank >= ARRAY_RANK_LIMIT)
    FEprogram_error (Earray_rank_too_large);
  if (rank == 1)
    return Fsi_make_vector (xcar (dimensions), element_type,
                            initial_element, adjustable, Qnil,
                            displaced_to, displaced_index_offset);
  if (!rank && dimensions != Qnil)
    return Fsi_make_vector (dimensions, element_type,
                            initial_element, adjustable, Qnil,
                            displaced_to, displaced_index_offset);

  int total_size = check_dimensions (dimensions);

  check_array_type (element_type);

  lisp array;
  if (element_type == Qcharacter)
    array = make_string_array ();
  else
    array = make_general_array ();

  if (rank)
    {
      xarray_dims (array) = (int *)xmalloc (sizeof (int) * rank);
      for (int i = 0; i < rank; i++, dimensions = xcdr (dimensions))
        xarray_dims (array) [i] = fixnum_value (xcar (dimensions));
    }

  xarray_rank (array) = rank;
  xarray_total_size (array) = total_size;
  xarray_adjustable (array) = adjustable != Qnil;

  if (displaced_to == Qnil)
    return allocate_contents (array, initial_element);
  else
    return displace_array (array, total_size, element_type,
                           displaced_to, displaced_index_offset);
}

lisp
Farray_element_type (lisp array)
{
  lisp type = array_element_type (array);
  if (type == Qnil)
    FEtype_error (array, Qarray);
  return type;
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Farray_rank (lisp array)
{
  if (pointerp (array))
    switch (object_typeof (array))
      {
      case Tsimple_vector:
      case Tsimple_string:
      case Tcomplex_vector:
      case Tcomplex_string:
        return make_fixnum (1);

      case Tarray:
      case Tstring_array:
        return make_fixnum (xarray_rank (array));
      }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Farray_dimension (lisp array, lisp axis_num)
{
  int axis = fixnum_value (axis_num);
  if (pointerp (array))
    switch (object_typeof (array))
      {
      case Tsimple_vector:
      case Tsimple_string:
        if (axis)
          FErange_error (axis_num);
        return make_fixnum (xvector_length (array));

      case Tcomplex_vector:
      case Tcomplex_string:
        if (axis)
          FErange_error (axis_num);
        return make_fixnum (xvector_dimension (array));

      case Tarray:
      case Tstring_array:
        if (axis < 0 || axis >= xarray_rank (array))
          FErange_error (axis_num);
        return make_fixnum (xarray_dims (array) [axis]);
      }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Farray_row_major_index (lisp array, lisp indexes)
{
  if (pointerp (array))
    {
      switch (object_typeof (array))
        {
        case Tsimple_vector:
        case Tsimple_string:
          return make_fixnum (vector_index (indexes, xvector_length (array)));

        case Tcomplex_vector:
        case Tcomplex_string:
          return make_fixnum (vector_index (indexes, xvector_dimension (array)));

        case Tarray:
        case Tstring_array:
          return make_fixnum (array_index (array, indexes));
        }
    }
  return FEtype_error (array, Qarray);
}

static int
check_row_major_index (lisp index, int maxl)
{
  int i = fixnum_value (index);
  if (i < 0 || i >= maxl)
    FErange_error (index);
  return i;
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Frow_major_aref (lisp array, lisp index)
{
  if (pointerp (array))
    {
      int i;
      switch (object_typeof (array))
        {
        case Tsimple_vector:
          i = check_row_major_index (index, xvector_length (array));
          return xvector_contents (array) [i];

        case Tcomplex_vector:
          i = check_row_major_index (index, xvector_dimension (array));
          return xvector_contents (array) [i];

        case Tsimple_string:
          i = check_row_major_index (index, xstring_length (array));
          return make_char (xstring_contents (array) [i]);

        case Tcomplex_string:
          i = check_row_major_index (index, xstring_dimension (array));
          return make_char (xstring_contents (array) [i]);

        case Tarray:
          i = check_row_major_index (index, xarray_total_size (array));
          return xgeneral_array_contents (array) [i];

        case Tstring_array:
          i = check_row_major_index (index, xarray_total_size (array));
          return make_char (xstring_array_contents (array) [i]);
        }
    }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Fsi_row_major_aset (lisp array, lisp index, lisp value)
{
  if (pointerp (array))
    {
      int i;
      switch (object_typeof (array))
        {
        case Tsimple_vector:
          i = check_row_major_index (index, xvector_length (array));
          xvector_contents (array) [i] = value;
          return value;

        case Tcomplex_vector:
          i = check_row_major_index (index, xvector_dimension (array));
          xvector_contents (array) [i] = value;
          return value;

        case Tsimple_string:
          i = check_row_major_index (index, xstring_length (array));
          check_char (value);
          xstring_contents (array) [i] = xchar_code (value);
          return value;

        case Tcomplex_string:
          i = check_row_major_index (index, xstring_dimension (array));
          check_char (value);
          xstring_contents (array) [i] = xchar_code (value);
          return value;

        case Tarray:
          i = check_row_major_index (index, xarray_total_size (array));
          xgeneral_array_contents (array) [i] = value;
          return value;

        case Tstring_array:
          i = check_row_major_index (index, xarray_total_size (array));
          check_char (value);
          xstring_array_contents (array) [i] = xchar_code (value);
          return value;
        }
    }
  return FEtype_error (array, Qarray);
}

/*GENERIC_FUNCTION:ARRAY*/
lisp
Fadjustable_array_p (lisp array)
{
  if (pointerp (array))
    {
      switch (object_typeof (array))
        {
        case Tsimple_vector:
        case Tsimple_string:
          return Qnil;

        case Tcomplex_vector:
        case Tcomplex_string:
        case Tarray:
        case Tstring_array:
          return boole (xarray_adjustable (array));
        }
    }
  return FEtype_error (array, Qarray);
}

static void
check_displace_array (lisp array, lisp dest, int dest_size)
{
  for (lisp x = xarray_referenced_list (array); consp (x); x = xcdr (x))
    {
      lisp r = xcar (x);
      assert (xarray_displaced_to (r) == array);

      if (r == dest)
        FEprogram_error (Ecannot_replace_displace_array);

      if ((fixnum_value (Farray_total_size (r)) * array_element_size (r)
           + ((char *)xbase_vector_contents (r)
              - (char *)xbase_vector_contents (array))) > dest_size)
        FEprogram_error (Edisplaced_to_array_too_small);

      check_displace_array (r, dest, dest_size);
    }
}

static inline void
check_displace_array (lisp array, lisp dest)
{
  check_displace_array (array, dest,
                        (array_element_size (dest)
                         * fixnum_value (Farray_total_size (dest))));
}

static void
adjust_replace_array (lisp array1, lisp array2)
{
  if (xarray_displaced_to (array1) != Qnil
      && base_array_p (xarray_displaced_to (array1)))
    {
      lisp x;
      for (x = xarray_referenced_list (xarray_displaced_to (array1));
           consp (x); x = xcdr (x))
        if (xcar (x) == array2)
          {
            xcar (x) = array1;
            break;
          }
      assert (consp (x));
    }

  for (lisp x = xarray_referenced_list (array1); consp (x); x = xcdr (x))
    {
      assert (xarray_displaced_to (xcar (x)) == array2);
      xarray_displaced_to (xcar (x)) = array1;
    }
}

lisp
Fsi_replace_array (lisp array1, lisp array2)
{
  check_array (array1);
  check_array (array2);

  if (array1 == array2)
    FEprogram_error (Eis_same_array);

  if (Fadjustable_array_p (array1) == Qnil)
    FEprogram_error (Eis_not_adjustable_array);
  if (Fadjustable_array_p (array2) == Qnil)
    FEprogram_error (Eis_not_adjustable_array);

  if (array_element_type (array1) != array_element_type (array2))
    FEprogram_error (Earray_type_mismatch);

  if (xarray_rank (array1) != xarray_rank (array2))
    FEprogram_error (Earray_rank_mismatch);

  check_displace_array (array1, array2);
  check_displace_array (array2, array1);

  adjust_displace_array (array1, xbase_vector_contents (array2));
  adjust_displace_array (array2, xbase_vector_contents (array1));

  swap (xarray_total_size (array1), xarray_total_size (array2));
  swap (xbase_vector_contents (array1), xbase_vector_contents (array2));
  if (xarray_rank (array1) == 1)
    swap (xvector_dimension (array1), xvector_dimension (array2));
  else
    swap (xarray_dims (array1), xarray_dims (array2));
  swap (xarray_displaced_to (array1), xarray_displaced_to (array2));
  swap (xarray_referenced_list (array1), xarray_referenced_list (array2));
  swap (xarray_has_fillp (array1), xarray_has_fillp (array2));

  adjust_replace_array (array1, array2);
  adjust_replace_array (array2, array1);

  return Qt;
}
