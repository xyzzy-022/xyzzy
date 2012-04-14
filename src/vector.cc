#include "stdafx.h"
#include "ed.h"
#include "sequence.h"

lisp
alloc_vector (int size)
{
  lisp vector = make_simple_vector ();
  xvector_contents (vector) = (lisp *)xmalloc (sizeof (lisp) * size);
  xvector_length (vector) = size;
  return vector;
}

lisp
alloc_complex_vector (int fillp, int size, int adjustable)
{
  assert (fillp <= size);
  lisp vector = make_complex_vector ();
  xvector_contents (vector) = (lisp *)xmalloc (sizeof (lisp) * size);
  xvector_dimension (vector) = size;
  xvector_length (vector) = fillp >= 0 ? fillp : size;
  xarray_adjustable (vector) = adjustable;
  xarray_has_fillp (vector) = fillp >= 0;
  return vector;
}

lisp
copy_vector (lisp src)
{
  assert (general_vector_p (src));
  lisp vec = alloc_vector (xvector_length (src));
  bcopy (xvector_contents (src), xvector_contents (vec), xvector_length (src));
  return vec;
}

lisp
make_vector_from_list (lisp list, int length)
{
  assert (consp (list) || list == Qnil);
  lisp vector = alloc_vector (length);
  lisp *p = xvector_contents (vector);
  int i;
  for (i = 0; i < length && consp (list); i++, list = xcdr (list))
    *p++ = xcar (list);
  if (i < length)
    {
      lisp tail = i ? p[-1] : Qnil;
      for (; i < length; i++)
        *p++ = tail;
    }
  return vector;
}

lisp
subseq_vector (lisp vector, lisp lstart, lisp lend)
{
  check_general_vector (vector);
  int start, end;
  seq_start_end (xvector_length (vector), start, end, lstart, lend);
  lisp v = alloc_vector (end - start);
  bcopy (xvector_contents (vector) + start, xvector_contents (v), end - start);
  return v;
}

static lisp
fill_vector (lisp vector, int size, lisp init)
{
  bfill (xvector_contents (vector), size, init);
  return vector;
}

lisp
make_vector (int size, lisp init)
{
  return fill_vector (alloc_vector (size), size, init);
}

static lisp
make_complex_vector (int size, int fillp, lisp init, int adjustable)
{
  return fill_vector (alloc_complex_vector (fillp, size, adjustable),
                      size, init);
}

static int
check_fill_pointer (lisp fill_pointer, int dims)
{
  if (fill_pointer == Qnil)
    return -1;
  if (fill_pointer == Qt)
    return dims;
  int fillp = fixnum_value (fill_pointer);
  if (fillp < 0 || fillp > dims)
    FErange_error (fill_pointer);
  return fillp;
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fsi_make_vector (lisp dimension, lisp element_type, lisp initial_element,
                 lisp adjustable, lisp fill_pointer,
                 lisp displaced_to, lisp displaced_index_offset)
{
  int dims = fixnum_value (dimension);
  if (dims < 0 || dims >= MAX_VECTOR_LENGTH)
    FErange_error (dimension);

  check_array_type (element_type);

  if (adjustable == Qnil && fill_pointer == Qnil && displaced_to == Qnil)
    {
      if (element_type == Qcharacter)
        return make_string (initial_char_elem (initial_element), dims);
      else
        return make_vector (dims, initial_element);
    }
  else
    {
      int fillp = check_fill_pointer (fill_pointer, dims);
      if (displaced_to == Qnil)
        {
          if (element_type == Qcharacter)
            return make_complex_string (initial_char_elem (initial_element),
                                        fillp, dims, adjustable != Qnil);
          else
            return make_complex_vector (dims, fillp, initial_element,
                                        adjustable != Qnil);
        }
      else
        {
          lisp newvec;
          if (element_type == Qcharacter)
            newvec = make_complex_string ();
          else
            newvec = make_complex_vector ();
          xvector_dimension (newvec) = dims;
          xvector_length (newvec) = fillp >= 0 ? fillp : dims;
          xarray_adjustable (newvec) = adjustable != Qnil;
          xarray_has_fillp (newvec) = fillp >= 0;
          return displace_array (newvec, dims, element_type,
                                 displaced_to, displaced_index_offset);
        }
    }
}

/*GENERIC_FUNCTION:SIMPLE-VECTOR*/
lisp
Fsvref (lisp vector, lisp index)
{
  int i = fixnum_value (index);
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
        if (i < 0 || i >= xvector_length (vector))
          FErange_error (index);
        return xvector_contents (vector) [i];

      case Tsimple_string:
        if (i < 0 || i >= xstring_length (vector))
          FErange_error (index);
        return make_char (xstring_contents (vector) [i]);
      }
  return FEtype_error (vector, Qsimple_vector);
}

/*GENERIC_FUNCTION:SIMPLE-VECTOR*/
lisp
Fsi_svset (lisp vector, lisp index, lisp value)
{
  int i = fixnum_value (index);
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
        if (i < 0 || i >= xvector_length (vector))
          FErange_error (index);
        xvector_contents (vector) [i] = value;
        return value;

      case Tsimple_string:
        check_char (value);
        if (i < 0 || i >= xstring_length (vector))
          FErange_error (index);
        xstring_contents (vector) [i] = xchar_code (value);
        return value;
      }
  return FEtype_error (vector, Qsimple_vector);
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fvector_push (lisp element, lisp vector)
{
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        FEprogram_error (Evector_has_no_fill_pointer, vector);

      case Tcomplex_vector:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        if (xvector_length (vector) == xvector_dimension (vector))
          return Qnil;
        xvector_contents (vector) [xvector_length (vector)] = element;
        return make_fixnum (xvector_length (vector)++);

      case Tcomplex_string:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        check_char (element);
        if (xstring_length (vector) == xstring_dimension (vector))
          return Qnil;
        xstring_contents (vector) [xstring_length (vector)] = xchar_code (element);
        return make_fixnum (xstring_length (vector)++);
      }
  return FEtype_error (vector, Qvector);
}

static int
check_extension (lisp extension)
{
  if (!extension || extension == Qnil)
    return 64;

  int ext = fixnum_value (extension);
  if (ext <= 0 || ext >= MAX_VECTOR_LENGTH)
    FErange_error (extension);
  return ext;
}

int
realloc_element (lisp vector, int ext, int size)
{
  assert (ext > 0);
  int new_size = xvector_dimension (vector) + ext;
  lisp disp = xarray_displaced_to (vector);
  if (disp != Qnil)
    {
      int offset = ((char *)xbase_vector_contents (vector)
                    - (char *)xbase_vector_contents (disp)) / size;
      int total = fixnum_value (Farray_total_size (disp));
      if (offset + new_size > total)
        FEprogram_error (Edisplaced_to_array_too_small);
      xvector_dimension (vector) = new_size;
      return 0;
    }
  else
    {
      void *newvec = xmalloc (size * new_size);
      adjust_displace_array (vector, newvec);
      memcpy (newvec, xbase_vector_contents (vector),
              size * xvector_length (vector));
      xfree (xbase_vector_contents (vector));
      xbase_vector_contents (vector) = newvec;
      xvector_dimension (vector) += ext;
      return 1;
    }
}

static inline void
adjust_vector (lisp vector, lisp extension)
{
  int osize = xvector_dimension (vector);
  if (realloc_element (vector, check_extension (extension), sizeof (lisp)))
    bfill (xvector_contents (vector), osize, xvector_dimension (vector), Qnil);
}

static inline void
adjust_string (lisp string, lisp extension)
{
  realloc_element (string, check_extension (extension), sizeof (Char));
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fvector_push_extend (lisp element, lisp vector, lisp extension)
{
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        FEprogram_error (Evector_has_no_fill_pointer, vector);

      case Tcomplex_vector:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        if (xvector_length (vector) == xvector_dimension (vector))
          {
            if (!xarray_adjustable (vector))
              FEprogram_error (Evector_is_not_adjustable, vector);
            adjust_vector (vector, extension);
          }
        xvector_contents (vector) [xvector_length (vector)] = element;
        return make_fixnum (xvector_length (vector)++);

      case Tcomplex_string:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        check_char (element);
        if (xstring_length (vector) == xstring_dimension (vector))
          {
            if (!xarray_adjustable (vector))
              FEprogram_error (Evector_is_not_adjustable, vector);
            adjust_string (vector, extension);
          }
        xstring_contents (vector) [xstring_length (vector)] = xchar_code (element);
        return make_fixnum (xstring_length (vector)++);
      }
  return FEtype_error (vector, Qvector);
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fvector_pop (lisp vector)
{
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        FEprogram_error (Evector_has_no_fill_pointer, vector);

      case Tcomplex_vector:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        if (!xvector_length (vector))
          FEprogram_error (Efill_pointer_is_zero);
        return xvector_contents (vector) [--xvector_length (vector)];

      case Tcomplex_string:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        if (!xstring_length (vector))
          FEprogram_error (Efill_pointer_is_zero);
        return make_char (xstring_contents (vector) [--xstring_length (vector)]);
      }
  return FEtype_error (vector, Qvector);
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Ffill_pointer (lisp vector)
{
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        FEprogram_error (Evector_has_no_fill_pointer, vector);

      case Tcomplex_vector:
      case Tcomplex_string:
        if (!xarray_has_fillp (vector))
          FEprogram_error (Evector_has_no_fill_pointer, vector);
        return make_fixnum (xvector_length (vector));
      }
  return FEtype_error (vector, Qvector);
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fsi_set_fill_pointer (lisp vector, lisp fillp)
{
  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        FEprogram_error (Evector_has_no_fill_pointer, vector);

      case Tcomplex_vector:
      case Tcomplex_string:
        {
          if (!xarray_has_fillp (vector))
            FEprogram_error (Evector_has_no_fill_pointer, vector);
          int f = fixnum_value (fillp);
          if (f < 0 || f > xvector_dimension (vector))
            FErange_error (fillp);
          xvector_length (vector) = f;
          return fillp;
        }
      }
  return FEtype_error (vector, Qvector);
}

/*GENERIC_FUNCTION:VECTOR*/
lisp
Fsi_set_vector_length (lisp vector, lisp length)
{
  int l = fixnum_value (length);
  if (l < 0)
    FErange_error (length);

  if (pointerp (vector))
    switch (object_typeof (vector))
      {
      case Tsimple_vector:
      case Tsimple_string:
        if (l > xvector_length (vector))
          FErange_error (length);
        xvector_length (vector) = l;
        return Qt;

      case Tcomplex_vector:
      case Tcomplex_string:
        if (l > xvector_dimension (vector))
          FErange_error (length);
        xvector_length (vector) = l;
        return Qt;
      }
  return FEtype_error (vector, Qvector);
}
