#include "ed.h"

lchunk *
make_chunk ()
{
  lchunk *p = ldata <lchunk, Tchunk>::lalloc ();
  p->type = Qnil;
  p->size = 0;
  p->data = 0;
  p->owner = Qnil;
  return p;
}

static void *
chunk_ptr (lisp chunk, lisp lsize)
{
  char *p0 = (char *)xchunk_data (chunk);
  char *pe = p0 + fixnum_value (lsize);
  if (pe < p0 || pe > p0 + xchunk_size (chunk)) // check overflow
    FErange_error (lsize);
  return p0;
}

static void *
chunk_ptr (lisp chunk, lisp loffset, lisp lsize)
{
  char *p0 = (char *)xchunk_data (chunk);
  char *p = p0 + unsigned_long_value (loffset);
  if (p < p0)  // avoid overflow
    FErange_error (loffset);
  char *pe = p + fixnum_value (lsize);
  if (pe < p || pe > p0 + xchunk_size (chunk))
    FErange_error (lsize);
  return p;
}

static void *
chunk_ptr (char *address, lisp lsize)
{
  char *ae = address + fixnum_value (lsize);
  if (ae < address)
    FErange_error (lsize);
  return address;
}

lisp
Fsi_make_chunk (lisp type, lisp lsize, lisp src_chunk, lisp loffset)
{
  int size = fixnum_value (lsize);
  if (size < 0)
    FErange_error (lsize);

  lisp chunk = make_chunk ();
  xchunk_type (chunk) = type;
  xchunk_size (chunk) = size;

  if (!src_chunk || src_chunk == Qnil)
    {
      if (!loffset || loffset == Qnil)
        {
          xchunk_data (chunk) = xmalloc (size);
          xchunk_owner (chunk) = chunk;
        }
      else
        {
          xchunk_data (chunk) =
            chunk_ptr ((char *)unsigned_long_value (loffset), lsize);
          xchunk_owner (chunk) = Qnil;
        }
    }
  else
    {
      check_chunk (src_chunk);
      xchunk_owner (chunk) = src_chunk;
      if (!loffset || loffset == Qnil)
        xchunk_data (chunk) = chunk_ptr (src_chunk, lsize);
      else
        xchunk_data (chunk) = chunk_ptr (src_chunk, loffset, lsize);
    }
  return chunk;
}

lisp
Fsi_make_string_chunk (lisp string)
{
  check_string (string);
  int l = w2sl (xstring_contents (string), xstring_length (string));
  lisp chunk = make_chunk ();
  xchunk_type (chunk) = Qnil;
  xchunk_size (chunk) = l + 1;
  char *b = (char *)xmalloc (l + 1);
  xchunk_data (chunk) = b;
  xchunk_owner (chunk) = chunk;
  w2s (b, xstring_contents (string), xstring_length (string));
  return chunk;
}

lisp
Fsi_chunk_data (lisp chunk)
{
  check_chunk (chunk);
  return make_fixnum (long (xchunk_data (chunk)));
}

lisp
Fsi_chunk_size (lisp chunk)
{
  check_chunk (chunk);
  return make_fixnum (xchunk_size (chunk));
}

lisp
Fsi_chunk_type (lisp chunk)
{
  check_chunk (chunk);
  return xchunk_type (chunk);
}

lisp
Fsi_chunk_owner (lisp chunk)
{
  check_chunk (chunk);
  return xchunk_owner (chunk);
}

lisp
Fsi_address_of (lisp object)
{
  return make_fixnum (long (object));
}

static char *
calc_chunk_ptr (lisp chunk, lisp loffset)
{
  check_chunk (chunk);
  char *p0 = (char *)xchunk_data (chunk);
  if (!loffset || loffset == Qnil)
    return p0;
  char *p = p0 + unsigned_long_value (loffset);
  if (p < p0 || p > p0 + xchunk_size (chunk))
    FErange_error (loffset);
  return p;
}

static lisp
fill_chunk (lisp chunk, int byte, lisp loffset, lisp lsize)
{
  char *p = calc_chunk_ptr (chunk, loffset);
  char *pe = (char *)xchunk_data (chunk) + xchunk_size (chunk);
  int size;
  if (!lsize || lsize == Qnil)
    size = pe - p;
  else
    {
      size = fixnum_value (lsize);
      if (size < 0 || p + size > pe)
        FErange_error (lsize);
    }
  memset (p, byte, size);
  return Qt;
}

lisp
Fsi_fill_chunk (lisp chunk, lisp lbyte, lisp loffset, lisp lsize)
{
  return fill_chunk (chunk, fixnum_value (lbyte), loffset, lsize);
}

lisp
Fsi_clear_chunk (lisp chunk, lisp loffset, lisp lsize)
{
  return fill_chunk (chunk, 0, loffset, lsize);
}

lisp
Fsi_copy_chunk (lisp fchunk, lisp tchunk, lisp lsize, lisp foffset, lisp toffset)
{
  char *f = calc_chunk_ptr (fchunk, foffset);
  char *t = calc_chunk_ptr (tchunk, toffset);
  char *fe = (char *)xchunk_data (fchunk) + xchunk_size (fchunk);
  char *te = (char *)xchunk_data (tchunk) + xchunk_size (tchunk);
  int size;
  if (!lsize || lsize == Qnil)
    size = min (fe - f, te - t);
  else
    {
      size = fixnum_value (lsize);
      if (size < 0 || f + size > fe || t + size > te)
        FErange_error (lsize);
    }

  memmove (t, f, size);
  return Qt;
}

static void *
chunk_ptr (lisp chunk, lisp loffset, int size)
{
  check_chunk (chunk);
  char *p0 = (char *)xchunk_data (chunk);
  char *p = p0 + unsigned_long_value (loffset);
  if (p < p0)  // avoid overflow
    FErange_error (loffset);
  char *pe = p + size;
  if (pe > p0 + xchunk_size (chunk))
    FErange_error (loffset);
  return p;
}

lisp
Fsi_unpack_int8 (lisp chunk, lisp offset)
{
  char *p = (char *)chunk_ptr (chunk, offset, sizeof *p);
  return make_fixnum (*p);
}

lisp
Fsi_unpack_uint8 (lisp chunk, lisp offset)
{
  u_char *p = (u_char *)chunk_ptr (chunk, offset, sizeof *p);
  return make_fixnum (*p);
}

lisp
Fsi_unpack_int16 (lisp chunk, lisp offset)
{
  short *p = (short *)chunk_ptr (chunk, offset, sizeof *p);
  return make_fixnum (*p);
}

lisp
Fsi_unpack_uint16 (lisp chunk, lisp offset)
{
  u_short *p = (u_short *)chunk_ptr (chunk, offset, sizeof *p);
  return make_fixnum (*p);
}

lisp
Fsi_unpack_int32 (lisp chunk, lisp offset)
{
  long *p = (long *)chunk_ptr (chunk, offset, sizeof *p);
  return make_fixnum (*p);
}

lisp
Fsi_unpack_uint32 (lisp chunk, lisp offset)
{
  u_long *p = (u_long *)chunk_ptr (chunk, offset, sizeof *p);
  return make_integer (long_to_large_int (*p));
}

lisp
Fsi_unpack_float (lisp chunk, lisp offset)
{
  float *p = (float *)chunk_ptr (chunk, offset, sizeof *p);
  return make_single_float (*p);
}

lisp
Fsi_unpack_double (lisp chunk, lisp offset)
{
  double *p = (double *)chunk_ptr (chunk, offset, sizeof *p);
  return make_double_float (*p);
}

// si:unpack-string chunk offset &optional size (zero_term t)
lisp
Fsi_unpack_string (lisp chunk, lisp loffset, lisp lsize, lisp lzero_term)
{
  check_chunk (chunk);
  char *p0 = (char *)xchunk_data (chunk);
  char *p = p0 + unsigned_long_value (loffset);
  if (p < p0 || p > p0 + xchunk_size (chunk))
    FErange_error (loffset);
  char *pe;
  if (!lsize || lsize == Qnil)
    pe = p0 + xchunk_size (chunk);
  else
    {
      pe = p + fixnum_value (lsize);
      if (pe < p || pe > p0 + xchunk_size (chunk))
        FErange_error (lsize);
    }
  int zero_term = !lzero_term || lzero_term != Qnil;
  size_t l = s2wl (p, pe, zero_term);
  lisp string = make_string (l);
  s2w (xstring_contents (string), p, pe, zero_term);
  return string;
}

long
cast_to_long (lisp object)
{
  if (pointerp (object))
    switch (object_typeof (object))
      {
      case Tchunk:
        return long (xchunk_data (object));

      case Tdll_module:
        return long (xdll_module_handle (object));

      case Tdll_function:
        return long (xdll_function_proc (object));

      case Tc_callable:
        return long (xc_callable_insn (object));
      }
  return coerce_to_long (object);
}

lisp
Fsi_pack_int8 (lisp chunk, lisp offset, lisp value)
{
  char *p = (char *)chunk_ptr (chunk, offset, sizeof *p);
  *p = char (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_uint8 (lisp chunk, lisp offset, lisp value)
{
  u_char *p = (u_char *)chunk_ptr (chunk, offset, sizeof *p);
  *p = u_char (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_int16 (lisp chunk, lisp offset, lisp value)
{
  short *p = (short *)chunk_ptr (chunk, offset, sizeof *p);
  *p = short (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_uint16 (lisp chunk, lisp offset, lisp value)
{
  u_short *p = (u_short *)chunk_ptr (chunk, offset, sizeof *p);
  *p = u_short (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_int32 (lisp chunk, lisp offset, lisp value)
{
  long *p = (long *)chunk_ptr (chunk, offset, sizeof *p);
  *p = long (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_uint32 (lisp chunk, lisp offset, lisp value)
{
  u_long *p = (u_long *)chunk_ptr (chunk, offset, sizeof *p);
  *p = u_long (cast_to_long (value));
  return value;
}

lisp
Fsi_pack_float (lisp chunk, lisp offset, lisp value)
{
  float *p = (float *)chunk_ptr (chunk, offset, sizeof *p);
  *p = coerce_to_single_float (value);
  return value;
}

lisp
Fsi_pack_double (lisp chunk, lisp offset, lisp value)
{
  double *p = (double *)chunk_ptr (chunk, offset, sizeof *p);
  *p = coerce_to_double_float (value);
  return value;
}

lisp
Fsi_pack_string (lisp chunk, lisp loffset, lisp value, lisp lsize)
{
  check_chunk (chunk);
  check_string (value);
  char *p0 = (char *)xchunk_data (chunk);
  char *p = p0 + unsigned_long_value (loffset);
  if (p < p0 || p > p0 + xchunk_size (chunk))
    FErange_error (loffset);
  char *pe;
  if (!lsize || lsize == Qnil)
    pe = p0 + xchunk_size (chunk);
  else
    {
      pe = p + fixnum_value (lsize);
      if (pe < p || pe > p0 + xchunk_size (chunk))
        FErange_error (lsize);
    }
  w2s (p, pe, xstring_contents (value), xstring_length (value));
  return value;
}
