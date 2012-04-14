#include "stdafx.h"
#include "ed.h"
#include "except.h"
#include "mman.h"

#define MAGIC(a,b,c,d) (((d)<<24)+((c)<<16)+((b)<<8)+(a))

#define DIC_MAGIC MAGIC ('E', 'D', 'I', 'C')
#define IDX_MAGIC MAGIC ('E', 'D', 'I', 'X')
#define IDX_MAGIC_NOMEM MAGIC ('E', 'D', 'i', 'x')

struct dic_head
{
  long dh_magic;
  long dh_size;
};

struct idx_head
{
  long ih_magic;
  long ih_size;
  long ih_hash;
  long ih_offset[1];
};

struct idx_index
{
  long i_data;
  long i_offset;
};

#include <pshpack2.h>
struct dic_string
{
  u_short l;
  char data[1];
};
#include <poppack.h>

#define _P(O, OFF) ((const char *)(O) + (OFF))

static lisp
lookup_dictionary (const dic_head *dich, const idx_head *idxh,
                   lisp lword, lisp lidx)
{
  u_int h = ihashpjw (xstring_contents (lword),
                      xstring_length (lword)) % idxh->ih_hash;

  for (const idx_index *index = (const idx_index *)_P (idxh, idxh->ih_offset[h]);
       index->i_data; index++)
    {
      const dic_string *s = (const dic_string *)_P (dich, index->i_data);
      if (s->l == xstring_length (lword)
          && strequal (s->data, xstring_contents (lword), xstring_length (lword)))
        {
          lisp result = Qnil;
          for (const long *offset = (const long *)_P (idxh, index->i_offset);
               *offset; offset++)
            {
              s = (const dic_string *)_P (dich, *offset);
              result = xcons (make_string (s->data, s->l), result);
            }
          return result;
        }
    }
  return Qnil;
}

static lisp
lookup_dictionary_nomem (const dic_head *dich, const idx_head *idxh,
                         lisp lword, lisp lidx)
{
  u_int h = ihashpjw (xstring_contents (lword),
                      xstring_length (lword)) % idxh->ih_hash;
  lisp result = Qnil;
  for (const idx_index *index = (const idx_index *)_P (idxh, idxh->ih_offset[h]);
       index->i_data; index++)
    {
      const dic_string *s = (const dic_string *)_P (dich, index->i_data);
      if (s->l == xstring_length (lword)
          && strequal (s->data, xstring_contents (lword), xstring_length (lword)))
        {
          const dic_string *x = (const dic_string *)_P (dich, index->i_offset);
          result = xcons (make_string (x->data, x->l), result);
        }
    }
  return result;
}

lisp
Flookup_dictionary (lisp ldir, lisp ldic, lisp lidx, lisp lword)
{
  lword = Fstring (lword);
  check_string (ldic);
  check_string (lidx);
  if (xstring_length (ldic) >= PATH_MAX)
    FEprogram_error (Epath_name_too_long, ldic);
  if (xstring_length (lidx) >= PATH_MAX)
    FEprogram_error (Epath_name_too_long, lidx);

  char path[PATH_MAX * 2 + 1];
  char *pe = pathname2cstr (ldir, path);
  if (pe != path && pe[-1] != '/')
    *pe++ = '/';

  w2s (pe, ldic);
  mapf dic;
  if (!dic.open (path, FILE_FLAG_RANDOM_ACCESS))
    file_error (GetLastError (), ldic);

  const dic_head *dich = (const dic_head *)dic.base ();
  if (dic.size () < sizeof *dich || dich->dh_magic != DIC_MAGIC
      || dich->dh_size != long (dic.size ()))
    FEprogram_error (Einvalid_dictionary, ldic);

  w2s (pe, lidx);
  mapf idx;
  if (!idx.open (path, FILE_FLAG_RANDOM_ACCESS))
    file_error (GetLastError (), lidx);

  const idx_head *idxh = (const idx_head *)idx.base ();
  if (idx.size () < sizeof *idxh
      || (idxh->ih_magic != IDX_MAGIC && idxh->ih_magic != IDX_MAGIC_NOMEM)
      || idxh->ih_size != long (idx.size ()) || idxh->ih_hash <= 0)
    FEprogram_error (Einvalid_dictionary, lidx);

  lisp result = Qnil;
  try
    {
      if (idxh->ih_magic == IDX_MAGIC)
        result = lookup_dictionary (dich, idxh, lword, lidx);
      else
        result = lookup_dictionary_nomem (dich, idxh, lword, lidx);
    }
  catch (Win32Exception &e)
    {
      if (e.code == EXCEPTION_IN_PAGE_ERROR)
        FEsimple_win32_error (ERROR_FILE_CORRUPT);
      if (e.code == EXCEPTION_ACCESS_VIOLATION)
        FEprogram_error (Einvalid_dictionary, lidx);
      throw e;
    }
  return result;
}
