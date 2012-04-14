#include "stdafx.h"
#include "ed.h"
#include "except.h"
#include "mman.h"

#define MAGIC 'CODX'

static void
snarf_doc (FILE *fp, lisp vec, lisp symlist)
{
  for (lisp *v = xvector_contents (vec), *ve = v + xvector_length (vec); v < ve; v++)
    for (lisp p = *v; consp (p); p = xcdr (p))
      {
        lisp symbol = xcar (p);
        for (lisp pl = xsymbol_plist (symbol), val; consp (pl); pl = xcdr (val))
          {
            val = xcdr (pl);
            if (!consp (val))
              break;
            if (stringp (xcar (val)))
              for (lisp sl = symlist; consp (sl); sl = xcdr (sl))
                if (xcar (sl) == xcar (pl))
                  {
                    long off = ftell (fp);
                    lisp doc = xcar (val);
                    fwrite (&off, sizeof off, 1, fp);
                    fwrite (&xstring_length (doc), sizeof xstring_length (doc), 1, fp);
                    fwrite (xstring_contents (doc), xstring_length (doc),
                            sizeof *xstring_contents (doc), fp);
                    xcar (val) = make_fixnum (off);
                    break;
                  }
          }
      }
}

lisp
Fsi_snarf_documentation (lisp lpath, lisp symlist)
{
  char path[MAX_PATH + 1];
  pathname2cstr (lpath, path);
  FILE *fp = fopen (path, "wb");
  if (!fp)
    FEsimple_crtl_error (errno, lpath);

  long magic = MAGIC;
  fwrite (&magic, sizeof magic, 1, fp);

  for (lisp pl = xsymbol_value (Vpackage_list); consp (pl); pl = xcdr (pl))
    {
      lisp package = xcar (pl);
      snarf_doc (fp, xpackage_external (package), symlist);
      snarf_doc (fp, xpackage_internal (package), symlist);
    }

  fclose (fp);
  return Qnil;
}

static lisp
apropos_doc (const Char *p, int l)
{
  const Char *p0, *pe;
  for (p0 = p, pe = p + l; p < pe && *p != '\n'; p++)
    ;
  return make_string (p0, p - p0);
}

lisp
Fsi_get_documentation_string (lisp symbol, lisp indicator, lisp apropos, lisp lpath)
{
  lisp doc = Fget (symbol, indicator, Qnil);
  if (stringp (doc))
    {
      if (apropos == Qnil)
        return doc;
      return apropos_doc (xstring_contents (doc), xstring_length (doc));
    }
  else
    {
      long pos;
      if (!safe_fixnum_value (doc, &pos) || pos <= 0)
        return Qnil;

      char path[MAX_PATH + 1];
      pathname2cstr (lpath, path);

      try
        {
          mapf mf;
          if (!mf.open (path, FILE_FLAG_RANDOM_ACCESS))
            file_error (GetLastError (), lpath);
          const char *base = (const char *)mf.base ();
          if (mf.size () < sizeof (long)
              || *(long *)base != MAGIC
              || mf.size () < pos + sizeof (long) * 2
              || *(long *)(base + pos) != pos)
            FEsimple_error (Einvalid_doc_file, lpath);
          base += pos + sizeof (long);
          long l = *(long *)base;
          if (l < 0 || pos + l * sizeof (Char) > mf.size ())
            FEsimple_error (Einvalid_doc_file, lpath);
          base += sizeof (long);
          if (apropos == Qnil)
            return make_string ((const Char *)base, l);
          return apropos_doc ((const Char *)base, l);
        }
      catch (Win32Exception &e)
        {
          if (e.code == EXCEPTION_IN_PAGE_ERROR)
            FEsimple_win32_error (ERROR_FILE_CORRUPT);
          if (e.code == EXCEPTION_ACCESS_VIOLATION)
            FEsimple_error (Einvalid_doc_file, lpath);
          throw e;
        }
    }
  return Qnil;
}
