#ifndef _binfo_h_
#define _binfo_h_

#include "version.h"

class buffer_info
{
  const Window *const b_wp;
  const Buffer *const b_bufp;
  char **const b_posp;
  char **const b_percentp;
  int *const b_ime;
  static const char *const b_eol_name[];

  char *minor_mode (lisp, char *, char *, int &) const;
public:
  buffer_info (const Window *wp, const Buffer *bp, char **posp, int *ime, char **percentp)
       : b_wp (wp), b_bufp (bp), b_posp (posp), b_ime (ime), b_percentp(percentp) {}
  char *format (lisp, char *, char *) const;
  char *modified (char *, int) const;
  char *read_only (char *, int) const;
  char *progname (char *b, char *be) const
    {return stpncpy (b, ProgramName, be - b);}
  char *version (char *, char *, int) const;
  char *buffer_name (char *, char *) const;
  char *file_name (char *, char *, int) const;
  char *file_or_buffer_name (char *, char *, int) const;
  char *mode_name (char *, char *, int) const;
  char *encoding (char *b, char *be) const
    {return w2s (b, be, xchar_encoding_name (b_bufp->lchar_encoding));}
  char *eol_code (char *b, char *be) const
    {return stpncpy (b, b_eol_name[b_bufp->b_eol_code], be - b);}
  char *ime_mode (char *, char *) const;
  char *position (char *, char *) const;
  char *host_name (char *, char *, int) const;
  char *process_id (char *, char *) const;
  char *admin_user (char *, char *) const;
  char *percent(char *, char *) const;
};

#endif
