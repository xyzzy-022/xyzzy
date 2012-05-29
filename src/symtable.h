#ifndef _symtable_h_
# define _symtable_h_

struct lfns
{
  const char *name;
  lfunction_proc fn;
  lisp *sym;
  lisp lfn;
  u_char size;
  u_char nargs: 4;
  u_char nopts: 4;
  u_char flags;
  u_char interactive;
#ifdef DEBUG_GC
  u_char called;
#endif
};

#ifdef DEBUG_GC
#define MARK_FUNCALL(F) (xfunction_tab (F)->called |= 1)
#endif

struct lvars
{
  const char *name;
  lisp *sym;
  u_short size;
  u_short flags;
};

struct lintr
{
  const char *s;
  lisp str;
};

extern lfns lsp_fns[];
extern lfns cl_fns[];
extern lfns sys_fns[];
extern lfns ed_fns[];
extern lvars lsp_vars[];
extern lvars cl_vars[];
extern lvars sys_vars[];
extern lvars ed_vars[];
extern lvars kwd_vars[];
extern lvars unint_vars[];
extern lintr intrs[];

#endif
