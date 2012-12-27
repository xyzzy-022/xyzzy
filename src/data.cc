#include "stdafx.h"
#include "ed.h"
#include "lex.h"
#include "symtable.h"
#include "mainframe.h"

lisp Qnil;
lisp Qunbound;

protect_gc *protect_gc::gcl;
dyn_protect_gc *dyn_protect_gc::gcl;
lex_env *lex_env::le;
int suppress_gc::sg_suppress_p;
static nonlocal_data default_nonlocal_data;
nonlocal_data *nonlocal_jump::d = &default_nonlocal_data;

int
find_zero_bit (u_long *p, int size)
{
  for (int i = 0; i < size; i++)
    if (p[i] != u_long (-1))
      {
        i *= sizeof (u_long) * CHAR_BIT;
        for (int ie = i + sizeof (u_long) * CHAR_BIT; i < ie; i++)
          if (!bitisset (p, i))
            return i;
      }
  return -1;
}

ldataP::ldataP ()
     : ld_heap (LDATA_PAGE_SIZE), ld_rep (0), ld_freep (0)
{
}

ldata_rep *
ldataP::alloc (int type)
{
  ldata_rep *p = (ldata_rep *)ld_heap.alloc ();
  if (!p)
    FEstorage_error ();

  if (!ld_lower_bound)
    {
      ld_lower_bound = (char *)p;
      ld_upper_bound = (char *)p + LDATA_PAGE_SIZE;
    }
  else if ((char *)p < ld_lower_bound)
    ld_lower_bound = (char *)p;
  else if ((char *)p + LDATA_PAGE_SIZE > ld_upper_bound)
    ld_upper_bound = (char *)p + LDATA_PAGE_SIZE;

  p->dr_type = type;
  bzero (p->dr_used, sizeof p->dr_used);
  bzero (p->dr_gc, sizeof p->dr_gc);
  p->dr_prev = 0;
  if (ld_rep)
    ld_rep->dr_prev = p;
  p->dr_next = ld_rep;
  ld_rep = p;
  return p;
}

void
ldataP::free (ldata_rep *p)
{
  if (p->dr_prev)
    p->dr_prev->dr_next = p->dr_next;
  else
    ld_rep = p->dr_next;
  if (p->dr_next)
    p->dr_next->dr_prev = p->dr_prev;
  ld_heap.free (p);
}

inline void
ldataP::morecore (int type, int size)
{
  ldata_rep *p = alloc (type);
  char *d = p->dr_data;
  ld_freep = (ldata_free_rep *)d;
  for (char *de = d + size * (LDATASIZE_NOBJS (size) - 1); d < de; d += size)
    ((ldata_free_rep *)d)->lf_next = (ldata_free_rep *)(d + size);
  ((ldata_free_rep *)d)->lf_next = 0;
}

char *
ldataP::do_alloc (int type, int size)
{
  if (!ld_freep)
    morecore (type, size);
  char *r = (char *)ld_freep;
  ld_freep = ld_freep->lf_next;
  bitset (used_place (r), bit_index (r));
  ld_nwasted++;
  return r;
}

int
ldataP::count_reps ()
{
  int n = 0;
  for (ldata_rep *p = ld_rep; p; p = p->dr_next)
    n++;
  return n;
}

void
ldataP::get_reps (ldata_rep **b)
{
  for (ldata_rep *p = ld_rep; p; p = p->dr_next)
    *b++ = p;
}

void
ldataP::alloc_reps (ldata_rep **b, int n, int f)
{
  for (int i = 0; i < n; i++)
    alloc (f);
  for (ldata_rep *p = ld_rep; p; p = p->dr_next)
    *b++ = p;
}

int
ldataP::find (void *obj, int type, int size)
{
  if (((pointer_t (obj) & LDATA_PAGE_MASK)
       - offsetof (ldata_rep, dr_data)) % size)
    return 0;

  ldata_rep *r = (ldata_rep *)(pointer_t (obj) & ~LDATA_PAGE_MASK);
  if (r->dr_type != type)
    return 0;

  for (ldata_rep *p = ld_rep; p; p = p->dr_next)
    if (p == r)
      return 1;
  return 0;
}

static int
find_object (lisp obj)
{
#define DECLARE_LDATA(a, b) if (ldata <a, b>::find ((char *)obj)) return 1;
#include "dataP.h"
  return 0;
}

void
dummy_for_instance ()
{
#define DECLARE_LDATA(a, b) ldata <a, b>::lalloc ();
#include "dataP.h"
}

template <class T, u_int F>
inline int
ldata <T, F>::count_reps ()
{
  return l_ld.count_reps ();
}

template <class T, u_int F>
inline void
ldata <T, F>::alloc_reps (ldata_rep **p, int n)
{
  l_ld.alloc_reps (p, n, F);
}

template <class T, u_int F>
inline void
ldata <T, F>::get_reps (ldata_rep **b)
{
  l_ld.get_reps (b);
}

void
ldataP::link_unused (int size)
{
  ld_freep = 0;
  for (ldata_rep *lp = ld_rep; lp; lp = lp->dr_next)
    for (char *d = lp->dr_data, *de = d + size * LDATASIZE_NOBJS (size);
         d < de; d += size)
      if (!bitisset (lp->dr_used, bit_index (d)))
        {
          ((ldata_free_rep *)d)->lf_next = ld_freep;
          ld_freep = (ldata_free_rep *)d;
        }
}

template <class T, u_int F>
inline void
ldata <T, F>::link_unused ()
{
  l_ld.link_unused (sizeof (T));
}

void
ldataP::free_all_reps ()
{
  for (ldata_rep *p = ld_rep, *next; p; p = next)
    {
      next = p->dr_next;
      ld_heap.free (p);
    }
  ld_rep = 0;
}

template <class T, u_int F>
inline void
ldata <T, F>::free_all_reps ()
{
  l_ld.free_all_reps ();
}

#if 0
template <class T>
static void
delete_lisp_object (T *obj)
{
  delete obj;
}

static void
cleanup_object (ldata_rep *lp, u_int size, void (*delete_fn)(void *))
{
  for (; lp; lp = lp->dr_next)
    for (char *d = lp->dr_data, *de = d + size * LDATASIZE_NOBJS (size);
         d < de; d += size)
      if (bitisset (lp->dr_used, bit_index (d)))
        (*delete_fn)(d);
}

template <class T, u_int F>
void
ldata <T, F>::cleanup ()
{
  cleanup_object (l_ld.ld_rep, sizeof (T),
                  (void (*)(void *))(void (*)(T *))delete_lisp_object);
}

static void
sweep_object (ldataP &ld, u_int size, void (*delete_fn)(void *), int &xnuses, int &xnfrees)
{
  int nuses = 0, nfrees = 0;

  ld.ld_freep = 0;
  for (ldata_rep *lp = ld.ld_rep, *next; lp; lp = next)
    {
      next = lp->dr_next;
      char *d = lp->dr_data;
      char *de = d + size * LDATASIZE_NOBJS (size);

      for (int i = 0; i < numberof (lp->dr_gc); i++)
        if (lp->dr_gc[i])
          goto doit;

      for (; d < de; d += size)
        if (bitisset (lp->dr_used, bit_index (d)))
          (*delete_fn)(d);

#ifdef DEBUG_GC
      memset (lp->dr_data, 0, de - lp->dr_data);
#endif
      ld.free (lp);
      continue;

    doit:
      for (; d < de; d += size)
        {
          int index = bit_index (d);
          if (bitisset (lp->dr_gc, index))
            {
              assert (bitisset (lp->dr_used, index));
              nuses++;
              bitclr (lp->dr_gc, index);
            }
          else
            {
              if (bitisset (lp->dr_used, index))
                {
                  (*delete_fn)(d);
                  bitclr (lp->dr_used, index);
#ifdef DEBUG_GC
                  memset (d, 0, size);
#endif
                }
              ((ldata_free_rep *)d)->lf_next = ld.ld_freep;
              ld.ld_freep = (ldata_free_rep *)d;
              nfrees++;
            }
        }
    }

  xnuses = nuses;
  xnfrees = nfrees;
}

template <class T, u_int F>
void
ldata <T, F>::sweep ()
{
  sweep_object (l_ld, sizeof (T),
                (void (*)(void *))(void (*)(T *))delete_lisp_object,
                l_nuses, l_nfrees);
}

#else
template <class T, u_int F>
void
ldata <T, F>::sweep ()
{
  l_nuses = 0;
  l_nfrees = 0;

  l_ld.ld_freep = 0;

  for (ldata_rep *lp = l_ld.ld_rep, *next; lp; lp = next)
    {
      next = lp->dr_next;
      T *d = (T *)lp->dr_data;
      T *de = d + LDATA_NOBJS (T);

      for (int i = 0; i < numberof (lp->dr_gc); i++)
        if (lp->dr_gc[i])
          goto doit;

      for (; d < de; d++)
        if (bitisset (lp->dr_used, bit_index (d)))
          delete d;

      l_ld.free (lp);
      continue;

    doit:
      for (; d < de; d++)
        {
          int index = bit_index (d);
          if (bitisset (lp->dr_gc, index))
            {
              assert (bitisset (lp->dr_used, index));
              l_nuses++;
              bitclr (lp->dr_gc, index);
            }
          else
            {
              if (bitisset (lp->dr_used, index))
                {
                  delete d;
                  bitclr (lp->dr_used, index);
                }
              ((ldata_free_rep *)d)->lf_next = l_ld.ld_freep;
              l_ld.ld_freep = (ldata_free_rep *)d;
              l_nfrees++;
            }
        }
    }
}

template <class T, u_int F>
void
ldata <T, F>::cleanup ()
{
  for (ldata_rep *lp = l_ld.ld_rep; lp; lp = lp->dr_next)
    for (T *d = (T *)lp->dr_data, *de = d + LDATA_NOBJS (T); d < de; d++)
      if (bitisset (lp->dr_used, bit_index (d)))
        delete d;
}
#endif

/*GENERIC_FUNCTION*/
void
cleanup_lisp_objects ()
{
#ifndef DEBUG
  ldata <lstream, Tstream>::cleanup ();
  ldata <lwin32_menu, Twin32_menu>::cleanup ();
  ldata <lwin32_dde_handle, Twin32_dde_handle>::cleanup ();
  ldata <loledata, Toledata>::cleanup ();
  ldata <lwait_object, Twait_object>::cleanup ();
#else
# define DECLARE_LDATA(a, b) ldata <a, b>::cleanup ();
# include "dataP.h"
#endif
}

template <class T, u_int F>
inline lisp
ldata <T, F>::countof ()
{
  return xcons (make_fixnum (l_nuses), make_fixnum (l_nfrees));
}

template <class T, u_int F>
inline void
ldata <T, F>::unuse (T *object)
{
  u_long *used = used_place (object);
  int index = bit_index (object);
  assert (bitisset (used, index));
  delete object;
  bitclr (used, index);
  ((ldata_free_rep *)object)->lf_next = l_ld.ld_freep;
  l_ld.ld_freep = (ldata_free_rep *)object;
}

static void
mark_toplev_list (lisp p)
{
  for (; consp (p); p = xcdr (p))
    bitset (gc_place (p), bit_index (p));
}

/*GENERIC_FUNCTION*/
static void
gc_mark_object (lisp object)
{
  while (1)
    {
      assert (object);
      if (!object || immediatep (object))
        return;

      u_long *dr_gc = gc_place (object);
      int index = bit_index (object);
      if (bitisset (dr_gc, index))
        return;
      bitset (dr_gc, index);

      switch (object_typeof (object))
        {
        case Tcons:
          gc_mark_object (xcar (object));
          object = xcdr (object);
          break;

        case Tsymbol:
          gc_mark_object (xsymbol_function (object));
          gc_mark_object (xsymbol_plist (object));
          gc_mark_object (xsymbol_package (object));
          gc_mark_object (xsymbol_name (object));
          object = xsymbol_value (object);
          break;

        case Tlong_int:
        case Tsingle_float:
        case Tdouble_float:
        case Tbignum:
          return;

        case Tregexp:
          object = xregexp_source (object);
          break;

        case Tfraction:
          gc_mark_object (xfract_num (object));
          object = xfract_den (object);
          break;

        case Tcomplex:
          gc_mark_object (xcomplex_real (object));
          object = xcomplex_imag (object);
          break;

        case Tsimple_string:
          return;

        case Tsimple_vector:
          {
            lisp *p = xvector_contents (object);
            lisp *pe = p + xvector_length (object);
            for (; p < pe; p++)
              gc_mark_object (*p);
            return;
          }

        case Tcomplex_vector:
          {
            lisp *p = xvector_contents (object);
            lisp *pe = p + xvector_dimension (object);
            for (; p < pe; p++)
              gc_mark_object (*p);
            mark_toplev_list (xarray_referenced_list (object));
            object = xarray_displaced_to (object);
            break;
          }

        case Tarray:
          {
            lisp *p = xgeneral_array_contents (object);
            lisp *pe = p + xarray_total_size (object);
            for (; p < pe; p++)
              gc_mark_object (*p);
            mark_toplev_list (xarray_referenced_list (object));
            object = xarray_displaced_to (object);
            break;
          }

        case Tcomplex_string:
        case Tstring_array:
          mark_toplev_list (xarray_referenced_list (object));
          object = xarray_displaced_to (object);
          break;

        case Tfunction:
          object = xfunction_name (object);
          break;

        case Tclosure:
          gc_mark_object (xclosure_vars (object));
          gc_mark_object (xclosure_fns (object));
          gc_mark_object (xclosure_frame (object));
          gc_mark_object (xclosure_name (object));
          object = xclosure_body (object);
          break;

        case Tstream:
          switch (xstream_type (object))
            {
            case st_file_input:
            case st_file_output:
            case st_file_io:
              object = xfile_stream_pathname (object);
              break;

            case st_string_input:
            case st_string_output:
              gc_mark_object (xstring_stream_input (object));
              object = xstring_stream_output (object);
              break;

            case st_synonym:
            case st_broadcast:
            case st_concatenated:
            case st_two_way:
            case st_echo:
              gc_mark_object (xcomposite_stream_input (object));
              object = xcomposite_stream_output (object);
              break;

            case st_status:
            case st_keyboard:
            case st_wstream:
            case st_socket:
            case st_debug_output:
              return;

            case st_buffer:
              gc_mark_object (xbuffer_stream_eob (object));
              object = xbuffer_stream_marker (object);
              break;

            case st_general_input:
              gc_mark_object (xgeneral_input_stream_listen_callback (object));
              gc_mark_object (xgeneral_input_stream_string (object));
              goto general_stream;

            case st_general_output:
              gc_mark_object (xgeneral_output_stream_flush_callback (object));
              goto general_stream;

            general_stream:
              gc_mark_object (xgeneral_stream_io_callback (object));
              object = xgeneral_stream_close_callback (object);
              break;

            default:
              assert (0);
              return;
            }
          break;

        case Tpackage:
          gc_mark_object (xpackage_name (object));
          gc_mark_object (xpackage_nicknames (object));
          gc_mark_object (xpackage_use_list (object));
          gc_mark_object (xpackage_used_by_list (object));
          gc_mark_object (xpackage_shadowings (object));
          gc_mark_object (xpackage_external (object));
          gc_mark_object (xpackage_documentation (object));
          object = xpackage_internal (object);
          break;

        case Trandom_state:
        case Twindow:
        case Tbuffer:
        case Tsyntax_table:
        case Tmarker:
        case Terror:
        case Twin32_dde_handle:
        case Twait_object:
          return;

        case Toledata:
          if (xoledata_name (object))
            gc_mark_object (xoledata_name (object));
          if (!xoledata_event (object))
            return;
          object = xoledata_event (object)->handlers ();
          break;

        case Tprocess:
          gc_mark_object (xprocess_buffer (object));
          gc_mark_object (xprocess_command (object));
          gc_mark_object (xprocess_incode (object));
          object = xprocess_outcode (object);
          break;

        case Tchar_encoding:
          gc_mark_object (xchar_encoding_name (object));
          object = xchar_encoding_display_name (object);
          break;

        case Thash_table:
          {
            hash_entry *e = xhash_table_entry (object);
            hash_entry *ee = e + xhash_table_size (object);
            for (; e < ee; e++)
              {
                gc_mark_object (e->key);
                gc_mark_object (e->value);
              }
            object = xhash_table_rehash_size (object);
            break;
          }

        case Tstruct_def:
          {
            gc_mark_object (xstrdef_name (object));
            gc_mark_object (xstrdef_type (object));
            gc_mark_object (xstrdef_includes (object));
            gc_mark_object (xstrdef_constructors (object));
            gc_mark_object (xstrdef_print_function (object));
            gc_mark_object (xstrdef_report (object));
            for (struct_slotdesc *s = xstrdef_slotdesc (object),
                 *se = s + xstrdef_nslots (object);
                 s < se; s++)
              {
                gc_mark_object (s->name);
                gc_mark_object (s->default_init);
                gc_mark_object (s->type);
                gc_mark_object (s->read_only);
                gc_mark_object (s->offset);
              }
            return;
          }

        case Tstruct_data:
          {
            gc_mark_object (xstrdata_def (object));
            for (lisp *d = xstrdata_data (object),
                 *de = d + xstrdata_nslots (object);
                 d < de; d++)
              gc_mark_object (*d);
            return;
          }

        case Treadtable:
          {
            for (readtab_rep *r = xreadtable_rep (object),
                 *re = r + READTABLE_REP_SIZE;
                 r < re; r++)
              {
                gc_mark_object (r->lfunc);
                if (r->disp)
                  for (disptab_rep *d = r->disp, *de = d + READTABLE_REP_SIZE;
                       d < de; d++)
                    gc_mark_object (d->lfunc);
              }
            return;
          }

        case Twin32_menu:
          gc_mark_object (xwin32_menu_init (object));
          gc_mark_object (xwin32_menu_tag (object));
          object = xwin32_menu_command (object);
          break;

        case Tchunk:
          gc_mark_object (xchunk_type (object));
          object = xchunk_owner (object);
          break;

        case Tdll_module:
          object = xdll_module_name (object);
          break;

        case Tdll_function:
          gc_mark_object (xdll_function_module (object));
          object = xdll_function_name (object);
          break;

        case Tc_callable:
          object = xc_callable_function (object);
          break;

        case Tenvironment:
          gc_mark_object (xenvironment_var (object));
          gc_mark_object (xenvironment_frame (object));
          object = xenvironment_fns (object);
          break;

        default:
          assert (0);
          return;
        }
    }
}

static inline void
gc_mark (lfns *p)
{
  for (; p->name; p++)
    {
      gc_mark_object (*p->sym);
      gc_mark_object (p->lfn);
    }
}

static inline void
gc_mark (lvars *p)
{
  for (; p->name; p++)
    gc_mark_object (*p->sym);
}

static inline void
gc_mark (lintr *p)
{
  for (; p->s; p++)
    gc_mark_object (p->str);
}

static lisp
gc_mark_list (lisp list)
{
  lisp ol, nl, cdr;
  for (ol = list, nl = Qnil; consp (ol); ol = cdr)
    {
      cdr = xcdr (ol);
      lisp x = xcar (ol);
      if (bitisset (gc_place (x), bit_index (x)))
        {
          bitset (gc_place (ol), bit_index (ol));
          xcdr (ol) = nl;
          nl = ol;
        }
    }
  return nl;
}

#ifdef DEBUG_GC
static void
shift_funcall_mark (lfns *p)
{
  for (; p->name; p++)
    p->called = (p->called & 0xc0) | ((p->called << 1) & 0x7f);
}

static void
mark_stack_trace ()
{
  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    if (p->type != stack_trace::empty)
      {
        lisp fn = p->fn;
        if (symbolp (fn))
          fn = xsymbol_function (fn);
        if (functionp (fn))
          xfunction_tab (fn)->called |= 0x80;
      }
}
#endif

void
gc_mark_in_stack ()
{
  jmp_buf regs;
  setjmp (regs);

  int tem = 0;
  lisp *beg = (lisp *)&tem, *end = (lisp *)app.initial_stack;
  for (; beg < end; beg++)
    {
      lisp p = *beg;
      if (!pointerp (p)
          || (char *)p < ldataP::ld_lower_bound
          || (char *)p >= ldataP::ld_upper_bound
          || (pointer_t (p) & LDATA_PAGE_MASK) < offsetof (ldata_rep, dr_data))
        continue;

      ldata_rep *r = (ldata_rep *)(pointer_t (p) & ~LDATA_PAGE_MASK);
      if (IsBadWritePtr (r, LDATA_PAGE_SIZE))
        continue;

      int index = bit_index (p);
      if (bitisset (r->dr_used, index) && !bitisset (r->dr_gc, index)
          && find_object (p))
        gc_mark_object (p);
    }
}

void
gc_mark_object ()
{
  gc_mark_object (Qnil);
  gc_mark_object (Qunbound);

  gc_mark (lsp_fns);
  gc_mark (cl_fns);
  gc_mark (sys_fns);
  gc_mark (ed_fns);
  gc_mark (lsp_vars);
  gc_mark (cl_vars);
  gc_mark (sys_vars);
  gc_mark (kwd_vars);
  lisp olist = xsymbol_value (Vdll_module_list); // module‚Í‚ ‚Æ‚Å‚â‚é
  xsymbol_value (Vdll_module_list) = Qnil;
  gc_mark (unint_vars);
  xsymbol_value (Vdll_module_list) = olist;
  gc_mark (ed_vars);
  gc_mark (intrs);

  nonlocal_data *d = nonlocal_jump::data ();
  gc_mark_object (d->type);
  gc_mark_object (d->value);
  gc_mark_object (d->tag);
  gc_mark_object (d->id);

  {
    for (protect_gc *gcp = protect_gc::gcl; gcp; gcp = gcp->last)
      for (lisp *p = gcp->var, *pe = p + gcp->nvars; p < pe; p++)
        gc_mark_object (*p);
  }
  {
    for (dyn_protect_gc *gcp = dyn_protect_gc::gcl; gcp; gcp = gcp->next)
      for (lisp *p = gcp->var, *pe = p + gcp->nvars; p < pe; p++)
        gc_mark_object (*p);
  }
  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    if (p->type != stack_trace::empty)
      {
        gc_mark_object (p->fn);
        if (p->args[0])
          gc_mark_object (p->args[0]);
        if (p->args[1])
          gc_mark_object (p->args[1]);
      }

  for (lex_env *lp = lex_env::le; lp; lp = lp->last)
    {
      gc_mark_object (lp->lex_var);
      gc_mark_object (lp->lex_fns);
      gc_mark_object (lp->lex_frame);
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    gc_mark_object (wp->lwp);
  for (Window *wp = app.active_frame.reserved; wp; wp = wp->w_next)
    gc_mark_object (wp->lwp);
  for (Window *wp = app.active_frame.deleted; wp; wp = wp->w_next)
    gc_mark_object (wp->lwp);

  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    {
      for (lisp *x = &bp->Buffer_gc_start; x <= &bp->Buffer_gc_end; x++)
        gc_mark_object (*x);
      for (textprop *t = bp->b_textprop; t; t = t->t_next)
        gc_mark_object (t->t_tag);
    }

  toplev_gc_mark (gc_mark_object);
  process_gc_mark (gc_mark_object);
  g_frame.gc_mark (gc_mark_object);
  app.user_timer.gc_mark (gc_mark_object);

  gc_mark_in_stack ();

  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    bp->lmarkers = gc_mark_list (bp->lmarkers);

  xsymbol_value (Vdll_module_list) =
    gc_mark_list (xsymbol_value (Vdll_module_list));
}

void
gc (int nomsg)
{
  if (suppress_gc::gc_suppressed_p ())
    return;

  app.in_gc = 1;

  if (nomsg < 0)
    nomsg = xsymbol_value (Vgarbage_collection_messages) == Qnil;

  int msglen = 0;
  if (!nomsg)
    msglen = app.status_window.text (get_message_string (Mgarbage_collecting));

  ldataP::ld_nwasted = 0;
  gc_mark_object ();

#define DECLARE_LDATA(a, b) ldata <a, b>::sweep ();
#include "dataP.h"

  bignum_allocated_bytes = 0;

#ifdef DEBUG_GC
  shift_funcall_mark (lsp_fns);
  shift_funcall_mark (cl_fns);
  shift_funcall_mark (sys_fns);
  shift_funcall_mark (ed_fns);
  mark_stack_trace ();
#endif

  if (!nomsg)
    {
      if (msglen)
        app.status_window.restore ();
      else
        app.status_window.text (get_message_string (Mgarbage_collecting_done));
    }

  _heapmin ();
  app.in_gc = 0;
}

lisp
Fgc (lisp nomsg)
{
  gc (nomsg && nomsg != Qnil);
#if 0
  int i = 1;
#define DECLARE_LDATA(a, b) \
  multiple_value::value (i++) = ldata <a, b>::countof ();
#include "dataP.h"
  multiple_value::value (0) = Qnil;
  multiple_value::count () = i;
#endif
  return Qnil;
}

lisp
interactive_string (lisp p)
{
  int n = xfunction_interactive (p);
  if (!n)
    return 0;
  return intrs[n - 1].str;
}

void
destruct_string (lisp string)
{
  assert (stringp (string));
  if (simple_string_p (string))
    ldata <lsimple_string, Tsimple_string>::unuse ((lsimple_string *)string);
}

void
destruct_regexp (lisp regexp)
{
  assert (regexpp (regexp));
  ldata <lregexp, Tregexp>::unuse ((lregexp *)regexp);
}

int ldataP::ld_nwasted;
char *ldataP::ld_upper_bound;
char *ldataP::ld_lower_bound;

#define DECLARE_LDATA(a, b) \
  ldataP ldata <a, b>::l_ld; \
  int ldata <a, b>::l_nuses; \
  int ldata <a, b>::l_nfrees;
#include "dataP.h"

static void
init_syms (lvars *v, lfns *f, lisp pkg, int self_bind)
{
  lisp *vec = xvector_contents (xpackage_external (pkg));
  int hashsize = xvector_length (xpackage_external (pkg));
  for (; v->name; v++)
    {
      lsymbol *symbol = make_symbol (make_string_simple (v->name, v->size),
                                     v->flags);
      *v->sym = symbol;
      u_int hash = hashpjw (xsymbol_name (symbol), hashsize);
      vec[hash] = xcons (symbol, vec[hash]);
      if (lambda_key_p (symbol) || self_bind)
        symbol->value = symbol;
      else if (specialp (symbol))
        symbol->value = Qnil;
      symbol->package = pkg;
    }

  if (f)
    for (; f->name; f++)
      {
        lsymbol *symbol = make_symbol (make_string_simple (f->name, f->size));
        *f->sym = symbol;
        u_int hash = hashpjw (xsymbol_name (symbol), hashsize);
        vec[hash] = xcons (symbol, vec[hash]);
        f->lfn = make_function (f->fn, symbol->name, f->flags,
                                f->nargs, f->nopts, f->interactive);
#ifdef DEBUG_GC
        xfunction_tab (f->lfn) = f;
#endif
        symbol->fn = f->lfn;
        symbol->package = pkg;
      }
}

static void
init_default_nonlocal_data ()
{
  default_nonlocal_data.type = Qnil;
  default_nonlocal_data.value = Qnil;
  default_nonlocal_data.tag = Qnil;
  default_nonlocal_data.id = Qnil;
}

#define SIMPLE_STRING(NAME) make_string_simple (NAME, sizeof NAME - 1)

#define LISP_INTSIZE 101
#define LISP_EXTSIZE 331
#define CL_INTSIZE 101
#define CL_EXTSIZE 331
#define SYS_INTSIZE 101
#define SYS_EXTSIZE 101
#define KWD_INTSIZE 11
#define KWD_EXTSIZE 331
#define USR_INTSIZE 331
#define USR_EXTSIZE 211
#define CL_USR_INTSIZE 331
#define CL_USR_EXTSIZE 211
#define ED_INTSIZE 211
#define ED_EXTSIZE 331

void
init_syms ()
{
  Qnil = make_symbol (SIMPLE_STRING ("nil"), SFconstant);
  Qunbound = make_symbol (SIMPLE_STRING ("unbound"));

  xsymbol_function (Qnil) = Qunbound;
  xsymbol_value (Qnil) = Qnil;
  xsymbol_plist (Qnil) = Qnil;
  xsymbol_package (Qnil) = Qnil;

  xsymbol_function (Qunbound) = Qunbound;
  xsymbol_value (Qunbound) = Qunbound;

  for (lintr *li = intrs; li->s; li++)
    if (*li->s)
      li->str = make_string (li->s);
    else
      li->str = Qnil;

  lisp lsp = make_package (SIMPLE_STRING ("lisp"), Qnil,
                           LISP_INTSIZE, LISP_EXTSIZE);
  lisp cl = make_package (SIMPLE_STRING ("common-lisp"), Qnil,
                           CL_INTSIZE, CL_EXTSIZE);
  lisp sys = make_package (SIMPLE_STRING ("system"),
                           make_list (SIMPLE_STRING ("si"),
                                      SIMPLE_STRING ("sys"),
                                      0),
                           SYS_INTSIZE, SYS_EXTSIZE);
  lisp kwd = make_package (SIMPLE_STRING ("keyword"), Qnil,
                           KWD_INTSIZE, KWD_EXTSIZE);
  lisp usr = make_package (SIMPLE_STRING ("user"), Qnil,
                           USR_INTSIZE, USR_EXTSIZE);
  lisp cl_usr = make_package (SIMPLE_STRING ("common-lisp-user"),
                              make_list (SIMPLE_STRING ("cl-user"), 0),
                              CL_USR_INTSIZE, CL_USR_EXTSIZE);
  lisp ed = make_package (SIMPLE_STRING ("editor"),
                          xcons (SIMPLE_STRING ("ed"), Qnil),
                          ED_INTSIZE, ED_EXTSIZE);

  xsymbol_package (Qnil) = lsp;

  xpackage_use_list (sys) = xcons (lsp, Qnil);
  xpackage_use_list (ed) = xcons (lsp, Qnil);
  xpackage_use_list (usr) = make_list (lsp, ed, 0);
  xpackage_use_list (cl) = make_list (lsp, 0);
  xpackage_use_list (cl_usr) = make_list (cl, ed, 0);
  xpackage_used_by_list (lsp) = make_list (sys, ed, usr, cl, 0);
  xpackage_used_by_list (cl) = make_list (cl_usr, 0);
  xpackage_used_by_list (ed) = make_list (cl_usr, usr, 0);

  u_int hash = hashpjw (xsymbol_name (Qnil), LISP_EXTSIZE);
  lisp *vec = xvector_contents (xpackage_external (lsp));
  vec[hash] = xcons (Qnil, vec[hash]);

  init_syms (lsp_vars, lsp_fns, lsp, 0);
  init_syms (cl_vars, cl_fns, cl, 0);
  init_syms (sys_vars, sys_fns, sys, 0);
  init_syms (kwd_vars, 0, kwd, 1);
  init_syms (ed_vars, ed_fns, ed, 0);

  lisp name = make_string_simple ("", 0);
  for (lvars *v = unint_vars; v->name; v++)
    {
      lsymbol *symbol = make_symbol (name, v->flags);
      *v->sym = symbol;
    }

  xsymbol_value (Vlisp_package) = lsp;
  xsymbol_value (Vcommon_lisp_package) = cl;
  xsymbol_value (Vsystem_package) = sys;
  xsymbol_value (Vkeyword_package) = kwd;
  xsymbol_value (Vuser_package) = usr;
  xsymbol_value (Vcommon_lisp_user_package) = cl_usr;
  xsymbol_value (Veditor_package) = ed;
  xsymbol_value (Vpackage_list) = make_list (lsp, sys, kwd, usr, ed, cl, cl_usr, 0);
  xsymbol_value (Vbuiltin_package_list) = Fcopy_list (xsymbol_value (Vpackage_list));
  xsymbol_value (Vpackage) = usr;

  multiple_value::value (0) = Qnil;
  multiple_value::count () = 1;

  init_default_nonlocal_data ();
}

template <class T, u_int F>
class ldata_iter
{
  T *i_d, *i_de;
  ldata_rep **i_rep;
#ifdef DEBUG
  ldata_rep **i_rep0;
#endif
public:
  ldata_iter (ldata_rep **, int);
  T *next ();
};

template <class T, u_int F>
ldata_iter <T, F>::ldata_iter (ldata_rep **r, int n)
{
#ifdef DEBUG
  i_rep0 = r;
#endif
  i_rep = r + n;
  i_d = i_de = 0;
}

template <class T, u_int F>
T *
ldata_iter <T, F>::next ()
{
  if (i_d == i_de)
    {
      assert (i_rep > i_rep0);
      i_rep--;
      i_d = (T *)(*i_rep)->dr_data;
      i_de = i_d + LDATA_NOBJS (T);
    }
  assert (bitisset ((*i_rep)->dr_used, bit_index (i_d)));
  return i_d++;
}

static void
combine_syms (lvars *v, lfns *f,
              ldata_iter <lsymbol, Tsymbol> &syms,
              ldata_iter <lfunction, Tfunction> &fns)
{
  for (; v->name; v++)
    *v->sym = syms.next ();

  if (f)
    for (; f->name; f++)
      {
        *f->sym = syms.next ();
        f->lfn = fns.next ();
        xfunction_fn (f->lfn) = f->fn;
#ifdef DEBUG_GC
        xfunction_tab (f->lfn) = f;
#endif
      }
}

void
combine_syms ()
{
  int n = ldata <lsymbol, Tsymbol>::count_reps ();
  ldata_rep **r = (ldata_rep **)alloca (sizeof *r * n);
  ldata <lsymbol, Tsymbol>::get_reps (r);
  ldata_iter <lsymbol, Tsymbol> syms (r, n);

  n = ldata <lfunction, Tfunction>::count_reps ();
  r = (ldata_rep **)alloca (sizeof *r * n);
  ldata <lfunction, Tfunction>::get_reps (r);
  ldata_iter <lfunction, Tfunction> fns (r, n);

  n = ldata <lsimple_string, Tsimple_string>::count_reps ();
  r = (ldata_rep **)alloca (sizeof *r * n);
  ldata <lsimple_string, Tsimple_string>::get_reps (r);
  ldata_iter <lsimple_string, Tsimple_string> strs (r, n);

  Qnil = syms.next ();
  Qunbound = syms.next ();

  strs.next ();
  strs.next ();
  for (lintr *li = intrs; li->s; li++)
    if (*li->s)
      li->str = strs.next ();
    else
      li->str = Qnil;

  combine_syms (lsp_vars, lsp_fns, syms, fns);
  combine_syms (cl_vars, cl_fns, syms, fns);
  combine_syms (sys_vars, sys_fns, syms, fns);
  combine_syms (kwd_vars, 0, syms, fns);
  combine_syms (ed_vars, ed_fns, syms, fns);
  combine_syms (unint_vars, 0, syms, fns);

  multiple_value::value (0) = Qnil;
  multiple_value::count () = 1;

  init_default_nonlocal_data ();
}

#define DECLARE_LDATA_BEGIN static const int ldata_begin = __LINE__;
#define DECLARE_LDATA_END static const int ldata_end = __LINE__;
#define DECLARE_LDATA(a, b)
#include "dataP.h"
static const int nobject_type = ldata_end - ldata_begin - 1;

struct dump_header
{
  long magic;
  long version;
  long file_size;
  long file_size_not;
  int nobject_type;
  int nreps;
  lisp nil;
};

struct addr_order
{
  int i;
  ldata_rep *p;
};

class dump_error
{
public:
  dump_error (){}
};

static addr_order *addr_orderp;
static int nreps;
static ldata_rep **laddrp;

static int __cdecl
search_addr (const void *p1, const void *p2)
{
  const char *x = (const char *)p1;
  const char *y = (const char *)((const addr_order *)p2)->p;
  if (x == y)
    return 0;
  return x < y ? -1 : 1;
}

static lisp
lmap (lisp p)
{
  if (immediatep (p))
    return p;
  addr_order *ap = (addr_order *)bsearch (lisp (pointer_t (p) & ~LDATA_PAGE_MASK), addr_orderp,
                                          nreps, sizeof *addr_orderp, search_addr);
  assert (ap);
  return lisp (u_long (ap->i) + (pointer_t (p) & LDATA_PAGE_MASK));
}

static inline lisp
rlmap (lisp p)
{
  if (immediatep (p))
    return p;
  assert (pointer_t (p) / LDATA_PAGE_SIZE < pointer_t (nreps));
  return lisp ((char *)laddrp[pointer_t (p) / LDATA_PAGE_SIZE]
               + (pointer_t (p) & LDATA_PAGE_MASK));
}

static int __cdecl
compare_addr (const void *p1, const void *p2)
{
  const char *x = (const char *)((const addr_order *)p1)->p;
  const char *y = (const char *)((const addr_order *)p2)->p;
  assert (x != y);
  return x < y ? -1 : 1;
}

static void
writef (FILE *fp, const void *p, size_t size)
{
  if (size && fwrite (p, size, 1, fp) != 1)
    {
      int e = errno;
      fclose (fp);
      FEsimple_crtl_error (e);
    }
}

static inline void
writef (FILE *fp, lisp x)
{
  x = lmap (x);
  writef (fp, &x, sizeof x);
}

static inline void
readf (FILE *fp, void *b, size_t size)
{
  if (size && fread (b, size, 1, fp) != 1)
    throw dump_error ();
}

static inline lisp
readl (FILE *fp)
{
  lisp x;
  readf (fp, &x, sizeof x);
  return rlmap (x);
}

static void
dump_object (FILE *fp, const lcons *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lcons *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->car);
        writef (fp, d->cdr);
      }
}

static void
rdump_object (FILE *fp, lcons *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lcons *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->car = readl (fp);
        d->cdr = readl (fp);
      }
}

static void
dump_object (FILE *fp, const lsymbol *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lsymbol *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->flags, sizeof d->flags);
        writef (fp, d->value);
        writef (fp, d->fn);
        writef (fp, d->plist);
        writef (fp, d->package);
        writef (fp, d->name);
      }
}

static void
rdump_object (FILE *fp, lsymbol *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lsymbol *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->flags, sizeof d->flags);
        d->value = readl (fp);
        d->fn = readl (fp);
        d->plist = readl (fp);
        d->package = readl (fp);
        d->name = readl (fp);
      }
}

template <class T>
void
dump_simple (FILE *fp, T *d, int n, const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (T *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      writef (fp, (const void *)d, sizeof *d);
}

template <class T>
void
rdump_simple (FILE *fp, T *d, int n, const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (T *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      readf (fp, (void *)d, sizeof *d);
}

static inline void
dump_object (FILE *fp, const llong_int *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  dump_simple (fp, d, n, used);
}

static inline void
rdump_object (FILE *fp, llong_int *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  rdump_simple (fp, d, n, used);
}

static void
dump_object (FILE *fp, const lfraction *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lfraction *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->num);
        writef (fp, d->den);
      }
}

static void
rdump_object (FILE *fp, lfraction *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lfraction *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->num = readl (fp);
        d->den = readl (fp);
      }
}

static void
dump_object (FILE *fp, const lbignum *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lbignum *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      writef (fp, d->rep,
              (d->rep->br_len
               ? sizeof *d->rep + (d->rep->br_len - 1) * sizeof *d->rep->br_data
               : sizeof *d->rep));
}

static void
rdump_object (FILE *fp, lbignum *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lbignum *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        bignum_rep r;
        readf (fp, &r, sizeof r);
        if (r.zerop ())
          d->rep = &bignum_rep_zero;
        else if (r.br_len == 1 && r.br_data[0] == 1)
          d->rep = r.plusp () ? &bignum_rep_one : &bignum_rep_minus_one;
        else
          {
            d->rep = br_new (r.br_len);
            d->rep->br_sign = r.br_sign;
            d->rep->br_data[0] = r.br_data[0];
            readf (fp, &d->rep->br_data[1],
                   (r.br_len - 1) * sizeof *d->rep->br_data);
          }
      }
}

static inline void
dump_object (FILE *fp, const lsingle_float *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  dump_simple (fp, d, n, used);
}

static inline void
rdump_object (FILE *fp, lsingle_float *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  rdump_simple (fp, d, n, used);
}

static inline void
dump_object (FILE *fp, const ldouble_float *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  dump_simple (fp, d, n, used);
}

static inline void
rdump_object (FILE *fp, ldouble_float *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  rdump_simple (fp, d, n, used);
}

static void
dump_object (FILE *fp, const lcomplex *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lcomplex *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->real);
        writef (fp, d->imag);
      }
}

static void
rdump_object (FILE *fp, lcomplex *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lcomplex *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->real = readl (fp);
        d->imag = readl (fp);
      }
}

static void
dump_object (FILE *fp, const lclosure *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lclosure *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->body);
        writef (fp, d->vars);
        writef (fp, d->fns);
        writef (fp, d->frame);
        writef (fp, d->name);
      }
}

static void
rdump_object (FILE *fp, lclosure *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lclosure *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->body = readl (fp);
        d->vars = readl (fp);
        d->fns = readl (fp);
        d->frame = readl (fp);
        d->name = readl (fp);
      }
}

static void
dump_vector_contents (FILE *fp, lisp *p, int l)
{
  for (lisp *pe = p + l; p < pe; p++)
    writef (fp, *p);
}

static void *
rdump_vector_contents (FILE *fp, int l)
{
  void *p0 = xmalloc (sizeof (lisp) * l);
  readf (fp, p0, sizeof (lisp) * l);
  for (lisp *p = (lisp *)p0, *pe = p + l; p < pe; p++)
    *p = rlmap (*p);
  return p0;
}

static void
dump_displaced_offset (FILE *fp, const lbase_array *d)
{
  lbase_vector *b = (lbase_vector *)d->displaced_to;
  ptrdiff_t diff = (char *)d->contents - (char *)b->contents;
  writef (fp, &diff, sizeof diff);
}

static void
rdump_displaced_offset (FILE *fp, lbase_array *d)
{
  ptrdiff_t diff;
  readf (fp, &diff, sizeof diff);
  d->contents = (void *)((diff << 1) | 1);
}

static void
fixup_displaced_offset (lbase_array *d)
{
  if (!(ptrdiff_t (d->contents) & 1))
    return;
  fixup_displaced_offset ((lbase_array *)d->displaced_to);
  d->contents = (void *)((char *)((lbase_vector *)d->displaced_to)->contents
                         + (ptrdiff_t (d->contents) >> 1));
}

template <class T, u_int F>
void
ldata <T, F>::array_fixup_displaced_offset ()
{
  for (ldata_rep *lp = l_ld.ld_rep; lp; lp = lp->dr_next)
    for (T *d = (T *)lp->dr_data, *de = d + LDATA_NOBJS (T); d < de; d++)
      if (bitisset (lp->dr_used, bit_index (d)))
        fixup_displaced_offset (d);
}

static void
dump_object (FILE *fp, const lsimple_vector *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lsimple_vector *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        dump_vector_contents (fp, (lisp *)d->contents, d->length);
      }
}

static void
rdump_object (FILE *fp, lsimple_vector *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lsimple_vector *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        d->contents = rdump_vector_contents (fp, d->length);
      }
}

static void
dump_object (FILE *fp, const lcomplex_vector *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lcomplex_vector *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, d->displaced_to);
        writef (fp, d->referenced_list);
        writef (fp, &d->adjustable, sizeof d->adjustable);
        writef (fp, &d->has_fillp, sizeof d->has_fillp);
        writef (fp, &d->dimension, sizeof d->dimension);
        if (d->displaced_to == Qnil)
          dump_vector_contents (fp, (lisp *)d->contents, d->dimension);
        else
          dump_displaced_offset (fp, d);
      }
}

static void
rdump_object (FILE *fp, lcomplex_vector *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lcomplex_vector *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        d->displaced_to = readl (fp);
        d->referenced_list = readl (fp);
        readf (fp, &d->adjustable, sizeof d->adjustable);
        readf (fp, &d->has_fillp, sizeof d->has_fillp);
        readf (fp, &d->dimension, sizeof d->dimension);
        if (d->displaced_to == Qnil)
          d->contents = rdump_vector_contents (fp, d->dimension);
        else
          rdump_displaced_offset (fp, d);
        d->rank = 1;
        d->dims = &d->dimension;
      }
}

static void
dump_object (FILE *fp, const lsimple_string *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lsimple_string *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, d->contents, sizeof (Char) * d->length);
      }
}

static void
rdump_object (FILE *fp, lsimple_string *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lsimple_string *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        d->contents = xmalloc (sizeof (Char) * d->length);
        readf (fp, d->contents, sizeof (Char) * d->length);
      }
}

static void
dump_object (FILE *fp, const lcomplex_string *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lcomplex_string *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, d->displaced_to);
        writef (fp, d->referenced_list);
        writef (fp, &d->adjustable, sizeof d->adjustable);
        writef (fp, &d->has_fillp, sizeof d->has_fillp);
        writef (fp, &d->dimension, sizeof d->dimension);
        if (d->displaced_to == Qnil)
          writef (fp, d->contents, sizeof (Char) * d->dimension);
        else
          dump_displaced_offset (fp, d);
      }
}

static void
rdump_object (FILE *fp, lcomplex_string *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lcomplex_string *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        d->displaced_to = readl (fp);
        d->referenced_list = readl (fp);
        readf (fp, &d->adjustable, sizeof d->adjustable);
        readf (fp, &d->has_fillp, sizeof d->has_fillp);
        readf (fp, &d->dimension, sizeof d->dimension);
        if (d->displaced_to == Qnil)
          {
            d->contents = xmalloc (sizeof (Char) * d->dimension);
            readf (fp, d->contents, sizeof (Char) * d->dimension);
          }
        else
          rdump_displaced_offset (fp, d);
        d->rank = 1;
        d->dims = &d->dimension;
      }
}

static void
dump_object (FILE *fp, const lgeneral_array *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lgeneral_array *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, &d->rank, sizeof d->rank);
        writef (fp, d->dims, sizeof *d->dims * d->rank);
        writef (fp, d->displaced_to);
        writef (fp, d->referenced_list);
        writef (fp, &d->adjustable, sizeof d->adjustable);
        if (d->displaced_to == Qnil)
          dump_vector_contents (fp, (lisp *)d->contents, d->length);
        else
          dump_displaced_offset (fp, d);
      }
}

static void
rdump_object (FILE *fp, lgeneral_array *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lgeneral_array *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        readf (fp, &d->rank, sizeof d->rank);
        if (!d->rank)
          d->dims = 0;
        else
          {
            d->dims = (int *)xmalloc (sizeof *d->dims * d->rank);
            readf (fp, d->dims, sizeof *d->dims * d->rank);
          }
        d->displaced_to = readl (fp);
        d->referenced_list = readl (fp);
        readf (fp, &d->adjustable, sizeof d->adjustable);
        d->has_fillp = 0;
        if (d->displaced_to == Qnil)
          d->contents = rdump_vector_contents (fp, d->length);
        else
          rdump_displaced_offset (fp, d);
      }
}

static void
dump_object (FILE *fp, const lstring_array *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lstring_array *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, &d->rank, sizeof d->rank);
        writef (fp, d->dims, sizeof *d->dims * d->rank);
        writef (fp, d->displaced_to);
        writef (fp, d->referenced_list);
        writef (fp, &d->adjustable, sizeof d->adjustable);
        if (d->displaced_to == Qnil)
          writef (fp, d->contents, sizeof (Char) * d->length);
        else
          dump_displaced_offset (fp, d);
      }
}

static void
rdump_object (FILE *fp, lstring_array *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lstring_array *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        readf (fp, &d->rank, sizeof d->rank);
        if (!d->rank)
          d->dims = 0;
        else
          {
            d->dims = (int *)xmalloc (sizeof *d->dims * d->rank);
            readf (fp, d->dims, sizeof *d->dims * d->rank);
          }
        d->displaced_to = readl (fp);
        d->referenced_list = readl (fp);
        readf (fp, &d->adjustable, sizeof d->adjustable);
        d->has_fillp = 0;
        if (d->displaced_to == Qnil)
          {
            d->contents = xmalloc (sizeof (Char) * d->length);
            readf (fp, d->contents, sizeof (Char) * d->length);
          }
        else
          rdump_displaced_offset (fp, d);
      }
}

static void
dump_object (FILE *fp, const lstream *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lstream *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->type, sizeof d->type);
        switch (d->type)
          {
          case st_file_input:
          case st_file_output:
          case st_file_io:
            writef (fp, lisp (d->pathname));
            break;

          case st_string_input:
          case st_string_output:
          case st_synonym:
          case st_broadcast:
          case st_concatenated:
          case st_two_way:
          case st_echo:
            writef (fp, lisp (d->input));
            writef (fp, lisp (d->output));
            break;

          case st_status:
          case st_keyboard:
          case st_wstream:
          case st_socket:
          case st_debug_output:
            break;

          case st_buffer:
            writef (fp, lisp (d->input));
            writef (fp, lisp (d->output));
            break;

          case st_general_input:
            writef (fp, lisp (d->input));
            writef (fp, lisp (d->output));
            writef (fp, lisp (d->pathname));
            break;

          case st_general_output:
            writef (fp, lisp (d->input));
            writef (fp, lisp (d->output));
            writef (fp, lisp (d->pathname));
            break;

          default:
            assert (0);
            break;
          }
      }
}

static void
rdump_object (FILE *fp, lstream *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lstream *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->pending = lChar_EOF;
        d->column = 0;
        d->linenum = 1;
        d->start = 0;
        d->end = 0;
        d->alt_pathname = 0;
        d->open_p = 0;
        d->encoding = lstream::ENCODE_CANON;
        readf (fp, &d->type, sizeof d->type);
        switch (d->type)
          {
          case st_file_input:
          case st_file_output:
          case st_file_io:
            d->pathname = readl (fp);
            d->input = 0;
            d->output = 0;
            break;

          case st_string_input:
          case st_string_output:
          case st_synonym:
          case st_broadcast:
          case st_concatenated:
          case st_two_way:
          case st_echo:
            d->pathname = Qnil;
            d->input = (void *)readl (fp);
            d->output = (void *)readl (fp);
            break;

          case st_status:
          case st_keyboard:
          case st_wstream:
          case st_socket:
          case st_debug_output:
            d->pathname = Qnil;
            d->input = 0;
            d->output = 0;
            break;

          case st_buffer:
            d->pathname = Qnil;
            d->input = (void *)readl (fp);
            d->output = (void *)readl (fp);
            break;

          case st_general_input:
            d->input = (void *)readl (fp);
            d->output = (void *)readl (fp);
            d->pathname = readl (fp);
            d->alt_pathname = (char *)Qnil;
            d->start = 0;
            break;

          case st_general_output:
            d->input = (void *)readl (fp);
            d->output = (void *)readl (fp);
            d->pathname = readl (fp);
            break;

          default:
            assert (0);
            break;
          }
      }
}

static void
dump_object (FILE *fp, const lpackage *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lpackage *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->name);
        writef (fp, d->nicknames);
        writef (fp, d->use_list);
        writef (fp, d->used_by_list);
        writef (fp, d->shadowings);
        writef (fp, d->internal);
        writef (fp, d->external);
        writef (fp, d->documentation);
      }
}

static void
rdump_object (FILE *fp, lpackage *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lpackage *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->name = readl (fp);
        d->nicknames = readl (fp);
        d->use_list = readl (fp);
        d->used_by_list = readl (fp);
        d->shadowings = readl (fp);
        d->internal = readl (fp);
        d->external = readl (fp);
        d->documentation = readl (fp);
      }
}

static void
dump_object (FILE *fp, const lfunction *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lfunction *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->name);
        writef (fp, &d->flags, sizeof d->flags);
        writef (fp, &d->nargs, sizeof d->nargs);
        writef (fp, &d->nopts, sizeof d->nopts);
        writef (fp, &d->interactive, sizeof d->interactive);
      }
}

static void
rdump_object (FILE *fp, lfunction *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lfunction *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->name = readl (fp);
        readf (fp, &d->flags, sizeof d->flags);
        readf (fp, &d->nargs, sizeof d->nargs);
        readf (fp, &d->nopts, sizeof d->nopts);
        readf (fp, &d->interactive, sizeof d->interactive);
      }
}

static void
dump_object (FILE *fp, const lstruct_def *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lstruct_def *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->name);
        writef (fp, d->type);
        writef (fp, d->includes);
        writef (fp, d->constructors);
        writef (fp, d->print_function);
        writef (fp, d->report);
        writef (fp, &d->nslots, sizeof d->nslots);
        dump_vector_contents (fp, (lisp *)d->slotdesc,
                              d->nslots * (sizeof *d->slotdesc / sizeof (lisp)));
        writef (fp, &d->named, sizeof d->named);
        writef (fp, &d->read_only, sizeof d->read_only);
        writef (fp, &d->important, sizeof d->important);
      }
}

static void
rdump_object (FILE *fp, lstruct_def *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lstruct_def *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->name = readl (fp);
        d->type = readl (fp);
        d->includes = readl (fp);
        d->constructors = readl (fp);
        d->print_function = readl (fp);
        d->report = readl (fp);
        readf (fp, &d->nslots, sizeof d->nslots);
        d->slotdesc = (struct_slotdesc *)
          rdump_vector_contents (fp, d->nslots * (sizeof *d->slotdesc / sizeof (lisp)));
        readf (fp, &d->named, sizeof d->named);
        readf (fp, &d->read_only, sizeof d->read_only);
        readf (fp, &d->important, sizeof d->important);
      }
}

static void
dump_object (FILE *fp, const lstruct_data *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lstruct_data *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->def);
        writef (fp, &d->nslots, sizeof d->nslots);
        dump_vector_contents (fp, d->data, d->nslots);
      }
}

static void
rdump_object (FILE *fp, lstruct_data *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lstruct_data *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->def = readl (fp);
        readf (fp, &d->nslots, sizeof d->nslots);
        d->data = (lisp *)rdump_vector_contents (fp, d->nslots);
      }
}

static inline void
dump_object (FILE *, const lwindow *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lwindow *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lwindow *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      d->wp = 0;
}

static inline void
dump_object (FILE *, const lbuffer *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lbuffer *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lbuffer *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      d->bp = 0;
}

static void
dump_object (FILE *fp, const lsyntax_table *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lsyntax_table *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      writef (fp, d->table, sizeof *d->table);
}

static void
rdump_object (FILE *fp, lsyntax_table *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lsyntax_table *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->table = (syntax_table *)xmalloc (sizeof *d->table);
        readf (fp, d->table, sizeof *d->table);
      }
}

static inline void
dump_object (FILE *, const lmarker *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lmarker *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lmarker *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      d->buffer = 0;
}

static inline void
dump_object (FILE *, const lprocess *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lprocess *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lprocess *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->data = 0;
        d->status = PS_NONE;
        d->buffer = Qnil;
        d->command = Qnil;
        d->incode = Qnil;
        d->outcode = Qnil;
      }
}

static void
dump_object (FILE *fp, const lregexp *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lregexp *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->length, sizeof d->length);
        writef (fp, &d->flags, sizeof d->flags);
        writef (fp, d->pattern, sizeof (Char) * d->length);
        writef (fp, d->source);
      }
}

static void
rdump_object (FILE *fp, lregexp *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lregexp *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->length, sizeof d->length);
        readf (fp, &d->flags, sizeof d->flags);
        d->pattern = (Char *)xmalloc (sizeof (Char) * d->length);
        readf (fp, d->pattern, sizeof (Char) * d->length);
        d->source = readl (fp);
      }
}

static inline void
dump_object (FILE *, const lwin32_menu *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lwin32_menu *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lwin32_menu *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->handle = 0;
        d->id = 0;
        d->init = Qnil;
        d->command = Qnil;
        d->tag = Qnil;
      }
}

static inline void
dump_object (FILE *, const lwin32_dde_handle *, int,
             const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lwin32_dde_handle *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lwin32_dde_handle *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      d->hconv = 0;
}

#define HT_EQ 0
#define HT_EQL 1
#define HT_EQUAL 2
#define HT_EQUALP 3

static void
dump_object (FILE *fp, const lhash_table *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lhash_table *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        int test;
        if (d->test == Feq)
          test = HT_EQ;
        else if (d->test == Feql)
          test = HT_EQL;
        else if (d->test == Fequal)
          test = HT_EQUAL;
        else
          test = HT_EQUALP;
        writef (fp, &test, sizeof test);
        writef (fp, &d->size, sizeof d->size);
        writef (fp, d->rehash_size);
        writef (fp, &d->rehash_threshold, sizeof d->rehash_threshold);
        writef (fp, &d->used, sizeof d->used);
        writef (fp, &d->count, sizeof d->count);
        for (const hash_entry *e = d->entry, *ee = e + d->size; e < ee; e++)
          {
            writef (fp, e->key);
            writef (fp, e->value);
          }
      }
}

static void
rdump_object (FILE *fp, lhash_table *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lhash_table *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        int test;
        readf (fp, &test, sizeof test);
        if (test == HT_EQ)
          d->test = Feq;
        else if (test == HT_EQL)
          d->test = Feql;
        else if (test == HT_EQUAL)
          d->test = Fequal;
        else
          d->test = Fequalp;
        readf (fp, &d->size, sizeof d->size);
        d->rehash_size = readl (fp);
        readf (fp, &d->rehash_threshold, sizeof d->rehash_threshold);
        readf (fp, &d->used, sizeof d->used);
        readf (fp, &d->count, sizeof d->count);
        d->entry = (hash_entry *)xmalloc (sizeof *d->entry * d->size);
        readf (fp, d->entry, sizeof *d->entry * d->size);
        for (hash_entry *e = d->entry, *ee = e + d->size; e < ee; e++)
          {
            e->key = rlmap (e->key);
            e->value = rlmap (e->value);
          }
      }
}

static void
dump_object (FILE *fp, const lreadtable *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lreadtable *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->rcase, sizeof d->rcase);
        for (const readtab_rep *r = d->rep, *re = r + READTABLE_REP_SIZE;
             r < re; r++)
          {
            writef (fp, &r->type, sizeof r->type);
            writef (fp, r->lfunc);
            char disp = r->disp ? 1 : 0;
            writef (fp, &disp, sizeof disp);
            if (disp)
              for (const disptab_rep *dr = r->disp, *dre = dr + READTABLE_REP_SIZE;
                   dr < dre; dr++)
                writef (fp, dr->lfunc);
          }
      }
}

static void
rdump_object (FILE *fp, lreadtable *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lreadtable *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->rcase, sizeof d->rcase);
        d->rep = (readtab_rep *)xmalloc (sizeof (readtab_rep) * READTABLE_REP_SIZE);
        bzero (d->rep, sizeof (readtab_rep) * READTABLE_REP_SIZE);
        for (readtab_rep *r = d->rep, *re = r + READTABLE_REP_SIZE;
             r < re; r++)
          {
            readf (fp, &r->type, sizeof r->type);
            r->lfunc = readl (fp);
            r->cfunc = get_reader_macro_function (r->lfunc);
            char disp;
            readf (fp, &disp, sizeof disp);
            if (disp)
              {
                r->disp = (disptab_rep *)xmalloc (sizeof (disptab_rep) * READTABLE_REP_SIZE);
                for (disptab_rep *dr = r->disp, *dre = dr + READTABLE_REP_SIZE;
                     dr < dre; dr++)
                  {
                    dr->lfunc = readl (fp);
                    dr->cfunc = get_reader_dispmacro_function (dr->lfunc);
                  }
              }
          }
      }
}

static inline void
dump_object (FILE *fp, const lerror *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  dump_simple (fp, d, n, used);
}

static inline void
rdump_object (FILE *fp, lerror *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  rdump_simple (fp, d, n, used);
}

static inline void
dump_object (FILE *fp, const lrandom_state *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lrandom_state *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->object.index (), sizeof (d->object.index ()));
        writef (fp, d->object.state (), sizeof (uint32_t) * Random::INDEX_MAX);
      }
}

static inline void
rdump_object (FILE *fp, lrandom_state *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lrandom_state *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->object.alloc_random_state ();
        readf (fp, &d->object.index (), sizeof d->object.index ());
        readf (fp, d->object.state (), sizeof (uint32_t) * Random::INDEX_MAX);
      }
}

static void
dump_object (FILE *fp, const lchunk *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lchunk *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->type);
        writef (fp, &d->size, sizeof d->size);
        writef (fp, d->owner);
        if (d->owner == lisp (d))
          writef (fp, d->data, d->size);
        else if (d->owner == Qnil)
          writef (fp, &d->data, sizeof d->data);
        else
          {
            ptrdiff_t diff = (char *)d->data - (char *)xchunk_data (d->owner);
            writef (fp, &diff, sizeof diff);
          }
      }

  assert (sizeof (void *) >= sizeof (ptrdiff_t));
}

static void
rdump_object (FILE *fp, lchunk *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lchunk *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->type = readl (fp);
        readf (fp, &d->size, sizeof d->size);
        d->owner = readl (fp);
        if (d->owner == d)
          {
            d->data = xmalloc (d->size);
            readf (fp, d->data, d->size);
          }
        else if (d->owner == Qnil)
          readf (fp, &d->data, sizeof d->data);
        else
          {
            ptrdiff_t diff;
            readf (fp, &diff, sizeof diff);
            d->data = (void *)diff;
          }
      }
}

static void
fixup_chunk_offset (lchunk *d)
{
  if (d->owner == d || d->owner == Qnil)
    return;
  void *p = (void *)((char *)((lchunk *)d->owner)->data
                     + ptrdiff_t (d->data));
  if (d->data == p)
    return;
  fixup_chunk_offset ((lchunk *)d->owner);
  d->data = p;
}

template <class T, u_int F>
void
ldata <T, F>::chunk_fixup_data_offset ()
{
  for (ldata_rep *lp = l_ld.ld_rep; lp; lp = lp->dr_next)
    for (T *d = (T *)lp->dr_data, *de = d + LDATA_NOBJS (T); d < de; d++)
      if (bitisset (lp->dr_used, bit_index (d)))
        fixup_chunk_offset (d);
}


static void
dump_object (FILE *fp, const ldll_module *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const ldll_module *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      writef (fp, d->name);
}

static void
load_dyn_library (ldll_module *p)
{
  char *s = (char *)alloca (xstring_length (p->name) * 2 + 1);
  w2s (s, p->name);
  p->loaded = 0;
  HMODULE h = GetModuleHandle (s);
  if (!h)
    {
      h = WINFS::LoadLibrary (s);
      if (h)
        p->loaded = 1;
    }
  p->handle = h;
}

static void
rdump_object (FILE *fp, ldll_module *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (ldll_module *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->name = readl (fp);
        load_dyn_library (d);
      }
}

static void
dump_object (FILE *fp, const ldll_function *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const ldll_function *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->module);
        writef (fp, d->name);
        writef (fp, &d->nargs, sizeof d->nargs);
        writef (fp, d->arg_types, d->nargs);
        writef (fp, &d->arg_size, sizeof d->arg_size);
        writef (fp, &d->return_type, sizeof d->return_type);
        writef (fp, &d->vaarg_p, sizeof d->vaarg_p);
      }
}

static FARPROC
load_dyn_function (const ldll_function *d)
{
  if (!xdll_module_handle (d->module))
    return 0;
  char *s = (char *)alloca (xstring_length (d->name) * 2 + 1);
  w2s (s, d->name);
  return GetProcAddress (xdll_module_handle (d->module), s);
}

static void
rdump_object (FILE *fp, ldll_function *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (ldll_function *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->module = readl (fp);
        d->name = readl (fp);
        readf (fp, &d->nargs, sizeof d->nargs);
        d->arg_types = (u_char *)xmalloc (d->nargs);
        readf (fp, d->arg_types, d->nargs);
        readf (fp, &d->arg_size, sizeof d->arg_size);
        readf (fp, &d->return_type, sizeof d->return_type);
        readf (fp, &d->vaarg_p, sizeof d->vaarg_p);
        d->proc = load_dyn_function (d);
      }
}

static void
dump_object (FILE *fp, const lc_callable *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lc_callable *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, d->function);
        writef (fp, &d->nargs, sizeof d->nargs);
        writef (fp, d->arg_types, d->nargs);
        writef (fp, &d->arg_size, sizeof d->arg_size);
        writef (fp, &d->return_type, sizeof d->return_type);
        writef (fp, &d->convention, sizeof d->convention);
      }
}

static void
rdump_object (FILE *fp, lc_callable *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lc_callable *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->function = readl (fp);
        readf (fp, &d->nargs, sizeof d->nargs);
        d->arg_types = (u_char *)xmalloc (d->nargs);
        readf (fp, d->arg_types, d->nargs);
        readf (fp, &d->arg_size, sizeof d->arg_size);
        readf (fp, &d->return_type, sizeof d->return_type);
        readf (fp, &d->convention, sizeof d->convention);
        init_c_callable (d);
      }
}

static inline void
dump_object (FILE *, const loledata *, int, const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, loledata *d, int n, const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (loledata *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->name = 0;
        d->disp = 0;
        d->enumerator = 0;
        d->event = 0;
      }
}

static inline void
dump_object (FILE *, const lwait_object *, int, const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lwait_object *d, int n, const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lwait_object *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      d->hevent = 0;
}

static void
dump_object (FILE *fp, const lchar_encoding *d, int n,
             const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (const lchar_encoding *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        writef (fp, &d->type, sizeof d->type);
        writef (fp, d->name);
        writef (fp, d->display_name);
        writef (fp, &d->u, sizeof d->u);
      }
}

static void
rdump_object (FILE *fp, lchar_encoding *d, int n,
              const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lchar_encoding *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        readf (fp, &d->type, sizeof d->type);
        d->name = readl (fp);
        d->display_name = readl (fp);
        readf (fp, &d->u, sizeof d->u);
      }
}

static inline void
dump_object (FILE *, const lenvironment *, int, const u_long [LDATA_MAX_OBJECTS_PER_LONG])
{
}

static void
rdump_object (FILE *fp, lenvironment *d, int n, const u_long used[LDATA_MAX_OBJECTS_PER_LONG])
{
  for (lenvironment *de = d + n; d < de; d++)
    if (bitisset (used, bit_index (d)))
      {
        d->lvar = Qnil;
        d->lframe = Qnil;
        d->lfns = Qnil;
      }
}

template <class T, u_int F>
void
ldata <T, F>::dump_reps (FILE *fp)
{
  for (const ldata_rep *lp = l_ld.ld_rep; lp; lp = lp->dr_next)
    {
      writef (fp, lp->dr_used, sizeof lp->dr_used);
      dump_object (fp, (const T *)lp->dr_data, LDATA_NOBJS (T), lp->dr_used);
    }
}

#define DMAGIC 0xef1380feL
extern int dump_version;

lisp
Fdump_xyzzy (lisp filename)
{
  char path_buf[PATH_MAX + 1];
  const char *path;
  if (!filename || filename == Qnil)
    {
      filename = xsymbol_value (Qdump_image_path);
      path = app.dump_image;
    }
  else
    {
      pathname2cstr (filename, path_buf);
      path = path_buf;
    }

  protect_gc gcpro (filename);

  gc (1);

  int i = 0;
  int counts[nobject_type];
#define DECLARE_LDATA(a, b) counts[i++] = ldata <a, b>::count_reps ();
#include "dataP.h"

  nreps = 0;
  for (i = 0; i < nobject_type; i++)
    nreps += counts[i];

  ldata_rep **reps = (ldata_rep **)alloca (sizeof *reps * nreps);
  ldata_rep **r = reps;
  i = 0;
#define DECLARE_LDATA(a, b) ldata <a, b>::get_reps (r); r += counts[i++];
#include "dataP.h"

  addr_order *ap = (addr_order *)alloca (sizeof *ap * nreps);
  addr_orderp = ap;
  r = reps;
  for (i = 0; i < nreps; i++, r++, ap++)
    {
      ap->i = i * LDATA_PAGE_SIZE;
      ap->p = *r;
    }
  qsort (addr_orderp, nreps, sizeof *addr_orderp, compare_addr);

  FILE *fp = fopen (path, "wb");
  if (!fp)
    FEsimple_crtl_error (errno, filename);

  dump_header head;
  head.magic = DMAGIC;
  head.version = dump_version;
  head.file_size = 0;
  head.file_size_not = 0;
  head.nobject_type = nobject_type;
  head.nreps = nreps;
  head.nil = lmap (Qnil);
  writef (fp, &head, sizeof head);
  writef (fp, counts, sizeof counts);

#define DECLARE_LDATA(a, b) ldata <a, b>::dump_reps (fp);
#include "dataP.h"
  long off = ftell (fp);
  head.file_size = off;
  head.file_size_not = ~off;
  fseek (fp, 0, SEEK_SET);
  writef (fp, &head, sizeof head);
  fclose (fp);

  return Qnil;
}

template <class T, u_int F>
void
ldata <T, F>::rdump_reps (FILE *fp)
{
  for (ldata_rep *lp = l_ld.ld_rep; lp; lp = lp->dr_next)
    {
      readf (fp, lp->dr_used, sizeof lp->dr_used);
      rdump_object (fp, (T *)lp->dr_data, LDATA_NOBJS (T), lp->dr_used);
    }
}

static int
rdump_xyzzy (FILE *fp)
{
  dump_header head;
  readf (fp, &head, sizeof head);
  if (head.magic != DMAGIC
      || head.version != dump_version
      || head.file_size != _filelength (_fileno (fp))
      || head.file_size_not != ~head.file_size
      || head.nobject_type != nobject_type)
    return 0;

  int counts[nobject_type];
  readf (fp, counts, sizeof counts);
  int i, n;
  for (i = 0, n = 0; i < nobject_type; i++)
    n += counts[i];
  if (n != head.nreps)
    return 0;

  nreps = n;
  laddrp = (ldata_rep **)alloca (sizeof (ldata_rep *) * n);

  i = 0;
  ldata_rep **lp = laddrp;
#define DECLARE_LDATA(a, b) \
  ldata <a, b>::alloc_reps (lp, counts[i]); lp += counts[i++];
#include "dataP.h"

  Qnil = rlmap (head.nil);

#define DECLARE_LDATA(a, b) ldata <a, b>::rdump_reps (fp);
#include "dataP.h"

  return 1;
}

static int dump_flag;

int
rdump_xyzzy ()
{
  FILE *fp = _fsopen (app.dump_image, "rb", _SH_DENYWR);
  if (!fp)
    return 0;

  dump_flag = 0;
  try
    {
      dump_flag = rdump_xyzzy (fp) && getc (fp) == EOF;
    }
  catch (dump_error)
    {
    }

  fclose (fp);

  if (dump_flag)
    {
#define DECLARE_LDATA(a, b) /* empty */
#define DECLARE_LARRAY(a, b) ldata<a, b>::array_fixup_displaced_offset ();
#include "dataP.h"

      ldata <lchunk, Tchunk>::chunk_fixup_data_offset ();

#define DECLARE_LDATA(a, b) ldata <a, b>::link_unused ();
#include "dataP.h"
    }
  else
    {
#define DECLARE_LDATA(a, b) ldata <a, b>::free_all_reps ();
#include "dataP.h"
    }

  return dump_flag;
}

lisp
Fxyzzy_dumped_p ()
{
  return boole (dump_flag);
}


#ifdef DEBUG_GC
static void
output_funcall_mark (FILE *fp, lfns *p, const char *pkg = "")
{
  for (; p->name; p++)
    if (p->called)
      {
        for (int f = 1; f <= 0x20; f <<= 1)
          putc (p->called & f ? 'o' : ' ', fp);
        putc (p->called & 0x7f ? '*' : ' ', fp);
        putc (p->called & 0x80 ? '@' : ' ', fp);
        fprintf (fp, ": %s%.*s\n", pkg, p->size, p->name);
      }
}

void
output_funcall_mark (FILE *fp)
{
  fprintf (fp, "Funcall list:\n");
  output_funcall_mark (fp, lsp_fns);
  output_funcall_mark (fp, cl_fns);
  output_funcall_mark (fp, sys_fns, "si:");
  output_funcall_mark (fp, ed_fns);
}
#endif /* DEBUG_GC */

void
rehash_all_hash_tables ()
{
  int n = ldata <lhash_table, Thash_table>::count_reps ();
  ldata_rep **r = (ldata_rep **)alloca (sizeof *r * n);
  ldata <lhash_table, Thash_table>::get_reps (r);
  ldata_iter <lhash_table, Thash_table> tables (r, n);
  lhash_table **h = new lhash_table*[n * LDATA_NOBJS (lhash_table)];
  int count = 0;
  for (int i = 0; i < n; i++)
    {
      lhash_table *d = tables.next ();
      for (lhash_table *de = d + LDATA_NOBJS (lhash_table); d < de; d++)
        if (bitisset (used_place (d), bit_index (d)))
          h[count++] = d;
    }
  for (int j = 0; j < count; j++)
    hash_table_rehash (h[j], 0);

  delete[] h;
}
