#include "stdafx.h"
#include "ed.h"
#include "syntaxinfo.h"
#include "filer.h"
#include "binfo.h"
#include "buffer-bar.h"
#include "version.h"

fixed_heap Chunk::c_heap (sizeof (Char) * TEXT_SIZE);
fixed_heap Chunk::c_breaks_heap (BREAKS_SIZE);
Buffer *Buffer::b_blist;
Buffer *Buffer::b_dlist;
Buffer *Buffer::b_last_selected_buffer;

long Buffer::b_total_create_count;
Buffer *Buffer::b_last_title_bar_buffer;
int Buffer::b_title_bar_text_order;
int Buffer::b_default_fold_mode = FOLD_NONE;
int Buffer::b_default_linenum_mode = LNMODE_DISP;
int Buffer::b_default_kinsoku_mode = KINSOKU_MODE_MASK;
int Buffer::b_default_kinsoku_extend_limit = 3;
int Buffer::b_default_kinsoku_shorten_limit = 10;
u_char Buffer::b_buffer_bar_modified_any;

fixed_heap ChunkHeap::a_heap (8192);
fixed_heap textprop_heap::a_heap (4096);
const u_char Chunk::c_breaks_mask[] = {1, 2, 4, 8, 16, 32, 64, 128};

class enum_buffer
{
  static enum_buffer *eb_root;
  enum_buffer *eb_last;
  Buffer *eb_bp;
public:
  enum_buffer () : eb_last (eb_root), eb_bp (Buffer::b_blist) {eb_root = this;}
  ~enum_buffer () {eb_root = eb_last;}
  Buffer *next ()
    {
      Buffer *bp = eb_bp;
      if (eb_bp)
        eb_bp = eb_bp->b_next;
      return bp;
    }
  static void deleted (Buffer *bp)
    {
      for (enum_buffer *b = eb_root; b; b = b->eb_last)
        if (b->eb_bp == bp)
          b->eb_bp = bp->b_next;
    }
};

enum_buffer *enum_buffer::eb_root;

void
Chunk::clear ()
{
  c_used = 0;
  c_nlines = 0;
  c_nbreaks = 0;
  bzero (c_breaks, BREAKS_SIZE);
  c_first_eol = -1;
  c_last_eol = -1;
  c_bstate = syntax_state::SS_INVALID;
}

Chunk *
Buffer::alloc_chunk ()
{
  Chunk *cp = b_chunk_heap.alloc ();
  if (!cp)
    return 0;
  cp->c_prev = 0;
  cp->c_next = 0;
  cp->c_used = 0;
  cp->c_nlines = -1;
  cp->c_nbreaks = -1;
  cp->c_first_eol = -1;
  cp->c_last_eol = -1;
  cp->c_bstate = syntax_state::SS_INVALID;
  cp->c_text = (Char *)Chunk::c_heap.alloc ();
  if (cp->c_text)
    {
      cp->c_breaks = (u_char *)Chunk::c_breaks_heap.alloc ();
      if (cp->c_breaks)
        return cp;
      Chunk::c_heap.free (cp->c_text);
    }
  b_chunk_heap.free (cp);
  return 0;
}

void
Buffer::free_chunk (Chunk *cp)
{
  assert (cp);
  if (cp->c_text)
    Chunk::c_heap.free (cp->c_text);
  if (cp->c_breaks)
    Chunk::c_breaks_heap.free (cp->c_breaks);
  b_chunk_heap.free (cp);
}

void
Buffer::free_all_chunks (Chunk *cp)
{
  Chunk *next;
  for (; cp; cp = next)
    {
      next = cp->c_next;
      free_chunk (cp);
    }
}

Buffer::Buffer (lisp name, lisp filename, lisp dirname, int temporary)
{
  b_prev = b_next = 0;
  b_ldisp = 0;

  b_tab_columns = app.default_tab_columns;
  b_local_tab_columns = 0;

  lbp = temporary ? Qnil : make_buffer ();

  b_chunkb = alloc_chunk ();
  if (!b_chunkb)
    FEstorage_error ();
  b_chunke = b_chunkb;
  b_nchars = 0;
  b_nlines = 1;
  b_nfolded = 1;

  b_textprop = 0;
  b_textprop_cache = 0;

  b_contents.p1 = 0;
  b_contents.p2 = 0;

  b_point = 0;
  b_mark = NO_MARK_SET;

  b_disp = 0;

  b_version = -1;

  long n;
  if (!safe_fixnum_value (xsymbol_value (Vdefault_eol_code), &n)
      || !exact_valid_eol_code_p (n))
    n = eol_crlf;
  b_eol_code = eol_code (n);

  lchar_encoding = symbol_value_char_encoding (Vdefault_fileio_encoding);

  b_selection_type = SELECTION_VOID;
  b_selection_point = NO_MARK_SET;
  b_selection_marker = NO_MARK_SET;
  b_selection_column = 0;

  b_reverse_temp = SELECTION_VOID;
  b_reverse_region.p1 = NO_MARK_SET;
  b_reverse_region.p2 = NO_MARK_SET;

  b_truncated = 0;

  b_modified = 0;
  b_modified_count = 0;
  b_modified_region.p1 = -1;
  b_last_modified = -1;

  b_need_auto_save = 0;
  b_done_auto_save = 0;
  b_make_backup = 0;
  b_buffer_name_modified = 1;
  b_buffer_bar_modified = 0;
  b_buffer_bar_fg = COLORREF (-1);
  b_buffer_bar_bg = COLORREF (-1);

  b_hlock = INVALID_HANDLE_VALUE;

  b_narrow_depth = 0;
  b_last_narrow_depth = 0;

  lvar = Qnil;
  lmap = Qnil;
  lminor_map = Qnil;
  lsyntax_table = xsymbol_value (Vdefault_syntax_table);
  lbuffer_name = name;
  ldirectory = (stringp (dirname)
                ? dirname
                : (selected_buffer ()
                   ? selected_buffer ()->ldirectory
                   : xsymbol_value (Qdefault_dir)));
  lfile_name = filename;
  lalternate_file_name = Qnil;
  lmarkers = Qnil;
  lminibuffer_buffer = Qnil;
  lminibuffer_default = Qnil;
  ldialog_title = Qnil;
  lcomplete_type = Qnil;
  lcomplete_list = Qnil;
  lprocess = Qnil;
  lmenu = Qnil;
  lwaitobj_list = Qnil;
  lkinsoku_bol_chars = Qnil;
  lkinsoku_eol_chars = Qnil;
  b_kinsoku_mode = KINSOKU_DEFAULT;
  b_kinsoku_extend_limit = -1;
  b_kinsoku_shorten_limit = -1;

  b_minibufferp = 0;
  b_prompt = 0;
  b_prompt_length = 0;
  b_prompt_columns = 0;
  *b_prompt_arg = 0;

  b_undo = 0;
  b_redo = 0;
  b_undo_count = 0;

  b_excursion = 0;
  b_restriction = 0;

  b_colors_enable = 0;

  b_fold_mode = FOLD_DEFAULT;
  b_fold_columns = Buffer::FOLD_NONE;
  init_fold_width (b_default_fold_mode);

  b_hjump_columns = -1;

  b_linenum_mode = LNMODE_DEFAULT;

  b_ime_mode = kbd_queue::IME_MODE_OFF;

  b_wflags = 0;
  b_wflags_mask = -1;

  b_stream_cache.p_chunk = 0;
  b_post_modified_hook_enabled = 0;
  b_in_post_modified_hook = 0;

  if (!temporary)
    {
      b_create_count = b_total_create_count++;
      link_list ();
      dlist_force_add_tail ();
      xbuffer_bp (lbp) = this;
      if (!internal_buffer_p ())
        buffer_bar_created ();
    }
}

Buffer *
Buffer::create_buffer (lisp name, lisp filename, lisp dirname)
{
  Buffer *bp = new Buffer (name, filename, dirname);
  if (selected_buffer ())
    selected_buffer ()->run_hook (Vcreate_buffer_hook, bp->lbp);
  return bp;
}

void
Buffer::erase ()
{
  Chunk *cp = b_chunkb;
  b_chunkb = cp->c_next;
  cp->c_prev = 0;
  cp->c_next = 0;
  cp->clear ();
  delete_contents ();

  b_chunkb = cp;
  b_chunke = cp;

  b_textprop_heap.free_all ();
  b_textprop = 0;
  b_textprop_cache = 0;

  b_nchars = 0;
  b_nlines = 1;
  b_nfolded = 1;

  b_contents.p1 = 0;
  b_contents.p2 = 0;

  b_point = 0;
  if (b_mark != NO_MARK_SET)
    b_mark = 0;

  b_disp = 0;

  if (b_selection_point != NO_MARK_SET)
    b_selection_point = 0;
  if (b_selection_marker != NO_MARK_SET)
    b_selection_marker = 0;
  b_selection_column = 0;

  if (b_reverse_region.p1 != NO_MARK_SET)
    {
      b_reverse_region.p1 = 0;
      b_reverse_region.p2 = 0;
    }

  b_truncated = 0;

  b_modified = 0;
  b_modified_region.p1 = -1;
  b_last_modified = -1;
  maybe_modify_buffer_bar ();

  b_need_auto_save = 0;
  b_done_auto_save = 0;
  b_make_backup = 0;

  b_modtime.clear ();

  unlock_file ();

  b_narrow_depth = 0;
  b_last_narrow_depth = 0;

  b_prompt = 0;
  b_prompt_length = 0;
  b_prompt_columns = 0;
  *b_prompt_arg = 0;

  b_undo = 0;
  b_redo = 0;
  b_undo_count = 0;

  b_excursion = 0;
  b_restriction = 0;

  b_stream_cache.p_chunk = 0;

  for (lisp x = lmarkers; consp (x); x = xcdr (x))
    if (xmarker_point (xcar (x)) != NO_MARK_SET)
      xmarker_point (xcar (x)) = 0;

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      {
        wp->w_last_bufp = 0;
        wp->w_bufp = 0;
        wp->set_buffer (this);
      }

  for (WindowConfiguration *wc = WindowConfiguration::wc_chain; wc; wc = wc->wc_prev)
    for (int i = 0; i < wc->wc_nwindows; i++)
      if (wc->wc_data[i].bufp == this)
        {
          wc->wc_data[i].point = 0;
          wc->wc_data[i].disp = 0;
          if (wc->wc_data[i].mark != NO_MARK_SET)
            wc->wc_data[i].mark = 0;
          if (wc->wc_data[i].selection_point != NO_MARK_SET)
            wc->wc_data[i].selection_point = 0;
          if (wc->wc_data[i].selection_marker != NO_MARK_SET)
            wc->wc_data[i].selection_marker = 0;
          if (wc->wc_data[i].reverse_region.p1 != NO_MARK_SET)
            {
              wc->wc_data[i].reverse_region.p1 = 0;
              wc->wc_data[i].reverse_region.p2 = 0;
            }
        }
}

void
Buffer::delete_contents ()
{
  for (save_excursion *se = b_excursion; se; se = se->prev ())
    se->invalid ();
  for (save_restriction *sr = b_restriction; sr; sr = sr->prev ())
    sr->invalid ();

  clear_undo_info ();
  free_all_chunks (b_chunkb);
}

Buffer::~Buffer ()
{
  for (WindowConfiguration *wc = WindowConfiguration::wc_chain; wc; wc = wc->wc_prev)
    for (int i = 0; i < wc->wc_nwindows; i++)
      if (wc->wc_data[i].bufp == this)
        wc->wc_data[i].bufp = 0;

  enum_buffer::deleted (this);
  buffer_bar::buffer_deleted (this);

  cleanup_waitobj_list ();

  unlock_file ();
  if (bufferp (lbp))
    unlink_list ();
  dlist_remove ();
  delete_all_markers ();
  delete_contents ();
  if (bufferp (lbp))
    xbuffer_bp (lbp) = 0;
}

void
Buffer::delete_all_markers ()
{
  for (lisp p = lmarkers; consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      xmarker_buffer (x) = 0;
      xmarker_point (x) = NO_MARK_SET;
    }
  lmarkers = Qnil;
}

void
Buffer::link_list ()
{
  if (!b_blist)
    {
      b_prev = b_next = 0;
      b_blist = this;
      b_version = 1;
    }
  else
    {
      long ver = 1;
      Buffer *bp;
      for (bp = b_blist;; bp = bp->b_next)
        {
          int f = bcmp (xstring_contents (lbuffer_name),
                        xstring_contents (bp->lbuffer_name),
                        min (xstring_length (lbuffer_name),
                             xstring_length (bp->lbuffer_name)));
          if (!f)
            f = xstring_length (lbuffer_name) - xstring_length (bp->lbuffer_name);
          if (f < 0)
            break;
          if (!f)
            {
              if (ver < bp->b_version)
                break;
              ver = bp->b_version + 1;
            }

          if (!bp->b_next)
            {
              bp->b_next = this;
              b_prev = bp;
              b_next = 0;
              b_version = ver;
              return;
            }
        }

      b_prev = bp->b_prev;
      b_next = bp;
      if (!b_prev)
        b_blist = this;
      else
        b_prev->b_next = this;
      bp->b_prev = this;
      b_version = ver;
    }
}

void
Buffer::unlink_list () const
{
  if (!b_prev)
    b_blist = b_next;
  else
    b_prev->b_next = b_next;
  if (b_next)
    b_next->b_prev = b_prev;
}

void
create_default_buffers ()
{
  Buffer *bp = Buffer::create_buffer (make_string ("*scratch*"), Qnil, Qnil);
  Buffer::create_buffer (make_string (" *Minibuf0*"), Qnil, Qnil);
  selected_window ()->set_buffer (bp);
}

Buffer *
Buffer::find_buffer (const Char *name, int l, long version)
{
  Buffer *bp;
  for (bp = b_blist; bp; bp = bp->b_next)
    if (xstring_length (bp->lbuffer_name) == l
        && !bcmp (xstring_contents (bp->lbuffer_name), name, l)
        && (version == -1 || version == bp->b_version))
      break;
  return bp;
}

Buffer *
Buffer::find_buffer (lisp name, long version, int do_parse)
{
  if (!xstring_length (name))
    return 0;

  const Char *p0 = xstring_contents (name);

  Buffer *bp = find_buffer (p0, xstring_length (name), version);
  if (bp || version != -1 || !do_parse)
    return bp;

  const Char *p = p0 + xstring_length (name) - 1;
  if (*p != '>')
    return 0;

  for (p--; p > p0 && digit_char_p (*p); p--)
    ;
  if (p == p0 || *p != '<')
    return 0;

  int l = p - p0;
  for (version = 0, p++; *p != '>'; p++)
    version = version * 10 + *p - '0';

  return find_buffer (p0, l, version);
}

Buffer *
Buffer::coerce_to_buffer (lisp object)
{
  if (!object || object == Qnil)
    return selected_buffer ();

  Buffer *bp;
  if (stringp (object))
    {
      bp = find_buffer (object, -1, 1);
      if (!bp)
        FEsimple_error (Eno_such_buffer, object);
    }
  else
    {
      check_buffer (object);
      bp = xbuffer_bp (object);
      if (!bp)
        FEsimple_error (Edeleted_buffer);
    }
  return bp;
}

Buffer *
Buffer::make_internal_buffer (const char *bufname)
{
  lisp name = make_string (bufname);
  Buffer *bp = Buffer::find_buffer (name, 1, 0);
  if (bp)
    bp->erase ();
  else
    bp = create_buffer (name, Qnil, Qnil);

  bp->b_ime_mode = kbd_queue::IME_MODE_OFF;
  bp->set_local_variable (Vbuffer_read_only, Qnil);
  bp->set_local_variable (Vkept_undo_information, Qnil);
  bp->set_local_variable (Vneed_not_save, Qt);
  bp->set_local_variable (Vauto_save, Qnil);
  bp->set_local_variable (Slock_file, Qnil);

  return bp;
}

lisp
Ferase_buffer (lisp buffer)
{
  Buffer::coerce_to_buffer (buffer)->erase ();
  return Qt;
}

lisp
Fbuffer_size (lisp buffer)
{
  return make_fixnum (Buffer::coerce_to_buffer (buffer)->b_nchars);
}

lisp
Fbuffer_lines (lisp buffer)
{
  return make_fixnum (Buffer::coerce_to_buffer (buffer)->count_lines ());
}

lisp
Fcreate_new_buffer (lisp buffer_name)
{
  check_string (buffer_name);
  if (!xstring_length (buffer_name))
    FEsimple_error (Eempty_buffer_names_not_allowed);
  if (xstring_length (buffer_name) >= BUFFER_NAME_MAX)
    FEsimple_error (Ebuffer_name_too_long, buffer_name);
  return Buffer::create_buffer (buffer_name, Qnil, Qnil)->lbp;
}

lisp
Fselected_buffer ()
{
  return selected_buffer ()->lbp;
}

lisp
Fdeleted_buffer_p (lisp lbuffer)
{
  check_buffer (lbuffer);
  return boole (!xbuffer_bp (lbuffer));
}

Buffer *
Buffer::next_buffer (int internal_p)
{
  Buffer *bp = this;
  do
    {
      bp = bp->b_next ? bp->b_next : Buffer::b_blist;
      if (bp == this)
        break;
    }
  while (!internal_p && bp->internal_buffer_p ());
  return bp;
}

Buffer *
Buffer::prev_buffer (int internal_p)
{
  Buffer *bp = this;
  do
    {
      if (!bp->b_prev)
        for (; bp->b_next; bp = bp->b_next)
          ;
      else
        bp = bp->b_prev;
      if (bp == this)
        break;
    }
  while (!internal_p && bp->internal_buffer_p ());
  return bp;
}

lisp
Fget_next_buffer (lisp buffer, lisp prev, lisp tab_order, lisp linternal_p)
{
  int internal_p = linternal_p && linternal_p != Qnil;
  int tab_order_p = tab_order && tab_order != Qnil;
  Buffer *bp = 0;
  if (buffer == Ktop)
    {
      if (tab_order_p)
        bp = buffer_bar::get_top_buffer ();
      if (!bp)
        {
          bp = Buffer::b_blist;
          if (!internal_p && bp->internal_buffer_p ())
            bp = bp->next_buffer (internal_p);
        }
    }
  else if (buffer == Kbottom)
    {
      if (tab_order_p)
        bp = buffer_bar::get_bottom_buffer ();
      if (!bp)
        bp = Buffer::b_blist->prev_buffer (internal_p);
    }
  else
    {
      Buffer *obp = Buffer::coerce_to_buffer (buffer);
      bp = (tab_order_p
            ? (!prev || prev == Qnil
               ? buffer_bar::next_buffer (obp)
               : buffer_bar::prev_buffer (obp))
            : 0);
      if (!bp)
        bp = (!prev || prev == Qnil
              ? obp->next_buffer (internal_p)
              : obp->prev_buffer (internal_p));
    }
  return bp->lbp;
}

lisp
Fset_buffer (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  Window *wp = selected_window ();
  if (wp->minibuffer_window_p ())
    FEsimple_error (Ecannot_switch_in_minibuffer_window);
  wp->set_buffer (bp);
  return Qt;
}

lisp
Fget_file_buffer (lisp filename)
{
  filename = Fmerge_pathnames (filename, selected_buffer ()->ldirectory);
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (!bp->b_minibufferp && stringp (bp->lfile_name)
        && string_equalp (bp->lfile_name, filename))
      return bp->lbp;
  return Qnil;
}

lisp
Fcreate_file_buffer (lisp name)
{
  lisp filename = Fmerge_pathnames (name, selected_buffer ()->ldirectory);
  Fcheck_valid_pathname (filename);
  if (Ffile_directory_p (filename) != Qnil)
    file_error (Eis_a_directory, name);
  return Buffer::create_buffer (Ffile_namestring (filename), filename,
                                Fdirectory_namestring (filename))->lbp;
}

char *
Buffer::buffer_name (char *b, char *be) const
{
  b = w2s (b, be, lbuffer_name);
  if (b >= be - 1 || b_version == 1)
    return b;
  char t[64];
  sprintf (t, "<%d>", b_version);
  return stpncpy (b, t, be - b);
}

char *
Buffer::quoted_buffer_name (char *b, char *be, int qc, int qe) const
{
  b = w2s_quote (b, be, lbuffer_name, qc, qe);
  if (b >= be - 1 || b_version == 1)
    return b;
  char t[64];
  sprintf (t, "<%d>", b_version);
  return stpncpy (b, t, be - b);
}

void
Buffer::modify_mode_line () const
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      wp->w_disp_flags |= Window::WDF_MODELINE;
}

lisp
Fset_buffer_modified_p (lisp flag, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  bp->b_modified = bp->b_need_auto_save = flag != Qnil;
  bp->modify_mode_line ();
  Buffer::maybe_modify_buffer_bar ();
  if (!bp->b_modified)
    bp->save_modtime_undo (bp->b_modtime);
  if (!bp->b_modified && symbol_value (Slock_file, bp) == Kedit)
    bp->unlock_file ();
  return Qt;
}

lisp
Fbuffer_modified_p (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (bp->b_modified)
    return make_fixnum (bp->b_modified_count & LSHORT_INT_MAX);
  return Qnil;
}

lisp
Fbuffer_modified_count (lisp buffer)
{
  return make_fixnum (Buffer::coerce_to_buffer (buffer)->b_modified_count
                      & LSHORT_INT_MAX);
}

lisp
Fset_buffer_truncated_p (lisp flag, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  bp->b_truncated = flag != Qnil;
  bp->modify_mode_line ();
  return Qt;
}

lisp
Fbuffer_truncated_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->b_truncated);
}

lisp
Ffile_visited_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->lfile_name != Qnil);
}

void
Buffer::dlist_remove ()
{
  if (b_dlist == this)
    b_dlist = b_ldisp;
  else
    for (Buffer *d = b_dlist; d; d = d->b_ldisp)
      if (d->b_ldisp == this)
        {
          d->b_ldisp = b_ldisp;
          break;
        }
  b_ldisp = 0;
}

void
Buffer::dlist_add_head ()
{
  if (!internal_buffer_p () && b_dlist != this)
    {
      dlist_remove ();
      b_ldisp = b_dlist;
      b_dlist = this;
    }
}

void
Buffer::dlist_force_add_tail ()
{
  if (!internal_buffer_p ())
    {
      dlist_remove ();
      if (!b_dlist)
        b_dlist = this;
      else
        {
          Buffer *d;
          for (d = b_dlist; d->b_ldisp; d = d->b_ldisp)
            ;
          d->b_ldisp = this;
        }
    }
}

void
Buffer::dlist_add_tail ()
{
  if (b_ldisp)
    dlist_force_add_tail ();
}

static int
in_pseudo_frame_p (Buffer *bp)
{
  if (xsymbol_function (Vbuffer_in_any_pseudo_frame_p) == Qunbound
      || xsymbol_function (Vbuffer_in_any_pseudo_frame_p) == Qnil)
    return 0;

  suppress_gc sgc;
  try
    {
      return funcall_1 (Vbuffer_in_any_pseudo_frame_p, bp->lbp) != Qnil;
    }
  catch (nonlocal_jump &)
    {
    }
  return 0;
}

Buffer *
Buffer::dlist_find ()
{
  for (Buffer *bp = b_dlist; bp; bp = bp->b_ldisp)
    if (Fget_buffer_window (bp->lbp, Qnil) == Qnil
        && !in_pseudo_frame_p (bp))
      return bp;
  if (b_dlist && b_dlist->b_ldisp)
    return b_dlist->b_ldisp;
  Buffer *bp = selected_buffer ();
  if (!bp->internal_buffer_p ())
    return bp;
  return bp->next_buffer (0);
}

lisp
Fdelete_buffer (lisp buffer)
{
  if (bufferp (buffer) && !xbuffer_bp (buffer))
    return Qnil;

  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (bp->b_minibufferp)
    return Qnil;

  if (bp->run_hook_while_success (Vbefore_delete_buffer_hook, bp->lbp) == Qnil)
    return Qnil;

  if (buffer_has_process (bp))
    FEsimple_error (Ebuffer_has_subprocess);

  if (bp->run_hook_while_success (Vdelete_buffer_hook, bp->lbp) == Qnil)
    return Qnil;

  if (bp == Buffer::b_last_title_bar_buffer)
    Buffer::b_last_title_bar_buffer = 0;

  bp->dlist_add_tail ();
  Buffer *newbp = Buffer::dlist_find ();
  if (bp == newbp)
    newbp = newbp->b_ldisp;
  if (!newbp)
    newbp = Buffer::b_dlist;
  if (!newbp || bp == newbp)
    {
      bp->erase ();
      bp->lfile_name = Qnil;
      bp->lalternate_file_name = Qnil;
      Frename_buffer (make_string ("*scratch*"), bp->lbp);
      if (bp->read_only_p ())
        bp->set_local_variable (Vbuffer_read_only, Qnil);
      suppress_gc sgc;
      Ffuncall (xsymbol_value (Vinitial_buffer_mode), Qnil);
      return Qnil;
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == bp)
      {
        wp->w_last_bufp = 0;
        wp->w_bufp = 0;
        wp->set_buffer (newbp);
      }

  delete bp;
  return Qt;
}

lisp
Fbuffer_name (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (bp->b_version == 1)
    return bp->lbuffer_name;

  Char buf[BUFFER_NAME_MAX * 2];
  bcopy (xstring_contents (bp->lbuffer_name), buf, xstring_length (bp->lbuffer_name));
  char v[64];
  sprintf (v, "<%d>", bp->b_version);
  Char *be = s2w (buf + xstring_length (bp->lbuffer_name), v);
  return make_string (buf, be - buf);
}

lisp
Ffind_buffer (lisp name)
{
  check_string (name);
  Buffer *bp = Buffer::find_buffer (name, -1, 1);
  return bp ? bp->lbp : Qnil;
}

lisp
Fother_buffer (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  Buffer *nbp = Buffer::dlist_find ();
  if (bp == nbp)
    {
      bp->dlist_add_tail ();
      nbp = Buffer::dlist_find ();
    }
  return nbp->lbp;
}

lisp
Fbury_buffer (lisp buffer)
{
  if (!buffer || buffer == Qnil)
    selected_window ()->set_buffer (xbuffer_bp (Fother_buffer (0)));
  Buffer::coerce_to_buffer (buffer)->dlist_add_tail ();
  return Qnil;
}

lisp
Fset_buffer_file_name (lisp name, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (name == Qnil)
    {
      if (bp->lfile_name == name)
        return Qt;
      bp->unlock_file ();
      bp->lfile_name = name;
    }
  else
    {
      lisp filename = Fmerge_pathnames (name, bp->ldirectory);
      if (string_equalp (filename, bp->lfile_name))
        return Qt;
      Fcheck_valid_pathname (filename);
      if (Ffile_directory_p (filename) != Qnil)
        file_error (Eis_a_directory, name);
      lisp dir = Fdirectory_namestring (filename);
      if (bp->file_locked_p ())
        bp->lock_file (filename, 1);
      bp->ldirectory = dir;
      bp->lfile_name = filename;
    }
  bp->modify_mode_line ();
  bp->update_modtime ();
  bp->b_buffer_name_modified = 1;
  return Qt;
}

lisp
Fget_buffer_file_name (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lfile_name;
}

lisp
Fset_buffer_alternate_file_name (lisp name, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (name != Qnil)
    check_string (name);
  bp->lalternate_file_name = name;
  return Qt;
}

lisp
Fget_buffer_alternate_file_name (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lalternate_file_name;
}

lisp
Fdefault_directory (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->ldirectory;
}

lisp
Fset_default_directory (lisp dir, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  lisp d = Fmerge_pathnames (dir, bp->ldirectory);
  if (Ffile_directory_p (d) == Qnil)
    file_error (Enot_a_directory, dir);
  bp->ldirectory = Fappend_trail_slash (d);
  bp->modify_mode_line ();
  return Qt;
}

lisp
Frename_buffer (lisp name, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  check_string (name);
  if (!xstring_length (name))
    FEsimple_error (Eempty_buffer_names_not_allowed);
  if (xstring_length (name) >= BUFFER_NAME_MAX)
    FEsimple_error (Ebuffer_name_too_long, name);
  if (bp->b_minibufferp)
    return Qnil;
  bp->unlink_list ();
  bp->lbuffer_name = name;
  bp->link_list ();
  bp->modify_mode_line ();
  bp->b_buffer_name_modified = 1;
  bp->modify_buffer_bar ();
  return Qt;
}

lisp
Fbuffer_fileio_encoding (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lchar_encoding;
}

lisp
Fbuffer_eol_code (lisp buffer)
{
  return make_fixnum (Buffer::coerce_to_buffer (buffer)->b_eol_code);
}

lisp
Fset_buffer_fileio_encoding (lisp encoding, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  check_char_encoding (encoding);
  if (xchar_encoding_type (encoding) == encoding_auto_detect)
    FEtype_error (encoding, Qchar_encoding);
  bp->lchar_encoding = encoding;
  bp->modify_mode_line ();
  return Qt;
}

lisp
Fset_buffer_eol_code (lisp code, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  int n = fixnum_value (code);
  if (!valid_eol_code_p (n))
    FErange_error (code);
  bp->b_eol_code = exact_eol_code (n);
  bp->modify_mode_line ();
  return Qt;
}

lisp
Fbuffer_list (lisp keys)
{
  if (find_keyword_bool (Kbuffer_bar_order, keys))
    {
      lisp r = buffer_bar::list_buffers ();
      if (r)
        return r;
    }

  Buffer *bp = Buffer::b_blist;
  if (!bp)
    return Qnil;
  lisp result = xcons (bp->lbp, Qnil);
  lisp p = result;
  for (bp = bp->b_next; bp; bp = bp->b_next)
    {
      xcdr (p) = xcons (bp->lbp, Qnil);
      p = xcdr (p);
    }
  return result;
}

lisp
Fenum_buffers (lisp fn)
{
  Buffer *bp;
  enum_buffer e;
  while ((bp = e.next ()))
    {
      lisp r = funcall_1 (fn, bp->lbp);
      if (r != Qnil)
        return r;
    }
  return Qnil;
}

lisp
Ffind_name_buffer (lisp name)
{
  check_string (name);
  lisp result = Qnil;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (xstring_length (bp->lbuffer_name) == xstring_length (name)
        && !bcmp (xstring_contents (bp->lbuffer_name),
                  xstring_contents (name),
                  xstring_length (name)))
      result = xcons (bp->lbp, result);
  return result;
}

lisp
Fneed_buffer_save_p (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  return boole (bp->b_modified
                && !bp->internal_buffer_p ()
                && symbol_value (Vneed_not_save, bp) == Qnil);
}

int
Buffer::count_modified_buffers ()
{
  int n = 0;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (Fneed_buffer_save_p (bp->lbp) != Qnil)
      n++;
  return n;
}

lisp
Fcount_modified_buffers ()
{
  return make_fixnum (Buffer::count_modified_buffers ());
}

lisp
Fcount_buffers (lisp all)
{
  int n = 0;
  if (!all)
    all = Qnil;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (all != Qnil || !bp->internal_buffer_p ())
      n++;
  return make_fixnum (n);
}

static int
call_hooks (lisp hook)
{
  Buffer *bp;
  enum_buffer e;
  while ((bp = e.next ()))
    {
      try
        {
          if (bp->run_hook_while_success (hook, bp->lbp) == Qnil)
            return 0;
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
          return 0;
        }
    }
  return 1;
}

int
Buffer::query_kill_xyzzy ()
{
  if (app.kbdq.disablep ())
    return 0;
  if (!call_hooks (Vbefore_delete_buffer_hook))
    return 0;
  if (!query_kill_subprocesses ())
    return 0;
  if (!call_hooks (Vdelete_buffer_hook))
    return 0;
#if 0
  if (!count_modified_buffers ())
    return 1;
#endif
  try
    {
      lisp hook = xsymbol_value (Vquery_kill_xyzzy_hook);
      if (hook != Qnil && hook != Qunbound)
        {
          dynamic_bind dyn (Vquery_kill_xyzzy_hook, Qnil);
          if (call_hook_nargs (hook, Qnil, Qt) == Qnil)
            return 0;
        }
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
      return 0;
    }

  int n = count_modified_buffers ();
  if (!n)
    return 1;
  return format_yes_or_no_p (Mmodified_buffers_exist, n);
}

int
Buffer::kill_xyzzy (int query)
{
  if (query && !query_kill_xyzzy ())
    return 0;
  Filer::close_mlfiler ();
  selected_buffer ()->safe_run_hook (Vkill_xyzzy_hook, 1);
  PostQuitMessage (0);
  return 1;
}

lisp
Fkill_xyzzy (lisp lexit_code)
{
  if (!lexit_code)
    lexit_code = Qt;
  if (lexit_code == Qt)
    app.exit_code = EXIT_SUCCESS;
  else if (lexit_code == Qnil)
    app.exit_code = EXIT_FAILURE;
  else
    app.exit_code = fixnum_value (lexit_code);

  if (Buffer::kill_xyzzy (1))
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      nld->type = Qexit_this_level;
      nld->value = Qnil;
      nld->tag = Qnil;
      nld->id = xsymbol_value (Vierror_silent_quit);
      throw nonlocal_jump ();
    }
  return Qnil;
}

char *
Buffer::store_title (lisp x, char *b, char *be) const
{
  if (x == lbuffer_name)
    return buffer_name (b, be);
  return w2s (b, x);
}

void
Buffer::refresh_title_bar () const
{
  lisp fmt = symbol_value (Vtitle_bar_format, this);
  if (stringp (fmt))
    {
      char buf[512 + 10];
      buffer_info binfo (0, this, 0, 0, 0);
      *binfo.format (fmt, buf, buf + 512) = 0;
      SetWindowText (app.toplev, buf);
    }
  else
    {
      lisp x;
      if (stringp (lfile_name))
        x = lfile_name;
      else if (stringp (lalternate_file_name))
        x = lalternate_file_name;
      else
        x = lbuffer_name;

      int l = (xstring_length (x) * 2 + strlen (TitleBarString) + 32 + 8);
      char *b0 = (char *)alloca (l);
      char *b = b0;
      if (Fadmin_user_p () == Qt && sysdep.Win6p ())
        b = stpcpy (b, "管理者: ");
      if (xsymbol_value (Vtitle_bar_text_order) != Qnil)
        strcpy (stpcpy (store_title (x, b, b + l), " - "), TitleBarString);
      else
        store_title (x, stpcpy (stpcpy (b, TitleBarString), " - "), b + l);

      SetWindowText (app.toplev, b0);
    }
  b_last_title_bar_buffer = 0; // 次回タイトルバーを強制的に再描画させる
}

void
Buffer::set_frame_title (int update)
{
  Window *wp = selected_window ();
  int order = xsymbol_value (Vtitle_bar_text_order) != Qnil;
  if (!wp->minibuffer_window_p ()
      && (update
          || b_buffer_name_modified
          || b_last_title_bar_buffer != this
          || b_title_bar_text_order != order))
    {
      refresh_title_bar ();
      b_buffer_name_modified = 0;
      b_last_title_bar_buffer = this;
      b_title_bar_text_order = order;
    }
}

lisp
Frefresh_title_bar ()
{
  Window *wp = selected_window ();
  Buffer *bp = selected_buffer ();
  if (!wp->minibuffer_window_p ())
    bp->refresh_title_bar ();
  return Qt;
}

void
Buffer::change_colors (const XCOLORREF *cc)
{
  if (cc)
    {
      if (b_colors_enable)
        {
          int i;
          for (i = 0; i < USER_DEFINABLE_COLORS; i++)
            if (b_colors[i] != cc[i])
              break;
          if (i == USER_DEFINABLE_COLORS)
            return;
        }
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        b_colors[i] = cc[i];
      b_colors_enable = 1;
    }
  else
    {
      if (!b_colors_enable)
        return;
      b_colors_enable = 0;
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      wp->change_color ();
}

lisp
Fset_buffer_colors (lisp lcolors, lisp lbuffer)
{
  if (lcolors == Qnil)
    Buffer::coerce_to_buffer (lbuffer)->change_colors (0);
  else
    {
      XCOLORREF cc[USER_DEFINABLE_COLORS];
      check_general_vector (lcolors);
      memcpy (cc, Window::default_xcolors, sizeof cc);
      int n = min (USER_DEFINABLE_COLORS, xvector_length (lcolors));
      for (int i = 0; i < n; i++)
        cc[i] = fixnum_value (xvector_contents (lcolors) [i]);
      Buffer::coerce_to_buffer (lbuffer)->change_colors (cc);
    }
  return Qt;
}

lisp
Fget_buffer_colors (lisp lbuffer)
{
  lisp v = make_vector (USER_DEFINABLE_COLORS, Qnil);
  Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
  if (bp->b_colors_enable)
    {
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        xvector_contents (v) [i] = make_fixnum (bp->b_colors[i]);
    }
  else
    {
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        xvector_contents (v) [i] = make_fixnum (Window::default_xcolors[i]);
    }
  return v;
}

void
change_local_colors (const XCOLORREF *cc, int dir, int subdir)
{
  selected_buffer ()->change_colors (cc);
  if (dir)
    {
      lisp hook = xsymbol_value (Vchange_buffer_colors_hook);
      if (hook != Qunbound && hook != Qnil)
        {
          try
            {
              lisp v;
              if (!cc)
                v = Qnil;
              else
                {
                  v = make_vector (USER_DEFINABLE_COLORS, Qnil);
                  for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
                    xvector_contents (v) [i] = make_fixnum (cc[i]);
                }
              funcall_3 (hook, selected_buffer ()->lbp, v, boole (subdir));
            }
          catch (nonlocal_jump &)
            {
              print_condition (nonlocal_jump::data ());
            }
        }
    }
}

void
Buffer::refresh_buffer () const
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      wp->w_disp_flags |= Window::WDF_WINDOW;
}

void
Buffer::window_size_changed ()
{
  if (b_fold_mode == FOLD_DEFAULT
      ? b_default_fold_mode == FOLD_WINDOW
      : b_fold_mode == FOLD_WINDOW)
    init_fold_width (FOLD_WINDOW);
}

void
Buffer::fold_width_modified ()
{
  b_nfolded = -1;
  for (Chunk *cp = b_chunkb; cp; cp = cp->c_next)
    cp->c_nbreaks = -1;
}

void
Buffer::init_fold_width (int w)
{
  if (w == FOLD_WINDOW)
    {
      w = -1;
      for (const Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
        if (wp->w_bufp == this)
          {
            int cx = wp->w_ech.cx;
            if (wp->flags () & Window::WF_LINE_NUMBER)
              cx -= Window::LINENUM_COLUMNS + 1;
            if (wp->flags () & Window::WF_FOLD_MARK)
              cx--;
            w = max (w, cx);
          }
      if (w == -1)
        return;
      if (w < MIN_FOLD_WIDTH)
        w = MIN_FOLD_WIDTH;
      else if (w > MAX_FOLD_WIDTH)
        w = MAX_FOLD_WIDTH;
    }
  if (b_fold_columns != w)
    {
      b_fold_columns = w;
      fold_width_modified ();
      refresh_buffer ();
    }
}

static int
fold_width (lisp width)
{
  if (width == Qnil)
    return Buffer::FOLD_NONE;
  if (width == Qt)
    return Buffer::FOLD_WINDOW;
  int w = fixnum_value (width);
  if (w < MIN_FOLD_WIDTH || w > MAX_FOLD_WIDTH)
    FErange_error (width);
  return w;
}

lisp
Fset_default_fold_width (lisp width)
{
  int w = fold_width (width);
  if (w == Buffer::b_default_fold_mode)
    return Qt;
  Buffer::b_default_fold_mode = w;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (bp->b_fold_mode == Buffer::FOLD_DEFAULT
        && !bp->b_minibufferp)
      bp->init_fold_width (w);
  return Qt;
}

lisp
Fset_buffer_fold_width (lisp width, lisp buffer)
{
  int w = fold_width (width);
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (bp->b_minibufferp)
    return Qnil;
  if (bp->b_fold_mode == w)
    return Qt;
  bp->b_fold_mode = w;
  bp->init_fold_width (w);
  return Qt;
}

static lisp
lfold_width (int w)
{
  switch (w)
    {
    case Buffer::FOLD_NONE:
      return Qnil;
    case Buffer::FOLD_WINDOW:
      return Qt;
    default:
      return make_fixnum (w);
    }
}

lisp
Fdefault_fold_width ()
{
  return lfold_width (Buffer::b_default_fold_mode);
}

lisp
Fbuffer_fold_width (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  multiple_value::count () = 2;
  multiple_value::value (1) = boole (bp->b_fold_mode != Buffer::FOLD_DEFAULT);
  return lfold_width (bp->b_fold_mode == Buffer::FOLD_DEFAULT
                      ? Buffer::b_default_fold_mode
                      : bp->b_fold_mode);
}

lisp
Fbuffer_fold_column (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  return (bp->b_fold_columns == Buffer::FOLD_NONE
          ? Qnil : make_fixnum (bp->b_fold_columns));
}

lisp
Fhjump_columns (lisp buffer)
{
  multiple_value::count () = 2;
  if (buffer && buffer != Qnil)
    {
      Buffer *bp = Buffer::coerce_to_buffer (buffer);
      if (bp->b_hjump_columns > 0)
        {
          multiple_value::value (1) = Qt;
          return make_fixnum (bp->b_hjump_columns);
        }
    }
  multiple_value::value (1) = Qnil;
  return make_fixnum (Window::w_hjump_columns);
}

lisp
Fset_hjump_columns (lisp column, lisp buffer)
{
  int n;
  if (column == Qnil)
    n = -1;
  else
    {
      n = fixnum_value (column);
      if (n <= 0 || n > 32)
        FErange_error (column);
    }

  if (!buffer || buffer == Qnil)
    {
      if (n < 0)
        FEtype_error (column, Qinteger);
      Window::w_hjump_columns = n;
    }
  else
    {
      Buffer *bp = Buffer::coerce_to_buffer (buffer);
      bp->b_hjump_columns = n;
    }
  return Qt;
}

lisp
Fdefault_line_number_mode ()
{
  return boole (Buffer::b_default_linenum_mode == Buffer::LNMODE_DISP);
}

lisp
Fbuffer_line_number_mode (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->linenum_mode ()
                == Buffer::LNMODE_DISP);
}

lisp
Fset_default_line_number_mode (lisp lf)
{
  int f = lf == Qnil ? Buffer::LNMODE_LF : Buffer::LNMODE_DISP;
  if (f != Buffer::b_default_linenum_mode)
    {
      Buffer::b_default_linenum_mode = f;
      for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
        if (bp->b_linenum_mode == Buffer::LNMODE_DEFAULT)
          bp->refresh_buffer ();
    }
  return Qt;
}

lisp
Fset_buffer_line_number_mode (lisp lf, lisp buffer)
{
  int f = lf == Qnil ? Buffer::LNMODE_LF : Buffer::LNMODE_DISP;
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (f != bp->b_linenum_mode)
    {
      bp->b_linenum_mode = f;
      bp->refresh_buffer ();
    }
  return Qt;
}

lisp
Fkinsoku_eol_chars (lisp lbuffer)
{
  return (lbuffer && lbuffer != Qnil
          ? Buffer::coerce_to_buffer (lbuffer)->kinsoku_eol_chars ()
          : xsymbol_value (Vdefault_kinsoku_eol_chars));
}

lisp
Fkinsoku_bol_chars (lisp lbuffer)
{
  return (lbuffer && lbuffer != Qnil
          ? Buffer::coerce_to_buffer (lbuffer)->kinsoku_bol_chars ()
          : xsymbol_value (Vdefault_kinsoku_bol_chars));
}

static int
chars_equal (lisp x, lisp y)
{
  if (x == y)
    return 1;
  if (stringp (x) != stringp (y))
    return 0;
  if (!stringp (x))
    return 1;
  const Char *const p0 = xstring_contents (x);
  const Char *const pe = p0 + xstring_length (x);
  const Char *const q0 = xstring_contents (y);
  const Char *const qe = q0 + xstring_length (y);
  const Char *p, *q;
  for (p = p0; p < pe; p++)
    {
      for (q = q0; q < qe && *p != *q; q++)
        ;
      if (q == qe)
        return 0;
    }
  for (q = q0; q < qe; q++)
    {
      for (p = p0; p < pe && *p != *q; p++)
        ;
      if (p == pe)
        return 0;
    }
  return 1;
}

static int __cdecl
compare_Char (const void *p1, const void *p2)
{
  return *(const Char *)p1 - *(const Char *)p2;
}

static lisp
check_kinsoku_chars (lisp string)
{
  if (string == Qnil)
    return string;
  check_string (string);
  int l = xstring_length (string);
  if (l <= 1)
    return string;
  Char *const p0 = (Char *)alloca (sizeof (Char) * l);
  bcopy (xstring_contents (string), p0, l);
  qsort (p0, l, sizeof *p0, compare_Char);
  Char *p = p0, *const pe = p + l, *d = p0;
  *d++ = *p++;
  for (; p < pe; p++)
    if (*p != d[-1])
      *d++ = *p;
  return make_string (p0, d - p0);
}

lisp
Fset_kinsoku_chars (lisp bol, lisp eol, lisp lbuffer)
{
  bol = check_kinsoku_chars (bol);
  eol = check_kinsoku_chars (eol);
  if (lbuffer && lbuffer != Qnil)
    {
      Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
      lisp obol = bp->kinsoku_bol_chars ();
      lisp oeol = bp->kinsoku_eol_chars ();
      bp->lkinsoku_bol_chars = bol;
      bp->lkinsoku_eol_chars = eol;
      if (chars_equal (obol, bol) && chars_equal (oeol, eol))
        return Qt;
      bp->fold_width_modified ();
      bp->refresh_buffer ();
    }
  else
    {
      if (chars_equal (xsymbol_value (Vdefault_kinsoku_bol_chars), bol)
          && chars_equal (xsymbol_value (Vdefault_kinsoku_eol_chars), eol))
        return Qt;
      xsymbol_value (Vdefault_kinsoku_bol_chars) = bol;
      xsymbol_value (Vdefault_kinsoku_eol_chars) = eol;
      for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
        if (!stringp (bp->lkinsoku_bol_chars)
            || !stringp (bp->lkinsoku_eol_chars))
          {
            bp->fold_width_modified ();
            bp->refresh_buffer ();
          }
    }
  return Qt;
}

lisp
Fkinsoku_mode (lisp lbuffer)
{
  return make_fixnum (lbuffer && lbuffer != Qnil
                      ? Buffer::coerce_to_buffer (lbuffer)->kinsoku_mode ()
                      : Buffer::b_default_kinsoku_mode);
}

lisp
Fset_kinsoku_mode (lisp lmode, lisp lbuffer)
{
  if (lbuffer && lbuffer != Qnil)
    {
      Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
      int omode = bp->kinsoku_mode ();
      if (lmode == Qnil)
        bp->b_kinsoku_mode = Buffer::KINSOKU_DEFAULT;
      else
        bp->b_kinsoku_mode = fixnum_value (lmode) & Buffer::KINSOKU_MODE_MASK;
      if (omode != bp->kinsoku_mode ())
        {
          bp->fold_width_modified ();
          bp->refresh_buffer ();
        }
    }
  else
    {
      int mode = fixnum_value (lmode) & Buffer::KINSOKU_MODE_MASK;
      if (mode != Buffer::b_default_kinsoku_mode)
        {
          for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
            if (bp->b_kinsoku_mode == Buffer::KINSOKU_DEFAULT
                && bp->kinsoku_mode () != mode)
              {
                bp->fold_width_modified ();
                bp->refresh_buffer ();
              }
          Buffer::b_default_kinsoku_mode = mode;
        }
    }
  return Qt;
}

lisp
Fkinsoku_extend_limit (lisp lbuffer)
{
  return make_fixnum (lbuffer && lbuffer != Qnil
                      ? Buffer::coerce_to_buffer (lbuffer)->kinsoku_extend_limit ()
                      : Buffer::b_default_kinsoku_extend_limit);
}

lisp
Fkinsoku_shorten_limit (lisp lbuffer)
{
  return make_fixnum (lbuffer && lbuffer != Qnil
                      ? Buffer::coerce_to_buffer (lbuffer)->kinsoku_shorten_limit ()
                      : Buffer::b_default_kinsoku_shorten_limit);
}

static int
kinsoku_limit (lisp lchars)
{
  int nchars = fixnum_value (lchars);
  if (nchars < 0 || nchars >= Buffer::MAX_KINSOKU_LIMIT)
    FErange_error (lchars);
  return nchars;
}

lisp
Fset_kinsoku_extend_limit (lisp lchars, lisp lbuffer)
{
  if (lbuffer && lbuffer != Qnil)
    {
      Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
      int ochars = bp->kinsoku_extend_limit ();
      if (lchars == Qnil)
        bp->b_kinsoku_extend_limit = -1;
      else
        bp->b_kinsoku_extend_limit = kinsoku_limit (lchars);
      if (bp->kinsoku_extend_limit () != ochars)
        {
          bp->fold_width_modified ();
          bp->refresh_buffer ();
        }
    }
  else
    {
      int nchars = kinsoku_limit (lchars);
      if (nchars != Buffer::b_default_kinsoku_extend_limit)
        {
          for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
            if (bp->b_kinsoku_extend_limit < 0
                && bp->kinsoku_extend_limit () != nchars)
              {
                bp->fold_width_modified ();
                bp->refresh_buffer ();
              }
          Buffer::b_default_kinsoku_extend_limit = nchars;
        }
    }
  return Qt;
}

lisp
Fset_kinsoku_shorten_limit (lisp lchars, lisp lbuffer)
{
  if (lbuffer && lbuffer != Qnil)
    {
      Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
      int ochars = bp->kinsoku_shorten_limit ();
      if (lchars == Qnil)
        bp->b_kinsoku_shorten_limit = -1;
      else
        bp->b_kinsoku_shorten_limit = kinsoku_limit (lchars);
      if (bp->kinsoku_shorten_limit () != ochars)
        {
          bp->fold_width_modified ();
          bp->refresh_buffer ();
        }
    }
  else
    {
      int nchars = kinsoku_limit (lchars);
      if (nchars != Buffer::b_default_kinsoku_shorten_limit)
        {
          for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
            if (bp->b_kinsoku_shorten_limit < 0
                && bp->kinsoku_shorten_limit () != nchars)
              {
                bp->fold_width_modified ();
                bp->refresh_buffer ();
              }
          Buffer::b_default_kinsoku_shorten_limit = nchars;
        }
    }
  return Qt;
}

void
Buffer::change_ime_mode ()
{
  if (b_last_selected_buffer != this)
    {
      b_last_selected_buffer = this;
      if (xsymbol_value (Vsave_buffer_ime_mode) != Qnil)
        app.kbdq.toggle_ime (b_ime_mode);
    }
}

lisp
Fupdate_mode_line (lisp lbuffer)
{
  if (lbuffer == Qt)
    Window::modify_all_mode_line ();
  else
    Buffer::coerce_to_buffer (lbuffer)->modify_mode_line ();
  return Qt;
}

lisp
Fbuffer_ime_mode (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->b_ime_mode
                == kbd_queue::IME_MODE_ON);
}

lisp
Fset_buffer_ime_mode (lisp f, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  int omode = bp->b_ime_mode;
  bp->b_ime_mode = (f == Qnil
                    ? kbd_queue::IME_MODE_OFF
                    : kbd_queue::IME_MODE_ON);
  if (bp->b_ime_mode != omode
      && bp == selected_buffer ()
      && xsymbol_value (Vsave_buffer_ime_mode) != Qnil)
    app.kbdq.toggle_ime (bp->b_ime_mode);
  return Qt;
}

lisp
Fenable_post_buffer_modified_hook (lisp f, lisp buffer)
{
  Buffer::coerce_to_buffer (buffer)->b_post_modified_hook_enabled = f != Qnil;
  return Qt;
}

lisp
Fpost_buffer_modified_hook_enabled_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->b_post_modified_hook_enabled);
}
