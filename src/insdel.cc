#include "stdafx.h"
#include "ed.h"
#include "syntaxinfo.h"
#include "sequence.h"
#include "byte-stream.h"

#ifdef DEBUG
void
Buffer::check_valid () const
{
  long nchars = 0;
  for (const Chunk *cp = b_chunkb; cp; cp = cp->c_next)
    {
      nchars += cp->c_used;
      if (!cp->c_next)
        assert (cp == b_chunke);
    }
  assert (nchars == b_nchars);

  nchars = 0;
  for (const Chunk *cp = b_chunke; cp; cp = cp->c_prev)
    {
      nchars += cp->c_used;
      if (!cp->c_prev)
        assert (cp == b_chunkb);
    }
  assert (nchars == b_nchars);
}
#endif /* DEBUG */

void
Buffer::check_read_only () const
{
  if (read_only_p ())
    FEread_only_buffer ();
}

void
Buffer::prepare_modify_buffer ()
{
  if (b_in_post_modified_hook)
    FEread_only_buffer ();

  if (!b_modified && !verify_modtime ()
      && !yes_or_no_p (Mfile_has_changed_on_disk))
    FEplain_error (Eedit_canceled);

  if (!file_locked_p () && symbol_value (Slock_file, this) != Qnil
      && lock_file () == Kshared && !yes_or_no_p (Mfile_has_already_locked))
    {
      unlock_file ();
      FEplain_error (Eedit_canceled);
    }
}

void
Buffer::modify ()
{
  if (!b_modified)
    {
      b_modified = 1;
      modify_mode_line ();
      maybe_modify_buffer_bar ();
    }
  b_modified_count++;
  b_nlines = -1;
  b_need_auto_save = 1;
  b_nfolded = -1;
  b_stream_cache.p_chunk = 0;
}

void
Buffer::set_modified_region (point_t p1, point_t p2)
{
  b_last_modified = p1;
  if (b_modified_region.p1 == -1)
    {
      b_modified_region.p1 = p1;
      b_modified_region.p2 = p2;
    }
  else
    {
      b_modified_region.p1 = min (b_modified_region.p1, p1);
      b_modified_region.p2 = max (b_modified_region.p2, p2);
    }
}

void
Buffer::modify_chunk (Chunk *cp) const
{
  cp->c_nlines = -1;
  cp->c_nbreaks = -1;
  cp->c_bstate = syntax_state::SS_INVALID;
}

void
Buffer::prepare_modify_region (Window *wp, point_t p1, point_t p2)
{
  assert (p1 < p2);
  assert (p1 >= b_contents.p1);
  assert (p2 <= b_contents.p2);

  check_read_only ();

  prepare_modify_buffer ();

  goto_char (wp->w_point, p1);
  if (!save_modify_undo (wp->w_point, p2 - p1))
    FEstorage_error ();

  long size = p2 - p1;
  Chunk *cp = wp->w_point.p_chunk;
  modify_chunk (cp);
  size -= cp->c_used - wp->w_point.p_offset;
  for (cp = cp->c_next; cp && size > 0; cp = cp->c_next)
    {
      modify_chunk (cp);
      size -= cp->c_used;
    }

  modify ();
  set_modified_region (p1, p2);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
}

point_t
Buffer::prepare_modify_region (Window *wp, lisp from, lisp to)
{
  point_t p1 = coerce_to_restricted_point (from);
  point_t p2 = coerce_to_restricted_point (to);
  if (p1 == p2)
    return -1;
  if (p1 > p2)
    swap (p1, p2);
  prepare_modify_region (wp, p1, p2);
  return p2;
}

void
Buffer::set_point (Point &point, point_t goal) const
{
  point.p_point = 0;
  point.p_chunk = b_chunkb;
  point.p_offset = 0;
  goto_char (point, goal);
}

void
Buffer::set_point_no_restrictions (Point &point, point_t goal)
{
  no_restrictions nr (this);
  set_point (point, goal);
}

inline void
move_chunk (Chunk *cp, int src, int dst, int size)
{
  if (src != dst)
    memmove (cp->c_text + dst, cp->c_text + src, sizeof (Char) * size);
}

inline void
move_chunk (const Chunk *sp, int src, Chunk *dp, int dst, int size)
{
  memmove (dp->c_text + dst, sp->c_text + src, sizeof (Char) * size);
}

static void
adjust (Chunk *&chunk, int &offset)
{
  Chunk *cp = chunk;
  int o = offset;
  while (o > cp->c_used)
    {
      o -= cp->c_used;
      cp = cp->c_next;
      assert (cp);
    }
  chunk = cp;
  offset = o;
}

void
copy_chunk (const Char *src, Chunk *dst, int doff, int size)
{
  while (1)
    {
      int n = min (dst->c_used - doff, size);
      bcopy (src, dst->c_text + doff, n);
      size -= n;
      if (!size)
        return;
      src += n;
      doff += n;
      if (doff == dst->c_used)
        {
          dst = dst->c_next;
          doff = 0;
        }
    }
}

void
Buffer::move_before_gap (Point &w_point, int size) const
{
  Chunk *cur = w_point.p_chunk;
  int rest = cur->rest ();
  int n = cur->c_used - w_point.p_offset;
  move_chunk (cur, w_point.p_offset, Chunk::TEXT_SIZE - n, n);
  cur->c_used = Chunk::TEXT_SIZE;
  modify_chunk (cur);

  Chunk *prev = cur->c_prev;
  int pused = prev->c_used;
  prev->c_used += size - rest;
  modify_chunk (prev);
  copy_chunk (cur->c_text, prev, pused, w_point.p_offset);

  pused += w_point.p_offset;
  adjust (prev, pused);
  if (pused == prev->c_used)
    {
      prev = prev->c_next;
      pused = 0;
    }
  w_point.p_chunk = prev;
  w_point.p_offset = pused;
}

static void
adjust_dst (Chunk *&chunk, int &offset)
{
  Chunk *cp = chunk;
  int o = offset;
  while (o > Chunk::TEXT_SIZE)
    {
      o -= Chunk::TEXT_SIZE;
      cp = cp->c_next;
      assert (cp);
    }
  chunk = cp;
  offset = o;
}

static void
copy_chunk_reverse (const Chunk *src, int soff, Chunk *dst, int doff, int size)
{
  if (!size)
    return;
  soff += size;
  doff += size;
  adjust_dst (dst, doff);
  while (1)
    {
      int n = min (soff, doff);
      if (n)
        {
          n = min (n, size);
          soff -= n;
          doff -= n;
          move_chunk (src, soff, dst, doff, n);
          size -= n;
          if (!size)
            return;
        }
      if (!doff)
        {
          dst = dst->c_prev;
          doff = Chunk::TEXT_SIZE;
        }
      if (!soff)
        {
          src = src->c_prev;
          soff = src->c_used;
        }
    }
}

void
Buffer::move_after_gap (Point &w_point, int size) const
{
  Chunk *cp = w_point.p_chunk;
  Chunk *next = cp->c_next;
  int n = size - cp->rest ();
  move_chunk (next, 0, n, next->c_used);
  next->c_used += n;
  modify_chunk (next);
  copy_chunk_reverse (cp, w_point.p_offset, cp, w_point.p_offset + size,
                      cp->c_used - w_point.p_offset);
  cp->c_used = Chunk::TEXT_SIZE;
  modify_chunk (cp);
}

int
Buffer::allocate_new_chunks (Point &w_point, int requested)
{
  Chunk *chunk = w_point.p_chunk;
  int need = requested - chunk->rest ();
  Chunk *prev = chunk;
  Chunk *head = alloc_chunk ();
  for (Chunk *cp = head; cp; cp = cp->c_next)
    {
      cp->c_prev = prev;
      prev = cp;
      if (need > Chunk::TEXT_SIZE)
        {
          cp->c_used = Chunk::TEXT_SIZE;
          need -= Chunk::TEXT_SIZE;
          cp->c_next = alloc_chunk ();
        }
      else
        {
          cp->c_used = need;
          cp->c_next = chunk->c_next;
          if (cp->c_next)
            cp->c_next->c_prev = cp;
          else
            b_chunke = cp;
          chunk->c_next = head;
          copy_chunk_reverse (chunk, w_point.p_offset,
                              chunk, w_point.p_offset + requested,
                              chunk->c_used - w_point.p_offset);
          modify_chunk (chunk);
          chunk->c_used = Chunk::TEXT_SIZE;
          if (w_point.p_offset == chunk->c_used)
            {
              w_point.p_chunk = chunk->c_next;
              w_point.p_offset = 0;
            }
          return 1;
        }
    }

  free_all_chunks (head);
  return 0;
}

int
Buffer::move_gap (Point &w_point, int requested)
{
  Chunk *chunk = w_point.p_chunk;

  int rest = chunk->rest ();
  if (rest >= requested)
    {
      move_chunk (chunk, w_point.p_offset, w_point.p_offset + requested,
                  chunk->c_used - w_point.p_offset);
      chunk->c_used += requested;
      modify_chunk (chunk);
      return 1;
    }

  Chunk *cp = chunk->c_next;
  if (cp && rest + cp->rest () >= requested)
    {
      move_after_gap (w_point, requested);
      return 1;
    }

  cp = chunk->c_prev;
  if (cp && rest + cp->rest () >= requested)
    {
      move_before_gap (w_point, requested);
      return 1;
    }

  return allocate_new_chunks (w_point, requested);
}

void
Buffer::adjust_insertion (const Point &point, int size)
{
  point_t opoint = point.p_point;

#define ADJINS(P) if ((P) > opoint) (P) += size
#define ADJINS2(P) if ((P) >= opoint) (P) += size

  ADJINS (b_point);
  ADJINS (b_mark);
  ADJINS (b_selection_point);
  ADJINS (b_selection_marker);
  ADJINS (b_reverse_region.p1);
  ADJINS (b_reverse_region.p2);
  ADJINS (b_disp);

  textprop_adjust_insertion (opoint, size);

  for (save_excursion *se = b_excursion; se; se = se->prev ())
    ADJINS (se->se_point);

  for (save_restriction *sr = b_restriction; sr; sr = sr->prev ())
    {
      ADJINS (sr->sr_contents.p1);
      ADJINS2 (sr->sr_contents.p2);
    }

  for (lisp marker = lmarkers; consp (marker); marker = xcdr (marker))
    {
      lisp x = xcar (marker);
      ADJINS (xmarker_point (x));
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      {
        ADJINS (wp->w_point.p_point);
        if (&wp->w_point != &point)
          set_point_no_restrictions (wp->w_point, wp->w_point.p_point);
        ADJINS (wp->w_mark);
        ADJINS (wp->w_selection_point);
        ADJINS (wp->w_selection_marker);
        ADJINS (wp->w_reverse_region.p1);
        ADJINS (wp->w_reverse_region.p2);
        ADJINS (wp->w_disp);
        ADJINS (wp->w_last_disp);
      }

  for (WindowConfiguration *wc = WindowConfiguration::wc_chain;
       wc; wc = wc->wc_prev)
    for (WindowConfiguration::Data *d = wc->wc_data, *de = d + wc->wc_nwindows;
         d < de; d++)
      if (d->bufp == this)
        {
          ADJINS (d->point);
          ADJINS (d->disp);
          ADJINS (d->mark);
          ADJINS (d->selection_point);
          ADJINS (d->selection_marker);
          ADJINS (d->reverse_region.p1);
          ADJINS (d->reverse_region.p2);
        }
}

int
Buffer::pre_insert_chars (Point &point, int size)
{
  UndoInfo *undo = setup_insert_undo (point.p_point, size);
  if (!undo)
    return 0;

  if (!move_gap (point, size))
    {
      discard_insert_undo (undo);
      return 0;
    }

  save_insert_undo (undo, point.p_point, size);
  return 1;
}

void
Buffer::post_insert_chars (Point &point, int size)
{
  modify ();
  set_modified_region (point.p_point, point.p_point);
  b_modified_region.p2 += size;
  b_nchars += size;
  b_contents.p2 += size;
  adjust_insertion (point, size);
  forward_char (point, size);
  b_stream_cache = point;
#ifdef DEBUG
  check_valid ();
#endif
}

int
Buffer::insert_chars_internal (Point &point, const insertChars *ichars,
                               int nargs, int repeat)
{
  double total_length = 0;
  for (int i = 0; i < nargs; i++)
    total_length += ichars[i].length;
  total_length *= repeat;
  if (total_length > INT_MAX)
    return 0;

  int size = int (total_length);
  if (!size)
    return 1;

  if (!pre_insert_chars (point, size))
    return 0;

  Chunk *cp = point.p_chunk;
  int off = point.p_offset;
  for (int i = 0; i < repeat; i++)
    for (int j = 0; j < nargs; j++)
      {
        int rest = ichars[j].length;
        const Char *s = ichars[j].string;
        while (rest > 0)
          {
            int n = min (cp->c_used - off, rest);
            bcopy (s, cp->c_text + off, n);
            s += n;
            off += n;
            rest -= n;
            if (off == cp->c_used)
              {
                cp = cp->c_next;
                off = 0;
              }
          }
      }

  post_insert_chars (point, size);
  return 1;
}

int
Buffer::insert_chars_internal (Point &point, const Char *string,
                               int length, int repeat)
{
  insertChars ichars;
  ichars.string = string;
  ichars.length = length;
  return insert_chars_internal (point, &ichars, 1, repeat);
}

void
Buffer::insert_chars (Window *wp, const insertChars *ic, int n, int repeat)
{
  prepare_modify_buffer ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  point_t opoint = wp->w_point.p_point;
  if (!insert_chars_internal (wp->w_point, ic, n, repeat))
    FEstorage_error ();
  post_buffer_modified (Kinsert, wp->w_point, opoint, wp->w_point.p_point);
}

void
Buffer::insert_chars (Window *wp, const Char *string, int length, int repeat)
{
  prepare_modify_buffer ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  point_t opoint = wp->w_point.p_point;
  if (!insert_chars_internal (wp->w_point, string, length, repeat))
    FEstorage_error ();
  post_buffer_modified (Kinsert, wp->w_point, opoint, wp->w_point.p_point);
}

void
Buffer::insert_chars (Point &point, const Char *string, int length)
{
  prepare_modify_buffer ();
  point_t opoint = point.p_point;
  if (!insert_chars_internal (point, string, length, 1))
    FEstorage_error ();
  post_buffer_modified (Kinsert, point, opoint, point.p_point);
}

lisp
Finsert (lisp args)
{
  int nargs = xlist_length (args);
  if (!nargs)
    FEtoo_few_arguments ();

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  bp->check_read_only ();

  int repeat = 1;
  Char *tem;
  insertChars *ichars = (insertChars *)alloca ((sizeof *ichars + sizeof *tem)
                                               * nargs);
  tem = (Char *)(ichars + nargs);
  int i;
  for (i = 0; i < nargs; i++, args = xcdr (args))
    {
      lisp x = xcar (args);
      if (charp (x))
        {
          *tem = xchar_code (x);
          ichars[i].string = tem++;
          ichars[i].length = 1;
        }
      else if (stringp (x))
        {
          ichars[i].string = xstring_contents (x);
          ichars[i].length = xstring_length (x);
        }
      else if (i && i == nargs - 1)
        {
          repeat = fixnum_value (x);
          if (repeat < 0)
            FErange_error (x);
          break;
        }
      else
        FEtype_error (x, xsymbol_value (Qor_string_character));
    }

  bp->insert_chars (wp, ichars, i, repeat);
  return Qt;
}

void
Buffer::insert_file_contents (Window *wp, lisp filename, lisp visit,
                              lisp loffset, lisp lsize, ReadFileContext &rfc)
{
  check_read_only ();

  if (!visit)
    visit = Qnil;
  if (visit == Qnil)
    prepare_modify_buffer ();

  char path[PATH_MAX + 1];
  pathname2cstr (filename, path);

  if (special_file_p (path))
    file_error (Eis_character_special_file, filename);

  int offset, size;
  if (!loffset || loffset == Qnil)
    offset = 0;
  else
    {
      offset = fixnum_value (loffset);
      if (offset < 0)
        FErange_error (loffset);
    }

  if (!lsize || lsize == Qnil)
    size = -1;
  else
    {
      size = fixnum_value (lsize);
      if (size < 0)
        FErange_error (lsize);
    }

  Chunk *t_chunk;
  if (!wp->w_point.p_offset
      || wp->w_point.p_offset == wp->w_point.p_chunk->c_used)
    t_chunk = 0;
  else
    {
      t_chunk = alloc_chunk ();
      if (!t_chunk)
        FEstorage_error ();
    }

  long n;

  if (!safe_fixnum_value (symbol_value (Vexpected_eol_code, this), &n)
      || !valid_eol_code_p (n))
    n = eol_guess;
  rfc.r_expect_eol = eol_code (n);

  rfc.r_expect_char_encoding = symbol_value (Vexpected_fileio_encoding, this);
  if (!char_encoding_p (rfc.r_expect_char_encoding))
    rfc.r_expect_char_encoding = Qnil;
  rfc.r_char_encoding = lchar_encoding;

  read_file_contents (rfc, path, offset, size);

  if (rfc.r_status == ReadFileContext::RFCS_IOERR)
    {
      if (t_chunk)
        free_chunk (t_chunk);
      if (rfc.r_chunk)
        free_all_chunks (rfc.r_chunk);
      file_error (rfc.r_errcode, filename);
    }

  if (!rfc.r_chunk)
    {
      if (t_chunk)
        free_chunk (t_chunk);
      switch (rfc.r_status)
        {
        case ReadFileContext::RFCS_MEM:
          FEstorage_error ();

        case ReadFileContext::RFCS_OPEN:
          file_error (rfc.r_errcode, filename);

        default:
          break;
        }
    }
  else
    {
      if (visit != Qnil)
        clear_undo_info ();
      else
        {
          UndoInfo *u = setup_insert_undo (wp->w_point.p_point, rfc.r_nchars);
          if (!u)
            {
              if (t_chunk)
                free_chunk (t_chunk);
              free_all_chunks (rfc.r_chunk);
              FEstorage_error ();
            }
          save_insert_undo (u, wp->w_point.p_point, rfc.r_nchars);
        }

      if (!b_nchars)
        {
          free_all_chunks (b_chunkb);
          b_chunkb = rfc.r_chunk;
          b_chunke = rfc.r_tail;
        }
      else
        {
          Chunk *cp = wp->w_point.p_chunk;
          if (!wp->w_point.p_offset)
            {
              rfc.r_chunk->c_prev = cp->c_prev;
              if (!cp->c_prev)
                b_chunkb = rfc.r_chunk;
              else
                cp->c_prev->c_next = rfc.r_chunk;
              rfc.r_tail->c_next = cp;
              cp->c_prev = rfc.r_tail;
            }
          else if (wp->w_point.p_offset == wp->w_point.p_chunk->c_used)
            {
              assert (!cp->c_next);
              cp->c_next = rfc.r_chunk;
              rfc.r_chunk->c_prev = cp;
              b_chunke = rfc.r_tail;
            }
          else
            {
              bcopy (cp->c_text + wp->w_point.p_offset, t_chunk->c_text,
                     cp->c_used - wp->w_point.p_offset);
              t_chunk->c_used = cp->c_used - wp->w_point.p_offset;
              modify_chunk (t_chunk);
              t_chunk->c_next = cp->c_next;
              if (t_chunk->c_next)
                t_chunk->c_next->c_prev = t_chunk;
              else
                b_chunke = t_chunk;
              rfc.r_tail->c_next = t_chunk;
              t_chunk->c_prev = rfc.r_tail;
              rfc.r_chunk->c_prev = cp;
              cp->c_next = rfc.r_chunk;
              cp->c_used = wp->w_point.p_offset;
              modify_chunk (cp);
            }
        }
      wp->w_point.p_chunk = rfc.r_chunk;
      wp->w_point.p_offset = 0;
    }

  modify ();
  set_modified_region (wp->w_point.p_point, wp->w_point.p_point);
  b_modified_region.p2 += rfc.r_nchars;
  b_nchars += rfc.r_nchars;
  b_contents.p2 += rfc.r_nchars;
  adjust_insertion (wp->w_point, rfc.r_nchars);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
#ifdef DEBUG
  check_valid ();
#endif

  if (visit != Qnil)
    {
      b_eol_code = exact_eol_code (rfc.r_eol_code, b_eol_code);
      lchar_encoding = rfc.r_char_encoding;
      b_modified = 0;
      b_need_auto_save = 0;
      b_modtime = rfc.r_modtime;
      if (symbol_value (Slock_file, this) == Kedit)
        unlock_file ();
      save_modtime_undo (b_modtime);
      maybe_modify_buffer_bar ();
    }
  else
    post_buffer_modified (Kinsert, wp->w_point,
                          wp->w_point.p_point,
                          wp->w_point.p_point + rfc.r_nchars);
}

lisp
Finsert_file_contents (lisp filename, lisp visit, lisp offset, lisp size)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  ReadFileContext rfc;
  bp->insert_file_contents (wp, filename, visit, offset, size, rfc);
  multiple_value::count () = 3;
  multiple_value::value (1) = boole (rfc.r_status == ReadFileContext::RFCS_NOERR);
  multiple_value::value (2) = make_fixnum (rfc.r_nchars);
  return make_fixnum (rfc.r_nlines);
}

void
Buffer::delete_chunk (Chunk *cp)
{
  Chunk *prev = cp->c_prev;
  Chunk *next = cp->c_next;
  if (!prev && !next)
    {
      cp->clear ();
      return;
    }
  if (prev)
    prev->c_next = next;
  else
    b_chunkb = next;
  if (next)
    next->c_prev = prev;
  else
    b_chunke = prev;
  free_chunk (cp);
}

void
Buffer::adjust_deletion (const Point &point, int size)
{
  point_t from = point.p_point;

#define ADJDEL(P) \
  if ((P) > from) (P) = max (point_t ((P) - size), from)

  ADJDEL (b_point);
  ADJDEL (b_mark);
  ADJDEL (b_selection_point);
  ADJDEL (b_selection_marker);
  ADJDEL (b_reverse_region.p1);
  ADJDEL (b_reverse_region.p2);
  ADJDEL (b_disp);

  textprop_adjust_deletion (from, size);

  for (save_excursion *se = b_excursion; se; se = se->prev ())
    ADJDEL (se->se_point);

  for (save_restriction *sr = b_restriction; sr; sr = sr->prev ())
    {
      ADJDEL (sr->sr_contents.p1);
      ADJDEL (sr->sr_contents.p2);
    }

  for (lisp marker = lmarkers; consp (marker); marker = xcdr (marker))
    {
      lisp x = xcar (marker);
      ADJDEL (xmarker_point (x));
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == this)
      {
        if (point.p_point < wp->w_disp && point.p_point + size > wp->w_disp)
          wp->w_disp_flags |= Window::WDF_DELETE_TOP;
        ADJDEL (wp->w_point.p_point);
        if (&wp->w_point != &point)
          set_point_no_restrictions (wp->w_point, wp->w_point.p_point);
        ADJDEL (wp->w_mark);
        ADJDEL (wp->w_selection_point);
        ADJDEL (wp->w_selection_marker);
        ADJDEL (wp->w_reverse_region.p1);
        ADJDEL (wp->w_reverse_region.p2);
        ADJDEL (wp->w_disp);
        ADJDEL (wp->w_last_disp);
      }

  for (WindowConfiguration *wc = WindowConfiguration::wc_chain;
       wc; wc = wc->wc_prev)
    for (WindowConfiguration::Data *d = wc->wc_data, *de = d + wc->wc_nwindows;
         d < de; d++)
      if (d->bufp == this)
        {
          ADJDEL (d->point);
          ADJDEL (d->disp);
          ADJDEL (d->mark);
          ADJDEL (d->selection_point);
          ADJDEL (d->selection_marker);
          ADJDEL (d->reverse_region.p1);
          ADJDEL (d->reverse_region.p2);
        }
}

int
Buffer::delete_region_internal (Point &point, point_t from, point_t to)
{
  from = min (max (from, b_contents.p1), b_contents.p2);
  to = min (max (to, b_contents.p1), b_contents.p2);

  if (from == to)
    return 1;
  if (from > to)
    swap (from, to);

  int size = to - from;
  goto_char (point, from);

  if (!save_delete_undo (point, size))
    return 0;

  Chunk *cp = point.p_chunk;
  int off = point.p_offset;
  while (1)
    {
      int rest = cp->c_used - off;
      if (size >= rest)
        {
          Chunk *next = cp->c_next;
          if (!off)
            delete_chunk (cp);
          else
            {
              cp->c_used = off;
              modify_chunk (cp);
            }
          size -= rest;
          if (!size)
            break;
          cp = next;
          off = 0;
        }
      else
        {
          cp->c_used -= size;
          modify_chunk (cp);
          bcopy (cp->c_text + off + size, cp->c_text + off, cp->c_used - off);
          break;
        }
    }

  size = to - from;
  modify ();
  set_modified_region (from, to);
  b_modified_region.p2 -= size;
  b_nchars -= size;
  b_contents.p2 -= size;
  set_point (point, from);
  adjust_deletion (point, size);
#ifdef DEBUG
  check_valid ();
#endif
  return 1;
}

void
Buffer::delete_region (Window *wp, point_t from, point_t to)
{
  prepare_modify_buffer ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  if (!delete_region_internal (wp->w_point, from, to))
    FEstorage_error ();
  post_buffer_modified (Kdelete, wp->w_point,
                        wp->w_point.p_point, wp->w_point.p_point);
}

lisp
Fdelete_region (lisp from, lisp to)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p1 = bp->coerce_to_point (from);
  point_t p2 = bp->coerce_to_point (to);
  if (p1 != p2)
    {
      bp->check_read_only ();
      bp->delete_region (wp, p1, p2);
    }
  return Qt;
}

void
Buffer::overwrite_chars (Window *wp, const Char *p, int size)
{
  prepare_modify_region (wp, wp->w_point.p_point, wp->w_point.p_point + size);
  copy_chunk (p, wp->w_point.p_chunk, wp->w_point.p_offset, size);
  post_buffer_modified (Kmodify, wp->w_point,
                        wp->w_point.p_point, wp->w_point.p_point + size);
}

void
Buffer::substring (const Point &point, int size, Char *b) const
{
  const Chunk *cp = point.p_chunk;
  int n = cp->c_used - point.p_offset;
  if (n >= size)
    bcopy (cp->c_text + point.p_offset, b, size);
  else
    {
      bcopy (cp->c_text + point.p_offset, b, n);
      b += n;
      size -= n;
      for (cp = cp->c_next; size > cp->c_used; cp = cp->c_next)
        {
          bcopy (cp->c_text, b, cp->c_used);
          b += cp->c_used;
          size -= cp->c_used;
        }
      bcopy (cp->c_text, b, size);
    }
}

void
Buffer::substring (point_t point, int size, Char *b) const
{
  Point p;
  set_point (p, point);
  substring (p, size, b);
}

lisp
Buffer::substring (point_t p1, point_t p2) const
{
  p1 = min (max (p1, b_contents.p1), b_contents.p2);
  p2 = min (max (p2, b_contents.p1), b_contents.p2);
  if (p1 > p2)
    swap (p1, p2);
  int size = p2 - p1;
  lisp x = make_string (size);
  substring (p1, size, xstring_contents (x));
  return x;
}

lisp
Fbuffer_substring (lisp p1, lisp p2, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  return bp->substring (bp->coerce_to_point (p1), bp->coerce_to_point (p2));
}

static int
encoding_auto_detect_p (lisp encoding)
{
  return (char_encoding_p (encoding)
          && xchar_encoding_type (encoding) == encoding_auto_detect);
}

static int
encoding_sjis_p (lisp encoding)
{
  return (!char_encoding_p (encoding)
          || xchar_encoding_type (encoding) == encoding_sjis);
}

static int
encoding_utf16_p (lisp encoding)
{
  return (char_encoding_p (encoding)
          && xchar_encoding_type (encoding) == encoding_utf16);
}

static void *
galloc (CLIPBOARDTEXT &clp, int size)
{
  clp.hgl = GlobalAlloc (GMEM_MOVEABLE, size);
  if (!clp.hgl)
    return 0;
  void *p = GlobalLock (clp.hgl);
  if (p)
    return p;
  GlobalFree (clp.hgl);
  clp.hgl = 0;
  return 0;
}

static int
make_cf_text_sjis (CLIPBOARDTEXT &clp, lisp string)
{
  const Char *s = xstring_contents (string);
  const Char *const se = s + xstring_length (string);

  int extra;
  for (extra = 0; s < se; s++)
    if (*s == '\n' || DBCP (*s))
      extra++;

  clp.fmt = CF_TEXT;
  char *b = (char *)galloc (clp, xstring_length (string) + extra + 1);
  if (!b)
    return 0;

  for (s = xstring_contents (string); s < se; s++)
    {
      Char cc = *s;
      if (DBCP (cc))
        {
          if (code_charset (cc) == ccs_cp932)
            *b++ = cc >> 8;
          else
            {
              Char c2 = wc2cp932 (i2w (cc));
              if (c2 != Char (-1))
                {
                  cc = c2;
                  if (DBCP (cc))
                    *b++ = cc >> 8;
                }
              else
                cc = '?';
            }
        }
      else if (cc == '\n')
        *b++ = '\r';
      *b++ = char (cc);
    }
  *b = 0;

  GlobalUnlock (clp.hgl);
  return 1;
}

static int
make_cf_text (CLIPBOARDTEXT &clp, lisp string, lisp encoding)
{
  if (encoding_sjis_p (encoding) || encoding_auto_detect_p (encoding))
    return make_cf_text_sjis (clp, string);

  Char_input_string_stream str1 (string);
  encoding_output_stream_helper is1 (encoding, str1, eol_crlf);
  int l = is1->total_length ();

  clp.fmt = CF_TEXT;
  char *b = (char *)galloc (clp, l + 1);
  if (!b)
    return 0;

  Char_input_string_stream str2 (string);
  encoding_output_stream_helper is2 (encoding, str2, eol_crlf);
  is2->copyto ((u_char *)b, l + 1);

  GlobalUnlock (clp.hgl);
  return 1;
}

static int
make_cf_wtext (CLIPBOARDTEXT &clp, lisp string)
{
  const Char *s = xstring_contents (string);
  const Char *const se = s + xstring_length (string);

  int extra;
  for (extra = 0; s < se; s++)
    if (*s == '\n')
      extra++;

  clp.fmt = CF_UNICODETEXT;
  ucs2_t *b = (ucs2_t *)galloc (clp, (xstring_length (string) + extra + 1) * sizeof *b);
  if (!b)
    return 0;

  for (s = xstring_contents (string); s < se; s++)
    {
      if (*s == '\n')
        *b++ = '\r';
      ucs2_t c = i2w (*s);
      if (c == ucs2_t (-1))
        {
          if (utf16_undef_char_high_p (*s) && s < se - 1
              && utf16_undef_char_low_p (s[1]))
            {
              c = utf16_undef_pair_to_ucs2 (*s, s[1]);
              s++;
            }
          else
            c = DEFCHAR;
        }
      *b++ = c;
    }
  *b = 0;

  GlobalUnlock (clp.hgl);
  return 1;
}

int
make_clipboard_text (CLIPBOARDTEXT &clp, lisp string, int req)
{
  clp.hgl = 0;
  clp.fmt = 0;
  lisp encoding = symbol_value (Vclipboard_char_encoding, selected_buffer ());
  if (req == CF_UNICODETEXT || encoding_utf16_p (encoding))
    return make_cf_wtext (clp, string);
  return make_cf_text (clp, string, encoding);
}

static int
open_clipboard (HWND hwnd)
{
  for (int i = 0; i < 100; i++)
    {
      if (OpenClipboard (hwnd))
        return 1;
      Sleep (0);
    }
  return 0;
}

lisp
Fcopy_to_clipboard (lisp string)
{
  check_string (string);
  if (!xstring_length (string))
    return Qnil;

  CLIPBOARDTEXT clp[2];
  bzero (clp, sizeof clp);
  lisp encoding = symbol_value (Vclipboard_char_encoding, selected_buffer ());
  if (encoding_utf16_p (encoding))
    {
      if (!make_cf_wtext (clp[0], string))
        FEstorage_error ();
    }
  else if (encoding_sjis_p (encoding) || encoding_auto_detect_p (encoding))
    {
      if (!make_cf_text_sjis (clp[0], string))
        FEstorage_error ();
    }
  else
    {
      if (!make_cf_wtext (clp[0], string))
        FEstorage_error ();
      if (!make_cf_text (clp[1], string, encoding))
        {
          GlobalFree (clp[0].hgl);
          FEstorage_error ();
        }
    }

  int result = 0;
  if (open_clipboard (app.toplev))
    {
      if (EmptyClipboard ())
        for (int i = 0; i < numberof (clp) && clp[i].hgl; i++)
          {
            if (!SetClipboardData (clp[i].fmt, clp[i].hgl))
              break;
            result = 1;
            clp[i].hgl = 0;
          }
      CloseClipboard ();
    }
  for (int i = 0; i < numberof (clp); i++)
    if (clp[i].hgl)
      GlobalFree (clp[i].hgl);
  xsymbol_value (Vclipboard_newer_than_kill_ring_p) = Qnil;
  xsymbol_value (Vkill_ring_newer_than_clipboard_p) = Qnil;
  return boole (result);
}

static int
count_cf_text_length (const u_char *string)
{
  int l = 0;
  const u_char *s;
  for (s = string; *s;)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              s++;
              break;
            }
          l++;
          s += 2;
        }
      else
        {
          if (*s == '\r' && s[1] == '\n')
            l++;
          s++;
        }
    }
  return s - string - l;
}

static int
make_string_from_cf_text_sjis (lisp lstring, const u_char *s)
{
  int l = count_cf_text_length (s);
  Char *b = (Char *)malloc (l * sizeof *b);
  if (!b)
    return 0;
  xstring_contents (lstring) = b;
  xstring_length (lstring) = l;
  while (*s)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              *b = *s;
              break;
            }
          *b++ = (*s << 8) | s[1];
          s += 2;
        }
      else if (*s == '\r' && s[1] == '\n')
        s++;
      else
        *b++ = *s++;
    }
  return 1;
}

static int
make_string_from_cf_text (lisp lstring, const u_char *s)
{
  const char* ss = reinterpret_cast<const char*> (s);
  lisp encoding = symbol_value (Vclipboard_char_encoding, selected_buffer ());
  if (encoding_auto_detect_p (encoding))
    encoding = detect_char_encoding (ss, strlen (ss));
  if (encoding_sjis_p (encoding))
    return make_string_from_cf_text_sjis (lstring, s);

  int sl = strlen (ss);
  xinput_strstream str1 (ss, sl);
  encoding_input_stream_helper is1 (encoding, str1);
  int l = is1->total_length ();
  Char *b = (Char *)malloc (l * sizeof *b);
  if (!b)
    return 0;
  xstring_contents (lstring) = b;
  xinput_strstream str2 (ss, sl);
  encoding_input_stream_helper is2 (encoding, str2);
  int c;
  while ((c = is2->get ()) != xstream::eof)
    if (c != '\r')
      *b++ = c;
    else
      while (1)
        {
          c = is2->get ();
          if (c == '\n')
            {
              *b++ = c;
              break;
            }
          *b++ = '\r';
          if (c == xstream::eof)
            goto eof;
          if (c != '\r')
            {
              *b++ = c;
              break;
            }
        }
eof:
  xstring_length (lstring) = b - xstring_contents (lstring);

  return 1;
}

static int
count_cf_wtext_length (const ucs2_t *string)
{
  int l = 0;
  const ucs2_t *s;
  for (s = string; *s; s++)
    if (*s == '\r')
      {
        if (s[1] == '\n')
          {
            l--;
            s++;
          }
      }
    else if (w2i (*s) == Char (-1))
      l++;
  return s - string + l;
}

static int
make_string_from_cf_wtext (lisp lstring, const ucs2_t *s, int lang)
{
  int l = count_cf_wtext_length (s);
  Char *b = (Char *)malloc (l * sizeof *b);
  if (!b)
    return 0;
  xstring_contents (lstring) = b;
  xstring_length (lstring) = l;

  const Char *const translate = cjk_translate_table (lang);

  switch (lang)
    {
    case ENCODING_LANG_JP:
    case ENCODING_LANG_JP2:
    default:
      {
        int to_half = xsymbol_value (Vunicode_to_half_width) != Qnil;
        for (; *s; s++)
          if (*s != '\r' || s[1] != '\n')
            {
              Char cc;
              if (to_half
                  || (cc = wc2cp932 (*s)) == Char (-1)
                  || ccs_1byte_94_charset_p (code_charset (cc)))
                cc = w2i (*s);
              if (cc == Char (-1))
                {
                  *b++ = utf16_ucs2_to_undef_pair_high (*s);
                  *b++ = utf16_ucs2_to_undef_pair_low (*s);
                }
              else
                *b++ = cc;
            }
      }
      break;

    case ENCODING_LANG_KR:
    case ENCODING_LANG_CN_GB:
    case ENCODING_LANG_CN_BIG5:
      for (; *s; s++)
        if (*s != '\r' || s[1] != '\n')
          {
            Char cc = w2i (*s);
            if (cc == Char (-1))
              {
                *b++ = utf16_ucs2_to_undef_pair_high (*s);
                *b++ = utf16_ucs2_to_undef_pair_low (*s);
              }
            else
              {
                if (!ccs_1byte_94_charset_p (code_charset (cc)))
                  {
                    Char t = translate[*s];
                    if (t != Char (-1))
                      cc = t;
                  }
                *b++ = cc;
              }
          }
      break;

    case ENCODING_LANG_CN:
      for (; *s; s++)
        if (*s != '\r' || s[1] != '\n')
          {
            Char cc = w2i (*s);
            if (cc == Char (-1))
              {
                *b++ = utf16_ucs2_to_undef_pair_high (*s);
                *b++ = utf16_ucs2_to_undef_pair_low (*s);
              }
            else
              {
                if (!ccs_1byte_94_charset_p (code_charset (cc)))
                  {
                    Char t = wc2gb2312_table[*s];
                    if (t != Char (-1) || (t = wc2big5_table[*s]) != Char (-1))
                      cc = t;
                  }
                *b++ = cc;
              }
          }
      break;
    }
  
  return 1;
}

int
make_string_from_clipboard_text (lisp lstring, const void *data, UINT fmt, int lang)
{
  switch (fmt)
    {
    case CF_TEXT:
      return make_string_from_cf_text (lstring, (const u_char *)data);

    case CF_UNICODETEXT:
      return make_string_from_cf_wtext (lstring, (const ucs2_t *)data, lang);

    default:
      assert (0);
      return 0;
    }
}

static int
get_clipboatd_data (UINT fmt, lisp lstring, int lang)
{
  HGLOBAL hgl = GetClipboardData (fmt);
  if (!hgl)
    return -1;
  void *data = GlobalLock (hgl);
  if (!data)
    return 0;
  int r = make_string_from_clipboard_text (lstring, data, fmt, lang);
  GlobalUnlock (hgl);
  return r;
}

lisp
Fget_clipboard_data ()
{
  int result = -1;
  lisp lstring = make_simple_string ();
  if (open_clipboard (app.toplev))
    {
      lisp encoding = symbol_value (Vclipboard_char_encoding,
                                    selected_buffer ());
      if (encoding_utf16_p (encoding))
        result = get_clipboatd_data (CF_UNICODETEXT, lstring,
                                     xchar_encoding_utf_cjk (encoding));
      if (result == -1)
        {
          UINT fmt = 0;
          while ((fmt = EnumClipboardFormats (fmt)))
            if (fmt == CF_TEXT || fmt == CF_UNICODETEXT)
              {
                result = get_clipboatd_data (fmt, lstring, ENCODING_LANG_NIL);
                break;
              }
        }
      CloseClipboard ();
    }
  if (!result)
    FEstorage_error ();
  if (result == -1)
    return Qnil;
  return lstring;
}

lisp
Fclipboard_empty_p ()
{
  return boole (!IsClipboardFormatAvailable (CF_TEXT)
                && (sysdep.WinNTp ()
                    || !IsClipboardFormatAvailable (CF_UNICODETEXT)));
}

textprop *
Buffer::find_textprop (point_t point) const
{
  for (textprop *p = textprop_head (point); p; p = p->t_next)
    if (p->t_range.p2 > point)
      return p;
  return 0;
}

void
Buffer::textprop_adjust_insertion (point_t point, int size)
{
  for (textprop *p = textprop_head (point); p; p = p->t_next)
    if (p->t_range.p2 > point)
      {
        if (p->t_range.p1 > point)
          p->t_range.p1 += size;
        p->t_range.p2 += size;
        while ((p = p->t_next))
          {
            p->t_range.p1 += size;
            p->t_range.p2 += size;
          }
        break;
      }
}

void
Buffer::textprop_adjust_deletion (point_t point, int size)
{
  point_t pe = point + size;
  for (textprop *p = textprop_head (point), *prev = 0, *next; p; prev = p, p = next)
    {
      next = p->t_next;
      if (p->t_range.p1 >= pe)
        {
          do
            {
              p->t_range.p1 -= size;
              p->t_range.p2 -= size;
            }
          while (p = p->t_next);
          break;
        }
      if (p->t_range.p2 > point)
        {
          if (p->t_range.p1 > point)
            p->t_range.p1 = max (point_t (p->t_range.p1 - size), point);
          p->t_range.p2 = max (point_t (p->t_range.p2 - size), point);
          if (p->t_range.p1 == p->t_range.p2)
            {
              free_textprop (p);
              if (prev)
                {
                  prev->t_next = next;
                  p = prev;
                }
              else
                {
                  b_textprop = next;
                  p = 0;
                }
            }
        }
    }
}

static inline void
copy_textprop (const textprop *p, textprop *t)
{
  t->t_attrib = p->t_attrib;
  t->t_tag = p->t_tag;
}

textprop *
Buffer::add_textprop (point_t p1, point_t p2)
{
  textprop *p, *prev;
  for (p = textprop_head (p1), prev = 0; p; prev = p, p = p->t_next)
    if (*p > p1)
      break;

  if (p && p->t_range.p1 == p1 && p->t_range.p2 == p2)
    return p;

  textprop *q = make_textprop ();
  if (!q)
    return 0;
  q->t_range.p1 = p1;
  q->t_range.p2 = p2;

  textprop **pl = prev ? &prev->t_next : &b_textprop;

  if (p)
    {
      if (p2 <= p->t_range.p1)
        {
          q->t_next = p;
          *pl = q;
        }
      else if (p2 < p->t_range.p2)
        {
          if (p1 <= p->t_range.p1)
            {
              q->t_next = p;
              p->t_range.p1 = p2;
              *pl = q;
            }
          else
            {
              textprop *t = make_textprop ();
              if (!t)
                {
                  free_textprop (q);
                  return 0;
                }
              t->t_next = p->t_next;
              q->t_next = t;
              p->t_next = q;
              t->t_range.p1 = p2;
              t->t_range.p2 = p->t_range.p2;
              p->t_range.p2 = p1;
              copy_textprop (p, t);
            }
        }
      else
        {
          if (p1 <= p->t_range.p1)
            {
              p->t_range.p1 = p1;
              p->t_range.p2 = p2;
              free_textprop (q);
              q = p;
            }
          else
            {
              q->t_next = p->t_next;
              p->t_next = q;
              p->t_range.p2 = p1;
            }
        }
    }
  else
    {
      q->t_next = *pl;
      *pl = q;
    }

  for (p = q->t_next; p && p->t_range.p1 < p2; p = q->t_next)
    {
      if (p->t_range.p2 > p2)
        {
          p->t_range.p1 = p2;
          break;
        }
      q->t_next = p->t_next;
      free_textprop (p);
    }

  return q;
}

static int
check_attrib (lisp lc1, lisp lc2, lisp bold, lisp underline, lisp strikeout, lisp lcc, lisp lextend)
{
  int attrib = 0;
  if (lc1 != Qnil)
    attrib |= ((fixnum_value (lc1) & (GLYPH_TEXTPROP_NCOLORS - 1))
               << GLYPH_TEXTPROP_FG_SHIFT_BITS);
  if (lc2 != Qnil)
    attrib |= ((fixnum_value (lc2) & (GLYPH_TEXTPROP_NCOLORS - 1))
               << GLYPH_TEXTPROP_BG_SHIFT_BITS);
  if (bold != Qnil)
    attrib |= GLYPH_BOLD;
  if (underline != Qnil)
    attrib |= GLYPH_UNDERLINE;
  if (strikeout != Qnil)
    attrib |= GLYPH_STRIKEOUT;
  if (lcc != Qnil)
    {
      check_char (lcc);
      int cc = xchar_code (lcc);
      if (cc > ' ' && cc < 0x7f)
        attrib |= cc;
    }
  if (lextend != Qnil)
    attrib |= TEXTPROP_EXTEND_EOL_BIT;
  return attrib;
}

static int
attrib_value (lisp keys)
{
  return check_attrib (find_keyword (Kforeground, keys, Qnil),
                       find_keyword (Kbackground, keys, Qnil),
                       find_keyword (Kbold, keys, Qnil),
                       find_keyword (Kunderline, keys, Qnil),
                       find_keyword (Kstrike_out, keys, Qnil),
                       find_keyword (Kprefix, keys, Qnil),
                       find_keyword (Kextend, keys, Qnil));
}

static lisp
set_text_attribute (lisp lp1, lisp lp2, lisp tag, int attrib)
{
  Buffer *bp = selected_buffer ();
  point_t p1 = bp->coerce_to_restricted_point (lp1);
  point_t p2 = bp->coerce_to_restricted_point (lp2);
  if (p1 > p2)
    swap (p1, p2);

  textprop *p = bp->add_textprop (p1, p2);
  if (!p)
    FEstorage_error ();

  p->t_attrib = attrib;
  p->t_tag = tag;
  bp->b_textprop_cache = p;
  bp->set_modified_region (p1, p2);
  return Qt;
}

lisp
Fset_text_attribute (lisp lp1, lisp lp2, lisp tag, lisp keys)
{
  return set_text_attribute (lp1, lp2, tag, attrib_value (keys));
}

lisp
Fclear_all_text_attributes ()
{
  Buffer *bp = selected_buffer ();
  if (bp->b_textprop)
    {
      bp->b_textprop_heap.free_all ();
      bp->b_textprop = 0;
      bp->b_textprop_cache = 0;
      bp->refresh_buffer ();
    }
  return Qt;
}

static lisp
delete_text_attributes (test_proc &test)
{
  Buffer *bp = selected_buffer ();
  for (textprop *t = bp->b_textprop, *prev = 0, *next; t; t = next)
    {
      next = t->t_next;
      if (test.test (t->t_tag))
        {
          if (prev)
            {
              prev->t_next = next;
              bp->b_textprop_cache = prev;
            }
          else
            {
              bp->b_textprop = next;
              if (bp->b_textprop_cache == t)
                bp->b_textprop_cache = 0;
            }
          bp->free_textprop (t);
        }
      else
        prev = t;
    }
  bp->refresh_buffer ();
  return Qt;
}

lisp
Fdelete_text_attributes (lisp tag, lisp keys)
{
  seq_testproc test (tag, keys);
  return delete_text_attributes (test);
}

lisp
Fdelete_text_attributes_if (lisp pred, lisp keys)
{
  seq_testproc_if test (pred, keys);
  return delete_text_attributes (test);
}

lisp
Fdelete_text_attributes_if_not (lisp pred, lisp keys)
{
  seq_testproc_if_not test (pred, keys);
  return delete_text_attributes (test);
}

lisp
Fdelete_text_attribute_point (lisp lpoint)
{
  Buffer *bp = selected_buffer ();
  point_t point = bp->coerce_to_point (lpoint);
  for (textprop *t = bp->b_textprop, *prev = 0;
       t && t->t_range.p1 <= point; prev = t, t = t->t_next)
    if (*t > point)
      {
        if (prev)
          {
            prev->t_next = t->t_next;
            bp->b_textprop_cache = prev;
          }
        else
          {
            bp->b_textprop = t->t_next;
            if (bp->b_textprop_cache == t)
              bp->b_textprop_cache = 0;
          }
        bp->set_modified_region (t->t_range.p1, t->t_range.p2);
        bp->free_textprop (t);
        return Qt;
      }
  return Qnil;
}

lisp
Ffind_text_attribute_point (lisp lpoint)
{
  Buffer *bp = selected_buffer ();
  point_t point = bp->coerce_to_point (lpoint);
  for (textprop *t = bp->textprop_head (point);
       t && t->t_range.p1 <= point; t = t->t_next)
    if (*t > point)
      {
        int attrib = t->t_attrib;
        multiple_value::count () = 10;
        multiple_value::value (9) = boole (attrib & TEXTPROP_EXTEND_EOL_BIT);
        multiple_value::value (8) = attrib & 0xff ? make_char (attrib & 0xff) : Qnil;
        multiple_value::value (7) = boole (attrib & GLYPH_STRIKEOUT);
        multiple_value::value (6) = boole (attrib & GLYPH_UNDERLINE);
        multiple_value::value (5) = boole (attrib & GLYPH_BOLD);
        multiple_value::value (4) = (attrib & ((GLYPH_TEXTPROP_NCOLORS - 1)
                                               << GLYPH_TEXTPROP_BG_SHIFT_BITS)
                                     ? make_fixnum ((attrib >> GLYPH_TEXTPROP_BG_SHIFT_BITS)
                                                    & (GLYPH_TEXTPROP_NCOLORS - 1))
                                     : Qnil);
        multiple_value::value (3) = (attrib & ((GLYPH_TEXTPROP_NCOLORS - 1)
                                               << GLYPH_TEXTPROP_FG_SHIFT_BITS)
                                     ? make_fixnum ((attrib >> GLYPH_TEXTPROP_FG_SHIFT_BITS)
                                                    & (GLYPH_TEXTPROP_NCOLORS - 1))
                                     : Qnil);
        multiple_value::value (2) = t->t_tag;
        multiple_value::value (1) = make_fixnum (t->t_range.p2);
        return make_fixnum (t->t_range.p1);
      }
  return Qnil;
}

static lisp
find_text_attribute (test_proc &test, lisp keys)
{
  Buffer *bp = selected_buffer ();
  lisp lstart = find_keyword (Kstart, keys, Qnil);
  lisp lend = find_keyword (Kend, keys, Qnil);
  lisp from_end = find_keyword (Kfrom_end, keys, Qnil);
  point_t start = lstart == Qnil ? bp->b_contents.p1 : bp->coerce_to_point (lstart);
  point_t end = lend == Qnil ? bp->b_contents.p2: bp->coerce_to_point (lend);
  textprop *match = 0;
  if (from_end == Qnil)
    {
      for (textprop *t = bp->textprop_head (start); t && *t < end; t = t->t_next)
        if (t->t_range.p1 >= start && test.test (t->t_tag))
          {
            match = t;
            break;
          }
    }
  else
    {
      for (textprop *t = bp->textprop_head (start); t && *t < end; t = t->t_next)
        if (t->t_range.p1 >= start && test.test (t->t_tag))
          match = t;
    }
  if (!match)
    return Qnil;

  multiple_value::count () = 3;
  multiple_value::value (2) = match->t_tag;
  multiple_value::value (1) = make_fixnum (match->t_range.p2);
  return make_fixnum (match->t_range.p1);
}

lisp
Ffind_text_attribute (lisp tag, lisp keys)
{
  seq_testproc test (tag, keys);
  return find_text_attribute (test, keys);
}

lisp
Ffind_text_attribute_if (lisp pred, lisp keys)
{
  seq_testproc_if test (pred, keys);
  return find_text_attribute (test, keys);
}

lisp
Ffind_text_attribute_if_not (lisp pred, lisp keys)
{
  seq_testproc_if_not test (pred, keys);
  return find_text_attribute (test, keys);
}

static lisp
modify_text_attributes (test_proc &test, lisp keys)
{
  int attrib = attrib_value (keys);
  Buffer *bp = selected_buffer ();
  lisp lstart = find_keyword (Kstart, keys, Qnil);
  lisp lend = find_keyword (Kend, keys, Qnil);
  point_t start = lstart == Qnil ? bp->b_contents.p1 : bp->coerce_to_point (lstart);
  point_t end = lend == Qnil ? bp->b_contents.p2: bp->coerce_to_point (lend);
  for (textprop *t = bp->textprop_head (start); t && *t < end; t = t->t_next)
    if (t->t_range.p1 >= start && test.test (t->t_tag))
      t->t_attrib = attrib;
  bp->refresh_buffer ();
  return Qt;
}

lisp
Fmodify_text_attributes (lisp tag, lisp keys)
{
  seq_testproc test (tag, keys);
  return modify_text_attributes (test, keys);
}

lisp
Fmodify_text_attributes_if (lisp pred, lisp keys)
{
  seq_testproc_if test (pred, keys);
  return modify_text_attributes (test, keys);
}

lisp
Fmodify_text_attributes_if_not (lisp pred, lisp keys)
{
  seq_testproc_if_not test (pred, keys);
  return modify_text_attributes (test, keys);
}

static lisp
push_attrib (lisp x, lisp key, lisp value)
{
  return xcons (key, xcons (value, x));
}

lisp
Flist_text_attributes (lisp lstart, lisp lend)
{
  Buffer *bp = selected_buffer ();
  point_t start = (lstart && lstart != Qnil
                   ? bp->coerce_to_point (lstart)
                   : bp->b_contents.p1);
  point_t end = (lend && lend != Qnil
                 ? bp->coerce_to_point (lend)
                 : bp->b_contents.p2);

  lisp r = Qnil;
  for (const textprop *t = bp->b_textprop; t && *t < end; t = t->t_next)
    if (*t > start)
      {
        lisp x = Qnil;
        int attrib = t->t_attrib;
        if (attrib & TEXTPROP_EXTEND_EOL_BIT)
          x = push_attrib (x, Kextend, Qt);
        if (attrib & 0xff)
          x = push_attrib (x, Kprefix, make_char (attrib & 0xff));
        if (attrib & GLYPH_STRIKEOUT)
          x = push_attrib (x, Kstrike_out, Qt);
        if (attrib & GLYPH_UNDERLINE)
          x = push_attrib (x, Kunderline, Qt);
        if (attrib & GLYPH_BOLD)
          x = push_attrib (x, Kbold, Qt);
        if (attrib & ((GLYPH_TEXTPROP_NCOLORS - 1) << GLYPH_TEXTPROP_BG_SHIFT_BITS))
          x = push_attrib (x, Kbackground,
                           make_fixnum ((attrib >> GLYPH_TEXTPROP_BG_SHIFT_BITS)
                                        & (GLYPH_TEXTPROP_NCOLORS - 1)));
        if (attrib & ((GLYPH_TEXTPROP_NCOLORS - 1) << GLYPH_TEXTPROP_FG_SHIFT_BITS))
          x = push_attrib (x, Kforeground,
                           make_fixnum ((attrib >> GLYPH_TEXTPROP_FG_SHIFT_BITS)
                                        & (GLYPH_TEXTPROP_NCOLORS - 1)));
        x = xcons (t->t_tag, x);
        x = xcons (make_fixnum (t->t_range.p2), x);
        x = xcons (make_fixnum (t->t_range.p1), x);
        r = xcons (x, r);
      }
  return Fnreverse (r);
}
