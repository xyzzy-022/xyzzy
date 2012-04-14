#include "stdafx.h"
#include "ed.h"
#include "mainframe.h"
#include "buffer-bar.h"
#include "colors.h"

buffer_bar *buffer_bar::b_bar;
Buffer *buffer_bar::b_last_buffer;

#pragma warning (disable: 4355)
buffer_bar::buffer_bar (dock_frame &frame)
     : tab_bar (frame, Vbuffer_bar), b_drop_target (this), b_drop_index (-1)
{
}
#pragma warning (default: 4355)

Buffer *
buffer_bar::current () const
{
  int i = get_cursel ();
  return i >= 0 ? nth (i) : 0;
}

char *
buffer_bar::set_buffer_name (const Buffer *bp, char *buf, int size)
{
  char *b = buf;
  if (bp->b_modified)
    {
      *b++ = '*';
      *b++ = ' ';
    }
  bp->buffer_name (b, buf + size - 2);
  return buf;
}

int
buffer_bar::insert (const Buffer *bp, int i)
{
  TC_ITEM ti;
  ti.mask = TCIF_TEXT | TCIF_PARAM;
  char buf[BUFFER_NAME_MAX * 2 + 32];
  ti.pszText = set_buffer_name (bp, buf, sizeof buf);
  ti.lParam = LPARAM (bp);
  return insert_item (i, ti);
}

int
buffer_bar::modify (const Buffer *bp, int i)
{
  TC_ITEM ti;
  ti.mask = TCIF_TEXT | TCIF_PARAM;
  char buf[BUFFER_NAME_MAX * 2 + 32];
  ti.pszText = set_buffer_name (bp, buf, sizeof buf);
  ti.lParam = LPARAM (bp);
  return set_item (i, ti);
}

int
buffer_bar::notify (NMHDR *nm, LRESULT &result)
{
  switch (nm->code)
    {
    default:
      return 0;

    case TCN_SELCHANGE:
    case TCN_SELCHANGING:
      if (!app.kbdq.idlep ()
          || selected_window ()->minibuffer_window_p ())
        {
          result = 1; // prevent the selection
          return 1;
        }
      break;
    }

  if (nm->code != TCN_SELCHANGE)
    return 0;
  Buffer *bp = current ();
  if (bp)
    {
      try
        {
          selected_buffer ()->run_hook (Vbuffer_bar_hook, bp->lbp);
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
        }
      b_last_buffer = 0;
      refresh_screen (1);
    }
  return 1;
}

int
buffer_bar::need_text (TOOLTIPTEXT &ttt)
{
  if (ttt.uFlags & TTF_IDISHWND)
    return 0;

  Buffer *bp = nth (ttt.hdr.idFrom);
  if (!bp)
    return 0;

  lisp x;
  if (stringp (bp->lfile_name))
    x = bp->lfile_name;
  else if (stringp (bp->lalternate_file_name))
    x = bp->lalternate_file_name;
  else
    x = bp->lbuffer_name;

  w2s (b_ttbuf, b_ttbuf + TTBUFSIZE, x);
  ttt.lpszText = b_ttbuf;
  ttt.hinst = 0;
  return 1;
}

void
buffer_bar::tab_color (const Buffer *bp, COLORREF &fg, COLORREF &bg)
{
  if (bp == selected_buffer ())
    {
      fg = get_misc_color (MC_BUFTAB_SEL_FG);
      bg = get_misc_color (MC_BUFTAB_SEL_BG);
    }
  else
    {
      Window *wp;
      for (wp = app.active_frame.windows; wp; wp = wp->w_next)
        if (wp->w_bufp == bp)
          {
            fg = get_misc_color (MC_BUFTAB_DISP_FG);
            bg = get_misc_color (MC_BUFTAB_DISP_BG);
            break;
          }
      if (!wp)
        {
          fg = get_misc_color (MC_BUFTAB_FG);
          bg = get_misc_color (MC_BUFTAB_BG);
        }
    }
}

void
buffer_bar::draw_item (const draw_item_struct &dis)
{
  Buffer *bp = (Buffer *)dis.data;
  char buf[BUFFER_NAME_MAX * 2 + 32];
  set_buffer_name (bp, buf, sizeof buf);

  if (bp->b_modified)
    bp->b_buffer_bar_modified |= Buffer::BUFFER_BAR_LAST_MODIFIED_FLAG;
  else
    bp->b_buffer_bar_modified &= ~Buffer::BUFFER_BAR_LAST_MODIFIED_FLAG;

  COLORREF fg, bg;
  tab_color (bp, fg, bg);
  bp->b_buffer_bar_fg = fg;
  bp->b_buffer_bar_bg = bg;

  tab_bar::draw_item (dis, buf, strlen (buf), fg, bg);
}

void
buffer_bar::insert_buffers ()
{
  set_no_redraw ();
  int current = -1;
  int i = 0;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (!bp->internal_buffer_p ())
      {
        insert (bp, i);
        if (bp == selected_buffer ())
          current = i;
        i++;
        bp->b_buffer_bar_modified &= Buffer::BUFFER_BAR_LAST_MODIFIED_FLAG;
      }
  if (current >= 0)
    set_cursel (current);
  Buffer::b_buffer_bar_modified_any = 0;
  set_redraw ();
}

int
buffer_bar::create (HWND hwnd_parent)
{
  if (!tab_bar::create (hwnd_parent))
    return 0;
  RegisterDragDrop (b_hwnd, &b_drop_target);
  return 1;
}

int
buffer_bar::make_instance ()
{
  if (!b_bar)
    {
      b_bar = new buffer_bar (g_frame);
      if (!b_bar->create (app.toplev))
        return 0;
      b_bar->insert_buffers ();
      g_frame.add (b_bar);
    }
  return 1;
}

void
buffer_bar::delete_buffer (Buffer *bp)
{
  set_no_redraw ();
  int current = -1, found = -1;
  int n = item_count ();
  for (int i = 0; i < n; i++)
    {
      Buffer *p = nth (i);
      if (p == bp)
        found = i;
      if (p == selected_buffer ())
        current = i;
      if (found >= 0 && current >= 0)
        break;
    }
  if (current >= 0)
    set_cursel (current);
  if (found >= 0)
    delete_item (found);
  Buffer::b_buffer_bar_modified_any |= Buffer::BUFFER_BAR_DELETED;
}

Buffer *
buffer_bar::next_buffer (Buffer *bp, int dir) const
{
  int n = item_count ();
  for (int i = 0; i < n; i++)
    if (nth (i) == bp)
      return nth (dir > 0
                  ? i == n - 1 ? 0 : i + 1
                  : i ? i - 1 : n - 1);
  return 0;
}

Buffer *
buffer_bar::top_buffer () const
{
  return item_count () > 0 ? nth (0) : 0;
}

Buffer *
buffer_bar::bottom_buffer () const
{
  int n = item_count ();
  return n > 0 ? nth (n - 1) : 0;
}

static int __cdecl
compare_buffer (const void *p1, const void *p2)
{
  return (*(Buffer **)p1)->b_create_count - (*(Buffer **)p2)->b_create_count;
}

void
buffer_bar::update_ui ()
{
  int n = item_count ();
  if (Buffer::b_buffer_bar_modified_any & Buffer::BUFFER_BAR_CREATED)
    {
      set_no_redraw ();

      int nbuffers = 0;
      for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
        if (bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_CREATED
            && !bp->internal_buffer_p ())
          nbuffers++;

      if (nbuffers > 0)
        {
          int i = 0;
          Buffer **buffers = (Buffer **)alloca (sizeof *buffers * nbuffers);
          for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
            if (bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_CREATED)
              {
                if (!bp->internal_buffer_p ())
                  buffers[i++] = bp;
                bp->b_buffer_bar_modified &= ~(Buffer::BUFFER_BAR_CREATED
                                               | Buffer::BUFFER_BAR_MODIFIED);
              }

          qsort (buffers, nbuffers, sizeof *buffers, compare_buffer);

          for (i = 0; i < nbuffers; i++)
            if (insert (buffers[i], n) >= 0)
              n++;
        }
    }

  int cur = -1;
  int mod = Buffer::b_buffer_bar_modified_any & Buffer::BUFFER_BAR_DELETED;

  if (Buffer::b_buffer_bar_modified_any & Buffer::BUFFER_BAR_MODIFIED
      || b_last_buffer != selected_buffer ())
    {
      for (int i = 0; i < n; i++)
        {
          Buffer *bp = nth (i);
          if (!bp)
            continue;
          if (bp == selected_buffer ())
            cur = i;
          if (bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_MODIFIED)
            ;
          else if (bp->b_modified
                   ? !(bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_LAST_MODIFIED_FLAG)
                   : bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_LAST_MODIFIED_FLAG)
            ;
          else
            {
              COLORREF fg, bg;
              tab_color (bp, fg, bg);
              if (bp->b_buffer_bar_fg != fg
                  || bp->b_buffer_bar_bg != bg)
                {
                  bp->b_buffer_bar_fg = fg;
                  bp->b_buffer_bar_bg = bg;
                  set_no_redraw ();
                  mod = 1;
                }
              continue;
            }
          set_no_redraw ();
          mod = 1;
          bp->b_buffer_bar_modified &= ~Buffer::BUFFER_BAR_MODIFIED;
          modify (bp, i);
        }
    }
  else
    cur = -2;

  if (cur >= 1 && xsymbol_value (Vbuffer_bar_selected_buffer_to_first) != Qnil
      && GetFocus () != b_hwnd)
    {
      Buffer *bp = selected_buffer ();
      if (!bp->internal_buffer_p ())
        {
          set_cursel (0);
          delete_item (cur);
          insert (bp, 0);
          cur = 0;
          mod = 1;
        }
    }

  if (mod)
    InvalidateRect (b_hwnd, 0, 1);

  set_redraw ();
  if (cur >= -1)
    set_cursel (cur);

  b_last_buffer = selected_buffer ();
  Buffer::b_buffer_bar_modified_any = 0;
  tab_bar::update_ui ();
}

lisp
buffer_bar::context_menu (int i)
{
  Buffer *bp;
  lisp lbp;

  if (i < 0)
    {
      bp = 0;
      lbp = Qnil;
    }
  else
    {
      bp = nth (i);
      if (!bp)
        return Qnil;
      lbp = bp->lbp;
    }
  lisp hook = symbol_value (Vbuffer_bar_context_menu_handler, bp);
  return (hook != Qnil && hook != Qunbound
          ? funcall_1 (hook, lbp) : Qnil);
}

LRESULT
buffer_bar::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_DESTROY:
      RevokeDragDrop (b_hwnd);
      break;

    case WM_TIMER:
      if (wparam == DROP_TIMER_ID)
        {
          int index = b_drop_index;
          KillTimer (b_hwnd, DROP_TIMER_ID);
          b_drop_index = -1;
          if (index >= 0
              && (app.drag_window || app.kbdq.idlep ())
              && !selected_window ()->minibuffer_window_p ())
            {
              Buffer *bp = nth (index);
              if (bp)
                {
                  try
                    {
                      selected_buffer ()->run_hook (Vbuffer_bar_hook, bp->lbp);
                    }
                  catch (nonlocal_jump &)
                    {
                      print_condition (nonlocal_jump::data ());
                    }
                  refresh_screen (1);
                }
            }
          return 0;
        }
      break;
    }
  return tab_bar::wndproc (msg, wparam, lparam);
}

void
buffer_bar::drag_enter (int x, int y)
{
  b_drop_index = -1;
  drag_over (x, y);
}

void
buffer_bar::drag_over (int x, int y)
{
  TC_HITTESTINFO htinfo;
  htinfo.pt.x = x;
  htinfo.pt.y = y;
  ScreenToClient (b_hwnd, &htinfo.pt);
  int index = hit_test (htinfo);
  if (index != b_drop_index)
    {
      KillTimer (b_hwnd, DROP_TIMER_ID);
      if (index >= 0 && index != get_cursel ())
        {
          b_drop_index = index;
          SetTimer (b_hwnd, DROP_TIMER_ID, 1000, 0);
        }
      else
        b_drop_index = -1;
    }
}

void
buffer_bar::drag_leave ()
{
  KillTimer (b_hwnd, DROP_TIMER_ID);
  b_drop_index = -1;
}

lisp
buffer_bar::buffer_list () const
{
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    bp->b_buffer_bar_modified &= ~Buffer::BUFFER_BAR_MARK;
  lisp r = Qnil;
  for (int i = 0, n = item_count (); i < n; i++)
    {
      Buffer *bp = nth (i);
      if (bp)
        {
          r = xcons (bp->lbp, r);
          bp->b_buffer_bar_modified |= Buffer::BUFFER_BAR_MARK;
        }
    }
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (bp->b_buffer_bar_modified & Buffer::BUFFER_BAR_MARK)
      bp->b_buffer_bar_modified &= ~Buffer::BUFFER_BAR_MARK;
    else
      r = xcons (bp->lbp, r);
  return Fnreverse (r);
}

lisp
Fcreate_buffer_bar ()
{
  if (!buffer_bar::make_instance ())
    FEsimple_error (ECannot_create_toolbar);
  return Vbuffer_bar;
}
