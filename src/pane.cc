#include "stdafx.h"
#include "ed.h"
#include "mainframe.h"
#include "resource.h"

static int BORDER_SIZE = 3;

#define GRIPPER_WIDTH 4
#define MIN_PANE_SIZE 2

#if 0
#include <stdio.h>
static void
log (const char *fmt, ...)
{
  FILE *fp = fopen ("foo.log", "a");
  if (fp)
    {
      va_list ap;
      va_start (ap,fmt);
      vfprintf (fp, fmt, ap);
      va_end (ap);
      fclose (fp);
    }
}
#endif

pane::pane (splitter *spl, HWND hwnd, int cx, int cy, int flags)
     : p_prev (0),
       p_next (0),
       p_parent (spl),
       p_hwnd (hwnd),
       p_flags (flags),
       p_vert ((flags & XPIS_MASK) == XPIS_LEFT
               || (flags & XPIS_MASK) == XPIS_RIGHT),
       p_cursize (-1),
       p_idealsize (p_vert ? cx : cy),
       p_minsize (-1),
       p_maxsize (-1),
       p_step (1),
       p_initialized (0)
{
  p_reqsize.cx = cx;
  p_reqsize.cy = cy;
  memset (&p_rect, 0, sizeof p_rect);
  p_initialized = 1;
}

splitter::splitter ()
     : s_head (0),
       s_tail (0),
       s_in_resize (0),
       s_terminating (0)
{
}

splitter::~splitter ()
{
  s_terminating = 1;
  for (pane *p = s_head, *next; p; p = next)
    {
      next = p->next ();
      delete p;
    }
}

void
splitter::init (HWND hwnd, HWND hwnd_frame)
{
  s_hwnd = hwnd;
  s_hwnd_frame = hwnd_frame;
}

void
splitter::link_pane (pane *p)
{
  if (!s_head)
    s_head = s_tail = p;
  else
    {
      s_tail->next () = p;
      p->prev () = s_tail;
      s_tail = p;
    }
}

void
splitter::unlink_pane (pane *p)
{
  if (p->prev ())
    p->prev ()->next () = p->next ();
  else
    s_head = p->next ();
  if (p->next ())
    p->next ()->prev () = p->prev ();
  else
    s_tail = p->prev ();
}

pane *
splitter::add_pane (HWND hwnd, int cx, int cy, int flags)
{
  pane *p = new pane (this, hwnd, cx, cy, flags);
  if (!p)
    return 0;
  if (!p->good ())
    {
      delete p;
      return 0;
    }
  link_pane (p);
  return p;
}

void
splitter::remove_pane (pane *p)
{
  if (!s_terminating)
    {
      unlink_pane (p);
      delete p;
      calc_geometry ();
    }
}

pane *
splitter::create_pane (HWND hwnd, int cx, int cy, int flags)
{
  if (!hwnd || GetParent (hwnd) != s_hwnd)
    return 0;
  pane *p = add_pane (hwnd, cx, cy, flags);
  if (p)
    calc_geometry ();
  return p;
}

void
pane::calc_geometry (RECT &r)
{
  p_rect = r;
  int place = p_flags & XPIS_MASK;
  int sz = (p_vert ? r.right - r.left : r.bottom - r.top);

  if (sz <= 0)
    {
      p_cursize = 0;
      if (p_vert)
        r.left = r.right;
      else
        r.top = r.bottom;
    }
  else
    {
      p_cursize = min (p_idealsize, sz);
      if (p_cursize > sz - (GRIPPER_WIDTH + BORDER_SIZE)
          && sz >= GRIPPER_WIDTH + BORDER_SIZE)
        p_cursize = sz - (GRIPPER_WIDTH + BORDER_SIZE);
      switch (place)
        {
        case XPIS_LEFT:
          r.left += p_cursize + BORDER_SIZE;
          r.left = min (r.left, r.right);
          break;
        case XPIS_RIGHT:
          r.right -= p_cursize + BORDER_SIZE;
          r.right = max (r.left, r.right);
          break;
        case XPIS_TOP:
          r.top += p_cursize + BORDER_SIZE;
          r.top = min (r.top, r.bottom);
          break;
        default:
          r.bottom -= p_cursize + BORDER_SIZE;
          r.bottom = max (r.top, r.bottom);
          break;
        }
    }

  RECT rr = p_rect;
  switch (place)
    {
    case XPIS_LEFT:
      rr.right = rr.left + p_cursize;
      break;
    case XPIS_RIGHT:
      rr.left = rr.right - p_cursize;
      break;
    case XPIS_TOP:
      rr.bottom = rr.top + p_cursize;
      break;
    default:
      rr.top = rr.bottom - p_cursize;
      break;
    }

  SetWindowPos (p_hwnd, 0, rr.left, rr.top,
                rr.right - rr.left, rr.bottom - rr.top,
                SWP_NOZORDER | SWP_NOACTIVATE);
}

void
splitter::calc_geometry (pane *p, const RECT &rr)
{
  RECT r = rr;
  s_in_resize = 1;
  for (; p; p = p->next ())
    p->calc_geometry (r);
  SetWindowPos (s_hwnd_frame, 0, r.left, r.top,
                r.right - r.left, r.bottom - r.top,
                SWP_NOZORDER | SWP_NOACTIVATE);
  s_in_resize = 0;
}

void
splitter::calc_geometry ()
{
  if (s_in_resize)
    return;
  calc_geometry (s_head, s_rect);
}

void
splitter::resize (const RECT &r)
{
  s_rect = r;
  calc_geometry ();
}

pane *
splitter::find_pane (XPIHANDLE h) const
{
  for (pane *p = s_head; p; p = p->next ())
    if ((pane *)h == p)
      return p;
  return 0;
}

pane *
splitter::find_pane (HWND hwnd) const
{
  for (pane *p = s_head; p; p = p->next ())
    if (p->hwnd () == hwnd)
      return p;
  return 0;
}

pane *
splitter::find_pane (const POINT &pos) const
{
  pane *p;
  for (p = s_tail; p; p = p->prev ())
    if (PtInRect (&p->rect (), pos))
      {
        RECT r;
        GetWindowRect (p->hwnd (), &r);
        ScreenToClient (s_hwnd, (POINT *)&r);
        ScreenToClient (s_hwnd, (POINT *)&r + 1);
        if (PtInRect (&r, pos))
          return 0;
        break;
      }
  return p;
}

int
splitter::set_cursor ()
{
  POINT pos;
  GetCursorPos (&pos);
  ScreenToClient (s_hwnd, &pos);
  pane *p = find_pane (pos);
  if (!p)
    return 0;
  p->set_cursor ();
  return 1;
}

int
pane::calc_size (const POINT &opos, int x, int y, int minsz, int maxsz, int osz) const
{
  int sz;
  switch (p_flags & XPIS_MASK)
    {
    case XPIS_LEFT:
      sz = p_cursize + x - opos.x;
      break;

    case XPIS_TOP:
      sz = p_cursize + y - opos.y;
      break;

    case XPIS_RIGHT:
      sz = p_cursize + opos.x - x;
      break;

    default:
      sz = p_cursize + opos.y - y;
      break;
    }
  int nsz = sz < osz ? sz + p_step - 1 : sz;
  nsz = nsz / p_step * p_step;
  if (sz < osz && nsz > osz)
    nsz -= p_step;
  nsz = min (max (nsz, minsz), maxsz);
  return nsz;
}

void
pane::calc_rect (RECT &r, int sz) const
{
  switch (p_flags & XPIS_MASK)
    {
    case XPIS_LEFT:
      r.left = p_rect.left + sz - GRIPPER_WIDTH / 2;
      r.right = r.left + GRIPPER_WIDTH;
      break;

    case XPIS_TOP:
      r.top = p_rect.top + sz - GRIPPER_WIDTH / 2;
      r.bottom = r.top + GRIPPER_WIDTH;
      break;

    case XPIS_RIGHT:
      r.left = p_rect.right - sz - GRIPPER_WIDTH;
      r.right = r.left + GRIPPER_WIDTH;
      break;

    default:
      r.top = p_rect.bottom - sz - GRIPPER_WIDTH;
      r.bottom = r.top + GRIPPER_WIDTH;
      break;
    }
}

int
pane::move_splitter (const POINT &pos)
{
  int sz = p_vert ? p_rect.right - p_rect.left : p_rect.bottom - p_rect.top;
  int mn = sz, mx = sz;
  if (p_minsize > 0)
    mn = p_minsize;
  if (p_maxsize > 0)
    mx = p_maxsize;

  mn = min (min (mn, MIN_PANE_SIZE + BORDER_SIZE), p_cursize);
  mx = max (min (mx, sz - (GRIPPER_WIDTH + BORDER_SIZE)), p_cursize);

  sz = p_cursize;

  RECT r (p_rect);
  calc_rect (r, sz);

  HWND hwnd = p_parent->hwnd ();
  SetCapture (hwnd);
  frameDC fdc (hwnd);
  fdc.paint (r);

  MSG msg;
  while (1)
    {
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          break;
        }
      if (GetCapture () != hwnd)
        break;
      switch (msg.message)
        {
        case WM_MOUSEMOVE:
        case WM_LBUTTONUP:
          fdc.paint (r);
          sz = calc_size (pos, short (LOWORD (msg.lParam)),
                          short (HIWORD (msg.lParam)), mn, mx, sz);
          calc_rect (r, sz);
          if (msg.message == WM_MOUSEMOVE)
            {
              fdc.paint (r);
              break;
            }
          ReleaseCapture ();
          p_idealsize = sz;
          p_parent->calc_geometry (this, p_rect);
          return 1;

        case WM_CANCELMODE:
          ReleaseCapture ();
          goto done;

        case WM_KEYDOWN:
          break;

        default:
          DispatchMessage (&msg);
          break;
        }
    }
done:
  fdc.paint (r);
  return 1;
}

int
splitter::move_splitter (int x, int y)
{
  POINT pos;
  pos.x = x;
  pos.y = y;
  pane *p = find_pane (pos);
  if (!p)
    return 0;
  return p->move_splitter (pos);
}

int
pane::set_size (int sz, int mn, int mx, int step)
{
  p_minsize = mn >= 0 ? mn : -1;
  p_maxsize = mx >= 0 ? mx : -1;
  if (p_maxsize >= 0 && p_minsize >= 0 && p_maxsize < p_minsize)
    p_maxsize = p_minsize;
  if (step > 0)
    p_step = step;
  if (p_minsize > 0)
    p_minsize = p_minsize / p_step * p_step;
  if (p_maxsize > 0)
    p_maxsize = p_maxsize / p_step * p_step;
  if (sz >= 0)
    {
      p_idealsize = sz / p_step * p_step;
      p_parent->calc_geometry (this, p_rect);
    }
  return 1;
}

void
splitter::recalc_order (pane *p, int flags)
{
  if (p->prev ())
    p->prev ()->next () = p->next ();
  else
    s_head = p->next ();
  if (p->next ())
    p->next ()->prev () = p->prev ();
  else
    s_tail = p->prev ();

  pane *next;
  if ((flags & XPIS_ORDMASK) == XPIS_INSIDE)
    next = 0;
  else
    {
      if (!(flags & XPIS_GROUP))
        next = s_head;
      else
        for (next = s_head; next; next = next->next ())
          if (p->pflags () == next->pflags ())
            break;
    }

  if (next)
    {
      p->prev () = next->prev ();
      p->next () = next;
      if (p->prev ())
        p->prev ()->next () = p;
      else
        s_head = p;
      if (p->next ())
        p->next ()->prev () = p;
      else
        s_tail = p;
    }
  else
    {
      p->prev () = s_tail;
      if (s_tail)
        s_tail->next () = p;
      else
        s_head = p;
      p->next () = 0;
      s_tail = p;
    }
}

int
pane::set_pos (int flags)
{
  switch (flags & XPIS_POSMASK)
    {
    case XPIS_LEFT:
    case XPIS_TOP:
    case XPIS_RIGHT:
    case XPIS_BOTTOM:
      p_flags &= ~XPIS_MASK;
      p_flags |= flags & XPIS_MASK;
      p_vert = ((flags & XPIS_MASK) == XPIS_LEFT
                || (flags & XPIS_MASK) == XPIS_RIGHT);
      break;
    }

  if ((flags & XPIS_ORDMASK) != XPIS_NOORDER)
    p_parent->recalc_order (this, flags);
  p_parent->calc_geometry ();
  return 1;
}

XPIHANDLE WINAPI
xpiCreatePane (HWND hwnd, int cx, int cy, DWORD flags)
{
  return (XPIHANDLE)g_frame.m_splitter.create_pane (hwnd, cx, cy, flags);
}

BOOL WINAPI
xpiSetPaneSize (XPIHANDLE h, int size, int min, int max, int step)
{
  pane *p = g_frame.m_splitter.find_pane (h);
  return p ? p->set_size (size, min, max, step) : 0;
}

BOOL WINAPI
xpiSetPanePos (XPIHANDLE h, DWORD flags)
{
  pane *p = g_frame.m_splitter.find_pane (h);
  return p ? p->set_pos (flags) : 0;
}

lisp
Fsi_plugin_arg ()
{
  static FARPROC procs[] =
    {
      (FARPROC)xpiCreatePane,
      (FARPROC)xpiSetPaneSize,
      (FARPROC)xpiSetPanePos,
      0,
    };
  return make_fixnum (long (procs));
}
