#include "stdafx.h"
#include "ed.h"
#include "dockbar.h"
#include "mman.h"

const char dock_bar::b_dock_bar_prop[] = "dock_bar::prop";
char dock_bar::b_ttbuf[TTBUFSIZE];
const char tab_bar::b_tab_bar_spin_prop[] = "tab_bar::spin::prop";

dock_bar::dock_bar (dock_frame &frame, lisp name, int dockable)
     : b_hwnd (0), b_wndproc (0), b_frame (frame), b_lname (name),
       b_edge (EDGE_TOP), b_border (BORDER_ALL),
       b_dockable (dockable), b_status (0)
{
}

dock_bar::~dock_bar ()
{
  unsubclass ();
  if (IsWindow (b_hwnd))
    DestroyWindow (b_hwnd);
}

int
dock_bar::check_edge (int edge) const
{
  if (!(dockable () & (1 << edge)))
    for (edge = 0; edge < EDGE_MAX; edge++)
      if (dockable () & (1 << edge))
        break;
  return edge;
}

int
dock_bar::subclass ()
{
  if (!SetProp (b_hwnd, b_dock_bar_prop, HANDLE (this)))
    return 0;
  b_wndproc = (WNDPROC)SetWindowLong (b_hwnd, GWL_WNDPROC, LONG (WNDPROC (wndproc)));
  if (b_wndproc)
    return 1;
  RemoveProp (b_hwnd, b_dock_bar_prop);
  return 0;
}

void
dock_bar::unsubclass ()
{
  if (b_wndproc)
    {
      SetWindowLong (b_hwnd, GWL_WNDPROC, LONG (b_wndproc));
      RemoveProp (b_hwnd, b_dock_bar_prop);
      b_wndproc = 0;
    }
}

void
dock_bar::set_redraw ()
{
  if (status () & DOCK_STAT_NOREDRAW)
    {
      InvalidateRect (b_hwnd, 0, 0);
      sendmsg (WM_SETREDRAW, 1, 0);
      modify_status (DOCK_STAT_NOREDRAW, 0);
    }
}

void
dock_bar::set_no_redraw ()
{
  if (!(status () & DOCK_STAT_NOREDRAW))
    {
      sendmsg (WM_SETREDRAW, 0, 0);
      modify_status (0, DOCK_STAT_NOREDRAW);
    }
}

void
dock_bar::calc_window_size (SIZE &sz, int vert) const
{
  calc_client_size (sz, vert);
  if (!vert)
    {
      sz.cx += HORZ_LEFT_PAD + HORZ_RIGHT_PAD;
      sz.cy += HORZ_TOP_PAD + HORZ_BOTTOM_PAD;
    }
  else
    {
      sz.cx += VERT_LEFT_PAD + VERT_RIGHT_PAD;
      sz.cy += VERT_TOP_PAD + VERT_BOTTOM_PAD;
    }
}

void
dock_bar::draw_borders (HDC hdc, RECT &r) const
{
  int oleft = r.left;
  if (border () & BORDER_LEFT)
    {
      draw_vline (hdc, r.top, r.bottom, r.left, sysdep.btn_highlight);
      r.left++;
    }
  if (border () & BORDER_TOP)
    {
      draw_hline (hdc, r.left, r.right, r.top, sysdep.btn_highlight);
      r.top++;
    }
  if (border () & BORDER_RIGHT)
    {
      r.right--;
      draw_vline (hdc, r.top, r.bottom, r.right, sysdep.btn_shadow);
    }
  if (border () & BORDER_BOTTOM)
    {
      r.bottom--;
      draw_hline (hdc, oleft, r.right, r.bottom, sysdep.btn_shadow);
    }
}

void
dock_bar::draw_gripper (HDC hdc, const RECT &r) const
{
  if (!dock_vert_p ())
    {
      draw_vline (hdc, r.top + 2, r.bottom - 2, r.left + 2, sysdep.btn_highlight);
      draw_vline (hdc, r.top + 1, r.bottom - 2, r.left + 4, sysdep.btn_shadow);
      draw_hline (hdc, r.left + 2, r.left + 4, r.top + 1, sysdep.btn_highlight);
      draw_hline (hdc, r.left + 2, r.left + 5, r.bottom - 2, sysdep.btn_shadow);
    }
  else
    {
      draw_vline (hdc, r.top + 2, r.top + 4, r.left + 1, sysdep.btn_highlight);
      draw_hline (hdc, r.left + 2, r.right - 2, r.top + 2, sysdep.btn_highlight);
      draw_vline (hdc, r.top + 2, r.top + 5, r.right - 2, sysdep.btn_shadow);
      draw_hline (hdc, r.left + 1, r.right - 2, r.top + 4, sysdep.btn_shadow);
    }
}

void
dock_bar::erase_non_client () const
{
  RECT cr, wr;
  GetClientRect (b_hwnd, &cr);
  GetWindowRect (b_hwnd, &wr);
  MapWindowPoints (HWND_DESKTOP, b_hwnd, (POINT *)&wr, 2);
  cr.top = max (cr.top, wr.top);
  cr.bottom = min (cr.bottom, wr.bottom);
  OffsetRect (&cr, -wr.left, -wr.top);
  OffsetRect (&wr, -wr.left, -wr.top);

  HDC hdc = GetWindowDC (b_hwnd);
  draw_borders (hdc, wr);
  ExcludeClipRect (hdc, cr.left, cr.top, cr.right, cr.bottom);
#if 0
  IntersectClipRect (hdc, wr.left, wr.top, wr.right, wr.bottom);
  sendmsg (WM_ERASEBKGND, WPARAM (hdc), 0);
#else
  fill_rect (hdc, wr, sysdep.btn_face);
#endif
  adjust_gripper (hdc, wr, cr);
  draw_gripper (hdc, wr);
  ReleaseDC (b_hwnd, hdc);
}

int
dock_bar::nc_calc_size (RECT &r) const
{
  if (!dock_vert_p ())
    {
      r.left += HORZ_LEFT_PAD;
      r.right -= HORZ_RIGHT_PAD;
      r.top += HORZ_TOP_PAD;
      r.bottom -= HORZ_BOTTOM_PAD;
    }
  else
    {
      r.left += VERT_LEFT_PAD;
      r.right -= VERT_RIGHT_PAD;
      r.top += VERT_TOP_PAD;
      r.bottom -= VERT_BOTTOM_PAD;
    }
  return 0;
}

int
dock_bar::lbtn_down (int x, int y)
{
  if ((!dock_vert_p () ? x : y) >= 0)
    return 0;
  b_frame.move_bar (this, x, y);
  return 1;
}

LRESULT CALLBACK
dock_bar::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  dock_bar *bar = from_hwnd (hwnd);
  return bar ? bar->wndproc (msg, wparam, lparam) : 0;
}

LRESULT
dock_bar::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_NCCALCSIZE:
      return nc_calc_size (*(RECT *)lparam);

    case WM_NCPAINT:
      erase_non_client ();
      return 0;

    case WM_NCHITTEST:
      return HTCLIENT;

    case WM_LBUTTONDOWN:
      if (!app.kbdq.idlep ())
        return 0;
      if (lbtn_down (short (LOWORD (lparam)), short (HIWORD (lparam))))
        return 0;
      break;

    case WM_LBUTTONUP:
    case WM_LBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_RBUTTONDBLCLK:
    case WM_XBUTTONDOWN:
    case WM_XBUTTONUP:
    case WM_XBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_MOUSEWHEEL:
      if (!app.kbdq.idlep ())
        return 0;
      break;

    case WM_KEYDOWN:
      if (!app.kbdq.idlep ())
        return 0;
      switch (wparam)
        {
        case VK_TAB:
          if (GetKeyState (VK_SHIFT) >= 0)
            b_frame.focus_next (this);
          else
            b_frame.focus_prev (this);
          return 0;

        case VK_APPS:
          if (do_context_menu (0))
            return 0;
          break;

        default:
          break;
        }
      break;

    case WM_CONTEXTMENU:
      if (!app.kbdq.idlep ())
        return 0;
      if (HWND (wparam) == b_hwnd)
        {
          POINT pt;
          pt.x = short (LOWORD (lparam));
          pt.y = short (HIWORD (lparam));
          ScreenToClient (b_hwnd, &pt);
          if (do_context_menu (&pt))
            return 0;
        }
      break;

    case WM_NCDESTROY:
      sendmsg (msg, wparam, lparam);
      unsubclass ();
      b_hwnd = 0;
      post_nc_destroy ();
      return 0;
    }
  return sendmsg (msg, wparam, lparam);
}

tool_bar::tool_bar (dock_frame &frame, lisp name)
     : dock_bar (frame, name, DOCKABLE_ALL), t_bm (0)
{
  t_bitmap_size.cx = 16;
  t_bitmap_size.cy = 15;
  t_button_size.cx = 23;
  t_button_size.cy = 22;
}

tool_bar::~tool_bar ()
{
  if (t_bm)
    b_frame.bm ().release (t_bm);
}

int
tool_bar::nc_calc_size (RECT &r) const
{
  dock_bar::nc_calc_size (r);
  if (!dock_vert_p () && !tb_flat_p ())
    r.top -= 2;
  return 0;
}

void
tool_bar::erase_bkgnd (HDC hdc) const
{
  RECT r;
  GetClientRect (b_hwnd, &r);
  fill_rect (hdc, r, sysdep.btn_face);
}

LRESULT
tool_bar::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_NCCALCSIZE:
      return nc_calc_size (*(RECT *)lparam);

    case WM_ERASEBKGND:
      erase_bkgnd (HDC (wparam));
      return 1;
    }
  return dock_bar::wndproc (msg, wparam, lparam);
}

int
tool_bar::create (HWND hwnd_parent, DWORD style, UINT id)
{
  if (!dock_bar::create (0, TOOLBARCLASSNAME, 0,
                         style, 0, 0, 0, 0, hwnd_parent,
                         (HMENU)id, app.hinst, 0))
    return 0;
  sendmsg (TB_BUTTONSTRUCTSIZE, sizeof (TBBUTTON), 0);
  return 1;
}

void
tool_bar::calc_client_size (SIZE &sz, int vert) const
{
  if (!vert)
    {
      sz.cx = 0;
      sz.cy = t_button_size.cy;
      int n = button_count ();
      for (int i = 0; i < n; i++)
        {
          TBBUTTON b;
          get_button (i, b);
          if (b.fsState & TBSTATE_HIDDEN)
            continue;
          if (b.fsStyle & TBSTYLE_SEP)
            sz.cx += b.iBitmap;
          else
            sz.cx += t_button_size.cx;
        }
    }
  else
    {
      sz.cx = t_button_size.cx;
      sz.cy = 0;
      int flat_p = tb_flat_p ();
      if (!flat_p)
        sz.cy += 2;
      int n = button_count ();
      for (int i = 0; i < n; i++)
        {
          TBBUTTON b;
          get_button (i, b);
          if (b.fsState & TBSTATE_HIDDEN)
            continue;
          if (b.fsStyle & TBSTYLE_SEP)
            sz.cy += flat_p ? b.iBitmap : b.iBitmap * 2 / 3;
          else
            sz.cy += t_button_size.cy - 1;
        }
    }
}

void
tool_bar::set_button (int i, const TBBUTTON &tb, int sep)
{
  DWORD ostyle = style ();
  set_style (ostyle & ~WS_VISIBLE);
  delete_button (i);
  insert_button (i, tb);
  set_style (ostyle);

  if (sep)
    InvalidateRect (b_hwnd, 0, 0);
  else
    {
      RECT r;
      if (item_rect (i, r))
        InvalidateRect (b_hwnd, &r, 0);
    }
}

void
tool_bar::set_bitmap ()
{
  BITMAP bm;
  GetObject (*t_bm, sizeof bm, &bm);

  TBADDBITMAP tbab;
  tbab.hInst = 0;
  tbab.nID = (UINT)(HBITMAP)*t_bm;
  add_bitmap (tbab, bm.bmWidth / 16);
}

int
tool_bar::load_bitmap (const char *filename)
{
  int e;
  t_bm = b_frame.bm ().load (filename, e);
  if (t_bm)
    set_bitmap ();
  return e;
}

void
tool_bar::reload_settings ()
{
  set_bitmap ();
  InvalidateRect (b_hwnd, 0, 1);
}

void
tool_bar::dock_edge ()
{
  dock_bar::dock_edge ();
  if (dock_vert_p ())
    {
      modify_style (TBSTYLE_WRAPABLE, 0);
      modify_style (0, TBSTYLE_WRAPABLE);
    }
  else
    {
      modify_style (0, TBSTYLE_WRAPABLE);
      modify_style (TBSTYLE_WRAPABLE, 0);

      if (!new_comctl_p ())
        {
          int n = button_count ();
          for (int i = 0; i < n; i++)
            {
              TBBUTTON b;
              if (get_button (i, b) && b.fsState & TBSTATE_WRAP)
                {
                  b.fsState &= ~TBSTATE_WRAP;
                  set_button (i, b);
                }
            }
        }
    }
}

tab_bar::tab_bar (dock_frame &frame, lisp name)
     : dock_bar (frame, name,
                 new_comctl_p () ? DOCKABLE_ALL : DOCKABLE_TOP | DOCKABLE_BOTTOM),
       t_tab_height (21), t_horz_width (60), t_horz_height (21)
{
  t_horz_text = xsymbol_value (Vtab_bar_horizontal_text) != Qnil;
}

int
tab_bar::create (HWND hwnd_parent)
{
  if (!create (hwnd_parent,
               (WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN
                | CCS_NORESIZE | CCS_NOPARENTALIGN | CCS_NODIVIDER
                | TCS_TOOLTIPS | (xsymbol_value (Vtab_bar_never_focus) != Qnil
                                  ? TCS_FOCUSNEVER : 0)),
               0))
    return 0;

  set_padding (6, 4);
  set_font (sysdep.ui_font ());
  calc_tab_height ();

  HWND hwnd_tt = get_tooltips ();
  if (hwnd_tt)
    set_tooltip_no_prefix (hwnd_tt);

  return 1;
}

DWORD
tab_bar::nth (int i) const
{
  TC_ITEM ti;
  ti.mask = TCIF_PARAM;
  return get_item (i, ti) ? ti.lParam : 0;
}

void
tab_bar::modify_spin ()
{
  HWND hwnd_spin = GetDlgItem (b_hwnd, IDC_TAB_SPIN);
  if (!hwnd_spin)
    return;

  HWND *buf = (HWND *)GetWindowLong (b_hwnd, 0);

  int offset;
  if (IsBadWritePtr (buf, sizeof *buf * 10)
      || (buf[offset = 9] != hwnd_spin
          && buf[offset = 6] != hwnd_spin)
      || buf[6] == buf[9])
    return;

  DWORD style = GetWindowLong (hwnd_spin, GWL_STYLE);
  if (style & UDS_HORZ ? !dock_vert_p () : dock_vert_p ())
    return;

  HWND hwnd = CreateWindowEx (GetWindowLong (hwnd_spin, GWL_EXSTYLE),
                              UPDOWN_CLASS, "", (style ^ UDS_HORZ) & ~UDS_WRAP,
                              0, 0, 0, 0, b_hwnd, HMENU (IDC_TAB_SPIN),
                              app.hinst, 0);
  if (!hwnd)
    return;

  DestroyWindow (hwnd_spin);
  buf[offset] = hwnd;
}

void
tab_bar::dock_edge ()
{
  dock_bar::dock_edge ();
  if (new_comctl_p ())
    {
      modify_spin ();
      switch (edge ())
        {
        case EDGE_TOP:
          modify_style (TCS_VERTICAL | TCS_BOTTOM | TCS_FIXEDWIDTH, 0);
          set_font (sysdep.ui_font ());
          set_item_size (0, 0);
          break;

        default:
          modify_style (TCS_VERTICAL | TCS_FIXEDWIDTH, TCS_BOTTOM);
          set_font (sysdep.ui_font ());
          set_item_size (0, 0);
          break;

        case EDGE_LEFT:
          if (t_horz_text)
            {
              modify_style (TCS_RIGHT, TCS_VERTICAL | TCS_FIXEDWIDTH);
              set_font (sysdep.ui_font ());
              set_item_size (t_horz_height, t_horz_width);
            }
          else
            {
              modify_style (TCS_RIGHT | TCS_FIXEDWIDTH, TCS_VERTICAL);
              set_font (sysdep.ui_font90 ());
              set_item_size (0, 0);
            }
          break;

        case EDGE_RIGHT:
          if (t_horz_text)
            {
              modify_style (0, TCS_VERTICAL | TCS_RIGHT | TCS_FIXEDWIDTH);
              set_font (sysdep.ui_font ());
              set_item_size (t_horz_height, t_horz_width);
            }
          else
            {
              modify_style (TCS_FIXEDWIDTH, TCS_VERTICAL | TCS_RIGHT);
              set_font (sysdep.ui_font270 ());
              set_item_size (0, 0);
            }
          break;
        }
    }
}

void
tab_bar::calc_tab_height ()
{
  int nitem = item_count ();
  if (!nitem)
    {
      TC_ITEM ti;
      ti.mask = TCIF_TEXT;
      ti.pszText = "xyzzy";
      insert_item (0, ti);
    }

  RECT r;
  bzero (&r, sizeof r);
  adjust_rect (0, r);
  t_tab_height = inverse_p () ? -r.bottom : r.top;
  get_item_rect (0, r);
  t_horz_height = r.bottom - r.top;
  if (!nitem)
    delete_item (0);

  HDC hdc = GetDC (b_hwnd);
  HGDIOBJ of = SelectObject (hdc, sysdep.ui_font ());
  SIZE sz;
  GetTextExtentPoint32 (hdc, "...", 3, &sz);
  t_dots = sz.cx;
  SelectObject (hdc, of);
  ReleaseDC (b_hwnd, hdc);
}

void
tab_bar::update_ui ()
{
  long x = style ();
  if ((x & TCS_FOCUSNEVER) != (xsymbol_value (Vtab_bar_never_focus) != Qnil
                               ? TCS_FOCUSNEVER : 0))
    {
      x ^= TCS_FOCUSNEVER;
      set_style (x);
    }
}

void
tab_bar::calc_client_size (SIZE &sz, int vert) const
{
  if (!vert)
    {
      sz.cx = DOCK_LENGTH_INFINITE;
      sz.cy = DOCK_BAR_CLIENT_HEIGHT;
    }
  else if (t_horz_text)
    {
      sz.cx = t_horz_width + GRIPPER_SIZE;
      sz.cy = DOCK_LENGTH_INFINITE;
    }
  else
    {
      sz.cx = DOCK_BAR_CLIENT_HEIGHT + 1;
      sz.cy = DOCK_LENGTH_INFINITE;
    }
}

int
tab_bar::abbrev_text (HDC hdc, char *s0, int l, int cx) const
{
  cx -= t_dots;
  if (cx <= 0)
    return 0;

  SIZE sz;
  char *se = s0 + l;
  do
    {
      se = CharPrev (s0, se);
      if (se == s0)
        break;
      GetTextExtentPoint32 (hdc, s0, se - s0, &sz);
    }
  while (sz.cx > cx);
  strcpy (se, "...");
  return se - s0 + 3;
}

void
tab_bar::draw_item (const draw_item_struct &dis, char *s, int l,
                    COLORREF fg, COLORREF bg) const
{
  SIZE sz;
  GetTextExtentPoint32 (dis.hdc, s, l, &sz);

  int x, y;
  switch (edge ())
    {
    default:
      x = (dis.r.left + dis.r.right - sz.cx) / 2;
      y = (dis.r.top + dis.r.bottom - sz.cy) / 2;
      if (dis.state & ODS_SELECTED)
        x--;
      if (edge () == EDGE_TOP)
        y++;
      break;

    case EDGE_LEFT:
      if (t_horz_text)
        goto horz_text;
      x = (dis.r.left + dis.r.right - sz.cy) / 2;
      y = (dis.r.top + dis.r.bottom + sz.cx) / 2;
      if (dis.state & ODS_SELECTED)
        y++;
      break;

    case EDGE_RIGHT:
      if (t_horz_text)
        goto horz_text;
      x = (dis.r.left + dis.r.right + sz.cy) / 2;
      y = (dis.r.top + dis.r.bottom - sz.cx) / 2;
      if (dis.state & ODS_SELECTED)
        y--;
      break;

    horz_text:
      {
        int cx = dis.r.right - dis.r.left - 8;
        x = dis.r.left + 5;
        y = (dis.r.top + dis.r.bottom - sz.cy) / 2;
        if (dis.state & ODS_SELECTED)
          y++;
        if (sz.cx > cx)
          l = abbrev_text (dis.hdc, s, l, cx);
        break;
      }
    }

  COLORREF ofg = SetTextColor (dis.hdc, fg);
  COLORREF obg = SetBkColor (dis.hdc, bg);
  ExtTextOut (dis.hdc, x, y, ETO_CLIPPED | ETO_OPAQUE, &dis.r, s, l, 0);
  SetTextColor (dis.hdc, ofg);
  SetBkColor (dis.hdc, obg);
  if (dis.state & ODS_SELECTED && GetFocus () == b_hwnd)
    {
      RECT r (dis.r);
      InflateRect (&r, -1, -1);
      DrawFocusRect (dis.hdc, &r);
    }
}

void
tab_bar::erase_bkgnd (HDC hdc)
{
  RECT cr, wr;
  GetClientRect (b_hwnd, &cr);
  GetWindowRect (b_hwnd, &wr);
  MapWindowPoints (HWND_DESKTOP, b_hwnd, (POINT *)&wr, 2);
  if (border () & BORDER_LEFT)
    wr.left++;
  if (border () & BORDER_TOP)
    wr.top++;
  if (border () & BORDER_RIGHT)
    wr.right--;
  if (border () & BORDER_BOTTOM)
    wr.bottom--;
  cr.left = max (cr.left, wr.left);
  cr.top = max (cr.top, wr.top);
  cr.right = min (cr.right, wr.right);
  cr.bottom = min (cr.bottom, wr.bottom);
  IntersectClipRect (hdc, cr.left, cr.top, cr.right, cr.bottom);

  int n = item_count ();
  if (n > 0)
    {
      RECT r;
      get_item_rect (0, r);
      if (n > 1)
        {
          RECT r2;
          get_item_rect (n - 1, r2);
          r.left = min (r.left, r2.left);
          r.top = min (r.top, r2.top);
          r.right = max (r.right, r2.right);
          r.bottom = max (r.bottom, r2.bottom);
        }
      r.left = max (r.left, cr.left);
      r.top = max (r.top, cr.top);
      r.right = min (r.right, cr.right);
      r.bottom = min (r.bottom, cr.bottom);

      HWND hwnd_spin = GetDlgItem (b_hwnd, IDC_TAB_SPIN);
      if (hwnd_spin && IsWindowVisible (hwnd_spin))
        {
          RECT spin_r;
          GetWindowRect (hwnd_spin, &spin_r);
          MapWindowPoints (HWND_DESKTOP, b_hwnd, (POINT *)&spin_r, 2);
          if (!dock_vert_p ())
            r.right = min (r.right, spin_r.left);
          else
            r.bottom = min (r.bottom, spin_r.top);
        }
      InflateRect (&r, -1, -1);
      HRGN hrgn = CreateRectRgnIndirect (&r);
      ExtSelectClipRgn (hdc, hrgn, RGN_DIFF);
      DeleteObject (hrgn);
      fill_rect (hdc, cr, sysdep.btn_face);
      SelectClipRgn (hdc, 0);
    }
  else
    fill_rect (hdc, cr, sysdep.btn_face);

  t_erasebkgnd_called = 1;
}

static inline int
intersect_p (const RECT &r1, const RECT &r2)
{
  return (r1.left < r2.right && r1.top < r2.bottom
          && r1.right > r2.left && r1.bottom > r2.top);
}

void
tab_bar::paint_left (HDC hdc, const RECT &cr, const RECT &clip, int n)
{
  HGDIOBJ of = SelectObject (hdc, t_horz_text ? sysdep.ui_font () : sysdep.ui_font90 ());
  draw_item_struct dis;
  dis.hdc = hdc;
  int cur = get_cursel ();
  for (int i = 0; i < n; i++)
    {
      RECT r;
      get_item_rect (i, r);
      r.right = cr.right - 1;
      if (!intersect_p (r, clip))
        continue;
      dis.r = r;
      dis.data = nth (i);
      if (i == cur)
        {
          r.top--;
          r.bottom++;
          r.left -= 2;
          dis.state = ODS_SELECTED;
          dis.r.left--;
        }
      else
        {
          dis.state = 0;
          dis.r.top++;
          dis.r.bottom--;
          dis.r.left++;
        }
      if (cur < 0 || i != cur + 1)
        {
          draw_hline (hdc, r.left + 1, r.right, r.top, sysdep.btn_highlight);
          draw_vline (hdc, r.top + 1, r.bottom - 1, r.left, sysdep.btn_highlight);
        }
      else
        draw_vline (hdc, r.top + 2, r.bottom - 1, r.left, sysdep.btn_highlight);
      if (i != cur - 1)
        draw_hline (hdc, r.left + 1, r.right, r.bottom - 1, sysdep.btn_shadow);
      draw_item (dis);
    }
  SelectObject (hdc, of);
}

void
tab_bar::paint_top (HDC hdc, const RECT &cr, const RECT &clip, int n)
{
  HGDIOBJ of = SelectObject (hdc, sysdep.ui_font ());
  draw_item_struct dis;
  dis.hdc = hdc;
  int cur = get_cursel ();
  for (int i = 0; i < n; i++)
    {
      RECT r;
      get_item_rect (i, r);
      r.bottom = cr.bottom - 1;
      if (!intersect_p (r, clip))
        continue;
      dis.r = r;
      dis.data = nth (i);
      if (i == cur)
        {
          r.left--;
          r.right++;
          r.top -= 2;
          dis.state = ODS_SELECTED;
          dis.r.top--;
        }
      else
        {
          dis.state = 0;
          dis.r.left++;
          dis.r.right--;
          dis.r.top++;
        }
      if (cur < 0 || i != cur + 1)
        {
          draw_vline (hdc, r.top + 1, r.bottom, r.left, sysdep.btn_highlight);
          draw_hline (hdc, r.left + 1, r.right - 1, r.top, sysdep.btn_highlight);
        }
      else
        draw_hline (hdc, r.left + 2, r.right - 1, r.top, sysdep.btn_highlight);
      if (i != cur - 1)
        draw_vline (hdc, r.top + 1, r.bottom, r.right - 1, sysdep.btn_shadow);
      draw_item (dis);
    }
  SelectObject (hdc, of);
}

void
tab_bar::paint_right (HDC hdc, const RECT &cr, const RECT &clip, int n)
{
  HGDIOBJ of = SelectObject (hdc, t_horz_text ? sysdep.ui_font () : sysdep.ui_font270 ());
  draw_item_struct dis;
  dis.hdc = hdc;
  int cur = get_cursel ();
  for (int i = 0; i < n; i++)
    {
      RECT r;
      get_item_rect (i, r);
      r.left = cr.left + 1;
      if (!intersect_p (r, clip))
        continue;
      dis.r = r;
      dis.data = nth (i);
      if (i == cur)
        {
          r.top--;
          r.bottom++;
          r.right += 2;
          dis.state = ODS_SELECTED;
          dis.r.right++;
        }
      else
        {
          dis.state = 0;
          dis.r.top++;
          dis.r.bottom--;
          dis.r.right--;
        }
      if (cur < 0 || i != cur + 1)
        {
          draw_hline (hdc, r.left, r.right - 1, r.top, sysdep.btn_highlight);
          draw_vline (hdc, r.top + 1, r.bottom - 1, r.right - 1, sysdep.btn_shadow);
        }
      else
        draw_vline (hdc, r.top + 2, r.bottom - 1, r.right - 1, sysdep.btn_shadow);
      if (i != cur - 1)
        draw_hline (hdc, r.left, r.right - 1, r.bottom - 1, sysdep.btn_shadow);
      draw_item (dis);
    }
  SelectObject (hdc, of);
}

void
tab_bar::paint_bottom (HDC hdc, const RECT &cr, const RECT &clip, int n)
{
  HGDIOBJ of = SelectObject (hdc, sysdep.ui_font ());
  draw_item_struct dis;
  dis.hdc = hdc;
  int cur = get_cursel ();
  for (int i = 0; i < n; i++)
    {
      RECT r;
      get_item_rect (i, r);
      r.top = cr.top + 1;
      if (!intersect_p (r, clip))
        continue;
      dis.r = r;
      dis.data = nth (i);
      if (i == cur)
        {
          r.left--;
          r.right++;
          r.bottom += 2;
          dis.state = ODS_SELECTED;
          dis.r.bottom++;
        }
      else
        {
          dis.state = 0;
          dis.r.left++;
          dis.r.right--;
          dis.r.bottom--;
        }
      if (cur < 0 || i != cur + 1)
        {
          draw_vline (hdc, r.top, r.bottom - 1, r.left, sysdep.btn_highlight);
          draw_hline (hdc, r.left + 1, r.right - 1, r.bottom - 1, sysdep.btn_shadow);
        }
      else
        draw_hline (hdc, r.left + 2, r.right - 1, r.bottom - 1, sysdep.btn_shadow);
      if (i != cur - 1)
        draw_vline (hdc, r.top, r.bottom - 1, r.right - 1, sysdep.btn_shadow);
      draw_item (dis);
    }
  SelectObject (hdc, of);
}

void
tab_bar::paint ()
{
  RECT cr;
  GetClientRect (b_hwnd, &cr);

  t_erasebkgnd_called = 0;

  PAINTSTRUCT ps;
  HDC hdc = BeginPaint (b_hwnd, &ps);

  RECT clip;
  GetClipBox (hdc, &clip);
  if (t_erasebkgnd_called)
    {
      if (!dock_vert_p ())
        {
          if (clip.left)
            {
              InvalidateRect (b_hwnd, &clip, 0);
              EndPaint (b_hwnd, &ps);
              RECT r (cr);
              r.left = 0;
              r.right = 2;
              InvalidateRect (b_hwnd, &r, 1);
              hdc = BeginPaint (b_hwnd, &ps);
              clip.left = 0;
            }
        }
      else
        {
          if (clip.top)
            {
              InvalidateRect (b_hwnd, &clip, 0);
              EndPaint (b_hwnd, &ps);
              RECT r (cr);
              r.top = 0;
              r.bottom = 2;
              InvalidateRect (b_hwnd, &r, 1);
              hdc = BeginPaint (b_hwnd, &ps);
              clip.top = 0;
            }
        }
    }

  HWND hwnd_spin = GetDlgItem (b_hwnd, IDC_TAB_SPIN);
  if (hwnd_spin && IsWindowVisible (hwnd_spin))
    {
      RECT r (cr);
      RECT wr;
      GetWindowRect (hwnd_spin, &wr);
      MapWindowPoints (HWND_DESKTOP, b_hwnd, (POINT *)&wr, 2);
      if (!dock_vert_p ())
        r.right = wr.left;
      else
        r.bottom = wr.top;
      IntersectClipRect (hdc, r.left, r.top, r.right, r.bottom);
    }

  int n = item_count ();
  if (n > 0)
    {
      SetTextColor (hdc, sysdep.btn_text);
      SetBkColor (hdc, sysdep.btn_face);
      switch (edge ())
        {
        case EDGE_TOP:
          paint_top (hdc, cr, clip, n);
          break;

        case EDGE_BOTTOM:
          if (new_comctl_p ())
            paint_bottom (hdc, cr, clip, n);
          else
            paint_top (hdc, cr, clip, n);
          break;

        case EDGE_LEFT:
          paint_left (hdc, cr, clip, n);
          break;

        default:
          paint_right (hdc, cr, clip, n);
          break;
        }
    }

  EndPaint (b_hwnd, &ps);
}

int
tab_bar::nc_calc_size (RECT &r) const
{
  if (!dock_vert_p ())
    {
      r.left += HORZ_LEFT_PAD;
      r.right -= HORZ_RIGHT_PAD;
      if (!inverse_p ())
        r.top = r.bottom - t_tab_height;
      else
        r.bottom = r.top + t_tab_height;
    }
  else
    {
      r.top += VERT_TOP_PAD;
      r.bottom -= VERT_BOTTOM_PAD;
      if (t_horz_text)
        {
          if (!inverse_p ())
            {
              r.right -= GRIPPER_SIZE;
              r.left = r.right - t_horz_width;
            }
          else
            {
              r.left += GRIPPER_SIZE;
              r.right = r.left + t_horz_width;
            }
        }
      else
        {
          if (!inverse_p ())
            r.left = r.right - t_tab_height;
          else
            r.right = r.left + t_tab_height;
        }
    }
  return 0;
}

LRESULT CALLBACK
tab_bar::spin_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  WNDPROC oproc = WNDPROC (GetProp (hwnd, b_tab_bar_spin_prop));
  switch (msg)
    {
    case UDM_SETRANGE:
      // ”ÍˆÍ‚ð‹t“]‚·‚éB
      lparam = MAKELONG (HIWORD (lparam), LOWORD (lparam));
      break;

    case WM_NCDESTROY:
      SetWindowLong (hwnd, GWL_WNDPROC, LONG (oproc));
      RemoveProp (hwnd, b_tab_bar_spin_prop);
      break;
    }
  return oproc ? CallWindowProc (oproc, hwnd, msg, wparam, lparam) : 0;
}

void
tab_bar::parent_notify (UINT msg, UINT id, HWND hwnd)
{
  if (msg == WM_CREATE && id == IDC_TAB_SPIN
      && !(GetWindowLong (hwnd, GWL_STYLE) & UDS_HORZ))
    {
      WNDPROC o = (WNDPROC)GetWindowLong (hwnd, GWL_WNDPROC);
      if (o && SetProp (hwnd, b_tab_bar_spin_prop, HANDLE (o)))
        SetWindowLong (hwnd, GWL_WNDPROC, LONG (spin_wndproc));
    }
}

int
tab_bar::notify_spin (NMHDR *nm, LRESULT &r) const
{
  if (nm && nm->idFrom == IDC_TAB_SPIN && nm->code == UDN_DELTAPOS
      && (GetWindowLong (nm->hwndFrom, GWL_STYLE)
          & (UDS_HORZ | UDS_WRAP)) == UDS_WRAP)
    {
      DWORD range = SendMessage (nm->hwndFrom, UDM_GETRANGE, 0, 0);

      // ”ÍˆÍ‚ª‹t“]‚µ‚Ä‚¢‚é
      int mn = LOWORD (range), mx = HIWORD (range);
      if (mn < mx)
        {
          const NM_UPDOWN *ud = (NM_UPDOWN *)nm;
          int pos = ud->iPos + ud->iDelta;
          if (pos < mn)
            {
              if (ud->iPos > mn)
                {
                  SendMessage (nm->hwndFrom, UDM_SETPOS, 0, MAKELPARAM (mn, 0));
                  PostMessage (b_hwnd, WM_HSCROLL,
                               MAKEWPARAM (SB_THUMBPOSITION, mn),
                               LPARAM (nm->hwndFrom));
                  PostMessage (b_hwnd, WM_HSCROLL,
                               MAKEWPARAM (SB_ENDSCROLL, mn),
                               LPARAM (nm->hwndFrom));
                }
              r = 1;
              return 1;
            }
          else if (pos >= mx)
            {
              if (ud->iPos < mx)
                {
                  SendMessage (nm->hwndFrom, UDM_SETPOS, 0, MAKELPARAM (mx, 0));
                  PostMessage (b_hwnd, WM_HSCROLL,
                               MAKEWPARAM (SB_THUMBPOSITION, mx),
                               LPARAM (nm->hwndFrom));
                  PostMessage (b_hwnd, WM_HSCROLL,
                               MAKEWPARAM (SB_ENDSCROLL, mx),
                               LPARAM (nm->hwndFrom));
                }
              r = 1;
              return 1;
            }
          r = 0;
          return 1;
        }
    }
  return 0;
}

void
tab_bar::adjust_gripper (HDC hdc, RECT &wr, const RECT &cr) const
{
  if (t_horz_text)
    switch (edge ())
      {
      case EDGE_LEFT:
        draw_vline (hdc, wr.top, wr.bottom, cr.right, sysdep.btn_highlight);
        wr.right = cr.right - 1;
        break;

      case EDGE_RIGHT:
        draw_vline (hdc, wr.top, wr.bottom, cr.left - 1, sysdep.btn_shadow);
        wr.left = cr.left + 1;
        break;
      }
}

int
tab_bar::set_cursor (WPARAM wparam, LPARAM lparam)
{
  if (app.kbdq.idlep () && HWND (wparam) == b_hwnd
      && t_horz_text && dock_vert_p ())
    {
      POINT p;
      GetCursorPos (&p);
      ScreenToClient (b_hwnd, &p);
      RECT r;
      GetClientRect (b_hwnd, &r);
      if (edge () == EDGE_LEFT ? p.x >= r.right : p.x < r.left)
        {
          SetCursor (sysdep.hcur_sizewe);
          return 1;
        }
    }
  return 0;
}

/* ‚È‚ñ‚¾‚±‚è‚á? */
int
tab_bar::lbtn_down (int x, int y)
{
  if (!app.kbdq.idlep () || !t_horz_text || !dock_vert_p ())
    return 0;

  RECT cr;
  GetClientRect (b_hwnd, &cr);
  if (edge () == EDGE_LEFT ? x < cr.right : x >= cr.left)
    return 0;

  POINT pt;
  pt.x = x;
  pt.y = y;
  ClientToScreen (b_hwnd, &pt);

  HWND hwnd_tt = get_tooltips ();
  if (hwnd_tt)
    {
      MSG msg;
      msg.hwnd = b_hwnd;
      msg.message = WM_LBUTTONDOWN;
      msg.wParam = 0;
      msg.lParam = MAKELPARAM (x, y);
      msg.time = GetMessageTime ();
      msg.pt = pt;
      SendMessage (hwnd_tt, TTM_RELAYEVENT, 0, (LPARAM)&msg);
    }

  MapWindowPoints (b_hwnd, HWND_DESKTOP, (POINT *)&cr, 2);

  RECT fr;
  GetClientRect (b_frame.hwnd_frame (), &fr);

  HWND hwnd_parent = GetParent (b_hwnd);
  int mn, mx;
  RECT wr;
  GetWindowRect (b_hwnd, &wr);
  if (edge () == EDGE_LEFT)
    {
      wr.left = cr.right;
      mn = cr.left + MIN_WIDTH;
      mx = wr.right + fr.right - 5;
    }
  else
    {
      wr.right = cr.left;
      mn = wr.left - fr.right + 5;
      mx = cr.right - MIN_WIDTH;
    }

  int ox = wr.left;
  int w = wr.right - wr.left;
  int dx = pt.x - wr.left;
  mn += dx;
  mx = mx - w + dx;

  MapWindowPoints (HWND_DESKTOP, hwnd_parent, (POINT *)&wr, 2);
  mn -= ox - wr.left;
  mx -= ox - wr.left;
  ox = wr.left;

  SetCapture (hwnd_parent);
  frameDC fdc (hwnd_parent, 0);

  fdc.frame_rect (wr);

  while (1)
    {
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          ReleaseCapture ();
          break;
        }
      if (GetCapture () != hwnd_parent)
        break;
      switch (msg.message)
        {
        case WM_MOUSEMOVE:
        case WM_LBUTTONUP:
          {
            int nx = short (LOWORD (msg.lParam));
            nx = max (min (nx, mx), mn);
            fdc.frame_rect (wr);
            wr.left = nx - dx;
            wr.right = wr.left + w;
            if (msg.message == WM_MOUSEMOVE)
              {
                fdc.frame_rect (wr);
                break;
              }
            ReleaseCapture ();

            if (edge () == EDGE_LEFT)
              t_horz_width += wr.left - ox;
            else
              t_horz_width -= wr.left - ox;
            b_frame.arrange_bar (this);
            b_frame.recalc_layout ();
            return 1;
          }

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
  fdc.frame_rect (wr);
  return 1;
}

int
tab_bar::move_tab (int x, int y)
{
  if (!app.kbdq.idlep () || GetKeyState (VK_LBUTTON) >= 0)
    return 0;
  POINT pt;
  GetCursorPos (&pt);
  ScreenToClient (b_hwnd, &pt);
  if (pt.x != x || pt.y != y)
    return 0;

  TC_HITTESTINFO htinfo;
  htinfo.pt = pt;
  int oindex = hit_test (htinfo);
  if (oindex < 0 || oindex != get_cursel ())
    return 0;

  HCURSOR hcur_old = GetCursor ();
  HCURSOR hcur_no = LoadCursor (0, IDC_NO);
  HCURSOR hcur_mv = LoadCursor (app.hinst,
                                MAKEINTRESOURCE (dock_vert_p ()
                                                 ? IDC_MOVEUD : IDC_MOVELR));

  int moved = 0;
  SetCapture (b_hwnd);
  while (1)
    {
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          break;
        }
      if (GetCapture () != b_hwnd)
        break;
      switch (msg.message)
        {
        case WM_MOUSEMOVE:
          {
            htinfo.pt.x = short (LOWORD (msg.lParam));
            htinfo.pt.y = short (HIWORD (msg.lParam));
            int index = hit_test (htinfo);
            SetCursor (index < 0 ? hcur_no : (index == oindex ? hcur_old : hcur_mv));
            break;
          }

        case WM_LBUTTONUP:
          {
            x = short (LOWORD (msg.lParam));
            y = short (HIWORD (msg.lParam));
            htinfo.pt.x = x;
            htinfo.pt.y = y;
            int index = hit_test (htinfo);
            if (index >= 0 && index != oindex)
              {
                RECT r;
                get_item_rect (index, r);
                if (dock_vert_p ()
                    ? y > (r.top + r.bottom) / 2
                    : x > (r.left + r.right) / 2)
                  {
                    if (index != oindex - 1)
                      index++;
                  }
                else if (index == oindex + 1)
                  index++;

                char b[1024];
                TC_ITEM ti;
                ti.mask = TCIF_TEXT | TCIF_IMAGE | TCIF_PARAM;
                ti.pszText = b;
                ti.cchTextMax = sizeof b;
                set_no_redraw ();
                if (get_item (oindex, ti) && insert_item (index, ti) >= 0)
                  {
                    set_cursel (index);
                    delete_item (oindex < index ? oindex : oindex + 1);
                  }
                set_redraw ();
                InvalidateRect (b_hwnd, 0, 1);
                moved = 1;
              }
            goto done;
          }

        case WM_CANCELMODE:
          goto done;

        case WM_RBUTTONDOWN:
        case WM_RBUTTONUP:
        case WM_KEYDOWN:
        case WM_SYSKEYDOWN:
          break;

        default:
          DispatchMessage (&msg);
          break;
        }
    }
done:
  if (GetCapture () == b_hwnd)
    ReleaseCapture ();
  return moved;
}

LRESULT
tab_bar::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_SETCURSOR:
      if (set_cursor (wparam, lparam))
        return 1;
      break;

    case WM_LBUTTONDOWN:
      {
        if (lbtn_down ((short)LOWORD (lparam), (short)HIWORD (lparam)))
          return 0;
        HWND hwnd_focus = GetFocus ();
        dock_bar::wndproc (msg, wparam, lparam);
        if (move_tab ((short)LOWORD (lparam), (short)HIWORD (lparam))
            && hwnd_focus)
          SetFocus (hwnd_focus);
        return 0;
      }

    case WM_NCCALCSIZE:
      return nc_calc_size (*(RECT *)lparam);

    case WM_ERASEBKGND:
      erase_bkgnd (HDC (wparam));
      return 1;

    case WM_VSCROLL:
      msg = WM_HSCROLL;
      break;

    case WM_PARENTNOTIFY:
      parent_notify (LOWORD (wparam), HIWORD (wparam), HWND (lparam));
      break;

    case WM_NOTIFY:
      {
        LRESULT r;
        if (notify_spin ((NMHDR *)lparam, r))
          return r;
        break;
      }

    case WM_PAINT:
      paint ();
      return 0;
    }

  return dock_bar::wndproc (msg, wparam, lparam);
}

int
tab_bar::do_context_menu (const POINT *pt)
{
  int i;
  POINT tem;

  if (!pt)
    {
      i = get_cursel ();
      if (i >= 0)
        {
          RECT r;
          get_item_rect (i, r);
          tem.x = (r.left + r.right) / 2;
          tem.y = r.bottom;
        }
      else
        tem.x = tem.y = 0;
      ClientToScreen (b_hwnd, &tem);
      pt = &tem;
    }
  else
    {
      int n = item_count ();
      for (i = n - 1; i >= 0; i--)
        {
          RECT r;
          get_item_rect (i, r);
          if (PtInRect (&r, *pt))
            break;
        }
      pt = 0;
    }

  try
    {
      lisp lmenu = context_menu (i);
      if (lmenu == Qnil)
        return 0;
      protect_gc gcpro (lmenu);
      track_popup_menu (lmenu, Kbutton2, pt);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }

  refresh_screen (1);
  return 1;
}

dock_frame::dock_frame ()
     : f_hwnd (0), f_arrange (0)
{
}

dock_frame::~dock_frame ()
{
}

LONG
dock_frame::arrange_horz (const dock_bar_list &bars, LONG cx)
{
  LONG y = 0, ymax = 0;
  for (dock_bar *bar = bars.head (); bar;)
    {
      LONG x = 0;
      dock_bar *bb, *be;
      for (bb = bar, be = bar->next ();
           be && be->rect ().top == bb->rect ().top;
           be = be->next ())
        ;

      int folded = 0;
      for (bar = bb; bar != be; bar = bar->next ())
        {
          SIZE sz;
          bar->calc_window_size (sz, 0);
          if (folded)
            bar->rect ().left = x;
          else
            bar->rect ().left = max (bar->rect ().left, x);
          if (bar->rect ().left + sz.cx > cx)
            {
              bar->rect ().left = max (cx - sz.cx, x);
              int w = bar->status () & dock_bar::DOCK_STAT_NEW ? sz.cx : MIN_SIZE;
              if (bar != bb && bar->rect ().left + w > cx)
                {
                  bb = bar;
                  folded = 1;
                  bar->rect ().left = 0;
                  y = ymax;
                }
            }
          bar->rect ().right = min (bar->rect ().left + sz.cx, cx);
          bar->rect ().top = y;
          bar->rect ().bottom = y + sz.cy;
          x = bar->rect ().right;
          bar->modify_status (dock_bar::DOCK_STAT_NEW, 0);
          ymax = max (ymax, bar->rect ().bottom);
        }
      y = ymax;
    }
  return ymax;
}

LONG
dock_frame::arrange_vert (const dock_bar_list &bars, LONG cy)
{
  LONG x = 0, xmax = 0;
  for (dock_bar *bar = bars.head (); bar;)
    {
      LONG y = 0;
      dock_bar *bb, *be;
      for (bb = bar, be = bar->next ();
           be && be->rect ().left == bb->rect ().left;
           be = be->next ())
        ;

      int folded = 0;
      for (bar = bb; bar != be; bar = bar->next ())
        {
          SIZE sz;
          bar->calc_window_size (sz, 1);
          if (folded)
            bar->rect ().top = y;
          else
            bar->rect ().top = max (bar->rect ().top, y);
          if (bar->rect ().top + sz.cy > cy)
            {
              bar->rect ().top = max (cy - sz.cy, y);
              int h = bar->status () & dock_bar::DOCK_STAT_NEW ? sz.cy : MIN_SIZE;
              if (bar != bb && bar->rect ().top + h > cy)
                {
                  bb = bar;
                  folded = 1;
                  bar->rect ().top = 0;
                  x = xmax;
                }
            }
          bar->rect ().bottom = min (bar->rect ().top + sz.cy, cy);
          bar->rect ().left = x;
          bar->rect ().right = x + sz.cx;
          y = bar->rect ().bottom;
          bar->modify_status (dock_bar::DOCK_STAT_NEW, 0);
          xmax = max (xmax, bar->rect ().right);
        }
      x = xmax;
    }
  return xmax;
}

LONG
dock_frame::horz_max (const dock_bar_list &bars, LONG cx)
{
  LONG ymax = 0;
  for (dock_bar *bar = bars.head (); bar; bar = bar->next ())
    {
      SIZE sz;
      bar->calc_window_size (sz, 0);
      if (bar->prev () && bar->prev ()->rect ().top == bar->rect ().top)
        bar->rect ().left = max (bar->rect ().left, bar->prev ()->rect ().right);
      bar->rect ().right = max (min (bar->rect ().left + sz.cx, cx), bar->rect ().left);
      ymax = max (ymax, bar->rect ().bottom);
    }
  return ymax;
}

LONG
dock_frame::vert_max (const dock_bar_list &bars, LONG cy)
{
  LONG xmax = 0;
  for (dock_bar *bar = bars.head (); bar; bar = bar->next ())
    {
      SIZE sz;
      bar->calc_window_size (sz, 1);
      if (bar->prev () && bar->prev ()->rect ().left == bar->rect ().left)
        bar->rect ().top = max (bar->rect ().top, bar->prev ()->rect ().bottom);
      bar->rect ().bottom = max (min (bar->rect ().top + sz.cy, cy), bar->rect ().top);
      xmax = max (xmax, bar->rect ().right);
    }
  return xmax;
}

void
dock_frame::dock_edge (dock_bar *bar, int edge, int cx)
{
  bar->dock_edge ();
  int border = dock_bar::BORDER_TOP | dock_bar::BORDER_BOTTOM;
  if (bar->rect ().left)
    border |= dock_bar::BORDER_LEFT;
  if (bar->rect ().right != cx)
    border |= dock_bar::BORDER_RIGHT;
  bar->modify_border (dock_bar::BORDER_LEFT | dock_bar::BORDER_RIGHT, border);
}

int
dock_frame::defer_window_pos (HDWP &hdwp, HWND &hwnd_before, int edge)
{
  const RECT &r = f_edge_rect[edge];
  int cx = r.right - r.left;
  for (dock_bar *bar = f_bars[edge].head (); bar; bar = bar->next ())
    {
      dock_edge (bar, edge, cx);
      hdwp = DeferWindowPos (hdwp, bar->b_hwnd, hwnd_before,
                             r.left + bar->rect ().left,
                             r.top + bar->rect ().top,
                             bar->rect ().right - bar->rect ().left,
                             bar->rect ().bottom - bar->rect ().top,
                             SWP_DRAWFRAME | SWP_NOACTIVATE);
      if (!hdwp)
        return 0;
      hwnd_before = bar->b_hwnd;
    }
  return 1;
}

void
dock_frame::set_window_pos (HWND &hwnd_before, int edge)
{
  const RECT &r = f_edge_rect[edge];
  int cx = r.right - r.left;
  for (dock_bar *bar = f_bars[edge].head (); bar; bar = bar->next ())
    {
      dock_edge (bar, edge, cx);
      SetWindowPos (bar->b_hwnd, hwnd_before,
                    r.left + bar->rect ().left,
                    r.top + bar->rect ().top,
                    bar->rect ().right - bar->rect ().left,
                    bar->rect ().bottom - bar->rect ().top,
                    SWP_DRAWFRAME | SWP_NOACTIVATE);
      hwnd_before = bar->b_hwnd;
    }
}

static void
set_rect (RECT &rc, LONG l, LONG t, LONG r, LONG b)
{
  rc.left = l;
  rc.top = t;
  rc.right = max (l, r);
  rc.bottom = max (t, b);
}

void
dock_frame::calc_layout (RECT &r, HWND hwnd_before)
{
  LONG cx = r.right - r.left;
  LONG top = (f_arrange & (1 << EDGE_TOP)
              ? arrange_horz (f_bars[EDGE_TOP], cx)
              : horz_max (f_bars[EDGE_TOP], cx));
  LONG bottom = (f_arrange & (1 << EDGE_BOTTOM)
                 ? arrange_horz (f_bars[EDGE_BOTTOM], cx)
                 : horz_max (f_bars[EDGE_BOTTOM], cx));
  LONG cy = r.bottom - r.top - top - bottom;
  LONG left = (f_arrange & (1 << EDGE_LEFT)
               ? arrange_vert (f_bars[EDGE_LEFT], cy)
               : vert_max (f_bars[EDGE_LEFT], cy));
  LONG right = (f_arrange & (1 << EDGE_RIGHT)
                ? arrange_vert (f_bars[EDGE_RIGHT], cy)
                : vert_max (f_bars[EDGE_RIGHT], cy));

  int nwindows = 0;
  for (int i = 0; i < EDGE_MAX; i++)
    nwindows += f_bars[i].length ();

  set_rect (f_edge_rect[EDGE_TOP],
            r.left, r.top + 1, r.right, r.top + top + 1);
  set_rect (f_edge_rect[EDGE_BOTTOM],
            r.left, r.bottom - bottom, r.right, r.bottom);
  set_rect (f_edge_rect[EDGE_LEFT],
            r.left, r.top + top + 1, r.left + left, r.bottom - bottom);
  set_rect (f_edge_rect[EDGE_RIGHT],
            r.right - right, r.top + top + 1, r.right, r.bottom - bottom);

  if (top)
    r.top += 1 + top;
  if (bottom)
    r.bottom -= 1 + bottom;
  r.left += left;
  r.right -= right;
  r.bottom = max (r.top, r.bottom);
  r.right = max (r.left, r.right);

  RECT cr, ir;
  GetClientRect (f_hwnd, &cr);

  set_rect (ir, 0, 0, r.left, cr.bottom);
  InvalidateRect (f_hwnd, &ir, 1);
  set_rect (ir, r.right, 0, cr.right, cr.bottom);
  InvalidateRect (f_hwnd, &ir, 1);
  set_rect (ir, r.left, 0, r.right, r.top);
  InvalidateRect (f_hwnd, &ir, 1);
  set_rect (ir, r.left, r.bottom, r.right, cr.bottom);
  InvalidateRect (f_hwnd, &ir, 1);

  HWND hwnd = hwnd_before;
  HDWP hdwp = BeginDeferWindowPos (nwindows);
  if (hdwp
      && defer_window_pos (hdwp, hwnd, EDGE_TOP)
      && defer_window_pos (hdwp, hwnd, EDGE_BOTTOM)
      && defer_window_pos (hdwp, hwnd, EDGE_LEFT)
      && defer_window_pos (hdwp, hwnd, EDGE_RIGHT))
    EndDeferWindowPos (hdwp);
  else
    {
      hwnd = hwnd_before;
      set_window_pos (hwnd, EDGE_TOP);
      set_window_pos (hwnd, EDGE_BOTTOM);
      set_window_pos (hwnd, EDGE_LEFT);
      set_window_pos (hwnd, EDGE_RIGHT);
    }

  f_arrange = 0;
}

void
dock_frame::add (dock_bar *bar)
{
  f_bars[EDGE_INVALID].add_tail (bar);
  bar->modify_dock_edge (EDGE_INVALID);
  if (IsWindowVisible (bar->b_hwnd))
    ShowWindow (bar->b_hwnd, SW_HIDE);
}

void
dock_frame::show (dock_bar *bar, int edge, const POINT *point, int horz_width)
{
  f_bars[bar->edge ()].remove (bar);
  f_arrange |= 1 << bar->edge ();

  if (horz_width > 0)
    {
      RECT r;
      GetClientRect (hwnd_frame (), &r);
      horz_width = min (horz_width, int (r.right - 5));
      bar->set_horz_width (horz_width);
    }

  edge = bar->check_edge (edge);
  int vert = dock_bar::vert_edge_p (edge);
  SIZE sz;
  bar->calc_window_size (sz, vert);
  if (!point)
    {
      dock_bar *tail = f_bars[edge].tail ();
      if (!tail)
        bar->rect ().left = bar->rect ().top = 0;
      else if (!vert)
        {
          if (sz.cx >= dock_bar::DOCK_LENGTH_INFINITE)
            {
              bar->rect ().left = 0;
              bar->rect ().top = tail->rect ().bottom;
            }
          else
            {
              bar->rect ().left = tail->rect ().right;
              bar->rect ().top = tail->rect ().top;
            }
        }
      else
        {
          if (sz.cy >= dock_bar::DOCK_LENGTH_INFINITE)
            {
              bar->rect ().top = 0;
              bar->rect ().left = tail->rect ().right;
            }
          else
            {
              bar->rect ().left = tail->rect ().left;
              bar->rect ().top = tail->rect ().bottom;
            }
        }
      f_bars[edge].add_tail (bar);
    }
  else
    {
      bar->rect ().left = point->x;
      bar->rect ().top = point->y;
      if (!vert)
        {
          dock_bar *b;
          for (b = f_bars[edge].head (); b; b = b->next ())
            if (bar->rect ().top < b->rect ().top
                || (bar->rect ().top == b->rect ().top
                    && bar->rect ().left <= b->rect ().left))
              {
                f_bars[edge].insert_before (bar, b);
                break;
              }
          if (!b)
            f_bars[edge].add_tail (bar);
        }
      else
        {
          dock_bar *b;
          for (b = f_bars[edge].head (); b; b = b->next ())
            if (bar->rect ().left < b->rect ().left
                || (bar->rect ().left == b->rect ().left
                    && bar->rect ().top <= b->rect ().top))
              {
                f_bars[edge].insert_before (bar, b);
                break;
              }
          if (!b)
            f_bars[edge].add_tail (bar);
        }
    }

  bar->rect ().right = bar->rect ().left + sz.cx;
  bar->rect ().bottom = bar->rect ().top + sz.cy;
  if (!point)
    bar->modify_status (0, dock_bar::DOCK_STAT_NEW);
  bar->modify_dock_edge (edge);
  f_arrange |= 1 << edge;
  if (!IsWindowVisible (bar->b_hwnd))
    ShowWindow (bar->b_hwnd, SW_SHOWNA);
}

void
dock_frame::hide (dock_bar *bar)
{
  if (bar->edge () != EDGE_INVALID)
    {
      f_bars[bar->edge ()].remove (bar);
      f_arrange |= 1 << bar->edge ();
      f_bars[EDGE_INVALID].add_tail (bar);
      bar->modify_dock_edge (EDGE_INVALID);
      if (IsWindowVisible (bar->b_hwnd))
        ShowWindow (bar->b_hwnd, SW_HIDE);
    }
}

void
dock_frame::remove (dock_bar *bar)
{
  if (bar->edge () != EDGE_INVALID)
    f_arrange |= 1 << bar->edge ();
  f_bars[bar->edge ()].remove (bar);
  delete bar;
}

void
dock_frame::limit_range (SIZE &sz, const RECT &w, const RECT &h)
{
  if (sz.cx >= dock_bar::DOCK_LENGTH_INFINITE)
    sz.cx = w.right - w.left;
  if (sz.cy >= dock_bar::DOCK_LENGTH_INFINITE)
    sz.cy = h.bottom - h.top;
}

void
dock_frame::drag_rect (RECT &r, int vert_p, int org_vert_p,
                       const POINT &pt, const SIZE *offset,
                       const SIZE &sz_horz, const SIZE &sz_vert)
{
  if (vert_p == org_vert_p)
    {
      r.left = pt.x - offset[0].cx;
      r.top = pt.y - offset[0].cy;
    }
  else
    {
      r.left = pt.x - offset[1].cx;
      r.top = pt.y - offset[1].cy;
    }
  if (vert_p)
    {
      r.right = r.left + sz_vert.cx;
      r.bottom = r.top + sz_vert.cy;
    }
  else
    {
      r.right = r.left + sz_horz.cx;
      r.bottom = r.top + sz_horz.cy;
    }
}

int
dock_frame::on_edge_p (const RECT &r, int vert_p, int edge) const
{
  RECT sr;
  sr.left = r.left;
  sr.top = r.top;
  if (vert_p)
    {
      sr.right = r.right;
      sr.bottom = r.top + r.right - r.left;
    }
  else
    {
      sr.right = r.left + r.bottom - r.top;
      sr.bottom = r.bottom;
    }
  const RECT &er = f_edge_rect[edge];
  return (sr.left < er.right && sr.right >= er.left
          && sr.top < er.bottom && sr.bottom >= er.top);
}

int
dock_frame::on_edge (const RECT &r, int dockable, int vert_p, int &edge) const
{
  if (vert_p
      ? ((dockable & dock_bar::DOCKABLE_LEFT
          && on_edge_p (r, vert_p, edge = EDGE_LEFT))
         || (dockable & dock_bar::DOCKABLE_RIGHT
             && on_edge_p (r, vert_p, edge = EDGE_RIGHT)))
      : ((dockable & dock_bar::DOCKABLE_TOP
          && on_edge_p (r, vert_p, edge = EDGE_TOP))
         || (dockable & dock_bar::DOCKABLE_BOTTOM
             && on_edge_p (r, vert_p, edge = EDGE_BOTTOM))))
    return vert_p;

  if (!vert_p
      ? ((dockable & dock_bar::DOCKABLE_LEFT
          && on_edge_p (r, vert_p, edge = EDGE_LEFT))
         || (dockable & dock_bar::DOCKABLE_RIGHT
             && on_edge_p (r, vert_p, edge = EDGE_RIGHT)))
      : ((dockable & dock_bar::DOCKABLE_TOP
          && on_edge_p (r, vert_p, edge = EDGE_TOP))
         || (dockable & dock_bar::DOCKABLE_BOTTOM
             && on_edge_p (r, vert_p, edge = EDGE_BOTTOM))))
    return !vert_p;

  edge = EDGE_INVALID;
  return vert_p;
}

void
dock_frame::move_bar (dock_bar *bar, RECT &r, int edge)
{
  if (edge == EDGE_INVALID || !(bar->dockable () & (1 << edge)))
    return;

  f_arrange |= (1 << bar->edge ()) | (1 << edge);
  f_bars[bar->edge ()].remove (bar);
  bar->modify_dock_edge (edge);

  OffsetRect (&r, -f_edge_rect[edge].left, -f_edge_rect[edge].top);
  if (!dock_bar::vert_edge_p (edge))
    {
      int h = r.bottom - r.top;
      if (r.top < -h / 3)
        {
          f_bars[edge].add_head (bar);
          bar->rect ().top = r.top;
        }
      else
        {
          int r1_3 = r.top + h / 3;
          int r2_3 = r.top + h * 2 / 3;
          bar->rect ().top = r.top;
          dock_bar *b;
          for (b = f_bars[edge].head (); b; b = b->next ())
            if (r2_3 <= b->rect ().bottom)
              {
                if ((b->prev ()
                     && b->prev ()->rect ().bottom != b->rect ().top
                     && r2_3 <= b->rect ().top)
                    || r1_3 <= b->rect ().top)
                  bar->rect ().top = r.top;
                else
                  {
                    bar->rect ().top = b->rect ().top;
                    while (r.left > b->rect ().left
                           && (b = b->next ())
                           && b->rect ().top == bar->rect ().top)
                      ;
                  }
                if (b)
                  f_bars[edge].insert_before (bar, b);
                break;
              }
          if (!b)
            f_bars[edge].add_tail (bar);
        }
      bar->rect ().left = r.left;
      bar->rect ().right = r.right;
      bar->rect ().bottom = bar->rect ().top + r.bottom - r.top;
    }
  else
    {
      int w = r.right - r.left;
      if (r.left < -w / 3)
        {
          f_bars[edge].add_head (bar);
          bar->rect ().left = r.left;
        }
      else
        {
          int r1_3 = r.left + w / 3;
          int r2_3 = r.left + w * 2 / 3;
          bar->rect ().left = r.left;
          dock_bar *b;
          for (b = f_bars[edge].head (); b; b = b->next ())
            if (r2_3 <= b->rect ().right)
              {
                if ((b->prev ()
                     && b->prev ()->rect ().right != b->rect ().left
                     && r2_3 <= b->rect ().left)
                    || r1_3 <= b->rect ().left)
                  bar->rect ().left = r.left;
                else
                  {
                    bar->rect ().left = b->rect ().left;
                    while (r.top > b->rect ().top
                           && (b = b->next ())
                           && b->rect ().left == bar->rect ().left)
                      ;
                  }
                if (b)
                  f_bars[edge].insert_before (bar, b);
                break;
              }
          if (!b)
            f_bars[edge].add_tail (bar);
        }
      bar->rect ().top = r.top;
      bar->rect ().right = bar->rect ().left + r.right - r.left;
      bar->rect ().bottom = r.bottom;
    }

  recalc_layout ();
}

int
dock_frame::move_bar (dock_bar *bar, int x, int y)
{
  POINT pt;
  pt.x = x;
  pt.y = y;
  ClientToScreen (bar->b_hwnd, &pt);

  RECT r;
  GetWindowRect (bar->b_hwnd, &r);

  SIZE offset[2];
  offset[0].cx = pt.x - r.left;
  offset[0].cy = pt.y - r.top;
  offset[1].cx = offset[0].cy;
  offset[1].cy = offset[0].cx;

  SIZE sz_horz, sz_vert;
  bar->calc_window_size (sz_horz, 0);
  bar->calc_window_size (sz_vert, 1);
  limit_range (sz_horz, f_edge_rect[EDGE_TOP], f_edge_rect[EDGE_LEFT]);
  limit_range (sz_vert, f_edge_rect[EDGE_TOP], f_edge_rect[EDGE_LEFT]);

  int org_vert_p = bar->dock_vert_p () ? 1 : 0;
  int vert_p = org_vert_p;

  if (vert_p)
    {
      offset[1].cx = offset[0].cy;
      offset[1].cy = max (0L, sz_horz.cy - offset[0].cx);
    }
  else
    {
      offset[1].cx = max (0L, sz_vert.cx - offset[0].cy);
      offset[1].cy = offset[0].cx;
    }

  SetCapture (f_hwnd);
  frameDC fdc (GetDesktopWindow (), DCX_WINDOW);

  drag_rect (r, vert_p, org_vert_p, pt, offset, sz_horz, sz_vert);
  fdc.frame_rect (r);

  int which_edge;

  while (1)
    {
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          ReleaseCapture ();
          break;
        }
      if (GetCapture () != f_hwnd)
        break;
      switch (msg.message)
        {
        case WM_MOUSEMOVE:
        case WM_LBUTTONUP:
          fdc.frame_rect (r);
          pt.x = short (LOWORD (msg.lParam));
          pt.y = short (HIWORD (msg.lParam));
          ClientToScreen (f_hwnd, &pt);
          drag_rect (r, vert_p, org_vert_p, pt, offset, sz_horz, sz_vert);
          MapWindowPoints (HWND_DESKTOP, f_hwnd, (POINT *)&r, 2);
          vert_p = on_edge (r, bar->dockable (), vert_p, which_edge);
          drag_rect (r, vert_p, org_vert_p, pt, offset, sz_horz, sz_vert);
          if (msg.message == WM_MOUSEMOVE)
            {
              fdc.frame_rect (r);
              break;
            }
          ReleaseCapture ();
          MapWindowPoints (HWND_DESKTOP, f_hwnd, (POINT *)&r, 2);
          move_bar (bar, r, which_edge);
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
  fdc.frame_rect (r);
  return 1;
}

int
dock_frame::draw_item (DRAWITEMSTRUCT *dis)
{
  dock_bar *bar = dock_bar::from_hwnd (dis->hwndItem);
  if (!bar)
    return 0;
  bar->draw_item (dis);
  return 1;
}

int
dock_frame::notify (NMHDR *nm, LRESULT &result)
{
  if (nm->code == TTN_NEEDTEXT)
    {
      TOOLINFO ti;
      ti.cbSize = sizeof ti;
      if (SendMessage (nm->hwndFrom, TTM_GETCURRENTTOOL, 0, LPARAM (&ti))
          && ti.lpszText == LPSTR_TEXTCALLBACK)
        {
          dock_bar *bar = dock_bar::from_hwnd (ti.hwnd);
          return bar ? bar->need_text (*(TOOLTIPTEXT *)nm) : 0;
        }
    }

  dock_bar *bar = dock_bar::from_hwnd (nm->hwndFrom);
  return bar ? bar->notify (nm, result) : 0;
}

void
dock_frame::reload_settings ()
{
  bm ().reload ();
  for (int i = 0; i <= EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      bar->reload_settings ();
}

void
dock_frame::cleanup ()
{
  for (int i = 0; i <= EDGE_MAX; i++)
    while (!f_bars[i].empty_p ())
      delete f_bars[i].remove_head ();
}

dock_bar *
dock_frame::find (lisp name) const
{
  for (int i = 0; i <= EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      if (bar->name () == name)
        return bar;
  return 0;
}

void
dock_frame::update_ui ()
{
  for (int i = 0; i <= EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      bar->update_ui ();
}

lisp
dock_frame::lookup_command (int id) const
{
  for (int i = 0; i < EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      {
        lisp r = bar->lookup_command (id);
        if (r)
          return r;
      }
  return Qnil;
}

void
dock_frame::gc_mark (void (*f)(lisp))
{
  for (int i = 0; i <= EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      bar->gc_mark (f);
}

lisp
dock_frame::list_bars () const
{
  lisp x = Qnil;

  for (dock_bar *bar = f_bars[EDGE_INVALID].head (); bar; bar = bar->next ())
    x = xcons (xcons (bar->name (), Qnil), x);
  lisp r = xcons (x, Qnil);

  for (int i = EDGE_MAX - 1; i >= 0; i--)
    {
      x = Qnil;
      for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
        {
          int w = bar->horz_width ();
          x = xcons (make_list (bar->name (),
                                make_fixnum (bar->rect ().left),
                                make_fixnum (bar->rect ().top),
                                w > 0 ? make_fixnum (w) : Qnil,
                                0),
                     x);
        }
      r = xcons (x, r);
    }
  return r;
}

void
dock_frame::refresh ()
{
  int x = xsymbol_value (Vtab_bar_horizontal_text) != Qnil;
  for (int i = 0; i < EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      {
        if (bar->set_horz_text_p (x))
          arrange_bar (bar);
      }
  if (f_arrange)
    recalc_layout ();
}

int
dock_frame::focus_next (const dock_bar *bar) const
{
  int edge;

  if (!bar)
    edge = 0;
  else
    {
      edge = bar->edge () + 1;
      while ((bar = bar->next ()))
        if (bar->focus ())
          return 1;
    }

  for (; edge < EDGE_MAX; edge++)
    for (bar = f_bars[edge].head (); bar; bar = bar->next ())
      if (bar->focus ())
        return 1;

  SetFocus (f_hwnd);
  return 0;
}

int
dock_frame::focus_prev (const dock_bar *bar) const
{
  int edge;

  if (!bar)
    edge = EDGE_MAX - 1;
  else
    {
      edge = bar->edge () - 1;
      while ((bar = bar->prev ()))
        if (bar->focus ())
          return 1;
    }

  for (; edge >= 0; edge--)
    for (bar = f_bars[edge].tail (); bar; bar = bar->prev ())
      if (bar->focus ())
        return 1;

  SetFocus (f_hwnd);
  return 0;
}

void
dock_frame::color_changed () const
{
  for (int i = 0; i < EDGE_MAX; i++)
    for (dock_bar *bar = f_bars[i].head (); bar; bar = bar->next ())
      bar->color_changed ();
}

int
tool_bm::load_mapped_bitmap (const char *filename, HBITMAP &hbm)
{
  hbm = 0;

  mapf mf;
  if (!mf.open (filename))
    return LMB_OPEN_FILE;
  if (mf.size () <= (sizeof (BITMAPFILEHEADER)
                     + sizeof (BITMAPINFOHEADER)))
    return LMB_BAD_FORMAT;

  const BITMAPFILEHEADER &bf = *(const BITMAPFILEHEADER *)mf.base ();
  const BITMAPINFOHEADER &bi = *(const BITMAPINFOHEADER *)(&bf + 1);

  if (bf.bfType != 'MB'
      || bi.biSize != sizeof bi
      || bi.biWidth <= 0
      || !bi.biHeight
      || bi.biCompression != BI_RGB
      || bi.biPlanes != 1)
    return LMB_BAD_FORMAT;

  if (bi.biBitCount != 4 && bi.biBitCount != 8)
    return LMB_UNSUPPORTED;

  if (bi.biClrUsed > (1U << bi.biBitCount))
    return LMB_BAD_FORMAT;

  int ncolors = bi.biClrUsed ? bi.biClrUsed : 1 << bi.biBitCount;
  DWORD offset = sizeof bf + sizeof bi + ncolors * sizeof (RGBQUAD);
  if (bf.bfOffBits)
    {
      if (bf.bfOffBits < offset)
        return LMB_BAD_FORMAT;
      offset = bf.bfOffBits;
    }
  if (offset + (((bi.biWidth * bi.biBitCount + 7) / 8 + 3) & ~3) * abs (bi.biHeight) > mf.size ())
    return LMB_BAD_FORMAT;

  struct {BITMAPINFOHEADER bi; RGBQUAD rgb[256];} b;
  b.bi = bi;
  const DWORD *src = (const DWORD *)(&bi + 1);
  DWORD *dst = (DWORD *)b.rgb;

  struct {DWORD from, to;} cm[] =
    {
#define RGB2BGR(c) (RGB (GetBValue (c), GetGValue (c), GetRValue (c)))
      {RGB2BGR (RGB (0, 0, 0)), RGB2BGR (sysdep.btn_text)},
      {RGB2BGR (RGB (0x80, 0x80, 0x80)), RGB2BGR (sysdep.btn_shadow)},
      {RGB2BGR (RGB (0xc0, 0xc0, 0xc0)), RGB2BGR (sysdep.btn_face)},
      {RGB2BGR (RGB (0xff, 0xff, 0xff)), RGB2BGR (sysdep.btn_highlight)},
#undef RGB2BGR
    };

  for (int i = 0; i < ncolors; i++)
    {
      dst[i] = src[i];
      for (int j = 0; j < numberof (cm); j++)
        if (src[i] == cm[j].from)
          {
            dst[i] = cm[j].to;
            break;
          }
    }

  HDC hdc = GetDC (0);
  hbm = CreateDIBitmap (hdc, &b.bi, CBM_INIT,
                        (const char *)mf.base () + offset,
                        (const BITMAPINFO *)&b, DIB_RGB_COLORS);
  ReleaseDC (0, hdc);
  return hbm ? LMB_NO_ERRORS : LMB_FAILED;
}

const tool_bm::bm_node *
tool_bm::load (const char *path, int &e)
{
  e = LMB_NO_ERRORS;

  for (bm_node *p = bm_list.head (); p; p = p->next ())
    if (strcaseeq (path, p->b_path))
      {
        p->incref ();
        return p;
      }

  bm_node *p = new bm_node;
  p->b_path = strdup (path);
  if (p->b_path)
    {
      HBITMAP hbm;
      e = load_mapped_bitmap (path, hbm);
      if (e == LMB_NO_ERRORS)
        {
          p->b_hbm = hbm;
          p->incref ();
          bm_list.add_head (p);
          return p;
        }
    }
  else
    e = LMB_NOMEM;
  delete p;
  return 0;
}

void
tool_bm::release (const bm_node *p)
{
  bm_node *q = const_cast <bm_node *> (p);
  if (!q->decref ())
    delete q;
}

void
tool_bm::reload ()
{
  for (bm_node *p = bm_list.head (); p; p = p->next ())
    {
      HBITMAP hbm;
      if (load_mapped_bitmap (p->b_path, hbm) == LMB_NO_ERRORS)
        {
          DeleteObject (p->b_hbm);
          p->b_hbm = hbm;
        }
    }
}

