#include "stdafx.h"
#include "ed.h"
#include "resource.h"
#include "print.h"
#include "preview.h"
#include "conf.h"

int preview_page_window::wndclass_initialized;
const char preview_page_window::PageClassName[] = "PreviewPage";

const preview_page_window::ids2scale preview_page_window::ids2scales[] =
{
  {IDS_SCALE_WINDOW, ADJUST_WINDOW},
  {IDS_SCALE_WIDTH, ADJUST_WIDTH},
  {IDS_SCALE_HEIGHT, ADJUST_HEIGHT},
  {IDS_SCALE400, 400},
  {IDS_SCALE300, 300},
  {IDS_SCALE200, 200},
  {IDS_SCALE150, 150},
  {IDS_SCALE100, 100},
  {IDS_SCALE80, 80},
  {IDS_SCALE60, 60},
  {IDS_SCALE40, 40},
  {IDS_SCALE20, 20},
};

preview_page_window::preview_page_window (const printer_device &dev,
                                          print_settings &settings,
                                          print_engine &engine)
     : p_dev (dev), p_settings (settings), p_engine (engine)
{
  p_scale = ADJUST_WINDOW;
  p_scroll_offset.x = 0;
  p_scroll_offset.y = 0;

  HDC hdc = GetDC (0);
  p_xdpi = GetDeviceCaps (hdc, LOGPIXELSX);
  p_ydpi = GetDeviceCaps (hdc, LOGPIXELSY);
  ReleaseDC (0, hdc);

  vsinfo.nMin = 0;
  vsinfo.nMax = -1;
  vsinfo.nPage = UINT (-1);
  vsinfo.nPos = -1;

  hsinfo.nMin = 0;
  hsinfo.nMax = -1;
  hsinfo.nPage = UINT (-1);
  hsinfo.nPos = -1;

  p_font_height = MulDiv (p_settings.ps_font[FONT_ASCII].point, p_dev.ydpi (), 720);

  //  p_total_pages = engine.count_total_pages ();
}

void
preview_page_window::update_scroll_bar (SCROLLINFO &si, int id,
                                        int pos, int page, int range)
{
  si.fMask = 0;
  if (page < range)
    {
      if (si.nMax == si.nMin && si.nPage == UINT (-1))
        ShowScrollBar (p_hwnd, id, 1);
      if (si.nMax != range || si.nPage != UINT (range))
        {
          si.nMax = range;
          si.nPage = page;
          si.fMask |= SIF_RANGE | SIF_PAGE;
        }
      if (si.nPos != pos)
        {
          si.nPos = pos;
          si.fMask |= SIF_POS;
        }
    }
  else
    {
      if (si.nMax != si.nMin)
        {
          si.nMax = si.nMin;
          si.nPage = UINT (-1);
          si.nPos = -1;
          si.fMask = SIF_RANGE;
        }
    }
  if (si.fMask)
    SetScrollInfo (p_hwnd, id, &si, 1);
}

inline void
preview_page_window::update_vscroll_bar ()
{
  p_scroll_offset.y = max (0, int (min (p_scroll_offset.y,
                                        (p_physsize_pxl.cy
                                         + MARGIN * 2 - p_client.cy))));
  update_scroll_bar (vsinfo, SB_VERT, p_scroll_offset.y,
                     p_client.cy, p_physsize_pxl.cy + MARGIN * 2);
}

inline void
preview_page_window::update_hscroll_bar ()
{
  p_scroll_offset.x = max (0, int (min (p_scroll_offset.x,
                                        (p_physsize_pxl.cx
                                         + MARGIN * 2 - p_client.cx))));
  update_scroll_bar (hsinfo, SB_HORZ, p_scroll_offset.x,
                     p_client.cx, p_physsize_pxl.cx + MARGIN * 2);
}

#define VSIZE (p_engine.cell_size ().cy * p_physsize_pxl.cy \
               / p_dev.physsize_pxl ().cy)
#define HSIZE (p_engine.cell_size ().cx * p_physsize_pxl.cx \
               / p_dev.physsize_pxl ().cx)

void
preview_page_window::vscroll (int code, int pos, int scale)
{
  switch (code)
    {
    case SB_LINEDOWN:
      pos = p_scroll_offset.y + VSIZE * scale;
      break;

    case SB_LINEUP:
      pos = p_scroll_offset.y - VSIZE * scale;
      break;

    case SB_PAGEDOWN:
      pos = p_scroll_offset.y + p_client.cy;
      break;

    case SB_PAGEUP:
      pos = p_scroll_offset.y - p_client.cy;
      break;

    case SB_THUMBTRACK:
      {
        ScrollInfo i;
        i.fMask = SIF_TRACKPOS;
        if (!GetScrollInfo (p_hwnd, SB_VERT, &i))
          return;
        pos = i.nTrackPos;
        break;
      }

    default:
      return;
    }

  pos = max (0, min (pos, int (p_physsize_pxl.cy + MARGIN * 2 - p_client.cy)));
  ScrollWindow (p_hwnd, 0, p_scroll_offset.y - pos, 0, 0);
  p_scroll_offset.y = pos;
  update_vscroll_bar ();
}

void
preview_page_window::hscroll (int code, int pos, int scale)
{
  switch (code)
    {
    case SB_LINELEFT:
      pos = p_scroll_offset.x - HSIZE * scale;
      break;

    case SB_LINERIGHT:
      pos = p_scroll_offset.x + HSIZE * scale;
      break;

    case SB_PAGELEFT:
      pos = p_scroll_offset.x - p_client.cx;
      break;

    case SB_PAGERIGHT:
      pos = p_scroll_offset.x + p_client.cx;
      break;

    case SB_THUMBTRACK:
      {
        ScrollInfo i;
        i.fMask = SIF_TRACKPOS;
        if (!GetScrollInfo (p_hwnd, SB_HORZ, &i))
          return;
        pos = i.nTrackPos;
        break;
      }

    default:
      return;
    }
  pos = max (0, min (pos, int (p_physsize_pxl.cx + MARGIN * 2 - p_client.cx)));
  ScrollWindow (p_hwnd, p_scroll_offset.x - pos, 0, 0, 0);
  p_scroll_offset.x = pos;
  update_hscroll_bar ();
}

int
preview_page_window::key_down (int c)
{
  switch (c)
    {
    case VK_LEFT:
      hscroll (SB_LINELEFT, 0);
      return 1;

    case VK_RIGHT:
      hscroll (SB_LINERIGHT, 0);
      return 1;

    case VK_UP:
      vscroll (SB_LINEUP, 0);
      return 1;

    case VK_DOWN:
      vscroll (SB_LINEDOWN, 0);
      return 1;

    case VK_PRIOR:
      prev_page ();
      return 1;

    case VK_NEXT:
      next_page ();
      return 1;

    default:
      return 0;
    }
}

void
preview_page_window::wheel (const wheel_info &wi)
{
  if (wi.wi_nlines == WHEEL_PAGESCROLL)
    {
      if (wi.wi_value > 0)
        prev_page ();
      else if (wi.wi_value < 0)
        next_page ();
    }
  else
    {
      if (wi.wi_value > 0)
        vscroll (SB_LINEUP, 0, wi.wi_nlines * wi.wi_value);
      else if (wi.wi_value < 0)
        vscroll (SB_LINEDOWN, 0, wi.wi_nlines * -wi.wi_value);
    }
}

void
preview_page_window::scaling (int mul, int div)
{
  mul = max (0, mul);

#define SCALE_X(D, S) (D = (S) * mul / div * p_dev.ydpi () / p_dev.xdpi ())
#define SCALE_Y(D, S) (D = (S) * mul / div)
#define SCALE_SIZE(D, S) (SCALE_X (D.cx, S.cx), SCALE_Y (D.cy, S.cy))
#define SCALE_RECT(D, S) (SCALE_X (D.left, S.left), SCALE_Y (D.top, S.top), \
                          SCALE_X (D.right, S.right), SCALE_Y (D.bottom, S.bottom))

  SCALE_SIZE (p_size, p_dev.size ());
  SCALE_SIZE (p_physsize_pxl, p_dev.physsize_pxl ());
  SCALE_RECT (p_min_margin_pxl, p_dev.min_margin_pxl ());
  SCALE_RECT (p_text_margin_pxl, p_settings.ps_text_margin_pxl);
  SCALE_Y (p_header_offset_pxl, p_settings.ps_header_offset_pxl);
  SCALE_Y (p_footer_offset_pxl, p_settings.ps_footer_offset_pxl);
  SCALE_X (p_column_sep_pxl, p_settings.ps_column_sep_pxl);

  d_font_height = p_font_height * mul / div;
}

void
preview_page_window::scaling (POINT *pt)
{
  POINT o1, o2;
  if (pt)
    {
      calc_origin (o1);
      o2 = o1;
      o1.x = ((p_scroll_offset.x + pt->x - o1.x)
              * p_dev.physsize_pxl ().cx / p_physsize_pxl.cx);
      o1.y = ((p_scroll_offset.y + pt->y - o1.y)
              * p_dev.physsize_pxl ().cy / p_physsize_pxl.cy);
    }

  SIZE osize = p_size;
  switch (p_scale)
    {
    case ADJUST_WINDOW:
      if (p_client.cx * p_dev.physsize_pxl ().cy
          < p_client.cy * p_dev.physsize_pxl ().cx)
        goto adjust_width;
      else
        goto adjust_height;

    case ADJUST_WIDTH:
    adjust_width:
      scaling (p_client.cx - MARGIN * 2, p_dev.physsize_pxl ().cx);
      break;

    case ADJUST_HEIGHT:
    adjust_height:
      scaling (p_client.cy - MARGIN * 2, p_dev.physsize_pxl ().cy);
      break;

    default:
      scaling (p_ydpi * p_scale, p_dev.ydpi () * 100);
      break;
    }

  if (pt)
    {
      p_scroll_offset.x = (((o1.x * p_physsize_pxl.cx + p_dev.physsize_pxl ().cx - 1)
                            / p_dev.physsize_pxl ().cx)
                           - (pt->x - o2.x));
      p_scroll_offset.y = (((o1.y * p_physsize_pxl.cy + p_dev.physsize_pxl ().cy - 1)
                            / p_dev.physsize_pxl ().cy)
                           - (pt->y - o2.y));
    }

  update_vscroll_bar ();
  update_hscroll_bar ();

  if (p_size.cx == osize.cx && p_size.cy == osize.cy)
    return;

  InvalidateRect (p_hwnd, 0, 1);
}

void
preview_page_window::size (int w, int h)
{
  p_client.cx = w;
  p_client.cy = h;
  scaling ();
}

int
preview_page_window::set_scale (int scale, int update, POINT *pt)
{
  if (scale == p_scale)
    return 0;
  for (int i = 0; i < numberof (ids2scales); i++)
    if (scale == ids2scales[i].scale)
      {
        p_scale = scale;
        break;
      }
  if (scale >= MIN_SCALE && scale <= MAX_SCALE)
    p_scale = scale;
  scaling (pt);
  return 1;
}

inline int
preview_page_window::get_scale () const
{
  return p_scale;
}

int
preview_page_window::scale_value () const
{
  int n, d;
  switch (get_scale ())
    {
    case ADJUST_WINDOW:
      if (p_client.cx * p_dev.physsize_pxl ().cy
          < p_client.cy * p_dev.physsize_pxl ().cx)
        goto adjust_width;
      else
        goto adjust_height;

    case ADJUST_WIDTH:
    adjust_width:
      n = p_client.cx - MARGIN * 2;
      d = p_dev.physsize_pxl ().cx;
      break;

    case ADJUST_HEIGHT:
    adjust_height:
      n = p_client.cy - MARGIN * 2;
      d = p_dev.physsize_pxl ().cy;
      break;

    default:
      return get_scale ();
    }

  return n * p_dev.ydpi () * 100 / (d * p_ydpi);
}

void
preview_page_window::update_buttons ()
{
  HWND parent = GetParent (p_hwnd);
  EnableWindow (GetDlgItem (parent, IDC_NEXTPAGE),
                p_engine.next_page_exist_p ());
  EnableWindow (GetDlgItem (parent, IDC_PREVPAGE),
                p_engine.prev_page_exist_p ());

  PostMessage (parent, WM_PRIVATE_UPDATE_PAGE,
               p_engine.current_page (), p_total_pages);
}

void
preview_page_window::invalidate () const
{
  POINT p0;
  calc_origin (p0);
  p0.x -= p_scroll_offset.x;
  p0.y -= p_scroll_offset.y;
  RECT r;
  r.left = p0.x + p_min_margin_pxl.left - 1;
  r.top = p0.y + p_min_margin_pxl.top - 1;
  r.right = (p0.x + p_physsize_pxl.cx
             - p_min_margin_pxl.right + 1);
  r.bottom = (p0.y + p_physsize_pxl.cy
              - p_min_margin_pxl.bottom + 1);
  InvalidateRect (p_hwnd, &r, 1);
}


int
preview_page_window::next_page ()
{
  if (!p_engine.next_page ())
    return 0;
  p_updated = 0;
  invalidate ();
  return 1;
}

int
preview_page_window::prev_page ()
{
  if (!p_engine.prev_page ())
    return 0;
  p_updated = 0;
  invalidate ();
  return 1;
}

void
preview_page_window::calc_origin (POINT &p) const
{
  if (p_physsize_pxl.cx < p_client.cx - MARGIN * 2)
    p.x = (p_client.cx - p_physsize_pxl.cx) / 2;
  else
    p.x = MARGIN;

  if (p_physsize_pxl.cy < p_client.cy - MARGIN * 2)
    p.y = (p_client.cy - p_physsize_pxl.cy) / 2;
  else
    p.y = MARGIN;
}

void
preview_page_window::paint_paper (HDC hdc) const
{
  HBRUSH hbr = CreateSolidBrush (sysdep.btn_shadow);
  HGDIOBJ obj = SelectObject (hdc, hbr);
  PatBlt (hdc, p_physsize_pxl.cx, SHADOW,
          SHADOW, p_physsize_pxl.cy, PATCOPY);
  PatBlt (hdc, SHADOW, p_physsize_pxl.cy,
          p_physsize_pxl.cx, SHADOW, PATCOPY);
  SelectObject (hdc, obj);
  DeleteObject (hbr);

  HPEN hpen = CreatePen (PS_SOLID, 2, RGB (0, 0, 0));
  obj = SelectObject (hdc, hpen);
  MoveToEx (hdc, 0, 0, 0);
  LineTo (hdc, 0, p_physsize_pxl.cy);
  LineTo (hdc, p_physsize_pxl.cx, p_physsize_pxl.cy);
  LineTo (hdc, p_physsize_pxl.cx, 0);
  LineTo (hdc, 0, 0);
  SelectObject (hdc, obj);
  DeleteObject (hpen);

  HFONT hfont = p_settings.make_font (p_dev, FONT_ASCII, -d_font_height);
  HGDIOBJ ofont = SelectObject (hdc, hfont);
  TEXTMETRIC tm;
  GetTextMetrics (hdc, &tm);
  int h = tm.tmHeight;
  SelectObject (hdc, ofont);
  DeleteObject (hfont);

  hpen = CreatePen (PS_DOT, 0, RGB (192, 192, 192));
  obj = SelectObject (hdc, hpen);
  MoveToEx (hdc,
            p_min_margin_pxl.left,
            p_text_margin_pxl.top,
            0);
  LineTo (hdc,
          p_physsize_pxl.cx - p_min_margin_pxl.right,
          p_text_margin_pxl.top);
  MoveToEx (hdc,
            p_min_margin_pxl.left,
            p_physsize_pxl.cy - p_text_margin_pxl.bottom,
            0);
  LineTo (hdc,
          p_physsize_pxl.cx - p_min_margin_pxl.right,
          p_physsize_pxl.cy - p_text_margin_pxl.bottom);
  MoveToEx (hdc,
            p_text_margin_pxl.left,
            p_min_margin_pxl.top,
            0);
  LineTo (hdc,
          p_text_margin_pxl.left,
          p_physsize_pxl.cy - p_min_margin_pxl.bottom);
  MoveToEx (hdc,
            p_physsize_pxl.cx - p_text_margin_pxl.right,
            p_min_margin_pxl.top,
            0);
  LineTo (hdc,
          p_physsize_pxl.cx - p_text_margin_pxl.right,
          p_physsize_pxl.cy - p_min_margin_pxl.bottom);

  if (*p_settings.ps_header && p_settings.ps_header_on)
    {
      MoveToEx (hdc, p_min_margin_pxl.left, p_header_offset_pxl, 0);
      LineTo (hdc,
              p_physsize_pxl.cx - p_min_margin_pxl.right,
              p_header_offset_pxl);
    }

  if (*p_settings.ps_footer && p_settings.ps_footer_on)
    {
      MoveToEx (hdc, p_min_margin_pxl.left,
                p_physsize_pxl.cy - p_footer_offset_pxl, 0);
      LineTo (hdc,
              p_physsize_pxl.cx - p_min_margin_pxl.right,
              p_physsize_pxl.cy - p_footer_offset_pxl);
    }

  if (p_settings.ps_multi_column > 1)
    {
      int w = ((p_physsize_pxl.cx
                - (p_text_margin_pxl.left + p_text_margin_pxl.right)
                - (p_settings.ps_multi_column - 1) * p_column_sep_pxl)
               / p_settings.ps_multi_column);
      for (int i = 1, x = p_text_margin_pxl.left + w;
           i < p_settings.ps_multi_column;
           i++, x += w)
        {
          MoveToEx (hdc, x, p_text_margin_pxl.top, 0);
          LineTo (hdc, x, p_physsize_pxl.cy - p_text_margin_pxl.top);
          x += p_column_sep_pxl;
          MoveToEx (hdc, x, p_text_margin_pxl.top, 0);
          LineTo (hdc, x, p_physsize_pxl.cy - p_text_margin_pxl.top);
        }
    }

  SelectObject (hdc, obj);
  DeleteObject (hpen);
}

void
preview_page_window::paint ()
{
  PAINTSTRUCT ps;
  HDC hdc = BeginPaint (p_hwnd, &ps);

  POINT p0;
  calc_origin (p0);

  SetViewportOrgEx (hdc, p0.x - p_scroll_offset.x, p0.y - p_scroll_offset.y, 0);

  paint_paper (hdc);

  SetMapMode (hdc, MM_ISOTROPIC);
  SetWindowExtEx (hdc, p_dev.physsize_pxl ().cx, p_dev.physsize_pxl ().cy, 0);
  SetViewportExtEx (hdc, p_physsize_pxl.cx, p_physsize_pxl.cy, 0);
  SetViewportOrgEx (hdc, p0.x - p_scroll_offset.x, p0.y - p_scroll_offset.y, 0);

  p_engine.paint (hdc, 1);

  EndPaint (p_hwnd, &ps);

  if (!p_updated)
    {
      p_updated = 1;
      update_buttons ();
    }
}

int
preview_page_window::set_cursor (int hittest)
{
  if (hittest != HTCLIENT)
    return 0;
  SetCursor (LoadCursor (app.hinst, MAKEINTRESOURCE (IDC_MAGCUR)));
  return 1;
}

void
preview_page_window::lbutton_down (int x, int y)
{
  POINT pt;
  pt.x = x;
  pt.y = y;
  if (!set_scale (min (scale_value () + 10, MAX_SCALE), 1, &pt))
    return;
  PostMessage (GetParent (p_hwnd), WM_PRIVATE_UPDATE_SCALE, 0, 0);
}

void
preview_page_window::rbutton_down (int x, int y)
{
  POINT pt;
  pt.x = x;
  pt.y = y;
  if (!set_scale (max (scale_value () - 10, MIN_SCALE), 1, &pt))
    return;
  PostMessage (GetParent (p_hwnd), WM_PRIVATE_UPDATE_SCALE, 0, 0);
}

LRESULT
preview_page_window::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_CREATE:
      p_updated = 0;
      break;

    case WM_PAINT:
      paint ();
      return 0;

    case WM_VSCROLL:
      vscroll (LOWORD (wparam), short (HIWORD (wparam)));
      return 0;

    case WM_HSCROLL:
      hscroll (LOWORD (wparam), short (HIWORD (wparam)));
      return 0;

    case WM_SIZE:
      size (LOWORD (lparam), HIWORD (lparam));
      return 0;

    case WM_KEYDOWN:
      if (key_down (wparam))
        return 0;
      break;

    case WM_SETCURSOR:
      if (set_cursor (LOWORD (lparam)))
        return 1;
      break;

    case WM_LBUTTONDOWN:
      lbutton_down (short (LOWORD (lparam)), short (HIWORD (lparam)));
      return 0;

    case WM_RBUTTONDOWN:
      rbutton_down (short (LOWORD (lparam)), short (HIWORD (lparam)));
      return 0;

    case WM_GETDLGCODE:
      return DLGC_WANTARROWS;

    default:
      {
        wheel_info wi;
        if (p_wheel.msg_handler (p_hwnd, msg, wparam, lparam, wi))
          {
            wheel (wi);
            return 0;
          }
        break;
      }
    }
  return DefWindowProc (p_hwnd, msg, wparam, lparam);
}

LRESULT CALLBACK
preview_page_window::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  preview_page_window *p;
  if (msg == WM_NCCREATE)
    {
      p = (preview_page_window *)((CREATESTRUCT *)lparam)->lpCreateParams;
      SetWindowLong (hwnd, 0, LONG (p));
      p->p_hwnd = hwnd;
    }
  else
    {
      p = (preview_page_window *)GetWindowLong (hwnd, 0);
      if (!p)
        return DefWindowProc (hwnd, msg, wparam, lparam);
    }
  return p->wndproc (msg, wparam, lparam);
}

int
preview_page_window::register_wndclass (HINSTANCE hinst)
{
  if (wndclass_initialized)
    return 1;
  WNDCLASS wc;
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof (preview_page_window *);
  wc.hInstance = hinst;
  wc.hIcon = 0;
  wc.hCursor = 0;
  wc.hbrBackground = HBRUSH (COLOR_WINDOW + 1);
  wc.lpszMenuName = 0;
  wc.lpszClassName = PageClassName;
  if (!RegisterClass (&wc))
    return 0;
  wndclass_initialized = 1;
  return 1;
}

int
preview_page_window::create (HWND hwnd, const RECT &r)
{
  if (!register_wndclass (app.hinst))
    return 0;

  if (!CreateWindowEx (sysdep.Win4p () ? WS_EX_CLIENTEDGE : 0,
                       PageClassName, "",
                       (WS_HSCROLL | WS_VSCROLL | WS_VISIBLE | WS_CHILD
                        | WS_CLIPSIBLINGS | WS_TABSTOP
                        | (sysdep.Win4p () ? 0 : WS_BORDER)),
                       r.left, r.top, r.right - r.left, r.bottom - r.top,
                       hwnd, 0, app.hinst, this))
    return 0;
  return 1;
}

preview_dialog::preview_dialog (const printer_device &dev, print_settings &settings,
                                print_engine &engine)
     : p_page (dev, settings, engine)
{
}

void
preview_dialog::calc_page_rect (RECT &cr) const
{
  RECT r, sw, btn;
  GetClientRect (p_hwnd, &r);
  GetClientRect (GetDlgItem (p_hwnd, IDC_NEXTPAGE), &btn);
  GetClientRect (p_hwnd_sw, &sw);
  cr.left = 0;
  cr.top = btn.bottom + 2;
  cr.right = r.right;
  cr.bottom = r.bottom - sw.bottom;
}

void
preview_dialog::set_scale_combo ()
{
  int cur = -1;
  for (int i = 0; i < numberof (preview_page_window::ids2scales); i++)
    if (preview_page_window::ids2scales[i].scale == p_page.get_scale ())
      {
        SendDlgItemMessage (p_hwnd, IDC_SCALE, CB_SETCURSEL, i, 0);
        return;
      }
  char b[64];
  sprintf (b, "%d%%", p_page.get_scale ());
  SetDlgItemText (p_hwnd, IDC_SCALE, b);
}

int
preview_dialog::init_dialog (HWND)
{
  p_hwnd_sw = CreateStatusWindow ((SBARS_SIZEGRIP | WS_CHILD
                                   | WS_VISIBLE | WS_CLIPCHILDREN),
                                  0, p_hwnd, 0);
  if (!p_hwnd_sw)
    return -1;

  set_window_icon (p_hwnd);

  if (!conf_load_geometry (p_hwnd, cfgPrintPreview))
    center_window (p_hwnd);
  int scale;
  if (read_conf (cfgPrintPreview, cfgScale, scale))
    p_page.set_scale (scale);

  RECT r;
  calc_page_rect (r);
  if (!p_page.create (p_hwnd, r))
    return -1;

  for (int i = 0; i < numberof (preview_page_window::ids2scales); i++)
    {
      char b[128];
      LoadString (app.hinst, preview_page_window::ids2scales[i].ids,
                  b, sizeof b);
      UINT idx = SendDlgItemMessage (p_hwnd, IDC_SCALE, CB_ADDSTRING, 0, LPARAM (b));
      SendDlgItemMessage (p_hwnd, IDC_SCALE, CB_SETITEMDATA,
                          idx, preview_page_window::ids2scales[i].scale);
    }

  set_scale_combo ();

  SetFocus (p_page.p_hwnd);

  return 1;
}

inline BOOL
preview_dialog::destroy ()
{
  conf_save_geometry (p_hwnd, cfgPrintPreview);
  write_conf (cfgPrintPreview, cfgScale, p_page.get_scale ());
  flush_conf ();
  return 0;
}

inline BOOL
preview_dialog::notify (NMHDR *)
{
  return 0;
}

inline BOOL
preview_dialog::size (int, int)
{
  RECT r;
  calc_page_rect (r);
  MoveWindow (p_page.p_hwnd, r.left, r.top, r.right - r.left, r.bottom - r.top, 1);
  return 0;
}

inline BOOL
preview_dialog::next_page (UINT)
{
  p_page.next_page ();
  SetFocus (p_page.p_hwnd);
  return 1;
}

inline BOOL
preview_dialog::prev_page (UINT)
{
  p_page.prev_page ();
  SetFocus (p_page.p_hwnd);
  return 1;
}

BOOL
preview_dialog::scale_command (int code)
{
  switch (code)
    {
    case CBN_SELCHANGE:
      {
        int idx = SendDlgItemMessage (p_hwnd, IDC_SCALE, CB_GETCURSEL, 0, 0);
        if (idx == CB_ERR)
          return 0;
        p_page.set_scale (SendDlgItemMessage (p_hwnd, IDC_SCALE,
                                              CB_GETITEMDATA, idx, 0), 1);
      }
      return 1;

    case CBN_CLOSEUP:
      SetFocus (p_page.p_hwnd);
      return 1;

    case CBN_KILLFOCUS:
      {
        char buf[128];
        GetDlgItemText (p_hwnd, IDC_SCALE, buf, sizeof buf);
        int i = SendDlgItemMessage (p_hwnd, IDC_SCALE, CB_FINDSTRINGEXACT,
                                    WPARAM (-1), LPARAM (buf));
        if (i != CB_ERR)
          return 1;
        char *b;
        for (b = buf; *b == ' '; b++)
          ;
        char *be;
        long v = strtol (b, &be, 10);
        if (v > 0 && be != b)
          {
            for (; *be == ' '; be++)
              ;
            if (*be == '%')
              for (be++; *be == ' '; be++)
                ;
            if (!*be)
              p_page.set_scale (v, 1);
          }
        set_scale_combo ();
        return 1;
      }
    }
  return 0;
}

BOOL
preview_dialog::command (UINT id, UINT code)
{
  switch (id)
    {
    case IDOK:
    case IDCANCEL:
    case IDC_PRINT:
      EndDialog (p_hwnd, id);
      return 1;

    case IDC_SCALE:
      return scale_command (code);

    case IDC_NEXTPAGE:
      return next_page (code);

    case IDC_PREVPAGE:
      return prev_page (code);
    }
  return 0;
}

inline BOOL
preview_dialog::quit ()
{
  EndDialog (p_hwnd, IDCANCEL);
  return 1;
}

inline void
preview_dialog::update_page (int page, int total)
{
  char b[128];
  //  sprintf (b, "ページ %d/%d", page, total);
  sprintf (b, "ページ %d", page);
  SendMessage (p_hwnd_sw, SB_SETTEXT, 0, LPARAM (b));
}

inline void
preview_dialog::update_scale ()
{
  set_scale_combo ();
}

BOOL
preview_dialog::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      if (init_dialog (HWND (wparam)) < 0)
        EndDialog (p_hwnd, 0);
      return 0;

    case WM_DESTROY:
      return destroy ();

    case WM_NOTIFY:
      return notify ((NMHDR *)lparam);

    case WM_COMMAND:
      return command (LOWORD (wparam), HIWORD (wparam));

    case WM_PRIVATE_QUIT:
      return quit ();

    case WM_ACTIVATEAPP:
      PostThreadMessage (app.quit_thread_id, WM_PRIVATE_ACTIVATEAPP,
                         wparam, lparam);
      return 0;

    case WM_SIZE:
      if (p_hwnd_sw)
        SendMessage (p_hwnd_sw, msg, wparam, lparam);
      return size (LOWORD (lparam), HIWORD (lparam));

    case WM_PRIVATE_UPDATE_PAGE:
      update_page (wparam, lparam);
      return 0;

    case WM_PRIVATE_UPDATE_SCALE:
      update_scale ();
      return 0;

    default:
      return 0;
    }
}

BOOL CALLBACK
preview_dialog::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  preview_dialog *p;
  if (msg == WM_INITDIALOG)
    {
      p = (preview_dialog *)lparam;
      SetWindowLong (hwnd, DWL_USER, lparam);
      p->p_hwnd = hwnd;
    }
  else
    {
      p = (preview_dialog *)GetWindowLong (hwnd, DWL_USER);
      if (!p)
        return 0;
    }
  return p->wndproc (msg, wparam, lparam);
}

int
preview_dialog::do_modal (HWND hwnd)
{
  return DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_PREVIEW),
                         hwnd, wndproc, LPARAM (this));
}
