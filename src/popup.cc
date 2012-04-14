#include "stdafx.h"
#include "ed.h"
#include "monitor.h"

static char *popup_text;
static RECT popup_rect;
static HWND hwnd_popup;
static HFONT hfont_popup;
static int f_popup_visible;
static int f_continue;
static int f_first_time;

#define XPAD 2
#define YPAD 1

#define TEXTCOLOR (!sysdep.Win4p () ? RGB (0, 0, 128) : GetSysColor (COLOR_INFOTEXT))
#define BACKCOLOR (!sysdep.Win4p () ? RGB (255, 255, 0) : GetSysColor (COLOR_INFOBK))

#define TIMER_ID 1

static void
calc_rect (HDC hdc, const RECT &scr, RECT &r, int num, int den)
{
  memset (&r, 0, sizeof r);
  r.right = (scr.right - scr.left) * num / den;
  DrawText (hdc, popup_text, -1, &r,
            (DT_EXTERNALLEADING | DT_CALCRECT | DT_EXPANDTABS
             | DT_LEFT | DT_NOPREFIX | DT_WORDBREAK));
}

static int
check_range (const RECT &scr, const RECT &r, const RECT &pos, POINT &p)
{
  int cx = r.right + XPAD * 2;
  int cy = r.bottom + YPAD * 2;
  if (pos.left + cx <= scr.right)
    p.x = pos.left;
  else
    p.x = max (scr.left, LONG (scr.right - cx));
  if (scr.top + cy <= pos.top)
    {
      p.y = pos.top - cy;
      return 1;
    }
  else
    {
      p.y = pos.bottom;
      return p.y + cy <= scr.bottom;
    }
}

static void
set_text (const char *text, const RECT &pos)
{
  RECT scr;
  monitor.get_workarea_from_rect (&pos, &scr);
  scr.left += 16;
  scr.top += 16;
  scr.right -= 16;
  scr.bottom -= 16;

  popup_text = (char *)text;

  RECT r[4];

  HDC hdc = GetDC (hwnd_popup);
  HGDIOBJ of = SelectObject (hdc, hfont_popup);
  calc_rect (hdc, scr, r[0], 1, 3);
  calc_rect (hdc, scr, r[1], 1, 2);
  calc_rect (hdc, scr, r[2], 3, 4);
  calc_rect (hdc, scr, r[3], 1, 1);
  SelectObject (hdc, of);
  ReleaseDC (hwnd_popup, hdc);
  InvalidateRect (hwnd_popup, 0, 1);

  int i;
  for (i = 0; i < numberof (r); i++)
    OffsetRect (&r[i], XPAD, YPAD);

  POINT p;
  for (i = 0; i < numberof (r); i++)
    if (check_range (scr, r[i], pos, p)
        && p.y != pos.bottom)
      break;
  if (i >= numberof (r) - 1)
    for (i = 0; i < numberof (r); i++)
      if (check_range (scr, r[i], pos, p))
        break;
  if (i < numberof (r))
    popup_rect = r[i];
  else
    {
      i--;
      popup_rect = r[i];
      if (pos.top - scr.top >= scr.bottom - pos.bottom)
        {
          p.y = pos.top - (r[i].bottom + YPAD * 2);
          if (p.y < scr.top)
            {
              r[i].bottom -= scr.top - p.y;
              p.y = scr.top;
            }
        }
      else
        p.y = pos.bottom;
    }

  SetWindowPos (hwnd_popup, 0, p.x, p.y,
                r[i].right + XPAD * 2, r[i].bottom + YPAD * 2,
                SWP_NOACTIVATE | SWP_NOZORDER);

  ShowWindow (hwnd_popup, SW_SHOWNA);
  f_popup_visible = 1;
}

static void
dopaint (HDC hdc)
{
  if (!popup_text)
    return;

  HGDIOBJ of = SelectObject (hdc, hfont_popup);
  SetTextColor (hdc, TEXTCOLOR);
  SetBkColor (hdc, BACKCOLOR);
  DrawText (hdc, popup_text, -1, &popup_rect,
            DT_EXTERNALLEADING | DT_EXPANDTABS | DT_LEFT | DT_NOPREFIX | DT_WORDBREAK);
  SelectObject (hdc, of);
}

static LRESULT CALLBACK
wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint (hwnd, &ps);
        dopaint (hdc);
        EndPaint (hwnd, &ps);
        return 0;
      }

    case WM_ERASEBKGND:
      {
        RECT r;
        GetClientRect (hwnd, &r);
        HDC hdc = HDC (wparam);
        HGDIOBJ obr = SelectObject (hdc, CreateSolidBrush (BACKCOLOR));
        PatBlt (hdc, 0, 0, r.right, r.bottom, PATCOPY);
        DeleteObject (SelectObject (hdc, obr));
        return 1;
      }

    case WM_MOUSEACTIVATE:
      return MA_NOACTIVATEANDEAT;

    case WM_TIMER:
      KillTimer (hwnd, wparam);
      erase_popup (1, 0);
      break;

    case WM_MOUSEMOVE:
    case WM_NCMOUSEMOVE:
      erase_popup (1, 1);
      break;

    case WM_DESTROY:
      hwnd_popup = 0;
      if (popup_text)
        xfree (popup_text);
      if (hfont_popup)
        {
          DeleteObject (hfont_popup);
          hfont_popup = 0;
        }
      return 0;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

static int
create_popup ()
{
  static const char wclass[] = "popup!?";

  f_first_time = 1;
  if (hwnd_popup)
    {
      KillTimer (hwnd_popup, TIMER_ID);
      return 1;
    }

  WNDCLASS wc;
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = app.hinst;
  wc.hIcon = 0;
  wc.hCursor = LoadCursor (0, IDC_ARROW);
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = wclass;
  if (!RegisterClass (&wc))
    return 0;

  hwnd_popup = CreateWindow (wclass, "",
                             WS_POPUP | WS_BORDER,
                             0, 0, 0, 0,
                             app.toplev, 0, app.hinst, 0);
  if (!hwnd_popup)
    return 0;

  if (!hfont_popup)
    {
      NONCLIENTMETRICS cm;
      cm.cbSize = sizeof cm;
      if (!SystemParametersInfo (SPI_GETNONCLIENTMETRICS, 0, &cm, 0))
        {
          HDC hdc = GetDC (hwnd_popup);
          memset (&cm.lfStatusFont, 0, sizeof cm.lfStatusFont);
          cm.lfStatusFont.lfHeight = MulDiv (9, GetDeviceCaps (hdc, LOGPIXELSY), 72);
          cm.lfStatusFont.lfCharSet = SHIFTJIS_CHARSET;
          strcpy (cm.lfStatusFont.lfFaceName, "MS UI Gothic");
          ReleaseDC (hwnd_popup, hdc);
        }
      hfont_popup = CreateFontIndirect (&cm.lfStatusFont);
    }

  return 1;
}

static void
calc_pos (lisp lpoint, RECT &r)
{
  Window *wp = selected_window ();
  wp->point2window_pos (wp->w_bufp->coerce_to_point (lpoint), *(POINT *)&r);

  r.right = r.left;
  r.bottom = r.top + app.text_font.cell ().cy;

  r.top -= 4;
  r.bottom += 8;

  ClientToScreen (wp->w_hwnd, (POINT *)&r);
  ClientToScreen (wp->w_hwnd, (POINT *)&r + 1);
}

lisp
Fpopup_string (lisp lstring, lisp lpoint, lisp ltimeout)
{
  f_continue = 0;

  int timeout = -1;
  if (ltimeout && ltimeout != Qnil)
    timeout = fixnum_value (ltimeout);

  RECT r;
  check_string (lstring);
  calc_pos (lpoint, r);

  if (!create_popup ())
    FEsimple_win32_error (GetLastError ());

  if (popup_text)
    {
      xfree (popup_text);
      popup_text = 0;
    }

  int l = w2sl (lstring) + 1;
  char *p = (char *)xmalloc (l);
  w2s (p, lstring);
  set_text (p, r);
  if (timeout > 0)
    SetTimer (hwnd_popup, TIMER_ID, timeout * 1000, 0);

  f_continue = 1;
  return Qt;
}

lisp
Fcontinue_popup ()
{
  f_continue = 1;
  return Qt;
}

void
erase_popup (int force, int mouse_move)
{
  if (mouse_move && f_first_time)
    {
      f_first_time = 0;
      return;
    }
  if ((force || !f_continue) && f_popup_visible)
    {
      f_popup_visible = 0;
      KillTimer (hwnd_popup, TIMER_ID);
      ShowWindow (hwnd_popup, SW_HIDE);
    }
  f_continue = 0;
}
