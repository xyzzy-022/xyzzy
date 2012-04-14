#include "stdafx.h"
#include "ed.h"
#include "monitor.h"

static const char csPopupList[] = "PopupList";
static WNDPROC org_wndproc;
static ATOM popup_list_atom;
static HWND hwnd_popup;

#define LIST_MAXW 800L
#define LIST_MINW 64L
#define LIST_MAXH 200L

static void
substitute_key (HWND hwnd, WPARAM &wparam, LPARAM lparam)
{
  BYTE state[256];
  WORD key[2] = {0};
  GetKeyboardState (state);
  if (ToAscii (wparam, lparam, state, key, 0) == 1)
    {
      int c = stdctl_operation (key[0]);
      if (c >= 0)
        wparam = c;
    }
}

static int
call_callback (HWND hwnd)
{
  int n = CallWindowProc (org_wndproc, hwnd, LB_GETCURSEL, 0, 0);
  if (n < 0)
    return 0;

  int l = CallWindowProc (org_wndproc, hwnd, LB_GETTEXTLEN, n, 0);
  if (l < 0)
    return 0;

  char *buf = (char *)alloca (l * 2 + 1);
  if (CallWindowProc (org_wndproc, hwnd, LB_GETTEXT, n, LPARAM (buf)) < 0)
    return 0;

  DestroyWindow (hwnd);

  try
    {
      funcall_1 (xsymbol_value (Vpopup_list_callback),
                 make_string (buf));
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
  return 1;
}

static LRESULT CALLBACK
wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_LBUTTONDBLCLK:
      call_callback (hwnd);
      return 0;

    case WM_KEYDOWN:
      substitute_key (hwnd, wparam, lparam);
      switch (wparam)
        {
        case VK_CONTROL:
        case VK_SHIFT:
        case VK_UP:
        case VK_DOWN:
        case VK_NEXT:
        case VK_PRIOR:
          break;

        case VK_SPACE:
          if (call_callback (hwnd))
            PostMessage (app.toplev, msg, wparam, lparam);
          return 0;

        case VK_RETURN:
        case VK_TAB:
          call_callback (hwnd);
          return 0;

        case VK_ESCAPE:
          DestroyWindow (hwnd);
          return 0;

        default:
          PostMessage (app.toplev, msg, wparam, lparam);
          DestroyWindow (hwnd);
          return 0;
        }
      break;

    case WM_CHAR:
      return 0;

    case WM_KILLFOCUS:
    case WM_PRIVATE_QUIT:
      DestroyWindow (hwnd);
      return 0;

    case WM_DESTROY:
      hwnd_popup = 0;
      break;

    default:
      break;
    }
  return CallWindowProc (org_wndproc, hwnd, msg, wparam, lparam);
}

static ATOM
define_wndclass ()
{
  WNDCLASS wc;

  if (!GetClassInfo (0, "ListBox", &wc))
    return 0;

  org_wndproc = wc.lpfnWndProc;
  wc.lpfnWndProc = wndproc;
  wc.hInstance = app.hinst;
  wc.lpszClassName = csPopupList;
  return RegisterClass (&wc);
}

lisp
Fpopup_list (lisp list, lisp callback, lisp lpoint)
{
  Window *wp = selected_window ();

  POINT pos;
  wp->point2window_pos ((!lpoint || lpoint == Qnil
                         ? wp->w_point.p_point : wp->w_bufp->coerce_to_point (lpoint)),
                        pos);
  ClientToScreen (wp->w_hwnd, &pos);

  if (!consp (list))
    return Qnil;

  for (lisp p = list; consp (p); p = xcdr (p))
    check_string (xcar (p));

  if (hwnd_popup)
    return Qnil;

  xsymbol_value (Vpopup_list_callback) = callback;

  if (!popup_list_atom)
    popup_list_atom = define_wndclass ();

  hwnd_popup = CreateWindowEx (WS_EX_DLGMODALFRAME, csPopupList, "",
                               WS_POPUP | WS_VSCROLL, 0, 0, 0, 0,
                               app.toplev, 0, app.hinst, 0);

  HFONT hf = sysdep.ui_font ();
  SendMessage (hwnd_popup, WM_SETFONT, WPARAM (hf), 1);

  SIZE sz;
  sz.cx = LIST_MINW;
  sz.cy = 0;

  HDC hdc = GetDC (hwnd_popup);
  HGDIOBJ of = SelectObject (hdc, hf);
  for (lisp p = list; consp (p); p = xcdr (p))
    {
      lisp s = xcar (p);
      char b[1024];
      int l = w2s (b, b + sizeof b, xstring_contents (s), xstring_length (s)) - b;
      SendMessage (hwnd_popup, LB_ADDSTRING, 0, LPARAM (b));
      SIZE ext;
      GetTextExtentPoint32 (hdc, b, l, &ext);
      sz.cx = max (sz.cx, ext.cx);
      sz.cy += ext.cy;
    }
  SelectObject (hdc, of);
  ReleaseDC (hwnd_popup, hdc);

  if (sz.cx > LIST_MAXW)
    sz.cx = LIST_MAXW;
  if (sz.cy > LIST_MAXH)
    {
      sz.cy = LIST_MAXH;
      sz.cx += sysdep.vscroll;
    }
  sz.cx += sysdep.edge.cx;
  sz.cy += sysdep.edge.cy;

  RECT r;
  r.left = r.top = 0;
  r.right = sz.cx;
  r.bottom = sz.cy;
  AdjustWindowRectEx (&r, WS_POPUP, 0, WS_EX_DLGMODALFRAME);

  sz.cx = r.right - r.left;
  sz.cy = r.bottom - r.top;

  RECT wk;
  monitor.get_workarea_from_point (pos, &wk);

  if (pos.y + app.text_font.cell ().cy + sz.cy <= wk.bottom)
    pos.y += app.text_font.cell ().cy;
  else if (wk.bottom - (pos.y + app.text_font.cell ().cy) > LIST_MAXH / 2)
    {
      pos.y += app.text_font.cell ().cy;
      sz.cy = wk.bottom - pos.y;
    }
  else
    {
      if (pos.y - sz.cy < wk.top)
        {
          sz.cy = pos.y - wk.top;
          pos.y = wk.top;
        }
      else
        pos.y -= sz.cy;
    }

  if (pos.x + sz.cx > wk.right)
    {
      pos.x = wk.right - sz.cx;
      if (pos.x < wk.left)
        {
          pos.x = wk.left;
          sz.cx = wk.right - wk.left;
        }
    }

  SetWindowPos (hwnd_popup, HWND_TOP, pos.x, pos.y, sz.cx, sz.cy, SWP_SHOWWINDOW);
  UpdateWindow (hwnd_popup);

  SendMessage (hwnd_popup, LB_SETCURSEL, 0, 0);

  return Qt;
}
