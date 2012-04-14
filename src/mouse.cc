#include "stdafx.h"
#include "ed.h"

#ifndef VK_XBUTTON1
#define VK_XBUTTON1 5
#define VK_XBUTTON2 6
#endif

#define MK_BUTTON_MASK \
  (MK_LBUTTON | MK_MBUTTON | MK_RBUTTON | MK_XBUTTON1 | MK_XBUTTON2)

#define WM_MOUSELAST_WIN5 WM_XBUTTONDBLCLK

static int
oob_small (int x, int c)
{
  if (x < -4 * c)
    return 4;
  if (x < -2 * c)
    return 3;
  if (x < -c)
    return 2;
  return 1;
}

static int
oob_large (int x, int c)
{
  if (x > c + 3)
    return 4;
  if (x > c + 1)
    return 3;
  if (x > c)
    return 2;
  return 1;
}

int
rowcol_from_point (Window *wp, int *xx, int *yy)
{
  int x = *xx, y = *yy;
  int wincx = wp->w_ech.cx;
  if (wp->flags () & Window::WF_LINE_NUMBER)
    {
      x -= (Window::LINENUM_COLUMNS + 1) * app.text_font.cell ().cx;
      wincx -= Window::LINENUM_COLUMNS + 1;
    }
  x -= app.text_font.cell ().cx / 2;
  x -= wp->w_bufp->b_prompt_columns * app.text_font.cell ().cx; // mini buffer
  wincx -= wp->w_bufp->b_prompt_columns;

  int oob = 0;
  if (x < 0)
    {
      oob = oob_small (x, app.text_font.cell ().cx);
      x = -1;
    }
  else
    {
      x /= app.text_font.cell ().cx;
      if (x >= wincx)
        {
          oob = oob_large (x, wincx);
          x = wincx;
        }
    }

  if (y < 0)
    {
      oob = oob_small (y, app.text_font.cell ().cy);
      y = -1;
    }
  else
    {
      y /= app.text_font.cell ().cy;
      if (y >= wp->w_ech.cy)
        {
          oob = oob_large (y, wp->w_ech.cy);
          y = wp->w_ech.cy;
        }
    }

  *yy = wp->w_last_top_linenum + y;
  *xx = wp->w_last_top_column + x;

  wp->w_ignore_scroll_margin = max (0, min (y, int (wp->w_ech.cy - 1 - y))) + 1;

  return oob;
}

void
mouse_state::dispatch (Window *wp, WPARAM wparam, LPARAM lparam, int op)
{
  Char c;
  static const Char keys[][3] =
    {
      {CCF_LBTNDOWN, CCF_LBTNUP, CCF_LBTNMOVE},
      {CCF_MBTNDOWN, CCF_MBTNUP, CCF_MBTNMOVE},
      {CCF_RBTNDOWN, CCF_RBTNUP, CCF_RBTNMOVE},
      {CCF_XBTN1DOWN, CCF_XBTN1UP, CCF_XBTN1MOVE},
      {CCF_XBTN2DOWN, CCF_XBTN2UP, CCF_XBTN2MOVE},
    };

  int mouse_move_p = 0;

  switch (wparam & MK_BUTTON_MASK)
    {
    case MK_LBUTTON:
      c = keys[0][op];
      break;

    case MK_MBUTTON:
      c = keys[1][op];
      break;

    case MK_RBUTTON:
      c = keys[2][op];
      break;

    case MK_XBUTTON1:
      c = keys[3][op];
      break;

    case MK_XBUTTON2:
      c = keys[4][op];
      break;

    default:
      if (op != MOVE || !app.toplevel_is_active)
        return;
      c = CCF_MOUSEMOVE;
      mouse_move_p = 1;
      break;
    }

  if (wparam & MK_SHIFT)
    c |= CCF_SHIFT_BIT;
  if (wparam & MK_CONTROL)
    c |= CCF_CTRL_BIT;
  if (GetKeyState (VK_MENU) < 0)
    c = function_to_meta_function (c);

  if (mouse_move_p)
    {
      if (c != ms_movechar)
        {
          ms_movechar = c;
          ms_enable_move = find_in_current_keymaps (c);
        }
      if (!ms_enable_move || !toplev_accept_mouse_move_p ())
        return;
    }

  int wm_mouse_last = sysdep.Win5p () ? WM_MOUSELAST_WIN5 : WM_MOUSELAST;

  int x = short (LOWORD (lparam));
  int y = short (HIWORD (lparam));
  int oob = rowcol_from_point (wp, &x, &y);
  if (op == MOVE && oob && wparam & MK_BUTTON_MASK)
    {
      switch (oob)
        {
        case 1:
          Sleep (110);
          break;
        case 2:
          Sleep (55);
          break;
        case 3:
          Sleep (1);
          break;
        default:
          Sleep (0);
          break;
        }
      MSG msg;
      if (!PeekMessage (&msg, wp->w_hwnd, WM_MOUSEFIRST, wm_mouse_last, PM_NOREMOVE))
        {
          int status;
          if (SwapMouseButton (0))
            {
              status = ((GetAsyncKeyState (VK_LBUTTON) < 0 ? MK_RBUTTON : 0)
                        | (GetAsyncKeyState (VK_RBUTTON) < 0 ? MK_LBUTTON : 0));
              SwapMouseButton (1);
            }
          else
            status = ((GetAsyncKeyState (VK_LBUTTON) < 0 ? MK_LBUTTON : 0)
                      | (GetAsyncKeyState (VK_RBUTTON) < 0 ? MK_RBUTTON : 0));

          if (GetAsyncKeyState (VK_MBUTTON) < 0)
            status |= MK_MBUTTON;
          if (sysdep.Win5p ())
            {
              if (GetAsyncKeyState (VK_XBUTTON1) < 0)
                status |= MK_XBUTTON1;
              if (GetAsyncKeyState (VK_XBUTTON2) < 0)
                status |= MK_XBUTTON2;
            }

          if (status)
            {
              POINT p;
              GetCursorPos (&p);
              ScreenToClient (wp->w_hwnd, &p);
              PostMessage (wp->w_hwnd, WM_MOUSEMOVE,
                           ((GetAsyncKeyState (VK_CONTROL) < 0 ? MK_CONTROL : 0)
                            | (GetAsyncKeyState (VK_SHIFT) < 0 ? MK_SHIFT : 0)
                            | status),
                           MAKELONG (p.x, p.y));
            }
        }
    }

  int oline = ms_line;
  int ocolumn = ms_column;
  ms_line = y;
  ms_column = x;

  xsymbol_value (Vlast_mouse_window) = wp->lwp;
  xsymbol_value (Vlast_mouse_line) = make_fixnum (ms_line);
  xsymbol_value (Vlast_mouse_column) = make_fixnum (ms_column);
  xsymbol_value (Vlast_mouse_click_count) = make_fixnum (ms_click_count);

  if (op == MOVE && ms_line == oline && ms_column == ocolumn)
    return;

  ms_kbdq.putc (c | LCHAR_MOUSE);
}

void
mouse_state::click_count (WPARAM wparam, LPARAM lparam)
{
  POINT p = {short (LOWORD (lparam)), short (HIWORD (lparam))};
  LONG t = GetMessageTime ();
  int w = sysdep.dblclk.cx / 2;
  int h = sysdep.dblclk.cy / 2;
  if (ms_last_modifier != wparam
      || p.x < ms_point.x - w || p.x > ms_point.x + w
      || p.y < ms_point.y - h || p.y > ms_point.y + h
      || (LONG)(ms_time + GetDoubleClickTime ()) < t)
    ms_click_count = 1;
  else
    ms_click_count++;
  ms_point = p;
  ms_time = t;
  ms_modifier = ms_last_modifier = wparam;
}

void
mouse_state::down (Window *wp, WPARAM wparam, LPARAM lparam, UINT button)
{
  if (!ms_kbdq.idlep ())
    return;
  if (wp->w_bufp && !((wparam & ~button) & MK_BUTTON_MASK))
    {
      ms_hwnd = wp->w_hwnd;
      SetCapture (ms_hwnd);
      wparam |= button;
      click_count (wparam, lparam);
      dispatch (wp, wparam, lparam, DOWN);
    }
}

void
mouse_state::move (Window *wp, WPARAM wparam, LPARAM lparam)
{
  if (!ms_kbdq.idlep ())
    return;
  if (wp->w_bufp
      && (ms_modifier & MK_BUTTON_MASK) == (wparam & MK_BUTTON_MASK))
    dispatch (wp, wparam, lparam, MOVE);
}

void
mouse_state::up (Window *wp, WPARAM wparam, LPARAM lparam, UINT button)
{
  int f = ms_modifier & button;
  ms_modifier &= ~button;
  if (!(ms_modifier & MK_BUTTON_MASK) && GetCapture () == ms_hwnd)
    ReleaseCapture ();
  if (!f || !ms_kbdq.idlep ())
    return;
  if (wp->w_bufp)
    dispatch (wp, ms_modifier | button, lparam, UP);
}

void
mouse_state::cancel ()
{
  if (GetCapture () == ms_hwnd)
    ReleaseCapture ();
  ms_modifier = 0;
  ms_last_modifier = 0;
  ms_hwnd = 0;
}

int
mouse_state::track_popup_menu (HMENU hmenu, lisp lbtn, const POINT *pt)
{
  POINT p;
  int button;

  if (!pt)
    {
      if (lbtn == Kbutton1)
        goto lbutton;
      if (lbtn == Kbutton2)
        goto rbutton;
      switch (ms_modifier & MK_BUTTON_MASK)
        {
        case MK_LBUTTON:
        lbutton:
          button = TPM_LEFTBUTTON;
          GetCursorPos (&p);
          break;

        case MK_RBUTTON:
        rbutton:
          button = TPM_RIGHTBUTTON;
          GetCursorPos (&p);
          break;

        default:
          {
            button = TPM_LEFTBUTTON;
            Window *wp = selected_window ();
            if (!wp)
              return 0;
            p.x = wp->caret_x () + app.text_font.cell ().cx;
            p.y = wp->caret_y () + app.text_font.cell ().cy;
            ClientToScreen (wp->w_hwnd, &p);
            break;
          }
        }
    }
  else
    {
      p = *pt;
      button = TPM_LEFTBUTTON;
    }

  int result = ms_kbdq.track_popup_menu (hmenu, button, p);
  cancel ();
  return result;
}

void
mouse_state::show_cursor ()
{
  if (ms_hidden)
    {
      ms_hidden = 0;
      while (ShowCursor (1) < 0)
        ;
    }
}

void
mouse_state::hide_cursor ()
{
  if (xsymbol_value (Vhide_mouse_cursor) != Qnil && !ms_hidden
      && !app.wait_cursor_depth)
    {
      RECT r;
      GetWindowRect (app.toplev, &r);
      DWORD pos = GetMessagePos ();
      POINT p;
      p.x = short (LOWORD (pos));
      p.y = short (HIWORD (pos));
      if (PtInRect (&r, p))
        {
          ms_hidden = 1;
          while (ShowCursor (0) >= 0)
            ;
          ms_last_pos = pos;
        }
    }
}

void
mouse_state::update_cursor (UINT msg, WPARAM wparam)
{
  if (!hhook_mouse)
    return;
  switch (msg)
    {
    case WM_ACTIVATE:
      if (LOWORD (wparam) == WA_INACTIVE)
        show_cursor ();
      break;

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
      switch (wparam)
        {
        case VK_SHIFT:
        case VK_CONTROL:
        case VK_MENU:
        case VK_LWIN:
        case VK_RWIN:
          break;

        default:
          hide_cursor ();
          break;
        }
      break;
    }
}

LRESULT CALLBACK
mouse_state::mouse_hook_proc (int code, WPARAM wparam, LPARAM lparam)
{
  if (code == HC_ACTION)
    switch (wparam)
      {
      case WM_NCMOUSEMOVE:
      case WM_MOUSEMOVE:
        {
          MOUSEHOOKSTRUCT *ms = (MOUSEHOOKSTRUCT *)lparam;
          if (ms_hidden && DWORD (MAKELONG (ms->pt.x, ms->pt.y)) != ms_last_pos)
            show_cursor ();
          break;
        }

      case WM_NCLBUTTONDOWN:
      case WM_NCLBUTTONUP:
      case WM_NCRBUTTONDOWN:
      case WM_NCRBUTTONUP:
      case WM_NCMBUTTONDOWN:
      case WM_NCMBUTTONUP:
      case WM_NCXBUTTONDOWN:
      case WM_NCXBUTTONUP:
      case WM_LBUTTONDOWN:
      case WM_LBUTTONUP:
      case WM_RBUTTONDOWN:
      case WM_RBUTTONUP:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_XBUTTONDOWN:
      case WM_XBUTTONUP:
      case WM_MOUSEWHEEL:
        show_cursor ();
        break;
      }
  return CallNextHookEx (hhook_mouse, code, wparam, lparam);
}

int mouse_state::ms_hidden;
DWORD mouse_state::ms_last_pos = DWORD (-1);
HHOOK mouse_state::hhook_mouse;
