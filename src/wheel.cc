#include "stdafx.h"
#include "ed.h"
#include "wheel.h"

static UINT
get_mouse_scroll_lines ()
{
  UINT nlines = 0;

  UINT PWM_MSH_SCROLL_LINES = RegisterWindowMessage (MSH_SCROLL_LINES);
  if (PWM_MSH_SCROLL_LINES)
    {
      HWND hwnd = FindWindow (MSH_WHEELMODULE_CLASS, MSH_WHEELMODULE_TITLE);
      if (hwnd)
        {
          nlines = SendMessage (hwnd, PWM_MSH_SCROLL_LINES, 0, 0);
          if (nlines)
            return nlines;
        }
    }

  HKEY hkey;
  if (RegOpenKeyEx (HKEY_CURRENT_USER,  "Control Panel\\Desktop",
                    0, KEY_QUERY_VALUE, &hkey) == ERROR_SUCCESS)
    {
      char buf[64];
      DWORD type;
      DWORD size = sizeof buf;
      if (RegQueryValueEx (hkey, "WheelScrollLines", 0, &type,
                           (BYTE *)buf, &size) == ERROR_SUCCESS
          && type == REG_SZ)
        nlines = strtoul (buf, 0, 10);
      RegCloseKey (hkey);
      if (nlines)
        return nlines;
    }

  if (SystemParametersInfo (SPI_GETWHEELSCROLLLINES, 0, &nlines, 0)
      && nlines)
    return nlines;

  return 3;
}

UINT mouse_wheel::mw_nlines = 0;
UINT mouse_wheel::PWM_MSH_MOUSEWHEEL;

mouse_wheel::mouse_wheel ()
     : mw_delta (0)
{
  if (!mw_nlines)
    {
      mw_nlines = get_mouse_scroll_lines ();
      PWM_MSH_MOUSEWHEEL = RegisterWindowMessage (MSH_MOUSEWHEEL);
    }
}

mouse_wheel::~mouse_wheel ()
{
}

int
mouse_wheel::msg_handler (HWND hwnd, UINT msg,
                          WPARAM wparam, LPARAM lparam, wheel_info &wi)
{
  if (msg == WM_MOUSEWHEEL)
    mw_delta += (short)HIWORD (wparam);
  else if (msg && msg == PWM_MSH_MOUSEWHEEL)
    mw_delta += (int)wparam;
  else
    return 0;

  int neg = mw_delta < 0;
  if (neg)
    mw_delta = -mw_delta;

  wi.wi_value = mw_delta / WHEEL_DELTA;
  wi.wi_nlines = mw_nlines;

  mw_delta %= WHEEL_DELTA;
  if (neg)
    {
      mw_delta = -mw_delta;
      wi.wi_value = -wi.wi_value;
    }
  wi.wi_pt.x = (short)LOWORD (lparam);
  wi.wi_pt.y = (short)HIWORD (lparam);
  return 1;
}

static ATOM auto_scroll_atom;
static const char auto_scroll_class_name[] = "autoScrollClass";

#define STATE_CENTER 0
#define STATE_UP 1
#define STATE_DOWN 2

#define BMWIDTH 28
#define BMHEIGHT 28
#define HOTSPOTX 13
#define HOTSPOTY 13

#define MIN_RANGE (BMHEIGHT / 2 - 1)

static const int timer_intervals[] = {300, 300, 200, 100, 50, 1};

static LRESULT CALLBACK
auto_scroll_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  if (msg == WM_PAINT)
    {
      PAINTSTRUCT ps;
      HDC hdc = BeginPaint (hwnd, &ps);
      HDC mem = CreateCompatibleDC (hdc);
      HGDIOBJ obm = SelectObject (mem, LoadBitmap (app.hinst, MAKEINTRESOURCE (IDB_WHEEL)));
      BitBlt (hdc, 0, 0, BMWIDTH, BMHEIGHT, mem, 0, 0, SRCCOPY);
      DeleteObject (SelectObject (mem, obm));
      DeleteDC (mem);
      EndPaint (hwnd, &ps);
      return 0;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

static ATOM
register_wndclass ()
{
  if (!auto_scroll_atom)
    {
      WNDCLASS wc;
      wc.style = 0;
      wc.lpfnWndProc = auto_scroll_wndproc;
      wc.cbClsExtra = 0;
      wc.cbWndExtra = 0;
      wc.hInstance = app.hinst;
      wc.hIcon = 0;
      wc.hCursor = 0;
      wc.hbrBackground = 0;
      wc.lpszMenuName = 0;
      wc.lpszClassName = auto_scroll_class_name;
      auto_scroll_atom = RegisterClass (&wc);
    }
  return auto_scroll_atom;
}

struct auto_scroll_param
{
  POINT o;
  POINT p;
  int state;
  int interval;
  HCURSOR hcur[3];
  UINT timer_id;
  void (__stdcall *callback)(int, void *);
  void *arg;
};

static void
do_auto_scroll (auto_scroll_param &param)
{
  int ostate = param.state;
  int ointerval = param.interval;

  if (param.p.y < param.o.y - MIN_RANGE)
    {
      param.state = STATE_UP;
      param.interval = (param.o.y - (param.p.y + MIN_RANGE - 15)) / 16;
    }
  else if (param.p.y > param.o.y + MIN_RANGE)
    {
      param.state = STATE_DOWN;
      param.interval = (param.p.y - (param.o.y + MIN_RANGE - 15)) / 16;
    }
  else
    {
      param.state = STATE_CENTER;
      param.interval = 0;
    }

  param.interval = min (param.interval, (int)numberof (timer_intervals) - 1);
  if (param.state != ostate
      || param.interval != ointerval)
    {
      KillTimer (0, param.timer_id);
      param.timer_id = SetTimer (0, 0, timer_intervals[param.interval], 0);
    }

  if (param.state != ostate)
    SetCursor (param.hcur[param.state]);

  if (param.state != STATE_CENTER)
    (*param.callback)(param.state == STATE_UP ? -1 : 1, param.arg);
}

int
begin_auto_scroll (HWND hwnd_parent, const POINT &point,
                   void (__stdcall *callback)(int, void *), void *arg)
{
  if (!register_wndclass ())
    return 0;

  HWND hwnd_scroll = CreateWindow (auto_scroll_class_name, "", WS_POPUP,
                                   0, 0, 0, 0, hwnd_parent,
                                   0, app.hinst, 0);
  if (!hwnd_scroll)
    return 0;

  HRGN hrgn = CreateEllipticRgn (0, 0, BMWIDTH + 1, BMHEIGHT + 1);
  if (!hrgn || !SetWindowRgn (hwnd_scroll, hrgn, 0))
    {
      DestroyWindow (hwnd_scroll);
      if (hrgn)
        DeleteObject (hrgn);
      return 0;
    }

  auto_scroll_param param;
  param.callback = callback;
  param.arg = arg;

  param.hcur[STATE_CENTER] = LoadCursor (app.hinst, MAKEINTRESOURCE (IDC_WHEEL_UD));
  param.hcur[STATE_UP] = LoadCursor (app.hinst, MAKEINTRESOURCE (IDC_WHEEL_U));
  param.hcur[STATE_DOWN] = LoadCursor (app.hinst, MAKEINTRESOURCE (IDC_WHEEL_D));

  param.o = point;
  SetWindowPos (hwnd_scroll, 0, param.o.x - HOTSPOTX, param.o.y - HOTSPOTY,
                BMWIDTH, BMHEIGHT,
                SWP_NOACTIVATE | SWP_SHOWWINDOW | SWP_NOZORDER);
  SetCapture (hwnd_scroll);

  SetCursor (param.hcur[STATE_CENTER]);
  param.state = STATE_CENTER;
  param.interval = 0;
  param.timer_id = SetTimer (0, 0, timer_intervals[0], 0);

  MSG msg;
  while (1)
    {
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          break;
        }

      switch (msg.message)
        {
        case WM_KEYDOWN:
        case WM_KEYUP:
        case WM_SYSKEYDOWN:
        case WM_SYSKEYUP:
        case WM_LBUTTONDOWN:
        case WM_MBUTTONDOWN:
        case WM_RBUTTONDOWN:
        case WM_XBUTTONDOWN:
        case WM_NCLBUTTONDOWN:
        case WM_NCMBUTTONDOWN:
        case WM_NCRBUTTONDOWN:
        case WM_NCXBUTTONDOWN:
        case WM_MOUSEWHEEL:
        case WM_CANCELMODE:
          goto done;

        case WM_MOUSEMOVE:
          break;

        case WM_TIMER:
          if (!msg.hwnd && msg.wParam == param.timer_id)
            {
              GetCursorPos (&param.p);
              do_auto_scroll (param);
              break;
            }
          /* fall thru... */
        default:
          DispatchMessage (&msg);
          break;
        }
    }
done:
  KillTimer (0, param.timer_id);
  ReleaseCapture ();
  DestroyWindow (hwnd_scroll);
  return 1;
}
