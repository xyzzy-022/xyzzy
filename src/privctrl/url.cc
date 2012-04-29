#include "stdafx.h"
#include "privctlimpl.h"

static void
dopaint (HWND hwnd, HDC hdc)
{
  HFONT hfont;
  HBRUSH hbr;
  HWND parent = GetParent (hwnd);
  if (parent)
    {
      hfont = HFONT (SendMessage (parent, WM_GETFONT, 0, 0));
      hbr = HBRUSH (SendMessage (parent, WM_CTLCOLORSTATIC,
                                 WPARAM (hdc), LPARAM (hwnd)));
    }
  else
    {
      hfont = 0;
      hbr = 0;
    }

  char *s[3];
  int l = GetWindowTextLength (hwnd);
  s[0] = (char *)_alloca (l + 1);
  GetWindowText (hwnd, s[0], l + 1);
  s[1] = strchr (s[0], '\001');
  if (s[1])
    {
      *s[1]++ = 0;
      s[2] = strchr (s[1], '\002');
      if (s[2])
        *s[2]++ = 0;
    }

  RECT r;
  GetClientRect (hwnd, &r);
  if (hbr)
    {
      FillRect (hdc, &r, hbr);
      SetBkMode (hdc, TRANSPARENT);
    }
  else
    SetBkMode (hdc, OPAQUE);

  LONG range = 0;
  COLORREF bg = GetTextColor (hdc);
  COLORREF hl = GetSysColor (COLOR_HIGHLIGHT);
  HGDIOBJ of = hfont ? SelectObject (hdc, hfont) : 0;
  int xmax = r.right;
  for (int i = 0; i < 3; i++)
    {
      if (!s[i])
        break;
      SIZE sz;
      l = strlen (s[i]);
      SetTextColor (hdc, i == 1 ? hl : bg);
      GetTextExtentPoint32 (hdc, s[i], l, &sz);
      r.right = min (r.left + sz.cx, xmax);
      ExtTextOut (hdc, r.left, 0, ETO_CLIPPED, &r, s[i], l, 0);
      if (i == 1)
        {
          range = MAKELONG (r.left, r.right);
          if (GetCapture () == hwnd)
            {
              COLORREF obg = SetBkColor (hdc, hl);
              RECT u;
              u.left = r.left;
              u.right = r.right;
              u.bottom = min (sz.cy + 1, r.bottom);
              u.top = u.bottom - 1;
              ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &u, "", 0, 0);
              SetBkColor (hdc, obg);
            }
        }
      r.left = r.right;
    }

  if (of)
    SelectObject (hdc, of);
  SetWindowLong (hwnd, 0, range);
}

static void
invalidate_link (HWND hwnd, RECT &r)
{
  LONG t = GetWindowLong (hwnd, 0);
  r.left = short (LOWORD (t));
  r.right = short (HIWORD (t));
  InvalidateRect (hwnd, &r, 0);
}

static LPARAM CALLBACK
URLWndProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint (hwnd, &ps);
        dopaint (hwnd, hdc);
        EndPaint (hwnd, &ps);
        return 0;
      }

    case WM_NCHITTEST:
      {
        POINT p;
        p.x = short (LOWORD (lparam));
        p.y = short (HIWORD (lparam));
        ScreenToClient (hwnd, &p);
        LONG t = GetWindowLong (hwnd, 0);
        int left = short (LOWORD (t));
        int right = short (HIWORD (t));
        return p.x >= left && p.x < right ? HTCLIENT : HTTRANSPARENT;
      }

    case WM_CANCELMODE:
      if (GetCapture () == hwnd)
        {
          ReleaseCapture ();
          RECT r;
          GetClientRect (hwnd, &r);
          invalidate_link (hwnd, r);
        }
      break;

    case WM_MOUSEMOVE:
      {
        RECT r;
        GetClientRect (hwnd, &r);
        if (GetCapture () == hwnd)
          {
            POINT p;
            p.x = short (LOWORD (lparam));
            p.y = short (HIWORD (lparam));
            if (!PtInRect (&r, p))
              {
                ReleaseCapture ();
                invalidate_link (hwnd, r);
              }
          }
        else
          {
            SetCapture (hwnd);
            invalidate_link (hwnd, r);
          }
        return 0;
      }

    case WM_LBUTTONUP:
      PostMessage (GetParent (hwnd), WM_COMMAND,
                   MAKEWPARAM (GetWindowLong (hwnd, GWL_ID), URLN_CLICKED),
                   LPARAM (hwnd));
      return 0;

    case WM_GETDLGCODE:
      return DLGC_STATIC;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

static HCURSOR hcur_harrow;

int
init_url_class ()
{
  static const unsigned char cursor_bits[] =
    {
      0xf8,0x7f,0xff,0xff,0xfc,0xff,0xff,0xff,
      0xf8,0x7f,0xff,0xff,0xf8,0x7f,0xff,0xff,
      0xf8,0x7f,0xff,0xff,0xf8,0x1f,0xff,0xff,
      0xf8,0x03,0xff,0xff,0xf8,0x00,0xff,0xff,
      0xf8,0x00,0x7f,0xff,0x88,0x00,0x3f,0xff,
      0x80,0x00,0x3f,0xff,0x80,0x00,0x3f,0xff,
      0xc0,0x00,0x3f,0xff,0xe0,0x00,0x3f,0xff,
      0xe0,0x00,0x3f,0xff,0xf0,0x00,0x3f,0xff,
      0xf0,0x00,0x7f,0xff,0xf8,0x00,0x7f,0xff,
      0xf8,0x00,0x7f,0xff,0xfc,0x00,0xff,0xff,
      0xfc,0x00,0xff,0xff,0xfc,0x00,0xff,0xff,
      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
      0x00,0x00,0x00,0x00,0x03,0x00,0x00,0x00,
      0x03,0x00,0x00,0x00,0x03,0x00,0x00,0x00,
      0x03,0x00,0x00,0x00,0x03,0x00,0x00,0x00,
      0x03,0x60,0x00,0x00,0x03,0x6c,0x00,0x00,
      0x03,0x6d,0x00,0x00,0x03,0x6d,0x80,0x00,
      0x33,0xfd,0x80,0x00,0x3b,0xff,0x80,0x00,
      0x1b,0xff,0x80,0x00,0x0b,0xff,0x80,0x00,
      0x0f,0xff,0x80,0x00,0x07,0xff,0x80,0x00,
      0x07,0xff,0x00,0x00,0x03,0xff,0x00,0x00,
      0x03,0xff,0x00,0x00,0x01,0xfe,0x00,0x00,
      0x01,0xfe,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    };

  HBITMAP hbm = CreateBitmap (32, 64, 1, 1, cursor_bits);
  ICONINFO ii;
  ii.fIcon = 0;
  ii.xHotspot = 6;
  ii.yHotspot = 0;
  ii.hbmMask = hbm;
  ii.hbmColor = 0;
  hcur_harrow = HCURSOR (CreateIconIndirect (&ii));
  DeleteObject (hbm);

  WNDCLASS wc;
  wc.style = CS_HREDRAW	| CS_VREDRAW;
  wc.lpfnWndProc = URLWndProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof (LONG);
  wc.hInstance = hinstDLL;
  wc.hIcon = 0;
  wc.hCursor = hcur_harrow;
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = WC_URLCLASSA;
  return RegisterClass (&wc);
}

void
cleanup_url_class ()
{
  if (hcur_harrow)
    DestroyCursor (hcur_harrow);
}
