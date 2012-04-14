#include "stdafx.h"
#include "ed.h"
#include "fnkey.h"

#define SHIFT_OFFSET MAX_Fn
#define CTRL_OFFSET (MAX_Fn * 2)
#define META_OFFSET (MAX_Fn * 4)

const FKWin::divinfo FKWin::fk_divinfo[] =
{
  {4, 1},
  {5, 1},
  {8, 2},
  {10, 2},
  {12, 3},
  {15, 3},
};

int FKWin::fk_default_nbuttons;

static inline void
set_window (HWND hwnd, FKWin *wp)
{
  SetWindowLong (hwnd, 0, LONG (wp));
}

static inline FKWin *
get_window (HWND hwnd)
{
  return (FKWin *)GetWindowLong (hwnd, 0);
}

FKWin::FKWin ()
     : fk_hwnd (0), fk_nbuttons (fk_default_nbuttons),
       fk_cur_btn (-1), fk_cur_on (-1), fk_vkey (0)
{
  int i;
  for (i = 0; i < numberof (fk_divinfo); i++)
    if (fk_nbuttons == fk_divinfo[i].nbuttons)
      break;
  if (i == numberof (fk_divinfo))
    fk_nbuttons = 12;

  TEXTMETRIC tm;
  HDC hdc = GetDC (0);
  HGDIOBJ ofont = SelectObject (hdc, sysdep.ui_font ());
  GetTextMetrics (hdc, &tm);
  fk_height = tm.tmHeight + 10;
  SelectObject (hdc, ofont);
  ReleaseDC (0, hdc);

  update_vkey (1);
}

void
FKWin::get_button_rect (int n, RECT &r) const
{
  r.left = fk_offset[n];
  r.right = r.left + fk_btn.cx;
  r.top = 2;
  r.bottom = r.top + fk_btn.cy;
}

void
FKWin::paint_text (HDC hdc, int n, const RECT &br, int offset) const
{
  RECT r;
  r.left = br.left + 1 + offset;
  r.right = br.right - 2 + offset;
  r.top = br.top + 1 + offset;
  r.bottom = br.bottom - 2 + offset;

  int ofg = SetTextColor (hdc, sysdep.btn_text);
  int obg = SetBkColor (hdc, sysdep.btn_face);
  HGDIOBJ of = SelectObject (hdc, sysdep.ui_font ());

  if (fk_vkey & FVK_META)
    n += META_OFFSET;
  if (fk_vkey & FVK_SHIFT)
    n += SHIFT_OFFSET;
  if (fk_vkey & FVK_CONTROL)
    n += CTRL_OFFSET;

  char buf[1024 + 1];
  lisp label = xvector_contents (xsymbol_value (Vfunction_bar_labels))[n];
  if (stringp (label))
    w2s (buf, xstring_contents (label), min (xstring_length (label), 512));
  else
    *buf = 0;

  ExtTextOut (hdc, r.left + 2, r.top + 2, ETO_CLIPPED | ETO_OPAQUE,
              &r, buf, strlen (buf), 0);

  SetBkColor (hdc, obg);
  SetTextColor (hdc, ofg);
  SelectObject (hdc, of);
}

void
FKWin::paint_off (HDC hdc, int n, const RECT &r) const
{
  paint_button_off (hdc, r);
  paint_text (hdc, n, r, 0);
}

void
FKWin::paint_on (HDC hdc, int n, const RECT &r) const
{
  paint_button_on (hdc, r);
  paint_text (hdc, n, r, 1);
}

void
FKWin::paint_buttons (HDC hdc) const
{
  draw_hline (hdc, 0, fk_sz.cx, 0, sysdep.btn_highlight);
  draw_hline (hdc, 0, fk_sz.cx, fk_sz.cy - 1, sysdep.btn_shadow);

  for (int i = 0; i < fk_nbuttons; i++)
    {
      RECT r;
      get_button_rect (i, r);
      if (i == fk_cur_on)
        paint_on (hdc, i, r);
      else
        paint_off (hdc, i, r);
    }
}

void
FKWin::refresh_button (int n) const
{
  HDC hdc = GetDC (fk_hwnd);
  RECT r;
  get_button_rect (n, r);
  if (n == fk_cur_on)
    paint_on (hdc, n, r);
  else
    paint_off (hdc, n, r);
  ReleaseDC (fk_hwnd, hdc);
}

static void
inc (int *b, int i, int e, int d)
{
  for (; i < e; i++)
    b[i] += d;
}

void
FKWin::OnSize (int cx, int cy)
{
  fk_sz.cx = cx;
  fk_sz.cy = cy;

  int i;
  for (i = 0; i < numberof (fk_divinfo); i++)
    if (fk_nbuttons == fk_divinfo[i].nbuttons)
      break;
  int ndiv = fk_divinfo[i].ndiv - 1;
#define SPC 1

  fk_btn.cx = (cx - 2 - ndiv * 3) / fk_nbuttons - SPC;
  fk_btn.cy = cy - 4;

  fk_offset[0] = 1;
  for (i = 1; i < fk_nbuttons; i++)
    fk_offset[i] = 1 + (fk_btn.cx + SPC) * i;
  int rest = fk_sz.cx - (fk_btn.cx + SPC) * fk_nbuttons - SPC;

  if (ndiv > 0)
    {
      int d = rest / ndiv;
      rest -= d * ndiv;
      for (i = 1; i <= ndiv; i++)
        inc (fk_offset, i * fk_nbuttons / (ndiv + 1), fk_nbuttons, d);
    }

  while (rest > 0)
    for (i = 1; i < fk_nbuttons && rest > 0; i++, rest--)
      inc (fk_offset, i, fk_nbuttons, 1);
}

void
FKWin::OnPaint ()
{
  PAINTSTRUCT ps;
  HDC hdc = BeginPaint (fk_hwnd, &ps);
  paint_buttons (hdc);
  EndPaint (fk_hwnd, &ps);
}

void
FKWin::button_on (int n)
{
  if (fk_cur_on != n)
    {
      if (fk_cur_on != -1)
        {
          int o = fk_cur_on;
          fk_cur_on = n;
          refresh_button (o);
        }
      else
        fk_cur_on = n;
      if (n >= 0)
        refresh_button (n);
    }
}

void
FKWin::OnLButtonDown (int x, int y, int keys)
{
  POINT pt;
  pt.x = x;
  pt.y = y;
  for (int i = 0; i < fk_nbuttons; i++)
    {
      RECT r;
      get_button_rect (i, r);
      if (PtInRect (&r, pt))
        {
          fk_cur_rect = r;
          fk_cur_btn = i;
          button_on (i);
          SetCapture (fk_hwnd);
          return;
        }
    }
}

void
FKWin::OnLButtonUp (int x, int y, int keys)
{
  if (GetCapture () == fk_hwnd)
    ReleaseCapture ();

  if (fk_cur_btn == -1)
    return;

  POINT pt;
  pt.x = x;
  pt.y = y;

  Char cc = CCF_F1 + fk_cur_btn;

  fk_cur_btn = -1;
  button_on (-1);

  if (!PtInRect (&fk_cur_rect, pt))
    return;

  if (fk_vkey & FVK_META)
    cc = function_to_meta_function (cc);
  if (fk_vkey & FVK_SHIFT)
    cc |= CCF_SHIFT_BIT;
  if (fk_vkey & FVK_CONTROL)
    cc |= CCF_CTRL_BIT;

  app.kbdq.putc (cc);
}

void
FKWin::OnMouseMove (int x, int y, int keys)
{
  if (fk_cur_btn == -1)
    return;

  POINT pt;
  pt.x = x;
  pt.y = y;

  if (PtInRect (&fk_cur_rect, pt))
    button_on (fk_cur_btn);
  else
    button_on (-1);
}

void
FKWin::OnKillFocus ()
{
  if (GetCapture () == fk_hwnd)
    ReleaseCapture ();
  fk_cur_btn = -1;
  button_on (-1);
}

void
FKWin::OnCancelMode ()
{
  OnKillFocus ();
}

int
FKWin::vk2fvk (int vk) const
{
  switch (vk)
    {
    case VK_SHIFT:
      return FVK_SHIFT;
    case VK_CONTROL:
      return FVK_CONTROL;
    case VK_MENU:
      return FVK_META;
    default:
      return 0;
    }
}

void
FKWin::update_vkey (int clear)
{
  int ok = fk_vkey;
  fk_vkey = 0;
  if (!clear)
    {
      if (GetAsyncKeyState (VK_SHIFT) < 0)
        fk_vkey |= vk2fvk (VK_SHIFT);
      if (GetAsyncKeyState (VK_CONTROL) < 0)
        fk_vkey |= vk2fvk (VK_CONTROL);
      if (GetAsyncKeyState (VK_MENU) < 0)
        fk_vkey |= vk2fvk (VK_MENU);
    }
  if (fk_hwnd && ok != fk_vkey)
    InvalidateRect (fk_hwnd, 0, 0);
}

void
FKWin::set_vkey (int vk)
{
  int fvk = vk2fvk (vk);
  if (fvk && !(fk_vkey & fvk))
    {
      fk_vkey |= fvk;
      InvalidateRect (fk_hwnd, 0, 0);
    }
}

void
FKWin::unset_vkey (int vk)
{
  int fvk = vk2fvk (vk);
  if (fk_vkey & fvk && GetKeyState (vk) >= 0)
    {
      fk_vkey &= ~fvk;
      InvalidateRect (fk_hwnd, 0, 0);
    }
}

int
FKWin::set_nbuttons (int n)
{
  if (n == fk_nbuttons)
    return 1;
  for (int i = 0; i < numberof (fk_divinfo); i++)
    if (n == fk_divinfo[i].nbuttons)
      {
        fk_nbuttons = n;
        fk_default_nbuttons = n;
        OnSize (fk_sz.cx, fk_sz.cy);
        InvalidateRect (fk_hwnd, 0, 1);
        return 1;
      }
  return 0;
}

LRESULT CALLBACK
fnkey_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_CREATE:
      {
        CREATESTRUCT *cs = (CREATESTRUCT *)lparam;
        FKWin *fk = (FKWin *)cs->lpCreateParams;
        set_window (hwnd, fk);
        fk->set_hwnd (hwnd);
        return 0;
      }

    case WM_NCDESTROY:
      delete get_window (hwnd);
      break;

    case WM_SIZE:
      get_window (hwnd)->OnSize (LOWORD (lparam), HIWORD (lparam));
      return 0;

    case WM_PAINT:
      get_window (hwnd)->OnPaint ();
      return 0;

    case WM_LBUTTONDOWN:
      get_window (hwnd)->OnLButtonDown (short (LOWORD (lparam)),
                                        short (HIWORD (lparam)),
                                        wparam);
      return 0;

    case WM_LBUTTONUP:
      get_window (hwnd)->OnLButtonUp (short (LOWORD (lparam)),
                                      short (HIWORD (lparam)),
                                      wparam);
      return 0;

    case WM_MOUSEMOVE:
      get_window (hwnd)->OnMouseMove (short (LOWORD (lparam)),
                                      short (HIWORD (lparam)),
                                      wparam);
      return 0;

    case WM_KILLFOCUS:
      get_window (hwnd)->OnKillFocus ();
      return 0;

    case WM_CANCELMODE:
      get_window (hwnd)->OnCancelMode ();
      break;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

lisp
Fset_function_bar_label (lisp lcc, lisp label)
{
  check_char (lcc);
  if (label != Qnil)
    check_string (label);

  Char cc = xchar_code (lcc);
  int meta = 0;
  if (meta_function_char_p (cc))
    {
      meta = 1;
      cc = meta_function_to_function (cc);
    }
  if (!function_char_p (cc))
    FEprogram_error (Ewrong_function_bar_char, lcc);

  int shift = cc & CCF_SHIFT_BIT;
  int ctl = cc & CCF_CTRL_BIT;
  cc &= ~(CCF_SHIFT_BIT | CCF_CTRL_BIT);
  if (cc < CCF_F1 || cc > CCF_Fn_MAX)
    FEprogram_error (Ewrong_function_bar_char, lcc);

  int index = cc - CCF_F1;
  int n = index;
  if (shift)
    index += SHIFT_OFFSET;
  if (ctl)
    index += CTRL_OFFSET;
  if (meta)
    index += META_OFFSET;

  xvector_contents (xsymbol_value (Vfunction_bar_labels))[index] = label;

  app.active_frame.fnkey->refresh_button (n);

  return Qt;
}

lisp
Fset_number_of_function_bar_labels (lisp n)
{
  if (!app.active_frame.fnkey->set_nbuttons (fixnum_value (n)))
    FEprogram_error (Ebad_function_bar_label_number, n);
  return Qt;
}

lisp
Fnumber_of_function_bar_labels ()
{
  return make_fixnum (app.active_frame.fnkey->get_nbuttons ());
}
