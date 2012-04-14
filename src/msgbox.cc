#include "stdafx.h"
#include "ed.h"
#include "msgbox.h"
#include "monitor.h"

void
XMessageBox::add_button (UINT id, const char *caption)
{
  if (nbuttons < MAX_BUTTONS)
    {
      btn[nbuttons].id = id;
      btn[nbuttons].caption = caption;
      nbuttons++;
    }
}

void
XMessageBox::set_button (int n, UINT id, const char *caption)
{
  if (n < nbuttons)
    btn[n].caption = caption;
  else
    add_button (id, caption);
}

void
XMessageBox::calc_text_rect (RECT &r) const
{
  HDC hdc = GetDC (hwnd);
  HGDIOBJ of = SelectObject (hdc, hfont);
  memset (&r, 0, sizeof r);
  r.right = GetSystemMetrics (SM_CXSCREEN) * 3 / 4;
  DrawText (hdc, msg, -1, &r,
            (DT_CALCRECT | DT_EXPANDTABS | DT_LEFT | DT_NOPREFIX
             | (f_no_wrap ? 0 : DT_WORDBREAK)));
  SelectObject (hdc, of);
  ReleaseDC (hwnd, hdc);
}

void
XMessageBox::calc_button_size (RECT br[MAX_BUTTONS]) const
{
  RECT r;
  r.left = r.top = 0;
  r.right = 35;
  r.bottom = 14;
  MapDialogRect (hwnd, &r);
  int sep = r.right / 7;

  HDC hdc = GetDC (hwnd);
  HGDIOBJ of = SelectObject (hdc, hfont);
  for (int i = 0; i < nbuttons; i++)
    {
      RECT tr;
      memset (&tr, 0, sizeof tr);
      DrawText (hdc, btn[i].caption, -1, &tr, DT_CALCRECT | DT_SINGLELINE);
      r.right = max (r.right, LONG (tr.right + 2 * sep));
    }
  SelectObject (hdc, of);
  ReleaseDC (hwnd, hdc);

  for (int i = 0; i < nbuttons; i++)
    {
      br[i].top = 0;
      br[i].bottom = r.bottom;
      br[i].left = i * (r.right + sep);
      br[i].right = br[i].left + r.right;
    }
}

HWND
XMessageBox::create_ctl (const char *cls, const char *caption, DWORD style,
                         UINT id, const RECT &r) const
{
  HWND c = CreateWindow (cls, caption, style,
                         r.left, r.top, r.right - r.left, r.bottom - r.top,
                         hwnd, HMENU (id), hinst, 0);
  SendMessage (c, WM_SETFONT, WPARAM (hfont), 0);
  return c;
}

inline void
XMessageBox::create_btn (const char *caption, UINT id, const RECT &r) const
{
  create_ctl ("Button", caption,
              WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
              id, r);
}

inline void
XMessageBox::create_label (const char *caption, const RECT &r, int edit_style) const
{
  if (edit_style)
    create_ctl ("Edit", caption,
                (WS_CHILD | WS_VISIBLE
                 | ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL
                 | ES_READONLY | edit_style),
                UINT (-1), r);
  else
    create_ctl ("Static", caption,
                WS_CHILD | WS_VISIBLE | WS_GROUP | SS_LEFT | SS_NOPREFIX,
                UINT (-1), r);
}

inline void
XMessageBox::create_icon (const RECT &r) const
{
  HWND c = create_ctl ("Static", "", WS_CHILD | WS_VISIBLE | WS_GROUP | SS_ICON,
                       UINT (-1), r);
  SendMessage (c, STM_SETICON, WPARAM (hicon), 0);
}

void
XMessageBox::create_buttons (const RECT br[MAX_BUTTONS]) const
{
  for (int i = 0; i < nbuttons; i++)
    create_btn (btn[i].caption, btn[i].id, br[i]);
}

BOOL
XMessageBox::init_dialog ()
{
  SendMessage (hwnd, WM_SETICON, 1,
               LPARAM (LoadIcon (app.hinst, MAKEINTRESOURCE (IDI_XYZZY))));

  hfont = HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0));

  RECT tr, br[MAX_BUTTONS], ir;
  ir.left = XOFF;
  ir.top = YOFF;
  if (hicon)
    {
      ir.right = XOFF + 32 + XOFF;
      ir.bottom = YOFF + 32;
    }
  else
    {
      ir.right = XOFF;
      ir.bottom = YOFF;
    }

  RECT warea;
  monitor.get_workarea_from_window (app.toplev, &warea);

  LONG maxw = (warea.right - warea.left) * 4 / 5;
  LONG maxh = (warea.bottom - warea.top) * 3 / 4;

  calc_text_rect (tr);

  int edit_style = 0;
  if ((tr.right >= maxw || tr.bottom >= maxh)
      && (f_crlf || !strchr (msg, '\n')))
    {
      if (tr.right >= maxw)
        {
          tr.right = maxw;
          edit_style |= WS_HSCROLL;
        }
      else
        tr.right += sysdep.vscroll + sysdep.border.cx * 2;
      if (tr.bottom >= maxh)
        {
          tr.bottom = maxh;
          edit_style |= WS_VSCROLL;
        }
      else
        tr.bottom += sysdep.hscroll + sysdep.border.cy * 2;
    }

  calc_button_size (br);

  tr.left += ir.right;
  tr.right += ir.right;
  tr.top += YOFF;
  tr.bottom += YOFF;
  int y;
  if (tr.bottom > ir.bottom)
    y = tr.bottom;
  else
    {
#if 0
      int d = ((ir.bottom + ir.top) - (tr.bottom - tr.top)) / 2 - tr.top;
#else
      int d = (ir.bottom + ir.top - tr.bottom - tr.top) / 2;
#endif
      tr.top += d;
      tr.bottom += d;
      y = ir.bottom;
    }
  y += YOFF;
  for (int i = 0; i < nbuttons; i++)
    {
      br[i].top += y;
      br[i].bottom += y;
    }

  RECT r;
  r.left = 0;
  r.top = 0;
  r.right = tr.right + XOFF;
  r.bottom = y;
  if (nbuttons)
    {
      r.bottom = br[0].bottom + YOFF;
      int w = br[nbuttons - 1].right + 2 * XOFF;
      int d;
      if (r.right > w)
        d = (r.right - (w - 2 * XOFF)) / 2;
      else
        {
          r.right = w;
          d = XOFF;
        }
      for (int i = 0; i < nbuttons; i++)
        {
          br[i].left += d;
          br[i].right += d;
        }
    }

  if (hicon)
    create_icon (ir);
  create_label (msg, tr, edit_style);
  create_buttons (br);

  AdjustWindowRectEx (&r, GetWindowLong (hwnd, GWL_STYLE), 0,
                      GetWindowLong (hwnd, GWL_EXSTYLE));

  SIZE sz;
  sz.cx = r.right - r.left;
  sz.cy = r.bottom - r.top;

  SetWindowPos (hwnd, 0,
                (warea.left + warea.right - sz.cx) / 2,
                (warea.top + warea.bottom - sz.cy) / 2 - r.top,
                sz.cx, sz.cy, SWP_NOZORDER | SWP_NOACTIVATE);
  SetWindowText (hwnd, title);

  if (close_id < 0)
    DeleteMenu (GetSystemMenu (hwnd, 0), SC_CLOSE, MF_BYCOMMAND);

  SetFocus (GetDlgItem (hwnd, btn[default_btn].id));
  SendMessage (hwnd, DM_SETDEFID, btn[default_btn].id, 0);
  return 0;
}

BOOL
XMessageBox::WndProc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      return init_dialog ();

    case WM_COMMAND:
      {
        UINT id = LOWORD (wparam);
        for (int i = 0; i < nbuttons; i++)
          if (id == btn[i].id)
            {
              EndDialog (hwnd, id);
              return 1;
            }
        if (id == IDCANCEL && close_id >= 0)
          EndDialog (hwnd, close_id);
        return 1;
      }

    case WM_CLOSE:
      if (close_id >= 0)
        EndDialog (hwnd, close_id);
      return 1;
    }
  return 0;
}

BOOL CALLBACK
XMessageBox::WndProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  XMessageBox *p;
  if (msg == WM_INITDIALOG)
    {
      SetWindowLong (hwnd, DWL_USER, lparam);
      p = (XMessageBox *)lparam;
      p->hwnd = hwnd;
      p->WndProc (msg, wparam, lparam);
    }
  else
    {
      p = (XMessageBox *)GetWindowLong (hwnd, DWL_USER);
      if (p)
        p->WndProc (msg, wparam, lparam);
    }
  return 0;
}

int
XMessageBox::doit (HWND hwnd)
{
  if (!nbuttons)
    add_button (IDBUTTON1, "OK");
  if (default_btn < 0 || default_btn >= nbuttons)
    default_btn = 0;
  if (nbuttons == 1 && close_id == -1)
    close_id = btn[0].id;
  return DialogBoxParam (hinst, MAKEINTRESOURCE (IDD_MSGBOX),
                         hwnd, WndProc, LPARAM (this));
}

int
MsgBoxEx (HWND hwnd, const char *msg, const char *title,
          int type, int defbtn, int icon, int beep,
          const char **captions, int ncaptions, int crlf, int no_wrap)
{
  XMessageBox mb (app.hinst, msg ? msg : "", title ? title : "エラー", crlf, no_wrap);
  if (!captions)
    ncaptions = 0;

  switch (type)
    {
    default:
      break;

    case MB_OK:
      mb.add_button (IDOK, "OK");
      mb.set_close (IDOK);
      break;

    case MB_OKCANCEL:
      mb.add_button (IDOK, "OK");
      mb.add_button (IDCANCEL, "キャンセル");
      mb.set_close (IDCANCEL);
      break;

    case MB_ABORTRETRYIGNORE:
      mb.add_button (IDABORT, "中止(&A)");
      mb.add_button (IDRETRY, "再試行(&R)");
      mb.add_button (IDIGNORE, "無視(&I)");
      break;

    case MB_YESNOCANCEL:
      mb.add_button (IDYES, "はい(&Y)");
      mb.add_button (IDNO, "いいえ(&N)");
      mb.add_button (IDCANCEL, "キャンセル");
      mb.set_close (IDCANCEL);
      break;

    case MB_YESNO:
      mb.add_button (IDYES, "はい(&Y)");
      mb.add_button (IDNO, "いいえ(&N)");
      break;

    case MB_RETRYCANCEL:
      mb.add_button (IDRETRY, "再試行(&R)");
      mb.add_button (IDCANCEL, "キャンセル");
      mb.set_close (IDCANCEL);
      break;
    }

  ncaptions = min (ncaptions, XMessageBox::MAX_BUTTONS);
  for (int i = 0; i < ncaptions; i++)
    if (captions[i])
      mb.set_button (i, XMessageBox::IDBUTTON1 + i, captions[i]);

  mb.set_default (defbtn);

  switch (icon)
    {
    case MB_ICONHAND:
      mb.set_icon (LoadIcon (0, IDI_HAND));
      break;

    case MB_ICONQUESTION:
      mb.set_icon (LoadIcon (0, IDI_QUESTION));
      break;

    case MB_ICONEXCLAMATION:
      mb.set_icon (LoadIcon (0, IDI_EXCLAMATION));
      break;

    case MB_ICONASTERISK:
      mb.set_icon (LoadIcon (0, IDI_ASTERISK));
      break;

    default:
      icon = MB_OK;
      break;
    }

  if (beep && xsymbol_value (Vvisible_bell) == Qnil)
    ding (icon);

  return mb.doit (hwnd);
}

int
MsgBox (HWND hwnd, const char *msg, const char *title, UINT flags, int beep)
{
  int defbtn;
  switch (flags & MB_DEFMASK)
    {
    default:
      defbtn = 0;
      break;
    case MB_DEFBUTTON2:
      defbtn = 1;
      break;
    case MB_DEFBUTTON3:
      defbtn = 2;
      break;
    }

  return MsgBoxEx (hwnd, msg, title, flags & MB_TYPEMASK,
                   defbtn, flags & MB_ICONMASK, beep, 0, 0, 0, 0);
}
