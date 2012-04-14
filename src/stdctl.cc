#include "stdafx.h"
#include "ed.h"

static const char csComboBox[] = "ComboBox";
static const char csEdit[] = "Edit";
static const char csListBox[] = "ListBox";

static WNDPROC org_lbx_wndproc;
static WNDPROC org_edt_wndproc;
static WNDPROC org_cbx_wndproc;

static inline LRESULT
lbx_sendmsg (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  return CallWindowProc (org_lbx_wndproc, hwnd, msg, wparam, lparam);
}

static inline LRESULT
cbx_sendmsg (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  return CallWindowProc (org_cbx_wndproc, hwnd, msg, wparam, lparam);
}

static inline LRESULT
edt_sendmsg (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  return CallWindowProc (org_edt_wndproc, hwnd, msg, wparam, lparam);
}

static int
stdctl_char_eq (int ch, lisp x)
{
  x = xsymbol_value (x);
  return charp (x) && ch == xchar_code (x);
}

int
stdctl_operation (int ch)
{
  if (stdctl_char_eq (ch, Vstd_control_up_char))
    return VK_UP;
  if (stdctl_char_eq (ch, Vstd_control_down_char))
    return VK_DOWN;
  if (stdctl_char_eq (ch, Vstd_control_default_char))
    return VK_RETURN;
  if (stdctl_char_eq (ch, Vstd_control_prior_char))
    return VK_PRIOR;
  if (stdctl_char_eq (ch, Vstd_control_next_char))
    return VK_NEXT;
  return -1;
}

static void
stdctl_default (HWND hwnd)
{
  HWND hwnd_parent = GetParent (hwnd);
  if (hwnd_parent)
    {
      DWORD id = SendMessage (hwnd_parent, DM_GETDEFID, 0, 0);
      if (HIWORD (id) == DC_HASDEFID)
        {
          HWND hwnd_btn = GetDlgItem (hwnd_parent, LOWORD (id));
          if (hwnd_btn)
            PostMessage (hwnd_parent, WM_COMMAND,
                         MAKEWPARAM (LOWORD (id), BN_CLICKED),
                         LPARAM (hwnd_btn));
        }
    }
}

static int
lbx_keydown (HWND hwnd, int vk)
{
  if (!(GetWindowLong (hwnd, GWL_STYLE) & LBS_EXTENDEDSEL)
      || (vk != VK_UP && vk != VK_DOWN && vk != VK_SPACE)
      || GetKeyState (VK_CONTROL) >= 0
      || GetKeyState (VK_SHIFT) < 0)
    return 0;

  int i = lbx_sendmsg (hwnd, LB_GETCARETINDEX, 0, 0);
  switch (vk)
    {
    case VK_UP:
      if (i > 0)
        lbx_sendmsg (hwnd, LB_SETCARETINDEX, i - 1, 0);
      break;

    case VK_DOWN:
      if (i < lbx_sendmsg (hwnd, LB_GETCOUNT, 0, 0) - 1)
        lbx_sendmsg (hwnd, LB_SETCARETINDEX, i + 1, 0);
      break;

    default:
      lbx_sendmsg (hwnd, LB_SETSEL, !lbx_sendmsg (hwnd, LB_GETSEL, i, 0), i);
      break;
    }
  return 1;
}

static int
lbx_char (HWND hwnd, int ch)
{
  int op = stdctl_operation (ch);
  if (op < 0)
    {
      HWND hwnd_parent = GetParent (hwnd);
      if (!hwnd_parent)
        return 0;
      PostMessage (hwnd_parent, WM_PRIVATE_LISTBOX_CHAR,
                   GetWindowLong (hwnd, GWL_ID), ch);
      return 1;
    }

  if (op == VK_RETURN)
    stdctl_default (hwnd);
  else
    {
      lbx_sendmsg (hwnd, WM_KEYDOWN, op, 0);
      lbx_sendmsg (hwnd, WM_KEYUP, op, 0);
    }
  return 1;
}

static LRESULT CALLBACK
lbx_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_KEYDOWN:
      if (lbx_keydown (hwnd, wparam))
        return 0;
      break;

    case WM_CHAR:
      if (lbx_char (hwnd, wparam))
        return 0;
      break;
    }
  return lbx_sendmsg (hwnd, msg, wparam, lparam);
}

static int
cbx_char (HWND hwnd, int ch)
{
  int op = stdctl_operation (ch);
  if (op < 0)
    return 0;
  if (op == VK_RETURN)
    stdctl_default (hwnd);
  else
    {
      cbx_sendmsg (hwnd, WM_KEYDOWN, op, 0);
      cbx_sendmsg (hwnd, WM_KEYUP, op, 0);
    }
  return 1;
}

static LRESULT CALLBACK
cbx_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  if (msg == WM_CHAR && cbx_char (hwnd, wparam))
    return 0;
  return cbx_sendmsg (hwnd, msg, wparam, lparam);
}

static int
edt_char (HWND hwnd, int ch)
{
  int op = stdctl_operation (ch);
  if (op < 0)
    return 0;

  char class_name[16];
  HWND hwnd_parent = GetParent (hwnd);
  if (!hwnd_parent
      || !GetClassName (hwnd_parent, class_name, sizeof class_name)
      || _stricmp (class_name, csComboBox))
    return 0;

  if (op == VK_RETURN)
    stdctl_default (hwnd_parent);
  else
    {
      edt_sendmsg (hwnd, WM_KEYDOWN, op, 0);
      edt_sendmsg (hwnd, WM_KEYUP, op, 0);
    }
  return 1;
}

static LRESULT CALLBACK
edt_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  if (msg == WM_CHAR && edt_char (hwnd, wparam))
    return 0;
  return CallWindowProc (org_edt_wndproc, hwnd, msg, wparam, lparam);
}

static int
init_hook (HINSTANCE hinst, const char *class_name,
           WNDPROC wndproc, WNDPROC &org_wndproc)
{
  WNDCLASS wc;
  if (!GetClassInfo (0, class_name, &wc))
    return 0;

  org_wndproc = wc.lpfnWndProc;
  wc.lpfnWndProc = wndproc;
  wc.hInstance = hinst;
  return RegisterClass (&wc);
}

void
stdctl_hook_init (HINSTANCE hinst)
{
  init_hook (hinst, csComboBox, cbx_wndproc, org_cbx_wndproc);
  init_hook (hinst, csEdit, edt_wndproc, org_edt_wndproc);
  init_hook (hinst, csListBox, lbx_wndproc, org_lbx_wndproc);
}
