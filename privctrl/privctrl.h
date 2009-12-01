#ifndef _PRIVCTRL_H_
# define _PRIVCTRL_H_

# ifdef __cplusplus
extern "C" {
# endif

# ifndef PRIVCTRLAPI
#  ifndef PRIVCTRL_NODLL
#   define PRIVCTRLAPI DECLSPEC_IMPORT WINAPI
#  else
#   define PRIVCTRLAPI WINAPI
#  endif
# endif

# define WC_URLCLASSA "URLLink"
# define WC_URLCLASSW L"URLLink"

# define URLN_CLICKED 0

# define WC_LISTVIEWEX "SysListViewEx32"

# define LVN_PROCESSKEY (LVN_LAST + 1)
typedef struct _LV_PROCESSKEY
{
  NMHDR hdr;
  UINT message;
  WPARAM wparam;
  LPARAM lparam;
}
  LV_PROCESSKEY;

# define LVS_TYPEMASKEX 7
# define LVS_EXREPORT 4
# define LVS_EXREPORTEX 5
# define LVS_EXTENDKBD 8
# define LVS_PROCESSKEY 0x10

# define LVM_SETEXSTYLE (LVM_FIRST + 800)
# define ListView_SetExStyle(hwnd, style) \
  SendMessage ((hwnd), LVM_SETEXSTYLE, (WPARAM)style, 0)

# define LVM_GETEXSTYLE (LVM_FIRST + 801)
# define ListView_GetExStyle(hwnd) \
  SendMessage ((hwnd), LVM_GETEXSTYLE, 0, 0)

# define LVM_ISEARCH (LVM_FIRST + 802)
# define ListView_ISearch(hwnd, c, wrap) \
  SendMessage ((hwnd), LVM_ISEARCH, (c), wrap)

# define LVM_FORWARDLINE (LVM_FIRST + 803)
# define ListView_ForwardLine(hwnd, n) \
  SendMessage ((hwnd), LVM_FORWARDLINE, (n), 0)

# define LVM_FORWARDPAGE (LVM_FIRST + 804)
# define ListView_ForwardPage(hwnd, n) \
  SendMessage ((hwnd), LVM_FORWARDPAGE, (n), 0)

# define LVM_SETENABLED (LVM_FIRST + 805)
# define ListView_SetEnabled(hwnd, n) \
  SendMessage ((hwnd), LVM_SETENABLED, (n), 0)

# define LVM_GETENABLED (LVM_FIRST + 806)
# define ListView_GetEnabled(hwnd) \
  SendMessage ((hwnd), LVM_GETENABLED, 0, 0)

# define LVM_SETSUBIMAGELIST (LVM_FIRST + 807)
# define ListView_SetSubImageList(hwnd, himl, iImageList) \
  (HIMAGELIST)(UINT)SendMessage ((hwnd), LVM_SETSUBIMAGELIST, \
                                 (WPARAM)(iImageList), \
                                 (LPARAM)(UINT)(HIMAGELIST)(himl))

# define LVSM_DOWN 0
# define LVSM_UP 1

# define LVM_SETSORTMARK (LVM_FIRST + 808)
# define ListView_SetSortMark(hwnd, index, dir) \
  SendMessage ((hwnd), LVM_SETSORTMARK, (index), (dir))

# define LVM_GOTOBOF (LVM_FIRST + 809)
# define ListView_GotoBOF(hwnd) \
  SendMessage ((hwnd), LVM_GOTOBOF, 0, 0)

# define LVM_GOTOEOF (LVM_FIRST + 810)
# define ListView_GotoEOF(hwnd) \
  SendMessage ((hwnd), LVM_GOTOEOF, 0, 0)

# define LVM_SETCURSORCOLOR (LVM_FIRST + 811)
# define ListView_SetCursorColor(hwnd, cc) \
  SendMessage ((hwnd), LVM_SETCURSORCOLOR, 0, (LPARAM)(COLORREF)(cc))

# define LVMC_UP 0
# define LVMC_DOWN 1
# define LVMC_DEF 2
# define LVMC_PAGEUP 3
# define LVMC_PAGEDOWN 4

# define LVM_SETMOVERCHAR (LVM_FIRST + 812)
# define ListView_SetMoverChar(hwnd, type, c) \
  SendMessage ((hwnd), LVM_SETMOVERCHAR, (type), (c))

# define LVM_SETHIGHLIGHTTEXTCOLOR (LVM_FIRST + 813)
# define ListView_SetHighlightTextColor(hwnd, cc) \
  SendMessage ((hwnd), LVM_SETHIGHLIGHTTEXTCOLOR, 0, (LPARAM)(COLORREF)(cc))

# define LVM_SETHIGHLIGHTCOLOR (LVM_FIRST + 814)
# define ListView_SetHighlightColor(hwnd, cc) \
  SendMessage ((hwnd), LVM_SETHIGHLIGHTCOLOR, 0, (LPARAM)(COLORREF)(cc))

# define LVM_SETPATHELLIPSE (LVM_FIRST + 815)
# define ListView_SetPathEllipse(hwnd, index, f) \
  SendMessage ((hwnd), LVM_SETPATHELLIPSE, (index), (f))

# ifndef PRIVCTRL_NODLL
void PRIVCTRLAPI InitPrivateControls ();
# else
void PRIVCTRLAPI InitPrivateControls (HINSTANCE);
# endif

# ifdef __cplusplus
}
# endif

#endif
