#ifndef _PRIVCTLIMPL_H_
# define _PRIVCTLIMPL_H_

# ifndef PRIVCTRL_NODLL
#  define PRIVCTRLAPI __declspec (dllexport) WINAPI
# else
#  define PRIVCTRLAPI WINAPI
# endif
# include "privctrl.h"

# ifndef EXTERN
#  define EXTERN extern
# endif

EXTERN HINSTANCE hinstDLL;

EXTERN ATOM hprop;
EXTERN ATOM hownerdraw;
#define ATOM2STR(a) LPSTR (a)

EXTERN int Win4p;

int subclass_parent (HWND);

int init_listview_class ();
int init_url_class ();
void cleanup_url_class ();

struct item_data
{
  DWORD style;
};

inline item_data *
get_item_data (HWND hwnd)
{
  return (item_data *)GetProp (hwnd, ATOM2STR (hprop));
}

inline BOOL
set_item_data (HWND hwnd, item_data *data)
{
  return SetProp (hwnd, ATOM2STR (hprop), HANDLE (data));
}

inline DWORD
get_ctl_style (HWND hwnd)
{
  return get_item_data (hwnd)->style;
}

inline DWORD
get_window_style (HWND hwnd)
{
  return GetWindowLong (hwnd, GWL_STYLE);
}

inline BOOL
set_window_style (HWND hwnd, DWORD style)
{
  return SetWindowLong (hwnd, GWL_STYLE, style);
}

typedef LONG (WINAPI *OWNERDRAWPROC)(UINT, DRAWITEMSTRUCT *);

inline OWNERDRAWPROC
get_owner_draw_proc (HWND hwnd)
{
  return OWNERDRAWPROC (GetProp (hwnd, ATOM2STR (hownerdraw)));
}

inline BOOL
set_owner_draw_proc (HWND hwnd, OWNERDRAWPROC proc)
{
  return SetProp (hwnd, ATOM2STR (hownerdraw), proc);
}

item_data *alloc_item_data (HWND, DWORD);

inline void
free_item_data (HWND hwnd)
{
  LocalFree (HLOCAL (RemoveProp (hwnd, ATOM2STR (hprop))));
}

#endif
