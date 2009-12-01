#ifndef _ctxmenu_h_
#define _ctxmenu_h_

#if _MSC_VER < 1100 && !defined (HAVE_ICONTEXTMENU2)

DEFINE_SHLGUID (IID_IContextMenu2, 0x000214f4l, 0, 0);

#undef INTERFACE
#define INTERFACE IContextMenu2

DECLARE_INTERFACE_ (IContextMenu2, IContextMenu)
{
  STDMETHOD(QueryInterface) (THIS_ REFIID riid, LPVOID * ppvObj) PURE;
  STDMETHOD_(ULONG,AddRef) (THIS) PURE;
  STDMETHOD_(ULONG,Release) (THIS) PURE;

  STDMETHOD(QueryContextMenu)(THIS_
                              HMENU hmenu,
                              UINT indexMenu,
                              UINT idCmdFirst,
                              UINT idCmdLast,
                              UINT uFlags) PURE;

  STDMETHOD(InvokeCommand)(THIS_
                           LPCMINVOKECOMMANDINFO lpici) PURE;

  STDMETHOD(GetCommandString)(THIS_
                              UINT idCmd,
                              UINT uType,
                              UINT *pwReserved,
                              LPSTR pszName,
                              UINT cchMax) PURE;

  STDMETHOD(HandleMenuMsg)(THIS_
                           UINT uMsg,
                           WPARAM wParam,
                           LPARAM lParam) PURE;
};

typedef IContextMenu2 *LPCONTEXTMENU2;

#endif /* _MSC_VER < 1100 && !HAVE_ICONTEXTMENU2 */

#endif /* not _ctxmenu_h_ */
