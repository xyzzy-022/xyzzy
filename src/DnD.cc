#include "stdafx.h"
#include "ed.h"
#include "ctxmenu.h"
#include "com.h"
#include "filer.h"
#include "buffer-bar.h"

HRESULT
ole_object::QueryInterface (IUnknown *self, REFIID siid, REFIID reqiid, void **v)
{
  if (!v)
    return E_INVALIDARG;
  *v = 0;
  if (reqiid == IID_IUnknown || reqiid == siid)
    *v = self;
  if (*v)
    {
      self->AddRef ();
      return S_OK;
    }
  /* マニュアル間違ってんじゃん。誰だ、
     S_OK if the interface is supported, S_FALSE if not.
     なんて書いたのは? */
  return E_NOINTERFACE;
}

STDMETHODIMP
ole_drop_source::QueryInterface (REFIID iid, void **v)
{
  return ods_obj.QueryInterface (this, IID_IDropSource, iid, v);
}

STDMETHODIMP_ (ULONG)
ole_drop_source::AddRef ()
{
  return ods_obj.AddRef ();
}

STDMETHODIMP_ (ULONG)
ole_drop_source::Release ()
{
  return ods_obj.Release ();
}

STDMETHODIMP
ole_drop_source::QueryContinueDrag (BOOL esc, DWORD key)
{
  if (esc)
    return DRAGDROP_S_CANCEL;

  if (!ods_buttons)
    ods_buttons = key & (MK_LBUTTON | MK_MBUTTON | MK_RBUTTON);

  if (!(key & ods_buttons))
    return DRAGDROP_S_DROP;

  return S_OK;
}

STDMETHODIMP
ole_drop_source::GiveFeedback (DWORD)
{
  return DRAGDROP_S_USEDEFAULTCURSORS;
}

STDMETHODIMP
ole_drop_target::QueryInterface (REFIID iid, void **v)
{
  return odt_obj.QueryInterface (this, IID_IDropTarget, iid, v);
}

STDMETHODIMP_ (ULONG)
ole_drop_target::AddRef ()
{
  return odt_obj.AddRef ();
}

STDMETHODIMP_ (ULONG)
ole_drop_target::Release ()
{
  return odt_obj.Release ();
}

STDMETHODIMP
ole_data_object::QueryInterface (REFIID iid, void **v)
{
  return odo_obj.QueryInterface (this, IID_IDataObject, iid, v);
}

STDMETHODIMP_ (ULONG)
ole_data_object::AddRef ()
{
  return odo_obj.AddRef ();
}

STDMETHODIMP_ (ULONG)
ole_data_object::Release ()
{
  return odo_obj.Release ();
}

STDMETHODIMP
ole_data_object::GetDataHere (FORMATETC *, STGMEDIUM *)
{
  return DATA_E_FORMATETC;
}

STDMETHODIMP
ole_data_object::GetCanonicalFormatEtc (FORMATETC *, FORMATETC *o)
{
  o->ptd = 0; 
  return E_NOTIMPL;
}

STDMETHODIMP
ole_data_object::SetData (FORMATETC *, STGMEDIUM *, BOOL)
{
  return E_NOTIMPL;
}

STDMETHODIMP
ole_data_object::DAdvise (FORMATETC *, DWORD, IAdviseSink *, DWORD *)
{
  return OLE_E_ADVISENOTSUPPORTED;
}

STDMETHODIMP
ole_data_object::DUnadvise (DWORD)
{
  return OLE_E_ADVISENOTSUPPORTED;
}

STDMETHODIMP
ole_data_object::EnumDAdvise (IEnumSTATDATA **)
{
  return OLE_E_ADVISENOTSUPPORTED;
}

STDMETHODIMP
ole_enum_FORMATETC::QueryInterface (REFIID iid, void **v)
{
  return oef_obj.QueryInterface (this, IID_IEnumFORMATETC, iid, v);
}

STDMETHODIMP_ (ULONG)
ole_enum_FORMATETC::AddRef ()
{
  return oef_obj.AddRef ();
}

STDMETHODIMP_ (ULONG)
ole_enum_FORMATETC::Release ()
{
  return oef_obj.Release ();
}

STDMETHODIMP
ole_enum_FORMATETC::Next (ULONG celt, FORMATETC *elt, ULONG *nfetched)
{
  if (!elt)
    return E_INVALIDARG;
  ULONG o = oef_index;
  ULONG n = min (max (o + celt, o), oef_count);
  memcpy (elt, oef_etc + o, sizeof *elt * (n - o));
  if (nfetched)
    *nfetched = n - o;
  oef_index = n;
  return n == o + celt ? S_OK : S_FALSE;
}

STDMETHODIMP
ole_enum_FORMATETC::Skip (ULONG celt)
{
  ULONG o = oef_index;
  oef_index = min (max (o + celt, o), oef_count);
  return oef_index == o + celt ? S_OK : S_FALSE;
}

STDMETHODIMP
ole_enum_FORMATETC::Reset ()
{
  oef_index = 0;
  return S_OK;
}

STDMETHODIMP
ole_enum_FORMATETC::Clone (IEnumFORMATETC **v)
{
  if (!v)
    return E_INVALIDARG;
  try
    {
      *v = new ole_enum_FORMATETC (*this);
    }
  catch (nonlocal_jump &)
    {
      *v = 0;
      return E_OUTOFMEMORY;
    }
  return S_OK;
}

static int
make_idl (HWND hwnd, lisp ldir, void *param,
          int (*fn)(HWND, IShellFolder *, ITEMIDLIST **, int, int, void *))
{
  int nfiles = 0;
  for (int i = -1;
       (i = ListView_GetNextItem (hwnd, i, LVNI_SELECTED)) >= 0;
       nfiles++)
    ;
  if (!nfiles)
    return 0;

  safe_com <IMalloc> ialloc;
  ole_error (SHGetMalloc (&ialloc));

  safe_com <IShellFolder> desktop;
  ole_error (SHGetDesktopFolder (&desktop));

  char *dir = (char *)alloca (xstring_length (ldir) * 2 + 1);
  w2s (dir, ldir);
  map_sl_to_backsl (dir);

  int sz = max (int (strlen (dir) + 1), MAX_PATH) + MAX_PATH;
  wchar_t *w = (wchar_t *)alloca (sz * sizeof (wchar_t));
  MultiByteToWideChar (CP_ACP, 0, dir, -1, w, sz);

  ULONG eaten;
  safe_idl dir_idl (ialloc);
  ole_error (desktop->ParseDisplayName (hwnd, 0, w, &eaten, &dir_idl, 0));

  safe_com <IShellFolder> sf;
  ole_error (desktop->BindToObject (dir_idl, 0, IID_IShellFolder, (void **)&sf));

  ITEMIDLIST **idls = (ITEMIDLIST **)alloca (sizeof *idls * nfiles);
  safe_vidl (ialloc, idls, nfiles);

  int nstored = 0, focused = -1;
  for (int i = -1; (nstored < nfiles
                    && (i = ListView_GetNextItem (hwnd, i, LVNI_SELECTED)) >= 0);)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM | LVIF_STATE;
      lvi.stateMask = LVIS_FOCUSED;
      if (ListView_GetItem (hwnd, &lvi))
        {
#if 0
          MultiByteToWideChar (CP_ACP, 0, ((filer_data *)lvi.lParam)->name,
                               -1, w, sz);
#else
          const filer_data *f = (const filer_data *)lvi.lParam;
          MultiByteToWideChar (CP_ACP, 0, *f->name ? f->name : "..", -1, w, sz);
#endif
          ole_error (sf->ParseDisplayName (hwnd, 0, w, &eaten,
                                           &idls[nstored], 0));
          if (lvi.state & LVIS_FOCUSED)
            focused = nstored;
          nstored++;
        }
    }
  if (!nstored)
    return 0;

  return (*fn)(hwnd, sf, idls, nstored, focused, param);
}

static int
drag_file_name (HWND hwnd, IShellFolder *sf,
                ITEMIDLIST **idls, int nidls,
                int focused, void *)
{
  safe_com <IDataObject> data_obj;
  ole_error (sf->GetUIObjectOf (hwnd, nidls, (const ITEMIDLIST **)idls,
                                IID_IDataObject, 0, (void **)&data_obj));
#if 0
  POINT point, pcl;
  GetCursorPos (&point);
  pcl = point;
  ScreenToClient (hwnd, &pcl);
  HIMAGELIST hil = ListView_CreateDragImage (hwnd, item, &pcl);
  if (hil)
    {
      ImageList_BeginDrag (hil, 0, 8, 8);
      ImageList_DragEnter (GetDesktopWindow (), point.x, point.y);
    }
#endif
  filer_drop_source drop_src;
  DWORD effect = DROPEFFECT_COPY | DROPEFFECT_MOVE | DROPEFFECT_LINK;
  HRESULT hr = DoDragDrop (data_obj, &drop_src, effect, &effect);
  ole_error (hr);

#if 0
  if (hil)
    {
      ImageList_DragLeave (GetDesktopWindow ());
      ImageList_EndDrag ();
      ImageList_Destroy (hil);
    }
#endif
  return (hr == DRAGDROP_S_DROP
          && effect != DROPEFFECT_COPY     // ??? なんでNT4だとDROPEFFECT_MOVEが
          && effect != DROPEFFECT_LINK);   // 返ってこない?
}

int
drag_file_name (HWND hwnd, lisp ldir, int item)
{
  return make_idl (hwnd, ldir, &item, drag_file_name);
}

class safe_menu
{
  HMENU hmenu;
public:
  safe_menu (HMENU h) : hmenu (h) {}
  ~safe_menu () {if (hmenu) DestroyMenu (hmenu);}
  operator HMENU () {return hmenu;}
};

struct ctx_menu_param
{
  HWND hwnd_owner;
  POINT point;
  IContextMenu2 **ctxmenu2;
};

static int
shell_context_menu (HWND hwnd, IShellFolder *sf,
                    ITEMIDLIST **idls, int nidls,
                    int focused, void *vparam)
{
  ctx_menu_param &param = *(ctx_menu_param *)vparam;
  safe_menu menu (CreatePopupMenu ());
  if (!menu)
    FEsimple_win32_error (GetLastError ());

  if (focused > 0)
    swap (idls[0], idls[focused]);

  IContextMenu *ctx_menu;
  safe_com <IContextMenu> ctx_menu1;
  safe_com <IContextMenu2> ctx_menu2;
  ole_error (sf->GetUIObjectOf (hwnd, nidls, (const ITEMIDLIST **)idls,
                                IID_IContextMenu, 0, (void **)&ctx_menu1));
  if (ctx_menu1->QueryInterface (IID_IContextMenu2, (void **)&ctx_menu2) != S_OK)
    ctx_menu = ctx_menu1;
  else
    ctx_menu = ctx_menu2;

#ifndef CMF_CANRENAME
#define CMF_CANRENAME 0x00000010
#endif
  ole_error (ctx_menu->QueryContextMenu (menu, 0, 1, 65535, CMF_NORMAL | CMF_CANRENAME));
  *param.ctxmenu2 = ctx_menu2;
  int id = TrackPopupMenu (menu,
                           TPM_LEFTALIGN | TPM_TOPALIGN | TPM_RETURNCMD | TPM_RIGHTBUTTON,
                           param.point.x, param.point.y, 0, param.hwnd_owner, 0);
  *param.ctxmenu2 = 0;
  if (id <= 0)
    return 0;

  HWND hwnd_active = GetActiveWindow ();
  int enabled = IsWindowEnabled (app.toplev);

  CMINVOKECOMMANDINFO ci;
  ci.cbSize = sizeof ci;
  ci.fMask = 0;
  ci.hwnd = hwnd;
  ci.lpVerb = MAKEINTRESOURCE (id - 1);
  ci.lpParameters = "";
  ci.lpDirectory = "";
  ci.nShow = SW_SHOWNORMAL;
  ole_error (ctx_menu->InvokeCommand (&ci));

  SetActiveWindow (hwnd_active);
  if (!enabled && IsWindowEnabled (app.toplev))
    EnableWindow (app.toplev, 0);

  return 1;
}

int
shell_context_menu (HWND hwnd, lisp ldir, const POINT &point, HWND hwnd_owner,
                    IContextMenu2 **ctx2)
{
  ctx_menu_param param;
  param.hwnd_owner = hwnd_owner;
  param.point = point;
  param.ctxmenu2 = ctx2;
  return make_idl (hwnd, ldir, (void *)&param, shell_context_menu);
}

void
filer_drop_target::hilite_item (int o)
{
  if (fdt_hilited == o)
    return;
  if (o != -1)
    ListView_SetItemState (fdt_view->fv_hwnd, o, 0, LVIS_DROPHILITED);
  if (fdt_hilited != -1)
    ListView_SetItemState (fdt_view->fv_hwnd, fdt_hilited,
                           LVIS_DROPHILITED, LVIS_DROPHILITED);
}

int
filer_drop_target::in_client_p (const POINTL &pl) const
{
  POINT p;
  p.x = pl.x;
  p.y = pl.y;
  ScreenToClient (fdt_view->fv_hwnd, &p);
  RECT r;
  GetClientRect (fdt_view->fv_hwnd, &r);
  if (!PtInRect (&r, p))
    return 0;

  HWND header = GetWindow (fdt_view->fv_hwnd, GW_CHILD);
  if (header)
    {
      GetClientRect (header, &r);
      if (p.y < r.bottom)
        return 0;
    }
  return 1;
}

void
filer_drop_target::scroll_view (const POINTL &pt) const
{
  int top = ListView_GetTopIndex (fdt_view->fv_hwnd);
  if (top < 0)
    return;
  int bottom = top + ListView_GetCountPerPage (fdt_view->fv_hwnd);

  LV_HITTESTINFO ht;
  ht.pt.x = pt.x;
  ht.pt.y = pt.y;
  ScreenToClient (fdt_view->fv_hwnd, &ht.pt);
  int index = ListView_HitTest (fdt_view->fv_hwnd, &ht);
  if (index < 0)
    return;

  int goal = -1;

  HWND header = GetWindow (fdt_view->fv_hwnd, GW_CHILD);
  if (header)
    {
      RECT r;
      GetClientRect (header, &r);
      if (ht.pt.y < r.bottom)
        goal = top - 1;
    }
  else if (index == top)
    goal = top - 1;

  if (index == bottom)
    {
      RECT cr, r;
      GetClientRect (fdt_view->fv_hwnd, &cr);
      ListView_GetItemRect (fdt_view->fv_hwnd, index, &r, LVIR_BOUNDS);
      goal = cr.bottom == r.bottom ? bottom + 1 : bottom;
    }

  if (goal >= 0)
    {
      goal = min (goal, ListView_GetItemCount (fdt_view->fv_hwnd));
      ListView_EnsureVisible (fdt_view->fv_hwnd, goal, 0);
    }
}

inline int
filer_drop_target::target_path_length () const
{
  return (xstring_length (fdt_view->get_directory ()) * 2
          + MAX_PATH + 10);
}

void
filer_drop_target::target_path (char *buf, const POINTL &pt)
{
  w2s (buf, fdt_view->get_directory ());

  LV_HITTESTINFO ht;
  ht.pt.x = pt.x;
  ht.pt.y = pt.y;
  ScreenToClient (fdt_view->fv_hwnd, &ht.pt);
  int index = ListView_HitTest (fdt_view->fv_hwnd, &ht);
  if (index < 0)
    return;

  RECT r;
  ListView_GetItemRect (fdt_view->fv_hwnd, index, &r, LVIR_LABEL);
  if (ht.pt.x >= r.right)
    return;

  LV_ITEM lv;
  lv.iItem = index;
  lv.iSubItem = 0;
  lv.mask = LVIF_PARAM;
  if (!ListView_GetItem (fdt_view->fv_hwnd, &lv))
    return;

  const filer_data *f = (filer_data *)lv.lParam;
  if (f->attr & FILE_ATTRIBUTE_DIRECTORY)
    {
      fdt_hilited = index;
      if (*f->name)
        strcpy (strappend (buf, f->name), "/");
      else
        {
          char *sl = find_last_slash (buf);
          if (!sl)
            return;
          if (sl[1])
            {
              sl[1] = 0;
              return;
            }
          *sl = 0;
          char *up = find_last_slash (buf);
          if (!up)
            *sl = '/';
          else
            up[1] = 0;
        }
    }
}

static UINT CF_SHELLIDLIST = RegisterClipboardFormat (CFSTR_SHELLIDLIST);

class safe_STGMEDIUM
{
  STGMEDIUM *medium;
public:
  safe_STGMEDIUM (STGMEDIUM &medium_) : medium (&medium_) {}
  ~safe_STGMEDIUM ()
    {
      GlobalUnlock (medium->hGlobal);
      ReleaseStgMedium (medium);
    }
};

int
filer_drop_target::check_self (const char *path, char *base, char *target)
{
  if (!*base)
    {
      if (strlen (path) >= PATH_MAX)
        return 0;
      strcpy (base, path);
      char *sl = find_last_slash (base);
      if (!sl)
        return 0;
      sl[1] = 0;
    }

  char *name = (char *)alloca (strlen (path) + 1);
  strcpy (name, path);
  map_backsl_to_sl (name);
  return !sub_directory_p (target, name);
}

int
filer_drop_target::check_self (const wchar_t *w, char *base, char *target)
{
  int l = wcslen (w) * 2 + 1;
  char *p = (char *)alloca (l);
  WideCharToMultiByte (CP_OEMCP, 0, w, -1, p, l, 0, 0);
  return check_self (p, base, target);
}

int
filer_drop_target::check_self (const POINTL &pt)
{
  char *target = (char *)alloca (target_path_length ());
  target_path (target, pt);
  char *tbuf = (char *)alloca (strlen (target) + 1);

  FORMATETC etc;
  etc.cfFormat = CF_HDROP;
  etc.ptd = 0;
  etc.dwAspect = DVASPECT_CONTENT;
  etc.lindex = -1;
  etc.tymed = TYMED_HGLOBAL;

  STGMEDIUM medium;
  if (FAILED (fdt_data->GetData (&etc, &medium)))
    return 0;

  safe_STGMEDIUM smedium (medium);

  DROPFILES *df = (DROPFILES *)GlobalLock (medium.hGlobal);

  char base_path[PATH_MAX];
  *base_path = 0;

  if (!df->fWide)
    {
      for (const char *p = (char *)df + df->pFiles; *p; p += strlen (p) + 1)
        if (!check_self (p, base_path, strcpy (tbuf, target)))
          return 0;
    }
  else
    {
      for (const wchar_t *p = (wchar_t *)((char *)df + df->pFiles);
           *p; p += wcslen (p) + 1)
        if (!check_self (p, base_path, strcpy (tbuf, target)))
          return 0;
    }

  map_backsl_to_sl (base_path);
  return *base_path && !same_file_p (target, base_path);
}

lisp
filer_drop_target::make_drop_file (const char *path, const char *base_path,
                                   char *target, int link)
{
  char *name = (char *)alloca (strlen (path) + 5);
  strcpy (name, path);
  map_backsl_to_sl (name);
  if (!link && sub_directory_p (target, name))
    return 0;
  DWORD a = WINFS::GetFileAttributes (name);
  if (a == -1)
    return 0;
  if (a & FILE_ATTRIBUTE_DIRECTORY)
    {
      char *sl = find_last_slash (name);
      if (!sl || sl[1])
        strcat (name, "/");
    }

  if (!link && _memicmp (base_path, name, strlen (base_path)))
    return 0;
  return make_string (name);
}

lisp
filer_drop_target::make_drop_file (const wchar_t *w, const char *base_path,
                                   char *target, int link)
{
  int l = wcslen (w) * 2 + 1;
  char *p = (char *)alloca (l);
  WideCharToMultiByte (CP_OEMCP, 0, w, -1, p, l, 0, 0);
  return make_drop_file (p, base_path, target, link);
}

int
filer_drop_target::process_drop (IDataObject *data_obj, const POINTL &pt,
                                 DWORD effect)
{
  char *target = (char *)alloca (target_path_length ());
  target_path (target, pt);
  char *tbuf = (char *)alloca (strlen (target) + 1);
  lisp ltarget = make_string (target);

  FORMATETC etc;
  etc.cfFormat = CF_HDROP;
  etc.ptd = 0;
  etc.dwAspect = DVASPECT_CONTENT;
  etc.lindex = -1;
  etc.tymed = TYMED_HGLOBAL;

  STGMEDIUM medium;
  if (FAILED (data_obj->GetData (&etc, &medium)))
    return 0;

  safe_STGMEDIUM smedium (medium);

  DROPFILES *df = (DROPFILES *)GlobalLock (medium.hGlobal);

  char *base_path = 0;

  if (!df->fWide)
    {
      const char *p = (char *)df + df->pFiles;
      base_path = (char *)alloca (strlen (p) + 1);
      strcpy (base_path, p);
    }
  else
    {
      const wchar_t *w = (wchar_t *)((char *)df + df->pFiles);
      int l = wcslen (w) * 2 + 1;
      base_path = (char *)alloca (l);
      WideCharToMultiByte (CP_OEMCP, 0, w, -1, base_path, l, 0, 0);
    }

  if (!base_path)
    return 0;

  map_backsl_to_sl (base_path);
  char *sl = find_last_slash (base_path);
  if (!sl)
    return 0;
  sl[1] = 0;
  if (effect != DROPEFFECT_LINK && same_file_p (target, base_path))
    return 0;

  lisp lsrc = make_string (base_path);

  lisp lfiles = Qnil;

  if (!df->fWide)
    {
      for (const char *p = (char *)df + df->pFiles; *p; p += strlen (p) + 1)
        {
          lisp x = make_drop_file (p, base_path, strcpy (tbuf, target),
                                   effect == DROPEFFECT_LINK);
          if (!x)
            return 0;
          lfiles = xcons (x, lfiles);
        }
    }
  else
    {
      for (const wchar_t *p = (wchar_t *)((char *)df + df->pFiles);
           *p; p += wcslen (p) + 1)
        {
          lisp x = make_drop_file (p, base_path, strcpy (tbuf, target),
                                   effect == DROPEFFECT_LINK);
          if (!x)
            return 0;
          lfiles = xcons (x, lfiles);
        }
    }

  if (lfiles == Qnil)
    return 0;

  lisp e;
  switch (effect)
    {
    case DROPEFFECT_MOVE:
      e = Kmove;
      break;

    case DROPEFFECT_COPY:
      e = Kcopy;
      break;

    case DROPEFFECT_LINK:
      e = Klink;
      break;

    default:
      return 0;
    }

  lisp args = make_list (e, lfiles, lsrc, ltarget, 0);
  protect_gc gcpro (args);
  ForceSetForegroundWindow (Filer::current_filer ()->id_hwnd);
  suppress_gc sgc;
  Ffuncall (Vfiler_drag_and_drop_helper, args);
  Filer::current_filer ()->end_dispatch ();
  end_wait_cursor (1);
  return 1;
}

DWORD
filer_drop_target::query_drop (DWORD key, const POINTL &pt, DWORD *effect)
{
  DWORD req = *effect;
  *effect = DROPEFFECT_NONE;

  int ohilite = fdt_hilited;
  fdt_hilited = -1;

  if (fdt_accept && fdt_view && fdt_data && in_client_p (pt))
    {
      *effect = (key & MK_CONTROL
                 ? (key & MK_SHIFT ? DROPEFFECT_LINK : DROPEFFECT_COPY)
                 : (key & MK_SHIFT ? DROPEFFECT_MOVE : DROPEFFECT_NONE));
      if (*effect == DROPEFFECT_NONE)
        {
          if (req & DROPEFFECT_MOVE)
            *effect = DROPEFFECT_MOVE;
          else if (req & DROPEFFECT_COPY)
            *effect = DROPEFFECT_COPY;
          else if (req & DROPEFFECT_LINK)
            *effect = DROPEFFECT_LINK;
        }
      else if (!(*effect & req))
        *effect = DROPEFFECT_NONE;

      if (*effect != DROPEFFECT_NONE && !check_self (pt))
        {
          if (*effect != DROPEFFECT_LINK)
            *effect = DROPEFFECT_NONE;
          req &= ~(DROPEFFECT_MOVE | DROPEFFECT_COPY);
        }
    }

  if (*effect == DROPEFFECT_NONE)
    {
      fdt_hilited = -1;
      req = 0;
    }
  hilite_item (ohilite);
  return req;
}

static void
set_menu_state (HMENU hmenu, DWORD effect, DWORD req, UINT id, DWORD f)
{
  MENUITEMINFO mi;
  mi.cbSize = sizeof mi;
  mi.fMask = MIIM_STATE;
  mi.fState = 0;
  if (effect == f)
    mi.fState |= MFS_DEFAULT;
  if (!(req & f))
    mi.fState |= MFS_GRAYED;
  if (mi.fState)
    SetMenuItemInfo (hmenu, id, 0, &mi);
}

void
filer_drop_target::ask_user (DWORD *effect, DWORD req)
{
  if (*effect == DROPEFFECT_NONE)
    return;

  HMENU hmenu = LoadMenu (app.hinst, MAKEINTRESOURCE (IDM_DnD));
  HMENU hsub = GetSubMenu (hmenu, 0);
  set_menu_state (hsub, *effect, req, IDC_MOVE, DROPEFFECT_MOVE);
  set_menu_state (hsub, *effect, req, IDC_COPY, DROPEFFECT_COPY);
  set_menu_state (hsub, *effect, req, IDC_LINK, DROPEFFECT_LINK);

  POINT p;
  GetCursorPos (&p);
  switch (TrackPopupMenu (hsub, TPM_RETURNCMD | TPM_LEFTALIGN | TPM_LEFTBUTTON,
                          p.x, p.y, 0, fdt_view->fv_hwnd, 0))
    {
    case IDC_MOVE:
      *effect = DROPEFFECT_MOVE;
      break;

    case IDC_COPY:
      *effect = DROPEFFECT_COPY;
      break;

    case IDC_LINK:
      *effect = DROPEFFECT_LINK;
      break;

    default:
      *effect = DROPEFFECT_NONE;
      break;
    }
  DestroyMenu (hmenu);
}

STDMETHODIMP
filer_drop_target::DragEnter (IDataObject *data_obj, DWORD key,
                              POINTL pt, DWORD *effect)
{
  FORMATETC etc;
  etc.cfFormat = CF_HDROP;
  etc.ptd = 0;
  etc.dwAspect = DVASPECT_CONTENT;
  etc.lindex = -1;
  etc.tymed = TYMED_HGLOBAL;

  fdt_accept = data_obj->QueryGetData (&etc) == S_OK;
  fdt_key = key;

  fdt_data = data_obj;
  fdt_data->AddRef ();

  query_drop (key, pt, effect);
  return S_OK;
}

STDMETHODIMP
filer_drop_target::DragOver (DWORD key, POINTL pt, DWORD *effect)
{
  query_drop (key, pt, effect);
  scroll_view (pt);
  return S_OK;
}

STDMETHODIMP
filer_drop_target::DragLeave ()
{
  int ohilite = fdt_hilited;
  fdt_hilited = -1;
  hilite_item (ohilite);
  fdt_accept = 0;
  if (fdt_data)
    {
      fdt_data->Release ();
      fdt_data = 0;
    }
  return S_OK;
}

STDMETHODIMP
filer_drop_target::Drop (IDataObject *data_obj, DWORD key,
                         POINTL pt, DWORD *effect)
{
  DWORD req = query_drop (key, pt, effect);

  if (fdt_data)
    {
      fdt_data->Release ();
      fdt_data = 0;
    }

  if (fdt_key & MK_RBUTTON)
    ask_user (effect, req);

  if (fdt_hilited >= 0)
    {
      ListView_SetItemState (fdt_view->fv_hwnd, fdt_hilited, 0, LVIS_DROPHILITED);
      fdt_hilited = -1;
    }

  if (*effect == DROPEFFECT_NONE)
    return S_OK;

  HRESULT hr = E_UNEXPECTED;
  try
    {
      if (process_drop (data_obj, pt, *effect))
        hr = S_OK;
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }

  if (hr != S_OK)
    *effect = DROPEFFECT_NONE;
  app.status_window.clear ();
  return hr;
}

int
goto_pt (const POINTL &pt)
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp)
      {
        RECT r;
        GetClientRect (wp->w_hwnd, &r);
        POINT p;
        p.x = pt.x;
        p.y = pt.y;
        ScreenToClient (wp->w_hwnd, &p);
        if (PtInRect (&r, p))
          {
            if (wp->w_bufp->read_only_p ())
              break;
            int x = p.x;
            int y = p.y;
            if (x < r.left + app.text_font.cell ().cx / 2)
              x -= app.text_font.cell ().cx / 2;
            if (x >= r.right - app.text_font.cell ().cx / 2)
              x += app.text_font.cell ().cx / 2;
            if (y < r.top + app.text_font.cell ().cy / 2)
              y -= app.text_font.cell ().cy / 2;
            if (y >= r.bottom - app.text_font.cell ().cy / 2)
              y += app.text_font.cell ().cy / 2;
            rowcol_from_point (wp, &x, &y);
            if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
              {
                wp->w_bufp->linenum_point (wp->w_point, max (1, y));
                wp->w_bufp->goto_column (wp->w_point, x, 0);
              }
            else
              {
                wp->w_bufp->folded_linenum_point (wp->w_point, max (1, y));
                wp->w_bufp->folded_goto_column (wp->w_point, x, 0);
              }
            wp->w_bufp->check_range (wp->w_point);
            app.drop_window = wp;
            refresh_screen (0);
            return 1;
          }
      }
  app.drop_window = 0;
  refresh_screen (0);
  return 0;
}

void
text_drop_target::query_drop (DWORD key, const POINTL &pt, DWORD *effect)
{
  if (!tdt_accept)
    *effect = DROPEFFECT_NONE;
  else
    {
      DWORD req = *effect;
      if (app.drag_window && app.drag_window->w_bufp != app.drag_buffer)
        *effect = DROPEFFECT_COPY;
      else
        *effect = key & MK_CONTROL ? DROPEFFECT_COPY : DROPEFFECT_MOVE;
      if (!(*effect & req))
        *effect = DROPEFFECT_NONE;
      else if (!goto_pt (pt))
        *effect = DROPEFFECT_NONE;
    }
}

STDMETHODIMP
text_drop_target::DragEnter (IDataObject *data_obj, DWORD key,
                             POINTL pt, DWORD *effect)
{
  app.f_in_drop = 1;

  if (xsymbol_value (Venable_DnD_edit) == Qnil)
    tdt_accept = 0;
  else
    {
      FORMATETC etc;
      etc.cfFormat = CF_TEXT;
      etc.ptd = 0;
      etc.dwAspect = DVASPECT_CONTENT;
      etc.lindex = -1;
      etc.tymed = TYMED_HGLOBAL;
      tdt_accept = data_obj->QueryGetData (&etc) == S_OK;
    }
  tdt_key = key;

  query_drop (key, pt, effect);
  return S_OK;
}

STDMETHODIMP
text_drop_target::DragOver (DWORD key, POINTL pt, DWORD *effect)
{
  query_drop (key, pt, effect);
  return S_OK;
}

STDMETHODIMP
text_drop_target::DragLeave ()
{
  app.drop_window = 0;
  app.f_in_drop = 0;
  refresh_screen (0);
  return S_OK;
}

static UINT CF_XYZZYTEXT = RegisterClipboardFormat ("xyzzy internal text");

struct xyzzytext_header
{
  int size;
  Char data[1];
};

STDMETHODIMP
text_drop_target::Drop (IDataObject *data_obj, DWORD key,
                        POINTL pt, DWORD *effect)
{
  app.f_in_drop = 0;
  query_drop (key, pt, effect);
  if (*effect == DROPEFFECT_NONE)
    return S_OK;

  if (*effect == DROPEFFECT_MOVE
      && app.drag_window
      && app.drag_window->w_bufp == app.drag_buffer
      && app.drop_window->w_bufp == app.drag_window->w_bufp
      && app.drop_window->w_point.p_point >= app.drag_region.p1
      && app.drop_window->w_point.p_point <= app.drag_region.p2)
    {
      *effect = DROPEFFECT_NONE;
      return S_OK;
    }

  FORMATETC etc;
  etc.cfFormat = CF_XYZZYTEXT;
  etc.ptd = 0;
  etc.dwAspect = DVASPECT_CONTENT;
  etc.lindex = -1;
  etc.tymed = TYMED_HGLOBAL;

  STGMEDIUM medium;
  HRESULT hr = data_obj->GetData (&etc, &medium);
  if (hr == DATA_E_FORMATETC)
    {
      etc.cfFormat = CF_UNICODETEXT;
      hr = data_obj->GetData (&etc, &medium);
      if (hr == DATA_E_FORMATETC)
        {
          etc.cfFormat = CF_TEXT;
          hr = data_obj->GetData (&etc, &medium);
        }
    }
  if (FAILED (hr))
    return E_UNEXPECTED;

  safe_STGMEDIUM smedium (medium);

  hr = S_OK;
  void *ptr = GlobalLock (medium.hGlobal);
  if (ptr)
    {
      try
        {
          Window *wp = app.drop_window;
          Buffer *bp = wp->w_bufp;
          bp->check_read_only ();
          hr = E_OUTOFMEMORY;
          point_t opoint = wp->w_point.p_point;
          if (etc.cfFormat == CF_TEXT || etc.cfFormat == CF_UNICODETEXT)
            {
              lisp x = make_simple_string ();
              if (make_string_from_clipboard_text (x, ptr, etc.cfFormat,
                                                   ENCODING_LANG_NIL))
                {
                  bp->insert_chars (app.drop_window->w_point,
                                    xstring_contents (x), xstring_length (x));
                  hr = S_OK;
                }
            }
          else
            {
              xyzzytext_header *x = (xyzzytext_header *)ptr;
              bp->insert_chars (app.drop_window->w_point, x->data, x->size);
              hr = S_OK;
            }
          if (hr == S_OK)
            bp->goto_char (wp->w_point, opoint);
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
          *effect = DROPEFFECT_NONE;
        }
      GlobalUnlock (medium.hGlobal);
    }
  refresh_screen (!app.drag_window
                  || app.drop_window->w_bufp != app.drag_window->w_bufp);

  if (hr != S_OK)
    *effect = DROPEFFECT_NONE;
  return hr;
}

class text_enum_FORMATETC: public ole_enum_FORMATETC
{
protected:
  static FORMATETC tef_etc[];

public:
  text_enum_FORMATETC ();
  ~text_enum_FORMATETC ();

  STDMETHOD_ (ULONG, Release) ();
};

class text_data_object: public ole_data_object
{
  Buffer *tdo_bp;
  point_t tdo_p1, tdo_p2;

public:
  text_data_object (Buffer *, point_t, point_t);
  ~text_data_object ();
  void make_invalid ();

  STDMETHOD_ (ULONG, Release) ();
  STDMETHOD (GetData) (FORMATETC *, STGMEDIUM *);
  STDMETHOD (QueryGetData) (FORMATETC *);
  STDMETHOD (EnumFormatEtc) (DWORD, IEnumFORMATETC **);
};

class text_drop_source: public ole_drop_source
{
};

FORMATETC text_enum_FORMATETC::tef_etc[] =
{
  {0, 0, DVASPECT_CONTENT, -1, TYMED_HGLOBAL},
  {CF_UNICODETEXT, 0, DVASPECT_CONTENT, -1, TYMED_HGLOBAL},
  {CF_TEXT, 0, DVASPECT_CONTENT, -1, TYMED_HGLOBAL},
};

inline
text_enum_FORMATETC::text_enum_FORMATETC ()
     : ole_enum_FORMATETC (numberof (tef_etc), tef_etc)
{
  tef_etc[0].cfFormat = CF_XYZZYTEXT;
}

inline
text_enum_FORMATETC::~text_enum_FORMATETC ()
{
}

STDMETHODIMP_ (ULONG)
text_enum_FORMATETC::Release ()
{
  ULONG x = ole_enum_FORMATETC::Release ();
  if (!x)
    delete this;
  return x;
}

inline
text_data_object::text_data_object (Buffer *bp, point_t p1, point_t p2)
     : tdo_bp (bp), tdo_p1 (p1), tdo_p2 (p2)
{
}

inline
text_data_object::~text_data_object ()
{
}

inline void
text_data_object::make_invalid ()
{
  tdo_bp = 0;
}

STDMETHODIMP_ (ULONG)
text_data_object::Release ()
{
  ULONG x = ole_data_object::Release ();
  if (!x)
    delete this;
  return x;
}

STDMETHODIMP
text_data_object::GetData (FORMATETC *etc, STGMEDIUM *medium)
{
  bzero (medium, sizeof *medium);

  if ((etc->cfFormat == CF_TEXT
       || etc->cfFormat == CF_UNICODETEXT
       || etc->cfFormat == CF_XYZZYTEXT)
      && etc->dwAspect == DVASPECT_CONTENT
      && etc->tymed & TYMED_HGLOBAL)
    {
      if (!tdo_bp)
        return E_UNEXPECTED;
      try
        {
          lisp string = tdo_bp->substring (tdo_p1, tdo_p2);
          HGLOBAL h;
          if (etc->cfFormat == CF_TEXT || etc->cfFormat == CF_UNICODETEXT)
            {
              CLIPBOARDTEXT clp;
              h = make_clipboard_text (clp, string, etc->cfFormat) ? clp.hgl : 0;
            }
          else
            {
              h = GlobalAlloc (GMEM_MOVEABLE,
                               (offsetof (xyzzytext_header, data)
                                + sizeof (Char) * xstring_length (string)));
              if (h)
                {
                  xyzzytext_header *p = (xyzzytext_header *)GlobalLock (h);
                  if (p)
                    {
                      p->size = xstring_length (string);
                      bcopy (xstring_contents (string), p->data, p->size);
                      GlobalUnlock (h);
                    }
                  else
                    {
                      GlobalFree (h);
                      h = 0;
                    }
                }
            }
          destruct_string (string);
          if (!h)
            return E_OUTOFMEMORY;
          medium->tymed = TYMED_HGLOBAL;
          medium->hGlobal = h; 
          return S_OK;
        }
      catch (nonlocal_jump &)
        {
          return E_OUTOFMEMORY;
        }
    }

  return DATA_E_FORMATETC;
}

STDMETHODIMP
text_data_object::QueryGetData (FORMATETC *etc)
{
  if ((etc->cfFormat == CF_TEXT
       || etc->cfFormat == CF_UNICODETEXT
       || etc->cfFormat == CF_XYZZYTEXT)
      && etc->dwAspect == DVASPECT_CONTENT
      && etc->tymed & TYMED_HGLOBAL)
    return S_OK;
  return S_FALSE;
}

STDMETHODIMP
text_data_object::EnumFormatEtc (DWORD dir, IEnumFORMATETC **etc)
{
  switch (dir)
    {
    case DATADIR_GET:
      try
        {
          *etc = new text_enum_FORMATETC;
          return S_OK;
        }
      catch (nonlocal_jump &)
        {
          return E_OUTOFMEMORY;
        }

    case DATADIR_SET:
      return E_NOTIMPL;

    default:
      return E_INVALIDARG;
    }
}

lisp
Fdrag_region (lisp from, lisp to)
{
  Buffer *bp = selected_buffer ();
  point_t p1 = bp->coerce_to_point (from);
  point_t p2 = bp->coerce_to_point (to);
  if (p1 > p2)
    swap (p1, p2);
  app.drop_window = 0;
  text_data_object *data_obj = new text_data_object (bp, p1, p2);
  HRESULT hr = 0;
  DWORD effect = DROPEFFECT_COPY | DROPEFFECT_MOVE;
  try
    {
      app.drag_window = selected_window ();
      app.drag_buffer = selected_buffer ();
      app.drag_region.p1 = p1;
      app.drag_region.p2 = p2;
      text_drop_source drop_src;
      hr = DoDragDrop (data_obj, &drop_src, effect, &effect);
    }
  catch (...)
    {
    }

  app.drag_window = 0;
  app.drag_buffer = 0;
  data_obj->make_invalid ();
  data_obj->Release ();

  ole_error (hr);

  if (hr != DRAGDROP_S_DROP)
    return Qnil;

  lisp leffect;
  switch (effect)
    {
    case DROPEFFECT_COPY:
      leffect = Kcopy;
      break;

    case DROPEFFECT_MOVE:
      leffect = Kmove;
      break;

    default:
      return Qnil;
    }

  multiple_value::count () = 2;
  multiple_value::value (1) = app.drop_window ? app.drop_window->lwp : Qnil;
  app.drop_window = 0;
  return leffect;
}

STDMETHODIMP
buffer_bar_drop_target::DragEnter (IDataObject *data_obj, DWORD key,
                                   POINTL pt, DWORD *effect)
{
  if (xsymbol_value (Venable_DnD_edit) == Qnil)
    b_accept = 0;
  else
    {
      FORMATETC etc;
      etc.cfFormat = CF_TEXT;
      etc.ptd = 0;
      etc.dwAspect = DVASPECT_CONTENT;
      etc.lindex = -1;
      etc.tymed = TYMED_HGLOBAL;
      b_accept = data_obj->QueryGetData (&etc) == S_OK;
      if (b_accept)
        b_bar->drag_enter (pt.x, pt.y);
    }
  *effect = DROPEFFECT_NONE;
  return S_OK;
}

STDMETHODIMP
buffer_bar_drop_target::DragOver (DWORD key, POINTL pt, DWORD *effect)
{
  if (b_accept)
    b_bar->drag_over (pt.x, pt.y);
  *effect = DROPEFFECT_NONE;
  return S_OK;
}

STDMETHODIMP
buffer_bar_drop_target::DragLeave ()
{
  b_bar->drag_leave ();
  b_accept = 0;
  return S_OK;
}

STDMETHODIMP
buffer_bar_drop_target::Drop (IDataObject *data_obj, DWORD key,
                              POINTL pt, DWORD *effect)
{
  b_bar->drag_leave ();
  b_accept = 0;
  *effect = DROPEFFECT_NONE;
  return S_OK;
}
