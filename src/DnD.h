#ifndef _DnD_h_
# define _DnD_h_

#include <ole2.h>
#include <ole2ver.h>

class ole_object
{
protected:
  int o_ref;
public:
  ole_object () : o_ref (1) {}
  ~ole_object () {}

  HRESULT QueryInterface (IUnknown *, REFIID, REFIID, void **);
  ULONG AddRef () {return ++o_ref;}
  ULONG Release () {return --o_ref;}
};

class ole_drop_source: public IDropSource
{
protected:
  ole_object ods_obj;
  int ods_buttons;

public:
  ole_drop_source () : ods_buttons (0) {}
  ~ole_drop_source () {}

  STDMETHOD (QueryInterface) (REFIID, void **);
  STDMETHOD_ (ULONG, AddRef) ();
  STDMETHOD_ (ULONG, Release) ();

  STDMETHOD (QueryContinueDrag) (BOOL, DWORD);
  STDMETHOD (GiveFeedback) (DWORD);
};

class ole_drop_target: public IDropTarget
{
protected:
  ole_object odt_obj;

public:
  ole_drop_target () {}
  ~ole_drop_target () {}

  STDMETHOD (QueryInterface) (REFIID, void **);
  STDMETHOD_ (ULONG, AddRef) ();
  STDMETHOD_ (ULONG, Release) ();
};

class ole_enum_FORMATETC: public IEnumFORMATETC
{
protected:
  ole_object oef_obj;

  ULONG oef_index;
  ULONG oef_count;
  const FORMATETC *const oef_etc;

public:
  ole_enum_FORMATETC (const ole_enum_FORMATETC &src)
       : oef_index (src.oef_index),
         oef_count (src.oef_count),
         oef_etc (src.oef_etc)
    {}
  ole_enum_FORMATETC (ULONG count, const FORMATETC *etc)
       : oef_index (0), oef_count (count), oef_etc (etc)
    {}
  ~ole_enum_FORMATETC () {}

  STDMETHOD (QueryInterface) (REFIID, void **);
  STDMETHOD_ (ULONG, AddRef) ();
  STDMETHOD_ (ULONG, Release) ();

  STDMETHOD (Next) (ULONG, FORMATETC *, ULONG *);
  STDMETHOD (Skip) (ULONG);
  STDMETHOD (Reset) ();
  STDMETHOD (Clone) (IEnumFORMATETC **);
};

class ole_data_object: public IDataObject
{
protected:
  ole_object odo_obj;

public:
  ole_data_object () {}
  ~ole_data_object () {}

  STDMETHOD (QueryInterface) (REFIID, void **);
  STDMETHOD_ (ULONG, AddRef) ();
  STDMETHOD_ (ULONG, Release) ();

  STDMETHOD (GetDataHere) (FORMATETC *, STGMEDIUM *);
  STDMETHOD (GetCanonicalFormatEtc) (FORMATETC *, FORMATETC *);
  STDMETHOD (SetData) (FORMATETC *, STGMEDIUM *, BOOL);
  STDMETHOD (DAdvise) (FORMATETC *, DWORD, IAdviseSink *, DWORD *);
  STDMETHOD (DUnadvise) (DWORD);
  STDMETHOD (EnumDAdvise) (IEnumSTATDATA **);
};

class filer_drop_source: public ole_drop_source
{
};

class FilerView;

class filer_drop_target: public ole_drop_target
{
private:
  int fdt_accept;
  const FilerView *fdt_view;
  IDataObject *fdt_data;
  int fdt_hilited;
  DWORD fdt_key;

  int check_self (const POINTL &);
  DWORD query_drop (DWORD, const POINTL &, DWORD *);
  void target_path (char *, const POINTL &);
  int target_path_length () const;
  int process_drop (IDataObject *, const POINTL &, DWORD);
  void hilite_item (int);
  void ask_user (DWORD *, DWORD);
  int in_client_p (const POINTL &) const;
  void scroll_view (const POINTL &) const;
  static int check_self (const char *path, char *base, char *target);
  static int check_self (const wchar_t *w, char *base, char *target);
  static lisp make_drop_file (const char *, const char *, char *, int);
  static lisp make_drop_file (const wchar_t *, const char *, char *, int);

public:
  filer_drop_target ()
       : fdt_accept (0), fdt_view (0), fdt_data (0), fdt_hilited (-1) {}
  ~filer_drop_target () {}
  void set_view (const FilerView *v) {fdt_view = v;}
  STDMETHOD (DragEnter) (IDataObject *, DWORD, POINTL, DWORD *);
  STDMETHOD (DragOver) (DWORD, POINTL, DWORD *);
  STDMETHOD (DragLeave) ();
  STDMETHOD (Drop) (IDataObject *, DWORD, POINTL, DWORD *);
};

class text_drop_target: public ole_drop_target
{
  int tdt_accept;
  DWORD tdt_key;

  void query_drop (DWORD, const POINTL &, DWORD *);
public:
  text_drop_target () : tdt_accept (0) {}
  ~text_drop_target () {}
  STDMETHOD (DragEnter) (IDataObject *, DWORD, POINTL, DWORD *);
  STDMETHOD (DragOver) (DWORD, POINTL, DWORD *);
  STDMETHOD (DragLeave) ();
  STDMETHOD (Drop) (IDataObject *, DWORD, POINTL, DWORD *);
};

class buffer_bar;

class buffer_bar_drop_target: public ole_drop_target
{
  buffer_bar *const b_bar;
  int b_accept;
public:
  buffer_bar_drop_target (buffer_bar *bar) : b_bar (bar), b_accept (0) {}
  ~buffer_bar_drop_target () {}
  STDMETHOD (DragEnter) (IDataObject *, DWORD, POINTL, DWORD *);
  STDMETHOD (DragOver) (DWORD, POINTL, DWORD *);
  STDMETHOD (DragLeave) ();
  STDMETHOD (Drop) (IDataObject *, DWORD, POINTL, DWORD *);
};


int drag_file_name (HWND hwnd, lisp, int);

interface IContextMenu2;

int shell_context_menu (HWND, lisp, const POINT &, HWND, IContextMenu2 **);

#endif
