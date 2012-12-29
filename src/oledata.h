// -*-C++-*-
#ifndef _oledata_h_
# define _oledata_h_

interface IConnectionPoint;

class event_sink: public IDispatch
{
protected:
  ULONG s_ref;
  IConnectionPoint *s_conn_point;
  ITypeInfo *s_typeinfo;
  IID s_iid;
  DWORD s_cookie;
  lisp s_handlers;

public:
  event_sink (IConnectionPoint *, ITypeInfo *, IID);
  virtual ~event_sink ();
  lisp handlers () const {return s_handlers;}
  void add_handler (lisp id, lisp fn)
    {s_handlers = xcons (xcons (id, fn), s_handlers);}
  ITypeInfo *typeinfo () const {return s_typeinfo;}
  STDMETHOD (advise) ();
  STDMETHOD_ (ULONG, AddRef) ();
  STDMETHOD_ (ULONG, Release) ();
  STDMETHOD (QueryInterface) (REFIID, void **);
  STDMETHOD (GetTypeInfoCount) (UINT *);
  STDMETHOD (GetTypeInfo) (UINT, LCID, ITypeInfo **);
  STDMETHOD (GetIDsOfNames) (REFIID, OLECHAR **, UINT, LCID, DISPID *);
  STDMETHOD (Invoke) (DISPID, REFIID, LCID, unsigned short, DISPPARAMS *,
                      VARIANT *, EXCEPINFO *, unsigned int *);
};

class loledata: public lisp_object
{
public:
  lisp name;
  IDispatch *disp;
  IEnumVARIANT *enumerator;
  event_sink *event;
  ~loledata ()
    {
      if (event) event->Release ();
      if (enumerator) enumerator->Release ();
      if (disp) disp->Release ();
    }
};

# define oledata_p(X) typep ((X), Toledata)

inline void
check_oledata (lisp x)
{
  check_type (x, Toledata, Qoledata);
}

inline lisp &
xoledata_name (lisp x)
{
  assert (oledata_p (x));
  return ((loledata *)x)->name;
}

inline IDispatch *&
xoledata_disp (lisp x)
{
  assert (oledata_p (x));
  return ((loledata *)x)->disp;
}

inline IEnumVARIANT *&
xoledata_enumerator (lisp x)
{
  assert (oledata_p (x));
  return ((loledata *)x)->enumerator;
}

inline event_sink *&
xoledata_event (lisp x)
{
  assert (oledata_p (x));
  return ((loledata *)x)->event;
}

inline loledata *
make_oledata ()
{
  loledata *p = ldata <loledata, Toledata>::lalloc ();
  p->name = 0;
  p->disp = 0;
  p->enumerator = 0;
  p->event = 0;
  return p;
}

void set_oledata_name (lisp x);
#endif
