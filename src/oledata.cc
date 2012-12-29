#include "stdafx.h"
#include "ed.h"
#include "com.h"
#include "oleconv.h"

class safe_variant
{
  VARIANT &v;
public:
  safe_variant (VARIANT &v_) : v (v_) {}
  ~safe_variant () {VariantClear (&v);}
};

static lisp
bstr2obj (BSTR bstr)
{
  USES_CONVERSION;
  return make_string (bstr ? W2A (bstr) : "");
}

static BSTR
obj2bstr (lisp obj)
{
  int l = i2wl (obj);
  BSTR bstr = SysAllocStringLen (0, l - 1);
  if (!bstr)
    FEstorage_error ();
  i2w (obj, bstr);
  return bstr;
}

static inline lisp
stringy (lisp x)
{
  if (symbolp (x))
    x = xsymbol_name (x);
  check_string (x);
  return x;
}

class safe_array_locker
{
public:
  safe_array_locker(SAFEARRAY *sa_) : sa(sa_)
    {
      ole_error (SafeArrayLock (sa));
    }
  ~safe_array_locker()
    {
      ole_error (SafeArrayUnlock (sa));
    }
private:
  SAFEARRAY *sa;
};

static lisp
variant2obj (VARIANT *v)
{
  safe_variant sv (*v);

  if (V_ISARRAY (v))
    {
      SAFEARRAY *sa = V_ISBYREF (v) ? *V_ARRAYREF (v) : V_ARRAY (v);
      int dim = SafeArrayGetDim (sa);
      if (dim == 1)
        {
          long l, u;
          ole_error (SafeArrayGetLBound (sa, 1, &l));
          ole_error (SafeArrayGetUBound (sa, 1, &u));
          lisp object = make_vector (u - l + 1, Qnil);
          {
            safe_array_locker locker (sa);
            VARIANT variant;
            VariantInit (&variant);

            for (lisp *vec = xvector_contents (object); l <= u; l++, vec++)
              {
                safe_variant sv (variant);
                V_VT (&variant) = (V_VT (v) & ~VT_ARRAY) | VT_BYREF;
                ole_error (SafeArrayPtrOfIndex (sa, &l, &V_BYREF (&variant)));
                *vec = variant2obj (&variant);
              }
          }
          return object;
        }
      return FEprogram_error (Ecannot_convert_from_variant);
    }

  for (; V_VT (v) == (VT_BYREF | VT_VARIANT); v = V_VARIANTREF (v))
    ;
  switch(V_VT (v) & ~VT_BYREF)
    {
    case VT_EMPTY:
    case VT_NULL:
      multiple_value::count () = 0;
      return Qnil;

    case VT_UI1:
      return make_fixnum (V_ISBYREF (v) ? *V_UI1REF (v) : V_UI1 (v));

    case VT_I2:
      return make_fixnum (V_ISBYREF (v) ? *V_I2REF (v) : V_I2 (v));

    case VT_I4:
      return make_fixnum (V_ISBYREF (v) ? *V_I4REF (v) : V_I4 (v));

    case VT_R4:
      return make_single_float (V_ISBYREF (v) ? *V_R4REF (v) : V_R4 (v));

    case VT_R8:
      return make_double_float (V_ISBYREF (v) ? *V_R8REF (v) : V_R8 (v));

    case VT_BSTR:
      return bstr2obj (V_ISBYREF (v) ? *V_BSTRREF (v) : V_BSTR (v));

    case VT_ERROR:
      return make_fixnum (V_ISBYREF (v) ? *V_ERRORREF (v) : V_ERROR (v));

    case VT_BOOL:
      return boole (V_ISBYREF (v) ? *V_BOOLREF (v) : V_BOOL (v));

    case VT_DISPATCH:
      {
        lisp obj = make_oledata ();
        xoledata_disp (obj) = V_ISBYREF (v) ? *V_DISPATCHREF (v) : V_DISPATCH (v);
        if (xoledata_disp (obj))
          xoledata_disp (obj)->AddRef ();
        return obj;
      }

    case VT_UNKNOWN:
      {
        IUnknown *unk = V_ISBYREF (v) ? *V_UNKNOWNREF (v) : V_UNKNOWN (v);
        if (unk)
          {
            lisp obj = make_oledata ();
            IDispatch *disp = 0;
            HRESULT hr = unk->QueryInterface (IID_IDispatch, (void **)&disp);
            if (SUCCEEDED (hr))
              xoledata_disp (obj) = disp;
            else
              {
                IEnumVARIANT *e = 0;
                ole_error (unk->QueryInterface (IID_IEnumVARIANT, (void **)&e));
                xoledata_enumerator (obj) = e;
                if (e)
                  e->Reset ();
              }
            return obj;
          }
        break;
      }

    default:
      {
        VARIANT variant;
        VariantInit (&variant);
        ole_error (VariantChangeTypeEx (&variant, v, LOCALE_USER_DEFAULT, 0, VT_BSTR));
        safe_variant sv (variant);
        if (V_VT (&variant) == VT_BSTR)
          return bstr2obj (V_BSTR (&variant));
        break;
      }
    }

  return FEprogram_error (Ecannot_convert_from_variant);
}

static void obj2variant (lisp, VARIANT &);

class safe_array
{
  SAFEARRAY *sa;
  int locked;
public:
  safe_array () : sa (0), locked (0) {}
  ~safe_array ()
    {
      if (locked)
        SafeArrayUnlock (sa);
      if (sa)
        SafeArrayDestroy (sa);
    }
  int create (int type, int dim, SAFEARRAYBOUND *b)
    {
      sa = SafeArrayCreate (type, dim, b);
      return int (sa);
    }
  HRESULT lock ()
    {
      HRESULT hr = SafeArrayLock (sa);
      locked = SUCCEEDED (hr);
      return hr;
    }
  HRESULT unlock ()
    {
      HRESULT hr = SafeArrayUnlock (sa);
      if (SUCCEEDED (hr))
        locked = 0;
      return hr;
    }
  operator SAFEARRAY * () const {return sa;}
  SAFEARRAY *finish () {SAFEARRAY *x = sa; sa = 0; return x;}
};

static SAFEARRAY *
vector2variant (const lisp *vec, int len)
{
  SAFEARRAYBOUND b;
  b.lLbound = 0;
  b.cElements = len;
  safe_array sa;
  if (!sa.create (VT_VARIANT, 1, &b))
    FEstorage_error ();

  ole_error (sa.lock ());
  for (long i = 0; i < len; i++, vec++)
    {
      VARIANT variant;
      bzero (&variant, sizeof variant);
      safe_variant sv (variant);
      obj2variant (*vec, variant);
      ole_error (SafeArrayPutElement (sa, &i, &variant));
    }
  ole_error (sa.unlock ());
  return sa.finish ();
}

static SAFEARRAY *
list2variant (lisp list)
{
  int len = xlist_length (list);
  SAFEARRAYBOUND b;
  b.lLbound = 0;
  b.cElements = len;
  safe_array sa;
  if (!sa.create (VT_VARIANT, 1, &b))
    FEstorage_error ();

  ole_error (sa.lock ());
  for (long i = 0; i < len; i++, list = xcdr (list))
    {
      VARIANT variant;
      bzero (&variant, sizeof variant);
      safe_variant sv (variant);
      obj2variant (xcar (list), variant);
      ole_error (SafeArrayPutElement (sa, &i, &variant));
    }
  ole_error (sa.unlock ());
  return sa.finish ();
}

/*GENERIC_FUNCTION*/
static void
obj2variant (lisp object, VARIANT &variant)
{
  if (immediatep (object))
    {
      if (short_int_p (object))
        {
          V_VT (&variant) = VT_I4;
          V_I4 (&variant) = xshort_int_value (object);
          return;
        }
      if (charp (object))
        {
          V_VT (&variant) = VT_I2;
          V_I2 (&variant) = xchar_code (object);
          return;
        }
    }
  else
    {
      switch (object_typeof (object))
        {
        case Tsymbol:
          if (object == Qt || object == Qnil)
            {
              V_VT (&variant) = VT_BOOL;
              V_BOOL (&variant) = object == Qt ? VARIANT_TRUE : VARIANT_FALSE;
              return;
            }
          if (object == Kempty)
            {
              V_VT (&variant) = VT_EMPTY;
              return;
            }
          if (object == Knull)
            {
              V_VT (&variant) = VT_NULL;
              return;
            }
          break;

        case Tlong_int:
          V_VT (&variant) = VT_I4;
          V_I4 (&variant) = xlong_int_value (object);
          return;

        case Tsingle_float:
          V_VT (&variant) = VT_R4;
          V_R4 (&variant) = xsingle_float_value (object);
          return;

        case Tdouble_float:
          V_VT (&variant) = VT_R8;
          V_R8 (&variant) = xdouble_float_value (object);
          return;

        case Toledata:
          if (xoledata_disp (object))
            {
              V_VT (&variant) = VT_DISPATCH;
              V_DISPATCH (&variant) = xoledata_disp (object);
              xoledata_disp (object)->AddRef ();
            }
          else if (xoledata_enumerator (object))
            {
              V_VT (&variant) = VT_UNKNOWN;
              V_UNKNOWN (&variant) = xoledata_enumerator (object);
              xoledata_enumerator (object)->AddRef ();
            }
          else
            V_VT (&variant) = VT_EMPTY;
          return;

        case Tsimple_string:
        case Tcomplex_string:
          V_VT (&variant) = VT_BSTR;
          V_BSTR (&variant) = obj2bstr (object);
          return;

        case Tsimple_vector:
        case Tcomplex_vector:
          V_VT (&variant) = VT_VARIANT | VT_ARRAY;
          V_ARRAY (&variant) = vector2variant (xvector_contents (object),
                                               xvector_length (object));
          return;

        case Tcons:
          V_VT (&variant) = VT_VARIANT | VT_ARRAY;
          V_ARRAY (&variant) = list2variant (object);
          return;

        case Tbignum:
        case Tfraction:
        case Tarray:
        case Tstring_array:
          break;
        }
    }
  FEprogram_error (Ecannot_convert_to_variant, object);
}

static void
objs2variant (IDispatch *disp, lisp lprop, lisp args, lisp named_args, DISPPARAMS &params)
{
  OLECHAR** arg_names = (OLECHAR **)alloca (sizeof (OLECHAR *) * (params.cNamedArgs + 1));
  DISPID* rgdispids = (DISPID*)alloca (sizeof (DISPID) * (params.cNamedArgs + 1));
  try
    {
      // set method name
      arg_names[0] = I2W (stringy (lprop));

      // set named args
      lisp x = named_args;
      for (int i = 0; consp (x); i++, x = Fcddr (x))
        {
          lisp lkey = stringy (xcar (x));
          lisp lval = Fcadr (x);

          arg_names[i + 1] = I2W (lkey);
          obj2variant (lval, params.rgvarg[i]);
        }

      HRESULT hr = disp->GetIDsOfNames (IID_NULL, arg_names, params.cNamedArgs + 1,
                                        LOCALE_USER_DEFAULT, rgdispids);
      if (FAILED (hr))
        FEprogram_error (Eget_named_args_info_failed, xcons (lprop, named_args));

      for (int i = params.cNamedArgs - 1; i >= 0; i--)
        params.rgdispidNamedArgs[i] = rgdispids[i + 1];

      // set normal args
      for (int i = params.cArgs - 1; consp (args); i--, args = xcdr (args))
        obj2variant (xcar (args), params.rgvarg[i]);
    }
  catch (nonlocal_jump &)
    {
      for (int i = params.cArgs - 1; i >= 0; i--)
        VariantClear (&params.rgvarg[i]);
      throw;
    }
}

static void
objs2variant (lisp args, DISPPARAMS &params)
{
  try
    {
      for (int i = params.cArgs - 1; consp (args); i--, args = xcdr (args))
        obj2variant (xcar (args), params.rgvarg[i]);
    }
  catch (nonlocal_jump &)
    {
      for (int i = params.cArgs - 1; i >= 0; i--)
        VariantClear (&params.rgvarg[i]);
      throw;
    }
}

static DISPID
get_dispid (IDispatch *disp, lisp lprop)
{
  wchar_t *wprop = I2W (stringy (lprop));
  DISPID dispid;
  ole_error (disp->GetIDsOfNames (IID_NULL, &wprop, 1, LOCALE_USER_DEFAULT, &dispid));
  return dispid;
}

static void
cleanup (HRESULT hr, EXCEPINFO &excep)
{
  if (GetScode (hr) == DISP_E_EXCEPTION)
    {
      if (excep.bstrSource)
        SysFreeString (excep.bstrSource);
      if (excep.bstrDescription)
        SysFreeString (excep.bstrDescription);
      if (excep.bstrHelpFile)
        SysFreeString (excep.bstrHelpFile);
    }
}

static int
count_named_args (lisp named_args)
{
  if (named_args == Qnil)
    return 0;

  if (!consp (named_args))
    FEprogram_error (Einvalid_named_args_list, named_args);

  int c = 0;
  for (; consp (named_args); named_args = xcdr (named_args))
    {
      lisp key = xcar (named_args);
      if (!(stringp (key) || symbolp (key)))
        FEprogram_error (Einvalid_named_args_list, named_args);

      named_args = xcdr (named_args);
      if (!consp (named_args))
        FEprogram_error (Einvalid_named_args_list, named_args);

      c++;
    }

  return c;
}

static lisp
ole_invoke (lisp lobj, lisp lprop, lisp args, lisp named_args, int flags)
{
  check_oledata (lobj);
  if (!xoledata_disp (lobj))
    FEprogram_error (Einvalid_idispatch);
  if (!args) args = Qnil;
  if (!named_args) named_args = Qnil;

  DISPID dispid = get_dispid (xoledata_disp (lobj), lprop);
  int c_named_args = count_named_args (named_args);

  DISPPARAMS params;
  bzero (&params, sizeof params);
  params.cArgs = xlist_length (args) + c_named_args;
  params.rgvarg = (VARIANT *)alloca (sizeof (VARIANT) * params.cArgs);
  bzero (params.rgvarg, sizeof (VARIANT) * params.cArgs);

  if (c_named_args == 0)
    objs2variant (args, params);
  else
    {
      params.cNamedArgs = c_named_args;
      params.rgdispidNamedArgs = (DISPID *)alloca (sizeof (DISPID) * params.cNamedArgs);
      bzero (params.rgdispidNamedArgs, sizeof (DISPID) * params.cNamedArgs);
      objs2variant (xoledata_disp (lobj), lprop, args, named_args, params);
    }

  EXCEPINFO excep;
  bzero (&excep, sizeof excep);
  UINT argerr = UINT (-1);
  VARIANT result;
  bzero (&result, sizeof result);

  HRESULT hr = xoledata_disp (lobj)->Invoke (dispid, IID_NULL, LOCALE_USER_DEFAULT,
                                             flags, &params, &result, &excep, &argerr);
  if (FAILED (hr) && GetScode (hr) == DISP_E_EXCEPTION && dispid >= 0x8000)
    {
      cleanup (hr, excep);
      bzero (&excep, sizeof excep);
      hr = xoledata_disp (lobj)->Invoke (dispid, IID_NULL, LOCALE_USER_DEFAULT,
                                         flags, &params, 0, &excep, &argerr);
    }

  for (int i = params.cArgs - 1; i >= 0; i--)
    VariantClear (&params.rgvarg[i]);

  if (FAILED (hr))
    {
      cleanup (hr, excep);
      FEsimple_win32_error (hr, lprop);
    }

  return variant2obj (&result);
}

class safe_tlib_attr
{
protected:
  ITypeLib *ti;
  TLIBATTR *attr;
public:
  safe_tlib_attr (ITypeLib *ti_) : ti (ti_), attr (0) {}
  ~safe_tlib_attr () {if (attr) ti->ReleaseTLibAttr (attr);}
  TLIBATTR **operator & () {return &attr;}
  operator TLIBATTR * () {return attr;}
  TLIBATTR *operator -> () const {return attr;}
};

class safe_type_attr
{
protected:
  ITypeInfo *ti;
  TYPEATTR *attr;
public:
  safe_type_attr (ITypeInfo *ti_) : ti (ti_), attr (0) {}
  ~safe_type_attr () {if (attr) ti->ReleaseTypeAttr (attr);}
  TYPEATTR **operator & () {return &attr;}
  operator TYPEATTR * () {return attr;}
  TYPEATTR *operator -> () const {return attr;}
};

static void
get_typelib (IDispatch *disp, safe_com <ITypeLib> &tlib)
{
  safe_com <ITypeInfo> info;
  ole_error (disp->GetTypeInfo (0, LOCALE_SYSTEM_DEFAULT, &info));
  u_int index;
  ole_error (info->GetContainingTypeLib (&tlib, &index));
}

static int
get_default_source2 (IDispatch *disp, safe_com <ITypeInfo> &typeinfo, IID &iid)
{
  safe_com <IProvideClassInfo2> pci2;
  if (FAILED (disp->QueryInterface (IID_IProvideClassInfo2, (void **)&pci2))
      || !pci2)
    return 0;

  if (FAILED (pci2->GetGUID (GUIDKIND_DEFAULT_SOURCE_DISP_IID, &iid)))
    return 0;

  if (IsEqualIID (iid, GUID_NULL))
    return 0;

  safe_com <ITypeLib> tlib;
  get_typelib (disp, tlib);
  ole_error (tlib->GetTypeInfoOfGuid (iid, &typeinfo));
  return 1;
}

#define IMPLTYPE_MASK \
  (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FRESTRICTED)
#define IMPLTYPE_DEFAULTSOURCE \
  (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE)

static void
get_default_source (IDispatch *disp, safe_com <ITypeInfo> &typeinfo, IID &iid)
{
  iid = GUID_NULL;

  if (get_default_source2 (disp, typeinfo, iid))
    return;

  safe_com <IProvideClassInfo> pci;
  ole_error (disp->QueryInterface (IID_IProvideClassInfo, (void **)&pci));

  safe_com <ITypeInfo> clsinfo;
  ole_error (pci->GetClassInfo (&clsinfo));

  safe_type_attr attr (clsinfo);
  ole_error (clsinfo->GetTypeAttr (&attr));
  if (attr->typekind != TKIND_COCLASS)
    ole_error (E_NOINTERFACE);

  for (u_int i = 0; i < attr->cImplTypes; i++)
    {
      int flags;
      if (SUCCEEDED (clsinfo->GetImplTypeFlags (i, &flags))
          && (flags & IMPLTYPE_MASK) == IMPLTYPE_DEFAULTSOURCE)
        {
          HREFTYPE reftype;
          ole_error (clsinfo->GetRefTypeOfImplType (i, &reftype));
          safe_com <ITypeInfo> impl;
          ole_error (clsinfo->GetRefTypeInfo (reftype, &impl));
          safe_type_attr attr (impl);
          ole_error (impl->GetTypeAttr (&attr));
          iid = attr->guid;
          typeinfo = impl;
          typeinfo->AddRef ();
          return;
        }
    }

  ole_error (E_NOINTERFACE);
}

static void
get_typelib_fullpath (safe_com <ITypeLib> &tlib, BSTR &path)
{
  safe_tlib_attr attr (tlib);

  ole_error (tlib->GetLibAttr (&attr));
  ole_error (QueryPathOfRegTypeLib (attr->guid,
                                    attr->wMajorVerNum,
                                    attr->wMinorVerNum,
                                    LOCALE_SYSTEM_DEFAULT,
                                    &path));
}

static lisp
get_type_name (IDispatch *disp)
{
  safe_com <ITypeInfo> info;
  ole_error (disp->GetTypeInfo (0, LOCALE_SYSTEM_DEFAULT, &info));

  BSTR bstr = 0;
  if (SUCCEEDED (info->GetDocumentation (MEMBERID_NIL, &bstr, 0, 0, 0))
      && bstr)
    {
      lisp r = bstr2obj (bstr);
      SysFreeString (bstr);
      return r;
    }

  return Qnil;
}

static lisp
get_oledata_name (lisp x)
{
  try
    {
      if (xoledata_disp (x))
        return get_type_name (xoledata_disp (x));
      else if (xoledata_enumerator (x))
        return get_type_name ((IDispatch *)xoledata_enumerator (x));
      else
        return Qnil;
    }
  catch (nonlocal_jump &)
    {
      return Qnil;
    }
}

void
set_oledata_name (lisp x)
{
  if (!xoledata_name (x))
    xoledata_name (x) = get_oledata_name (x);
}

static void
get_interface_id (IDispatch *disp, const wchar_t *path, const wchar_t *name,
                  safe_com <ITypeInfo> &typeinfo, IID &iid)
{
  safe_com <ITypeLib> tlib;
  if (path)
    ole_error (LoadTypeLib (path, &tlib));
  else
    {
      BSTR bstr;
      get_typelib (disp, tlib);
      get_typelib_fullpath (tlib, bstr);
      if (bstr)
        {
          ole_error (LoadTypeLib (bstr, &tlib));
          SysFreeString (bstr);
        }
    }

  int n = tlib->GetTypeInfoCount ();
  for (int i = 0; i < n; i++)
    {
      safe_com <ITypeInfo> info;
      ole_error (tlib->GetTypeInfo (i, &info));

      safe_type_attr attr (info);
      ole_error (info->GetTypeAttr (&attr));

      if (attr->typekind != TKIND_COCLASS)
        continue;

      for (u_int j = 0; j < attr->cImplTypes; j++)
        {
          HREFTYPE reftype;
          if (FAILED (info->GetRefTypeOfImplType (j, &reftype)))
            continue;

          safe_com <ITypeInfo> impl;
          if (FAILED (info->GetRefTypeInfo (reftype, &impl)) || !impl)
            continue;

          BSTR bstr = 0;
          if (SUCCEEDED (impl->GetDocumentation (MEMBERID_NIL, &bstr, 0, 0, 0))
              && bstr && !wcscmp (bstr, name))
            {
              SysFreeString (bstr);

              safe_type_attr impl_attr (info);
              if (SUCCEEDED (impl->GetTypeAttr (&impl_attr)) && impl_attr)
                {
                  iid = impl_attr->guid;
                  typeinfo = impl;
                  typeinfo->AddRef ();
                  return;
                }
            }
          if (bstr)
            SysFreeString (bstr);
        }
    }

  ole_error (E_NOINTERFACE);
}

event_sink::event_sink (IConnectionPoint *conn_point, ITypeInfo *typeinfo, IID iid)
     : s_ref (1), s_conn_point (conn_point), s_typeinfo (typeinfo),
       s_iid (iid), s_cookie (0), s_handlers (Qnil)
{
  s_conn_point->AddRef ();
  s_typeinfo->AddRef ();
}

event_sink::~event_sink ()
{
  if (s_cookie)
    s_conn_point->Unadvise (s_cookie);
  s_conn_point->Release ();
  s_typeinfo->Release ();
}

STDMETHODIMP
event_sink::advise ()
{
  return s_conn_point->Advise (this, &s_cookie);
}

STDMETHODIMP_ (ULONG)
event_sink::AddRef ()
{
  return ++s_ref;
}

STDMETHODIMP_ (ULONG)
event_sink::Release ()
{
  if (--s_ref)
    return s_ref;
  delete this;
  return 0;
}

STDMETHODIMP
event_sink::QueryInterface (REFIID iid, void **v)
{
  if (!v || !this)
    return E_INVALIDARG;
  *v = 0;
  if (iid != IID_IUnknown && iid != IID_IDispatch && iid != s_iid)
    return E_NOINTERFACE;
  *v = this;
  AddRef ();
  return S_OK;
}

STDMETHODIMP
event_sink::GetTypeInfoCount (UINT *n)
{
  if (!n)
    return E_INVALIDARG;
  *n = 0;
  return S_OK;
}

STDMETHODIMP
event_sink::GetTypeInfo (UINT, LCID, ITypeInfo **info)
{
  if (!info)
    return E_INVALIDARG;
  *info = 0;
  return DISP_E_BADINDEX;
}

STDMETHODIMP
event_sink::GetIDsOfNames (REFIID, OLECHAR **, UINT, LCID, DISPID *)
{
  return DISP_E_UNKNOWNNAME;
}

STDMETHODIMP
event_sink::Invoke (DISPID dispid, REFIID iid, LCID lcid, unsigned short flags,
                    DISPPARAMS *dispparams, VARIANT *result,
                    EXCEPINFO *excep, unsigned int *argerr)
{
  if (result)
    VariantClear (result);
  if (excep)
    bzero (excep, sizeof *excep);
  if (argerr)
    *argerr = 0;

  try
    {
      for (lisp p = handlers (); consp (p); p = xcdr (p))
        {
          lisp q = xcar (p);
          if (consp (q) && fixnum_value (xcar (q)) == dispid)
            {
              lisp largs = Qnil;
              for (int i = dispparams->cArgs - 1; i >= 0; i--)
                largs = xcons (variant2obj (&dispparams->rgvarg[i]), largs);
              save_cursor_depth cursor_depth;
              protect_gc gcpro (largs);
              suppress_gc sgc;
              Ffuncall (xcdr (q), largs);
              multiple_value::clear ();
              return S_OK;
            }
        }
    }
  catch (nonlocal_jump &)
    {
    }

  multiple_value::clear ();
  return S_OK;
}

lisp
Fole_method (lisp lobj, lisp lprop, lisp args)
{
  return ole_invoke (lobj, lprop, args, Qnil, DISPATCH_METHOD | DISPATCH_PROPERTYGET);
}

lisp
Fole_method_star (lisp lobj, lisp lprop, lisp args, lisp named_args)
{
  return ole_invoke (lobj, lprop, args, named_args, DISPATCH_METHOD | DISPATCH_PROPERTYGET);
}

lisp
Fole_getprop (lisp lobj, lisp lprop, lisp args)
{
  return ole_invoke (lobj, lprop, args, Qnil, DISPATCH_PROPERTYGET);
}

lisp
Fole_putprop (lisp lobj, lisp lprop, lisp lvalue, lisp args)
{
  check_oledata (lobj);
  if (!xoledata_disp (lobj))
    FEprogram_error (Einvalid_idispatch);

  DISPID dispid = get_dispid (xoledata_disp (lobj), lprop);

  DISPPARAMS params;
  bzero (&params, sizeof params);
  params.cArgs = xlist_length (args) + 1;
  params.rgvarg = (VARIANT *)alloca (sizeof (VARIANT) * params.cArgs);
  bzero (params.rgvarg, sizeof (VARIANT) * params.cArgs);

  DISPID dispid_putprop = DISPID_PROPERTYPUT;
  params.cNamedArgs = 1;
  params.rgdispidNamedArgs = &dispid_putprop;

  obj2variant (lvalue, params.rgvarg[0]);
  objs2variant (args, params);

  EXCEPINFO excep;
  bzero (&excep, sizeof excep);
  UINT argerr = UINT (-1);

  HRESULT hr = xoledata_disp (lobj)->Invoke (dispid, IID_NULL, LOCALE_USER_DEFAULT,
                                             DISPATCH_PROPERTYPUT,
                                             &params, 0, &excep, &argerr);
  for (int i = params.cArgs - 1; i >= 0; i--)
    VariantClear (&params.rgvarg[i]);

  if (FAILED (hr))
    {
      cleanup (hr, excep);
      FEsimple_win32_error (hr, lprop);
    }

  return Qt;
}

lisp
Fole_create_object (lisp lprogid)
{
  check_string (lprogid);
  CLSID clsid;
  ole_error (CLSIDFromProgID (I2W (lprogid), &clsid), lprogid);

  lisp obj = make_oledata ();
  safe_com <IUnknown> unk;
  ole_error (CoCreateInstance (clsid, 0, CLSCTX_INPROC_SERVER | CLSCTX_LOCAL_SERVER,
                               IID_IUnknown, (void **)&unk),
             lprogid);

  IDispatch *disp = 0;
  ole_error (unk->QueryInterface (IID_IDispatch, (void **)&disp), lprogid);

  xoledata_disp (obj) = disp;
  return obj;
}

lisp
Fole_get_object (lisp lprogid)
{
  check_string (lprogid);
  CLSID clsid;
  ole_error (CLSIDFromProgID (I2W (lprogid), &clsid), lprogid);

  lisp obj = make_oledata ();
  safe_com <IUnknown> unk;
  ole_error (GetActiveObject (clsid, 0, &unk), lprogid);

  IDispatch *disp = 0;
  ole_error (unk->QueryInterface (IID_IDispatch, (void **)&disp), lprogid);

  xoledata_disp (obj) = disp;
  return obj;
}

lisp
Fole_create_event_sink (lisp lobj, lisp linterface, lisp tlbfile)
{
  check_oledata (lobj);
  if (!xoledata_disp (lobj))
    FEprogram_error (Einvalid_idispatch);

  safe_com <ITypeInfo> typeinfo;
  IID iid;

  if (linterface && linterface != Qnil)
    {
      check_string (linterface);
      if (!tlbfile || tlbfile == Qnil)
        get_interface_id (xoledata_disp (lobj), 0, I2W (linterface), typeinfo, iid);
      else
        {
          check_string (tlbfile);
          char path[PATH_MAX + 1];
          pathname2cstr (tlbfile, path);
          map_sl_to_backsl (path);
          USES_CONVERSION;
          get_interface_id (xoledata_disp (lobj), A2W (path), I2W (linterface), typeinfo, iid);
        }
    }
  else
    get_default_source (xoledata_disp (lobj), typeinfo, iid);

  safe_com <IConnectionPointContainer> conn;
  ole_error (xoledata_disp (lobj)->QueryInterface
             (IID_IConnectionPointContainer, (void **)&conn));

  safe_com <IConnectionPoint> conn_point;
  ole_error (conn->FindConnectionPoint (iid, &conn_point));

  safe_com <event_sink> sink (new event_sink (conn_point, typeinfo, iid));
  ole_error (sink->advise ());

  if (xoledata_event (lobj))
    xoledata_event (lobj)->Release ();

  xoledata_event (lobj) = sink;
  xoledata_event (lobj)->AddRef ();

  return Qt;
}

lisp
Fset_ole_event_handler (lisp lobj, lisp levent, lisp lfn)
{
  check_oledata (lobj);
  if (!xoledata_disp (lobj))
    FEprogram_error (Einvalid_idispatch);
  if (!xoledata_event (lobj))
    FEprogram_error (Eno_event_sink);

  check_string (levent);
  OLECHAR *name = I2W (levent);
  MEMBERID memid;
  ole_error (xoledata_event (lobj)->typeinfo ()->GetIDsOfNames (&name, 1, &memid));
  xoledata_event (lobj)->add_handler (make_fixnum (memid), lfn);
  return Qt;
}

lisp
Fole_enumerator_create (lisp lobj)
{
  check_oledata (lobj);
  if (!xoledata_disp (lobj))
    FEprogram_error (Einvalid_idispatch);

  DISPPARAMS params;
  bzero (&params, sizeof params);

  VARIANT result;
  VariantInit (&result);

  EXCEPINFO excep;
  bzero (&excep, sizeof excep);

  UINT argerr = UINT (-1);

  HRESULT hr = xoledata_disp (lobj)->Invoke (DISPID_NEWENUM, IID_NULL, LOCALE_USER_DEFAULT,
                                             DISPATCH_METHOD | DISPATCH_PROPERTYGET,
                                             &params, &result, &excep, &argerr);
  if (FAILED (hr))
    {
      VariantClear (&result);
      FEprogram_error (Ecreate_ienum_failed);
    }

  IEnumVARIANT *p = 0;
  if (V_VT (&result) == VT_UNKNOWN)
    hr = V_UNKNOWN (&result)->QueryInterface (IID_IEnumVARIANT, (void **)&p);
  else if (V_VT (&result) == VT_DISPATCH)
    hr = V_DISPATCH (&result)->QueryInterface (IID_IEnumVARIANT, (void **)&p);

  VariantClear (&result);
  if (FAILED (hr) || !p)
    FEprogram_error (Ecreate_ienum_failed);

  p->Reset ();
  lisp obj = make_oledata ();
  xoledata_enumerator (obj) = p;

  return obj;
}

lisp
Fole_enumerator_next (lisp lobj)
{
  check_oledata (lobj);
  if (!xoledata_enumerator (lobj))
    FEprogram_error (Einvalid_ienum_variant);

  VARIANT v;
  VariantInit (&v);
  safe_variant sv (v);
  u_long n = 0;
  ole_error (xoledata_enumerator (lobj)->Next (1, &v, &n));
  if (n != 1)
    return Kempty;
  return variant2obj (&v);
}

lisp
Fole_enumerator_reset (lisp lobj)
{
  check_oledata (lobj);
  if (!xoledata_enumerator (lobj))
    FEprogram_error (Einvalid_ienum_variant);
  HRESULT hr = xoledata_enumerator (lobj)->Reset ();
  ole_error (hr);
  return boole (hr == S_OK);
}

lisp
Fole_enumerator_skip (lisp lobj, lisp ln)
{
  check_oledata (lobj);
  if (!xoledata_enumerator (lobj))
    FEprogram_error (Einvalid_ienum_variant);
  long n = ln || ln == Qnil ? 1 : fixnum_value (ln);
  HRESULT hr = xoledata_enumerator (lobj)->Skip (n);
  ole_error (hr);
  return boole (hr == S_OK);
}


/*
DevStudio/SharedIDE/bin/DEVSHL.DLL
DevStudio/SharedIDE/bin/ide/DEVDBG.PKG

(setq app (ole-get-object "MSDEV.Application"))
(ole-create-event-sink app "IDispApplicationEvents" "DevStudio/SharedIDE/bin/DEVSHL.DLL")
(set-ole-event-handler app "WindowActivate" #'(lambda (&rest x)))

(require "ole")
(setq deb #{app.debugger})
(ole-create-event-sink deb "IDispDebuggerEvents" "DevStudio/SharedIDE/bin/ide/DEVDBG.PKG")

 */
