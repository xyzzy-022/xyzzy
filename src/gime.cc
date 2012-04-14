#include "stdafx.h"
#include "gime.h"
#ifndef DECLSPEC_UUID
# define DECLSPEC_UUID(X)
#endif
#ifndef MIDL_INTERFACE
# define MIDL_INTERFACE(X) interface
#endif
#ifdef HAVE_DIMM_H
/* Dimm.h は SDK から持ってくること。SDK を持っていない場合は
   Makefile の -DHAVE_DIMM_H をコメントアウトすればコンパイル
   はできる。*/
#include <Dimm.h>
#endif

GlobalIME::GlobalIME ()
     : gi_app (0), gi_pump (0)
{
  ImmGetPropertyProc = (IMMGETPROPERTYPROC)GetProcAddress (GetModuleHandle ("imm32.dll"),
                                                           "ImmGetProperty");
}

HRESULT
GlobalIME::init (ATOM *atoms, int natoms)
{
#ifdef HAVE_DIMM_H
  HRESULT hr = CoCreateInstance (CLSID_CActiveIMM, 0, CLSCTX_INPROC_SERVER,
                                 IID_IActiveIMMApp, (void **)&gi_app);
  if (hr != S_OK)
    return hr;

  hr = gi_app->QueryInterface (IID_IActiveIMMMessagePumpOwner, (void **)&gi_pump);
  if (hr != S_OK)
    return hr;

  hr = gi_app->Activate (1);
  if (hr != S_OK)
    return hr;

  hr = gi_pump->Start ();
  if (hr != S_OK)
    return hr;

  return gi_app->FilterClientWindows (atoms, natoms);
#else /* not HAVE_DIMM_H */
  return E_NOTIMPL;
#endif /* not HAVE_DIMM_H */
}

void
GlobalIME::cleanup ()
{
#ifdef HAVE_DIMM_H
  if (gi_pump)
    {
      gi_pump->End ();
      gi_pump->Release ();
      gi_pump = 0;
    }

  if (gi_app)
    {
      gi_app->Deactivate ();
      gi_app->Release ();
      gi_app = 0;
    }
#endif
}

int
GlobalIME::enable (ATOM *atoms, int natoms)
{
  if (gi_app)
    return 1;
  HRESULT hr = init (atoms, natoms);
  if (hr == S_OK)
    return 1;
  cleanup ();
  SetLastError (hr);
  return 0;
}

LRESULT
GlobalIME::DefWindowProc (HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
#ifdef HAVE_DIMM_H
  LRESULT lr;
  if (gi_app && gi_app->OnDefWindowProc (hwnd, msg, wp, lp, &lr) == S_OK)
    return lr;
#endif /* HAVE_DIMM_H */
  return ::DefWindowProc (hwnd, msg, wp, lp);
}

BOOL
GlobalIME::TranslateMessage (const MSG *msg)
{
#ifdef HAVE_DIMM_H
  if (gi_pump && gi_pump->OnTranslateMessage (msg) == S_OK)
    return 0;
#endif /* HAVE_DIMM_H */
  return ::TranslateMessage (msg);
}

HIMC
GlobalIME::ImmGetContext (HWND hwnd)
{
#ifdef HAVE_DIMM_H
  HIMC himc;
  if (gi_app)
    return gi_app->GetContext (hwnd, &himc) == S_OK ? himc : 0;
#endif /* HAVE_DIMM_H */
  return ::ImmGetContext (hwnd);
}

BOOL
GlobalIME::ImmReleaseContext (HWND hwnd, HIMC himc)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->ReleaseContext (hwnd, himc) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmReleaseContext (hwnd, himc);
}

BOOL
GlobalIME::ImmSetCompositionWindow (HIMC himc, COMPOSITIONFORM *cf)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->SetCompositionWindow (himc, cf) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmSetCompositionWindow (himc, cf);
}

BOOL
GlobalIME::ImmSetCompositionFont (HIMC himc, LOGFONT *lf)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->SetCompositionFontA (himc, lf) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmSetCompositionFont (himc, lf);
}

LONG
GlobalIME::ImmGetCompositionString (HIMC himc, DWORD index, void *buf, DWORD bufl)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    {
      LONG len;
      return gi_app->GetCompositionStringA (himc, index, bufl, &len, buf) == S_OK ? len : 0;
    }
#endif /* HAVE_DIMM_H */
  return ::ImmGetCompositionString (himc, index, buf, bufl);
}

LONG
GlobalIME::ImmGetCompositionStringW (HIMC himc, DWORD index, void *buf, DWORD bufl)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    {
      LONG len;
      return gi_app->GetCompositionStringW (himc, index, bufl, &len, buf) == S_OK ? len : 0;
    }
#endif /* HAVE_DIMM_H */
  return ::ImmGetCompositionStringW (himc, index, buf, bufl);
}

BOOL
GlobalIME::ImmGetOpenStatus (HIMC himc)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return !BOOL (gi_app->GetOpenStatus (himc)); // マニュアルと逆じゃんボケ
#endif /* HAVE_DIMM_H */
  return ::ImmGetOpenStatus (himc);
}

BOOL
GlobalIME::ImmSetOpenStatus (HIMC himc, BOOL f)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->SetOpenStatus (himc, f) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmSetOpenStatus (himc, f);
}

BOOL
GlobalIME::ImmSetCompositionString (HIMC himc, DWORD index, void *comp,
                                    DWORD compl, void *read, DWORD readl)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->SetCompositionStringA (himc, index, comp,
                                          compl, read, readl) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmSetCompositionString (himc, index, comp, compl, read, readl);
}

BOOL
GlobalIME::ImmConfigureIME (HKL hkl, HWND hwnd, DWORD mode, REGISTERWORD *data)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    return gi_app->ConfigureIMEA (hkl, hwnd, mode, data) == S_OK;
#endif /* HAVE_DIMM_H */
  return ::ImmConfigureIME (hkl, hwnd, mode, data);
}

UINT
GlobalIME::ImmGetVirtualKey (HWND hwnd)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    {
      UINT vk;
      return gi_app->GetVirtualKey (hwnd, &vk) == S_OK ? vk : UINT (-1);
    }
#endif /* HAVE_DIMM_H */
  return ::ImmGetVirtualKey (hwnd);
}

UINT
GlobalIME::ImmGetDescription (HKL hkl, LPTSTR buf, UINT size)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    {
      UINT n;
      return gi_app->GetDescriptionA (hkl, size, buf, &n) == S_OK ? n : 0;
    }
#endif /* HAVE_DIMM_H */
  return ::ImmGetDescription (hkl, buf, size);
}

DWORD
GlobalIME::ImmGetProperty (HKL hkl, DWORD index)
{
#ifdef HAVE_DIMM_H
  if (gi_app)
    {
      DWORD r;
      return gi_app->GetProperty (hkl, index, &r) == S_OK ? r : 0;
    }
#endif /* HAVE_DIMM_H */
  return ImmGetPropertyProc ? (*ImmGetPropertyProc)(hkl, index) : 0;
}
