#ifndef _gime_h_
#define _gime_h_

interface IActiveIMMApp;
interface IActiveIMMMessagePumpOwner;

class GlobalIME
{
protected:
  IActiveIMMApp *gi_app;
  IActiveIMMMessagePumpOwner *gi_pump;

  HRESULT init (ATOM *, int);
  void cleanup ();

private:
  GlobalIME (const GlobalIME &);
  void operator = (const GlobalIME &);

  typedef DWORD (WINAPI *IMMGETPROPERTYPROC)(HKL, DWORD);

  IMMGETPROPERTYPROC ImmGetPropertyProc;
public:
  GlobalIME ();
  int enable (ATOM *, int);
  void disable () {cleanup ();}
  int enable_p () const {return int (gi_app);}
  LRESULT DefWindowProc (HWND, UINT, WPARAM, LPARAM);
  BOOL TranslateMessage (const MSG *);
  LONG ImmGetCompositionString (HIMC, DWORD, void *, DWORD);
  LONG ImmGetCompositionStringW (HIMC, DWORD, void *, DWORD);
  HIMC ImmGetContext (HWND);
  BOOL ImmGetOpenStatus (HIMC);
  BOOL ImmReleaseContext (HWND, HIMC);
  BOOL ImmSetCompositionFont (HIMC, LOGFONT *);
  BOOL ImmSetCompositionWindow (HIMC, COMPOSITIONFORM *);
  BOOL ImmSetOpenStatus (HIMC, BOOL);
  BOOL ImmSetCompositionString (HIMC, DWORD, void *, DWORD, void *, DWORD);
  BOOL ImmConfigureIME (HKL, HWND, DWORD, REGISTERWORD *);
  UINT ImmGetVirtualKey (HWND);
  UINT ImmGetDescription (HKL, LPTSTR, UINT);
  DWORD ImmGetProperty (HKL, DWORD);
};

#endif /* _gime_h_ */
