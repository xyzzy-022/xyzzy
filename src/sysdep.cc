#include "stdafx.h"
#include "sysdep.h"
#include "vfs.h"
#include "resource.h"

Sysdep sysdep;

Sysdep::Sysdep ()
{
  os_ver.dwOSVersionInfoSize = sizeof os_ver;
  GetVersionEx (&os_ver);

  init_wintype ();
  init_machine_type ();
  init_process_type ();

  GetCurrentDirectory (sizeof curdir, curdir);
  if (*curdir == '\\')
    {
      GetWindowsDirectory (curdir, sizeof curdir);
      WINFS::SetCurrentDirectory (curdir);
    }

  DWORD len = sizeof host_name;
  if (!GetComputerName (host_name, &len))
    *host_name = 0;

  process_id = GetCurrentProcessId ();

  hbr_white = GetStockObject (WHITE_BRUSH);
  hbr_black = GetStockObject (BLACK_BRUSH);
  hpen_white = GetStockObject (WHITE_PEN);
  hpen_black = GetStockObject (BLACK_PEN);

  perf_counter_present_p = QueryPerformanceFrequency ((LARGE_INTEGER *)&perf_freq);

  comctl32_version = get_dll_version ("comctl32.dll");
  shell32_version = get_dll_version ("shell32.dll");

  load_colors ();
  load_settings ();
  load_cursors ();
  hcur_current = hcur_arrow;

  hfont_ui = 0;
  hfont_ui90 = 0;
  hfont_ui270 = 0;

  LOGFONT lf;
  memset (&lf, 0, sizeof lf);
  lf.lfHeight = 12;
  strcpy (lf.lfFaceName, "Arial");
  hfont_ruler = CreateFontIndirect (&lf);
  HDC hdc = GetDC (0);
  HGDIOBJ of = SelectObject (hdc, hfont_ruler);
  GetTextExtentPoint32 (hdc, "0", 1, &ruler_ext);
  SelectObject (hdc, of);
}

Sysdep::~Sysdep ()
{
  if (hfont_ui)
    DeleteObject (hfont_ui);
  if (hfont_ui90)
    DeleteObject (hfont_ui90);
  if (hfont_ui270)
    DeleteObject (hfont_ui270);
  if (hfont_ruler)
    DeleteObject (hfont_ruler);
}

void
Sysdep::init_wintype ()
{
  switch (sysdep.os_ver.dwPlatformId)
    {
    case VER_PLATFORM_WIN32s:
      wintype = WINTYPE_WIN32S;
      windows_name = windows_short_name = "32s";
      break;

    case VER_PLATFORM_WIN32_WINDOWS:
      if (version () >= WIN98_VERSION)
        {
          wintype = WINTYPE_WINDOWS_98;
          if (version () >= WINME_VERSION)
            {
              windows_name = "Me";
              windows_short_name = "wme";
            }
          else
            {
              windows_name = "98";
              windows_short_name = "w98";
            }
        }
      else
        {
          wintype = WINTYPE_WINDOWS_95;
          windows_name = "95";
          windows_short_name = "w95";
        }
      break;

    case VER_PLATFORM_WIN32_NT:
      if (Win5p ())
        {
          wintype = WINTYPE_WINDOWS_NT5;
          if (version () >= WINXP_VERSION)
            {
              windows_name = "XP";
              windows_short_name = "wxp";
            }
          else
            {
              windows_name = "2000";
              windows_short_name = "w2k";
            }
        }
      else if (Win6p ())
        {
          wintype = WINTYPE_WINDOWS_NT6;
          // 設定ファイルのパス (user-config-path) が変わるため wxp のままとする
          windows_short_name = "wxp";
          if (version () >= WIN7_VERSION)
              windows_name = "7";
          else if (version () >= WIN8_VERSION)
              windows_name = "8";
          else
              windows_name = "Vista";
        }
      else
        {
          wintype = WINTYPE_WINDOWS_NT;
          windows_name = "NT";
          windows_short_name = "wnt";
        }
      break;

    default:
      wintype = WINTYPE_UNKNOWN;
      windows_name = "(unknown)";
      windows_short_name = "unk";
      break;
    }
}

void
Sysdep::init_machine_type ()
{
  SYSTEM_INFO info;
  GetNativeSystemInfo(&info);
  switch (info.wProcessorArchitecture)
    {
    case PROCESSOR_ARCHITECTURE_INTEL:
      machine_type = MACHINETYPE_X86;
      break;
    case PROCESSOR_ARCHITECTURE_AMD64:
      machine_type = MACHINETYPE_X64;
      break;
    case PROCESSOR_ARCHITECTURE_IA64:
      machine_type = MACHINETYPE_IA64;
      break;
    case PROCESSOR_ARCHITECTURE_UNKNOWN:
      machine_type = MACHINETYPE_UNKNOWN;
      break;
    }
}

void
Sysdep::init_process_type ()
{
  typedef BOOL (WINAPI *ISWOW64PROCESS) (HANDLE, PBOOL);
  ISWOW64PROCESS IsWow64Process = (ISWOW64PROCESS)GetProcAddress (GetModuleHandle ("kernel32"),
                                                                  "IsWow64Process");
  BOOL isWow64 = FALSE;
  if (!IsWow64Process || !IsWow64Process (GetCurrentProcess (), &isWow64))
    {
      process_type = PROCESSTYPE_UNKNOWN;
      return;
    }

  if (isWow64)
    process_type = PROCESSTYPE_WOW64;
  else
    process_type = PROCESSTYPE_NATIVE;
}

HFONT
Sysdep::create_ui_font (int e)
{
  LOGFONT lf;
  bzero (&lf, sizeof lf);
  lf.lfHeight = 12;
  lf.lfCharSet = SHIFTJIS_CHARSET;
  lf.lfEscapement = e;
  strcpy (lf.lfFaceName, "MS UI Gothic");
  return CreateFontIndirect (&lf);
}

HFONT
Sysdep::ui_font ()
{
  if (!hfont_ui)
    hfont_ui = create_ui_font (0);
  return hfont_ui;
}

HFONT
Sysdep::ui_font90 ()
{
  if (!hfont_ui90)
    hfont_ui90 = create_ui_font (900);
  return hfont_ui90;
}

HFONT
Sysdep::ui_font270 ()
{
  if (!hfont_ui270)
    hfont_ui270 = create_ui_font (2700);
  return hfont_ui270;
}

void
Sysdep::load_colors ()
{
  btn_text = GetSysColor (COLOR_BTNTEXT);
  btn_highlight = GetSysColor (COLOR_BTNHIGHLIGHT);
  btn_shadow = GetSysColor (COLOR_BTNSHADOW);
  btn_face = GetSysColor (COLOR_BTNFACE);
  window_text = GetSysColor (COLOR_WINDOWTEXT);
  window = GetSysColor (COLOR_WINDOW);
  gray_text = GetSysColor (COLOR_GRAYTEXT);
  highlight_text = GetSysColor (COLOR_HIGHLIGHTTEXT);
  highlight = GetSysColor (COLOR_HIGHLIGHT);
}

void
Sysdep::load_settings ()
{
  border.cx = GetSystemMetrics (SM_CXBORDER);
  border.cy = GetSystemMetrics (SM_CYBORDER);

  dblclk.cx = GetSystemMetrics (SM_CXDOUBLECLK);
  dblclk.cy = GetSystemMetrics (SM_CYDOUBLECLK);

  vscroll = GetSystemMetrics (SM_CXVSCROLL);
  hscroll = GetSystemMetrics (SM_CYHSCROLL);

  if (Win4p ())
    {
      edge.cx = GetSystemMetrics (SM_CXEDGE) * 2;
      edge.cy = GetSystemMetrics (SM_CYEDGE) * 2;
    }
  else
    {
      edge.cx = 0;
      edge.cy = 0;
    }
}

void
Sysdep::load_cursors ()
{
  HINSTANCE hinst = GetModuleHandle (0);
  hcur_arrow = LoadCursor (0, IDC_ARROW);
  hcur_revarrow = LoadCursor (hinst, MAKEINTRESOURCE (IDC_REVARROW));
  hcur_ibeam = LoadCursor (0, IDC_IBEAM);
  hcur_wait = LoadCursor (0, IDC_WAIT);
  hcur_sizewe = LoadCursor (hinst, MAKEINTRESOURCE (IDC_SPLITH));
  hcur_sizens = LoadCursor (hinst, MAKEINTRESOURCE (IDC_SPLITV));
}

#ifndef DLLVER_PLATFORM_WINDOWS

typedef struct _DllVersionInfo
{
  DWORD cbSize;
  DWORD dwMajorVersion;
  DWORD dwMinorVersion;
  DWORD dwBuildNumber;
  DWORD dwPlatformID;
}
  DLLVERSIONINFO;

#define DLLVER_PLATFORM_WINDOWS 0x00000001
#define DLLVER_PLATFORM_NT      0x00000002

typedef HRESULT (CALLBACK *DLLGETVERSIONPROC)(DLLVERSIONINFO *);

#endif /* not DLLVER_PLATFORM_WINDOWS */

DWORD
Sysdep::get_dll_version (const char *name)
{
  HINSTANCE hinst = GetModuleHandle (name);
  if (!hinst)
    return 0;

  DLLGETVERSIONPROC DllGetVersion =
    DLLGETVERSIONPROC (GetProcAddress (hinst, "DllGetVersion"));

  if (!DllGetVersion)
    return 0;

  DLLVERSIONINFO dvi = {sizeof dvi};
  if (SUCCEEDED (DllGetVersion (&dvi)))
    return PACK_VERSION (dvi.dwMajorVersion, dvi.dwMinorVersion);

  return 0;
}
