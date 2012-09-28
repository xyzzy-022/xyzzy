#ifndef _sysdep_h_
# define _sysdep_h_

# include "cdecl.h"

struct Sysdep
{
  Sysdep ();
  ~Sysdep ();

  OSVERSIONINFO os_ver;

  SIZE border;
  SIZE dblclk;
  SIZE edge;
  UINT vscroll;
  UINT hscroll;

  COLORREF btn_text;
  COLORREF btn_highlight;
  COLORREF btn_shadow;
  COLORREF btn_face;
  COLORREF window_text;
  COLORREF gray_text;
  COLORREF highlight_text;
  COLORREF highlight;
  COLORREF window;

  HGDIOBJ hbr_white;
  HGDIOBJ hbr_black;
  HGDIOBJ hpen_white;
  HGDIOBJ hpen_black;

  HCURSOR hcur_arrow;
  HCURSOR hcur_revarrow;
  HCURSOR hcur_ibeam;
  HCURSOR hcur_wait;
  HCURSOR hcur_sizewe;
  HCURSOR hcur_sizens;

  HCURSOR hcur_current;

  enum windows_type
    {
      WINTYPE_UNKNOWN,
      WINTYPE_WIN32S,
      WINTYPE_WINDOWS_95,
      WINTYPE_WINDOWS_98,
      WINTYPE_WINDOWS_NT,
      WINTYPE_WINDOWS_NT5,
      WINTYPE_WINDOWS_NT6,
    };

  windows_type wintype;
  const char *windows_name;
  const char *windows_short_name;

  HFONT hfont_ruler;
  SIZE ruler_ext;

  enum machine_type
    {
      MACHINETYPE_UNKNOWN,
      MACHINETYPE_X86,
      MACHINETYPE_X64,
      MACHINETYPE_IA64,
    };

  machine_type machine_type;

  enum process_type
    {
      PROCESSTYPE_UNKNOWN,
      PROCESSTYPE_NATIVE,
      PROCESSTYPE_WOW64,
    };

  process_type process_type;
private:
  HFONT hfont_ui;
  HFONT hfont_ui90;
  HFONT hfont_ui270;
  static HFONT create_ui_font (int);
  void init_wintype ();
  void init_machine_type ();
  void init_process_type ();
public:
  HFONT ui_font ();
  HFONT ui_font90 ();
  HFONT ui_font270 ();

  char curdir[PATH_MAX];
  char host_name[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD process_id;

  int64_t perf_freq;
  int perf_counter_present_p;

  DWORD comctl32_version;
  DWORD shell32_version;

  void load_colors ();
  void load_settings ();
  void load_cursors ();

  static DWORD get_dll_version (const char *);

#define PACK_VERSION(MAJ, MIN) MAKELONG ((MIN), (MAJ))
  enum
    {
      WIN98_VERSION = PACK_VERSION (4, 10),
      WINME_VERSION = PACK_VERSION (4, 90),
      WINXP_VERSION = PACK_VERSION (5, 1),
      WIN7_VERSION = PACK_VERSION (6, 1),
      WIN8_VERSION = PACK_VERSION (6, 2),
    };
  DWORD version () const
    {return PACK_VERSION (os_ver.dwMajorVersion, os_ver.dwMinorVersion);}

  int Win4p () const
    {return os_ver.dwMajorVersion >= 4;}
  int Win5p () const
    {return os_ver.dwMajorVersion >= 5;}
  int Win6p () const
    {return os_ver.dwMajorVersion >= 6;}
  int Win6_1p () const
    {return os_ver.dwMajorVersion >= 6 && os_ver.dwMinorVersion >= 1;}
  int Win95p () const
    {return wintype == WINTYPE_WINDOWS_95;}
  int Win98p () const
    {return wintype == WINTYPE_WINDOWS_98;}
  int WinNTp () const
    {return os_ver.dwPlatformId == VER_PLATFORM_WIN32_NT;}
};


extern Sysdep sysdep;

#endif
