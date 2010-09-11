#include "ed.h"
#include "environ.h"
#include "conf.h"
#include "fnkey.h"

const char Registry::base[] = "Software\\Free Software\\Xyzzy\\";
const char Registry::Settings[] = "Settings";

#define ALLOC_SUBKEY(VAR, SUBKEY) \
  char *(VAR) = (char *)alloca (sizeof base + strlen (SUBKEY)); \
  memcpy ((VAR), base, sizeof base - 1), \
  strcpy ((VAR) + sizeof base - 1, (SUBKEY))

void
ReadRegistry::open_local (const char *subkey)
{
  ALLOC_SUBKEY (b, subkey);
  if (RegOpenKeyEx (HKEY_CURRENT_USER, b, 0, KEY_READ, &hkey) != ERROR_SUCCESS)
    hkey = 0;
}

ReadRegistry::ReadRegistry (HKEY h, const char *subkey)
{
  if (!h)
    open_local (subkey);
  else if (RegOpenKeyEx (h, subkey, 0, KEY_READ, &hkey) != ERROR_SUCCESS)
    hkey = 0;
}

WriteRegistry::WriteRegistry (const char *subkey)
{
  ALLOC_SUBKEY (b, subkey);
  DWORD x;
  DWORD e = RegCreateKeyEx (HKEY_CURRENT_USER, b, 0, 0, REG_OPTION_NON_VOLATILE,
                            KEY_WRITE, 0, &hkey, &x);
  if (e != ERROR_SUCCESS)
    {
      hkey = 0;
      SetLastError (e);
    }
}

int
ReadRegistry::get (const char *key, void *buf, DWORD size, DWORD req) const
{
  assert (!fail ());
  DWORD type;
  return (RegQueryValueEx (hkey, (char *)key, 0, &type,
                           (BYTE *)buf, &size) == ERROR_SUCCESS
          && type == req) ? size : -1;
}

int
ReadRegistry::query (const char *key, DWORD *type) const
{
  assert (!fail ());
  DWORD size = 0;
  if (RegQueryValueEx (hkey, (char *)key, 0, type, 0, &size) == ERROR_SUCCESS)
    return size;
  return -1;
}

int
WriteRegistry::set (const char *key, DWORD type, const void *buf, int size) const
{
  assert (!fail ());
  DWORD e = RegSetValueEx (hkey, key, 0, type, (BYTE *)buf, size);
  if (e == ERROR_SUCCESS)
    return 1;
  SetLastError (e);
  return 0;
}

int
WriteRegistry::remove (const char *key) const
{
  assert (!fail ());
  DWORD e = RegDeleteValue (hkey, key);
  if (e == ERROR_SUCCESS || e == ERROR_FILE_NOT_FOUND)
    return 1;
  SetLastError (e);
  return 0;
}

lisp
Fwrite_registry (lisp lsection, lisp lkey, lisp val)
{
  lsection = Fstring (lsection);
  char *section = (char *)alloca (w2sl (lsection) + 1);
  w2s (section, lsection);

  char *key;
  if (lkey == Qnil)
    key = 0;
  else
    {
      lkey = Fstring (lkey);
      key = (char *)alloca (w2sl (lkey) + 1);
      w2s (key, lkey);
    }

  if (val != Qnil && !stringp (val) && !fixnump (val))
    FEtype_error (val, xsymbol_value (Qor_string_integer));

  WriteRegistry r (section);
  if (r.fail ())
    FEsimple_win32_error (GetLastError (), lsection);

  if (val == Qnil)
    {
      if (!r.remove (key))
        FEsimple_win32_error (GetLastError (), lkey);
      return Qt;
    }

  if (stringp (val))
    {
      int l = w2sl (val);
      char *b = (char *)alloca (l + 1);
      w2s (b, val);
      if (!r.set (key, b, l + 1))
        FEsimple_win32_error (GetLastError (), lkey);
      return Qt;
    }

  if (!r.set (key, fixnum_value (val)))
    FEsimple_win32_error (GetLastError (), lkey);
  return Qt;
}

lisp
Fwrite_registry_literally (lisp lsection, lisp lkey, lisp val)
{
  lsection = Fstring (lsection);
  char *section = (char *)alloca (w2sl (lsection) + 1);
  w2s (section, lsection);

  char *key;
  if (lkey == Qnil)
    key = 0;
  else
    {
      lkey = Fstring (lkey);
      key = (char *)alloca (w2sl (lkey) + 1);
      w2s (key, lkey);
    }

  if (val != Qnil)
    check_string (val);

  WriteRegistry r (section);
  if (r.fail ())
    FEsimple_win32_error (GetLastError (), lsection);

  if (val == Qnil)
    {
      if (!r.remove (key))
        FEsimple_win32_error (GetLastError (), lkey);
    }
  else
    {
      if (!r.set (key, (const void *)xstring_contents (val),
                  sizeof (Char) * xstring_length (val)))
        FEsimple_win32_error (GetLastError (), lkey);
    }
  return Qt;
}

static HKEY
check_root (lisp lroot)
{
  if (!lroot || lroot == Qnil)
    return 0;
  if (lroot == Kclasses_root)
    return HKEY_CLASSES_ROOT;
  if (lroot == Kcurrent_user)
    return HKEY_CURRENT_USER;
  if (lroot == Klocal_machine)
    return HKEY_LOCAL_MACHINE;
  if (lroot == Kusers)
    return HKEY_USERS;
  return 0;
}

lisp
Fread_registry (lisp lsection, lisp lkey, lisp lroot)
{
  lsection = Fstring (lsection);
  char *section = (char *)alloca (w2sl (lsection) + 1);
  w2s (section, lsection);

  char *key;
  if (lkey == Qnil)
    key = 0;
  else
    {
      lkey = Fstring (lkey);
      key = (char *)alloca (w2sl (lkey) + 1);
      w2s (key, lkey);
    }

  ReadRegistry r (check_root (lroot), section);
  if (r.fail ())
    return Qnil;

  DWORD type;
  int l = r.query (key, &type);
  if (l < 0)
    return Qnil;

  switch (type)
    {
    case REG_DWORD:
      {
        int x;
        if (!r.get (key, &x))
          FEsimple_win32_error (GetLastError (), lkey);
        return make_fixnum (x);
      }

    case REG_SZ:
      {
        char *b = (char *)alloca (l + 1);
        if (!r.get (key, b, l, type))
          FEsimple_win32_error (GetLastError (), lkey);
        return make_string (b);
      }

    case REG_EXPAND_SZ:
      {
        char *b = (char *)alloca (l + 1);
        if (!r.get (key, b, l, type))
          FEsimple_win32_error (GetLastError (), lkey);
        l = ExpandEnvironmentStrings (b, 0, 0);
        if (!l)
          FEsimple_win32_error (GetLastError (), lkey);
        char *b2 = (char *)alloca (l + 1);
        if (!ExpandEnvironmentStrings (b, b2, l + 1))
          FEsimple_win32_error (GetLastError (), lkey);
        return make_string (b2);
      }

    case REG_MULTI_SZ:
      {
        char *b = (char *)alloca (l + 1);
        if (!r.get (key, b, l, type))
          FEsimple_win32_error (GetLastError (), lkey);
        lisp p = Qnil;
        do
          {
            p = xcons (make_string (b), p);
            b += strlen (b) + 1;
          }
        while (*b);
        return Fnreverse (p);
      }

    case REG_BINARY:
      if (l && !(l % sizeof (Char)))
        {
          lisp p = make_string (l / sizeof (Char));
          if (!r.get (key, (void *)xstring_contents (p), l))
            FEsimple_win32_error (GetLastError (), lkey);
          return p;
        }
      return Qnil;

    default:
      return Qnil;
    }
}

lisp
Flist_registry_key (lisp lsection, lisp lroot)
{
  lsection = Fstring (lsection);
  char *section = (char *)alloca (w2sl (lsection) + 1);
  w2s (section, lsection);

  EnumRegistry r (check_root (lroot), section);
  if (r.fail ())
    return Qnil;

  lisp p = Qnil;
  for (int i = 0;; i++)
    {
      char name[1024];
      DWORD namel = sizeof name;
      FILETIME ft;
      int e = RegEnumKeyEx (r, i, name, &namel, 0, 0, 0, &ft);
      if (e == ERROR_SUCCESS)
        p = xcons (make_string (name), p);
      else
        break;
    }
  return p;
}

lisp
Fsi_delete_registry_tree ()
{
  reg_delete_tree ();
  return Qnil;
}

lisp
Fget_decoded_time ()
{
  SYSTEMTIME s;
  GetLocalTime (&s);
  multiple_value::value (1) = make_fixnum (s.wMinute);
  multiple_value::value (2) = make_fixnum (s.wHour);
  multiple_value::value (3) = make_fixnum (s.wDay);
  multiple_value::value (4) = make_fixnum (s.wMonth);
  multiple_value::value (5) = make_fixnum (s.wYear);
  multiple_value::value (6) = make_fixnum ((s.wDayOfWeek + 6) % 7);

  TIME_ZONE_INFORMATION t;
  switch (GetTimeZoneInformation (&t))
    {
    case TIME_ZONE_ID_UNKNOWN:
    case TIME_ZONE_ID_STANDARD:
      multiple_value::value (7) = Qnil;
      multiple_value::value (8) = make_fixnum (t.Bias / 60);
      break;

    case TIME_ZONE_ID_DAYLIGHT:
      multiple_value::value (7) = Qt;
      multiple_value::value (8) = make_fixnum (t.Bias / 60);
      break;

    default:
      multiple_value::value (7) = Qnil;
      multiple_value::value (8) = make_fixnum (0);
      break;
    }

  multiple_value::count () = 9;
  return make_fixnum (s.wSecond);
}

#define BASE_YEAR 1900

static inline int
count_leap_years (int y)
{
  return y / 4 - y / 100 + y / 400;
}

static inline int
leap_years_since_base_year (int year)
{
  return count_leap_years (year) - count_leap_years (BASE_YEAR);
}

static inline int
leap_year_p (int y)
{
  return !(y % 4) && (y % 100 || !(y % 400));
}

lisp
decoded_time_to_universal_time (int year, int mon, int day,
                                int hour, int min, int sec, int timezone)
{
  static const int days_of_month[] =
    {0, -1, 30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333,};
  int leap_years = leap_years_since_base_year (year);
  day += days_of_month[mon];
  if (mon <= 2 && leap_year_p (year))
    day--;
  year -= BASE_YEAR;
  bignum_rep_long ry (year);
  bignum_rep_long ly (leap_years);
  safe_bignum_rep r (multiply (0, &ry, 365 * 86400L));
  safe_bignum_rep r2 (multiply (0, &ly, 86400L));
  r = add (r, r, r2, 0);
  r = add (r, r, (day * 86400L + hour * 3600 + min * 60 + sec + timezone), 0);
  return make_integer (r.release ());
}

#define FILETIME_UNIT_PER_SECOND 10000000
#define FILETIME_UTC_BASE   9435484800i64   // FileTime (1900/1/1 0:0:0)

lisp
file_time_to_universal_time (const FILETIME &ft)
{
  __int64 i = *(__int64 *)&ft;
  i = i / FILETIME_UNIT_PER_SECOND - FILETIME_UTC_BASE;
  return make_integer (*(large_int *)&i);
}

lisp
Fget_universal_time ()
{
  SYSTEMTIME st;
  GetSystemTime (&st);
  return decoded_time_to_universal_time (st.wYear, st.wMonth, st.wDay,
                                         st.wHour, st.wMinute, st.wSecond, 0);
}

static int
get_timezone (lisp ltimezone, int *daylight)
{
  *daylight = 0;
  if (!ltimezone || ltimezone == Qnil)
    {
      TIME_ZONE_INFORMATION t;
      switch (GetTimeZoneInformation (&t))
        {
        case TIME_ZONE_ID_UNKNOWN:
        case TIME_ZONE_ID_STANDARD:
          return t.Bias * 60;

        case TIME_ZONE_ID_DAYLIGHT:
          *daylight = -3600;
          return t.Bias * 60;

        default:
          return 0;
        }
    }
  else
    {
      long timezone;
      if (safe_fixnum_value (ltimezone, &timezone))
        {
          if (timezone < -24 || timezone > 24)
            FErange_error (ltimezone);
          timezone *= 3600;
        }
      else
        {
          if (!rationalp (ltimezone))
            FEtype_error (ltimezone, Qrational);
          if (bignump (ltimezone))
            FErange_error (ltimezone);
          lisp t = number_multiply (ltimezone, make_fixnum (3600));
          if (!integerp (t))
            FEsimple_error (Etimezone_is_integral_multiple_of_1_3600);
          if (bignump (t))
            FErange_error (ltimezone);
          timezone = fixnum_value (t);
          if (timezone < -24 * 3600 || timezone > 24 * 3600)
            FErange_error (ltimezone);
        }
      return timezone;
    }
}

#define SECONDS_PER_DAY 86400

void
decode_universal_time (lisp lutc, decoded_time *dt)
{
  bignum_rep_long utcl;
  safe_bignum_rep utc (add (0, coerce_to_bignum_rep (lutc, &utcl),
                            long (dt->timezone + dt->daylight), 1));
  bignum_rep_long spd (u_long (SECONDS_PER_DAY));
  bignum_rep *q, *r;
  truncate (q, r, utc, &spd);
  safe_bignum_rep qq (q), rr (r);
  u_long t = r->to_ulong ();
  dt->hour = t / 3600;
  dt->min = t / 60 % 60;
  dt->sec = t % 60;
  dt->dow = remainder (q, 7);

  bignum_rep_long dpy (u_long (365));
  bignum_rep *yq, *yr;
  truncate (yq, yr, q, &dpy);
  safe_bignum_rep yqq (yq), yrr (yr);

  dt->year = BASE_YEAR + yq->to_ulong ();
  int ndays = yr->to_ulong ();
  ndays -= leap_years_since_base_year (dt->year - 1) - 1;
  while (ndays <= 0)
    {
      dt->year--;
      ndays += leap_year_p (dt->year) ? 366 : 365;
    }
  static const int days_in_month[] =
    {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  for (int mon = 1; mon <= 12; mon++)
    {
      int dom = days_in_month[mon];
      if (mon == 2 && leap_year_p (dt->year))
        dom++;
      if (ndays <= dom)
        break;
      ndays -= dom;
    }

  dt->day = ndays;
  dt->mon = mon;
}

lisp
Fdecode_universal_time (lisp lutc, lisp ltimezone)
{
  decoded_time dt;
  dt.timezone = get_timezone (ltimezone, &dt.daylight);
  decode_universal_time (lutc, &dt);

  multiple_value::value (1) = make_fixnum (dt.min);
  multiple_value::value (2) = make_fixnum (dt.hour);
  multiple_value::value (3) = make_fixnum (dt.day);
  multiple_value::value (4) = make_fixnum (dt.mon);
  multiple_value::value (5) = make_fixnum (dt.year);
  multiple_value::value (6) = make_fixnum (dt.dow);
  multiple_value::value (7) = boole (dt.daylight);
  long t = dt.timezone / 3600;
  multiple_value::value (8) = (t * 3600 == dt.timezone
                               ? make_fixnum (t)
                               : make_ratio (make_fixnum (dt.timezone),
                                             make_fixnum (3600)));
  multiple_value::count () = 9;
  return make_fixnum (dt.sec);
}

lisp
Fencode_universal_time (lisp lsec, lisp lmin, lisp lhour,
                        lisp lday, lisp lmon, lisp lyear,
                        lisp ltimezone)
{
  int sec = fixnum_value (lsec);
  int min = fixnum_value (lmin);
  int hour = fixnum_value (lhour);
  int day = fixnum_value (lday);
  int mon = fixnum_value (lmon);
  if (mon < 1 || mon > 12)
    FErange_error (lmon);
  int year = fixnum_value (lyear);
  if (year >= 0 && year < 100)
    {
      SYSTEMTIME s;
      GetLocalTime (&s);
      year += s.wYear / 100 * 100;
      if (year < s.wYear - 50)
        year += 100;
      else if (year >= s.wYear + 50)
        year -= 100;
    }

  int daylight;
  int timezone = get_timezone (ltimezone, &daylight);
  return decoded_time_to_universal_time (year, mon, day,
                                         hour, min, sec, timezone + daylight);
}

lisp
Fget_internal_real_time ()
{
  return make_fixnum (GetTickCount () & LONG_MAX);
}

lisp
Fsi_performance_counter ()
{
  __int64 x;
  if (sysdep.perf_counter_present_p
      && QueryPerformanceCounter ((LARGE_INTEGER *)&x))
    return make_integer (*(large_int *)&x);
  return Fget_internal_real_time ();
}

lisp
Fsoftware_type ()
{
  return xsymbol_value (Qsoftware_type);
}

lisp
Fsoftware_version ()
{
  return xsymbol_value (Qsoftware_version);
}

lisp
Fsoftware_version_display_string ()
{
  return xsymbol_value (Qsoftware_version_display_string);
}

lisp
Fuser_name ()
{
  return xsymbol_value (Vuser_name);
}

lisp
Fmachine_name ()
{
  return xsymbol_value (Vmachine_name);
}

lisp
Fos_major_version ()
{
  return xsymbol_value (Vos_major_version);
}

lisp
Fos_minor_version ()
{
  return xsymbol_value (Vos_minor_version);
}

lisp
Fos_build_number ()
{
  return xsymbol_value (Vos_build_number);
}

lisp
Fos_platform ()
{
  return xsymbol_value (Vos_platform);
}

lisp
Fos_csd_version ()
{
  return xsymbol_value (Vos_csd_version);
}

void
init_environ ()
{
  char b[256];
  DWORD n = sizeof b;
  if (GetUserName (b, &n))
    xsymbol_value (Vuser_name) = make_string (b);
  else
    xsymbol_value (Vuser_name) = make_string ("unknown");

  n = sizeof b;
  if (GetComputerName (b, &n))
    xsymbol_value (Vmachine_name) = make_string (b);
  else
    xsymbol_value (Vmachine_name) = make_string ("unknown");

  xsymbol_value (Vos_major_version) = make_fixnum (sysdep.os_ver.dwMajorVersion);
  xsymbol_value (Vos_minor_version) = make_fixnum (sysdep.os_ver.dwMinorVersion);
  xsymbol_value (Vos_build_number) = make_fixnum (sysdep.os_ver.dwBuildNumber);
  xsymbol_value (Vos_csd_version) = make_string (sysdep.os_ver.szCSDVersion);

  switch (sysdep.wintype)
    {
    case Sysdep::WINTYPE_WIN32S:
      xsymbol_value (Vos_platform) = Vwin32s;
      xsymbol_value (Vfeatures) = xcons (Kwin32s, xsymbol_value (Vfeatures));
      break;

    case Sysdep::WINTYPE_WINDOWS_95:
      xsymbol_value (Vos_platform) = Vwindows_95;
      xsymbol_value (Vfeatures) = xcons (Kwindows_95, xsymbol_value (Vfeatures));
      break;

    case Sysdep::WINTYPE_WINDOWS_98:
      xsymbol_value (Vos_platform) = Vwindows_98;
      xsymbol_value (Vfeatures) = xcons (Kwindows_98, xsymbol_value (Vfeatures));
      if (sysdep.version () >= Sysdep::WINME_VERSION)
        {
          xsymbol_value (Vos_platform) = Vwindows_me;
          xsymbol_value (Vfeatures) = xcons (Kwindows_me, xsymbol_value (Vfeatures));
        }
      break;

    case Sysdep::WINTYPE_WINDOWS_NT:
    case Sysdep::WINTYPE_WINDOWS_NT5:
      xsymbol_value (Vos_platform) = Vwindows_nt;
      xsymbol_value (Vfeatures) = xcons (Kwindows_nt, xsymbol_value (Vfeatures));
      if (sysdep.wintype == Sysdep::WINTYPE_WINDOWS_NT5)
        {
          xsymbol_value (Vos_platform) = Vwindows_2000;
          xsymbol_value (Vfeatures) = xcons (Kwindows_2000, xsymbol_value (Vfeatures));
          if (sysdep.version () >= Sysdep::WINXP_VERSION)
            {
              xsymbol_value (Vos_platform) = Vwindows_xp;
              xsymbol_value (Vfeatures) = xcons (Kwindows_xp, xsymbol_value (Vfeatures));
            }
        }
      break;

    default:
      xsymbol_value (Vos_platform) = Qnil;
      break;
    }
}

lisp
Fget_windows_directory ()
{
  return xsymbol_value (Qwindows_dir);
}

lisp
Fget_system_directory ()
{
  return xsymbol_value (Qsystem_dir);
}

int environ::save_window_size = 1;
int environ::save_window_position = 1;
int environ::restore_window_size;
int environ::restore_window_position;

int
environ::load_geometry (int cmdshow, POINT *point, SIZE *size)
{
  read_conf (cfgMisc, cfgSaveWindowSize, save_window_size);
  read_conf (cfgMisc, cfgSaveWindowPosition, save_window_position);
  read_conf (cfgMisc, cfgWindowFlags, Window::w_default_flags);
  restore_window_size = save_window_size;
  restore_window_position = save_window_position;
  read_conf (cfgMisc, cfgRestoreWindowSize, restore_window_size);
  read_conf (cfgMisc, cfgRestoreWindowPosition, restore_window_position);

  int x;
  if (read_conf (cfgMisc, cfgFnkeyLabels, x))
    FKWin::default_nbuttons () = x;
  read_conf (cfgMisc, cfgFoldMode, Buffer::b_default_fold_mode);
  if (Buffer::b_default_fold_mode != Buffer::FOLD_NONE
      && Buffer::b_default_fold_mode != Buffer::FOLD_WINDOW
      && Buffer::b_default_fold_mode < 4
      && Buffer::b_default_fold_mode > 30000)
    Buffer::b_default_fold_mode = Buffer::FOLD_NONE;
  read_conf (cfgMisc, cfgFoldLineNumMode, Buffer::b_default_linenum_mode);
  if (Buffer::b_default_linenum_mode != Buffer::LNMODE_DISP
      && Buffer::b_default_linenum_mode != Buffer::LNMODE_LF)
    Buffer::b_default_linenum_mode = Buffer::LNMODE_DISP;

  point->x = point->y = CW_USEDEFAULT;
  size->cx = size->cy = CW_USEDEFAULT;

  SIZE scr;
  scr.cx = GetSystemMetrics (SM_CXSCREEN);
  scr.cy = GetSystemMetrics (SM_CYSCREEN);

  char name[64];
  sprintf (name, "%dx%d", scr.cx, scr.cy);
  WINDOWPLACEMENT w;
  if (read_conf (cfgMisc, name, w)
      && w.rcNormalPosition.left < w.rcNormalPosition.right
      && w.rcNormalPosition.top < w.rcNormalPosition.bottom)
    {
      if (environ::restore_window_size)
        {
          cmdshow = w.showCmd;
          size->cx = w.rcNormalPosition.right - w.rcNormalPosition.left;
          size->cy = w.rcNormalPosition.bottom - w.rcNormalPosition.top;
          size->cx = min (size->cx, scr.cx);
          size->cy = min (size->cy, scr.cy);
        }
      if (environ::restore_window_position)
        {
          point->x = w.rcNormalPosition.left;
          point->y = w.rcNormalPosition.top;
          if (point->x >= scr.cx)
            point->x = scr.cx / 2;
          point->x = max (point->x, LONG (-scr.cx / 2));
          if (point->y >= scr.cy)
            point->y = scr.cy / 2;
          point->y = max (point->y, LONG (-scr.cy / 2));
          if (environ::restore_window_size)
            {
              if (point->x + size->cx < 10)
                point->x = 0;
              if (point->y + size->cy < 10)
                point->y = 0;
            }
        }
    }

  return cmdshow;
}

void
environ::save_geometry ()
{
  save_window_size = xsymbol_value (Vsave_window_size) != Qnil;
  save_window_position = xsymbol_value (Vsave_window_position) != Qnil;

  if (save_window_size || save_window_position)
    {
      WINDOWPLACEMENT w;
      w.length = sizeof w;
      if (GetWindowPlacement (app.toplev, &w))
        {
          char name[256];
          sprintf (name, "%dx%d",
                   GetSystemMetrics (SM_CXSCREEN), GetSystemMetrics (SM_CYSCREEN));
          if (!save_window_size || !save_window_position)
            {
              WINDOWPLACEMENT ow;
              if (read_conf (cfgMisc, name, ow)
                  && ow.rcNormalPosition.left < ow.rcNormalPosition.right
                  && ow.rcNormalPosition.top < ow.rcNormalPosition.bottom)
                {
                  int old_cx = ow.rcNormalPosition.right - ow.rcNormalPosition.left;
                  int old_cy = ow.rcNormalPosition.bottom - ow.rcNormalPosition.top;
                  int new_cx = w.rcNormalPosition.right - w.rcNormalPosition.left;
                  int new_cy = w.rcNormalPosition.bottom - w.rcNormalPosition.top;

                  if (!save_window_position)
                    {
                      w.showCmd = ow.showCmd;
                      w.rcNormalPosition.left = ow.rcNormalPosition.left;
                      w.rcNormalPosition.top = ow.rcNormalPosition.top;
                    }

                  if (!save_window_size)
                    {
                      w.rcNormalPosition.right = w.rcNormalPosition.left + old_cx;
                      w.rcNormalPosition.bottom = w.rcNormalPosition.top + old_cy;
                    }
                  else
                    {
                      w.rcNormalPosition.right = w.rcNormalPosition.left + new_cx;
                      w.rcNormalPosition.bottom = w.rcNormalPosition.top + new_cy;
                    }
                }
            }

          write_conf (cfgMisc, name, w);
        }
    }

  write_conf (cfgMisc, cfgSaveWindowSize, save_window_size);
  write_conf (cfgMisc, cfgSaveWindowPosition, save_window_position);
  write_conf (cfgMisc, cfgRestoreWindowSize,
              xsymbol_value (Vrestore_window_size) != Qnil);
  write_conf (cfgMisc, cfgRestoreWindowPosition,
              xsymbol_value (Vrestore_window_position) != Qnil);
  write_conf (cfgMisc, cfgWindowFlags, Window::w_default_flags, 1);
  write_conf (cfgMisc, cfgFnkeyLabels, FKWin::default_nbuttons ());
  write_conf (cfgMisc, cfgFoldMode, Buffer::b_default_fold_mode);
  write_conf (cfgMisc, cfgFoldLineNumMode, Buffer::b_default_linenum_mode);
  flush_conf ();
}

lisp
Fsi_getenv (lisp var)
{
  check_string (var);
  char *v = (char *)alloca (xstring_length (var) * 2 + 1);
  w2s (v, var);
  char *e = getenv (v);
  return e ? make_string (e) : Qnil;
}

lisp
Fsi_putenv (lisp var)
{
  check_string (var);
  char *v = (char *)alloca (xstring_length (var) * 2 + 1);
  w2s (v, var);
  int r = _putenv (v);
  return r == 0 ? Qt : Qnil;
}

lisp
Fsi_system_root ()
{
  return xsymbol_value (Qmodule_dir);
}

lisp
Fuser_config_path ()
{
  return xsymbol_value (Quser_config_path);
}

lisp
Fsi_dump_image_path ()
{
  return xsymbol_value (Qdump_image_path);
}
