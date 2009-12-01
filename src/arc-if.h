#ifndef _ARC_IF_H_
# define _ARC_IF_H_

# include "comm-arc.h"

#define COMMARC_EMULATE_FNS

#define METHOD_INDEX(NAME) AI_ ## NAME ## _INDEX
#define METHOD_TYPE(NAME) AI_ ## NAME ## _PROC

#define DEFNAMEDMETHOD(NAME, ERR, TYPE, ARGS) \
  { lock lk (*this); \
    FARPROC fn = getfn (NAME); \
    if (!fn) return (ERROR); \
    return ((TYPE)fn) ARGS; }

#define DEFMETHOD(NAME, ERR, ARGS) \
  DEFNAMEDMETHOD (METHOD_INDEX (NAME), ERR, METHOD_TYPE (NAME), ARGS)

class ArchiverInterface
{
protected:
  typedef WORD (WINAPI *METHOD_TYPE (get_version))();
  typedef BOOL (WINAPI *METHOD_TYPE (get_running))();
  typedef WORD (WINAPI *METHOD_TYPE (get_bkgnd_mode))();
  typedef WORD (WINAPI *METHOD_TYPE (set_bkgnd_mode))(BOOL);
  typedef int (WINAPI *METHOD_TYPE (doit))(HWND, LPCSTR, LPSTR, DWORD);
  typedef BOOL (WINAPI *METHOD_TYPE (check_archive))(LPCSTR, int);
  typedef int (WINAPI *METHOD_TYPE (get_file_count))(LPCSTR);
  typedef HARC (WINAPI *METHOD_TYPE (open))(HWND, LPCSTR, DWORD);
  typedef int (WINAPI *METHOD_TYPE (close))(HARC);
  typedef int (WINAPI *METHOD_TYPE (find_first))(HARC, LPCSTR, INDIVIDUALINFO *);
  typedef int (WINAPI *METHOD_TYPE (find_next))(HARC, INDIVIDUALINFO *);
  typedef BOOL (WINAPI *METHOD_TYPE (set_owner_window))(HWND);
  typedef BOOL (WINAPI *METHOD_TYPE (clear_owner_window))();
  typedef WORD (WINAPI *METHOD_TYPE (get_sub_version))();
  typedef BOOL (WINAPI *METHOD_TYPE (config_dialog))(HWND, LPSTR, int);

  enum
    {
      METHOD_INDEX (get_version),
      METHOD_INDEX (get_running),
      METHOD_INDEX (get_bkgnd_mode),
      METHOD_INDEX (set_bkgnd_mode),
      METHOD_INDEX (doit),
      METHOD_INDEX (check_archive),
      METHOD_INDEX (get_file_count),
      METHOD_INDEX (open),
      METHOD_INDEX (close),
      METHOD_INDEX (find_first),
      METHOD_INDEX (find_next),
      METHOD_INDEX (set_owner_window),
      METHOD_INDEX (clear_owner_window),
      METHOD_INDEX (get_sub_version),
      METHOD_INDEX (config_dialog),
      MAX_METHOD
    };

  FARPROC ai_fns[MAX_METHOD];

private:
  HMODULE ai_hmodule;
  const char *const ai_module_name;
  const char *const ai_prefix;

  static const char *ai_names[];
#ifdef COMMARC_EMULATE_FNS
  virtual const FARPROC *emulate_fns () const {return 0;}
#endif
  virtual int patch_module (void *) const {return 0;}

protected:
  ArchiverInterface (const char *, const char *);

  FARPROC getfn (int) const;
  FARPROC getfn (const char *name) const
    {return ai_hmodule ? GetProcAddress (ai_hmodule, name) : 0;}

public:
  class lock
    {
      ArchiverInterface &l_ai;
      HMODULE l_omodule;
    public:
      lock (const ArchiverInterface &);
      ~lock ();
    };
  friend class lock;

  virtual int doit (HWND hwnd, LPCSTR cmdline, LPSTR output, DWORD size) const
    {DEFMETHOD (doit, ERROR_METHOD, (hwnd, cmdline, output, size));}
  WORD get_version () const
    {DEFMETHOD (get_version, 0, ());}
  BOOL get_running () const
    {DEFMETHOD (get_running, 0, ());}
  BOOL get_bkgnd_mode () const
    {DEFMETHOD (get_bkgnd_mode, 0, ());}
  BOOL set_bkgnd_mode (BOOL f) const
    {DEFMETHOD (set_bkgnd_mode, 0, (f));}
  BOOL check_archive (LPCSTR filename, int mode) const
    {DEFMETHOD (check_archive, 0, (filename, mode));}
  int get_file_count (LPCSTR filename) const
    {DEFMETHOD (get_file_count, -1, (filename));}
  HARC open (HWND hwnd, LPCSTR filename, DWORD mode) const
    {DEFMETHOD (open, 0, (hwnd, filename, mode));}
  int close (HARC harc) const
    {DEFMETHOD (close, ERROR_METHOD, (harc));}
  int find_first (HARC harc, LPCSTR filename, INDIVIDUALINFO *vi) const
    {DEFMETHOD (find_first, -1, (harc, filename, vi));}
  int find_next (HARC harc, INDIVIDUALINFO *vi) const
    {DEFMETHOD (find_next, -1, (harc, vi));}
  BOOL set_owner_window (HWND hwnd) const
    {DEFMETHOD (set_owner_window, 0, (hwnd));}
  BOOL clear_owner_window () const
    {DEFMETHOD (clear_owner_window, 0, ());}
  WORD get_sub_version () const
    {DEFMETHOD (get_sub_version, 0, ());}
  BOOL config_dialog (HWND hwnd, LPSTR buf, int mode) const
    {DEFMETHOD (config_dialog, 0, (hwnd, buf, mode));}
};

class IshInterface: public ArchiverInterface
{
  enum {METHOD_INDEX (ish_doit) = METHOD_INDEX (doit)};
  typedef int (WINAPI *METHOD_TYPE (ish_doit))(HWND, LPCSTR);
public:
  IshInterface () : ArchiverInterface ("ISH32.DLL", "Ish") {}
  virtual int doit (HWND hwnd, LPCSTR cmdline, LPSTR, DWORD) const
    {DEFMETHOD (ish_doit, ERROR_METHOD, (hwnd, cmdline));}
};

class TarInterface: public ArchiverInterface
{
public:
  TarInterface () : ArchiverInterface ("TAR32.DLL", "Tar") {}
  int get_archive_type (const char *path) const
    {DEFNAMEDMETHOD ("TarGetArchiveType", -1, int (WINAPI *)(LPCSTR), (path));}
};

class UnarjInterface: public ArchiverInterface
{
public:
  UnarjInterface () : ArchiverInterface ("UNARJ32J.DLL", "Unarj") {}
};

class UnlhaInterface: public ArchiverInterface
{
public:
  UnlhaInterface () : ArchiverInterface ("UNLHA32.DLL", "Unlha") {}
};

class UnzipInterface: public ArchiverInterface
{
#ifdef COMMARC_EMULATE_FNS
  static FARPROC unzip_emulate_fns[];
  virtual const FARPROC *emulate_fns () const {return unzip_emulate_fns;}
#endif
  virtual int patch_module (void *) const;
public:
  UnzipInterface () : ArchiverInterface ("UNZIP32.DLL", "UnZip") {}
};

class ZipInterface: public ArchiverInterface
{
public:
  ZipInterface () : ArchiverInterface ("ZIP32J.DLL", "Zip") {}
};

class CabInterface: public ArchiverInterface
{
public:
  CabInterface () : ArchiverInterface ("CAB32.DLL", "Cab") {}
};

class UnrarInterface: public ArchiverInterface
{
public:
  UnrarInterface () : ArchiverInterface ("UNRAR32.DLL", "Unrar") {}
};

class BgaInterface: public ArchiverInterface
{
public:
  BgaInterface () : ArchiverInterface ("BGA32.DLL", "Bga") {}
};

class Yz1Interface: public ArchiverInterface
{
public:
  Yz1Interface () : ArchiverInterface ("YZ1.DLL", "Yz1") {}
  int set_default_passwd (HARC harc, const char *passwd) const
    {DEFNAMEDMETHOD ("Yz1SetDefaultPassword", -1,
                     int (WINAPI *)(HARC, const char *), (harc, passwd));}
};

class UnGCAInterface: public ArchiverInterface
{
public:
  UnGCAInterface () : ArchiverInterface ("UnGCA32.DLL", "UnGCA") {}
};

class JackInterface: public ArchiverInterface
{
public:
  JackInterface () : ArchiverInterface ("JACK32.DLL", "Jack") {}
};

class SevenZipInterface: public ArchiverInterface
{
public:
  SevenZipInterface () : ArchiverInterface ("7-zip32.dll", "SevenZip") {}
};

#endif
