#include "stdafx.h"
#ifdef __XYZZY__
# include "ed.h"
# include "except.h"
# define ERROR_DEAD 0x8fff
#else
# include <malloc.h>
# include <mbstring.h>
# include <mbctype.h>
# include <string.h>
# ifdef DEBUG
#  include <stdio.h>
# endif
# define alloca _alloca
# define jrindex(a, b) (char *)_mbsrchr ((const u_char *)(a), (b))

typedef unsigned char u_char;

#endif /* not __XYZZY__ */

#include "arc-if.h"
#include "archiver.h"
#include "safe_ptr.h"

class stdio_file
{
  FILE *fp;
public:
  stdio_file () : fp (0) {}
  stdio_file (FILE *fp_) : fp (fp_) {}
  ~stdio_file () {if (fp) fclose (fp);}
  operator FILE * () const {return fp;}
  FILE *operator -> () const {return fp;}
};

#if defined (__XYZZY__) && !defined (NEED_EXTRACTINGINFO)
# define NEED_EXTRACTINGINFO
#endif

#ifndef __XYZZY__
static char *
stpcpy (char *dest, const char *src)
{
  int l = strlen (src);
  memcpy (dest, src, l + 1);
  return dest + l;
}
#endif

static char *
docopy (char *d, const char *s)
{
  while (*s)
    {
      u_char c = u_char (*s++);
      if (_ismbblead (c) && *s)
        {
          *d++ = c;
          *d++ = *s++;
        }
      else if (c == '/')
        *d++ = '\\';
      else
        *d++ = c;
    }
  *d = 0;
  return d;
}

static int
has_trail_slash (const char *b)
{
  char *backsl = jrindex (b, '\\');
  return backsl && !backsl[1];
}

static char *
copy_path (char *b, const char *o, const char *d, int dirp = 0)
{
  const char *spc = strchr (d, ' ');
  if (spc)
    *b++ = '"';
  b = stpcpy (b, o);
  char *b0 = b;
  b = docopy (b, d);
  if (dirp && !has_trail_slash (b0))
    *b++ = '\\';
  if (spc)
    *b++ = '"';
  *b = 0;
  return b;
}

inline static char *
copy_dir (char *b, const char *opt, const char *d)
{
  return copy_path (b, opt, d, 1);
}

const char *const ArchiverP::null_suffixes[] = {0};

void
ArchiverP::sepmap (char *s, int f, int t)
{
  while (*s)
    {
      u_char c = u_char (*s);
      if (_ismbblead (c) && s[1])
        s += 2;
      else if (c == f)
        *s++ = t;
      else
        s++;
    }
}

int
ArchiverP::match_suffix (const char *path, int l,
                         const char *const *list) const
{
  for (; *list; list++)
    {
      int x = strlen (*list);
      if (l >= x && !_memicmp (*list, path + l - x, x))
        return 1;
    }
  return 0;
}

#ifdef NEED_EXTRACTINGINFO
static LRESULT CALLBACK
NotifyWndProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  static const UINT extract = RegisterWindowMessage (WM_ARCEXTRACT);
  if (msg == extract)
    {
      EXTRACTINGINFO *i = (EXTRACTINGINFO *)lparam;
      if (i)
        {
#ifdef __XYZZY__
# ifdef DEBUG
          printf ("%d: %s %s %d/%d\n", wparam, i->szSourceFileName,
                  i->szDestFileName, i->dwWriteSize, i->dwFileSize);
# endif
          char buf[1024];
          if (*i->szDestFileName)
            {
              if (!wparam)
                app.status_window.puts (i->szDestFileName, 1);
              else if (wparam == 1)
                {
                  sprintf (buf, "%s(%u/%u)...",
                           i->szDestFileName, i->dwWriteSize, i->dwFileSize);
                  app.status_window.puts (buf, 1);
                }
            }
          else if (wparam == 4)
            {
              sprintf (buf, "Updating %s...", i->szSourceFileName);
              app.status_window.puts (buf, 1);
            }
          if (QUITP)
            return 1;
#else /* not __XYZZY__ */
# ifdef DEBUG
          printf ("%s -> %s  %u/%u\n",
                  i->szSourceFileName,
                  i->szDestFileName,
                  i->dwWriteSize,
                  i->dwFileSize);
          fflush (stdout);
# endif /* DEBUG */
#endif /* not __XYZZY__ */
        }
      return 0;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

static const char NotifyClass[] = "ArcNotify";

int
register_wndclass ()
{
  WNDCLASS wc;
  wc.style = 0;
  wc.lpfnWndProc = NotifyWndProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = GetModuleHandle (0);
  wc.hIcon = 0;
  wc.hCursor = 0;
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = NotifyClass;
  return RegisterClass (&wc);
}

static HWND
create_notify_window (HWND parent)
{
  static int done;
  if (!done)
    done = register_wndclass ();
  return CreateWindow (NotifyClass, "", parent ? WS_CHILD : WS_OVERLAPPEDWINDOW,
                       0, 0, 0, 0, parent, 0, GetModuleHandle (0), 0);
}
#endif /* NEED_EXTRACTINGINFO */

int
ArchiverP::doit (HWND hwnd, const char *data) const
{
  ArchiverInterface::lock lk (ar_interface);
#ifdef NEED_EXTRACTINGINFO
  HWND hfocus = GetFocus ();
  HWND notify = create_notify_window (hwnd);
  if (notify)
    ar_interface.set_owner_window (notify);
#endif /* NEED_EXTRACTINGINFO */
  int enabled = hwnd && IsWindowEnabled (hwnd);
  char buf[1024];
#ifndef __XYZZY__
  int r = ar_interface.doit (hwnd, data, buf, sizeof buf - 2);
#else
  int r = ERROR_DEAD;
  try
    {
      r = ar_interface.doit (hwnd, data, buf, sizeof buf - 2);
    }
  catch (Win32Exception &)
    {
    }
#endif /* __XYZZY__ */
  if (enabled)
    EnableWindow (hwnd, 1);

#ifdef __XYZZY__
  app.status_window.puts ("done", 1);
#endif

#ifdef NEED_EXTRACTINGINFO
  if (notify)
    {
      ar_interface.clear_owner_window ();
      DestroyWindow (notify);
    }
  if (hfocus)
    SetFocus (hfocus);
#endif /* NEED_EXTRACTINGINFO */

  // tar32—p
  BYTE state[256];
  GetKeyboardState (state);
  for (int i = 0; i < 256; i++)
    if (state[i] & 0x80 && GetAsyncKeyState (i) >= 0)
      state[i] &= ~0x80;
  SetKeyboardState (state);

  return r;
}

int
ArchiverP::extract (HWND hwnd, const char *data) const
{
#ifdef DEBUG
  printf ("Command: %s\n", data);
  fflush (stdout);
#endif
  Fbegin_wait_cursor ();
  int x = doit (hwnd, data);
  Fend_wait_cursor ();
#ifdef DEBUG
  printf ("RESULT=%x\n", x);
  fflush (stdout);
#endif
  return x;
}

int
ArchiverP::extract_noresp (HWND hwnd, const char *cmd,
                           int cmdl, const char *path) const
{
  stdio_file fp (fopen (path, "rb"));
  if (!fp)
    return ARC_ERROR_FILE_OPEN;
  size_t size = _filelength (_fileno (fp));
  safe_ptr <char> buf = new char [cmdl + size + 1];
  memcpy (buf, cmd, cmdl);
  if (fread (buf + cmdl, 1, size, fp) != size)
    return ARC_ERROR_CANNOT_READ;
  buf[cmdl + size] = 0;
  for (char *p = buf; *p; p++)
    if (*p == '\r' || *p == '\n')
      *p = ' ';
  return extract (hwnd, buf);
}

int
ArchiverP::extract (HWND hwnd, const char *path, const char *destdir,
                    const char *dir_opt, const char *opt,
                    const char *respopt, const char *respfile) const
{
  int len = (strlen (path) + strlen (destdir)
             + strlen (dir_opt) + strlen (opt)
             + strlen (respfile) + 128);
  if (respopt)
    len += strlen (respopt);
  char *b0 = (char *)alloca (len);
  char *b = stpcpy (b0, opt);
  *b++ = ' ';
  b = copy_path (b, "", path);
  *b++ = ' ';
  b = copy_dir (b, dir_opt, destdir);
  if (*respfile)
    {
      *b++ = ' ';
      if (!respopt)
        return extract_noresp (hwnd, b0, b - b0, respfile);
      copy_path (b, respopt, respfile);
    }
  return extract (hwnd, b0);
}

int
ArchiverP::check_archive (const char *path) const
{
  try
    {
      return ar_interface.check_archive (path, CHECKARCHIVE_BASIC);
    }
  catch (Win32Exception &)
    {
      return 0;
    }
}

int
ArchiverP::create (HWND hwnd, const char *path, const char *respfile,
                   const char *respopt, const char *opt) const
{
  int len = strlen (path) + strlen (respfile)  + strlen (opt) + 128;
  if (respopt)
    len += strlen (respopt);
  char *b0 = (char *)alloca (len);
  char *b = stpcpy (b0, opt);
  *b++ = ' ';
  b = copy_path (b, "", path);
  *b++ = ' ';
  if (!respopt)
    return extract_noresp (hwnd, b0, b - b0, respfile);
  copy_path (b, respopt, respfile);
  return extract (hwnd, b0);
}

int
ArchiverP::remove (HWND hwnd, const char *path, const char *respfile,
                   const char *respopt, const char *opt) const
{
  return create (hwnd, path, respfile, respopt, opt);
}

int
ArchiverP::create_sfx (HWND hwnd, const char *path,
                       const char *opt1, const char *opt2) const
{
  char *b0 = (char *)alloca (strlen (path) + strlen (opt1) + strlen (opt2) + 32);
  char *b = stpcpy (b0, opt1);
  *b++ = ' ';
  b = stpcpy (b, opt2);
  *b++ = ' ';
  copy_path (b, "", path);
  return extract (hwnd, b0);
}

void
ArchiverP::puts_extract (FILE *fp, char *name) const
{
  sepbacksl (name);
  putc ('"', fp);
  fputs (name, fp);
  putc ('"', fp);
}

void
ArchiverP::puts_create (FILE *fp, char *name, const char *) const
{
  sepbacksl (name);
  putc ('"', fp);
  if (*name == '-')
    {
      putc ('.', fp);
      putc ('\\', fp);
    }
  fputs (name, fp);
  putc ('"', fp);
}

const char *const Ish::suffixes[] = {".ish", 0};

int
Ish::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  if (*respfile)
    return ERROR_NOT_SUPPORT;
  int x = ArchiverP::extract (hwnd, path, destdir, "/f=", "/r", "", "");
  return x == ERROR_ALREADY_EXIST ? 0 : x;
}

const char *const Tar::suffixes[] =
{".tgz", ".taz", ".gz", ".Z", ".tar", ".bz2", ".lzma", ".xz", 0};

int
Tar::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  int x;
  switch (*respfile ? -1 : tar.get_archive_type (path))
    {
    case ARCHIVETYPE_GZ:
    case ARCHIVETYPE_Z:
    case ARCHIVETYPE_BZ2:
    case ARCHIVETYPE_LZMA:
    case ARCHIVETYPE_XZ:
      {
        char *dest = (char *)alloca (strlen (path) + 16);
        docopy (dest, path);
        char *backsl = jrindex (dest, '\\');
        if (backsl)
          dest = backsl + 1;
        int l = strlen (dest);
        if (l > 2 && (!_stricmp (dest + l - 2, ".Z")
                      || !_stricmp (dest + l - 2, "_Z")))
          dest[l - 2] = 0;
        else if (l > 3 && (!_stricmp (dest + l - 3, ".gz")
                           || !_stricmp (dest + l - 3, "_gz")
                           || !_stricmp (dest + l - 3, ".xz")
                           || !_stricmp (dest + l - 3, "_xz")))
          dest[l - 3] = 0;
        else if (l > 4 && (!_stricmp (dest + l - 4, ".bz2")
                           || !_stricmp (dest + l - 4, "_bz2")))
          dest[l - 4] = 0;
        else if (l > 5 && (!_stricmp (dest + l - 5, ".lzma")
                           || !_stricmp (dest + l - 5, "_lzma")))
          dest[l - 5] = 0;
        else
          strcat (dest, ".extracted");
        x = ArchiverP::extract (hwnd, path, destdir, "", "xfo", "", dest);
      }
      break;

    default:
      x = ArchiverP::extract (hwnd, path, destdir, "",
                              *respfile ? "--check-all-path=1 -xfo" : "xfo", "@", respfile);
      break;
    }

  return x < 0x8000 ? 0 : x;
}

int
Tar::create (HWND hwnd, const char *path, const char *respfile) const
{
  int l = strlen (path);
#define EQ(EXT) (sizeof (EXT) - 1 <= l \
                 && !_memicmp (path + l - (sizeof (EXT) - 1), (EXT), \
                               sizeof (EXT) - 1))
  const char *cmd;
  if (EQ (".tgz") || EQ (".tar.gz"))
    cmd = "cfz";
  else if (EQ (".taz") || EQ (".tar.Z"))
    cmd = "cfZ";
  else if (EQ (".tar.bz2"))
    cmd = "cfB";
  else if (EQ (".tar.lzma"))
    cmd = "-c --lzma --";
  else if (EQ (".tar.xz"))
    cmd = "cfJ";
  else if (EQ (".gz"))
    cmd = "cfzG";
  else if (EQ (".bz2"))
    cmd = "cfBG";
  else if (EQ (".Z"))
    cmd = "cfZG";
  else if (EQ (".lzma"))
    cmd = "-c --lzma -G --";
  else if (EQ (".xz"))
    cmd = "cfJG";
  else
    cmd = "cf";
#undef EQ
  return ArchiverP::create (hwnd, path, respfile, "@", cmd);
}

const char *const Arj::suffixes[] = {".arj", 0};

int
Arj::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             *respfile ? "x -iup" : "x -iu", "!", respfile);
}

const char *const Lha::csuffixes[] = {".lzh", 0};
const char *const Lha::esuffixes[] = {".lzh", ".exe", 0};

int
Lha::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             *respfile ? "e -a1m1nx1p1jf0" : "e -a1m1nx1jf0",
                             "@", respfile);
}

int
Lha::check_archive (const char *path) const
{
  try
    {
      return lha.check_archive (path, CHECKARCHIVE_RAPID | CHECKARCHIVE_RECOVERY);
    }
  catch (Win32Exception &)
    {
      return 0;
    }
}

int
Lha::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, "@", "a -d1n");
}

int
Lha::remove (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::remove (hwnd, path, respfile, "@", "d -p1n");
}

int
Lha::create_sfx (HWND hwnd, const char *path, const char *opt) const
{
  return ArchiverP::create_sfx (hwnd, path, "s", opt);
}

void
Lha::puts_extract (FILE *fp, char *name) const
{
  sepbacksl (name);
  putc ('"', fp);
  if (*name == '-')
    fputs ("-gb", fp);
  fputs (name, fp);
  putc ('"', fp);
}

static void
zip_puts (FILE *fp, const char *name)
{
  for (const u_char *p = (const u_char *)name; *p;)
    {
      u_char c = *p++;
      if (_ismbblead (c) && *p)
        {
          putc (c, fp);
          putc (*p++, fp);
        }
      else if (c == '\\')
        putc ('/', fp);
      else if (c == '[')
        {
          putc ('[', fp);
          putc ('[', fp);
          putc (']', fp);
        }
      else
        putc (c, fp);
    }
}

const char *const Unzip::esuffixes[] = {".zip", ".exe", 0};

int
Unzip::extract (HWND hwnd, const char *path,
                const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "", "-u --i -o", "@", respfile);
}

void
Unzip::puts_extract (FILE *fp, char *name) const
{
  putc ('"', fp);
  zip_puts (fp, name);
  putc ('"', fp);
}

const char *const Zip::csuffixes[] = {".zip", 0};

int
Zip::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, "@", "-r -q");
}

int
Zip::remove (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::remove (hwnd, path, respfile, "@", "-d -q");
}

int
Zip::check_archive (const char *path) const
{
  try
    {
      return zip.get_version () && unzip.check_archive (path);
    }
  catch (Win32Exception &)
    {
      return 0;
    }
}

void
Zip::puts_create (FILE *fp, char *name, const char *) const
{
  putc ('"', fp);
  if (*name == '-')
    {
      putc ('.', fp);
      putc ('/', fp);
    }
  zip_puts (fp, name);
  putc ('"', fp);
}

const char *const Cab::csuffixes[] = {".cab", 0};
const char *const Cab::esuffixes[] = {".cab", ".exe", 0};

int
Cab::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "", "-x -n -i", "@", respfile);
}

int
Cab::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, "@", "-a -r");
}

const char *const Unrar::esuffixes[] = {".rar", ".exe", 0};

int
Unrar::extract (HWND hwnd, const char *path,
                const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "", "-x -u -s --", "@", respfile);
}

const char *const Bga::suffixes[] = {".gza", ".bza", 0};

int
Bga::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             "x -a -i -n -r", "@", respfile);
}

int
Bga::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, "@", "a -a -i -n -r");
}

int
Bga::remove (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::remove (hwnd, path, respfile, "@", "d -i");
}

const char *const Yz1::suffixes[] = {".yz1", 0};

int
Yz1::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             "x -i -y", 0, respfile);
}

int
Yz1::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, 0, "c -i -y");
}

const char *const UnGCA::esuffixes[] = {".gca", 0};

int
UnGCA::extract (HWND hwnd, const char *path,
                const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             *respfile ? "ex -xx1 -yx1" : "e -yx1",
                             "@", respfile);
}

const char *const SevenZip::suffixes[] = {".7z", 0};

int
SevenZip::extract (HWND hwnd, const char *path,
              const char *destdir, const char *respfile) const
{
  return ArchiverP::extract (hwnd, path, destdir, "",
                             "x -hide", "@", respfile);
}

int
SevenZip::create (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::create (hwnd, path, respfile, "@", "a -hide -ms=off");
}

int
SevenZip::remove (HWND hwnd, const char *path, const char *respfile) const
{
  return ArchiverP::remove (hwnd, path, respfile, "@", "d -hide -ms=off");
}

void
SevenZip::puts_create (FILE *fp, char *name, const char *path) const
{
  sepbacksl (name);
  putc ('"', fp);
  if (*name == '-')
    {
      putc ('.', fp);
      putc ('\\', fp);
    }
  fputs (name, fp);
  DWORD a = GetFileAttributes (path);
  if (a != ~0 && a & FILE_ATTRIBUTE_DIRECTORY)
    {
      if (!has_trail_slash (path))
        putc ('\\', fp);
      putc ('*', fp);
    }
  putc ('"', fp);
}

Archiver::Archiver ()
     : a_zip (a_unzip)
{
  arcs[0] = &a_lha;
  arcs[1] = &a_unzip;
  arcs[2] = &a_tar;
  arcs[3] = &a_arj;
  arcs[4] = &a_ish;
  arcs[5] = &a_zip;
  arcs[6] = &a_cab;
  arcs[7] = &a_unrar;
  arcs[8] = &a_bga;
  arcs[9] = &a_yz1;
  arcs[10] = &a_ungca;
  arcs[11] = &a_seven_zip;
}

const ArchiverP *
Archiver::get_creator (const char *path) const
{
  int l = strlen (path);
  for (int i = 0; i < NARCS; i++)
    if (arcs[i]->match_csuffix (path, l))
      return arcs[i];
  return 0;
}

int
Archiver::check_file_size (const char *path)
{
  WIN32_FIND_DATA fd;
  return (strict_get_file_data (path, fd)
          && (fd.nFileSizeHigh || fd.nFileSizeLow));
}

const ArchiverP *
Archiver::check_archive (const char *path) const
{
  for (int i = 0; i < NARCS; i++)
    if (arcs[i]->reliable_checker_p ()
        && arcs[i]->check_archive (path))
      return arcs[i];
  return 0;
}

const ArchiverP *
Archiver::get_extractor (const char *path) const
{
  if (!check_file_size (path))
    return 0;

  int l = strlen (path);
  for (int i = 0; i < NARCS; i++)
    if (arcs[i]->match_esuffix (path, l) && arcs[i]->check_archive (path))
      return arcs[i];
  return check_archive (path);
}

const ArchiverP *
Archiver::get_remover (const char *path) const
{
  if (!check_file_size (path))
    return 0;

  int l = strlen (path);
  for (int i = 0; i < NARCS; i++)
    if (arcs[i]->match_rsuffix (path, l) && arcs[i]->check_archive (path))
      return arcs[i];
  return check_archive (path);
}

int
Archiver::extract (HWND hwnd, const char *path, const char *destdir,
                   const char *respfile) const
{
  const ArchiverP *ar = get_extractor (path);
  return ar ? ar->extract (hwnd, path, destdir, respfile) : ERROR_NOT_ARC_FILE;
}

int
Archiver::create (HWND hwnd, const char *path, const char *respfile) const
{
  const ArchiverP *ar = get_creator (path);
  return ar ? ar->create (hwnd, path, respfile) : ERROR_NOT_ARC_FILE;
}

int
Archiver::create_sfx (HWND hwnd, const char *path, const char *opt) const
{
  int l = strlen (path);
  for (int i = 0; i < NARCS; i++)
    if (arcs[i]->match_csuffix (path, l))
      return arcs[i]->create_sfx (hwnd, path, opt);
  return ERROR_NOT_ARC_FILE;
}

union obsolete_date
{
  struct
    {
      u_int sec: 5;
      u_int min: 6;
      u_int hour: 5;
      u_int day: 5;
      u_int mon: 4;
      u_int year: 7;
    } b;
  struct
    {
      WORD time;
      WORD date;
    } w;
};

lisp
Archiver::list (const char *path, int file_name_only) const
{
  const ArchiverP *ar = get_extractor (path);
  if (!ar)
    return 0;

  try
    {
      const ArchiverInterface &ai = ar->ar_interface;
      ArchiverInterface::lock lk (ai);
      HARC harc = ai.open (0, path, 0);
      if (!harc)
        return 0;
      ar->post_open (harc);

      lisp result = Qnil;
      protect_gc gcpro (result);
      try
        {
          INDIVIDUALINFO ii;
          for (int nomore = ai.find_first (harc, ar->match_any (), &ii);
               !nomore; nomore = ai.find_next (harc, &ii))
            {
              obsolete_date d;
              d.w.time = ii.wTime;
              d.w.date = ii.wDate;
              if (file_name_only)
                result = xcons (make_string (ii.szFileName), result);
              else
                result = xcons (make_list
                                (make_string (ii.szFileName),
                                 make_string (ii.szAttribute),
                                 make_integer (int64_t (ii.dwOriginalSize)),
                                 make_list (make_fixnum (1980 + d.b.year),
                                            make_fixnum (d.b.mon),
                                            make_fixnum (d.b.day),
                                            make_fixnum (d.b.hour),
                                            make_fixnum (d.b.min),
                                            make_fixnum (d.b.sec * 2),
                                            0),
                                 0),
                                result);
            }
          ai.close (harc);
        }
      catch (...)
        {
          ai.close (harc);
          throw;
        }

      return Fnreverse (result);
    }
  catch (Win32Exception &)
    {
      return Qnil;
    }
}

int
Archiver::get_version (const ArchiverP &a, char *buf)
{
  try
    {
      ArchiverInterface::lock lk (a.ar_interface);
      WORD v = a.ar_interface.get_version ();
      if (!v)
        return 0;
      WORD sv = a.ar_interface.get_sub_version ();
      if (sv)
        sprintf (buf, "%d.%d.%d.%d", v / 100, v % 100, sv / 100, sv % 100);
      else
        sprintf (buf, "%d.%02d", v / 100, v % 100);
      return 1;
    }
  catch (Win32Exception &)
    {
      return 0;
    }
}

int
Archiver::config_dialog (const ArchiverP &a, HWND hwnd, int mode)
{
  char buf[1024];
  *buf = 0;
  try
    {
      return a.ar_interface.config_dialog (hwnd, buf, (mode ? PACK_CONFIG_MODE
                                                       : UNPACK_CONFIG_MODE)) == 1;
    }
  catch (Win32Exception &)
    {
      return 0;
    }
}

const ArchiverP *
Archiver::get (lisp dll) const
{
  if (dll == Kish32)
    return &a_ish;
  if (dll == Ktar32)
    return &a_tar;
  if (dll == Kunlha32)
    return &a_lha;
  if (dll == Kunarj32)
    return &a_arj;
  if (dll == Kunzip32)
    return &a_unzip;
  if (dll == Kzip32j)
    return &a_zip;
  if (dll == Kcab32)
    return &a_cab;
  if (dll == Kunrar32)
    return &a_unrar;
  if (dll == Kbga32)
    return &a_bga;
  if (dll == Kyz1)
    return &a_yz1;
  if (dll == Kungca32)
    return &a_ungca;
  if (dll == K7_zip)
    return &a_seven_zip;
  return 0;
}

#ifdef __XYZZY__

static Archiver archiver;

static void
archiver_error (int e, lisp path, message_code mc)
{
  static const struct
    {
      message_code ae;
      int xe;
      int warn;
    } ec[] =
      {
        {ARC_ERROR_DISK_SPACE, ERROR_DISK_SPACE, 1},
        {ARC_ERROR_READ_ONLY, ERROR_READ_ONLY, 1},
        {ARC_ERROR_USER_SKIP, ERROR_USER_SKIP, 1},
        {ARC_ERROR_UNKNOWN_TYPE, ERROR_UNKNOWN_TYPE, 1},
        {ARC_ERROR_METHOD, ERROR_METHOD, 1},
        {ARC_ERROR_PASSWORD_FILE, ERROR_PASSWORD_FILE, 1},
        {ARC_ERROR_VERSION, ERROR_VERSION, 1},
        {ARC_ERROR_FILE_CRC, ERROR_FILE_CRC, 1},
        {ARC_ERROR_FILE_OPEN, ERROR_FILE_OPEN, 1},
        {ARC_ERROR_MORE_FRESH, ERROR_MORE_FRESH, 1},
        {ARC_ERROR_NOT_EXIST, ERROR_NOT_EXIST, 1},
        {ARC_ERROR_ALREADY_EXIST, ERROR_ALREADY_EXIST, 1},
        {ARC_ERROR_TOO_MANY_FILES, ERROR_TOO_MANY_FILES, 1},
        {ARC_ERROR_DIRECTORY, ERROR_DIRECTORY, 0},
        {ARC_ERROR_CANNOT_WRITE, ERROR_CANNOT_WRITE, 0},
        {ARC_ERROR_HUFFMAN_CODE, ERROR_HUFFMAN_CODE, 0},
        {ARC_ERROR_COMMENT_HEADER, ERROR_COMMENT_HEADER, 0},
        {ARC_ERROR_HEADER_CRC, ERROR_HEADER_CRC, 0},
        {ARC_ERROR_HEADER_BROKEN, ERROR_HEADER_BROKEN, 0},
        {ARC_ERROR_ARC_FILE_OPEN, ERROR_ARC_FILE_OPEN, 0},
        {ARC_ERROR_NOT_ARC_FILE, ERROR_NOT_ARC_FILE, 0},
        {ARC_ERROR_CANNOT_READ, ERROR_CANNOT_READ, 0},
        {ARC_ERROR_FILE_STYLE, ERROR_FILE_STYLE, 0},
        {ARC_ERROR_COMMAND_NAME, ERROR_COMMAND_NAME, 0},
        {ARC_ERROR_MORE_HEAP_MEMORY, ERROR_MORE_HEAP_MEMORY, 0},
        {ARC_ERROR_ENOUGH_MEMORY, ERROR_ENOUGH_MEMORY, 0},
        {ARC_ERROR_DEAD, ERROR_DEAD, 0},
        {ARC_ERROR_NOT_SUPPORT, ERROR_NOT_SUPPORT, 0},
      };
  if (e < 0x8000)
    return;
  for (int i = 0; i < numberof (ec); i++)
    if (e == ec[i].xe)
      {
        if (ec[i].warn)
          {
            warn (ec[i].ae, path);
            return;
          }
        mc = ec[i].ae;
        break;
      }
  FEarchiver_error (mc, path);
}

static lisp
extract_or_remove (lisp lpath, lisp ldir, lisp lfiles)
{
  char path[PATH_MAX + 1], dir[PATH_MAX + 1];
  char temp_name[PATH_MAX + 1];

  pathname2cstr (lpath, path);
  if (ldir)
    pathname2cstr (ldir, dir);

  const ArchiverP *ar;
  if (ldir)
    {
      ar = archiver.get_extractor (path);
      if (!ar)
        file_error (Euncompress_not_supported, lpath);
    }
  else
    {
      if (!consp (lfiles))
        return Qnil;
      ar = archiver.get_remover (path);
      if (!ar)
        file_error (Eremove_not_supported, lpath);
    }

  if (!consp (lfiles))
    *temp_name = 0;
  else
    {
      char temp_path[PATH_MAX + 1];
      GetTempPath (sizeof temp_path, temp_path);
      WINFS::GetTempFileName (temp_path, "xyz", 0, temp_name);
      stdio_file fp (fopen (temp_name, "w"));
      if (!fp)
        {
          WINFS::DeleteFile (temp_name);
          file_error (Ecannot_make_temp_file_name);
        }

      for (; consp (lfiles); lfiles = xcdr (lfiles))
        {
          char name[PATH_MAX * 2 + 1];
          lisp lname = xcar (lfiles);
          check_string (lname);
          if (xstring_length (lname) > PATH_MAX)
            FEsimple_error (Epath_name_too_long, lname);
          w2s (name, lname);
          ar->puts_extract (fp, name);
          putc ('\n', fp);
        }
    }

  try
    {
      if (ldir)
        archiver_error (ar->extract (get_active_window (), path, dir, temp_name),
                        lpath, Eextract_error);
      else
        archiver_error (ar->remove (get_active_window (), path, temp_name),
                        lpath, Eremove_error);
    }
  catch (nonlocal_jump &)
    {
      if (*temp_name)
        WINFS::DeleteFile (temp_name);
      throw;
    }

  if (*temp_name)
    WINFS::DeleteFile (temp_name);

  return Qt;
}

lisp
Fextract_archive (lisp lpath, lisp ldir, lisp lfiles)
{
  return extract_or_remove (lpath, ldir, lfiles);
}

lisp
Fdelete_file_in_archive (lisp lpath, lisp lfiles)
{
  return extract_or_remove (lpath, 0, lfiles);
}

lisp
Fcreate_archive (lisp larcname, lisp lfiles, lisp ldir)
{
  char arcname[PATH_MAX + 1], dir[PATH_MAX + 1];
  pathname2cstr (larcname, arcname);
  pathname2cstr (ldir, dir);
  if (!consp (lfiles))
    return Qnil;

  const ArchiverP *ar = archiver.get_creator (arcname);
  if (!ar)
    file_error (Ecompress_not_supported, larcname);

  docopy (dir, dir); // '/' -> '\\'
  if (!has_trail_slash (dir))
    strcat (dir, "\\");
  size_t dirl = strlen (dir);

  char temp_name[PATH_MAX + 1], temp_path[PATH_MAX + 1];
  GetTempPath (sizeof temp_path, temp_path);
  WINFS::GetTempFileName (temp_path, "xyz", 0, temp_name);

  WINFS::SetCurrentDirectory (dir);

  try
    {
      {
        stdio_file fp (fopen (temp_name, "w"));
        if (!fp)
          file_error (Ecannot_make_temp_file_name);
        for (; consp (lfiles); lfiles = xcdr (lfiles))
          {
            pathname2cstr (xcar (lfiles), temp_path);
            docopy (temp_path, temp_path); // '/' -> '\\'
            char *b = temp_path;
            if (!_memicmp (temp_path, dir, dirl))
              b += dirl;
            else if (strlen (temp_path) == dirl - 1
                     && !_memicmp (temp_path, dir, dirl))
              continue;
            ar->puts_create (fp, b, temp_path);
            putc ('\n', fp);
          }
      }
      int e = ar->create (get_active_window (), arcname, temp_name);
      if (e == -1)
        file_error (Ecompress_not_supported, larcname);
      archiver_error (e, larcname, Ecompress_error);
    }
  catch (nonlocal_jump &)
    {
      WINFS::DeleteFile (temp_name);
      WINFS::SetCurrentDirectory (sysdep.curdir);
      throw;
    }

  WINFS::DeleteFile (temp_name);
  WINFS::SetCurrentDirectory (sysdep.curdir);

  return Qnil;
}

lisp
Fconvert_to_SFX (lisp larcname, lisp lopt)
{
  char arcname[PATH_MAX + 1], dirname[PATH_MAX + 1];
  pathname2cstr (larcname, arcname);
  strcpy (dirname, arcname);
  char *sl = find_last_slash (dirname);
  if (sl)
    {
      *sl = 0;
      WINFS::SetCurrentDirectory (dirname);
    }
  char *opt = "";
  if (lopt && lopt != Qnil)
    {
      opt = (char *)alloca (w2sl (lopt) + 1);
      w2s (opt, lopt);
    }

  try
    {
      archiver_error (archiver.create_sfx (get_active_window (), arcname, opt),
                      larcname, Ecompress_error);
    }
  catch (nonlocal_jump &)
    {
      WINFS::SetCurrentDirectory (sysdep.curdir);
      throw;
    }
  WINFS::SetCurrentDirectory (sysdep.curdir);
  return Qt;
}

lisp
Flist_archive (lisp larcname, lisp file_name_only)
{
  char arcname[PATH_MAX + 1];
  pathname2cstr (larcname, arcname);
  lisp x = archiver.list (arcname, file_name_only && file_name_only != Qnil);
  if (!x)
    FEarchiver_error (ARC_ERROR_NOT_ARC_FILE, larcname);
  return x;
}

lisp
Farchiver_dll_version (lisp dll)
{
  char buf[128];
  const ArchiverP *p = archiver.get (dll);
  return p && Archiver::get_version (*p, buf) ? make_string (buf) : Qnil;
}

lisp
Farchiver_dll_config_dialog (lisp dll, lisp mode)
{
  const ArchiverP *p = archiver.get (dll);
  return boole (p && Archiver::config_dialog (*p, get_active_window (),
                                              mode && mode != Qnil));
}
#endif /* __XYZZY__ */
