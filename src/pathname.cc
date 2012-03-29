#include "ed.h"
#include "pathname.h"
#include "dyn-handle.h"
#include "environ.h"
#include <io.h>
#include <math.h>
#include "except.h"
#include "mman.h"
#include <winioctl.h>
#include "thread.h"
#include "xstrlist.h"
#include "vwin32.h"
#include "version.h"

static lisp
file_error_condition (int e)
{
  switch (e)
    {
    case ERROR_FILE_NOT_FOUND:
      return QCfile_not_found;

    case ERROR_PATH_NOT_FOUND:
      return QCpath_not_found;

    case ERROR_ACCESS_DENIED:
      return QCaccess_denied;

    case ERROR_INVALID_DRIVE:
      return QCinvalid_drive;

    case ERROR_CURRENT_DIRECTORY:
      return QCcurrent_directory;

    case ERROR_NOT_SAME_DEVICE:
      return QCnot_same_device;

    case ERROR_WRITE_PROTECT:
      return QCwrite_protected;

    case ERROR_BAD_UNIT:
      return QCbad_unit;

    case ERROR_NOT_READY:
      return QCdevice_not_ready;

    case ERROR_SHARING_VIOLATION:
      return QCsharing_violation;

    case ERROR_LOCK_VIOLATION:
      return QClock_violation;

    case ERROR_WRONG_DISK:
      return QCwrong_disk;

    case ERROR_FILE_EXISTS:
    case ERROR_ALREADY_EXISTS:
      return QCfile_exists;

    case ERROR_DIR_NOT_EMPTY:
      return QCnot_empty;

    default:
      return QCfile_error;
    }
}

void
file_error (message_code c, lisp path)
{
  FEfile_error (c, path);
}

void
file_error (message_code c)
{
  FEfile_error (c, Qnil);
}

void
file_error (int e, lisp path)
{
  FEwin32_file_error (xsymbol_value (file_error_condition (e)), e, path);
}

void
file_error (int e)
{
  FEwin32_file_error (xsymbol_value (file_error_condition (e)), e);
}

static Char *
skip_device_or_host (const Char *p, const Char *pe)
{
  if (dir_separator_p (*p) && dir_separator_p (p[1]))
    {
      // skip hostname
      for (p += 2; p < pe && !dir_separator_p (*p); p++)
        ;
      // skip shared name
      if (p < pe)
        for (p++; p < pe && !dir_separator_p (*p); p++)
          ;
    }
  else if (alpha_char_p (*p) && p[1] == ':')
    p += 2;
  return (Char *)p;
}

static Char *
copy_Chars (Char *b, const Char *p, const Char *pe)
{
  int l = pe - p;
  bcopy (p, b, l);
  return b + l;
}

static char devdirs[26][PATH_MAX];

int
set_device_dir (const char *path, int f)
{
  if (!WINFS::SetCurrentDirectory (path))
    return 0;
  if (f || xsymbol_value (Vauto_update_per_device_directory) != Qnil)
    {
      char curdir[PATH_MAX];
      if (GetCurrentDirectory (sizeof curdir, curdir)
          && alpha_char_p (*curdir & 255) && curdir[1] == ':')
        strcpy (devdirs[_char_downcase (*curdir) - 'a'], curdir + 2);
    }
  return 1;
}

const char *
get_device_dir (int c)
{
  return devdirs[c];
}

static Char *
get_device_dir (Char *b, const Char *p, int l)
{
  if (l == 2 && alpha_char_p (*p) && p[1] == ':'
      && *devdirs[_char_downcase (*p) - 'a'])
    return s2w (b, devdirs[_char_downcase (*p) - 'a']);

  char buf[PATH_MAX + 1], path[PATH_MAX + 1], *tem;
  w2s (buf, p, l);
  if (WINFS::GetFullPathName (buf, sizeof path, path, &tem))
    {
      Char *be = s2w (b, path);
      return copy_Chars (b, skip_device_or_host (b, be), be);
    }
  return b;
}

static void
parse_name_simple (pathname &path, const Char *p, const Char *pe)
{
  path.dev = p;
  path.deve = skip_device_or_host (p, pe);
  path.trail = path.deve;
  path.traile = pe;
}

static void
parse_name (pathname &path, const Char *p, const Char *pe)
{
  const Char *t = skip_device_or_host (p, pe);
  if (t == p)
    path.dev = path.deve = 0;
  else
    {
      path.dev = p;
      path.deve = t;
      p = t;
    }

  path.trail = p;
  path.traile = pe;
  pe--;
  const Char *pe2 = pe - 1;
  while (p < pe)
    {
      while (p < pe && dir_separator_p (*p))
        {
          p++;
          if (dir_separator_p (*p))  //  `//' or `\\'
            path.trail = p;
          else if (*p == '~' && (p == pe || dir_separator_p (p[1])))
            {
              path.trail = p;
              p++;
            }
          else if (p <= pe2 && alpha_char_p (*p) && p[1] == ':')
            {
              path.dev = p;
              p += 2;
              path.deve = p;
              path.trail = p;
            }
        }
      while (p < pe && !dir_separator_p (*p))
        p++;
    }
}

int
parse_namestring (pathbuf_t buf, const Char *name, int nl, const Char *defalt, int dl)
{
  pathname path, tem;

  parse_name (path, name, name + nl);
  const Char *trail = path.trail;
  const Char *traile = path.traile;

  if (trail < traile && *trail == '~'
      && (trail + 1 == traile || dir_separator_p (trail[1])))
    {
      lisp home = xsymbol_value (Qhome_dir);
      parse_name_simple (tem, xstring_contents (home),
                         xstring_contents (home) + xstring_length (home));
      path.dev = tem.dev;
      path.deve = tem.deve;
      if (trail + 1 == traile)
        path.trail = traile;
      else
        path.trail += 2;
      trail = tem.trail;
      traile = tem.traile;
    }
  if (path.dev == path.deve)
    path.dev = 0;

  Char *b = buf;
  Char *root;

  int abs = trail < traile && dir_separator_p (*trail);
  if (path.dev && abs)
    {
      b = root = copy_Chars (b, path.dev, path.deve);
      b = copy_Chars (b, trail, traile);
    }
  else
    {
      parse_name_simple (tem, defalt, defalt + dl);
      if (!path.dev)
        {
          path.dev = tem.dev;
          path.deve = tem.deve;
        }
      b = root = copy_Chars (b, path.dev, path.deve);
      if (!abs)
        {
          int l = path.deve - path.dev;
          if (tem.deve - tem.dev == l && !memicmp (path.dev, tem.dev, sizeof *path.dev * l))
            b = copy_Chars (b, tem.trail, tem.traile);
          else
            b = get_device_dir (b, path.dev, l);
          if (b == buf || !dir_separator_p (b[-1]))
            *b++ = SEPCHAR;
          b = copy_Chars (b, trail, traile);
        }
      else
        b = copy_Chars (b, trail, traile);
    }

  if (trail != path.trail)
    {
      if (b == buf || !dir_separator_p (b[-1]))
        *b++ = SEPCHAR;
      b = copy_Chars (b, path.trail, path.traile);
    }
  *b = 0;

  Char *p = root, *pe = b;
  b = root;
  while (p < pe)
    {
      if (dir_separator_p (*p))
        {
          if (p[1] == '.')
            {
              if (!p[2] || dir_separator_p (p[2]))
                {
                  p += 2;
                  continue;
                }
              else if (p[2] == '.' && (!p[3] || dir_separator_p (p[3])))
                {
                  for (; b > root && !dir_separator_p (b[-1]); b--)
                    ;
                  if (b != root)
                    b--;
                  p += 3;
                  continue;
                }
            }
        }
      *b++ = *p++;
    }
  if (b == root)
    *b++ = SEPCHAR;
  else if (b > root + 1 && dir_separator_p (b[-1]))
    b--;

  if (b[-1] == '.')
    {
      for (p = b - 1; p > root && p[-1] == '.'; p--)
        ;
      if (p[-1] != SEPCHAR)
        for (const Char *q = p; q > root;)
          {
            Char c = *--q;
            if (c == SEPCHAR)
              {
                b = p;
                break;
              }
            if (c == '*' || c == '?')
              break;
          }
    }

  return b - buf;
}

void
map_backsl_to_sl (Char *p, int l)
{
  for (int i = 0; i < l; i++, p++)
    if (*p == '\\')
      *p = '/';
}

static void
coerce_to_pathname (lisp &pathname, pathbuf_t buf, const Char *&b, int &l)
{
  if (stringp (pathname))
    {
      if (xstring_length (pathname) >= WPATH_MAX)
        FEsimple_error (Epath_name_too_long, pathname);

      l = parse_namestring (buf, xstring_contents (pathname), xstring_length (pathname), b, l);
      if (l >= WPATH_MAX)
        FEsimple_error (Epath_name_too_long, pathname);
      map_backsl_to_sl (buf, l);
      b = buf;
    }
  else if (streamp (pathname) && file_stream_p (pathname)
           && stringp (xfile_stream_pathname (pathname)))
    {
      pathname = xfile_stream_pathname (pathname);
      b = xstring_contents (pathname);
      l = xstring_length (pathname);
    }
  else
    FEtype_error (pathname, Qpathname);
}

static lisp
default_directory ()
{
  if (selected_window ())
    {
      Buffer *bp = selected_buffer ();
      if (bp)
        return bp->ldirectory;
    }
  return xsymbol_value (Qdefault_dir);
}

static lisp
coerce_to_pathname (lisp pathname, pathbuf_t buf, const Char *&b, const Char *&be)
{
  lisp d = default_directory ();
  b = xstring_contents (d);
  int l = xstring_length (d);
  coerce_to_pathname (pathname, buf, b, l);
  be = b + l;
  return pathname;
}

lisp
Fmerge_pathnames (lisp pathname, lisp defaults)
{
  lisp d = default_directory ();
  const Char *b = xstring_contents (d);
  int l = xstring_length (d);
  pathbuf_t buf1, buf2;
  if (defaults && defaults != Qnil)
    coerce_to_pathname (defaults, buf1, b, l);
  coerce_to_pathname (pathname, buf2, b, l);

  if (b != buf2)
    return pathname;

  if (stringp (pathname)
      && l == xstring_length (pathname)
      && !bcmp (b, xstring_contents (pathname), l))
    return pathname;

  return make_string (b, l);
}

lisp
Fnamestring (lisp name)
{
  return Fmerge_pathnames (name, 0);
}

static int
has_trail_slash_p (lisp pathname, int dot)
{
  if (stringp (pathname) && xstring_length (pathname))
    {
      const Char *p = xstring_contents (pathname);
      if (dir_separator_p (p[xstring_length (pathname) - 1]))
        return 1;
      if (dot && p[xstring_length (pathname) - 1] == '.'
          && (xstring_length (pathname) == 1
              || dir_separator_p (p[xstring_length (pathname) - 2])))
        return 1;
    }
  return 0;
}

lisp
Fappend_trail_slash (lisp pathname)
{
  check_string (pathname);
  if (has_trail_slash_p (pathname, 0))
    return pathname;
  lisp p = make_string (xstring_length (pathname) + 1);
  bcopy (xstring_contents (pathname), xstring_contents (p), xstring_length (pathname));
  xstring_contents (p) [xstring_length (pathname)] = SEPCHAR;
  return p;
}

lisp
Fremove_trail_slash (lisp pathname)
{
  check_string (pathname);
  if (!has_trail_slash_p (pathname, 0))
    return pathname;
  return make_string (xstring_contents (pathname), xstring_length (pathname) - 1);
}

lisp
Ffile_namestring (lisp pathname)
{
  if (has_trail_slash_p (pathname, 1))
    return make_string ("");

  pathbuf_t buf;
  const Char *p0, *pe;
  pathname = coerce_to_pathname (pathname, buf, p0, pe);
  for (const Char *p = pe; p > p0; p--)
    if (p[-1] == SEPCHAR)
      return make_string (p, pe - p);
  return pathname;
}

lisp
Fdirectory_namestring (lisp pathname)
{
  int dirp = has_trail_slash_p (pathname, 1);
  pathbuf_t buf;
  const Char *p0, *pe;
  pathname = coerce_to_pathname (pathname, buf, p0, pe);
  if (dirp && p0 != pe)
    {
      if (p0 != buf)
        bcopy (p0, buf, pe - p0);
      Char *be = buf + (pe - p0);
      if (!dir_separator_p (be[-1]))
        *be++ = '/';
      if (stringp (pathname)
          && be - buf == xstring_length (pathname)
          && !bcmp (buf, xstring_contents (pathname), xstring_length (pathname)))
        return pathname;
      return make_string (buf, be - buf);
    }
  for (const Char *p = pe; p > p0; p--)
    if (p[-1] == SEPCHAR)
      return make_string (p0, p - p0);
  return make_string ("");
}

lisp
Fpathname_host (lisp pathname)
{
  pathbuf_t buf;
  const Char *p0, *pe;
  pathname = coerce_to_pathname (pathname, buf, p0, pe);
  if (pe - p0 < 3)
    return Qnil;
  if (*p0 != SEPCHAR || p0[1] != SEPCHAR)
    return Qnil;
  p0 += 2;
  const Char *p;
  for (p = p0; p < pe && *p != SEPCHAR; p++)
    ;
  if (p == p0)
    return Qnil;
  return make_string (p0, p - p0);
}

lisp
Fpathname_device (lisp pathname)
{
  pathbuf_t buf;
  const Char *p, *pe;
  pathname = coerce_to_pathname (pathname, buf, p, pe);
  if (pe - p < 2)
    return Qnil;
  if (alpha_char_p (*p) && p[1] == ':')
    return make_string (*p, 1);
  return Qnil;
}

lisp
Fpathname_directory (lisp pathname)
{
  pathbuf_t buf;
  const Char *p, *pe;
  pathname = coerce_to_pathname (pathname, buf, p, pe);
  lisp dirs = Qnil;
  if (p + 3 <= pe && *p == SEPCHAR && p[1] == SEPCHAR)
    for (p += 2; p < pe && *p != SEPCHAR; p++)
      ;
  else if (p + 2 <= pe && alpha_char_p (*p) && p[1] == ':')
    p += 2;

  while (p < pe)
    {
      const Char *p0 = p;
      for (; p < pe && *p != SEPCHAR; p++)
        ;
      if (p == pe)
        break;
      if (p != p0)
        dirs = xcons (make_string (p0, p - p0), dirs);
      p++;
    }
  return Fnreverse (dirs);
}

static const Char *
pathname_name_type (lisp pathname, pathbuf_t buf,
                    const Char *&name, const Char *&name_e,
                    const Char *&type, const Char *&type_e)
{
  const Char *p0, *pe;
  pathname = coerce_to_pathname (pathname, buf, p0, pe);
  const Char *p;
  for (p = pe; p > p0; p--)
    if (p[-1] == SEPCHAR)
      break;
  const Char *dot;
  for (dot = p; dot < pe && *dot == '.'; dot++)
    ;
  const Char *p2;
  for (p2 = pe; p2 > dot; p2--)
    if (p2[-1] == '.')
      break;
  name = p;
  name_e = p2 == dot ? pe : p2 - 1;
  type = p2 != dot ? p2 : pe;
  type_e = pe;
  return p0;
}

lisp
Fpathname_name (lisp pathname)
{
  pathbuf_t buf;
  const Char *name, *name_e, *type, *type_e;
  pathname_name_type (pathname, buf, name, name_e, type, type_e);
  return name == name_e ? Qnil : make_string (name, name_e - name);
}

lisp
Fpathname_type (lisp pathname)
{
  pathbuf_t buf;
  const Char *name, *name_e, *type, *type_e;
  pathname_name_type (pathname, buf, name, name_e, type, type_e);
  return type == type_e ? Qnil : make_string (type, type_e - type);
}

char *
pathname2cstr (lisp pathname, char *buf)
{
  pathbuf_t tem;
  const Char *p, *pe;
  pathname = coerce_to_pathname (pathname, tem, p, pe);
  return w2s (buf, p, pe - p);
}

static int
file_attributes (lisp pathname)
{
  char path[PATH_MAX + 1];
  pathname2cstr (pathname, path);
  return WINFS::GetFileAttributes (path);
}

lisp
Ffile_exist_p (lisp file)
{
  int x = file_attributes (file);
  return boole (x != -1);
}

lisp
Ffile_readable_p (lisp file)
{
  int x = file_attributes (file);
  return boole (!(x & FILE_ATTRIBUTE_DIRECTORY));
}

lisp
Ffile_writable_p (lisp file)
{
  int x = file_attributes (file);
  return boole (!(x & (FILE_ATTRIBUTE_DIRECTORY
                       | FILE_ATTRIBUTE_SYSTEM
                       | FILE_ATTRIBUTE_READONLY)));
}

lisp
Ffile_executable_p (lisp file)
{
  /*NOTYET*/
  return Qnil;
}

lisp
Ffile_directory_p (lisp file)
{
  int x = file_attributes (file);
  return boole (x != -1 && x & FILE_ATTRIBUTE_DIRECTORY);
}

int
special_file_p (const char *path)
{
  HANDLE h = WINFS::CreateFile (path, GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
  if (h == INVALID_HANDLE_VALUE)
    return 0;
  int dev = GetFileType (h) != FILE_TYPE_DISK;
  CloseHandle (h);
  return dev;
}

lisp
Fspecial_file_p (lisp file)
{
  char path[PATH_MAX + 1];
  pathname2cstr (file, path);
  return boole (special_file_p (path));
}

lisp
Fvalid_path_p (lisp file)
{
  int x = file_attributes (file);
  if (x != -1)
    return Qt;
  return boole (GetLastError () == ERROR_FILE_NOT_FOUND);
}

lisp
Fcheck_valid_pathname (lisp path)
{
  int x = file_attributes (path);
  if (x != -1)
    return Qt;
  int e = GetLastError ();
  if (e != ERROR_FILE_NOT_FOUND)
    file_error (e, path);
  return Qnil;
}

lisp
Ftruename (lisp pathname)
{
  char path[PATH_MAX + 1], truename[PATH_MAX + 1];
  pathname2cstr (pathname, path);
  if (WINFS::GetFileAttributes (path) == -1)
    file_error (GetLastError (), pathname);

  map_sl_to_backsl (path);
  char *sl = 0;
  if (alpha_char_p (*path & 0xff) && path[1] == ':' && path[2] == '\\')
    sl = path + 2;
  else if (*path == '\\' && path[1] == '\\')
    {
      sl = jindex (path + 2, '\\');
      if (sl)
        sl = jindex (sl + 1, '\\');
    }
  if (!sl)
    sl = jindex (path, '\\');
  if (!sl)
    strcpy (truename, path);
  else
    {
      sl++;
      memcpy (truename, path, sl - path);
      char *t = truename + (sl - path);
      while (1)
        {
          char *p = jindex (sl, '\\');
          if (p)
            *p = 0;
          WIN32_FIND_DATA fd;
          if (WINFS::get_file_data (path, fd))
            t = stpcpy (t, fd.cFileName);
          else if (p)
            t = stpncpy (t, sl, p - sl);
          else
            t = stpcpy (t, sl);
          if (!p)
            break;
          *p = '\\';
          sl = p + 1;
          *t++ = '\\';
        }
      *t = 0;
    }
  map_backsl_to_sl (truename);

  Char w[PATH_MAX + 1];
  int l = s2w (w, truename) - w;
  if (stringp (pathname) && l == xstring_length (pathname)
      && !bcmp (w, xstring_contents (pathname), l))
    return pathname;
  return make_string (w, l);
}

lisp
Fuser_homedir_pathname ()
{
  return xsymbol_value (Qhome_dir);
}

int
match_suffixes (const char *name, lisp ignores)
{
  int l = strlen (name);
  for (; consp (ignores); ignores = xcdr (ignores))
    {
      lisp x = xcar (ignores);
      if (!stringp (x))
        continue;
      if (xstring_length (x) <= l
          && strequal (name + l - xstring_length (x), xstring_contents (x)))
        return 1;
    }
  return 0;
}

lisp
Ffile_system_supports_long_file_name_p (lisp path)
{
  pathbuf_t buf;
  const Char *p, *pe;
  coerce_to_pathname (path, buf, p, pe);
  if (pe - p < 2)
    return Qnil;
  Char *t = skip_device_or_host (p, pe);
  if (p != buf)
    bcopy (p, buf, t - p);
  t = buf + (t - p);
  *t++ = SEPCHAR;
  char cbuf[PATH_MAX + 1];
  w2s (cbuf, buf, t - buf);

  DWORD maxl, flags;
  return boole (WINFS::GetVolumeInformation (cbuf, 0, 0, 0, &maxl, &flags, 0, 0) && maxl > 12);
}

lisp
Fpath_equal (lisp lpath1, lisp lpath2)
{
  char path1[PATH_MAX + 1], path2[PATH_MAX + 1];
  pathname2cstr (lpath1, path1);
  pathname2cstr (lpath2, path2);
  return boole (same_file_p (path1, path2));
}

static int
sub_dirp_by_name (const char *dir, const char *parent)
{
  int dl = strlen (dir);
  const char *de = find_last_slash (dir);
  if (de && !de[1])
    dl--;
  int pl = strlen (parent);
  const char *pe = find_last_slash (parent);
  if (pe && !pe[1])
    pl--;
  if (dl < pl)
    return 0;
  if (_memicmp (dir, parent, pl))
    return 0;
  return !dir[pl] || dir[pl] == '/';
}

int
sub_directory_p (char *dir, const char *parent)
{
  if (sub_dirp_by_name (dir, parent))
    {
      DWORD a = WINFS::GetFileAttributes (dir);
      if (a == -1 || !(a & FILE_ATTRIBUTE_DIRECTORY))
        return 0;
      a = WINFS::GetFileAttributes (parent);
      if (a == -1 || !(a & FILE_ATTRIBUTE_DIRECTORY))
        return 0;
      return 1;
    }

  dyn_handle dh (WINFS::CreateFile (parent, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                    0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0));
  if (!dh.valid ())
    return 0;
  BY_HANDLE_FILE_INFORMATION info;
  info.dwFileAttributes = 0;
  GetFileInformationByHandle (dh, &info);
  if (!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
    return 0;

  while (1)
    {
      HANDLE h = WINFS::CreateFile (dir, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                    0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
      if (h == INVALID_HANDLE_VALUE)
        return 0;
      BY_HANDLE_FILE_INFORMATION i;
      i.dwFileAttributes = 0;
      GetFileInformationByHandle (h, &i);
      CloseHandle (h);
      if (!(i.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
        return 0;
      if (i.dwVolumeSerialNumber == info.dwVolumeSerialNumber
          && i.nFileIndexHigh == info.nFileIndexHigh
          && i.nFileIndexLow == info.nFileIndexLow)
        return 1;
      char *sl = find_last_slash (dir);
      if (!sl)
        return 0;
      if (!sl[1])
        {
          *sl = 0;
          sl = find_last_slash (dir);
          if (!sl)
            return 0;
        }
      sl[1] = 0;
      if (!find_last_slash (dir))
        return 1;
    }
}

lisp
Fsub_directory_p (lisp ldir, lisp lparent)
{
  char dir[PATH_MAX + 1], parent[PATH_MAX + 1];
  pathname2cstr (ldir, dir);
  pathname2cstr (lparent, parent);

  return boole (sub_directory_p (dir, parent));
}

lisp
Fcompile_file_pathname (lisp pathname)
{
  pathbuf_t buf;
  const Char *name, *name_e, *type, *type_e;
  const Char *p0 = pathname_name_type (pathname, buf, name, name_e, type, type_e);
  if (name == name_e)
    return Qnil;
  int l = type_e - p0;
  if (p0 != buf)
    bcopy (p0, buf, l);
  Char *b = buf + l;
  if (type_e - type == 1 && (*type == 'l' || *type == 'L'))
    *b++ = *type == 'l' ? 'c' : 'C';
  else
    {
      if (type == type_e)
        *b++ = '.';
      else if (Ffile_system_supports_long_file_name_p (pathname) == Qnil)
        b -= type_e - type;
      else
        *b++ = '.';
      *b++ = 'l';
      *b++ = 'c';
    }
  return make_string (buf, b - buf);
}

lisp
Ffind_load_path (lisp filename)
{
  static const char *const ext[] = {".lc", ".l", "", 0};

  check_string (filename);
  if (xstring_length (filename) >= WPATH_MAX)
    FEsimple_error (Epath_name_too_long, filename);

  char file[PATH_MAX + 1];
  w2s (file, filename);

  for (const char *const *e = ext; *e; e++)
    for (lisp p = xsymbol_value (Vload_path); consp (p); p = xcdr (p))
      {
        lisp x = xcar (p);
        if (stringp (x) && xstring_length (x) < WPATH_MAX)
          {
            char path[PATH_MAX * 2 + 1];
            pathname2cstr (x, path);
            int l = strlen (path);
            if (l && path[l - 1] != SEPCHAR)
              path[l++] = SEPCHAR;
            strcpy (stpcpy (path + l, file), *e);
            DWORD a = WINFS::GetFileAttributes (path);
            if (a != DWORD (-1) && !(a & FILE_ATTRIBUTE_DIRECTORY))
              return make_string (path);
          }
      }
  return Qnil;
}

void
FileTime::file_modtime (lisp filename, int dir_ok)
{
  char path[PATH_MAX + 1];
  pathname2cstr (filename, path);
  WIN32_FIND_DATA fd;
  if (!WINFS::get_file_data (path, fd)
      || (!dir_ok && fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
    clear ();
  else
    {
      dwLowDateTime = fd.ftLastWriteTime.dwLowDateTime;
      dwHighDateTime = fd.ftLastWriteTime.dwHighDateTime;
    }
}

lisp
Fcwd ()
{
  return xsymbol_value (Qdefault_dir);
}

lisp
Fmake_temp_file_name (lisp lprefix, lisp lsuffix, lisp dir, lisp dirp)
{
  char temp[PATH_MAX + 1], prefix[32], suffix[32];

  if (lprefix == Qnil)
    lprefix = 0;
  if (lprefix)
    {
      check_string (lprefix);
      if (xstring_length (lprefix) > 10)
        FEsimple_error (Eprefix_too_long, lprefix);
      w2s (prefix, lprefix);
    }

  if (lsuffix == Qnil)
    lsuffix = 0;
  if (lsuffix)
    {
      check_string (lsuffix);
      if (xstring_length (lsuffix) > 10)
        FEsimple_error (Esuffix_too_long, lsuffix);
      w2s (suffix, lsuffix);
    }

  if (dir && dir != Qnil)
    {
      if (Ffile_directory_p (dir) == Qnil)
        file_error (Enot_a_directory, dir);
      pathname2cstr (dir, temp);
    }
  else if (!GetTempPath (sizeof temp, temp))
    file_error (Ecannot_make_temp_file_name);
  char *sl = find_last_slash (temp);
  if (!sl)
    file_error (Ecannot_make_temp_file_name);
  if (sl[1])
    strcat (sl, "/");
  if (!make_temp_file_name (temp, lprefix ? prefix : 0, lsuffix ? suffix : 0,
                            0, dirp && dirp != Qnil))
    file_error (Ecannot_make_temp_file_name);
  map_backsl_to_sl (temp);
  return make_string (temp);
}

lisp
Ffile_write_time (lisp file)
{
  FileTime t (file, 1);
  if (t.voidp ())
    return Qnil;
#if 1
  return file_time_to_universal_time (t);
#else
  SYSTEMTIME st;
  FileTimeToSystemTime (&t, &st);
  return decoded_time_to_universal_time (st.wYear, st.wMonth, st.wDay,
                                         st.wHour, st.wMinute, st.wSecond, 0);
#endif
}

lisp
Fset_file_write_time (lisp lpath, lisp lutc)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  decoded_time dt;
  dt.timezone = dt.daylight = 0;
  decode_universal_time (lutc, &dt);
  SYSTEMTIME st;
  st.wYear = dt.year;
  st.wMonth = dt.mon;
  st.wDay = dt.day;
  st.wHour = dt.hour;
  st.wMinute = dt.min;
  st.wSecond = dt.sec;
  st.wMilliseconds = 0;
  FILETIME ft;
  SystemTimeToFileTime (&st, &ft);
  dyn_handle w (WINFS::CreateFile (path, GENERIC_WRITE,
                                   FILE_SHARE_READ | FILE_SHARE_WRITE, 0,
                                   OPEN_EXISTING,
                                   FILE_FLAG_BACKUP_SEMANTICS, 0));
  if (!w.valid () || !SetFileTime (w, 0, 0, &ft))
    file_error (GetLastError (), lpath);
  return Qt;
}

lisp
Ffile_newer_than_file_p (lisp file1, lisp file2)
{
  FileTime t1 (file1, 1), t2 (file2, 1);
  if (t1.voidp ())
    return Qnil;
  if (t2.voidp ())
    return Qt;
  return boole (t1 > t2);
}

static lisp
exist_option (lisp option, lisp keys)
{
  lisp x = find_keyword (option, keys);
  if (x == Qnil)
    x = Kerror;
  else if (x != Kskip && x != Kerror)
    FEprogram_error ((option == Kif_exists
                      ? Einvalid_if_exists_option
                      : Einvalid_if_does_not_exist_option),
                     x);
  return x;
}

static lisp
access_denied_option (lisp keys)
{
  lisp x = find_keyword (Kif_access_denied, keys);
  if (x == Qnil)
    x = Kerror;
  else if (x != Kerror && x != Kforce && x != Kskip)
    FEprogram_error (Einvalid_if_access_denied_option, x);
  return x;
}

static DWORD
solve_access_denied (lisp access_denied, const char *path, lisp lpath)
{
  if (access_denied == Kskip)
    return DWORD (-1);
  if (access_denied == Kforce)
    {
      DWORD atr = WINFS::GetFileAttributes (path);
      if (atr == -1)
        file_error (GetLastError (), lpath);
      if (!(atr & (FILE_ATTRIBUTE_HIDDEN
                   | FILE_ATTRIBUTE_READONLY
                   | FILE_ATTRIBUTE_SYSTEM)))
        file_error (ERROR_ACCESS_DENIED, lpath);
      if (!WINFS::SetFileAttributes (path, (atr & ~(FILE_ATTRIBUTE_HIDDEN
                                                    | FILE_ATTRIBUTE_READONLY
                                                    | FILE_ATTRIBUTE_SYSTEM))))
        file_error (GetLastError (), lpath);
      return atr;
    }
  else
    file_error (ERROR_ACCESS_DENIED, lpath);
  return 0; // not reached
}

lisp
Fdelete_file (lisp name, lisp keys)
{
  char buf[PATH_MAX + 10];
  pathname2cstr (name, buf);
  lisp not_exist = exist_option (Kif_does_not_exist, keys);
  lisp access_denied = access_denied_option (keys);
  if (find_keyword_bool (Krecycle, keys))
    {
      typedef int (WINAPI *SHFILEOPERATION)(SHFILEOPSTRUCT *);
      SHFILEOPERATION f = (SHFILEOPERATION)GetProcAddress (GetModuleHandle ("shell32"),
                                                           "SHFileOperation");
      if (!f)
        FEsimple_error (ESHFileOperation_not_supported);

      map_sl_to_backsl (buf);
      buf[strlen (buf) + 1] = 0;

      SHFILEOPSTRUCT fs = {0};
      fs.wFunc = FO_DELETE;
      fs.pFrom = buf;
#ifndef FOF_NOERRORUI
#define FOF_NOERRORUI 0x0400
#endif
      fs.fFlags = (FOF_ALLOWUNDO | FOF_FILESONLY | FOF_NOCONFIRMATION
                   | FOF_NOERRORUI | FOF_SILENT);
      if ((*f)(&fs))
        FEfile_error (Edelete_failed, name);
    }
  else
    {
      if (!WINFS::DeleteFile (buf))
        {
          int e = GetLastError ();
          if (e == ERROR_ACCESS_DENIED)
            {
              DWORD atr = solve_access_denied (access_denied, buf, name);
              if (atr == -1)
                return Qnil;
              if (WINFS::DeleteFile (buf))
                return Qt;
              e = GetLastError ();
              WINFS::SetFileAttributes (buf, atr);
            }
          if (e == ERROR_FILE_NOT_FOUND && not_exist == Kskip)
            return Qnil;
          file_error (e, name);
        }
    }
  return Qt;
}

static int
copyn (char *d, const char *s, int n, int l)
{
  if (n < l)
    l = check_kanji2 (s, n) ? n - 1 : n;
  memcpy (d, s, l);
  d[l] = 0;
  return l;
}

static void
rename_short_name (const char *fpath, const char *tname, const char *longname)
{
  char temppath[PATH_MAX + 1], tempname[PATH_MAX + 1], realpath[PATH_MAX + 1];
  int l = tname - fpath;
  memcpy (temppath, fpath, l);
  temppath[l] = 0;
  map_sl_to_backsl (temppath);
  memcpy (realpath, fpath, l);
  strcpy (realpath + l, longname);

  if (!GetTempFileName (temppath, "xyz", 0, tempname))
    return;
  if (!WINFS::DeleteFile (tempname)
      || !WINFS::MoveFile (realpath, tempname))
    return;

  HANDLE h = WINFS::CreateFile (fpath, GENERIC_READ, 0, 0, CREATE_NEW,
                                FILE_ATTRIBUTE_ARCHIVE, 0);

  if (h != INVALID_HANDLE_VALUE)
    {
      int r = WINFS::MoveFile (tempname, realpath);
      CloseHandle (h);
      WINFS::DeleteFile (fpath);
      if (r)
        return;
    }

  if (WINFS::MoveFile (tempname, realpath))
    return;

  if (WINFS::MoveFile (tempname, fpath)
      && WINFS::MoveFile (fpath, realpath))
    return;

  char buf[PATH_MAX * 3];
  map_backsl_to_sl (tempname);
  sprintf (buf, get_message_string (Erename_failed), tempname, realpath);
  MsgBox (get_active_window (), buf, TitleBarString,
          MB_OK | MB_ICONEXCLAMATION,
          xsymbol_value (Vbeep_on_error) != Qnil);
}

static void
check_short_names (const char *from_path, const char *to_path)
{
  if (xsymbol_value (Vrename_alternate_file_name) == Qnil)
    return;

  WIN32_FIND_DATA from_fd, to_fd;
  if (!WINFS::get_file_data (to_path, to_fd))
    return;

  if (!*to_fd.cAlternateFileName
      || strcaseeq (to_fd.cFileName, to_fd.cAlternateFileName))
    return;

  if (!WINFS::get_file_data (from_path, from_fd))
    return;

  if (*from_fd.cAlternateFileName
      && !strcaseeq (from_fd.cFileName, from_fd.cAlternateFileName))
    return;

  if (!strcaseeq (from_fd.cFileName, to_fd.cAlternateFileName))
    return;

  const char *sf = find_last_slash (from_path);
  if (!sf || !strcaseeq (sf + 1, from_fd.cFileName))
    return;

  const char *st = find_last_slash (to_path);
  if (!st || !strcaseeq (st + 1, to_fd.cAlternateFileName))
    return;

  rename_short_name (to_path, st + 1, to_fd.cFileName);
}

enum
{
  OFW_OK1,
  OFW_OK2,
  OFW_SKIP,
  OFW_BAD
};

class safe_write_handle: public dyn_handle
{
  int sw_complete;
  int sw_delete_if_fail;
  DWORD sw_atr;
  char sw_path[PATH_MAX + 1];
public:
  safe_write_handle (lisp);
  ~safe_write_handle ();
  void set_org_atr (DWORD atr) {sw_atr = atr;}
  void complete () {sw_complete = 1;}
  int open_for_write (lisp, int, int &);
  const char *path () const {return sw_path;}
  int ensure_room (LONG, LONG);
};

safe_write_handle::safe_write_handle (lisp path)
     : sw_complete (0), sw_delete_if_fail (0), sw_atr (DWORD (-1))
{
  pathname2cstr (path, sw_path);
}

safe_write_handle::~safe_write_handle ()
{
  if (!sw_complete && valid ())
    {
      if (sw_atr != -1)
        WINFS::SetFileAttributes (sw_path, sw_atr);
      if (sw_delete_if_fail)
        {
          close ();
          WINFS::DeleteFile (sw_path);
        }
    }
}

int
safe_write_handle::open_for_write (lisp if_exists, int open_mode, int &e)
{
  sw_delete_if_fail = 0;
  fix (WINFS::CreateFile (sw_path, GENERIC_WRITE, 0, 0, open_mode,
                          FILE_ATTRIBUTE_ARCHIVE | FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (valid ())
    {
      if (open_mode == OPEN_EXISTING)
        ;
      else if (open_mode != OPEN_ALWAYS
               || GetLastError () != ERROR_ALREADY_EXISTS)
        sw_delete_if_fail = 1;
      return OFW_OK1;
    }

  e = GetLastError ();
  if (if_exists == Kskip)
    return ((e == ERROR_FILE_EXISTS || e == ERROR_ALREADY_EXISTS)
            ? OFW_SKIP : OFW_BAD);

  if (if_exists != Knewer || e != ERROR_FILE_NOT_FOUND)
    return OFW_BAD;

  fix (WINFS::CreateFile (sw_path, GENERIC_WRITE, 0, 0, CREATE_ALWAYS,
                          FILE_ATTRIBUTE_ARCHIVE | FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (valid ())
    {
      sw_delete_if_fail = 1;
      return OFW_OK2;
    }
  e = GetLastError ();
  return OFW_BAD;
}

int
safe_write_handle::ensure_room (LONG hi, LONG lo)
{
  if (SetFilePointer (*this, lo, &hi, FILE_BEGIN) == -1)
    {
      int e = GetLastError ();
      if (e != NO_ERROR)
        return e;
    }
  if (!SetEndOfFile (*this))
    return GetLastError ();

  sw_delete_if_fail = 1;

  if (SetFilePointer (*this, 0, 0, FILE_BEGIN) == -1)
    return GetLastError ();

  return NO_ERROR;
}

lisp
Fcopy_file (lisp from_name, lisp to_name, lisp keys)
{
  char fromf[PATH_MAX + 1];
  pathname2cstr (from_name, fromf);

  safe_write_handle w (to_name);

  check_short_names (fromf, w.path ());

  lisp if_exists = find_keyword (Kif_exists, keys);
  if (if_exists == Qnil)
    if_exists = Kerror;

  int copy_attrib = find_keyword_bool (Kcopy_attributes, keys, 1);

  int open_mode;
  if (if_exists == Kerror)
    open_mode = CREATE_NEW;
  else if (if_exists == Koverwrite)
    open_mode = OPEN_ALWAYS;
  else if (if_exists == Kskip)
    open_mode = CREATE_NEW;
  else if (if_exists == Knewer)
    open_mode = OPEN_EXISTING;
  else
    FEprogram_error (Einvalid_if_exists_option, if_exists);

  lisp access_denied = access_denied_option (keys);

  dyn_handle r (WINFS::CreateFile (fromf, GENERIC_READ, FILE_SHARE_READ, 0,
                                   OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (!r.valid ())
    {
      r.fix (WINFS::CreateFile (fromf, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                0, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0));
      if (!r.valid ())
        file_error (GetLastError (), from_name);
    }

  int e;
  int ofw = w.open_for_write (if_exists, open_mode, e);
  if (ofw == OFW_BAD && e == ERROR_ACCESS_DENIED)
    {
      DWORD atr = solve_access_denied (access_denied, w.path (), to_name);
      if (atr == -1)
        return Qnil;
      w.set_org_atr (atr);
      ofw = w.open_for_write (if_exists, open_mode, e);
    }

  switch (ofw)
    {
    case OFW_OK1:
      if (if_exists == Knewer)
        {
          FILETIME tr, tw;
          if (!GetFileTime (r, 0, 0, &tr))
            file_error (GetLastError (), from_name);
          if (!GetFileTime (w, 0, 0, &tw))
            file_error (GetLastError (), to_name);
          if (CompareFileTime (&tr, &tw) <= 0)
            return Qnil;
        }
      break;

    case OFW_OK2:
      break;

    case OFW_SKIP:
      return Qnil;

    default:
      file_error (e, to_name);
    }

  DWORD hi, lo;
  lo = GetFileSize (r, &hi);
  if (lo == -1 && (e = GetLastError ()) != NO_ERROR)
    file_error (e, from_name);

  e = w.ensure_room (hi, lo);
  if (e != NO_ERROR)
    file_error (e, to_name);

  while (1)
    {
      DWORD nread, nwrite;
      char buf[0x10000];
      if (!ReadFile (r, buf, sizeof buf, &nread, 0))
        file_error (GetLastError (), from_name);
      if (!WriteFile (w, buf, nread, &nwrite, 0))
        file_error (GetLastError (), to_name);
      if (nread != nwrite)
        file_error (Ewrite_error, to_name);
      if (nread < sizeof buf)
        break;
    }

  if (!SetEndOfFile (w))
    file_error (GetLastError (), to_name);

  w.complete ();

  if (copy_attrib)
    {
      FILETIME t;
      if (GetFileTime (r, 0, 0, &t))
        SetFileTime (w, 0, 0, &t);
      DWORD atr = WINFS::GetFileAttributes (fromf);
      if (atr != -1)
        WINFS::SetFileAttributes (w.path (), atr);
    }

  return Qt;
}

lisp
Frename_file (lisp from_name, lisp to_name, lisp keys)
{
  char fromf[PATH_MAX + 1], tof[PATH_MAX + 1];
  pathname2cstr (from_name, fromf);
  pathname2cstr (to_name, tof);

  check_short_names (fromf, tof);

  lisp if_exists = find_keyword (Kif_exists, keys);
  if (if_exists == Qnil)
    if_exists = Kerror;

  if (if_exists != Kerror && if_exists != Koverwrite
      && if_exists != Kskip && if_exists != Knewer)
    FEprogram_error (Einvalid_if_exists_option, if_exists);

  lisp access_denied = access_denied_option (keys);

  while (1)
    {
      if (WINFS::MoveFile (fromf, tof))
        return Qt;

      int e = GetLastError ();
      switch (e)
        {
        case ERROR_ALREADY_EXISTS:
        case ERROR_FILE_EXISTS:
          if (if_exists == Kerror)
            file_error (e, to_name);
          if (if_exists == Kskip)
            return Qnil;
          if (if_exists == Knewer)
            {
              dyn_handle f (WINFS::CreateFile (fromf, 0, 0, 0, OPEN_EXISTING, 0, 0));
              if (!f.valid ())
                file_error (GetLastError (), from_name);
              dyn_handle t (WINFS::CreateFile (tof, 0, 0, 0, OPEN_EXISTING, 0, 0));
              if (!t.valid ())
                file_error (GetLastError (), to_name);
              FILETIME tf, tt;
              if (!GetFileTime (f, 0, 0, &tf))
                file_error (GetLastError (), from_name);
              if (!GetFileTime (t, 0, 0, &tt))
                file_error (GetLastError (), to_name);
              if (CompareFileTime (&tf, &tt) <= 0)
                return Qnil;
            }
          if (!WINFS::DeleteFile (tof))
            {
              e = GetLastError ();
              DWORD atr = WINFS::GetFileAttributes (tof);
              if (atr != DWORD (-1) && atr & FILE_ATTRIBUTE_DIRECTORY)
                file_error (ERROR_ACCESS_DENIED, to_name);
              if (e != ERROR_ACCESS_DENIED)
                file_error (e, to_name);
              atr = solve_access_denied (access_denied, tof, to_name);
              if (atr == -1)
                return Qnil;
              if (!WINFS::DeleteFile (tof))
                {
                  e = GetLastError ();
                  WINFS::SetFileAttributes (tof, atr);
                  file_error (e, to_name);
                }
            }
          break;

        default:
          {
            dyn_handle f (WINFS::CreateFile (fromf, GENERIC_READ | GENERIC_WRITE, 0, 0,
                                             OPEN_EXISTING, 0, 0));
            if (!f.valid ())
              file_error (e, from_name);
            file_error (e, to_name);
          }
        }
    }
}

static int
mkdirhier (char *path, int exists_ok)
{
  if (WINFS::CreateDirectory (path, 0))
    return 1;
  if (exists_ok)
    {
      DWORD a = WINFS::GetFileAttributes (path);
      if (a != -1 && a & FILE_ATTRIBUTE_DIRECTORY)
        return 1;
    }
  else
    {
      int e = GetLastError ();
      if (e == ERROR_FILE_EXISTS || e == ERROR_ALREADY_EXISTS)
        return 0;
    }
  map_sl_to_backsl (path);
  for (char *p = path; (p = jindex (p, '\\')); *p++ = '\\')
    {
      *p = 0;
      WINFS::CreateDirectory (path, 0);
    }
  if (WINFS::CreateDirectory (path, 0))
    return 1;
  DWORD a = WINFS::GetFileAttributes (path);
  return a != -1 && a & FILE_ATTRIBUTE_DIRECTORY;
}

lisp
Fcreate_directory (lisp dirname, lisp keys)
{
  char name[PATH_MAX + 1];
  pathname2cstr (dirname, name);
  lisp if_exists = exist_option (Kif_exists, keys);
  if (!mkdirhier (name, if_exists == Kskip))
    {
      int e = GetLastError ();
      if (!e)
        e = ERROR_FILE_EXISTS;
      file_error (e, dirname);
    }
  return Qt;
}

lisp
Fdelete_directory (lisp dirname, lisp keys)
{
  char name[PATH_MAX + 1];
  pathname2cstr (dirname, name);
  lisp not_exist = exist_option (Kif_does_not_exist, keys);
  lisp access_denied = access_denied_option (keys);
  if (!WINFS::RemoveDirectory (name))
    {
      int e = GetLastError ();
      if (e == ERROR_ACCESS_DENIED)
        {
          DWORD atr = solve_access_denied (access_denied, name, dirname);
          if (atr == -1)
            return Qnil;
          if (WINFS::RemoveDirectory (name))
            return Qt;
          e = GetLastError ();
          WINFS::SetFileAttributes (name, atr);
        }
      if (e == ERROR_FILE_NOT_FOUND && not_exist == Kskip)
        return Qnil;
      file_error (e, dirname);
    }
  return Qt;
}

static lisp
map_sl (lisp path, Char from, Char to)
{
  check_string (path);
  Char *p0 = (Char *)alloca (xstring_length (path) * sizeof *p0);
  bcopy (xstring_contents (path), p0, xstring_length (path));
  int f = 0;
  for (Char *p = p0, *pe = p0 + xstring_length (path); p < pe; p++)
    if (*p == from)
      {
        *p = to;
        f = 1;
      }
  return f ? make_string (p0, xstring_length (path)) : path;
}

lisp
Fmap_slash_to_backslash (lisp path)
{
  return map_sl (path, '/', '\\');
}

lisp
Fmap_backslash_to_slash (lisp path)
{
  return map_sl (path, '\\', '/');
}

static void
wnet_error ()
{
  DWORD e = GetLastError ();
  if (e == ERROR_SUCCESS)
    return;
  if (e != ERROR_EXTENDED_ERROR)
    FEsimple_win32_error (e);

  char n[1024], d[1024];
  *n = 0, *d = 0;
  WNetGetLastError (&e, d, sizeof d, n, sizeof n);
  FEnetwork_error (make_string (n), make_string (d));
}

lisp
Fnetwork_connect_dialog ()
{
  if (WNetConnectionDialog (get_active_window (), RESOURCETYPE_DISK) == NO_ERROR)
    return Qt;
  wnet_error ();
  return Qnil;
}

lisp
Fnetwork_disconnect_dialog ()
{
  if (WNetDisconnectDialog (get_active_window (), RESOURCETYPE_DISK) == NO_ERROR)
    return Qt;
  wnet_error ();
  return Qnil;
}

lisp
Fget_file_attributes (lisp lpath)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  DWORD atr = WINFS::GetFileAttributes (path);
  if (atr == -1)
    file_error (GetLastError (), lpath);
  return make_fixnum (atr);
}

#define VALID_FILE_ATTRIBUTES \
  (FILE_ATTRIBUTE_ARCHIVE \
   | FILE_ATTRIBUTE_NORMAL \
   | FILE_ATTRIBUTE_HIDDEN \
   | FILE_ATTRIBUTE_READONLY \
   | FILE_ATTRIBUTE_SYSTEM)

lisp
Fset_file_attributes (lisp lpath, lisp latr)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  DWORD atr = fixnum_value (latr) & VALID_FILE_ATTRIBUTES;
  if (!WINFS::SetFileAttributes (path, atr))
    file_error (GetLastError (), lpath);
  return Qt;
}

lisp
Fmodify_file_attributes (lisp lpath, lisp lon, lisp loff)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  DWORD on = fixnum_value (lon) & VALID_FILE_ATTRIBUTES;
  DWORD off = ((loff && loff != Qnil)
               ? (fixnum_value (loff) & VALID_FILE_ATTRIBUTES) : 0);
  DWORD atr = WINFS::GetFileAttributes (path);
  if (atr == -1)
    file_error (GetLastError (), lpath);
  if (!WINFS::SetFileAttributes (path, (atr & ~off) | on))
    file_error (GetLastError (), lpath);
  return Qt;
}

int
strict_get_file_data (const char *path, WIN32_FIND_DATA &fd)
{
  for (const u_char *p = (const u_char *)path; *p;)
    {
      if (SJISP (*p) && p[1])
        p += 2;
      else
        {
          if (*p == '?' || *p == '*')
            {
              SetLastError (ERROR_INVALID_NAME);
              return 0;
            }
          p++;
        }
    }
  return WINFS::get_file_data (path, fd);
}

lisp
Ffile_length (lisp lpath)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  WIN32_FIND_DATA fd;
  if (!strict_get_file_data (path, fd))
    return Qnil;
  large_int i;
  i.hi = fd.nFileSizeHigh;
  i.lo = fd.nFileSizeLow;
  return make_integer (i);
}

struct gdu
{
  int recursive;
  double nbytes;
  double blocks;
  u_long blocksize;
  int nfiles;
  int ndirs;
};

static void
get_disk_usage (char *path, gdu *du)
{
  QUIT;
  int l = strlen (path);
  if (l >= PATH_MAX)
    return;
  char *pe = path + l;
  *pe = '*';
  pe[1] = 0;

  WIN32_FIND_DATA fd;
  HANDLE h = WINFS::FindFirstFile (path, &fd);
  if (h != INVALID_HANDLE_VALUE)
    {
      find_handle fh (h);
      do
        {
          if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
              if (!du->recursive
                  || (*fd.cFileName == '.'
                      && (!fd.cFileName[1]
                          || (fd.cFileName[1] == '.' && !fd.cFileName[2]))))
                continue;
              du->ndirs++;
              strcpy (stpcpy (pe, fd.cFileName), "/");
              get_disk_usage (path, du);
            }
          else
            {
              double size = ((fd.nFileSizeHigh
                              * double (1 << (sizeof (DWORD) * 4))
                              * double (1 << (sizeof (DWORD) * 4)))
                             + fd.nFileSizeLow);
              du->nbytes += size;
              du->blocks += ceil (size / du->blocksize);
              du->nfiles++;
            }
        }
      while (WINFS::FindNextFile (h, &fd));
    }
}

lisp
Fget_disk_usage (lisp dirname, lisp recursive)
{
  char path[PATH_MAX * 2];
  pathname2cstr (dirname, path);
  char *p = jrindex (path, '/');
  if (p && p[1])
    strcat (p, "/");
  gdu du;
  bzero (&du, sizeof du);

  WINFS::SetCurrentDirectory (path);
  DWORD SectorsPerCluster;
  DWORD BytesPerSector;
  DWORD FreeClusters;
  DWORD Clusters;
  int f = WINFS::GetDiskFreeSpace (0, &SectorsPerCluster, &BytesPerSector,
                                   &FreeClusters, &Clusters);
  int e = GetLastError ();
  WINFS::SetCurrentDirectory (sysdep.curdir);

  if (!f)
    file_error (e, dirname);

  if (!recursive)
    recursive = Qnil;
  du.recursive = recursive == Qt;
  du.blocksize = SectorsPerCluster * BytesPerSector;
  if (!du.blocksize)
    du.blocksize = 1024;
  if (recursive == Qnil || recursive == Qt)
    get_disk_usage (path, &du);

  lisp block = make_fixnum (du.blocksize);
  multiple_value::value (1) =
    number_multiply (make_integer (long_to_large_int (Clusters)), block);
  multiple_value::value (2) =
    number_multiply (make_integer (long_to_large_int (FreeClusters)), block);
  multiple_value::value (3) =
    number_multiply (make_integer (double_to_bignum_rep (du.blocks)), block);
  multiple_value::value (4) = make_integer (double_to_bignum_rep (du.nbytes));
  multiple_value::value (5) = make_fixnum (du.ndirs);
  multiple_value::value (6) = make_fixnum (du.nfiles);
  multiple_value::count () = 7;
  return block;
}

lisp
Fformat_drive (lisp ldrive, lisp lquick)
{
  int drive;
  if (!ldrive || ldrive == Qnil)
    drive = 0;
  else if (charp (ldrive))
    {
      Char c = xchar_code (ldrive);
      if (lower_char_p (c))
        drive = c - 'a';
      else if (upper_char_p (c))
        drive = c - 'A';
      else
        FErange_error (ldrive);
    }
  else
    {
      drive = fixnum_value (ldrive);
      if (drive < 0 || drive >= 26)
        FErange_error (ldrive);
    }

  HMODULE shell = GetModuleHandle ("shell32.dll");
  if (!shell)
    FEsimple_win32_error (GetLastError ());

  int (__stdcall *fmt)(HWND, int, int, int) =
    (int (__stdcall *)(HWND, int, int, int))GetProcAddress (shell, "SHFormatDrive");
  if (!fmt)
    FEsimple_win32_error (GetLastError ());

  HWND hwnd = get_active_window ();
  HWND focus = GetFocus ();
  int f = (*fmt)(hwnd, drive, 0, lquick && lquick != Qnil);
  EnableWindow (hwnd, 1);
  SetFocus (focus);
  return boole (!f);
}

lisp
Fcompare_file (lisp file1, lisp file2)
{
  char path1[PATH_MAX + 1], path2[PATH_MAX + 1];
  pathname2cstr (file1, path1);
  pathname2cstr (file2, path2);
  mapf mf1, mf2;
  if (!mf1.open (path1))
    file_error (GetLastError (), file1);
  if (!mf2.open (path2))
    file_error (GetLastError (), file2);

  if (mf1.size () != mf2.size ())
    return Qnil;

  lisp r = Qnil;
  try
    {
      r = boole (!memcmp (mf1.base (), mf2.base (), mf1.size ()));
    }
  catch (Win32Exception &e)
    {
      if (e.code == EXCEPTION_IN_PAGE_ERROR)
        FEsimple_win32_error (ERROR_FILE_CORRUPT);
      throw e;
    }
  return r;
}

lisp
Ffile_property (lisp lpath)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  map_sl_to_backsl (path);

  HMODULE shell = GetModuleHandle ("shell32.dll");
  if (!shell)
    FEsimple_win32_error (GetLastError ());

  int (__stdcall *ex)(SHELLEXECUTEINFO *) =
    (int (__stdcall *)(SHELLEXECUTEINFO *))GetProcAddress (shell, "ShellExecuteExA");
  if (!ex)
    FEsimple_win32_error (GetLastError ());

  SHELLEXECUTEINFO sei;
  bzero (&sei, sizeof sei);
  sei.cbSize = sizeof sei;
  sei.lpFile = path;
  sei.lpVerb = "properties";
  sei.fMask = SEE_MASK_INVOKEIDLIST;
  if (!(*ex)(&sei))
    FEsimple_win32_error (GetLastError ());
  return Qt;
}

#define LOCK_TIMEOUT 10000
#define LOCK_RETRIES 20

#ifndef FILE_DEVICE_MASS_STORAGE
#define FILE_DEVICE_MASS_STORAGE 0x0000002d
#endif
#ifndef IOCTL_STORAGE_BASE
#define IOCTL_STORAGE_BASE FILE_DEVICE_MASS_STORAGE
#endif
#ifndef IOCTL_STORAGE_MEDIA_REMOVAL
#define IOCTL_STORAGE_MEDIA_REMOVAL \
  CTL_CODE (IOCTL_STORAGE_BASE, 0x0201, METHOD_BUFFERED, FILE_READ_ACCESS)
#endif
#ifndef IOCTL_STORAGE_EJECT_MEDIA
#define IOCTL_STORAGE_EJECT_MEDIA \
  CTL_CODE (IOCTL_STORAGE_BASE, 0x0202, METHOD_BUFFERED, FILE_READ_ACCESS)
#endif

static lisp
eject_media_winnt (int drive, int type)
{
  int flags = 0;
  switch (type)
    {
    default:
      file_error (ERROR_BAD_DEVICE);

    case DRIVE_REMOVABLE:
      flags = GENERIC_READ | GENERIC_WRITE;
      break;

    case DRIVE_CDROM:
      flags = GENERIC_READ;
      break;
    }

  char dev[] = "\\\\.\\x:";
  dev[4] = char (drive);

  dyn_handle h (CreateFile (dev, flags, FILE_SHARE_READ | FILE_SHARE_WRITE,
                            0, OPEN_EXISTING, 0, 0));
  if (!h.valid ())
    file_error (GetLastError ());

  DWORD nbytes;
  for (int retry = 0;; retry++)
    {
      if (DeviceIoControl (h, FSCTL_LOCK_VOLUME, 0, 0, 0, 0, &nbytes, 0))
        break;
      if (retry == LOCK_RETRIES)
        file_error (GetLastError ());
      Sleep (LOCK_TIMEOUT / LOCK_RETRIES);
    }

  if (!DeviceIoControl (h, FSCTL_DISMOUNT_VOLUME, 0, 0, 0, 0, &nbytes, 0))
    file_error (GetLastError ());

  PREVENT_MEDIA_REMOVAL pmr;
  pmr.PreventMediaRemoval = 0;
  if (!DeviceIoControl (h, IOCTL_STORAGE_MEDIA_REMOVAL,
                        &pmr, sizeof pmr, 0, 0, &nbytes, 0))
    file_error (GetLastError ());

  if (!DeviceIoControl (h, IOCTL_STORAGE_EJECT_MEDIA, 0, 0, 0, 0, &nbytes, 0))
    file_error (GetLastError ());

  return Qt;
}

static const u_char fat32_devcat[] = {0x48, 0x08};

static int
win9x_lock_logical_volume (HANDLE hvwin32, int drive, int lock_level, int perm)
{
  for (int i = 0; i < numberof (fat32_devcat); i++)
    {
      DIOC_REGISTERS regs = {0};
      regs.reg_EAX = 0x440d;
      regs.reg_EBX = MAKEWORD (drive, lock_level);
      regs.reg_ECX = MAKEWORD (0x4a, fat32_devcat[i]);
      regs.reg_EDX = perm;

      DWORD nbytes;
      if (DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_IOCTL,
                           &regs, sizeof regs, &regs, sizeof regs,
                           &nbytes, 0)
          && !(regs.reg_Flags & X86_CARRY_FLAG))
        return 1;
    }
  return 0;
}

static int
win9x_unlock_logical_volume (HANDLE hvwin32, int drive)
{
  for (int i = 0; i < numberof (fat32_devcat); i++)
    {
      DIOC_REGISTERS regs = {0};
      regs.reg_EAX = 0x440d;
      regs.reg_EBX = drive;
      regs.reg_ECX = MAKEWORD (0x6A, fat32_devcat[i]);

      DWORD nbytes;
      if (DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_IOCTL,
                           &regs, sizeof regs, &regs, sizeof regs,
                           &nbytes, 0)
          && !(regs.reg_Flags & X86_CARRY_FLAG))
        return 1;
    }
  return 0;
}

static int
win9x_unlock_media (HANDLE hvwin32, int drive)
{
  DIOC_REGISTERS regs = {0};
  PARAMBLOCK pb;
  pb.bOperation = 2;
  pb.bNumLocks = 0;

  regs.reg_EAX = 0x440d;
  regs.reg_EBX = drive;
  regs.reg_ECX = MAKEWORD (0x48, 0x08);
  regs.reg_EDX = DWORD (&pb);

  DWORD nbytes;
  if (!DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_IOCTL,
                        &regs, sizeof regs, &regs, sizeof regs,
                        &nbytes, 0)
      || (regs.reg_Flags & X86_CARRY_FLAG
          && regs.reg_EAX != 0x01 && regs.reg_EAX != 0xb0))
    return 0;

  for (int i = 0; i < pb.bNumLocks; ++i)
    {
      pb.bOperation = 1;
      regs.reg_EAX = 0x440d;
      regs.reg_EBX = drive;
      regs.reg_ECX = MAKEWORD (0x48, 0x08);
      regs.reg_EDX = DWORD (&pb);

      if (!DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_IOCTL,
                            &regs, sizeof regs, &regs, sizeof regs,
                            &nbytes, 0)
          || regs.reg_Flags & X86_CARRY_FLAG)
        return 0;
    }
  return 1;
}

static int
win9x_eject_media (HANDLE hvwin32, int drive)
{
  DIOC_REGISTERS regs = {0};
  regs.reg_EAX = 0x440d;
  regs.reg_EBX = drive;
  regs.reg_ECX = MAKEWORD (0x49, 0x08);

  DWORD nbytes;
  return (DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_IOCTL,
                           &regs, sizeof regs, &regs, sizeof regs,
                           &nbytes, 0)
          && !(regs.reg_Flags & X86_CARRY_FLAG));
}

static lisp
eject_media_win9x (int drive)
{
  dyn_handle hvwin32 (CreateFile ("\\\\.\\vwin32", 0, 0, 0, 0,
                                  FILE_FLAG_DELETE_ON_CLOSE, 0));
  if (!hvwin32.valid ())
    file_error (GetLastError ());

  drive = char_upcase (drive) - ('A' - 1);
  int e = 0;

  if (!win9x_lock_logical_volume (hvwin32, drive, 0, 0))
    e = ERROR_DRIVE_LOCKED;
  else
    {
      if (!win9x_unlock_media (hvwin32, drive))
        e = ERROR_UNABLE_TO_LOCK_MEDIA;
      else if (!win9x_eject_media (hvwin32, drive))
        e = ERROR_UNABLE_TO_UNLOAD_MEDIA;
      win9x_unlock_logical_volume (hvwin32, drive);
    }
  if (e)
    file_error (e);
  return Qt;
}

lisp
Feject_media (lisp ldrive)
{
  check_char (ldrive);
  if (!alpha_char_p (xchar_code (ldrive)))
    file_error (ERROR_INVALID_DRIVE);

  char root[] = "x:\\";
  root[0] = char (xchar_code (ldrive));
  int type = GetDriveType (root);
  switch (type)
    {
    default:
      file_error (ERROR_BAD_DEVICE);

    case DRIVE_REMOVABLE:
    case DRIVE_CDROM:
      break;
    }

  return (sysdep.WinNTp ()
          ? eject_media_winnt (xchar_code (ldrive), type)
          : eject_media_win9x (xchar_code (ldrive)));
}

class list_net_resources: public worker_thread
{
protected:
  int m_error;
  int m_nomem;
  int m_pair;
  xstring_pair_list m_list;

  list_net_resources (int pair)
       : m_error (NO_ERROR), m_nomem (0), m_pair (pair) {}
  virtual void thread_main ();
  virtual void doit () = 0;
public:
  void signal_error () const;
  lisp make_list ();
};

void
list_net_resources::thread_main ()
{
  try
    {
      doit ();
    }
  catch (nonlocal_jump &)
    {
      m_nomem = 1;
    }
}

void
list_net_resources::signal_error () const
{
  if (m_nomem)
    FEstorage_error ();
  if (m_error != NO_ERROR)
    FEsimple_win32_error (m_error);
}

lisp
list_net_resources::make_list ()
{
  if (!start ())
    FEsimple_win32_error (GetLastError ());
  if (!wait ())
    FEquit ();
  signal_error ();
  return m_list.make_list (m_pair);
}

class list_servers: public list_net_resources
{
protected:
  virtual void doit () {list (0);}
  int list (NETRESOURCE *);
public:
  list_servers (int pair) : list_net_resources (pair) {}
};

int
list_servers::list (NETRESOURCE *r0)
{
  HANDLE h;
  m_error = WINFS::WNetOpenEnum (RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, r0, &h);
  if (m_error != NO_ERROR)
    return 0;
  wnet_enum_handle weh (h);
  while (!interrupted ())
    {
      NETRESOURCE rb[8192];
      DWORD nent = DWORD (-1), size = sizeof rb;
      m_error = WNetEnumResource (h, &nent, rb, &size);
      if (m_error == ERROR_NO_MORE_ITEMS)
        {
          m_error = NO_ERROR;
          return 1;
        }
      if (m_error != NO_ERROR)
        return 0;

      NETRESOURCE *r = rb;
      for (DWORD i = 0; i < nent && !interrupted (); i++, r++)
        switch (r->dwDisplayType)
          {
          case RESOURCEDISPLAYTYPE_GENERIC:
          case RESOURCEDISPLAYTYPE_DOMAIN:
          case RESOURCEDISPLAYTYPE_NETWORK:
            if (r->dwUsage & RESOURCEUSAGE_CONTAINER)
              list (r);
            break;

          case RESOURCEDISPLAYTYPE_SERVER:
            if (r->lpRemoteName)
              m_list.add (r->lpRemoteName + 2,
                          m_pair && r->lpComment ? r->lpComment : "");
            break;
          }
    }
  return 0;
}

lisp
Flist_servers (lisp comment_p)
{
  worker_thread_helper <list_servers> ls
    (new list_servers (comment_p && comment_p != Qnil));
  return ls->make_list ();
}

class list_server_resources: public list_net_resources
{
protected:
  virtual void doit ();
public:
  list_server_resources (lisp lserver, int pair);
  ~list_server_resources () {delete m_server;}
private:
  char *m_server;
};

list_server_resources::list_server_resources (lisp lserver, int pair)
     : list_net_resources (pair)
{
  check_string (lserver);
  m_server = new char [xstring_length (lserver) * 2 + 3];
  m_server[0] = '\\';
  m_server[1] = '\\';
  w2s (m_server + 2, lserver);
}

void
list_server_resources::doit ()
{
  int l = strlen (m_server) + 1;

  NETRESOURCE r;
  r.dwScope = RESOURCE_GLOBALNET;
  r.dwType = RESOURCETYPE_ANY;
  r.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER;
  r.dwUsage = RESOURCEUSAGE_CONTAINER;
  r.lpLocalName = 0;
  r.lpRemoteName = m_server;
  r.lpComment = "";
  r.lpProvider = 0;

  HANDLE h;
  m_error = WINFS::WNetOpenEnum (RESOURCE_GLOBALNET, RESOURCETYPE_DISK, 0, &r, &h);
  if (m_error != NO_ERROR)
    return;

  wnet_enum_handle weh (h);
  while (!interrupted ())
    {
      NETRESOURCE rb[8192];
      DWORD nent = DWORD (-1), size = sizeof rb;
      m_error = WNetEnumResource (h, &nent, rb, &size);
      if (m_error == ERROR_NO_MORE_ITEMS)
        {
          m_error = NO_ERROR;
          return;
        }
      if (m_error != NO_ERROR)
        return;

      NETRESOURCE *r = rb;
      for (DWORD i = 0; i < nent && !interrupted (); i++, r++)
        switch (r->dwDisplayType)
          {
          case RESOURCEDISPLAYTYPE_SHARE:
            if (r->lpRemoteName)
              m_list.add (r->lpRemoteName + l,
                          m_pair && r->lpComment ? r->lpComment : "");
            break;
          }
    }
}

lisp
Flist_server_resources (lisp lserver, lisp comment_p)
{
  worker_thread_helper <list_server_resources>
    ls (new list_server_resources (lserver, comment_p && comment_p != Qnil));
  return ls->make_list ();
}

lisp
Fset_per_device_directory (lisp lpath)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  if (!set_device_dir (path, 1))
    file_error (GetLastError (), lpath);
  WINFS::SetCurrentDirectory (sysdep.curdir);
  return Qt;
}

lisp
Fget_short_path_name (lisp lpath)
{
  char path[PATH_MAX + 1], spath[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  map_sl_to_backsl (path);
  if (!GetShortPathName (path, spath, PATH_MAX))
    file_error (GetLastError (), lpath);
  map_backsl_to_sl (spath);
  if (stringp (lpath) && xstring_length (lpath)
      && dir_separator_p (xstring_contents (lpath)[xstring_length (lpath) - 1]))
    {
      char *sl = find_last_slash (spath);
      if (sl && sl[1])
        strcat (sl, "/");
    }
  return make_string (spath);
}

lisp
make_file_info (const WIN32_FIND_DATA &fd)
{
  large_int sz;
  sz.hi = fd.nFileSizeHigh;
  sz.lo = fd.nFileSizeLow;
  return make_list (make_fixnum (fd.dwFileAttributes),
                    //file_time_to_universal_time (fd.ftCreationTime),
                    //file_time_to_universal_time (fd.ftLastAccessTime),
                    file_time_to_universal_time (fd.ftLastWriteTime),
                    make_integer (sz),
                    (*fd.cAlternateFileName
                     ? make_string (fd.cAlternateFileName)
                     : Qnil),
                    0);
}

lisp
Fget_file_info (lisp lpath)
{
  char path[PATH_MAX + 1];
  pathname2cstr (lpath, path);
  WIN32_FIND_DATA fd;
  if (!strict_get_file_data (path, fd))
    file_error (GetLastError (), lpath);
  return make_file_info (fd);
}

char *
root_path_name (char *buf, const char *path)
{
  if (*path && path[1] == ':')
    {
      buf[0] = path[0];
      buf[1] = ':';
      buf[2] = '\\';
      buf[3] = 0;
    }
  else
    {
      strcpy (buf, path);
      map_sl_to_backsl (buf);
      if (*buf == '\\')
        {
          if (buf[1] == '\\')
            {
              char *p = jindex (buf + 2, '\\');
              if (p)
                {
                  char *p2 = jindex (p + 1, '\\');
                  if (p2)
                    p2[1] = 0;
                  else
                    strcat (p + 1, "\\");
                }
            }
          else
            buf[1] = 0;
        }
    }
  return buf;
}
