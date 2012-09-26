#include "stdafx.h"
#include "ed.h"
#include "kanji.h"
#include "except.h"
#include "mman.h"
#include "encoding.h"

Chunk *
Buffer::read_chunk (ReadFileContext &rfc, xread_stream &sin)
{
  int c = sin.get (), c2;
  if (c == xread_stream::eof)
    return 0;
  sin.putback (c);

  Chunk *cp = alloc_chunk ();
  if (!cp)
    {
      rfc.r_status = ReadFileContext::RFCS_MEM;
      return 0;
    }

  int nlines = 0;
  Char *p = cp->c_text;
  Char *const pe = p + Chunk::TEXT_SIZE;
  switch (rfc.r_expect_eol)
    {
    case eol_guess:
      while (p < pe)
        {
          c = sin.get ();
          if (c == xread_stream::eof)
            break;
          if (c == '\n')
            {
              nlines++;
              rfc.r_expect_eol = rfc.r_eol_code = eol_lf;
              *p++ = c;
              goto lf;
            }
          else if (c == '\r')
            {
              rfc.r_cr++;
              c2 = sin.get ();
              if (c2 == '\n')
                {
                  nlines++;
                  rfc.r_expect_eol = rfc.r_eol_code = eol_crlf;
                  *p++ = c2;
                  goto crlf;
                }
              else
                sin.putback (c2);
            }
          else if (c == 'Z' - '@')
            {
              c2 = sin.get ();
              if (c2 == xread_stream::eof)
                goto done;
              sin.putback (c2);
            }
          *p++ = Char (c);
        }
      break;

    default:
    lf:
      while (p < pe)
        {
          c = sin.get ();
          if (c == xread_stream::eof)
            break;
          if (c == '\n')
            nlines++;
          *p++ = Char (c);
        }
      break;

    case eol_crlf:
    crlf:
      while (p < pe)
        {
          c = sin.get ();
          if (c == xread_stream::eof)
            break;
          if (c == '\n')
            nlines++;
          else if (c == '\r')
            {
              c2 = sin.get ();
              if (c2 == '\n')
                {
                  c = '\n';
                  nlines++;
                }
              else
                sin.putback (c2);
            }
          else if (c == 'Z' - '@')
            {
              c2 = sin.get ();
              if (c2 == xread_stream::eof)
                goto done;
              sin.putback (c2);
            }
          *p++ = Char (c);
        }
      break;

    case eol_cr:
      while (p < pe)
        {
          c = sin.get ();
          if (c == xread_stream::eof)
            break;
          if (c == '\n')
            nlines++;
          else if (c == '\r')
            {
              nlines++;
              c = '\n';
              c2 = sin.get ();
              if (c2 != '\n')
                sin.putback (c2);
            }
          *p++ = Char (c);
        }
      break;
    }
done:
  cp->c_used = p - cp->c_text;
  cp->c_nlines = nlines;
  rfc.r_nchars += cp->c_used;
  rfc.r_nlines += nlines;
  return cp;
}

void
static fixup_nl_code (ReadFileContext &rfc)
{
  if (rfc.r_eol_code == eol_guess && rfc.r_cr)
    {
      rfc.r_eol_code = eol_cr;
      rfc.r_nlines = 0;
      for (Chunk *cp = rfc.r_chunk; cp; cp = cp->c_next)
        {
          int nlines = 0;
          for (Char *p = cp->c_text, *pe = p + cp->c_used; p < pe; p++)
            if (*p == '\r')
              {
                *p = '\n';
                nlines++;
              }
          cp->c_nlines = nlines;
          cp->c_nbreaks = -1;
          rfc.r_nlines += nlines;
        }
    }
}

int
Buffer::read_file_contents (ReadFileContext &rfc, xread_stream &sin)
{
  int nchunks = 1;
  long total_bytes = sin.input_stream ().rest_chars ();
  DWORD last_tick = GetTickCount () + 1000;
  char msg[64];

  *msg = 0;
  Fbegin_wait_cursor ();
  rfc.r_chunk = read_chunk (rfc, sin);
  if (!rfc.r_chunk)
    {
      Fend_wait_cursor ();
      return 0;
    }
  Chunk *cp, *prev;
  for (cp = rfc.r_chunk; cp; cp = cp->c_next)
    {
      cp->c_next = read_chunk (rfc, sin);

      DWORD t = GetTickCount ();
      if (int (t - last_tick) >= 300)
        {
          last_tick = t;
          sprintf (msg, "Reading %d/%d bytes...",
                   total_bytes - sin.input_stream ().rest_chars (),
                   total_bytes);
          app.status_window.text (msg);
        }
    }

  if (*msg)
    app.status_window.restore ();

  for (prev = 0, cp = rfc.r_chunk; cp; cp = cp->c_next)
    {
      cp->c_prev = prev;
      prev = cp;
    }
  rfc.r_tail = prev;
  if (!rfc.r_tail->c_used)
    {
      cp = rfc.r_tail->c_prev;
      free_chunk (rfc.r_tail);
      rfc.r_tail = cp;
      if (!cp)
        {
          rfc.r_chunk = 0;
          Fend_wait_cursor ();
          return 0;
        }
      cp->c_next = 0;
    }

  fixup_nl_code (rfc);
  Fend_wait_cursor ();
  return 1;
}

static inline lisp
detect_encoding (const mapf &mf)
{
  long size;
  if (!safe_fixnum_value (xsymbol_value (Vdetect_char_encoding_buffer_size), &size))
    size = DEFAULT_DETECT_BUFFER_SIZE;
  if (size <= 0)
    size = DEFAULT_DETECT_BUFFER_SIZE;
  if (INT_MAX < size)
    size = INT_MAX;

  return detect_char_encoding (static_cast <const char *> (mf.base ()),
                               min (static_cast <int> (mf.size ()), static_cast <int> (size)),
                               mf.size ());
}

static eol_code
detect_eol_code (const mapf &mf)
{
  const u_char *p = (const u_char *)mf.base ();
  const u_char *const pe = p + min (0x8000UL, mf.size ());

  if (p < pe)
    {
      if (*p == '\n')
        return eol_lf;
      for (p++; (p = (const u_char *)memchr (p, '\n', pe - p)); p++)
        if (p[-1] != '\r')
          return eol_lf;
    }
  return eol_guess;
}

int
Buffer::read_file_contents (ReadFileContext &rfc, const char *filename,
                            int read_offset, int read_size)
{
  rfc.r_status = ReadFileContext::RFCS_NOERR;
  rfc.r_errcode = 0;
  rfc.r_nchars = 0;
  rfc.r_nlines = 0;
  rfc.r_chunk = 0;
  rfc.r_eol_code = rfc.r_expect_eol;
  rfc.r_cr = 0;

  mapf mf;
  if (!mf.open (filename, FILE_FLAG_SEQUENTIAL_SCAN, 1))
    {
      rfc.r_status = ReadFileContext::RFCS_OPEN;
      rfc.r_errcode = GetLastError ();
      return 0;
    }

  const char *bb = (const char *)mf.base () + read_offset;
  const char *be = (const char *)mf.base () + mf.size ();
  bb = min (bb, be);
  if (read_size >= 0)
    be = min (be, bb + read_size);

  xinput_strstream str (bb, be - bb);

  WIN32_FIND_DATA fd;
  if (WINFS::get_file_data (filename, fd))
    rfc.r_modtime = fd.ftLastWriteTime;
  else if (!GetFileTime (mf, 0, 0, &rfc.r_modtime))
    rfc.r_modtime.clear ();

  try
    {
      if (char_encoding_p (rfc.r_expect_char_encoding)
          && xchar_encoding_type (rfc.r_expect_char_encoding) == encoding_auto_detect)
        rfc.r_expect_char_encoding = detect_encoding (mf);

      if (!char_encoding_p (rfc.r_expect_char_encoding))
        {
          rfc.r_expect_char_encoding = rfc.r_char_encoding;
          if (rfc.r_expect_eol == eol_guess
              && xchar_encoding_type (rfc.r_char_encoding) == encoding_sjis)
            rfc.r_eol_code = rfc.r_expect_eol = detect_eol_code (mf);
        }

      rfc.r_char_encoding = rfc.r_expect_char_encoding;

      encoding_input_stream_helper sin (rfc.r_char_encoding, str, 1);
      int r = read_file_contents (rfc, sin);

      if (xchar_encoding_type (rfc.r_char_encoding) == encoding_utf16
          && !(xchar_encoding_utf_flags (rfc.r_char_encoding)
               & (ENCODING_UTF_BE | ENCODING_UTF_LE)))
        {
          if (sin.utf16_byte_order () == ENCODING_UTF_BE)
            rfc.r_char_encoding =
              symbol_value_char_encoding (Vencoding_default_utf16be_bom);
          else if (sin.utf16_byte_order () == ENCODING_UTF_LE)
            rfc.r_char_encoding =
              symbol_value_char_encoding (Vencoding_default_utf16le_bom);
        }
      return r;
    }
  catch (Win32Exception &e)
    {
      if (e.code != EXCEPTION_IN_PAGE_ERROR)
        throw e;
      rfc.r_status = ReadFileContext::RFCS_IOERR;
      rfc.r_errcode = ERROR_FILE_CORRUPT;
    }
  return 0;
}

int
Buffer::readin_chunk (ReadFileContext &rfc, xread_stream &sin)
{
  Fbegin_wait_cursor ();
  rfc.r_chunk = read_chunk (rfc, sin);
  Fend_wait_cursor ();
  if (!rfc.r_chunk)
    return 0;
  fixup_nl_code (rfc);
  return 1;
}

int
Buffer::readin_chunk (ReadFileContext &rfc, const char *filename)
{
  rfc.r_status = ReadFileContext::RFCS_NOERR;
  rfc.r_errcode = 0;
  rfc.r_nchars = 0;
  rfc.r_nlines = 0;
  rfc.r_chunk = 0;
  rfc.r_expect_eol = eol_guess;
  rfc.r_eol_code = eol_guess;
  rfc.r_cr = 0;

  mapf mf;
  if (!mf.open (filename, FILE_FLAG_SEQUENTIAL_SCAN, 1))
    {
      rfc.r_status = ReadFileContext::RFCS_OPEN;
      rfc.r_errcode = GetLastError ();
      return 0;
    }
  xinput_strstream str ((const char *)mf.base (), mf.size ());

  try
    {
      rfc.r_expect_char_encoding = detect_encoding (mf);
      if (!char_encoding_p (rfc.r_expect_char_encoding)
          || xchar_encoding_type (rfc.r_expect_char_encoding) == encoding_auto_detect)
        rfc.r_expect_char_encoding = xsymbol_value (Qencoding_sjis);
      rfc.r_char_encoding = rfc.r_expect_char_encoding;

      encoding_input_stream_helper sin (rfc.r_char_encoding, str, 1);
      return readin_chunk (rfc, sin);
    }
  catch (Win32Exception &e)
    {
      if (e.code != EXCEPTION_IN_PAGE_ERROR)
        throw e;
      rfc.r_status = ReadFileContext::RFCS_IOERR;
      rfc.r_errcode = ERROR_FILE_CORRUPT;
    }
  return 0;
}

struct WriteCharException
{
  int error;
  WriteCharException () : error (GetLastError ()) {}
};

void
Buffer::file_modtime (FileTime &ft)
{
  if (stringp (lfile_name))
    ft.file_modtime (lfile_name, 0);
  else
    ft.clear ();
}

int
Buffer::verify_modtime ()
{
  FileTime ft;
  file_modtime (ft);
  return ft.voidp () || ft == b_modtime;
}

lisp
Fclear_visited_file_modtime (lisp buffer)
{
  Buffer::coerce_to_buffer (buffer)->b_modtime.clear ();
  return Qnil;
}

lisp
Fupdate_visited_file_modtime (lisp buffer)
{
  Buffer::coerce_to_buffer (buffer)->update_modtime ();
  return Qnil;
}

lisp
Fverify_visited_file_modtime (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->verify_modtime ());
}

static int
pathname_equal (const char *path1, const char *path2)
{
  int l1 = strlen (path1);
  int l2 = strlen (path2);
  if (l1 == l2)
    return !_memicmp (path1, path2, l1);
  if (l1 == l2 + 1)
    return path1[l2] == '/' && !_memicmp (path1, path2, l2);
  if (l2 == l1 + 1)
    return path2[l1] == '/' && !_memicmp (path1, path2, l1);
  return 0;
}

int
same_file_p (const char *path1, const char *path2)
{
  if (pathname_equal (path1, path2))
    return WINFS::GetFileAttributes (path1) != -1;

  BY_HANDLE_FILE_INFORMATION i1, i2;
  HANDLE h1 = WINFS::CreateFile (path1, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                 0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if (h1 == INVALID_HANDLE_VALUE)
    h1 = WINFS::CreateFile (path1, 0, 0, 0, OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS, 0);
  HANDLE h2 = WINFS::CreateFile (path2, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                 0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if (h2 == INVALID_HANDLE_VALUE)
    h2 = WINFS::CreateFile (path2, 0, 0, 0, OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS, 0);
  int eq = (h1 != INVALID_HANDLE_VALUE
            && h2 != INVALID_HANDLE_VALUE
            && GetFileInformationByHandle (h1, &i1)
            && GetFileInformationByHandle (h2, &i2)
            && i1.dwVolumeSerialNumber == i2.dwVolumeSerialNumber
            && i1.nFileIndexLow == i2.nFileIndexLow
            && i1.nFileIndexHigh == i2.nFileIndexHigh);
  if (h1 != INVALID_HANDLE_VALUE)
    CloseHandle (h1);
  if (h2 != INVALID_HANDLE_VALUE)
    CloseHandle (h2);
  return eq;
}

static int
fatfs_basename (char *name)
{
  char *dot = jindex (name, '.');
  if (!dot)
    {
      if (strlen (name) > 8)
        name[check_kanji2 (name, 8) ? 7 : 8] = 0;
      return 0;
    }
  else
    {
      if (dot - name > 8)
        strcpy (name + (check_kanji2 (name, 8) ? 7 : 8), dot);
      return 1;
    }
}

static void
fatfs_append_suffix (char *name, int c)
{
  char *dot = jindex (name, '.');
  if (!dot)
    return;
  dot++;
  int l = strlen (dot);
  if (l >= 3)
    {
      if (SJISP (*(u_char *)dot))
        l = 2;
      else if (SJISP (((u_char *)dot) [1]))
        l = 1;
      else
        l = 2;
    }
  dot[l] = c;
  dot[l + 1] = 0;
}

static int
get_volume_info (const char *path, char *volname, DWORD volsize,
                 DWORD *serial, DWORD *maxl,
                 DWORD *flags, char *fsname, DWORD fssize)
{
  char buf[PATH_MAX + 1];
  return WINFS::GetVolumeInformation (root_path_name (buf, path),
                                      volname, volsize, serial,
                                      maxl, flags, fsname, fssize);
}

static int
fs_support_long_name (const char *path)
{
  DWORD maxl, flags;
  return get_volume_info (path, 0, 0, 0, &maxl, &flags, 0, 0) && maxl > 12;
}

static int
make_temp_file_name (char *path, char *p, int dirp, HANDLE tmpl,
                     int nchars, int &serial, int max_serial)
{
  nchars--;
  SetLastError (ERROR_SUCCESS);
  int fail = 0;
  for (int i = 0; i < max_serial; i++)
    {
      int t = serial;
      serial = (serial + 1) % max_serial;
      for (int j = nchars; j >= 0; j--, t /= 36)
        p[j] = downcase_digit_char[t % 36];
      if (!dirp)
        {
          HANDLE h = WINFS::CreateFile (path, GENERIC_WRITE, 0, 0, CREATE_NEW,
                                        FILE_ATTRIBUTE_ARCHIVE, tmpl);
          if (h != INVALID_HANDLE_VALUE)
            {
              CloseHandle (h);
              return 1;
            }
        }
      else if (WINFS::CreateDirectory (path, 0))
        return 1;

      int e = GetLastError ();
      if (e == ERROR_PATH_NOT_FOUND)
        break;
      if (e != ERROR_FILE_EXISTS && e != ERROR_ALREADY_EXISTS && ++fail > 10)
        break;
    }
  return 0;
}

int
make_temp_file_name (char *dir, const char *prefix, const char *suffix,
                     HANDLE tmpl, int dirp)
{
  const int max_serial = 36 * 36 * 36 * 36;
  static int serial = -1;
  if (serial == -1)
    serial = u_long (GetTickCount () * GetCurrentProcessId ()) % max_serial;

  char *d = dir + strlen (dir);
  sprintf (d, "%sXXXX.%s",
           prefix ? prefix : "~xyz", suffix ? suffix : "tmp");
  d += prefix ? strlen (prefix) : 4;

  return make_temp_file_name (dir, d, dirp, tmpl, 4, serial, max_serial);
}

static int
backup_dirname (char *backup, const char *original, Buffer *bp)
{
  strcpy (backup, original);
  lisp hook = symbol_value (Vmake_backup_filename_hook, bp);
  if (hook == Qunbound || hook == Qnil)
    return 1;

  try
    {
      lisp r = funcall_1 (hook, bp->lfile_name);
      if (r == Qnil)
        return 1;
      pathname2cstr (r, backup);
      char *p = find_last_slash (backup);
      if (!p)
        return 0;
      if (p[1])
        strcat (p, "/");
      p = find_last_slash (original);
      if (!p)
        return 0;
      strcat (backup, p + 1);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
      return 0;
    }
  return 1;
}

int
Buffer::make_auto_save_file_name (char *name)
{
  if (!stringp (lfile_name))
    {
      GetModuleFileName (0, name, PATH_MAX);
      char *p = jrindex (name, '\\');
      if (!p)
        return 0;
      p = stpcpy (p + 1, "#unnamed.");
      p[3] = 0;

      static int serial = 0;
      return make_temp_file_name (name, p, 0, 0, 3, serial, 36 * 36 * 36);
    }
  else
    {
      char xorgname[PATH_MAX + 1];
      char orgname[PATH_MAX + 1];

      pathname2cstr (lfile_name, xorgname);

      lisp x = symbol_value (Vauto_save_to_backup_directory, this);
      if (x == Qunbound || x == Qnil)
        strcpy (orgname, xorgname);
      else if (!backup_dirname (orgname, xorgname, this))
        return 0;

      int longname = fs_support_long_name (orgname);

      char *sl = find_last_slash (orgname);
      if (!sl)
        return 0;
      sl++;
      int l = sl - orgname;
      memcpy (name, orgname, l);
      name[l] = '#';
      strcpy (name + l + 1, sl);

      if (longname)
        strcat (name + l, "#");
      else if (fatfs_basename (name + l))
        fatfs_append_suffix (name + l, '#');
      else if (strlen (name + l) == 8)
        strcpy (name + l + 8, ".#");
      else
        strcat (name + l, "#");

      if (same_file_p (name, orgname))
        name[l] = '%';

      return 1;
    }
}

void
Buffer::delete_auto_save_file ()
{
  char name[PATH_MAX + 1];

  if (!stringp (lfile_name) || !b_done_auto_save)
    return;
  if (make_auto_save_file_name (name))
    WINFS::DeleteFile (name);
  b_done_auto_save = 0;
}

static int
pack_backupfile (char *old_name, char *oe, u_char *bitmap, int max_versions)
{
  char new_name[PATH_MAX + 1], *ne = new_name + (oe - old_name);
  memcpy (new_name, old_name, oe - old_name);
  int i, j;
  for (i = 1, j = 1; i < max_versions; i++)
    if (bitmap[i] && i != j)
      {
        sprintf (oe, "%d~", i);
        while (j < i)
          {
            sprintf (ne, "%d~", j++);
            if (WINFS::MoveFile (old_name, new_name)
                || GetLastError () != ERROR_ALREADY_EXISTS)
              break;
          }
      }
  return j;
}

int
Buffer::make_backup_file_name (char *backup, const char *xoriginal)
{
  char original[PATH_MAX + 1];
  if (!backup_dirname (original, xoriginal, this))
    {
      *backup = 0;
      return Ecannot_create_backup_file;
    }
  int fail = 0;
  int longname = fs_support_long_name (original);
  strcpy (backup, original);
  char *name = find_last_slash (backup);
  if (!name)
    {
      *backup = 0;
      return Ecannot_create_backup_file;
    }
  name++;

  lisp verctl = symbol_value (Vversion_control, this);
  if (verctl != Qnever)
    {
      if (!longname)
        {
          for (char *p = jindex (name, '.'); p; p = jindex (p, '.'))
            *p++ = '~';
          fatfs_basename (name);
        }

      int namelen = strlen (name);

#define MAXVERSIONS 1000
#define MAXVERCHARS 3
      int max_versions = longname ? MAXVERSIONS : 100;
      int max_verchars = longname ? MAXVERCHARS : 2;
      u_char bitmap[MAXVERSIONS];
      bzero (bitmap, sizeof bitmap);

      WIN32_FIND_DATA fd;
      char tem[2];
      tem[0] = name[0];
      tem[1] = name[1];
      name[0] = '*';
      name[1] = 0;
      HANDLE h = WINFS::FindFirstFile (backup, &fd);
      name[0] = tem[0];
      name[1] = tem[1];
      if (h != INVALID_HANDLE_VALUE)
        {
          do
            {
              char *p = &fd.cFileName[namelen];
              if (*p == '.')
                {
                  *p = 0;
                  if (!strcasecmp (fd.cFileName, name))
                    {
                      int i, n = 0;
                      for (i = 1; i <= max_verchars && digit_char_p (p[i]); i++)
                        n = n * 10 + p[i] - '0';
                      if (i > 1 && p[i] == '~' && !p[i + 1])
                        {
                          bitmap[n] = 1;
                          verctl = Qt;
                        }
                    }
                }
            }
          while (WINFS::FindNextFile (h, &fd));
          FindClose (h);
        }

      if (verctl != Qnil)
        {
          int oldver = symbol_value_as_integer (Vkept_old_versions, this);
          int newver = symbol_value_as_integer (Vkept_new_versions, this) - 1;

          int i, n;
          for (i = 0, n = 0; i < max_versions && n < oldver; i++)
            if (bitmap[i])
              {
                bitmap[i] = 2;
                n++;
              }
          for (i = max_versions - 1, n = 0; i >= 0 && n < newver; i--)
            if (bitmap[i])
              {
                bitmap[i] = 2;
                n++;
              }

          char *ext = name + namelen;
          ext[0] = '.';
          ext[1] = 0;

          for (i = 0; i < max_versions; i++)
            if (bitmap[i] == 1)
              {
                sprintf (ext + 1, "%d~", i);
                if (!same_file_p (backup, xoriginal))
                  WINFS::DeleteFile (backup);
              }

          for (i = max_versions - 1; i > 0; i--)
            if (bitmap[i])
              break;
          if (++i < max_versions)
            {
              sprintf (ext + 1, "%d~", i);
              if (!same_file_p (backup, xoriginal))
                return 0;
            }

          if (symbol_value (Vpack_backup_file_name, this) != Qnil)
            {
              i = pack_backupfile (backup, ext + 1, bitmap, max_versions);
              if (i < max_versions)
                {
                  sprintf (ext + 1, "%d~", i);
                  if (!same_file_p (backup, xoriginal))
                    return 0;
                }
            }

          fail = Mcannot_create_numbered_backup_file;
        }
      strcpy (backup, original);
    }

  if (!longname)
    {
      if (fatfs_basename (name))
        fatfs_append_suffix (name, '~');
      else
        {
          if (strlen (name) == 8)
            strcpy (name + 8, ".~");
          else
            strcat (name, "~");
        }
    }
  else
    strcat (name, "~");

  if (same_file_p (backup, xoriginal))
    {
      strcpy (name, "%BACKUP%~");
      fail = Ecannot_create_backup_file;
      if (same_file_p (backup, xoriginal))
        *backup = 0;
    }
  if (*backup && !WINFS::DeleteFile (backup)
      && GetLastError () != ERROR_FILE_NOT_FOUND)
    {
      *backup = 0;
      return Ecannot_delete_backup_file;
    }
  return fail;
}

class xinput_buffer_stream: public xinput_stream <Char>
{
  const Chunk *s_cp;
  int s_offset;
  long s_rest;

  virtual int refill ()
    {
      if (s_rest <= 0 || !s_cp)
        return eof;

      int nchars;
      while (!(nchars = min (s_rest, long (s_cp->c_used - s_offset))))
        {
          s_offset = 0;
          s_cp = s_cp->c_next;
          if (!s_cp)
            return eof;
        }
      int c = setbuf (s_cp->c_text + s_offset,
                      s_cp->c_text + s_offset + nchars);
      s_rest -= nchars;
      s_cp = s_cp->c_next;
      s_offset = 0;
      return c;
    }
public:
  xinput_buffer_stream (const Chunk *cp, int offset, long rest)
       : s_cp (cp), s_offset (offset), s_rest (rest) {}
};

class xwrite_buffer
{
public:
  HANDLE w_hfile;

  xwrite_buffer () : w_hfile (INVALID_HANDLE_VALUE) {}
  ~xwrite_buffer () {close ();}
  void close ();
  int open (const char *, DWORD);
  void write (const void *, DWORD) const;
};

int
xwrite_buffer::open (const char *path, DWORD mode)
{
  w_hfile = WINFS::CreateFile (path, GENERIC_WRITE, 0, 0, mode,
                               FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, 0);
  return w_hfile != INVALID_HANDLE_VALUE;
}

void
xwrite_buffer::close ()
{
  if (w_hfile != INVALID_HANDLE_VALUE)
    {
      CloseHandle (w_hfile);
      w_hfile = INVALID_HANDLE_VALUE;
    }
}

void
xwrite_buffer::write (const void *b, DWORD l) const
{
  if (l)
    {
      DWORD r;
      if (!WriteFile (w_hfile, b, l, &r, 0) || r != l)
        throw WriteCharException ();
    }
}

int
Buffer::write_region (xwrite_stream &sout, xwrite_buffer &xbuf, int &error)
{
  try
    {
      char buf[0x10000], *const be = buf + sizeof buf;
      while (1)
        {
          for (char *b = buf; b < be; b++)
            {
              int c = sout.get ();
              if (c == xwrite_stream::eof)
                {
                  xbuf.write (buf, b - buf);
                  xbuf.close ();
                  return sout.nlines ();
                }
              *b = c;
            }
          xbuf.write (buf, sizeof buf);
        }
    }
  catch (WriteCharException e)
    {
      error = e.error;
      return -1;
    }
}

void
Buffer::init_write_region_param (write_region_param &wr_param,
                                 lisp encoding, lisp eol) const
{
  if (!encoding || encoding == Qnil)
    wr_param.encoding = lchar_encoding;
  else
    {
      check_char_encoding (encoding);
      if (xchar_encoding_type (encoding) == encoding_auto_detect)
        FEtype_error (encoding, Qchar_encoding);
      wr_param.encoding = encoding;
    }

  if (!eol || eol == Qnil)
    wr_param.eol = b_eol_code;
  else
    {
      int n = fixnum_value (eol);
      if (!valid_eol_code_p (n) || n == eol_guess)
        n = eol_crlf;
      wr_param.eol = eol_code (n);
    }
}

int
Buffer::write_region (const char *filename, point_t p1, point_t p2,
                      int append, write_region_param &wr_param)
{
  xwrite_buffer xbuf;
  wr_param.error_open = 1;

  LONG lo, hi;
  if (append && xbuf.open (filename, OPEN_EXISTING))
    {
      hi = 0;
      lo = SetFilePointer (xbuf.w_hfile, 0, &hi, FILE_END);
      if (lo == -1 && hi == -1)
        {
          wr_param.error = GetLastError ();
          return -1;
        }
    }
  else
    {
      if (!xbuf.open (filename, CREATE_ALWAYS))
        {
          wr_param.error = GetLastError ();
          return -1;
        }
      lo = hi = -1;
      append = 0;
    }

  wr_param.error_open = 0;

  Fbegin_wait_cursor ();

  Point point;
  set_point_no_restrictions (point, p1);
  xinput_buffer_stream sin (point.p_chunk, point.p_offset, p2 - p1);
  encoding_output_stream_helper sout (wr_param.encoding, sin, wr_param.eol);
  int r = write_region (sout, xbuf, wr_param.error);
  if (r == -1)
    {
      if (append)
        {
          SetFilePointer (xbuf.w_hfile, lo, &hi, FILE_BEGIN);
          SetEndOfFile (xbuf.w_hfile);
        }
      xbuf.close ();
      if (!append)
        WINFS::DeleteFile (filename);
    }

  Fend_wait_cursor ();
  return r;
}

/* •…‚ê95‚ÅAMoveFile ("foo.txt", "foo.txt~") ‚µ‚½‚Æ‚«‚ÉA
   AlternateFileName ‚ª ``FOO.TXT'' ‚É‚È‚é‚Ì‚É‘Î‰ž */

static int
make_backup_file (const char *filename, char *backup, int &result)
{
  if (!sysdep.WinNTp ())
    {
      char *name = find_last_slash (backup);
      if (name && name[1] != '.')
        {
          name++;
          char *period = jindex (name, '.');
          if (!period
              || (period - name <= 8
                  && !jindex (period + 1, '.')
                  && strlen (period) > 4))
            {
              char tem[PATH_MAX + 1];
              if (!period)
                period = name + strlen (name);
              int l = period - backup;
              if (period - name >= 8)
                l = name - backup + (check_kanji2 (name, 7) ? 6 : 7);
              memcpy (tem, backup, l);
              memset (tem + l, '~', 7);
              l += 7;
              for (int i = 0;; i++)
                {
                  sprintf (tem + l, "%x%s", i, period);
                  if (WINFS::MoveFile (filename, tem))
                    break;
                  int e = GetLastError ();
                  if (e != ERROR_ALREADY_EXISTS)
                    return 0;
                }
              if (!WINFS::MoveFile (tem, backup))
                {
                  strcpy (backup, tem);
                  if (!result)
                    result = Mclumsy_backup_filename;
                }
              return 1;
            }
        }
    }
  return WINFS::MoveFile (filename, backup);
}

static void
make_temp_file (char *tmpname, const char *filename)
{
  strcpy (tmpname, filename);
  char *p = find_last_slash (tmpname);
  if (!p)
    file_error (Ecannot_make_temp_file_name);
  p[1] = 0;
  if (!make_temp_file_name (tmpname))
    {
      int e = GetLastError ();
      if (e == ERROR_PATH_NOT_FOUND)
        {
          p[1] = 0;
          file_error (e, make_string (tmpname));
        }
      file_error (Ecannot_make_temp_file_name);
    }
}

class keep_lock
{
  Buffer *bp;
  int locked;
public:
  keep_lock (Buffer *bp_)
       : bp (bp_), locked (bp->file_locked_p ())
    {
      bp->unlock_file ();
    }
  ~keep_lock ()
    {
      if (locked)
        bp->lock_file ();
    }
  void cancel_lock () {locked = 0;}
};

lisp
Buffer::save_buffer (lisp encoding, lisp eol)
{
  write_region_param wr_param;
  init_write_region_param (wr_param, encoding, eol); // check errors

  if (!b_modified)
    {
      format_message (Mbuffer_needs_not_save);
      return Qt;
    }

  if (!stringp (lfile_name))
    {
      lisp r = run_hook_until_success (Vsave_buffer_no_filenames_hook);
      if (r != Qnil)
        return r;
      file_error (Eno_file_name);
    }

  if (b_truncated && !yes_or_no_p (Mbuffer_is_truncated, lbuffer_name))
    return Qnil;

  lisp r = run_hook_until_success (Vbefore_save_buffer_hook);
  if (r != Qnil)
    return r;

  init_write_region_param (wr_param, encoding, eol);

  char filename[PATH_MAX + 1];
  pathname2cstr (lfile_name, filename);
  if (special_file_p (filename))
    file_error (Eis_character_special_file, lfile_name);

  DWORD filemode = WINFS::GetFileAttributes (filename);
  if (filemode != -1 && filemode & FILE_ATTRIBUTE_READONLY)
    file_error (Eis_write_protected, lfile_name);

  FileTime modtime (lfile_name, 0);
  if (!modtime.voidp () && modtime != b_modtime
      && !yes_or_no_p (Mdisk_file_has_changed))
    FEsilent_quit ();

  char tmpname[PATH_MAX + 1];
  char backup[PATH_MAX + 1];
  int backup_result = 0;
  int nlines = 0;

  lisp precious_flag = symbol_value (Vfile_precious_flag, this);
  lisp by_copying = symbol_value (Vbackup_by_copying, this);
  if (precious_flag != Qnil && by_copying == Kremote)
    {
      switch (GetDriveType (root_path_name (tmpname, filename)))
        {
        case DRIVE_REMOVABLE:
        case DRIVE_FIXED:
        case DRIVE_CDROM:
        case DRIVE_RAMDISK:
          by_copying = Qnil;
          break;
        }
    }

  if (precious_flag == Qnil)
    {
      format_message (Mwriting);

      nlines = write_region (filename, 0, b_nchars, 0, wr_param);
      if (nlines < 0)
        {
          if (wr_param.error)
            file_error (wr_param.error, lfile_name);
          file_error (Ewrite_error, lfile_name);
        }
    }
  else if (by_copying != Qnil)
    {
      keep_lock lock (this);
      int real_backup = 0;

      filemode = WINFS::GetFileAttributes (filename);
      if (filemode == -1)
        *backup = 0;
      else
        {
          if ((!b_make_backup
               || symbol_value (Vmake_backup_file_always, this) != Qnil)
              && symbol_value (Vmake_backup_files, this) != Qnil)
            {
              backup_result = make_backup_file_name (backup, filename);
              if (!*backup)
                file_error (message_code (backup_result));
              real_backup = 1;
            }
          else
            make_temp_file (backup, filename);
        }

      if (*backup && !CopyFile (filename, backup, 0))
        {
          int e = GetLastError ();
          WINFS::DeleteFile (backup);
          file_error (e, lfile_name);
        }

      if (real_backup)
        b_make_backup = 1;

      format_message (Mwriting);

      nlines = write_region (filename, 0, b_nchars, 0, wr_param);
      if (nlines < 0)
        {
          if (!wr_param.error_open && *backup)
            FEfile_lost_error (lfile_name, make_string (backup));
          if (wr_param.error)
            file_error (wr_param.error, lfile_name);
          file_error (Ewrite_error, lfile_name);
        }

      if (!real_backup && *backup)
        WINFS::DeleteFile (backup);

      lock.cancel_lock ();
    }
  else
    {
      make_temp_file (tmpname, filename);

      format_message (Mwriting);

      int error;
      nlines = write_region (tmpname, 0, b_nchars, 0, wr_param);
      if (nlines < 0)
        {
          if (wr_param.error)
            file_error (wr_param.error, lfile_name);
          file_error (Ewrite_error, lfile_name);
        }

      int file_lost = 0;

      keep_lock lock (this);

      filemode = WINFS::GetFileAttributes (filename);
      if (filemode != -1)
        {
          WINFS::SetFileAttributes (tmpname, filemode);
          if ((!b_make_backup
               || symbol_value (Vmake_backup_file_always, this) != Qnil)
              && symbol_value (Vmake_backup_files, this) != Qnil)
            {
              backup_result = make_backup_file_name (backup, filename);
              if (!*backup)
                {
                  WINFS::DeleteFile (tmpname);
                  file_error (message_code (backup_result));
                }
              if (!make_backup_file (filename, backup, backup_result))
                {
                  error = GetLastError ();
                  WINFS::DeleteFile (tmpname);
                  file_error (error, lfile_name);
                }
              if (backup_result != Ecannot_create_backup_file)
                b_make_backup = 1;
            }
          else
            {
              if (!WINFS::DeleteFile (filename))
                {
                  error = GetLastError ();
                  if (error != ERROR_FILE_NOT_FOUND)
                    {
                      WINFS::DeleteFile (tmpname);
                      file_error (error, lfile_name);
                    }
                }
            }
          file_lost = 1;
        }

      if (!WINFS::MoveFile (tmpname, filename))
        {
          if (file_lost)
            FEfile_lost_error (lfile_name, make_string (tmpname));
          error = GetLastError ();
          WINFS::DeleteFile (tmpname);
          file_error (error, lfile_name);
        }

      lock.cancel_lock ();
    }

  delete_auto_save_file ();
  b_modified = 0;
  b_need_auto_save = 0;
  b_modtime.file_modtime (lfile_name, 0);
  modify_mode_line ();
  maybe_modify_buffer_bar ();

  lisp flock = symbol_value (Slock_file, this);
  if (flock != Qnil && flock != Kedit)
    lock_file ();

  save_modtime_undo (b_modtime);

  format_message (MFwrote_n_lines, nlines);

  if (backup_result)
    warn_msgbox (message_code (backup_result), make_string (backup));

  run_hook (Vafter_save_buffer_hook);
  return Qt;
}

lisp
Fsave_buffer (lisp encoding, lisp eol)
{
  return selected_buffer ()->save_buffer (encoding, eol);
}

lisp
Fdelete_auto_save_file (lisp buffer)
{
  Buffer::coerce_to_buffer (buffer)->delete_auto_save_file ();
  return Qt;
}

void
do_auto_save (int not_all, int unnamed)
{
  char name[PATH_MAX + 16];
  int f = 0;
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (bp->b_need_auto_save
        && (unnamed || stringp (bp->lfile_name))
        && (!not_all || symbol_value (Vauto_save, bp) != Qnil)
        && bp->make_auto_save_file_name (name) && !special_file_p (name))
      {
        if (!f)
          {
            format_message (Mauto_saving);
            f = 1;
          }
        write_region_param wr_param;
        bp->init_write_region_param (wr_param, 0, 0);
        if (bp->write_region (name, 0, bp->b_nchars, 0, wr_param) >= 0)
          {
            bp->b_need_auto_save = 0;
            bp->b_done_auto_save = 1;
          }
      }
  if (f)
    format_message (Mauto_saving_done);
  app.auto_save_count = 0;
}

lisp
Fdo_auto_save (lisp not_all)
{
  do_auto_save (not_all && not_all != Qnil, 0);
  return Qt;
}

lisp
Fwrite_region (lisp from, lisp to, lisp filename, lisp append,
               lisp encoding, lisp eol)
{
  char path[PATH_MAX + 1];
  pathname2cstr (filename, path);

  if (special_file_p (path))
    file_error (Eis_character_special_file, filename);

  Buffer *bp = selected_buffer ();
  point_t p1 = bp->coerce_to_restricted_point (from);
  point_t p2 = bp->coerce_to_restricted_point (to);
  if (p1 > p2)
    swap (p1, p2);

  write_region_param wr_param;
  bp->init_write_region_param (wr_param, encoding, eol);

  int nlines = bp->write_region (path, p1, p2, append && append != Qnil, wr_param);
  if (nlines < 0)
    {
      if (wr_param.error)
        file_error (wr_param.error, filename);
      file_error (Ewrite_error, filename);
    }
  return make_fixnum (nlines);
}

lisp
Buffer::lock_file (lisp name, int force)
{
  if (!force && file_locked_p ())
    return Qnil;

  if (!stringp (name))
    return Qnil;

  char filename[PATH_MAX + 1];
  pathname2cstr (name, filename);

  lisp result = Qt;
  int share = (symbol_value (Vexclusive_lock_file, this) == Qnil
               ? FILE_SHARE_READ : 0);
  HANDLE h;
  if (share)
    {
      h = WINFS::CreateFile (filename, GENERIC_READ, 0, 0, OPEN_EXISTING,
                             FILE_ATTRIBUTE_ARCHIVE, 0);
      if (h == INVALID_HANDLE_VALUE)
        {
          if (GetLastError () == ERROR_SHARING_VIOLATION)
            result = Kshared;
        }
      else
        CloseHandle (h);
    }

  h = WINFS::CreateFile (filename, GENERIC_READ, share, 0, OPEN_EXISTING,
                         FILE_ATTRIBUTE_ARCHIVE, 0);
  if (h == INVALID_HANDLE_VALUE)
    {
      int error = GetLastError ();
      if (error != ERROR_FILE_NOT_FOUND)
        file_error (error, name);
      h = WINFS::CreateFile (filename, GENERIC_READ, share, 0, CREATE_NEW,
                             FILE_ATTRIBUTE_ARCHIVE | FILE_FLAG_DELETE_ON_CLOSE, 0);
      if (h == INVALID_HANDLE_VALUE)
        file_error (GetLastError (), name);

      update_modtime ();
      b_make_backup = 1;
    }

  unlock_file ();
  b_hlock = h;

  return result;
}

int
Buffer::unlock_file ()
{
  if (!file_locked_p ())
    return 0;
  if (!CloseHandle (b_hlock))
    return 0;
  b_hlock = INVALID_HANDLE_VALUE;
  return 0;
}

lisp
Flock_file (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lock_file ();
}

lisp
Funlock_file (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->unlock_file ());
}

lisp
Ffile_locked_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->file_locked_p ());
}
