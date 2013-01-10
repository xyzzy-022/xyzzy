#include "stdafx.h"
#include "system.h"

lisp
Fsi_uuid_create (lisp keys)
{
  UUID uuid;

  if (find_keyword_bool (Ksequential, keys))
    rpc_error (UuidCreateSequential (&uuid));
  else
    rpc_error (UuidCreate (&uuid));

  safe_rpc_str uuidstr;
  rpc_error (UuidToString (&uuid, &uuidstr));

  multiple_value::count () = 2;
  multiple_value::value (1) = make_list (
    make_integer (int64_t (uuid.Data1)),           // time-low
    make_fixnum (uuid.Data2),                      // time-mid
    make_fixnum (uuid.Data3),                      // time-high-and-version
    make_fixnum (uuid.Data4[0]),                   // clock-seq-and-reserved
    make_fixnum (uuid.Data4[1]),                   // clock-seq-low
    make_list (                                    // node
      make_fixnum (uuid.Data4[2]),
      make_fixnum (uuid.Data4[3]),
      make_fixnum (uuid.Data4[4]),
      make_fixnum (uuid.Data4[5]),
      make_fixnum (uuid.Data4[6]),
      make_fixnum (uuid.Data4[7]),
      0),
    0);

  return uuidstr.make_string ();
}

lisp
Fsi_get_key_state (lisp lvkey)
{
  int vkey = fixnum_value (lvkey);
  int flag = GetKeyState (vkey);

  multiple_value::count () = 2;
  multiple_value::value (1) = boole (flag & 0x01);
  return boole (flag < 0);
}

lisp
Fsi_search_path (lisp lfile, lisp lpath, lisp lext)
{
  char *path = 0;
  char *file = 0;
  char *ext = 0;

  check_string (lfile);
  file = (char *)alloca (xstring_length (lfile) * 2 + 1);
  w2s (file, lfile);

  if (lpath && lpath != Qnil)
    {
      path = (char *)alloca (xstring_length (lpath) * 2 + 1);
      w2s (path, lpath);
    }
  if (lext && lext != Qnil)
    {
      ext = (char *)alloca (xstring_length (lext) * 2 + 1);
      w2s (ext, lext);
    }

  DWORD len = SearchPath (path, file, ext, 0, 0, 0);
  if (!len)
    return Qnil;

  char *file_part = 0;
  char *buffer = (char *)alloca (len);
  if (!SearchPath (path, file, ext, len, buffer, &file_part))
    return Qnil;

  return buffer ? make_path (buffer, 0) : Qnil;
}

lisp
Fadmin_user_p ()
{
  if (IsUserAnAdmin ())
    return Qt;
  else
    return Qnil;
}
