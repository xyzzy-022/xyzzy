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
    make_integer (long_to_large_int (uuid.Data1)), // time-low
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
