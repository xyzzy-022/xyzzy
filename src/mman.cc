#include "stdafx.h"
#include "cdecl.h"
#include "mman.h"
#include "vfs.h"

void
mapf::close ()
{
  if (mf_base)
    UnmapViewOfFile (mf_base);
  if (mf_hmap)
    CloseHandle (mf_hmap);
  if (mf_hfile != INVALID_HANDLE_VALUE)
    CloseHandle (mf_hfile);
  init ();
}

int
mapf::open (const char *path, int mode, int share_ok)
{
  mf_hfile = WINFS::CreateFile (path, GENERIC_READ, FILE_SHARE_READ, 0,
                                OPEN_EXISTING, mode, 0);
  if (mf_hfile == INVALID_HANDLE_VALUE)
    {
      if (!share_ok)
        return 0;
      mf_hfile = WINFS::CreateFile (path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                                    0, OPEN_EXISTING, mode, 0);
      if (mf_hfile == INVALID_HANDLE_VALUE)
        return 0;
    }

  mf_size = GetFileSize (mf_hfile, 0);
  if (mf_size)
    {
      mf_hmap = CreateFileMapping (mf_hfile, 0, PAGE_READONLY, 0, 0, 0);
      if (!mf_hmap)
        return 0;

      mf_base = (u_char *)MapViewOfFile (mf_hmap, FILE_MAP_READ, 0, 0, 0);
      if (!mf_base)
        return 0;
    }
  return 1;
}
