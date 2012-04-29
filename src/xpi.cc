#include "stdafx.h"
#include "xpi.h"

static const FARPROC *xpi_procs;
static int xpi_nprocs;

#define XPICREATEPANE  0
#define XPISETPANESIZE 1
#define XPISETPANEPOS  2

BOOL WINAPI
xpiInit (const void *arg)
{
  xpi_procs = (const FARPROC *)arg;
  xpi_nprocs = 0;
  if (!xpi_procs)
    return 0;
  for (; xpi_procs[xpi_nprocs]; xpi_nprocs++)
    ;
  return 1;
}

#define XPI_CALL(ORD, ERR, RETTYPE, ARGTYPE, ARGS) \
  return (xpi_nprocs >= (ORD) \
          ? ((RETTYPE (WINAPI *)ARGTYPE)xpi_procs[(ORD)])ARGS \
          : (ERR))

XPIHANDLE WINAPI
xpiCreatePane (HWND hwnd, int cx, int cy, DWORD flags)
{
  XPI_CALL (XPICREATEPANE, 0, XPIHANDLE,
            (HWND, int, int, DWORD),
            (hwnd, cx, cy, flags));
}

BOOL WINAPI
xpiSetPaneSize (XPIHANDLE h, int size, int min, int max, int step)
{
  XPI_CALL (XPISETPANESIZE, 0, BOOL,
            (XPIHANDLE, int, int, int, int),
            (h, size, min, max, step));
}

BOOL WINAPI
xpiSetPanePos (XPIHANDLE h, DWORD flags)
{
  XPI_CALL (XPISETPANEPOS, 0, BOOL,
            (XPIHANDLE, DWORD),
            (h, flags));
}
