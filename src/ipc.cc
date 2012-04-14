#include "stdafx.h"
#include "ipc.h"

#define HWND_MAX 253

struct hwnd_buf
{
  LONG lock;
  LONG lock2;
  HWND hwnd_lock;
  HWND hwnd[HWND_MAX];
};

#pragma data_seg (".shared")
static hwnd_buf xwb = {0};
#pragma data_seg ()
#pragma comment (linker, "-section:.shared,RWS")

int xyzzy_instance::xi_inst;

HWND
xyzzy_hwnd::get (int i)
{
  if (xwb.hwnd[i] && !IsWindow (xwb.hwnd[i]))
    xwb.hwnd[i] = 0;
  return xwb.hwnd[i];
}

xyzzy_hwnd::xyzzy_hwnd (HWND hwnd)
{
  while (InterlockedExchange (&xwb.lock, 1))
    {
      if (!InterlockedExchange (&xwb.lock2, 1))
        {
          HWND h = xwb.hwnd_lock;
          if (h && !IsWindow (h))
            {
              xwb.hwnd_lock = 0;
              InterlockedExchange (&xwb.lock, 0);
            }
          InterlockedExchange (&xwb.lock2, 0);
        }
      Sleep (0);
    }
  xwb.hwnd_lock = hwnd;
}

xyzzy_hwnd::~xyzzy_hwnd ()
{
  xwb.hwnd_lock = 0;
  InterlockedExchange (&xwb.lock, 0);
}

int
xyzzy_hwnd::find (HWND hwnd) const
{
  for (int i = 0; i < HWND_MAX; i++)
    if (get (i) == hwnd)
      return i;
  return -1;
}

HWND
xyzzy_hwnd::next (int &i) const
{
  int o = i;
  for (i++; i < HWND_MAX; i++)
    {
      HWND hwnd = get (i);
      if (hwnd)
        return hwnd;
    }
  for (i = 0; i < o; i++)
    {
      HWND hwnd = get (i);
      if (hwnd)
        return hwnd;
    }
  return 0;
}

HWND
xyzzy_hwnd::prev (int &i) const
{
  int o = i;
  for (i--; i >= 0; i--)
    {
      HWND hwnd = get (i);
      if (hwnd)
        return hwnd;
    }
  for (i = HWND_MAX - i; i > o; i--)
    {
      HWND hwnd = get (i);
      if (hwnd)
        return hwnd;
    }
  return 0;
}

int
xyzzy_hwnd::set (HWND hwnd) const
{
  int i = find (hwnd);
  if (i < 0)
    {
      i = find (0);
      if (i >= 0)
        xwb.hwnd[i] = hwnd;
    }
  return i;
}

int
xyzzy_hwnd::clr (HWND hwnd) const
{
  int i = find (hwnd);
  if (i < 0)
    return 0;
  xwb.hwnd[i] = 0;
  return 1;
}

int
xyzzy_hwnd::count () const
{
  int n = 0;
  for (int i = 0; i < HWND_MAX; i++)
    if (get (i))
      n++;
  return n;
}
