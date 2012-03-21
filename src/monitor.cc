#include <windows.h>
#include "monitor.h"

bool
Monitor::get_workarea_from_point (POINT point, LPRECT lpworkarea)
{
  HMONITOR monitor = MonitorFromPoint (point, MONITOR_DEFAULTTOPRIMARY);
  return get_workarea_from_monitor (monitor, lpworkarea);
}

bool
Monitor::get_workarea_from_rect (LPCRECT lprc, LPRECT lpworkarea)
{
  HMONITOR monitor = MonitorFromRect (lprc, MONITOR_DEFAULTTOPRIMARY);
  return get_workarea_from_monitor (monitor, lpworkarea);
}

bool
Monitor::get_workarea_from_window (const HWND hwnd, LPRECT lpworkarea)
{
  HMONITOR monitor = MonitorFromWindow (hwnd, MONITOR_DEFAULTTOPRIMARY);
  return get_workarea_from_monitor (monitor, lpworkarea);
}

bool
Monitor::get_workarea_from_monitor (const HMONITOR& monitor, LPRECT lpworkarea)
{
  MONITORINFO info;
  memset (&info, 0, sizeof info);
  info.cbSize = sizeof info;
  if (!GetMonitorInfo(monitor, &info))
    return false;

  *lpworkarea = info.rcWork;
}

Monitor monitor;
