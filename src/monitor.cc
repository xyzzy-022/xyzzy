#include "stdafx.h"
#include "monitor.h"

HMONITOR
Monitor::get_monitor_from_rect (LPCRECT lprc)
{
  return MonitorFromRect (lprc, MONITOR_DEFAULTTONULL);
}

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
Monitor::get_monitorinfo_from_window (const HWND hwnd, LPMONITORINFO lpmonitorinfo)
{
  HMONITOR monitor = MonitorFromWindow (hwnd, MONITOR_DEFAULTTOPRIMARY);
  return get_monitorinfo_from_monitor (monitor, lpmonitorinfo);
}

bool
Monitor::get_workarea_from_monitor (const HMONITOR& monitor, LPRECT lpworkarea)
{
  MONITORINFO info;
  if (!get_monitorinfo_from_monitor (monitor, &info))
    return false;

  *lpworkarea = info.rcWork;
  return true;
}

bool
Monitor::get_monitorinfo_from_monitor (const HMONITOR& monitor, LPMONITORINFO lpmonitorinfo)
{
  memset (lpmonitorinfo, 0, sizeof *lpmonitorinfo);
  lpmonitorinfo->cbSize = sizeof *lpmonitorinfo;
  return GetMonitorInfo(monitor, lpmonitorinfo) ? true : false;
}

Monitor monitor;
