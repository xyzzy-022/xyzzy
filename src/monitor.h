#ifndef _monitor_h_
# define _monitor_h_

# include <windows.h>

class Monitor
{
public:
  HMONITOR get_monitor_from_rect (LPCRECT lprc);
  bool get_workarea_from_point (POINT point, LPRECT lpworkarea);
  bool get_workarea_from_rect (LPCRECT lprc, LPRECT lpworkarea);
  bool get_workarea_from_window (const HWND hwnd, LPRECT lpworkarea);
private:
  bool get_workarea_from_monitor (const HMONITOR& monitor, LPRECT lpworkarea);
};

extern Monitor monitor;

#endif /* _monitor_h_ */
