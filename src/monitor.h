#ifndef _monitor_h_
# define _monitor_h_

class Monitor
{
public:
  bool get_workarea_from_point (POINT point, LPRECT lpworkarea);
  bool get_workarea_from_rect (LPCRECT lprc, LPRECT lpworkarea);
  bool get_workarea_from_window (const HWND hwnd, LPRECT lpworkarea);
private:
  bool get_workarea_from_monitor (const HMONITOR& monitor, LPRECT lpworkarea);
};

extern Monitor monitor;

#endif /* _monitor_h_ */
