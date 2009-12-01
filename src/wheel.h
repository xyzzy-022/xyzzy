#ifndef _WHEEL_H_
#define _WHEEL_H_

struct wheel_info
{
  int wi_value;
  UINT wi_nlines;
  POINT wi_pt;
};

class mouse_wheel
{
private:
  static UINT mw_nlines;
  static UINT PWM_MSH_MOUSEWHEEL;
  int mw_delta;
public:
  mouse_wheel ();
  ~mouse_wheel ();
  int msg_handler (HWND, UINT, WPARAM, LPARAM, wheel_info &);
};

int begin_auto_scroll (HWND, const POINT &,
                       void (__stdcall *)(int, void *), void *);

#endif /* _WHEEL_H_ */
