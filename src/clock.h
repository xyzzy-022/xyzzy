#ifndef _clock_h_
# define _clock_h_

class Clock
{
  HWND cl_hwnd;
  int cl_width;
  int cl_extent;
  char cl_lbuf[16];
public:
  void init (HWND);
  void size ();
  void timer ();
};
#endif
