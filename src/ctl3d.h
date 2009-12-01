#ifndef _ctl3d_h_
# define _ctl3d_h_


class Ctl3dThread
{
public:
  Ctl3dThread ();
  ~Ctl3dThread ();
};

struct Ctl3dState
{
  HINSTANCE hinst;
  HINSTANCE hlib;
  BOOL (WINAPI *Register)(HINSTANCE);
  BOOL (WINAPI *Unregister)(HINSTANCE);
  BOOL (WINAPI *AutoSubclass)(HINSTANCE);
  BOOL (WINAPI *UnAutoSubclass)();
  BOOL (WINAPI *ColorChange)();
  void (WINAPI *WinIniChange)();
  int registered;
};

class Ctl3d
{
  static Ctl3dState state;
public:
  ~Ctl3d ();
  static void enable (HINSTANCE);
  static void ini_change ();
  static void color_change ();
  friend class Ctl3dThread;
};

#endif
