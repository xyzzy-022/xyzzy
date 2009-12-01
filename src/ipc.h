#ifndef _ipc_h_
#define _ipc_h_

class xyzzy_hwnd
{
  static HWND get (int);

public:
  xyzzy_hwnd (HWND);
  ~xyzzy_hwnd ();
  int find (HWND) const;
  HWND next (int &) const;
  HWND prev (int &) const;
  int set (HWND) const;
  int clr (HWND) const;
  int count () const;
};

class xyzzy_instance
{
  HWND xi_hwnd;
  static int xi_inst;
public:
  xyzzy_instance (HWND hwnd) : xi_hwnd (hwnd)
    {
      xyzzy_hwnd xh (xi_hwnd);
      xi_inst = xh.set (xi_hwnd);
    }
  ~xyzzy_instance ()
    {
      xyzzy_hwnd xh (xi_hwnd);
      xh.clr (xi_hwnd);
    }
  static int instnum () {return xi_inst;}
};

#endif
