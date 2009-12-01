#ifndef _mainframe_h_
#define _mainframe_h_

#include "dockbar.h"
#include "pane.h"

class main_frame: public dock_frame
{
public:
  splitter m_splitter;

public:
  void init (HWND toplev, HWND frame)
    {
      dock_frame::init (toplev, frame);
      m_splitter.init (toplev, frame);
    }
  void resize (RECT &r, HWND hwnd_before)
    {
      dock_frame::calc_layout (r, hwnd_before);
      if (!r.top)
        r.top++;
      m_splitter.resize (r);
    }
  int set_cursor (HWND hwnd, WPARAM wparam, LPARAM lparam)
    {
      if (HWND (wparam) == hwnd && LOWORD (lparam) == HTCLIENT)
        return m_splitter.set_cursor ();
      return 0;
    }
  void lbtn_down (LPARAM lparam)
    {
      m_splitter.move_splitter ((short)LOWORD (lparam),
                                (short)HIWORD (lparam));
    }
  void child_destroy (HWND hwnd)
    {
      pane *p = m_splitter.find_pane (hwnd);
      if (p)
        m_splitter.remove_pane (p);
    }
  virtual void recalc_layout () {recalc_toplevel ();}
};

extern main_frame g_frame;

#endif /* _mainframe_h_ */
