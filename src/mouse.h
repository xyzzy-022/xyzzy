#ifndef _mouse_h_
# define _mouse_h_

class kbd_queue;

class mouse_state
{
protected:
  kbd_queue &ms_kbdq;
  UINT ms_modifier;
  UINT ms_last_modifier;
  HWND ms_hwnd;
  POINT ms_point;
  LONG ms_time;
  int ms_click_count;
  int ms_line;
  int ms_column;
  Char ms_movechar;
  int ms_enable_move;

  static DWORD ms_last_pos;
  static int ms_hidden;
  static HHOOK hhook_mouse;

  enum {DOWN, UP, MOVE};

  void click_count (WPARAM, LPARAM);
  void dispatch (Window *, WPARAM, LPARAM, int);

  static LRESULT CALLBACK mouse_hook_proc (int, WPARAM, LPARAM);

public:
  void down (Window *, WPARAM, LPARAM, UINT);
  void up (Window *, WPARAM, LPARAM, UINT);
  void move (Window *, WPARAM, LPARAM);
  void cancel ();
  int track_popup_menu (HMENU, lisp, const POINT *);
  mouse_state (kbd_queue &);
  void clear_move () {ms_movechar = 0;}
  static void update_cursor (UINT, WPARAM);
  static void show_cursor ();
  static void hide_cursor ();
  static void install_hook ()
    {hhook_mouse = SetWindowsHookEx (WH_MOUSE, mouse_hook_proc,
                                     0, GetCurrentThreadId ());}
  static void remove_hook ()
    {if (hhook_mouse) UnhookWindowsHookEx (hhook_mouse);}
};

inline
mouse_state::mouse_state (kbd_queue &kbdq)
     : ms_kbdq (kbdq), ms_modifier (0), ms_last_modifier (0),
       ms_hwnd (0), ms_time (0), ms_line (0), ms_column (0),
       ms_movechar (0), ms_enable_move (0)
{
}

int rowcol_from_point (Window *, int *, int *);

#endif
