#ifndef _preview_h_
#define _preview_h_

#include "wheel.h"

class preview_page_window
{
public:
  enum
    {
      ADJUST_WINDOW = 0,
      ADJUST_WIDTH = -1,
      ADJUST_HEIGHT = -2
    };
  struct ids2scale {int ids; int scale;};
  static const ids2scale ids2scales[];

protected:
  enum { MARGIN = 7 };
  enum { SHADOW = 4 };
  enum { MAX_SCALE = 400, MIN_SCALE = 10 };

  int p_scale;
  POINT p_scroll_offset;

  const printer_device &p_dev;
  print_settings &p_settings;
  print_engine &p_engine;

  int p_xdpi;
  int p_ydpi;
  SIZE p_size;
  SIZE p_physsize_pxl;
  RECT p_min_margin_pxl;
  RECT p_text_margin_pxl;
  LONG p_header_offset_pxl;
  LONG p_footer_offset_pxl;
  LONG p_column_sep_pxl;

  int p_total_pages;
  int p_updated;
  int p_font_height;
  int d_font_height;
  SIZE p_client;

  mouse_wheel p_wheel;

  void vscroll (int, int, int = 1);
  void hscroll (int, int, int = 1);
  void wheel (const wheel_info &);
  void size (int, int);
  void paint ();
  int key_down (int);
  int set_cursor (int);
  void lbutton_down (int, int);
  void rbutton_down (int, int);

  void scaling (POINT * = 0);
  void scaling (int, int);

  void paint_paper (HDC) const;
  void calc_origin (POINT &) const;

  ScrollInfo hsinfo;
  ScrollInfo vsinfo;
  void update_scroll_bar (SCROLLINFO &, int, int, int, int);
  void update_vscroll_bar ();
  void update_hscroll_bar ();

  void update_buttons ();
  void invalidate () const;

  LRESULT wndproc (UINT, WPARAM, LPARAM);
  static LRESULT CALLBACK wndproc (HWND, UINT, WPARAM, LPARAM);
  static int register_wndclass (HINSTANCE);

  static int wndclass_initialized;
  static const char PageClassName[];
public:
  HWND p_hwnd;
  preview_page_window (const printer_device &, print_settings &, print_engine &);
  int set_scale (int, int = 0, POINT * = 0);
  int get_scale () const;
  int scale_value () const;
  int next_page ();
  int prev_page ();
  int create (HWND, const RECT &);
};

class preview_dialog
{
protected:
  int init_dialog (HWND);
  BOOL destroy ();
  BOOL notify (NMHDR *);
  BOOL command (UINT, UINT);
  BOOL quit ();
  BOOL size (int, int);

  BOOL scale_command (int);
  BOOL next_page (UINT);
  BOOL prev_page (UINT);
  void update_page (int, int);
  void update_scale ();

  void calc_page_rect (RECT &) const;
  void set_scale_combo ();

  BOOL wndproc (UINT, WPARAM, LPARAM);
  static BOOL CALLBACK wndproc (HWND, UINT, WPARAM, LPARAM);

  HWND p_hwnd_sw;
  HWND p_hwnd;
  preview_page_window p_page;

public:
  preview_dialog (const printer_device &, print_settings &, print_engine &);
  int do_modal (HWND);
};

#endif /* _preview_h_ */
