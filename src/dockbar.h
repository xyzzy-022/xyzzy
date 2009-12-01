#ifndef _dockbar_h_
#define _dockbar_h_

#ifndef TBSTYLE_FLAT
#define TBSTYLE_FLAT 0x0800
#endif

#ifndef TCS_BOTTOM
#define TCS_BOTTOM 2
#define TCS_RIGHT 2
#define TCS_HOTTRACK 0x40
#define TCS_VERTICAL 0x80
#endif

#include "xlist.h"

class dock_frame;

class dock_bar: public xlist_node <dock_bar>
{
public:
  HWND b_hwnd;
private:
  friend dock_frame;
  RECT b_rect;
  POINT b_required;
  WNDPROC b_wndproc;
protected:
  dock_frame &b_frame;
  lisp b_lname;
  enum {TTBUFSIZE = 256};
  static char b_ttbuf[TTBUFSIZE];
private:
  u_char b_edge;
  u_char b_border;
  u_char b_dockable;
  u_char b_status;
  static const char b_dock_bar_prop[];
public:
  enum
    {
      DOCK_STAT_NEW = 1,
      DOCK_STAT_NOREDRAW = 2
    };
  enum
    {
      HORZ_LEFT_PAD = 10,
      HORZ_RIGHT_PAD = 3,
      HORZ_TOP_PAD = 3,
      HORZ_BOTTOM_PAD = 1,

      VERT_LEFT_PAD = 2,
      VERT_RIGHT_PAD = 1,
      VERT_TOP_PAD = 8,
      VERT_BOTTOM_PAD = 4
    };

  enum
    {
      EDGE_LEFT,
      EDGE_TOP,
      EDGE_RIGHT,
      EDGE_BOTTOM,
      EDGE_MAX
    };
  enum
    {
      BORDER_LEFT = 1 << EDGE_LEFT,
      BORDER_TOP = 1 << EDGE_TOP,
      BORDER_RIGHT = 1 << EDGE_RIGHT,
      BORDER_BOTTOM = 1 << EDGE_BOTTOM,
      BORDER_ALL = BORDER_LEFT | BORDER_TOP | BORDER_RIGHT | BORDER_BOTTOM
    };
  enum
    {
      DOCKABLE_LEFT = BORDER_LEFT,
      DOCKABLE_TOP = BORDER_TOP,
      DOCKABLE_RIGHT = BORDER_RIGHT,
      DOCKABLE_BOTTOM = BORDER_BOTTOM,
      DOCKABLE_ALL = BORDER_ALL
    };

  enum {DOCK_BAR_CLIENT_HEIGHT = 22};
  //enum {DOCK_BAR_DEFAULT_HEIGHT = DOCK_BAR_CLIENT_HEIGHT + HORZ_TOP_PAD + HORZ_BOTTOM_PAD};
  enum {DOCK_LENGTH_INFINITE = 0x7fff};

protected:
  dock_bar (dock_frame &, lisp, int);
  virtual ~dock_bar ();
  static int new_comctl_p ()
    {return sysdep.comctl32_version >= PACK_VERSION (4, 70);}
private:
  static LRESULT CALLBACK wndproc (HWND, UINT, WPARAM, LPARAM);
protected:
  virtual LRESULT wndproc (UINT, WPARAM, LPARAM);
  int subclass ();
  void unsubclass ();
  void erase_non_client () const;
  void draw_borders (HDC, RECT &) const;
  void draw_gripper (HDC, const RECT &) const;
  virtual void adjust_gripper (HDC, RECT &, const RECT &) const {}
  int nc_calc_size (RECT &) const;
  int lbtn_down (int, int);
  void modify_dock_edge (int edge) {b_edge = edge;}
  virtual void dock_edge () {}
  virtual void post_nc_destroy () {}
  virtual int do_context_menu (const POINT *) {return 0;}
private:
  RECT &rect () {return b_rect;}
  void modify_border (int remove, int add)
    {b_border = (b_border & ~remove) | add;}
  void modify_status (int remove, int add)
    {b_status = (b_status & ~remove) | add;}
public:
  const RECT &rect () const {return b_rect;}
  static dock_bar *from_hwnd (HWND hwnd)
    {return (dock_bar *)GetProp (hwnd, b_dock_bar_prop);}
  LRESULT sendmsg (UINT msg, WPARAM wparam, LPARAM lparam) const
    {return CallWindowProc (b_wndproc, b_hwnd, msg, wparam, lparam);}
  int create (DWORD exstyle, const char *class_name, const char *window_name,
              DWORD style, int x, int y, int cx, int cy, HWND hwnd_parent,
              HMENU hmenu, HINSTANCE hinst, void *param)
    {
      b_hwnd = CreateWindowEx (exstyle, class_name, window_name, style,
                               x, y, cx, cy, hwnd_parent, hmenu, hinst, param);
      return b_hwnd && subclass ();
    }
  long style () const {return GetWindowLong (b_hwnd, GWL_STYLE);}
  void set_style (long x) const {SetWindowLong (b_hwnd, GWL_STYLE, x);}
  void modify_style (long remove, long add) const
    {set_style ((style () & ~remove) | add);}
  static int vert_edge_p (int edge)
    {return (1 << edge) & ((1 << EDGE_LEFT) | (1 << EDGE_RIGHT));}
  int dock_vert_p () const {return vert_edge_p (edge ());}
  int edge () const {return b_edge;}
  int check_edge (int) const;
  int border () const {return b_border;}
  int dockable () const {return b_dockable;}
  int status () const {return b_status;}
  static void set_tooltip_no_prefix (HWND hwnd_tt)
    {SetWindowLong (hwnd_tt, GWL_STYLE,
                    GetWindowLong (hwnd_tt, GWL_STYLE) | TTS_NOPREFIX);}
  virtual void calc_window_size (SIZE &, int) const;
  virtual void calc_client_size (SIZE &, int) const = 0;
  virtual void reload_settings () {}
  virtual int notify (NMHDR *, LRESULT &) {return 0;}
  virtual int need_text (TOOLTIPTEXT &) {return 0;}
  void set_redraw ();
  void set_no_redraw ();
  virtual void draw_item (DRAWITEMSTRUCT *) {}
  virtual void update_ui () {}
  virtual lisp lookup_command (int) const {return 0;}
  lisp name () const {return b_lname;}
  virtual void gc_mark (void (*f)(lisp)) {(*f)(b_lname);}
  virtual void *ident () const {return 0;}
  virtual int focus () const {return 0;}
  virtual void color_changed () const {}
  virtual int set_horz_text_p (int) {return 0;}
  virtual int horz_width () const {return -1;}
  virtual void set_horz_width (int) {}
};

class tool_bm
{
public:
  class bm_node: public xlist_node <bm_node>
    {
      HBITMAP b_hbm;
      int b_ref;
      const char *b_path;
      bm_node () : b_hbm (0), b_ref (0), b_path (0) {}
      ~bm_node ()
        {
          if (b_hbm)
            DeleteObject (b_hbm);
          xfree ((void *)b_path);
        }
      void incref () {b_ref++;}
      int decref () {return --b_ref;}
      friend tool_bm;
    public:
      operator HBITMAP () const {return b_hbm;}
    };
  enum
    {
      LMB_NO_ERRORS,
      LMB_OPEN_FILE = Ecannot_open_bmp_file,
      LMB_BAD_FORMAT = Ebad_bmp_format,
      LMB_UNSUPPORTED = Eunsupported_bmp_format,
      LMB_NOMEM = Estorage_condition,
      LMB_FAILED = Ecannot_create_bitmap
    };
private:
  xlist <bm_node> bm_list;
protected:
  static int load_mapped_bitmap (const char *, HBITMAP &);
public:
  const bm_node *load (const char *, int &);
  void release (const bm_node *);
  void reload ();
};

class tool_bar: public dock_bar
{
protected:
  SIZE t_button_size;
  SIZE t_bitmap_size;
  const tool_bm::bm_node *t_bm;
protected:
  virtual void dock_edge ();
  void set_bitmap ();
  int nc_calc_size (RECT &) const;
  virtual LRESULT wndproc (UINT, WPARAM, LPARAM);
  int tb_flat_p () const {return new_comctl_p () && style () & TBSTYLE_FLAT;}
  void erase_bkgnd (HDC) const;
public:
  tool_bar (dock_frame &, lisp);
  ~tool_bar ();
  int create (HWND, DWORD, UINT);
  int set_bitmap_size (int cx, int cy)
    {
      t_bitmap_size.cx = cx;
      t_bitmap_size.cy = cy;
      return sendmsg (TB_SETBITMAPSIZE, 0, MAKELONG (cx, cy));
    }
  int set_button_size (int cx, int cy)
    {
      t_button_size.cx = cx;
      t_button_size.cy = cy;
      return sendmsg (TB_SETBUTTONSIZE, 0, MAKELONG (cx, cy));
    }
  int add_bitmap (const TBADDBITMAP &tbab, int n)
    {return sendmsg (TB_ADDBITMAP, n, LPARAM (&tbab));}
  int button_count () const
    {return sendmsg (TB_BUTTONCOUNT, 0, 0);}
  int add_buttons (int n, const TBBUTTON *b)
    {return sendmsg (TB_ADDBUTTONS, n, LPARAM (b));}
  HWND get_tooltips () const
    {return HWND (sendmsg (TB_GETTOOLTIPS, 0, 0));}
  int get_button (int i, TBBUTTON &b) const
    {return sendmsg (TB_GETBUTTON, i, LPARAM (&b));}
  int insert_button (int i, const TBBUTTON &b)
    {return sendmsg (TB_INSERTBUTTON, i, LPARAM (&b));}
  int delete_button (int i)
    {return sendmsg (TB_DELETEBUTTON, i, 0);}
  void set_button (int, const TBBUTTON &, int = 0);
  int item_rect (int i, RECT &r)
    {return sendmsg (TB_GETITEMRECT, i, LPARAM (&r));}
  int get_state (int id)
    {return sendmsg (TB_GETSTATE, id, 0);}
  int set_state (int id, int state)
    {return sendmsg (TB_SETSTATE, id, MAKELONG (state, 0));}
  int enable_button (int id, int f)
    {return sendmsg (TB_ENABLEBUTTON, id, MAKELONG (f ? 1 : 0, 0));}
  int check_button (int id, int f)
    {return sendmsg (TB_CHECKBUTTON, id, MAKELONG (f ? 1 : 0, 0));}
  int press_button (int id, int f)
    {return sendmsg (TB_PRESSBUTTON, id, MAKELONG (f ? 1 : 0, 0));}
  virtual void calc_client_size (SIZE &, int) const;
  int load_bitmap (const char *);
  virtual void reload_settings ();
};

class tab_bar: public dock_bar
{
protected:
  int t_tab_height;
  int t_horz_text;
  int t_horz_width;
  int t_horz_height;
private:
  int t_erasebkgnd_called;
  int t_dots;
  static const char b_tab_bar_spin_prop[];
protected:
  enum {IDC_TAB_SPIN = 1};
  enum {GRIPPER_SIZE = 3};
  enum {MIN_WIDTH = 20};
  struct draw_item_struct
    {
      HDC hdc;
      int state;
      RECT r;
      DWORD data;
    };
  virtual LRESULT wndproc (UINT, WPARAM, LPARAM);
  int nc_calc_size (RECT &) const;
  void paint ();
  void erase_bkgnd (HDC);
  int inverse_p () const {return style () & TCS_BOTTOM;}
  virtual void dock_edge ();
  void draw_item (const draw_item_struct &, char *, int,
                  COLORREF, COLORREF) const;
  virtual void draw_item (const draw_item_struct &) {}
  virtual void update_ui ();
  int abbrev_text (HDC, char *, int, int) const;
  virtual void adjust_gripper (HDC, RECT &, const RECT &) const;
  int lbtn_down (int, int);
  int move_tab (int, int);
  int set_cursor (WPARAM, LPARAM);
  virtual int do_context_menu (const POINT *);
  virtual lisp context_menu (int) {return Qnil;}
private:
  void modify_spin ();
  void paint_left (HDC, const RECT &, const RECT &, int);
  void paint_top (HDC, const RECT &, const RECT &, int);
  void paint_right (HDC, const RECT &, const RECT &, int);
  void paint_bottom (HDC, const RECT &, const RECT &, int);
  int notify_spin (NMHDR *, LRESULT &) const;
  static void parent_notify (UINT, UINT, HWND);
  static LRESULT CALLBACK spin_wndproc (HWND, UINT, WPARAM, LPARAM);
public:
  tab_bar (dock_frame &, lisp);
  ~tab_bar () {}
  int create (HWND hwnd_parent, DWORD style, UINT id)
    {
      return dock_bar::create (0, WC_TABCONTROL, "",
                               style, 0, 0, 0, 0, hwnd_parent,
                               (HMENU)id, app.hinst, 0);
    }
  int create (HWND);
  virtual void calc_client_size (SIZE &, int) const;
  void calc_tab_height ();
  void set_font (HFONT hf)
    {sendmsg (WM_SETFONT, WPARAM (hf), 0);}
  int insert_item (int i, const TC_ITEM &ti)
    {return sendmsg (TCM_INSERTITEM, i, LPARAM (&ti));}
  int delete_item (int i)
    {return sendmsg (TCM_DELETEITEM, i, 0);}
  int set_item (int i, const TC_ITEM &ti)
    {return sendmsg (TCM_SETITEM, i, LPARAM (&ti));}
  int get_item (int i, TC_ITEM &ti) const
    {return sendmsg (TCM_GETITEM, i, LPARAM (&ti));}
  void set_padding (int cx, int cy) const
    {sendmsg (TCM_SETPADDING, 0, MAKELONG (cx, cy));}
  void adjust_rect (int f, RECT &r) const
    {sendmsg (TCM_ADJUSTRECT, f, LPARAM (&r));}
  int item_count () const
    {return sendmsg (TCM_GETITEMCOUNT, 0, 0);}
  int set_cursel (int i)
    {return sendmsg (TCM_SETCURSEL, i, 0);}
  int get_cursel () const
    {return sendmsg (TCM_GETCURSEL, 0, 0);}
  HWND get_tooltips () const
    {return (HWND)sendmsg (TCM_GETTOOLTIPS, 0, 0);}
  int get_item_rect (int i, RECT &r) const
    {return sendmsg (TCM_GETITEMRECT, i, LPARAM (&r));}
  int set_item_size (int cx, int cy)
    {return sendmsg (TCM_SETITEMSIZE, 0, MAKELPARAM (cx, cy));}
  int hit_test (TC_HITTESTINFO &info)
    {return sendmsg (TCM_HITTEST, 0, LPARAM (&info));}
  DWORD nth (int) const;
  virtual int focus () const
    {
      if (style () & TCS_FOCUSNEVER)
        return 0;
      SetFocus (b_hwnd);
      return 1;
    }
  virtual void color_changed () const {InvalidateRect (b_hwnd, 0, 0);}
  int horz_text_p () const {return t_horz_text;}
  virtual int set_horz_text_p (int x)
    {
      if (t_horz_text ? x : !x)
        return 0;
      t_horz_text = !t_horz_text;
      return 1;
    }
  virtual int horz_width () const {return t_horz_width;}
  virtual void set_horz_width (int w) {t_horz_width = max (MIN_WIDTH, w);}
};

class dock_frame
{
public:
  enum
    {
      EDGE_LEFT = dock_bar::EDGE_LEFT,
      EDGE_TOP = dock_bar::EDGE_TOP,
      EDGE_RIGHT = dock_bar::EDGE_RIGHT,
      EDGE_BOTTOM = dock_bar::EDGE_BOTTOM,
      EDGE_INVALID,
      EDGE_MAX = EDGE_INVALID
    };
private:
  typedef xlist <dock_bar> dock_bar_list;

  HWND f_hwnd;
  HWND f_hwnd_frame;
  dock_bar_list f_bars[EDGE_MAX + 1];
  RECT f_edge_rect[EDGE_MAX];
  int f_arrange;
  tool_bm f_bm;

  enum {MIN_SIZE = 32};

  static LONG arrange_horz (const dock_bar_list &, LONG);
  static LONG arrange_vert (const dock_bar_list &, LONG);
  static LONG horz_max (const dock_bar_list &, LONG);
  static LONG vert_max (const dock_bar_list &, LONG);
  static void dock_edge (dock_bar *, int, int);
  int defer_window_pos (HDWP &, HWND &, int);
  void set_window_pos (HWND &, int);
  static void limit_range (SIZE &, const RECT &, const RECT &);
  static void drag_rect (RECT &, int, int, const POINT &, const SIZE *,
                         const SIZE &, const SIZE &);
  int on_edge (const RECT &, int, int, int &) const;
  int on_edge_p (const RECT &, int, int) const;
  void move_bar (dock_bar *, RECT &, int);
public:
  dock_frame ();
  ~dock_frame ();
  tool_bm &bm () {return f_bm;}
  void init (HWND hwnd, HWND hwnd_frame)
    {f_hwnd = hwnd; f_hwnd_frame = hwnd_frame;}
  HWND hwnd_frame () const {return f_hwnd_frame;}
  void add (dock_bar *);
  void show (dock_bar *, int, const POINT *, int);
  void hide (dock_bar *);
  void remove (dock_bar *);
  void calc_layout (RECT &, HWND);
  int move_bar (dock_bar *, int, int);
  void reload_settings ();
  int notify (NMHDR *, LRESULT &);
  void cleanup ();
  int modified () const {return f_arrange;}
  void arrange_bar (const dock_bar *bar) {f_arrange |= 1 << bar->edge ();}
  int draw_item (DRAWITEMSTRUCT *);
  dock_bar *find (lisp) const;
  void update_ui ();
  lisp lookup_command (int) const;
  void gc_mark (void (*)(lisp));
  lisp list_bars () const;
  int focus_next (const dock_bar *) const;
  int focus_prev (const dock_bar *) const;
  void color_changed () const;
  void refresh ();
  virtual void recalc_layout () = 0;
};

#endif /* _dockbar_h_ */
