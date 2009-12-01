#ifndef _ldialog_h_
# define _ldialog_h_

class dlgctrl: public lisp_object
{
public:
  int id () const;
  lisp symid () const;
  lisp wclass () const;
  DWORD style () const;
  lisp keyword () const;
};

inline int
dlgctrl::id () const
{
  return xshort_int_value (xcar (lisp (this)));
}

inline lisp
dlgctrl::symid () const
{
  return xcar (xcdr (lisp (this)));
}

inline lisp
dlgctrl::wclass () const
{
  return xcar (xcdr (xcdr (lisp (this))));
}

inline DWORD
dlgctrl::style () const
{
  return fixnum_value (xcar (xcdr (xcdr (xcdr (lisp (this))))));
}

inline lisp
dlgctrl::keyword () const
{
  return xcdr (xcdr (xcdr (xcdr (lisp (this)))));
}

# define ctl_fn(CLASS, NAME) CONCAT3 (CLASS, _, NAME)

# define with_ctl1(WCLASS, FN, CLASS, RESULT) \
  if (WCLASS == CONCAT (K, CLASS)) RESULT ctl_fn (CLASS, FN); else

# define with_wctl(WCLASS, FN, RESULT) \
  with_ctl1 (WCLASS, FN, button, RESULT) \
  with_ctl1 (WCLASS, FN, edit, RESULT) \
  with_ctl1 (WCLASS, FN, static, RESULT) \
  with_ctl1 (WCLASS, FN, listbox, RESULT) \
  /*with_ctl1 (WCLASS, FN, scrollbar, RESULT) */\
  with_ctl1 (WCLASS, FN, combobox, RESULT) \
  with_ctl1 (WCLASS, FN, link, RESULT)

# define with_cctl(WCLASS, FN, RESULT) \
  with_ctl1 (WCLASS, FN, spin, RESULT)

# define with_ctl(WCLASS, FN, RESULT) \
  with_wctl (WCLASS, FN, RESULT) \
  with_cctl (WCLASS, FN, RESULT);

# define declare_wctl(TYPE, FN, ARGLIST) \
  TYPE ctl_fn (button, FN) ARGLIST; \
  TYPE ctl_fn (edit, FN) ARGLIST; \
  TYPE ctl_fn (static, FN) ARGLIST; \
  TYPE ctl_fn (listbox, FN) ARGLIST; \
  /*TYPE ctl_fn (scrollbar, FN) ARGLIST; */\
  TYPE ctl_fn (combobox, FN) ARGLIST; \
  TYPE ctl_fn (link, FN) ARGLIST

# define declare_cctl(TYPE, FN, ARGLIST) \
  TYPE ctl_fn (spin, FN) ARGLIST

# define declare_ctl(TYPE, FN, ARGLIST) \
  declare_wctl(TYPE, FN, ARGLIST); \
  declare_cctl(TYPE, FN, ARGLIST)

struct dlg_txtwidth
{
  HDC hdc;
  int l;
};

class Dialog
{
public:
  lisp d_item;
  lisp d_init;
  lisp d_result;
  lisp d_retval;
  HWND d_hwnd;
protected:
  HGLOBAL d_h;
  DLGTEMPLATE *d_tmpl;
  int d_can_close;

  protect_gc d_gcpro1, d_gcpro2, d_gcpro3;

  static lisp find_handler (lisp, lisp);
  static WORD *store_unicode (WORD *, lisp);

  declare_ctl (void, pre_init, (dlgctrl *));
  declare_ctl (void, init, (dlgctrl *, lisp));
  declare_wctl (void, command, (dlgctrl *, UINT));
  declare_cctl (void, command, (dlgctrl *, NMHDR *));
  declare_ctl (lisp, result, (dlgctrl *));
  declare_ctl (void, invalidate, (dlgctrl *));

  int send_ltext (int, int, WPARAM, lisp, dlg_txtwidth * = 0) const;
  void enable_windows (dlgctrl *, int);
  void invalidate_ctrls (dlgctrl *);
  static lisp check_result_type (dlgctrl *, const char *);
  static lisp warn (lisp);
  lisp make_lb_string (int, int, int, int);
  int get_result (dlgctrl *);
  int button_push (dlgctrl *);

public:
  Dialog (lisp);
  ~Dialog ();
  void init_items ();
  void create_dialog_template (lisp, lisp, DWORD, int);
  dlgctrl *get_item (int) const;
  dlgctrl *get_item (lisp) const;
  void process_command (int, UINT);
  void process_notify (NMHDR *);
  void center_window () const;
  void draw_item (int, DRAWITEMSTRUCT *);
  void listbox_char (int, int);
  static int measure_item (HWND, MEASUREITEMSTRUCT *);
  DLGTEMPLATE *get_template () const;
};

inline DLGTEMPLATE *
Dialog::get_template () const
{
  return d_tmpl;
}

class PropSheet;

class PropPage: public Dialog
{
protected:
  int get_result ();

public:
  PropSheet *p_parent;
  lisp p_ident;
  lisp *p_result;
  int p_page_no;

  PropPage ();
  ~PropPage ();
  void create_template (lisp, lisp);
  void init_page (PropSheet *, int, PROPSHEETPAGE *, lisp);
  void kill_active ();
  void set_active () const;
  void reset () const;
};

class PropSheet
{
public:
  PropPage *ps_pages;
  int ps_result;
  int ps_moved;
  int ps_curpage;

  PropSheet ();
  ~PropSheet ();
};

struct PropSheetFont
{
  enum {IDD_PROPSHEET = 1006};
  static WCHAR face[];
  static int point;
  static int face_len;
  static int load ();
  static void find_font (const DLGTEMPLATE *);
  static HGLOBAL change_font (const DLGTEMPLATE *, DWORD);
  static HGLOBAL change_font (const char *);
};

#endif
