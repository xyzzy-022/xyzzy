#ifndef _printdlg_h_
#define _printdlg_h_

class print_dialog
{
protected:
  int init_dialog (HWND);
  BOOL command (UINT, UINT);
  BOOL destroy ();
  BOOL notify (NMHDR *);
  BOOL quit ();
  BOOL ok (UINT);
  BOOL cancel (UINT);
  BOOL clicked (UINT, int) const;
  BOOL range_command (UINT, int, UINT, LONG, const char *) const;

  int notice (UINT id, UINT ids) const
    {return print_engine::notice (m_hwnd, id, ids);}
  int notice (UINT id, UINT ids, int arg) const
    {return print_engine::notice (m_hwnd, id, ids, arg);}
  void set_margin_text (UINT, LONG, const char *) const;
  void set_margin (UINT, UINT, LONG, int, int, int, const char *) const;
  LONG parse_margin_text (UINT, const char *) const;
  void init_margin (int) const;
  void enable_linenums (int) const;
  void enable_pages (int) const;
  void update_ncopies () const;
  int get_int (UINT, BOOL *, BOOL, int) const;
  int check_margin_text (UINT, LONG &, LONG, LONG, const char *) const;
  void add_lang () const;
  BOOL notify_spin (NMHDR *, const char *);
  BOOL print_setup ();
  int preview ();
  BOOL set_font ();
  void set_font_face (int) const;
  int lang_command (int) const;
  int current_lang () const;
  int get_result (int);
  void set_print_range () const;
  void update_font_size ();
  int get_copies ();
  int recommend_size ();
  void check_proportional_font () const;
  BOOL wndproc (UINT, WPARAM, LPARAM);
  static BOOL CALLBACK wndproc (HWND, UINT, WPARAM, LPARAM);
  void init_history (UINT, const char *);
  int history_command (UINT, UINT, UINT, UINT);
  void move_btn_focus (UINT, UINT);
  int add_history (UINT, UINT, UINT, UINT);
  int delete_history (UINT, UINT, UINT, UINT);
  void save_history (UINT, const char *);
  int find_history (UINT, const char *);
  static int find_menu_text (HMENU, int, char *, int);
  int format_popup (UINT, class subclass_combo &);

private:
  HWND m_hwnd;
  printer_device &m_dev;
  print_settings &m_settings;
  print_engine &m_engine;

public:
  print_dialog (printer_device &dev, print_settings &settings, print_engine &engine)
       : m_dev (dev), m_settings (settings), m_engine (engine) {}
  ~print_dialog () {}
  int do_modal (HWND);
};

#endif /* _printdlg_h_ */
