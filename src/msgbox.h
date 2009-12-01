#ifndef _msgbox_h_
# define _msgbox_h_

class XMessageBox
{
public:
  enum {MAX_BUTTONS = 5};
  enum
    {
      IDBUTTON1 = 1000,
      IDBUTTON2,
      IDBUTTON3,
      IDBUTTON4,
      IDBUTTON5,
    };
protected:
  HINSTANCE hinst;
  const char *msg;
  const char *title;
  HFONT hfont;
  HICON hicon;
  HWND hwnd;
  enum {XOFF = 14, YOFF = 12};
  int nbuttons;
  struct
    {
      UINT id;
      const char *caption;
    } btn[MAX_BUTTONS];
  int close_id;
  int default_btn;
  int f_crlf;
  int f_no_wrap;

  BOOL WndProc (UINT, WPARAM, LPARAM);
  BOOL init_dialog ();
  void calc_text_rect (RECT &) const;
  void calc_button_size (RECT br[MAX_BUTTONS]) const;
  HWND create_ctl (const char *, const char *, DWORD, UINT, const RECT &) const;
  void create_btn (const char *, UINT, const RECT &) const;
  void create_label (const char *, const RECT &, int) const;
  void create_icon (const RECT &) const;
  void create_buttons (const RECT br[MAX_BUTTONS]) const;
  static BOOL CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);
public:
  XMessageBox (HINSTANCE hinst_, const char *msg_, const char *title_,
               int crlf, int no_wrap)
       : hinst (hinst_), msg (msg_), title (title_), nbuttons (0),
         close_id (-1), default_btn (0), hicon (0),
         f_crlf (crlf), f_no_wrap (no_wrap) {}
  void add_button (UINT, const char *);
  void set_button (int, UINT, const char *);
  void set_default (int n) {default_btn = n;}
  void set_close (int id) {close_id = id;}
  void set_icon (HICON h) {hicon = h;}
  int doit (HWND);
};

int MsgBox (HWND, const char *, const char *, UINT, int);
int MsgBoxEx (HWND, const char *, const char *, int, int, int, int,
              const char **, int, int, int);

#endif
