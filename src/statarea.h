#ifndef _statarea_h_
#define _statarea_h_

class status_area
{
  enum {ST_TIME, ST_POS, ST_CODE, ST_UNICODE, ST_MAX};

  HWND s_hwnd;
  HFONT s_hfont;
  int s_flags;
  int s_clwidth;
  int s_borders[3];
  int s_min_ext[ST_MAX];
  int s_extent[ST_MAX];
  int s_order[ST_MAX];
  int s_nitems;
  int s_dow;
  char *s_lbuf[ST_MAX];
  char s_timeb[16];             // " XX/XX XX:XX "
  char s_posb[32];              // " XXXXXXXXXX:XXXXXXXXXX "
  char s_codeb[8];              // " XXXX "
  char s_unicodeb[12];          // " U+XXXX "

  static const char s_nil[];
  static const char s_eof[];

  void clear_cache ();
  int get_extent (const char *) const;
  int calc_extent (int, const char *);
  void set_parts () const;
  void update (int) const;
  void update_all ();
  int position ();
  int char_code ();
  int char_unicode ();
  int time ();
  void parse_format (const Char *, int);
  static lisp format_modified_p ();
  static int char_ext (HDC hdc, char c)
    {
      SIZE sz;
      GetTextExtentPoint32 (hdc, &c, 1, &sz);
      return sz.cx;
    }
  static int char_max_ext (HDC, char, char);
public:
  void init (HWND);
  void resize ();
  void update ();
  void timer ();
  void reload_settings ();
};

#endif /* _statarea_h_ */
