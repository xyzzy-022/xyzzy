#ifndef _print_h_
#define _print_h_

#define MAX_HEADER_LENGTH 256

struct PRLOGFONT
{
  int point;
  u_char charset;
  u_char bold;
  u_char italic;
  char face[LF_FACESIZE];
};

class print_settings;

class printer_device
{
private:
  static HGLOBAL pd_devmode;
  static HGLOBAL pd_devnames;
  HDC pd_hdc;

  int pd_xdpi;
  int pd_ydpi;
  SIZE pd_size;
  SIZE pd_physsize_pxl;
  RECT pd_min_margin_pxl;
  SIZE pd_physsize_mm;
  RECT pd_min_margin_mm;
  LONG pd_max_copies;
  DWORD pd_dm_fields;

private:
  static void notice_pderr (int);
  static int do_print_dialog1 (PRINTDLG &);
  static int do_print_dialog (PRINTDLG &);
  static int get_defaults ();
  static HDC create_dc ();
  void get_dev_spec ();

public:
  printer_device ();
  ~printer_device ();
  int init ();
  int create_printer_dc ();
  int print_setup_dialog (HWND);
  void set_dev_copies (const print_settings &);
  void get_dev_copies (print_settings &);

  LONG xpxl2mm (LONG x) const
    {return MulDiv (x, 254, pd_xdpi);}
  LONG ypxl2mm (LONG y) const
    {return MulDiv (y, 254, pd_ydpi);}
  LONG mm2xpxl (LONG x) const
    {return MulDiv (x, pd_xdpi, 254);}
  LONG mm2ypxl (LONG y) const
    {return MulDiv (y, pd_ydpi, 254);}
  LONG pt2xpxl (LONG x) const
    {return MulDiv (x, pd_xdpi, 720);}
  LONG pt2ypxl (LONG x) const
    {return MulDiv (x, pd_ydpi, 720);}
  int xdpi () const {return pd_xdpi;}
  int ydpi () const {return pd_ydpi;}
  const SIZE &size () const {return pd_size;}
  const SIZE &physsize_pxl () const {return pd_physsize_pxl;}
  const RECT &min_margin_pxl () const {return pd_min_margin_pxl;}
  const SIZE &physsize_mm () const {return pd_physsize_mm;}
  const RECT &min_margin_mm () const {return pd_min_margin_mm;}
  LONG max_copies () const {return pd_max_copies;}
  DWORD dm_fields () const {return pd_dm_fields;}
  operator HDC () const {return pd_hdc;}
};

class print_settings
{
public:
  enum print_range
    {
      RANGE_ALL,
      RANGE_LINENUM,
      RANGE_SELECTION,
      RANGE_PAGE
    };
  enum {COLUMN_MAX = 16};

  RECT ps_text_margin_mm;
  LONG ps_header_offset_mm;
  LONG ps_footer_offset_mm;
  LONG ps_column_sep_mm;
  LONG ps_line_spacing_pt;
  int ps_print_linenum;
  int ps_multi_column;
  int ps_fold_width;
  char ps_header[MAX_HEADER_LENGTH];
  char ps_footer[MAX_HEADER_LENGTH];
  int ps_header_on;
  int ps_footer_on;
  int ps_collate;
  int ps_ncopies;
  int ps_recommend_size;
  int ps_show_proportional;
  int ps_use_bitmap;

  print_range ps_print_range;
  long ps_range_start;
  long ps_range_end;

  PRLOGFONT ps_font[FONT_MAX];

  RECT ps_text_margin_pxl;
  LONG ps_header_offset_pxl;
  LONG ps_footer_offset_pxl;
  LONG ps_column_sep_pxl;
  LONG ps_line_spacing_pxl;

private:
  static int CALLBACK check_valid_font (const ENUMLOGFONT *, const NEWTEXTMETRIC *,
                                        DWORD, LPARAM);

public:
  print_settings ();
  void init_faces ();
  void load_conf ();
  void save_conf ();
  void calc_pxl (const printer_device &);
  HFONT make_font (HDC, int, int) const;
  HFONT make_font (HDC hdc, const printer_device &dev, int charset) const
    {return make_font (hdc, charset, -dev.pt2ypxl (ps_font[charset].point));}
};

struct glyph_width
{
  HDC hdc;
  const HFONT *hfonts;
  int height;
  short pixel[CHAR_LIMIT];
};

struct PaintCtx;

class print_engine
{
  Buffer *pe_bp;
  const printer_device &pe_dev;
  print_settings &pe_settings;

  Point pe_point;
  long pe_linenum;
  int pe_page;
  int pe_total_pages;
  SYSTEMTIME pe_time;
  int pe_start_page;
  int pe_end_page;

  SIZE pe_cell;
  SIZE pe_print_cell;
  int pe_fixed_pitch;
  HFONT pe_hfonts[FONT_MAX];
  POINT pe_offset[FONT_MAX];
  int pe_offset2x[FONT_MAX];
  glyph_width pe_glyph_width;

  RECT pe_area;
  int pe_sep_pxl;
  int pe_page_width;
  SIZE pe_ech;
  POINT pe_header;
  POINT pe_footer;
  int pe_fold_columns;
  fold_parameter pe_fold_param;

  int pe_digit_width;
  int pe_linenum_width;
  int pe_start_pixel;
  int pe_copying_width;

  HBITMAP pe_hbm;
  void *pe_bits;
  struct
    {
      BITMAPINFOHEADER bi;
      RGBQUAD rgb[2];
    } pe_bi;

public:
  enum
    {
      LINENUM_UNINITIALIZED = -1,
      LINENUM_EOB = 0
    };
  struct page_info
    {
      point_t point;
      long linenum;
    };

private:
  class page_cache
    {
    public:
      enum {PAGES_PER_BLOCK = 128};
      struct cache_block
        {
          cache_block *b_next;
          int b_page;
          int b_used;
          page_info b_block[PAGES_PER_BLOCK];
        };
    private:
      cache_block *c_block;

      cache_block *find_block (int);
      cache_block *alloc_block (int);
      page_info *alloc_page (int);

    public:
      page_cache () : c_block (0) {}
      ~page_cache () {cleanup ();}
      page_info *find (int);
      int save (const Point &, int, int);
      void cleanup ();
    };

  page_cache pe_cache;
  page_info pe_next;

  int record_page (int, const Point &, int, int);
  void cleanup ();

  int next_line (Point &) const;
  int form_feed_p (const Point &) const;

  void paint_ascii (PaintCtx &, Char) const;
  void paint_kana (PaintCtx &, Char) const;
  void paint_kanji (PaintCtx &, Char) const;
  void paint_latin (PaintCtx &, Char, int) const;
  void paint_jisx0212 (PaintCtx &, Char) const;
  void paint_full_width (PaintCtx &, Char, int) const;
  void paint_lucida (PaintCtx &, Char) const;
  int paint_line (HDC, int, int, Point &, long &) const;
  void paint_header (HDC);
  void paint_footer (HDC);
  int paint_fmt (HDC, const char *, int);
  void paint_string (HDC, int, int, const char *, int) const;
  int get_extent (const char *, int) const;

  SYSTEMTIME &current_time ();

  char *fmt_filename_short (char *, char *);
  char *fmt_filename_long (char *, char *);
  char *fmt_buffer_name (char *, char *);
  char *fmt_page_no (char *, char *);
  char *fmt_total_page_no (HDC, char *, char *);
  char *fmt_year4 (char *, char *);
  char *fmt_year2 (char *, char *);
  char *fmt_month (char *, char *, int, int, int);
  char *fmt_day (char *, char *, int);
  char *fmt_week (char *, char *, int, int);
  char *fmt_hour24 (char *, char *, int);
  char *fmt_hour12 (char *, char *, int, int, int);
  char *fmt_minute (char *, char *, int);
  char *fmt_second (char *, char *, int);
  int format (HDC, const char *, char *, int, char *&, char *&);
  static char *fmt (char *, char *, const char *, int);

  int skip_page (HDC, Point &, long &);

  void init_font (HDC);
  int init_area (HDC);
  int doprint1 (HWND);
  int preview1 (HWND);
  void set_print_range () const;
  int make_bitmap (HDC);

public:
  print_engine (Buffer *, const printer_device &, print_settings &);
  ~print_engine ();
  int count_total_pages (HDC);
  void paint (HDC, int);
  enum
    {
      WIDTH_TOO_SMALL = 1,
      HEIGHT_TOO_SMALL,
      FOLD_TOO_SMALL,
      CREATE_BM_FAILED,
      HEADER_OFFSET_TOO_LARGE,
      FOOTER_OFFSET_TOO_LARGE,
      FOLD_TOO_LARGE = 0x80000000
    };
  int init (HDC, int, int);
  int next_page ();
  int prev_page ();
  int next_page_exist_p ();
  int prev_page_exist_p ();
  int current_page () const {return pe_page;}
  int set_start_end (HDC, int, int, int);
  const SIZE &cell_size () const {return pe_print_cell;}
  int doprint (HWND);
  int preview (HWND);
  static int init_error (HWND, int, UINT = UINT (-1), UINT = UINT (-1),
                         UINT = UINT (-1), UINT = UINT (-1), UINT = UINT (-1));
  static int bad_range (HWND);
  static int notice (HWND, UINT, UINT);
  static int notice (HWND, UINT, UINT, int);
};


#endif /* _print_h_ */
