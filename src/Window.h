#ifndef _Window_h_
# define _Window_h_

typedef u_long glyph_t;

/*
 GLYPH BITS

  3               2              1           0
 10 98765 4 3 2 1 0987 6 5 4 3 2109 8 76543210
 nn nnnnn b b b b nnnn b b b b nnnn b cccccccc
 -- ----- | | | | ---- | | | | ---- |
  \   \    \ \ \ \  \   \ \ \ \  \   \__________ 0: Char  1: Bitmap
   \   \    \ \ \ \  \   \ \ \ \  \_____________ textprop-fg-bit == 0:
    \   \    \ \ \ \  \   \ \ \ \                  0: normal    4: keyword3   8: -         12: linenum
     \   \    \ \ \ \  \   \ \ \ \                 1: ctrl      5: keyword1r  9: -         13: string
      \   \    \ \ \ \  \   \ \ \ \                2: keyword1  6: keyword2r 10: -         14: tag
       \   \    \ \ \ \  \   \ \ \ \               3: keyword2  7: keyword3r 11: -         15: comment
        \   \    \ \ \ \  \   \ \ \ \            textprop-fg-bit == 1:
         \   \    \ \ \ \  \   \ \ \ \             textprop fg
          \   \    \ \ \ \  \   \ \ \ \_________ hidden
           \   \    \ \ \ \  \   \ \ \__________ selected
            \   \    \ \ \ \  \   \ \___________ reversed
             \   \    \ \ \ \  \   \____________ textprop-fg-bit
              \   \    \ \ \ \  \_______________ textprop bg
               \   \    \ \ \ \_________________ bold
                \   \    \ \ \__________________ italic (yet)
                 \   \    \ \___________________ underline
                  \   \    \____________________ strikeout
                   \   \________________________ charset
                    \___________________________ category
                                                   0: SBCS   2: DBCS lead
                                                   1: JUNK   3: DBCS trail
*/

# define GLYPH_NFOREGROUND_COLORS 128

# define GLYPH_COLOR_SHIFT_BITS 9

# define GLYPH_TEXTPROP_COLOR_NBITS 4
# define GLYPH_TEXTPROP_NCOLORS (1 << GLYPH_TEXTPROP_COLOR_NBITS)
# define GLYPH_TEXTPROP_FG_SHIFT_BITS 9
# define GLYPH_TEXTPROP_BG_SHIFT_BITS 17
# define TEXTPROP_EXTEND_EOL_BIT 0x80000000

# define GLYPH_COLOR_MASK        0x001fff00
# define  GLYPH_FOREGROUND_MASK  0x0000fe00
# define  GLYPH_BACKGROUND_MASK  0x0000e000
# define  GLYPH_FORE_COLOR_MASK  0x0001fe00
# define  GLYPH_BACK_COLOR_MASK  0x001efe00

# define GLYPH_TEXTPROP_FG_BIT   0x00010000

# define GLYPH_BITMAP_BIT        0x00000100
# define  GLYPH_BM               GLYPH_BITMAP_BIT
# define    GLYPH_BM_BACKSL      (GLYPH_BM | FontSet::backsl)
# define    GLYPH_BM_SEP         (GLYPH_BM | FontSet::sep)
# define    GLYPH_BM_NEWLINE     (GLYPH_BM | FontSet::newline)
# define    GLYPH_BM_HTAB        (GLYPH_BM | FontSet::htab)
# define    GLYPH_BM_HALFSPC     (GLYPH_BM | FontSet::halfspc)
# define    GLYPH_BM_FULLSPC1    (GLYPH_BM | FontSet::fullspc1)
# define    GLYPH_BM_FULLSPC2    (GLYPH_BM | FontSet::fullspc2)
# define    GLYPH_BM_BLANK       (GLYPH_BM | FontSet::blank)
# define    GLYPH_BM_WBLANK1     (GLYPH_BM | FontSet::wblank1)
# define    GLYPH_BM_WBLANK2     (GLYPH_BM | FontSet::wblank2)
# define    GLYPH_BM_FOLD_SEP0   (GLYPH_BM | FontSet::fold_sep0)
# define    GLYPH_BM_FOLD_MARK_SEP0 (GLYPH_BM | FontSet::fold_mark_sep0)
# define    GLYPH_BM_FOLD_SEP1   (GLYPH_BM | FontSet::fold_sep1)
# define    GLYPH_BM_FOLD_MARK_SEP1 (GLYPH_BM | FontSet::fold_mark_sep1)
# define  GLYPH_TEXT_MASK        (0x00001e00 | GLYPH_TEXTPROP_FG_BIT)
# define   GLYPH_KEYWORD_SHIFT_BITS 9
# define    GLYPH_CTRL           0x00000200
# define    GLYPH_KEYWORD1       0x00000400
# define    GLYPH_KEYWORD2       0x00000600
# define    GLYPH_KEYWORD3       0x00000800
# define    GLYPH_KEYWORD1R      0x00000a00
# define    GLYPH_KEYWORD2R      0x00000c00
# define    GLYPH_KEYWORD3R      0x00000e00
//# define    GLYPH_KEYWORD7       0x00001000
//# define    GLYPH_KEYWORD8       0x00001200
//# define    GLYPH_KEYWORD9       0x00001400
# define    GLYPH_LINENUM        0x00001800
# define    GLYPH_STRING         0x00001a00
# define    GLYPH_TAG            0x00001c00
# define    GLYPH_COMMENT        0x00001e00

# define GLYPH_HIDDEN            0x00002000
# define GLYPH_SELECTED          0x00004000
# define GLYPH_REVERSED          0x00008000

# define GLYPH_BOLD              0x00200000
//# define GLYPH_ITALIC          0x00400000
# define GLYPH_UNDERLINE         0x00800000
# define GLYPH_STRIKEOUT         0x01000000

# define GLYPH_CHARSET_MASK      0x3e000000
# define GLYPH_CHARSET_SHIFT_BIT 25
# define GLYPH_CHARSET(X)        ((X) & GLYPH_CHARSET_MASK)

# define MAKE_GLYPH_CHARSET(X)        ((X) << GLYPH_CHARSET_SHIFT_BIT)
# define GLYPH_CHARSET_USASCII        MAKE_GLYPH_CHARSET (0)
# define GLYPH_CHARSET_JISX0201_KANA  MAKE_GLYPH_CHARSET (1)
# define GLYPH_CHARSET_JISX0208       MAKE_GLYPH_CHARSET (2)
# define GLYPH_CHARSET_JISX0212       MAKE_GLYPH_CHARSET (3)
# define GLYPH_CHARSET_JISX0212_HALF  MAKE_GLYPH_CHARSET (4)
# define GLYPH_CHARSET_KSC5601        MAKE_GLYPH_CHARSET (5)
# define GLYPH_CHARSET_GB2312         MAKE_GLYPH_CHARSET (6)
# define GLYPH_CHARSET_BIG5           MAKE_GLYPH_CHARSET (7)
# define GLYPH_CHARSET_ISO8859_1_2    MAKE_GLYPH_CHARSET (8)
# define GLYPH_CHARSET_ISO8859_3_4    MAKE_GLYPH_CHARSET (9)
# define GLYPH_CHARSET_ISO8859_5      MAKE_GLYPH_CHARSET (10)
# define GLYPH_CHARSET_ISO8859_7      MAKE_GLYPH_CHARSET (11)
# define GLYPH_CHARSET_ISO8859_9_10   MAKE_GLYPH_CHARSET (12)
# define GLYPH_CHARSET_ISO8859_13     MAKE_GLYPH_CHARSET (13)
# define GLYPH_CHARSET_IPA            MAKE_GLYPH_CHARSET (14)
# define GLYPH_CHARSET_SMLCDM         MAKE_GLYPH_CHARSET (15)
# ifdef CCS_UJP_MIN
#  if (CCS_UJP_HALF_MAX - CCS_UJP_HALF_MIN) != 0x2ff
#   error "wrong GLYPH_CHARSET_UJP"
#  endif
#  define GLYPH_CHARSET_UJP           MAKE_GLYPH_CHARSET (16)
#  define GLYPH_CHARSET_UJP_H1        MAKE_GLYPH_CHARSET (17)
#  define GLYPH_CHARSET_UJP_H2        MAKE_GLYPH_CHARSET (18)
#  define GLYPH_CHARSET_UJP_H3        MAKE_GLYPH_CHARSET (19)
# endif
# ifdef CCS_ULATIN_MIN
#  if (CCS_ULATIN_MAX - CCS_ULATIN_MIN) != 0x1ff
#   error "wrong GLYPH_CHARSET_ULATIN"
#  endif
#  define GLYPH_CHARSET_ULATIN1       MAKE_GLYPH_CHARSET (20)
#  define GLYPH_CHARSET_ULATIN2       MAKE_GLYPH_CHARSET (21)
# endif
# define GLYPH_CHARSET_GEORGIAN       MAKE_GLYPH_CHARSET (22)

# define GLYPH_CATEGORY_MASK     0xc0000000
# define  GLYPH_JUNK             0x40000000
# define  GLYPH_LEAD             0x80000000
# define  GLYPH_TRAIL            0xc0000000

# define GLYPH_MAX_KEYWORDS      6

static inline void
glyph_make_junk (glyph_t *g)
{
  *g = (*g & ~GLYPH_CATEGORY_MASK) | GLYPH_JUNK;
}

static inline int
glyph_lead_p (glyph_t c)
{
  return (c & GLYPH_CATEGORY_MASK) == GLYPH_LEAD;
}

static inline int
glyph_trail_p (glyph_t c)
{
  return (c & GLYPH_CATEGORY_MASK) == GLYPH_TRAIL;
}

struct glyph_data
{
  short gd_len;
  short gd_mod;
  glyph_t gd_cc[1];
};

struct glyph_rep
{
  SIZE gr_size;
  glyph_data **gr_oglyph;
  glyph_data **gr_nglyph;
  int gr_ref;
  glyph_rep (int, int);
  void *operator new (size_t, void *);
#if _MSC_VER >= 1100
  void operator delete (void *, void *) {}
#endif
  void copy (const glyph_rep *);
  static int size (int, int);
};

inline int
glyph_rep::size (int w, int h)
{
  return (sizeof (glyph_rep)
          + (sizeof (glyph_data *)
             + (sizeof (glyph_data) + sizeof (glyph_t) * (w + 1))) * h * 2);
}

inline void *
glyph_rep::operator new (size_t, void *p)
{
  return p;
}

struct Glyphs
{
  glyph_rep *g_rep;
  Glyphs ();
  ~Glyphs ();
  Glyphs (const Glyphs &);
  Glyphs (glyph_rep *);
  void operator = (Glyphs &);
protected:
  void delete_rep ();
};

inline
Glyphs::Glyphs ()
     : g_rep (0)
{
}

inline
Glyphs::Glyphs (const Glyphs &src)
     : g_rep (src.g_rep)
{
  if (g_rep)
    g_rep->gr_ref++;
}

inline
Glyphs::Glyphs (glyph_rep *rep)
     : g_rep (rep)
{
  if (g_rep)
    g_rep->gr_ref++;
}

inline void
Glyphs::delete_rep ()
{
  if (g_rep)
    {
      assert (g_rep->gr_ref);
      if (!--g_rep->gr_ref)
        free (g_rep);
    }
}

inline
Glyphs::~Glyphs ()
{
  delete_rep ();
}

inline void
Glyphs::operator = (Glyphs &src)
{
  delete_rep ();
  g_rep = src.g_rep;
  if (g_rep)
    g_rep->gr_ref++;
}

class ScrollInfo: public tagSCROLLINFO
{
public:
  enum seen {undef, yes, no};
  seen sb_seen;
  ScrollInfo () : sb_seen (undef) {cbSize = sizeof (SCROLLINFO);}
};

class mode_line_painter
{
public:
  virtual void no_format_specifier() = 0;
  virtual int first_paint(HDC hdc, int start_px) = 0;
  virtual void update_paint(HDC hdc) = 0;
  virtual bool need_repaint_all() = 0;

  char* get_posp() { return posp; }
  void set_posp(char* p) { posp = p; }

private:
  char* posp;
};

class mode_line_percent_painter: public mode_line_painter
{
public:

  virtual void no_format_specifier() {
	  m_point_pixel = -1;
  }
  virtual int first_paint(HDC hdc, int start_px) {
	  m_point_pixel = start_px;
      m_last_percent = -1;
	  return paint_percent(hdc);
  }
  virtual void update_paint(HDC hdc) {
	  if(m_point_pixel == -1) // not in the format.
		  return ;
	  if(m_last_percent == m_percent)
		  return;
	  paint_percent(hdc);
  }

  virtual bool need_repaint_all();
  // end of commmon interface.

  mode_line_percent_painter() {
	  m_modeline_paramp = 0;

	  m_point_pixel = -1;

	  m_percent = -1;
	  m_last_width = -1;

	  m_ml_size.cx = 0xdeadbeef;
	  m_ml_size.cy = 0xdeadbeef;
  }

  inline void setup_paint(ModelineParam *param, int percent, const SIZE& winsize) {
	  m_modeline_paramp = param;
	  m_percent = percent;
	  m_ml_size.cx = winsize.cx;
	  m_ml_size.cy = winsize.cy;
  }

  static int calc_percent(Buffer *bufp, point_t point);

private:
  int paint_percent (HDC hdc);
  int m_point_pixel;
  int m_percent;
  SIZE m_ml_size;
  ModelineParam *m_modeline_paramp;


  int m_last_percent;
  int m_last_width;
};

class mode_line_point_painter : public mode_line_painter
{
public :
  int m_column;
  int m_plinenum;
  SIZE m_ml_size;


  int m_point_pixel;
  int m_last_ml_column;
  int m_last_ml_linenum;
  int m_last_ml_point_width;

  ModelineParam *m_modeline_paramp;

  mode_line_point_painter()
  {
	  m_modeline_paramp = 0;

	  m_point_pixel = -1;
	  m_last_ml_column = -1;
	  m_last_ml_linenum = -1;
	  m_last_ml_point_width = -1;


	  m_plinenum = 1;
	  m_column = 0;

	  m_ml_size.cx = 0xdeadbeef;
	  m_ml_size.cy = 0xdeadbeef;
  }
  virtual void no_format_specifier() {
	  m_point_pixel = -1;
  }
  virtual int first_paint(HDC hdc, int start_px) {
	  m_point_pixel = start_px;
      m_last_ml_column = m_last_ml_linenum = -1;
	  return paint_point(hdc);
  }
  virtual void update_paint(HDC hdc) {
	  paint_point(hdc);
  }

  virtual bool need_repaint_all();

  inline void setup_paint(ModelineParam *param, int column, int plinenum, const SIZE& winsize) {
	  m_modeline_paramp = param;
	  m_column = column;
	  m_plinenum = plinenum;
	  m_ml_size.cx = winsize.cx;
	  m_ml_size.cy = winsize.cy;
  }

private:
  int paint_point (HDC hdc);
};

struct wheel_info;

struct Window
{
  static wcolor_index forecolor_indexes[GLYPH_NFOREGROUND_COLORS];
  static wcolor_index backcolor_indexes[GLYPH_NFOREGROUND_COLORS];
  static COLORREF default_colors[WCOLOR_MAX];
  static XCOLORREF default_xcolors[USER_DEFINABLE_COLORS];
  enum modeline_color_index
    {MLCI_FOREGROUND, MLCI_BACKGROUND};
  static COLORREF modeline_colors[2];
  static XCOLORREF modeline_xcolors[2];

  static COLORREF w_textprop_forecolor[GLYPH_TEXTPROP_NCOLORS];
  static COLORREF w_textprop_backcolor[GLYPH_TEXTPROP_NCOLORS];
  static XCOLORREF w_textprop_xforecolor[GLYPH_TEXTPROP_NCOLORS];
  static XCOLORREF w_textprop_xbackcolor[GLYPH_TEXTPROP_NCOLORS];

  Window *w_prev;
  Window *w_next;

  lisp lwp;

  HWND w_hwnd;
  HWND w_hwnd_ml;
  RECT w_order;
  RECT w_rect;
  SIZE w_client;
  SIZE w_ech;
  SIZE w_ch_max;

  enum {RIGHT_PADDING = 2};
  SIZE w_clsize;

  Buffer *w_last_bufp;
  Buffer *w_bufp;

  Point w_point;
  point_t w_mark;

  point_t w_last_point;

  point_t w_disp;
  point_t w_last_disp;
  long w_last_top_linenum;
  long w_last_top_column;
  long w_last_mark_linenum;

  long w_linenum;
  long w_plinenum;
  long w_column;
  long w_goal_column;
  long w_top_column;

  enum {LINENUM_COLUMNS = 6};
  static int w_hjump_columns;

  mode_line_point_painter w_point_painter;
  mode_line_percent_painter w_percent_painter;
  SIZE w_ml_size;

  int w_last_percentage;
  int w_percentage_pixel;

  enum
    {
      WF_LINE_NUMBER = 1 << 0,
      WF_RULER = 1 << 1,
      WF_NEWLINE = 1 << 2,
      WF_HTAB = 1 << 3,
      WF_FULLSPC = 1 << 4,
      WF_VSCROLL_BAR = 1 << 5,
      WF_EOF = 1 << 6,
      WF_HSCROLL_BAR = 1 << 7,
      WF_MODE_LINE = 1 << 8,
      WF_FUNCTION_BAR = 1 << 9,
      WF_FOLD_MARK = 1 << 10,
      WF_CURSOR_LINE = 1 << 11,
      WF_HALFSPC = 1 << 12,
      WF_ALT_VSCROLL_BAR = 1 << 13,
      WF_SCROLLING = 1 << 14,
      WF_BGCOLOR_MODE = 1 << 15,
      WF_FOLD_LINE = 1 << 16,
    };
  static int w_default_flags;
  int w_flags_mask;
  int w_flags;
  int w_last_flags;

  ScrollInfo w_vsinfo;
  ScrollInfo w_hsinfo;

  Glyphs w_glyphs;

  Buffer::selection_type w_selection_type;
  point_t w_selection_point;
  point_t w_selection_marker;
  long w_selection_column;
  Region w_selection_region;

  Buffer::selection_type w_reverse_temp;
  Region w_reverse_region;

  const COLORREF *w_colors;
  COLORREF w_colors_buf[WCOLOR_MAX];

  enum
    {
      WDF_WINDOW = 1 << 0,
      WDF_GOAL_COLUMN = 1 << 1,
      WDF_SET_GOAL_COLUMN = 1 << 2,
      WDF_MODELINE = 1 << 3,
      WDF_REFRAME_SCROLL = 1 << 4,
      WDF_WINSIZE_CHANGED = 1 << 5,
      WDF_PENDING = 1 << 6,
      WDF_DELETE_TOP = 1 << 7
    };
  int w_disp_flags;

  int w_inverse_mode_line;
  int w_ime_mode_line;

  enum
    {
      LV_MODE_NAME,
      LV_MODE_LINE_FORMAT,
      LV_READ_ONLY,
      LV_OVERWRITE,
      LV_AUTO_FILL,
      NLAST_VERS
    };
  lisp w_last_vars[NLAST_VERS]; // no need gc

  struct {int x1, x2, ypixel;} w_cursor_line;

  long w_ruler_top_column;
  long w_ruler_column;
  long w_ruler_fold_column;

  int w_ignore_scroll_margin;

  static void init_colors (const XCOLORREF * = 0, const XCOLORREF * = 0,
                           const XCOLORREF * = 0, const XCOLORREF * = 0);
  void change_color ();

  int flags () const;
  void set_flags (int);
  void clr_flags (int);
  void use_default_flags (int);

  void update_vscroll_bar ();
  int vscroll_lines () const;
  void process_vscroll (int);
  void update_hscroll_bar ();
  void process_hscroll (int);
  void wheel_scroll (const wheel_info &);

  Window (int = 0, int = 0);
  Window (const Window &);
  void init (int, int);
  ~Window ();
  static void create_default_windows ();

  void save_buffer_params ();
  void set_buffer_params (Buffer *);
  void set_buffer (Buffer *);
  void calc_client_size (int, int);
  void reframe ();
  void paint_glyphs (HDC, HDC, const glyph_t *, const glyph_t *, const glyph_t *,
                     char *, const INT *, int, int, int) const;
  void paint_line (HDC, HDC, glyph_data *, const glyph_data *,
                   char *, int, const INT *) const;
  void paint_window (HDC) const;
  void paint_region (HDC, int, int) const;
  void find_motion () const;
  void redraw_window (Point &, long, int, int) const;
  int kwdmatch (lisp, const Char *, const Chunk *, int &, int &, int, int &, int) const;
  int kwdmatch (lisp, const Point &, int &, int &, int &, int) const;
  int redraw_line (glyph_data *, Point &, long, long, int, lisp,
                   syntax_info *, textprop *&, class regexp_kwd &) const;
  int next_draw_point (Point &, long) const;
  void scroll_lines (int);
  int refresh (int);
  void pending_refresh ();
  void scroll_down_region (int, int, int, int) const;
  void scroll_up_region (int, int, int, int) const;
  void update_window ();
  void clear_window ();
  void paint_mode_line (HDC);
  void paint_mode_line ();
  void paint_background (HDC) const;
  void paint_background (HDC, int, int, int, int) const;
  void winsize_changed (int, int);
  point_t bol_point (point_t) const;
  point_t folded_bol_point (point_t) const;
  void paint_minibuffer_message (lisp);

  int scroll_window (long, int = 0);
  int scroll_window_horizontally (long, int = 0);
  int folded_scroll_window (long, int = 0);
  int folded_scroll_window_horizontally (long, int = 0);

  void caret_size (SIZE &) const;
  int caret_column () const
    {
      int x = w_column - w_last_top_column + w_bufp->b_prompt_columns;
      if (w_last_flags & WF_LINE_NUMBER)
        x += LINENUM_COLUMNS + 1;
      return x;
    }
  int caret_line () const
    {return w_linenum - w_last_top_linenum;}
  static int caret_xpixel (int column)
    {return (column * app.text_font.cell ().cx
             + app.text_font.cell ().cx / 2);}
  static int caret_ypixel (int line)
    {return line * app.text_font.cell ().cy;}
  int caret_x () const
    {return caret_xpixel (caret_column ());}
  int caret_y () const
    {return caret_ypixel (caret_line ());}
  void hide_caret () const;
  void update_caret () const;
  static void update_last_caret ();
  static void update_caret (HWND, int, int, int, int, COLORREF);
  static void delete_caret ();
  static void compute_geometry (const SIZE & = app.active_frame.size,
                                int = app.text_font.cell ().cy);
  static void move_all_windows (int = 1);
  static void repaint_all_windows ();
  static void destroy_windows ();

  void split (int, int);
  int minibuffer_window_p () const;
  static Window *minibuffer_window ();
  void delete_other_windows ();
  void close ();
  static int count_windows ();
  enum
    {
      RE_LEFT = 1,
      RE_RIGHT = 2,
      RE_TOP = 4,
      RE_BOTTOM = 8
    };
  int find_resizeable_edge (LONG RECT::*, LONG RECT::*, LONG RECT::*, LONG RECT::*) const;
  int find_resizeable_edges () const;
  static Window *find_point_window (POINT &p);
  static Window *find_scr_point_window (const POINT &, int, int *);
  void resize_edge (LONG RECT::*, LONG RECT::*, LONG RECT::*, LONG RECT::*) const;
  void resize_edge (int) const;
  int delete_window ();

  void set_window ();

  static Window *coerce_to_window (lisp);
  static Window *find_point_window (const POINT &, int &);
  Window *find_resizeable_window (LONG RECT::*, LONG RECT::*, LONG RECT::*, LONG RECT::*, LONG RECT::*) const;
  Window *find_horiz_window (LONG RECT::*) const;
  Window *find_vert_window (LONG RECT::*) const;
  int get_horiz_min (int, int) const;
  int get_horiz_max (int, int) const;
  int get_vert_min (int, int) const;
  int get_vert_max (int, int) const;
  void change_vert_size (int, int, int);
  void change_horiz_size (int, int, int);
  static int find_vert_order (int);
  static int find_horiz_order (int);
  int enlarge_window_horiz (int);
  int enlarge_window_vert (int);
  int enlarge_window (int, int);

  static int frame_window_setcursor (HWND, WPARAM, LPARAM);
  static int frame_window_resize (HWND, LPARAM, const POINT * = 0);
  int frame_window_resize (HWND, const POINT &, int);

  int redraw_mode_line ();
  static void modify_all_mode_line ();

  void update_mode_line_vars (int, lisp);
  void update_mode_line_vars ();

  static void change_parameters (const FontSetParam &);
  static void change_parameters (const FontSetParam &,
                                 const XCOLORREF *, const XCOLORREF *,
                                 const XCOLORREF *, const XCOLORREF *,
                                 bool change_color_p = true);
  void invalidate_glyphs ();

  void discard_invalid_region (const PAINTSTRUCT &, RECT &);

  int alloc_glyph_rep ();
  COLORREF glyph_forecolor (glyph_t) const;
  COLORREF glyph_backcolor (glyph_t) const;

  void calc_ruler_rect (RECT &) const;
  void calc_ruler_box (const RECT &, RECT &) const;
  void paint_ruler (HDC, const RECT &, int, int, int) const;
  void paint_ruler (HDC) const;
  void paint_ruler_box (HDC, const RECT &) const;
  void erase_ruler (HDC, const RECT &r) const;
  void update_ruler ();

  void erase_cursor_line (HDC) const;
  void paint_cursor_line (HDC, int) const;
  void point2window_pos (point_t, POINT &) const;
};

inline int
Window::flags () const
{
  int f = w_flags | (w_default_flags & w_flags_mask);
  if (w_bufp)
    f = (f & w_bufp->b_wflags_mask) | w_bufp->b_wflags;
  return f;
}

inline void
Window::set_flags (int f)
{
  w_flags |= f;
  w_flags_mask &= ~f;
}

inline void
Window::clr_flags (int f)
{
  w_flags &= ~f;
  w_flags_mask &= ~f;
}

inline void
Window::use_default_flags (int f)
{
  w_flags &= ~f;
  w_flags_mask |= f;
}

inline int
Window::minibuffer_window_p () const
{
  return !w_next;
}

inline void
Window::paint_background (HDC hdc) const
{
  paint_background (hdc, 0, 0, w_rect.right, w_rect.bottom);
}

inline void
Window::paint_window (HDC hdc) const
{
  paint_region (hdc, 0, w_ch_max.cy);
}

inline COLORREF
Window::glyph_forecolor (glyph_t c) const
{
  u_int y = c & ((GLYPH_TEXTPROP_NCOLORS - 1) << GLYPH_TEXTPROP_FG_SHIFT_BITS);
  return (c & GLYPH_TEXTPROP_FG_BIT
          ? w_textprop_forecolor[(c & ((GLYPH_TEXTPROP_NCOLORS - 1)
                                       << GLYPH_TEXTPROP_FG_SHIFT_BITS))
                                 >> GLYPH_TEXTPROP_FG_SHIFT_BITS]
          : w_colors[forecolor_indexes[(c & GLYPH_FOREGROUND_MASK)
                                       >> GLYPH_COLOR_SHIFT_BITS]]);
}

inline COLORREF
Window::glyph_backcolor (glyph_t c) const
{
  u_int y = c & ((GLYPH_TEXTPROP_NCOLORS - 1) << GLYPH_TEXTPROP_BG_SHIFT_BITS);
  if ((c & GLYPH_BACKGROUND_MASK) || !y)
    {
      if (c & GLYPH_TEXTPROP_FG_BIT)
        c &= ~((GLYPH_TEXTPROP_NCOLORS - 1) << GLYPH_TEXTPROP_FG_SHIFT_BITS);
      return w_colors[backcolor_indexes[(c & GLYPH_FOREGROUND_MASK)
                                        >> GLYPH_COLOR_SHIFT_BITS]];
    }
  return w_textprop_backcolor[y >> GLYPH_TEXTPROP_BG_SHIFT_BITS];
}

struct WindowConfiguration
{
  struct Data
    {
      Window *wp;
      Buffer *bufp;
      point_t point;
      point_t disp;
      point_t mark;
      point_t selection_point;
      point_t selection_marker;
      Buffer::selection_type selection_type;
      Buffer::selection_type reverse_temp;
      Region reverse_region;
      long top_column;
      int flags_mask;
      int flags;
      RECT order;
      RECT rect;
    };
  static WindowConfiguration *wc_chain;
  WindowConfiguration *wc_prev;
  Window *wc_selected;
  int wc_nwindows;
  SIZE wc_size;
  Data *wc_data;
  WindowConfiguration ();
  ~WindowConfiguration ();
};

void refresh_screen (int);
void pending_refresh_screen ();

#endif
