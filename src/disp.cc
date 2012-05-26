#include "stdafx.h"
#include "ed.h"
#include "binfo.h"
#include "syntaxinfo.h"
#define DEFINE_LUCIDA_OFFSET_TABLE
#include "lucida-width.h"
#include "jisx0212-hash.h"
#include "mainframe.h"
#include "regex.h"

class color_caret
{
public:
  enum {RGB_MAX = 256};
private:
  HBITMAP cc_hbm;
  struct
    {
      BITMAPINFOHEADER b;
      RGBQUAD rgb[RGB_MAX];
    } cc_bi;
public:
  void load_colors ();
  color_caret ();
  ~color_caret ();
  void create (HWND, HBITMAP, int, int, COLORREF);
  void destroy ();
};

void
color_caret::load_colors ()
{
  memset (&cc_bi.b, 0, sizeof cc_bi.b);
  cc_bi.b.biSize = sizeof cc_bi.b;
  cc_bi.b.biPlanes = 1;

  PALETTEENTRY pe[RGB_MAX];
  HDC hdc = GetDC (0);
  cc_bi.b.biBitCount = GetDeviceCaps (hdc, BITSPIXEL) * GetDeviceCaps (hdc, PLANES);
  int n = GetSystemPaletteEntries (hdc, 0, RGB_MAX, pe);
  ReleaseDC (0, hdc);

  for (int i = 0; i < n; i++)
    {
      cc_bi.rgb[i].rgbRed = pe[i].peRed;
      cc_bi.rgb[i].rgbGreen = pe[i].peGreen;
      cc_bi.rgb[i].rgbBlue = pe[i].peBlue;
      cc_bi.rgb[i].rgbReserved = 0;
    }
  cc_bi.b.biClrUsed = n;
}

color_caret::color_caret ()
     : cc_hbm (0)
{
  load_colors ();
}

color_caret::~color_caret ()
{
  if (cc_hbm)
    DeleteObject (cc_hbm);
}

void
color_caret::destroy ()
{
  DestroyCaret ();
  if (cc_hbm)
    {
      DeleteObject (cc_hbm);
      cc_hbm = 0;
    }
}

void
color_caret::create (HWND hwnd, HBITMAP hbm, int w, int h, COLORREF cc)
{
  HBITMAP old_hbm = cc_hbm;
  cc_hbm = 0;
  if (!hbm)
    {
      cc_bi.b.biWidth = w;
      cc_bi.b.biHeight = h;
      HDC hdc = GetDC (0);
      HDC hdcmem = CreateCompatibleDC (hdc);
      cc_hbm = CreateDIBitmap (hdc, &cc_bi.b, 0, 0,
                               (BITMAPINFO *)&cc_bi, DIB_RGB_COLORS);
      HGDIOBJ obm = SelectObject (hdcmem, cc_hbm);
      fill_rect (hdcmem, 0, 0, w, h, cc);
      SelectObject (hdcmem, obm);
      DeleteDC (hdcmem);
      ReleaseDC (0, hdc);
      hbm = cc_hbm;
    }
  CreateCaret (hwnd, hbm, w, h);
  if (old_hbm)
    DeleteObject (old_hbm);
}

static color_caret xcaret;

void
reload_caret_colors ()
{
  xcaret.load_colors ();
}

int
Buffer::next_char (Point &point) const
{
  Chunk *cp = point.p_chunk;
  if (point.p_offset == cp->c_used)
    return 0;
  point.p_point++;
  if (++point.p_offset == cp->c_used)
    {
      if (!cp->c_next)
        return 1;
      point.p_chunk = cp->c_next;
      point.p_offset = 0;
    }
  return 1;
}

#define CARET_SHAPE_BLOCK 0
#define CARET_SHAPE_THIN 1
#define CARET_SHAPE_HALF 2
#define CARET_SHAPE_UNDERLINE 3

static void
calc_caret_shape (SIZE &size, int ovwrt, int dbcp, int selection)
{
  if (selection)
    {
      size.cx = 2 * sysdep.border.cx;
      size.cy = app.text_font.size ().cy;
    }
  else
    {
      lisp sym;
      if (ovwrt)
        sym = Voverwrite_caret_shape;
      else
        sym = Vnormal_caret_shape;

      long shape;
      safe_fixnum_value (xsymbol_value (sym), &shape);
      switch (shape)
        {
        case CARET_SHAPE_THIN:
          size.cx = 2 * sysdep.border.cx;
          size.cy = app.text_font.size ().cy;
          break;

        default:
        case CARET_SHAPE_BLOCK:
          size.cy = app.text_font.size ().cy;
          goto width;

        case CARET_SHAPE_HALF:
          size.cy = app.text_font.size ().cy / 2;
          goto width;

        case CARET_SHAPE_UNDERLINE:
          size.cy = 2 * sysdep.border.cy;
          goto width;

        width:
          size.cx = app.text_font.cell ().cx;
          if (dbcp)
            size.cx *= 2;
          break;
        }
    }
}

void
Window::caret_size (SIZE &size) const
{
  calc_caret_shape (size,
                    symbol_value (Voverwrite_mode, w_bufp) != Qnil,
                    (w_point.p_offset != w_point.p_chunk->c_used
                     && w_point.ch () >= 256
                     && char_width (w_point.ch ()) == 2),
                    ((w_selection_type != Buffer::SELECTION_VOID
                      && (w_selection_point == NO_MARK_SET
                          || ((w_selection_point <= w_selection_marker)
                              ? (w_point.p_point >= w_selection_point
                                 && w_point.p_point < w_selection_marker)
                              : (w_point.p_point >= w_selection_marker
                                 && w_point.p_point < w_selection_point))))
                     || app.f_in_drop));
}

void
Window::hide_caret () const
{
  if (app.active_frame.has_caret == w_hwnd && app.active_frame.caret_on)
    {
      HideCaret (w_hwnd);
      app.active_frame.caret_on = 0;
    }
}

void
Window::delete_caret ()
{
  if (app.active_frame.has_caret)
    {
      xcaret.destroy ();
      app.active_frame.caret_on = 0;
      app.active_frame.has_caret = 0;
    }
}

void
Window::update_last_caret ()
{
  if (app.active_frame.windows)
    {
      if (selected_window ())
        selected_window ()->update_caret ();
      Window *mini = Window::minibuffer_window ();
      if (mini && mini != selected_window ())
        mini->update_caret ();
    }
}

void
Window::update_caret (HWND hwnd, int x, int y, int w, int h, COLORREF cc)
{
  int gray_caret = app.f_in_drop;
  if (!app.ime_composition
      && app.last_blink_caret != (xsymbol_value (Vblink_caret) != Qnil))
    {
      app.last_blink_caret = xsymbol_value (Vblink_caret) != Qnil;
      if (app.last_blink_caret)
        restore_caret_blink_time ();
      else
        set_caret_blink_time ();
    }
  if (!app.active_frame.has_caret)
    {
      app.active_frame.has_caret = hwnd;
      xcaret.create (hwnd, gray_caret ? HBITMAP (1) : 0, w, h, cc);
      ShowCaret (hwnd);
    }
  else if (app.active_frame.has_caret != hwnd
           || w != app.active_frame.caret_size.cx
           || h != app.active_frame.caret_size.cy
           || cc != app.active_frame.last_caret_color
           || app.active_frame.gray_caret != gray_caret)
    {
      app.active_frame.has_caret = hwnd;
      xcaret.destroy ();
      xcaret.create (hwnd, gray_caret ? HBITMAP (1) : 0, w, h, cc);
      ShowCaret (hwnd);
    }
  else if (!app.active_frame.caret_on)
    ShowCaret (hwnd);
  app.active_frame.has_caret_last = hwnd;
  app.active_frame.last_caret_color = cc;
  app.active_frame.caret_on = 1;
  app.active_frame.caret_size.cx = w;
  app.active_frame.caret_size.cy = h;
  app.active_frame.caret_pos.x = x;
  app.active_frame.caret_pos.y = y;
  app.active_frame.gray_caret = gray_caret;
  SetCaretPos (app.active_frame.caret_pos.x,
               app.active_frame.caret_pos.y + app.text_font.size ().cy - h);
  set_ime_caret ();
}

void
Window::update_caret () const
{
  int show, prompt = 0;

  if (app.f_in_drop)
    show = w_bufp && app.drop_window == this;
  else if (!app.active_frame.has_focus)
    show = 0;
  else if (stringp (xsymbol_value (Vminibuffer_message))
           && xsymbol_value (Vminibuffer_prompt) != Qnil
           && app.minibuffer_prompt_column >= 0)
    {
      show = minibuffer_window_p ();
      prompt = 1;
    }
  else
    show = w_bufp && selected_window () == this;

  if (!show)
    {
      if (app.active_frame.has_caret == w_hwnd)
        delete_caret ();
    }
  else
    {
      COLORREF cc = (app.ime_open_mode == kbd_queue::IME_MODE_ON
                     ? w_colors[WCOLOR_IMECARET]
                     : w_colors[WCOLOR_CARET]);
      COLORREF bg = w_colors[WCOLOR_BACK];
      SIZE sz;
      int x, y;
      if (prompt)
        {
          calc_caret_shape (sz, 0, 0, 0);
          x = app.minibuffer_prompt_column - 1;
          y = 0;
        }
      else
        {
          caret_size (sz);
          x = caret_column ();
          y = caret_line ();

          if (w_glyphs.g_rep && y >= 0 && y < w_ch_max.cy)
            {
              int x1 = x + 1;
              const glyph_data *gd = w_glyphs.g_rep->gr_oglyph[y];
              if (x1 < gd->gd_len)
                {
                  glyph_t gc = gd->gd_cc[x1];
                  bg = glyph_backcolor (gc);
                  if (cc == bg)
                    {
                      COLORREF fg = glyph_forecolor (gc);
                      bg = RGB ((GetRValue (fg) + GetRValue (bg)) / 2,
                                (GetGValue (fg) + GetGValue (bg)) / 2,
                                (GetBValue (fg) + GetBValue (bg)) / 2);
                    }
                }
            }
        }

      cc = (bg ^ cc) & 0xffffff;

      HDC hdc = GetDC (w_hwnd);
      cc = GetNearestColor (hdc, cc);
      ReleaseDC (w_hwnd, hdc);

      update_caret (w_hwnd, caret_xpixel (x), caret_ypixel (y),
                    sz.cx, sz.cy, cc);
    }
}

class paint_chars_ctx
{
  int p_x, p_y;
  RECT p_r;
  int p_right;
  const int p_cellw;
public:
  paint_chars_ctx (int x, int y, const RECT &r, int w)
       : p_x (x), p_y (y), p_cellw (w * app.text_font.cell ().cx)
    {
      p_r.left = r.left;
      p_r.top = r.top;
      p_r.bottom = r.bottom;
      p_right = r.right;
    }
  void paint (HDC hdc, ucs2_t wc, int flags)
    {
      p_r.right = min (int (p_r.left + p_cellw), p_right);
      ExtTextOutW (hdc, p_x, p_y, flags, &p_r, &wc, 1, 0);
      p_r.left = p_r.right;
      p_x += p_cellw;
    }
  void paint_lucida (HDC, ucs2_t, int);
};

void
paint_chars_ctx::paint_lucida (HDC hdc, ucs2_t wc, int flags)
{
  const FontObject &f = app.text_font.font (FONT_ASCII);
  int o = (LUCIDA_OFFSET (wc - UNICODE_SMLCDM_MIN) * f.size ().cy / LUCIDA_BASE_HEIGHT
           + app.text_font.cell ().cx / 2);
  p_r.right = min (int (p_r.left + p_cellw), p_right);
  ExtTextOutW (hdc, p_x + o, p_y, flags, &p_r, &wc, 1, 0);
  p_r.left = p_r.right;
  p_x += p_cellw;
}

static inline void
paint_ascii_chars (HDC hdc, int x, int y, int flags, const RECT &r,
                   const char *string, int len, const INT *padding)
{
  const FontObject &f = app.text_font.font (FONT_ASCII);
  ExtTextOut (hdc, x + f.offset ().x, y + f.offset ().y, flags,
              &r, string, len, f.need_pad_p () ? padding : 0);
}

static inline void
paint_jp_chars (HDC hdc, int x, int y, int flags, const RECT &r,
                const char *string, int len, const INT *padding)
{
  const FontObject &f = app.text_font.font (FONT_JP);
  HGDIOBJ of = SelectObject (hdc, f);
  ExtTextOut (hdc, x + f.offset ().x, y + f.offset ().y, flags,
              &r, string, len, f.need_pad_p () ? padding : 0);
  SelectObject (hdc, of);
}

static inline void
paint_full_width_chars (HDC hdc, int x, int y, int flags, const RECT &r,
                        const char *string, int len, const FontObject &f)
{
  HGDIOBJ of = SelectObject (hdc, f);
  paint_chars_ctx ctx (x + f.offset ().x, y + f.offset ().y, r, 2);
  for (const u_char *s = (const u_char *)string, *const se = s + len; s < se; s += 2)
    ctx.paint (hdc, i2w ((s[0] << 8) | s[1]), flags);
  SelectObject (hdc, of);
}

static inline void
paint_half_width_chars (HDC hdc, int x, int y, int flags, const RECT &r,
                        const char *string, int len, const INT *padding,
                        int c, const FontObject &f)
{
  HGDIOBJ of = SelectObject (hdc, f);
  paint_chars_ctx ctx (x + f.offset ().x, y + f.offset ().y, r, 1);
  for (const u_char *s = (const u_char *)string, *const se = s + len; s < se; s++)
    ctx.paint (hdc, i2w (c + *s), flags);
  SelectObject (hdc, of);
}

static inline void
paint_jisx0212_half_width_chars (HDC hdc, int x, int y, int flags, const RECT &r,
                                 const char *string, int len, const INT *padding)
{
  const FontObject &f = app.text_font.font (FONT_JP);
  HGDIOBJ of = SelectObject (hdc, f);
  paint_chars_ctx ctx (x + f.offset ().x, y + f.offset ().y, r, 1);
  for (const u_char *s = (const u_char *)string, *const se = s + len; s < se; s++)
    ctx.paint (hdc, i2w (jisx0212_half_width_table[*s]), flags);
  SelectObject (hdc, of);
}

static inline void
paint_chars_lucida (HDC hdc, int x, int y, int flags, const RECT &r,
                    const char *string, int len, const INT *padding, int c)
{
  static LOGFONT lf = {0,0,0,0,0,0,0,0,0,0,0,0,0,LUCIDA_FACE_NAME};
  lf.lfHeight = app.text_font.font (FONT_ASCII).size ().cy;
  HGDIOBJ of = SelectObject (hdc, CreateFontIndirect (&lf));
  paint_chars_ctx ctx (x, y, r, 1);
  for (const u_char *s = (const u_char *)string, *const se = s + len; s < se; s++)
    ctx.paint_lucida (hdc, i2w (c + *s), flags);
  DeleteObject (SelectObject (hdc, of));
}

static void
paint_chars (HDC hdc, int x, int y, int flags, const RECT &r,
             glyph_t charset, const char *string, int len, const INT *padding)
{
#define PAINT_FULL_WIDTH_CHARS(FONT) \
  paint_full_width_chars (hdc, x, y, flags, r, string, len, app.text_font.font (FONT))
#define PAINT_HALF_WIDTH_CHARS(OFFSET, FONT) \
  paint_half_width_chars (hdc, x, y, flags, r, string, len, padding, \
                          OFFSET, app.text_font.font (FONT))
#define PAINT_JISX0212_HALF_WIDTH_CHARS() \
  paint_jisx0212_half_width_chars (hdc, x, y, flags, r, string, len, padding)
#define PAINT_CHARS_LUCIDA(OFFSET) \
  paint_chars_lucida (hdc, x, y, flags, r, string, len, padding, OFFSET)

  switch (charset)
    {
    default:
    case GLYPH_CHARSET_USASCII:
      paint_ascii_chars (hdc, x, y, flags, r, string, len, padding);
      break;

    case GLYPH_CHARSET_JISX0201_KANA:
    case GLYPH_CHARSET_JISX0208:
      paint_jp_chars (hdc, x, y, flags, r, string, len, padding);
      break;

    case GLYPH_CHARSET_JISX0212:
      PAINT_FULL_WIDTH_CHARS (FONT_JP);
      break;

    case GLYPH_CHARSET_GB2312:
      PAINT_FULL_WIDTH_CHARS (FONT_CN_SIMPLIFIED);
      break;

    case GLYPH_CHARSET_BIG5:
      PAINT_FULL_WIDTH_CHARS (FONT_CN_TRADITIONAL);
      break;

    case GLYPH_CHARSET_KSC5601:
      PAINT_FULL_WIDTH_CHARS (FONT_HANGUL);
      break;

    case GLYPH_CHARSET_JISX0212_HALF:
      PAINT_JISX0212_HALF_WIDTH_CHARS ();
      break;

    case GLYPH_CHARSET_ISO8859_1_2:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_1 << 7, FONT_LATIN);
      break;

    case GLYPH_CHARSET_ISO8859_3_4:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_3 << 7, FONT_LATIN);
      break;

    case GLYPH_CHARSET_ISO8859_5:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_5 << 7, FONT_CYRILLIC);
      break;

    case GLYPH_CHARSET_ISO8859_7:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_7 << 7, FONT_GREEK);
      break;

    case GLYPH_CHARSET_ISO8859_9_10:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_9 << 7, FONT_LATIN);
      break;

    case GLYPH_CHARSET_ISO8859_13:
      PAINT_HALF_WIDTH_CHARS (ccs_iso8859_13 << 7, FONT_LATIN);
      break;

    case GLYPH_CHARSET_GEORGIAN:
      PAINT_HALF_WIDTH_CHARS (CCS_GEORGIAN_MIN, FONT_GEORGIAN);
      break;

    case GLYPH_CHARSET_IPA:
      PAINT_HALF_WIDTH_CHARS (CCS_IPA_MIN, FONT_JP); // XXX
      break;

    case GLYPH_CHARSET_SMLCDM:
      PAINT_CHARS_LUCIDA (CCS_SMLCDM_MIN);
      break;

#ifdef CCS_ULATIN_MIN
    case GLYPH_CHARSET_ULATIN1:
      PAINT_HALF_WIDTH_CHARS (CCS_ULATIN_MIN, FONT_LATIN);
      break;

    case GLYPH_CHARSET_ULATIN2:
      PAINT_HALF_WIDTH_CHARS (CCS_ULATIN_MIN + 256, FONT_LATIN);
      break;
#endif
#ifdef CCS_UJP_MIN
    case GLYPH_CHARSET_UJP:
      PAINT_FULL_WIDTH_CHARS (FONT_JP);
      break;

    case GLYPH_CHARSET_UJP_H1:
      PAINT_HALF_WIDTH_CHARS (CCS_UJP_MIN, FONT_JP);
      break;

    case GLYPH_CHARSET_UJP_H2:
      PAINT_HALF_WIDTH_CHARS (CCS_UJP_MIN + 256, FONT_JP);
      break;

    case GLYPH_CHARSET_UJP_H3:
      PAINT_HALF_WIDTH_CHARS (CCS_UJP_MIN + 512, FONT_JP);
      break;
#endif
    }
}

void
Window::paint_glyphs (HDC hdc, HDC hdcmem, const glyph_t *gstart, const glyph_t *g,
                      const glyph_t *ge, char *buf, const INT *padding,
                      int x, int y, int yoffset) const
{
  RECT r;
  r.top = y + yoffset;
  r.bottom = y + app.text_font.cell ().cy;
  r.right = x;
  glyph_t gsum = 0;
  const glyph_t *gfrom = g;

  while (g < ge)
    {
      const glyph_t *g0 = g;
      char *be = buf;
      glyph_t c = *g++;
      gsum |= c;
      *be++ = char (c);
      c &= GLYPH_COLOR_MASK | GLYPH_CHARSET_MASK;
      while (g < ge && (*g & (GLYPH_COLOR_MASK | GLYPH_CHARSET_MASK)) == c)
        {
          gsum |= *g;
          *be++ = char (*g++);
        }

      r.left = r.right;
      r.right += (be - buf) * app.text_font.cell ().cx;
      if (r.right > w_clsize.cx)
        {
          r.right = w_clsize.cx;
          if (r.left > r.right)
            break;
        }

      char *b = buf;
      if (!(c & GLYPH_CHARSET_MASK))
        {
          for (; b < be && *b == ' '; b++)
            ;
          g0 += b - buf;
        }
      const glyph_t *g1;
      for (g1 = g;
           g1 > g0 && (g1[-1] & ~GLYPH_COLOR_MASK) == ' ';
           g1--)
        ;
      be -= g - g1;

      COLORREF ofg = SetTextColor (hdc, glyph_forecolor (c));
      COLORREF obg = SetBkColor (hdc, glyph_backcolor (c));

      if (c & GLYPH_BITMAP_BIT)
        {
          int x = r.left + (b - buf) * app.text_font.cell ().cx;
          for (; b < be; b++, x += app.text_font.cell ().cx)
            {
              int w = w_clsize.cx - x;
              if (w <= 0)
                break;
              if (w > app.text_font.cell ().cx)
                w = app.text_font.cell ().cx;
              BitBlt (hdc, x, r.top, w, app.text_font.cell ().cy,
                      hdcmem, app.text_font.cell ().cx * (*b & 0xff), yoffset, SRCCOPY);
            }
        }
      else
        paint_chars (hdc, r.left + (b - buf) * app.text_font.cell ().cx, y,
                     ETO_OPAQUE | ETO_CLIPPED, r, GLYPH_CHARSET (c), b, be - b, padding);

      SetTextColor (hdc, ofg);
      SetBkColor (hdc, obg);
    }

  if (gsum & GLYPH_BOLD)
    {
      int omode = SetBkMode (hdc, TRANSPARENT);
      g = gfrom;
      if (g > gstart && g[-1] & GLYPH_BOLD)
        {
          g--;
          if (glyph_trail_p (*g))
            g--;
        }

      while (1)
        {
          for (; g < ge && !(*g & GLYPH_BOLD); g++)
            ;
          if (g == ge)
            break;
          char *be = buf;
          const glyph_t *g0 = g;
          glyph_t c0 = *g++;
          *be++ = char (c0);
          glyph_t c = c0 & (GLYPH_FORE_COLOR_MASK | GLYPH_BITMAP_BIT
                            | GLYPH_CHARSET_MASK | GLYPH_BOLD);
          for (; g < ge && (*g & (GLYPH_FORE_COLOR_MASK | GLYPH_BITMAP_BIT
                                  | GLYPH_CHARSET_MASK | GLYPH_BOLD)) == c; g++)
            *be++ = char (*g);

          COLORREF ofg = SetTextColor (hdc, glyph_forecolor (c0));

          r.left = x + (g0 - gfrom) * app.text_font.cell ().cx + 1;
          r.right = x + (g - gfrom) * app.text_font.cell ().cx + 1;
          if (r.right > w_clsize.cx)
            {
              r.right = w_clsize.cx;
              if (r.left > r.right)
                break;
            }

          if (c & GLYPH_BITMAP_BIT)
            {
              COLORREF obg = SetBkColor (hdc, glyph_backcolor (c0));
              char *b = buf;
              for (int x = r.left; b < be; b++, x += app.text_font.cell ().cx)
                if ((*b & 0xff) == FontSet::backsl)
                  {
                    int w = w_clsize.cx - x;
                    if (w <= 0)
                      break;
                    if (w > app.text_font.cell ().cx)
                      w = app.text_font.cell ().cx;
                    BitBlt (hdc, x, r.top, w, app.text_font.cell ().cy,
                            hdcmem, app.text_font.cell ().cx * FontSet::bold_backsl, yoffset, SRCCOPY);
                  }
              SetBkColor (hdc, obg);
            }
          else
            paint_chars (hdc, r.left, y, ETO_CLIPPED, r, GLYPH_CHARSET (c),
                         buf, be - buf, padding);

          SetTextColor (hdc, ofg);
        }
      SetBkMode (hdc, omode);
    }

  if (gsum & (!yoffset ? GLYPH_UNDERLINE | GLYPH_STRIKEOUT : GLYPH_UNDERLINE))
    {
      g = gfrom;
      while (1)
        {
          for (; g < ge && !(*g & (GLYPH_UNDERLINE | GLYPH_STRIKEOUT)); g++)
            ;
          if (g == ge)
            break;
          const glyph_t *g0 = g;
          glyph_t c = *g++ & (GLYPH_FORE_COLOR_MASK | GLYPH_UNDERLINE | GLYPH_STRIKEOUT);
          for (; g < ge && (*g & (GLYPH_FORE_COLOR_MASK | GLYPH_UNDERLINE | GLYPH_STRIKEOUT)) == c; g++)
            ;

          COLORREF obg = SetBkColor (hdc, glyph_forecolor (c));
          r.left = x + (g0 - gfrom) * app.text_font.cell ().cx;
          r.right = x + (g - gfrom) * app.text_font.cell ().cx;
          if (r.right > w_clsize.cx)
            {
              r.right = w_clsize.cx;
              if (r.left > r.right)
                break;
            }

          if (c & GLYPH_UNDERLINE)
            {
              r.bottom = y + app.text_font.size ().cy;
              r.top = r.bottom - app.text_font.line_width ();
              ExtTextOut (hdc, r.left, r.top, ETO_OPAQUE, &r, "", 0, 0);
            }
          if (!yoffset && c & GLYPH_STRIKEOUT)
            {
              r.top = y + app.text_font.size ().cy / 2;
              r.bottom = r.top + app.text_font.line_width ();
              ExtTextOut (hdc, r.left, r.top, ETO_OPAQUE, &r, "", 0, 0);
            }

          SetBkColor (hdc, obg);
        }
    }
}

void
Window::paint_line (HDC hdc, HDC hdcmem, glyph_data *ogd, const glyph_data *ngd,
                    char *buf, int y, const INT *padding) const
{
  const glyph_t *n = ngd->gd_cc, *ne = n + ngd->gd_len;
  glyph_t *o = ogd->gd_cc, *oe = o + ogd->gd_len;

  for (; n < ne && o < oe && *n == *o; n++, o++)
    ;
  if (n == ne && o == oe)
    return;
  if (glyph_trail_p (*n))
    n--, o--;
  if (o > ogd->gd_cc && o[-1] & GLYPH_BOLD)
    {
      n--, o--;
      if (glyph_trail_p (*n))
        n--, o--;
    }

  const glyph_t *nfd = n;
  glyph_t *ofd = o;

  for (n = ne, o = oe; n > nfd && o > ofd && n[-1] == o[-1]; n--, o--)
    ;
  if (n < ne && glyph_trail_p (*n))
    n++, o++;
  if (o > ogd->gd_cc && o[-1] & GLYPH_BOLD)
    {
      if (o < oe)
        {
          n++, o++;
          if (n < ne && glyph_trail_p (*n))
            n++, o++;
        }
      else
        PatBlt (hdc, ((ogd->gd_len - 1) * app.text_font.cell ().cx + app.text_font.cell ().cx / 2), y,
                app.text_font.cell ().cx, app.text_font.cell ().cy, PATCOPY);
    }

  const glyph_t *nls = n;
  glyph_t *ols = o;

  int dl = (nls - nfd) - (ols - ofd);

  if (!dl)
    {
      paint_glyphs (hdc, hdcmem, ngd->gd_cc, nfd, nls, buf, padding,
                    ((nfd - ngd->gd_cc - 1) * app.text_font.cell ().cx
                     + app.text_font.cell ().cx / 2),
                    y, 0);
      for (o = ofd, n = nfd; n < nls;)
        *o++ = *n++;
    }
  else
    {
      if (ogd->gd_len - (ols - ogd->gd_cc) <= 3)
        {
          paint_glyphs (hdc, hdcmem, ngd->gd_cc, nfd, ne, buf, padding,
                        ((nfd - ngd->gd_cc - 1) * app.text_font.cell ().cx
                         + app.text_font.cell ().cx / 2),
                        y, 0);
          if (dl < 0 && ogd->gd_len > ngd->gd_len)
            PatBlt (hdc,
                    ((ngd->gd_len - 1) * app.text_font.cell ().cx
                     + app.text_font.cell ().cx / 2),
                    y,
                    (ogd->gd_len - ngd->gd_len) * app.text_font.cell ().cx,
                    app.text_font.cell ().cy,
                    PATCOPY);
        }
      else
        {
          RECT r;
          r.top = y;
          r.bottom = y + app.text_font.cell ().cy;
          r.left = ((ols - ogd->gd_cc - 1) * app.text_font.cell ().cx
                    + app.text_font.cell ().cx / 2);
          r.right = ((ogd->gd_len - 1) * app.text_font.cell ().cx
                     + app.text_font.cell ().cx / 2);
          int dx = dl * app.text_font.cell ().cx;
          if (r.right + dx > w_clsize.cx)
            {
              r.right = w_clsize.cx - dx;
              r.right = max (r.left, r.right);
            }
          ScrollWindow (w_hwnd, dx, 0, &r, 0);
          if (dl < 0)
            {
              if (r.right > w_clsize.cx)
                {
                  int x = ((w_clsize.cx + app.text_font.cell ().cx / 2)
                           / app.text_font.cell ().cx + dl);
                  if (x >= 0 && x < ngd->gd_len)
                    {
                      const glyph_t *g = ngd->gd_cc + x;
                      int l = 1;
                      if (x && glyph_trail_p (*g))
                        {
                          g--;
                          x--;
                          l = 2;
                        }
                      else if (glyph_lead_p (*g))
                        l = 2;
                      paint_glyphs (hdc, hdcmem, ngd->gd_cc, g, g + l, buf, padding,
                                    (x - 1) * app.text_font.cell ().cx + app.text_font.cell ().cx / 2,
                                    y, 0);
                    }
                }
              r.left = ((ngd->gd_len - 1) * app.text_font.cell ().cx
                        + app.text_font.cell ().cx / 2);
              ValidateRect (w_hwnd, &r);
              PatBlt (hdc, r.left, r.top,
                      r.right - r.left, app.text_font.cell ().cy,
                      PATCOPY);
            }
          else
            {
              r.right = ((nls - ngd->gd_cc - 1) * app.text_font.cell ().cx
                         + app.text_font.cell ().cx / 2);
              ValidateRect (w_hwnd, &r);
            }
          paint_glyphs (hdc, hdcmem, ngd->gd_cc, nfd, nls, buf, padding,
                        ((nfd - ngd->gd_cc - 1) * app.text_font.cell ().cx
                         + app.text_font.cell ().cx / 2),
                        y, 0);
        }
      for (o = ofd, n = nfd; n < ne;)
        *o++ = *n++;
      *o = 0;
      ogd->gd_len = ngd->gd_len;
    }
}

void
Window::erase_cursor_line (HDC hdc) const
{
  if (w_cursor_line.ypixel < 0 || !w_glyphs.g_rep)
    return;

  int y = w_cursor_line.ypixel / app.text_font.cell ().cy;
  if (y >= 0 && y < w_ch_max.cy)
    {
      HDC xhdc = hdc;
      if (!hdc)
        {
          hide_caret ();
          hdc = GetDC (w_hwnd);
        }

      int x1 = (w_cursor_line.x1 - app.text_font.cell ().cx / 2) / app.text_font.cell ().cx + 1;
      int x2pixel = w_cursor_line.x2;
      if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
        {
          int w = w_cursor_line.x1 + ((w_bufp->b_fold_columns - w_last_top_column)
                                      * app.text_font.cell ().cx);
          if (x2pixel < w)
            x2pixel = w;
        }
      int x2 = (x2pixel - app.text_font.cell ().cx / 2
                + app.text_font.cell ().cx - 1) / app.text_font.cell ().cx + 1;

      const glyph_data *gd = w_glyphs.g_rep->gr_oglyph[y];
      const glyph_t *g = gd->gd_cc + x1, *ge = gd->gd_cc + min (int (gd->gd_len), x2);
      int x = w_cursor_line.x1;
      HGDIOBJ of = SelectObject (hdc, app.text_font.font (FONT_ASCII));
      HDC hdcmem = CreateCompatibleDC (hdc);
      HGDIOBJ obm = SelectObject (hdcmem, app.text_font.hbm ());
      HGDIOBJ obr = SelectObject (hdc, CreateSolidBrush (w_colors[WCOLOR_BACK]));

      INT *padding;
      if (!app.text_font.need_pad_p ())
        padding = 0;
      else
        {
          padding = (INT *)alloca (sizeof *padding * w_ch_max.cx);
          for (int i = 0; i < w_ch_max.cx; i++)
            padding[i] = app.text_font.cell ().cx;
        }
      char *buf = (char *)alloca (w_ch_max.cx + 3);
      paint_glyphs (hdc, hdcmem, gd->gd_cc, g, ge, buf, padding, x,
                    (w_cursor_line.ypixel - app.text_font.cell ().cy + 1),
                    app.text_font.cell ().cy - 1);
      SelectObject (hdcmem, obm);
      DeleteDC (hdcmem);
      SelectObject (hdc, of);
      DeleteObject (SelectObject (hdc, obr));

      x += (ge - g) * app.text_font.cell ().cx;
      if (x < w_cursor_line.x2)
        draw_hline (hdc, x, w_cursor_line.x2,
                    w_cursor_line.ypixel, w_colors[WCOLOR_BACK]);
      if (hdc != xhdc)
        ReleaseDC (w_hwnd, hdc);
    }
  const_cast <Window *> (this)->w_cursor_line.ypixel = -1;
}

void
Window::paint_cursor_line (HDC hdc, int f) const
{
  int x1, x2, y;
  int paint = (w_last_flags & WF_CURSOR_LINE
               && (selected_window () == this
                   || (w_bufp && xsymbol_value (Vshow_cursor_line_always) != Qnil)));
  int erase;
  int inverse = symbol_value (Vinverse_cursor_line, w_bufp) != Qnil;

  if (paint)
    {
      x1 = ((w_last_flags & WF_LINE_NUMBER ? LINENUM_COLUMNS + 1 : 0)
            * app.text_font.cell ().cx + app.text_font.cell ().cx / 2);
      x2 = w_ch_max.cx * app.text_font.cell ().cx;
      if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
        {
          int w = x1 + ((w_bufp->b_fold_columns - w_last_top_column)
                        * app.text_font.cell ().cx);
          if (w < x2)
            x2 = w;
        }
      if (x2 > w_clsize.cx)
        x2 = w_clsize.cx;

      y = (w_linenum - w_last_top_linenum + 1) * app.text_font.cell ().cy - 1;

      if (y != w_cursor_line.ypixel
          || x1 != w_cursor_line.x1
          || x2 != w_cursor_line.x2)
        erase = 1;
      else if (!f)
        return;
      else
        erase = inverse;
    }
  else
    {
      if (w_cursor_line.ypixel < 0)
        return;
      erase = 1;
    }

  HDC xhdc = hdc;
  if (!hdc)
    {
      hide_caret ();
      hdc = GetDC (w_hwnd);
    }

  if (erase)
    erase_cursor_line (hdc);

  if (paint)
    {
      if (inverse)
        {
          HGDIOBJ open = SelectObject
            (hdc, CreatePen (PS_SOLID, 0, w_colors[WCOLOR_CURSOR] ^ w_colors[WCOLOR_BACK]));
          int omode = SetROP2 (hdc, R2_XORPEN);
          MoveToEx (hdc, x1, y, 0);
          LineTo (hdc, x2, y);
          SetROP2 (hdc, omode);
          DeleteObject (SelectObject (hdc, open));
        }
      else
        draw_hline (hdc, x1, x2, y, w_colors[WCOLOR_CURSOR]);

      const_cast <Window *> (this)->w_cursor_line.ypixel = y;
      const_cast <Window *> (this)->w_cursor_line.x1 = x1;
      const_cast <Window *> (this)->w_cursor_line.x2 = x2;
    }

  if (hdc != xhdc)
    ReleaseDC (w_hwnd, hdc);
}

#define MAX_KWDLEN 256

#define KWD_PROP  1
#define KWD_FILL  2
#define KWD_KWD2  4

static int
kwd_val (int xval, int f, int &revkwd)
{
  if (xval < 0)
    return f;

  if (xval & KWD_PROP)
    {
      f |= xval & (((GLYPH_TEXTPROP_NCOLORS - 1)
                    << GLYPH_TEXTPROP_FG_SHIFT_BITS)
                   | ((GLYPH_TEXTPROP_NCOLORS - 1)
                      << GLYPH_TEXTPROP_BG_SHIFT_BITS)
                   | GLYPH_BOLD | GLYPH_UNDERLINE | GLYPH_STRIKEOUT);
      if (f & ((GLYPH_TEXTPROP_NCOLORS - 1)
               << GLYPH_TEXTPROP_FG_SHIFT_BITS))
        f |= GLYPH_TEXTPROP_FG_BIT;
    }
  else
    {
      int x = xval & (15 << GLYPH_COLOR_SHIFT_BITS);
      if (xsymbol_value (Vinhibit_reverse_keywords) != Qnil
          && x >= GLYPH_KEYWORD1R && x <= GLYPH_KEYWORD3R)
        x += GLYPH_KEYWORD1 - GLYPH_KEYWORD1R;
      f |= x | (xval & (GLYPH_BOLD | GLYPH_UNDERLINE | GLYPH_STRIKEOUT));
    }
  if (xval & KWD_FILL && !revkwd)
    revkwd = f & ~GLYPH_HIDDEN;
  return f;
}

static inline int
kwd_val (lisp x, int f, int &revkwd)
{
  return kwd_val (xshort_int_value (x), f, revkwd);
}

int
Window::kwdmatch (lisp kwdhash, const Char *p, const Chunk *cp,
                  int &symlen, int &revkwd, int defalt,
                  int &long_kwd, int kwdf) const
{
  const syntax_table *tab = xsyntax_table (w_bufp->lsyntax_table);

  Char cc = *p;
  if (!ascii_char_p (cc))
    {
      symlen = 0;
      long_kwd = 0;
      return defalt;
    }

  int f;
  int prefix = 0;
  switch (xchar_syntax (tab, cc))
    {
    case SCsymbol_prefix:
      prefix = 1;
      f = 0;
      break;

    case SCword:
    case SCsymbol:
    case SCescape:
      f = 0;
      break;

    case SCtag_start:
      f = GLYPH_HIDDEN;
      break;

    default:
      symlen = 0;
      long_kwd = 0;
      return defalt;
    }

  if (long_kwd)
    {
      symlen = 0;
      return defalt;
    }

  if (!f && !prefix)
    {
      Char pc;
      if (p != cp->c_text)
        pc = p[-1];
      else
        {
          const Chunk *prev = cp->c_prev;
          pc = prev ? prev->c_text[prev->c_used - 1] : Char (-1);
        }
      if (ascii_char_p (pc)
          && (xchar_syntax (tab, pc) == SCword
              || xchar_syntax (tab, pc) == SCsymbol))
        {
          symlen = 0;
          long_kwd = 0;
          return defalt;
        }
    }

  p++;
  const Char *pe = cp->c_text + cp->c_used;

  Char buf[MAX_KWDLEN];
  int l = 1, sl = 1;
  int f_pound = tab->flags & SYNTAX_OPT_CPP && cc == '#';
  buf[0] = cc;
  while (1)
    {
      if (p == pe)
        {
          cp = cp->c_next;
          if (!cp)
            break;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
      cc = *p++;
      if (f_pound && (cc == ' ' || cc == '\t'))
        ;
      else
        {
          if (!ascii_char_p (cc)
              || (xchar_syntax (tab, cc) != SCword
                  && xchar_syntax (tab, cc) != SCsymbol))
            break;
          if (sl < numberof (buf))
            buf[sl] = cc;
          else
            {
              long_kwd = 1;
              break;
            }
          sl++;
          f_pound = 0;
        }
      l++;
    }

  symlen = l;
  if (sl < numberof (buf))
    {
      temporary_string t (buf, sl);
      lisp x = gethash (t.string (), kwdhash, Qnil);
      if (x != Qnil)
        {
          if (!short_int_p (x))
            {
              if (!(kwdf & syntax_state::KWD_OK))
                goto nomatch;
              return f;
            }
          int xval = xshort_int_value (x);
          if (!(kwdf & syntax_state::KWD_OK) && (xval < 0 || !(xval & KWD_KWD2)))
            goto nomatch;
          f &= ~KWD_KWD2;
          return kwd_val (x, f, revkwd);
        }
    }
nomatch:
  if (f & GLYPH_HIDDEN)
    symlen = 1;
  return defalt;
}

int
Window::kwdmatch (lisp kwdhash, const Point &point,
                  int &symlen, int &revkwd, int &long_kwd, int kwdf) const
{
  const syntax_table *tab = xsyntax_table (w_bufp->lsyntax_table);
  const Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  int tag = 0;
  Char cc = *p;
  int l = 0;
  int f;
  int sym = (ascii_char_p (cc)
             && (xchar_syntax (tab, cc) == SCword
                 || xchar_syntax (tab, cc) == SCsymbol));
  if (sym)
    {
      for (;; l++)
        {
          if (l == MAX_KWDLEN)
            {
              long_kwd = 1;
              symlen = 0;
              return 0;
            }
          if (p == cp->c_text)
            {
              if (!cp->c_prev)
                break;
              cp = cp->c_prev;
              p = cp->c_text + cp->c_used;
            }
          cc = *--p;
          if (ascii_char_p (cc))
            {
              switch (xchar_syntax (tab, cc))
                {
                case SCtag_start:
                  tag = 1;
                  l++;
                  goto end;

                case SCescape:
                case SCsymbol_prefix:
                  l++;
                  goto end;

                case SCword:
                case SCsymbol:
                  continue;
                }
            }
          if (++p == cp->c_text + cp->c_used)
            {
              cp = cp->c_next;
              p = cp->c_text;
            }
          break;
        }
    end:
      if (!l)
        return 0;
      f = kwdmatch (kwdhash, p, cp, symlen, revkwd, 0, long_kwd, kwdf);
    }
  else
    f = 0;

  if (tab->flags & SYNTAX_OPT_CPP && !f)
    {
      const Chunk *cq = cp;
      const Char *q = p;
      int qs = 0;
      int lq = 0;
      while (1)
        {
          if (q == cq->c_text)
            {
              if (!cq->c_prev)
                break;
              cq = cq->c_prev;
              q = cq->c_text + cq->c_used;
            }
          cc = *--q;
          lq++;
          if (cc == '#')
            {
              qs++;
              break;
            }
          if (cc != ' ' && cc != '\t')
            break;
          if (lq == MAX_KWDLEN)
            {
              long_kwd = 1;
              symlen = 0;
              return 0;
            }
          qs = 1;
        }
      if (qs == 2 && (f = kwdmatch (kwdhash, q, cq, symlen,
                                    revkwd, 0, long_kwd, kwdf)))
        l += lq;
    }
  if (!f && tag && l > 1)
    {
      if (++p == cp->c_text + cp->c_used)
        {
          cp = cp->c_next;
          p = cp->c_text;
        }
      l--;
      f = kwdmatch (kwdhash, p, cp, symlen, revkwd, 0, long_kwd, kwdf);
    }
  f &= ~GLYPH_HIDDEN;
  symlen -= l;
  return f;
}

static inline glyph_t *
glyph_dbchar (glyph_t *g, Char cc, int f, int flags)
{
  u_char c1, c2;

  switch (code_charset (cc))
    {
    default:
      if (flags & Window::WF_FULLSPC && cc == 0x8140U)
        {
          *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | GLYPH_LEAD | GLYPH_BM_FULLSPC1;
          *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | GLYPH_TRAIL | GLYPH_BM_FULLSPC2;
        }
      else
        {
          c1 = u_char (cc >> 8);
          c2 = u_char (cc & 0xff);
          if (!c2 || !SJISP (c1))
            goto undef_char;
          *g++ = f | c1 | GLYPH_LEAD | GLYPH_CHARSET_JISX0208;
          *g++ = f | c2 | GLYPH_TRAIL | GLYPH_CHARSET_JISX0208;
        }
      break;

    case ccs_jisx0212:
      f |= GLYPH_CHARSET_JISX0212;
      goto output;

    case ccs_gb2312:
      f |= GLYPH_CHARSET_GB2312;
      goto output;

    case ccs_big5:
      f |= GLYPH_CHARSET_BIG5;
      goto output;

    case ccs_ksc5601:
      f |= GLYPH_CHARSET_KSC5601;
      goto output;

#ifdef CCS_UJP_MIN
    case ccs_ujp:
      f |= GLYPH_CHARSET_UJP;
      goto output;
#endif

    output:
      if (i2w (cc) == ucs2_t (-1))
        goto undef_char;
      c1 = u_char (cc >> 8);
      c2 = u_char (cc & 0xff);
      *g++ = f | c1 | GLYPH_LEAD;
      *g++ = f | c2 | GLYPH_TRAIL;
      break;

    undef_char:
      *g++ = f | GLYPH_LEAD | GLYPH_BM_WBLANK1;
      *g++ = f | GLYPH_TRAIL | GLYPH_BM_WBLANK2;
      break;
    }
  return g;
}

static inline glyph_t *
glyph_sbchar (glyph_t *g, Char cc, int f, int flags)
{
  int ccs = code_charset (cc);
  switch (ccs)
    {
    case ccs_iso8859_1:
    case ccs_iso8859_2:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 255) | GLYPH_CHARSET_ISO8859_1_2;
      break;

    case ccs_iso8859_3:
    case ccs_iso8859_4:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 255) | GLYPH_CHARSET_ISO8859_3_4;
      break;

    case ccs_iso8859_5:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 127) | GLYPH_CHARSET_ISO8859_5;
      break;

    case ccs_iso8859_7:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 127) | GLYPH_CHARSET_ISO8859_7;
      break;

    case ccs_iso8859_9:
    case ccs_iso8859_10:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 255) | GLYPH_CHARSET_ISO8859_9_10;
      break;

    case ccs_iso8859_13:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 255) | GLYPH_CHARSET_ISO8859_13;
      break;

    case ccs_jisx0212:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = (f | GLYPH_CHARSET_JISX0212_HALF
              | jisx0212_half_width_hash[cc % JISX0212_HALF_WIDTH_HASH_SIZE]);
      break;

    case ccs_jisx0201_kana:
      if (SJISP (cc))
        goto bad_char;
      *g++ = f | GLYPH_CHARSET_JISX0201_KANA | cc;
      break;

    case ccs_georgian:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 127) | GLYPH_CHARSET_GEORGIAN;
      break;

    case ccs_ipa:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 127) | GLYPH_CHARSET_IPA;
      break;

    case ccs_smlcdm:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = f | (cc & 255) | GLYPH_CHARSET_SMLCDM;
      break;

    case ccs_usascii:
      if (app.text_font.use_backsl_p () && cc == '\\')
        *g++ = f | GLYPH_BM_BACKSL;
      else if (flags & Window::WF_HALFSPC && cc == ' ')
        *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | GLYPH_BM_HALFSPC;
      else
        *g++ = f | cc;
      break;

#ifdef CCS_ULATIN_MIN
    case ccs_ulatin:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = (f | (cc & 255) | (GLYPH_CHARSET_ULATIN1
                                + MAKE_GLYPH_CHARSET ((cc - CCS_ULATIN_MIN) >> 8)));
      break;
#endif
#ifdef CCS_UJP_MIN
    case ccs_ujp:
      if (i2w (cc) == ucs2_t (-1))
        goto bad_char;
      *g++ = (f | (cc & 255) | (GLYPH_CHARSET_UJP_H1
                                + MAKE_GLYPH_CHARSET ((cc - CCS_UJP_MIN) >> 8)));
      break;
#endif

    default:
    bad_char:
      *g++ = f | GLYPH_BM_BLANK;
      break;
    }
  return g;
}

static inline glyph_t *
glyph_bmchar (glyph_t *g, Char bm, lisp ch, int f, int n)
{
  ch = xsymbol_value (ch);
  if (ch == Qnil)
    for (int i = 0; i < n; i++)
      *g++ = f | ' ';
  else if (charp (ch) && char_width (xchar_code (ch)) == 1)
    for (int i = 0; i < n; i++)
      g = glyph_sbchar (g, xchar_code (ch), f, 0);
  else
    for (int i = 0; i < n; i++)
      *g++ = f | bm;
  return g;
}

class regexp_kwd
{
private:
  lisp rk_list;
  char rk_fastmap[256];
  int rk_can_fastmap;
  point_t rk_last_try;
  point_t rk_match_beg;
  point_t rk_match_end;
  int rk_val;
  int rk_vals[MAX_KWDLEN];
  int rk_use_vals;
  int rk_match;
  const Buffer *const rk_bufp;
  const syntax_table *const rk_tab;
  int rk_ctx_mask;

  lisp check_format (lisp);
  int sc2mask (int sc) {return 1 << ((sc >> GLYPH_KEYWORD_SHIFT_BITS) & 3);}
public:
  regexp_kwd::regexp_kwd (lisp, point_t, const Buffer *);
  int kwdmatch (const Point &, int, int &);
  int kwdmatch_begin (const Point &, int);
  int valid_p () const {return int (rk_list);}
  point_t match_beg () const {return rk_match_beg;}
  point_t match_end () const {return rk_match_end;}
  int value (int i) const {return rk_use_vals ? rk_vals[i] : rk_val;}
  int match_p () const {return rk_match;}
  void clear () {rk_match = 0;}
  int ctx_mask () const {return rk_ctx_mask;}
  int possible_match_p (Char c) const
    {return !rk_can_fastmap || rk_fastmap[c >= 0x100 ? c >> 8 : c];}
};

// (regexp color [context [begin [end]]])

lisp
regexp_kwd::check_format (lisp x)
{
  if (!consp (x))
    return 0;
  lisp regex = xcar (x);
  if (!regexpp (regex))
    return 0;

  // color
  if (!consp (x = xcdr (x)))
    return 0;
  lisp c = xcar (x);
  if (short_int_p (c))
    ;
  else
    {
      if (!consp (c))
        return 0;
      do
        {
          lisp a = xcar (c);
          if (!consp (a)
              || !short_int_p (xcar (a))
              || (xcdr (a) != Qnil && !short_int_p (xcdr (a))))
            return 0;
          int v = xshort_int_value (xcar (a));
          if (v < 0 || v >= MAX_REGS)
            return 0;
          c = xcdr (c);
        }
      while (consp (c));
    }

  int ctx;
  if (!consp (x = xcdr (x)))
    ctx = 1;
  else
    {
      // context
      if (!short_int_p (xcar (x)))
        return 0;
      ctx = xshort_int_value (xcar (x));

      if (consp (x = xcdr (x)))
        {
          // begin
          if (!short_int_p (xcar (x)))
            return 0;
          int v = xshort_int_value (xcar (x));
          if (v <= -MAX_REGS || v >= MAX_REGS)
            return 0;

          // end
          if (consp (x = xcdr (x)))
            {
              if (!short_int_p (xcar (x)))
                return 0;
              v = xshort_int_value (xcar (x));
              if (v <= -MAX_REGS || v >= MAX_REGS)
                return 0;
            }
        }
    }
  rk_ctx_mask |= ctx;
  return regex;
}

inline
regexp_kwd::regexp_kwd (lisp list, point_t point, const Buffer *bp)
     : rk_list (list), rk_last_try (point_t (-1)),
       rk_match_beg (point), rk_match_end (point),
       rk_match (0), rk_val (0), rk_use_vals (0),
       rk_bufp (bp), rk_tab (xsyntax_table (rk_bufp->lsyntax_table)),
       rk_ctx_mask (0)
{
  if (!consp (rk_list))
    {
      rk_list = 0;
      return;
    }

  lisp r = rk_list;
  rk_can_fastmap = 1;
  bzero (rk_fastmap, sizeof rk_fastmap);
  do
    {
      lisp regex = check_format (xcar (r));
      if (!regex)
        xcar (r) = Qnil;
      else if (!Regexp::merge_fastmap (regex, rk_fastmap, rk_tab))
        {
          rk_can_fastmap = 0;
          break;
        }
    }
  while (consp (r = xcdr (r)));

  for (; consp (r); r = xcdr (r))
    if (!check_format (xcar (r)))
      xcar (r) = Qnil;
}

int
regexp_kwd::kwdmatch (const Point &point, int scolor, int &revkwd)
{
  int mask = sc2mask (scolor);
  if (mask & ctx_mask ())
    {
      const point_t limit = min (rk_bufp->b_contents.p2,
                                 point_t (point.p_point + MAX_KWDLEN - 1));
      lisp r = rk_list;
      do
        {
          lisp x = xcar (r);
          if (consp (x))
            {
              lisp regex = xcar (x);
              x = xcdr (x);
              lisp val = xcar (x);
              int ctx = consp (x = xcdr (x)) ? xshort_int_value (xcar (x)) : 1;
              if (mask & ctx)
                {
                  try
                    {
                      extern u_char char_no_translate_table[];
                      Regexp re (char_no_translate_table, rk_tab);
                      re.compile (regex, 0);
                      if (re.match (rk_bufp, point, rk_bufp->b_contents.p1, limit)
                          && re.re_regs.end[0] <= limit)
                        {
                          point_t beg, end;
                          if (!consp (x = xcdr (x)))
                            {
                              beg = re.re_regs.start[0];
                              end = re.re_regs.end[0];
                            }
                          else
                            {
                              int b = xshort_int_value (xcar (x));
                              int e = (consp (x = xcdr (x))
                                       ? xshort_int_value (xcar (x)) : 0);
                              if (b >= 0)
                                {
                                  if (b > re.re_regs.nregs)
                                    goto nomatch;
                                  beg = re.re_regs.start[b];
                                }
                              else
                                {
                                  b = -b;
                                  if (b > re.re_regs.nregs)
                                    goto nomatch;
                                  beg = re.re_regs.end[b];
                                }
                              if (e >= 0)
                                {
                                  if (e > re.re_regs.nregs)
                                    goto nomatch;
                                  end = re.re_regs.end[e];
                                }
                              else
                                {
                                  e = -e;
                                  if (e > re.re_regs.nregs)
                                    goto nomatch;
                                  end = re.re_regs.start[e];
                                }
                            }

                          if (end > beg && beg >= point.p_point)
                            {
                              if (end > point.p_point + MAX_KWDLEN)
                                end = point.p_point + MAX_KWDLEN;
                              rk_match = 1;
                              if (consp (val))
                                {
                                  rk_use_vals = 1;
                                  int vals[MAX_REGS];
                                  vals[0] = 0;
                                  for (int i = 1; i <= re.re_regs.nregs; i++)
                                    vals[i] = -1;
                                  do
                                    {
                                      lisp a = xcar (val);
                                      if (xcdr (a) != Qnil)
                                        vals[xshort_int_value (xcar (a))] =
                                          kwd_val (xcdr (a), 0, revkwd);
                                      val = xcdr (val);
                                    }
                                  while (consp (val));

                                  for (int i = end - beg - 1; i >= 0; i--)
                                    rk_vals[i] = 0;

                                  for (int i = 0; i <= re.re_regs.nregs; i++)
                                    if (vals[i] >= 0)
                                      {
                                        point_t b = re.re_regs.start[i];
                                        point_t e = re.re_regs.end [i];
                                        if (b >= 0 && e >= 0)
                                          {
                                            b = max (b, beg);
                                            e = min (e, end);
                                            for (int o = end - b - 1; b < e; b++, o--)
                                            rk_vals[o] = vals[i];
                                          }
                                      }
                                }
                              else
                                {
                                  rk_use_vals = 0;
                                  rk_val = kwd_val (val, 0, revkwd);
                                }
                              rk_match_beg = beg;
                              rk_match_end = end;
                              rk_last_try = end;
                              return end - point.p_point;
                            }
                        }
                    }
                  catch (nonlocal_jump &)
                    {
                    }
                }
            }
        nomatch:;
        }
      while (consp (r = xcdr (r)));
    }
  clear ();
  rk_last_try = point.p_point;
  return 0;
}

inline int
regexp_kwd::kwdmatch_begin (const Point &opoint, int scolor)
{
  if (!(sc2mask (scolor) & ctx_mask ()))
    return 0;

  Point point;
  point.p_point = opoint.p_point;

  Chunk *cp = opoint.p_chunk;
  const Char *p = cp->c_text + opoint.p_offset;
  int l;
  for (l = 0; l < MAX_KWDLEN; l++)
    {
      if (point.p_point <= rk_last_try)
        break;
      if (p == cp->c_text)
        {
          if (!cp->c_prev)
            break;
          cp = cp->c_prev;
          p = cp->c_text + cp->c_used;
        }
      if (*--p == CC_LFD)
        {
          if (++p == cp->c_text + cp->c_used && cp->c_next)
            {
              cp = cp->c_next;
              p = cp->c_text;
            }
          break;
        }
      point.p_point--;
    }
  if (!l)
    return 0;

  point.p_chunk = cp;
  point.p_offset = p - cp->c_text;

  do
    {
      int revkwd;
      if (kwdmatch (point, scolor, revkwd) > 0
          && match_end () > opoint.p_point)
        return match_end () - opoint.p_point;
      point.p_point++;
      if (++point.p_offset == point.p_chunk->c_used)
        {
          point.p_chunk = point.p_chunk->c_next;
          if (!point.p_chunk)
            break;
          point.p_offset = 0;
        }
    }
  while (point.p_point < opoint.p_point);
  return 0;
}

int
Window::redraw_line (glyph_data *gd, Point &point, long vlinenum, long plinenum,
                     int hide, lisp kwdhash, syntax_info *psi, textprop *&tprop,
                     regexp_kwd &re_kwd) const
{
  glyph_t *g = gd->gd_cc;
  glyph_t *const ge = g + w_ch_max.cx;

  if (g < ge)
    *g++ = ' ';

  for (; tprop; tprop = tprop->t_next)
    if (*tprop > point.p_point)
      break;

  int wflags = flags ();
  if (wflags & WF_LINE_NUMBER)
    {
      glyph_t f = (vlinenum == w_last_mark_linenum
                   ? (GLYPH_REVERSED | GLYPH_LINENUM)
                   : GLYPH_LINENUM);
      if (plinenum != -1 && point.p_point && point.prevch () != '\n')
        {
          f |= ' ';
          for (glyph_t *e = min (ge, g + LINENUM_COLUMNS); g < e; g++)
            *g = f;
          if (g < ge)
            *g++ = GLYPH_LINENUM | GLYPH_BM_SEP;
        }
      else
        {
          if (plinenum == -1)
            plinenum = vlinenum;
          if (w_ch_max.cx >= LINENUM_COLUMNS + 1)
            {
              glyph_t *p = g + LINENUM_COLUMNS;
              do
                {
                  *--p = f | (plinenum % 10 + '0');
                  plinenum /= 10;
                }
              while (p > g && plinenum);
              while (p > g)
                *--p = f | ' ';
              g += LINENUM_COLUMNS;
              *g++ = GLYPH_LINENUM | GLYPH_BM_SEP;
            }
          else
            {
              char buf[32];
              if (plinenum >= 1000000)
                sprintf (buf, "%06d", plinenum % 1000000);
              else
                sprintf (buf, "%6d", plinenum);
              int n = min (6, int (w_ch_max.cx));
              for (int i = 0; i < n; i++)
                *g++ = f | buf[i];
              if (g < ge)
                *g++ = GLYPH_LINENUM | GLYPH_BM_SEP;
            }
        }

      if (tprop && point.p_point >= tprop->t_range.p1
          && *tprop > point.p_point
          && tprop->t_attrib & 0xff
          && g == gd->gd_cc + LINENUM_COLUMNS + 2)
        g[-2] = tprop->t_attrib & ~TEXTPROP_EXTEND_EOL_BIT;
    }

  if (w_bufp->b_prompt_columns)
    {
      glyph_t *ge2 = g + w_bufp->b_prompt_columns;
      if (ge2 > ge)
        ge2 = ge;
      if (vlinenum == 1)
        {
          for (const u_char *u = (u_char *)w_bufp->b_prompt_arg; *u && g < ge;)
            *g++ = *u++;
          const Char *s = w_bufp->b_prompt;
          const Char *se = s + w_bufp->b_prompt_length;
          while (s < se && g < ge)
            {
              Char cc = *s++;
              if (cc < ' ')
                {
                  if (g + 1 == ge)
                    break;
                  *g++ = GLYPH_CTRL | '^';
                  *g++ = cc + (GLYPH_CTRL | '@');
                }
              else if (cc == CC_DEL)
                {
                  if (g + 1 == ge)
                    break;
                  *g++ = GLYPH_CTRL | '^';
                  *g++ = GLYPH_CTRL | '?';
                }
              else if (char_width (cc) == 2)
                {
                  if (g + 1 == ge)
                    break;
                  g = glyph_dbchar (g, cc, 0, 0);
                }
              else
                g = glyph_sbchar (g, cc, 0, 0);
            }
        }
      while (g < ge2)
        *g++ = ' ';
    }

  int seltype = w_selection_type & Buffer::SELECTION_TYPE_MASK;
  long rcol1 = w_goal_column;
  long rcol2 = w_selection_column;
  if (rcol1 > rcol2)
    swap (rcol1, rcol2);

  int symlen = 0;
  int kwdflag = 0;
  int revkwd = 0;
  int long_kwd = 0;
  int scolor;

  glyph_t *const g0 = g;

  point_t limit;
  Point fold_eol;
  if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
    {
      fold_eol = point;
      w_bufp->folded_go_eol (fold_eol);
      limit = min (w_bufp->b_nchars, point_t (fold_eol.p_point + 1));
    }
  else
    limit = w_bufp->b_nchars;

  const int fold_column = w_bufp->b_fold_columns - w_top_column;
  if (w_top_column)
    {
      long col = w_bufp->forward_column (point, w_top_column, 0, 1, 0);
      if (col < w_top_column || point.p_point >= limit)
        {
          if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
            {
              point = fold_eol;
              if (wflags & WF_FOLD_LINE && fold_column >= 0)
                {
                  glyph_t *const e = g0 + fold_column;
                  if (g < e && e < ge)
                    {
                      for (; g < e; g++)
                        *g = ' ';
                      *g++ = (GLYPH_CTRL | GLYPH_BM_FOLD_SEP0) + (vlinenum & 1);
                    }
                }
            }
          *g = 0;
          gd->gd_len = g - gd->gd_cc;
          gd->gd_mod = 1;
          return 0;
        }

      if (psi)
        {
          psi->point_syntax (point);
          scolor = syntax_state::ss_colors[syntax_state::SS_NORMAL][psi->si.ss_state];
          if (scolor & (syntax_state::KWD_OK | syntax_state::KWD2_OK)
              && !re_kwd.match_p () && kwdhash
              && point.p_offset < point.p_chunk->c_used)
            kwdflag = kwdmatch (kwdhash, point, symlen, revkwd, long_kwd, scolor);
        }
      else
        scolor = 0;

      int n = min (int (col - w_top_column), ge - g);
      if (n)
        {
          int f = 0;
          if (point.p_point < w_bufp->b_contents.p1
              || point.p_point >= w_bufp->b_contents.p2)
            f |= GLYPH_HIDDEN;

          if (hide && f & GLYPH_HIDDEN)
            f = 0;
          else
            {
              if (seltype != Buffer::SELECTION_VOID
                  && point.p_point > w_selection_region.p1
                  && point.p_point <= w_selection_region.p2
                  && (seltype != Buffer::SELECTION_RECTANGLE
                      || (w_top_column >= rcol1 && w_top_column < rcol2)))
                f |= GLYPH_SELECTED;
              if (w_reverse_region.p1 != NO_MARK_SET
                  && point.p_point > w_reverse_region.p1
                  && point.p_point <= w_reverse_region.p2)
                f |= GLYPH_REVERSED;
            }

          while (n-- > 0)
            *g++ = f | ' ';
        }
    }
  else
    {
      if (psi)
        {
          psi->point_syntax (point);
          scolor = syntax_state::ss_colors[syntax_state::SS_NORMAL][psi->si.ss_state];
          if (scolor & (syntax_state::KWD_OK | syntax_state::KWD2_OK)
              && !re_kwd.match_p ()
              && w_bufp->b_fold_columns != Buffer::FOLD_NONE
              && kwdhash && point.p_point && point.prevch () != '\n'
              && point.p_offset < point.p_chunk->c_used)
            kwdflag = kwdmatch (kwdhash, point, symlen, revkwd, long_kwd, scolor);
        }
      else
        scolor = 0;
    }

  if (re_kwd.valid_p () && (!symlen || !((kwdflag ^ scolor) | re_kwd.match_p ())))
    {
      if (re_kwd.match_p () && re_kwd.match_end () > point.p_point)
        symlen = re_kwd.match_end () - point.p_point;
      else
        symlen = re_kwd.kwdmatch_begin (point, scolor);
      if (!symlen)
        re_kwd.clear ();
    }

  for (; tprop; tprop = tprop->t_next)
    if (tprop->t_range.p2 > point.p_point)
      break;

  int start_in_range = (point.p_point >= w_bufp->b_contents.p1
                        && point.p_point <= w_bufp->b_contents.p2);

  Chunk *cp = point.p_chunk;
  const Char *p = cp->c_text + point.p_offset;
  const Char *pe = cp->c_text + cp->c_used;
  int eol = 0, eof = 0;
  int exceed = 0;
  syntax_state osi;
  int last_attrib = 0;

  syntax_state::define_chunk (cp);

  while (g < ge)
    {
      if (p == pe)
        {
          if (psi)
            cp->update_syntax (*psi);
          if (!cp->c_next)
            {
              eof = 1;
              if (wflags & WF_EOF
                  && (!hide || w_bufp->b_contents.p2 == w_bufp->b_nchars))
                {
                  if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
                    {
                      Point tem (fold_eol);
                      if (w_bufp->next_char (tem))
                        break;
                    }

                  int f = GLYPH_CTRL;
                  if (last_attrib & TEXTPROP_EXTEND_EOL_BIT)
                    f |= last_attrib & ((GLYPH_TEXTPROP_NCOLORS - 1)
                                        << GLYPH_TEXTPROP_BG_SHIFT_BITS);
                  int n = min (ge - g, 5);
                  for (int i = 0; i < n; i++)
                    *g++ = f | "[EOF]"[i];
                }
              break;
            }
          cp = cp->c_next;
          p = cp->c_text;
          pe = p + cp->c_used;
          syntax_state::define_chunk (cp);
        }

      if (point.p_point == limit)
        break;

      int f = 0;
      if (point.p_point < w_bufp->b_contents.p1
          || point.p_point >= w_bufp->b_contents.p2)
        f |= GLYPH_HIDDEN;

      if (seltype != Buffer::SELECTION_VOID
          && point.p_point >= w_selection_region.p1
          && point.p_point < w_selection_region.p2
          && (seltype != Buffer::SELECTION_RECTANGLE
              || w_top_column + (g - g0) >= rcol1))
        f |= GLYPH_SELECTED;

      if (w_reverse_region.p1 != NO_MARK_SET
          && point.p_point >= w_reverse_region.p1
          && point.p_point < w_reverse_region.p2)
        f |= GLYPH_REVERSED;

      int kwdf = 0;
      if (psi)
        {
          osi = psi->si;
          (psi->si.*syntax_state::update) (p);
          psi->point = point.p_point + 1;
          if (g > g0 && !(g[-1] & (GLYPH_TRAIL | GLYPH_TEXTPROP_FG_BIT)))
            g[-1] |= syntax_state::ss_prev_colors[osi.ss_state][psi->si.ss_state];
          scolor = syntax_state::ss_colors[osi.ss_state][psi->si.ss_state];
          int kwd_ok = scolor & (syntax_state::KWD_OK | syntax_state::KWD2_OK);
          scolor &= ~(syntax_state::KWD_OK | syntax_state::KWD2_OK);
          f |= scolor;
          if (!re_kwd.match_p ())
            {
              if (kwd_ok)
                {
                  if (kwdhash && !symlen)
                    kwdflag = kwdmatch (kwdhash, p, cp, symlen,
                                        revkwd, scolor, long_kwd, kwd_ok);
                  if (symlen > 0)
                    {
                      symlen--;
                      if (kwdflag & GLYPH_HIDDEN)
                        kwdflag &= ~GLYPH_HIDDEN;
                      else
                        {
                          f = (f & ~GLYPH_TEXT_MASK) | kwdflag;
                          kwdf = 1;
                        }
                    }
                }
              else
                symlen = 0;
            }
        }
      else
        scolor = 0;

      if (re_kwd.valid_p ())
        {
          if (!(symlen | kwdf) || !((kwdflag ^ scolor) | re_kwd.match_p ()))
            {
              if (re_kwd.possible_match_p (*p))
                {
                  point.p_chunk = cp;
                  point.p_offset = p - cp->c_text;
                  symlen = re_kwd.kwdmatch (point, scolor, revkwd);
                  if (symlen)
                    {
                      symlen--;
                      if (point.p_point >= re_kwd.match_beg ())
                        f = (f & ~GLYPH_TEXT_MASK) | re_kwd.value (symlen);
                      if (!symlen)
                        re_kwd.clear ();
                    }
                }
            }
          else if (re_kwd.match_p ())
            {
              symlen--;
              if (point.p_point >= re_kwd.match_beg ())
                f = (f & ~GLYPH_TEXT_MASK) | re_kwd.value (symlen);
              if (!symlen)
                re_kwd.clear ();
            }
        }

      last_attrib = 0;
      if (tprop && point.p_point >= tprop->t_range.p1
          && *tprop > point.p_point)
        {
          if (point.p_point < tprop->t_range.p2)
            {
              if (!(f & (GLYPH_HIDDEN | GLYPH_SELECTED /*| GLYPH_REVERSED*/)))
                {
                  if (tprop->t_attrib & ((GLYPH_TEXTPROP_NCOLORS - 1)
                                         << GLYPH_TEXTPROP_FG_SHIFT_BITS))
                    f = (f & ~GLYPH_TEXT_MASK) | GLYPH_TEXTPROP_FG_BIT;
                  f |= tprop->t_attrib & ~(TEXTPROP_EXTEND_EOL_BIT | 0xff);
                }
              last_attrib = tprop->t_attrib;
            }
          while (point.p_point >= tprop->t_range.p2 - 1)
            {
              tprop = tprop->t_next;
              if (!tprop)
                break;
            }
        }

      Char cc = *p++;
      point.p_point++;

      if (hide && f & GLYPH_HIDDEN)
        {
          if (cc == CC_LFD)
            {
              eol = 1;
              break;
            }
          else if (cc == CC_TAB)
            {
              int col = w_top_column + (g - g0);
              int goal = ((col + w_bufp->b_tab_columns) / w_bufp->b_tab_columns
                          * w_bufp->b_tab_columns);
              int n = min (goal - col, ge - g);
              while (n-- > 0)
                *g++ = ' ';
            }
          else if (char_width (cc) == 2)
            {
              if (g + 1 == ge)
                {
                  exceed = 1;
                  break;
                }
              *g++ = ' ';
              *g++ = ' ';
            }
          else
            *g++ = ' ';
        }
      else
        {
          glyph_t *gr = g;

          if (cc < ' ')
            {
              if (cc == CC_LFD)
                {
                  if (seltype == Buffer::SELECTION_RECTANGLE)
                    f &= ~GLYPH_SELECTED;
                  if (wflags & WF_NEWLINE)
                    g = glyph_bmchar (g, GLYPH_BM_NEWLINE, Vdisplay_newline_char,
                                      (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL, 1);
                  else
                    *g++ = (f & ~GLYPH_TEXT_MASK) | ' ';
                  eol = 1;
                  break;
                }
              else if (cc == CC_TAB)
                {
                  int col = w_top_column + (g - g0);
                  int goal = ((col + w_bufp->b_tab_columns) / w_bufp->b_tab_columns
                              * w_bufp->b_tab_columns);
                  int n = min (goal - col, ge - g);
                  if (wflags & WF_HTAB)
                    {
                      g = glyph_bmchar (g, GLYPH_BM_HTAB, Vdisplay_first_tab_char,
                                        (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL, 1);
                      if (--n > 0)
                        g = glyph_bmchar (g, '.', Vdisplay_rest_tab_char,
                                          (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL, n);
                    }
                  else
                    while (n-- > 0)
                      *g++ = f | ' ';
                }
              else
                {
                  if (g + 1 == ge)
                    {
                      exceed = 1;
                      break;
                    }
                  *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | '^';
                  *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | cc + '@';
                }
            }
          else if (cc == CC_DEL)
            {
              if (g + 1 == ge)
                {
                  exceed = 1;
                  break;
                }
              *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | '^';
              *g++ = (f & ~GLYPH_TEXT_MASK) | GLYPH_CTRL | '?';
            }
          else if (char_width (cc) == 2)
            {
              if (g + 1 == ge)
                {
                  exceed = 1;
                  break;
                }
              g = glyph_dbchar (g, cc, f, wflags);
            }
          else
            g = glyph_sbchar (g, cc, f, wflags);

          if (f & GLYPH_SELECTED
              && seltype == Buffer::SELECTION_RECTANGLE
              && w_top_column + (g - g0) > rcol2)
            while (gr < g)
              *gr++ &= ~GLYPH_SELECTED;
        }
    }

  int end_in_range = (point.p_point >= w_bufp->b_contents.p1
                      && point.p_point <= w_bufp->b_contents.p2);

  glyph_t *tailg = g;
  int hiddenf = g > g0 ? g[-1] & GLYPH_HIDDEN : 0;
  int f = (last_attrib & TEXTPROP_EXTEND_EOL_BIT
           ? last_attrib & ((GLYPH_TEXTPROP_NCOLORS - 1)
                            << GLYPH_TEXTPROP_BG_SHIFT_BITS)
           : 0);
  glyph_t *const grev = (w_bufp->b_fold_columns == Buffer::FOLD_NONE
                         ? ge : min (ge, max (g0 + max (fold_column, 0), g)));
  glyph_t *const gfold = g0 + fold_column;

  if (end_in_range)
    {
      if (w_bufp->b_fold_columns != Buffer::FOLD_NONE
          && wflags & WF_FOLD_MARK
          && !eof && !eol && g < ge
          && !exceed && point.p_point == limit)
        {
          if (g == gfold && wflags & WF_FOLD_LINE)
            *g++ = ((hiddenf
                     ? (GLYPH_CTRL | GLYPH_BM_FOLD_SEP0)
                     : (GLYPH_CTRL | GLYPH_BM_FOLD_MARK_SEP0))
                    + (vlinenum & 1));
          else
            *g++ = GLYPH_CTRL | '<' | f | hiddenf;
        }

      if (f)
        {
          f |= ' ';
          for (; g < grev; g++)
            *g = f;
        }
    }

  if (revkwd && (start_in_range || end_in_range || !hide))
    {
#define REVMASK (GLYPH_TEXT_MASK | ((GLYPH_TEXTPROP_NCOLORS - 1) \
                                    << GLYPH_TEXTPROP_BG_SHIFT_BITS))
      if ((revkwd & ((GLYPH_TEXTPROP_NCOLORS - 1)
                     << GLYPH_TEXTPROP_BG_SHIFT_BITS))
          || (!(revkwd & GLYPH_TEXTPROP_FG_BIT)
              && (revkwd & (15 << GLYPH_COLOR_SHIFT_BITS)) >= GLYPH_KEYWORD1R
              && (revkwd & (15 << GLYPH_COLOR_SHIFT_BITS)) <= GLYPH_KEYWORD3R))
        {
          for (glyph_t *p = g0; p < g; p++)
            *p = (*p & ~REVMASK) | revkwd;
          revkwd |= ' ' | hiddenf;
          for (; g < grev; g++)
            *g = revkwd;
        }
      else
        {
          for (glyph_t *p = g0; p < g; p++)
            if (!(*p & GLYPH_CTRL))
              *p = (*p & ~REVMASK) | revkwd;
        }
    }

  if (w_bufp->b_fold_columns != Buffer::FOLD_NONE
      && wflags & WF_FOLD_LINE && g <= gfold && gfold < ge)
    {
      for (; g < gfold; g++)
        *g = ' ';
      *g++ = (GLYPH_CTRL | GLYPH_BM_FOLD_SEP0) + (vlinenum & 1);
    }

  for (; g > gd->gd_cc && g[-1] == ' '; g--)
    ;

  *g = 0;
  gd->gd_len = g - gd->gd_cc;
  gd->gd_mod = 1;

  if (p == pe && cp->c_next)
    {
      if (psi)
        cp->update_syntax (*psi);
      point.p_chunk = cp->c_next;
      point.p_offset = 0;
    }
  else
    {
      point.p_chunk = cp;
      point.p_offset = p - cp->c_text;
    }

  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    return eol;

  if (psi && point.p_offset < point.p_chunk->c_used && tailg > g0
      && psi->si.maybe_comment_p ())
    {
      osi = psi->si;
      syntax_state::define_chunk (point.p_chunk);
      (osi.*syntax_state::update) (&point.ch ());
      if (syntax_state::ss_colors[psi->si.ss_state][osi.ss_state] == GLYPH_COMMENT)
        if (!(tailg[-1] & GLYPH_TEXTPROP_FG_BIT))
          tailg[-1] |= GLYPH_COMMENT;
    }

  point = fold_eol;
  return 0;
}

void
Window::scroll_down_region (int y1, int y2, int dy, int offset) const
{
  glyph_data **g = w_glyphs.g_rep->gr_oglyph + y1;
  int maxl = 0;
  for (int i = y1; i <= y2; i++, g++)
    maxl = max (maxl, int ((*g)->gd_len));
  if (maxl == offset)
    return;

  erase_cursor_line (0);

  g = w_glyphs.g_rep->gr_oglyph;
  for (int yd = y2, ys = y2 - dy; ys >= y1; yd--, ys--)
    if (g[ys]->gd_len > offset)
      {
        memcpy (g[yd]->gd_cc + offset, g[ys]->gd_cc + offset,
                sizeof (glyph_t) * (g[ys]->gd_len + 1 - offset));
        for (glyph_t *p = g[yd]->gd_cc + g[yd]->gd_len,
             *pe = g[yd]->gd_cc + offset;
             p < pe; p++)
          *p = GLYPH_JUNK;
        g[yd]->gd_len = g[ys]->gd_len;
      }

  RECT r;
  r.left = (offset - 1) * app.text_font.cell ().cx + app.text_font.cell ().cx / 2;
  r.right = min (w_client.cx, LONG ((maxl - 1) * app.text_font.cell ().cx
                                    + app.text_font.cell ().cx / 2));
  r.top = y1 * app.text_font.cell ().cy;
  r.bottom = (y2 + 1) * app.text_font.cell ().cy;
  ScrollWindow (w_hwnd, 0, dy * app.text_font.cell ().cy, 0, &r);
  r.bottom = r.top + dy * app.text_font.cell ().cy;
  ValidateRect (w_hwnd, &r);
}

void
Window::scroll_up_region (int y1, int y2, int dy, int offset) const
{
  glyph_data **g = w_glyphs.g_rep->gr_oglyph + y1;
  int maxl = 0;
  for (int i = y1; i <= y2; i++, g++)
    maxl = max (maxl, int ((*g)->gd_len));
  if (maxl == offset)
    return;

  erase_cursor_line (0);

  g = w_glyphs.g_rep->gr_oglyph;
  for (int yd = y1, ys = y1 + dy; ys <= y2; yd++, ys++)
    if (g[ys]->gd_len > offset)
      {
        memcpy (g[yd]->gd_cc + offset, g[ys]->gd_cc + offset,
                sizeof (glyph_t) * (g[ys]->gd_len + 1 - offset));
        for (glyph_t *p = g[yd]->gd_cc + g[yd]->gd_len,
             *pe = g[yd]->gd_cc + offset;
             p < pe; p++)
          *p++ = GLYPH_JUNK;
        g[yd]->gd_len = g[ys]->gd_len;
      }

  RECT r;
  r.left = (offset - 1) * app.text_font.cell ().cx + app.text_font.cell ().cx / 2;
  r.right = min (w_client.cx, LONG ((maxl - 1) * app.text_font.cell ().cx
                                    + app.text_font.cell ().cx / 2));
  r.top = y1 * app.text_font.cell ().cy;
  r.bottom = (y2 + 1) * app.text_font.cell ().cy;
  ScrollWindow (w_hwnd, 0, -dy * app.text_font.cell ().cy, 0, &r);
  r.top = r.bottom - dy * app.text_font.cell ().cy;
  ValidateRect (w_hwnd, &r);
}

#define NO_MATCH 0
#define FULL_MATCH 1
#define HALF_MATCH 2

static int
compare_glyph (const glyph_data *g1, const glyph_data *g2, int offset)
{
  if (g1->gd_len != g2->gd_len)
    return NO_MATCH;
  if (offset < g1->gd_len)
    {
      if (memcmp (g1->gd_cc + offset, g2->gd_cc + offset,
                  sizeof (glyph_t) * (g1->gd_len - offset)))
        return NO_MATCH;
      return (memcmp (g1->gd_cc, g2->gd_cc, sizeof (glyph_t) * offset)
              ? HALF_MATCH : FULL_MATCH);
    }
  return (memcmp (g1->gd_cc, g2->gd_cc, sizeof (glyph_t) * g1->gd_len)
          ? NO_MATCH : FULL_MATCH);
}

void
Window::find_motion () const
{
  int offset = (flags () & WF_LINE_NUMBER) ? LINENUM_COLUMNS + 1 : 0;
  glyph_data **og = w_glyphs.g_rep->gr_oglyph;
  glyph_data **ng = w_glyphs.g_rep->gr_nglyph;
  int y1, y2;
  for (y1 = 0; y1 < w_ech.cy; y1++)
    {
      int f = compare_glyph (og[y1], ng[y1], offset);
      if (f == NO_MATCH)
        break;
      ng[y1]->gd_mod = f != FULL_MATCH;
    }

  for (y2 = w_ech.cy - 1; y2 > y1; y2--)
    {
      int f = compare_glyph (og[y2], ng[y2], offset);
      if (f == NO_MATCH)
        break;
      ng[y2]->gd_mod = f != FULL_MATCH;
    }

  if (y1 == y2)
    return;

  struct {int y1, y2, dy, f, n;} down, up;
  down.n = 0;
  up.n = 0;

  for (int y = y2 - 1; y > y1; y--)
    {
      int f = compare_glyph (og[y], ng[y2], offset);
      if (!f)
        continue;
      int oy, ny;
      for (oy = y - 1, ny = y2 - 1; oy >= y1; oy--, ny--)
        {
          int f2 = compare_glyph (og[oy], ng[ny], offset);
          if (!f2)
            break;
          f |= f2;
        }
      down.y1 = oy + 1;
      down.y2 = y2;
      down.dy = y2 - y;
      down.f = f;
      down.n = y - 1 - oy;
      break;
    }

  for (int i = 0; i < 3; i++, y2--)
    {
      for (int y = y2 - 1; y >= y1; y--)
        {
          int f = compare_glyph (og[y2], ng[y], offset);
          if (!f)
            continue;
          int oy, ny;
          for (oy = y2 - 1, ny = y - 1; ny >= y1; oy--, ny--)
            {
              int f2 = compare_glyph (og[oy], ng[ny], offset);
              if (!f2)
                break;
              f |= f2;
            }
          int n = y2 - 1 - oy;
          if (n > up.n)
            {
              up.y1 = ny + 1;
              up.y2 = y2;
              up.dy = y2 - y;
              up.f = f;
              up.n = n;
            }
          break;
        }
      if (up.n > 3)
        break;
    }

  if (down.n > up.n)
    {
      if (down.n >= 2)
        scroll_down_region (down.y1, down.y2, down.dy,
                            down.f == FULL_MATCH ? 0 : offset);
    }
  else
    {
      if (up.n >= 2)
        scroll_up_region (up.y1, up.y2, up.dy,
                          up.f == FULL_MATCH ? 0 : offset);
    }
}

void
Window::paint_region (HDC hdc, int from, int to) const
{
  HGDIOBJ of = SelectObject (hdc, app.text_font.font (FONT_ASCII));
  HDC hdcmem = CreateCompatibleDC (hdc);
  HGDIOBJ obm = SelectObject (hdcmem, app.text_font.hbm ());
  HGDIOBJ obr = SelectObject (hdc, CreateSolidBrush (w_colors[WCOLOR_BACK]));

  INT *padding;
  if (!app.text_font.need_pad_p ())
    padding = 0;
  else
    {
      padding = (INT *)alloca (sizeof *padding * w_ch_max.cx);
      for (int i = 0; i < w_ch_max.cx; i++)
        padding[i] = app.text_font.cell ().cx;
    }
  char *buf = (char *)alloca (w_ch_max.cx + 3);
  glyph_data **g = w_glyphs.g_rep->gr_nglyph + from;
  glyph_data **og = w_glyphs.g_rep->gr_oglyph + from;
  for (int y = from * app.text_font.cell ().cy, ye = to * app.text_font.cell ().cy;
       y < ye; y += app.text_font.cell ().cy, g++, og++)
    if ((*g)->gd_mod)
      {
        paint_line (hdc, hdcmem, *og, *g, buf, y, padding);
        (*g)->gd_mod = 0;
      }

  SelectObject (hdcmem, obm);
  DeleteDC (hdcmem);
  SelectObject (hdc, of);
  DeleteObject (SelectObject (hdc, obr));
}

void
Window::redraw_window (Point &p, long vlinenum, int all, int hide) const
{
  lisp kwdhash;
  syntax_info psi (w_bufp,
                   symbol_value (Vparentheses_hash_table, w_bufp),
                   symbol_value (Vhtml_highlight_mode, w_bufp) != Qnil);
  syntax_info *ppsi;
  if (minibuffer_window_p ()
      || symbol_value (Vhighlight_keyword, w_bufp) == Qnil)
    {
      kwdhash = 0;
      ppsi = 0;
    }
  else
    {
      kwdhash = symbol_value (Vkeyword_hash_table, w_bufp);
      if (!hash_table_p (kwdhash))
        kwdhash = 0;
      ppsi = &psi;
    }

  regexp_kwd re_kwd (symbol_value (Vregexp_keyword_list, w_bufp),
                     p.p_point, w_bufp);

  int plf = (flags () & WF_LINE_NUMBER
             && w_bufp->b_fold_columns != Buffer::FOLD_NONE
             && w_bufp->linenum_mode () == Buffer::LNMODE_LF);
  long plinenum = plf ? w_bufp->point_linenum (p) : -1;
  textprop *tprop = w_bufp->textprop_head (p.p_point);
  glyph_data **g = w_glyphs.g_rep->gr_nglyph;
  for (int y = 0; y < w_ch_max.cy; y++, g++, vlinenum++)
    {
      if ((!all && !(*g)->gd_mod)
          || !redraw_line (*g, p, vlinenum, plinenum, hide,
                           kwdhash, ppsi, tprop, re_kwd))
        {
          if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
            w_bufp->go_eol (p);
          else
#if 0
            w_bufp->folded_forward_column (p, w_bufp->b_fold_columns, 0, 0, 0);
#else
            w_bufp->folded_go_eol (p);
#endif
          if (!w_bufp->next_char (p))
            {
              for (y++, g++; y < w_ch_max.cy; y++, g++)
                {
                  (*g)->gd_len = 0;
                  (*g)->gd_mod = 1;
                  *(*g)->gd_cc = 0;
                }
              break;
            }
        }
      if (plf && p.prevch () == '\n')
        plinenum++;
    }
}

void
Window::scroll_lines (int dy)
{
  erase_cursor_line (0);

  glyph_data **og = w_glyphs.g_rep->gr_oglyph;
  int maxl = 0;
  for (int i = 0; i < w_ch_max.cy; i++, og++)
    maxl = max (maxl, int ((*og)->gd_len));

  RECT r;
  r.left = 0;
  r.right = min (w_client.cx, LONG ((maxl - 1) * app.text_font.cell ().cx
                                    + app.text_font.cell ().cx / 2));
  r.top = 0;
  if (dy < 0)
    r.bottom = w_ech.cy * app.text_font.cell ().cy;
  else
    r.bottom = w_client.cy;
  ScrollWindow (w_hwnd, 0, dy * app.text_font.cell ().cy, 0, &r);
  if (dy < 0)
    {
      r.bottom = w_client.cy;
      r.top = ((r.bottom + dy * app.text_font.cell ().cy)
               / app.text_font.cell ().cy * app.text_font.cell ().cy);
    }
  else
    {
      r.top = 0;
      r.bottom = dy * app.text_font.cell ().cy;
    }
  ValidateRect (w_hwnd, &r);

  if (dy < 0)
    {
      dy = -dy;
      og = w_glyphs.g_rep->gr_oglyph;
      glyph_data **ng = w_glyphs.g_rep->gr_nglyph;
      glyph_data **ogx = og;
      glyph_data **ngx = ng;
      glyph_data **osave = (glyph_data **)alloca (sizeof (glyph_data *) * dy * 2);
      glyph_data **nsave = osave + dy;
      int i;
      for (i = 0; i < dy; i++)
        {
          osave[i] = *og++;
          nsave[i] = *ng++;
        }
      for (; i < w_ech.cy; i++)
        {
          *ogx++ = *og++;
          (*ng)->gd_mod = 0;
          *ngx++ = *ng++;
        }
      og -= dy;
      for (i = 0; i < dy; i++, og++)
        {
          osave[i]->gd_len = (*og)->gd_len;
          memcpy (osave[i]->gd_cc, (*og)->gd_cc,
                  sizeof (glyph_t) * ((*og)->gd_len + 1));
          *ogx++ = osave[i];
          *ngx++ = nsave[i];
          nsave[i]->gd_mod = 1;
        }
      if (w_ech.cy < w_ch_max.cy)
        (*ngx)->gd_mod = 1;
    }
  else
    {
      og = w_glyphs.g_rep->gr_oglyph + w_ch_max.cy;
      glyph_data **ng = w_glyphs.g_rep->gr_nglyph + w_ch_max.cy;
      glyph_data **ogx = og;
      glyph_data **ngx = ng;
      glyph_data **osave = (glyph_data **)alloca (sizeof (glyph_data *) * dy * 2);
      glyph_data **nsave = osave + dy;
      int i;
      for (i = 0; i < dy; i++)
        {
          osave[i] = *--og;
          nsave[i] = *--ng;
        }
      for (; i < w_ch_max.cy; i++)
        {
          *--ogx = *--og;
          *--ngx = *--ng;
          (*ngx)->gd_mod = 0;
        }
      og += dy - 1;
      for (i = 0; i < dy; i++, og--)
        {
          osave[i]->gd_len = (*og)->gd_len;
          memcpy (osave[i]->gd_cc, (*og)->gd_cc,
                  sizeof (glyph_t) * ((*og)->gd_len + 1));
          *--ogx = osave[i];
          *--ngx = nsave[i];
          (*ngx)->gd_mod = 1;
        }
    }
}

static void
set_region (Region &r, point_t p1, point_t p2)
{
  if (p1 > p2)
    swap (p1, p2);
  if (r.p1 == -1)
    {
      r.p1 = p1;
      r.p2 = p2;
    }
  else
    {
      r.p1 = min (r.p1, p1);
      r.p2 = max (r.p2, p2);
    }
}

point_t
Window::bol_point (point_t goal) const
{
  Point p (w_point);
  w_bufp->goto_char (p, goal);
  w_bufp->goto_bol (p);
  return p.p_point;
}

point_t
Window::folded_bol_point (point_t goal) const
{
  Point p (w_point);
  w_bufp->goto_char (p, goal);
  w_bufp->folded_goto_bol (p);
  return p.p_point;
}

void
Window::reframe ()
{
  assert (w_bufp);

  if (w_bufp->b_fold_columns != Buffer::FOLD_NONE)
    w_bufp->folded_count_lines ();

  Region modr = w_bufp->b_modified_region;
  if ((w_selection_type & (Buffer::CONTINUE_PRE_SELECTION
                           | Buffer::PRE_SELECTION)) == Buffer::PRE_SELECTION)
    {
      set_region (modr, w_selection_region.p1, w_selection_region.p2);
      w_selection_type = Buffer::SELECTION_VOID;
      w_selection_point = NO_MARK_SET;
      w_selection_marker = NO_MARK_SET;
    }
  (int &)w_selection_type &= ~Buffer::CONTINUE_PRE_SELECTION;

  if (w_reverse_region.p1 != NO_MARK_SET)
    {
      set_region (modr, w_reverse_region.p1, w_reverse_region.p2);
      if ((w_reverse_temp & (Buffer::CONTINUE_PRE_SELECTION
                             | Buffer::PRE_SELECTION)) == Buffer::PRE_SELECTION)
        {
          w_reverse_region.p1 = NO_MARK_SET;
          w_reverse_region.p2 = NO_MARK_SET;
          w_reverse_temp = Buffer::SELECTION_VOID;
        }
    }
  (int &)w_reverse_temp &= ~Buffer::CONTINUE_PRE_SELECTION;

  if (w_selection_type != Buffer::SELECTION_VOID)
    {
      point_t p1, p2;
      point_t p = (w_selection_point == NO_MARK_SET
                   ? w_point.p_point : w_selection_point);
      if (w_selection_marker < p)
        {
          p1 = w_selection_marker;
          p2 = p;
        }
      else
        {
          p1 = p;
          p2 = w_selection_marker;
        }

      switch (w_selection_type & Buffer::SELECTION_TYPE_MASK)
        {
        case Buffer::SELECTION_RECTANGLE:
          if (w_disp_flags & (WDF_GOAL_COLUMN | WDF_SET_GOAL_COLUMN))
            set_region (modr, p1, p2);
          break;

        case Buffer::SELECTION_LINEAR:
          if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
            {
              p1 = bol_point (p1);
              p2 = bol_point (p2);
            }
          else
            {
              p1 = folded_bol_point (p1);
              p2 = folded_bol_point (p2);
            }
          break;
        }
      if (p1 != w_selection_region.p1)
        set_region (modr, p1, w_selection_region.p1);
      if (p2 != w_selection_region.p2)
        set_region (modr, p2, w_selection_region.p2);
      w_selection_region.p1 = p1;
      w_selection_region.p2 = p2;
    }

  int need_repaint = ((w_disp_flags & (WDF_WINDOW | WDF_MODELINE | WDF_PENDING))
                      || modr.p1 != -1
                      || w_last_disp != w_disp
                      || w_last_top_column != w_top_column);

  long mark_linenum = ((symbol_value (Vinverse_mark_line, w_bufp) != Qnil
                        && w_mark != NO_MARK_SET)
                       ? (w_bufp->b_fold_columns == Buffer::FOLD_NONE
                          ? w_bufp->point_linenum (w_mark)
                          : w_bufp->folded_point_linenum (w_mark))
                       : -1);
  if (mark_linenum != w_last_mark_linenum)
    {
      w_last_mark_linenum = mark_linenum;
      need_repaint = 1;
    }

  if (!need_repaint && w_point.p_point == w_last_point)
    {
#ifdef DEBUG
      if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
        {
          assert (w_linenum == w_bufp->point_linenum (w_point));
          assert (w_column == w_bufp->point_column (w_point));
        }
      else
        {
          assert (w_linenum == w_bufp->folded_point_linenum (w_point));
          assert (w_column == w_bufp->folded_point_column (w_point));
        }
#endif
      if (w_disp_flags & WDF_GOAL_COLUMN)
        w_goal_column = w_column;
      paint_cursor_line (0, 0);
      return;
    }

  w_last_point = w_point.p_point;

  long linenum, column;
  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      linenum = w_bufp->point_linenum (w_point);
      column = w_bufp->point_column (w_point);
      w_plinenum = linenum;
    }
  else
    {
      linenum = w_bufp->folded_point_linenum_column (w_point, &column);
      w_plinenum = (w_bufp->linenum_mode () == Buffer::LNMODE_LF
                    ? w_bufp->point_linenum (w_point)
                    : linenum);
    }

  long olinenum = w_linenum;
  w_linenum = linenum;
  w_column = column;
  if (w_disp_flags & WDF_GOAL_COLUMN)
    w_goal_column = column;

  int maxwidth = w_ech.cx - w_bufp->b_prompt_columns;
  if (flags () & WF_LINE_NUMBER)
    maxwidth -= LINENUM_COLUMNS + 1;

  if (w_point.p_offset != w_point.p_chunk->c_used)
    {
      Char c = w_point.ch ();
      if (c != CC_LFD && c != CC_TAB && char_width (c) == 2)
        maxwidth--;
    }

  maxwidth = max (maxwidth, 0);

  int hjump = w_bufp->b_hjump_columns;
  if (hjump <= 0)
    hjump = w_hjump_columns;

  if (column < w_top_column)
    w_top_column = column / hjump * hjump;
  else if (column >= w_top_column + maxwidth)
    w_top_column = ((column - maxwidth + hjump) / hjump * hjump);

  if (column < w_top_column || column - w_top_column >= maxwidth)
    w_top_column = column;

  if (w_top_column != w_last_top_column)
    need_repaint = 1;

  int hide = symbol_value (Vhide_restricted_region, w_bufp) != Qnil;
  if (hide && w_disp < w_bufp->b_contents.p1)
    w_disp = w_bufp->b_contents.p1;

  long last_linenum, disp_linenum;
  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      last_linenum = w_bufp->point_linenum (w_last_disp);
      disp_linenum = (w_disp == w_last_disp
                      ? last_linenum
                      : w_bufp->point_linenum (w_disp));
    }
  else
    {
      last_linenum = w_bufp->folded_point_linenum (w_last_disp);
      disp_linenum = (w_disp == w_last_disp
                      ? last_linenum
                      : w_bufp->folded_point_linenum (w_disp));
    }

  if (flags () & WF_SCROLLING)
    w_disp_flags |= WDF_REFRAME_SCROLL;

  long scroll_margin;
  safe_fixnum_value (symbol_value (Vscroll_margin, w_bufp),
                     &scroll_margin);
  scroll_margin = max (0L, min (scroll_margin, long ((w_ech.cy - 1) / 2)));

  if (w_ignore_scroll_margin)
    {
      if (!scroll_margin
          || (linenum >= disp_linenum + scroll_margin
              && linenum < disp_linenum + w_ech.cy - scroll_margin))
        w_ignore_scroll_margin = 0;
      else
        scroll_margin = min (w_ignore_scroll_margin - 1, int (w_ech.cy - 1) / 2);
    }

  long jump_scroll;
  safe_fixnum_value (symbol_value (Vjump_scroll_threshold, w_bufp),
                     &jump_scroll);
  jump_scroll = max (0L, jump_scroll);

  if (w_disp_flags & WDF_DELETE_TOP
      && xsymbol_value (Vold_relocation_method) == Qnil)
    disp_linenum = linenum - max (0L, min (olinenum - w_last_top_linenum,
                                           long (w_ech.cy - 1)));
  else if (linenum < disp_linenum + scroll_margin)
    {
      if (linenum >= disp_linenum - jump_scroll
          || w_disp_flags & WDF_REFRAME_SCROLL
          || w_disp > w_bufp->b_contents.p2)
        disp_linenum = linenum - scroll_margin;
      else
        disp_linenum = linenum - w_ech.cy / 2;
    }
  else if (linenum >= disp_linenum + w_ech.cy - scroll_margin)
    {
      if (linenum < disp_linenum + w_ech.cy + jump_scroll
          || w_disp_flags & WDF_REFRAME_SCROLL
          || w_disp < w_bufp->b_contents.p1)
        disp_linenum = linenum - w_ech.cy + scroll_margin + 1;
      else
        disp_linenum = linenum - w_ech.cy / 2;
    }
  disp_linenum = max (1L, disp_linenum);

  Point df;
  disp_linenum = (w_bufp->b_fold_columns == Buffer::FOLD_NONE
                  ? w_bufp->linenum_point (df, disp_linenum)
                  : w_bufp->folded_linenum_point (df, disp_linenum));

  if (!need_repaint && df.p_point == w_last_disp)
    {
      w_disp = w_last_disp;
      paint_cursor_line (0, 0);
      return;
    }

  w_last_disp = w_disp = df.p_point;

  if (!w_glyphs.g_rep && !alloc_glyph_rep ())
    {
      w_last_top_column = w_top_column;
      w_last_top_linenum = disp_linenum;
      w_last_flags = flags ();
      return;
    }

  hide_caret ();

  if (!need_repaint && w_top_column == w_last_top_column)
    {
      int dy = last_linenum - disp_linenum;
      if (dy && dy >= -w_ch_max.cy * 2 / 3 && dy <= w_ch_max.cy * 2 / 3)
        {
          scroll_lines (dy);
          redraw_window (df, disp_linenum, 0, hide);
          goto paint;
        }
    }

  redraw_window (df, disp_linenum, 1, hide);
  if (xsymbol_value (Vsi_find_motion) != Qnil)
    find_motion ();

paint:
  HDC hdc = GetDC (w_hwnd);
  if (w_cursor_line.ypixel >= 0
      && w_cursor_line.x1 == app.text_font.cell ().cx / 2
      && flags () & WF_LINE_NUMBER)
    erase_cursor_line (hdc);
  paint_window (hdc);
  w_last_top_column = w_top_column;
  w_last_top_linenum = disp_linenum;
  w_last_flags = flags ();
  paint_cursor_line (hdc, 1);
  ReleaseDC (w_hwnd, hdc);
  return;
}

void
Window::paint_minibuffer_message (lisp string)
{
  if (!w_glyphs.g_rep && !alloc_glyph_rep ())
    return;

  glyph_data **gr = w_glyphs.g_rep->gr_nglyph;

  glyph_t *g = (*gr)->gd_cc;
  glyph_t *ge = g + w_ch_max.cx;
  *g++ = ' ';

  const Char *p = xstring_contents (string);
  const Char *pe = p + xstring_length (string);

  while (g < ge && p < pe)
    {
      Char cc = *p++;
      if (cc < ' ')
        {
          if (g + 1 == ge)
            break;
          *g++ = GLYPH_CTRL | '^';
          *g++ = GLYPH_CTRL | cc + '@';
        }
      else if (cc == CC_DEL)
        {
          if (g + 1 == ge)
            break;
          *g++ = GLYPH_CTRL | '^';
          *g++ = GLYPH_CTRL | '?';
        }
      else if (char_width (cc) == 2)
        {
          if (g + 1 == ge)
            break;
          g = glyph_dbchar (g, cc, 0, 0);
        }
      else
        g = glyph_sbchar (g, cc, 0, 0);
    }

  app.minibuffer_prompt_column = g - (*gr)->gd_cc;

  for (; g > (*gr)->gd_cc && g[-1] == ' '; g--)
    ;
  *g = 0;
  (*gr)->gd_len = g - (*gr)->gd_cc;
  (*gr)->gd_mod = 1;

  gr++;
  for (int y = 1; y < w_ch_max.cy; y++, gr++)
    {
      (*gr)->gd_len = 0;
      (*gr)->gd_cc[0] = 0;
      (*gr)->gd_mod = 1;
    }

  hide_caret ();

  HDC hdc = GetDC (w_hwnd);
  paint_window (hdc);
  ReleaseDC (w_hwnd, hdc);

  update_caret ();
}

void
Window::clear_window ()
{
  if (!w_glyphs.g_rep && !alloc_glyph_rep ())
    return;

  glyph_data **g = w_glyphs.g_rep->gr_nglyph;
  for (int y = 0; y < w_ch_max.cy; y++, g++)
    {
      (*g)->gd_len = 0;
      (*g)->gd_cc[0] = 0;
      (*g)->gd_mod = 1;
    }

  HDC hdc = GetDC (w_hwnd);
  paint_window (hdc);
  paint_cursor_line (hdc, 1);
  ReleaseDC (w_hwnd, hdc);
}

static inline void
format_point (char *b, int l, int c)
{
  sprintf (b, "%10u:%-10u", l, c + 1);
}

/*
  012345678901234567890
  xxxxxxxxxx:xxxxxxxxxx
      654321 123456
 */
static void
point_from_end (const char *buf, const char *&bb, const char *&be)
{
  const char *b, *e;
  for (b = buf + 4; b > buf && b[-1] != ' '; b--)
    ;
  for (e = buf + 17; *e && *e != ' '; e++)
    ;
  bb = b;
  be = e;
}

static int
calc_point_width (int l, int c)
{
  char buf[32];
  format_point (buf, l, c);
  const char *b, *e;
  point_from_end (buf, b, e);
  return e - b;
}

static void
format_percent(char* buf, int size, int percent)
{
  sprintf_s(buf, size, "%d", percent);
}

bool
mode_line_percent_painter::need_repaint_all ()
{
	char buf[32];
	format_percent(buf, 32, m_percent);
	return m_point_pixel >= 0 && strlen(buf) != m_last_width;

}

int
mode_line_percent_painter::calc_percent (Buffer* bufp, point_t point)
{
  if(bufp->b_nchars > 0)
    return (100*point) / bufp->b_nchars;
  if(point == 0) // 0/0, treat as 0.
	return 0;
  return -1;
}


int
mode_line_percent_painter::paint_percent (HDC hdc)
{

  RECT r;
  r.top = 1;
  r.bottom = m_ml_size.cy - 1;
  r.left = m_point_pixel;

  char nb[32];
  format_percent(nb, 32, m_percent);
  m_last_width = strlen(nb);

  SIZE size;
  GetTextExtentPoint32 (hdc, nb, m_last_width, &size);

  long right = size.cx + r.left;
  r.right = min(right, m_ml_size.cx - 1);

  ExtTextOut (hdc,
              m_point_pixel ,
              1 + m_modeline_paramp->m_exlead,
              ETO_OPAQUE | ETO_CLIPPED, &r, nb, m_last_width, 0);
  m_last_percent = m_percent;
  

  return (int)right;
}


bool
mode_line_point_painter::need_repaint_all()
{
	return m_point_pixel >= 0 && calc_point_width (m_plinenum, m_column) != m_last_ml_point_width;
}

int
mode_line_point_painter::paint_point (HDC hdc)
{
  if (m_point_pixel < 0)
    return 0;
  if (m_column == m_last_ml_column && m_plinenum == m_last_ml_linenum)
    return 0;

  RECT r;
  r.top = 1;
  r.bottom = m_ml_size.cy - 1;

  char nb[32];
  format_point (nb, m_plinenum, m_column);
  const char *b, *e;
  point_from_end (nb, b, e);
  m_last_ml_point_width = e - b;

  int x0 = (m_point_pixel + m_modeline_paramp->m_exts[1]
            - m_modeline_paramp->m_exts[b - nb]);
  int right = (x0 + m_modeline_paramp->m_exts[e - nb]
               + m_modeline_paramp->m_exts[1]);

  if (m_last_ml_linenum < 0)
    {
      r.left = m_point_pixel;
      r.right = min (right, int (m_ml_size.cx - 1));
    }
  else
    {
      char ob[32];
      format_point (ob, m_last_ml_linenum, m_last_ml_column);
      int ib = b - nb, ie = e - nb;
      for (; ib < ie && ob[ib] == nb[ib]; ib++)
        ;
      for (; ie > ib && ob[ie - 1] == nb[ie - 1]; ie--)
        ;
      r.left = x0 + m_modeline_paramp->m_exts[ib];
      r.right = min (x0 + m_modeline_paramp->m_exts[ie], int (m_ml_size.cx - 1));
      b = nb + ib;
      e = nb + ie;
    }

  for (; b < e && *b == ' '; b++)
    ;
  for (; e > b && e[-1] == ' '; e--)
    ;

  ExtTextOut (hdc,
              x0 + m_modeline_paramp->m_exts[b - nb],
              1 + m_modeline_paramp->m_exlead,
              ETO_OPAQUE | ETO_CLIPPED, &r, b, e - b, 0);
  m_last_ml_column = m_column;
  m_last_ml_linenum = m_plinenum;
  return right;
}


void
Window::paint_mode_line (HDC hdc)
{
  char *b0, *b;
  char *posp = 0;
  char *percentp = 0;

  w_ime_mode_line = 0;
  lisp fmt = symbol_value (Vmode_line_format, w_bufp);
  if (stringp (fmt))
    {
      int l = max (int (w_ch_max.cx), 512);
      b0 = (char *)alloca (l + 10);
      b = b0;
      *b++ = ' ';

      buffer_info binfo (this, w_bufp, &posp, &w_ime_mode_line, &percentp);
      b = binfo.format (fmt, b, b0 + l);
    }
  else
    b0 = b = 0;

  COLORREF ofg, obg;
  if (w_inverse_mode_line)
    {
      ofg = SetTextColor (hdc, modeline_colors[MLCI_FOREGROUND]);
      obg = SetBkColor (hdc, modeline_colors[MLCI_BACKGROUND]);
#if 0
      HBRUSH hbr = CreateSolidBrush (sysdep.btn_text);
      HGDIOBJ obr = SelectObject (hdc, hbr);
      PatBlt (hdc, 0, 0, w_ml_size.cx, w_ml_size.cy, PATCOPY);
      SelectObject (hdc, obr);
      DeleteObject (hbr);
#endif
    }
  else
    {
      ofg = SetTextColor (hdc, w_colors[WCOLOR_MODELINE_FG]);
      obg = SetBkColor (hdc, w_colors[WCOLOR_MODELINE_BG]);
    }
  HGDIOBJ of = SelectObject (hdc, app.modeline_param.m_hfont);

  RECT r;
  r.left = 1;
  r.top = 1;
  r.right = w_ml_size.cx - 1;
  r.bottom = w_ml_size.cy - 1;

  std::list<mode_line_painter*> painters;
  w_point_painter.set_posp(posp);
  w_percent_painter.set_posp(percentp);
  if(posp) {
	  w_point_painter.setup_paint(&app.modeline_param, w_column, w_plinenum, w_ml_size);
	  painters.push_back(&w_point_painter);
  }
  else
  {
	  w_point_painter.no_format_specifier();
  }


  if(percentp) {
	  w_percent_painter.setup_paint(&app.modeline_param, mode_line_percent_painter::calc_percent(w_bufp, w_point.p_point), w_ml_size);

	  if(posp && posp > percentp) // tenuki sort.
		  painters.push_front(&w_percent_painter);
	  else
		  painters.push_back(&w_percent_painter);
  }
  else
  {
	  w_percent_painter.no_format_specifier();
  }


  if (painters.size() == 0)
    {
      ExtTextOut (hdc, 1, 1 + app.modeline_param.m_exlead,
                  ETO_OPAQUE | ETO_CLIPPED, &r, b0, b - b0, 0);
    }
  else
    {
	  char *b1 = b0;
	  for(std::list<mode_line_painter*>::iterator it = painters.begin(); it != painters.end(); it++)
	  {
		  mode_line_painter * painter = *it;

		  int point_start_px;

		  if(painter->get_posp() - b1 == 0)
		  {
			  point_start_px = r.left;
		  }
		  else
		  {
			  SIZE size;
			  GetTextExtentPoint32 (hdc, b1, painter->get_posp() - b1, &size);

			  point_start_px = r.left + size.cx;

			  r.right = min (point_start_px, int (w_ml_size.cx - 1));
			  ExtTextOut (hdc, r.left, 1 + app.modeline_param.m_exlead,
						  ETO_OPAQUE | ETO_CLIPPED, &r, b1, painter->get_posp() - b1, 0);
		  }

		  r.left = painter->first_paint(hdc, point_start_px);
		  b1 = painter->get_posp();
	  }

      r.right = w_ml_size.cx - 1;
      ExtTextOut (hdc, r.left, 1 + app.modeline_param.m_exlead,
                  ETO_OPAQUE | ETO_CLIPPED, &r, b1, b - b1, 0);
    }



  SelectObject (hdc, of);
  SetTextColor (hdc, ofg);
  SetTextColor (hdc, obg);

  HGDIOBJ open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_highlight));
  MoveToEx (hdc, 0, w_ml_size.cy - 2, 0);
  LineTo (hdc, 0, 0);
  LineTo (hdc, w_ml_size.cx - 1, 0);
  DeleteObject (SelectObject (hdc, open));

  open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_shadow));
  LineTo (hdc, w_ml_size.cx - 1, w_ml_size.cy - 1);
  LineTo (hdc, -1, w_ml_size.cy - 1);
  DeleteObject (SelectObject (hdc, open));
}

void
Window::paint_mode_line ()
{
  PAINTSTRUCT ps;
  if (w_disp_flags & WDF_MODELINE)
    {
      BeginPaint (w_hwnd_ml, &ps);
      EndPaint (w_hwnd_ml, &ps);
      HDC hdc = GetDC (w_hwnd_ml);
      paint_mode_line (hdc);
      ReleaseDC (w_hwnd_ml, hdc);
    }
  else
    {
      HDC hdc = BeginPaint (w_hwnd_ml, &ps);
      paint_mode_line (hdc);
      EndPaint (w_hwnd_ml, &ps);
    }
}

inline void
Window::update_mode_line_vars (int i, lisp var)
{
  lisp val = symbol_value (var, w_bufp);
  if (w_last_vars[i] != val)
    {
      w_last_vars[i] = val;
      w_disp_flags |= WDF_MODELINE;
    }
}

inline void
Window::update_mode_line_vars ()
{
  update_mode_line_vars (LV_MODE_NAME, Vmode_name);
  update_mode_line_vars (LV_MODE_LINE_FORMAT, Vmode_line_format);
  update_mode_line_vars (LV_READ_ONLY, Vbuffer_read_only);
  update_mode_line_vars (LV_OVERWRITE, Voverwrite_mode);
  update_mode_line_vars (LV_AUTO_FILL, Vauto_fill);
}

int
Window::redraw_mode_line ()
{
  if (!w_hwnd_ml)
    return 0;
  if (xsymbol_value (Vinverse_mode_line) == Qnil)
    {
      if (w_inverse_mode_line)
        {
          w_disp_flags |= WDF_MODELINE;
          w_inverse_mode_line = 0;
        }
    }
  else if (w_inverse_mode_line != (selected_window () == this))
    {
      w_disp_flags |= WDF_MODELINE;
      w_inverse_mode_line ^= 1;
    }

  update_mode_line_vars ();

  int r;

  HDC hdc = GetDC (w_hwnd_ml);
  // a little slow. we can avoid this setup if we check validity.
  w_point_painter.setup_paint(&app.modeline_param, w_column, w_plinenum, w_ml_size);
  w_percent_painter.setup_paint(&app.modeline_param, mode_line_percent_painter::calc_percent(w_bufp, w_point.p_point), w_ml_size);

  if (w_disp_flags & WDF_MODELINE
      || w_point_painter.need_repaint_all()
	  || w_percent_painter.need_repaint_all())
    {
      paint_mode_line (hdc);
      w_disp_flags &= ~WDF_MODELINE;
      r = 1;
    }
  else
    {
      COLORREF ofg, obg;
      if (w_inverse_mode_line)
        {
          ofg = SetTextColor (hdc, modeline_colors[MLCI_FOREGROUND]);
          obg = SetBkColor (hdc, modeline_colors[MLCI_BACKGROUND]);
        }
      else
        {
          ofg = SetTextColor (hdc, w_colors[WCOLOR_MODELINE_FG]);
          obg = SetBkColor (hdc, w_colors[WCOLOR_MODELINE_BG]);
        }
      HGDIOBJ of = SelectObject (hdc, app.modeline_param.m_hfont);

	  // order is not important.
	  w_point_painter.update_paint(hdc);
	  w_percent_painter.update_paint(hdc);

      SelectObject (hdc, of);
      SetTextColor (hdc, ofg);
      SetTextColor (hdc, obg);
      r = 0;
    }
  ReleaseDC (w_hwnd_ml, hdc);
  return r;
}

int
Window::refresh (int f)
{
  assert (IsWindow (w_hwnd));

  if (!w_next && stringp (xsymbol_value (Vminibuffer_message)))
    {
      if (w_disp_flags & (WDF_WINDOW | WDF_PENDING))
        {
          paint_minibuffer_message (xsymbol_value (Vminibuffer_message));
          w_disp_flags &= ~(WDF_WINDOW | WDF_PENDING);
        }
      return 0;
    }

  if (w_bufp != w_last_bufp)
    {
      w_last_bufp = w_bufp;
      w_top_column = 0;
      w_selection_region.p1 = -1;
      w_disp_flags |= WDF_WINDOW | WDF_MODELINE;
    }

  if (flags () != w_last_flags)
    w_disp_flags |= WDF_WINDOW;

  int r = 0;
  if (w_bufp)
    {
      int owf = w_disp_flags;
      Buffer::selection_type ost = w_selection_type;
      Buffer::selection_type ort = w_reverse_temp;
      if (!f)
        {
          (int &)w_selection_type |= Buffer::CONTINUE_PRE_SELECTION;
          (int &)w_reverse_temp |= Buffer::CONTINUE_PRE_SELECTION;
        }

      if (w_bufp->b_last_narrow_depth != w_bufp->b_narrow_depth)
        {
          if (!w_bufp->b_last_narrow_depth || !w_bufp->b_narrow_depth)
            w_disp_flags |= WDF_WINDOW | WDF_MODELINE;
          else
            w_disp_flags |= WDF_WINDOW;
        }

      if (f)
        w_bufp->check_range (w_point);

      reframe ();
      if (flags () & Window::WF_RULER)
        update_ruler ();
      r = redraw_mode_line ();

      if (f)
        w_disp_flags = 0;
      else
        {
          w_disp_flags = owf & (WDF_GOAL_COLUMN | WDF_SET_GOAL_COLUMN);
          w_selection_type = ost;
          w_reverse_temp = ort;
        }
    }
  else if (w_disp_flags & (WDF_WINDOW | WDF_PENDING))
    clear_window ();

  update_vscroll_bar ();
  update_hscroll_bar ();
  update_caret ();
  return r;
}

void
Window::pending_refresh ()
{
  if (!w_bufp)
    return;

  if ((w_selection_type & (Buffer::CONTINUE_PRE_SELECTION
                           | Buffer::PRE_SELECTION)) == Buffer::PRE_SELECTION)
    {
      w_disp_flags |= WDF_WINDOW;
      w_selection_type = Buffer::SELECTION_VOID;
      w_selection_point = NO_MARK_SET;
      w_selection_marker = NO_MARK_SET;
    }
  (int &)w_selection_type &= ~Buffer::CONTINUE_PRE_SELECTION;

  if (w_reverse_region.p1 != NO_MARK_SET
      && (w_reverse_temp & (Buffer::CONTINUE_PRE_SELECTION
                            | Buffer::PRE_SELECTION)) == Buffer::PRE_SELECTION)
    {
      w_disp_flags |= WDF_WINDOW;
      w_reverse_region.p1 = NO_MARK_SET;
      w_reverse_region.p2 = NO_MARK_SET;
      w_reverse_temp = Buffer::SELECTION_VOID;
    }
  (int &)w_reverse_temp &= ~Buffer::CONTINUE_PRE_SELECTION;

  w_disp_flags |= WDF_REFRAME_SCROLL | WDF_PENDING;

  if (w_point.p_point == w_last_point)
    {
#ifdef DEBUG
      if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
        {
          assert (w_linenum == w_bufp->point_linenum (w_point));
          assert (w_column == w_bufp->point_column (w_point));
        }
      else
        {
          assert (w_linenum == w_bufp->folded_point_linenum (w_point));
          assert (w_column == w_bufp->folded_point_column (w_point));
        }
#endif
    }
  else
    {
      w_last_point = w_point.p_point;
      if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
        {
          w_linenum = w_bufp->point_linenum (w_point);
          w_column = w_bufp->point_column (w_point);
        }
      else
        w_linenum = w_bufp->folded_point_linenum_column (w_point, &w_column);
    }

  if (w_linenum < w_last_top_linenum)
    w_last_top_linenum = w_linenum;
  else if (w_linenum >= w_last_top_linenum + w_ech.cy)
    w_last_top_linenum = w_linenum - w_ech.cy + 1;

  if (w_disp_flags & WDF_GOAL_COLUMN)
    w_goal_column = w_column;
}

void
refresh_screen (int f)
{
  Window::destroy_windows ();
  if (app.active_frame.windows_moved)
    Window::move_all_windows ();

  if (g_frame.modified ())
    recalc_toplevel ();

  lisp lmenu = (win32_menu_p (selected_buffer ()->lmenu)
                ? selected_buffer ()->lmenu
                : (win32_menu_p (xsymbol_value (Vdefault_menu))
                   ? xsymbol_value (Vdefault_menu)
                   : Qnil));
  if (lmenu != xsymbol_value (Vlast_active_menu))
    {
      if (SetMenu (app.toplev, lmenu == Qnil ? 0 : xwin32_menu_handle (lmenu)))
        {
          DrawMenuBar (app.toplev);
#ifndef WINDOWBLINDS_FIXED // WindowBlinds‘Îô
          if (lmenu == Qnil || xsymbol_value (Vlast_active_menu) == Qnil)
            {
              RECT r;
              GetWindowRect (app.toplev, &r);
              int w = r.right - r.left;
              int h = r.bottom - r.top;
              MoveWindow (app.toplev, r.left, r.top, w - 1, h - 1, 1);
              MoveWindow (app.toplev, r.left, r.top, w, h, 1);
            }
#endif /* WINDOWBLINDS_FIXED */
          xsymbol_value (Vlast_active_menu) = lmenu;
        }
    }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    UpdateWindow (wp->w_hwnd);

  int update_title_bar = 0;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->refresh (f) && wp == selected_window ())
      update_title_bar = 1;

  app.stat_area.update ();

  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    {
      bp->b_modified_region.p1 = -1;
      bp->b_last_narrow_depth = bp->b_narrow_depth;
    }

  if (f)
    {
      Buffer *bp = selected_buffer ();
      g_frame.update_ui ();
      bp->change_ime_mode ();
      bp->set_frame_title (update_title_bar);
      bp->dlist_add_head ();
      Fundo_boundary ();
    }
}

void
pending_refresh_screen ()
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    wp->pending_refresh ();

  selected_buffer ()->dlist_add_head ();
}

void
Window::paint_background (HDC hdc, int x, int y, int w, int h) const
{
  HBRUSH hbr = CreateSolidBrush (w_colors[WCOLOR_BACK]);
  HGDIOBJ obr = SelectObject (hdc, hbr);
  PatBlt (hdc, x, y, w, h, PATCOPY);
  SelectObject (hdc, obr);
  DeleteObject (hbr);
}

void
Window::winsize_changed (int w, int h)
{
  int ow = w_clsize.cx;
  w_clsize.cx = w - RIGHT_PADDING;
  if (w_clsize.cx < 0)
    w_clsize.cx = 0;
  w_clsize.cy = h;
#if 0
  if (w_clsize.cx < ow)
    {
      HDC hdc = GetDC (w_hwnd);
      paint_background (hdc, w_clsize.cx, 0, RIGHT_PADDING, h);
      ReleaseDC (w_hwnd, hdc);
    }
#else
  RECT r;
  r.left = w_clsize.cx < ow ? w_clsize.cx : ow;
  r.top = 0;
  r.right = r.left + RIGHT_PADDING;
  r.bottom = h;
  InvalidateRect (w_hwnd, &r, 1);
#endif
  w_disp_flags |= WDF_WINSIZE_CHANGED;
}

void
Window::discard_invalid_region (const PAINTSTRUCT &ps, RECT &r)
{
  r.left = max (0L, ((ps.rcPaint.left - app.text_font.cell ().cx / 2)
                     / app.text_font.cell ().cx));
  r.right = min (w_ch_max.cx,
                 ((ps.rcPaint.right + app.text_font.cell ().cx
                   + app.text_font.cell ().cx / 2 - 1)
                  / app.text_font.cell ().cx));
  r.right = max (r.left, r.right);
  r.top = max (0L, ps.rcPaint.top / app.text_font.cell ().cy);
  r.bottom = min (w_ch_max.cy,
                  ((ps.rcPaint.bottom + app.text_font.cell ().cy - 1)
                   / app.text_font.cell ().cy));
  r.bottom = max (r.top, r.bottom);

  glyph_data **og = w_glyphs.g_rep->gr_oglyph;
  glyph_data **ng = w_glyphs.g_rep->gr_nglyph;
  for (int y = r.top; y < r.bottom; y++)
    if (og[y]->gd_len > r.left || ps.fErase)
      {
        ng[y]->gd_mod = 1;
        glyph_t *p = &og[y]->gd_cc[r.left];
        for (int l = r.right - r.left; l > 0; l--)
          *p++ = GLYPH_JUNK;
        if (r.right > og[y]->gd_len)
          {
            *p = 0;
            og[y]->gd_len = short (r.right);
          }
      }
}

void
Window::update_window ()
{
  PAINTSTRUCT ps;
  HDC hdc = BeginPaint (w_hwnd, &ps);

  if (!w_glyphs.g_rep)
    {
      EndPaint (w_hwnd, &ps);
      w_disp_flags |= WDF_WINDOW;
      refresh (0);
      return;
    }

  RECT r;
  discard_invalid_region (ps, r);

  if (w_disp_flags & WDF_WINSIZE_CHANGED)
    {
      EndPaint (w_hwnd, &ps);
      w_disp_flags &= ~WDF_WINSIZE_CHANGED;
      w_disp_flags |= WDF_WINDOW;
      refresh (0);
    }
  else
    {
      paint_region (hdc, r.top, r.bottom);
      EndPaint (w_hwnd, &ps);
      paint_cursor_line (0, 1);
    }
  if (this == selected_window ()
      && !app.ime_composition
      && GetFocus () == app.toplev)
    update_caret ();
}

lisp
Frefresh_screen (lisp f)
{
  if (!f || f == Qnil)
    refresh_screen (0);
  else if (f != Qt)
    refresh_screen (1);
  else
    for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
      wp->w_disp_flags |= Window::WDF_WINDOW;
  return Qt;
}
