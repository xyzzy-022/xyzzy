#include "stdafx.h"
#include "ed.h"
#include "conf.h"

const UINT FontSet::fs_lang_id[] =
{
  IDS_LANG_ASCII,
  IDS_LANG_JAPANESE,
  IDS_LANG_LATIN,
  IDS_LANG_CYRILLIC,
  IDS_LANG_GREEK,
  IDS_LANG_CN_SIMPLIFIED,
  IDS_LANG_CN_TRADITIONAL,
  IDS_LANG_KSC5601,
  IDS_LANG_GEORGIAN,
};

const lisp *const FontSet::fs_lang_key[] =
{
  &Kascii,
  &Kjapanese,
  &Klatin,
  &Kcyrillic,
  &Kgreek,
  &Kcn_simplified,
  &Kcn_traditional,
  &Kksc5601,
  &Kgeorgian,
};

const char *const FontSet::fs_regent[] =
{
  "Ascii",
  "Japanese",
  "Latin",
  "Cyrillic",
  "Greek",
  "GB2312",
  "BIG5",
  "KSC5601",
  "Georgian",
};

const FontSet::fontface FontSet::fs_default_face[] =
{
  {"FixedSys", "ÇlÇr ÉSÉVÉbÉN", SHIFTJIS_CHARSET},
  {"FixedSys", "ÇlÇr ÉSÉVÉbÉN", SHIFTJIS_CHARSET},
  {"Courier New"},
  {"Courier New"},
  {"Courier New"},
  {"MS Hei", 0, GB2312_CHARSET},
  {"MingLiu", 0, CHINESEBIG5_CHARSET},
  {"GulimChe", 0, HANGEUL_CHARSET},
  {"BPG Courier New U"},
};

int
FontObject::create (const char *face, int h, int charset)
{
  LOGFONT lf;
  bzero (&lf, sizeof lf);
  strcpy (lf.lfFaceName, face);
  lf.lfHeight = h;
  lf.lfCharSet = charset;
  lf.lfPitchAndFamily = FIXED_PITCH;
  return create (lf);
}

int
FontObject::create (const LOGFONT &lf)
{
  HFONT h = CreateFontIndirect (&lf);
  if (!h)
    return 0;
  if (fo_hfont)
    DeleteObject (fo_hfont);
  fo_hfont = h;
  GetObject (h, sizeof fo_logfont, &fo_logfont);
  return 1;
}

void
FontObject::get_metrics ()
{
  SIZE ex1, ex2;

  HDC hdc = GetDC (0);
  get_metrics (hdc, ex1, ex2);
  ReleaseDC (0, hdc);
}

void
FontObject::get_metrics (HDC hdc, SIZE &ex1, SIZE &ex2)
{
  HGDIOBJ of = SelectObject (hdc, fo_hfont);
  TEXTMETRIC tm;
  GetTextMetrics (hdc, &tm);
  fo_size.cx = tm.tmAveCharWidth;
  fo_size.cy = tm.tmAscent + tm.tmDescent;
  fo_ascent = tm.tmAscent;
  GetTextExtentPoint32 (hdc, "A", 1, &ex1);
  GetTextExtentPoint32 (hdc, "Ç†", 2, &ex2);
  SelectObject (hdc, of);
}

void
FontObject::calc_offset (const SIZE &sz)
{
  fo_offset.x = (sz.cx - fo_size.cx) / 2;
  fo_offset.y = (sz.cy - fo_size.cy) / 2;
}

const bool
FontObject::update (LOGFONT &lf, const lisp keys, const bool recommend_size_p)
{
  check_cons (keys);
  lisp lface = find_keyword (Kface, keys);
  lisp lsize = find_keyword (Ksize, keys);

  bool update = false;
  if (lsize != Qnil && !recommend_size_p)
    {
      int size = fixnum_value (lsize);
      int old_size;
      int pixel;
      if (find_keyword_bool (Ksize_pixel_p, keys))
        {
          old_size =lf.lfHeight;
          pixel = size;
        }
      else
        {
          old_size = FontObject::pixel_to_point (lf.lfHeight);
          pixel = FontObject::point_to_pixel (size);
        }
      if (pixel < FONT_SIZE_MIN_PIXEL || pixel > FONT_SIZE_MAX_PIXEL)
        FErange_error (lsize);
      if (old_size != size)
        {
          lf.lfHeight = pixel;
          lf.lfWidth = 0;
          update = true;
        }
    }

  if (lface != Qnil)
    {
      check_string (lface);
      char *face = (char *)alloca (xstring_length (lface) * 2 + 1);
      w2s (face, lface);
      if (strcmp (lf.lfFaceName, face) != 0)
        {
          strcpy (lf.lfFaceName, face);
          update = true;
        }
    }

  return update;
}

void
FontSet::paint_newline_bitmap (HDC hdc)
{
  int h = fs_size.cy / 2;
  int y0 = fs_size.cy - 2;
  int ox = fs_cell.cx * newline + 2;
  int y;
  for (y = 0; y < h; y++)
    SetPixel (hdc, ox, y0 - y, RGB (0, 0, 0));
  for (y = 0; y < h / 2 - 1; y++)
    SetPixel (hdc, ox + y, y0 - y, RGB (0, 0, 0));
  int w, x;
  for (w = (y + 1) / 2, x = y; x >= w; x--)
    SetPixel (hdc, ox + x, y0 - y, RGB (0, 0, 0));
  for (x++; y < h; y++)
    SetPixel (hdc, ox + x, y0 - y, RGB (0, 0, 0));
  for (y--; x >= 0; x--)
    SetPixel (hdc, ox + x, y0 - y, RGB (0, 0, 0));
}

void
FontSet::paint_backsl_bitmap (HDC hdc)
{
  HGDIOBJ of = SelectObject (hdc, fs_font[FONT_ASCII]);

  TextOut (hdc, fs_cell.cx * backsl, 0, "/", 1);
  StretchBlt (hdc, fs_cell.cx * backsl, 0, fs_cell.cx, fs_cell.cy,
              hdc, fs_cell.cx * (backsl + 1) - 1, 0, -fs_cell.cx, fs_cell.cy,
              SRCCOPY);

  TextOut (hdc, fs_cell.cx * bold_backsl, 0, "/", 1);
  int omode = SetBkMode (hdc, TRANSPARENT);
  TextOut (hdc, fs_cell.cx * bold_backsl + 1, 0, "/", 1);
  SetBkMode (hdc, omode);
  StretchBlt (hdc, fs_cell.cx * bold_backsl, 0, fs_cell.cx, fs_cell.cy,
              hdc, fs_cell.cx * (bold_backsl + 1) - 1, 0, -fs_cell.cx, fs_cell.cy,
              SRCCOPY);

  SelectObject (hdc, of);
}

void
FontSet::paint_sep_bitmap (HDC hdc)
{
  int x = fs_cell.cx * sep + fs_cell.cx / 4;
  MoveToEx (hdc, x, 0, 0);
  LineTo (hdc, x, fs_cell.cy);
}

void
FontSet::paint_tab_bitmap (HDC hdc)
{
  int h = fs_ascent / 4;
  int x0 = fs_cell.cx * htab + (fs_cell.cx - h) / 2;
  int y0 = fs_ascent - 1;
  MoveToEx (hdc, x0, y0, 0);
  LineTo (hdc, x0 + h, y0);
  LineTo (hdc, x0, y0 - h);
  LineTo (hdc, x0, y0);
}

void
FontSet::paint_fullspc_bitmap (HDC hdc)
{
  int h = fs_ascent / 4;
  if (!h)
    h = 2;
  else if (h & 1)
    h++;
  int w = fs_size.cx * 2 * 3 / 4;
  if (!w)
    w = 2;
  else if (w & 1)
    w++;

  int x1 = fs_cell.cx * fullspc1 + (fs_size.cx * 2 - w) / 2;
  int x2 = x1 + w;
  int y1 = fs_ascent - 1;
  int y2 = fs_ascent - h;

  for (int x = x1; x < x2; x += 2)
    {
      SetPixel (hdc, x, y1, RGB (0, 0, 0));
      SetPixel (hdc, x + 1, y2, RGB (0, 0, 0));
    }
  x2--;
  for (int y = y1 - 2; y > y2; y -= 2)
    {
      SetPixel (hdc, x1, y, RGB (0, 0, 0));
      SetPixel (hdc, x2, y + 1, RGB (0, 0, 0));
    }
}

void
FontSet::paint_halfspc_bitmap (HDC hdc)
{
  int h = fs_size.cy / 5;
  if (h < 3)
    h = 3;

  MoveToEx (hdc, fs_size.cx * halfspc + 1, fs_ascent - h, 0);
  LineTo (hdc, fs_size.cx * halfspc + 1, fs_ascent - 1);
  LineTo (hdc, fs_size.cx * (halfspc + 1) - 2, fs_ascent - 1);
  LineTo (hdc, fs_size.cx * (halfspc + 1) - 2, fs_ascent - h - 1);
}

void
FontSet::paint_blank (HDC hdc)
{
  if (fs_size.cx > 2 && fs_ascent > 2)
    {
      PatBlt (hdc, fs_cell.cx * blank + 1, 1,
              fs_size.cx - 2, fs_ascent - 2, BLACKNESS);
      PatBlt (hdc, fs_cell.cx * wblank1 + 1, 1,
              fs_size.cx * 2 - 2, fs_ascent - 2, BLACKNESS);
    }
}

void
FontSet::paint_fold_bitmap (HDC hdc)
{
  int s0 = fs_cell.cx * fold_sep0;
  int s1 = fs_cell.cx * fold_sep1;
  int m0 = fs_cell.cx * fold_mark_sep0;
  int m1 = fs_cell.cx * fold_mark_sep1;

  PatBlt (hdc, s0, 0, fs_cell.cx, fs_cell.cy, WHITENESS);
  PatBlt (hdc, s1, 0, fs_cell.cx, fs_cell.cy, WHITENESS);
  PatBlt (hdc, m0, 0, fs_cell.cx, fs_cell.cy, WHITENESS);
  PatBlt (hdc, m1, 0, fs_cell.cx, fs_cell.cy, WHITENESS);

  const FontObject &f = fs_font[FONT_ASCII];
  HGDIOBJ of = SelectObject (hdc, f);
  char c = '<';
  ExtTextOut (hdc, m0 + f.offset ().x, f.offset ().y, 0, 0, &c, 1, 0);
  ExtTextOut (hdc, m1 + f.offset ().x, f.offset ().y, 0, 0, &c, 1, 0);
  SelectObject (hdc, of);

  for (int y = 0; y < fs_cell.cy; y += 2)
    {
      SetPixel (hdc, s0, y, RGB (0, 0, 0));
      SetPixel (hdc, m0, y, RGB (0, 0, 0));
    }
  for (int y = fs_cell.cy & 1; y < fs_cell.cy; y += 2)
    {
      SetPixel (hdc, s1, y, RGB (0, 0, 0));
      SetPixel (hdc, m1, y, RGB (0, 0, 0));
    }
}

void
FontSet::create_bitmap ()
{
  if (fs_hbm)
    DeleteObject (fs_hbm);
  fs_hbm = CreateBitmap (fs_cell.cx * max_bitmap, fs_cell.cy, 1, 1, 0);
  HDC hdc = GetDC (0);
  HDC hdcmem = CreateCompatibleDC (hdc);
  ReleaseDC (0, hdc);
  HGDIOBJ obm = SelectObject (hdcmem, fs_hbm);
  HGDIOBJ open = SelectObject (hdcmem, CreatePen (PS_SOLID, 0, RGB (0, 0, 0)));
  PatBlt (hdcmem, 0, 0, fs_cell.cx * max_bitmap, fs_cell.cy, WHITENESS);
  paint_newline_bitmap (hdcmem);
  paint_backsl_bitmap (hdcmem);
  paint_sep_bitmap (hdcmem);
  paint_tab_bitmap (hdcmem);
  paint_fullspc_bitmap (hdcmem);
  paint_halfspc_bitmap (hdcmem);
  paint_blank (hdcmem);
  paint_fold_bitmap (hdcmem);
  DeleteObject (SelectObject (hdcmem, open));
  SelectObject (hdcmem, obm);
  DeleteDC (hdcmem);
}

int
FontSet::create (const FontSetParam &param)
{
  SIZE ex[FONT_MAX][2];
  HDC hdc = GetDC (0);

  fs_line_spacing = max (0, min (param.fs_line_spacing, 30));
  fs_use_backsl = param.fs_use_backsl;
  fs_recommend_size = param.fs_recommend_size;
  fs_size_pixel = param.fs_size_pixel;

  if (!fs_recommend_size)
    {
      for (int i = 0; i < FONT_MAX; i++)
        fs_font[i].create (param.fs_logfont[i]);

      for (int i = 0; i < FONT_MAX; i++)
        fs_font[i].get_metrics (hdc, ex[i][0], ex[i][1]);
    }
  else
    {
      fs_font[FONT_ASCII].create (param.fs_logfont[FONT_ASCII]);
      fs_font[FONT_ASCII].get_metrics (hdc, ex[FONT_ASCII][0], ex[FONT_ASCII][1]);

      for (int i = 1; i < FONT_MAX; i++)
        for (int h = fs_font[FONT_ASCII].size ().cy; h > 0; h--)
          {
            LOGFONT lf (param.fs_logfont[i]);
            lf.lfHeight = h;
            lf.lfWidth = 0;
            fs_font[i].create (lf);
            fs_font[i].get_metrics (hdc, ex[i][0], ex[i][1]);
            if (fs_font[i].size ().cx <= fs_font[FONT_ASCII].size ().cx)
              break;
          }
    }

  fs_size = fs_font[FONT_ASCII].size ();

  for (int i = 0; i < FONT_MAX; i++)
    if (fs_font[i].size ().cx > fs_size.cx)
      {
        LOGFONT lf (param.fs_logfont[i]);
        lf.lfWidth = fs_size.cx;
        fs_font[i].create (lf);
        fs_font[i].get_metrics (hdc, ex[i][0], ex[i][1]);
      }

  ReleaseDC (0, hdc);

  fs_cell.cx = fs_size.cx;
  fs_cell.cy = fs_size.cy + fs_line_spacing;
  fs_ascent = fs_font[FONT_JP].ascent ();
  fs_line_width = fs_size.cy / 12;
  if (!fs_line_width)
    fs_line_width = 1;

  fs_need_pad = 0;
  for (int i = 0; i < FONT_MAX; i++)
    {
      fs_font[i].calc_offset (fs_size);
      if (fs_font[i].size ().cx != fs_size.cx
          || ex[i][0].cx * 2 != ex[i][0].cx)
        {
          fs_font[i].require_pad ();
          fs_need_pad = 1;
        }
    }

  create_bitmap ();
  save_params (param);
  return 1;
}

void
FontSet::save_params (const FontSetParam &param)
{
  for (int i = 0; i < FONT_MAX; i++)
    write_conf (cfgFont, regent (i), param.fs_logfont[i]);
  write_conf (cfgFont, cfgLineSpacing, param.fs_line_spacing);
  write_conf (cfgFont, cfgBackslash, param.fs_use_backsl);
  write_conf (cfgFont, cfgRecommendSize, param.fs_recommend_size);
  write_conf (cfgFont, cfgSizePixel, param.fs_size_pixel);
  flush_conf ();
}

static int CALLBACK
fix_charset_proc (ENUMLOGFONT *elf, NEWTEXTMETRIC *, int type, LPARAM lparam)
{
  HDC hdc = GetDC (0);
  FontSetParam &param = *(FontSetParam *)lparam;
  if (*elf->elfLogFont.lfFaceName != '@')
    for (int i = 0; i < FONT_MAX; i++)
      {
        if (font_exist_p (hdc, param.fs_logfont[i].lfFaceName, param.fs_logfont[i].lfCharSet))
          continue;
        if (!strcmp (elf->elfLogFont.lfFaceName, param.fs_logfont[i].lfFaceName))
          param.fs_logfont[i].lfCharSet = elf->elfLogFont.lfCharSet;
      }
  ReleaseDC (0, hdc);
  return 1;
}

void
FontSet::load_params (FontSetParam &param)
{
  bzero (&param, sizeof param);

  if (!read_conf (cfgFont, cfgLineSpacing, param.fs_line_spacing))
    param.fs_line_spacing = 0;
  if (!read_conf (cfgFont, cfgBackslash, param.fs_use_backsl))
    param.fs_use_backsl = 0;
  if (!read_conf (cfgFont, cfgRecommendSize, param.fs_recommend_size))
    param.fs_recommend_size = 0;
  if (!read_conf (cfgFont, cfgSizePixel, param.fs_size_pixel))
    param.fs_size_pixel = 0;
  for (int i = 0; i < FONT_MAX; i++)
    if (!read_conf (cfgFont, regent (i), param.fs_logfont[i]))
      *param.fs_logfont[i].lfFaceName = 0;

  for (int i = 0; i < FONT_MAX; i++)
    {
      if (!*param.fs_logfont[i].lfFaceName)
        {
          strcpy (param.fs_logfont[i].lfFaceName, default_face (i, 0));
          if (!i)
            {
              LOGFONT lf;
              GetObject (GetStockObject (SYSTEM_FIXED_FONT), sizeof lf, &lf);
              param.fs_logfont[0].lfHeight = lf.lfHeight;
            }
          else
            param.fs_logfont[i].lfHeight = param.fs_logfont[0].lfHeight;
        }
      param.fs_logfont[i].lfPitchAndFamily &= ~3;
      param.fs_logfont[i].lfPitchAndFamily |= FIXED_PITCH;
    }

  HDC hdc = GetDC (0);
  EnumFontFamiliesEx (hdc, 0, FONTENUMPROC (fix_charset_proc), LPARAM (&param), 0);
  ReleaseDC (0, hdc);
}

void
FontSet::init ()
{
  FontSetParam param;
  load_params (param);
  create (param);
}

lisp
FontSet::make_alist () const
{
  lisp r = Qnil;
  for (int i = 0; i < FONT_MAX; i++)
    {
      LOGFONT lf = font (i).logfont ();
      int size = lf.lfHeight;
      if (!size_pixel_p ())
        size = FontObject::pixel_to_point (size);
      r = xcons (make_list (FontSet::lang_key (i),
                            Kface, make_string (lf.lfFaceName),
                            Ksize, make_fixnum (size),
                            Ksize_pixel_p, boole (size_pixel_p ()),
                            0),
                 r);
    }

  return Fnreverse (r);
}

const bool
FontSet::update (FontSetParam &param, const lisp lfontset) const
{
  // Initialize FontSetParam by current setting.
  param.fs_use_backsl = use_backsl_p ();
  param.fs_line_spacing = line_spacing ();
  param.fs_recommend_size = recommend_size_p ();
  param.fs_size_pixel = size_pixel_p ();
  for (int i = 0; i < FONT_MAX; i++)
    param.fs_logfont[i] = font (i).logfont ();

  // Update FontSetParam.fs_logfont by lfontset;
  bool update = false;
  for (lisp x = lfontset; consp (x); x = xcdr (x))
    {
      check_cons (xcar (x));
      lisp llang = Fcaar (x);
      lisp keys = Fcdar (x);

      int n = FontSet::lang_key_index (llang);
      if (n < 0)
        FEsimple_error (Einvalid_charset, llang);

      if (FontObject::update (param.fs_logfont[n], keys, (llang != Kascii && recommend_size_p ())))
        update = true;
    }

  return update;
}

lisp
Fget_text_fontset ()
{
  return app.text_font.make_alist ();
}

lisp
Fset_text_fontset (lisp lfontset)
{
  check_cons (lfontset);

  FontSetParam param;
  if (!app.text_font.update (param, lfontset))
    return Qnil;

  Window::change_parameters (param);
  refresh_screen (0);

  return Qt;
}

int
get_font_height (HWND hwnd)
{
  HFONT hfont = HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0));
  HDC hdc = GetDC (hwnd);
  HGDIOBJ ofont = SelectObject (hdc, hfont);
  TEXTMETRIC tm;
  GetTextMetrics (hdc, &tm);
  SelectObject (hdc, ofont);
  ReleaseDC (hwnd, hdc);
  return tm.tmHeight;
}

static int CALLBACK
check_valid_font (const ENUMLOGFONT *, const NEWTEXTMETRIC *,
                  DWORD, LPARAM lparam)
{
  *(bool *)lparam = true;
  return 0;
}

bool
font_exist_p (const HDC hdc, const char *face, BYTE charset)
{
  bool exists = false;

  LOGFONT font;
  memset (&font, 0, sizeof LOGFONT);
  font.lfCharSet = charset;
  strcpy (font.lfFaceName, face);

  EnumFontFamiliesEx (hdc, &font,
                      FONTENUMPROC (check_valid_font),
                      LPARAM (&exists), 0);

  return exists;
}
