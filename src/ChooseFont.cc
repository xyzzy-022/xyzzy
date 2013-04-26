#include "stdafx.h"
#include "ed.h"
#include "ChooseFont.h"

ChooseFontP::ChooseFontP ()
{
  cf_hil = ImageList_LoadBitmap (app.hinst,
                                MAKEINTRESOURCE (IDB_TT),
                                18, 1, RGB (0, 0, 255));
}

ChooseFontP::~ChooseFontP ()
{
  if (cf_hil)
    ImageList_Destroy (cf_hil);
}

void
ChooseFontP::add_lang (HWND hwnd)
{
  for (int i = 0; i < FONT_MAX; i++)
    {
      char buf[128];
      *buf = 0;
      LoadString (app.hinst, FontSet::lang_id (i), buf, sizeof buf);
      int idx = SendDlgItemMessage (hwnd, IDC_LANG, CB_ADDSTRING, 0, LPARAM (buf));
      SendDlgItemMessage (hwnd, IDC_LANG, CB_SETITEMDATA, idx, i);
    }
}

int CALLBACK
ChooseFontP::enum_font_name_proc (ENUMLOGFONT *elf, NEWTEXTMETRIC *, int type, LPARAM lparam)
{
  if (*elf->elfLogFont.lfFaceName != '@'
      && (elf->elfLogFont.lfPitchAndFamily & 3) == FIXED_PITCH)
    {
      HWND hwnd = HWND (lparam);
      if (SendMessage (hwnd, LB_FINDSTRINGEXACT, WPARAM (-1), LPARAM (elf->elfLogFont.lfFaceName)) == LB_ERR)
        {
          int i = SendMessage (hwnd, LB_ADDSTRING, 0, LPARAM (elf->elfLogFont.lfFaceName));
          SendMessage (hwnd, LB_SETITEMDATA, i, (elf->elfLogFont.lfCharSet << 8) | type);
        }
    }
  return 1;
}

void
ChooseFontP::add_font_name (HWND hwnd, HDC hdc)
{
  EnumFontFamiliesEx (hdc, 0, FONTENUMPROC (enum_font_name_proc),
                      LPARAM (GetDlgItem (hwnd, IDC_NAMELIST)), 0);
}

int CALLBACK
ChooseFontP::enum_font_size_proc (ENUMLOGFONT *elf, NEWTEXTMETRIC *, int type, LPARAM lparam)
{
  HWND hwnd = ((xdpi *)lparam)->hwnd;
  int dpi = ((xdpi *)lparam)->dpi;
  int pixel = ((xdpi *)lparam)->pixel;
  char b[16];
  if (type & TRUETYPE_FONTTYPE)
    {
      if (!pixel)
        {
          static const int tt[] =
            {6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 18, 20, 22, 24, 26, 28, 36,};
          if (SendMessage (hwnd, LB_FINDSTRINGEXACT, WPARAM (-1), LPARAM ("  6")) == LB_ERR)
            for (int i = 0; i < numberof (tt); i++)
              {
                sprintf (b, "%3d", tt[i]);
                SendMessage (hwnd, LB_ADDSTRING, 0, LPARAM (b));
              }
        }
      else
        {
          if (SendMessage (hwnd, LB_FINDSTRINGEXACT, WPARAM (-1), LPARAM ("  8")) == LB_ERR)
            for (int i = FONT_SIZE_MIN_PIXEL; i <= FONT_SIZE_MAX_PIXEL; i++)
              {
                sprintf (b, "%3d", i);
                SendMessage (hwnd, LB_ADDSTRING, 0, LPARAM (b));
              }
        }
    }
  else
    {
      sprintf (b, "%3d", (pixel ? elf->elfLogFont.lfHeight
                          : MulDiv (elf->elfLogFont.lfHeight, 72, dpi)));
      if (SendMessage (hwnd, LB_FINDSTRINGEXACT,
                       WPARAM (-1), LPARAM (b)) == LB_ERR)
        SendMessage (hwnd, LB_ADDSTRING, 0, LPARAM (b));
    }
  return 1;
}

void
ChooseFontP::add_font_size (HWND hwnd, int i)
{
  char face[LF_FACESIZE];
  if (SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETTEXT, i, LPARAM (face)) == LB_ERR)
    return;
  SendDlgItemMessage (hwnd, IDC_SIZELIST, WM_SETREDRAW, 0, 0);
  SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_RESETCONTENT, 0, 0);

  xdpi x;
  x.hwnd = GetDlgItem (hwnd, IDC_SIZELIST);
  x.dpi = cf_dpi;
  x.pixel = cf_param.fs_size_pixel;

  HDC hdc = GetDC (hwnd);
  EnumFontFamilies (hdc, face, FONTENUMPROC (enum_font_size_proc), LPARAM (&x));
  ReleaseDC (hwnd, hdc);

  SendDlgItemMessage (hwnd, IDC_SIZELIST, WM_SETREDRAW, 1, 0);
  InvalidateRect (GetDlgItem (hwnd, IDC_SIZELIST), 0, 0);
}

void
ChooseFontP::change_font_size (HWND hwnd, int size)
{
  int i = SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETCURSEL, 0, 0);
  if (i == LB_ERR)
    return;

  add_font_size (hwnd, i);

  struct {int index, point;} min, max;
  min.index = max.index = -1;

  char b[16];
  int n = SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETCOUNT, 0, 0);
  for (i = 0; i < n; i++)
    if (SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETTEXT, i, LPARAM (b)) != LB_ERR)
      {
        int x = atoi (b);
        if (x <= size && (min.index == -1 || x > min.point))
          {
            min.index = i;
            min.point = x;
          }
        if (x >= size && (max.index == -1 || x < max.point))
          {
            max.index = i;
            max.point = x;
          }
      }

  SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_SETCURSEL,
                      ((min.index == -1 && max.index == -1)
                       ? 0
                       : (min.index == -1
                          ? max.index
                          : (max.index == -1
                             ? min.index
                             : (size - min.point <= max.point - size
                                ? min.index : max.index)))),
                      0);

  notify_font_size (hwnd, LBN_SELCHANGE);
}

void
ChooseFontP::notify_lang (HWND hwnd, int code)
{
  if (code != CBN_SELCHANGE)
    return;
  int i = SendDlgItemMessage (hwnd, IDC_LANG, CB_GETCURSEL, 0, 0);
  if (i == LB_ERR)
    return;
  i = SendDlgItemMessage (hwnd, IDC_LANG, CB_GETITEMDATA, i, 0);
  if (i < 0 || i >= FONT_MAX)
    return;

  int j = SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_FINDSTRINGEXACT,
                              WPARAM (-1), LPARAM (cf_param.fs_logfont[i].lfFaceName));
  if (j == LB_ERR)
    j = 0;
  SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_SETCURSEL, j, 0);

  change_font_size (hwnd,
                    (cf_param.fs_size_pixel
                     ? cf_param.fs_logfont[i].lfHeight
                     : MulDiv (cf_param.fs_logfont[i].lfHeight, 72, cf_dpi)));
}

void
ChooseFontP::notify_font_name (HWND hwnd, int code)
{
  if (code != LBN_SELCHANGE)
    return;
  int i = SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETCURSEL, 0, 0);
  if (i == LB_ERR)
    return;
  char b[16];
  if (SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETTEXT, i, LPARAM (b)) == LB_ERR)
    return;
  change_font_size (hwnd, atoi (b));
}

void
ChooseFontP::notify_font_size (HWND hwnd, int code)
{
  if (code != LBN_SELCHANGE)
    return;

  int lang = SendDlgItemMessage (hwnd, IDC_LANG, CB_GETCURSEL, 0, 0);
  if (lang == LB_ERR)
    return;
  lang = SendDlgItemMessage (hwnd, IDC_LANG, CB_GETITEMDATA, lang, 0);
  if (lang < 0 || lang >= FONT_MAX)
    return;

  int i = SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETCURSEL, 0, 0);
  if (i == LB_ERR)
    return;
  char name[LF_FACESIZE];
  if (SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETTEXT, i, LPARAM (name)) == LB_ERR)
    return;

  int j = SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETCURSEL, 0, 0);
  if (j == LB_ERR)
    return;
  char b[16];
  if (SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETTEXT, j, LPARAM (b)) == LB_ERR)
    return;

  BYTE charset = BYTE (SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETITEMDATA, i, 0) >> 8);
  LOGFONT lf;
  bzero (&lf, sizeof lf);
  lf.lfHeight = cf_param.fs_size_pixel ? atoi (b) : MulDiv (atoi (b), cf_dpi, 72);
  lf.lfCharSet = charset;
  strcpy (lf.lfFaceName, name);

  cf_param.fs_logfont[lang] = lf;

  HFONT hfont = CreateFontIndirect (&lf);
  HFONT hfdlg = HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0));
  HFONT hfctl = HFONT (SendDlgItemMessage (hwnd, IDC_SAMPLE, WM_GETFONT, 0, 0));
  SendDlgItemMessage (hwnd, IDC_SAMPLE, WM_SETFONT, WPARAM (hfont), MAKELPARAM (0, 0));
  if (hfctl != hfdlg)
    DeleteObject (hfctl);
  InvalidateRect (GetDlgItem (hwnd, IDC_SAMPLE), 0, 0);
}

void
ChooseFontP::notify_size_pixel (HWND hwnd, int code)
{
  if (code != BN_CLICKED)
    return;
  int i = SendDlgItemMessage (hwnd, IDC_SIZE_PIXEL, BM_GETCHECK, 0, 0);
  if (i != cf_param.fs_size_pixel)
    {
      cf_param.fs_size_pixel = i;

      int i = SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETCURSEL, 0, 0);
      if (i == LB_ERR)
        return;
      char b[16];
      if (SendDlgItemMessage (hwnd, IDC_SIZELIST, LB_GETTEXT, i, LPARAM (b)) == LB_ERR)
        return;
      int sz = atoi (b);
      if (cf_param.fs_size_pixel)
        sz = MulDiv (sz, cf_dpi, 72);
      else
        sz = MulDiv (sz, 72, cf_dpi);
      change_font_size (hwnd, sz);
    }
}

void
ChooseFontP::draw_font_list (HWND, DRAWITEMSTRUCT *dis)
{
  COLORREF ofg, obg;

  if (dis->itemState & ODS_SELECTED)
    {
      ofg = SetTextColor (dis->hDC, sysdep.highlight_text);
      obg = SetBkColor (dis->hDC, sysdep.highlight);
    }
  else
    {
      ofg = SetTextColor (dis->hDC, sysdep.window_text);
      obg = SetBkColor (dis->hDC, sysdep.window);
    }

  const RECT &r = dis->rcItem;
  if (dis->itemID != UINT (-1))
    {
      char b[LF_FACESIZE];
      *b = 0;
      SendMessage (dis->hwndItem, LB_GETTEXT, dis->itemID, LPARAM (b));

      SIZE size;
      GetTextExtentPoint32 (dis->hDC, "0", 1, &size);

      ExtTextOut (dis->hDC, r.left + 18, (r.top + r.bottom - size.cy) / 2,
                  ETO_OPAQUE, &r, b, strlen (b), 0);

      if (dis->itemData & TRUETYPE_FONTTYPE)
        ImageList_Draw (cf_hil, 0, dis->hDC,
                        r.left, (r.top + r.bottom - 12) / 2, ILD_TRANSPARENT);
    }

  if (dis->itemState & ODS_FOCUS)
    DrawFocusRect (dis->hDC, &r);
  SetTextColor (dis->hDC, ofg);
  SetBkColor (dis->hDC, obg);
}

static const struct {BYTE charset; const char *string;} samples[] =
{
  {0, "AaBbCcXxYyZz"},
  {SHIFTJIS_CHARSET, "Aa\x82\xa0\x82\x9f\x83\x41\x83\x40\x88\x9f\x89\x46"},
  {CHINESEBIG5_CHARSET, "Aa\xa4\x40\xa4\x41\xc9\x40\xc9\x41"},
  {GB2312_CHARSET, "AaBb\xb0\xa1\xb0\xa2"},
  {HANGEUL_CHARSET, "Aa\xb0\xa1\xb0\xa2\xca\xa1\xca\xa2"},
  {HEBREW_CHARSET, "AaBb\xe0\xe1\xf9\xfa"},
  {ARABIC_CHARSET, "AaBb\xc7\xc8\xe7\xe8"},
  {GREEK_CHARSET, "AaBb\xc1\xe1\xc2\xe2"},
  {TURKISH_CHARSET, "AaBb\xc0\xe0\xde\xfe\xdf"},
  {RUSSIAN_CHARSET, "AaBb\xc0\xe0\xdf\xff"},
  {BALTIC_CHARSET, "AaBb\xc0\xe0\xdd\xfd"},
};

void
ChooseFontP::draw_sample (HWND hwnd, DRAWITEMSTRUCT *dis)
{
  const char *sample = samples[0].string;
  int i = SendDlgItemMessage (hwnd, IDC_NAMELIST, LB_GETCURSEL, 0, 0);
  if (i != LB_ERR)
    {
      BYTE charset = BYTE (SendDlgItemMessage (hwnd, IDC_NAMELIST,
                                               LB_GETITEMDATA, i, 0) >> 8);
      for (int i = 0; i < numberof (samples); i++)
        if (charset == samples[i].charset)
          {
            sample = samples[i].string;
            break;
          }
    }

  HFONT hf = HFONT (SendMessage (dis->hwndItem, WM_GETFONT, 0, 0));
  HGDIOBJ of = SelectObject (dis->hDC, hf);
  COLORREF ofg = SetTextColor (dis->hDC, cf_fg);
  COLORREF obg = SetBkColor (dis->hDC, cf_bg);
  int l = strlen (sample);
  SIZE size = {0};
  GetTextExtentPoint32 (dis->hDC, sample, l, &size);
  const RECT &r = dis->rcItem;
  ExtTextOut (dis->hDC, (r.left + r.right - size.cx) / 2,
              (r.top + r.bottom - size.cy) / 2,
              ETO_CLIPPED | ETO_OPAQUE, &r, sample, l, 0);

  SetTextColor (dis->hDC, ofg);
  SetBkColor (dis->hDC, obg);
  SelectObject (dis->hDC, of);

  paint_button_on (dis->hDC, r);
}

int
ChooseFontP::draw_item (HWND hwnd, int id, DRAWITEMSTRUCT *dis)
{
  switch (id)
    {
    case IDC_NAMELIST:
      draw_font_list (hwnd, dis);
      return 1;

    case IDC_SAMPLE:
      draw_sample (hwnd, dis);
      return 1;

    default:
      return 0;
    }
}

void
ChooseFontP::init_dialog (HWND hwnd)
{
  add_lang (hwnd);
  SendDlgItemMessage (hwnd, IDC_LANG, CB_SETCURSEL, 0, 0);

  HDC hdc = GetDC (hwnd);
  cf_dpi = GetDeviceCaps (hdc, LOGPIXELSY);
  add_font_name (hwnd, hdc);
  ReleaseDC (hwnd, hdc);

  SendDlgItemMessage (hwnd, IDC_SIZE_PIXEL, BM_SETCHECK,
                      cf_param.fs_size_pixel ? 1 : 0, 0);

  notify_lang (hwnd, LBN_SELCHANGE);
}

int
ChooseFontP::do_command (HWND hwnd, int id, int code)
{
  switch (id)
    {
    case IDC_LANG:
      notify_lang (hwnd, code);
      return 1;

    case IDC_NAMELIST:
      notify_font_name (hwnd, code);
      return 1;

    case IDC_SIZELIST:
      notify_font_size (hwnd, code);
      return 1;

    case IDC_SIZE_PIXEL:
      notify_size_pixel (hwnd, code);
      return 1;

    default:
      return 0;
    }
}

void
ChooseFontP::do_destroy (HWND hwnd)
{
  HFONT hfdlg = HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0));
  HFONT hfctl = HFONT (SendDlgItemMessage (hwnd, IDC_SAMPLE, WM_GETFONT, 0, 0));
  if (hfctl != hfdlg)
    DeleteObject (hfctl);
}

void
ChooseFontP::set_color (HWND hwnd, COLORREF fg, COLORREF bg)
{
  cf_fg = fg;
  cf_bg = bg;
  InvalidateRect (GetDlgItem (hwnd, IDC_SAMPLE), 0, 0);
}
