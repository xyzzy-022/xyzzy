#include "stdafx.h"
#include "ed.h"
#include "print.h"
#include "preview.h"
#include "conf.h"
#include "version.h"

HGLOBAL printer_device::pd_devmode;
HGLOBAL printer_device::pd_devnames;

static const char default_header[] = "%F%l%r%:w, %0d %:m %Y %0h:%0M:%0s";
static const char default_footer[] = "- %p -";

print_settings::print_settings ()
{
  ps_text_margin_mm.left = 159;
  ps_text_margin_mm.top = 286;
  ps_text_margin_mm.right = 159;
  ps_text_margin_mm.bottom = 286;
  ps_header_offset_mm = 159;
  ps_footer_offset_mm = 159;
  ps_column_sep_mm = 95;
  ps_line_spacing_pt = 0;
  ps_print_linenum = 0;
  ps_multi_column = 1;
  ps_fold_width = 0;
  *ps_header = 0;
  *ps_footer = 0;
  ps_header_on = 1;
  ps_footer_on = 1;
  ps_collate = 0;
  ps_ncopies = 1;
  ps_recommend_size = 1;
  ps_show_proportional = 1;
  ps_use_bitmap = 0;

  ps_print_range = RANGE_ALL;
  ps_range_start = -1;
  ps_range_end = -1;

  bzero (&ps_font, sizeof ps_font);
  init_faces ();
}

void
print_settings::init_faces ()
{
  for (int i = 0; i < FONT_MAX; i++)
    if (!*ps_font[i].face)
      {
        strcpy (ps_font[i].face, FontSet::default_face (i, 1));
        ps_font[i].charset = FontSet::default_charset (i);
        ps_font[i].point = 100;
        ps_font[i].bold = 0;
        ps_font[i].italic = 0;
      }
}

void
print_settings::load_conf ()
{
  RECT r;
  int x;

  if (read_conf (cfgPrint, cfgTextMargin, r))
    ps_text_margin_mm = r;
  if (read_conf (cfgPrint, cfgHeaderOffset, x))
    ps_header_offset_mm = x;
  if (read_conf (cfgPrint, cfgFooterOffset, x))
    ps_footer_offset_mm = x;
  if (read_conf (cfgPrint, cfgColumnSep, x))
    ps_column_sep_mm = x;
  if (read_conf (cfgPrint, cfgLineSpacing, x) && x >= 0)
    ps_line_spacing_pt = x;
  if (read_conf (cfgPrint, cfgLineNumber, x))
    ps_print_linenum = x;
  if (read_conf (cfgPrint, cfgColumns, x) && x >= 1 && x <= COLUMN_MAX)
    ps_multi_column = x;
  if (read_conf (cfgPrint, cfgFoldColumns, x) && x >= 0)
    ps_fold_width = x;
  if (!read_conf (cfgPrint, cfgHeader, ps_header, sizeof ps_header))
    strcpy (ps_header, default_header);
  if (!read_conf (cfgPrint, cfgFooter, ps_footer, sizeof ps_footer))
    strcpy (ps_footer, default_footer);
  if (read_conf (cfgPrint, cfgHeaderOn, x))
    ps_header_on = x ? 1 : 0;
  if (read_conf (cfgPrint, cfgFooterOn, x))
    ps_footer_on = x ? 1 : 0;
  if (!read_conf (cfgPrint, cfgRecommendSize, ps_recommend_size))
    ps_recommend_size = 1;
  if (!read_conf (cfgPrint, cfgShowProportional, ps_show_proportional))
    ps_show_proportional = 1;
  if (!read_conf (cfgPrint, cfgUseBitmap, ps_use_bitmap))
    ps_use_bitmap = 0;
  for (int i = 0; i < FONT_MAX; i++)
    if (!read_conf (cfgPrint, FontSet::regent (i), ps_font[i]))
      *ps_font[i].face = 0;
  init_faces ();
}

void
print_settings::save_conf ()
{
  write_conf (cfgPrint, cfgTextMargin, ps_text_margin_mm);
  write_conf (cfgPrint, cfgHeaderOffset, ps_header_offset_mm);
  write_conf (cfgPrint, cfgFooterOffset, ps_footer_offset_mm);
  write_conf (cfgPrint, cfgColumnSep, ps_column_sep_mm);
  write_conf (cfgPrint, cfgLineSpacing, ps_line_spacing_pt);
  write_conf (cfgPrint, cfgLineNumber, ps_print_linenum);
  write_conf (cfgPrint, cfgColumns, ps_multi_column);
  write_conf (cfgPrint, cfgFoldColumns, ps_fold_width);
  conf_write_string (cfgPrint, cfgHeader, ps_header);
  conf_write_string (cfgPrint, cfgFooter, ps_footer);
  write_conf (cfgPrint, cfgHeaderOn, ps_header_on);
  write_conf (cfgPrint, cfgFooterOn, ps_footer_on);
  write_conf (cfgPrint, cfgRecommendSize, ps_recommend_size);
  write_conf (cfgPrint, cfgShowProportional, ps_show_proportional);
  write_conf (cfgPrint, cfgUseBitmap, ps_use_bitmap);
  for (int i = 0; i < FONT_MAX; i++)
    write_conf (cfgPrint, FontSet::regent (i), ps_font[i]);

  flush_conf ();
}

void
print_settings::calc_pxl (const printer_device &dev)
{
  ps_text_margin_pxl.left = dev.mm2xpxl (ps_text_margin_mm.left);
  ps_text_margin_pxl.top = dev.mm2ypxl (ps_text_margin_mm.top);
  ps_text_margin_pxl.right = dev.mm2xpxl (ps_text_margin_mm.right);
  ps_text_margin_pxl.bottom = dev.mm2ypxl (ps_text_margin_mm.bottom);
  ps_header_offset_pxl = dev.mm2ypxl (ps_header_offset_mm);
  ps_footer_offset_pxl = dev.mm2ypxl (ps_footer_offset_mm);
  ps_column_sep_pxl = dev.mm2xpxl (ps_column_sep_mm);
  ps_line_spacing_pxl = dev.pt2ypxl (ps_line_spacing_pt);
}

int CALLBACK
print_settings::check_valid_font (const ENUMLOGFONT *, const NEWTEXTMETRIC *,
                                  DWORD, LPARAM lparam)
{
  *(int *)lparam = 1;
  return 0;
}

HFONT
print_settings::make_font (HDC hdc, int charset, int height) const
{
  if (charset != FONT_ASCII)
    {
      int exists = 0;
      EnumFontFamilies (hdc, ps_font[charset].face,
                        FONTENUMPROC (check_valid_font),
                        LPARAM (&exists));
      if (!exists)
        charset = FONT_ASCII;
    }

  LOGFONT lf;
  bzero (&lf, sizeof lf);
  strcpy (lf.lfFaceName, ps_font[charset].face);
  lf.lfHeight = height;
  lf.lfCharSet = ps_font[charset].charset;
  lf.lfItalic = ps_font[charset].italic;
  if (ps_font[charset].bold)
    lf.lfWeight = 700;

  return CreateFontIndirect (&lf);
}

printer_device::printer_device ()
     : pd_hdc (0)
{
}

printer_device::~printer_device ()
{
  if (pd_hdc)
    DeleteDC (pd_hdc);
}

void
printer_device::notice_pderr (int e)
{
  message_code m;
  switch (e)
    {
    default:
      m = EPDERR_INITFAILURE;
      break;
    case PDERR_LOADDRVFAILURE:
      m = EPDERR_LOADDRVFAILURE;
      break;
    case PDERR_NODEFAULTPRN:
      m = EPDERR_NODEFAULTPRN;
      break;
    case PDERR_NODEVICES:
      m = EPDERR_NODEVICES;
      break;
    case PDERR_PRINTERNOTFOUND:
      m = EPDERR_PRINTERNOTFOUND;
      break;
    }
  warn_msgbox (m);
}

int
printer_device::do_print_dialog1 (PRINTDLG &pd)
{
  while (1)
    {
      if (PrintDlg (&pd))
        return IDOK;

      DWORD e = CommDlgExtendedError ();
      switch (e)
        {
        case 0:
          return IDCANCEL;

        case PDERR_DEFAULTDIFFERENT:
          if (pd.hDevNames)
            {
              DEVNAMES *dn = (DEVNAMES *)GlobalLock (pd.hDevNames);
              if (dn)
                {
                  WORD f = dn->wDefault & DN_DEFAULTPRN;
                  dn->wDefault &= ~DN_DEFAULTPRN;
                  GlobalUnlock (pd.hDevNames);
                  if (f)
                    break;
                }
            }
          /* fall thru... */
        case PDERR_DNDMMISMATCH:
        case PDERR_LOADDRVFAILURE:
        case PDERR_PRINTERNOTFOUND:
          if (!pd.hDevNames)
            return e;
          GlobalFree (pd.hDevNames);
          pd.hDevNames = 0;
          pd_devnames = 0;
          if (pd.hDevMode)
            GlobalFree (pd.hDevMode);
          pd.hDevMode = 0;
          pd_devmode = 0;
          break;

        default:
          return e;
        }
    }
}

int
printer_device::do_print_dialog (PRINTDLG &pd)
{
  pd.hDevNames = pd_devnames;
  pd.hDevMode = pd_devmode;
  int e = do_print_dialog1 (pd);
  if (e == IDOK)
    {
      if (pd_devnames != pd.hDevNames)
        {
          if (pd_devnames)
            GlobalFree (pd_devnames);
          pd_devnames = pd.hDevNames;
        }
      if (pd_devmode != pd.hDevMode)
        {
          if (pd_devmode)
            GlobalFree (pd_devmode);
          pd_devmode = pd.hDevMode;
        }
    }
  else
    {
      if (pd.hDevNames && pd.hDevNames != pd_devnames)
        GlobalFree (pd.hDevNames);
      if (pd.hDevMode && pd.hDevMode != pd_devmode)
        GlobalFree (pd.hDevMode);
      if (e != IDCANCEL)
        notice_pderr (e);
    }
  return e;
}

int
printer_device::get_defaults ()
{
  if (pd_devnames)
    return 1;

  PRINTDLG pd;
  bzero (&pd, sizeof pd);
  pd.lStructSize = sizeof pd;
  pd.Flags = PD_RETURNDEFAULT;

  return do_print_dialog (pd) == IDOK;
}

HDC
printer_device::create_dc ()
{
  if (!pd_devnames)
    return 0;

  DEVNAMES *dn = (DEVNAMES *)GlobalLock (pd_devnames);
  if (!dn)
    return 0;
  DEVMODE *dm = pd_devmode ? (DEVMODE *)GlobalLock (pd_devmode) : 0;

  HDC hdc = CreateDC ((const char *)dn + dn->wDriverOffset,
                      (const char *)dn + dn->wDeviceOffset,
                      (const char *)dn + dn->wOutputOffset,
                      dm);
  GlobalUnlock (pd_devnames);
  if (dm)
    GlobalUnlock (pd_devmode);
  return hdc;
}

void
printer_device::get_dev_spec ()
{
  pd_xdpi = GetDeviceCaps (pd_hdc, LOGPIXELSX);
  pd_ydpi = GetDeviceCaps (pd_hdc, LOGPIXELSY);
  pd_size.cx = GetDeviceCaps (pd_hdc, HORZRES);
  pd_size.cy = GetDeviceCaps (pd_hdc, VERTRES);
  pd_physsize_pxl.cx = GetDeviceCaps (pd_hdc, PHYSICALWIDTH);
  pd_physsize_pxl.cy = GetDeviceCaps (pd_hdc, PHYSICALHEIGHT);
  pd_min_margin_pxl.left = GetDeviceCaps (pd_hdc, PHYSICALOFFSETX);
  pd_min_margin_pxl.top = GetDeviceCaps (pd_hdc, PHYSICALOFFSETY);
  pd_min_margin_pxl.right = pd_physsize_pxl.cx - (pd_min_margin_pxl.left + pd_size.cx);
  pd_min_margin_pxl.bottom = pd_physsize_pxl.cy - (pd_min_margin_pxl.top + pd_size.cy);

  pd_physsize_mm.cx = xpxl2mm (pd_physsize_pxl.cx);
  pd_physsize_mm.cy = ypxl2mm (pd_physsize_pxl.cy);
  pd_min_margin_mm.left = xpxl2mm (pd_min_margin_pxl.left);
  pd_min_margin_mm.top = ypxl2mm (pd_min_margin_pxl.top);
  pd_min_margin_mm.right = xpxl2mm (pd_min_margin_pxl.right);
  pd_min_margin_mm.bottom = ypxl2mm (pd_min_margin_pxl.bottom);

  pd_max_copies = 1;
  pd_dm_fields = 0;

  if (pd_devnames)
    {
      DEVNAMES *dn = (DEVNAMES *)GlobalLock (pd_devnames);
      if (dn)
        {
          DEVMODE *dm = pd_devmode ? (DEVMODE *)GlobalLock (pd_devmode) : 0;
          pd_max_copies =
            DeviceCapabilities ((const char *)dn + dn->wDeviceOffset,
                                (const char *)dn + dn->wOutputOffset,
                                DC_COPIES, 0, dm);
          pd_dm_fields =
            DeviceCapabilities ((const char *)dn + dn->wDeviceOffset,
                                (const char *)dn + dn->wOutputOffset,
                                DC_FIELDS, 0, dm);
          if (dm)
            GlobalUnlock (pd_devmode);
          GlobalUnlock (pd_devnames);
        }
    }
}

int
printer_device::init ()
{
  HGLOBAL odevnames = pd_devnames;

  if (!get_defaults ())
    return 0;

  pd_hdc = create_dc ();
  if (!pd_hdc)
    {
      if (!odevnames)
        return 0;

      GlobalFree (pd_devnames);
      pd_devnames = 0;
      if (pd_devmode)
        {
          GlobalFree (pd_devmode);
          pd_devmode = 0;
        }

      if (!get_defaults ())
        return 0;
      pd_hdc = create_dc ();
      if (!pd_hdc)
        return 0;
    }
  get_dev_spec ();
  return 1;
}

int
printer_device::create_printer_dc ()
{
  HDC hdc = create_dc ();
  if (!hdc)
    return 0;
  DeleteDC (pd_hdc);
  pd_hdc = hdc;
  get_dev_spec ();
  return 1;
}

int
printer_device::print_setup_dialog (HWND hwnd)
{
  PRINTDLG pd;
  bzero (&pd, sizeof pd);
  pd.lStructSize = sizeof pd;
  pd.hwndOwner = hwnd;
  pd.Flags = PD_PRINTSETUP;
  if (do_print_dialog (pd) != IDOK)
    return 0;
  return create_printer_dc ();
}

void
printer_device::set_dev_copies (const print_settings &ps)
{
  if (pd_devmode)
    {
      DEVMODE *dm = (DEVMODE *)GlobalLock (pd_devmode);
      if (dm)
        {
          dm->dmCopies = 1;
          dm->dmCollate = DMCOLLATE_FALSE;
          if (pd_max_copies > 1 && ps.ps_ncopies > 0)
            {
              dm->dmFields |= DM_COPIES;
              dm->dmCopies = ps.ps_ncopies;
              if (pd_dm_fields & DM_COLLATE)
                {
                  dm->dmFields |= DM_COLLATE;
                  dm->dmCollate = (ps.ps_collate
                                   ? DMCOLLATE_TRUE
                                   : DMCOLLATE_FALSE);
                }
              else
                dm->dmFields &= ~DM_COLLATE;
            }
          else
            dm->dmFields &= ~(DM_COPIES | DM_COLLATE);
          GlobalUnlock (pd_devmode);
        }
    }
}

void
printer_device::get_dev_copies (print_settings &ps)
{
  if (pd_devmode)
    {
      DEVMODE *dm = (DEVMODE *)GlobalLock (pd_devmode);
      if (dm)
        {
          ps.ps_ncopies = dm->dmFields & DM_COPIES ? dm->dmCopies : 1;
          ps.ps_collate = (dm->dmFields & DM_COLLATE
                           ? dm->dmCollate == DMCOLLATE_TRUE
                           : 0);
          GlobalUnlock (pd_devmode);
        }
    }
}

///////////////////////////////////////////////



#include "lucida-width.h"


#define LINENUM_WIDTH 6

print_engine::print_engine (Buffer *bp, const printer_device &dev,
                            print_settings &settings)
     : pe_bp (bp), pe_dev (dev), pe_settings (settings),
       pe_hbm (0)
{
  for (int i = 0; i < FONT_MAX; i++)
    pe_hfonts[i] = 0;
}

void
print_engine::cleanup ()
{
  for (int i = 0; i < FONT_MAX; i++)
    if (pe_hfonts[i])
      {
        DeleteObject (pe_hfonts[i]);
        pe_hfonts[i] = 0;
      }
  if (pe_hbm)
    {
      DeleteObject (pe_hbm);
      pe_hbm = 0;
    }
  pe_cache.cleanup ();
}

print_engine::~print_engine ()
{
  cleanup ();
}

void
print_engine::init_font (HDC hdc)
{
  for (int i = 0; i < FONT_MAX; i++)
    pe_hfonts[i] = pe_settings.make_font (hdc, pe_dev, i);

  HGDIOBJ of = SelectObject (hdc, pe_hfonts[FONT_ASCII]);

  TEXTMETRIC tm;
  GetTextMetrics (hdc, &tm);
  pe_cell.cx = tm.tmAveCharWidth;
  pe_cell.cy = tm.tmHeight;

  pe_print_cell = pe_cell;
  pe_print_cell.cy += pe_settings.ps_line_spacing_pxl;

  pe_fixed_pitch = 1;

  for (int i = 0; i < FONT_MAX; i++)
    {
      SelectObject (hdc, pe_hfonts[i]);
      GetTextMetrics (hdc, &tm);
      pe_offset[i].x = (pe_cell.cx - tm.tmAveCharWidth) / 2;
      pe_offset2x[i] = pe_cell.cx - tm.tmAveCharWidth;
      pe_offset[i].y = (pe_cell.cy - tm.tmHeight) / 2;
      if (tm.tmPitchAndFamily & TMPF_FIXED_PITCH)
        pe_fixed_pitch = 0;
    }

  if (!pe_fixed_pitch)
    {
      int i;
      for (i = 0; i < FONT_MAX; i++)
        pe_offset[i].x = pe_offset2x[i] = 0;

      pe_glyph_width.hdc = hdc;
      pe_glyph_width.hfonts = pe_hfonts;
      pe_glyph_width.height = pe_print_cell.cy;
      for (i = 0; i < numberof (pe_glyph_width.pixel); i++)
        pe_glyph_width.pixel[i] = -1;
      for (i = ' '; i < CC_DEL; i++)
        get_glyph_width (i, pe_glyph_width);
      for (i = 0; i < ' '; i++)
        pe_glyph_width.pixel[i] = (get_glyph_width ('^', pe_glyph_width)
                                   + get_glyph_width ('@' + i, pe_glyph_width));
      pe_glyph_width.pixel[CC_DEL] = (get_glyph_width ('^', pe_glyph_width)
                                      + get_glyph_width ('?' + i, pe_glyph_width));
    }

  SelectObject (hdc, of);
}

int
print_engine::init_area (HDC hdc)
{
  pe_area.left = pe_settings.ps_text_margin_pxl.left;
  pe_area.top = pe_settings.ps_text_margin_pxl.top;
  pe_area.right = pe_dev.physsize_pxl ().cx - pe_settings.ps_text_margin_pxl.right;
  pe_area.bottom = pe_dev.physsize_pxl ().cy - pe_settings.ps_text_margin_pxl.bottom;

  pe_sep_pxl = pe_settings.ps_column_sep_pxl;
  pe_page_width = ((pe_area.right - pe_area.left
                    - (pe_settings.ps_multi_column - 1) * pe_sep_pxl)
                   / pe_settings.ps_multi_column);

  if (pe_fixed_pitch)
    {
      int max_chars;
      pe_ech.cx = pe_page_width / pe_print_cell.cx;
      if (!pe_settings.ps_fold_width)
        {
          pe_fold_columns = pe_ech.cx;
          if (pe_settings.ps_print_linenum)
            pe_fold_columns -= LINENUM_WIDTH + 1;
          if (pe_fold_columns < 4)
            return WIDTH_TOO_SMALL;
          max_chars = pe_ech.cx;
        }
      else
        {
          pe_fold_columns = pe_settings.ps_fold_width;
          if (pe_fold_columns < 4)
            return FOLD_TOO_SMALL;
          int extra = pe_settings.ps_print_linenum ? LINENUM_WIDTH + 1 : 0;
          if (pe_ech.cx < extra + 4)
            return WIDTH_TOO_SMALL;
          max_chars = pe_fold_columns + extra;
          if (max_chars > pe_ech.cx)
            return FOLD_TOO_LARGE | (pe_ech.cx - extra);
        }

      int right_margin = (pe_settings.ps_text_margin_pxl.right
                          - pe_dev.min_margin_pxl ().right);
      if (pe_settings.ps_multi_column > 1
          && pe_settings.ps_column_sep_pxl < right_margin)
        right_margin = pe_settings.ps_column_sep_pxl;
      int extend = ((pe_page_width + right_margin) / pe_print_cell.cx - max_chars) / 2;
      pe_fold_param.extend_limit = max (0, min (pe_fold_param.extend_limit, extend));
    }
  else
    {
      pe_linenum_width = 0;
      pe_start_pixel = 0;
      pe_copying_width = pe_page_width;
      pe_digit_width = 0;
      if (pe_settings.ps_print_linenum)
        {
          for (int i = '0'; i <= '9'; i++)
            pe_digit_width = max (pe_digit_width, (int)pe_glyph_width.pixel[i]);
          pe_linenum_width = pe_digit_width * LINENUM_WIDTH;
          pe_start_pixel = pe_linenum_width + pe_glyph_width.pixel['m'];
          pe_copying_width -= pe_start_pixel;
        }
      if (pe_copying_width / pe_print_cell.cx < 4)
        return WIDTH_TOO_SMALL;
    }

  pe_ech.cy = (pe_area.bottom - pe_area.top
               + pe_settings.ps_line_spacing_pxl) / pe_print_cell.cy;
  if (pe_ech.cy <= 0)
    return HEIGHT_TOO_SMALL;

  pe_header.y = pe_settings.ps_header_offset_pxl;
  if (*pe_settings.ps_header && pe_settings.ps_header_on
      && pe_header.y + pe_cell.cy > pe_area.top)
    return HEADER_OFFSET_TOO_LARGE;

  pe_footer.y = (pe_dev.physsize_pxl ().cy
                 - pe_settings.ps_footer_offset_pxl - pe_cell.cy);
  if (*pe_settings.ps_footer && pe_settings.ps_footer_on
      && pe_footer.y < pe_area.bottom)
    return FOOTER_OFFSET_TOO_LARGE;

  return 0;
}

int
print_engine::make_bitmap (HDC hdc)
{
  memset (&pe_bi, 0, sizeof pe_bi);
  BITMAPINFOHEADER &bi = pe_bi.bi;
  pe_bi.bi.biSize = sizeof pe_bi.bi;
  pe_bi.bi.biWidth = pe_sep_pxl + pe_page_width;
  pe_bi.bi.biHeight = pe_cell.cy;
  pe_bi.bi.biPlanes = 1;
  pe_bi.bi.biBitCount = 1;
  pe_bi.rgb[1].rgbRed = pe_bi.rgb[1].rgbGreen = pe_bi.rgb[1].rgbBlue = 255;

  pe_hbm = CreateDIBSection (hdc, (BITMAPINFO *)&pe_bi, DIB_RGB_COLORS,
                             &pe_bits, 0, 0);
  return pe_hbm != 0;
}

int
print_engine::init (HDC hdc, int save, int preview)
{
  cleanup ();

  pe_bp->set_point (pe_point, pe_bp->b_contents.p1);
  pe_linenum = pe_bp->point_linenum (pe_point);
  pe_page = 1;
  pe_next.linenum = LINENUM_UNINITIALIZED;

  pe_start_page = 1;
  pe_end_page = -1;

  pe_total_pages = -1;
  pe_time.wYear = WORD (-1);

  pe_fold_param.mode = pe_bp->kinsoku_mode ();
  pe_fold_param.extend_limit = pe_bp->kinsoku_extend_limit ();
  pe_fold_param.shorten_limit = pe_bp->kinsoku_shorten_limit ();

  if (save)
    pe_cache.save (pe_point, pe_linenum, pe_page);

  pe_settings.calc_pxl (pe_dev);
  init_font (hdc);
  int r = init_area (hdc);
  if (r)
    return r;

  if (!preview && pe_settings.ps_use_bitmap && !make_bitmap (hdc))
    return CREATE_BM_FAILED;

  return 0;
}

int
print_engine::init_error (HWND hwnd, int r, UINT left, UINT top, UINT fold,
                          UINT header, UINT footer)
{
  switch (r)
    {
    case 0:
      return 1;

    case WIDTH_TOO_SMALL:
      return notice (hwnd, left, IDS_PRINT_AREA_TOO_SMALL);

    case HEIGHT_TOO_SMALL:
      return notice (hwnd, top, IDS_PRINT_AREA_TOO_SMALL);

    case FOLD_TOO_SMALL:
      return notice (hwnd, fold, IDS_FOLD_WIDTH_TOO_SMALL);

    case HEADER_OFFSET_TOO_LARGE:
      return notice (hwnd, header, IDS_HEADER_OFFSET_TOO_LARGE);

    case FOOTER_OFFSET_TOO_LARGE:
      return notice (hwnd, footer, IDS_FOOTER_OFFSET_TOO_LARGE);

    case CREATE_BM_FAILED:
      return notice (hwnd, UINT (-1), IDS_CREATE_BM_FAILED);

    default:
      return notice (hwnd, fold, IDS_FOLD_WIDTH_TOO_LARGE,
                     r & ~FOLD_TOO_LARGE);
    }
}

int
print_engine::record_page (int save, const Point &point, int linenum, int page)
{
  if (save)
    return pe_cache.save (point, linenum, page);
  pe_next.point = point.p_point;
  pe_next.linenum = linenum;
  return 1;
}

int
print_engine::next_line (Point &point) const
{
  Chunk *cp = point.p_chunk;
  point_t opoint = point.p_point - point.p_offset;
  if (pe_fixed_pitch)
    pe_bp->parse_fold_line (point, pe_fold_columns, pe_fold_param);
  else
    pe_bp->parse_fold_line (point, pe_copying_width, pe_glyph_width, pe_fold_param);
  if (!point.p_chunk)
    {
      while (1)
        {
          opoint += cp->c_used;
          if (!cp->c_next)
            break;
          cp = cp->c_next;
        }
      point.p_point = opoint;
      point.p_offset = cp->c_used;
      point.p_chunk = cp;
      return 0;
    }

  for (; cp != point.p_chunk; cp = cp->c_next)
    opoint += cp->c_used;
  point.p_offset++;
  point.p_point = opoint + point.p_offset;
  if (point.p_offset == cp->c_used)
    {
      if (!cp->c_next)
        return 0;
      point.p_chunk = cp->c_next;
      point.p_offset = 0;
    }
  return 1;
}

int
print_engine::form_feed_p (const Point &point) const
{
  if (!pe_bp->bolp (point) || pe_bp->eobp (point) || point.ch () != CC_FF)
    return 0;
  Point next (point);
  pe_bp->next_char (next);
  return pe_bp->eobp (next) || next.ch () == CC_LFD;
}

struct PaintCtx
{
  HDC hdc;
  int x;
  int y;
  int column;
};

/* ÇﬂÇ¡ÇøÇ·ÇƒÇØÇ∆Å[Ç»é¿ëïÇæÇ™ÅAÉvÉäÉìÉ^ëäéËÇæÇ©ÇÁ
   Ç±ÇÒÇ»Ç‡ÇÒÇ≈ä®ïŸÇµÇ∆Ç¢ÇΩÇÈÅB*/

void
print_engine::paint_ascii (PaintCtx &ctx, Char cc) const
{
  if (cc != ' ')
    {
      char c = SJISP (cc) ? 0 : char (cc);
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, &c, 1, 0);
    }
  ctx.column++;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx
            : get_glyph_width (cc, pe_glyph_width));
}

void
print_engine::paint_kana (PaintCtx &ctx, Char cc) const
{
  char c = char (cc);
  SelectObject (ctx.hdc, pe_hfonts[FONT_JP]);
  ExtTextOut (ctx.hdc, ctx.x + pe_offset[FONT_JP].x,
              ctx.y + pe_offset[FONT_JP].y, 0, 0, &c, 1, 0);
  ctx.column++;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx
            : get_glyph_width (cc, pe_glyph_width));
}

void
print_engine::paint_kanji (PaintCtx &ctx, Char cc) const
{
  if (char_width (cc) == 2)
    {
      char b[2];
      b[0] = cc >> 8;
      b[1] = char (cc);
      if (!b[1] || !SJISP (b[0] & 255))
        b[0] = char (0x81), b[1] = char (0x45);
      SelectObject (ctx.hdc, pe_hfonts[FONT_JP]);
      ExtTextOut (ctx.hdc, ctx.x + pe_offset2x[FONT_JP],
                  ctx.y + pe_offset[FONT_JP].y, 0, 0, b, 2, 0);
      ctx.column += 2;
      ctx.x += (pe_fixed_pitch
                ? pe_print_cell.cx * 2
                : get_glyph_width (cc, pe_glyph_width));
    }
  else
    {
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, "", 1, 0);
      ctx.column++;
      ctx.x += (pe_fixed_pitch
                ? pe_print_cell.cx
                : get_glyph_width (cc, pe_glyph_width));
    }
}

void
print_engine::paint_jisx0212 (PaintCtx &ctx, Char cc) const
{
  int l = char_width (cc);
  ucs2_t wc = i2w (cc);
  if (wc != ucs2_t (-1))
    {
      int o = l == 2 ? pe_offset2x[FONT_JP] : pe_offset[FONT_JP].x;
      SelectObject (ctx.hdc, pe_hfonts[FONT_JP]);
      ExtTextOutW (ctx.hdc, ctx.x + o, ctx.y + pe_offset[FONT_JP].y,
                   0, 0, &wc, 1, 0);
    }
  else
    {
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, "\0", l, 0);
    }
  ctx.column += l;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx * l
            : get_glyph_width (cc, pe_glyph_width));
}

void
print_engine::paint_full_width (PaintCtx &ctx, Char cc, int f) const
{
  ucs2_t wc = i2w (cc);
  if (wc != ucs2_t (-1))
    {
      SelectObject (ctx.hdc, pe_hfonts[f]);
      ExtTextOutW (ctx.hdc, ctx.x + pe_offset[f].x, ctx.y + pe_offset[f].y,
                   0, 0, &wc, 1, 0);
    }
  else
    {
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, "\0", 2, 0);
    }
  ctx.column += 2;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx * 2
            : get_glyph_width (cc, pe_glyph_width));
}

void
print_engine::paint_latin (PaintCtx &ctx, Char cc, int f) const
{
  ucs2_t wc = i2w (cc);
  if (wc != ucs2_t (-1))
    {
      SelectObject (ctx.hdc, pe_hfonts[f]);
      ExtTextOutW (ctx.hdc, ctx.x + pe_offset[f].x, ctx.y + pe_offset[f].y,
                   0, 0, &wc, 1, 0);
    }
  else
    {
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, "", 1, 0);
    }
  ctx.column++;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx
            : get_glyph_width (cc, pe_glyph_width));
}

void
print_engine::paint_lucida (PaintCtx &ctx, Char cc) const
{
  ucs2_t wc = i2w (cc);
  if (wc != ucs2_t (-1))
    {
      static LOGFONT lf = {0,0,0,0,0,0,0,0,0,0,0,0,0,LUCIDA_FACE_NAME};
      lf.lfHeight = pe_cell.cy;
      HGDIOBJ of = SelectObject (ctx.hdc, CreateFontIndirect (&lf));
      int o;
      if (pe_fixed_pitch)
        o = (LUCIDA_OFFSET (wc - UNICODE_SMLCDM_MIN)
             * pe_cell.cy / LUCIDA_BASE_HEIGHT) + pe_print_cell.cx / 2;
      else
        {
          const lucida_spacing *p = &lucida_spacing_table[wc - UNICODE_SMLCDM_MIN];
          o = p->a >= 0 ? 0 : -p->a * pe_cell.cy / LUCIDA_BASE_HEIGHT;
        }
      ExtTextOutW (ctx.hdc, ctx.x + o, ctx.y, 0, 0, &wc, 1, 0);
      DeleteObject (SelectObject (ctx.hdc, of));
    }
  else
    {
      SelectObject (ctx.hdc, pe_hfonts[FONT_ASCII]);
      ExtTextOut (ctx.hdc, ctx.x, ctx.y, 0, 0, "", 1, 0);
    }
  ctx.column++;
  ctx.x += (pe_fixed_pitch
            ? pe_print_cell.cx
            : get_glyph_width (cc, pe_glyph_width));
}

int
print_engine::paint_line (HDC hdc, int x, int y, Point &cur_point, long &linenum) const
{
  Point point = cur_point;
  next_line (cur_point);
  if (cur_point.p_point > pe_bp->b_contents.p2)
    pe_bp->goto_char (cur_point, pe_bp->b_contents.p2);

  PaintCtx ctx;
  ctx.hdc = hdc;
  ctx.x = x;
  ctx.y = y;
  ctx.column = 0;

  if (pe_bp->bolp (point)
      || (pe_bp->b_fold_columns != Buffer::FOLD_NONE
          && pe_bp->linenum_mode () != Buffer::LNMODE_LF))
    {
      if (hdc && pe_settings.ps_print_linenum)
        {
          char b[16];
          sprintf (b, "%*d ", LINENUM_WIDTH, linenum % 1000000);
          if (pe_fixed_pitch)
            for (int i = 0; i < LINENUM_WIDTH + 1; i++)
              paint_ascii (ctx, b[i]);
          else
            {
              for (int i = 0; i < LINENUM_WIDTH + 1; i++)
                {
                  ctx.x = x + pe_digit_width * i;
                  paint_ascii (ctx, b[i]);
                }
              ctx.x = x + pe_start_pixel;
            }
        }
      linenum++;
    }
  else
    {
      if (hdc && pe_settings.ps_print_linenum)
        {
          if (pe_fixed_pitch)
            for (int i = 0; i < LINENUM_WIDTH + 1; i++)
              paint_ascii (ctx, ' ');
          else
            ctx.x = x + pe_start_pixel;
        }
    }

  if (form_feed_p (point))
    return 1;

  if (!hdc)
    return pe_bp->eobp (cur_point);

  ctx.column = 0;

  for (; point.p_point < cur_point.p_point && !pe_bp->eobp (point); pe_bp->next_char (point))
    {
      Char cc = point.ch ();
      switch (code_charset (cc))
        {
        case ccs_usascii:
          if (cc < ' ')
            {
              if (cc == CC_LFD)
                break;
              if (cc == CC_TAB)
                {
                  int goal = ((ctx.column + pe_bp->b_tab_columns) / pe_bp->b_tab_columns
                              * pe_bp->b_tab_columns);
                  int n = goal - ctx.column;
                  while (n-- > 0)
                    paint_ascii (ctx, ' ');
                }
              else
                {
                  paint_ascii (ctx, '^');
                  paint_ascii (ctx, cc + '@');
                }
            }
          else if (cc == CC_DEL)
            {
              paint_ascii (ctx, '^');
              paint_ascii (ctx, '?');
            }
          else
            paint_ascii (ctx, cc);
          break;

        case ccs_jisx0201_kana:
          paint_kana (ctx, cc);
          break;

        case ccs_jisx0212:
#ifdef CCS_UJP_MIN
        case ccs_ujp:
#endif
          paint_jisx0212 (ctx, cc);
          break;

        case ccs_gb2312:
          paint_full_width (ctx, cc, FONT_CN_SIMPLIFIED);
          break;

        case ccs_ksc5601:
          paint_full_width (ctx, cc, FONT_HANGUL);
          break;

        case ccs_big5:
          paint_full_width (ctx, cc, FONT_CN_TRADITIONAL);
          break;

        case ccs_iso8859_1:
        case ccs_iso8859_2:
        case ccs_iso8859_3:
        case ccs_iso8859_4:
        case ccs_iso8859_9:
        case ccs_iso8859_10:
        case ccs_iso8859_13:
#ifdef CCS_ULATIN_MIN
        case ccs_ulatin:
#endif
          paint_latin (ctx, cc, FONT_LATIN);
          break;

        case ccs_iso8859_5:
          paint_latin (ctx, cc, FONT_CYRILLIC);
          break;

        case ccs_iso8859_7:
          paint_latin (ctx, cc, FONT_GREEK);
          break;

        case ccs_georgian:
          paint_latin (ctx, cc, FONT_GEORGIAN);
          break;

        case ccs_ipa:
          paint_latin (ctx, cc, FONT_JP);
          break;

        case ccs_smlcdm:
          paint_lucida (ctx, cc);
          break;

        default:
          paint_kanji (ctx, cc);
          break;
        }
    }

  return pe_bp->eobp (cur_point);
}

void
print_engine::paint_string (HDC hdc, int x, int y, const char *s, int l) const
{
  PaintCtx ctx;
  ctx.hdc = hdc;
  ctx.x = x;
  ctx.y = y;
  ctx.column = 0;
  for (const u_char *p = (const u_char *)s, *pe = p + l; p < pe;)
    {
      int c = *p++;
      if (SJISP (c) && p != pe)
        paint_kanji (ctx, (c << 8) | *p++);
      else if (kana_char_p (c))
        paint_kana (ctx, c);
      else
        paint_ascii (ctx, c);
    }
}

int
print_engine::get_extent (const char *s, int l) const
{
  int cx = 0;
  for (const u_char *p = (const u_char *)s, *pe = p + l; p < pe;)
    {
      int c = *p++;
      if (SJISP (c) && p != pe)
        cx += get_glyph_width ((c << 8) | *p++, pe_glyph_width);
      else
        cx += get_glyph_width (c, pe_glyph_width);
    }
  return cx;
}

int
print_engine::paint_fmt (HDC hdc, const char *fmt, int y)
{
  if (!*fmt)
    return 0;
  char buf[4096];
  char *left, *right;
  int hline = format (hdc, fmt, buf, sizeof buf, left, right);
  int width[3];
  int x[3];
  char *b[4];
  b[0] = buf;
  b[1] = left ? left : buf;
  b[2] = (right && right >= b[1]) ? right : b[1] + strlen (b[1]);
  b[3] = b[2] + strlen (b[2]);
  if (pe_fixed_pitch)
    for (int i = 0; i < 3; i++)
      width[i] = (b[i + 1] - b[i]) * pe_print_cell.cx;
  else
    for (int i = 0; i < 3; i++)
      width[i] = get_extent (b[i], b[i + 1] - b[i]);
  x[0] = pe_area.left;
  x[2] = pe_area.right - width[2];
  x[1] = (x[0] + width[0] + x[2] - width[1]) / 2;
  for (int i = 0; i < 3; i++)
    paint_string (hdc, x[i], y, b[i], b[i + 1] - b[i]);
  return hline;
}

void
print_engine::paint_header (HDC hdc)
{
  if (!pe_settings.ps_header_on || !paint_fmt (hdc, pe_settings.ps_header, pe_header.y))
    return;
  MoveToEx (hdc, pe_area.left, pe_header.y + pe_cell.cy, 0);
  LineTo (hdc, pe_area.right, pe_header.y + pe_cell.cy);
}

void
print_engine::paint_footer (HDC hdc)
{
  if (!pe_settings.ps_footer_on || !paint_fmt (hdc, pe_settings.ps_footer, pe_footer.y))
    return;
  MoveToEx (hdc, pe_area.left, pe_footer.y, 0);
  LineTo (hdc, pe_area.right, pe_footer.y);
}

void
print_engine::paint (HDC hdc_print, int save)
{
  HDC hdc = hdc_print;
  HGDIOBJ obm = 0;
  if (pe_hbm)
    {
      hdc = CreateCompatibleDC (hdc_print);
      obm = SelectObject (hdc, pe_hbm);
    }

  int omode = SetBkMode (hdc, TRANSPARENT);
  HGDIOBJ of = SelectObject (hdc, pe_hfonts[FONT_JP]);
  pe_glyph_width.hdc = hdc;

  paint_header (hdc);
  paint_footer (hdc);

  Point point (pe_point);
  long linenum (pe_linenum);
  int x = pe_area.left;
  int w = pe_page_width + pe_sep_pxl;
  for (int col = 0; col < pe_settings.ps_multi_column; col++)
    {
      for (int line = 0, y = pe_area.top; line < pe_ech.cy;
           line++, y += pe_print_cell.cy)
        {
          int eop;
          if (pe_hbm)
            {
              fill_rect (hdc, 0, 0, w, pe_cell.cy, RGB (255, 255, 255));
              eop = paint_line (hdc, 0, 0, point, linenum);
              GdiFlush ();
              SetDIBitsToDevice (hdc_print, x, y, w, pe_cell.cy,
                                 0, 0, 0, pe_cell.cy,
                                 pe_bits, (BITMAPINFO *)&pe_bi, DIB_RGB_COLORS);
            }
          else
            eop = paint_line (hdc, x, y, point, linenum);
          if (eop)
            {
              if (pe_bp->eobp (point))
                goto done;
              break;
            }
        }
      x += w;
    }
done:
  SelectObject (hdc, of);
  SetBkMode (hdc, omode);

  if (pe_hbm)
    {
      SelectObject (hdc, obm);
      DeleteDC (hdc);
    }

  record_page (save, point,
               pe_bp->eobp (point) || pe_page == pe_end_page ? LINENUM_EOB : linenum,
               pe_page + 1);
}

SYSTEMTIME &
print_engine::current_time ()
{
  if (pe_time.wYear == WORD (-1))
    GetLocalTime (&pe_time);
  return pe_time;
}

int
print_engine::skip_page (HDC hdc, Point &point, long &linenum)
{
  HGDIOBJ of = SelectObject (hdc, pe_hfonts[FONT_JP]);
  pe_glyph_width.hdc = hdc;
  for (int col = 0; col < pe_settings.ps_multi_column; col++)
    for (int line = 0; line < pe_ech.cy; line++)
      if (paint_line (0, 0, 0, point, linenum))
        {
          if (pe_bp->eobp (point))
            {
              SelectObject (hdc, of);
              return 0;
            }
          break;
        }
  SelectObject (hdc, of);
  return !pe_bp->eobp (point);
}

int
print_engine::count_total_pages (HDC hdc)
{
  if (pe_total_pages != -1)
    return pe_total_pages;

  Fbegin_wait_cursor ();

  int npages = pe_start_page - 1;
  Point point (pe_point);
  long linenum (pe_linenum);

  do
    npages++;
  while (skip_page (hdc, point, linenum));

  Fend_wait_cursor ();

  pe_total_pages = npages;
  return npages;
}

int
print_engine::set_start_end (HDC hdc, int save, int start, int end)
{
  if (end > 0 && start > end)
    swap (start, end);
  pe_start_page = max (1, start);
  pe_end_page = end > 0 ? max (start, end) : 0;

  Point point (pe_point);
  long linenum (pe_linenum);
  for (; pe_page < start; pe_page++)
    {
      int f = skip_page (hdc, point, linenum);
      pe_point = point;
      pe_linenum = linenum;
      if (!f)
        {
          record_page (save, point, LINENUM_EOB, pe_page + 1);
          return 0;
        }
      record_page (save, point, linenum, pe_page + 1);
    }
  return 1;
}

int
print_engine::next_page ()
{
  if (pe_end_page > 0 && pe_page >= pe_end_page)
    return 0;
  if (pe_next.linenum != LINENUM_UNINITIALIZED)
    {
      if (pe_next.linenum == LINENUM_EOB)
        return 0;
      pe_bp->goto_char (pe_point, pe_next.point);
      pe_linenum = pe_next.linenum;
    }
  else
    {
      page_info *pi = pe_cache.find (pe_page + 1);
      if (!pi)
        return 0;
      if (pi->linenum == LINENUM_EOB)
        return 0;
      pe_bp->goto_char (pe_point, pi->point);
      pe_linenum = pi->linenum;
    }
  pe_page++;
  return 1;
}

int
print_engine::prev_page ()
{
  if (pe_page <= pe_start_page)
    return 0;
  page_info *pi = pe_cache.find (pe_page - 1);
  if (!pi)
    return 0;
  pe_bp->goto_char (pe_point, pi->point);
  pe_linenum = pi->linenum;
  pe_page--;
  return 1;
}

int
print_engine::next_page_exist_p ()
{
  if (pe_page == pe_end_page)
    return 0;
  page_info *pi = pe_cache.find (pe_page + 1);
  return pi && pi->linenum != LINENUM_EOB;
}

int
print_engine::prev_page_exist_p ()
{
  return pe_page > pe_start_page && pe_cache.find (pe_page - 1);
}

inline char *
print_engine::fmt_buffer_name (char *b, char *be)
{
  return pe_bp->buffer_name (b, be);
}

char *
print_engine::fmt_filename_short (char *b, char *be)
{
  lisp name;
  if (!stringp (name = pe_bp->lfile_name)
      && !stringp (name = pe_bp->lalternate_file_name))
    return fmt_buffer_name (b, be);

  const Char *p0, *pe, *p;
  for (p0 = xstring_contents (name),
       pe = p0 + xstring_length (name), p = pe;
       p > p0 && p[-1] != '/' && p[-1] != '\\';
       p--)
    ;
  char s[PATH_MAX + 1];
  w2s (s, p, pe - p);
  return stpncpy (b, s, be - b);
}

char *
print_engine::fmt_filename_long (char *b, char *be)
{
  lisp name;
  if (!stringp (name = pe_bp->lfile_name)
      && !stringp (name = pe_bp->lalternate_file_name))
    return fmt_buffer_name (b, be);

  char s[PATH_MAX + 1];
  w2s (s, name);
  return stpncpy (b, s, be - b);
}

char *
print_engine::fmt (char *b, char *be, const char *f, int v)
{
  char s[256];
  sprintf (s, f, v);
  return stpncpy (b, s, be - b);
}

inline char *
print_engine::fmt_page_no (char *b, char *be)
{
  return fmt (b, be, "%d", current_page ());
}

inline char *
print_engine::fmt_total_page_no (HDC hdc, char *b, char *be)
{
  return fmt (b, be, "%u", count_total_pages (hdc));
}

inline char *
print_engine::fmt_year4 (char *b, char *be)
{
  return fmt (b, be, "%04u", current_time ().wYear);
}

inline char *
print_engine::fmt_year2 (char *b, char *be)
{
  return fmt (b, be, "%02u", current_time ().wYear % 100);
}

char *
print_engine::fmt_month (char *b, char *be, int zero, int star, int colon)
{
  static const char month_names[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
  static const char *const month_full_names[] =
    { "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December",};
  int m = current_time ().wMonth;
  if (m < 1 || m > 12)
    return b;
  if (zero)
    return fmt (b, be, "%02d", m);
  if (star)
    return stpncpy (b, month_full_names[m - 1], be - b);
  if (colon)
    return stpncpy (b, &month_names[3 * (m - 1)], min (3, be - b));
  return fmt (b, be, "%d", m);
}

inline char *
print_engine::fmt_day (char *b, char *be, int zero)
{
  return fmt (b, be, zero ? "%02d" : "%d", current_time ().wDay);
}

char *
print_engine::fmt_week (char *b, char *be, int star, int colon)
{
  static const char day_names[] = "SunMonTueWedThuFriSat";
  static const char *const day_full_names[] =
    { "Sunday","Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday",};
  static const char day_japanese_names[] = "ì˙åéâŒêÖñÿã‡ìy";
  int w = current_time ().wDayOfWeek;
  if (w < 0 || w > 6)
    return b;
  if (star)
    return stpncpy (b, day_full_names[w], be - b);
  if (colon)
    return stpncpy (b, &day_names[3 * w], min (3, be - b));
  return stpncpy (b, &day_japanese_names[2 * w], min (2, be - b));
}

inline char *
print_engine::fmt_hour24 (char *b, char *be, int zero)
{
  return fmt (b, be, zero ? "%02d" : "%d", current_time ().wHour);
}

char *
print_engine::fmt_hour12 (char *b, char *be, int zero, int star, int colon)
{
  int h = current_time ().wHour;
  if (star)
    return stpncpy (b, colon ? (h < 12 ? "am" : "pm") : (h < 12 ? "AM" : "PM"),
                    min (2, be - b));
  h %= 12;
  if (colon)
    h++;
  return fmt (b, be, zero ? "%02d" : "%d", h);
}

inline char *
print_engine::fmt_minute (char *b, char *be, int zero)
{
  return fmt (b, be, zero ? "%02d" : "%d", current_time ().wMinute);
}

inline char *
print_engine::fmt_second (char *b, char *be, int zero)
{
  return fmt (b, be, zero ? "%02d" : "%d", current_time ().wSecond);
}

/*
%f   ÉtÉ@ÉCÉãñº
%F   ÉtÉ@ÉCÉãñº(ÉfÉBÉåÉNÉgÉäïtÇ´)
%b   ÉoÉbÉtÉ@ñº
%p   ÉyÅ[ÉWî‘çÜ
%P   ëçÉyÅ[ÉWêî

%Y   êºóÔ(YYYY)
%y   êºóÔ(YY)
%m   åé  (1Å`12)
%0m  åé  (01Å`12)
%*m  åé  (JanuaryÅ`December)
%:m  åé  (JanÅ`Dec)
%d   ì˙  (1Å`31)
%0d  ì˙  (01Å`31)
%*w  ójì˙ (SundayÅ`Saturday)
%:w  ójì˙ (SunÅ`Sat)
%w   ójì˙ (ì˙Å`ìy)
%h   éû  (0Å`23)
%0h  éû  (00Å`23)
%H   éû  (0Å`11)
%0H  éû  (00Å`11)
%:H   éû (1Å`12)
%0:H  éû (01Å`12)
%*H      (AM/PM)
%*:H     (am/pm)
%M   ï™  (0Å`59)
%0M  ï™  (00Å`59)
%s   ïb  (0Å`59)
%0s  ïb  (00Å`59)

%-   â°ê¸
 */

int
print_engine::format (HDC hdc, const char *fmt, char *b, int l,
                      char *&left, char *&right)
{
  const u_char *f = (const u_char *)fmt;
  char *be = b + l - 2;
  int hline = 0;
  left = right = 0;
  while (*f && b < be)
    {
      if (*f != '%')
        {
          if (SJISP (*f) && f[1])
            *b++ = *f++;
          *b++ = *f++;
        }
      else
        {
          int colon = 0;
          int star = 0;
          int zero = 0;
          f++;
          int c;
          while (1)
            {
              c = *f++;
              if (c == ':')
                colon = 1;
              else if (c == '*')
                star = 1;
              else if (c == '0')
                zero = 1;
              else
                break;
            }

          switch (c)
            {
            case 0:
              goto done;

            case 'f':
              b = fmt_filename_short (b, be);
              break;

            case 'F':
              b = fmt_filename_long (b, be);
              break;

            case 'b':
              b = fmt_buffer_name (b, be);
              break;

            case 'p':
              b = fmt_page_no (b, be);
              break;

            case 'P':
              b = fmt_total_page_no (hdc, b, be);
              break;

            case 'Y':
              b = fmt_year4 (b, be);
              break;

            case 'y':
              b = fmt_year2 (b, be);
              break;

            case 'm':
              b = fmt_month (b, be, zero, star, colon);
              break;

            case 'd':
              b = fmt_day (b, be, zero);
              break;

            case 'w':
              b = fmt_week (b, be, star, colon);
              break;

            case 'h':
              b = fmt_hour24 (b, be, zero);
              break;

            case 'H':
              b = fmt_hour12 (b, be, zero, star, colon);
              break;

            case 'M':
              b = fmt_minute (b, be, zero);
              break;

            case 's':
              b = fmt_second (b, be, zero);
              break;

            case 'l':
              if (!right)
                left = b;
              break;

            case 'r':
              if (!right)
                right = b;
              break;

            case '-':
              hline = 1;
              break;

            default:
              *b++ = c;
              if (SJISP (c) && *f)
                *b++ = *f++;
              break;
            }
        }
    }
done:
  *b++ = 0;
  return hline;
}

void
print_engine::set_print_range () const
{
  Region r;
  if (pe_settings.ps_print_range == print_settings::RANGE_LINENUM)
    {
      Point point;
      long start = pe_settings.ps_range_start;
      long end = pe_settings.ps_range_end;
      if (start > end)
        swap (start, end);
      pe_bp->set_point (point, 0);
      if (pe_bp->b_fold_columns == Buffer::FOLD_NONE)
        {
          pe_bp->linenum_point (point, start);
          pe_bp->goto_bol (point);
        }
      else
        {
          pe_bp->folded_linenum_point (point, start);
          pe_bp->folded_goto_bol (point);
        }
      r.p1 = point.p_point;
      if (pe_bp->b_fold_columns == Buffer::FOLD_NONE)
        {
          pe_bp->linenum_point (point, end);
          pe_bp->goto_eol (point);
        }
      else
        {
          pe_bp->folded_linenum_point (point, end);
          pe_bp->folded_goto_eol (point);
        }
      r.p2 = point.p_point;
    }
  else
    {
      r.p1 = min (max (selected_window ()->w_point.p_point,
                       pe_bp->b_contents.p1),
                  pe_bp->b_contents.p2);
      r.p2 = min (max (selected_window ()->w_selection_marker,
                       pe_bp->b_contents.p1),
                  pe_bp->b_contents.p2);
      if (r.p1 > r.p2)
        swap (r.p1, r.p2);
    }
  pe_bp->b_contents = r;
}

static int user_abort;

static BOOL CALLBACK
printing_dlgproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      center_window (hwnd);
      set_window_icon (hwnd);
      break;

    case WM_COMMAND:
      if (LOWORD (wparam) == IDCANCEL)
        {
          user_abort = 1;
          return 1;
        }
      break;
    }
  return 0;
}

static BOOL CALLBACK
abort_proc (HDC, int error)
{
  if (error && error != SP_OUTOFDISK)
    return 0;

  MSG msg;
  while (!user_abort && PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
    {
      if (msg.message == WM_QUIT)
        {
          PostQuitMessage (0);
          return 0;
        }
      XyzzyTranslateMessage (&msg);
      DispatchMessage (&msg);
    }
  return !user_abort;
}

int
print_engine::doprint1 (HWND hwnd)
{
  int r = init (pe_dev, 0, 0);
  if (r)
    return init_error (hwnd, r);

  if (pe_settings.ps_print_range == print_settings::RANGE_PAGE
      && !set_start_end (pe_dev, 0, pe_settings.ps_range_start,
                         pe_settings.ps_range_end))
    return bad_range (hwnd);

  char *docname;
  lisp name;
  if (stringp (name = pe_bp->lfile_name)
      || stringp (name = pe_bp->lalternate_file_name))
    {
      docname = (char *)alloca (xstring_length (name) * 2 + 32);
      w2s (docname, name);
    }
  else
    {
      int l = xstring_length (pe_bp->lbuffer_name) * 2 + 32;
      docname = (char *)alloca (l);
      pe_bp->buffer_name (docname, docname + l);
    }

  SetAbortProc (pe_dev, abort_proc);

  DOCINFO di;
  bzero (&di, sizeof di);
  di.cbSize = sizeof di;
  di.lpszDocName = docname;

  user_abort = 0;
  HWND printing = CreateDialog (app.hinst, MAKEINTRESOURCE (IDD_PRINTING),
                                app.toplev, printing_dlgproc);
  SetDlgItemText (printing, IDC_DOCNAME, docname);
  ShowWindow (printing, SW_SHOW);
  UpdateWindow (printing);
  EnableWindow (app.toplev, 0);

  if (StartDoc (pe_dev, &di) == SP_ERROR)
    {
      EnableWindow (app.toplev, 1);
      DestroyWindow (printing);
      FEsimple_win32_error (GetLastError ());
    }

  int error = 0;
  int page = 1;

  do
    {
      char b[32];
      sprintf (b, "Page %u", page++);
      SetDlgItemText (printing, IDC_PAGENUM, b);

      if (StartPage (pe_dev) == SP_ERROR)
        {
          error = GetLastError ();
          break;
        }

      SetViewportOrgEx (pe_dev,
                        -pe_dev.min_margin_pxl ().left,
                        -pe_dev.min_margin_pxl ().top,
                        0);

      paint (pe_dev, 0);

      if (EndPage (pe_dev) == SP_ERROR)
        {
          error = GetLastError ();
          break;
        }

      if (!abort_proc (pe_dev, 0))
        break;
    }
  while (next_page ());

  if (!error && !user_abort)
    EndDoc (pe_dev);
  else
    AbortDoc (pe_dev);

  EnableWindow (app.toplev, 1);
  DestroyWindow (printing);

  if (user_abort)
    return 0;
  if (error)
    FEsimple_win32_error (error);
  return 1;
}

int
print_engine::doprint (HWND hwnd)
{
  if (pe_settings.ps_print_range == print_settings::RANGE_ALL
      || pe_settings.ps_print_range == print_settings::RANGE_PAGE)
    return doprint1 (hwnd);

  no_restrictions nr (pe_bp);
  set_print_range ();
  return doprint1 (hwnd);
}

int
print_engine::preview1 (HWND hwnd)
{
  int r = init (pe_dev, 1, 1);
  if (r)
    return init_error (hwnd, r);

  if (pe_settings.ps_print_range == print_settings::RANGE_PAGE
      && !set_start_end (pe_dev, 1,
                         pe_settings.ps_range_start,
                         pe_settings.ps_range_end))
    return bad_range (hwnd);

  preview_dialog d (pe_dev, pe_settings, *this);
  return d.do_modal (get_active_window ());
}

int
print_engine::preview (HWND hwnd)
{
  if (pe_settings.ps_print_range == print_settings::RANGE_ALL
      || pe_settings.ps_print_range == print_settings::RANGE_PAGE)
    return preview1 (hwnd);

  no_restrictions nr (pe_bp);
  set_print_range ();
  return preview1 (hwnd);
}

int
print_engine::bad_range (HWND hwnd)
{
  return notice (hwnd, UINT (-1), IDS_MAX_PAGE_TOO_LARGE);
}

int
print_engine::notice (HWND hwnd, UINT id, UINT ids)
{
  char b[256];
  LoadString (app.hinst, ids, b, sizeof b);
  MsgBox (hwnd, b, TitleBarString, MB_OK | MB_ICONEXCLAMATION,
          xsymbol_value (Vbeep_on_error) != Qnil);
  if (id != UINT (-1))
    SetFocus (GetDlgItem (hwnd, id));
  return 0;
}

int
print_engine::notice (HWND hwnd, UINT id, UINT ids, int arg)
{
  char fmt[256], b[512];
  LoadString (app.hinst, ids, fmt, sizeof fmt);
  wsprintf (b, fmt, arg);
  MsgBox (hwnd, b, TitleBarString, MB_OK | MB_ICONEXCLAMATION,
          xsymbol_value (Vbeep_on_error) != Qnil);
  if (id != UINT (-1))
    SetFocus (GetDlgItem (hwnd, id));
  return 0;
}

void
print_engine::page_cache::cleanup ()
{
  for (cache_block *p = c_block, *next; p; p = next)
    {
      next = p->b_next;
      free (p);
    }
  c_block = 0;
}

print_engine::page_cache::cache_block *
print_engine::page_cache::find_block (int page)
{
  assert (page > 0);
  page--;
  page /= PAGES_PER_BLOCK;
  for (cache_block *p = c_block; p; p = p->b_next)
    if (p->b_page == page)
      return p;
  return 0;
}

print_engine::page_cache::cache_block *
print_engine::page_cache::alloc_block (int page)
{
  assert (page > 0);
  assert (!find_block (page));
  page--;
  page /= PAGES_PER_BLOCK;
  cache_block *p = (cache_block *)malloc (sizeof *p);
  if (!p)
    return 0;
#ifdef DEBUG
  for (int i = 0; i < PAGES_PER_BLOCK; i++)
    p->b_block[i].linenum = LINENUM_UNINITIALIZED;
#endif
  p->b_page = page;
  p->b_used = 0;
  p->b_next = c_block;
  c_block = p;
  return p;
}

print_engine::page_info *
print_engine::page_cache::find (int page)
{
  cache_block *p = find_block (page);
  if (!p)
    return 0;
  page = (page - 1) % PAGES_PER_BLOCK;
  if (p->b_used <= page)
    return 0;
  assert (p->b_block[page].linenum != LINENUM_UNINITIALIZED);
  return &p->b_block[page];
}

print_engine::page_info *
print_engine::page_cache::alloc_page (int page)
{
  cache_block *p = find_block (page);
  if (!p)
    p = alloc_block (page);
  if (!p)
    return 0;
  page = (page - 1) % PAGES_PER_BLOCK;
  assert (p->b_used == page);
  assert (p->b_block[page].linenum == LINENUM_UNINITIALIZED);
  return &p->b_block[p->b_used++];
}

int
print_engine::page_cache::save (const Point &point, int linenum, int page)
{
  page_info *pi = find (page);
  if (pi)
    {
      assert (pi->point == point.p_point);
      assert (pi->linenum == linenum);
      return 1;
    }

  pi = alloc_page (page);
  if (!pi)
    return 0;

  pi->point = point.p_point;
  pi->linenum = linenum;

  return 1;
}

int
get_glyph_width (Char cc, const glyph_width &gw)
{
  if (gw.pixel[cc] >= 0)
    return gw.pixel[cc];

  SIZE sz;
  int f;
  switch (code_charset (cc))
    {
    case ccs_usascii:
      {
        char c = SJISP (cc) ? 0 : char (cc);
        SelectObject (gw.hdc, gw.hfonts[FONT_ASCII]);
        GetTextExtentPoint32 (gw.hdc, &c, 1, &sz);
        break;
      }

    case ccs_jisx0201_kana:
      {
        char c = char (cc);
        SelectObject (gw.hdc, gw.hfonts[FONT_JP]);
        GetTextExtentPoint32 (gw.hdc, &c, 1, &sz);
        break;
      }

    case ccs_jisx0212:
#ifdef CCS_UJP_MIN
    case ccs_ujp:
#endif
      f = FONT_JP;
      goto unicode_char;

    case ccs_gb2312:
      f = FONT_CN_SIMPLIFIED;
      goto unicode_char;

    case ccs_ksc5601:
      f = FONT_HANGUL;
      goto unicode_char;

    case ccs_big5:
      f = FONT_CN_TRADITIONAL;
      goto unicode_char;

    case ccs_iso8859_1:
    case ccs_iso8859_2:
    case ccs_iso8859_3:
    case ccs_iso8859_4:
    case ccs_iso8859_9:
    case ccs_iso8859_10:
    case ccs_iso8859_13:
#ifdef CCS_ULATIN_MIN
    case ccs_ulatin:
#endif
      f = FONT_LATIN;
      goto unicode_char;

    case ccs_iso8859_5:
      f = FONT_CYRILLIC;
      goto unicode_char;

    case ccs_iso8859_7:
      f = FONT_GREEK;
      goto unicode_char;

    case ccs_georgian:
      f = FONT_GEORGIAN;
      goto unicode_char;

    case ccs_ipa:
      f = FONT_JP;
      goto unicode_char;

    case ccs_smlcdm:
      {
        ucs2_t wc = i2w (cc);
        if (wc != ucs2_t (-1))
          {
            const lucida_spacing *p = &lucida_spacing_table[wc - UNICODE_SMLCDM_MIN];
            if (p->a >= 0)
              sz.cx = p->a * 2 + p->b;
            else
              sz.cx = LUCIDA_SPACING * 2 + p->b;
            sz.cx = sz.cx * gw.height / LUCIDA_BASE_HEIGHT;
          }
        else
          {
            SelectObject (gw.hdc, gw.hfonts[FONT_ASCII]);
            GetTextExtentPoint32 (gw.hdc, "\0", char_width (cc), &sz);
          }
        break;
      }

    unicode_char:
      {
        ucs2_t wc = i2w (cc);
        if (wc != ucs2_t (-1))
          {
            SelectObject (gw.hdc, gw.hfonts[f]);
            GetTextExtentPoint32W (gw.hdc, &wc, 1, &sz);
          }
        else
          {
            SelectObject (gw.hdc, gw.hfonts[FONT_ASCII]);
            GetTextExtentPoint32 (gw.hdc, "\0", char_width (cc), &sz);
          }
        break;
      }

    default:
      if (char_width (cc) == 2)
        {
          char b[2];
          b[0] = cc >> 8;
          b[1] = char (cc);
          if (!b[1] || !SJISP (b[0] & 255))
            b[0] = char (0x81), b[1] = char (0x45);
          SelectObject (gw.hdc, gw.hfonts[FONT_JP]);
          GetTextExtentPoint32 (gw.hdc, b, 2, &sz);
        }
      else
        {
          SelectObject (gw.hdc, gw.hfonts[FONT_ASCII]);
          GetTextExtentPoint32 (gw.hdc, "", 1, &sz);
        }
      break;
    }

  const_cast <short *> (gw.pixel)[cc] = (short)sz.cx;
  return sz.cx;
}
