#include "stdafx.h"
#include "ed.h"
#include "ldialog.h"
#include "ColorDialog.h"
#include "conf.h"
#include "font.h"

static void
paint_color_list (DRAWITEMSTRUCT *dis, const char *string, COLORREF color)
{
  COLORREF bg = (dis->itemState & ODS_SELECTED
                 ? sysdep.highlight : sysdep.window);
  COLORREF obg = SetBkColor (dis->hDC, bg);
  COLORREF ofg = SetTextColor (dis->hDC, (dis->itemState & ODS_SELECTED
                                          ? sysdep.highlight_text
                                          : sysdep.window_text));
  const RECT &r = dis->rcItem;
  if (dis->itemID != UINT (-1))
    {
      SIZE size;
      GetTextExtentPoint32 (dis->hDC, "M", 1, &size);
      size.cx = size.cx * 5 / 2;
      ExtTextOut (dis->hDC,
                  r.left + size.cx,
                  (r.top + r.bottom - size.cy) / 2,
                  ETO_OPAQUE | ETO_CLIPPED, &r, string, strlen (string), 0);

      HGDIOBJ open = SelectObject (dis->hDC, sysdep.hpen_black);
      HBRUSH hbr = CreateSolidBrush (color);
      HGDIOBJ obr = SelectObject (dis->hDC, hbr);
      Rectangle (dis->hDC, r.left + 2, r.top + 1,
                 r.left + size.cx - 2, r.bottom - 1);
      DeleteObject (SelectObject (dis->hDC, obr));
      SelectObject (dis->hDC, open);
    }
  if (dis->itemState & ODS_FOCUS)
    DrawFocusRect (dis->hDC, &r);
  SetBkColor (dis->hDC, obg);
  SetBkColor (dis->hDC, ofg);
}

class SelectColor
{
  HWND hwnd;
  XCOLORREF cc;
  int current_id;
  COLORREF colors[20];
  static COLORREF cust[16];
  static int initialized;

  int find_match (const XCOLORREF &) const;
  static BOOL CALLBACK select_color_dlgproc (HWND, UINT, WPARAM, LPARAM);
  BOOL dlgproc (UINT, WPARAM, LPARAM);
  void do_command (int, int);
  void draw_button (int, DRAWITEMSTRUCT *);
  void draw_combo (DRAWITEMSTRUCT *);
  static void measure_item (HWND, int, MEASUREITEMSTRUCT *);
  void add_combo ();
  void init_dialog ();

public:
  SelectColor ();
  ~SelectColor ();
  int select_color (HWND, XCOLORREF &);
};

COLORREF SelectColor::cust[16];
int SelectColor::initialized;

static const char rColors[] = "\\Colors";
static const char rCustColors[] = "CustColors";

SelectColor::SelectColor ()
{
  if (!initialized)
    {
      initialized = 1;
      for (int i = 0; i < numberof (cust); i++)
        {
          char name[16];
          sprintf (name, "%s%d", cfgCustColor, i);
          if (!read_conf (cfgColors, name, cust[i]))
            cust[i] = RGB (255, 255, 255);
        }
    }
  HDC hdc = GetDC (0);
  for (int i = 0; i < numberof (colors); i++)
    colors[i] = GetNearestColor (hdc, PALETTEINDEX (i));
  ReleaseDC (0, hdc);
}

SelectColor::~SelectColor ()
{
  for (int i = 0; i < numberof (cust); i++)
    {
      char name[16];
      sprintf (name, "%s%d", cfgCustColor, i);
      write_conf (cfgColors, name, cust[i], 1);
    }
  flush_conf ();
}

int
SelectColor::find_match (const XCOLORREF &c) const
{
  if (c.syscolor_index () >= 0)
    return -1;
  COLORREF cr (c);
  for (int i = 0; i < numberof (colors); i++)
    if (colors[i] == cr)
      return i + IDC_BUTTON1;
  return -1;
}

void
SelectColor::do_command (int id, int code)
{
  switch (id)
    {
    case IDOK:
    case IDCANCEL:
      EndDialog (hwnd, id);
      break;

    case IDC_COMBO:
      if (code == CBN_SELCHANGE)
        {
          int i = SendDlgItemMessage (hwnd, IDC_COMBO, CB_GETCURSEL, 0, 0);
          if (i == CB_ERR)
            return;
          i = SendDlgItemMessage (hwnd, IDC_COMBO, CB_GETITEMDATA, i, 0);
          if (i == CB_ERR)
            return;
          cc = XCOLORREF (RGB (0, 0, 0), i - IDS_COLOR_SCROLLBAR);
          if (current_id >= 0)
            InvalidateRect (GetDlgItem (hwnd, current_id), 0, 0);
          current_id = -1;
          InvalidateRect (GetDlgItem (hwnd, IDC_CURRENT), 0, 0);
        }
      break;

    case IDC_OTHER:
      {
        CHOOSECOLOR xc;
        xc.lStructSize = sizeof xc;
        xc.hwndOwner = hwnd;
        xc.rgbResult = cc;
        xc.lpCustColors = cust;
        xc.Flags = CC_RGBINIT;
        if (ChooseColor (&xc))
          {
            cc = xc.rgbResult;
            int i = find_match (cc);
            if (i != current_id)
              {
                if (current_id >= 0)
                  InvalidateRect (GetDlgItem (hwnd, current_id), 0, 0);
                if (i >= 0)
                  InvalidateRect (GetDlgItem (hwnd, i), 0, 0);
                current_id = i;
              }
            InvalidateRect (GetDlgItem (hwnd, IDC_CURRENT), 0, 0);
            SendDlgItemMessage (hwnd, IDC_COMBO, CB_SETCURSEL, WPARAM (-1), 0);
          }
        break;
      }
    }
}

void
SelectColor::draw_combo (DRAWITEMSTRUCT *dis)
{
  if (dis->itemID == UINT (-1))
    paint_color_list (dis, "", RGB (0, 0, 0));
  else
    {
      char b[256];
      if (!LoadString (app.hinst, dis->itemData, b, sizeof b))
        *b = 0;
      paint_color_list (dis, b, GetSysColor (dis->itemData - IDS_COLOR_SCROLLBAR));
    }
}

void
SelectColor::draw_button (int id, DRAWITEMSTRUCT *dis)
{
  COLORREF fg;
  int on = 0;
  if (id == IDC_CURRENT)
    {
      on = 1;
      fg = cc;
    }
  else if (id >= IDC_BUTTON1 && id <= IDC_BUTTON20)
    {
      if (dis->itemState & ODS_SELECTED)
        {
          if (current_id < 0)
            SendDlgItemMessage (hwnd, IDC_COMBO, CB_SETCURSEL, WPARAM (-1), 0);
          if (current_id != id)
            {
              if (current_id >= 0)
                InvalidateRect (GetDlgItem (hwnd, current_id), 0, 0);
              InvalidateRect (GetDlgItem (hwnd, IDC_CURRENT), 0, 0);
            }
          current_id = id;
          cc = colors[id - IDC_BUTTON1];
        }
      fg = colors[id - IDC_BUTTON1];
      on = current_id == id;
    }
  else
    return;

  HDC hdc = dis->hDC;
  const RECT &r = dis->rcItem;
  HGDIOBJ obr = SelectObject (hdc, CreateSolidBrush (fg));
  PatBlt (hdc, r.left, r.top, r.right - r.left, r.bottom - r.top, PATCOPY);
  DeleteObject (SelectObject (hdc, obr));

  if (on)
    paint_button_on (hdc, r);
  else
    paint_button_off (hdc, r);

  if (dis->itemState & ODS_FOCUS)
    {
      RECT rr (r);
      rr.right--;
      rr.bottom--;
      InflateRect (&rr, -2, -2);
      if (on)
        OffsetRect (&rr, 1, 1);
      DrawFocusRect (hdc, &rr);
    }
}

void
SelectColor::measure_item (HWND hwnd, int id, MEASUREITEMSTRUCT *mis)
{
  if (id == IDC_COMBO)
    mis->itemHeight = get_font_height (hwnd) + 2;
}

void
SelectColor::add_combo ()
{
  int cur = cc.syscolor_index ();
  if (cur >= 0)
    cur += IDS_COLOR_SCROLLBAR;
  HWND combo = GetDlgItem (hwnd, IDC_COMBO);
  for (int i = IDS_COLOR_SCROLLBAR; i <= IDS_COLOR_BTNHIGHLIGHT; i++)
    {
      int j = SendMessage (combo, CB_ADDSTRING, 0, i);
      if (j != CB_ERR && cur == i)
        SendMessage (combo, CB_SETCURSEL, j, 0);
    }
}

void
SelectColor::init_dialog ()
{
  center_window (hwnd);
  set_window_icon (hwnd);
  add_combo ();
  HDC hdc = GetDC (hwnd);
  if (GetDeviceCaps (hdc, RASTERCAPS) & RC_PALETTE)
    EnableWindow (GetDlgItem (hwnd, IDC_OTHER), 0);
  else
    {
      int nbits = GetDeviceCaps (hdc, BITSPIXEL);
      int nplanes = GetDeviceCaps (hdc, PLANES);
      if (nbits <= 8 && nplanes <= 256
          && (1 << nbits) * nplanes <= 256)
        EnableWindow (GetDlgItem (hwnd, IDC_OTHER), 0);
    }
  ReleaseDC (hwnd, hdc);
}

BOOL
SelectColor::dlgproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      init_dialog ();
      return 1;

    case WM_COMMAND:
      do_command (LOWORD (wparam), HIWORD (wparam));
      return 1;

    case WM_DRAWITEM:
      if (wparam == IDC_COMBO)
        draw_combo ((DRAWITEMSTRUCT *)lparam);
      else
        draw_button (wparam, (DRAWITEMSTRUCT *)lparam);
      return 1;

    default:
      return 0;
    }
}

BOOL CALLBACK
SelectColor::select_color_dlgproc (HWND hwnd, UINT msg,
                                   WPARAM wparam, LPARAM lparam)
{
  SelectColor *p;
  if (msg == WM_INITDIALOG)
    {
      p = (SelectColor *)lparam;
      SetWindowLong (hwnd, DWL_USER, lparam);
      p->hwnd = hwnd;
    }
  else if (msg == WM_MEASUREITEM)
    {
      measure_item (hwnd, wparam, (MEASUREITEMSTRUCT *)lparam);
      return 1;
    }
  else
    {
      p = (SelectColor *)GetWindowLong (hwnd, DWL_USER);
      if (!p)
        return 0;
    }
  return p->dlgproc (msg, wparam, lparam);
}

int
SelectColor::select_color (HWND parent, XCOLORREF &c)
{
  cc = c;
  current_id = find_match (cc);
  if (DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_SELECT_COLOR),
                      parent, select_color_dlgproc, LPARAM (this)) == IDOK)
    {
      c = cc;
      return 1;
    }
  return 0;
}

void
ChangeColorsPageP::set_active () const
{
  ccp_parent->ps_curpage = ccp_page_no;
}

void
ChangeColorsPageP::reset () const
{
  ccp_parent->ps_result = IDCANCEL;
}

BOOL CALLBACK
ChangeColorsPageP::ccp_dialog_proc (HWND hwnd, UINT msg,
                                    WPARAM wparam, LPARAM lparam)
{
  ChangeColorsPageP *p;
  if (msg == WM_INITDIALOG)
    {
      p = (ChangeColorsPageP *)((PROPSHEETPAGE *)lparam)->lParam;
      SetWindowLong (hwnd, DWL_USER, LPARAM (p));
      p->ccp_hwnd = hwnd;
      if (!p->ccp_parent->ps_moved)
        {
          p->ccp_parent->ps_moved = 1;
          center_window (GetParent (hwnd));
          set_window_icon (GetParent (hwnd));
        }
    }
  else if (msg == WM_MEASUREITEM)
    {
      measure_item (hwnd, wparam, (MEASUREITEMSTRUCT *)lparam);
      return 1;
    }
  else
    {
      p = (ChangeColorsPageP *)GetWindowLong (hwnd, DWL_USER);
      if (!p)
        return 0;
    }
  return p->dialog_proc (msg, wparam, lparam);
}

void
ChangeColorsPageP::init_page (UINT idd, PropSheet *sheet, int page_no,
                              PROPSHEETPAGE *psp)
{
  ccp_page_no = page_no;
  ccp_parent = sheet;
  ccp_hg = PropSheetFont::change_font (MAKEINTRESOURCE (idd));

  psp->dwSize = sizeof *psp;
  psp->dwFlags = ccp_hg ? PSP_DLGINDIRECT : 0;
  psp->hInstance = app.hinst;
  if (ccp_hg)
    psp->pResource = (DLGTEMPLATE *)GlobalLock (ccp_hg);
  else
    psp->pszTemplate = MAKEINTRESOURCE (idd);
  psp->pszIcon = 0;
  psp->pszTitle = 0;
  psp->lParam = LPARAM (this);
  psp->pfnCallback = 0;
  psp->pfnDlgProc = ccp_dialog_proc;

  ccp_modified = 0;
}

int
ChangeColorsPageP::get_result ()
{
  if (memcmp (ccp_curcc, ccp_cc, sizeof *ccp_cc * ccp_ncolors))
    {
      memcpy (ccp_cc, ccp_curcc, sizeof *ccp_cc * ccp_ncolors);
      ccp_modified = 1;
    }
  return 1;
}

void
ChangeColorsPageP::init_dialog ()
{
  for (int i = 0; i < ccp_ncolors; i++)
    SendDlgItemMessage (ccp_hwnd, IDC_COLOR_LIST, LB_ADDSTRING, 0, i);
  EnableWindow (GetDlgItem (ccp_hwnd, IDC_SET_COLOR), 0);
}

BOOL
ChangeColorsPageP::do_command (int id, int code)
{
  switch (id)
    {
    case IDC_SET_COLOR:
      {
        SelectColor sc;
        int n = SendDlgItemMessage (ccp_hwnd, IDC_COLOR_LIST, LB_GETCURSEL, 0, 0);
        if (n != LB_ERR && sc.select_color (ccp_hwnd, ccp_curcc[n]))
          {
            RECT r;
            if (SendDlgItemMessage (ccp_hwnd, IDC_COLOR_LIST,
                                    LB_GETITEMRECT, n, LPARAM (&r)) != LB_ERR)
              InvalidateRect (GetDlgItem (ccp_hwnd, IDC_COLOR_LIST), &r, 0);
            notify_color (n);
          }
        return 1;
      }

    case IDC_COLOR_LIST:
      switch (code)
        {
        case LBN_SELCHANGE:
          EnableWindow (GetDlgItem (ccp_hwnd, IDC_SET_COLOR),
                        SendDlgItemMessage (ccp_hwnd, IDC_COLOR_LIST, LB_GETCURSEL, 0, 0) >= 0);
          return 1;

        case LBN_DBLCLK:
          PostMessage (ccp_hwnd, WM_COMMAND,
                       MAKEWPARAM (IDC_SET_COLOR, BN_CLICKED), 0);
          return 1;
        }
      return 0;

    default:
      return 0;
    }
}

BOOL
ChangeColorsPageP::do_notify (int, NMHDR *nm)
{
  switch (nm->code)
    {
    case PSN_KILLACTIVE:
      SetWindowLong (ccp_hwnd, DWL_MSGRESULT, !get_result ());
      return 1;

    case PSN_SETACTIVE:
      set_active ();
      return 1;

    case PSN_RESET:
      reset ();
      return 1;
    }
  return 0;
}

BOOL
ChangeColorsPageP::draw_item (int id, DRAWITEMSTRUCT *dis)
{
  switch (id)
    {
    case IDC_COLOR_LIST:
      if (dis->itemID == UINT (-1))
        paint_color_list (dis, "", RGB (0, 0, 0));
      else if (prop_fg_p (dis->itemData))
        {
          char b[32];
          sprintf (b, "•¶Žš%d", dis->itemData - PROP_FG_OFFSET + 1);
          paint_color_list (dis, b, ccp_curcc[dis->itemData]);
        }
      else if (prop_bg_p (dis->itemData))
        {
          char b[32];
          sprintf (b, "”wŒi%d", dis->itemData - PROP_BG_OFFSET + 1);
          paint_color_list (dis, b, ccp_curcc[dis->itemData]);
        }
      else if (misc_p (dis->itemData))
        paint_color_list (dis, misc_color_name (dis->itemData - MISC_OFFSET),
                          ccp_curcc[dis->itemData]);
      else
        paint_color_list (dis, wcolor_index_names[dis->itemData].display_name,
                          ccp_curcc[dis->itemData]);
      return 1;

    default:
      return 0;
    }
}

void
ChangeColorsPageP::measure_item (HWND hwnd, int, MEASUREITEMSTRUCT *mis)
{
  mis->itemHeight = max (get_font_height (hwnd), 14);
}

BOOL
ChangeColorsPageP::dialog_proc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      init_dialog ();
      return 1;

    case WM_DESTROY:
      do_destroy ();
      return 1;

    case WM_COMMAND:
      return do_command (LOWORD (wparam), HIWORD (wparam));

    case WM_DRAWITEM:
      return draw_item (wparam, (DRAWITEMSTRUCT *)lparam);

    case WM_NOTIFY:
      return do_notify (wparam, (NMHDR *)lparam);

    default:
      return 0;
    }
}

BOOL
ChooseFontPage::draw_item (int id, DRAWITEMSTRUCT *dis)
{
  if (cfp_font.draw_item (ccp_hwnd, id, dis))
    return 1;
  return ChangeColorsPageP::draw_item (id, dis);
}

BOOL
ChooseFontPage::do_command (int id, int code)
{
  if (id == IDC_TEST)
    {
      if (!get_result ())
        return 1;
      Window::change_parameters (cfp_param, colors (), ml_colors (),
                                 fg_colors (), bg_colors ());
      modify_misc_colors (misc_colors (), 0);
      refresh_screen (0);
      cfp_restore = 1;
      return 1;
    }
  if (cfp_font.do_command (ccp_hwnd, id, code))
    return 1;
  return ChangeColorsPageP::do_command (id, code);
}

void
ChooseFontPage::notify_color (int n)
{
  if (n == WCOLOR_TEXT || n == WCOLOR_BACK)
    cfp_font.set_color (ccp_hwnd, ccp_curcc[WCOLOR_TEXT], ccp_curcc[WCOLOR_BACK]);
  ChangeColorsPageP::notify_color (n);
}

int
ChooseFontPage::get_result ()
{
  char b[128];
  if (!GetDlgItemText (ccp_hwnd, IDC_LSP, b, sizeof b))
    *b = 0;
  int lsp;
  if (!check_integer_format (b, &lsp) || lsp < 0 || lsp > 30)
    {
      warn_msgbox (Eline_spacing_must_0_to_30);
      SetFocus (GetDlgItem (ccp_hwnd, IDC_LSP));
      return 0;
    }
  if (lsp != cfp_param.fs_line_spacing)
    {
      cfp_param.fs_line_spacing = lsp;
      ccp_modified = 1;
    }

  int i = SendDlgItemMessage (ccp_hwnd, IDC_BACKSL, BM_GETCHECK, 0, 0);
  if (i != cfp_param.fs_use_backsl)
    {
      cfp_param.fs_use_backsl = i;
      ccp_modified = 1;
    }

  i = SendDlgItemMessage (ccp_hwnd, IDC_RECOMMEND_SIZE, BM_GETCHECK, 0, 0);
  if (i != cfp_param.fs_recommend_size)
    {
      cfp_param.fs_recommend_size = i;
      ccp_modified = 1;
    }

  for (i = 0; i < FONT_MAX; i++)
    if (cfp_font.cf_param.fs_logfont[i].lfHeight != cfp_param.fs_logfont[i].lfHeight
        || cfp_font.cf_param.fs_logfont[i].lfCharSet != cfp_param.fs_logfont[i].lfCharSet
        || strcmp (cfp_font.cf_param.fs_logfont[i].lfFaceName, cfp_param.fs_logfont[i].lfFaceName))
      {
        cfp_param.fs_logfont[i] = cfp_font.cf_param.fs_logfont[i];
        ccp_modified = 1;
      }
  if (cfp_font.cf_param.fs_size_pixel != cfp_param.fs_size_pixel)
    {
      cfp_param.fs_size_pixel = cfp_font.cf_param.fs_size_pixel;
      ccp_modified = 1;
    }

  ChangeColorsPageP::get_result ();

  return 1;
}

void
ChooseFontPage::init_dialog ()
{
  ChangeColorsPageP::init_dialog ();

  cfp_font.init_dialog (ccp_hwnd);

  SetDlgItemInt (ccp_hwnd, IDC_LSP, cfp_param.fs_line_spacing, 0);
  SendDlgItemMessage (ccp_hwnd, IDC_LSPSPIN, UDM_SETRANGE,
                      0, MAKELONG (30, 0));
  SendDlgItemMessage (ccp_hwnd, IDC_LSPSPIN, UDM_SETPOS,
                      0, MAKELONG (short (cfp_param.fs_line_spacing), 0));

  SendDlgItemMessage (ccp_hwnd, IDC_BACKSL, BM_SETCHECK,
                      cfp_param.fs_use_backsl ? 1 : 0, 0);
  SendDlgItemMessage (ccp_hwnd, IDC_RECOMMEND_SIZE, BM_SETCHECK,
                      cfp_param.fs_recommend_size ? 1 : 0, 0);
}

void
ChooseFontPage::do_destroy ()
{
  cfp_font.do_destroy (ccp_hwnd);
  ChangeColorsPageP::do_destroy ();
}

void
ChooseFontPage::init_page (PropSheet *sheet, int page_no, PROPSHEETPAGE *psp)
{
  ChangeColorsPageP::init_page (IDD_FONT, sheet, page_no, psp);

  for (int i = 0; i < FONT_MAX; i++)
    GetObject (app.text_font.font (i), sizeof cfp_param.fs_logfont[i],
               &cfp_param.fs_logfont[i]);
  cfp_param.fs_line_spacing = app.text_font.line_spacing ();
  cfp_param.fs_use_backsl = app.text_font.use_backsl_p ();
  cfp_param.fs_recommend_size = app.text_font.recommend_size_p ();
  cfp_param.fs_size_pixel = app.text_font.size_pixel_p ();
  cfp_font.cf_param = cfp_param;
  cfp_font.cf_fg = Window::default_xcolors[WCOLOR_TEXT];
  cfp_font.cf_bg = Window::default_xcolors[WCOLOR_BACK];

  for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
    ccp_cc[i] = Window::default_xcolors[i];
  ccp_cc[MODELINE_FG_OFFSET] = Window::modeline_xcolors[Window::MLCI_FOREGROUND];
  ccp_cc[MODELINE_BG_OFFSET] = Window::modeline_xcolors[Window::MLCI_BACKGROUND];
  for (int i = 0; i < NPROPS; i++)
    {
      ccp_cc[PROP_FG_OFFSET + i] = Window::w_textprop_xforecolor[i + 1];
      ccp_cc[PROP_BG_OFFSET + i] = Window::w_textprop_xbackcolor[i + 1];
    }
  for (int i = 0; i < NMISCS; i++)
    ccp_cc[MISC_OFFSET + i] = get_misc_color (i);

  memcpy (ccp_curcc, ccp_cc, sizeof ccp_cc);

  cfp_org_param = cfp_param;
  assert (sizeof cfp_org_cc == sizeof ccp_cc);
  memcpy (cfp_org_cc, ccp_cc, sizeof ccp_cc);
}

ChooseFontPage::ChooseFontPage ()
     : ChangeColorsPageP (MAX_CCP_COLORS), cfp_restore (0)
{
}

void
ChangeColorsDialog::init_dialog ()
{
  ChangeColorsPageP::init_dialog ();
  SendDlgItemMessage (ccp_hwnd, IDC_DIR, BM_SETCHECK, ccd_dir, 0);
  EnableWindow (GetDlgItem (ccp_hwnd, IDC_SUBDIR), ccd_dir);
  if (ccd_dir)
    SendDlgItemMessage (ccp_hwnd, IDC_SUBDIR, BM_SETCHECK, ccd_subdir, 0);
  Window *wp = selected_window ();
  EnableWindow (GetDlgItem (ccp_hwnd, IDC_DEFAULT),
                wp->w_bufp && wp->w_bufp->b_colors_enable);
  EnableWindow (GetDlgItem (ccp_hwnd, IDC_STD),
                wp->w_bufp && wp->w_bufp->b_colors_enable);
}

BOOL
ChangeColorsDialog::do_command (int id, int code)
{
  switch (id)
    {
    case IDC_STD:
      {
        SendDlgItemMessage (ccp_hwnd, IDC_STD, BM_SETSTYLE,
                            BS_PUSHBUTTON, MAKELPARAM (0, 0));
        SendMessage (ccp_hwnd, DM_SETDEFID, IDOK, 0);
        EnableWindow (GetDlgItem (ccp_hwnd, IDC_STD), 0);
        for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
          ccp_curcc[i] = Window::default_xcolors[i];
        InvalidateRect (GetDlgItem (ccp_hwnd, IDC_COLOR_LIST), 0, 0);
        return 1;
      }

    case IDC_DIR:
      EnableWindow (GetDlgItem (ccp_hwnd, IDC_SUBDIR),
                    SendDlgItemMessage (ccp_hwnd, IDC_DIR, BM_GETCHECK, 0, 0));
      return 1;

    default:
      return ChangeColorsPageP::do_command (id, code);
    }
}

void
ChangeColorsDialog::notify_color (int n)
{
  EnableWindow (GetDlgItem (ccp_hwnd, IDC_STD), 1);
  ChangeColorsPageP::notify_color (n);
}

int
ChangeColorsDialog::get_result ()
{
  ChangeColorsPageP::get_result ();

  int i = SendDlgItemMessage (ccp_hwnd, IDC_DIR, BM_GETCHECK, 0, 0);
  if (i != ccd_dir)
    {
      ccd_dir = i;
      ccp_modified = 1;
    }

  if (IsWindowEnabled (GetDlgItem (ccp_hwnd, IDC_SUBDIR)))
    {
      i = SendDlgItemMessage (ccp_hwnd, IDC_SUBDIR, BM_GETCHECK, 0, 0);
      if (i != ccd_subdir)
        {
          ccd_subdir = i;
          ccp_modified = 1;
        }
    }

  i = SendDlgItemMessage (ccp_hwnd, IDC_DEFAULT, BM_GETCHECK, 0, 0);
  if (i != ccd_default)
    {
      ccd_default = i;
      ccp_modified = 1;
    }

  return 1;
}

void
ChangeColorsDialog::init_page (PropSheet *sheet, int page_no, PROPSHEETPAGE *psp)
{
  ChangeColorsPageP::init_page (IDD_COLOR, sheet, page_no, psp);

  Window *wp = selected_window ();
  if (wp->w_bufp && wp->w_bufp->b_colors_enable)
    {
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        ccp_cc[i] = wp->w_bufp->b_colors[i];
    }
  else
    {
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        ccp_cc[i] = Window::default_xcolors[i];
    }
  memcpy (ccp_curcc, ccp_cc, sizeof ccp_cc);
  if (!ccd_dir)
    ccd_subdir = 0;
}
