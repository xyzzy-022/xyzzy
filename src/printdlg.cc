#include "stdafx.h"
#include "ed.h"
#include "resource.h"
#include "print.h"
#include "printdlg.h"
#include "conf.h"

static const char unit_mm[] = "mm";
static const char unit_pt[] = "pt";

class subclass_combo
{
  WNDPROC m_owndproc;
  HWND m_hwnd;
  int m_beg, m_end;
public:
  subclass_combo ()
       : m_owndproc (0), m_hwnd (0), m_beg (-1), m_end (-1) {}
  void subclass (HWND, UINT, WNDPROC);
  LRESULT wndproc (HWND, UINT, WPARAM, LPARAM);
  void insert (const char *);
};

void
subclass_combo::subclass (HWND hwnd_parent, UINT id, WNDPROC wndproc)
{
  m_hwnd = GetWindow (GetDlgItem (hwnd_parent, id), GW_CHILD);
  m_owndproc = (WNDPROC)SetWindowLong (m_hwnd, GWL_WNDPROC, LONG (wndproc));
}

LRESULT
subclass_combo::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_KILLFOCUS:
      CallWindowProc (m_owndproc, hwnd, EM_GETSEL,
                      LPARAM (&m_beg), LPARAM (&m_end));
      break;

    case WM_DESTROY:
      SetWindowLong (hwnd, GWL_WNDPROC, LONG (m_owndproc));
      break;
    }
  return CallWindowProc (m_owndproc, hwnd, msg, wparam, lparam);
}

void
subclass_combo::insert (const char *s)
{
  if (m_end < 0)
    m_end = GetWindowTextLength (m_hwnd);
  SendMessage (m_hwnd, EM_SETSEL, m_end, m_end);
  SendMessage (m_hwnd, EM_REPLACESEL, 0, LPARAM (s));
  m_end += strlen (s);
}

static subclass_combo sc_header, sc_footer;

static LRESULT CALLBACK
header_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  return sc_header.wndproc (hwnd, msg, wparam, lparam);
}

static LRESULT CALLBACK
footer_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  return sc_footer.wndproc (hwnd, msg, wparam, lparam);
}

void
print_dialog::set_margin_text (UINT edit, LONG value, const char *unit) const
{
  char b[32];
  sprintf (b, "%d.%d%s", value / 10, value % 10, unit);
  SetDlgItemText (m_hwnd, edit, b);
}

void
print_dialog::set_margin (UINT edit, UINT spin, LONG value,
                          int min, int max, int setpos,
                          const char *unit) const
{
  SendDlgItemMessage (m_hwnd, spin, UDM_SETRANGE, 0, MAKELONG (max, min));
  if (setpos)
    {
      set_margin_text (edit, value, unit);
      SendDlgItemMessage (m_hwnd, spin, UDM_SETPOS, 0, MAKELONG (value, 0));
    }
}

LONG
print_dialog::parse_margin_text (UINT edit, const char *unit) const
{
  char buf[128], *b, *be;
  GetDlgItemText (m_hwnd, edit, buf, sizeof buf);
  for (b = buf; *b == ' '; b++)
    ;
  double d = strtod (b, &be);
  if (b == be)
    return -1;
  for (; *be == ' '; be++)
    ;
  if (!*be || !strcmp (be, unit))
    return LONG (d * 10 + .5);
  return -1;
}

#define MAX_MARGIN_X (m_dev.physsize_mm ().cx \
                      - (m_dev.min_margin_mm ().left \
                         + m_dev.min_margin_mm ().right))
#define MAX_MARGIN_Y (m_dev.physsize_mm ().cy \
                      - (m_dev.min_margin_mm ().top \
                         + m_dev.min_margin_mm ().bottom))

#define MAX_LINE_SPACING 720

void
print_dialog::init_margin (int setpos) const
{
  set_margin (IDC_LEFT, IDC_LEFT_SPIN, m_settings.ps_text_margin_mm.left,
              m_dev.min_margin_mm ().left,
              MAX_MARGIN_X, setpos, unit_mm);
  set_margin (IDC_TOP, IDC_TOP_SPIN, m_settings.ps_text_margin_mm.top,
              m_dev.min_margin_mm ().top,
              MAX_MARGIN_Y, setpos, unit_mm);
  set_margin (IDC_RIGHT, IDC_RIGHT_SPIN, m_settings.ps_text_margin_mm.right,
              m_dev.min_margin_mm ().right,
              MAX_MARGIN_X, setpos, unit_mm);
  set_margin (IDC_BOTTOM, IDC_BOTTOM_SPIN, m_settings.ps_text_margin_mm.bottom,
              m_dev.min_margin_mm ().bottom,
              MAX_MARGIN_Y, setpos, unit_mm);
  set_margin (IDC_HEADER_OFFSET, IDC_HEADER_OFFSET_SPIN,
              m_settings.ps_header_offset_mm,
              0, MAX_MARGIN_Y, setpos, unit_mm);
  set_margin (IDC_FOOTER_OFFSET, IDC_FOOTER_OFFSET_SPIN,
              m_settings.ps_footer_offset_mm,
              0, MAX_MARGIN_Y, setpos, unit_mm);
  set_margin (IDC_COLUMN_SEP, IDC_COLUMN_SEP_SPIN,
              m_settings.ps_column_sep_mm,
              0, MAX_MARGIN_X, setpos, unit_mm);
}

void
print_dialog::enable_linenums (int f) const
{
  EnableWindow (GetDlgItem (m_hwnd, IDC_START_LINE), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_END_LINE), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_LINE_FROM), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_LINE_TO), f);
}

void
print_dialog::enable_pages (int f) const
{
  EnableWindow (GetDlgItem (m_hwnd, IDC_START_PAGE), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_END_PAGE), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_PAGE_FROM), f);
  EnableWindow (GetDlgItem (m_hwnd, IDC_PAGE_TO), f);
}

void
print_dialog::update_ncopies () const
{
  int ncopies = m_dev.max_copies () > 1;
  int collate = ncopies && m_dev.dm_fields () & DM_COLLATE;

  EnableWindow (GetDlgItem (m_hwnd, IDC_NCOPIES), ncopies);
  EnableWindow (GetDlgItem (m_hwnd, IDC_NCOPIES_STATIC), ncopies);
  EnableWindow (GetDlgItem (m_hwnd, IDC_NCOPIES_SPIN), ncopies);
  EnableWindow (GetDlgItem (m_hwnd, IDC_COLLATE), collate);

  if (ncopies)
    {
      SetDlgItemInt (m_hwnd, IDC_NCOPIES, m_settings.ps_ncopies, 0);
      SendDlgItemMessage (m_hwnd, IDC_NCOPIES_SPIN, UDM_SETRANGE,
                          0, MAKELONG (m_dev.max_copies (), 1));
      SendDlgItemMessage (m_hwnd, IDC_NCOPIES_SPIN, UDM_SETPOS,
                          0, MAKELONG (m_settings.ps_ncopies, 0));
      if (collate)
        SendDlgItemMessage (m_hwnd, IDC_COLLATE, BM_SETCHECK,
                            m_settings.ps_collate ? 1 : 0, 0);
    }
}

void
print_dialog::add_lang () const
{
  for (int i = 0; i < FONT_MAX; i++)
    {
      char buf[128];
      *buf = 0;
      LoadString (app.hinst, FontSet::lang_id (i), buf, sizeof buf);
      int idx = SendDlgItemMessage (m_hwnd, IDC_LANG, CB_ADDSTRING, 0, LPARAM (buf));
      SendDlgItemMessage (m_hwnd, IDC_LANG, CB_SETITEMDATA, idx, i);
    }
  SendDlgItemMessage (m_hwnd, IDC_LANG, CB_SETCURSEL, FONT_ASCII, 0);
  set_font_face (FONT_ASCII);
}

int
print_dialog::get_int (UINT id, BOOL *f, BOOL sign, int defalt) const
{
  int v = GetDlgItemInt (m_hwnd, id, f, sign);
  if (*f)
    return v;
  int l = GetWindowTextLength (GetDlgItem (m_hwnd, id));
  if (l)
    {
      char *b = (char *)alloca (l + 1);
      GetDlgItemText (m_hwnd, id, b, l + 1);
      for (; *b == ' ' || *b == '\t'; b++)
        ;
      if (*b)
        return 0;
    }
  *f = 1;
  return defalt;
}

int
print_dialog::check_margin_text (UINT id, LONG &r, LONG min, LONG max,
                                 const char *unit) const
{
  LONG x = parse_margin_text (id, unit);
  if (x < 0)
    {
      notice (id, IDS_WRONG_MARGIN);
      return 0;
    }
  if (x < min)
    {
      notice (id, IDS_MARGIN_TOO_SMALL);
      return 0;
    }
  if (x > max)
    {
      notice (id, IDS_MARGIN_TOO_LARGE);
      return 0;
    }
  r = x;
  return 1;
}


int
print_dialog::get_result (int save)
{
  GetDlgItemText (m_hwnd, IDC_HEADER, m_settings.ps_header, sizeof m_settings.ps_header);
  GetDlgItemText (m_hwnd, IDC_FOOTER, m_settings.ps_footer, sizeof m_settings.ps_footer);
  m_settings.ps_header_on = IsDlgButtonChecked (m_hwnd, IDC_CHEADER) == 1;
  m_settings.ps_footer_on = IsDlgButtonChecked (m_hwnd, IDC_CFOOTER) == 1;

  BOOL f;

  if (IsDlgButtonChecked (m_hwnd, IDC_RANGE_ALL))
    m_settings.ps_print_range = print_settings::RANGE_ALL;
  else if (IsDlgButtonChecked (m_hwnd, IDC_RANGE_LINE))
    {
      m_settings.ps_print_range = print_settings::RANGE_LINENUM;
      m_settings.ps_range_start = get_int (IDC_START_LINE, &f, 0, 1);
      if (!f || m_settings.ps_range_start <= 0)
        return notice (IDC_START_LINE, IDS_START_LINE);
      m_settings.ps_range_end = get_int (IDC_END_LINE, &f, 0, INT_MAX);
      if (!f || m_settings.ps_range_end <= 0)
        return notice (IDC_END_LINE, IDS_END_LINE);
    }
  else if (IsDlgButtonChecked (m_hwnd, IDC_RANGE_PAGE))
    {
      m_settings.ps_print_range = print_settings::RANGE_PAGE;
      m_settings.ps_range_start = get_int (IDC_START_PAGE, &f, 0, 1);
      if (!f || m_settings.ps_range_start <= 0)
        return notice (IDC_START_PAGE, IDS_START_PAGE);
      m_settings.ps_range_end = get_int (IDC_END_PAGE, &f, 0, -1);
      if (!f || m_settings.ps_range_end <= 0)
        return notice (IDC_END_PAGE, IDS_END_PAGE);
    }
  else
    m_settings.ps_print_range = print_settings::RANGE_SELECTION;

  m_settings.ps_print_linenum = IsDlgButtonChecked (m_hwnd, IDC_LINE_NUMBER) == 1;
  update_font_size ();

  if (!get_copies ())
    return 0;
  m_dev.set_dev_copies (m_settings);

  m_settings.ps_show_proportional = IsDlgButtonChecked (m_hwnd, IDC_PROPORTIONAL) == 1;
  m_settings.ps_use_bitmap = IsDlgButtonChecked (m_hwnd, IDC_USE_BITMAP) == 1;

  m_settings.ps_multi_column = GetDlgItemInt (m_hwnd, IDC_MULTI_COLUMN, &f, 0);
  if (!f || m_settings.ps_multi_column < 1
      || m_settings.ps_multi_column > print_settings::COLUMN_MAX)
    return notice (IDC_MULTI_COLUMN, IDS_MULTI_COLUMN);

  if (IsWindowEnabled (GetDlgItem (m_hwnd, IDC_FOLD)))
    {
      m_settings.ps_fold_width = GetDlgItemInt (m_hwnd, IDC_FOLD, &f, 0);
      if (!f || m_settings.ps_fold_width < 0)
        return notice (IDC_FOLD, IDS_FOLD);
    }

  if (!check_margin_text (IDC_LEFT, m_settings.ps_text_margin_mm.left,
                          m_dev.min_margin_mm ().left,
                          MAX_MARGIN_X, unit_mm)
      || !check_margin_text (IDC_TOP, m_settings.ps_text_margin_mm.top,
                             m_dev.min_margin_mm ().top,
                             MAX_MARGIN_Y, unit_mm)
      || !check_margin_text (IDC_RIGHT, m_settings.ps_text_margin_mm.right,
                             m_dev.min_margin_mm ().right,
                             MAX_MARGIN_X, unit_mm)
      || !check_margin_text (IDC_BOTTOM, m_settings.ps_text_margin_mm.bottom,
                             m_dev.min_margin_mm ().bottom,
                             MAX_MARGIN_Y, unit_mm)
      || (m_settings.ps_header_on
          && !check_margin_text (IDC_HEADER_OFFSET, m_settings.ps_header_offset_mm,
                                 0, MAX_MARGIN_Y, unit_mm))
      || (m_settings.ps_footer_on
          && !check_margin_text (IDC_FOOTER_OFFSET, m_settings.ps_footer_offset_mm,
                                 0, MAX_MARGIN_Y, unit_mm))
      || !check_margin_text (IDC_COLUMN_SEP, m_settings.ps_column_sep_mm,
                             0, MAX_MARGIN_X, unit_mm)
      || !check_margin_text (IDC_LINE_SPACING, m_settings.ps_line_spacing_pt,
                             0, MAX_LINE_SPACING, unit_pt))
    return 0;

  int r = m_engine.init (m_dev, 0, 1);
  if (r)
    return print_engine::init_error (m_hwnd, r, IDC_LEFT, IDC_TOP, IDC_FOLD,
                                     IDC_HEADER_OFFSET, IDC_FOOTER_OFFSET);

  if (m_settings.ps_print_range == print_settings::RANGE_PAGE
      && !m_engine.set_start_end (m_dev, 0,
                                  m_settings.ps_range_start,
                                  m_settings.ps_range_end))
    return print_engine::bad_range (m_hwnd);

  if (save)
    {
      save_history (IDC_HEADER, cfgHeader);
      save_history (IDC_FOOTER, cfgFooter);
    }

  return 1;
}

int
print_dialog::get_copies ()
{
  if (m_dev.max_copies () > 1)
    {
      BOOL f;
      int ncopies = get_int (IDC_NCOPIES, &f, 0, -1);
      if (ncopies <= 0 || ncopies > m_dev.max_copies ())
        return notice (IDC_NCOPIES, IDS_WRONG_NCOPIES, m_dev.max_copies ());
      m_settings.ps_ncopies = ncopies;
      m_settings.ps_collate = (m_dev.dm_fields () & DM_COLLATE
                               ? IsDlgButtonChecked (m_hwnd, IDC_COLLATE) == 1
                               : 0);
    }
  else
    {
      m_settings.ps_ncopies = 1;
      m_settings.ps_collate = 0;
    }
  return 1;
}

void
print_dialog::init_history (UINT id_combo, const char *section)
{
  HWND hwnd_combo = GetDlgItem (m_hwnd, id_combo);
  SendMessage (hwnd_combo, CB_ADDSTRING, 0, LPARAM (""));
  char kbuf[4096];
  memset (kbuf, 0, sizeof kbuf);
  read_conf (section, 0, kbuf, sizeof kbuf);
  for (const char *key = kbuf; *key; key += strlen (key) + 1)
    {
      char buf[MAX_HEADER_LENGTH];
      if (read_conf (section, key, buf, sizeof buf))
        SendMessage (hwnd_combo, CB_ADDSTRING, 0, LPARAM (buf));
    }
}

void
print_dialog::save_history (UINT id_combo, const char *section)
{
  HWND hwnd_combo = GetDlgItem (m_hwnd, id_combo);
  delete_conf (section);
  int n = SendMessage (hwnd_combo, CB_GETCOUNT, 0, 0);
  for (int i = 0; i < n; i++)
    {
      // WinME の CB_GETLBTEXTLEN は文字数を返すらしいので(ただし未確認)、
      // バッファを倍にしておく。
      char buf[MAX_HEADER_LENGTH * 2 + 2];
      int l = SendMessage (hwnd_combo, CB_GETLBTEXTLEN, i, 0);
      if (l > 0 && l < MAX_HEADER_LENGTH
          && SendMessage (hwnd_combo, CB_GETLBTEXT, i, LPARAM (buf)) > 0)
        {
          char key[32];
          sprintf (key, "%d", i);
          conf_write_string (section, key, buf);
        }
    }
}

int
print_dialog::init_dialog (HWND)
{
  center_window (m_hwnd);
  set_window_icon (m_hwnd);

  init_history (IDC_HEADER, cfgHeader);
  init_history (IDC_FOOTER, cfgFooter);

  SetDlgItemText (m_hwnd, IDC_HEADER, m_settings.ps_header);
  SetDlgItemText (m_hwnd, IDC_FOOTER, m_settings.ps_footer);

  add_history (IDC_HEADER, IDC_ADD_HEADER, IDC_DELETE_HEADER, BN_CLICKED);
  add_history (IDC_FOOTER, IDC_ADD_FOOTER, IDC_DELETE_FOOTER, BN_CLICKED);

  history_command (IDC_HEADER, CBN_EDITCHANGE, IDC_ADD_HEADER, IDC_DELETE_HEADER);
  history_command (IDC_FOOTER, CBN_EDITCHANGE, IDC_ADD_FOOTER, IDC_DELETE_FOOTER);

  SendDlgItemMessage (m_hwnd, IDC_CHEADER, BM_SETCHECK,
                      m_settings.ps_header_on ? 1 : 0, 0);
  SendDlgItemMessage (m_hwnd, IDC_CFOOTER, BM_SETCHECK,
                      m_settings.ps_footer_on ? 1 : 0, 0);

  init_margin (1);
  add_lang ();
  check_proportional_font ();

  set_margin (IDC_LINE_SPACING, IDC_LINE_SPACING_SPIN,
              m_settings.ps_line_spacing_pt, 0, MAX_LINE_SPACING,
              1, unit_pt);

  SendDlgItemMessage (m_hwnd, IDC_RECOMMEND_SIZE, BM_SETCHECK,
                      m_settings.ps_recommend_size ? 1 : 0, 0);
  SendDlgItemMessage (m_hwnd, IDC_PROPORTIONAL, BM_SETCHECK,
                      m_settings.ps_show_proportional ? 1 : 0, 0);
  SendDlgItemMessage (m_hwnd, IDC_USE_BITMAP, BM_SETCHECK,
                      m_settings.ps_use_bitmap ? 1 : 0, 0);

  CheckRadioButton (m_hwnd, IDC_RANGE_ALL, IDC_RANGE_PAGE,
                    IDC_RANGE_ALL + m_settings.ps_print_range);
  EnableWindow (GetDlgItem (m_hwnd, IDC_RANGE_SELECT),
                m_settings.ps_print_range == print_settings::RANGE_SELECTION);

  enable_linenums (m_settings.ps_print_range == print_settings::RANGE_LINENUM);
  enable_pages (m_settings.ps_print_range == print_settings::RANGE_PAGE);
  update_ncopies ();

  SendDlgItemMessage (m_hwnd, IDC_LINE_NUMBER, BM_SETCHECK,
                      m_settings.ps_print_linenum ? 1 : 0, 0);

  SendDlgItemMessage (m_hwnd, IDC_MULTI_COLUMN_SPIN, UDM_SETRANGE,
                      0, MAKELONG (print_settings::COLUMN_MAX, 1));
  SendDlgItemMessage (m_hwnd, IDC_MULTI_COLUMN_SPIN, UDM_SETPOS,
                      0, MAKELONG (m_settings.ps_multi_column, 0));
  SetDlgItemInt (m_hwnd, IDC_MULTI_COLUMN, m_settings.ps_multi_column, 0);

  SendDlgItemMessage (m_hwnd, IDC_FOLD_SPIN, UDM_SETRANGE,
                      0, MAKELONG (1000, 0));
  SendDlgItemMessage (m_hwnd, IDC_FOLD_SPIN, UDM_SETPOS,
                      0, MAKELONG (m_settings.ps_fold_width, 0));
  SetDlgItemInt (m_hwnd, IDC_FOLD, m_settings.ps_fold_width, 0);

  sc_header.subclass (m_hwnd, IDC_HEADER, header_wndproc);
  sc_footer.subclass (m_hwnd, IDC_FOOTER, footer_wndproc);

  return 0;
}

BOOL
print_dialog::destroy ()
{
  return 0;
}

BOOL
print_dialog::notify_spin (NMHDR *nm, const char *unit)
{
  if (nm->code != UDN_DELTAPOS)
    return 0;
  int range = SendMessage (nm->hwndFrom, UDM_GETRANGE, 0, 0);
  NM_UPDOWN *u = (NM_UPDOWN *)nm;
  if (u->iPos + u->iDelta < HIWORD (range)
      || u->iPos + u->iDelta > LOWORD (range))
    return 1;

  set_margin_text (GetWindowLong (HWND (SendMessage (nm->hwndFrom,
                                                     UDM_GETBUDDY, 0, 0)),
                                  GWL_ID),
                   u->iPos + u->iDelta, unit);
  return 0;
}

BOOL
print_dialog::notify (NMHDR *nm)
{
  switch (nm->idFrom)
    {
    case IDC_LEFT_SPIN:
    case IDC_TOP_SPIN:
    case IDC_RIGHT_SPIN:
    case IDC_BOTTOM_SPIN:
    case IDC_HEADER_OFFSET_SPIN:
    case IDC_FOOTER_OFFSET_SPIN:
    case IDC_COLUMN_SEP_SPIN:
      return notify_spin (nm, unit_mm);

    case IDC_LINE_SPACING_SPIN:
      return notify_spin (nm, unit_pt);
    }
  return 0;
}

BOOL
print_dialog::ok (UINT id)
{
  if (!get_result (1))
    return 1;
  EndDialog (m_hwnd, id);
  return 1;
}

BOOL
print_dialog::cancel (UINT)
{
  EndDialog (m_hwnd, IDCANCEL);
  return 1;
}

BOOL
print_dialog::clicked (UINT id, int code) const
{
  if (code == BN_CLICKED)
    {
      enable_linenums (id == IDC_RANGE_LINE);
      enable_pages (id == IDC_RANGE_PAGE);
      CheckDlgButton (m_hwnd, IDC_RANGE_ALL, id == IDC_RANGE_ALL);
      CheckDlgButton (m_hwnd, IDC_RANGE_LINE, id == IDC_RANGE_LINE);
      CheckDlgButton (m_hwnd, IDC_RANGE_SELECT, id == IDC_RANGE_SELECT);
      CheckDlgButton (m_hwnd, IDC_RANGE_PAGE, id == IDC_RANGE_PAGE);
    }
  return 1;
}

BOOL
print_dialog::range_command (UINT id, int code, UINT spin,
                             LONG defalt, const char *unit) const
{
  if (code != EN_KILLFOCUS)
    return 0;

  LONG val = parse_margin_text (id, unit);
  if (val < 0)
    val = defalt;
  int range = SendDlgItemMessage (m_hwnd, spin, UDM_GETRANGE, 0, 0);
  val = min (max (val, LONG (HIWORD (range))), LONG (LOWORD (range)));
  SendDlgItemMessage (m_hwnd, spin, UDM_SETPOS, 0, MAKELONG (short (val), 0));

  set_margin_text (id, val, unit);

  return 1;
}

int
print_dialog::current_lang () const
{
  int i = SendDlgItemMessage (m_hwnd, IDC_LANG, CB_GETCURSEL, 0, 0);
  if (i == CB_ERR)
    return -1;
  i = SendDlgItemMessage (m_hwnd, IDC_LANG, CB_GETITEMDATA, i, 0);
  return i >= 0 && i < FONT_MAX ? i : -1;
}

void
print_dialog::set_font_face (int lang) const
{
  char buf[LF_FACESIZE + 32];
  char point[32];
  if (m_settings.ps_font[lang].point % 10)
    sprintf (point, "%d.%d",
             m_settings.ps_font[lang].point / 10,
             m_settings.ps_font[lang].point % 10);
  else
    sprintf (point, "%d", m_settings.ps_font[lang].point / 10);
  sprintf (buf, "%s, %s",
           m_settings.ps_font[lang].face,
           point);
  SetDlgItemText (m_hwnd, IDC_FACE, buf);
}

int
print_dialog::lang_command (int code) const
{
  if (code != CBN_SELCHANGE)
    return 0;
  int lang = current_lang ();
  if (lang < 0)
    return 0;
  set_font_face (lang);
  return 0;
}

void
print_dialog::update_font_size ()
{
  m_settings.ps_recommend_size = IsDlgButtonChecked (m_hwnd, IDC_RECOMMEND_SIZE) == 1;
  m_settings.ps_show_proportional = IsDlgButtonChecked (m_hwnd, IDC_PROPORTIONAL) == 1;
  if (m_settings.ps_recommend_size)
    for (int i = 0; i < FONT_MAX; i++)
      m_settings.ps_font[i].point = m_settings.ps_font[FONT_ASCII].point;
}

void
print_dialog::check_proportional_font () const
{
  int fixed = 1;
  for (int i = 0; i < FONT_MAX; i++)
    {
      HGDIOBJ of = SelectObject (m_dev, m_settings.make_font (m_dev, m_dev, i));
      TEXTMETRIC tm;
      GetTextMetrics (m_dev, &tm);
      DeleteObject (SelectObject (m_dev, of));
      if (tm.tmPitchAndFamily & TMPF_FIXED_PITCH)
        fixed = 0;
    }
  EnableWindow (GetDlgItem (m_hwnd, IDC_FOLD), fixed);
  EnableWindow (GetDlgItem (m_hwnd, IDC_FOLD_STATIC), fixed);
}

int
print_dialog::recommend_size ()
{
  update_font_size ();
  int lang = current_lang ();
  if (lang >= 0)
    set_font_face (lang);
  return 0;
}

BOOL
print_dialog::set_font ()
{
  int lang = current_lang ();
  if (lang < 0)
    return 0;

  update_font_size ();

  LOGFONT lf;
  bzero (&lf, sizeof lf);
  strcpy (lf.lfFaceName, m_settings.ps_font[lang].face);
  HDC hdc = GetDC (m_hwnd);
  lf.lfHeight = MulDiv (m_settings.ps_font[lang].point, GetDeviceCaps (hdc, LOGPIXELSY), 720);
  ReleaseDC (m_hwnd, hdc);
  lf.lfCharSet = m_settings.ps_font[lang].charset;
  lf.lfItalic = m_settings.ps_font[lang].italic;
  if (m_settings.ps_font[lang].bold)
    lf.lfWeight = 700;

  CHOOSEFONT cf;
  bzero (&cf, sizeof cf);
  cf.lStructSize = sizeof cf;
  cf.hwndOwner = m_hwnd;
  cf.hDC = m_dev;
  cf.lpLogFont = &lf;
  cf.Flags = (CF_FORCEFONTEXIST
              | CF_INITTOLOGFONTSTRUCT | CF_LIMITSIZE
              | CF_NOSIMULATIONS | CF_NOVECTORFONTS
              | CF_PRINTERFONTS);
  if (!m_settings.ps_show_proportional)
    cf.Flags |= CF_FIXEDPITCHONLY;

  cf.nSizeMin = 5;
  cf.nSizeMax = 72;
  if (ChooseFont (&cf))
    {
      strcpy (m_settings.ps_font[lang].face, lf.lfFaceName);
      m_settings.ps_font[lang].charset = lf.lfCharSet;
      m_settings.ps_font[lang].point = cf.iPointSize;
      m_settings.ps_font[lang].italic = lf.lfItalic;
      m_settings.ps_font[lang].bold = lf.lfWeight >= 700;
      update_font_size ();
      set_font_face (lang);
      check_proportional_font ();
    }
  return 0;
}

BOOL
print_dialog::print_setup ()
{
  if (!get_copies ())
    return 1;
  m_dev.set_dev_copies (m_settings);
  if (!m_dev.print_setup_dialog (m_hwnd))
    return 1;
  m_dev.get_dev_copies (m_settings);
  update_ncopies ();
  init_margin (0);
  check_proportional_font ();
  return 1;
}

int
print_dialog::preview ()
{
  if (!get_result (0))
    return IDOK;

  int r = m_engine.preview (m_hwnd);
  Fdo_events ();
  if (!r)
    print_engine::bad_range (m_hwnd);
  return r;
}

int
print_dialog::find_history (UINT id, const char *s)
{
  char *buf = (char *)alloca (strlen (s) * 2 + 2);
  int i = -1;
  while (1)
    {
      int o = i;
      i = SendDlgItemMessage (m_hwnd, id, CB_FINDSTRINGEXACT, WPARAM (i), LPARAM (s));
      if (i == CB_ERR || i <= o)
        return -1;
      if (!*s)
        return i;
      if (SendDlgItemMessage (m_hwnd, id, CB_GETLBTEXT, i, LPARAM (buf)) != CB_ERR
          && !strcmp (s, buf))
        return i;
    }
}

int
print_dialog::history_command (UINT id_combo, UINT code, UINT id_add, UINT id_del)
{
  int fadd, fdel;

  switch (code)
    {
    case CBN_SELCHANGE:
      {
        int n = SendDlgItemMessage (m_hwnd, id_combo, CB_GETCURSEL, 0, 0);
        if (n == CB_ERR)
          return 0;
        fadd = 0;
        fdel = SendDlgItemMessage (m_hwnd, id_combo, CB_GETLBTEXTLEN, n, 0) > 0;
        break;
      }

    case CBN_EDITCHANGE:
      {
        char buf[MAX_HEADER_LENGTH];
        if (!GetDlgItemText (m_hwnd, id_combo, buf, sizeof buf))
          fadd = fdel = 0;
        else
          {
            fadd = find_history (id_combo, buf) < 0;
            fdel = !fadd;
          }
        break;
      }

    default:
      return 0;
    }

  EnableWindow (GetDlgItem (m_hwnd, id_add), fadd);
  EnableWindow (GetDlgItem (m_hwnd, id_del), fdel);
  return 1;
}

void
print_dialog::move_btn_focus (UINT from, UINT to)
{
  if (GetFocus () == GetDlgItem (m_hwnd, from))
    {
      SetFocus (GetDlgItem (m_hwnd, to));
      SendDlgItemMessage (m_hwnd, from, BM_SETSTYLE, BS_PUSHBUTTON, MAKELPARAM (1, 0));
      SendMessage (m_hwnd, DM_SETDEFID, IDOK, 0);
    }
}

int
print_dialog::add_history (UINT id_combo, UINT id_add, UINT id_del, UINT code)
{
  if (code != BN_CLICKED)
    return 0;

  char buf[MAX_HEADER_LENGTH];
  if (GetDlgItemText (m_hwnd, id_combo, buf, sizeof buf)
      && find_history (id_combo, buf) < 0)
    SendDlgItemMessage (m_hwnd, id_combo, CB_ADDSTRING, 0, LPARAM (buf));
  move_btn_focus (id_add, id_combo);
  history_command (id_combo, CBN_EDITCHANGE, id_add, id_del);
  return 1;
}

int
print_dialog::delete_history (UINT id_combo, UINT id_add, UINT id_del, UINT code)
{
  if (code != BN_CLICKED)
    return 0;

  char buf[MAX_HEADER_LENGTH];
  if (GetDlgItemText (m_hwnd, id_combo, buf, sizeof buf))
    {
      int n = find_history (id_combo, buf);
      if (n >= 0 && SendDlgItemMessage (m_hwnd, id_combo,
                                        CB_DELETESTRING, n, 0) != CB_ERR)
        SetDlgItemText (m_hwnd, id_combo, "");
    }
  move_btn_focus (id_del, id_combo);
  history_command (id_combo, CBN_EDITCHANGE, id_add, id_del);
  return 1;
}

int
print_dialog::find_menu_text (HMENU hmenu, int id, char *buf, int size)
{
  if (GetMenuString (hmenu, id, buf, size, MF_BYCOMMAND))
    return 1;
  for (int i = GetMenuItemCount (hmenu) - 1; i >= 0; i--)
    {
      HMENU hsub = GetSubMenu (hmenu, i);
      if (hsub && GetMenuString (hsub, id, buf, size, MF_BYCOMMAND))
        return 1;
    }
  return 0;
}

int
print_dialog::format_popup (UINT id_btn, subclass_combo &sc)
{
  HWND hwnd_btn = GetDlgItem (m_hwnd, id_btn);
  RECT r;
  GetWindowRect (hwnd_btn, &r);

  HMENU hmenu = LoadMenu (app.hinst, MAKEINTRESOURCE (IDM_PRINTFMT));
  HMENU hsub = GetSubMenu (hmenu, 0);
  int cmd = TrackPopupMenu (hsub, (TPM_LEFTALIGN | TPM_LEFTBUTTON | TPM_TOPALIGN
                                   | TPM_NONOTIFY | TPM_RETURNCMD),
                            r.right, r.top, 0, m_hwnd, 0);

  char text[256];
  if (cmd <= 0 || !find_menu_text (hsub, cmd, text, sizeof text))
    *text = 0;

  DestroyMenu (hmenu);

  char *p = strchr (text, '\t');
  if (p)
    sc.insert (p + 1);

  return 1;
}

BOOL
print_dialog::command (UINT id, UINT code)
{
  switch (id)
    {
    case IDOK:
    case IDC_CLOSE:
      return ok (id);

    case IDCANCEL:
      return cancel (code);

    case IDC_RANGE_ALL:
    case IDC_RANGE_LINE:
    case IDC_RANGE_SELECT:
    case IDC_RANGE_PAGE:
      return clicked (id, code);

    case IDC_LEFT:
      return range_command (id, code, IDC_LEFT_SPIN,
                            m_settings.ps_text_margin_mm.left, unit_mm);

    case IDC_TOP:
      return range_command (id, code, IDC_TOP_SPIN,
                            m_settings.ps_text_margin_mm.top, unit_mm);

    case IDC_RIGHT:
      return range_command (id, code, IDC_RIGHT_SPIN,
                            m_settings.ps_text_margin_mm.right, unit_mm);

    case IDC_BOTTOM:
      return range_command (id, code, IDC_BOTTOM_SPIN,
                            m_settings.ps_text_margin_mm.bottom, unit_mm);

    case IDC_HEADER_OFFSET:
      return range_command (id, code, IDC_HEADER_OFFSET_SPIN,
                            m_settings.ps_header_offset_mm, unit_mm);

    case IDC_FOOTER_OFFSET:
      return range_command (id, code, IDC_FOOTER_OFFSET_SPIN,
                            m_settings.ps_footer_offset_mm, unit_mm);

    case IDC_COLUMN_SEP:
      return range_command (id, code, IDC_COLUMN_SEP_SPIN,
                            m_settings.ps_column_sep_mm, unit_mm);

    case IDC_LINE_SPACING:
      return range_command (id, code, IDC_LINE_SPACING_SPIN,
                            m_settings.ps_line_spacing_pt, unit_pt);

    case IDC_PRINT_SETUP:
      return print_setup ();

    case IDC_PREVIEW:
      if (preview () == IDC_PRINT)
        return ok (IDOK);
      return 1;

    case IDC_FONT:
      return set_font ();

    case IDC_LANG:
      return lang_command (code);

    case IDC_RECOMMEND_SIZE:
      return recommend_size ();

    case IDC_HEADER:
      return history_command (id, code, IDC_ADD_HEADER, IDC_DELETE_HEADER);

    case IDC_FOOTER:
      return history_command (id, code, IDC_ADD_FOOTER, IDC_DELETE_FOOTER);

    case IDC_ADD_HEADER:
      return add_history (IDC_HEADER, IDC_ADD_HEADER, IDC_DELETE_HEADER, code);

    case IDC_ADD_FOOTER:
      return add_history (IDC_FOOTER, IDC_ADD_FOOTER, IDC_DELETE_FOOTER, code);

    case IDC_DELETE_HEADER:
      return delete_history (IDC_HEADER, IDC_ADD_HEADER, IDC_DELETE_HEADER, code);

    case IDC_DELETE_FOOTER:
      return delete_history (IDC_FOOTER, IDC_ADD_FOOTER, IDC_DELETE_FOOTER, code);

    case IDC_HEADER_POPUP:
      return format_popup (IDC_HEADER_POPUP, sc_header);

    case IDC_FOOTER_POPUP:
      return format_popup (IDC_FOOTER_POPUP, sc_footer);
    }
  return 0;
}

BOOL
print_dialog::quit ()
{
  return cancel (0);
}

BOOL
print_dialog::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      if (init_dialog (HWND (wparam)) < 0)
        EndDialog (m_hwnd, 0);
      return 1;

    case WM_DESTROY:
      return destroy ();

    case WM_NOTIFY:
      return notify ((NMHDR *)lparam);

    case WM_COMMAND:
      return command (LOWORD (wparam), HIWORD (wparam));

    case WM_PRIVATE_QUIT:
      return quit ();

    case WM_ACTIVATEAPP:
      PostThreadMessage (app.quit_thread_id, WM_PRIVATE_ACTIVATEAPP,
                         wparam, lparam);
      return 0;

    default:
      return 0;
    }
}

BOOL CALLBACK
print_dialog::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  print_dialog *p;
  if (msg == WM_INITDIALOG)
    {
      p = (print_dialog *)lparam;
      SetWindowLong (hwnd, DWL_USER, lparam);
      p->m_hwnd = hwnd;
    }
  else
    {
      p = (print_dialog *)GetWindowLong (hwnd, DWL_USER);
      if (!p)
        return 0;
    }
  return p->wndproc (msg, wparam, lparam);
}

int
print_dialog::do_modal (HWND hwnd)
{
  return DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_PRINT),
                         hwnd, wndproc, LPARAM (this));
}

static lisp
print_buffer (lisp buffer, int show_dlg)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);

  printer_device dev;
  Fbegin_wait_cursor ();
  int r = dev.init ();
  Fend_wait_cursor ();
  if (!r)
    return Qnil;

  print_settings settings;
  settings.load_conf ();
  print_engine engine (bp, dev, settings);
  if (show_dlg)
    {
      if (bp == selected_buffer ()
          && selected_window ()->w_selection_type != Buffer::SELECTION_VOID)
        settings.ps_print_range = print_settings::RANGE_SELECTION;

      print_dialog d (dev, settings, engine);
      r = d.do_modal (get_active_window ());
      if (r == IDOK || r == IDC_CLOSE)
        settings.save_conf ();

      Fdo_events ();
      if (r != IDOK)
        return Qnil;
    }

  if (!dev.create_printer_dc ())
    {
      warn_msgbox (EPDERR_INITFAILURE);
      return Qnil;
    }

  engine.doprint (get_active_window ());

  return Qt;
}

lisp
Fprint_dialog (lisp buffer)
{
  return print_buffer (buffer, 1);
}

lisp
Fprint_buffer (lisp buffer)
{
  return print_buffer (buffer, 0);
}
