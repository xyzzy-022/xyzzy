#include "stdafx.h"
#include "ed.h"

const char status_area::s_nil[] = " ";
const char status_area::s_eof[] = " EOF ";

int
status_area::char_max_ext (HDC hdc, char c1, char c2)
{
  int cx = 0;
  for (; c1 <= c2; c1++)
    {
      int x = char_ext (hdc, c1);
      cx = max (cx, x);
    }
  return cx;
}

void
status_area::clear_cache ()
{
  for (int i = 0; i < ST_MAX; i++)
    {
      s_extent[i] = -1;
      *s_lbuf[i] = 0;
    }
}

void
status_area::reload_settings ()
{
  s_hfont = HFONT (SendMessage (s_hwnd, WM_GETFONT, 0, 0));
  SendMessage (s_hwnd, SB_GETBORDERS, 0, LPARAM (s_borders));

  HDC hdc = GetDC (s_hwnd);
  HGDIOBJ of = SelectObject (hdc, s_hfont);

  int spc = char_ext (hdc, ' ');
  int colon = char_ext (hdc, ':');
  int dig = char_max_ext (hdc, '0', '9');
  int hex = char_max_ext (hdc, 'A', 'F');
  hex = max (hex, dig);
  int u = char_ext (hdc, 'U');
  int plus = char_ext (hdc, '+');

  SelectObject (hdc, of);
  ReleaseDC (s_hwnd, hdc);

  s_min_ext[ST_TIME] = 0;
  s_min_ext[ST_POS] = spc * 2 + colon + dig * 10;
  s_min_ext[ST_CODE] = spc * 2 + hex * 4;
  s_min_ext[ST_UNICODE] = spc * 2 + hex * 4 + u + plus;
  clear_cache ();
}

void
status_area::init (HWND hwnd)
{
  s_hwnd = hwnd;
  s_clwidth = 0;
  s_nitems = 1;
  s_order[0] = ST_TIME;
  s_flags = 1 << ST_TIME;
  s_dow = 0;

  s_lbuf[ST_TIME] = s_timeb;
  s_lbuf[ST_POS] = s_posb;
  s_lbuf[ST_CODE] = s_codeb;
  s_lbuf[ST_UNICODE] = s_unicodeb;
  reload_settings ();
}

void
status_area::resize ()
{
  RECT r;
  GetClientRect (s_hwnd, &r);
  s_clwidth = r.right;
  update_all ();
}

void
status_area::update_all ()
{
  clear_cache ();
  time ();
  position ();
  char_code ();
  char_unicode ();
  update (-1);
}

int
status_area::get_extent (const char *s) const
{
  HDC hdc = GetDC (s_hwnd);
  HGDIOBJ of = SelectObject (hdc, s_hfont);
  SIZE sz;
  GetTextExtentPoint32 (hdc, s, strlen (s), &sz);
  SelectObject (hdc, of);
  ReleaseDC (s_hwnd, hdc);
  return sz.cx;
}

int
status_area::calc_extent (int n, const char *b)
{
  if (!strcmp (b, s_lbuf[n]))
    return 0;
  strcpy (s_lbuf[n], b);
  int w = get_extent (b);
  w = max (w, s_min_ext[n]);
  if (w == s_extent[n])
    return 1 << n;
  s_extent[n] = w;
  return -1;
}

int
status_area::position ()
{
  Window *wp = selected_window ();
  if (!wp)
    return calc_extent (ST_POS, s_nil);
  char b[32];
  sprintf (b, " %5d:%d ", wp->w_plinenum, wp->w_column + 1);
  return calc_extent (ST_POS, b);
}

int
status_area::char_code ()
{
  Window *wp = selected_window ();
  if (!wp)
    return calc_extent (ST_CODE, s_nil);
  if (wp->w_bufp->eobp (wp->w_point))
    return calc_extent (ST_CODE, s_eof);
  char b[8];
  Char c = wp->w_point.ch ();
  sprintf (b, c < 0x100 ? " %02X " : " %04X ", c);
  return calc_extent (ST_CODE, b);
}

int
status_area::char_unicode ()
{
  Window *wp = selected_window ();
  if (!wp)
    return calc_extent (ST_UNICODE, s_nil);
  if (wp->w_bufp->eobp (wp->w_point))
    return calc_extent (ST_UNICODE, s_eof);
  ucs2_t wc = i2w (wp->w_point.ch ());
  if (wc == ucs2_t (-1))
    return calc_extent (ST_UNICODE, s_nil);
  char b[16];
  sprintf (b, " U+%04X ", wc);
  return calc_extent (ST_UNICODE, b);
}

int
status_area::time ()
{
  SYSTEMTIME st;
  GetLocalTime (&st);
  char b[32];

  if (!s_dow)
    sprintf (b, " %02d/%02d %02d:%02d ",
             st.wMonth, st.wDay,
             st.wHour, st.wMinute);
  else
    sprintf (b, " %02d/%02d(%2.2s) %02d:%02d ",
             st.wMonth, st.wDay,
             "“úŒŽ‰Î…–Ø‹à“y" + st.wDayOfWeek % 7 * 2,
             st.wHour, st.wMinute);
  return calc_extent (ST_TIME, b);
}

void
status_area::set_parts () const
{
  int w[ST_MAX + 1];
  if (s_nitems)
    {
      int cx = s_clwidth;
      if (!IsZoomed (app.toplev))
        cx -= s_borders[0] + sysdep.vscroll + sysdep.border.cx * 2;

      w[s_nitems] = cx;
      for (int i = s_nitems - 1; i >= 0; i--)
        w[i] = w[i + 1] - (s_extent[s_order[i]] + s_borders[2] * 2) - s_borders[0];
    }
  else
    w[0] = s_clwidth;
  SendMessage (s_hwnd, SB_SETPARTS, s_nitems + 1, LPARAM (w));
}

void
status_area::update (int f) const
{
  if (!f)
    return;
  if (f == -1)
    set_parts ();
  for (int i = 0; i < s_nitems; i++)
    {
      int n = s_order[i];
      if (f & (1 << n))
        SendMessage (s_hwnd, SB_SETTEXT, i + 1, LPARAM (s_lbuf[n]));
    }
}

lisp
status_area::format_modified_p ()
{
  lisp fmt = symbol_value (Vstatus_bar_format, selected_buffer ());
  if (!stringp (fmt))
    fmt = Qnil;
  lisp ofmt = xsymbol_value (Vlast_status_bar_format);
  if (fmt == ofmt
      || (stringp (fmt) && stringp (ofmt)
          && string_equal (fmt, ofmt)))
    return 0;
  xsymbol_value (Vlast_status_bar_format) = fmt;
  return fmt;
}

void
status_area::update ()
{
  if (!s_clwidth)
    return;

  lisp fmt = format_modified_p ();
  if (fmt)
    {
      if (stringp (fmt))
        parse_format (xstring_contents (fmt), xstring_length (fmt));
      else
        {
          Char c = 't';
          parse_format (&c, 1);
        }
    }
  else
    {
      int f = 0;
      if (s_flags & (1 << ST_POS))
        f |= position ();
      if (s_flags & (1 << ST_CODE))
        f |= char_code ();
      if (s_flags & (1 << ST_UNICODE))
        f |= char_unicode ();
      update (f);
    }
}

void
status_area::timer ()
{
  if (!s_clwidth || !(s_flags & (1 << ST_TIME)))
    return;
  update (time ());
}

void
status_area::parse_format (const Char *p, int l)
{
  s_nitems = 0;
  s_flags = 0;

  for (const Char *const pe = p + l; p < pe; p++)
    {
      int n;
      switch (*p)
        {
        default:
          continue;

        case 'T':
          n = ST_TIME;
          s_dow = 1;
          break;

        case 't':
          n = ST_TIME;
          s_dow = 0;
          break;

        case 'p':
          n = ST_POS;
          break;

        case 'c':
          n = ST_CODE;
          break;

        case 'u':
          n = ST_UNICODE;
          break;
        }

      if (!(s_flags & (1 << n)))
        {
          s_flags |= 1 << n;
          s_order[s_nitems++] = n;
        }
    }

  update_all ();
}
