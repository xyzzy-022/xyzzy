#include "stdafx.h"
#include "ed.h"
#include "conf.h"
#include "ipc.h"
#include "wheel.h"

#define RULER_HEIGHT 13
#define FRAME_WIDTH 2

int Window::w_hjump_columns = 8;
int Window::w_default_flags = (WF_LINE_NUMBER | WF_RULER | WF_NEWLINE | WF_MODE_LINE
                               | WF_VSCROLL_BAR | WF_EOF | WF_FOLD_MARK);
WindowConfiguration *WindowConfiguration::wc_chain;

#define TXF WCOLOR_TEXT
#define TXB WCOLOR_BACK
#define CXF WCOLOR_CTRL
#define K1F WCOLOR_KWD1
#define K2F WCOLOR_KWD2
#define K3F WCOLOR_KWD3
#define STF WCOLOR_STRING
#define CMF WCOLOR_COMMENT
#define TGF WCOLOR_TAG
#define GRF WCOLOR_GRAY
#define HIF WCOLOR_HIGHLIGHT_TEXT
#define HIB WCOLOR_HIGHLIGHT
#define BTF WCOLOR_BTNTEXT
#define BTB WCOLOR_BTNSHADOW
#define RVB WCOLOR_REVERSE
#define LNF WCOLOR_LINENUM

wcolor_index Window::forecolor_indexes[] =
{
//nrm ctl kw1 kw2 kw3 k1r k2r k3r -   -   -    -  lnm  str tag com   // 反転 選択 無効
  TXF,CXF,K1F,K2F,K3F,TXB,TXB,TXB,TXF,TXF,TXF,TXF,LNF,STF,TGF,CMF,  //   -    -    -
  GRF,GRF,GRF,GRF,GRF,TXB,TXB,TXB,GRF,GRF,GRF,GRF,LNF,GRF,GRF,GRF,  //   -    -    o
  HIF,CXF,HIF,HIF,HIF,HIF,HIF,HIF,HIF,HIF,HIF,HIF,LNF,HIF,HIF,HIF,  //   -    o    -
  TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   -    o    o
  TXB,CXF,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   o    -    -
  TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   o    -    o
  BTF,CXF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,BTF,  //   o    o    -
  TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   o    o    o
};

wcolor_index Window::backcolor_indexes[] =
{
//nrm ctl kw1 kw2 kw3 k1r k2r k3r -   -   -    -  lnm str tag com   // 反転 選択 無効
  TXB,TXB,TXB,TXB,TXB,K1F,K2F,K3F,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   -    -    -
  TXB,TXB,TXB,TXB,TXB,GRF,GRF,GRF,TXB,TXB,TXB,TXB,TXB,TXB,TXB,TXB,  //   -    -    o
  HIB,HIB,HIB,HIB,HIB,GRF,GRF,GRF,HIB,HIB,HIB,HIB,HIB,HIB,HIB,HIB,  //   -    o    -
  GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,  //   -    o    o
  RVB,RVB,RVB,RVB,RVB,RVB,RVB,RVB,RVB,RVB,RVB,RVB,LNF,RVB,RVB,RVB,  //   o    -    -
  GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,  //   o    -    o
  BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,BTB,  //   o    o    -
  GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,GRF,  //   o    o    o
};

COLORREF Window::default_colors[WCOLOR_MAX];
XCOLORREF Window::default_xcolors[USER_DEFINABLE_COLORS];
COLORREF Window::modeline_colors[2];
XCOLORREF Window::modeline_xcolors[2];

COLORREF Window::w_textprop_forecolor[GLYPH_TEXTPROP_NCOLORS] =
{
  RGB (0x00, 0x00, 0x00),
  RGB (0xff, 0x00, 0x00),
  RGB (0x00, 0xff, 0x00),
  RGB (0xff, 0xff, 0x00),
  RGB (0x00, 0x00, 0xff),
  RGB (0xff, 0x00, 0xff),
  RGB (0x00, 0xff, 0xff),
  RGB (0xff, 0xff, 0xff),
  RGB (0x00, 0x00, 0x00),
  RGB (0x80, 0x00, 0x00),
  RGB (0x00, 0x80, 0x00),
  RGB (0x80, 0x80, 0x00),
  RGB (0x00, 0x00, 0x80),
  RGB (0x80, 0x00, 0x80),
  RGB (0x00, 0x80, 0x80),
  RGB (0x80, 0x80, 0x80),
};

COLORREF Window::w_textprop_backcolor[GLYPH_TEXTPROP_NCOLORS] =
{
  RGB (0x00, 0x00, 0x00),
  RGB (0xff, 0x00, 0x00),
  RGB (0x00, 0xff, 0x00),
  RGB (0xff, 0xff, 0x00),
  RGB (0x00, 0x00, 0xff),
  RGB (0xff, 0x00, 0xff),
  RGB (0x00, 0xff, 0xff),
  RGB (0xff, 0xff, 0xff),
  RGB (0x00, 0x00, 0x00),
  RGB (0x80, 0x00, 0x00),
  RGB (0x00, 0x80, 0x00),
  RGB (0x80, 0x80, 0x00),
  RGB (0x00, 0x00, 0x80),
  RGB (0x80, 0x00, 0x80),
  RGB (0x00, 0x80, 0x80),
  RGB (0x80, 0x80, 0x80),
};

XCOLORREF Window::w_textprop_xforecolor[GLYPH_TEXTPROP_NCOLORS];
XCOLORREF Window::w_textprop_xbackcolor[GLYPH_TEXTPROP_NCOLORS];

const wcolor_index_name wcolor_index_names[] =
{
  {cfgTextColor, RGB (0, 0, 0), "文字色"},
  {cfgBackColor, RGB (0xff, 0xff, 0xff), "背景色"},
  {cfgCtlColor, RGB (0x80, 0x80, 0), "制御文字"},
  {cfgSelectionTextColor, RGB (0xff, 0xff, 0xff), "選択文字色"},
  {cfgSelectionBackColor, RGB (0, 0, 0), "選択背景色"},
  {cfgKwdColor1, RGB (0, 0, 0xff), "キーワード1"},
  {cfgKwdColor2, RGB (0, 0x40, 0), "キーワード2"},
  {cfgKwdColor3, RGB (0x80, 0, 0x80), "キーワード3"},
  {cfgStringColor, RGB (0, 0x40, 0), "文字列"},
  {cfgCommentColor, RGB (0, 0x80, 0), "コメント"},
  {cfgTagColor, RGB (0x40, 0x40, 0), "タグ"},
  {cfgCursorColor, RGB (0x80, 0, 0x80), "行カーソル"},
  {cfgCaretColor, RGB (0, 0, 0), "キャレット"},
  {cfgImeCaretColor, RGB (0x80, 0, 0), "IMEキャレット"},
  {cfgLinenum, RGB (0, 0, 0), "行番号"},
  {cfgReverse, RGB (0, 0, 0), "ニセ反転色"},
  {cfgUnselectedModeLineFg, RGB (0, 0, 0), "モード行文字色"},
  {cfgUnselectedModeLineBg, RGB (0, 0, 0), "モード行背景色"},

  {0, RGB (0, 0, 0), "選択モード行文字色"},
  {0, RGB (0, 0, 0), "選択モード行背景色"},
};

ModelineParam::ModelineParam ()
     : m_hfont (0)
{
}

ModelineParam::~ModelineParam ()
{
  if (m_hfont && m_hfont != HFONT (GetStockObject (SYSTEM_FONT)))
    DeleteObject (m_hfont);
}

void
ModelineParam::init (HFONT hf)
{
  if (!hf)
    m_hfont = HFONT (GetStockObject (SYSTEM_FONT));
  else
    {
      LOGFONT lf;
      GetObject (hf, sizeof lf, &lf);
      m_hfont = CreateFontIndirect (&lf);
    }
  TEXTMETRIC tm;
  HDC hdc = GetDC (0);
  HGDIOBJ of = SelectObject (hdc, m_hfont);
  GetTextMetrics (hdc, &tm);
  m_height = tm.tmExternalLeading + tm.tmHeight;
  m_exlead = tm.tmExternalLeading + 1;

  for (int i = 0; i < 22; i++)
    {
      SIZE size;
      GetTextExtentPoint32 (hdc, "0000000000:0000000000", i, &size);
      m_exts[i] = size.cx;
    }
  SelectObject (hdc, of);
  ReleaseDC (0, hdc);
}

StatusWindow::StatusWindow ()
     : sw_hwnd (0), sw_b (sw_buf)
{
  sw_last.l = 0;
  sw_last.textf = 0;
}

void
StatusWindow::restore ()
{
  SendMessage (sw_hwnd, SB_SETTEXT, SBT_OWNERDRAW | 0, LPARAM (&sw_last));
  UpdateWindow (sw_hwnd);
}

int
StatusWindow::text (const char *s)
{
  SendMessage (sw_hwnd, SB_SETTEXT, 0, LPARAM (s));
  UpdateWindow (sw_hwnd);
  sw_last.textf = 1;
  return sw_last.l;
}

void
StatusWindow::puts (const Char *b, int size)
{
  for (const Char *be = b + size; b < be; b++)
    putc (*b);
}

int
StatusWindow::putc (Char c)
{
  if (c == '\n')
    newline ();
  else
    {
      if (sw_b == sw_buf + TEXT_MAX)
        return 0;
      if (c == '\t')
        for (ucs2_t *const be = min (sw_b + 4, sw_buf + TEXT_MAX);
             sw_b < be; sw_b++)
          *sw_b = ' ';
      else if (c < ' ' || c == CC_DEL)
        {
          *sw_b++ = '^';
          if (sw_b == sw_buf + TEXT_MAX)
            return 0;
          *sw_b++ = c == CC_DEL ? '?' : c + '@';
        }
      else
        *sw_b++ = i2w (c);
    }
  return 1;
}

void
StatusWindow::newline ()
{
  flush ();
  sw_b = sw_buf;
}

void
StatusWindow::flush ()
{
  int l = sw_b - sw_buf;
  if (l && (sw_last.textf || l != sw_last.l
            || memcmp (sw_last.buf, sw_buf, sizeof *sw_buf * l)))
    {
      memcpy (sw_last.buf, sw_buf, sizeof *sw_buf * l);
      sw_last.l = l;
      sw_last.textf = 0;
      SendMessage (sw_hwnd, SB_SETTEXT, SBT_OWNERDRAW | 0, LPARAM (&sw_last));
      UpdateWindow (sw_hwnd);
    }
}

void
StatusWindow::puts (const char *s, int fl)
{
  for (const u_char *p = (const u_char *)s; *p;)
    if (SJISP (*p) && p[1])
      {
        putc ((*p << 8) | p[1]);
        p += 2;
      }
    else
      putc (*p++);

  if (fl)
    newline ();
}

void
StatusWindow::puts (int code, int fl)
{
  puts (get_message_string (code), fl);
}

void
StatusWindow::clear (int no_update)
{
  if (sw_last.l || sw_last.textf)
    {
      sw_last.l = 0;
      sw_last.textf = 0;
      if (!no_update)
        {
          SendMessage (sw_hwnd, SB_SETTEXT, 0, LPARAM (""));
          UpdateWindow (sw_hwnd);
        }
    }
  sw_b = sw_buf;
}

void
StatusWindow::set (HWND hwnd)
{
  sw_hwnd = hwnd;
  restore ();
}

int
StatusWindow::paint (const DRAWITEMSTRUCT *dis)
{
  if (dis->itemData != DWORD (&sw_last))
    return 0;

  TEXTMETRIC tm;
  GetTextMetrics (dis->hDC, &tm);

  COLORREF ofg = SetTextColor (dis->hDC, sysdep.btn_text);
  COLORREF obg = SetBkColor (dis->hDC, sysdep.btn_face);

  int x = dis->rcItem.left + 1;
  int y = (dis->rcItem.top + dis->rcItem.bottom - tm.tmHeight) / 2;

#if 1
  RECT r = dis->rcItem;
  r.right = x;
  for (const ucs2_t *b = sw_last.buf, *const be = b + sw_last.l;
       b < be; b++)
    {
      SIZE sz;
      GetTextExtentPoint32W (dis->hDC, b, 1, &sz);
      r.right += sz.cx;
      ExtTextOutW (dis->hDC, x, y, ETO_CLIPPED | ETO_OPAQUE,
                   &r, b, 1, 0);
      r.left = r.right;
      x += sz.cx;
    }
#else
  ExtTextOutW (dis->hDC, x, y,
               ETO_CLIPPED | ETO_OPAQUE, &dis->rcItem,
               sw_last.buf, sw_last.l, 0);
#endif
  SetTextColor (dis->hDC, ofg);
  SetBkColor (dis->hDC, obg);

  return 1;
}


glyph_rep::glyph_rep (int w, int h)
{
  gr_size.cx = w;
  gr_size.cy = h;
  gr_oglyph = (glyph_data **)((char *)this + sizeof *this);
  gr_nglyph = gr_oglyph + h;
  gr_ref = 0;

  char *p = (char *)(gr_nglyph + h);
  for (int i = 0; i < 2 * h; i++)
    {
      gr_oglyph[i] = (glyph_data *)p;
      gr_oglyph[i]->gd_len = 0;
      gr_oglyph[i]->gd_mod = 0;
      gr_oglyph[i]->gd_cc[0] = 0;
      p += sizeof (glyph_data) + sizeof (glyph_t) * (w + 1);
    }
  assert (p - (char *)this == size (w, h));
}

void
glyph_rep::copy (const glyph_rep *src)
{
  if (src)
    {
      int h = min (gr_size.cy, src->gr_size.cy);
      int y;
      for (y = 0; y < h; y++)
        {
          int w = min (gr_size.cx, LONG (src->gr_oglyph[y]->gd_len));
          memcpy (gr_oglyph[y]->gd_cc, src->gr_oglyph[y]->gd_cc, sizeof (glyph_t) * w);
          glyph_t *g, *ge;
          for (g = gr_oglyph[y]->gd_cc + w,
               ge = gr_oglyph[y]->gd_cc + gr_size.cx;
               g < ge; g++)
            *g = GLYPH_JUNK;
          *g = 0;
          gr_oglyph[y]->gd_len = short (gr_size.cx);
        }

      for (; y < gr_size.cy; y++)
        {
          glyph_t *g, *ge;
          for (g = gr_oglyph[y]->gd_cc, ge = g + gr_size.cx; g < ge; g++)
            *g = GLYPH_JUNK;
          *g = 0;
          gr_oglyph[y]->gd_len = short (gr_size.cx);
        }

      if (gr_size.cy >= src->gr_size.cy && src->gr_size.cy)
        for (glyph_t *g = gr_oglyph[src->gr_size.cy - 1]->gd_cc,
             *ge = g + gr_oglyph[src->gr_size.cy - 1]->gd_len;
             g < ge; g++)
          glyph_make_junk (g);
      if (gr_size.cx >= src->gr_size.cx && src->gr_size.cx)
        for (y = 0; y < h; y++)
          glyph_make_junk (&gr_oglyph[y]->gd_cc[src->gr_size.cx - 1]);
    }
  else
    {
      for (int y = 0; y < gr_size.cy; y++)
        {
          glyph_t *g, *ge;
          for (g = gr_oglyph[y]->gd_cc, ge = g + gr_size.cx; g < ge; g++)
            *g = GLYPH_JUNK;
          *g = 0;
          gr_oglyph[y]->gd_len = short (gr_size.cx);
        }
    }
}

void
Window::init (int minibufp, int temporary)
{
  w_last_bufp = 0;
  w_disp_flags = WDF_WINDOW | WDF_MODELINE;
  w_last_mark_linenum = -1;
  bzero (&w_rect, sizeof w_rect);
  bzero (&w_order, sizeof w_order);
  bzero (w_last_vars, sizeof w_last_vars);
  bzero (&w_clsize, sizeof w_clsize);
  bzero (&w_ech, sizeof w_ech);
  w_colors = default_colors;
  w_inverse_mode_line = 0;
  w_ime_mode_line = 0;


  w_cursor_line.ypixel = -1;

  w_ruler_top_column = -1;
  w_ruler_column = -1;
  w_ruler_fold_column = Buffer::FOLD_NONE;

  w_ignore_scroll_margin = 0;

  if (temporary)
    return;

  lwp = make_window ();

  if (!CreateWindowEx (sysdep.Win4p () ? WS_EX_CLIENTEDGE : 0,
                       Application::ClientClassName, "",
                       (WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE
                        | WS_VSCROLL | WS_HSCROLL),
                       0, 0, 0, 0, app.active_frame.hwnd, 0, app.hinst, this))
    FEstorage_error ();

  if (minibufp)
    w_hwnd_ml = 0;
  else if (!CreateWindow (Application::ModelineClassName, "",
                          WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE,
                          0, 0, 0, 0,
                          app.active_frame.hwnd, 0, app.hinst, this))
    {
      DestroyWindow (w_hwnd);
      FEstorage_error ();
    }

  xwindow_wp (lwp) = this;

  w_vsinfo.nMin = 1;
  w_vsinfo.nMax = -1;
  w_vsinfo.nPage = UINT (-1);
  w_vsinfo.nPos = -1;
  update_vscroll_bar ();

  w_hsinfo.nMin = 0;
  w_hsinfo.nMax = -1;
  w_hsinfo.nPage = UINT (-1);
  w_hsinfo.nPos = -1;
  update_hscroll_bar ();
}

Window::Window (const Window &src)
{
  lwp = Qnil;
  w_next = w_prev = 0;
#define CP(x) (x = src.x)
  CP (w_bufp);
  CP (w_flags_mask);
  CP (w_flags);
  CP (w_last_flags);
  CP (w_point);
  CP (w_mark);
  CP (w_last_point);
  CP (w_disp);
  CP (w_last_disp);
  CP (w_last_top_linenum);
  CP (w_last_top_column);
  CP (w_linenum);
  CP (w_plinenum);
  CP (w_column);
  CP (w_goal_column);
  CP (w_top_column);
  CP (w_selection_type);
  CP (w_selection_point);
  CP (w_selection_marker);
  CP (w_reverse_temp);
  CP (w_reverse_region);
  CP (w_selection_column);
  CP (w_selection_region);
#undef CP
  init (0, 0);
}

#if defined(_MSC_VER) && (_MSC_VER < 1600)
/* なんか知らんがinternal compiler error が出るようになってしまったので
   てきとーに対処。*/
#pragma optimize("g", off)
#endif

Window::Window (int minibufp, int temporary)
{
  lwp = Qnil;
  w_bufp = 0;
  w_next = w_prev = 0;

  w_flags_mask = minibufp ? WF_NEWLINE : -1;
  w_flags = 0;
  w_last_flags = flags ();

  bzero (&w_point, sizeof w_point);
  w_mark = NO_MARK_SET;
  w_last_point = 0;
  w_disp = 0;
  w_last_disp = 0;
  w_last_top_linenum = 1;
  w_last_top_column = 0;
  w_linenum = 1;
  w_plinenum = 1;
  w_column = 0;
  w_goal_column = 0;
  w_top_column = 0;
  w_selection_type = Buffer::SELECTION_VOID;
  w_selection_point = NO_MARK_SET;
  w_selection_marker = NO_MARK_SET;
  w_selection_column = 0;
  w_selection_region.p1 = -1;
  w_reverse_temp = Buffer::SELECTION_VOID;
  w_reverse_region.p1 = NO_MARK_SET;
  w_reverse_region.p2 = NO_MARK_SET;

  init (minibufp, temporary);
}
#if defined(_MSC_VER) && (_MSC_VER < 1600)
#pragma optimize("", on)
#endif

Window::~Window ()
{
  if (windowp (lwp))
    xwindow_wp (lwp) = 0;
  if (IsWindow (w_hwnd))
    DestroyWindow (w_hwnd);
  if (IsWindow (w_hwnd_ml))
    DestroyWindow (w_hwnd_ml);
}

void
Window::save_buffer_params ()
{
  if (!w_bufp)
    return;
  w_bufp->b_point = w_point.p_point;
  w_bufp->b_mark = w_mark;
  w_bufp->b_selection_point = w_selection_point;
  w_bufp->b_selection_marker = w_selection_marker;
  w_bufp->b_selection_type = w_selection_type;
  w_bufp->b_selection_column = w_selection_column;
  w_bufp->b_reverse_temp = w_reverse_temp;
  w_bufp->b_reverse_region = w_reverse_region;
  w_bufp->b_disp = w_disp;
}

void
Window::change_color ()
{
  COLORREF cbuf[USER_DEFINABLE_COLORS];
  COLORREF *cc;
  if (w_bufp && w_bufp->b_colors_enable)
    {
      for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
        cbuf[i] = w_bufp->b_colors[i];
      cc = cbuf;
    }
  else
    cc = default_colors;

  int i;
  for (i = 0; i < USER_DEFINABLE_COLORS; i++)
    if (cc[i] != w_colors[i])
      break;
  if (i == USER_DEFINABLE_COLORS)
    return;
  if (cc == cbuf)
    {
      memcpy (w_colors_buf, default_colors, sizeof w_colors_buf);
      memcpy (w_colors_buf, cbuf, sizeof cbuf);
      w_colors = w_colors_buf;
    }
  else
    w_colors = default_colors;
  invalidate_glyphs ();
}

void
Window::set_buffer_params (Buffer *bp)
{
  w_bufp = bp;
  w_point.p_point = 0;
  w_point.p_chunk = bp->b_chunkb;
  w_point.p_offset = 0;
  bp->goto_char (w_point, bp->b_point);
  w_mark = bp->b_mark;
  w_selection_point = bp->b_selection_point;
  w_selection_marker = bp->b_selection_marker;
  w_selection_type = bp->b_selection_type;
  w_selection_column = bp->b_selection_column;
  w_reverse_temp = bp->b_reverse_temp;
  w_reverse_region = bp->b_reverse_region;
  w_disp = bp->b_disp;
  w_last_disp = w_disp;
  w_goal_column = 0;
  w_disp_flags |= WDF_WINDOW | WDF_MODELINE | WDF_GOAL_COLUMN;
  change_color ();
}

void
Window::set_buffer (Buffer *bp)
{
  if (bp != w_bufp)
    {
      Buffer *obp = w_bufp;
      save_buffer_params ();
      set_buffer_params (bp);
      bp->window_size_changed ();
      if (obp)
        obp->window_size_changed ();
      Buffer::maybe_modify_buffer_bar ();
      w_ignore_scroll_margin = 1;
    }
}

void
Window::set_window ()
{
  assert (this);
  assert (xwindow_wp (lwp) == this);
  app.active_frame.selected = this;
  w_bufp->check_range (w_point);
}

void
Window::init_colors (const XCOLORREF *colors, const XCOLORREF *mlcolors,
                     const XCOLORREF *fg_colors, const XCOLORREF *bg_colors)
{
  int i;
  if (colors)
    for (i = 0; i < USER_DEFINABLE_COLORS; i++)
      default_xcolors[i] = colors[i];
  if (mlcolors)
    for (i = 0; i < numberof (modeline_xcolors); i++)
      modeline_xcolors[i] = mlcolors[i];
  if (fg_colors)
    for (i = 1; i < numberof (w_textprop_xforecolor); i++)
      w_textprop_xforecolor[i] = fg_colors[i];
  if (bg_colors)
    for (i = 1; i < numberof (w_textprop_xbackcolor); i++)
      w_textprop_xbackcolor[i] = bg_colors[i];

  for (i = 0; i < USER_DEFINABLE_COLORS; i++)
    default_colors[i] = default_xcolors[i];
  for (i = 0; i < numberof (modeline_xcolors); i++)
    modeline_colors[i] = modeline_xcolors[i];
  for (i = 1; i < numberof (w_textprop_xforecolor); i++)
    w_textprop_forecolor[i] = w_textprop_xforecolor[i];
  for (i = 1; i < numberof (w_textprop_xbackcolor); i++)
    w_textprop_backcolor[i] = w_textprop_xbackcolor[i];

  default_colors[WCOLOR_GRAY] = sysdep.gray_text;
  default_colors[WCOLOR_BTNSHADOW] = sysdep.btn_shadow;
  default_colors[WCOLOR_BTNTEXT] = sysdep.btn_text;

  HDC hdc = GetDC (0);
  for (i = 0; i < WCOLOR_MAX; i++)
    default_colors[i] = GetNearestColor (hdc, default_colors[i]);
  for (i = 0; i < numberof (modeline_xcolors); i++)
    modeline_colors[i] = GetNearestColor (hdc, modeline_colors[i]);
  for (i = 1; i < numberof (w_textprop_forecolor); i++)
    w_textprop_forecolor[i] = GetNearestColor (hdc, w_textprop_forecolor[i]);
  for (i = 1; i < numberof (w_textprop_backcolor); i++)
    w_textprop_backcolor[i] = GetNearestColor (hdc, w_textprop_backcolor[i]);
  ReleaseDC (0, hdc);

  for (i = 0; i < USER_DEFINABLE_COLORS; i++)
    write_conf (cfgColors, wcolor_index_names[i].name, default_xcolors[i].rgb, 1);
  write_conf (cfgColors, cfgModeLineFg, modeline_xcolors[MLCI_FOREGROUND].rgb, 1);
  write_conf (cfgColors, cfgModeLineBg, modeline_xcolors[MLCI_BACKGROUND].rgb, 1);
  for (i = 1; i < numberof (w_textprop_forecolor); i++)
    {
      char b[32];
      sprintf (b, "%s%d", cfgFg, i);
      write_conf (cfgColors, b, w_textprop_xforecolor[i].rgb, 1);
      sprintf (b, "%s%d", cfgBg, i);
      write_conf (cfgColors, b, w_textprop_xbackcolor[i].rgb, 1);
    }
  flush_conf ();
}

inline void
Window::invalidate_glyphs ()
{
  if (w_glyphs.g_rep)
    w_glyphs.g_rep->copy (0);
  w_disp_flags |= WDF_WINDOW | WDF_WINSIZE_CHANGED;
  w_cursor_line.ypixel = -1;
}

void
Window::change_parameters (const FontSetParam &param)
{
  change_parameters (param, 0, 0, 0, 0, false);
}

void
Window::change_parameters (const FontSetParam &param,
                           const XCOLORREF *colors, const XCOLORREF *mlcolors,
                           const XCOLORREF *fg, const XCOLORREF *bg,
                           bool change_color_p)
{
  int ocell = app.text_font.cell ().cy;

  app.text_font.create (param);
  if (change_color_p)
    init_colors (colors, mlcolors, fg, bg);

  compute_geometry (app.active_frame.size, ocell);

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    wp->invalidate_glyphs ();
}

static void
set_bgmode ()
{
  wcolor_index c = (Window::w_default_flags & Window::WF_BGCOLOR_MODE
                    ? WCOLOR_TEXT : WCOLOR_REVERSE);
  for (int i = GLYPH_REVERSED >> GLYPH_COLOR_SHIFT_BITS;
       i < (GLYPH_REVERSED >> GLYPH_COLOR_SHIFT_BITS) + 16;
       i++)
    if (i != ((GLYPH_REVERSED | GLYPH_LINENUM) >> GLYPH_COLOR_SHIFT_BITS))
      Window::backcolor_indexes[i] = c;
}

void
Window::create_default_windows ()
{
  app.text_font.init ();

  XCOLORREF cc[USER_DEFINABLE_COLORS];
  for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
    cc[i] = wcolor_index_names[i].rgb;
  cc[WCOLOR_TEXT] = XCOLORREF (sysdep.window_text, COLOR_WINDOWTEXT);
  cc[WCOLOR_BACK] = XCOLORREF (sysdep.window, COLOR_WINDOW);
  cc[WCOLOR_HIGHLIGHT_TEXT] = XCOLORREF (sysdep.highlight_text,
                                         COLOR_HIGHLIGHTTEXT);
  cc[WCOLOR_HIGHLIGHT] = XCOLORREF (sysdep.highlight, COLOR_HIGHLIGHT);
  cc[WCOLOR_REVERSE] = XCOLORREF (GetSysColor (COLOR_BACKGROUND),
                                  COLOR_BACKGROUND);
  cc[WCOLOR_LINENUM] = cc[WCOLOR_TEXT];
  cc[WCOLOR_MODELINE_FG] = XCOLORREF (sysdep.btn_text, COLOR_BTNTEXT);
  cc[WCOLOR_MODELINE_BG] = XCOLORREF (sysdep.btn_face, COLOR_BTNFACE);

  XCOLORREF mlcc[2];
  mlcc[0] = XCOLORREF (sysdep.btn_highlight, COLOR_BTNHIGHLIGHT);
  mlcc[1] = XCOLORREF (sysdep.btn_text, COLOR_BTNTEXT);

  XCOLORREF fg[GLYPH_TEXTPROP_NCOLORS], bg[GLYPH_TEXTPROP_NCOLORS];
  for (int i = 0; i < GLYPH_TEXTPROP_NCOLORS; i++)
    {
      fg[i] = w_textprop_forecolor[i];
      bg[i] = w_textprop_backcolor[i];
    }

  int c;
  for (int i = 0; i < USER_DEFINABLE_COLORS; i++)
    {
      if (read_conf (cfgColors, wcolor_index_names[i].name, c))
        cc[i] = c;
      else if (i == WCOLOR_LINENUM)
        cc[i] = cc[WCOLOR_TEXT];
    }
  if (read_conf (cfgColors, cfgModeLineFg, c))
    mlcc[0] = c;
  if (read_conf (cfgColors, cfgModeLineBg, c))
    mlcc[1] = c;
  for (int i = 1; i < GLYPH_TEXTPROP_NCOLORS; i++)
    {
      char b[32];
      sprintf (b, "%s%d", cfgFg, i);
      if (read_conf (cfgColors, b, c))
        fg[i] = c;
      sprintf (b, "%s%d", cfgBg, i);
      if (read_conf (cfgColors, b, c))
        bg[i] = c;
    }

  init_colors (cc, mlcc, fg, bg);
  set_bgmode ();

  Window *wp = new Window ();
  Window *mwp = new Window (1);

  wp->w_order.left = 0;
  wp->w_order.top = 0;
  wp->w_order.right = 1;
  wp->w_order.bottom = 1;

  mwp->w_rect.top = 0;
  mwp->w_rect.bottom = app.text_font.size ().cy + sysdep.edge.cy;

  wp->w_prev = 0;
  wp->w_next = mwp;
  mwp->w_prev = wp;
  mwp->w_next = 0;

  app.active_frame.windows = wp;
  app.active_frame.selected = wp;

  SIZE osize = {0, 0};
  if (!IsIconic (app.toplev))
    {
      RECT r;
      GetClientRect (app.active_frame.hwnd, &r);
      app.active_frame.size.cx = r.right;
      app.active_frame.size.cy = r.bottom;
    }
  else
    {
      app.active_frame.size.cx = 10;
      app.active_frame.size.cy = 10;
    }
  Window::compute_geometry (osize);
  Window::move_all_windows (0);
}

int
Window::alloc_glyph_rep ()
{
  void *tem = malloc (glyph_rep::size (w_ch_max.cx, w_ch_max.cy));
  if (!tem)
    return 0;
  glyph_rep *rep = new (tem) glyph_rep (w_ch_max.cx, w_ch_max.cy);
  rep->copy (w_glyphs.g_rep);
  w_glyphs = Glyphs (rep);
  return 1;
}

void
Window::calc_client_size (int width, int height)
{
  w_client.cx = max (0, width);
  w_client.cy = max (0, height);
  w_ech.cx = max (0L, ((w_client.cx - app.text_font.cell ().cx / 2)
                       / app.text_font.cell ().cx));
  w_ech.cy = w_client.cy / app.text_font.cell ().cy;
  w_ch_max.cx = (w_client.cx + app.text_font.cell ().cx
                 + app.text_font.cell ().cx / 2 - 1) / app.text_font.cell ().cx;
  w_ch_max.cy = (w_client.cy + app.text_font.cell ().cy - 1) / app.text_font.cell ().cy;
  if (!w_ech.cx && w_ch_max.cx)
    w_ech.cx = 1;
  if (!w_ech.cy && w_ch_max.cy)
    w_ech.cy = 1;
  if (!w_glyphs.g_rep
      || w_glyphs.g_rep->gr_size.cx != w_ch_max.cx
      || w_glyphs.g_rep->gr_size.cy != w_ch_max.cy)
    {
      if (!alloc_glyph_rep ())
        w_glyphs = Glyphs (0);
      w_disp_flags |= WDF_WINDOW | WDF_MODELINE | WDF_WINSIZE_CHANGED;
    }
}

static void
compute_size (int *o, int n, int old_size, int new_size)
{
  if (old_size < 0)
    old_size = 0;
  if (new_size < 0)
    new_size = 0;
  int *const w = (int *)alloca (sizeof *w * n);
  int i;
  for (i = 0; i < n; i++)
    w[i] = o[i + 1] - o[i];
  int diff_size = new_size - old_size;
  if (!old_size)
    {
      int d = diff_size / n;
      for (i = 0; i < n; i++)
        w[i] += d;
    }
  else
    for (i = 0; i < n; i++)
      w[i] += w[i] * diff_size / old_size;

  int sum = 0;
  for (i = 0; i < n; i++)
    {
      if (w[i] < 0)
        w[i] = 0;
      sum += w[i];
    }

  int d = new_size - sum;

  if (d > 0)
    for (; d > 0; d--)
      w[(new_size + d) % n]++;
  else
    for (d = -d; d > 0; d--)
      w[(new_size + d) % n]--;

  for (o[0] = 0, i = 1; i <= n; i++)
    {
      o[i] = o[i - 1] + w[i - 1];
      if (o[i] < o[i - 1])
        o[i] = o[i - 1];
    }

  for (o[n] = new_size, i = n - 1; i > 0; i--)
    if (o[i] > o[i + 1])
      o[i] = o[i + 1];
}

void
Window::compute_geometry (const SIZE &old_size, int lcell)
{
  if (!app.active_frame.windows)
    return;

  const SIZE &new_size = app.active_frame.size;

  // compute minibuffer window geometry
  Window *wp;
  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    ;
  wp->w_rect.left = 0;
  wp->w_rect.right = new_size.cx;
  int old_h = wp->w_rect.bottom - wp->w_rect.top;
  int old_l = static_cast<int> (old_h / lcell);
  int new_h = old_l * app.text_font.cell ().cy + 4;
  int min_h = app.text_font.cell ().cy + 4;
  int max_h = new_size.cy - (sysdep.edge.cy + FRAME_WIDTH + min_h + app.modeline_param.m_height + 4);
  new_h = max (new_h, min_h);
  if (max_h < new_h)
    new_h = old_h;

  wp->w_rect.bottom = new_size.cy;
  wp->w_rect.top = new_size.cy - new_h;
  wp->calc_client_size (wp->w_rect.right - sysdep.edge.cx,
                        wp->w_rect.bottom - wp->w_rect.top - sysdep.edge.cy);

  long nx = 0, ny = 0;
  long ow = 0, oh = 0;
  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    {
      nx = max (nx, wp->w_order.right);
      ny = max (ny, wp->w_order.bottom);
      ow = max (ow, wp->w_rect.right);
      oh = max (oh, wp->w_rect.bottom);
    }

  // compute normal windows geometry
  int *const ox = (int *)alloca (sizeof *ox * (nx + 1));
  int *const oy = (int *)alloca (sizeof *oy * (ny + 1));
  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    {
      ox[wp->w_order.left] = wp->w_rect.left;
      oy[wp->w_order.top] = wp->w_rect.top;
      ox[wp->w_order.right] = wp->w_rect.right;
      oy[wp->w_order.bottom] = wp->w_rect.bottom;
    }

  compute_size (ox, nx, ow, new_size.cx);
  compute_size (oy, ny, oh, new_size.cy - new_h);

  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    {
      wp->w_rect.left = ox[wp->w_order.left];
      wp->w_rect.top = oy[wp->w_order.top];
      wp->w_rect.right = ox[wp->w_order.right];
      wp->w_rect.bottom = oy[wp->w_order.bottom];

      int cx = wp->w_rect.right - wp->w_rect.left - sysdep.edge.cx;
      int cy = wp->w_rect.bottom - wp->w_rect.top - sysdep.edge.cy;
      if (wp->flags () & WF_VSCROLL_BAR)
        cx -= sysdep.vscroll;
      if (wp->flags () & WF_HSCROLL_BAR)
        cy -= sysdep.hscroll;
      if (wp->w_rect.right != app.active_frame.size.cx)
        cx -= FRAME_WIDTH;
      cx -= RIGHT_PADDING;
      if (wp->w_hwnd_ml)
        cy -= app.modeline_param.m_height + 4 + FRAME_WIDTH;
      if (!wp->minibuffer_window_p () && wp->flags () & WF_RULER)
        cy -= RULER_HEIGHT;

      wp->calc_client_size (cx, cy);
    }

  app.active_frame.windows_moved = 1;
}

void
Window::move_all_windows (int update)
{
  int mod = 0;
  app.active_frame.windows_moved = 0;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    {
      int cx, cy;
      int mlh;
      if (wp->w_hwnd_ml)
        {
          mlh = wp->flags () & WF_MODE_LINE ? app.modeline_param.m_height + 4 : 0;
          cx = wp->w_rect.right == app.active_frame.size.cx ? 0 : FRAME_WIDTH;
          cy = FRAME_WIDTH;
        }
      else
        {
          mlh = 0;
          cx = cy = 0;
        }

      RECT or, nr;
      if (!mod)
        GetWindowRect (wp->w_hwnd, &or);

      SIZE size;
      size.cx = max (0, int (wp->w_rect.right - wp->w_rect.left - cx));
      int ruler = (!wp->minibuffer_window_p () && wp->flags () & WF_RULER
                   ? RULER_HEIGHT : 0);
      size.cy = max (0, int (wp->w_rect.bottom - wp->w_rect.top - mlh - cy - ruler));
      MoveWindow (wp->w_hwnd, wp->w_rect.left, wp->w_rect.top + ruler,
                  size.cx, size.cy, 1);

      if (!mod)
        {
          GetWindowRect (wp->w_hwnd, &nr);
          mod = memcmp (&or, &nr, sizeof or);
        }

      RECT r;
      GetClientRect (wp->w_hwnd, &r);
      wp->calc_client_size (r.right - RIGHT_PADDING, r.bottom);
      if (wp->w_hwnd_ml)
        {
          if (!mod)
            GetWindowRect (wp->w_hwnd_ml, &or);
          wp->w_ml_size.cx = size.cx;
          wp->w_ml_size.cy = mlh;
          InvalidateRect (wp->w_hwnd_ml, 0, 1);
          MoveWindow (wp->w_hwnd_ml,
                      wp->w_rect.left, wp->w_rect.bottom - mlh - cy, size.cx, mlh, 1);
          if (!mod)
            {
              GetWindowRect (wp->w_hwnd_ml, &nr);
              mod = memcmp (&or, &nr, sizeof or);
            }
        }
    }

  for (Window *wp = app.active_frame.reserved; wp; wp = wp->w_next)
    {
      MoveWindow (wp->w_hwnd, 0, 0, 0, 0, 1);
      if (wp->w_hwnd_ml)
        MoveWindow (wp->w_hwnd_ml, 0, 0, 0, 0, 1);
    }

  if (mod)
    {
      for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
        if (wp->w_bufp)
          wp->w_bufp->window_size_changed ();

      InvalidateRect (app.active_frame.hwnd, 0, 1);
      InvalidateRect (app.toplev, 0, 1);
      if (update)
        {
          UpdateWindow (app.active_frame.hwnd);
          UpdateWindow (app.toplev);
        }
    }
}

void
Window::repaint_all_windows ()
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (!GetUpdateRect (wp->w_hwnd, 0, 0))
      wp->update_window ();
}

void
Window::destroy_windows ()
{
  Window *wp, *next;
  for (wp = app.active_frame.deleted; wp; wp = next)
    {
      next = wp->w_next;
      delete wp;
    }
  app.active_frame.deleted = 0;
}

void
Window::update_vscroll_bar ()
{
  w_vsinfo.fMask = 0;
  if (flags () & WF_VSCROLL_BAR)
    {
      if (w_vsinfo.sb_seen != ScrollInfo::yes)
        {
          w_vsinfo.sb_seen = ScrollInfo::yes;
          ShowScrollBar (w_hwnd, SB_VERT, 1);
        }
      int nlines = (w_bufp
                    ? (w_bufp->b_fold_columns == Buffer::FOLD_NONE
                       ? w_bufp->count_lines ()
                       : w_bufp->folded_count_lines ())
                    : 1);
      if (!(flags () & WF_ALT_VSCROLL_BAR))
        nlines += w_ech.cy - 1;
      else
        nlines = max (nlines, int (w_last_top_linenum + w_ech.cy - 1));
      if (w_vsinfo.nMax != nlines || w_vsinfo.nPage != UINT (w_ech.cy))
        {
          w_vsinfo.nMax = nlines;
          w_vsinfo.nPage = w_ech.cy;
          w_vsinfo.fMask |= SIF_RANGE | SIF_PAGE;
        }
      if (w_vsinfo.nPos != w_last_top_linenum)
        {
          w_vsinfo.nPos = w_last_top_linenum;
          w_vsinfo.fMask |= SIF_POS;
        }
      if (w_vsinfo.fMask)
        w_vsinfo.fMask |= SIF_DISABLENOSCROLL;
    }
  else
    {
      if (w_vsinfo.sb_seen != ScrollInfo::no)
        {
          ShowScrollBar (w_hwnd, SB_VERT, 0);
          w_vsinfo.sb_seen = ScrollInfo::no;
          w_vsinfo.nMax = w_vsinfo.nMin;
          w_vsinfo.nPage = UINT (-1);
          w_vsinfo.nPos = -1;
          w_vsinfo.fMask = SIF_RANGE;
        }
    }
  if (w_vsinfo.fMask)
    SetScrollInfo (w_hwnd, SB_VERT, &w_vsinfo, 1);
}

int
Window::vscroll_lines () const
{
  int h = w_ech.cy;
  if (xsymbol_value (Vpage_scroll_half_window) != Qunbound
      && xsymbol_value (Vpage_scroll_half_window) != Qnil)
    h /= 2;
  else
    h -= symbol_value_as_integer (Vnext_screen_context_lines, w_bufp);
  return max (h, 1);
}

void
Window::process_vscroll (int code)
{
  if (!w_bufp)
    return;

  switch (code)
    {
    case SB_LINEDOWN:
      if (!scroll_window (max (symbol_value_as_integer (Vscroll_bar_step, w_bufp), 1)))
        return;
      break;

    case SB_LINEUP:
      if (!scroll_window (-max (symbol_value_as_integer (Vscroll_bar_step, w_bufp), 1)))
        return;
      break;

    case SB_PAGEDOWN:
      if (!scroll_window (vscroll_lines ()))
        return;
      break;

    case SB_PAGEUP:
      if (!scroll_window (-vscroll_lines ()))
        return;
      break;

    case SB_THUMBTRACK:
      {
        ScrollInfo i;
        i.fMask = SIF_TRACKPOS;
        GetScrollInfo (w_hwnd, SB_VERT, &i);
        if (!scroll_window (i.nTrackPos, 1))
          return;
        break;
      }

    default:
      return;
    }

  refresh_screen (1);
}

void
Window::wheel_scroll (const wheel_info &wi)
{
  if (!w_bufp || !wi.wi_value)
    return;

  lisp hook = symbol_value (Vmouse_wheel_handler, w_bufp);
  if (hook != Qunbound && hook != Qnil)
    {
      try
        {
          funcall_3 (hook, lwp, make_fixnum (-wi.wi_value),
                     (wi.wi_nlines == WHEEL_PAGESCROLL
                      ? Qnil : make_fixnum (wi.wi_nlines)));
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
        }
      refresh_screen (1);
    }
}

void
Window::update_hscroll_bar ()
{
  w_hsinfo.fMask = 0;
  if (flags () & WF_HSCROLL_BAR)
    {
      if (w_hsinfo.sb_seen != ScrollInfo::yes)
        {
          w_hsinfo.sb_seen = ScrollInfo::yes;
          ShowScrollBar (w_hwnd, SB_HORZ, 1);
        }
      int pos;
      int w;
      if (!w_bufp || w_bufp->b_fold_columns == Buffer::FOLD_NONE)
        {
#define PAGES_PER_WIDTH 20
          w = w_ech.cx * PAGES_PER_WIDTH;
          pos = min (w_last_top_column, long (w_ech.cx * (PAGES_PER_WIDTH - 2)));
#undef PAGES_PER_WIDTH
        }
      else
        {
          w = w_bufp->b_fold_columns;
          if (flags () & WF_LINE_NUMBER)
            w += LINENUM_COLUMNS;
          pos = min (w_last_top_column, long (w - w_ech.cx));
        }
      if (w_hsinfo.nMax != w || w_hsinfo.nPage != UINT (w_ech.cx))
        {
          w_hsinfo.nMax = w;
          w_hsinfo.nPage = w_ech.cx;
          w_hsinfo.fMask |= SIF_RANGE | SIF_PAGE;
        }
      if (w_hsinfo.nPos != pos)
        {
          w_hsinfo.nPos = pos;
          w_hsinfo.fMask |= SIF_POS;
        }
      if (w_hsinfo.fMask)
        w_hsinfo.fMask |= SIF_DISABLENOSCROLL;
    }
  else
    {
      if (w_hsinfo.sb_seen != ScrollInfo::no)
        {
          w_hsinfo.sb_seen = ScrollInfo::no;
          ShowScrollBar (w_hwnd, SB_HORZ, 0);
          w_hsinfo.nMax = w_hsinfo.nMin;
          w_hsinfo.nPage = UINT (-1);
          w_hsinfo.nPos = -1;
          w_hsinfo.fMask = SIF_RANGE;
        }
    }
  if (w_hsinfo.fMask)
    SetScrollInfo (w_hwnd, SB_HORZ, &w_hsinfo, 1);
}

void
Window::process_hscroll (int code)
{
  if (!w_bufp)
    return;

  switch (code)
    {
    case SB_LINELEFT:
      if (!scroll_window_horizontally (-2))
        return;
      break;

    case SB_LINERIGHT:
      if (!scroll_window_horizontally (2))
        return;
      break;

    case SB_PAGELEFT:
      if (!scroll_window_horizontally (-w_ech.cx))
        return;
      break;

    case SB_PAGERIGHT:
      if (!scroll_window_horizontally (w_ech.cx))
        return;
      break;

    case SB_THUMBTRACK:
      {
        ScrollInfo i;
        i.fMask = SIF_TRACKPOS;
        GetScrollInfo (w_hwnd, SB_HORZ, &i);
        if (!scroll_window_horizontally (i.nTrackPos - w_hsinfo.nPos))
          return;
        break;
      }

    default:
      return;
    }

  refresh_screen (1);
}

Window *
Window::coerce_to_window (lisp object)
{
  if (!object || object == Qnil)
    return selected_window ();
  check_window (object);
  if (!xwindow_wp (object))
    FEprogram_error (Edeleted_window);
  return xwindow_wp (object);
}

Window *
Window::minibuffer_window ()
{
  Window *wp;
  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    ;
  return wp;
}

int
Window::count_windows ()
{
  int n = 0;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next, n++)
    ;
  return n;
}

void
Window::modify_all_mode_line ()
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    wp->w_disp_flags |= WDF_MODELINE;
}

void
Window::split (int nlines, int verticalp)
{
  if (minibuffer_window_p ())
    FEsimple_error (Ecannot_split_minibuffer_window);

  int h0, h1;
  int pxl;
  int current;

  if (!verticalp)
    {
      if (!nlines)
        {
          h0 = w_ech.cy / 2;
          h1 = w_ech.cy - h0 - 1;
          current = w_linenum - w_last_top_linenum < h0 ? 0 : 1;
          pxl = (w_rect.bottom - w_rect.top) / 2;
        }
      else
        {
          int ml = app.modeline_param.m_height + 4;
          if (nlines > 0)
            {
              h0 = nlines;
              h1 = w_ech.cy - h0 - 1;
              current = 0;
            }
          else
            {
              h1 = -nlines;
              h0 = w_ech.cy - h1 - 1;
              current = 1;
            }
          pxl = (h0 * app.text_font.cell ().cy + ml
                 + (w_rect.bottom - w_rect.top - ml
                    - (h0 + h1) * app.text_font.cell ().cy) / 2);
        }

      if (h0 < 1 || h1 < 1)
        FEsimple_error (Ecannot_split);
    }
  else
    {
      if (!nlines)
        {
          h0 = w_ech.cx / 2;
          h1 = w_ech.cx - h0 - 1;
          current = w_column - w_last_top_column < h0 ? 0 : 1;
          pxl = (w_rect.right - w_rect.left) / 2;
        }
      else
        {
          if (nlines > 0)
            {
              h0 = nlines;
              h1 = w_ech.cx - h0 - 1;
              current = 0;
            }
          else
            {
              h1 = -nlines;
              h0 = w_ech.cx - h1 - 1;
              current = 1;
            }
          pxl = (h0 * app.text_font.cell ().cx
                 + (w_rect.right - w_rect.left
                    - (h0 + h1) * app.text_font.cell ().cx) / 2);
        }
#define WINDOW_WIDTH_MIN 10
      if (h0 < WINDOW_WIDTH_MIN || h1 < WINDOW_WIDTH_MIN)
        FEsimple_error (Ecannot_split);
    }

  Window *wp = new Window (*this);
  if (w_next)
    w_next->w_prev = wp;
  wp->w_next = w_next;
  wp->w_prev = this;
  w_next = wp;

  if (!verticalp)
    {
      wp->w_rect = w_rect;
      wp->w_order = w_order;

      w_rect.bottom = w_rect.top + pxl;
      wp->w_rect.top = w_rect.bottom;

      Window *w;
      for (w = app.active_frame.windows; w->w_next; w = w->w_next)
        if (w != wp && w->w_rect.top == wp->w_rect.top)
          {
            w_order.bottom = w->w_order.top;
            wp->w_order.top = w->w_order.top;
            break;
          }

      if (!w->w_next)
        {
          int y, o;
          for (w = app.active_frame.windows, y = o = 0; w->w_next; w = w->w_next)
            if (w->w_rect.top < wp->w_rect.top && w->w_rect.top > y)
              {
                y = w->w_rect.top;
                o = w->w_order.top;
              }
          w_order.bottom = o + 1;
          wp->w_order.top = o + 1;
          for (w = app.active_frame.windows; w->w_next; w = w->w_next)
            {
              if (w != wp && w->w_order.top > o)
                w->w_order.top++;
              if (w != this && w->w_order.bottom > o)
                w->w_order.bottom++;
            }
        }
    }
  else
    {
      wp->w_rect = w_rect;
      wp->w_order = w_order;

      w_rect.right = w_rect.left + pxl;
      wp->w_rect.left = w_rect.right;

      Window *w;
      for (w = app.active_frame.windows; w->w_next; w = w->w_next)
        if (w != wp && w->w_rect.left == wp->w_rect.left)
          {
            w_order.right = w->w_order.left;
            wp->w_order.left = w->w_order.left;
            break;
          }

      if (!w->w_next)
        {
          int x, o;
          for (w = app.active_frame.windows, x = o = 0; w->w_next; w = w->w_next)
            if (w->w_rect.left < wp->w_rect.left && w->w_rect.left > x)
              {
                x = w->w_rect.left;
                o = w->w_order.left;
              }
          w_order.right = o + 1;
          wp->w_order.left = o + 1;
          for (w = app.active_frame.windows; w->w_next; w = w->w_next)
            {
              if (w != wp && w->w_order.left > o)
                w->w_order.left++;
              if (w != this && w->w_order.right > o)
                w->w_order.right++;
            }
        }
    }

  if (current)
    wp->set_window ();

  compute_geometry ();
  wp->change_color ();
  Buffer::maybe_modify_buffer_bar ();
}

lisp
Fsplit_window (lisp arg, lisp verticalp)
{
  selected_window ()->split (!arg || arg == Qnil || arg == Qt ? 0 : fixnum_value (arg),
                             verticalp && verticalp != Qnil);
  return Qt;
}

void
Window::close ()
{
  xwindow_wp (lwp) = 0;

  for (WindowConfiguration *wc = WindowConfiguration::wc_chain; wc; wc = wc->wc_prev)
    for (WindowConfiguration::Data *d = wc->wc_data, *de = d + wc->wc_nwindows; d < de; d++)
      if (d->wp == this)
        {
          w_next = app.active_frame.reserved;
          app.active_frame.reserved = this;
          return;
        }

  w_next = app.active_frame.deleted;
  app.active_frame.deleted = this;
}

void
Window::delete_other_windows ()
{
  if (minibuffer_window_p ())
    return;

  Window *mini = minibuffer_window ();

  int f = 0;
  for (Window *wp = app.active_frame.windows, *next; wp; wp = next)
    {
      next = wp->w_next;
      if (wp != this && wp != mini)
        {
          wp->save_buffer_params ();
          wp->close ();
          f = 1;
        }
    }
  if (!f)
    return;

  app.active_frame.windows = this;
  set_window ();
  w_prev = 0;
  w_next = mini;
  mini->w_prev = this;
  mini->w_next = 0;

  w_order.left = 0;
  w_order.right = 1;
  w_order.top = 0;
  w_order.bottom = 1;

  compute_geometry ();
  Buffer::maybe_modify_buffer_bar ();
}

lisp
Fdelete_other_windows ()
{
  selected_window ()->delete_other_windows ();
  return Qt;
}

int
Window::find_resizeable_edge (LONG RECT::*edge1, LONG RECT::*edge2,
                              LONG RECT::*match1, LONG RECT::*match2) const
{
  int n = 0;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (!wp->minibuffer_window_p () && wp->w_order.*edge1 == w_order.*edge2)
      {
        if (wp->w_order.*match1 == w_order.*match1)
          n++;
        if (wp->w_order.*match2 == w_order.*match2)
          n++;
      }
  return n == 2;
}

int
Window::find_resizeable_edges () const
{
  int f = 0;
  if (find_resizeable_edge (&RECT::right, &RECT::left, &RECT::top, &RECT::bottom))
    f |= RE_LEFT;
  if (find_resizeable_edge (&RECT::left, &RECT::right, &RECT::top, &RECT::bottom))
    f |= RE_RIGHT;
  if (find_resizeable_edge (&RECT::bottom, &RECT::top, &RECT::left, &RECT::right))
    f |= RE_TOP;
  if (find_resizeable_edge (&RECT::top, &RECT::bottom, &RECT::left, &RECT::right))
    f |= RE_BOTTOM;
  return f;
}

void
Window::resize_edge (LONG RECT::*edge1, LONG RECT::*edge2,
                     LONG RECT::*match1, LONG RECT::*match2) const
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (!wp->minibuffer_window_p ()
        && wp->w_order.*edge1 == w_order.*edge2
        && wp->w_order.*match1 >= w_order.*match1
        && wp->w_order.*match2 <= w_order.*match2)
      {
        wp->w_order.*edge1 = w_order.*edge1;
        wp->w_rect.*edge1 = w_rect.*edge1;
      }

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (!wp->minibuffer_window_p () && wp->w_order.*edge2 == w_order.*edge2)
      return;

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (!wp->minibuffer_window_p ())
      {
        if (wp->w_order.*edge2 > w_order.*edge2)
          (wp->w_order.*edge2)--;
        if (wp->w_order.*edge1 > w_order.*edge2)
          (wp->w_order.*edge1)--;
      }
}

void
Window::resize_edge (int f) const
{
  if (f & RE_LEFT)
    resize_edge (&RECT::right, &RECT::left, &RECT::top, &RECT::bottom);
  else if (f & RE_RIGHT)
    resize_edge (&RECT::left, &RECT::right, &RECT::top, &RECT::bottom);
  else if (f & RE_TOP)
    resize_edge (&RECT::bottom, &RECT::top, &RECT::left, &RECT::right);
  else if (f & RE_BOTTOM)
    resize_edge (&RECT::top, &RECT::bottom, &RECT::left, &RECT::right);
}

Window *
Window::find_point_window (POINT &p)
{
  Window *wp;
  for (wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (PtInRect (&wp->w_rect, p))
      break;
  return wp;
}

Window *
Window::find_scr_point_window (const POINT &pt, int ml, int *in_ml)
{
  Window *wp;
  for (wp = app.active_frame.windows; wp; wp = wp->w_next)
    {
      RECT r;
      GetWindowRect (wp->w_hwnd, &r);
      if (PtInRect (&r, pt))
        {
          if (in_ml)
            *in_ml = 0;
          break;
        }
      if (ml && wp->w_hwnd_ml)
        {
          GetWindowRect (wp->w_hwnd_ml, &r);
          if (PtInRect (&r, pt))
            {
              if (in_ml)
                *in_ml = 1;
              break;
            }
        }
    }
  return wp;
}

int
Window::delete_window ()
{
  if (minibuffer_window_p ())
    return 0;

  Window *can;
  int f;

  if (!w_prev)
    {
      can = w_next;
      if (can->minibuffer_window_p ())
        FEsimple_error (Eonly_one_window);
      f = find_resizeable_edges ();
      if (!f)
        return 0;
      can->w_prev = 0;
      app.active_frame.windows = can;
    }
  else
    {
      f = find_resizeable_edges ();
      if (!f)
        return 0;
      can = w_prev;
      can->w_next = w_next;
      if (w_next)
        w_next->w_prev = can;
    }

  POINT op;
  op.x = w_rect.left + caret_x ();
  op.y = w_rect.top + caret_y ();

  resize_edge (f);
  save_buffer_params ();
  close ();
  Window *wp = find_point_window (op);
  if (!wp || !wp->w_bufp)
    wp = can;
  wp->set_window ();
  compute_geometry ();
  Buffer::maybe_modify_buffer_bar ();
  return 1;
}

lisp
Fdelete_window ()
{
  return boole (selected_window ()->delete_window ());
}

lisp
Fdeleted_window_p (lisp window)
{
  check_window (window);
  return boole (!xwindow_wp (window));
}

lisp
Fselected_window ()
{
  assert (xwindow_wp (selected_window ()->lwp));
  assert (xwindow_wp (selected_window ()->lwp) == selected_window ());
  return selected_window ()->lwp;
}

lisp
Fminibuffer_window ()
{
  return Window::minibuffer_window ()->lwp;
}

lisp
Fminibuffer_window_p (lisp window)
{
  check_window (window);
  return boole (xwindow_wp (window) && xwindow_wp (window)->minibuffer_window_p ());
}

lisp
Fwindow_buffer (lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  return wp->w_bufp ? wp->w_bufp->lbp : Qnil;
}

lisp
Fnext_window (lisp window, lisp minibufp)
{
  Window *wp = Window::coerce_to_window (window);
  if (!minibufp)
    minibufp = Qnil;
  Window *next = wp->w_next;
  if (!next
      || (!next->w_bufp && minibufp != Qt)
      || (next->minibuffer_window_p ()
          && minibufp != Qnil && minibufp != Qt))
    next = app.active_frame.windows;
  return next->lwp;
}

lisp
Fprevious_window (lisp window, lisp minibufp)
{
  Window *wp = Window::coerce_to_window (window);
  if (!minibufp)
    minibufp = Qnil;
  Window *prev = wp->w_prev;
  if (!prev)
    prev = Window::minibuffer_window ();
  if ((!prev->w_bufp && minibufp != Qt)
      || (prev->minibuffer_window_p ()
          && minibufp != Qnil && minibufp != Qt))
    prev = prev->w_prev;
  return prev->lwp;
}

lisp
Fget_buffer_window (lisp buffer, lisp curwin)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  Window *cwp = ((curwin && curwin != Qnil)
                 ? Window::coerce_to_window (curwin) : 0);
  int f = 0;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == bp)
      {
        if (wp != cwp)
          return wp->lwp;
        if (f)
          return wp->lwp;
        f = 1;
      }
  return f ? cwp->lwp : Qnil;
}

lisp
Fset_window (lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  if (!wp->w_bufp)
    return Qnil;
  wp->set_window ();
  return Qt;
}

lisp
Fscreen_width ()
{
  return make_fixnum (app.active_frame.size.cx / app.text_font.cell ().cx);
}

lisp
Fscreen_height ()
{
  return make_fixnum (app.active_frame.size.cy / app.text_font.cell ().cy);
}

lisp
Fwindow_height (lisp window)
{
  int h = (Window::coerce_to_window (window)->w_clsize.cy
           / app.text_font.cell ().cy);
  return make_fixnum (max (h, 1));
}

lisp
Fwindow_width (lisp window)
{
  int w = ((Window::coerce_to_window (window)->w_clsize.cx
            - app.text_font.cell ().cx / 2)
           / app.text_font.cell ().cx);
  return make_fixnum (max (w, 1));
}

lisp
Fwindow_lines (lisp window)
{
  int h = (Window::coerce_to_window (window)->w_clsize.cy
           / app.text_font.cell ().cy);
  return make_fixnum (max (h, 1));
}

lisp
Fwindow_columns (lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  int w = ((wp->w_clsize.cx - app.text_font.cell ().cx / 2)
           / app.text_font.cell ().cx);
  if (wp->flags () & Window::WF_LINE_NUMBER)
    w -= Window::LINENUM_COLUMNS + 1;
  if (wp->flags () & Window::WF_FOLD_MARK
      && wp->w_bufp
      && wp->w_bufp->b_fold_mode == Buffer::FOLD_WINDOW)
    w--;
  return make_fixnum (max (w, 1));
}

lisp
Fget_window_line (lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  if (!wp->w_bufp)
    return Qnil;
  return make_fixnum (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                      ? (wp->w_bufp->point_linenum (wp->w_point)
                         - wp->w_bufp->point_linenum (wp->w_disp))
                      : (wp->w_bufp->folded_point_linenum (wp->w_point)
                         - wp->w_bufp->folded_point_linenum (wp->w_disp)));
}

lisp
Fget_window_start_line (lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  if (!wp->w_bufp)
    return Qnil;
  return make_fixnum (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                      ? wp->w_bufp->point_linenum (wp->w_disp)
                      : wp->w_bufp->folded_point_linenum (wp->w_disp));
}

lisp
Fget_window_handle (lisp window)
{
  if (!window || window == Qnil)
    return make_fixnum (long (app.toplev));
  return make_fixnum (long (Window::coerce_to_window (window)->w_hwnd));
}

int
Window::find_horiz_order (int y)
{
  int y0 = y;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    {
      if (wp->w_rect.top > y && (y0 == y || wp->w_rect.top < y0))
        y0 = wp->w_rect.top;
      if (wp->w_rect.bottom > y && (y0 == y || wp->w_rect.bottom < y0))
        y0 = wp->w_rect.bottom;
    }
  return y == y0 ? -1 : y0;
}

void
Window::change_horiz_size (int bottom, int xmin, int xmax)
{
  int obottom = w_rect.bottom;
  if (obottom == bottom)
    return;

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.left < xmax && wp->w_rect.right > xmin)
      {
        if (wp->w_rect.top == obottom)
          wp->w_rect.top = bottom;
        if (wp->w_rect.bottom == obottom)
          wp->w_rect.bottom = bottom;
      }

  for (int y = find_horiz_order (-1), order = 0;
       y >= 0; y = find_horiz_order (y), order++)
    for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
      {
        if (wp->w_rect.top == y)
          wp->w_order.top = order;
        if (wp->w_rect.bottom == y)
          wp->w_order.bottom = order;
      }

  compute_geometry ();
}

int
Window::find_vert_order (int x)
{
  int x0 = x;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    {
      if (wp->w_rect.left > x && (x0 == x || wp->w_rect.left < x0))
        x0 = wp->w_rect.left;
      if (wp->w_rect.right > x && (x0 == x || wp->w_rect.right < x0))
        x0 = wp->w_rect.right;
    }
  return x == x0 ? -1 : x0;
}

void
Window::change_vert_size (int right, int ymin, int ymax)
{
  int oright = w_rect.right;
  if (oright == right)
    return;

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.top < ymax && wp->w_rect.bottom > ymin)
      {
        if (wp->w_rect.left == oright)
          wp->w_rect.left = right;
        if (wp->w_rect.right == oright)
          wp->w_rect.right = right;
      }

  for (int x = find_vert_order (-1), order = 0;
       x >= 0; x = find_vert_order (x), order++)
    for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
      {
        if (wp->w_rect.left == x)
          wp->w_order.left = order;
        if (wp->w_rect.right == x)
          wp->w_order.right = order;
      }

  compute_geometry ();
}

int
Window::enlarge_window_horiz (int n)
{
  Window *wp1 = find_horiz_window (&RECT::left);
  Window *wp2 = find_horiz_window (&RECT::right);
  if (!wp1 || !wp2)
    return 0;
  int goal = w_rect.bottom + n * app.text_font.cell ().cy;
  if (goal < get_horiz_min (wp1->w_rect.left, wp2->w_rect.right)
      || goal > get_horiz_max (wp1->w_rect.left, wp2->w_rect.right))
    return 0;
  change_horiz_size (goal, wp1->w_rect.left, wp2->w_rect.right);
  return 1;
}

int
Window::enlarge_window_vert (int n)
{
  Window *wp1 = find_vert_window (&RECT::top);
  Window *wp2 = find_vert_window (&RECT::bottom);
  if (!wp1 || !wp2)
    return 0;
  int goal = w_rect.right + n * app.text_font.cell ().cx;
  if (goal < get_vert_min (wp1->w_rect.top, wp2->w_rect.bottom)
      || goal > get_vert_max (wp1->w_rect.top, wp2->w_rect.bottom))
    return 0;
  change_vert_size (goal, wp1->w_rect.top, wp2->w_rect.bottom);
  return 1;
}

int
Window::enlarge_window (int n, int side)
{
  if (!n)
    return 1;
  if (!side)
    {
      Window *wp1, *wp2;
      for (wp1 = app.active_frame.windows; wp1; wp1 = wp1->w_next)
        if (wp1->w_rect.bottom == w_rect.top
            && wp1->w_rect.left < w_rect.right
            && wp1->w_rect.right > w_rect.left)
          break;
      for (wp2 = app.active_frame.windows; wp2; wp2 = wp2->w_next)
        if (wp2->w_rect.top == w_rect.bottom
            && wp2->w_rect.left < w_rect.right
            && wp2->w_rect.right > w_rect.left)
          break;
      return ((n < 0 && wp1 && wp2 && !wp2->w_next
               && wp1->enlarge_window_horiz (-n))
              || enlarge_window_horiz (n)
              || (wp1 && wp1->enlarge_window_horiz (-n)));
    }
  else
    {
      if (enlarge_window_vert (n))
        return 1;
      Window *wp;
      for (wp = app.active_frame.windows; wp; wp = wp->w_next)
        if (wp->w_rect.right == w_rect.left
            && wp->w_rect.top < w_rect.bottom
            && wp->w_rect.bottom > w_rect.top)
          break;
      return wp && wp->enlarge_window_vert (-n);
    }
}

lisp
Fenlarge_window (lisp nlines, lisp side)
{
  if (!selected_window ()->enlarge_window (((!nlines || nlines == Qnil)
                                            ? 1 : fixnum_value (nlines)),
                                           side && side != Qnil))
    FEsimple_error (Ecannot_change_window_size);
  return Qt;
}

Window *
Window::find_point_window (const POINT &point, int &vert)
{
  if (app.active_frame.windows)
    for (Window *wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
      if (PtInRect (&wp->w_rect, point))
        {
          if (point.x >= wp->w_rect.right - (FRAME_WIDTH + 1))
            vert = 1;
          else if (point.y >= wp->w_rect.bottom - (FRAME_WIDTH + 1))
            vert = 0;
          else
            continue;
          return wp;
        }
  return 0;
}

int
Window::frame_window_setcursor (HWND hwnd, WPARAM, LPARAM lparam)
{
  if (LOWORD (lparam) == HTCLIENT)
    {
      POINT point;
      GetCursorPos (&point);
      ScreenToClient (hwnd, &point);
      int vert;
      if (find_point_window (point, vert))
        {
          SetCursor (vert ? sysdep.hcur_sizewe : sysdep.hcur_sizens);
          return 1;
        }
    }
  return 0;
}

Window *
Window::find_resizeable_window (LONG RECT::*target,
                                LONG RECT::*emin, LONG RECT::*emax,
                                LONG RECT::*edge1, LONG RECT::*edge2) const
{
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp != this && wp->w_rect.*edge1 == w_rect.*edge2)
      {
        if (wp->w_rect.*target == w_rect.*target)
          return wp;
        if (wp->w_rect.*emin < w_rect.*target && wp->w_rect.*emax > w_rect.*target)
          return wp->find_resizeable_window (target, emin, emax, edge2, edge1);
      }
  return 0;
}

inline Window *
Window::find_horiz_window (LONG RECT::*target) const
{
  return find_resizeable_window (target, &RECT::left, &RECT::right,
                                 &RECT::top, &RECT::bottom);
}

inline Window *
Window::find_vert_window (LONG RECT::*target) const
{
  return find_resizeable_window (target, &RECT::top, &RECT::bottom,
                                 &RECT::left, &RECT::right);
}

int
Window::get_horiz_min (int xmin, int xmax) const
{
  int y = w_rect.top;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.bottom == w_rect.bottom
        && wp->w_rect.left < xmax && wp->w_rect.right > xmin)
      y = max (y, int (wp->w_rect.top));
  y += (app.modeline_param.m_height + 4 + app.text_font.cell ().cy
        + sysdep.edge.cy + FRAME_WIDTH);
  if (!minibuffer_window_p () && flags () & WF_RULER)
    y += RULER_HEIGHT;
  return min (y, int (w_rect.bottom));
}

int
Window::get_horiz_max (int xmin, int xmax) const
{
  Window *mini = minibuffer_window ();
  int y = mini->w_rect.bottom;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.top == w_rect.bottom
        && wp->w_rect.left < xmax && wp->w_rect.right > xmin)
      y = min (y, int (wp->w_rect.bottom));
  if (y == mini->w_rect.bottom)
    y -= app.text_font.cell ().cy + sysdep.edge.cy;
  else
    {
      y -= (app.modeline_param.m_height + 4 + app.text_font.cell ().cy
            + sysdep.edge.cy + FRAME_WIDTH);
      if (flags () & WF_RULER)
        y -= RULER_HEIGHT;
    }
  return y;
}

int
Window::get_vert_min (int ymin, int ymax) const
{
  int x = w_rect.left;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.right == w_rect.right
        && wp->w_rect.top < ymax && wp->w_rect.bottom > ymin)
      x = max (x, int (wp->w_rect.left));
  x += app.text_font.cell ().cx * WINDOW_WIDTH_MIN;
  return min (x, int (w_rect.right));
}

int
Window::get_vert_max (int ymin, int ymax) const
{
  int x = app.active_frame.size.cx;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_rect.left == w_rect.right
        && wp->w_rect.top < ymax && wp->w_rect.bottom > ymin)
      x = min (x, int (wp->w_rect.right));
  x -= app.text_font.cell ().cx * WINDOW_WIDTH_MIN;
  return x;
}

static void
paint_resize_line (HWND hwnd, const RECT &cr, int vert)
{
  RECT r = cr;
  if (vert)
    r.left -= FRAME_WIDTH;
  else
    r.top -= FRAME_WIDTH;
  MapWindowPoints (hwnd, app.toplev, (POINT *)&r, 2);
  HDC hdc = GetDC (app.toplev);
  HBITMAP hbm = LoadBitmap (app.hinst, MAKEINTRESOURCE (IDB_CHECK));
  HBRUSH hbr = CreatePatternBrush (hbm);
  DeleteObject (hbm);
  HGDIOBJ obr = SelectObject (hdc, hbr);
  PatBlt (hdc, r.left, r.top, r.right - r.left, r.bottom - r.top, PATINVERT);
  SelectObject (hdc, obr);
  DeleteObject (hbr);
  ReleaseDC (app.toplev, hdc);
}

int
Window::frame_window_resize (HWND hwnd, const POINT &point, int vert)
{
  int nmin, nmax;
  int d;
  RECT r;
  if (vert)
    {
      Window *top = find_vert_window (&RECT::top);
      if (!top)
        return 0;
      Window *bottom = find_vert_window (&RECT::bottom);
      if (!bottom)
        return 0;
      nmin = get_vert_min (top->w_rect.top, bottom->w_rect.bottom);
      nmax = get_vert_max (top->w_rect.top, bottom->w_rect.bottom);
      r.top = top->w_rect.top;
      r.bottom = bottom->w_rect.bottom;
      r.left = r.right = w_rect.right;
      d = w_rect.right - point.x;
    }
  else
    {
      Window *left = find_horiz_window (&RECT::left);
      if (!left)
        return 0;
      Window *right = find_horiz_window (&RECT::right);
      if (!right)
        return 0;
      nmin = get_horiz_min (left->w_rect.left, right->w_rect.right);
      nmax = get_horiz_max (left->w_rect.left, right->w_rect.right);
      r.left = left->w_rect.left;
      r.right = right->w_rect.right;
      r.top = r.bottom = w_rect.bottom;
      d = w_rect.bottom - point.y;
    }
  paint_resize_line (hwnd, r, vert);
  SetCapture (hwnd);

  MSG msg;
  while (1)
    {
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (0);
          break;
        }
      if (GetCapture () != hwnd)
        break;
      switch (msg.message)
        {
        case WM_MOUSEMOVE:
          paint_resize_line (hwnd, r, vert);
          if (vert)
            r.left = r.right = min (max (nmin,
                                         short (LOWORD (msg.lParam)) + d),
                                    nmax);
          else
            r.top = r.bottom = min (max (nmin,
                                         short (HIWORD (msg.lParam)) + d),
                                    nmax);
          paint_resize_line (hwnd, r, vert);
          break;

        case WM_LBUTTONUP:
          ReleaseCapture ();
          paint_resize_line (hwnd, r, vert);
          if (vert)
            change_vert_size (min (max (nmin, short (LOWORD (msg.lParam)) + d), nmax),
                              r.top, r.bottom);
          else
            change_horiz_size (min (max (nmin, short (HIWORD (msg.lParam)) + d), nmax),
                               r.left, r.right);
          refresh_screen (0);
          return 1;

        case WM_CANCELMODE:
          ReleaseCapture ();
          goto done;

        case WM_KEYDOWN:
          break;

        default:
          DispatchMessage (&msg);
          break;
        }
    }
done:
  paint_resize_line (hwnd, r, vert);
  return 1;
}

int
Window::frame_window_resize (HWND hwnd, LPARAM lparam, const POINT *real)
{
  POINT point;
  point.x = short (LOWORD (lparam));
  point.y = short (HIWORD (lparam));
  int vert;
  Window *wp = Window::find_point_window (point, vert);
  if (!wp)
    return 0;
  return wp->frame_window_resize (hwnd, real ? *real : point, vert);
}

WindowConfiguration::WindowConfiguration ()
{
  wc_nwindows = Window::count_windows ();
  wc_data = new Data[wc_nwindows];

  wc_selected = selected_window ();
  wc_size = app.active_frame.size;
  wc_prev = wc_chain;
  wc_chain = this;

  Data *d = wc_data;
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next, d++)
    {
      d->wp = wp;
      d->bufp = wp->w_bufp;
      d->point = wp->w_point.p_point;
      d->disp = wp->w_disp;
      d->mark = wp->w_mark;
      d->selection_point = wp->w_selection_point;
      d->selection_marker = wp->w_selection_marker;
      d->selection_type = wp->w_selection_type;
      d->reverse_temp = wp->w_reverse_temp;
      d->reverse_region = wp->w_reverse_region;
      d->top_column = wp->w_top_column;
      d->flags_mask = wp->w_flags_mask;
      d->flags = wp->w_flags;
      d->order = wp->w_order;
      d->rect = wp->w_rect;
    }
}

WindowConfiguration::~WindowConfiguration ()
{
  wc_chain = wc_prev;

  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    wp->w_disp_flags &= ~(Window::WDF_WINDOW | Window::WDF_MODELINE);
  for (Window *wp = app.active_frame.reserved; wp; wp = wp->w_next)
    wp->w_disp_flags &= ~(Window::WDF_WINDOW | Window::WDF_MODELINE);

  for (int i = 0; i < wc_nwindows; i++)
    wc_data[i].wp->w_disp_flags |= Window::WDF_WINDOW | Window::WDF_MODELINE;

  Buffer *bp = Buffer::dlist_find ();

  Window *reserved = 0, *next;
  for (Window *wp = app.active_frame.windows; wp; wp = next)
    {
      next = wp->w_next;
      if (!(wp->w_disp_flags & Window::WDF_WINDOW))
        {
          wp->w_next = reserved;
          reserved = wp;
        }
    }
  for (Window *wp = app.active_frame.reserved; wp; wp = next)
    {
      next = wp->w_next;
      if (!(wp->w_disp_flags & Window::WDF_WINDOW))
        {
          wp->w_next = reserved;
          reserved = wp;
        }
    }

  app.active_frame.selected = wc_selected;
  app.active_frame.windows = wc_data[0].wp;
  for (int i = 0; i < wc_nwindows; i++)
    {
      Window *wp = wc_data[i].wp;
      wp->w_prev = i ? wc_data[i - 1].wp : 0;
      wp->w_next = i != wc_nwindows - 1 ? wc_data[i + 1].wp : 0;

      xwindow_wp (wp->lwp) = wp;
      if (wc_data[i].bufp)
        {
          wp->set_buffer_params (wc_data[i].bufp);
          wp->w_bufp->goto_char (wp->w_point, wc_data[i].point);
          wp->w_disp = wc_data[i].disp;
          wp->w_mark = wc_data[i].mark;
          wp->w_selection_point = wc_data[i].selection_point;
          wp->w_selection_marker = wc_data[i].selection_marker;
          wp->w_selection_type = wc_data[i].selection_type;
          wp->w_reverse_temp = wc_data[i].reverse_temp;
          wp->w_reverse_region = wc_data[i].reverse_region;
          wp->w_top_column = wc_data[i].top_column;
        }
      else
	{
          wp->w_bufp = 0;
          if (!wp->minibuffer_window_p ())
            wp->set_buffer_params (bp);
          else
            wp->change_color ();
	}
      wp->w_flags_mask = wc_data[i].flags_mask;
      wp->w_flags = wc_data[i].flags;
      wp->w_rect = wc_data[i].rect;
      wp->w_order = wc_data[i].order;
    }

  assert (xwindow_wp (selected_window ()->lwp));
  assert (xwindow_wp (selected_window ()->lwp) == selected_window ());

  Window::compute_geometry (wc_size);

  app.active_frame.reserved = 0;
  for (Window *wp = reserved; wp; wp = next)
    {
      next = wp->w_next;
      wp->close ();
    }

  delete [] wc_data;
}

lisp
Fpos_not_visible_in_window_p (lisp point, lisp window)
{
  Window *wp = Window::coerce_to_window (window);
  Buffer *bp = wp->w_bufp;
  if (!bp)
    return Qnil;
  Point cur (wp->w_point);
  bp->goto_char (cur, bp->coerce_to_point (point));
  long top, linenum;
  if (bp->b_fold_columns == Buffer::FOLD_NONE)
    {
      linenum = bp->point_linenum (cur);
      top = bp->point_linenum (wp->w_disp);
    }
  else
    {
      linenum = bp->folded_point_linenum (cur);
      top = bp->folded_point_linenum (wp->w_disp);
    }
  return (linenum < top
          ? make_fixnum (-1)
          : (linenum >= top + wp->w_ech.cy
             ? make_fixnum (1)
             : Qnil));
}

lisp
Fget_window_flags ()
{
  return make_fixnum (Window::w_default_flags);
}

static int
check_modified_flags (Window *wp, int df)
{
  int recompute = 0;
  if (df & Window::WF_VSCROLL_BAR)
    {
      wp->update_vscroll_bar ();
      recompute = 1;
    }
  if (df & Window::WF_HSCROLL_BAR)
    {
      wp->update_hscroll_bar ();
      recompute = 1;
    }
  if (df & (Window::WF_MODE_LINE | Window::WF_RULER
            | Window::WF_LINE_NUMBER | Window::WF_FOLD_MARK))
    recompute = 1;
  return recompute;
}

lisp
Fset_window_flags (lisp flags)
{
  int f = fixnum_value (flags);
  int recompute = 0;
  int dflags = Window::w_default_flags;
  for (Window *w = app.active_frame.windows; w; w = w->w_next)
    {
      Window::w_default_flags = dflags;
      int of = w->flags ();
      Window::w_default_flags = f;
      int df = of ^ w->flags ();
      if (check_modified_flags (w, df))
        recompute = 1;
      w->w_disp_flags |= Window::WDF_WINDOW;
      if (df & (Window::WF_BGCOLOR_MODE | Window::WF_LINE_NUMBER))
        w->invalidate_glyphs ();
    }
  Window::w_default_flags = f;
  set_bgmode ();
  if ((f ^ dflags) & Window::WF_FUNCTION_BAR)
    recalc_toplevel ();
  else if (recompute)
    Window::compute_geometry ();
  return Qt;
}

lisp
Fget_local_window_flags (lisp lobj)
{
  int flag, mask;
  if (bufferp (lobj))
    {
      Buffer *bp = Buffer::coerce_to_buffer (lobj);
      flag = bp->b_wflags;
      mask = bp->b_wflags_mask;
    }
  else
    {
      Window *wp = Window::coerce_to_window (lobj);
      flag = wp->w_flags;
      mask = wp->w_flags_mask;
    }
  multiple_value::count () = 2;
  multiple_value::value (1) = make_fixnum (~mask & ~flag);
  return make_fixnum (flag);
}

static void
modify_wflags (int &flags, int &mask, int val, lisp lon)
{
  if (lon == Qt)
    {
      flags |= val;
      mask &= ~val;
    }
  else if (lon == Qnil)
    {
      flags &= ~val;
      mask &= ~val;
    }
  else
    {
      flags &= ~val;
      mask |= val;
    }
}

lisp
Fset_local_window_flags (lisp lobj, lisp lflags, lisp lon)
{
  int flags = fixnum_value (lflags);
  int recompute = 0;
  if (bufferp (lobj))
    {
      Buffer *bp = Buffer::coerce_to_buffer (lobj);
      int old_flags = bp->b_wflags;
      int old_flags_mask = bp->b_wflags_mask;
      int new_flags = old_flags;
      int new_flags_mask = old_flags_mask;
      modify_wflags (new_flags, new_flags_mask, flags, lon);
      if (Window::minibuffer_window ()->w_bufp == bp)
        {
          new_flags &= ~(Window::WF_MODE_LINE | Window::WF_RULER);
          new_flags_mask &= ~(Window::WF_MODE_LINE | Window::WF_RULER);
        }
      for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
        if (wp->w_bufp == bp)
          {
            bp->b_wflags = old_flags;
            bp->b_wflags_mask = old_flags_mask;
            int oflags = wp->flags ();
            bp->b_wflags = new_flags;
            bp->b_wflags_mask = new_flags_mask;
            int df = oflags ^ wp->flags ();
            wp->w_disp_flags |= Window::WDF_WINDOW;
            if (check_modified_flags (wp, df))
              recompute = 1;
          }
      bp->b_wflags = new_flags;
      bp->b_wflags_mask = new_flags_mask;
    }
  else
    {
      Window *wp = Window::coerce_to_window (lobj);
      int oflags = wp->flags ();
      modify_wflags (wp->w_flags, wp->w_flags_mask, flags, lon);
      if (wp->minibuffer_window_p ())
        {
          wp->w_flags &= ~(Window::WF_MODE_LINE | Window::WF_RULER);
          wp->w_flags_mask &= ~(Window::WF_MODE_LINE | Window::WF_RULER);
        }
      wp->w_disp_flags |= Window::WDF_WINDOW;
      int df = oflags ^ wp->flags ();
      if (check_modified_flags (wp, df))
        recompute = 1;
    }
  if (recompute)
    Window::compute_geometry ();
  return Qt;
}

lisp
Fsi_instance_number ()
{
  int i = xyzzy_instance::instnum ();
  return i >= 0 ? make_fixnum (i) : Qnil;
}

static void
activate_xyzzy_window (HWND hwnd)
{
  Fbegin_wait_cursor ();
  DWORD r;
  int ok = SendMessageTimeout (hwnd, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, 1000, &r);
  Fend_wait_cursor ();
  if (!ok)
    FEsimple_error (Etarget_xyzzy_is_busy);
  ForceSetForegroundWindow (hwnd);
  PostMessage (hwnd, WM_PRIVATE_FOREGROUND, 0, 0);
}

static lisp
next_xyzzy_window (int next)
{
  int i = xyzzy_instance::instnum ();
  if (i < 0)
    i = -1;
  xyzzy_hwnd xh (app.toplev);
  HWND hwnd = next ? xh.next (i) : xh.prev (i);
  if (!hwnd)
    return Qnil;
  activate_xyzzy_window (hwnd);
  return Qt;
}

lisp
Fnext_xyzzy_window ()
{
  return next_xyzzy_window (1);
}

lisp
Fprevious_xyzzy_window ()
{
  return next_xyzzy_window (0);
}

lisp
Fcount_xyzzy_instance ()
{
  xyzzy_hwnd xh (app.toplev);
  return make_fixnum (xh.count ());
}

lisp
Flist_xyzzy_windows ()
{
  xyzzy_hwnd xh (app.toplev);
  int i = -1;
  lisp p = Qnil;
  while (1)
    {
      int o = i;
      HWND hwnd = xh.next (i);
      if (!hwnd || i <= o)
        break;
      char buf[256];
      if (GetWindowText (hwnd, buf, sizeof buf))
        p = xcons (xcons (make_fixnum (i), make_string (buf)), p);
    }
  return Fnreverse (p);
}

lisp
Factivate_xyzzy_window (lisp x)
{
  int i = fixnum_value (x);
  int o = i--;
  xyzzy_hwnd xh (app.toplev);
  HWND hwnd = xh.next (i);
  if (!hwnd || i != o)
    return Qnil;
  activate_xyzzy_window (hwnd);
  return Qt;
}

static lisp
wc_point_marker (point_t point, Buffer *bp)
{
  if (point == NO_MARK_SET)
    return Qnil;
  lisp marker = Fmake_marker (bp->lbp);
  xmarker_point (marker) = point;
  return marker;
}

lisp
Fwindow_coordinate (lisp lwindow)
{
  Window *wp = Window::coerce_to_window (lwindow);
  return make_list (make_fixnum (wp->w_rect.left),
                    make_fixnum (wp->w_rect.top),
                    make_fixnum (wp->w_rect.right),
                    make_fixnum (wp->w_rect.bottom),
                    0);
}

lisp
Fcurrent_window_configuration ()
{
  lisp ldefs = Qnil;
  for (Window *wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    {
      Buffer *bp = wp->w_bufp;
      ldefs = xcons (make_list (wp->lwp,
                                bp->lbp,
                                wc_point_marker (wp->w_point.p_point, bp),
                                wc_point_marker (wp->w_disp, bp),
                                wc_point_marker (wp->w_mark, bp),
                                wc_point_marker (wp->w_selection_point, bp),
                                wc_point_marker (wp->w_selection_marker, bp),
                                (wp->w_selection_type != Buffer::SELECTION_VOID
                                 ? make_fixnum (wp->w_selection_type) : Qnil),
                                boole (wp->w_reverse_temp != Buffer::SELECTION_VOID),
                                wc_point_marker (wp->w_reverse_region.p1, bp),
                                wc_point_marker (wp->w_reverse_region.p2, bp),
                                make_fixnum (wp->w_top_column),
                                make_fixnum (wp->w_flags_mask),
                                make_fixnum (wp->w_flags),
                                make_list (make_fixnum (wp->w_order.left),
                                           make_fixnum (wp->w_order.top),
                                           make_fixnum (wp->w_order.right),
                                           make_fixnum (wp->w_order.bottom),
                                           0),
                                make_list (make_fixnum (wp->w_rect.left),
                                           make_fixnum (wp->w_rect.top),
                                           make_fixnum (wp->w_rect.right),
                                           make_fixnum (wp->w_rect.bottom),
                                           0),
                                0),
                     ldefs);
    }

  return make_list (Qwindow_configuration,
                    Fselected_window (),
                    Fnreverse (ldefs),
                    make_list (make_fixnum (app.active_frame.size.cx),
                               make_fixnum (app.active_frame.size.cy),
                               0),
                    0);
}

struct winconf
{
  lisp lwp;
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

static point_t
wc_marker_point (lisp x, int def = NO_MARK_SET)
{
  if (x == Qnil)
    return def;
  if (short_int_p (x))
    return xshort_int_value (x);
  if (long_int_p (x))
    return xlong_int_value (x);
  if (!markerp (x) || !xmarker_buffer (x)
      || xmarker_point (x) == NO_MARK_SET)
    return def;
  return xmarker_point (x);
}

static void
wc_rect (RECT &r, lisp x)
{
  if (xlist_length (x) != 4)
    FEprogram_error (Einvalid_window_configuration);
  r.left = fixnum_value (xcar (x));
  x = xcdr (x);
  r.top = fixnum_value (xcar (x));
  x = xcdr (x);
  r.right = fixnum_value (xcar (x));
  x = xcdr (x);
  r.bottom = fixnum_value (xcar (x));
}

static int
wc_calc_order (winconf *conf, int nwindows,
            LONG RECT::*edge1, LONG RECT::*edge2, int sz)
{
  int norders = -1;
  for (int i = 0; i < nwindows; i++)
    {
      if (conf[i].order.*edge1 >= conf[i].order.*edge2
          || conf[i].order.*edge1 < 0
          || conf[i].rect.*edge1 >= conf[i].rect.*edge2
          || conf[i].rect.*edge1 < 0)
        FEprogram_error (Einvalid_window_configuration);
      if (conf[i].order.*edge2 > norders)
        norders = conf[i].order.*edge2;
    }
  if (norders > nwindows * 2)
    FEprogram_error (Einvalid_window_configuration);

  int *pixels = (int *)alloca (sizeof *pixels * (norders + 1));

  for (int i = 0; i <= norders; i++)
    pixels[i] = -1;

  for (int i = 0; i < nwindows; i++)
    {
      if (pixels[conf[i].order.*edge1] == -1)
        pixels[conf[i].order.*edge1] = conf[i].rect.*edge1;
      else if (pixels[conf[i].order.*edge1] != conf[i].rect.*edge1)
        FEprogram_error (Einvalid_window_configuration);
      if (pixels[conf[i].order.*edge2] == -1)
        pixels[conf[i].order.*edge2] = conf[i].rect.*edge2;
      else if (pixels[conf[i].order.*edge2] != conf[i].rect.*edge2)
        FEprogram_error (Einvalid_window_configuration);
    }

  if (pixels[0] || pixels[norders] > sz)
    FEprogram_error (Einvalid_window_configuration);

  for (int i = 1; i <= norders; i++)
    if (pixels[i] == -1 || pixels[i] < pixels[i - 1])
      FEprogram_error (Einvalid_window_configuration);

  return norders;
}

static void
wc_check_order (winconf *conf, int nwindows, const SIZE &sz)
{
  int nx = wc_calc_order (conf, nwindows, &RECT::left, &RECT::right, sz.cx);
  int ny = wc_calc_order (conf, nwindows, &RECT::top, &RECT::bottom, sz.cy);
  const int n = nx * ny;
  char *const f = (char *)alloca (n);
  bzero (f, n);
  for (int i = 0; i < nwindows; i++)
    for (int y = conf[i].order.top; y < conf[i].order.bottom; y++)
      for (int x = conf[i].order.left; x < conf[i].order.right; x++)
        if (f[y * nx + x]++)
          FEprogram_error (Einvalid_window_configuration);
  for (int i = 0; i < n; i++)
    if (!f[i])
      FEprogram_error (Einvalid_window_configuration);
}

static point_t
wc_range (Buffer *bp, point_t point)
{
  return min (max (point, bp->b_contents.p1), bp->b_contents.p2);
}

static void
wc_restore (winconf *conf, int nwindows, const SIZE &size,
            lisp lselected_window, int curw)
{
  Buffer *const bp = selected_buffer ();
  Window *cur_wp = 0;
  Window *odeleted = app.active_frame.deleted;
  for (int i = 0; i < nwindows; i++)
    {
      if (!conf[i].wp)
        {
          Window *wp = new Window ();
          wp->w_next = app.active_frame.deleted;
          app.active_frame.deleted = wp;
          conf[i].wp = wp;
        }
      if (conf[i].lwp == Qnil)
        conf[i].lwp = conf[i].wp->lwp;
      else
        conf[i].wp->lwp = conf[i].lwp;
      if (conf[i].lwp == lselected_window)
        cur_wp = conf[i].wp;
    }

  if (curw >= 0 && curw < nwindows)
    cur_wp = conf[curw].wp;

  app.active_frame.deleted = odeleted;

  Window *wp;
  for (wp = app.active_frame.windows; wp->w_next; wp = wp->w_next)
    wp->w_disp_flags &= ~(Window::WDF_WINDOW | Window::WDF_MODELINE);
  Window *const mini_wp = wp;
  for (wp = app.active_frame.reserved; wp; wp = wp->w_next)
    wp->w_disp_flags &= ~(Window::WDF_WINDOW | Window::WDF_MODELINE);

  for (int i = 0; i < nwindows; i++)
    conf[i].wp->w_disp_flags |= (Window::WDF_WINDOW
                                 | Window::WDF_MODELINE
                                 | Window::WDF_GOAL_COLUMN);
  mini_wp->w_disp_flags |= (Window::WDF_WINDOW
                            | Window::WDF_MODELINE
                            | Window::WDF_GOAL_COLUMN);

  Window *reserved = 0, *next;
  for (wp = app.active_frame.windows; wp->w_next; wp = next)
    {
      next = wp->w_next;
      if (!(wp->w_disp_flags & Window::WDF_WINDOW))
        {
          wp->w_next = reserved;
          reserved = wp;
        }
    }
  for (wp = app.active_frame.reserved; wp; wp = next)
    {
      next = wp->w_next;
      if (!(wp->w_disp_flags & Window::WDF_WINDOW))
        {
          wp->w_next = reserved;
          reserved = wp;
        }
    }

  long ymax = -1;
  app.active_frame.selected = cur_wp ? cur_wp : conf[0].wp;
  app.active_frame.windows = conf[0].wp;
  for (int i = 0; i < nwindows; i++)
    {
      wp = conf[i].wp;
      wp->w_prev = i ? conf[i - 1].wp : 0;
      wp->w_next = i != nwindows - 1 ? conf[i + 1].wp : mini_wp;

      xwindow_wp (wp->lwp) = wp;
      Buffer *bufp = conf[i].bufp;
      if (bufp)
        {
          wp->set_buffer_params (conf[i].bufp);
          wp->w_bufp->goto_char (wp->w_point, conf[i].point);
          wp->w_disp = wc_range (bufp, conf[i].disp);
          wp->w_last_disp = wp->w_disp;

          wp->w_mark = conf[i].mark;
          if (wp->w_mark != NO_MARK_SET)
            wp->w_mark = wc_range (bufp, wp->w_mark);

          wp->w_selection_point = conf[i].selection_point;
          wp->w_selection_marker = conf[i].selection_marker;
          wp->w_selection_type = conf[i].selection_type;
          if (wp->w_selection_type != Buffer::SELECTION_VOID)
            {
              (int &)wp->w_selection_type &=
                Buffer::SELECTION_TYPE_MASK | Buffer::PRE_SELECTION;
              switch (wp->w_selection_type & Buffer::SELECTION_TYPE_MASK)
                {
                case Buffer::SELECTION_LINEAR:
                case Buffer::SELECTION_REGION:
                case Buffer::SELECTION_RECTANGLE:
                  break;

                default:
                  wp->w_selection_type = Buffer::SELECTION_VOID;
                  break;
                }
            }
          if (wp->w_selection_point == NO_MARK_SET
              || wp->w_selection_marker == NO_MARK_SET
              || wp->w_selection_type == Buffer::SELECTION_VOID)
            {
              wp->w_selection_point = NO_MARK_SET;
              wp->w_selection_marker = NO_MARK_SET;
              wp->w_selection_type = Buffer::SELECTION_VOID;
            }
          else
            {
              wp->w_selection_point = wc_range (bufp, wp->w_selection_point);
              wp->w_selection_marker = wc_range (bufp, wp->w_selection_marker);
            }

          wp->w_reverse_temp = conf[i].reverse_temp;
          wp->w_reverse_region = conf[i].reverse_region;

          if (wp->w_reverse_temp != Buffer::SELECTION_VOID)
            (int &)wp->w_reverse_temp &= Buffer::PRE_SELECTION;
          if (wp->w_reverse_region.p1 == NO_MARK_SET
              || wp->w_reverse_region.p2 == NO_MARK_SET)
            {
              wp->w_reverse_temp = Buffer::SELECTION_VOID;
              wp->w_reverse_region.p1 = NO_MARK_SET;
              wp->w_reverse_region.p2 = NO_MARK_SET;
            }
          else
            {
              wp->w_reverse_region.p1 = wc_range (bufp, wp->w_reverse_region.p1);
              wp->w_reverse_region.p2 = wc_range (bufp, wp->w_reverse_region.p2);
              if (wp->w_reverse_region.p1 > wp->w_reverse_region.p2)
                swap (wp->w_reverse_region.p1, wp->w_reverse_region.p2);
            }

          wp->w_top_column = conf[i].top_column;
          if (wp->w_top_column < 0)
            wp->w_top_column = 0;
        }
      else
	{
          wp->w_bufp = 0;
          if (!wp->minibuffer_window_p ())
            wp->set_buffer_params (bp);
          else
            wp->change_color ();
	}
      wp->w_goal_column = 0;
      wp->w_flags_mask = conf[i].flags_mask;
      wp->w_flags = conf[i].flags;
      wp->w_rect = conf[i].rect;
      wp->w_order = conf[i].order;
      ymax = max (ymax, wp->w_rect.bottom);
    }

  mini_wp->w_prev = conf[nwindows - 1].wp;
  mini_wp->w_rect.left = 0;
  mini_wp->w_rect.top = ymax;
  mini_wp->w_rect.right = size.cx;
  mini_wp->w_rect.bottom = size.cy;

  assert (xwindow_wp (selected_window ()->lwp));
  assert (xwindow_wp (selected_window ()->lwp) == selected_window ());

  Window::compute_geometry (size);

  app.active_frame.reserved = 0;
  for (wp = reserved; wp; wp = next)
    {
      next = wp->w_next;
      wp->close ();
    }
}

lisp
Fset_window_configuration (lisp lconf)
{
  lisp x = lconf;
  if (xlist_length (x) != 4 || xcar (x) != Qwindow_configuration)
    FEtype_error (lconf, Qwindow_configuration);

  x = xcdr (x);
  lisp lselected_window = xcar (x);
  long curw = -1;
  if (lselected_window != Qnil
      && !safe_fixnum_value (lselected_window, &curw))
    {
      curw = -1;
      check_window (lselected_window);
    }

  x = xcdr (x);
  lisp ldefs = xcar (x);
  if (!consp (ldefs))
    FEprogram_error (Einvalid_window_configuration);

  x = xcar (xcdr (x));
  if (xlist_length (x) != 2)
    FEprogram_error (Einvalid_window_configuration);

  SIZE size;
  size.cx = fixnum_value (xcar (x));
  size.cy = fixnum_value (xcar (xcdr (x)));
  if (size.cx < 0 || size.cy < 0)
    FEprogram_error (Einvalid_window_configuration);

  int nwindows = xlist_length (ldefs);
  if (nwindows < 1)
    FEprogram_error (Einvalid_window_configuration);

  int selected_window_ok = 0;
  winconf *conf = (winconf *)alloca (sizeof *conf * nwindows);
  for (int i = 0; i < nwindows; i++, ldefs = xcdr (ldefs))
    {
      x = xcar (ldefs);
      if (xlist_length (x) != 16)
        FEprogram_error (Einvalid_window_configuration);

      conf[i].lwp = xcar (x);
      if (conf[i].lwp == Qnil)
        conf[i].wp = 0;
      else
        {
          check_window (conf[i].lwp);
          if (lselected_window == conf[i].lwp)
            selected_window_ok = 1;
          conf[i].wp = xwindow_wp (conf[i].lwp);
          if (!conf[i].wp)
            {
              for (Window *wp = app.active_frame.reserved; wp; wp = wp->w_next)
                if (wp->lwp == conf[i].lwp)
                  {
                    conf[i].wp = wp;
                    break;
                  }
            }
          else if (conf[i].wp->minibuffer_window_p ())
            FEprogram_error (Einvalid_window_configuration);
        }
      x = xcdr (x);
      if (xcar (x) == Qnil)
        conf[i].bufp = 0;
      else
        {
          check_buffer (xcar (x));
          conf[i].bufp = xbuffer_bp (xcar (x));
        }
      x = xcdr (x);
      conf[i].point = wc_marker_point (xcar (x), 0);
      x = xcdr (x);
      conf[i].disp = wc_marker_point (xcar (x), 0);
      x = xcdr (x);
      conf[i].mark = wc_marker_point (xcar (x));
      x = xcdr (x);
      conf[i].selection_point = wc_marker_point (xcar (x));
      x = xcdr (x);
      conf[i].selection_marker = wc_marker_point (xcar (x));
      x = xcdr (x);
      conf[i].selection_type = (xcar (x) == Qnil
                                ? Buffer::SELECTION_VOID
                                : Buffer::selection_type (fixnum_value (xcar (x))));
      x = xcdr (x);
      conf[i].reverse_temp = (xcar (x) == Qnil
                              ? Buffer::SELECTION_VOID
                              : Buffer::selection_type (Buffer::PRE_SELECTION
                                                        | Buffer::CONTINUE_PRE_SELECTION));
      x = xcdr (x);
      conf[i].reverse_region.p1 = wc_marker_point (xcar (x));
      x = xcdr (x);
      conf[i].reverse_region.p2 = wc_marker_point (xcar (x));
      x = xcdr (x);
      conf[i].top_column = fixnum_value (xcar (x));
      x = xcdr (x);
      conf[i].flags_mask = fixnum_value (xcar (x));
      x = xcdr (x);
      conf[i].flags = fixnum_value (xcar (x));
      x = xcdr (x);
      wc_rect (conf[i].order, xcar (x));
      x = xcdr (x);
      wc_rect (conf[i].rect, xcar (x));
    }

  if (lselected_window != Qnil && curw < 0 && !selected_window_ok)
    FEprogram_error (Einvalid_window_configuration);

  wc_check_order (conf, nwindows, size);
  wc_restore (conf, nwindows, size, lselected_window, curw);

  return Qnil;
}

#ifndef SPI_GETFOREGROUNDLOCKTIMEOUT
#define SPI_GETFOREGROUNDLOCKTIMEOUT 0x2000
#define SPI_SETFOREGROUNDLOCKTIMEOUT 0x2001
#endif

void
ForceSetForegroundWindow (HWND hwnd)
{
  DWORD timeout;
  if (sysdep.version () >= Sysdep::WIN98_VERSION
      && SystemParametersInfo (SPI_GETFOREGROUNDLOCKTIMEOUT, 0, &timeout, 0))
    {
      SystemParametersInfo (SPI_SETFOREGROUNDLOCKTIMEOUT, 0, 0, 0);
      int ok = SetForegroundWindow (hwnd);
      SystemParametersInfo (SPI_SETFOREGROUNDLOCKTIMEOUT, 0, (void *)timeout, 0);
      if (!ok)
        {
          HWND hwnd_fg = GetForegroundWindow ();
          DWORD r;
          if (hwnd_fg && SendMessageTimeout (hwnd_fg, WM_NULL, 0, 0,
                                             SMTO_ABORTIFHUNG | SMTO_BLOCK, 100, &r))
            {
              DWORD id = GetWindowThreadProcessId (hwnd_fg, 0);
              AttachThreadInput (GetCurrentThreadId (), id, 1);
              SetForegroundWindow (hwnd);
              AttachThreadInput (GetCurrentThreadId (), id, 0);
            }
        }
    }
  else
    SetForegroundWindow (hwnd);
}

static void __stdcall
auto_scroll (int n, void *arg)
{
  Window *wp = (Window *)arg;
  if (wp->scroll_window (n))
    refresh_screen (1);
}

lisp
Fbegin_auto_scroll ()
{
  POINT p;
  GetCursorPos (&p);
  Window *wp = Window::find_scr_point_window (p, 0, 0);
  if (!wp)
    return Qnil;
  Buffer *bp = wp->w_bufp;
  if (!bp
      || (bp->b_fold_columns == Buffer::FOLD_NONE
          ? bp->count_lines ()
          : bp->folded_count_lines ()) <= 1
      || !begin_auto_scroll (wp->w_hwnd, p, auto_scroll, wp))
    return Qnil;
  return Qt;
}

void
Window::calc_ruler_rect (RECT &r) const
{
  POINT p = {0, 0};
  MapWindowPoints (w_hwnd, app.active_frame.hwnd, &p, 1);
  r.left = p.x + app.text_font.cell ().cx / 2;
  if (flags () & WF_LINE_NUMBER)
    r.left += (LINENUM_COLUMNS + 1) * app.text_font.cell ().cx;
  r.top = p.y - RULER_HEIGHT;
  r.right = p.x + w_clsize.cx + RIGHT_PADDING - 1;
  r.bottom = p.y - 3;
}

inline void
Window::calc_ruler_box (const RECT &r, RECT &br) const
{
  br.left = r.left + (w_ruler_column - w_ruler_top_column) * app.text_font.cell ().cx;
  br.right = br.left + app.text_font.cell ().cx;
  br.top = r.top;
  br.bottom = r.bottom;
}

void
Window::paint_ruler_box (HDC hdc, const RECT &r) const
{
  RECT br;
  calc_ruler_box (r, br);

  br.right--;
  draw_hline (hdc, br.left, br.right, br.top, sysdep.window_text);
  draw_vline (hdc, br.top, br.bottom, br.left, sysdep.window_text);
  draw_vline (hdc, br.top, br.bottom, br.right, sysdep.window_text);
  br.bottom--;
  draw_hline (hdc, br.left, br.right, br.bottom, sysdep.window_text);
  br.left++;
  br.top++;
  draw_hline (hdc, br.left, br.right, br.top, sysdep.btn_highlight);
  draw_vline (hdc, br.top, br.bottom, br.left, sysdep.btn_highlight);
  br.left++;
  br.top++;
  br.right--;
  draw_vline (hdc, br.top, br.bottom, br.right, sysdep.btn_shadow);
  br.bottom--;
  draw_hline (hdc, br.left, br.right, br.bottom, sysdep.btn_shadow);
  fill_rect (hdc, br.left, br.top, br.right - br.left, br.bottom - br.top, sysdep.btn_face);
}

inline void
Window::paint_ruler (HDC hdc, const RECT &r, int x, int y, int column) const
{
  if (!(column % 10))
    {
      char buf[32];
      int l = sprintf (buf, "%d", column);
      ExtTextOut (hdc, x - l * sysdep.ruler_ext.cx / 2, r.top,
                  ETO_CLIPPED, &r, buf, l, 0);
    }
  else if (!(column % 5))
    draw_vline (hdc, y - 2, y + 2, x, sysdep.window_text);
  else
    draw_vline (hdc, y - 1, y + 1, x, sysdep.window_text);
}

void
Window::paint_ruler (HDC hdc) const
{
  if (w_ruler_top_column < 0)
    return;

  RECT r;

  GetWindowRect (w_hwnd, &r);
  MapWindowPoints (HWND_DESKTOP, app.active_frame.hwnd, (POINT *)&r, 2);
  r.bottom = r.top;
  r.top -= RULER_HEIGHT;
  draw_hline (hdc, r.left, r.right - 1, r.top, sysdep.btn_highlight);
  draw_vline (hdc, r.top, r.bottom, r.left, sysdep.btn_highlight);
//  draw_hline (hdc, r.left, r.right, r.bottom, sysdep.btn_shadow);
  draw_vline (hdc, r.top, r.bottom, r.right - 1, sysdep.btn_shadow);

  calc_ruler_rect (r);

  if (w_ruler_fold_column == Buffer::FOLD_NONE)
    fill_rect (hdc, r, sysdep.window);
  else if (w_ruler_fold_column <= w_ruler_top_column)
    fill_rect (hdc, r, sysdep.btn_shadow);
  else
    {
      int x = r.left + ((w_ruler_fold_column - w_ruler_top_column)
                        * app.text_font.cell ().cx);
      if (x < r.right)
        {
          fill_rect (hdc, r.left, r.top, x - r.left, r.bottom - r.top, sysdep.window);
          fill_rect (hdc, x, r.top, r.right - x, r.bottom - r.top, sysdep.btn_shadow);
        }
      else
        fill_rect (hdc, r, sysdep.window);
    }

  HGDIOBJ of = SelectObject (hdc, sysdep.hfont_ruler);
  COLORREF ofg = SetTextColor (hdc, sysdep.window_text);
  int bkmode = SetBkMode (hdc, TRANSPARENT);

  int y = (r.top + r.bottom) / 2;
  for (int x = r.left + app.text_font.cell ().cx / 2, column = w_ruler_top_column + 1;
       x < r.right; x += app.text_font.cell ().cx, column++)
    paint_ruler (hdc, r, x, y, column);

  SetTextColor (hdc, ofg);
  SetBkMode (hdc, bkmode);
  SelectObject (hdc, of);

  if (w_ruler_column >= 0)
    paint_ruler_box (hdc, r);
}

void
Window::erase_ruler (HDC hdc, const RECT &r) const
{
  RECT br;
  calc_ruler_box (r, br);

  if (w_ruler_fold_column == Buffer::FOLD_NONE
      || w_ruler_column < w_ruler_fold_column)
    fill_rect (hdc, br, sysdep.window);
  else
    fill_rect (hdc, br, sysdep.btn_shadow);

  HGDIOBJ of = SelectObject (hdc, sysdep.hfont_ruler);
  COLORREF ofg = SetTextColor (hdc, sysdep.window_text);
  int bkmode = SetBkMode (hdc, TRANSPARENT);

  int y = (r.top + r.bottom) / 2;
  int x = (r.left + app.text_font.cell ().cx / 2
           + (w_ruler_column - w_ruler_top_column) * app.text_font.cell ().cx);
  int column = w_ruler_column + 1;
  paint_ruler (hdc, br, x, y, column);

  int rem = column % 10;
  if (rem)
    {
      column -= rem;
      x -= rem * app.text_font.cell ().cx;
      if (column && x >= r.left)
        paint_ruler (hdc, br, x, y, column);
      column += 10;
      x += 10 * app.text_font.cell ().cx;
      if (x < r.right)
        paint_ruler (hdc, br, x, y, column);
    }

  SetTextColor (hdc, ofg);
  SetBkMode (hdc, bkmode);
  SelectObject (hdc, of);
}

void
Window::update_ruler ()
{
  if (w_disp_flags & WDF_WINDOW
      || w_ruler_top_column != w_top_column
      || w_ruler_fold_column != w_bufp->b_fold_columns)
    {
      w_ruler_top_column = w_top_column;
      w_ruler_column = w_column;
      w_ruler_fold_column = w_bufp->b_fold_columns;
      HDC hdc = GetDC (app.active_frame.hwnd);
      paint_ruler (hdc);
      ReleaseDC (app.active_frame.hwnd, hdc);
    }
  else if (w_ruler_column != w_column)
    {
      HDC hdc = GetDC (app.active_frame.hwnd);
      RECT r;
      calc_ruler_rect (r);
      if (w_ruler_column >= 0)
        erase_ruler (hdc, r);
      w_ruler_column = w_column;
      paint_ruler_box (hdc, r);
      ReleaseDC (app.active_frame.hwnd, hdc);
    }
}

void
Window::point2window_pos (point_t point, POINT &p) const
{
  long linenum, column;

  point = max (min (point, w_bufp->b_contents.p2),
               w_bufp->b_contents.p1);
  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      Point p;
      w_bufp->set_point (p, point);
      linenum = w_bufp->point_linenum (p);
      column = w_bufp->point_column (p);
    }
  else
    linenum = w_bufp->folded_point_linenum_column (point, &column);

  p.x = column - w_last_top_column + w_bufp->b_prompt_columns;
  if (w_last_flags & Window::WF_LINE_NUMBER)
    p.x += Window::LINENUM_COLUMNS + 1;
  p.x = min (max (0L, p.x), w_ch_max.cx);
  p.x *= app.text_font.cell ().cx;
  p.x += app.text_font.cell ().cx / 2;

  p.y = linenum - w_last_top_linenum;
  p.y = min (max (0L, p.y), w_ch_max.cy);
  p.y *= app.text_font.cell ().cy;
}
