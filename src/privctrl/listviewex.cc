#include "stdafx.h"
#include "privctlimpl.h"
#include "mousemsg.h"

static WNDPROC ListViewProc;

#undef ListView_EnsureVisible
#define ListView_EnsureVisible(hwndLV, i, fPartialOK) \
  (BOOL)CallWindowProc (ListViewProc, (hwndLV), LVM_ENSUREVISIBLE, (WPARAM)(int)(i), MAKELPARAM((fPartialOK), 0))

#undef ListView_FindItem
#define ListView_FindItem(hwnd, iStart, plvfi) \
  (int)CallWindowProc (ListViewProc, (hwnd), LVM_FINDITEM, (WPARAM)(int)(iStart), (LPARAM)(const LV_FINDINFO FAR*)(plvfi))

#undef ListView_GetColumn
#define ListView_GetColumn(hwnd, iCol, pcol) \
    (BOOL)CallWindowProc (ListViewProc, (hwnd), LVM_GETCOLUMN, (WPARAM)(int)(iCol), (LPARAM)(LV_COLUMN FAR*)(pcol))

#undef ListView_GetImageList
#define ListView_GetImageList(hwnd, iImageList) \
  (HIMAGELIST)CallWindowProc (ListViewProc, (hwnd), LVM_GETIMAGELIST, (WPARAM)(INT)(iImageList), 0L)

#undef ListView_GetItem
#define ListView_GetItem(hwnd, pitem) \
  (BOOL)CallWindowProc (ListViewProc, (hwnd), LVM_GETITEM, 0, (LPARAM)(LV_ITEM FAR*)(pitem))

#undef ListView_GetItemCount
#define ListView_GetItemCount(hwnd) \
  (int)CallWindowProc (ListViewProc, (hwnd), LVM_GETITEMCOUNT, 0, 0L)

#undef ListView_GetItemRect
#define ListView_GetItemRect(hwnd, i, prc, code) \
  (BOOL)CallWindowProc (ListViewProc, (hwnd), LVM_GETITEMRECT, (WPARAM)(int)(i), \
                    ((prc) ? (((RECT FAR *)(prc))->left = (code),(LPARAM)(RECT FAR*)(prc)) : (LPARAM)(RECT FAR*)NULL))

#undef ListView_SetItemState
#define ListView_SetItemState(hwndLV, i, data, mask) \
  { LV_ITEM _ms_lvi;\
    _ms_lvi.stateMask = mask;\
    _ms_lvi.state = data;\
    CallWindowProc (ListViewProc, (hwndLV), LVM_SETITEMSTATE, (WPARAM)i, (LPARAM)(LV_ITEM FAR *)&_ms_lvi);\
  }

#undef ListView_GetItemText

#undef ListView_GetNextItem
#define ListView_GetNextItem(hwnd, i, flags) \
  (int)CallWindowProc (ListViewProc, (hwnd), LVM_GETNEXTITEM, (WPARAM)(int)(i), MAKELPARAM((flags), 0))

#undef ListView_Scroll
#define ListView_Scroll(hwndLV, dx, dy) \
  (BOOL)CallWindowProc (ListViewProc, (hwndLV), LVM_SCROLL, (WPARAM)(int)dx, (LPARAM)(int)dy)

#undef ListView_GetTopIndex
#define ListView_GetTopIndex(hwndLV) \
  (int)CallWindowProc (ListViewProc, (hwndLV), LVM_GETTOPINDEX, 0, 0)

#undef ListView_GetItemState
#define ListView_GetItemState(hwndLV, i, mask) \
  (UINT)CallWindowProc (ListViewProc, (hwndLV), LVM_GETITEMSTATE, (WPARAM)i, (LPARAM)mask)

#undef ListView_GetCountPerPage
#define ListView_GetCountPerPage(hwndLV) \
  (int)CallWindowProc (ListViewProc, (hwndLV), LVM_GETCOUNTPERPAGE, 0, 0)

#undef ListView_InsertColumn
#define ListView_InsertColumn(hwnd, iCol, pcol) \
  (int)CallWindowProc (ListViewProc, (hwnd), LVM_INSERTCOLUMN, (WPARAM)(int)(iCol), (LPARAM)(const LV_COLUMN FAR*)(pcol))

#define STATEIMAGEMASKTOINDEX(i) (((i) >> 12) - 1)
#define OFFSET_FIRST 2
#define OFFSET_REST 6
#define ISEARCH_TIMEOUT1 200
#define ISEARCH_TIMEOUT2 600

struct listview_item_data: public item_data
{
  HWND hwnd_header;
  DWORD style;
  SIZE client;
  COLORREF bkcolor;
  COLORREF textcolor;
  COLORREF textbkcolor;
  COLORREF cursor_color;
  COLORREF highlight_text_color;
  COLORREF highlight_color;
  DWORD itick;
  size_t icc;
  int f_ikeyup;
  int f_in_notify;
  int f_deleted;
  int isearch_cur;
  int pseudo_enable;
  int sort_mark_id;
  int sort_mark_dir;
  int path_ellipse_indices;
  HIMAGELIST hil_sub;
  u_char up_char;
  u_char down_char;
  u_char def_char;
  u_char pageup_char;
  u_char pagedown_char;
};

int WINAPI abbreviate_string (HDC hdc, char *buf, int maxpxl, int is_pathname);

static inline listview_item_data *
get_listview_item_data (HWND hwnd)
{
  return (listview_item_data *)get_item_data (hwnd);
}

static int
paint_text (HDC hdc, char *s, int l, int fmt, const RECT &r,
            int offset, int dots, int no_extend, int on, int path_ellipse)
{
  int w = r.right - r.left - 2 * offset;
  SIZE ext;
  GetTextExtentPoint32 (hdc, s, l, &ext);
  int trim = 0;
  int ofmt = fmt;
  if (l && ext.cx > w)
    {
      fmt = LVCFMT_LEFT;
      if (path_ellipse && abbreviate_string (hdc, s, w, 1))
        {
          l = strlen (s);
          GetTextExtentPoint32 (hdc, s, l, &ext);
        }
      else
        {
          w -= dots;
          int ll = l;
          char *se;
          for (se = CharPrev (s, s + l); se > s; se = CharPrev (s, se))
            {
              GetTextExtentPoint32 (hdc, s, se - s, &ext);
              if (ext.cx <= w)
                break;
            }
          l = se - s;
          if (l || ext.cx < w + dots + offset)
            {
              if (!l)
                l = IsDBCSLeadByte (*s & 0xff) ? 2 : 1;
              if (l != ll)
                {
                  strcpy (s + l, "...");
                  l += 3;
                  trim = 1;
                }
            }
        }
    }

  int x;
  switch (fmt)
    {
    case LVCFMT_LEFT:
      x = r.left + offset;
      break;

    case LVCFMT_RIGHT:
      x = r.right - ext.cx - offset;
      break;

    default:
      x = (r.left + r.right - ext.cx) / 2;
      break;
    }

  if (no_extend)
    {
      RECT rr;
      rr.left = r.left;
      rr.top = r.top;
      rr.right = x + ext.cx + offset;
      rr.bottom = r.bottom;
      ExtTextOut (hdc, x + on, (r.top + r.bottom - ext.cy) / 2 + on,
                  ETO_OPAQUE | ETO_CLIPPED, &rr, s, l, 0);
      return rr.right;
    }
  else
    {
      ExtTextOut (hdc, x + on, (r.top + r.bottom - ext.cy) / 2 + on,
                  ETO_OPAQUE | ETO_CLIPPED, &r, s, l, 0);
      return (ofmt == LVCFMT_RIGHT
              ? (trim ? r.left : x)
              : (trim ? r.right : x + ext.cx));
    }
}

static int
paint_item_text (HWND hwnd, HDC hdc, int item, int subitem, int fmt,
                 const RECT &r, int offset, int dots, int no_extend,
                 LPARAM lparam, const listview_item_data *data)
{
  char s[1024 + 10];
  LV_ITEM lvi;
  lvi.iSubItem = subitem;
  lvi.pszText = s;
  lvi.cchTextMax = 1024;
  int l = CallWindowProc (ListViewProc, hwnd, LVM_GETITEMTEXT, item, LPARAM (&lvi));
  l = min (l, 1024);
  if (s != lvi.pszText)
    memcpy (s, lvi.pszText, l);
  s[l] = 0;
  return paint_text (hdc, s, l, fmt, r, offset, dots, no_extend, 0,
                     data->path_ellipse_indices & (1 << subitem));
}

static void
paint_colors (const listview_item_data *data, int enabled, int selected,
              int hilited, COLORREF &fg, COLORREF &bg)
{
  if (!enabled)
    {
      if (hilited)
        {
          fg = data->textbkcolor;
          bg = GetSysColor (COLOR_BTNFACE);
        }
      else if (selected)
        {
          fg = data->textbkcolor;
          bg = GetSysColor (COLOR_GRAYTEXT);
        }
      else
        {
          fg = GetSysColor (COLOR_GRAYTEXT);
          bg = data->textbkcolor;
        }
    }
  else
    {
      if (hilited)
        {
          fg = data->highlight_text_color;
          bg = RGB (0, 0, 0);
        }
      else if (selected)
        {
          fg = data->highlight_text_color;
          bg = data->highlight_color;
        }
      else
        {
          fg = data->textcolor;
          bg = data->textbkcolor;
        }
    }
}

static LONG
listview_draw_item (UINT id, DRAWITEMSTRUCT *dis)
{
  HWND hwnd = dis->hwndItem;
  HDC hdc = dis->hDC;
  const RECT &r = dis->rcItem;
  const listview_item_data *data = get_listview_item_data (hwnd);

  LV_ITEM lvi;
  lvi.mask = LVIF_IMAGE | LVIF_STATE | LVIF_PARAM;
  lvi.iItem = dis->itemID;
  lvi.iSubItem = 0;
  lvi.stateMask = UINT (-1);
  if (!ListView_GetItem (hwnd, &lvi))
    return 1;

  int focus = GetFocus () == hwnd;
  int selected = ((focus || get_window_style (hwnd) & LVS_SHOWSELALWAYS)
                  && lvi.state & LVIS_SELECTED);
  int enabled = IsWindowEnabled (hwnd) && data->pseudo_enable;

  COLORREF fg, bg;
  paint_colors (data, enabled, selected, lvi.state & LVIS_DROPHILITED, fg, bg);
  COLORREF ofg = SetTextColor (hdc, fg);
  COLORREF obg = SetBkColor (hdc, bg);

  if (lvi.state & LVIS_STATEIMAGEMASK)
    {
      HIMAGELIST hil = ListView_GetImageList (hwnd, LVSIL_STATE);
#undef ILD_TRANSPARENT
#define ILD_TRANSPARENT 0
      if (hil)
        ImageList_Draw (hil,
                        STATEIMAGEMASKTOINDEX (lvi.state & LVIS_STATEIMAGEMASK),
                        hdc, r.left + OFFSET_FIRST, r.top, ILD_TRANSPARENT);
    }

  int index = lvi.iImage;
  HIMAGELIST hil;
  if (index < 0 && data->hil_sub)
    {
      hil = data->hil_sub;
      index = -1 - index;
    }
  else
    hil = ListView_GetImageList (hwnd, LVSIL_SMALL);
  if (hil)
    {
      RECT r;
      ListView_GetItemRect (hwnd, dis->itemID, &r, LVIR_ICON);
      int cx, cy;
      ImageList_GetIconSize (hil, &cx, &cy);
      if (cy < r.bottom - r.top)
        {
          HBRUSH hbr = CreateSolidBrush (data->textbkcolor);
          HGDIOBJ o = SelectObject (hdc, hbr);
          PatBlt (hdc, r.left, r.top, r.right - r.left, r.bottom - r.top, PATCOPY);
          SelectObject (hdc, o);
          DeleteObject (hbr);
        }

      COLORREF bg = data->bkcolor;
      COLORREF fg = bg;
      UINT flags = ILD_TRANSPARENT | (lvi.state & LVIS_OVERLAYMASK);
      if (!enabled)
        {
          if (lvi.state & LVIS_DROPHILITED)
            fg = GetSysColor (COLOR_BTNFACE);
          else if (selected)
            fg = GetSysColor (COLOR_GRAYTEXT);
          else
            fg = data->textbkcolor;
          flags |= ILD_BLEND50;
        }
      else if(lvi.state & LVIS_CUT)
        flags |= ILD_BLEND50;
      else if (lvi.state & LVIS_DROPHILITED)
        {
          fg = RGB (0, 0, 0);
          flags |= ILD_BLEND50;
        }
      else if (selected)
        {
          fg = data->highlight_color;
          flags |= ILD_BLEND50;
        }
      int top = r.top + ((r.bottom - r.top) / 2) - (cy / 2);
      ImageList_DrawEx (hil, index, hdc, r.left, top, 0, 0,
                        bg, fg, flags);
    }

  SIZE dots;
  GetTextExtentPoint32 (hdc, "...", 3, &dots);

  RECT label;
  ListView_GetItemRect (hwnd, dis->itemID, &label, LVIR_LABEL);

  int lleft = min (label.left, label.right);

  if (lvi.state & LVIS_DROPHILITED)
    {
      int rest = paint_item_text (hwnd, hdc, dis->itemID, 0, LVCFMT_LEFT, label,
                                  OFFSET_FIRST, dots.cx, 1, lvi.lParam, data);
      paint_colors (data, enabled, selected, 0, fg, bg);
      SetTextColor (hdc, fg);
      SetBkColor (hdc, bg);
      label.left = rest;
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &label, "", 0, 0);
    }
  else
    paint_item_text (hwnd, hdc, dis->itemID, 0, LVCFMT_LEFT, label,
                     OFFSET_FIRST, dots.cx, 0, lvi.lParam, data);

  LV_COLUMN lvc;
  lvc.mask = LVCF_FMT | LVCF_WIDTH;
  for (int i = 1; ListView_GetColumn (hwnd, i, &lvc); i++)
    {
      label.left = label.right;
      label.right += lvc.cx;
      paint_item_text (hwnd, hdc, dis->itemID, i, lvc.fmt & LVCFMT_JUSTIFYMASK,
                       label, OFFSET_REST, dots.cx, 0, lvi.lParam, data);
    }

  if ((data->style & LVS_TYPEMASKEX) == LVS_EXREPORTEX)
    {
      label.left = label.right;
      label.right = data->client.cx;
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &label, "", 0, 0);
    }

  if (focus && lvi.state & LVIS_FOCUSED)
    {
      if (data->style & (LVS_EXTENDKBD | LVS_PROCESSKEY))
        {
          HPEN hpen = CreatePen (PS_SOLID, 0, data->cursor_color);
          HGDIOBJ open = SelectObject (hdc, hpen);
          MoveToEx (hdc, lleft, label.bottom - 2, 0);
          LineTo (hdc, label.left, label.bottom - 2);
          SelectObject (hdc, open);
          DeleteObject (hpen);
        }
      else
        {
          label.left = lleft;
          DrawFocusRect (hdc, &label);
        }
    }

  SetTextColor (hdc, ofg);
  SetBkColor (hdc, obg);

  return 1;
}

static void
erase_bkgnd (HWND hwnd, HDC hdc)
{
  RECT r, br, ir;
  GetClipBox (hdc, &r);
  int nitems = ListView_GetItemCount (hwnd);
  if (nitems > 0)
    {
      ListView_GetItemRect (hwnd, nitems - 1, &ir, LVIR_ICON);
      ListView_GetItemRect (hwnd, nitems - 1, &br, LVIR_BOUNDS);
      if (ir.top >= r.bottom)
        {
          ir.top = r.top;
          ir.bottom = r.bottom;
          ir.right = ir.left;
          ir.left = r.left;
          r.top = r.bottom;
        }
      else if (ir.bottom < r.bottom)
        {
          ir.top = r.top;
          ir.bottom = ir.bottom;
          ir.right = ir.left;
          ir.left = r.left;
          r.top = ir.bottom;
        }
      else
        ir.left = ir.right;
    }
  else
    ir.left = ir.right = 0;
  listview_item_data *data = get_listview_item_data (hwnd);
  HBRUSH hbr = CreateSolidBrush (data->textbkcolor);
  HGDIOBJ o = SelectObject (hdc, hbr);
  if (ir.left != ir.right)
    {
      PatBlt (hdc, ir.left, ir.top, ir.right - ir.left, ir.bottom - ir.top, PATCOPY);
      if (r.right > br.right)
        PatBlt (hdc, br.right, ir.top, r.right - br.right, ir.bottom - ir.top, PATCOPY);
    }
  if (r.top != r.bottom)
    PatBlt (hdc, r.left, r.top, r.right - r.left, r.bottom - r.top, PATCOPY);
  SelectObject (hdc, o);
  DeleteObject (hbr);
}

int
change_style (HWND hwnd, DWORD style)
{
  if ((style & LVS_TYPEMASKEX) > LVS_EXREPORTEX)
    return 0;
  listview_item_data *data = get_listview_item_data (hwnd);
  if (style == data->style)
    return 1;

  if (set_window_style (hwnd,
                        ((get_window_style (hwnd) & ~(LVS_TYPEMASK
                                                      | LVS_OWNERDRAWFIXED))
                         | ((style & LVS_TYPEMASKEX) >= LVS_EXREPORT
                            ? LVS_REPORT | LVS_OWNERDRAWFIXED
                            : (style & LVS_TYPEMASKEX)))))
    {
      data->style = style;
      InvalidateRect (hwnd, 0, 1);
      return 1;
    }

  return 0;
}

static void
extend_clip_region (HWND hwnd)
{
  listview_item_data *data = get_listview_item_data (hwnd);
  if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT)
    {
      PAINTSTRUCT ps;
      HDC hdc = BeginPaint (hwnd, &ps);
      RECT clip;
      GetClipBox (hdc, &clip);
      clip.left = 0;
      clip.right = data->client.cx;
      InvalidateRect (hwnd, &clip, 0);
      EndPaint (hwnd, &ps);
    }
}

static void
repaint_selected_items (HWND hwnd)
{
  if ((get_ctl_style (hwnd) & LVS_TYPEMASKEX) < LVS_EXREPORT)
    return;

  RECT r, l;
  int i = ListView_GetNextItem (hwnd, -1, LVNI_FOCUSED);
  if (i != -1)
    {
      ListView_GetItemRect (hwnd, i, &r, LVIR_BOUNDS);
      ListView_GetItemRect (hwnd, i, &l, LVIR_LABEL);
      r.left = l.left;
      InvalidateRect (hwnd, &r, 0);
    }

  if(!(get_window_style (hwnd) & LVS_SHOWSELALWAYS))
    for (i = ListView_GetNextItem (hwnd, -1, LVNI_SELECTED);
         i != -1; i = ListView_GetNextItem (hwnd, i, LVNI_SELECTED))
      {
        ListView_GetItemRect (hwnd, i, &r, LVIR_BOUNDS);
        ListView_GetItemRect (hwnd, i, &l, LVIR_LABEL);
        r.left = l.left;
        InvalidateRect (hwnd, &r, 0);
      }
  UpdateWindow (hwnd);
}

static void
send_keydown (HWND hwnd, int vkey)
{
  HWND parent = GetParent (hwnd);
  if (parent)
    {
      LV_KEYDOWN lv;
      lv.hdr.hwndFrom = hwnd;
      lv.hdr.idFrom = GetWindowLong (hwnd, GWL_ID);
      lv.hdr.code = LVN_KEYDOWN;
      lv.wVKey = WORD (vkey);
      lv.flags = 0;
      SendMessage (parent, WM_NOTIFY, lv.hdr.idFrom, LPARAM (&lv));
    }
}

static void
find_header (HWND hwnd)
{
  listview_item_data *data = get_listview_item_data (hwnd);
  for (hwnd = GetWindow (hwnd, GW_CHILD); hwnd;
       hwnd = GetWindow (hwnd, GW_HWNDNEXT))
    {
      char b[128];
      GetClassName (hwnd, b, sizeof b);
      if (!strcmp (b, WC_HEADERA))
        {
          data->hwnd_header = hwnd;
          break;
        }
    }
}

static int
insert_column (HWND hwnd, listview_item_data *data,
               int col, const LV_COLUMN *lc)
{
  int r = ListView_InsertColumn (hwnd, col, lc);
  if (r >= 0)
    {
      HD_ITEM hi;
      hi.mask = HDI_FORMAT;
      hi.fmt = HDF_OWNERDRAW;
      if (lc->mask & LVCF_FMT)
        hi.fmt |= lc->fmt & LVCFMT_JUSTIFYMASK;
      Header_SetItem (data->hwnd_header, r, &hi);
    }
  return r;
}

static void
paint_up (HDC hdc, int x, const RECT &r, int on)
{
  int y = (r.top + r.bottom + 1) / 2 - 4 + on;
  x += on;
  HGDIOBJ open = SelectObject (hdc, CreatePen (PS_SOLID, 0,
                                               GetSysColor (COLOR_BTNSHADOW)));
  MoveToEx (hdc, x + 7, y, 0);
  LineTo (hdc, x, y);
  LineTo (hdc, x + 3, y + 7);
  DeleteObject (SelectObject (hdc, CreatePen (PS_SOLID, 0,
                                              GetSysColor (COLOR_BTNHIGHLIGHT))));
  MoveToEx (hdc, x + 7, y, 0);
  LineTo (hdc, x + 4, y + 7);
  DeleteObject (SelectObject (hdc, open));
}

static void
paint_down (HDC hdc, int x, const RECT &r, int on)
{
  int y = (r.top + r.bottom + 1) / 2 + 2 + on;
  x += on;
  HGDIOBJ open = SelectObject (hdc, CreatePen (PS_SOLID, 1,
                                               GetSysColor (COLOR_BTNSHADOW)));
  MoveToEx (hdc, x, y, 0);
  LineTo (hdc, x + 3, y - 7);
  DeleteObject (SelectObject (hdc, CreatePen (PS_SOLID, 0,
                                              GetSysColor (COLOR_BTNHIGHLIGHT))));
  MoveToEx (hdc, x, y, 0);
  LineTo (hdc, x + 7, y);
  LineTo (hdc, x + 4, y - 7);
  DeleteObject (SelectObject (hdc, open));
}

static void
draw_header (HWND hwnd, listview_item_data *data, const DRAWITEMSTRUCT *dis)
{
  char b[1024 + 10];
  HD_ITEM hi;
  hi.mask = HDI_FORMAT | HDI_TEXT;
  hi.pszText = b;
  hi.cchTextMax = 1024;
  if (!Header_GetItem (data->hwnd_header, dis->itemID, &hi))
    return;

  int sort_mark = (data->sort_mark_id >= 0
                   && int (dis->itemID) == data->sort_mark_id);

  RECT r (dis->rcItem);
  int fmt = hi.fmt & HDF_JUSTIFYMASK;
  if (sort_mark)
    {
      if (fmt != HDF_RIGHT)
        r.right -= 12;
      else
        r.left += 12;
      if (r.left >= r.right)
        return;
    }

  SIZE dots;
  GetTextExtentPoint32 (dis->hDC, "...", 3, &dots);

  int on = dis->itemState & ODS_SELECTED ? 1 : 0;
  int x = paint_text (dis->hDC, b, strlen (b), fmt,
                      r, OFFSET_REST, dots.cx, 0, on, 0);

  if (sort_mark)
    {
      if (fmt != HDF_RIGHT)
        {
          x = min (x + 16, r.right + 2);
          if (x + 8 < dis->rcItem.right)
            {
              if (data->sort_mark_dir == LVSM_DOWN)
                paint_down (dis->hDC, x, dis->rcItem, on);
              else
                paint_up (dis->hDC, x, dis->rcItem, on);
            }
        }
      else
        {
          x = max (x - 24, r.left - 10);
          if (x >= dis->rcItem.left)
            {
              if (data->sort_mark_dir == LVSM_DOWN)
                paint_down (dis->hDC, x, dis->rcItem, on);
              else
                paint_up (dis->hDC, x, dis->rcItem, on);
            }
        }
    }
}

static void
set_header_sort_mark (HWND hwnd, listview_item_data *data, int index, int dir)
{
  if (!data->hwnd_header)
    find_header (hwnd);
  if (data->sort_mark_id != index
      || data->sort_mark_dir != dir)
    {
      data->sort_mark_id = index;
      data->sort_mark_dir = dir;
      if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
          && data->hwnd_header)
        InvalidateRect (data->hwnd_header, 0, 1);
    }
}

static int
send_process_key (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam,
                  listview_item_data *data)
{
  HWND parent = GetParent (hwnd);
  if (!parent)
    return 0;
  LV_PROCESSKEY lv;
  lv.hdr.hwndFrom = hwnd;
  lv.hdr.idFrom = GetWindowLong (hwnd, GWL_ID);
  lv.hdr.code = LVN_PROCESSKEY;
  lv.message = msg;
  lv.wparam = wparam;
  lv.lparam = lparam;

  data->f_in_notify++;
  int f = SendMessage (parent, WM_NOTIFY, lv.hdr.idFrom, LPARAM (&lv));
  if (--data->f_in_notify || !data->f_deleted)
    return f;
  free_item_data (hwnd);
  return 1;
}

static int
get_current_focus (HWND hwnd, int &nitems)
{
  nitems = ListView_GetItemCount (hwnd);
  for (int i = 0; i < nitems; i++)
    if (ListView_GetItemState (hwnd, i, LVIS_FOCUSED))
      return i;
  return -1;
}

static void
make_visible (HWND hwnd, int i, int partial_ok)
{
  RECT r;
  ListView_GetItemRect (hwnd, i, &r, LVIR_BOUNDS);
  int d = r.bottom - r.top;
  int top = ListView_GetTopIndex (hwnd);
  if (i < top)
    ListView_Scroll (hwnd, 0, (i - top) * d);
  else
    {
      int bottom = top + ListView_GetCountPerPage (hwnd);
      if (i < bottom)
        return;
      if (i == bottom && partial_ok)
        {
          RECT cr;
          GetClientRect (hwnd, &cr);
          if (r.top < cr.bottom)
            return;
        }
      ListView_Scroll (hwnd, 0, (i - bottom + 1) * d);
    }
}

static int
set_focus (HWND hwnd, int cur, int next, int nitems)
{
  next = max (min (next, nitems - 1), 0);
  if (next != cur)
    {
      int f = (get_window_style (hwnd) & LVS_SINGLESEL
               ? LVIS_FOCUSED | LVIS_SELECTED : LVIS_FOCUSED);
      ListView_SetItemState (hwnd, next, f, f);
    }
  make_visible (hwnd, next, 0);
//  ListView_EnsureVisible (hwnd, next, 0);
  return next;
}

static int
forward_line (HWND hwnd, int nlines)
{
  int nitems;
  int cur = get_current_focus (hwnd, nitems);
  int next = cur == -1 ? 0 : cur + nlines;
  return set_focus (hwnd, cur, next, nitems);
}

static int
goto_bof (HWND hwnd)
{
  int nitems;
  int cur = get_current_focus (hwnd, nitems);
  return set_focus (hwnd, cur, 0, nitems);
}

static int
goto_eof (HWND hwnd)
{
  int nitems;
  int cur = get_current_focus (hwnd, nitems);
  return set_focus (hwnd, cur, nitems - 1, nitems);
}

static int
forward_page (HWND hwnd, int npages, listview_item_data *data)
{
  int top = ListView_GetTopIndex (hwnd);
  int nitems;
  int cur = get_current_focus (hwnd, nitems);
  int nlines = ListView_GetCountPerPage (hwnd);
  int nreq = npages;
  int next;

  if (!npages)
    return cur;
  if (cur == -1)
    next = 0;
  else if (npages > 0)
    {
      next = top + nlines - 1;
      if (cur == next)
        next += nlines;
    }
  else
    {
      if (cur == top)
        next = top - nlines;
      else
        next = top;
    }
  if (npages > 0)
    npages--;
  else
    npages++;
  next += nlines * npages;
  if (nreq > 0 && next < cur)
    next = cur;
  return set_focus (hwnd, cur, next, nitems);
}

static void
process_keydown (HWND hwnd, int vkey, listview_item_data *data)
{
  switch (vkey)
    {
    case VK_UP:
      forward_line (hwnd, -1);
      break;

    case VK_DOWN:
      forward_line (hwnd, 1);
      break;

    case VK_NEXT:
      forward_page (hwnd, 1, data);
      break;

    case VK_PRIOR:
      forward_page (hwnd, -1, data);
      break;
    }
  send_keydown (hwnd, vkey);
}

#define upcase(c) CharUpper (LPSTR (c & 0xff))
#define eql(c1, c2) (upcase (c1) == upcase (c2))

static int
isearch (HWND hwnd, int cc, int wrap, listview_item_data *data)
{
  int nitems;
  int cur = get_current_focus (hwnd, nitems);

  DWORD tick = GetTickCount ();
  LV_ITEM lvi;
  char *text = (char *)alloca (data->icc + 3);
  if (cur == data->isearch_cur && cur >= 0 && data->icc)
    {
      *text = 0;
      lvi.iSubItem = 0;
      lvi.pszText = text;
      lvi.cchTextMax = data->icc + 2;
      size_t l = CallWindowProc (ListViewProc, hwnd, LVM_GETITEMTEXT,
                                 cur, LPARAM (&lvi));
      if (lvi.pszText != text)
        {
          memcpy (text, lvi.pszText, data->icc + 2);
          text[data->icc + 2] = 0;
        }
      if (strlen (text) < data->icc)
        data->icc = 0;
    }
  else
    data->icc = 0;

  if (data->icc)
    {
      if ((!data->f_ikeyup && data->icc == 1)
          || tick > data->itick + (data->icc == 1 && *text == char (cc)
                                   ? ISEARCH_TIMEOUT1 : ISEARCH_TIMEOUT2))
        data->icc = 0;
      else if (eql (text[data->icc], cc))
        {
          data->icc++;
          data->itick = tick;
          data->f_ikeyup = 0;
          return 1;
        }
    }

  text[data->icc] = char (cc);
  text[data->icc + 1] = 0;

  data->itick = tick;
  data->f_ikeyup = 0;

  int found;
  LV_FINDINFO fi;
  fi.flags = LVFI_PARTIAL;
  if (wrap)
    fi.flags |= LVFI_WRAP;
  fi.psz = text;
  data->f_ikeyup = 1;
  while (1)
    {
      found = ListView_FindItem (hwnd, max (cur, 0), &fi);
      if (found == -1)
        {
          data->f_ikeyup = 0;
          if (data->icc == 1 && eql (text[0], cc))
            {
              data->icc = 0;
              text[1] = 0;
            }
          else
            {
              data->icc = 0;
              break;
            }
        }
      else
        {
          data->icc++;
          set_focus (hwnd, cur, found, nitems);
          break;
        }
    }
  data->isearch_cur = found;
  return found >= 0;
}

static void
process_chars (HWND hwnd, int cc, listview_item_data *data)
{
  int nitems;
  int cur = get_current_focus (hwnd, nitems);
  if (cc == ' ' && cur >= 0)
    {
      data->icc = 0;
      ListView_SetItemState (hwnd, cur,
                             (ListView_GetItemState (hwnd, cur, LVIS_SELECTED)
                              ? 0 : LVIS_SELECTED),
                             LVIS_SELECTED);
      set_focus (hwnd, cur, cur + 1, nitems);
    }
  else
    isearch (hwnd, cc, 1, data);
}

static int
process_keys (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam,
              listview_item_data *data)
{
  switch (msg)
    {
    case WM_KEYUP:
      data->f_ikeyup = 1;
      if (data->style & LVS_PROCESSKEY)
        send_process_key (hwnd, msg, wparam, lparam, data);
      return 1;

    case WM_SYSKEYUP:
      data->f_ikeyup = 1;
      if (data->style & LVS_PROCESSKEY)
        send_process_key (hwnd, msg, wparam, lparam, data);
      return 0;

    case WM_KEYDOWN:
      if (data->style & LVS_PROCESSKEY)
        return send_process_key (hwnd, msg, wparam, lparam, data);
      process_keydown (hwnd, wparam, data);
      return 1;

    case WM_CHAR:
      if (data->style & LVS_PROCESSKEY)
        return send_process_key (hwnd, msg, wparam, lparam, data);
      process_chars (hwnd, wparam, data);
      return 1;

    case WM_SYSCHAR:
    case WM_SYSKEYDOWN:
      if (data->style & LVS_PROCESSKEY)
        return send_process_key (hwnd, msg, wparam, lparam, data);
      return 0;

    default:
      return 0;
    }
}

static LPARAM CALLBACK
ListViewExProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_NCCREATE:
      {
        listview_item_data *data =
          (listview_item_data *)alloc_item_data (hwnd, sizeof *data);
        if (!data)
          return 0;
        if (!set_owner_draw_proc (hwnd, listview_draw_item))
          {
            free_item_data (hwnd);
            return 0;
          }
        subclass_parent (hwnd);
        data->hwnd_header = 0;
        data->sort_mark_id = -1;
        data->sort_mark_dir = LVSM_DOWN;
        data->path_ellipse_indices = 0;
        data->style = ((CREATESTRUCT *)lparam)->style & LVS_TYPEMASK;
        data->textcolor = GetSysColor (COLOR_WINDOWTEXT);
        data->textbkcolor = GetSysColor (COLOR_WINDOW);
        data->bkcolor = GetSysColor (COLOR_WINDOW);
        data->cursor_color = RGB (192, 0, 192);
        data->highlight_text_color = GetSysColor (COLOR_HIGHLIGHTTEXT);
        data->highlight_color = GetSysColor (COLOR_HIGHLIGHT);
        data->icc = 0;
        data->itick = 0;
        data->pseudo_enable = 1;
        data->hil_sub = 0;
        data->up_char = 'P' - '@';
        data->down_char = 'N' - '@';
        data->def_char = 'M' - '@';
        data->pageup_char = u_char (-1);
        data->pagedown_char = u_char (-1);
        data->f_in_notify = 0;
        data->f_deleted = 0;
      }
      break;

    case WM_NCDESTROY:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if (data && data->hil_sub)
          ImageList_Destroy (data->hil_sub);
        if (data->f_in_notify)
          data->f_deleted = 1;
        else
          free_item_data (hwnd);
        break;
      }

    case WM_CREATE:
      {
        LPARAM lr = CallWindowProc (ListViewProc, hwnd, msg, wparam, lparam);
        find_header (hwnd);
        return lr;
      }

    case WM_ERASEBKGND:
      erase_bkgnd (hwnd, HDC (wparam));
      return 1;

    case WM_PAINT:
      extend_clip_region (hwnd);
      break;

    case WM_SIZE:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        data->client.cx = LOWORD (lparam);
        data->client.cy = HIWORD (lparam);
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->style & (LVS_EXTENDKBD | LVS_PROCESSKEY))
          {
            int nitems;
            int cur = get_current_focus (hwnd, nitems);
            if (cur != -1)
              make_visible (hwnd, cur, 0);
          }
      }
      break;

    case WM_SYSKEYDOWN:
    case WM_SYSKEYUP:
    case WM_SYSCHAR:
    case WM_KEYDOWN:
    case WM_KEYUP:
    case WM_CHAR:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->style & (LVS_EXTENDKBD | LVS_PROCESSKEY)
            && process_keys (hwnd, msg, wparam, lparam, data))
          return 0;

        if (msg == WM_CHAR)
          {
            if (wparam == data->up_char)
              forward_line (hwnd, -1);
            else if (wparam == data->down_char)
              forward_line (hwnd, 1);
            else if (wparam == data->def_char)
              {
                HWND h = GetParent (hwnd);
                if (h)
                  {
                    char cls[16];
                    if (GetClassName (h, cls, sizeof cls)
                        && !strcmp (cls, "#32770"))
                      {
                        DWORD id = SendMessage (h, DM_GETDEFID, 0, 0);
                        if (HIWORD (id) == DC_HASDEFID)
                          {
                            HWND btn = GetDlgItem (h, LOWORD (id));
                            if (btn)
                              PostMessage (h, WM_COMMAND, MAKEWPARAM (LOWORD (id), BN_CLICKED),
                                           LPARAM (btn));
                          }
                      }
                  }
              }
            else if (wparam == data->pageup_char)
              forward_page (hwnd, -1, data);
            else if (wparam == data->pagedown_char)
              forward_page (hwnd, 1, data);
          }
        break;
      }

    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_XBUTTONDOWN:
    case WM_XBUTTONUP:
    case WM_MOUSEMOVE:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        data->icc = 0;
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->style & LVS_PROCESSKEY
            && send_process_key (hwnd, msg, wparam, lparam, data))
          return 0;
        break;
      }

    case WM_DRAWITEM:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        DRAWITEMSTRUCT *dis = (DRAWITEMSTRUCT *)lparam;
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->hwnd_header
            && dis->hwndItem == data->hwnd_header)
          {
            draw_header (hwnd, data, dis);
            return 1;
          }
        break;
      }

    case LVM_INSERTCOLUMN:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->hwnd_header)
          return insert_column (hwnd, data, wparam, (LV_COLUMN *)lparam);
        break;
      }

    case LVM_SETEXSTYLE:
      return change_style (hwnd, wparam);

    case LVM_GETEXSTYLE:
      return get_ctl_style (hwnd) & LVS_TYPEMASKEX;

    case LVM_SETTEXTCOLOR:
      get_listview_item_data (hwnd)->textcolor = COLORREF (lparam);
      break;

    case LVM_SETTEXTBKCOLOR:
      get_listview_item_data (hwnd)->textbkcolor = COLORREF (lparam);
      break;

    case LVM_SETBKCOLOR:
      get_listview_item_data (hwnd)->bkcolor = COLORREF (lparam);
      break;

    case LVM_SETCURSORCOLOR:
      get_listview_item_data (hwnd)->cursor_color = COLORREF (lparam);
      break;

    case LVM_SETHIGHLIGHTTEXTCOLOR:
      get_listview_item_data (hwnd)->highlight_text_color = COLORREF (lparam);
      break;

    case LVM_SETHIGHLIGHTCOLOR:
      get_listview_item_data (hwnd)->highlight_color = COLORREF (lparam);
      break;

    case LVM_ENSUREVISIBLE:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT
            && data->style & (LVS_EXTENDKBD | LVS_PROCESSKEY))
          {
            make_visible (hwnd, wparam, lparam);
            return 1;
          }
        break;
      }

    case LVM_ISEARCH:
      return isearch (hwnd, wparam, lparam, get_listview_item_data (hwnd));

    case LVM_FORWARDLINE:
      return forward_line (hwnd, wparam);

    case LVM_FORWARDPAGE:
      return forward_page (hwnd, wparam, get_listview_item_data (hwnd));

    case LVM_GOTOBOF:
      return goto_bof (hwnd);

    case LVM_GOTOEOF:
      return goto_eof (hwnd);

    case LVM_GETENABLED:
      return get_listview_item_data (hwnd)->pseudo_enable;

    case LVM_SETENABLED:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if ((data->style & LVS_TYPEMASKEX) >= LVS_EXREPORT)
          {
            if (!data->pseudo_enable != !wparam)
              {
                data->pseudo_enable = wparam;
                InvalidateRect (hwnd, 0, 0);
              }
            return 1;
          }
        return 0;
      }

    case LVM_SETSUBIMAGELIST:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        HIMAGELIST h = data->hil_sub;
        data->hil_sub = HIMAGELIST (lparam);
        return LPARAM (h);
      }

    case LVM_SETSORTMARK:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        set_header_sort_mark (hwnd, data, wparam, lparam);
        return 1;
      }

    case LVM_SETPATHELLIPSE:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        if (lparam)
          data->path_ellipse_indices |= 1 << wparam;
        else
          data->path_ellipse_indices &= ~(1 << wparam);
        InvalidateRect (hwnd, 0, 1);
        return 1;
      }

    case LVM_SETMOVERCHAR:
      {
        listview_item_data *data = get_listview_item_data (hwnd);
        switch (wparam)
          {
          case LVMC_UP:
            data->up_char = u_char (lparam);
            return 1;

          case LVMC_DOWN:
            data->down_char = u_char (lparam);
            return 1;

          case LVMC_DEF:
            data->def_char = u_char (lparam);
            return 1;

          case LVMC_PAGEUP:
            data->pageup_char = u_char (lparam);
            return 1;

          case LVMC_PAGEDOWN:
            data->pagedown_char = u_char (lparam);
            return 1;

          default:
            return 0;
          }
      }

    case WM_SETFOCUS:
    case WM_KILLFOCUS:
      CallWindowProc (ListViewProc, hwnd, msg, wparam, lparam);
      if (!wparam || GetParent (HWND (wparam)) != hwnd)
        repaint_selected_items (hwnd);
      return 0;
    }
  return CallWindowProc (ListViewProc, hwnd, msg, wparam, lparam);
}

int
init_listview_class ()
{
  HMODULE hcomctl32 = GetModuleHandle ("comctl32.dll");
  WNDCLASS wc;
  if (!GetClassInfo (hcomctl32, WC_LISTVIEWA, &wc))
    return 0;

  ListViewProc = wc.lpfnWndProc;

  wc.lpfnWndProc = ListViewExProc;
  wc.hInstance = hinstDLL;
  wc.lpszClassName = WC_LISTVIEWEX;

  return RegisterClass (&wc);
}
