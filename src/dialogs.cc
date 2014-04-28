#include "stdafx.h"
#include "ed.h"
#include "conf.h"
#include "privctrl.h"
#include "dialogs.h"
#include "ofn.h"
#include "thread.h"
#include "xstrlist.h"
#include "version.h"
#include "monitor.h"

void
set_window_icon (HWND hwnd)
{
  SendMessage (hwnd, WM_SETICON, 1,
               LPARAM (LoadIcon (app.hinst, MAKEINTRESOURCE (IDI_XYZZY))));
}

void
center_window (HWND hwnd)
{
  HWND owner = GetWindow (hwnd, GW_OWNER);
  if (!owner)
    owner = GetParent (hwnd);
  if (!owner)
    owner = app.toplev;

  RECT dr, or;
  GetWindowRect (hwnd, &dr);
  GetWindowRect (owner, &or);

  LONG left = (or.left + (or.right - or.left) / 3
               - (dr.right - dr.left) / 3);
  LONG top = (or.top + (or.bottom - or.top) / 3
              - (dr.bottom - dr.top) / 3);

  RECT work;
  monitor.get_workarea_from_window (owner, &work);

  left = min (max (left, work.left), work.right - (dr.right - dr.left));
  top = min (max (top, work.top), work.bottom - (dr.bottom - dr.top));

  SetWindowPos (hwnd, 0, left, top, 0, 0,
                SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
}

void
init_list_column (HWND list, int ncolumns, const int *width, const int *fmts,
                  int id_start, const char *entry, const char *key)
{
  int *v = (int *)alloca (sizeof *v * ncolumns);
  if (read_conf (entry, key, v, ncolumns))
    width = v;

  LV_COLUMN lvc;
  lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

  for (int i = 0; i < ncolumns; i++)
    {
      char buf[64];
      lvc.cx = width[i];
      LoadString (app.hinst, id_start + i, buf, sizeof buf);
      lvc.pszText = buf;
      lvc.iSubItem = i;
      lvc.fmt = fmts[i];
      ListView_InsertColumn (list, i, &lvc);
    }
}

void
save_list_column_width (HWND list, int ncolumns, const char *entry, const char *key)
{
  if (!IsWindow (list))
    return;

  int *v = (int *)alloca (sizeof *v * ncolumns);
  int good = 0;
  for (int i = 0; i < ncolumns; i++)
    {
      v[i] = ListView_GetColumnWidth (list, i);
      if (v[i] > 0)
        good = 1;
    }
  if (good)
    write_conf (entry, key, v, ncolumns);
  flush_conf ();
}

static void
set_list_char (HWND hwnd, int type, lisp lc)
{
  lc = xsymbol_value (lc);
  ListView_SetMoverChar (hwnd, type, (charp (lc) ? xchar_code (lc) : -1));
}

static void
set_list_chars (HWND hwnd)
{
  set_list_char (hwnd, LVMC_UP, Vstd_control_up_char);
  set_list_char (hwnd, LVMC_DOWN, Vstd_control_down_char);
  set_list_char (hwnd, LVMC_DEF, Vstd_control_default_char);
  set_list_char (hwnd, LVMC_PAGEUP, Vstd_control_prior_char);
  set_list_char (hwnd, LVMC_PAGEDOWN, Vstd_control_next_char);
}

static inline void
buffer_list_init_column (HWND list)
{
  static const int width[] = {117, 65, 100, 320};
  static const int fmts[] = {LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_LEFT, LVCFMT_LEFT};
  init_list_column (list, 4, width, fmts, IDS_SELECT_BUFFER1,
                    cfgBufferSelector, cfgColumn);

  HIMAGELIST hil = ImageList_LoadBitmap (app.hinst,
                                         MAKEINTRESOURCE (IDB_BUFSEL),
                                         17, 1, RGB (255, 255, 255));
  ListView_SetImageList (list, hil, LVSIL_SMALL);
}

static inline void
buffer_list_save_column (HWND list)
{
  save_list_column_width (list, 4, cfgBufferSelector, cfgColumn);
}

static int
store_buffer_name (HWND list, const Buffer *bp, LV_ITEM *lvi)
{
  int l = xstring_length (bp->lbuffer_name) * 2 + 32;
  char *b = (char *)alloca (l + 1);
  bp->buffer_name (b, b + l);
  lvi->pszText = b;
  return ListView_InsertItem (list, lvi);
}

static void
store_buffer_size (HWND list, const Buffer *bp, LV_ITEM *lvi)
{
  char b[32];
  sprintf (b, "%d", bp->b_nchars);
  lvi->pszText = b;
  ListView_SetItem (list, lvi);
}

static void
store_string (HWND list, lisp string, LV_ITEM *lvi)
{
  if (stringp (string))
    {
      char *b = (char *)alloca (xstring_length (string) * 2 + 1);
      w2s (b, string);
      lvi->pszText = b;
    }
  else
    lvi->pszText = "";
  ListView_SetItem (list, lvi);
}

static void
buffer_list_init_item (HWND list)
{
  int nbuffers = 0;
  for (const Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (!bp->internal_buffer_p ())
      nbuffers++;

  ListView_SetItemCount (list, nbuffers);

  int i = 0, cur = 0;
  for (const Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (!bp->internal_buffer_p ())
      {
        if (bp == selected_buffer ())
          cur = i;
        LV_ITEM lvi;
        lvi.mask = LVIF_TEXT | LVIF_PARAM | LVIF_IMAGE;
        lvi.iItem = i++;
        lvi.iSubItem = 0;
        lvi.lParam = LPARAM (bp);
        lvi.iImage = (bp->b_modified ? 1 : 0) + (bp->read_only_p () ? 2 : 0);
        lvi.iItem = store_buffer_name (list, bp, &lvi);
        lvi.mask = LVIF_TEXT;
        lvi.iSubItem = 1;
        store_buffer_size (list, bp, &lvi);
        lvi.iSubItem = 2;
        store_string (list, symbol_value (Vmode_name, bp), &lvi);
        lvi.iSubItem = 3;
        store_string (list, (stringp (bp->lfile_name)
                             ? bp->lfile_name : bp->lalternate_file_name),
                      &lvi);
      }

  ListView_SetItemState (list, cur, LVIS_SELECTED | LVIS_FOCUSED, UINT (-1));
  ListView_EnsureVisible (list, cur, 0);
}

static void
buffer_list_init (HWND list)
{
  ListView_SetExStyle (list, LVS_EXREPORTEX);
  ListView_SetPathEllipse (list, 3, 1);
  set_list_chars (list);
  buffer_list_init_column (list);
  buffer_list_init_item (list);
}

int
lv_find_selected_item (HWND list)
{
  return ListView_GetNextItem (list, -1, LVNI_SELECTED);
}

int
lv_find_focused_item (HWND list)
{
  return ListView_GetNextItem (list, -1, LVNI_FOCUSED);
}

static Buffer *
get_selected_item (HWND list)
{
  int i = lv_find_selected_item (list);
  if (i >= 0)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (list, &lvi))
        return (Buffer *)lvi.lParam;
    }
  return 0;
}


class select_buffer_comparator
{
private:
  enum buffer_list_sort_flags
    {
      SORT_NAME,
      SORT_SIZE,
      SORT_MODE,
      SORT_FILE,
      SORT_INDEX_MASK = 3,
      SORT_REV = 1 << 2,
    };

  // SORT FLAGS
  //
  //          0
  // 76543 2 10
  // nnnnn r tt
  // ----- | --
  //   \   \  \__ Sort Type (Name, Size, Mode, File)
  //    \   \____ Reverse Sort Flag
  //     \_______ Reserved
  int c_sort_flags;
  Buffer *c_selected_buffer;

public:
  static int CALLBACK compare_buffer (LPARAM p1, LPARAM p2, LPARAM param);
  static select_buffer_comparator *get_comparator (HWND dlg);

  select_buffer_comparator ()
       : c_selected_buffer (nullptr),
         c_sort_flags (SORT_NAME)
    {
      lisp save_flags = xsymbol_value (Vsave_buffer_list_sort_flags);
      lisp last_flags = xsymbol_value (Vlast_buffer_list_sort_flags);

      if (save_flags != Qnil && last_flags != Qnil)
        {
          // initialize by last settings
          if (fixnump (last_flags))
            c_sort_flags = fixnum_value (last_flags);
        }
      else
        {
          // initialize by default settings
          lisp v = xsymbol_value (Vbuffer_list_sort_type);
          if (fixnump (v))
            set_sort_type (fixnum_value (v));
          set_reverse (xsymbol_value (Vbuffer_list_sort_reverse) != Qnil);
        }
    }

  int sort_type () { return c_sort_flags & SORT_INDEX_MASK; }
  void set_sort_type (int type)
    {
      c_sort_flags = (c_sort_flags & ~SORT_INDEX_MASK) | (type & SORT_INDEX_MASK);
    }

  bool reverse () { return (c_sort_flags & SORT_REV) ? true : false; }
  void set_reverse (bool on)
    {
      if (on)
        c_sort_flags |= SORT_REV;
      else
        c_sort_flags &= ~SORT_REV;
    }

  int &sort_flags () { return c_sort_flags; }
  Buffer *&selected_buffer () { return c_selected_buffer; }

  int compare_buffer (LPARAM p1, LPARAM p2);
  void sort_items (HWND list);

private:
  void save_sort_item ();
};

int CALLBACK
select_buffer_comparator::compare_buffer (LPARAM p1, LPARAM p2, LPARAM param)
{
  select_buffer_comparator *comparator = reinterpret_cast <select_buffer_comparator *> (param);
  int d = comparator->compare_buffer (p1, p2);
  return comparator->reverse () ? -d : d;
}

select_buffer_comparator *
select_buffer_comparator::get_comparator (HWND dlg)
{
  return reinterpret_cast <select_buffer_comparator *> (GetWindowLong (dlg, DWL_USER));
}

int
select_buffer_comparator::compare_buffer (LPARAM p1, LPARAM p2)
{
  Buffer *b1 = (Buffer *)p1;
  Buffer *b2 = (Buffer *)p2;
  int param = sort_type ();

  if (xsymbol_value (Vsort_buffer_list_by_created_order) != Qnil)
    return b1->b_create_count - b2->b_create_count;
  for (int item = param, next = 0; item < 4; item = next++)
    {
      if (item == param && next)
        continue;
      lisp n1, n2;
      switch (item)
        {
        case 0:
          n1 = b1->lbuffer_name;
          n2 = b2->lbuffer_name;
          break;

        case 1:
          if (b1->b_nchars == b2->b_nchars)
            continue;
          return b1->b_nchars - b2->b_nchars;

        case 2:
          n1 = symbol_value (Vmode_name, b1);
          n2 = symbol_value (Vmode_name, b2);
          break;

        default:
          n1 = stringp (b1->lfile_name) ? b1->lfile_name : b1->lalternate_file_name;
          n2 = stringp (b2->lfile_name) ? b2->lfile_name : b2->lalternate_file_name;
          break;
        }
      if (!stringp (n1))
        {
          if (!stringp (n2))
            continue;
          return -1;
        }
      if (!stringp (n2))
        return 1;
      int d = (item == 2 || xsymbol_value (Vbuffer_list_sort_ignore_case) == Qnil
               ? bcmp (xstring_contents (n1), xstring_contents (n2),
                       min (xstring_length (n1), xstring_length (n2)))
               : memicmp (xstring_contents (n1), xstring_contents (n2),
                          sizeof (Char) * min (xstring_length (n1),
                                               xstring_length (n2))));
      if (!d)
        {
          d = xstring_length (n1) - xstring_length (n2);
          if (!d && !item)
            d = b1->b_version - b2->b_version;
        }
      if (d)
        return d;
    }
  return 0;
}

void
select_buffer_comparator::sort_items (HWND list)
{
  if (xsymbol_value (Vsort_buffer_list_by_created_order) == Qnil)
    {
      int direction = reverse () ? LVSM_UP : LVSM_DOWN;
      ListView_SetSortMark (list, sort_type (), direction);
    }

  ListView_SortItems (list, select_buffer_comparator::compare_buffer, this);
  int i = lv_find_selected_item (list);
  if (i >= 0)
    ListView_EnsureVisible (list, i, 0);

  save_sort_item ();
}

void
select_buffer_comparator::save_sort_item ()
{
  if (xsymbol_value (Vsave_buffer_list_sort_flags) == Qnil)
    xsymbol_value (Vlast_buffer_list_sort_flags) = Qnil;
  else
    xsymbol_value (Vlast_buffer_list_sort_flags) = make_fixnum (sort_flags ());
}

static void
offset_child_window (HWND dlg, UINT id, int dx, int dy)
{
  HWND hwnd = GetDlgItem (dlg, id);
  RECT r;
  GetWindowRect (hwnd, &r);
  MapWindowPoints (HWND_DESKTOP, dlg, (POINT *)&r, 2);
  OffsetRect (&r, dx, dy);
  SetWindowPos (hwnd, 0, r.left, r.top, r.right - r.left, r.bottom - r.top,
                SWP_NOACTIVATE | SWP_NOZORDER);
}

static void
resize_child_window (HWND dlg, UINT id, int dx, int dy)
{
  HWND hwnd = GetDlgItem (dlg, IDC_LIST);
  RECT r;
  GetWindowRect (hwnd, &r);
  SetWindowPos (hwnd, 0, 0, 0, r.right - r.left + dx, r.bottom - r.top + dy,
                SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
}

static BOOL CALLBACK
select_buffer_proc (HWND dlg, UINT msg, WPARAM wparam, LPARAM lparam)
{
  static const char cfgBufferSelector[] = "BufferSelector";
  static RECT init_rect;
  static RECT last_rect;
  select_buffer_comparator *comparator = nullptr;

  switch (msg)
    {
    case WM_INITDIALOG:
      GetWindowRect (dlg, &init_rect);
      last_rect = init_rect;
      if (!conf_load_geometry (dlg, cfgBufferSelector))
        center_window (dlg);
      set_window_icon (dlg);
      SetWindowLong (dlg, DWL_USER, lparam);
      buffer_list_init (GetDlgItem (dlg, IDC_LIST));
      comparator = reinterpret_cast <select_buffer_comparator *> (lparam);
      comparator->sort_items (GetDlgItem (dlg, IDC_LIST));
      ImmAssociateContext (GetDlgItem (dlg, IDC_LIST), 0);
      return 1;

    case WM_DESTROY:
      buffer_list_save_column (GetDlgItem (dlg, IDC_LIST));
      conf_save_geometry (dlg, cfgBufferSelector);
      return 1;

    case WM_NOTIFY:
      {
        NMHDR *nm = (NMHDR *)lparam;
        switch (nm->idFrom)
          {
          case IDC_LIST:
            switch (nm->code)
              {
              case LVN_COLUMNCLICK:
                if (xsymbol_value (Vsort_buffer_list_by_created_order) == Qnil)
                  {
                    comparator = select_buffer_comparator::get_comparator (dlg);
                    int sort_type = ((NM_LISTVIEW *)nm)->iSubItem;
                    if (comparator->sort_type () == sort_type)
                      {
                        comparator->set_reverse (!comparator->reverse ());
                      }
                    else
                      {
                        comparator->set_sort_type (sort_type);
                        comparator->set_reverse (false);
                      }
                    comparator->sort_items (nm->hwndFrom);
                  }
                return 1;

              case NM_DBLCLK:
                PostMessage (dlg, WM_COMMAND, IDOK, 0);
                return 1;
              }
            break;
          }
        return 0;
      }

    case WM_COMMAND:
      switch (LOWORD (wparam))
        {
        case IDOK:
          {
            Buffer *bp = get_selected_item (GetDlgItem (dlg, IDC_LIST));
            if (!bp)
              return 1;
            comparator = select_buffer_comparator::get_comparator (dlg);
            comparator->selected_buffer () = bp;
            EndDialog (dlg, IDOK);
            return 1;
          }

        case IDCANCEL:
          EndDialog (dlg, IDCANCEL);
          return 1;
        }
      return 0;

    case WM_PRIVATE_QUIT:
      EndDialog (dlg, IDCANCEL);
      return 1;

    case WM_ACTIVATEAPP:
      PostThreadMessage (app.quit_thread_id, WM_PRIVATE_ACTIVATEAPP,
                         wparam, lparam);
      return 0;

    case WM_GETMINMAXINFO:
      {
        MINMAXINFO *mmi = (MINMAXINFO *)lparam;
        mmi->ptMinTrackSize.x = init_rect.right - init_rect.left;
        mmi->ptMinTrackSize.y = init_rect.bottom - init_rect.top;
        return 1;
      }

    case WM_SIZE:
      {
        RECT r;
        GetWindowRect (dlg, &r);
        int dx = (r.right - r.left) - (last_rect.right - last_rect.left);
        int dy = (r.bottom - r.top) - (last_rect.bottom - last_rect.top);
        last_rect = r;
        offset_child_window (dlg, IDCANCEL, dx, dy);
        offset_child_window (dlg, IDOK, dx, dy);
        resize_child_window (dlg, IDC_LIST, dx, dy);
        return 1;
      }

    default:
      return 0;
    }
}

lisp
Fbuffer_selector ()
{
  select_buffer_comparator comparator;
  int r = DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_SELECT_BUFFER),
                          get_active_window (), select_buffer_proc, LPARAM (&comparator));
  Fdo_events ();
  if (r != IDOK)
    QUIT;
  return r == IDOK ? comparator.selected_buffer ()->lbp : Qnil;
}

static int
count_filter_size (lisp filters)
{
  int size = 0;
  for (; consp (filters); filters = xcdr (filters))
    {
      lisp f = xcar (filters);
      check_cons (f);
      lisp a = xcar (f), d = xcdr (f);
      check_string (a);
      check_string (d);
      size += w2sl (a) + w2sl (d) + 2;
    }
  if (size)
    size++;
  return size;
}

static void
make_filter_string (char *b, lisp filters)
{
  for (; consp (filters); filters = xcdr (filters))
    {
      lisp f = xcar (filters);
      lisp a = xcar (f), d = xcdr (f);
      b = w2s (b, a) + 1;
      b = w2s (b, d) + 1;
    }
  *b = 0;
}

static const UINT MSGFILEOK = RegisterWindowMessage (FILEOKSTRING);
static const UINT MSGLBSELCH = RegisterWindowMessage (LBSELCHSTRING);

struct OFN: public tagOFN
{
  HWND ofn_hwnd;
  int ofn_save;
  int ofn_encoding_req;
  lisp ofn_encoding;
  int ofn_eol_req;
  eol_code ofn_eol_code;
  int ofn_done;
  int ofn_ok_button;

  UINT wndproc (UINT, WPARAM, LPARAM);
  void init_dialog ();
  void on_size ();

  struct ids {int id; int code;};
  static const ids eol_list[];
  void init_eol_list ();
  void init_encoding_list ();
  void move_child (int, int, int);
  void get_result ();
  void *get_result (int, void *);
};

const OFN::ids OFN::eol_list[] =
{
  {IDS_EOL_LF, eol_lf},
  {IDS_EOL_CRLF, eol_crlf},
  {IDS_EOL_CR, eol_cr},
  {IDS_EOL_AUTO, eol_guess},
  {-1},
};

void
OFN::init_eol_list ()
{
  int index = 0;
  for (int i = 0; eol_list[i].id >= 0; i++)
    if (!ofn_save || eol_list[i].id != IDS_EOL_AUTO)
      {
        char b[64];
        LoadString (app.hinst, eol_list[i].id, b, sizeof b);
        int j = SendDlgItemMessage (ofn_hwnd, IDC_EOL_CODE, CB_ADDSTRING, 0, LPARAM (b));
        if (j != CB_ERR)
          {
            SendDlgItemMessage (ofn_hwnd, IDC_EOL_CODE, CB_SETITEMDATA, j, eol_list[i].code);
            if (eol_list[i].code == ofn_eol_code)
              index = j;
          }
      }
  SendDlgItemMessage (ofn_hwnd, IDC_EOL_CODE, CB_SETCURSEL, index, 0);
}

void
OFN::init_encoding_list ()
{
  int index = 0;
  for (lisp p = xsymbol_value (Vchar_encoding_list); consp (p); p = xcdr (p))
    {
      lisp encoding = xcar (p);
      if (char_encoding_p (encoding)
          && (!ofn_save || xchar_encoding_type (encoding) != encoding_auto_detect))
        {
          char b[256];
          w2s (b, b + sizeof b, xchar_encoding_display_name (encoding));
          int j = SendDlgItemMessage (ofn_hwnd, IDC_CHAR_ENCODING, CB_ADDSTRING, 0, LPARAM (b));
          if (j != CB_ERR)
            {
              SendDlgItemMessage (ofn_hwnd, IDC_CHAR_ENCODING, CB_SETITEMDATA,
                                  j, LPARAM (encoding));
              if (encoding == ofn_encoding)
                index = j;
            }
        }
    }
  SendDlgItemMessage (ofn_hwnd, IDC_CHAR_ENCODING, CB_SETCURSEL, index, 0);
}

void
OFN::init_dialog ()
{
  if (ofn_encoding_req)
    init_encoding_list ();
  else
    {
      if (Flags & OFN_EXPLORER)
        {
          EnableWindow (GetDlgItem (ofn_hwnd, IDC_STATIC_CHAR_ENCODING), 0);
          EnableWindow (GetDlgItem (ofn_hwnd, IDC_CHAR_ENCODING), 0);
        }
      else
        {
          ShowWindow (GetDlgItem (ofn_hwnd, IDC_STATIC_CHAR_ENCODING), SW_HIDE);
          ShowWindow (GetDlgItem (ofn_hwnd, IDC_CHAR_ENCODING), SW_HIDE);
        }
    }

  if (ofn_eol_req)
    init_eol_list ();
  else
    {
      if (Flags & OFN_EXPLORER)
        {
          EnableWindow (GetDlgItem (ofn_hwnd, IDC_STATIC_EOL_CODE), 0);
          EnableWindow (GetDlgItem (ofn_hwnd, IDC_EOL_CODE), 0);
        }
      else
        {
          ShowWindow (GetDlgItem (ofn_hwnd, IDC_STATIC_EOL_CODE), SW_HIDE);
          ShowWindow (GetDlgItem (ofn_hwnd, IDC_EOL_CODE), SW_HIDE);
        }
    }
}

void
OFN::move_child (int id, int x, int y)
{
  HWND hwnd = GetDlgItem (ofn_hwnd, id);
  RECT r;
  GetWindowRect (hwnd, &r);
  MoveWindow (hwnd, x, y, r.right - r.left, r.bottom - r.top, 1);
}

void
OFN::on_size ()
{
  if (Flags & OFN_EXPLORER)
    {
      HWND hwnd = GetParent (ofn_hwnd);

      if (ofn_encoding_req || ofn_eol_req)
        {
          RECT redt1, rcmb1, rstc3, rstc2;
          HWND h = GetDlgItem (hwnd, edt1);
          if (!h)
            h = GetDlgItem (hwnd, cmb13);
          GetWindowRect (h, &redt1);
          GetWindowRect (GetDlgItem (hwnd, cmb1), &rcmb1);
          GetWindowRect (GetDlgItem (hwnd, stc3), &rstc3);
          GetWindowRect (GetDlgItem (hwnd, stc2), &rstc2);
          MapWindowPoints (HWND_DESKTOP, ofn_hwnd, (POINT *)&redt1, 2);
          MapWindowPoints (HWND_DESKTOP, ofn_hwnd, (POINT *)&rcmb1, 2);
          MapWindowPoints (HWND_DESKTOP, ofn_hwnd, (POINT *)&rstc3, 2);
          MapWindowPoints (HWND_DESKTOP, ofn_hwnd, (POINT *)&rstc2, 2);

          int cmb_h = rcmb1.bottom - rcmb1.top;
          int cmb_dy = rcmb1.top - redt1.bottom;
          int stc_dy = rstc2.top - rcmb1.top;

          if (!(Flags & OFN_HIDEREADONLY))
            {
              GetWindowRect (GetDlgItem (hwnd, chx1), &rcmb1);
              MapWindowPoints (HWND_DESKTOP, ofn_hwnd, (POINT *)&rcmb1, 2);
            }

          move_child (IDC_CHAR_ENCODING, rcmb1.left, rcmb1.bottom + cmb_dy);
          move_child (IDC_STATIC_CHAR_ENCODING, rstc2.left,
                      rcmb1.bottom + cmb_dy + stc_dy);
          move_child (IDC_EOL_CODE, rcmb1.left, rcmb1.bottom + cmb_h + cmb_dy * 2);
          move_child (IDC_STATIC_EOL_CODE, rstc2.left, rcmb1.bottom + cmb_h + cmb_dy * 2 + stc_dy);
        }
    }
}

void *
OFN::get_result (int id, void *defalt)
{
  HWND hwnd = GetDlgItem (ofn_hwnd, id);
  int n = SendMessage (hwnd, CB_GETCURSEL, 0, 0);
  if (n == CB_ERR)
    return defalt;
  return (void *)SendMessage (hwnd, CB_GETITEMDATA, n, 0);
}

void
OFN::get_result ()
{
  if (ofn_encoding_req)
    {
      lisp x = (lisp)get_result (IDC_CHAR_ENCODING, ofn_encoding);
      for (lisp p = xsymbol_value (Vchar_encoding_list); consp (p); p = xcdr (p))
        if (x == xcar (p))
          {
            ofn_encoding = x;
            break;
          }
    }

  if (ofn_eol_req)
    ofn_eol_code = (eol_code)(int)get_result (IDC_EOL_CODE, (void *)ofn_eol_code);
}

UINT
OFN::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      init_dialog ();
      return 1;

    case WM_SIZE:
      if (lparam)
        {
          if (!ofn_done)
            {
              HWND hwnd;
              if (Flags & OFN_EXPLORER)
                {
                  hwnd = GetParent (ofn_hwnd);
                  if (ofn_ok_button)
                    CommDlg_OpenSave_SetControlText (hwnd, IDOK, "OK");
                }
              else
                hwnd = ofn_hwnd;
              center_window (hwnd);
              set_window_icon (hwnd);
              ofn_done = 1;
            }
          PostMessage (ofn_hwnd, WM_PRIVATE_SIZE, 0, 0);
        }
      return 0;

    case WM_PRIVATE_SIZE:
      on_size ();
      return 0;

    case WM_DESTROY:
      get_result ();
      return 0;

    default:
      return 0;
    }
}

static UINT CALLBACK
file_name_dialog_hook (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  OFN *ofn;
  if (msg == WM_INITDIALOG)
    {
      lparam = ((OPENFILENAME *)lparam)->lCustData;
      SetWindowLong (hwnd, DWL_USER, lparam);
      ofn = (OFN *)lparam;
      ofn->ofn_hwnd = hwnd;
    }
  else
    {
      ofn = (OFN *)GetWindowLong (hwnd, DWL_USER);
      if (!ofn)
        return 0;
    }
  return ofn->wndproc (msg, wparam, lparam);
}

lisp
Ffile_name_dialog (lisp keys)
{
  lisp lfilters = find_keyword (Kfilter, keys);
  int filter_size = count_filter_size (lfilters);
  int filter_index = find_keyword_int (Kfilter_index, keys, 1);
  lisp ltitle = find_keyword (Ktitle, keys);
  if (ltitle != Qnil)
    check_string (ltitle);
  lisp ldefault = find_keyword (Kdefault, keys);
  if (ldefault != Qnil)
    ldefault = Fnamestring (ldefault);
  lisp lext = find_keyword (Kextension, keys);
  if (lext != Qnil)
    check_string (lext);
  int save = find_keyword_bool (Ksave, keys);
  int multiple = find_keyword_bool (Kmultiple, keys);
  int must_exist = find_keyword_bool (Kmust_exist, keys);
  int explorer = sysdep.Win4p () && find_keyword_bool (Kexplorer, keys, 1);
  int hide_read_only = find_keyword_bool (Khide_read_only, keys, 1);
  int overwrite = find_keyword_bool (Koverwrite, keys);
  int read_only = find_keyword_bool (Kread_only, keys);
  lisp char_encoding = find_keyword (Kchar_encoding, keys);
  lisp leol_code = find_keyword (Keol_code, keys, 0);
  if (!leol_code)
    leol_code = find_keyword (Knewline_code, keys);

  char dir[PATH_MAX + 1];
  *dir = 0;
  lisp ldir = find_keyword (Kinitial_directory, keys);
  if (ldir != Qnil)
    {
      pathname2cstr (ldir, dir);
      DWORD a = WINFS::GetFileAttributes (dir);
      if (a == DWORD (-1) || !(a & FILE_ATTRIBUTE_DIRECTORY))
        *dir = 0;
    }

  if (!*dir)
    pathname2cstr (selected_buffer ()->ldirectory, dir);
  map_sl_to_backsl (dir);

  OFN ofn;
  bzero (&ofn, sizeof ofn);

  ofn.lStructSize = (sysdep.Win5p ()
                     ? OPENFILENAME_SIZE_VERSION_500
                     : OPENFILENAME_SIZE_VERSION_400);
  ofn.hwndOwner = get_active_window ();
  ofn.hInstance = app.hinst;
  ofn.lCustData = DWORD (&ofn);

  char buf[1024 * 32];
  if (stringp (ldefault) && xstring_length (ldefault) < sizeof buf / 2 - 1)
    {
      w2s (buf, ldefault);
      map_sl_to_backsl (buf);
    }
  else if (!filter_size)
    strcpy (buf, "*.*");
  else
    *buf = 0;
  ofn.lpstrFile = buf;
  ofn.nMaxFile = sizeof buf;

  ofn.Flags = (OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST | OFN_LONGNAMES
               | OFN_ENABLEHOOK | OFN_ENABLESIZING | OFN_SHAREAWARE);
  ofn.lpfnHook = file_name_dialog_hook;
  if (multiple)
    ofn.Flags |= OFN_ALLOWMULTISELECT;
  if (must_exist)
    ofn.Flags |= OFN_FILEMUSTEXIST;
  if (explorer)
    ofn.Flags |= OFN_EXPLORER;
  if (hide_read_only)
    ofn.Flags |= OFN_HIDEREADONLY;
  if (overwrite)
    ofn.Flags |= OFN_OVERWRITEPROMPT;
  if (read_only)
    ofn.Flags |= OFN_READONLY;

  if (sysdep.Win5p ())
    ofn.Flags |= OFN_DONTADDTORECENT | OFN_FORCESHOWHIDDEN;

  char *filter = 0;
  if (filter_size)
    {
      filter = (char *)alloca (filter_size);
      make_filter_string (filter, lfilters);
    }
  ofn.lpstrFilter = filter;
  ofn.nFilterIndex = filter_index;

  ofn.lpstrInitialDir = dir;
  int l = strlen (dir);
  if (!memicmp (dir, buf, l))
    {
      if (buf[l] == '\\')
        l++;
      strcpy (buf, buf + l);
    }

  char *title = 0;
  if (stringp (ltitle))
    {
      ofn.ofn_ok_button = 1;
      title = (char *)alloca (xstring_length (ltitle) * 2 + 1);
      w2s (title, ltitle);
    }
  ofn.lpstrTitle = title;

  char *ext = 0;
  if (stringp (lext))
    {
      ext = (char *)alloca (xstring_length (lext) * 2 + 1);
      w2s (ext, lext);
    }
  ofn.lpstrDefExt = ext;

  ofn.ofn_save = save;

  if (leol_code == Qnil)
    {
      ofn.ofn_eol_code = eol_code (-1);
      ofn.ofn_eol_req = 0;
    }
  else
    {
      long n;
      if (!safe_fixnum_value (leol_code, &n)
          || !valid_eol_code_p (n))
        n = eol_guess;
      if (save && n == eol_guess)
        n = eol_crlf;
      ofn.ofn_eol_code = eol_code (n);
      ofn.ofn_eol_req = 1;
    }

  if (char_encoding == Qnil)
    {
      ofn.ofn_encoding = Qnil;
      ofn.ofn_encoding_req = 0;
    }
  else
    {
      if (!char_encoding_p (char_encoding))
        char_encoding = xsymbol_value (save ? Qencoding_sjis : Qencoding_auto);
      ofn.ofn_encoding = char_encoding;
      ofn.ofn_encoding_req = 1;
    }

  if (!explorer)
    {
      ofn.Flags |= OFN_ENABLETEMPLATE;
      ofn.lpTemplateName = (multiple
                            ? MAKEINTRESOURCE (MULTIFILEOPENORD)
                            : MAKEINTRESOURCE (FILEOPENORD));
      if (!ofn.lpstrTitle && save)
        {
          title = (char *)alloca (256);
          LoadString (app.hinst, IDS_SAVE_AS, title, 256);
          ofn.lpstrTitle = title;
        }
    }
  else if (ofn.ofn_eol_req || ofn.ofn_encoding_req)
    {
      ofn.Flags |= OFN_ENABLETEMPLATE;
      ofn.lpTemplateName = MAKEINTRESOURCE (IDD_CUST_EXPLORER);
    }

  if (save ? !GetSaveFileName (&ofn) : !GetOpenFileName (&ofn))
    return Qnil;

  multiple_value::count () = 4;
  multiple_value::value (1) = make_fixnum (ofn.nFilterIndex);
  multiple_value::value (2) = ofn.ofn_encoding_req ? ofn.ofn_encoding : Qnil;
  multiple_value::value (3) = ((ofn.ofn_eol_req
                                && valid_eol_code_p (ofn.ofn_eol_code)
                                && ofn.ofn_eol_code != eol_guess)
                               ? make_fixnum (ofn.ofn_eol_code)
                               : Qnil);

  if (!multiple)
    {
      map_backsl_to_sl (buf);
      return make_string (buf);
    }

  lisp result = Qnil;
  if (ofn.Flags & OFN_EXPLORER)
    {
      char *b = buf + strlen (buf);
      map_backsl_to_sl (buf);
      if (!b[1])
        return xcons (make_string (buf), Qnil);
      char *e = b++;
      if (jrindex (buf, '/') != e - 1)
        *e++ = '/';
      while (*b)
        {
          char *p = stpcpy (e, b);
          b += p - e + 1;
          result = xcons (make_string (buf), result);
        }
    }
  else
    {
      char *b = jindex (buf, ' ');
      if (!b)
        {
          map_backsl_to_sl (buf);
          return xcons (make_string (buf), Qnil);
        }
      char *e = b++;
      *e = 0;
      if (jrindex (buf, '/') != e - 1)
        *e++ = '/';
      while (1)
        {
          char *b2 = jindex (b, ' ');
          if (b2)
            *b2 = 0;
          strcpy (e, b);
          char path[PATH_MAX], *name;
          if (WINFS::GetFullPathName (buf, sizeof path, path, &name))
            strcpy (e, name);
          map_backsl_to_sl (buf);
          result = xcons (make_string (buf), result);
          if (!b2)
            break;
          b = b2 + 1;
        }
    }

  return result;
}

struct ODN: public tagOFNA
{
  char odn_result[PATH_MAX + 1];
  void store_dirname (HWND);
  void selch (HWND, int);
  int ok (HWND);
  static int error (HWND, int);
};

void
ODN::store_dirname (HWND hwnd)
{
  SetDlgItemText (hwnd, IDC_PATH, odn_result);
}

void
ODN::selch (HWND hwnd, int id)
{
  char path[PATH_MAX];
  GetCurrentDirectory (sizeof path, path);
  if (!strcmp (path, odn_result))
    {
      if (id == lst2)
        PostMessage (hwnd, WM_COMMAND, IDOK, 0);
    }
  else
    {
      strcpy (odn_result, path);
      store_dirname (hwnd);
    }
}

int
ODN::error (HWND hwnd, int e)
{
  char buf[1024];
  FormatMessage ((FORMAT_MESSAGE_FROM_SYSTEM
                  | FORMAT_MESSAGE_IGNORE_INSERTS
                  | FORMAT_MESSAGE_MAX_WIDTH_MASK),
                 0, e, GetUserDefaultLangID (),
                 buf, sizeof buf, 0);
  MsgBox (hwnd, buf, TitleBarString, MB_OK | MB_ICONEXCLAMATION,
          xsymbol_value (Vbeep_on_error) != Qnil);
  return 1;
}

int
ODN::ok (HWND hwnd)
{
  char path[PATH_MAX];
  GetDlgItemText (hwnd, IDC_PATH, path, sizeof path);
  if (!*path)
    return 1;
  DWORD atr = WINFS::GetFileAttributes (path);
  if (atr == -1)
    return error (hwnd, GetLastError ());
  if (!(atr & FILE_ATTRIBUTE_DIRECTORY))
    return error (hwnd, ERROR_DIRECTORY);

  HWND drive = GetDlgItem (hwnd, cmb2);
  int l = strlen (path);
  strcpy (path + l, " ");
  int i = SendMessage (drive, CB_FINDSTRING, WPARAM (-1), LPARAM (path));
  path[l] = 0;
  if (i != CB_ERR)
    {
      if (SendMessage (drive, CB_GETCURSEL, 0, 0) == i)
        return 1;
      SendMessage (drive, CB_SETCURSEL, i, 0);
      PostMessage (hwnd, WM_COMMAND, MAKEWPARAM (cmb2, CBN_SELCHANGE),
                   LPARAM (drive));
      return 1;
    }

  char *tem;
  WINFS::GetFullPathName (path, sizeof odn_result, odn_result, &tem);
  return 0;
}

static UINT CALLBACK
directory_name_dialog_hook (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  if (msg == WM_INITDIALOG)
    {
      lparam = ((OPENFILENAME *)lparam)->lCustData;
      SetWindowLong (hwnd, DWL_USER, lparam);
      ((ODN *)lparam)->store_dirname (hwnd);
      center_window (hwnd);
      set_window_icon (hwnd);
      return 1;
    }

  ODN *odn = (ODN *)GetWindowLong (hwnd, DWL_USER);
  if (!odn)
    return 0;

  if (msg == MSGFILEOK)
    return odn->ok (hwnd);

  if (msg == MSGLBSELCH
      && HIWORD (lparam) == CD_LBSELCHANGE
      && (wparam == lst2 || wparam == cmb2))
    {
      odn->selch (hwnd, wparam);
      return 1;
    }
  return 0;
}

lisp
Fdirectory_name_dialog (lisp keys)
{
  lisp ltitle = find_keyword (Ktitle, keys);
  if (ltitle != Qnil)
    check_string (ltitle);

  lisp ldefault = find_keyword (Kdefault, keys);
  if (ldefault != Qnil)
    ldefault = Fnamestring (ldefault);
  else
    ldefault = selected_buffer ()->ldirectory;

  ODN odn;
  bzero (&odn, sizeof odn);

  odn.lStructSize = OPENFILENAME_SIZE_VERSION_400;
  odn.hwndOwner = get_active_window ();
  odn.hInstance = app.hinst;

  char buf[PATH_MAX];
  strcpy (buf, "FOO");
  odn.lpstrFile = buf;
  odn.nMaxFile = sizeof buf;

  odn.Flags = (OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST | OFN_LONGNAMES
               | OFN_ENABLEHOOK | OFN_HIDEREADONLY | OFN_ENABLETEMPLATE
               | OFN_NOVALIDATE);
  if (sysdep.Win5p ())
    odn.Flags |= OFN_DONTADDTORECENT | OFN_FORCESHOWHIDDEN;
  odn.lpfnHook = directory_name_dialog_hook;
  odn.lpTemplateName = MAKEINTRESOURCE (IDD_DIRECTORY);

  if (xstring_length (ldefault) < sizeof odn.odn_result / 2 - 1)
    {
      w2s (odn.odn_result, ldefault);
      map_sl_to_backsl (odn.odn_result);
      odn.lpstrInitialDir = odn.odn_result;
    }
  else
    strcpy (odn.odn_result, sysdep.curdir);

  char *title = 0;
  if (stringp (ltitle))
    {
      title = (char *)alloca (xstring_length (ltitle) * 2 + 1);
      w2s (title, ltitle);
    }
  odn.lpstrTitle = title;
  odn.lCustData = DWORD (&odn);

  if (!GetOpenFileName (&odn))
    return Qnil;

  map_backsl_to_sl (odn.odn_result);
  return make_string (odn.odn_result);
}

BOOL CALLBACK
IdleDialog::WndProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  IdleDialog *d;
  if (msg == WM_INITDIALOG)
    {
      d = (IdleDialog *)lparam;
      d->id_hwnd = hwnd;
      SetWindowLong (hwnd, DWL_USER, lparam);
    }
  else
    {
      d = (IdleDialog *)GetWindowLong (hwnd, DWL_USER);
      if (!d)
        return 0;
      if (msg == WM_NCDESTROY)
        {
          d->WndProc (msg, wparam, lparam);
          if (d->id_end == IE_ALIVE)
            d->id_end = IE_DEAD;
          if (d->id_auto_delete)
            delete d;
          else
            d->id_hwnd = 0;
          return 1;
        }
    }
  d->id_wndproc_depth++;
  BOOL f = d->WndProc (msg, wparam, lparam);
  d->id_wndproc_depth--;
  if (!d->id_wndproc_depth
      && d->id_end == IE_DEAD
      && d->id_modeless && IsWindow (d->id_hwnd))
    {
      d->id_end = IE_DELETE;
      DestroyWindow (d->id_hwnd);
    }
  return f;
}

void
IdleDialog::EndDialog (int r)
{
  id_result = r;
  id_end = IE_DEAD;
}

void
IdleDialog::process ()
{
  while (id_end == IE_ALIVE)
    {
      MSG msg;
      while (id_idle && id_end == IE_ALIVE)
        {
          while (id_end == IE_ALIVE && PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
            if (!IsDialogMessage (&msg))
              {
                XyzzyTranslateMessage (&msg);
                DispatchMessage (&msg);
              }
          IdleProc ();
        }
      while (id_end == IE_ALIVE && !id_idle && GetMessage (&msg, 0, 0, 0))
        if (!IsDialogMessage (&msg))
          {
            XyzzyTranslateMessage (&msg);
            DispatchMessage (&msg);
          }
    }
}

int
IdleDialog::DoModal (HWND owner, UINT id)
{
  id_modeless = 0;
  int enable_owner = 0;
  if (owner && IsWindowEnabled (owner))
    {
      EnableWindow (owner, 0);
      enable_owner = 1;
    }

  id_hwnd = CreateDialogParam (app.hinst, MAKEINTRESOURCE (id), owner,
                               WndProc, LPARAM (this));
  if (id_hwnd)
    {
      ShowWindow (id_hwnd, SW_SHOW);
      process ();
      SetWindowPos (id_hwnd, 0, 0, 0, 0, 0,
                    (SWP_HIDEWINDOW | SWP_NOSIZE | SWP_NOMOVE
                     | SWP_NOACTIVATE | SWP_NOZORDER));
    }

  if (enable_owner)
    EnableWindow (owner, 1);
  if (owner && GetActiveWindow () == id_hwnd)
    SetActiveWindow (owner);

  if (id_hwnd)
    DestroyWindow (id_hwnd);
  return id_result;
}

int
IdleDialog::Create (HWND owner, UINT id)
{
  id_modeless = 1;
  id_hwnd = CreateDialogParam (app.hinst, MAKEINTRESOURCE (id), owner,
                               WndProc, LPARAM (this));
  if (!id_hwnd)
    return 0;
  ShowWindow (id_hwnd, SW_SHOW);
  return 1;
}

IdleDialog::~IdleDialog ()
{
  if (IsWindow (id_hwnd))
    {
      DestroyWindow (id_hwnd);
      id_hwnd = 0;
    }
  id_end = IE_DEAD;
}

class list_volume_name: public worker_thread
{
protected:
  virtual void thread_main ();
private:
  xstring_list m_list;
  HWND m_hwnd;
  DWORD m_drives;
public:
  list_volume_name (HWND hwnd, DWORD drives)
       : m_hwnd (hwnd), m_drives (drives) {}
  ~list_volume_name () {}
  void interrupt ()
    {
      m_hwnd = 0;
      worker_thread::interrupt ();
    }
  const xstring_list &list () const {return m_list;}
};

void
list_volume_name::thread_main ()
{
  try
    {
      for (int c = 'a'; c <= 'z' && m_hwnd; c++)
        if (m_drives & (1 << (c - 'a')))
          {
            char name[5];
            sprintf (name, "%c:\\", c);
            int type = GetDriveType (name);
            char volname[1024];
            if (type != DRIVE_REMOVABLE
                && GetVolumeInformation (name, volname + 1, sizeof volname - 1,
                                         0, 0, 0, 0, 0)
                && volname[1])
              {
                *volname = c;
                m_list.add (volname);
              }
          }
      if (m_hwnd)
        PostMessage (m_hwnd, WM_PRIVATE_END_LIST_DRIVE, 0, 0);
    }
  catch (nonlocal_jump &)
    {
    }
}


class DriveDialog: public IdleDialog
{
  Char dd_defalt;
  DWORD dd_drives;
  LONG dd_maxw;
  worker_thread_helper <list_volume_name> dd_thread;
  int dd_indexes['z' - 'a' + 1];

  void setup_list (HWND);
  void insert_drives (HWND);
  void insert_volnames ();
  void init_dialog ();

  virtual BOOL WndProc (UINT, WPARAM, LPARAM);
  void result (int);
  virtual void IdleProc () {set_idle (0);}
public:
  int DoModal (HWND owner)
    {return IdleDialog::DoModal (owner, IDD_SELECT_DRIVE);}
  DriveDialog (Char d)
       : dd_defalt (d), dd_drives (0), dd_maxw (0) {}
  Char get_result () const {return dd_defalt;}
};

void
DriveDialog::setup_list (HWND hwnd)
{
  ImmAssociateContext (hwnd, 0);

  ListView_SetExStyle (hwnd, LVS_EXREPORTEX);
  set_list_chars (hwnd);

  LV_COLUMN lvc;
  lvc.mask = LVCF_FMT | LVCF_SUBITEM;
  for (int i = 0; i < 2; i++)
    {
      lvc.iSubItem = i;
      lvc.fmt = LVCFMT_LEFT;
      ListView_InsertColumn (hwnd, i, &lvc);
    }

  HIMAGELIST hil = ImageList_LoadBitmap (app.hinst,
                                         MAKEINTRESOURCE (IDB_FILESEL),
                                         16, 1, RGB (0, 0, 255));
  ListView_SetImageList (hwnd, hil, LVSIL_SMALL);
}

void
DriveDialog::insert_drives (HWND hwnd)
{
  HDC hdc = GetDC (hwnd);
  HGDIOBJ of = SelectObject (hdc, HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0)));

  int cur = 0, item = 0;
  for (int c = 'a'; c <= 'z'; c++)
    {
      int i = c - 'a';
      if (dd_drives & (1 << i))
        {
          dd_indexes[i] = item;
          if (c == dd_defalt)
            cur = item;

          char name[5];
          sprintf (name, "%c:\\", c);
          int type = GetDriveType (name);

          LV_ITEM lvi;
          lvi.mask = LVIF_TEXT | LVIF_PARAM | LVIF_IMAGE;
          lvi.iItem = item++;
          lvi.iSubItem = 0;
          lvi.lParam = c;
          if (type >= DRIVE_REMOVABLE && type <= DRIVE_RAMDISK)
            lvi.iImage = type + 4 - DRIVE_REMOVABLE;
          else
            lvi.mask &= ~LVIF_IMAGE;

          name[2] = 0;
          lvi.pszText = name;
          ListView_InsertItem (hwnd, &lvi);

          SIZE sz;
          GetTextExtentPoint32 (hdc, name, 2, &sz);
          dd_maxw = max (dd_maxw, sz.cx);
        }
      else
        dd_indexes[i] = -1;
    }

  SelectObject (hdc, of);
  ReleaseDC (hwnd, hdc);

  ListView_SetColumnWidth (hwnd, 0, dd_maxw * 2 + 20);

  ListView_SetItemState (hwnd, cur, LVIS_SELECTED | LVIS_FOCUSED, UINT (-1));
  ListView_EnsureVisible (hwnd, cur, 0);
}

void
DriveDialog::insert_volnames ()
{
  HWND hwnd = GetDlgItem (id_hwnd, IDC_LIST);
  HDC hdc = GetDC (hwnd);
  HGDIOBJ of = SelectObject (hdc, HFONT (SendMessage (hwnd, WM_GETFONT, 0, 0)));

  LONG maxw = 0;
  for (const xstring_node *p = dd_thread->list ().head (); p; p = p->next ())
    {
      const char *volname = *p;
      if (lower_char_p (*volname & 255))
        {
          int i = dd_indexes[*volname - 'a'];
          if (i >= 0)
            {
              volname++;
              ListView_SetItemText (hwnd, i, 1, (char *)volname);
              SIZE sz;
              GetTextExtentPoint32 (hdc, volname, strlen (volname), &sz);
              maxw = max (maxw, sz.cx);
            }
        }
    }

  if (maxw)
    ListView_SetColumnWidth (hwnd, 1, maxw + dd_maxw + 20);

  SelectObject (hdc, of);
  ReleaseDC (hwnd, hdc);
}

void
DriveDialog::init_dialog ()
{
  center_window (id_hwnd);
  set_window_icon (id_hwnd);

  if (dd_defalt > 0)
    dd_defalt = alpha_char_p (dd_defalt) ? char_downcase (dd_defalt) : Char (-1);
  dd_drives = GetLogicalDrives ();

  HWND hwnd = GetDlgItem (id_hwnd, IDC_LIST);
  setup_list (hwnd);
  insert_drives (hwnd);

  try
    {
      dd_thread.attach (new list_volume_name (id_hwnd, dd_drives));
      dd_thread->start ();
    }
  catch (nonlocal_jump &)
    {
    }
}

void
DriveDialog::result (int drive)
{
  if (dd_drives & (1 << (drive - 'a')))
    {
      dd_defalt = char_upcase (Char (drive));
      EndDialog (IDOK);
    }
}

BOOL
DriveDialog::WndProc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      init_dialog ();
      return 1;

    case WM_NOTIFY:
      {
        NMHDR *nm = (NMHDR *)lparam;
        switch (nm->idFrom)
          {
          case IDC_LIST:
            switch (nm->code)
              {
              case NM_DBLCLK:
                PostMessage (id_hwnd, WM_COMMAND, IDOK, 0);
                return 1;

              case LVN_KEYDOWN:
                if (GetKeyState (VK_CONTROL) >= 0
                    && GetKeyState (VK_SHIFT) >= 0
                    && GetKeyState (VK_MENU) >= 0)
                  {
                    int c = ((LV_KEYDOWN *)nm)->wVKey;
                    if (c >= 'A' && c <= 'Z')
                      result (char_downcase (c));
                    return 1;
                  }
                break;
              }
            break;
          }
        return 0;
      }

    case WM_COMMAND:
      switch (LOWORD (wparam))
        {
        case IDOK:
          {
            HWND list = GetDlgItem (id_hwnd, IDC_LIST);
            int i = lv_find_selected_item (list);
            if (i == -1)
              return 1;
            LV_ITEM lvi;
            lvi.iItem = i;
            lvi.iSubItem = 0;
            lvi.mask = LVIF_PARAM;
            if (ListView_GetItem (list, &lvi))
              result (lvi.lParam);
            return 1;
          }

        case IDCANCEL:
          EndDialog (IDCANCEL);
          return 1;
        }
      return 0;

    case WM_PRIVATE_END_LIST_DRIVE:
      if (dd_thread)
        insert_volnames ();
      return 1;

    case WM_PRIVATE_QUIT:
      EndDialog (IDCANCEL);
      return 1;

    case WM_ACTIVATEAPP:
      PostThreadMessage (app.quit_thread_id, WM_PRIVATE_ACTIVATEAPP,
                         wparam, lparam);
      return 0;

    case WM_DESTROY:
      if (dd_thread)
        dd_thread->interrupt ();
      return 1;

    default:
      return 0;
    }
}

lisp
Fdrive_dialog (lisp drive)
{
  Char d = Char (-1);
  if (drive && drive != Qnil)
    {
      check_char (drive);
      d = xchar_code (drive);
    }
  DriveDialog dlg (d);
  int r = dlg.DoModal (get_active_window ());
  Fdo_events ();
  if (r != IDOK)
    QUIT;
  return r == IDOK ? make_char (dlg.get_result ()) : Qnil;
}
