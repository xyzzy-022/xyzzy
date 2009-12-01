#include "ed.h"
#include "environ.h"
#include "conf.h"
#include "filer.h"
#include "colors.h"
#include <shlobj.h>
#include "ctxmenu.h"
#include "com.h"
#include <process.h>

#ifndef SHGFI_OVERLAYINDEX
#define SHGFI_OVERLAYINDEX 0x000000040
#endif

///////////////////////////////////////////////////////////////
// FilerView
///////////////////////////////////////////////////////////////

void
FilerView::cleanup_chunk ()
{
  for (find_chunk *p = fv_chunk, *next; p; p = next)
    {
      next = p->fc_cdr;
      delete p;
    }
  fv_chunk = 0;
}

FilerView::FilerView (lisp dir, lisp last_path)
     : fv_chunk (0), fv_hwnd (0), fv_hwnd_mask (0), fv_hwnd_path (0),
       fv_hwnd_marks (0), fv_gcpro (fv_lobjs, numberof (fv_lobjs)),
       fv_subscribed (0), fv_marks_changed (0), fv_sort (0), fv_parent (0),
       fv_hevent (0), fv_hthread (0),
       fv_stop_thread (0), fv_sequence (0), fv_icon_path (0)
{
  fv_ldir = dir;
  fv_llastdir = last_path;
  fv_lmask = Qnil;
  fv_lfile_mask = Qnil;
  fv_retrieve_icon =
    sysdep.Win4p () && xsymbol_value (Vfiler_retrieve_icon) != Qnil;

  if (fv_retrieve_icon)
    {
      fv_hevent = CreateEvent (0, 0, 0, 0);
      if (fv_hevent)
        {
          unsigned id;
          fv_hthread = (HANDLE)_beginthreadex (0, 0, thread_entry, this, 0, &id);
        }
    }
}

FilerView::~FilerView ()
{
  if (fv_hthread)
    {
      fv_stop_thread = 1;
      SetEvent (fv_hevent);
      WaitForSingleObject (fv_hthread, INFINITE);
      CloseHandle (fv_hthread);
    }
  cleanup_chunk ();
  if (fv_parent)
    write_conf (cfgFiler,
                (fv_parent->dual_window_p ()
                 ? fv_parent->left_window_p (this) ? cfgSortLeft : cfgSortRight
                 : cfgSort),
                fv_sort);
  if (fv_icon_path)
    free (fv_icon_path);
  if (fv_hevent)
    CloseHandle (fv_hevent);
}

void
FilerView::set_colors () const
{
  ListView_SetTextColor (fv_hwnd, get_misc_color (MC_FILER_FG));
  ListView_SetTextBkColor (fv_hwnd, get_misc_color (MC_FILER_BG));
  ListView_SetBkColor (fv_hwnd, get_misc_color (MC_FILER_BG));
  ListView_SetCursorColor (fv_hwnd, get_misc_color (MC_FILER_CURSOR));
  ListView_SetHighlightTextColor (fv_hwnd, get_misc_color (MC_FILER_HIGHLIGHT_FG));
  ListView_SetHighlightColor (fv_hwnd, get_misc_color (MC_FILER_HIGHLIGHT_BG));
  InvalidateRect (fv_hwnd, 0, 1);
}

int
FilerView::chdir (lisp dir)
{
  char *b = (char *)alloca (xstring_length (dir) * 2 + 1);
  w2s (b, dir);
  return WINFS::SetCurrentDirectory (b);
}

int
FilerView::chdevdir (lisp dir)
{
  char *b = (char *)alloca (xstring_length (dir) * 2 + 1);
  w2s (b, dir);
  return set_device_dir (b, 0);
}

lisp
FilerView::filename (const filer_data *d) const
{
  const char *name = *d->name ? d->name : "..";
  int l = xstring_length (fv_ldir);
  int sl = (d->attr & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
  lisp string = make_string (sl + l + s2wl (name));
  bcopy (xstring_contents (fv_ldir), xstring_contents (string), l);
  Char *b = s2w (&xstring_contents (string) [l], name);
  if (sl)
    *b = '/';
  return string;
}

int
FilerView::load_contents (const char *mask)
{
  if (!chdir (fv_ldir))
    file_error (GetLastError (), fv_ldir);

  WIN32_FIND_DATA fd;
  HANDLE h = WINFS::FindFirstFile ("*", &fd);
  int error = GetLastError ();
  fv_parent->restore_dir ();
  if (h == INVALID_HANDLE_VALUE && error != ERROR_FILE_NOT_FOUND)
    return 0;

  lisp ignores = symbol_value (Vignored_extensions, selected_buffer ());

  interrupt_thread ();
  cleanup_chunk ();

  int dotdot = 0;
  try
    {
      if (h != INVALID_HANDLE_VALUE)
        {
          do
            {
#ifndef PATHNAME_ESCAPE_TILDE
              if (*fd.cFileName == '~' && !fd.cFileName[1])
                continue;
#endif
              if (*fd.cFileName == '.')
                {
                  if (!fd.cFileName[1])
                    continue;
                  if (fd.cFileName[1] == '.' && !fd.cFileName[2])
                    {
                      dotdot = 1;
                      *fd.cFileName = 0;
                    }
                }

              if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
                {
                  if (mask)
                    {
                      if (*mask == GLOB_NOT
                          ? pathname_match_p (mask + 1, fd.cFileName)
                          : !pathname_match_p (mask, fd.cFileName))
                        continue;
                    }
                  else if (!fv_masks.empty_p ())
                    {
                      if (!fv_masks.match (fd.cFileName))
                        continue;
                    }
                  else if (match_suffixes (fd.cFileName, ignores))
                    continue;
                }
              new (this) filer_data (fd);
            }
          while (WINFS::FindNextFile (h, &fd));
        }

      if (!dotdot)
        {
          const Char *p = xstring_contents (fv_ldir);
          const Char *pe = p + xstring_length (fv_ldir);
          if (p + 2 < pe && *p == '/' && p[1] == '/')
            for (p += 2; p < pe && *p++ != '/';)
              ;
          while (p < pe && *p++ != '/')
            ;
          for (; pe > p && pe[-1] != '/'; pe--)
            ;
          if (p != pe)
            {
              FindClose (WINFS::FindFirstFile (".", &fd));
              new (this) filer_data (fd.ftLastWriteTime);
            }
        }
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }

  if (h != INVALID_HANDLE_VALUE)
    FindClose (h);

  find_chunk *head = 0, *cdr;
  for (find_chunk *fc = fv_chunk; fc; fc = cdr)
    {
      cdr = fc->fc_cdr;
      fc->fc_cdr = head;
      head = fc;
    }
  fv_chunk = head;

  return 1;
}

static void
insert_comma (char *b, int l)
{
  if (l >= 4 && xsymbol_value (Vfiler_format_comma) != Qnil)
    {
      char *f = b + l, *t = f + (l - 1) / 3;
      if (*f)
        {
          char *const bb = f;
          while (*++f)
            ;
          t += f - bb;
          *t = 0;
          while (f > bb)
            *--t = *--f;
        }
      else
        *t = 0;
      b += 3;
      while (f > b)
        {
          *--t = *--f;
          *--t = *--f;
          *--t = *--f;
          *--t = ',';
        }
    }
}

static inline void
print_size (double d, char *b)
{
  insert_comma (b, sprintf (b, "%.0f", d));
}

void
FilerView::add_list_view (const char *last)
{
  char lastb[PATH_MAX * 2];
  if (last && !*last)
    last = 0;
  if (!last && stringp (fv_llastdir))
    {
      int l = xstring_length (fv_ldir);
      if (xstring_length (fv_llastdir) >= l
          && !memicmp (xstring_contents (fv_llastdir),
                       xstring_contents (fv_ldir),
                       l * sizeof (Char)))
        {
          if (l && xstring_contents (fv_llastdir) [l - 1] == '/')
            ;
          else if (xstring_contents (fv_llastdir) [l] == '/')
            l++;
          else
            l = -1;
          if (l >= 0 && l <= xstring_length (fv_llastdir))
            {
              int e = xstring_length (fv_llastdir);
              if (e && xstring_contents (fv_llastdir) [e - 1] == '/')
                e--;
              w2s (lastb, xstring_contents (fv_llastdir) + l, e - l);
              if (*lastb)
                last = lastb;
            }
        }
    }

  fv_llastdir = fv_ldir;

  SendMessage (fv_hwnd, WM_SETREDRAW, 0, 0);
  ListView_DeleteAllItems (fv_hwnd);

  int nfiles = 0;
  for (find_chunk *fc = fv_chunk; fc; fc = fc->fc_cdr)
    nfiles += fc->fc_used;

  ListView_SetItemCount (fv_hwnd, nfiles);

  int i = 0, cur = -1, dot = -1;
  for (fc = fv_chunk; fc; fc = fc->fc_cdr)
    for (filer_data *f = fc->fc_data, *fe = f + fc->fc_used; f < fe; f++)
      {
        if (last && cur == -1 && strcaseeq (last, f->name))
          cur = i;

        LV_ITEM lvi;
        lvi.mask = LVIF_PARAM | LVIF_TEXT | LVIF_IMAGE;
        lvi.iItem = i++;
        lvi.iSubItem = 0;
        lvi.pszText = LPSTR_TEXTCALLBACK;
        lvi.lParam = LPARAM (f);

        if (!*f->name)
          dot = i - 1;

        if (!fv_retrieve_icon)
          f->icon_index = (*f->name
                           ? (f->attr & FILE_ATTRIBUTE_DIRECTORY
                              ? filer_data::ICON_DIRECTORY
                              : filer_data::ICON_REGULAR_FILE)
                           : filer_data::ICON_DOTDOT);
        lvi.iImage = I_IMAGECALLBACK;

        ListView_InsertItem (fv_hwnd, &lvi);
      }

  if (cur >= 0)
    {
      ListView_SetItemState (fv_hwnd, cur, LVIS_FOCUSED, UINT (-1));
    }
  else if (dot >= 0)
    {
      ListView_SetItemState (fv_hwnd, dot, LVIS_FOCUSED, UINT (-1));
    }

  SendMessage (fv_hwnd, WM_SETREDRAW, 1, 0);
}

void
FilerView::set_mask_text (const char *mask) const
{
  if (mask)
    {
      char *b = (char *)alloca (strlen (mask) + 16);
      stpcpy (stpcpy (b, "Mask: "), mask);
      SetWindowText (fv_hwnd_mask, b);
    }
  else
    fv_masks.set_text (fv_hwnd_mask);
}

void
FilerView::init_view (HWND hwnd, HWND hwnd_mask, HWND hwnd_marks,
                      HWND hwnd_path, int multi)
{
  static const int width[] = {121, 65, 116, 44};
  static const int fmts[] = {LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_LEFT, LVCFMT_LEFT};

  fv_hwnd = hwnd;
  ImmAssociateContext (fv_hwnd, 0);
  fv_hwnd_mask = hwnd_mask;
  fv_hwnd_marks = hwnd_marks;
  fv_hwnd_path = hwnd_path;

  if (!read_conf (cfgFiler,
                  (fv_parent->dual_window_p ()
                   ? fv_parent->left_window_p (this) ? cfgSortLeft : cfgSortRight
                   : cfgSort),
                  fv_sort))
    fv_sort = 0;

  ListView_SetExStyle (fv_hwnd, LVS_EXREPORTEX | LVS_EXTENDKBD | LVS_PROCESSKEY);
  ListView_SetSortMark (fv_hwnd,
                        (fv_sort & SORT_MASK) == SORT_EXT ? -1 : fv_sort & SORT_MASK,
                        (fv_sort & SORT_REV) ? LVSM_UP : LVSM_DOWN);

  init_list_column (fv_hwnd, 4, width, fmts, IDS_SELECT_FILE1, cfgFiler,
                    (fv_parent->dual_window_p ()
                     ? (fv_parent->left_window_p (this)
                        ? cfgColumnLeft : cfgColumnRight)
                     : cfgColumn));
  fv_regular_file_index = -(filer_data::ICON_REGULAR_FILE + 1);
  fv_directory_index = -(filer_data::ICON_DIRECTORY + 1);
  if (!fv_retrieve_icon)
    {
      HIMAGELIST hil = ImageList_LoadBitmap (app.hinst,
                                             MAKEINTRESOURCE (IDB_FILESEL),
                                             16, 1, RGB (0, 0, 255));
      ListView_SetImageList (fv_hwnd, hil, LVSIL_SMALL);
    }
  else
    {
      SHFILEINFO fi;
      HIMAGELIST hil =
        HIMAGELIST (SHGetFileInfo ("", 0, &fi, sizeof fi,
                                   (SHGFI_SYSICONINDEX | SHGFI_SMALLICON
                                    | SHGFI_USEFILEATTRIBUTES)));
      fv_regular_file_index = fi.iIcon;
      if (SHGetFileInfo ("", FILE_ATTRIBUTE_DIRECTORY, &fi, sizeof fi,
                         SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES))
        fv_directory_index = fi.iIcon;
      ListView_SetImageList (fv_hwnd, hil, LVSIL_SMALL);
      hil = ImageList_LoadBitmap (app.hinst,
                                  MAKEINTRESOURCE (IDB_FILESEL),
                                  16, 1, RGB (0, 0, 255));
      ListView_SetSubImageList (fv_hwnd, hil, 0);
    }

  DWORD style = GetWindowLong (fv_hwnd, GWL_STYLE);
  if (multi)
    style &= ~LVS_SINGLESEL;
  else
    style |= LVS_SINGLESEL;

  if (fv_retrieve_icon)
    style |= LVS_SHAREIMAGELISTS;

  SetWindowLong (fv_hwnd, GWL_STYLE, style);

  dropt.set_view (this);
  RegisterDragDrop (fv_hwnd, &dropt);
}

void
FilerView::save_column () const
{
  if (fv_hwnd)
    {
      save_list_column_width (fv_hwnd, 4, cfgFiler,
                              (fv_parent->dual_window_p ()
                               ? (fv_parent->left_window_p (this)
                                  ? cfgColumnLeft : cfgColumnRight)
                               : cfgColumn));
      RevokeDragDrop (fv_hwnd);
    }
}

static int
check_share_folder (const char *path)
{
  safe_com <IShellFolder> desktop;
  if (FAILED (SHGetDesktopFolder (&desktop)))
    return 0;

  safe_com <IMalloc> ialloc;
  if (FAILED (SHGetMalloc (&ialloc)))
    return 0;

  int l = strlen (path) + 1;
  wchar_t *w = (wchar_t *)alloca (sizeof *w * l);
  MultiByteToWideChar (CP_ACP, 0, path, -1, w, l);

  ULONG eaten, attr = SFGAO_SHARE;
  safe_idl idl (ialloc);
  if (FAILED (desktop->ParseDisplayName (0, 0, w, &eaten, &idl, &attr)))
    return 0;

  return attr & SFGAO_SHARE;
}

void
FilerView::dispinfo (LV_ITEM *lv)
{
  filer_data *d = (filer_data *)lv->lParam;
  switch (lv->iSubItem)
    {
    case 0:
      lv->pszText = *d->name ? d->name : "..";
      if (lv->mask & LVIF_IMAGE)
        {
          int image;
          {
            ex_lock lock (fv_lockobj);
            image = d->icon_index;
            if (image == filer_data::ICON_INVALID)
              d->icon_index = filer_data::ICON_INVALID_REF;
          }
          if (image != filer_data::ICON_INVALID && image != filer_data::ICON_INVALID_REF)
            {
              lv->iImage = image & 0xffffff;
              int overlay = (image >> 24) /* & 255*/;
              if (overlay > 0 && overlay < 16)
                {
                  lv->mask |= LVIF_STATE;
                  lv->stateMask = LVIS_OVERLAYMASK;
                  lv->state = INDEXTOOVERLAYMASK (overlay);
                }
              if (!(lv->mask & LVIF_TEXT))
                lv->mask |= LVIF_DI_SETITEM;
            }
          else
            {
              if (!*d->name)
                lv->iImage = -(filer_data::ICON_DOTDOT + 1);
              else
                lv->iImage = (d->attr & FILE_ATTRIBUTE_DIRECTORY
                              ? fv_directory_index
                              : fv_regular_file_index);
              lv->mask &= ~LVIF_DI_SETITEM;
            }
        }
      return;

    case 1:
      if (!(d->attr & FILE_ATTRIBUTE_DIRECTORY))
        {
          print_size (d->bytes, fv_buf);
          lv->pszText = fv_buf;
        }
      else
        lv->pszText = "";
      break;

    case 2:
      {
        FILETIME ft;
        FileTimeToLocalFileTime (&d->time, &ft);
        SYSTEMTIME st;
        FileTimeToSystemTime (&ft, &st);
        sprintf (fv_buf, "%04d/%02d/%02d %02d:%02d:%02d",
                 st.wYear, st.wMonth, st.wDay,
                 st.wHour, st.wMinute, st.wSecond);
        lv->pszText = fv_buf;
        break;
      }

    case 3:
      fv_buf[0] = d->attr & FILE_ATTRIBUTE_DIRECTORY ? 'd' : '-';
      fv_buf[1] = d->attr & FILE_ATTRIBUTE_ARCHIVE ? 'a' : '-';
      fv_buf[2] = d->attr & FILE_ATTRIBUTE_HIDDEN ? 'h' : '-';
      fv_buf[3] = d->attr & FILE_ATTRIBUTE_READONLY ? '-' : 'w';
      fv_buf[4] = d->attr & FILE_ATTRIBUTE_SYSTEM ? 's' : '-';
      fv_buf[5] = 0;
      lv->pszText = fv_buf;
      break;

    default:
      return;
    }
  lv->mask |= LVIF_DI_SETITEM;
}

static int
compare_filename (const char *s1, const char *s2, int param)
{
  if (!(param & FilerView::SORT_NUM))
    return (param & FilerView::SORT_CASE
            ? strcmp (s1, s2) : strcasecmp (s1, s2));

  extern u_char char_no_translate_table[];
  extern u_char char_translate_downcase_table[];
  const u_char *const translate = (param & FilerView::SORT_CASE
                                   ? char_no_translate_table
                                   : char_translate_downcase_table);
  const u_char *p1 = (const u_char *)s1;
  const u_char *p2 = (const u_char *)s2;
  while (*p1 && *p2)
    {
      u_char c1 = *p1++, c2 = *p2++;
      if (digit_char_p (c1) && digit_char_p (c2))
        {
          for (const u_char *const b1 = p1 - 1; digit_char_p (*p1); p1++)
            ;
          for (const u_char *const b2 = p2 - 1; digit_char_p (*p2); p2++)
            ;
          int l1 = p1 - b1, l2 = p2 - b2;
          if (l1 != l2)
            return l1 - l2;
          int d = memcmp (b1, b2, l1);
          if (d)
            return d;
        }
      else
        {
          c1 = translate[c1];
          c2 = translate[c2];
          if (c1 != c2)
            return c1 - c2;
          if (SJISP (c1) && *p1)
            {
              if (*p1 != *p2)
                return *p1 - *p2;
              p1++;
              p2++;
            }
        }
    }
  return *p1 - *p2;
}

static int CALLBACK
compare_file (LPARAM p1, LPARAM p2, LPARAM param)
{
  const filer_data *f1 = (const filer_data *)p1;
  const filer_data *f2 = (const filer_data *)p2;
  if (!*f1->name)
    return -1;
  if (!*f2->name)
    return 1;
  int d;
  if (!(param & FilerView::SORT_IDIR))
    {
      d = ((f2->attr & FILE_ATTRIBUTE_DIRECTORY)
           - (f1->attr & FILE_ATTRIBUTE_DIRECTORY));
      if (d)
        return d;
    }
  else
    d = 0;
  switch (param & FilerView::SORT_MASK)
    {
    case FilerView::SORT_SIZE:
      d = (f1->bytes == f2->bytes
           ? 0
           : f1->bytes < f2->bytes ? -1 : 1);
      break;

    case FilerView::SORT_DATE:
      d = CompareFileTime (&f2->time, &f1->time);
      break;

    case FilerView::SORT_EXT:
      {
        const char *p1, *p2;
        for (p1 = f1->name; *p1 == '.'; p1++)
          ;
        for (p2 = f2->name; *p2 == '.'; p2++)
          ;
        p1 = jrindex (p1, '.');
        p2 = jrindex (p2, '.');
        if (p1 && p2)
          d = compare_filename (p1, p2, param);
        else if (p1)
          d = 1;
        else if (p2)
          d = -1;
        break;
      }
    }
  if (!d)
    d = compare_filename (f1->name, f2->name, param);
  return param & FilerView::SORT_REV ? -d : d;
}

void
FilerView::sort () const
{
  ListView_SetSortMark (fv_hwnd,
                        (fv_sort & SORT_MASK) == SORT_EXT ? -1 : fv_sort & SORT_MASK,
                        (fv_sort & SORT_REV) ? LVSM_UP : LVSM_DOWN);
  ListView_SortItems (fv_hwnd, compare_file, fv_sort);
  int i = lv_find_focused_item (fv_hwnd);
  if (i >= 0)
    ListView_EnsureVisible (fv_hwnd, i, 0);
  else
    ListView_SetItemState (fv_hwnd, 0, LVIS_FOCUSED, LVIS_FOCUSED);
}

void
FilerView::sort (int param)
{
  if (param < 0)
    {
      param = (-1 - param) & SORT_MASK;
      if (param == SORT_EXT)
        return;
      if (param == (fv_sort & SORT_MASK))
        param = fv_sort ^ SORT_REV;
      param |= fv_sort & ~(SORT_MASK | SORT_REV);
    }
  if (fv_sort != param)
    {
      fv_sort = param;
      sort ();
    }
}

void
FilerView::set_title (const char *mask) const
{
  int l = xstring_length (fv_ldir) * 2 + 1;
  lisp title = fv_parent->title ();
  if (stringp (title))
    l += xstring_length (title) * 2 + 3;
  if (mask)
    l += strlen (mask) + 1;
  char *b0 = (char *)alloca (l);
  char *b = w2s (b0, fv_ldir);
  if (mask)
    b = stpcpy (b, mask);
  if (stringp (title))
    b = w2s (stpcpy (b, " - "), title);
  SetWindowText (fv_parent->id_hwnd, b0);
}

void
FilerView::set_title () const
{
  if (!stringp (fv_lmask))
    set_title (0);
  else
    {
      char *mask = (char *)alloca (xstring_length (fv_lmask) * 2 + 1);
      w2s (mask, fv_lmask);
      set_title (mask);
    }
}

void
FilerView::set_path () const
{
  if (fv_parent->dual_window_p ())
    {
      char *b = (char *)alloca (xstring_length (fv_ldir) * 2 + 1);
      w2s (b, fv_ldir);
      SetWindowText (fv_hwnd_path, b);
    }
}

class wait_cursor
{
  HCURSOR ocur;
public:
  wait_cursor () {ocur = SetCursor (sysdep.hcur_wait);}
  ~wait_cursor () {SetCursor (ocur);}
};

void
FilerView::reload (lisp lmask)
{
  wait_cursor wc;
  fv_subscribed = 0;
  fv_marks_changed = 1;
  char last[MAX_PATH];
  *last = 0;
  if (fv_llastdir == fv_ldir
      || (stringp (fv_llastdir) && string_equal (fv_llastdir, fv_ldir)))
    {
      LV_ITEM lvi;
      if (find_focused (&lvi) >= 0)
        strcpy (last, ((filer_data *)lvi.lParam)->name);
    }
  else
    {
      xsymbol_value (Vfiler_chdir_primary_p) =
        boole (fv_parent->primary_window_p (this));
      selected_buffer ()->safe_run_hook (Vfiler_chdir_hook, 1);
    }

  fv_lmask = lmask ? lmask : Qnil;
  char *mask;
  if (stringp (fv_lmask))
    {
      mask = (char *)alloca (xstring_length (fv_lmask) * 2 + 1);
      w2s (mask, fv_lmask);
    }
  else
    mask = 0;
  set_mask_text (mask);
  set_path ();
  if (fv_parent->primary_window_p (this))
    set_title (mask);
  SetDlgItemText (fv_parent->id_hwnd, IDC_NAME, "");
  if (load_contents (mask))
    {
      restart_thread ();
      add_list_view (*last ? last : 0);
    }
  sort ();
  echo_filename ();
  fv_parent->show_disk_info (this);
}

void
FilerView::delayed_reload ()
{
  if (!fv_subscribed)
    return;
  try
    {
      reload (Qnil);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
}

void
FilerView::show_marks (int force)
{
  if (!force && !fv_marks_changed)
    return;
  if (!fv_parent->check_idle ())
    return;

  fv_marks_changed = 0;

  int nfiles = 0, ndirs = 0;
  double nbytes = 0;
  for (int i = -1; (i = ListView_GetNextItem (fv_hwnd, i, LVNI_SELECTED)) >= 0;)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi))
        {
          const filer_data *f = (filer_data *)lvi.lParam;
          if (f->attr & FILE_ATTRIBUTE_DIRECTORY)
            ndirs++;
          else
            {
              nfiles++;
              nbytes += f->bytes;
            }
        }
    }

  if (ndirs + nfiles)
    {
      char b[256], nb[128];
      disk_space (nbytes, nb, (charp (xsymbol_value (Vfiler_mark_file_size_unit))
                               ? xchar_code (xsymbol_value (Vfiler_mark_file_size_unit))
                               : -1));
      sprintf (b, "Marks: %d dirs, %d files, total: %sytes", ndirs, nfiles, nb);
      SetWindowText (fv_hwnd_marks, b);
    }
  else
    SetWindowText (fv_hwnd_marks, "");
}

int
FilerView::set_directory (lisp dir)
{
  check_string (dir);
  chdir (fv_ldir);
  if (!chdevdir (dir))
    file_error (GetLastError (), dir);

  char cur[PATH_MAX];
  GetCurrentDirectory (sizeof cur, cur);
  fv_parent->restore_dir ();
  lisp lcur = make_string (cur);
  map_backsl_to_sl (xstring_contents (lcur),
                    xstring_length (lcur));
  lcur = Fappend_trail_slash (lcur);
  if (string_equal (fv_ldir, lcur))
    return 0;
  fv_llastdir = fv_ldir;
  fv_ldir = lcur;
  return 1;
}

void
FilerView::disk_space (double nbytes, char *buf, int c)
{
  const char *const u[] = {"B", "KB", "MB", "GB", "TB"};
  for (int i = 0; i < numberof (u) - 1 && c != *u[i] && nbytes >= 1024.0;
       i++, nbytes /= 1024.0)
    ;
  sprintf (buf, "%.2f", nbytes);
  for (char *b = buf + strlen (buf); b > buf && b[-1] == '0'; b--)
    ;
  if (b > buf && b[-1] == '.')
    b--;
  char *e = b;
  for (; *e != '.'; e--)
    ;
  strcpy (b, u[i]);
  insert_comma (buf, e - buf);
}

void
FilerView::display_disk_info (HWND hwnd, int n) const
{
  chdir (fv_ldir);
  DWORD s_per_c, b_per_s, free_c, total_c;
  if (!WINFS::GetDiskFreeSpace (0, &s_per_c, &b_per_s, &free_c, &total_c))
    s_per_c = b_per_s = free_c = total_c = 0;
  fv_parent->restore_dir ();

  char total[128], free[128];
  disk_space (double (total_c) * s_per_c * b_per_s, total, -1);
  disk_space (double (free_c) * s_per_c * b_per_s, free, -1);
  char buf[256];
  sprintf (buf, "Free: %s, Total: %s", free, total);
  SendMessage (hwnd, SB_SETTEXT, n, LPARAM (buf));
}

int
FilerView::find_focused (LV_ITEM *lvi)
{
  int i = ListView_GetNextItem (fv_hwnd, -1, LVNI_FOCUSED);
  if (i >= 0)
    {
      lvi->iItem = i;
      lvi->iSubItem = 0;
      lvi->mask = LVIF_PARAM | LVIF_STATE;
      lvi->stateMask = UINT (-1);
      if (ListView_GetItem (fv_hwnd, lvi)
          && lvi->state & LVIS_FOCUSED)
        return i;
    }
  return -1;
}

lisp
FilerView::get_drive () const
{
  if (xstring_length (fv_ldir) >= 2)
    {
      const Char *p = xstring_contents (fv_ldir);
      if (alpha_char_p (*p) && p[1] == ':')
        return make_char (*p);
    }
  return Qnil;
}

int
FilerView::mark (int fo)
{
  fv_marks_changed = 1;
  LV_ITEM lvi;
  int i = find_focused (&lvi);
  if (i == -1)
    return 0;
  if ((fo && ((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY)
      || !*((filer_data *)lvi.lParam)->name)
    return 0;
  ListView_SetItemState (fv_hwnd, i, LVIS_SELECTED, LVIS_SELECTED);
  return 1;
}

int
FilerView::mark_all (int fo)
{
  fv_marks_changed = 1;
  int nselected = 0;
  int nitems = ListView_GetItemCount (fv_hwnd);
  for (int i = 0; i < nitems; i++)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi)
          && (!fo || !(((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY))
          && *((filer_data *)lvi.lParam)->name)
        {
          ListView_SetItemState (fv_hwnd, i, LVIS_SELECTED, LVIS_SELECTED);
          nselected++;
        }
    }
  return nselected;
}

int
FilerView::toggle_mark (int fo)
{
  fv_marks_changed = 1;
  LV_ITEM lvi;
  int i = find_focused (&lvi);
  if (i == -1)
    return 0;
  if ((fo && ((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY)
      || !*((filer_data *)lvi.lParam)->name)
    return 0;
  ListView_SetItemState (fv_hwnd, i, ((lvi.state & LVIS_SELECTED)
                                      ? 0 : LVIS_SELECTED),
                         LVIS_SELECTED);
  return 1;
}

int
FilerView::toggle_all_marks (int fo)
{
  fv_marks_changed = 1;
  int nselected = 0;
  int nitems = ListView_GetItemCount (fv_hwnd);
  for (int i = 0; i < nitems; i++)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM | LVIF_STATE;
      lvi.stateMask = LVIS_SELECTED;
      if (ListView_GetItem (fv_hwnd, &lvi)
          && (!fo || !(((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY))
          && *((filer_data *)lvi.lParam)->name)
        {
          ListView_SetItemState (fv_hwnd, i, ((lvi.state & LVIS_SELECTED)
                                              ? 0 : LVIS_SELECTED),
                                 LVIS_SELECTED);
          if (lvi.state & LVIS_SELECTED)
            nselected++;
        }
    }
  return nselected;
}

lisp
FilerView::get_mark_files (int fo)
{
  lisp r = Qnil;
  for (int i = -1; (i = ListView_GetNextItem (fv_hwnd, i, LVNI_SELECTED)) >= 0;)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi))
        {
          filer_data *d = (filer_data *)lvi.lParam;
          if (*d->name && (!fo || !(d->attr & FILE_ATTRIBUTE_DIRECTORY)))
            r = xcons (filename (d), r);
        }
    }
  return Fnreverse (r);
}

lisp
FilerView::get_current_file ()
{
  LV_ITEM lvi;
  int i = find_focused (&lvi);
  if (i == -1)
    return Qnil;
  return filename ((filer_data *)lvi.lParam);
}

int
FilerView::current_file_directory_p ()
{
  LV_ITEM lvi;
  int i = find_focused (&lvi);
  if (i == -1)
    return 0;
  return ((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY;
}

int
FilerView::current_file_dot_dot_p ()
{
  LV_ITEM lvi;
  int i = find_focused (&lvi);
  if (i == -1)
    return 0;
  return !*((filer_data *)lvi.lParam)->name;
}

int
FilerView::search (lisp string, lisp lstart, lisp lreverse, lisp lwild)
{
  check_string (string);
  char *pat = (char *)alloca (xstring_length (string) * 2 + 2);
  char *pe = w2s (pat, string);

  int inc = !lreverse || lreverse == Qnil ? 1 : -1;
  int wild = lwild && lwild != Qnil;
  if (wild && lwild != Qt)
    {
      *pe++ = '*';
      *pe = 0;
    }

  int index;
  if (!lstart || lstart == Qnil)
    index = 0;
  else
    {
      index = ListView_GetNextItem (fv_hwnd, -1, LVNI_FOCUSED);
      if (index < 0)
        index = 0;
      if (lstart != Qt)
        index += inc;
    }

  int nitems = ListView_GetItemCount (fv_hwnd);
  for (int i = 0; i < nitems; i++, index += inc)
    {
      if (index < 0)
        index = nitems - 1;
      else if (index >= nitems)
        index = 0;

      LV_ITEM lvi;
      lvi.iItem = index;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi))
        {
          const char *name = ((filer_data *)lvi.lParam)->name;
          if (!*name)
            name = "..";
          if (wild ? pathname_match_p (pat, name) : !_stricmp (pat, name))
            {
              ListView_SetItemState (fv_hwnd, index, LVIS_FOCUSED, LVIS_FOCUSED);
              ListView_EnsureVisible (fv_hwnd, index, 0);
              return 1;
            }
        }
    }
  return 0;
}

int
FilerView::mark_match_files (lisp lmasks)
{
  file_masks masks (lmasks);
  if (masks.empty_p ())
    return 0;

  fv_marks_changed = 1;
  int nselected = 0;
  int nitems = ListView_GetItemCount (fv_hwnd);
  for (int i = 0; i < nitems; i++)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM | LVIF_STATE;
      lvi.stateMask = LVIS_SELECTED;
      if (ListView_GetItem (fv_hwnd, &lvi)
          && !(lvi.state & LVIS_SELECTED)
          && !(((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY)
          && masks.match (((filer_data *)lvi.lParam)->name))
        {
          ListView_SetItemState (fv_hwnd, i, LVIS_SELECTED, LVIS_SELECTED);
          nselected++;
        }
    }
  return nselected;
}

void
FilerView::clear_all_marks ()
{
  fv_marks_changed = 1;
  for (int i = -1; (i = ListView_GetNextItem (fv_hwnd, i, LVNI_SELECTED)) >= 0;)
    ListView_SetItemState (fv_hwnd, i, 0, LVIS_SELECTED);
}

int
FilerView::count_marks (int fo)
{
  int nmarks = 0;
  for (int i = -1; (i = ListView_GetNextItem (fv_hwnd, i, LVNI_SELECTED)) >= 0;)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi)
          && (!fo || !(((filer_data *)lvi.lParam)->attr & FILE_ATTRIBUTE_DIRECTORY)))
        nmarks++;
    }
  return nmarks;
}

void
FilerView::activate (int f)
{
  ListView_SetEnabled (fv_hwnd, f);
  EnableWindow (fv_hwnd_mask, f);
  if (fv_hwnd_path)
    EnableWindow (fv_hwnd_path, f);
  EnableWindow (fv_hwnd_marks, f);
  if (f)
    set_title ();
}

int
FilerView::calc_directory_size (int based_on_bytes)
{
  wait_cursor wc;
  int index = based_on_bytes ? 4 : 3;
  for (int i = -1; (i = ListView_GetNextItem (fv_hwnd, i, LVNI_SELECTED)) >= 0;)
    {
      LV_ITEM lvi;
      lvi.iItem = i;
      lvi.iSubItem = 0;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem (fv_hwnd, &lvi))
        {
          filer_data *d = (filer_data *)lvi.lParam;
          if (d->attr & FILE_ATTRIBUTE_DIRECTORY)
            {
              Fget_disk_usage (filename (d), Qt);
              d->bytes = coerce_to_double_float (multiple_value::value (index));
              char buf[128];
              *buf = '[';
              disk_space (d->bytes, buf + 1, -1);
              strcat (buf, "]");
              lvi.mask = LVIF_TEXT;
              lvi.iSubItem = 1;
              lvi.pszText = buf;
              ListView_SetItem (fv_hwnd, &lvi);
            }
        }
    }
  multiple_value::clear ();
  return 1;
}

void
FilerView::subscribe_reload (lisp path, int subdirp)
{
  if (!fv_subscribed)
    fv_subscribed = (subdirp
                     ? Fsub_directory_p (get_directory (), path)
                     : Fpath_equal (get_directory (), path)) != Qnil;
}

int
FilerView::modify_column_width (int n, int d) const
{
  if (n < 0 || n >= NHEADER_COLUMNS)
    FErange_error (make_fixnum (n));
  int ow = ListView_GetColumnWidth (fv_hwnd, n);
  RECT r;
  GetClientRect (fv_hwnd, &r);
  int nw = min (max (ow + d, 0), int (r.right));
  if (ow == nw)
    return 0;
  ListView_SetColumnWidth (fv_hwnd, n, MAKELPARAM (nw, 0));
  return 1;
}

void
FilerView::echo_filename ()
{
  if (xsymbol_value (Vfiler_echo_filename) == Qnil)
    app.status_window.clear ();
  else if (fv_parent->check_idle ())
    {
      LV_ITEM lvi;
      if (find_focused (&lvi) >= 0)
        {
          const filer_data *f = (filer_data *)lvi.lParam;
          app.status_window.text (*f->name ? f->name : "..");
        }
      app.status_window.clear (1);
    }
}

unsigned __stdcall
FilerView::thread_entry (void *p)
{
  ((FilerView *)p)->thread_main ();
  return 0;
}

class filer_data_enumerator
{
public:
  filer_data_enumerator (FilerView::find_chunk *chunk)
       : m_chunk0 (chunk), m_chunk (chunk), m_findex (0), m_state (0)
    {}

  filer_data *next ()
    {
      for (;;)
        {
          filer_data *fd = get_next ();
          if (fd)
            {
              if (m_state
                  ? fd->attr & FILE_ATTRIBUTE_DIRECTORY
                  : !(fd->attr & FILE_ATTRIBUTE_DIRECTORY))
                return fd;
            }
          else
            {
              if (m_state)
                return 0;
              m_state = 1;
              m_chunk = m_chunk0;
              m_findex = 0;
            }
        }
    }

private:
  filer_data *get_next ()
    {
      if (!m_chunk)
        return 0;
      while (m_findex == m_chunk->fc_used)
        {
          m_chunk = m_chunk->fc_cdr;
          if (!m_chunk)
            return 0;
          m_findex = 0;
        }
      return &m_chunk->fc_data[m_findex++];
    }

private:
  FilerView::find_chunk *m_chunk0;
  FilerView::find_chunk *m_chunk;
  int m_findex;
  int m_state;
};

void
FilerView::thread_main ()
{
  while (WaitForSingleObject (fv_hevent, INFINITE) == WAIT_OBJECT_0)
    {
      if (fv_stop_thread)
        break;

      char *path;
      int sequence;
      int len;
      find_chunk *chunk;

      {
        ex_lock lock (fv_lockobj);
        if (!fv_icon_path || !fv_chunk)
          continue;

        len = strlen (fv_icon_path);
        path = (char *)alloca (len + MAX_PATH + 1);
        strcpy (path, fv_icon_path);
        sequence = fv_sequence;
        chunk = fv_chunk;
      }

      DWORD update_tick = GetTickCount ();
      int has_ref = 0;

      filer_data_enumerator fe (chunk);

      for (;;)
        {
          if (fv_stop_thread)
            goto term;

          filer_data *fd;
          DWORD attr;
          {
            ex_lock lock (fv_lockobj);
            if (sequence != fv_sequence)
              break;

            fd = fe.next ();
            if (!fd)
              {
                if (has_ref)
                  InvalidateRect (fv_hwnd, 0, 0);
                break;
              }
            if (!*fd->name || (fd->icon_index != filer_data::ICON_INVALID
                               && fd->icon_index != filer_data::ICON_INVALID_REF))
              continue;

            strcpy (path + len, fd->name);
            attr = fd->attr;
          }

          SHFILEINFO fi;
          if (!SHGetFileInfo (path, attr, &fi, sizeof fi,
                              SHGFI_ICON | SHGFI_SMALLICON | SHGFI_OVERLAYINDEX))
            continue;

          DestroyIcon (fi.hIcon);

          {
            ex_lock lock (fv_lockobj);
            if (sequence != fv_sequence)
              break;
            if (fd->icon_index == filer_data::ICON_INVALID_REF)
              has_ref = 1;
            fd->icon_index = fi.iIcon;
          }

          if (has_ref)
            {
              DWORD tick = GetTickCount ();
              if (tick - update_tick > 100)
                {
                  update_tick = tick;
                  has_ref = 0;
                  InvalidateRect (fv_hwnd, 0, 0);
                }
            }
        }
    }
term:;
}

void
FilerView::interrupt_thread ()
{
  ex_lock lock (fv_lockobj);
  fv_sequence++;
}

void
FilerView::restart_thread ()
{
  if (!fv_hthread)
    return;
  char *path = (char *)malloc (xstring_length (fv_ldir) * 2 + 1);
  if (!path)
    return;
  w2s (path, fv_ldir);
  map_sl_to_backsl (path);

  ex_lock lock (fv_lockobj);
  if (fv_icon_path)
    free (fv_icon_path);
  fv_icon_path = path;
  fv_sequence++;
  SetEvent (fv_hevent);
}

///////////////////////////////////////////////////////////////
// Filer
///////////////////////////////////////////////////////////////

Filer *Filer::f_chain;
Filer *Filer::f_mlfiler;
int Filer::f_mlactive;

Filer::Filer (lisp dir, lisp sdir, lisp name, lisp last_path,
              lisp multi, lisp title, lisp dual, lisp left, int auto_delete)
     : f_multi (multi != Qnil),
       f_gcpro (f_lobjs, numberof (f_lobjs)), f_fdispatch (0),
       f_fv1 (dir, last_path), f_fv2 (sdir, last_path), f_viewer_on (0),
       f_changed_view (0), IdleDialog (auto_delete), f_idle_timer (0),
       f_ctx_menu2 (0)
{
  chain ();

  f_lkeymap = Qunbound;
  f_ltitle = title;

  if (dual == Qnil)
    {
      f_pview = &f_fv1;
      f_sview = 0;
    }
  else
    {
      if (left != Qnil)
        {
          f_pview = &f_fv1;
          f_sview = &f_fv2;
        }
      else
        {
          f_pview = &f_fv2;
          f_sview = &f_fv1;
        }
      f_pview->setdir (dir);
      f_sview->setdir (sdir);
    }

  f_fv1.set_parent (this);
  f_fv2.set_parent (this);

  f_lresult = Qnil;
  f_ldefmask = name;

  f_lguide_text = xsymbol_value (Vfiler_guide_text);
  f_guide_height = 0;
  if (dual_window_p ())
    {
      if (stringp (f_lguide_text))
        f_guide_nlines = 1;
      else
        {
          f_guide_nlines = 0;
          for (lisp p = f_lguide_text; consp (p); p = xcdr (p))
            if (++f_guide_nlines == MAX_GUIDE_NLINES)
              break;
        }
    }
}

lisp
Filer::result ()
{
  multiple_value::count () = 2;
  if (f_lresult == Qnil)
    {
      multiple_value::value (1) = Qnil;
      return f_pview->get_directory ();
    }
  multiple_value::value (1) = Qt;
  return (f_multi
          ? (consp (f_lresult)
             ? f_lresult
             : xcons (f_lresult, Qnil))
          : (consp (f_lresult)
             ? xcar (f_lresult)
             : f_lresult));
}

void
Filer::IdleProc ()
{
  set_idle (0);
}

static void
add_combo (HWND combo, lisp string)
{
  char *b = (char *)alloca (xstring_length (string) * 2 + 1);
  w2s (b, string);
  SendMessage (combo, CB_ADDSTRING, 0, LPARAM (b));
}

static void
add_file_name_histories (HWND combo)
{
  for (lisp p = xsymbol_value (Vminibuffer_file_name_history);
       consp (p); p = xcdr (p))
    if (stringp (xcar (p)))
      add_combo (combo, xcar (p));
}

static void
expand_combobox (int n, int d)
{
  for (HWND hwnd = GetTopWindow (0); hwnd;
       hwnd = GetNextWindow (hwnd, GW_HWNDNEXT))
    {
      char name[16];
      if (GetClassName (hwnd, name, 10) == 9
          && !strcmp (name, "ComboLBox"))
        {
          DWORD pid;
          GetWindowThreadProcessId (hwnd, &pid);
          if (pid == GetCurrentProcessId ())
            {
              RECT r;
              GetWindowRect (hwnd, &r);
              if (r.left)
                {
                  MoveWindow (hwnd, r.left, r.top,
                              (r.right - r.left) * n / d, r.bottom - r.top, 0);
                  break;
                }
            }
        }
    }
}

void
Filer::save_geometry_p (int &posp, int &sizep) const
{
  if (modeless_p ())
    posp = sizep = 1;
  else
    {
      posp = xsymbol_value (Vmodal_filer_save_position) != Qnil;
      sizep = xsymbol_value (Vmodal_filer_save_size) != Qnil;
    }
}

BOOL
Filer::InitDialog ()
{
  set_idle (0);

  HICON ico = LoadIcon (app.hinst,
                        (modeless_p ()
                         ? MAKEINTRESOURCE (IDI_FILER)
                         : MAKEINTRESOURCE (IDI_XYZZY)));
  SendMessage (id_hwnd, WM_SETICON, 1, LPARAM (ico));
  SendMessage (id_hwnd, WM_SETICON, 0, LPARAM (ico));

  HIMC himc = ImmGetContext (id_hwnd);
  if (himc)
    {
      ImmSetOpenStatus (himc, 0);
      ImmReleaseContext (id_hwnd, himc);
    }

  f_hwnd_status = CreateStatusWindow (((f_sview ? SBARS_SIZEGRIP : 0)
                                       | WS_CHILD | WS_VISIBLE
                                       | WS_CLIPCHILDREN),
                                      0, id_hwnd, 0);
  if (!f_hwnd_status)
    {
      EndDialog (IDCANCEL);
      return 0;
    }

  f_vwindow.init (id_hwnd, &f_vbuffer);

  f_fv1.init_view (GetDlgItem (id_hwnd, IDC_LIST1),
                   GetDlgItem (id_hwnd, IDC_MASK1),
                   GetDlgItem (id_hwnd, IDC_MARKS1),
                   dual_window_p () ? GetDlgItem (id_hwnd, IDC_PATH1) : 0,
                   f_multi);
  f_fv1.set_colors ();
  if (dual_window_p ())
    {
      f_fv2.init_view (GetDlgItem (id_hwnd, IDC_LIST2),
                       GetDlgItem (id_hwnd, IDC_MASK2),
                       GetDlgItem (id_hwnd, IDC_MARKS2),
                       GetDlgItem (id_hwnd, IDC_PATH2),
                       f_multi);
      f_fv2.set_colors ();
      try
        {
          f_sview->reload (Qnil);
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
        }
      f_sview->activate (0);
    }

  try
    {
      f_pview->reload (f_ldefmask);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
  f_pview->activate (1);

  int posp, sizep;
  save_geometry_p (posp, sizep);
  if (!conf_load_geometry (id_hwnd, cfgFiler,
                           dual_window_p () ? 0 : "s", posp, sizep)
      || !posp)
    center_window (id_hwnd);

  RECT r;
  GetClientRect (id_hwnd, &r);
  Size (r.right, r.bottom);

  if (!dual_window_p ())
    {
      HWND combo = GetDlgItem (id_hwnd, IDC_NAME);
      add_file_name_histories (combo);
      GetClientRect (combo, &r);
      if (sysdep.Win4p ())
        SendMessage (combo, CB_SETDROPPEDWIDTH, r.right * 3 / 2, 0);
      else
        expand_combobox (3, 2);
    }
  SetTimer (id_hwnd, TID_GC, 30000, 0);
  SetFocus (f_pview->fv_hwnd);
  return 0;
}

void
Filer::demand_reload () const
{
  f_pview->delayed_reload ();
  f_pview->show_marks ();
  UpdateWindow (f_pview->fv_hwnd);
  if (dual_window_p ())
    {
      f_sview->delayed_reload ();
      f_sview->show_marks ();
      UpdateWindow (f_sview->fv_hwnd);
    }
}

void
Filer::end_dispatch ()
{
  demand_reload ();
  xsymbol_value (Vquit_flag) = Qnil;
  xsymbol_value (Vinhibit_quit) = Qnil;
  xsymbol_value (Vevalhook) = Qnil;
  xsymbol_value (Vapplyhook) = Qnil;
  WINFS::clear_share_cache ();
}

int
Filer::dispatch (lChar cc)
{
  if (f_fdispatch)
    return 0;
  if (f_lkeymap == Qunbound)
    f_lkeymap = xsymbol_value (Vfiler_keymap);
  lisp fn = lookup_keymap (Char (cc), &f_lkeymap, 1);
  if (!fn)
    {
      f_keyseq.push (Char (cc), 1);
      return 1;
    }
  f_keyseq.done (Char (cc), 1);
  f_lkeymap = Qunbound;
  if (fn == Qnil)
    {
      app.status_window.puts (Ekey_not_bound, 1);
      return 0;
    }

  try
    {
      f_fdispatch = 1;
      xsymbol_value (Vfiler_last_command_char) = make_char (Char (cc));
      Ffuncall (fn, Qnil);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
  end_dispatch ();
  if (modeless_p ())
    refresh_screen (1);
  end_wait_cursor (1);
  f_fdispatch = 0;
  return 1;
}

int
Filer::process_keys (const LV_PROCESSKEY *pk)
{
  lChar cc;
  switch (pk->message)
    {
    case WM_KEYDOWN:
      switch (pk->wparam)
        {
        case CC_ESC:
          if (xsymbol_value (Vfiler_eat_esc) == Qnil)
            goto normal_char;
          close (Qnil);
          return 1;

        default:
        normal_char:
          cc = decode_keys (pk->wparam, pk->lparam);
          if (cc != lChar_EOF)
            {
              dispatch (cc);
              return 1;
            }
          break;
        }
      break;

    case WM_SYSKEYDOWN:
      cc = decode_syskeys (pk->wparam, pk->lparam);
      if (cc != lChar_EOF)
        {
          dispatch (cc);
          return 1;
        }
      break;

    case WM_CHAR:
      if (pk->wparam != CC_TAB && pk->wparam != CC_SPC)
        dispatch (decode_chars (pk->wparam));
      return 1;

    case WM_SYSCHAR:
      if (dispatch (decode_syschars (pk->wparam)))
        return 1;
      break;

    case WM_LBUTTONDOWN:
      {
        LV_HITTESTINFO ht;
        ht.pt.x = short (LOWORD (pk->lparam));
        ht.pt.y = short (HIWORD (pk->lparam));
        int i = ListView_HitTest (pk->hdr.hwndFrom, &ht);
        if (i >= 0)
          {
            int o = (xsymbol_value (Vfiler_click_toggle_marks_always) == Qnil
                     ? ListView_GetNextItem (pk->hdr.hwndFrom, -1, LVNI_FOCUSED)
                     : -1);
            ListView_SetItemState (pk->hdr.hwndFrom, i,
                                   LVIS_FOCUSED, LVIS_FOCUSED);
            if (GetFocus () == pk->hdr.hwndFrom)
              {
                if (o < 0 || i == o)
                  {
                    Ffiler_toggle_mark (0, 0);
                    pview ()->show_marks ();
                  }
                return 1;
              }
          }
      }
      /* fall thru... */
    case WM_MBUTTONDOWN:
      SetFocus (pk->hdr.hwndFrom);
      return 1;

    case WM_RBUTTONDOWN:
      PostMessage (id_hwnd, WM_PRIVATE_FILER_SHOW_MARKS, pk->hdr.idFrom, 0);
      break;

    case WM_KEYUP:
    case WM_RBUTTONUP:
      PostMessage (id_hwnd, WM_PRIVATE_FILER_KEYUP, 0, 0);
      break;

    case WM_LBUTTONUP:
    case WM_MBUTTONUP:
    case WM_XBUTTONUP:
      PostMessage (id_hwnd, WM_PRIVATE_FILER_KEYUP, 0, 0);
      return 1;
    }
  return 0;
}

void
Filer::context_menu (NMHDR *nm)
{
  try
    {
      POINT p;
      GetCursorPos (&p);
      shell_context_menu (nm->hwndFrom, (nm->idFrom == IDC_LIST1
                                         ? f_fv1.get_directory ()
                                         : f_fv2.get_directory ()),
                          p, id_hwnd, &f_ctx_menu2);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
}

int
Filer::context_menu (FilerView *v)
{
  int i = lv_find_focused_item (v->fv_hwnd);
  if (i < 0)
    return 0;
  RECT r;
  ListView_GetItemRect (v->fv_hwnd, i, &r, LVIR_BOUNDS);
  POINT p;
  p.x = r.left + 5;
  p.y = r.bottom;
  ClientToScreen (v->fv_hwnd, &p);
  return shell_context_menu (v->fv_hwnd, v->get_directory (),
                             p, id_hwnd, &f_ctx_menu2);
}

BOOL
Filer::Notify (NMHDR *nm)
{
  switch (nm->idFrom)
    {
    case IDC_LIST1:
    case IDC_LIST2:
      switch (nm->code)
        {
        case LVN_COLUMNCLICK:
          if (nm->idFrom == IDC_LIST1)
            f_fv1.sort (-((NM_LISTVIEW *)nm)->iSubItem - 1);
          else
            f_fv2.sort (-((NM_LISTVIEW *)nm)->iSubItem - 1);
          return 1;

        case LVN_PROCESSKEY:
          SetWindowLong (id_hwnd, DWL_MSGRESULT,
                         process_keys ((LV_PROCESSKEY *)nm));
          return 1;

        case NM_DBLCLK:
          SetDlgItemText (id_hwnd, IDC_NAME, "");
          PostMessage (id_hwnd, WM_COMMAND, IDOK, 0);
          return 1;

        case NM_SETFOCUS:
          if (dual_window_p ())
            {
              if (nm->idFrom == IDC_LIST1)
                swap_windows (&f_fv1);
              else
                swap_windows (&f_fv2);
            }
          return 1;

        case LVN_BEGINRDRAG:
          try
            {
              lisp path = (nm->idFrom == IDC_LIST1
                           ? f_fv1.get_directory ()
                           : f_fv2.get_directory ());
              if (drag_file_name (nm->hwndFrom, path,
                                  ((NM_LISTVIEW *)nm)->iItem))
                subscribe_reload (path, 1);
            }
          catch (nonlocal_jump &)
            {
              print_condition (nonlocal_jump::data ());
            }
          SetTimer (id_hwnd, TID_RELOAD, 1000, 0);
          return 1;

        case NM_RCLICK:
          context_menu (nm);
          return 1;

        case LVN_GETDISPINFO:
          if (nm->idFrom == IDC_LIST1)
            f_fv1.dispinfo (&((LV_DISPINFO *)nm)->item);
          else
            f_fv2.dispinfo (&((LV_DISPINFO *)nm)->item);
          return 1;

        case LVN_ITEMCHANGED:
          item_changed ((NM_LISTVIEW *)nm);
          return 1;
        }
      break;
    }
  return 0;
}

BOOL
Filer::Command (WPARAM wparam, LPARAM lparam)
{
  switch (LOWORD (wparam))
    {
    case IDOK:
      dispatch (CC_RET);
      return 1;

    case IDCANCEL:
      close (Qnil);
      return 1;
    }
  return 0;
}

int
Filer::adjust_control (int id, int right, int nomove)
{
  RECT r;
  HWND hwnd = GetDlgItem (id_hwnd, id);
  GetWindowRect (hwnd, &r);
  MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r, 2);
  int width, left;
  if (nomove)
    {
      left = r.left;
      width = right - left;
    }
  else
    {
      width = r.right - r.left;
      left = right - width;
    }
  MoveWindow (hwnd, left, r.top, width, r.bottom - r.top, 1);
  return left;
}

void
Filer::Size (int w, int h)
{
  if (!w)
    return;

  RECT r;
  GetClientRect (f_hwnd_status, &r);
  h -= r.bottom;

  if (!f_viewer_on)
    f_vwindow.resize (0, 0, 0, 0);

  if (!dual_window_p ())
    {
      GetWindowRect (f_fv1.fv_hwnd, &r);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r, 2);
      int spc = r.left;
      int cx = max (w - spc * 2, 0);
      int cy = h - r.top;
      if (f_viewer_on)
        {
          MoveWindow (f_fv1.fv_hwnd, r.left, r.top, cx, cy / 2 - 1, 1);
          f_vwindow.resize (r.left, r.top + cy / 2 + 1, cx, cy - (cy / 2 + 1));
        }
      else
        MoveWindow (f_fv1.fv_hwnd, r.left, r.top, cx, cy, 1);
      w -= spc;
      adjust_control (IDOK, w, 0);
      w = adjust_control (IDCANCEL, w, 0) - spc;
      adjust_control (IDC_NAME, w, 1);
      adjust_control (IDC_MASK1, w, 1);
      adjust_control (IDC_MARKS1, w, 1);
    }
  else
    {
      if (!f_guide_height)
        {
          HFONT hf = HFONT (SendMessage (id_hwnd, WM_GETFONT, 0, 0));
          HDC hdc = GetDC (id_hwnd);
          HGDIOBJ of = SelectObject (hdc, hf);
          TEXTMETRIC tm;
          GetTextMetrics (hdc, &tm);
          f_guide_height = tm.tmExternalLeading + tm.tmHeight;
          SelectObject (hdc, of);
          ReleaseDC (id_hwnd, hdc);
        }

      f_guide_area.left = 5;
      f_guide_area.right = w - 5;
      f_guide_area.bottom = h;
      f_guide_area.top = h - f_guide_height * f_guide_nlines;
      InvalidateRect (id_hwnd, &f_guide_area, 1);

      h = f_guide_area.top;

      RECT r1, r2;

      GetWindowRect (f_fv1.fv_hwnd, &r1);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r1, 2);
      GetWindowRect (f_fv2.fv_hwnd, &r2);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r2, 2);
      int b = (r2.left - r1.right) * 2;
      int cx = (w - b) / 2;
      int cy = h - r1.top;
      if (f_viewer_on == 1)
        {
          MoveWindow (f_fv1.fv_hwnd, 0, r1.top, cx, cy / 2 - 1, 1);
          f_vwindow.resize (0, r1.top + cy / 2 + 1, cx, cy - (cy / 2 + 1));
        }
      else
        MoveWindow (f_fv1.fv_hwnd, 0, r1.top, cx, cy, 1);
      cx = w - (w + b) / 2;
      cy = h - r2.top;
      if (f_viewer_on == 2)
        {
          int x = (w + b) / 2;
          MoveWindow (f_fv2.fv_hwnd, x, r2.top, cx, cy / 2 - 1, 1);
          f_vwindow.resize (x, r2.top + cy / 2 + 1, cx, cy - (cy / 2 + 1));
        }
      else
        MoveWindow (f_fv2.fv_hwnd, (w + b) / 2, r2.top, cx, cy, 1);

      GetWindowRect (f_fv1.fv_hwnd_mask, &r1);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r1, 2);
      MoveWindow (f_fv1.fv_hwnd_mask, 0, r1.top, w / 2 - b, r1.bottom - r1.top, 1);
      MoveWindow (f_fv2.fv_hwnd_mask, w / 2, r1.top, w / 2 - b, r1.bottom - r1.top, 1);

      GetWindowRect (f_fv1.fv_hwnd_path, &r1);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r1, 2);
      MoveWindow (f_fv1.fv_hwnd_path, 0, r1.top, w / 2 - b, r1.bottom - r1.top, 1);
      MoveWindow (f_fv2.fv_hwnd_path, w / 2, r1.top, w / 2 - b, r1.bottom - r1.top, 1);

      GetWindowRect (f_fv1.fv_hwnd_marks, &r1);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r1, 2);
      MoveWindow (f_fv1.fv_hwnd_marks, 0, r1.top, w / 2 - b, r1.bottom - r1.top, 1);
      MoveWindow (f_fv2.fv_hwnd_marks, w / 2, r1.top, w / 2 - b, r1.bottom - r1.top, 1);

      int i = lv_find_focused_item (f_fv1.fv_hwnd);
      ListView_EnsureVisible (f_fv1.fv_hwnd, i, 0);

      i = lv_find_focused_item (f_fv2.fv_hwnd);
      ListView_EnsureVisible (f_fv2.fv_hwnd, i, 0);

      int x[2];
      x[0] = w * 2 / 3;
      x[1] = -1;
      SendMessage (f_hwnd_status, SB_SETPARTS, 2, LPARAM (x));
      show_disk_info (f_pview);
    }
}

static void
paint_text (HDC hdc, lisp string, const RECT &r)
{
  if (!stringp (string))
    return;

  char *s = (char *)alloca (w2sl (string) + 1);
  w2s (s, string);
  ExtTextOut (hdc, r.left, r.top, ETO_CLIPPED | ETO_OPAQUE,
              &r, s, strlen (s), 0);
}

void
Filer::Paint ()
{
  PAINTSTRUCT ps;
  HDC hdc = BeginPaint (id_hwnd, &ps);
  if (f_guide_height)
    {
      HFONT hf = HFONT (SendMessage (id_hwnd, WM_GETFONT, 0, 0));
      HGDIOBJ of = SelectObject (hdc, hf);
      int obk = SetBkColor (hdc, sysdep.btn_face);

      if (stringp (f_lguide_text))
        paint_text (hdc, f_lguide_text, f_guide_area);
      else
        {
          RECT r (f_guide_area);
          int nlines = 0;
          for (lisp p = f_lguide_text; consp (p) && nlines < f_guide_nlines;
               p = xcdr (p), nlines++, r.top = r.bottom)
            {
              r.bottom = r.top + f_guide_height;
              paint_text (hdc, xcar (p), r);
            }
        }
      SetBkColor (hdc, obk);
      SelectObject (hdc, of);
    }
  EndPaint (id_hwnd, &ps);
}

BOOL
Filer::GetMinMaxInfo (MINMAXINFO *mmi)
{
  RECT r, dr;
  if (dual_window_p ())
    {
      GetWindowRect (GetDlgItem (id_hwnd, IDC_NAME), &r);
      MapWindowPoints (HWND_DESKTOP, id_hwnd, (POINT *)&r + 1, 1);
      mmi->ptMinTrackSize.x = r.right + 50;
    }
  else
    {
      GetWindowRect (GetDlgItem (id_hwnd, IDOK), &r);
      mmi->ptMinTrackSize.x = (r.right - r.left) * 4;
    }

  GetWindowRect (id_hwnd, &dr);
  GetWindowRect (GetDlgItem (id_hwnd, IDC_LIST1), &r);
  mmi->ptMinTrackSize.y = (dr.bottom - dr.top) - (r.bottom - r.top) + 50;
  SetWindowLong (id_hwnd, DWL_MSGRESULT, 0);
  return 1;
}

void
Filer::set_colors () const
{
  f_fv1.set_colors ();
  if (dual_window_p ())
    f_fv2.set_colors ();
}

void
Filer::modify_colors ()
{
  if (f_mlfiler)
    f_mlfiler->set_colors ();
  for (const Filer *p = f_chain; p; p = p->f_last)
    p->set_colors ();
}

void
Filer::save_geometry () const
{
  int posp, sizep;
  save_geometry_p (posp, sizep);
  conf_save_geometry (id_hwnd, cfgFiler,
                      dual_window_p () ? 0 : "s", posp, sizep);
  flush_conf ();
}

BOOL
Filer::WndProc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      return InitDialog ();

    case WM_NOTIFY:
      return Notify ((NMHDR *)lparam);

    case WM_DESTROY:
      KillTimer (id_hwnd, TID_GC);
      KillTimer (id_hwnd, TID_RELOAD);
      KillTimer (id_hwnd, TID_IDLE);
      if (dual_window_p ())
        {
          xsymbol_value (Vfiler_left_window_p) = boole (left_window_p ());
          xsymbol_value (Vfiler_primary_directory) = pv ()->get_directory ();
          xsymbol_value (Vfiler_primary_file_mask) = pv ()->get_file_mask ();
          xsymbol_value (Vfiler_secondary_directory) = sv ()->get_directory ();
          xsymbol_value (Vfiler_secondary_file_mask) = sv ()->get_file_mask ();
        }
      else
        xsymbol_value (Vfiler_last_file_mask) = pv ()->get_file_mask ();
      f_fv1.save_column ();
      if (dual_window_p ())
        f_fv2.save_column ();
      save_geometry ();
      if (IsWindowEnabled (app.toplev))
        SetActiveWindow (app.toplev);
      return 1;

    case WM_NCDESTROY:
      id_hwnd = 0;
      return 1;

    case WM_GETMINMAXINFO:
      return GetMinMaxInfo ((MINMAXINFO *)lparam);

    case WM_SIZE:
      SendMessage (f_hwnd_status, msg, wparam, lparam);
      Size (LOWORD (lparam), HIWORD (lparam));
      return 0;

    case WM_PAINT:
      Paint ();
      return 0;

    case WM_COMMAND:
      return Command (wparam, lparam);

    case WM_PRIVATE_QUIT:
      xsymbol_value (Vquit_flag) = Qnil;
      return 1;

    case WM_ACTIVATE:
      if (LOWORD (wparam) != WA_INACTIVE)
        {
          f_mlactive = f_mlfiler == this;
          app.status_window.set (f_hwnd_status);
          f_pview->echo_filename ();
        }
      return 0;

    case WM_ACTIVATEAPP:
      PostThreadMessage (app.quit_thread_id, WM_PRIVATE_ACTIVATEAPP,
                         wparam, lparam);
      return 0;

    case WM_TIMER:
      switch (wparam)
        {
        case TID_RELOAD:
          KillTimer (id_hwnd, TID_RELOAD);
          end_dispatch ();
          break;

        case TID_IDLE:
          KillTimer (id_hwnd, TID_IDLE);
          f_idle_timer = 0;
          f_pview->show_marks ();
          f_pview->echo_filename ();
          if (dual_window_p ())
            f_sview->show_marks ();
          break;
        }
      return 1;

    case WM_SYSCOLORCHANGE:
      set_colors ();
      SendMessage (f_hwnd_status, msg, wparam, lparam);
      return 0;

    case WM_PRIVATE_FILER_SHOW_MARKS:
      if (wparam == IDC_LIST1)
        f_fv1.show_marks (1);
      else
        f_fv2.show_marks (1);
      return 1;

    case WM_PRIVATE_FILER_KEYUP:
      do_keyup ();
      return 1;

    case WM_INITMENUPOPUP:
    case WM_MEASUREITEM:
      if (f_ctx_menu2)
        return f_ctx_menu2->HandleMenuMsg (msg, wparam, lparam) == NOERROR;
      return 0;

    case WM_DRAWITEM:
      if (f_ctx_menu2 && ((DRAWITEMSTRUCT *)lparam)->CtlType == ODT_MENU)
        return f_ctx_menu2->HandleMenuMsg (msg, wparam, lparam) == NOERROR;
      return app.status_window.paint ((DRAWITEMSTRUCT *)lparam);

    default:
      return 0;
    }
}

void
Filer::close_mlfiler ()
{
  if (!f_mlfiler)
    return;
  HWND hwnd = f_mlfiler->id_hwnd;
  f_mlfiler->EndDialog (IDCANCEL);
  SendMessage (hwnd, WM_NULL, 0, 0);
}

int
Filer::check_idle ()
{
  if (f_idle_timer)
    return 0;
  MSG msg;
  if (PeekMessage (&msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_NOREMOVE))
    {
      f_idle_timer = 1;
      SetTimer (id_hwnd, TID_IDLE, 300, 0);
      return 0;
    }
  return 1;
}

void
Filer::show_disk_info (const FilerView *v) const
{
  if (dual_window_p () && primary_window_p (v))
    v->display_disk_info (f_hwnd_status, 1);
}

lisp
Filer::get_text ()
{
  HWND hwnd = GetDlgItem (id_hwnd, IDC_NAME);
  int l = GetWindowTextLength (hwnd);
  if (!l)
    return Qnil;
  char *b = (char *)alloca (l + 2);
  GetWindowText (hwnd, b, l + 1);
  return make_string (b);
}

void
Filer::set_text (lisp string)
{
  check_string (string);
  char *b = (char *)alloca (xstring_length (string) * 2 + 1);
  w2s (b, string);
  SetDlgItemText (id_hwnd, IDC_NAME, b);
}

int
Filer::swap_windows (FilerView *fv)
{
  if (fv == f_pview)
    {
      SetFocus (fv->fv_hwnd);
      return 0;
    }
  if (!dual_window_p ())
    FEsimple_error (Efiler_is_not_in_dual_window_mode);
  swap (f_pview, f_sview);
  f_pview->activate (1);
  f_sview->activate (0);
  SetFocus (f_pview->fv_hwnd);
  show_disk_info (f_pview);
  item_changed (0);
  return 1;
}

void
Filer::subscribe_reload (lisp path, int subdirp) const
{
  f_pview->subscribe_reload (path, subdirp);
  if (dual_window_p ())
    f_sview->subscribe_reload (path, subdirp);
}

int
Filer::activate_modeless ()
{
  if (!f_mlfiler)
    return 0;
  SetActiveWindow (f_mlfiler->id_hwnd);
  if (IsIconic (f_mlfiler->id_hwnd))
    ShowWindow (f_mlfiler->id_hwnd, SW_RESTORE);
  return 1;
}

inline int
Filer::ancestor_p (HWND hwnd) const
{
  return hwnd == id_hwnd || GetParent (hwnd) == id_hwnd;
}

int
Filer::filer_ancestor_p (HWND hwnd)
{
  if (f_mlfiler && f_mlfiler->ancestor_p (hwnd))
    return 1;
  for (const Filer *p = f_chain; p; p = p->f_last)
    if (p->ancestor_p (hwnd))
      return 1;
  return 0;
}

int
Filer::IsDialogMessage (MSG *msg)
{
  if (msg->hwnd == f_fv1.fv_hwnd
      || msg->hwnd == f_fv2.fv_hwnd)
    switch (msg->message)
      {
      case WM_KEYDOWN:
        switch (msg->wParam)
          {
          case VK_ESCAPE:
            break;

          case VK_RETURN:
            if (GetKeyState (VK_MENU) < 0
                || GetKeyState (VK_SHIFT) < 0
                || GetKeyState (VK_CONTROL) < 0)
              return 0;
            break;

          case CC_BS:
            return 0;
          }
        break;

      case WM_SYSCHAR:
        if (!dual_window_p () && (msg->wParam == 'F' || msg->wParam == 'f'))
          break;
        return 0;

      case WM_SYSKEYDOWN:
        return 0;
      }
  return IdleDialog::IsDialogMessage (msg);
}

lChar
Filer::read_char () const
{
  while (1)
    {
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (msg.wParam);
          return lChar_EOF;
        }
      switch (msg.message)
        {
          lChar cc;
        case WM_KEYDOWN:
          cc = decode_keys (msg.wParam, msg.lParam);
          if (cc != lChar_EOF)
            return cc;
          XyzzyTranslateMessage (&msg);
          break;

        case WM_SYSKEYDOWN:
          cc = decode_syskeys (msg.wParam, msg.lParam);
          if (cc != lChar_EOF)
            return cc;
          XyzzyTranslateMessage (&msg);
          break;

        case WM_CHAR:
          return decode_chars (msg.wParam);

        case WM_SYSCHAR:
          return decode_syschars (msg.wParam);

        case WM_PRIVATE_QUIT:
          if (msg.hwnd == app.toplev && GetActiveWindow () == id_hwnd)
            {
              xsymbol_value (Vquit_flag) = Qnil;
              return xchar_code (app.lquit_char);
            }
          DispatchMessage (&msg);
          QUIT;
          break;

        case WM_LBUTTONDOWN:
        case WM_LBUTTONUP:
        case WM_LBUTTONDBLCLK:
        case WM_MBUTTONDOWN:
        case WM_MBUTTONUP:
        case WM_MBUTTONDBLCLK:
        case WM_RBUTTONDOWN:
        case WM_RBUTTONUP:
        case WM_RBUTTONDBLCLK:
        case WM_XBUTTONDOWN:
        case WM_XBUTTONUP:
        case WM_XBUTTONDBLCLK:
        case WM_MOUSEMOVE:
        case WM_MOUSEWHEEL:
          break;

        default:
          XyzzyTranslateMessage (&msg);
          DispatchMessage (&msg);
          break;
        }
    }
}

///////////////////////////////////////////////////////////////
// LISP Functions
///////////////////////////////////////////////////////////////

lisp
Ffiler_forward_line (lisp arg, lisp v)
{
  return make_fixnum (Filer::view (v)->forward_line ((!arg || arg == Qnil)
                                                     ? 1 : fixnum_value (arg)));
}

lisp
Ffiler_forward_page (lisp arg, lisp v)
{
  return make_fixnum (Filer::view (v)->forward_page ((!arg || arg == Qnil)
                                                     ? 1 : fixnum_value (arg)));
}

lisp
Ffiler_goto_bof (lisp v)
{
  return boole (Filer::view (v)->goto_bof ());
}

lisp
Ffiler_goto_eof (lisp v)
{
  return boole (Filer::view (v)->goto_eof ());
}

lisp
Ffiler_scroll_left (lisp v)
{
  return boole (Filer::view (v)->scroll_left ());
}

lisp
Ffiler_scroll_right (lisp v)
{
  return boole (Filer::view (v)->scroll_right ());
}

lisp
Ffiler_reload (lisp lmask, lisp v)
{
  if (lmask && lmask != Qnil)
    check_string (lmask);
  Filer::view (v)->reload (lmask);
  return Qt;
}

lisp
Ffiler_set_directory (lisp dir, lisp v)
{
  return boole (Filer::view (v)->set_directory (dir));
}

lisp
Ffiler_get_directory (lisp v)
{
  return Filer::view (v)->get_directory ();
}

lisp
Ffiler_get_drive (lisp v)
{
  return Filer::view (v)->get_drive ();
}

lisp
Ffiler_mark (lisp fo, lisp v)
{
  return boole (Filer::view (v)->mark (fo && fo != Qnil));
}

lisp
Ffiler_mark_all (lisp fo, lisp v)
{
  return make_fixnum (Filer::view (v)->mark_all (fo && fo != Qnil));
}

lisp
Ffiler_toggle_mark (lisp fo, lisp v)
{
  return boole (Filer::view (v)->toggle_mark (fo && fo != Qnil));
}

lisp
Ffiler_toggle_all_marks (lisp fo, lisp v)
{
  return make_fixnum (Filer::view (v)->toggle_all_marks (fo && fo != Qnil));
}

lisp
Ffiler_close (lisp result)
{
  Filer::current_filer ()->close (result);
  return Qt;
}

lisp
Ffiler_modal_p ()
{
  return boole (!Filer::current_filer ()->modeless_p ());
}

lisp
Ffiler_get_mark_files (lisp fo, lisp v)
{
  return Filer::view (v)->get_mark_files (fo && fo != Qnil);
}

lisp
Ffiler_get_current_file (lisp v)
{
  return Filer::view (v)->get_current_file ();
}

lisp
Ffiler_current_file_dot_dot_p (lisp v)
{
  return boole (Filer::view (v)->current_file_dot_dot_p ());
}

lisp
Ffiler_current_file_directory_p (lisp v)
{
  return boole (Filer::view (v)->current_file_directory_p ());
}

lisp
Ffiler_get_text ()
{
  return Filer::current_filer ()->get_text ();
}

lisp
Ffiler_set_text (lisp string)
{
  Filer::current_filer ()->set_text (string);
  return Qt;
}

lisp
Ffiler_isearch (lisp cc, lisp nowrap, lisp v)
{
  if (!cc || cc == Qnil)
    cc = xsymbol_value (Vfiler_last_command_char);
  check_char (cc);
  return boole (Filer::view (v)->isearch (xchar_code (cc), nowrap != Qnil));
}

lisp
Ffiler_set_file_mask (lisp masks, lisp v)
{
  Filer::view (v)->set_file_mask (masks);
  return Qt;
}

lisp
Ffiler_mark_match_files (lisp mask, lisp v)
{
  return make_fixnum (Filer::view (v)->mark_match_files (mask));
}

lisp
Ffiler_clear_all_marks (lisp v)
{
  Filer::view (v)->clear_all_marks ();
  return Qt;
}

lisp
Ffiler_count_marks (lisp fo, lisp v)
{
  return make_fixnum (Filer::view (v)->count_marks (fo && fo != Qnil));
}

lisp
Ffiler_sort (lisp arg, lisp v)
{
  Filer::view (v)->sort (fixnum_value (arg));
  return Qt;
}

lisp
Ffiler_get_sort_order (lisp v)
{
  return make_fixnum (Filer::view (v)->get_sort ());
}

lisp
Ffiler_dual_window_p ()
{
  return boole (Filer::current_filer ()->dual_window_p ());
}

lisp
Ffiler_left_window ()
{
  return boole (Filer::current_filer ()->left_window ());
}

lisp
Ffiler_right_window ()
{
  return boole (Filer::current_filer ()->right_window ());
}

lisp
Ffiler_left_window_p ()
{
  return boole (Filer::current_filer ()->left_window_p ());
}

lisp
Ffiler_subscribe_to_reload (lisp path, lisp subdirp)
{
  Filer::current_filer ()->subscribe_reload (path, subdirp && subdirp != Qnil);
  return Qt;
}

lisp
Ffiler_demand_reload ()
{
  Filer::current_filer ()->demand_reload ();
  return Qt;
}

lisp
Ffiler_calc_directory_size (lisp keep_marks)
{
  Filer::view (Qnil)->calc_directory_size (0);
  if (!keep_marks || keep_marks == Qnil)
    Filer::view (Qnil)->clear_all_marks ();
  return Qt;
}

lisp
Ffiler_calc_directory_byte_size (lisp keep_marks)
{
  Filer::view (Qnil)->calc_directory_size (1);
  if (!keep_marks || keep_marks == Qnil)
    Filer::view (Qnil)->clear_all_marks ();
  return Qt;
}

lisp
Ffiler_goto_file (lisp string, lisp start, lisp reverse, lisp wild)
{
  return boole (Filer::pview ()->search (string, start, reverse, wild));
}

lisp
Ffiler_modify_column_width (lisp n, lisp d, lisp v)
{
  return boole (Filer::view (v)->modify_column_width (fixnum_value (n), fixnum_value (d)));
}

lisp
Ffiler_context_menu ()
{
  return boole (Filer::current_filer ()->context_menu (Filer::view (Qnil)));
}

lisp
Ffiler_swap_windows ()
{
  Filer *f = Filer::current_filer ();
  if (!f->dual_window_p ())
    return Qnil;
  f->swap_windows (f->sview ());
  return Qt;
}

lisp
Ffiler_read_char ()
{
  lChar cc = Filer::current_filer ()->read_char ();
  return cc != lChar_EOF ? make_char (Char (cc)) : Qnil;
}

static void
filer_init_mask (const Filer &f)
{
  if (f.dual_window_p ())
    {
      f.pv ()->set_file_mask (xsymbol_value (Vfiler_primary_file_mask));
      f.sv ()->set_file_mask (xsymbol_value (Vfiler_secondary_file_mask));
    }
  else
    f.pv ()->set_file_mask (xsymbol_value (Vfiler_last_file_mask));
}

static int
filer_idd (lisp dual, int modeless)
{
  if (modeless)
    return IDD_FILER_DUAL_MODELESS;
  if (dual != Qnil)
    return IDD_FILER_DUAL;
  if (GetSystemMetrics (SM_CYSCREEN) < 600)
    return IDD_FILER_SMALL;
  return IDD_FILER;
}

lisp
Ffiler (lisp path, lisp multi, lisp title, lisp dual, lisp lmodeless)
{
  int modeless = lmodeless && lmodeless != Qnil;
  if (modeless)
    dual = Qt;
  if (!dual)
    dual = xsymbol_value (Vfiler_dual_window);

  if (modeless && Filer::activate_modeless ())
    return Qt;

  lisp dir, name;
  if (!path || path == Qnil)
    {
      dir = selected_buffer ()->ldirectory;
      name = Qnil;
    }
  else
    {
      path = Fnamestring (path);
      if (Ffile_directory_p (path) != Qnil)
        {
          dir = path;
          name = Qnil;
        }
      else
        {
          name = Ffile_namestring (path);
          if (xstring_length (name) && Fwild_pathname_p (name) == Qnil)
            return multi == Qnil ? path : xcons (path, Qnil);
          dir = Fdirectory_namestring (path);
        }
    }

  dir = Fappend_trail_slash (Fmap_backslash_to_slash (dir));
  if (Ffile_directory_p (dir) == Qnil)
    dir = xsymbol_value (Qdefault_dir);

  if (!title || title == Qnil)
    title = Qnil;
  else
    check_string (title);

  if (!multi)
    multi = Qnil;

  lisp leftp = xsymbol_value (Vfiler_left_window_p);

  lisp pdir, sdir;
  if (dual == Qnil)
    pdir = sdir = dir;
  else
    {
      pdir = xsymbol_value (Vfiler_primary_directory);
      if (!stringp (pdir) || Ffile_directory_p (pdir) == Qnil)
        pdir = dir;
      pdir = Fappend_trail_slash (Fmap_backslash_to_slash (pdir));
      sdir = xsymbol_value (Vfiler_secondary_directory);
      if (!stringp (sdir) || Ffile_directory_p (sdir) == Qnil)
        sdir = dir;
      sdir = Fappend_trail_slash (Fmap_backslash_to_slash (sdir));
    }

  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  if (wp->minibuffer_window_p () && bufferp (bp->lminibuffer_buffer)
      && xbuffer_bp (bp->lminibuffer_buffer))
    bp = xbuffer_bp (bp->lminibuffer_buffer);

  int r;
  if (!modeless)
    {
      enable_quit::disable ();
      Filer filer (pdir, sdir, name, bp->lfile_name, multi, title, dual, leftp, 0);
      dynamic_bind dynb (Vsi_condition_handlers, Qnil);
      filer_init_mask (filer);
      r = filer.DoModal (get_active_window (), filer_idd (dual, 0));
      Fdo_events ();
      if (r != IDOK)
        QUIT;
      return filer.result ();
    }
  else
    {
      Filer *filer = new Filer (pdir, sdir, name, bp->lfile_name,
                                multi, title, dual, leftp, 1);
      filer_init_mask (*filer);
      return boole (filer->Create (0, filer_idd (dual, 1)));
    }
}

int ViewerWindow::vw_initialized;
const char ViewerWindow::vw_classname[] = "viewer";

static inline void
set_window (HWND hwnd, ViewerWindow *wp)
{
  SetWindowLong (hwnd, 0, LONG (wp));
}

static inline ViewerWindow *
get_window (HWND hwnd)
{
  return (ViewerWindow *)GetWindowLong (hwnd, 0);
}

static LRESULT CALLBACK
vw_wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  ViewerWindow *wp;
  switch (msg)
    {
    case WM_CREATE:
      {
        CREATESTRUCT *cs = (CREATESTRUCT *)lparam;
        wp = (ViewerWindow *)cs->lpCreateParams;
        wp->w_hwnd = hwnd;
        set_window (hwnd, wp);
        return 0;
      }

    case WM_ERASEBKGND:
      get_window (hwnd)->paint_background (HDC (wparam));
      return 1;

    case WM_PAINT:
      get_window (hwnd)->update_window ();
      return 0;

    case WM_SIZE:
      if (wparam != SIZE_MINIMIZED)
        {
          wp = get_window (hwnd);
          if (wp)
            wp->update (LOWORD (lparam), HIWORD (lparam));
        }
      break;
    }
  return DefWindowProc (hwnd, msg, wparam, lparam);
}

ViewerWindow::ViewerWindow ()
     : Window (0, 1)
{
  w_hwnd = 0;
  w_hwnd_ml = 0;
  w_bufp = 0;
  w_colors = default_colors;
  w_flags_mask = 0;
  w_flags = WF_NEWLINE | WF_EOF;
  w_cursor_line.ypixel = -1;

  if (!vw_initialized)
    {
      WNDCLASS wc;
      wc.style = 0;
      wc.lpfnWndProc = vw_wndproc;
      wc.cbClsExtra = 0;
      wc.cbWndExtra = sizeof (ViewerWindow *);
      wc.hInstance = app.hinst;
      wc.hIcon = 0;
      wc.hCursor = sysdep.hcur_arrow;
      wc.hbrBackground = 0;
      wc.lpszMenuName = 0;
      wc.lpszClassName = vw_classname;
      if (RegisterClass (&wc))
        vw_initialized = 1;
    }
}

ViewerWindow::~ViewerWindow ()
{
  w_hwnd = 0;
  w_hwnd_ml = 0;
}

int
ViewerWindow::init (HWND parent, ViewerBuffer *bp)
{
  w_bufp = bp;
  w_point.p_point = 0;
  w_point.p_chunk = bp->b_chunkb;
  w_point.p_offset = 0;
  return (int)CreateWindowEx (sysdep.Win4p () ? WS_EX_CLIENTEDGE : 0,
                              vw_classname, "",
                              WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE,
                              0, 0, 0, 0, parent, 0, app.hinst, this);
}

void
ViewerWindow::resize (int x, int y, int w, int h)
{
  w_rect.left = x;
  w_rect.top = y;
  w_rect.right = x + w;
  w_rect.bottom = y + h;
  MoveWindow (w_hwnd, x, y, w, h, 1);
}

ViewerBuffer::ViewerBuffer ()
     : Buffer (Qnil, Qnil, Qnil, 1)
{
  b_fold_columns = 80;
  b_nfolded = -1;
}

void
ViewerBuffer::clean (ViewerWindow *wp)
{
  b_nchars = 0;
  b_chunkb->c_used = 0;
  b_contents.p2 = 0;
  wp->w_flags = 0;
}

int
ViewerBuffer::readin (ViewerWindow *wp, const char *path)
{
  clean (wp);
  if (path)
    {
      ReadFileContext rfc;
      bzero (&rfc, sizeof rfc);
      readin_chunk (rfc, path);
      if (rfc.r_status == ReadFileContext::RFCS_IOERR)
        {
          if (rfc.r_chunk)
            {
              free_all_chunks (rfc.r_chunk);
              rfc.r_chunk = 0;
            }
          return 0;
        }
      else if (rfc.r_chunk)
        {
          free_all_chunks (b_chunkb);
          b_chunkb = rfc.r_chunk;
          b_chunke = rfc.r_tail;
          b_nchars = rfc.r_nchars;
          b_contents.p1 = 0;
          b_contents.p2 = rfc.r_nchars;
          wp->w_point.p_chunk = b_chunkb;
          wp->w_flags = Window::WF_NEWLINE | Window::WF_EOF;
        }
    }

  wp->repaint ();
  return 1;
}

void
ViewerWindow::repaint ()
{
  if (!w_glyphs.g_rep || !w_point.p_chunk)
    return;
  Point p = w_point;
  redraw_window (p, 1, 1, 0);
  HDC hdc = GetDC (w_hwnd);
  paint_window (hdc);
  ReleaseDC (w_hwnd, hdc);
  w_disp_flags = 0;
}

void
ViewerWindow::update_window ()
{
  if (!w_glyphs.g_rep || w_disp_flags)
    {
      w_disp_flags = 0;
      PAINTSTRUCT ps;
      BeginPaint (w_hwnd, &ps);
      EndPaint (w_hwnd, &ps);
      if (w_glyphs.g_rep)
        {
          RECT r;
          discard_invalid_region (ps, r);
          repaint ();
        }
    }
  else
    Window::update_window ();
}

void
ViewerWindow::update (int w, int h)
{
  winsize_changed (w, h);
  calc_client_size (w - RIGHT_PADDING, h);
  w_disp_flags = WDF_WINDOW;
}

void
Filer::item_changed (NM_LISTVIEW *l)
{
  if (l && l->uChanged & LVIF_STATE
      && l->uNewState & LVIS_SELECTED
      && !(l->uOldState & LVIS_SELECTED)
      && l->lParam && !*((filer_data *)l->lParam)->name)
    ListView_SetItemState (l->hdr.hwndFrom, l->iItem, 0, LVIS_SELECTED);

  if (f_viewer_on)
    {
      if (l && l->uChanged & LVIF_STATE
          && l->uNewState & LVIS_FOCUSED
          && !(l->uOldState & LVIS_FOCUSED))
        f_changed_view = l->hdr.idFrom == IDC_LIST1 ? &f_fv1 : &f_fv2;
      else
        f_changed_view = f_pview;
    }
  else
    f_changed_view = 0;
}

void
Filer::do_keyup ()
{
  FilerView *v = f_changed_view;
  if (!v)
    return;
  f_changed_view = 0;
  LV_ITEM lvi;
  lvi.iItem = lv_find_focused_item (v->fv_hwnd);
  lvi.iSubItem = 0;
  lvi.mask = LVIF_PARAM;
  if (lvi.iItem < 0 || !ListView_GetItem (v->fv_hwnd, &lvi))
    f_vbuffer.readin (&f_vwindow, 0);
  else
    {
      const filer_data *d = (filer_data *)lvi.lParam;
      if (d->attr & FILE_ATTRIBUTE_DIRECTORY)
        f_vbuffer.readin (&f_vwindow, 0);
      else
        {
          lisp dir = v->get_directory ();
          char *path = (char *)alloca (xstring_length (dir) * 2 + MAX_PATH + 1);
          strcpy (w2s (path, dir), d->name);
          try
            {
              f_vbuffer.readin (&f_vwindow, path);
            }
          catch (nonlocal_jump &)
            {
              print_condition (nonlocal_jump::data ());
            }
        }
    }
}

void
Filer::show_viewer ()
{
  f_viewer_on++;
  if (f_viewer_on == (dual_window_p () ? 3 : 2))
    f_viewer_on = 0;
  if (f_viewer_on == 1)
    item_changed (0);
  f_vbuffer.clean (&f_vwindow);
  RECT r;
  GetClientRect (id_hwnd, &r);
  Size (r.right, r.bottom);
}

lisp
Ffiler_viewer ()
{
  Filer::current_filer ()->show_viewer ();
  return Qt;
}
