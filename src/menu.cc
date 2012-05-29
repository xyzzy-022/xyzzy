#include "stdafx.h"
#include "ed.h"

#define xwin32_menu_items xwin32_menu_command

static u_long used_id[(MENU_ID_RANGE_MAX - MENU_ID_RANGE_MIN) / (sizeof (u_long) * 8)];

#ifndef MIIM_STRING
#define MIIM_STRING 0x00000040
#define MIIM_BITMAP 0x00000080
#define MIIM_FTYPE 0x00000100

#define HBMMENU_CALLBACK ((HBITMAP)-1)
#define HBMMENU_SYSTEM ((HBITMAP)1)
#define HBMMENU_MBAR_RESTORE ((HBITMAP)2)
#define HBMMENU_MBAR_MINIMIZE ((HBITMAP)3)
#define HBMMENU_MBAR_CLOSE ((HBITMAP)5)
#define HBMMENU_MBAR_CLOSE_D ((HBITMAP)6)
#define HBMMENU_MBAR_MINIMIZE_D ((HBITMAP)7)
#define HBMMENU_POPUP_CLOSE ((HBITMAP)8)
#define HBMMENU_POPUP_RESTORE ((HBITMAP)9)
#define HBMMENU_POPUP_MAXIMIZE ((HBITMAP)10)
#define HBMMENU_POPUP_MINIMIZE ((HBITMAP)11)
#endif

#if WINVER >= 0x0500
typedef MENUITEMINFO MENUITEMINFO5;
#else
struct MENUITEMINFO5: public tagMENUITEMINFOA
{
  HBITMAP hbmpItem;
};
#endif

lwin32_menu *
make_win32_menu ()
{
  lwin32_menu *p = ldata <lwin32_menu, Twin32_menu>::lalloc ();
  p->handle = 0;
  p->id = 0;
  p->init = Qnil;
  p->command = Qnil;
  p->tag = Qnil;
  return p;
}

lwin32_menu::~lwin32_menu ()
{
  if (handle && IsMenu (handle))
    {
      for (int i = GetMenuItemCount (handle) - 1; i >= 0; i--)
        RemoveMenu (handle, i, MF_BYPOSITION);
      DestroyMenu (handle);
    }
  if (id > MENU_ID_RANGE_MIN && id < MENU_ID_RANGE_MAX)
    bitclr (used_id, id - MENU_ID_RANGE_MIN);
}

static void
init_mii (MENUITEMINFO5 &m, UINT flags, UINT id, const char *name)
{
  if (sysdep.Win5p () || sysdep.Win98p ())
    {
      m.cbSize = sizeof m;
      m.fMask = MIIM_BITMAP | MIIM_FTYPE | MIIM_ID;
      m.fType = MFT_RIGHTJUSTIFY;
      m.wID = id;
      m.hbmpItem = HBMMENU_MBAR_CLOSE;
    }
  else
    {
      m.cbSize = offsetof (MENUITEMINFO5, hbmpItem);
      m.fMask = MIIM_ID | MIIM_TYPE;
      m.fType = MFT_RIGHTJUSTIFY | MFT_BITMAP;
      m.wID = id;
      m.dwTypeData = (char *)HBMMENU_MBAR_CLOSE;
    }
}

static int
append_menu (HMENU hmenu, UINT flags, UINT id, const char *name)
{
  if (flags & MF_BITMAP)
    {
      MENUITEMINFO5 m;
      init_mii (m, flags, id, name);
      return InsertMenuItem (hmenu, GetMenuItemCount (hmenu), 1, &m);
    }
  else
    return AppendMenu (hmenu, flags, id, name);
}

static int
insert_menu (HMENU hmenu, UINT pos, UINT flags, UINT id, const char *name)
{
  if (flags & MF_BITMAP)
    {
      MENUITEMINFO5 m;
      init_mii (m, flags, id, name);
      return InsertMenuItem (hmenu, pos, flags & MF_BYPOSITION, &m);
    }
  else
    return InsertMenu (hmenu, pos, flags, id, name);
}

lisp
Fcreate_menu (lisp tag)
{
  lisp lmenu = make_win32_menu ();
  HMENU hmenu = CreateMenu ();
  if (!hmenu)
    {
      protect_gc gcpro (lmenu);
      gc (1);
      hmenu = CreateMenu ();
      if (!hmenu)
        FEsimple_win32_error (GetLastError ());
    }
  xwin32_menu_handle (lmenu) = hmenu;
  xwin32_menu_tag (lmenu) = tag ? tag : Qnil;
  return lmenu;
}

lisp
Fcreate_popup_menu (lisp tag)
{
  lisp lmenu = make_win32_menu ();
  HMENU hmenu = CreatePopupMenu ();
  if (!hmenu)
    {
      protect_gc gcpro (lmenu);
      gc (1);
      hmenu = CreatePopupMenu ();
      if (!hmenu)
        FEsimple_win32_error (GetLastError ());
    }
  xwin32_menu_handle (lmenu) = hmenu;
  xwin32_menu_tag (lmenu) = tag ? tag : Qnil;
  return lmenu;
}

void
check_popup_menu (lisp lmenu)
{
  check_win32_menu (lmenu);
  if (!xwin32_menu_handle (lmenu))
    {
      if (!xwin32_menu_id (lmenu))
        FEprogram_error (Euninitialized_menu_item);
      FEprogram_error (Eis_not_popup_menu);
    }
}

static void
redraw_menu (lisp lmenu)
{
  if (xsymbol_value (Vlast_active_menu) == lmenu)
    DrawMenuBar (app.toplev);
}

static void
add_menu (lisp lmenu, lisp item, UINT flags, const char *name, UINT id)
{
  lisp new_items = xcons (item, xwin32_menu_items (lmenu));
  if (!append_menu (xwin32_menu_handle (lmenu), flags, id, name))
    {
      protect_gc gcpro (new_items);
      gc (1);
      if (!append_menu (xwin32_menu_handle (lmenu), flags, id, name))
        FEsimple_win32_error (GetLastError ());
    }
  xwin32_menu_items (lmenu) = new_items;
  redraw_menu (lmenu);
}

static void
add_menu (lisp lmenu, lisp item, lisp name, UINT flags, UINT id)
{
  char b[1024];
  w2s (b, b + sizeof b, xstring_contents (name), xstring_length (name));
  add_menu (lmenu, item, flags, b, id);
}

static lisp
create_new_item (int &id, lisp tag, lisp command, lisp init)
{
  bitset (used_id, 0);
  id = find_zero_bit (used_id, numberof (used_id));
  if (id < 0)
    {
      gc (1);
      id = find_zero_bit (used_id, numberof (used_id));
      if (id < 0)
        FEprogram_error (Etoo_many_menu_items);
    }
  id += MENU_ID_RANGE_MIN;

  lisp litem = make_win32_menu ();
  xwin32_menu_id (litem) = id;
  xwin32_menu_tag (litem) = tag;
  xwin32_menu_command (litem) = command ? command : Qnil;
  xwin32_menu_init (litem) = init ? init : Qnil;
  return litem;
}

lisp
Fadd_popup_menu (lisp lmenu, lisp lpopup, lisp name)
{
  check_popup_menu (lmenu);
  check_string (name);
  check_popup_menu (lpopup);
  add_menu (lmenu, lpopup, name, MF_POPUP | MF_STRING,
            UINT (xwin32_menu_handle (lpopup)));
  return lpopup;
}

lisp
Fadd_menu_item (lisp lmenu, lisp tag, lisp item, lisp command, lisp init)
{
  check_popup_menu (lmenu);
  if (item != Kclose_box)
    check_string (item);
  int id;
  lisp litem = create_new_item (id, tag, command, init);
  protect_gc gcpro (litem);
  if (item != Kclose_box)
    add_menu (lmenu, litem, item, MF_STRING, id);
  else
    add_menu (lmenu, litem, MF_BITMAP, 0, id);
  bitset (used_id, id - MENU_ID_RANGE_MIN);
  return litem;
}

lisp
Fadd_menu_separator (lisp lmenu, lisp tag)
{
  check_popup_menu (lmenu);
  lisp litem = make_win32_menu ();
  xwin32_menu_tag (litem) = tag ? tag : Qnil;
  protect_gc gcpro (litem);
  add_menu (lmenu, litem, MF_SEPARATOR, 0, 0);
  return litem;
}

static int
find_tag_position (lisp &lmenu, lisp tag)
{
  for (lisp p = xwin32_menu_items (lmenu); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (xwin32_menu_tag (x) == tag)
        return xlist_length (xcdr (p));
      if (xwin32_menu_handle (x))
        {
          int pos = find_tag_position (x, tag);
          if (pos >= 0)
            {
              lmenu = x;
              return pos;
            }
        }
    }
  return -1;
}

lisp
Fget_menu_position (lisp lmenu, lisp tag)
{
  check_popup_menu (lmenu);
  int pos = find_tag_position (lmenu, tag);
  if (pos < 0)
    return Qnil;
  multiple_value::count () = 2;
  multiple_value::value (1) = lmenu;
  return make_fixnum (pos);
}

static void
insert_menu (lisp lmenu, int pos, lisp item, UINT flags, const char *name, UINT id)
{
  int l = xlist_length (xwin32_menu_items (lmenu));
  if (pos >= l)
    add_menu (lmenu, item, flags, name, id);
  else
    {
      lisp tem = xcons (item, Qnil);
      if (!InsertMenu (xwin32_menu_handle (lmenu), pos, MF_BYPOSITION | flags, id, name))
        {
          protect_gc gcpro (tem);
          gc (1);
          if (!InsertMenu (xwin32_menu_handle (lmenu), pos, MF_BYPOSITION | flags, id, name))
            FEsimple_win32_error (GetLastError ());
        }
      l -= pos;
      lisp p;
      for (p = xwin32_menu_items (lmenu); --l > 0; p = xcdr (p))
        assert (consp (p));
      assert (consp (p));
      xcdr (tem) = xcdr (p);
      xcdr (p) = tem;
      redraw_menu (lmenu);
    }
}

static void
insert_menu (lisp lmenu, int pos, lisp item, lisp name, UINT flags, UINT id)
{
  char b[1024];
  w2s (b, b + sizeof b, xstring_contents (name), xstring_length (name));
  insert_menu (lmenu, pos, item, flags, b, id);
}

lisp
Finsert_popup_menu (lisp lmenu, lisp position, lisp lpopup, lisp name)
{
  check_popup_menu (lmenu);
  check_string (name);
  check_popup_menu (lpopup);
  int pos = fixnum_value (position);
  if (pos < 0)
    FErange_error (position);
  insert_menu (lmenu, pos, lpopup, name, MF_POPUP | MF_STRING,
               UINT (xwin32_menu_handle (lpopup)));
  return lpopup;
}

lisp
Finsert_menu_item (lisp lmenu, lisp position, lisp tag, lisp item,
                   lisp command, lisp init)
{
  check_popup_menu (lmenu);
  if (item != Kclose_box)
    check_string (item);
  int pos = fixnum_value (position);
  if (pos < 0)
    FErange_error (position);
  int id;
  lisp litem = create_new_item (id, tag, command, init);
  protect_gc gcpro (litem);
  if (item != Kclose_box)
    insert_menu (lmenu, pos, litem, item, MF_STRING, id);
  else
    insert_menu (lmenu, pos, litem, MF_BITMAP, 0, id);
  bitset (used_id, id - MENU_ID_RANGE_MIN);
  return litem;
}

lisp
Finsert_menu_separator (lisp lmenu, lisp position, lisp tag)
{
  check_popup_menu (lmenu);
  int pos = fixnum_value (position);
  if (pos < 0)
    FErange_error (position);
  lisp litem = make_win32_menu ();
  xwin32_menu_tag (litem) = tag ? tag : Qnil;
  protect_gc gcpro (litem);
  insert_menu (lmenu, pos, litem, MF_SEPARATOR, 0, 0);
  return litem;
}

lisp
Fset_menu (lisp lmenu)
{
  if (lmenu != Qnil)
    check_popup_menu (lmenu);
  xsymbol_value (Vdefault_menu) = lmenu;
  return lmenu;
}

static lisp
get_menu (lisp lmenu, lisp tag, lisp positionp, int &pos)
{
  check_popup_menu (lmenu);
  if (positionp && positionp != Qnil)
    {
      pos = fixnum_value (tag);
      if (pos < 0)
        FErange_error (tag);
    }
  else
    {
      pos = find_tag_position (lmenu, tag);
      if (pos < 0)
        return Qnil;
    }

  int l = xlist_length (xwin32_menu_items (lmenu));
  if (pos >= l)
    return Qnil;

  l -= pos + 1;
  return Fnth (make_fixnum (l), xwin32_menu_items (lmenu));
}

lisp
Fget_menu (lisp lmenu, lisp tag, lisp positionp)
{
  int pos;
  return get_menu (lmenu, tag, positionp, pos);
}

lisp
Fdelete_menu (lisp lmenu, lisp tag, lisp positionp)
{
  int pos;
  lisp item = get_menu (lmenu, tag, positionp, pos);
  if (item != Qnil)
    {
      if (!RemoveMenu (xwin32_menu_handle (lmenu), pos, MF_BYPOSITION))
        FEsimple_win32_error (GetLastError ());

      redraw_menu (lmenu);

      delq (item, &xwin32_menu_items (lmenu));
    }
  return item;
}

int
init_menu_flags (lisp item)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  if (!bp)
    return MF_GRAYED | MF_UNCHECKED;
  if (symbolp (item))
    {
#define MF(X) ((X) ? MF_UNCHECKED | MF_ENABLED : MF_UNCHECKED | MF_GRAYED)
      if (item == Kmodified)
        return MF (bp->b_modified);
      if (item == Kundo)
        return MF (bp->b_undo && !bp->read_only_p ());
      if (item == Kredo)
        return MF (bp->b_redo && !bp->read_only_p ());
#define SELECTIONP(X) \
  ((wp->w_selection_type & Buffer::SELECTION_TYPE_MASK) == Buffer::X)
      if (item == Kany_selection)
        return MF (wp->w_selection_type != Buffer::SELECTION_VOID);
      if (item == Kmodify_any_selection)
        return MF (wp->w_selection_type != Buffer::SELECTION_VOID
                   && !bp->read_only_p ());
      if (item == Kselection)
        return MF (wp->w_selection_type != Buffer::SELECTION_VOID
                   && !SELECTIONP (SELECTION_RECTANGLE));
      if (item == Kmodify_selection)
        return MF (wp->w_selection_type != Buffer::SELECTION_VOID
                   && !SELECTIONP (SELECTION_RECTANGLE)
                   && !bp->read_only_p ());
      if (item == Krectangle)
        return MF (SELECTIONP (SELECTION_RECTANGLE));
      if (item == Kmodify_rectangle)
        return MF (SELECTIONP (SELECTION_RECTANGLE)
                   && !bp->read_only_p ());
      if (item == Kclipboard)
        return MF (!bp->read_only_p ()
                   && Fclipboard_empty_p () == Qnil);
    }

  if (item != Qnil)
    {
      lisp result = Qnil;
      try
        {
          result = Ffuncall (item, Qnil);
        }
      catch (nonlocal_jump &)
        {
        }

      int check = 0, gray = 0;
      multiple_value::value (0) = result;
      for (int i = 0; i < multiple_value::count (); i++)
        {
          if (multiple_value::value (i) == Kcheck)
            check = 1;
          if (multiple_value::value (i) == Kdisable)
            gray = 1;
        }

      return ((check ? MF_CHECKED : MF_UNCHECKED)
              | (gray ? MF_GRAYED : MF_ENABLED));
    }

  return MF_ENABLED | MF_UNCHECKED;
}

static int
init_menu_popup (lisp lmenu, int enablep)
{
  int f = 0;
  int pos = xlist_length (xwin32_menu_items (lmenu)) - 1;
  for (lisp p = xwin32_menu_items (lmenu); consp (p); p = xcdr (p), pos--)
    {
      int flags;
      lisp item = xcar (p);
      if (!xwin32_menu_id (item))
        {
          if (!xwin32_menu_handle (item))
            continue;
          if (init_menu_popup (item, enablep))
            flags = MF_ENABLED | MF_UNCHECKED;
          else
            flags = MF_GRAYED | MF_UNCHECKED;
        }
      else
        {
          if (xwin32_menu_init (item) == Kend_macro)
            flags = (app.kbdq.save_p ()
                     ? MF_ENABLED | MF_UNCHECKED
                     : MF_GRAYED | MF_UNCHECKED);
          else if (enablep)
            {
              if (xwin32_menu_init (item) != Qnil)
                flags = init_menu_flags (xwin32_menu_init (item));
              else if (xwin32_menu_command (item) == Qnil)
                flags = MF_GRAYED | MF_UNCHECKED;
              else
                flags = MF_ENABLED | MF_UNCHECKED;
            }
          else
            flags = MF_GRAYED | MF_UNCHECKED;
        }
      EnableMenuItem (xwin32_menu_handle (lmenu), pos,
                      MF_BYPOSITION | (flags & (MF_ENABLED | MF_DISABLED | MF_GRAYED)));
      CheckMenuItem (xwin32_menu_handle (lmenu), pos,
                     MF_BYPOSITION | (flags & (MF_CHECKED | MF_UNCHECKED)));
      if ((flags & (MF_ENABLED | MF_GRAYED | MF_DISABLED)) == MF_ENABLED)
        f = 1;
    }
  return f;
}

static char *
keyname (char *p, Char c)
{
  if (function_char_p (c))
    {
      if (pseudo_ctlchar_p (c))
        {
          p = stpcpy (p, "Ctrl+");
          *p++ = pseudo_ctl2char_table[c & 0xff];
          if (p[-1] == '&')
            *p++ = '&';
          *p = 0;
        }
      else
        {
          if (c & CCF_SHIFT_BIT)
            p = stpcpy (p, "Shift+");
          if (c & CCF_CTRL_BIT)
            p = stpcpy (p, "Ctrl+");
          const char *x = function_Char2name (c & ~(CCF_SHIFT_BIT | CCF_CTRL_BIT));
          if (x)
            p = stpcpy (p, x);
          else
            assert (0);
        }
    }
  else
    {
      const char *x = standard_Char2name (c);
      if (x)
        p = stpcpy (p, x);
      else
        {
          if (c < ' ' || c == CC_DEL)
            {
              p = stpcpy (p, "Ctrl+");
              *p++ = c == CC_DEL ? '?' : _char_downcase (c + '@');
            }
          else
            {
              if (c == '&')
                *p++ = char (c);
              *p++ = char (c);
            }
          *p = 0;
        }
    }
  return p;
}

#define ACC_SEP '\010'

static void
modify_menu_string (HMENU hmenu, int id, int pos, const Char *b, const Char *be)
{
  char olds[1024], news[2048];
  if (!GetMenuString (hmenu, pos, olds, sizeof olds, MF_BYPOSITION))
    return;
  strcpy (news, olds);
  char *p = jindex (news, ACC_SEP);
  if (p)
    *p = 0;
  if (b)
    {
      if (!p)
        p = news + strlen (news);
      int c = ACC_SEP;
      for (; b < be; b++)
        {
          *p++ = c;
          p = keyname (p, *b);
          c = ' ';
        }
    }
  if (strcmp (news, olds))
    ModifyMenu (hmenu, pos, MF_BYPOSITION | MF_STRING, id, news);
}

static void
modify_menu_string (lisp lmenu)
{
  int pos = xlist_length (xwin32_menu_items (lmenu)) - 1;
  lisp *map;
  int nmaps = 0;
  Buffer *bp = selected_buffer ();
  long n;

  if (safe_fixnum_value (Flist_length (bp->lminor_map), &n) && n > 0)
    {
      map = (lisp *)alloca (sizeof *map * (n + 3));
      map[nmaps++] = Fcurrent_selection_keymap ();
      for (lisp p = bp->lminor_map; consp (p) && nmaps <= n; nmaps++, p = xcdr (p))
        map[nmaps] = xcar (p);
    }
  else
    {
      map = (lisp *)alloca (sizeof *map * 3);
      map[nmaps++] = Fcurrent_selection_keymap ();
    }
  map[nmaps++] = bp->lmap;
  map[nmaps++] = xsymbol_value (Vglobal_keymap);

  for (lisp p = xwin32_menu_items (lmenu); consp (p); p = xcdr (p), pos--)
    {
      lisp item = xcar (p);
      if (xwin32_menu_id (item)
          && xwin32_menu_command (item) != Qnil
          && symbolp (xwin32_menu_command (item)))
        for (int i = 0; i < nmaps; i++)
          {
            Char b[5];
            Char *be = lookup_command_keyseq (xwin32_menu_command (item),
                                              map[i], map, i,
                                              b, b, b + numberof (b));
            if (be)
              {
                modify_menu_string (xwin32_menu_handle (lmenu),
                                    xwin32_menu_id (item), pos,
                                    b, be);
                break;
              }
          }
    }
}

static lisp
lookup_menu (lisp lmenu, HMENU hmenu)
{
  if (xwin32_menu_handle (lmenu) == hmenu)
    return lmenu;
  for (lisp p = xwin32_menu_items (lmenu); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (xwin32_menu_handle (x))
        {
          x = lookup_menu (x, hmenu);
          if (x)
            return x;
        }
    }
  return 0;
}

void
init_menu_popup (WPARAM wparam, LPARAM lparam)
{
  lisp lmenu = 0;
  if (win32_menu_p (xsymbol_value (Vtracking_menu)))
    lmenu = lookup_menu (xsymbol_value (Vtracking_menu), HMENU (wparam));
  if (!lmenu && win32_menu_p (xsymbol_value (Vlast_active_menu)))
    lmenu = lookup_menu (xsymbol_value (Vlast_active_menu), HMENU (wparam));
  if (!lmenu)
    return;
  modify_menu_string (lmenu);
  suppress_gc sgc;
  init_menu_popup (lmenu, app.kbdq.idlep ());
}

static lisp
lookup_menu_command (lisp lmenu, int id)
{
  for (lisp p = xwin32_menu_items (lmenu); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (xwin32_menu_id (x) == id)
        return xwin32_menu_command (x);
      if (xwin32_menu_handle (x))
        {
          x = lookup_menu_command (x, id);
          if (x)
            return x;
        }
    }
  return 0;
}

lisp
lookup_menu_command (int id)
{
  if (id)
    {
      if (win32_menu_p (xsymbol_value (Vtracking_menu)))
        {
          lisp x = lookup_menu_command (xsymbol_value (Vtracking_menu), id);
          if (x)
            return x;
        }
      if (win32_menu_p (xsymbol_value (Vlast_active_menu)))
        {
          lisp x = lookup_menu_command (xsymbol_value (Vlast_active_menu), id);
          if (x)
            return x;
        }
    }
  return Qnil;
}

lisp
track_popup_menu (lisp lmenu, lisp lbutton, const POINT *point)
{
  check_popup_menu (lmenu);
  dynamic_bind dynb (Vtracking_menu, lmenu);
  int id = app.mouse.track_popup_menu (xwin32_menu_handle (lmenu), lbutton, point);
  if (!id)
    return Qnil;
  lisp command = lookup_menu_command (lmenu, id);
  if (!command || command == Qnil)
    return Qnil;
  stack_trace trace (stack_trace::apply, Scommand_execute, command, 0);
  xsymbol_value (Vthis_command) = command;
  return Fcommand_execute (command, 0);
}

lisp
Ftrack_popup_menu (lisp lmenu, lisp lbutton)
{
  return track_popup_menu (lmenu, lbutton, 0);
}

lisp
Fuse_local_menu (lisp lmenu)
{
  if (lmenu != Qnil)
    check_popup_menu (lmenu);
  selected_buffer ()->lmenu = lmenu;
  return lmenu;
}

lisp
Fcurrent_menu (lisp buffer)
{
  if (!buffer)
    return (win32_menu_p (selected_buffer ()->lmenu)
            ? selected_buffer ()->lmenu
            : xsymbol_value (Vdefault_menu));
  else if (buffer == Qnil)
    return xsymbol_value (Vdefault_menu);
  else
    return Buffer::coerce_to_buffer (buffer)->lmenu;
}

lisp
Fcopy_menu_items (lisp old_menu, lisp new_menu)
{
  check_popup_menu (old_menu);
  check_popup_menu (new_menu);

  if (old_menu == new_menu)
    return new_menu;

  for (int i = GetMenuItemCount (xwin32_menu_handle (new_menu)) - 1; i >= 0; i--)
    RemoveMenu (xwin32_menu_handle (new_menu), i, MF_BYPOSITION);
  xwin32_menu_items (new_menu) = Fcopy_list (xwin32_menu_items (old_menu));

  int v5 = sysdep.Win5p () || sysdep.Win98p ();
  int count = GetMenuItemCount (xwin32_menu_handle (old_menu));
  for (int i = 0; i < count; i++)
    {
      MENUITEMINFO5 m;
      char name[1024];
      *name = 0;
      if (v5)
        {
          m.cbSize = sizeof m;
          m.fMask = (MIIM_BITMAP | MIIM_FTYPE | MIIM_ID
                     | MIIM_STRING | MIIM_SUBMENU);
          m.dwTypeData = name;
          m.cch = sizeof name;
        }
      else
        {
          m.cbSize = offsetof (MENUITEMINFO5, hbmpItem);
          m.fMask = MIIM_ID | MIIM_SUBMENU | MIIM_TYPE;
          m.dwTypeData = name;
          m.cch = sizeof name;
        }

      GetMenuItemInfo (xwin32_menu_handle (old_menu), i, 1, &m);
      if (!m.hSubMenu)
        m.fMask &= ~MIIM_SUBMENU;
      if (v5)
        {
          if (m.fType & MFT_SEPARATOR)
            m.fMask &= ~(MIIM_BITMAP | MIIM_STRING);
          if (!m.hbmpItem)
            m.fMask &= ~MIIM_BITMAP;
          if (!m.dwTypeData || !m.cch)
            m.fMask &= ~MIIM_STRING;
        }
      InsertMenuItem (xwin32_menu_handle (new_menu), i, 1, &m);
    }
  return new_menu;
}
