#include "stdafx.h"
#include "ed.h"
#include "mainframe.h"
#include "dockbar.h"
#include "colors.h"

class user_tab_bar: public tab_bar
{
  lisp u_item;
  lisp u_callback;
  static char u_ident;

  enum {ITEM_NAME_MAX = 128};

  virtual int need_text (TOOLTIPTEXT &);
  virtual void draw_item (const draw_item_struct &);
  virtual int notify (NMHDR *, LRESULT &);
  lisp nth (int i) const {return lisp (tab_bar::nth (i));}
  lisp current () const;
  void sel_change () const;


  static lisp &item_tag (lisp item) {return xcar (item);}
  static lisp &item_name (lisp item) {return xcar (xcdr (item));}
  static lisp &item_tooltip (lisp item) {return xcar (xcdr (xcdr (item)));}
  static lisp &item_menu (lisp item) {return xcdr (xcdr (xcdr (item)));}
  static lisp make_item (lisp tag, lisp name, lisp tooltip, lisp menu)
    {return xcons (tag, xcons (name, xcons (tooltip, menu)));}

  virtual lisp context_menu (int);

public:
  enum {UTB_FIRST, UTB_LAST, UTB_BEFORE, UTB_AFTER, UTB_MAX};
  user_tab_bar (dock_frame &, lisp, lisp);

  virtual void gc_mark (void (*)(lisp));
  void add_item (lisp, lisp, lisp, lisp, int, lisp);
  int modify_item (lisp, lisp, lisp, lisp);
  int delete_item (lisp);
  int select_item (lisp);
  lisp current_item ();
  lisp find_item (lisp);
  lisp list_items ();
  int item_pos (lisp, lisp &) const;
  int item_pos (lisp item) const
    {
      lisp tem;
      return item_pos (item, tem);
    }
  virtual void *ident () const {return (void *)&u_ident;}
  static user_tab_bar *check_bar (lisp);
};

char user_tab_bar::u_ident;

/* (item name . tooltip) */

user_tab_bar::user_tab_bar (dock_frame &frame, lisp name, lisp callback)
     : tab_bar (frame, name), u_item (Qnil), u_callback (callback)
{
}

void
user_tab_bar::gc_mark (void (*f)(lisp))
{
  (*f)(u_item);
  (*f)(u_callback);
  tab_bar::gc_mark (f);
}

lisp
user_tab_bar::current () const
{
  int i = get_cursel ();
  return i >= 0 ? nth (i) : 0;
}

int
user_tab_bar::item_pos (lisp item, lisp &place) const
{
  int n = item_count ();
  for (int i = 0; i < n; i++)
    {
      lisp p = nth (i);
      if (p && item_tag (p) == item)
        {
          place = p;
          return i;
        }
    }
  return -1;
}

void
user_tab_bar::sel_change () const
{
  lisp p = current ();
  if (!p)
    return;
  save_cursor_depth cursor_depth;
  try
    {
      funcall_1 (u_callback, item_tag (p));
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
}

int
user_tab_bar::notify (NMHDR *nm, LRESULT &result)
{
  switch (nm->code)
    {
    default:
      return 0;

    case TCN_SELCHANGE:
    case TCN_SELCHANGING:
      if (!app.kbdq.idlep ())
        {
          result = 1; // prevent the selection
          return 1;
        }
      break;
    }

  if (nm->code != TCN_SELCHANGE)
    return 0;

  sel_change ();
  return 1;
}

int
user_tab_bar::need_text (TOOLTIPTEXT &ttt)
{
  if (ttt.uFlags & TTF_IDISHWND)
    return 0;

  lisp item = nth (ttt.hdr.idFrom);
  if (!item)
    return 0;

  lisp tt = get_tooltip_text (item_tooltip (item));
  if (!stringp (tt))
    return 0;
  w2s (b_ttbuf, b_ttbuf + TTBUFSIZE, tt);
  ttt.lpszText = b_ttbuf;
  ttt.hinst = 0;
  return 1;
}

void
user_tab_bar::draw_item (const draw_item_struct &dis)
{
  lisp name = item_name ((lisp)dis.data);
  char buf[ITEM_NAME_MAX];
  int l = w2s (buf, buf + sizeof buf, name) - buf;
  if (dis.state & ODS_SELECTED)
    tab_bar::draw_item (dis, buf, l,
                        get_misc_color (MC_TAB_SEL_FG),
                        get_misc_color (MC_TAB_SEL_BG));
  else
    tab_bar::draw_item (dis, buf, l,
                        get_misc_color (MC_TAB_FG),
                        get_misc_color (MC_TAB_BG));
}

lisp
user_tab_bar::context_menu (int i)
{
  if (i < 0)
    return Qnil;
  lisp p = nth (i);
  if (!p)
    return Qnil;
  lisp menu = item_menu (p);
  if (menu == Qnil)
    return Qnil;
  if (win32_menu_p (menu))
    return menu;
  return funcall_1 (menu, item_tag (p));
}

void
user_tab_bar::add_item (lisp item, lisp name, lisp tooltip, lisp menu,
                        int pos, lisp base)
{
  check_string (name);

  lisp p = make_item (item, name, tooltip, menu);
  lisp q = xcons (p, u_item);

  int n;
  switch (pos)
    {
    case UTB_FIRST:
      n = 0;
      break;

    case UTB_LAST:
      n = item_count ();
      break;

    default:
      n = item_pos (base);
      if (n == -1)
        FEsimple_error (Etab_not_found, base);
      if (pos == UTB_AFTER)
        n++;
      break;
    }

  char buf[ITEM_NAME_MAX];
  w2s (buf, buf + sizeof buf, name);

  TC_ITEM ti;
  ti.mask = TCIF_TEXT | TCIF_PARAM;
  ti.pszText = buf;
  ti.lParam = LPARAM (p);
  if (insert_item (n, ti) < 0)
    FEstorage_error ();

  u_item = q;
}

int
user_tab_bar::modify_item (lisp item, lisp name, lisp tooltip, lisp menu)
{
  lisp p;
  int i = item_pos (item, p);
  if (i == -1)
    return 0;

  if (name && name != Qnil && name != Qt)
    {
      check_string (name);

      char buf[ITEM_NAME_MAX];
      w2s (buf, buf + sizeof buf, name);

      TC_ITEM ti;
      ti.mask = TCIF_TEXT;
      ti.pszText = buf;
      if (!set_item (i, ti))
        return 0;
      item_name (p) = name;
    }

  if (tooltip && tooltip != Qt)
    item_tooltip (p) = tooltip;
  if (menu && menu != Qt)
    item_menu (p) = menu;

  return 1;
}

int
user_tab_bar::delete_item (lisp item)
{
  lisp p;
  int i = item_pos (item, p);
  if (i == -1)
    return 0;

  int selchg = 0;
  if (i == get_cursel ())
    {
      if (i)
        selchg = set_cursel (i - 1) >= 0;
      else if (i != item_count () - 1)
        selchg = set_cursel (i + 1) >= 0;
    }

  int r = tab_bar::delete_item (i);
  if (r)
    delq (p, &u_item);

  if (selchg)
    sel_change ();

  return r;
}

int
user_tab_bar::select_item (lisp item)
{
  int i = item_pos (item);
  return i == -1 ? 0 : set_cursel (i) >= 0;
}

lisp
user_tab_bar::current_item ()
{
  lisp p = current ();
  return p ? p : Qnil;
}

lisp
user_tab_bar::list_items ()
{
  lisp r = Qnil;
  int n = item_count ();
  for (int i = 0; i < n; i++)
    {
      lisp p = nth (i);
      if (p)
        r = xcons (item_tag (p), r);
    }
  return Fnreverse (r);
}

user_tab_bar *
user_tab_bar::check_bar (lisp name)
{
  dock_bar *bar = g_frame.find (name);
  if (!bar)
    FEsimple_error (Eundefined_toolbar, name);
  if (bar->ident () != (void *)&u_ident)
    FEsimple_error (Enot_a_tab_bar, name);
  return (user_tab_bar *)bar;
}

lisp
Fcreate_tab_bar (lisp name, lisp callback)
{
  if (g_frame.find (name))
    FEsimple_error (Etoolbar_exist, name);

  user_tab_bar *bar = new user_tab_bar (g_frame, name, callback);

  try
    {
      bar->create (app.toplev);
    }
  catch (nonlocal_jump &)
    {
      delete bar;
      throw;
    }

  g_frame.add (bar);
  return name;
}

lisp
Ftab_bar_add_item (lisp name, lisp item, lisp string,
                   lisp tooltip, lisp menu, lisp keys)
{
  lisp u[user_tab_bar::UTB_MAX];

  u[user_tab_bar::UTB_FIRST] = find_keyword (Kfirst, keys);
  u[user_tab_bar::UTB_LAST] = find_keyword (Klast, keys);
  u[user_tab_bar::UTB_BEFORE] = find_keyword (Kbefore, keys);
  u[user_tab_bar::UTB_AFTER] = find_keyword (Kafter, keys);

  int pos = user_tab_bar::UTB_LAST;
  int i, n;
  for (i = 0, n = 0; i < user_tab_bar::UTB_MAX; i++)
    if (u[i] != Qnil)
      {
        pos = i;
        n++;
      }

  if (n > 1)
    FEsimple_error (Emultiple_position_specified);

  user_tab_bar::check_bar (name)->add_item (item, string,
                                            tooltip ? tooltip : Qnil,
                                            menu ? menu : Qnil,
                                            pos, u[pos]);
  return Qt;
}

lisp
Ftab_bar_delete_item (lisp name, lisp item)
{
  return boole (user_tab_bar::check_bar (name)->delete_item (item));
}

lisp
Ftab_bar_select_item (lisp name, lisp item)
{
  return boole (user_tab_bar::check_bar (name)->select_item (item));
}

lisp
Ftab_bar_current_item (lisp name)
{
  return user_tab_bar::check_bar (name)->current_item ();
}

lisp
Ftab_bar_find_item (lisp name, lisp item)
{
  return boole (user_tab_bar::check_bar (name)->item_pos (item) >= 0);
}

lisp
Ftab_bar_list_items (lisp name)
{
  return user_tab_bar::check_bar (name)->list_items ();
}

lisp
Ftab_bar_modify_item (lisp name, lisp item, lisp string,
                      lisp tooltip, lisp menu)
{
  return boole (user_tab_bar::check_bar (name)
                ->modify_item (item, string, tooltip, menu));
}
