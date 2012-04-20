#include "stdafx.h"
#include "ed.h"
#include "mainframe.h"
#include "dockbar.h"

class user_tool_bar: public tool_bar
{
  struct tool_item
    {
      lisp ti_tooltip;
      lisp ti_command;
      lisp ti_init;
      int ti_id;
      int ti_bitmap;
    };

  static u_long u_command_id[(TOOL_ID_RANGE_MAX - TOOL_ID_RANGE_MIN)
                             / (sizeof (u_long) * 8)];

  tool_item *u_item;
  int u_nitems;

  static void check_item (lisp);
  static int set_item (tool_item &, lisp);

  virtual int need_text (TOOLTIPTEXT &);
  int index_from_id (int) const;

public:
  user_tool_bar (dock_frame &, lisp);
  ~user_tool_bar ();

  void create (lisp, lisp);
  virtual void gc_mark (void (*)(lisp));
  virtual void update_ui ();
  virtual lisp lookup_command (int) const;
};

u_long user_tool_bar::u_command_id[(TOOL_ID_RANGE_MAX - TOOL_ID_RANGE_MIN)
                                   / (sizeof (u_long) * 8)];

user_tool_bar::user_tool_bar (dock_frame &frame, lisp name)
     : tool_bar (frame, name), u_item (0), u_nitems (0)
{
}

user_tool_bar::~user_tool_bar ()
{
  if (u_item)
    {
      for (int i = 0; i < u_nitems; i++)
        if (u_item[i].ti_id > TOOL_ID_RANGE_MIN
            && u_item[i].ti_id < TOOL_ID_RANGE_MAX)
          bitclr (u_command_id, u_item[i].ti_id - TOOL_ID_RANGE_MIN);
      delete [] u_item;
    }
}

void
user_tool_bar::gc_mark (void (*f)(lisp))
{
  for (int i = 0; i < u_nitems; i++)
    {
      (*f)(u_item[i].ti_tooltip);
      (*f)(u_item[i].ti_command);
      (*f)(u_item[i].ti_init);
    }
  tool_bar::gc_mark (f);
}

int
user_tool_bar::index_from_id (int id) const
{
  for (int i = 0; i < u_nitems; i++)
    if (u_item[i].ti_id == id)
      return i;
  return -1;
}

lisp
get_tooltip_text (lisp tt)
{
  if (tt == Qnil || stringp (tt))
    return tt;
  try
    {
      suppress_gc sgc;
      return Ffuncall (tt, Qnil);
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
    }
  return Qnil;
}

int
user_tool_bar::need_text (TOOLTIPTEXT &ttt)
{
  if (ttt.uFlags & TTF_IDISHWND)
    return 0;

  int i = index_from_id (ttt.hdr.idFrom);
  if (i < 0)
    return 0;
  lisp tt = get_tooltip_text (u_item[i].ti_tooltip);
  if (!stringp (tt))
    return 0;
  w2s (b_ttbuf, b_ttbuf + TTBUFSIZE, tt);
  ttt.lpszText = b_ttbuf;
  ttt.hinst = 0;
  return 1;
}

void
user_tool_bar::update_ui ()
{
  if (!IsWindowVisible (b_hwnd))
    return;
  suppress_gc sgc;
  for (int i = 0; i < u_nitems; i++)
    if (u_item[i].ti_id >= 0)
      {
        int flags;
        if (u_item[i].ti_init == Kend_macro)
          flags = (app.kbdq.save_p ()
                   ? MF_ENABLED | MF_UNCHECKED
                   : MF_GRAYED | MF_UNCHECKED);
        else
          {
            if (u_item[i].ti_init != Qnil)
              flags = init_menu_flags (u_item[i].ti_init);
            else if (u_item[i].ti_command == Qnil
                     || (symbolp (u_item[i].ti_command)
                         && xsymbol_function (u_item[i].ti_command) == Qunbound))
              flags = MF_GRAYED | MF_UNCHECKED;
            else
              flags = MF_ENABLED | MF_UNCHECKED;
          }

#if 0
        TBBUTTON tb;
        if (get_button (i, tb))
          {
            BYTE ostate = tb.fsState;
            tb.fsState = 0;
            if (!(flags & MF_GRAYED))
              tb.fsState |= TBSTATE_ENABLED;
            if (flags & MF_CHECKED)
              tb.fsState |= TBSTATE_CHECKED | TBSTATE_PRESSED;
            if (tb.fsState != ostate)
              set_button (i, tb);
          }
#else
        int ostate = get_state (u_item[i].ti_id);
        int state = ostate;
        if (flags & MF_GRAYED)
          state &= ~TBSTATE_ENABLED;
        else
          state |= TBSTATE_ENABLED;
        if (flags & MF_CHECKED)
          state |= TBSTATE_CHECKED | TBSTATE_PRESSED;
        else
          state &= ~(TBSTATE_CHECKED | TBSTATE_PRESSED);
        if (state != ostate)
          set_state (u_item[i].ti_id, state);
#endif
      }
}

lisp
user_tool_bar::lookup_command (int id) const
{
  int i = index_from_id (id);
  return i >= 0 ? u_item[i].ti_command : 0;
}

/*
  ("tooltip-text" bitmap-index [command [init]])
 */

void
user_tool_bar::check_item (lisp x)
{
  if (x == Ksep)
    return;
  check_cons (x);
  if (xcar (x) == Ksep)
    return;
  x = xcdr (x);
  check_cons (x);
  if (fixnum_value (xcar (x)) < 0)
    FErange_error (xcar (x));
}

int
user_tool_bar::set_item (tool_item &i, lisp x)
{
  if (x == Ksep || xcar (x) == Ksep)
    {
      i.ti_tooltip = Qnil;
      i.ti_bitmap = -1;
      i.ti_id = -1;
      i.ti_command = Qnil;
      i.ti_init = Qnil;
      return 0;
    }
  else
    {
      i.ti_tooltip = xcar (x);
      x = xcdr (x);
      i.ti_bitmap = fixnum_value (xcar (x));
      x = xcdr (x);
      if (consp (x))
        {
          i.ti_command = xcar (x);
          x = xcdr (x);
        }
      else
        i.ti_command = Qnil;
      if (consp (x))
        {
          i.ti_init = xcar (x);
          x = xcdr (x);
        }
      else
        i.ti_init = Qnil;
      i.ti_id = find_zero_bit (u_command_id, numberof (u_command_id));
      if (i.ti_id < 0)
        {
          gc (1);
          i.ti_id = find_zero_bit (u_command_id, numberof (u_command_id));
          if (i.ti_id < 0)
            FEprogram_error (Etoo_many_tool_items);
        }
      bitset (u_command_id, i.ti_id);
      i.ti_id += TOOL_ID_RANGE_MIN;
      return stringp (i.ti_tooltip);
    }
}

void
user_tool_bar::create (lisp bitmap, lisp items)
{
  check_string (bitmap);
  char bm_path[MAX_PATH + 1];
  pathname2cstr (bitmap, bm_path);

  u_nitems = 0;
  lisp p;
  for (p = items; consp (p); p = xcdr (p), u_nitems++)
    {
      check_item (xcar (p));
      QUIT;
    }

  if (!u_nitems)
    FEprogram_error (Eno_tool_items);

  u_item = new tool_item[u_nitems];
  int i;
  for (i = 0; i < u_nitems; i++)
    u_item[i].ti_id = -1;

  int has_tooltips = 0;
  for (p = items, i = 0; consp (p); p = xcdr (p), i++)
    has_tooltips |= set_item (u_item[i], xcar (p));

  if (!tool_bar::create (app.toplev,
                         (WS_CHILD | WS_CLIPSIBLINGS | CCS_NOMOVEY
                          | CCS_NORESIZE | CCS_NOPARENTALIGN | CCS_NODIVIDER
                          | (has_tooltips ? TBSTYLE_TOOLTIPS : 0)),
                         0))
    FEsimple_error (ECannot_create_toolbar);

  set_bitmap_size (16, 15);
  set_button_size (23, 22);
  if (new_comctl_p ())
    modify_style (0, TBSTYLE_FLAT);

  HWND hwnd_tt = get_tooltips ();
  if (hwnd_tt)
    set_tooltip_no_prefix (hwnd_tt);

  int e = load_bitmap (bm_path);
  if (e != tool_bm::LMB_NO_ERRORS)
    {
      if (e == Estorage_condition)
        FEstorage_error ();
      FEsimple_error (message_code (e), bitmap);
    }

  TBBUTTON *tbb = (TBBUTTON *)alloca (sizeof *tbb * u_nitems);
  for (i = 0; i < u_nitems; i++)
    if (u_item[i].ti_id == -1)
      {
        tbb[i].iBitmap = 8;
        tbb[i].idCommand = 0;
        tbb[i].fsState = 0;
        tbb[i].fsStyle = TBSTYLE_SEP;
        tbb[i].dwData = 0;
        tbb[i].iString = -1;
      }
    else
      {
        tbb[i].iBitmap = u_item[i].ti_bitmap;
        tbb[i].idCommand = u_item[i].ti_id;
        tbb[i].fsState = TBSTATE_ENABLED;
        tbb[i].fsStyle = TBSTYLE_BUTTON;
        tbb[i].dwData = 0;
        tbb[i].iString = -1;
      }

  add_buttons (u_nitems, tbb);
}

lisp
Fcreate_tool_bar (lisp name, lisp bitmap, lisp items)
{
  if (g_frame.find (name))
    FEsimple_error (Etoolbar_exist, name);

  user_tool_bar *bar = new user_tool_bar (g_frame, name);

  try
    {
      bar->create (bitmap, items);
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
Fshow_tool_bar (lisp name, lisp ledge, lisp lx, lisp ly, lisp lw)
{
  int edge = dock_frame::EDGE_TOP;
  if (ledge == Kleft)
    edge = dock_frame::EDGE_LEFT;
  else if (ledge == Ktop)
    edge = dock_frame::EDGE_TOP;
  else if (ledge == Kright)
    edge = dock_frame::EDGE_RIGHT;
  else if (ledge == Kbottom)
    edge = dock_frame::EDGE_BOTTOM;

  dock_bar *bar = g_frame.find (name);
  if (!bar)
    FEsimple_error (Eundefined_toolbar, name);

  int w = lw && lw != Qnil ? fixnum_value (lw) : -1;

  if (!lx || !ly || lx == Qnil || ly == Qnil)
    g_frame.show (bar, edge, 0, w);
  else
    {
      POINT p;
      p.x = fixnum_value (lx);
      p.y = fixnum_value (ly);
      g_frame.show (bar, edge, &p, w);
    }

  return Qt;
}

static lisp
edge_sym (int edge)
{
  switch (edge)
    {
    default:
      return Qnil;

    case dock_frame::EDGE_LEFT:
      return Kleft;

    case dock_frame::EDGE_TOP:
      return Ktop;

    case dock_frame::EDGE_RIGHT:
      return Kright;

    case dock_frame::EDGE_BOTTOM:
      return Kbottom;
    }
}

lisp
Fhide_tool_bar (lisp name)
{
  dock_bar *bar = g_frame.find (name);
  if (!bar)
    FEsimple_error (Eundefined_toolbar, name);

  RECT r (((const dock_bar *)bar)->rect ());
  int edge = bar->edge ();
  int w = bar->horz_width ();

  g_frame.hide (bar);

  multiple_value::value (1) = make_fixnum (r.left);
  multiple_value::value (2) = make_fixnum (r.top);
  multiple_value::value (3) = w > 0 ? make_fixnum (w) : Qnil;
  multiple_value::count () = 4;
  return edge_sym (edge);
}

lisp
Fdelete_tool_bar (lisp name)
{
  dock_bar *bar = g_frame.find (name);
  if (!bar)
    FEsimple_error (Eundefined_toolbar, name);
  g_frame.remove (bar);
  return Qt;
}

lisp
Ftool_bar_exist_p (lisp name)
{
  return boole (g_frame.find (name));
}

lisp
Ftool_bar_info (lisp name)
{
  dock_bar *bar = g_frame.find (name);
  if (!bar)
    FEsimple_error (Eundefined_toolbar, name);

  int w = bar->horz_width ();
  const RECT &r = ((const dock_bar *)bar)->rect ();
  multiple_value::value (1) = make_fixnum (r.left);
  multiple_value::value (2) = make_fixnum (r.top);
  multiple_value::value (3) = w > 0 ? make_fixnum (w) : Qnil;
  multiple_value::count () = 4;
  return edge_sym (bar->edge ());
}

lisp
Flist_tool_bars ()
{
  return g_frame.list_bars ();
}

lisp
Ffocus_tool_bar ()
{
  return boole (g_frame.focus_next (0));
}

lisp
Frefresh_tool_bars ()
{
  g_frame.refresh ();
  return Qt;
}
