#include "stdafx.h"
#include "ed.h"

class UndoInfo
{
protected:
  enum
    {
      F_MODIFIED = 1,
      F_BOUND = 2,
      F_STINGY_MASK = 3
    };
public:
  enum undo_status
    {
      NONE,
      UNDO,
      REDO
    };
protected:
  static undo_status u_status;
  UndoInfo *u_prev;
  point_t u_point;

  UndoInfo (point_t point) : u_point (point), u_prev (0) {}

  virtual int undo (Window *) {assert (0); return 1;}
  virtual int can_append_insert (point_t) const {return 0;}
  void modify ()
    {u_prev = (UndoInfo *)(pointer_t (u_prev) | F_MODIFIED);}
  void not_modify ()
    {u_prev = (UndoInfo *)(pointer_t (u_prev) & ~F_MODIFIED);}
  int boundp () const
    {return pointer_t (u_prev) & F_BOUND;}
  void bound ()
    {u_prev = (UndoInfo *)(pointer_t (u_prev) | F_BOUND);}
  void clear_bound ()
    {u_prev = (UndoInfo *)(pointer_t (u_prev) & ~F_BOUND);}
  int modifiedp () const
    {return pointer_t (u_prev) & F_MODIFIED;}
  UndoInfo *prev ()
    {return (UndoInfo *)(pointer_t (u_prev) & ~F_STINGY_MASK);}
  void chain (UndoInfo *p)
    {u_prev = (UndoInfo *)(pointer_t (p) | flags ());}
  int flags () const
    {return pointer_t (u_prev) & F_STINGY_MASK;}

public:
  UndoInfo () {}
  virtual ~UndoInfo () {}
  void *operator new (size_t size) {return malloc (size);}
  void operator delete (void *p) {free (p);}
  point_t undo_point () const {return u_point;}
  static undo_status status () {return u_status;}

  friend Buffer;
  friend class SaveUndoStatus;
  friend void remove_undo_info (UndoInfo *);
};

UndoInfo::undo_status UndoInfo::u_status;

static UndoInfo no_need_save_undo;
static UndoInfo append_insert;

class UndoInsert: public UndoInfo
{
  int u_size;
  virtual int can_append_insert (point_t point) const
    {return point >= u_point && point <= u_point + u_size;}

public:
  UndoInsert (point_t point, int size) : UndoInfo (point), u_size (size) {}
  virtual int undo (Window *);
  friend Buffer;
};

static void
remove_undo_info (UndoInfo *up)
{
  UndoInfo *prev;
  for (; up; up = prev)
    {
      prev = up->prev ();
      delete up;
    }
}

void
Buffer::clear_undo_info ()
{
  remove_undo_info (b_undo);
  remove_undo_info (b_redo);
  b_undo = b_redo = 0;
  b_undo_count = 0;
}

inline void
Buffer::chain_undo (UndoInfo *p)
{
  UndoInfo *&u = UndoInfo::u_status == UndoInfo::UNDO ? b_redo : b_undo;
  p->u_prev = u;
  if (b_modified)
    p->modify ();
  u = p;
}

int
Buffer::delete_surplus_undo ()
{
  if (b_undo && !b_undo->boundp ())
    return 1;

  int undo_max = symbol_value_as_integer (Vkept_undo_information, this);
  if (b_undo_count < undo_max)
    return 1;

  UndoInfo *up = b_undo, *next = 0;
  for (int i = 0; up; next = up, up = up->prev ())
    if (up->boundp () && ++i >= undo_max)
      {
        remove_undo_info (up);
        if (next)
          next->chain (0);
        if (up == b_undo)
          b_undo = 0;
        b_undo_count = i - 1;
        break;
      }
  return undo_max > 0;
}

UndoInfo *
Buffer::setup_save_undo ()
{
  switch (UndoInfo::u_status)
    {
    case UndoInfo::UNDO:
      return b_redo;

    case UndoInfo::REDO:
      return b_undo;

    default:
      remove_undo_info (b_redo);
      b_redo = 0;
      if (!delete_surplus_undo ())
        return &no_need_save_undo;
      return b_undo;
    }
}

UndoInfo *
Buffer::setup_insert_undo (point_t point, int size)
{
  UndoInfo *prev = setup_save_undo ();
  if (prev == &no_need_save_undo)
    return prev;
  if (prev && !prev->boundp () && prev->can_append_insert (point))
    return &append_insert;
  return new UndoInsert (point, size);
}

void
Buffer::save_insert_undo (UndoInfo *up, point_t point, int size)
{
  if (up == &no_need_save_undo)
    ;
  else if (up == &append_insert)
    {
      UndoInsert *p =
        (UndoInsert *)(UndoInfo::u_status == UndoInfo::UNDO ? b_redo : b_undo);
      p->u_size += size;
      p->u_point = min (p->u_point, point);
    }
  else
    chain_undo (up);
}

void
discard_insert_undo (UndoInfo *p)
{
  if (p != &no_need_save_undo && p != &append_insert)
    delete p;
}

class UndoRegion: public UndoInfo
{
protected:
  int u_size;
  Char *u_buffer;

  int save_region (const Buffer *, const Point &);
  UndoRegion (const Point &point, int size)
       : UndoInfo (point.p_point), u_size (size), u_buffer (0) {}
  virtual ~UndoRegion () {if (u_buffer) free (u_buffer);}
  virtual int undo (Window *) = 0;
  friend Buffer;
};

int
UndoRegion::save_region (const Buffer *bp, const Point &point)
{
  assert (point.p_point == u_point);
  u_buffer = (Char *)malloc (sizeof *u_buffer * u_size);
  if (!u_buffer)
    return 0;
  bp->substring (point, u_size, u_buffer);
  return 1;
}

class UndoDeleteRegion: public UndoRegion
{
public:
  UndoDeleteRegion (const Point &point, int size) : UndoRegion (point, size) {}
  virtual int undo (Window *);
};

class UndoModifyRegion: public UndoRegion
{
public:
  UndoModifyRegion (const Point &point, int size) : UndoRegion (point, size) {}
  virtual int undo (Window *);
};

class UndoChar: public UndoInfo
{
protected:
  Char cc;
  UndoChar (const Point &point) : UndoInfo (point.p_point), cc (point.ch ()) {}
  virtual int undo (Window *) = 0;
};

class UndoDeleteChar: public UndoChar
{
public:
  UndoDeleteChar (const Point &point) : UndoChar (point) {}
  virtual int undo (Window *);
};

class UndoModifyChar: public UndoChar
{
public:
  UndoModifyChar (const Point &point) : UndoChar (point) {}
  virtual int undo (Window *);
};

int
Buffer::save_delete_undo (const Point &point, int size)
{
  if (setup_save_undo () == &no_need_save_undo)
    return 1;
  if (size == 1)
    {
      UndoDeleteChar *p = new UndoDeleteChar (point);
      if (!p)
        return 0;
      chain_undo (p);
    }
  else
    {
      UndoDeleteRegion *p = new UndoDeleteRegion (point, size);
      if (!p)
        return 0;
      if (!p->save_region (this, point))
        {
          delete p;
          return 0;
        }
      chain_undo (p);
    }
  return 1;
}

int
Buffer::save_modify_undo (const Point &point, int size)
{
  if (setup_save_undo () == &no_need_save_undo)
    return 1;
  if (size == 1)
    {
      UndoModifyChar *p = new UndoModifyChar (point);
      if (!p)
        return 0;
      chain_undo (p);
    }
  else
    {
      UndoModifyRegion *p = new UndoModifyRegion (point, size);
      if (!p)
        return 0;
      if (!p->save_region (this, point))
        {
          delete p;
          return 0;
        }
      chain_undo (p);
    }
  return 1;
}

void
Buffer::save_modtime_undo (const FileTime &modtime)
{
  for (UndoInfo *u = b_undo; u; u = u->prev ())
    u->modify ();
  for (UndoInfo *u = b_redo; u; u = u->prev ())
    u->modify ();
}

void
Buffer::undo_boundary ()
{
  if (b_undo && !b_undo->boundp ())
    {
      b_undo->bound ();
      b_undo_count++;
    }
  if (b_redo)
    b_redo->bound ();
}

lisp
Fundo_boundary ()
{
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    bp->undo_boundary ();
  return Qnil;
}

int
Buffer::clear_undo_boundary ()
{
  if (b_undo && b_undo->boundp ())
    {
      b_undo->clear_bound ();
      b_undo_count--;
      return 1;
    }
  return 0;
}

lisp
Fclear_undo_boundary (lisp lbuffer)
{
  return boole (Buffer::coerce_to_buffer (lbuffer)->clear_undo_boundary ());
}

int
UndoInsert::undo (Window *wp)
{
  Buffer *bp = wp->w_bufp;
  if (u_point < bp->b_contents.p1 || u_point + u_size > bp->b_contents.p2)
    FEsimple_error (Eundo_outside_region);
  bp->delete_region (wp, u_point, u_point + u_size);
  return 1;
}

int
UndoDeleteRegion::undo (Window *wp)
{
  Buffer *bp = wp->w_bufp;
  if (u_point < bp->b_contents.p1 || u_point > bp->b_contents.p2)
    FEsimple_error (Eundo_outside_region);
  bp->goto_char (wp->w_point, u_point);
  bp->insert_chars (wp, u_buffer, u_size, 1);
  if (xsymbol_value (Vmove_forward_after_undo_deletion) == Qnil)
    bp->goto_char (wp->w_point, u_point);
  return 1;
}

int
UndoDeleteChar::undo (Window *wp)
{
  Buffer *bp = wp->w_bufp;
  if (u_point < bp->b_contents.p1 || u_point > bp->b_contents.p2)
    FEsimple_error (Eundo_outside_region);
  bp->goto_char (wp->w_point, u_point);
  bp->insert_chars (wp, &cc, 1, 1);
  if (xsymbol_value (Vmove_forward_after_undo_deletion) == Qnil)
    bp->goto_char (wp->w_point, u_point);
  return 1;
}

int
UndoModifyRegion::undo (Window *wp)
{
  Buffer *bp = wp->w_bufp;
  if (u_point < bp->b_contents.p1 || u_point + u_size > bp->b_contents.p2)
    FEsimple_error (Eundo_outside_region);
  bp->goto_char (wp->w_point, u_point);
  bp->overwrite_chars (wp, u_buffer, u_size);
  if (xsymbol_value (Vmove_forward_after_undo_deletion) == Qnil)
    bp->goto_char (wp->w_point, u_point);
  return 1;
}

int
UndoModifyChar::undo (Window *wp)
{
  Buffer *bp = wp->w_bufp;
  if (u_point < bp->b_contents.p1 || u_point + 1 > bp->b_contents.p2)
    FEsimple_error (Eundo_outside_region);
  bp->goto_char (wp->w_point, u_point);
  bp->overwrite_chars (wp, &cc, 1);
  if (xsymbol_value (Vmove_forward_after_undo_deletion) == Qnil)
    bp->goto_char (wp->w_point, u_point);
  return 1;
}

int
Buffer::undo_step (Window *wp, UndoInfo *&undo, int &mod)
{
  UndoInfo *u = undo;
  if (!u)
    return 0;
  u->undo (wp);
  mod = u->modifiedp ();
  undo = u->prev ();
  delete u;
  u = undo;
  if (!u || u->boundp ())
    {
      if (UndoInfo::u_status == UndoInfo::UNDO)
        b_undo_count--;
      return 0;
    }
  return 1;
}

void
Buffer::process_undo (UndoInfo *&undo)
{
  Window *wp = selected_window ();
  int mod;

  while (undo_step (wp, undo, mod))
    ;

  if (b_modified != mod)
    {
      b_modified = mod;
      b_need_auto_save = mod;
      modify_mode_line ();
      maybe_modify_buffer_bar ();
      if (!b_modified && symbol_value (Slock_file, this) == Kedit)
        unlock_file ();
    }
}

class SaveUndoStatus
{
public:
  SaveUndoStatus (UndoInfo::undo_status s)
    {
      UndoInfo::u_status = s;
    }
  ~SaveUndoStatus ()
    {
      UndoInfo::u_status = UndoInfo::NONE;
    }
};

lisp
Fundo ()
{
  Buffer *bp = selected_buffer ();
  if (symbol_value_as_integer (Vkept_undo_information, bp) <= 0)
    {
      bp->clear_undo_info ();
      FEsimple_error (Eundo_information_not_kept);
    }
  if (!bp->b_undo)
    FEsimple_error (Eno_undo_information);
  bp->check_read_only ();
  SaveUndoStatus s (UndoInfo::UNDO);
  bp->process_undo (bp->b_undo);
  //message (1, Eundo);
  return Qt;
}

lisp
Fredo ()
{
  Buffer *bp = selected_buffer ();
  if (!bp->b_redo)
    FEsimple_error (Ecannot_redo_more);
  bp->check_read_only ();
  SaveUndoStatus s (UndoInfo::REDO);
  bp->process_undo (bp->b_redo);
  //message (1, Eredo);
  return Qt;
}

lisp
Fbuffer_can_undo_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->b_undo);
}

lisp
Fbuffer_can_redo_p (lisp buffer)
{
  return boole (Buffer::coerce_to_buffer (buffer)->b_redo);
}

point_t
Buffer::last_modified_point () const
{
  for (UndoInfo *u = b_undo; u; u = u->prev ())
    if (u->undo_point () >= 0)
      return u->undo_point ();
  return b_last_modified;
}

lisp
Flast_modified_point (lisp buffer)
{
  point_t p = Buffer::coerce_to_buffer (buffer)->last_modified_point ();
  return p == -1 ? Qnil : make_fixnum (p);
}

void
Buffer::call_post_buffer_modified_hook (lisp ope, Point &point,
                                        point_t from, point_t to)
{
  lisp undo;
  switch (UndoInfo::status ())
    {
    case UndoInfo::UNDO:
      undo = Sundo;
      break;
    case UndoInfo::REDO:
      undo = Sredo;
      break;
    default:
      undo = Qnil;
      break;
    }

  suppress_gc sgc;
  point_t opoint = point.p_point;
  b_in_post_modified_hook = 1;
  try
    {
      run_hook (Vpost_buffer_modified_hook, lbp, ope,
                make_fixnum (from), make_fixnum (to), undo);
    }
  catch (nonlocal_jump &)
    {
      b_in_post_modified_hook = 0;
      b_post_modified_hook_enabled = 0;
      throw;
    }
  b_in_post_modified_hook = 0;
  if (point.p_point != opoint)
    goto_char (point, opoint);
}
