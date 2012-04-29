#include "stdafx.h"
#include "ed.h"

utimer::timer_entry::timer_entry (const utime_t &time, u_long interval,
                                  lisp fn, int one_shot_p)
     : te_time (time + interval * utime_t (UNITS_PER_SEC / 1000)),
       te_interval (interval), te_fn (fn),
       te_flags (one_shot_p ? (TE_ONE_SHOT | TE_NEW) : TE_NEW)
{
}

utimer::utimer ()
     : t_hwnd (0), t_timer_on (0), t_in_timer (0)
{
}

utimer::~utimer ()
{
  while (!t_entries.empty_p ())
    delete t_entries.remove_head ();
  while (!t_defers.empty_p ())
    delete t_defers.remove_head ();
}

void
utimer::stop_timer ()
{
  if (t_timer_on)
    {
      KillTimer (t_hwnd, TID_USER);
      t_timer_on = 0;
    }
}

void
utimer::start_timer (const utime_t &t)
{
  stop_timer ();

  timer_entry *entry = t_entries.head ();
  if (entry)
    {
      utime_t d = (entry->te_time - t) / (timer_entry::UNITS_PER_SEC / 1000);
      if (d < 0)
        d = 0;
      else if (d >= LONG_MAX)
        d = LONG_MAX;
      SetTimer (t_hwnd, TID_USER, UINT (d), 0);
      t_timer_on = 1;
    }
}

void
utimer::insert (timer_entry *entry)
{
  for (timer_entry *p = t_entries.head (); p; p = p->next ())
    if (entry->te_time < p->te_time)
      {
        t_entries.insert_before (entry, p);
        return;
      }
  t_entries.add_tail (entry);
}

void
utimer::timer ()
{
  stop_timer ();

  if (t_in_timer)
    return;
  t_in_timer = 1;

  utime_t t;
  current_time (t);
  while (1)
    {
      timer_entry *entry = t_entries.head ();
      if (!entry)
        break;
      if (entry->te_time > t)
        break;

      lisp fn = entry->te_fn;

      t_entries.remove (entry);
      if (entry->te_flags & timer_entry::TE_ONE_SHOT)
        delete entry;
      else
        t_defers.add_tail (entry);

      try {Ffuncall (fn, Qnil);} catch (nonlocal_jump &) {}
    }

  t_in_timer = 0;

  current_time (t);
  while (!t_defers.empty_p ())
    {
      timer_entry *entry = t_defers.remove_head ();
      if (entry->te_flags & timer_entry::TE_NEW)
        entry->te_flags &= ~timer_entry::TE_NEW;
      entry->te_time = t + (entry->te_interval
                            * utime_t (timer_entry::UNITS_PER_SEC / 1000));
      insert (entry);
    }

  start_timer (t);
}

void
utimer::add (u_long interval, lisp fn, int one_shot_p)
{
  utime_t t;
  current_time (t);
  timer_entry *entry = new timer_entry (t, interval, fn, one_shot_p);
  if (t_in_timer)
    t_defers.add_tail (entry);
  else
    {
      insert (entry);
      start_timer (t);
    }
}

int
utimer::remove (xlist <timer_entry> &list, lisp fn)
{
  for (timer_entry *p = list.head (); p; p = p->next ())
    if (p->te_fn == fn)
      {
        delete list.remove (p);
        return 1;
      }
  return 0;
}

int
utimer::remove (lisp fn)
{
  if (!remove (t_entries, fn) && !remove (t_defers, fn))
    return 0;
  if (!t_in_timer)
    {
      utime_t t;
      current_time (t);
      start_timer (t);
    }
  return 1;
}

void
utimer::gc_mark (void (*f)(lisp))
{
  for (timer_entry *p = t_entries.head (); p; p = p->next ())
    (*f)(p->te_fn);
  for (timer_entry *p = t_defers.head (); p; p = p->next ())
    (*f)(p->te_fn);
}

lisp
Fstart_timer (lisp linterval, lisp lfn, lisp lone_shot_p)
{
  double interval (coerce_to_double_float (linterval) * 1000);
  if (interval < 0 || interval > LONG_MAX)
    FErange_error (linterval);
  app.user_timer.add (u_long (interval), lfn, lone_shot_p && lone_shot_p != Qnil);
  return Qt;
}

lisp
Fstop_timer (lisp fn)
{
  return boole (app.user_timer.remove (fn));
}
