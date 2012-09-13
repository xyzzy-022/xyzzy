#ifndef _utimer_h_
#define _utimer_h_

#include "xlist.h"

class utimer
{
private:
  class timer_entry: public xlist_node <timer_entry>
    {
    public:
      typedef int64_t utime_t;
      enum {TE_NEW = 1, TE_ONE_SHOT = 2};
      enum {UNITS_PER_SEC = 10000000};
      utime_t te_time;
      u_long te_interval;
      lisp te_fn;
      int te_flags;
      timer_entry (const utime_t &, u_long, lisp, int);
    };

  typedef timer_entry::utime_t utime_t;
  xlist <timer_entry> t_entries;
  xlist <timer_entry> t_defers;
  HWND t_hwnd;
  int t_timer_on;
  int t_in_timer;

  static void current_time (utime_t &t)
    {
      SYSTEMTIME st;
      GetSystemTime (&st);
      SystemTimeToFileTime (&st, (FILETIME *)&t);
    }
  void start_timer (const utime_t &t);
  void stop_timer ();
  void insert (timer_entry *);
  static int remove (xlist <timer_entry> &, lisp);
public:
  utimer ();
  ~utimer ();
  void init (HWND hwnd) {t_hwnd = hwnd;}
  void cleanup () {stop_timer ();}
  void timer ();
  void add (u_long, lisp, int);
  int remove (lisp);
  void gc_mark (void (*)(lisp));
};

#endif /* _utimer_h_ */
