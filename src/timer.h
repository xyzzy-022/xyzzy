#ifndef _timer_h_
# define _timer_h_

enum
{
  TID_SLEEP = 1,
  TID_ITIMER,
  TID_USER
};

class itimer
{
  u_int it_counter;
public:
  enum {interval = 3};
  itimer () : it_counter (0) {}
  void reset ()
    {
      it_counter = 0;
    }
  void inc ()
    {
      if (it_counter != u_int (UINT_MAX))
        it_counter++;
    }
  int expired (int n) const
    {
      return n > 0 && it_counter > u_int (n / interval);
    }
};

#endif
