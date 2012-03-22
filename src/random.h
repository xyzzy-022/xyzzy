// -*-C++-*-
#ifndef _random_h_
# define _random_h_

/* random state */

#include "dsfmt/dSFMT.h"

class Random
{
public:
  enum {INDEX_MAX = (DSFMT_N + 1) * 4};

  void alloc_random_state ();
  void free_random_state ();
  void init_random_state (const Random& random);
  int & index () const;
  uint32_t * state () const;
  uint32_t & state (int index) const;

  void srandom (long);
  double random ();

private:
  dsfmt_t *dsfmt;
};

class lrandom_state: public lisp_object
{
public:
  Random object;

  ~lrandom_state () {object.free_random_state ();}
};

# define random_state_p(X) typep ((X), Trandom_state)

inline void
check_random_state (lisp x)
{
  check_type (x, Trandom_state, Qrandom_state);
}

inline Random &
xrandom_state_object (lisp x)
{
  assert (random_state_p (x));
  return ((lrandom_state *)x)->object;
}

inline lrandom_state *
make_random_state ()
{
  lrandom_state *p = ldata <lrandom_state, Trandom_state>::lalloc ();
  p->object.alloc_random_state ();
  return p;
}

lisp make_random_state (lisp);

#endif
