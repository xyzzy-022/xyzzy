// -*-C++-*-
#ifndef _random_h_
# define _random_h_

/* random state */

struct Random
{
  enum {INDEX_MAX = 55};
  enum {DEFAULT_SEED = 1};
  enum {RANDOM_BITS = BITS_PER_LONG - 1};
  enum {RANDOM_MAX = LONG_MAX & ~1};

  int index;
  long X[INDEX_MAX];

  void store ();
  void store_initial (long);
  Random (long = DEFAULT_SEED);
//  Random (Random &);

  void srandom (long);
  long random ();
};

class lrandom_state: public lisp_object
{
public:
  Random object;
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
  return ldata <lrandom_state, Trandom_state>::lalloc ();
}

lisp make_random_state (lisp);

#endif
