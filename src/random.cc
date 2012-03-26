#include "ed.h"
#include <time.h>

Random::Random (long seed)
{
  srandom (seed);
}

void
Random::store_initial (long seed)
{
  memset (X, 0, sizeof X);
  X[INDEX_MAX - 1] = seed;
  long *x = X;
  long t = 1;
  for (int i = 0, j = 20; i < INDEX_MAX - 2; i++, j += 21)
    {
      if (j >= INDEX_MAX - 1)
        j -= INDEX_MAX;
      x[j] = t;
      t = seed - t;
      if (t < 0)
        t += RANDOM_MAX;
      seed = x[j];
    }
}

void
Random::store ()
{
  int i;
  long *x, *x24;
#define x55 x
  for (i = 0, x = X, x24 = x + INDEX_MAX - 24; i < 24; i++, x++, x24++)
    {
      *x = *x24 - *x55;
      if (*x < 0)
        *x += RANDOM_MAX;
    }
  for (i = 24, x = X + 24, x24 = X; i < INDEX_MAX; i++, x++, x24++)
    {
      *x = *x24 - *x55;
      if (*x < 0)
        *x += RANDOM_MAX;
    }
}

void
Random::srandom (long seed)
{
  store_initial (seed);
  for (int i = 0; i < 5; i++)
    store ();
  index = 0;
}

long
Random::random ()
{
  if (index >= INDEX_MAX)
    {
      store ();
      index = 0;
    }
  return X[index++];
}

lisp
make_random_state (lisp keys)
{
  lisp v = find_keyword (Kdata, keys);
  if (!general_vector_p (v) || xvector_length (v) != Random::INDEX_MAX + 1)
    FEprogram_error (Einvalid_random_state_initializer, v);
  lisp *x = xvector_contents (v);
  for (int i = 0; i < Random::INDEX_MAX + 1; i++, x++)
    if (!fixnump (*x))
      FEprogram_error (Einvalid_random_state_initializer, v);
  x = xvector_contents (v);
  int n = fixnum_value (*x++);
  if (n < 0 || n >= Random::INDEX_MAX)
    FEprogram_error (Einvalid_random_state_initializer, v);
  lrandom_state *p = make_random_state ();
  p->object.index = n;
  for (int i = 0; i < Random::INDEX_MAX; i++, x++)
    p->object.X[i] = fixnum_value (*x);
  return p;
}

static lisp
coerce_to_random_state (lisp state)
{
  if (!state || state == Qnil)
    {
      state = xsymbol_value (Vrandom_state);
      if (!random_state_p (state))
        {
          xsymbol_value (Vrandom_state) = xsymbol_value (Vdefault_random_state);
          FEprogram_error (Erandom_state_is_not_random_state, state);
        }
    }
  else
    check_random_state (state);
  return state;
}

/*GENERIC_FUNCTION:NUMBER*/
lisp
Frandom (lisp number, lisp state)
{
  state = coerce_to_random_state (state);
  double d = double (xrandom_state_object (state).random ()) / Random::RANDOM_MAX;
  d = d * coerce_to_double_float (number);
  switch (number_typeof (number))
    {
    case Tshort_intP:
    case Tlong_int:
      return make_fixnum (int (d));

    case Tbignum:
      return make_integer (double_to_bignum_rep (d));

    case Tfraction:
      return flonum_to_rational (d);

    case Tsingle_float:
      return make_single_float (float (d));

    default: // double-float
      return make_double_float (d);
    }
}

lisp
Fmake_random_state (lisp state)
{
  if (state != Qt)
    state = coerce_to_random_state (state);
  lisp p = make_random_state ();
  if (state == Qt)
    xrandom_state_object (p).srandom (static_cast<long> (time (0)));
  else
    xrandom_state_object (p) = xrandom_state_object (state);
  return p;
}

lisp
Frandom_state_p (lisp object)
{
  return boole (random_state_p (object));
}
