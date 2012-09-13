#ifndef _BIGNUM_H_
# define _BIGNUM_H_

# define SHORT_PER_LONG \
  ((sizeof (long) + sizeof (short) - 1) / sizeof (short))
# define SHORT_PER_INT64 \
  ((sizeof (int64_t) + sizeof (short) - 1) / sizeof (short))

enum logope_code
{
  LOC_SET,
  LOC_CLR,
  LOC_1,
  LOC_2,
  LOC_C1,
  LOC_C2,

  LOC_AND,
  LOC_IOR,
  LOC_XOR,
  LOC_EQV,
  LOC_NAND,
  LOC_NOR,
  LOC_ANDC1,
  LOC_ANDC2,
  LOC_ORC1,
  LOC_ORC2
};

struct bignum_rep
{
#if 0
  u_short br_space;
  u_short br_len;
#else
  int br_space;
  int br_len;
#endif
  enum {NEGATIVE = 0, POSITIVE = 1};
  char br_sign;
  char br_allocated;
  u_short br_data[1];

  bignum_rep *normalize ();
  void copy (const bignum_rep *);
  void clear_from (int = 0);

  int constp () const;
  long log2 () const;
  long howlong () const;

  int zerop () const;
  int plusp () const;
  int minusp () const;
  int evenp () const;
  int oddp () const;
  int sign () const;

  long to_long () const;
  u_long to_ulong () const;
  double to_double () const;
  long coerce_to_long () const;
  int64_t coerce_to_int64 () const;
  int is_long () const;
  int is_ulong () const;
  int fmtwidth (u_long) const;
  char *to_ascii (char *, int, int, int, const char *) const;
};

struct bignum_rep_short: public bignum_rep
{
  bignum_rep_short (u_short);
};

struct bignum_rep_long: public bignum_rep
{
  u_short u[SHORT_PER_LONG - 1];
  void init (long);
  void init (u_long);
  bignum_rep_long () {/* warn: uninitialized */}
  bignum_rep_long (int);
  bignum_rep_long (u_int);
  bignum_rep_long (long);
  bignum_rep_long (u_long);
};

extern bignum_rep bignum_rep_zero;
extern bignum_rep bignum_rep_one;
extern bignum_rep bignum_rep_minus_one;

#define BIGNUM_ALLOCATE_MAX (64 * 1024)
extern long bignum_allocated_bytes;

inline int
bignum_rep::constp () const
{
  return !br_space;
}

inline int
bignum_rep::zerop () const
{
  return !br_len;
}

inline int
bignum_rep::plusp () const
{
  return br_sign == POSITIVE;
}

inline int
bignum_rep::minusp () const
{
  return br_sign == NEGATIVE;
}

inline int
bignum_rep::evenp () const
{
  return !br_len || !(br_data[0] & 1);
}

inline int
bignum_rep::oddp () const
{
  return br_len && br_data[0] & 1;
}

inline int
bignum_rep::sign () const
{
  return !br_len ? 0 : (br_sign == POSITIVE ? 1 : -1);
}

inline void
bignum_rep::copy (const bignum_rep *src)
{
  memcpy (br_data, src->br_data, min (br_len, src->br_len) * sizeof *br_data);
}

inline
bignum_rep_long::bignum_rep_long (int x)
{
  init (long (x));
}

inline
bignum_rep_long::bignum_rep_long (u_int x)
{
  init (u_long (x));
}

inline
bignum_rep_long::bignum_rep_long (long x)
{
  init (x);
}

inline
bignum_rep_long::bignum_rep_long (u_long x)
{
  init (x);
}

inline void
br_delete (bignum_rep *r)
{
  if (r && r->br_allocated)
    {
      bignum_allocated_bytes -= min (BIGNUM_ALLOCATE_MAX, r->br_len);
      delete [] (char *)r;
    }
}

bignum_rep *br_new (int);
int br_ucompare (const bignum_rep *, const bignum_rep *);
int br_compare (const bignum_rep *, const bignum_rep *);
bignum_rep *br_copy (bignum_rep *, const bignum_rep *, int);
bignum_rep *br_copy (bignum_rep *, u_long);
bignum_rep *br_copy (bignum_rep *, long);
bignum_rep *br_copy (bignum_rep *, uint64_t);
bignum_rep *br_copy (bignum_rep *, int64_t);

inline bignum_rep *
br_copy (bignum_rep *old, const bignum_rep *src)
{
  return br_copy (old, src, src->br_sign);
}

inline bignum_rep *
negate (bignum_rep *r, const bignum_rep *x)
{
  return br_copy (r, x, x->zerop () ? x->br_sign : int (!x->br_sign));
}

inline bignum_rep *
abs (bignum_rep *r)
{
  return r->plusp () ? r : negate (r, r);
}

bignum_rep *add (bignum_rep *, const bignum_rep *, const bignum_rep *, int);
bignum_rep *multiply (bignum_rep *, const bignum_rep *, const bignum_rep *);
bignum_rep *divide (bignum_rep *, const bignum_rep *, const bignum_rep *);
int remainder (const bignum_rep *, u_short);
void truncate (bignum_rep *&, bignum_rep *&,
               const bignum_rep *, const bignum_rep *);
void floor (bignum_rep *&, bignum_rep *&,
            const bignum_rep *, const bignum_rep *);
void ceiling (bignum_rep *&, bignum_rep *&,
              const bignum_rep *, const bignum_rep *);
void round (bignum_rep *&, bignum_rep *&,
            const bignum_rep *, const bignum_rep *);
void ash (bignum_rep *&, const bignum_rep *, long);
bignum_rep *expt (bignum_rep *, const bignum_rep *, long);
bignum_rep *gcd (const bignum_rep *, const bignum_rep *);
bignum_rep *lcm (const bignum_rep *, const bignum_rep *);
bignum_rep *isqrt (const bignum_rep *);
Char *ato_bignum_rep (bignum_rep *&, const Char *, int, int);
bignum_rep *ato_bignum_rep (const Char *, int, int);
void lognot (bignum_rep *&, const bignum_rep *);
void logope (bignum_rep *&, logope_code,
             const bignum_rep *, const bignum_rep *);
int logtest (const bignum_rep *, const bignum_rep *);
int logbitp (const bignum_rep *, long);
long logcount (const bignum_rep *);
bignum_rep *double_to_bignum_rep (double);
bignum_rep *double_to_bignum_rep_ratio (double, bignum_rep **);

inline bignum_rep *
add (bignum_rep *r, const bignum_rep *x, long y, int negate)
{
  bignum_rep_long yy (y);
  return add (r, x, &yy, negate);
}

inline bignum_rep *
multiply (bignum_rep *r, const bignum_rep *x, long y)
{
  bignum_rep_long yy (y);
  return multiply (r, x, &yy);
}

inline bignum_rep *
multiply (bignum_rep *r, const bignum_rep *x, u_short y)
{
  bignum_rep_short yy (y);
  return multiply (r, x, &yy);
}

class safe_bignum_rep
{
  bignum_rep *rep;
public:
  safe_bignum_rep () : rep (0) {}
  safe_bignum_rep (bignum_rep *r) : rep (r) {}
  ~safe_bignum_rep () {br_delete (rep);}
  void operator = (bignum_rep *r) {rep = r;}
  operator bignum_rep * () {return rep;}
  bignum_rep *operator -> () {return rep;}
  bignum_rep *release ()
    {
      bignum_rep *r = rep;
      rep = 0;
      return r;
    }
  void discard () {rep = 0;}
};

#endif
