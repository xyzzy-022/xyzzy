#include "ed.h"
#include <limits.h>
#include <string.h>
#include <math.h>

#define BR_SHIFT (sizeof (u_short) * CHAR_BIT)
#define BR_RADIX (u_long (1L << BR_SHIFT))
#define BR_MAX (u_long (BR_RADIX - 1))
#define BR_MIN (u_long (BR_RADIX >> 1))

#define BR_POSITIVE bignum_rep::POSITIVE
#define BR_NEGATIVE bignum_rep::NEGATIVE

#define BR_PADSIZE 16

bignum_rep bignum_rep_zero = {0, 0, BR_POSITIVE, 0, {0}};
bignum_rep bignum_rep_one = {0, 1, BR_POSITIVE, 0, {1}};
bignum_rep bignum_rep_minus_one = {0, 1, BR_NEGATIVE, 0, {1}};

long bignum_allocated_bytes;

static inline u_short
lowpart (u_long x)
{
  return u_short (x & BR_MAX);
}

static inline u_long
down (u_long x)
{
  return (x >> BR_SHIFT) & BR_MAX;
}

static inline u_long
up (u_long x)
{
  return x << BR_SHIFT;
}

bignum_rep *
bignum_rep::normalize ()
{
  for (const u_short *d = br_data + br_len; br_len > 0 && !*--d; br_len--)
    ;
  if (!br_len)
    br_sign = BR_POSITIVE;
  return this;
}

void
bignum_rep::clear_from (int from)
{
  int l = br_len - from;
  if (l > 0)
    memset (br_data + from, 0, sizeof *br_data * l);
}

bignum_rep_short::bignum_rep_short (u_short x)
{
  if (!x)
    br_len = 0;
  else
    {
      br_len = 1;
      br_data[0] = x;
    }
  br_sign = BR_POSITIVE;
  br_space = 1;
  br_allocated = 0;
}

void
bignum_rep_long::init (long x)
{
  if (x < 0)
    {
      x = -x;
      br_sign = BR_NEGATIVE;
    }
  else
    br_sign = BR_POSITIVE;

  u_short *d = br_data;
  while (x)
    {
      *d++ = lowpart (x);
      x = down (x);
    }

  br_len = d - br_data;
  br_space = SHORT_PER_LONG;
  br_allocated = 0;
}

void
bignum_rep_long::init (u_long x)
{
  u_short *d = br_data;
  while (x)
    {
      *d++ = lowpart (x);
      x = down (x);
    }
  br_len = d - br_data;
  br_space = 0;
  br_sign = BR_POSITIVE;
  br_allocated = 0;
}

bignum_rep *
br_new (int len)
{
  int size = (len + (BR_PADSIZE - 1)) & ~(BR_PADSIZE - 1);
  if (!size)
    size = BR_PADSIZE;
#if 0
  if (size >= BR_MAX)
    FEbignum_overflow ();
#endif
  bignum_rep *r = (bignum_rep *)new char[sizeof (bignum_rep)
                                         + (size - 1) * sizeof (u_short)];
  bignum_allocated_bytes += min (BIGNUM_ALLOCATE_MAX, len);
  r->br_space = size;
  r->br_len = len;
  r->br_allocated = 1;
  return r;
}

static bignum_rep *
br_alloc (bignum_rep *old, const bignum_rep *src, int len, int sign)
{
  if (!len)
    len = 1;
  bignum_rep *r;
  int srclen = src ? src->br_len : 0;
  if (!old || old->br_space < len)
    r = br_new (len);
  else
    {
      r = old;
      r->br_len = len;
    }
  r->br_sign = sign;
  if (!src)
    r->clear_from ();
  else
    {
      if (r != src)
        r->copy (src);
      r->clear_from (srclen);
    }
  if (r != old)
    br_delete (old);
  return r;
}

inline bignum_rep *
br_alloc (bignum_rep *old, const bignum_rep *src, int len)
{
  return br_alloc (old, src, len, src->br_sign);
}

inline bignum_rep *
br_realloc (bignum_rep *r, int len)
{
  return br_alloc (r, r, len, r->br_sign);
}

inline bignum_rep *
br_calloc (bignum_rep *r, int len)
{
  return br_alloc (r, 0, len, BR_POSITIVE);
}

inline bignum_rep *
br_calloc (int len)
{
  return br_calloc (0, len);
}

bignum_rep *
br_copy (bignum_rep *old, const bignum_rep *src, int sign)
{
  if (old == src && old->br_sign == sign)
    return old;
  if (!old || old->constp ())
    {
      if (src->zerop ())
        return &bignum_rep_zero;
      if (src->br_len == 1 && src->br_data[0] == 1)
        return sign == BR_POSITIVE ? &bignum_rep_one : &bignum_rep_minus_one;
    }
  return br_alloc (old, src, src->br_len, sign);
}

bignum_rep *
br_copy (bignum_rep *old, u_long x)
{
  bignum_rep *r = br_alloc (old, 0, SHORT_PER_LONG, BR_POSITIVE);
  u_short *d = r->br_data;
  while (x)
    {
      *d++ = lowpart (x);
      x = down (x);
    }
  r->br_len = d - r->br_data;
  return r;
}

bignum_rep *
br_copy (bignum_rep *old, long x)
{
  bignum_rep *r;
  if (x < 0)
    {
      r = br_copy (old, u_long (-x));
      r->br_sign = BR_NEGATIVE;
    }
  else
    r = br_copy (old, u_long (x));
  return r;
}

bignum_rep *
br_copy (bignum_rep *old, large_int li)
{
  int sign;
  if (li.hi >= 0)
    sign = BR_POSITIVE;
  else
    {
      li = negsi (li);
      sign = BR_NEGATIVE;
    }

  bignum_rep *r = br_alloc (old, 0, SHORT_PER_LONG * 2, sign);
  u_short *d = r->br_data;
  for (u_long x = li.lo; x; x = down (x))
    *d++ = lowpart (x);
  d = r->br_data + SHORT_PER_LONG;
  for (x = li.hi; x; x = down (x))
    *d++ += lowpart (x);
  r->br_len = d - r->br_data;
  return r;
}

bignum_rep *
br_copy_zero (bignum_rep *r)
{
  if (!r || r->constp ())
    return &bignum_rep_zero;
  r->br_len = 0;
  r->br_sign = BR_POSITIVE;
  return r;
}

bignum_rep *
br_copy_one (bignum_rep *r, int sign)
{
  if (!r || r->constp () || !r->br_space)
    return sign == BR_POSITIVE ? &bignum_rep_one : &bignum_rep_minus_one;
  r->br_len = 1;
  r->br_data[0] = 1;
  r->br_sign = sign;
  return r;
}

static inline int
br_compare (const u_short *x, const u_short *y, int l)
{
  int d = 0;
  for (x += l, y += l; l > 0 && !(d = *--x - *--y); l--)
    ;
  return d;
}

int
br_ucompare (const bignum_rep *x, const bignum_rep *y)
{
  int d = x->br_len - y->br_len;
  if (!d)
    d = br_compare (x->br_data, y->br_data, x->br_len);
  return d;
}

int
br_compare (const bignum_rep *x, const bignum_rep *y)
{
  int d = x->br_sign - y->br_sign;
  if (!d)
    {
      d = br_ucompare (x, y);
      if (x->br_sign == BR_NEGATIVE)
        d = -d;
    }
  return d;
}

int
bignum_rep::is_long () const
{
  if (br_len < SHORT_PER_LONG)
    return 1;
  if (br_len > SHORT_PER_LONG)
    return 0;
  u_long t = br_data[SHORT_PER_LONG - 1];
  if (t < BR_MIN)
    return 1;
  if (br_sign == BR_NEGATIVE && t == BR_MIN)
    {
      for (int i = 0; i < SHORT_PER_LONG - 1; i++)
        if (br_data[i])
          return 0;
      return 1;
    }
  return 0;
}

long
bignum_rep::to_long () const
{
  if (!br_len)
    return 0;
  if (br_len > SHORT_PER_LONG)
    return plusp () ? LONG_MAX : LONG_MIN;
  if (br_len < SHORT_PER_LONG)
    {
      long sum = br_data[br_len - 1];
      if (SHORT_PER_LONG > 2)
        for (int i = br_len - 2; i >= 0; i--)
          sum = up (sum) + br_data[i];
      return plusp () ? sum : -sum;
    }

  u_long t = br_data[SHORT_PER_LONG - 1];
  if (t >= BR_MIN)
    return plusp () ? LONG_MAX : LONG_MIN;
  t = up (t) + br_data[SHORT_PER_LONG - 2];
  if (SHORT_PER_LONG > 2)
    for (int i = SHORT_PER_LONG - 3; i >= 0; i--)
      t = up (t) + br_data[i];
  return plusp () ? t : -long (t);
}

int
bignum_rep::is_ulong () const
{
  if (minusp ())
    return 0;
  return br_len <= SHORT_PER_LONG;
}

u_long
bignum_rep::to_ulong () const
{
  if (!br_len)
    return 0;
  if (minusp ())
    return 0;
  if (br_len > SHORT_PER_LONG)
    return ULONG_MAX;
  u_long sum = 0;
  for (int i = br_len - 1; i >= 0; i--)
    sum = up (sum) + br_data[i];
  return sum;
}

long
bignum_rep::coerce_to_long () const
{
  if (!br_len)
    return 0;
  int l = min (br_len, int (SHORT_PER_LONG));
  u_long sum = 0;
  for (int i = l - 1; i >= 0; i--)
    sum = up (sum) + br_data[i];
  return plusp () ? sum : -long (sum);
}

double
bignum_rep::to_double () const
{
  if (!br_len)
    return 0;
  double d = br_data[br_len - 1];
  const double e = BR_RADIX;
  for (int i = br_len - 2; i >= 0; i--)
    d = d * e + br_data[i];
  return plusp () ? d : -d;
}

bignum_rep *
add (bignum_rep *r, const bignum_rep *x, const bignum_rep *y, int negate)
{
  int yl = y->br_len;
  if (!yl)
    return br_copy (r, x);

  int ysign = negate ? !y->br_sign : y->br_sign;
  int xl = x->br_len;
  if (!xl)
    return br_copy (r, y, ysign);

  int xsign = x->br_sign;

  int xr_eq = x == r;
  int yr_eq = y == r;

  const u_short *ap, *ae;
  const u_short *bp;
  u_short *rp, *re;

  if (xsign != ysign)
    {
      int f = br_ucompare (x, y);
      if (!f)
        return br_copy_zero (r);

      if (f < 0)
        {
          if (xr_eq || yr_eq)
            r = br_realloc (r, yl);
          else
            r = br_calloc (r, yl);
          r->br_sign = ysign;
          rp = r->br_data;
          re = rp + yl;
          ap = xr_eq ? rp : x->br_data;
          ae = ap + xl;
          bp = yr_eq ? rp : y->br_data;
        }
      else
        {
          if (xr_eq || yr_eq)
            r = br_realloc (r, xl);
          else
            r = br_calloc (r, xl);
          r->br_sign = xsign;
          rp = r->br_data;
          re = rp + xl;
          ap = yr_eq ? rp : y->br_data;
          ae = ap + yl;
          bp = xr_eq ? rp : x->br_data;
        }

      u_long borrow = 1;

      do
        {
          borrow += u_long (*bp++) + BR_MAX - u_long (*ap++);
          *rp++ = lowpart (borrow);
          borrow = down (borrow);
        }
      while (ap < ae);

      while (!borrow && rp < re)
        {
          borrow = u_long (*bp++) + BR_MAX;
          *rp++ = lowpart (borrow);
          borrow = down (borrow);
        }

      if (rp != bp)
        while (rp < re)
          *rp++ = *bp++;
    }
  else
    {
      if (yl >= xl)
        {
          if (xr_eq || yr_eq)
            r = br_realloc (r, yl + 1);
          else
            r = br_calloc (r, yl + 1);
          rp = r->br_data;
          re = rp + yl;
          ap = xr_eq ? rp : x->br_data;
          ae = ap + xl;
          bp = yr_eq ? rp : y->br_data;
        }
      else
        {
          if (xr_eq || yr_eq)
            r = br_realloc (r, xl + 1);
          else
            r = br_calloc (r, xl + 1);
          rp = r->br_data;
          re = rp + xl;
          ap = yr_eq ? rp : y->br_data;
          ae = ap + yl;
          bp = xr_eq ? rp : x->br_data;
        }
      r->br_sign = xsign;

      u_long sum = 0;

      do
        {
          sum += u_long (*bp++) + u_long (*ap++);
          *rp++ = lowpart (sum);
          sum = down (sum);
        }
      while (ap < ae);

      while (rp < re && sum)
        {
          sum += u_long (*bp++);
          *rp++ = lowpart (sum);
          sum = down (sum);
        }

      if (sum)
        *rp = lowpart (sum);
      else if (rp != bp)
        while (rp < re)
          *rp++ = *bp++;
    }

  return r->normalize ();
}

bignum_rep *
multiply (bignum_rep *r, const bignum_rep *x, const bignum_rep *y)
{
  int xl = x->br_len;
  int yl = y->br_len;
  int rsign = x->br_sign == y->br_sign;

  if (!xl || !yl)
    return br_copy_zero (r);
  if (xl == 1 && x->br_data[0] == 1)
    return br_copy (r, y, rsign);
  if (yl == 1 && y->br_data[0] == 1)
    return br_copy (r, x, rsign);

  int rl = xl + yl;

  bignum_rep *old;
  if (r == x || r == y)
    {
      old = r;
      r = br_calloc (rl);
    }
  else
    {
      old = 0;
      r = br_calloc (r, rl);
    }
  r->br_sign = rsign;

  const u_short *xp = x->br_data;
  const u_short *xe = xp + xl;
  const u_short *yp0 = y->br_data;
  const u_short *ye = yp0 + yl;
  u_short *rp0 = r->br_data;

  do
    {
      u_long xn = *xp++;
      if (xn)
        {
          const u_short *yp = yp0;
          u_short *rp = rp0++;
          u_long sum = 0;
          do
            {
              sum += u_long (*rp) + xn * u_long (*yp++);
              *rp++ = lowpart (sum);
              sum = down (sum);
            }
          while (yp < ye);
          if (sum)
            *rp = u_short (sum);
        }
      else
        rp0++;
    }
  while (xp < xe);
  br_delete (old);
  return r->normalize ();
}

static void
do_divide (u_short *r0, const u_short *y0, int yl, u_short *qs, int ql)
{
  const u_short *ye = y0 + yl;
  u_short d1 = y0[yl - 1];
  u_short d2 = y0[yl - 2];

  for (int l = ql - 1, i = l + yl; l >= 0; l--, i--)
    {
      u_short qhat;
      if (d1 == r0[i])
        qhat = BR_MAX;
      else
        {
          u_long lr = up (u_long (r0[i])) + r0[i - 1];
          qhat = u_short (lr / d1);
        }

      while (1)
        {
          u_short t[3];
          u_long prod = u_long (d2) * u_long (qhat);
          t[0] = lowpart (prod);
          prod = down (prod) + u_long (d1) * u_long (qhat);
          t[1] = lowpart (prod);
          prod = down (prod);
          t[2] = lowpart (prod);
          if (br_compare (t, &r0[i - 2], 3) > 0)
            qhat--;
          else
            break;
        }

      const u_short *yp = y0;
      u_short *rp = r0 + l;
      u_long prod = 0;
      u_long borrow = 1;
      while (yp < ye)
        {
          prod = u_long (qhat) * u_long (*yp++) + down (prod);
          borrow += u_long (*rp) + BR_MAX - u_long (lowpart (prod));
          *rp++ = lowpart (borrow);
          borrow = down (borrow);
        }
      borrow += u_long (*rp) + BR_MAX - u_long (down (prod));
      *rp = lowpart (borrow);
      borrow = down (borrow);

      if (!borrow)
        {
          qhat--;
          yp = y0;
          rp = r0 + l;
          borrow = 0;
          while (yp < ye)
            {
              borrow = u_long (*rp) + u_long (*yp++) + down (borrow);
              *rp++ = lowpart (borrow);
            }
          *rp = 0;
        }
      if (qs)
        qs[l] = qhat;
    }
}

int
remainder (const bignum_rep *r, u_short div)
{
  int xl = r->br_len;
  if (!xl || div == 1)
    return 0;

  const u_short *const x = r->br_data;
  const u_short *xp = x + xl;
  u_long rem = 0;
  do
    rem = (up (rem) + u_long (*--xp)) % div;
  while (xp > x);
  return rem;
}

#ifdef _M_IX86
# pragma warning (disable:4035)
static int
divide (const u_short *x, int xl, u_short xdiv, u_short *r)
{
  __asm
    {
      xor edi, edi;
      mov eax, xl;
      mov esi, x;
      mov di, xdiv;
      mov ecx, r;
      test eax, eax;
      je short dividend_zero;
      cmp di, 1;
      jne short divisor_not_one;
      xor eax, eax;
      jmp short dividend_zero;

    divisor_not_one:
      xor edx, edx;
      lea ebx, dword ptr [esi+eax*2];
      lea ecx, dword ptr [ecx+eax*2];

    again:
      xor eax, eax;
      sub ebx, 2;
      shl edx, 16;
      mov ax, word ptr [ebx];
      sub ecx, 2;
      add eax, edx;
      xor edx, edx;
      div edi;
      cmp esi, ebx;
      mov word ptr [ecx], ax;
      jb short again;
      mov eax, edx;
    dividend_zero:
    }
}
# pragma warning (default:4035)
#else /* not _M_IX86 */
static int
divide (const u_short *x, int xl, u_short div, u_short *r)
{
  if (!xl || div == 1)
    return 0;

  const u_short *xp = x + xl;
  u_short *rp = r + xl;
  u_long rem = 0;
  do
    {
      rem = up (rem) + u_long (*--xp);
      *--rp = u_short (rem / div);
      rem %= div;
    }
  while (xp > x);
  return rem;
}
#endif /* not _M_IX86 */

bignum_rep *
divide (bignum_rep *q, const bignum_rep *x, const bignum_rep *y)
{
  int xl = x->br_len;
  int yl = y->br_len;

  if (!yl)
    FEdivision_by_zero ();

  int f = br_ucompare (x, y);
  if (f < 0)
    return br_copy_zero (q);

  int qsign = x->br_sign == y->br_sign;
  if (!f)
    return br_copy_one (q, qsign);

  if (yl == 1)
    {
      u_short yy = y->br_data[0];
      q = br_alloc (q, x, x->br_len);
      divide (q->br_data, q->br_len, yy, q->br_data);
    }
  else
    {
      int ql = xl - yl + 1;
      u_short d = u_short (BR_RADIX / (y->br_data[yl - 1] + 1));
      if (d != 1 || q == y)
        {
          safe_bignum_rep yy (multiply (0, y, d));
          safe_bignum_rep r (d == 1 ? br_alloc (0, x, xl + 1) : multiply (0, x, d));
          q = br_calloc (q, ql);
          do_divide (r->br_data, yy->br_data, yl, q->br_data, ql);
        }
      else
        {
          safe_bignum_rep r (br_alloc (0, x, xl + 1));
          q = br_calloc (q, ql);
          do_divide (r->br_data, y->br_data, yl, q->br_data, ql);
        }
    }

  q->br_sign = qsign;
  return q->normalize ();
}

void
truncate (bignum_rep *&bq, bignum_rep *&br,
          const bignum_rep *x, const bignum_rep *y)
{
  bignum_rep *q = 0;
  bignum_rep *r = 0;

  int xl = x->br_len;
  int yl = y->br_len;

  if (!yl)
    FEdivision_by_zero ();

  int f = br_ucompare (x, y);
  if (f < 0)
    {
      r = br_copy (r, x);
      q = br_copy_zero (q);  // never fail
    }
  else
    {
      int qsign = x->br_sign == y->br_sign;
      if (!f)
        {
          q = br_copy_one (q, qsign);
          r = br_copy_zero (r);  // never fail
        }
      else if (yl == 1)
        {
          u_short yy = y->br_data[0];
          safe_bignum_rep qq (br_alloc (q, x, x->br_len));
          int rem = divide (qq->br_data, qq->br_len, yy, qq->br_data);
          r = br_copy (r, long (rem));
          q = qq.release ();
          if (rem)
            r->br_sign = x->br_sign;
        }
      else
        {
          int ql = xl - yl + 1;
          u_short d = u_short (BR_RADIX / (y->br_data[yl - 1] + 1));
          if (d != 1)
            {
              safe_bignum_rep yy (multiply (0, y, d));
              safe_bignum_rep rr (multiply (r, x, d));
              q = br_calloc (q, ql);
              r = rr.release ();
              do_divide (r->br_data, yy->br_data, yl, q->br_data, ql);
              r->normalize ();
              divide (r->br_data, r->br_len, d, r->br_data);
            }
          else
            {
              safe_bignum_rep rr (br_alloc (r, x, xl + 1));
              q = br_calloc (q, ql);
              r = rr.release ();
              do_divide (r->br_data, y->br_data, yl, q->br_data, ql);
            }
        }
      q->br_sign = qsign;
    }

  bq = q->normalize ();
  br = r->normalize ();
}

void
floor (bignum_rep *&q, bignum_rep *&r,
       const bignum_rep *x, const bignum_rep *y)
{
  truncate (q, r, x, y);
  if (!r->zerop () && (q->zerop () ? x->minusp () != y->minusp () : q->minusp ()))
    {
      safe_bignum_rep qq (q);
      safe_bignum_rep rr (r);
      qq = add (q, q, &bignum_rep_minus_one, 0);
      r = add (r, r, y, 0);
      rr.discard ();
      q = qq.release ();
    }
}

void
ceiling (bignum_rep *&q, bignum_rep *&r,
         const bignum_rep *x, const bignum_rep *y)
{
  truncate (q, r, x, y);
  if (!r->zerop () && (q->zerop () ? x->minusp () == y->minusp () : q->plusp ()))
    {
      safe_bignum_rep qq (q);
      safe_bignum_rep rr (r);
      qq = add (q, q, &bignum_rep_one, 0);
      r = add (r, r, y, 1);
      rr.discard ();
      q = qq.release ();
    }
}

void
round (bignum_rep *&q, bignum_rep *&r,
       const bignum_rep *x, const bignum_rep *y)
{
  truncate (q, r, x, y);
  if (r->zerop ())
    return;
  safe_bignum_rep qq (q);
  safe_bignum_rep rr (r);
  bignum_rep *r2 = add (0, r, r, 0);
  int f = br_ucompare (r2, y);
  br_delete (r2);
  if (f < 0 || (!f && q->evenp ()))
    {
      qq.discard ();
      rr.discard ();
    }
  else
    {
      f = q->zerop () ? x->minusp () != y->minusp () : q->minusp ();
      qq = add (q, q, &bignum_rep_one, f);
      r = add (r, r, y, !f);
      rr.discard ();
      q = qq.release ();
    }
}

inline long
bignum_rep::log2 () const
{
  return br_len ? (br_len - 1) * BR_SHIFT + ::log2 (br_data[br_len - 1]) : 0;
}

long
bignum_rep::howlong () const
{
  if (!br_len)
    return 0;
  int x = br_data[br_len - 1];
  if (minusp ())
    {
      for (int i = br_len - 2; i >= 0; i--)
        if (br_data[i])
          break;
      if (i < 0)
        x--;
    }
  return (br_len - 1) * BR_SHIFT + ::log2 (x);
}

static bignum_rep *
lshift (bignum_rep *r, const bignum_rep *x, long y)
{
  int xl = x->br_len;
  if (!xl || !y)
    return br_copy (r, x);

  int rsign = x->br_sign;

  long ay = y < 0 ? -y : y;
  int bw = ay / BR_SHIFT;
  int sw = ay % BR_SHIFT;

  if (y > 0)
    {
      int rl = bw + xl + 1;
      int xr_eq = x == r;
      if (xr_eq)
        r = br_realloc (r, rl);
      else
        r = br_calloc (r, rl);
      u_short *r0 = r->br_data;
      u_short *rp = r0 + rl;
      const u_short *x0 = xr_eq ? r0 : x->br_data;
      const u_short *xp = x0 + xl;

      u_long sum = 0;
      while (xp > x0)
        {
          sum = up (sum) + (u_long (*--xp) << sw);
          *--rp = lowpart (down (sum));
        }
      *--rp = lowpart (sum);
      while (rp > r0)
        *--rp = 0;
    }
  else
    {
      int rl = xl - bw;
      if (rl < 0)
        return br_copy_zero (r);

      int xr_eq = x == r;
      if (xr_eq)
        r = br_realloc (r, rl);
      else
        r = br_calloc (r, rl);
      u_short *rp = r->br_data;
      u_short *re = rp + rl;
      const u_short *x0 = xr_eq ? rp : x->br_data;
      const u_short *xe = x0 + xl;
      const u_short *xp = x0 + bw;
      u_long sum = u_long (*xp++) >> sw;
      int rw = BR_SHIFT - sw;
      while (xp < xe)
        {
          sum += u_long (*xp++) << rw;
          *rp++ = lowpart (sum);
          sum = down (sum);
        }
      *rp++ = lowpart (sum);
      while (rp < re)
        *rp++ = 0;
    }
  r->br_sign = rsign;
  return r->normalize ();
}

void
ash (bignum_rep *&r, const bignum_rep *x, long y)
{
  int f = x->minusp () && y < 0;
  r = lshift (r, x, y);
  if (f)
    {
      safe_bignum_rep rr (r);
      r = add (r, r, &bignum_rep_minus_one, 0);
      rr.discard ();
    }
}

bignum_rep *
expt (bignum_rep *r, const bignum_rep *x, long y)
{
  int rsign = ((x->br_sign == BR_POSITIVE || !(y & 1))
               ? BR_POSITIVE : BR_NEGATIVE);

  int xl = x->br_len;
  if (!y || (xl == 1 && x->br_data[0] == 1))
    return br_copy_one (r, rsign);
  if (!xl || y < 0)
    return br_copy_zero (r);
  if (y == 1)
    return br_copy (r, x);

  int size = (x->log2 () + 1) * y / BR_SHIFT + 2;
  safe_bignum_rep b (br_alloc (0, x, size, BR_POSITIVE));
  b->br_len = xl;
  r = br_calloc (r, size);
  r = br_copy_one (r, BR_POSITIVE);
  while (1)
    {
      if (y & 1)
        r = multiply (r, r, b);
      y >>= 1;
      if (!y)
        break;
      b = multiply (b, b, b);
    }
  r->br_sign = rsign;
  return r->normalize ();
}

bignum_rep *
gcd (const bignum_rep *x, const bignum_rep *y)
{
  int ul = x->br_len;
  if (!ul)
    return br_copy (0, y, BR_POSITIVE);

  int vl = y->br_len;
  if (!vl)
    return br_copy (0, x, BR_POSITIVE);

  if (ul > vl)
    {
      const bignum_rep *t = x;
      x = y;
      y = t;
      ul = vl;
      vl = y->br_len;
    }

  long nshift = 0;
  for (int i = 0; i < ul; i++)
    {
      u_long xy = x->br_data[i] | y->br_data[i];
      for (int j = 1; j < BR_RADIX; j <<= 1, nshift--)
        if (xy & j)
          goto found;
    }
found:
  safe_bignum_rep u, v;
  if (nshift)
    {
      u = lshift (0, x, nshift);
      v = lshift (0, y, nshift);
      u->br_sign = v->br_sign = BR_POSITIVE;
    }
  else
    {
      u = br_copy (0, x, BR_POSITIVE);
      v = br_copy (0, y, BR_POSITIVE);
    }

  safe_bignum_rep t ((u->br_data[0] & 1)
                     ? br_alloc (0, v, v->br_len, BR_NEGATIVE)
                     : br_alloc (0, u, u->br_len, BR_POSITIVE));

  while (!t->zerop ())
    {
      long sft = 0;
      int tl = t->br_len;
      for (i = 0; i < tl; i++)
        {
          u_long tt = t->br_data[i];
          for (int j = 1; j < BR_RADIX; j <<= 1, sft--)
            if (tt & j)
              goto found2;
        }
    found2:
      if (sft)
        t = lshift (t, t, sft);

      if (t->plusp ())
        {
          u = br_copy (u, t);
          t = add (t, t, v, 1);
        }
      else
        {
          v = br_copy (v, t, BR_POSITIVE);
          t = add (t, t, u, 0);
        }
    }
  if (nshift)
    u = lshift (u, u, -nshift);
  return u.release ();
}

bignum_rep *
lcm (const bignum_rep *x, const bignum_rep *y)
{
  if (x->zerop () || y->zerop ())
    return br_copy_zero (0);
  safe_bignum_rep r (gcd (x, y));
  r = divide (r, x, r);
  r = multiply (r, r, y);
  r = abs (r);
  return r.release ();
}

bignum_rep *
isqrt (const bignum_rep *x)
{
//  if (x->minusp ())
//    ;
  if (x->zerop ())
    return br_copy_zero (0);
  safe_bignum_rep r (lshift (0, x, -((x->log2 () - 1) / 2)));
  safe_bignum_rep q (divide (0, x, r));
  while (br_compare (q, r) < 0)
    {
      r = add (r, r, q, 0);
      r = lshift (r, r, -1);
      q = divide (q, x, r);
    }
  return r.release ();
}

void
lognot (bignum_rep *&r, const bignum_rep *x)
{
  r = negate (r, x);
  safe_bignum_rep rr (r);
  r = add (r, r, &bignum_rep_minus_one, 0);
  rr.discard ();
}

void
logope (bignum_rep *&r, logope_code ope,
        const bignum_rep *x, const bignum_rep *y)
{
  int xl = x->br_len;
  int yl = y->br_len;
  int rl = max (xl, yl) + 1;

  r = br_calloc (r, rl);
  u_long c, i1 = x->minusp (), i2 = y->minusp ();
  for (int i = 0; i < rl; i++)
    {
      c = i < xl ? x->br_data[i] : 0;
      i1 += x->minusp () ? lowpart (~c) : c;
      c = i < yl ? y->br_data[i] : 0;
      i2 += y->minusp () ? lowpart (~c) : c;
      switch (ope)
        {
        case LOC_AND:
          c = i1 & i2;
          break;

        case LOC_IOR:
          c = i1 | i2;
          break;

        case LOC_XOR:
          c = i1 ^ i2;
          break;

        case LOC_EQV:
          c = ~(i1 ^ i2);
          break;

        case LOC_NAND:
          c = ~(i1 & i2);
          break;

        case LOC_NOR:
          c = ~(i1 | i2);
          break;

        case LOC_ANDC1:
          c = ~i1 & i2;
          break;

        case LOC_ANDC2:
          c = i1 & ~i2;
          break;

        case LOC_ORC1:
          c = ~i1 | i2;
          break;

        case LOC_ORC2:
          c = i1 | ~i2;
          break;
        }
      r->br_data[i] = lowpart (c);
      i1 = down (i1);
      i2 = down (i2);
    }

  if (!(r->br_data[rl - 1] & BR_MIN))
    {
      r->normalize ();
      return;
    }

  for (i = 0; i < rl; i++)
    r->br_data[i] = ~r->br_data[i];

  safe_bignum_rep rr (r);
  r = add (r, r, &bignum_rep_one, 0);
  rr.discard ();
  r->br_sign = BR_NEGATIVE;
}

int
logtest (const bignum_rep *x, const bignum_rep *y)
{
  if (x->minusp () && y->minusp ())
    return 1;
  int xl = x->br_len;
  int yl = y->br_len;
  int rl = max (xl, yl);
  u_long c, i1 = x->minusp (), i2 = y->minusp ();
  for (int i = 0; i < rl; i++)
    {
      c = i < xl ? x->br_data[i] : 0;
      i1 += x->minusp () ? lowpart (~c) : c;
      c = i < yl ? y->br_data[i] : 0;
      i2 += y->minusp () ? lowpart (~c) : c;
      if (i1 & i2)
        return 1;
      i1 = down (i1);
      i2 = down (i2);
    }
  return 0;
}

int
logbitp (const bignum_rep *x, long index)
{
  int q = index / BR_SHIFT;
  int r = index % BR_SHIFT;
  if (x->plusp ())
    {
      if (x->br_len <= q)
        return 0;
      return x->br_data[q] & (1 << r);
    }
  else
    {
      if (x->br_len < q)
        return 1;
      u_long s = 1;
      for (int i = 0; i < q; i++)
        {
          s += lowpart (~x->br_data[i]);
          s = down (s);
        }
      s += lowpart (~(q < x->br_len ? x->br_data[q] : 0));
      return s & (1 << r);
    }
}

long
logcount (const bignum_rep *x)
{
  int xl = x->br_len;
  int n = 0;
  if (x->plusp ())
    {
      for (int i = 0; i < xl; i++)
        for (u_long c = x->br_data[i]; c; n++, c &= c - 1)
          ;
    }
  else
    {
      u_long s = 1;
      for (int i = 0; i < xl; i++)
        {
          s += lowpart (~x->br_data[i]);
          for (u_long c = lowpart (~s); c; n++, c &= c - 1)
            ;
          s = down (s);
        }
    }
  return n;
}

int
bignum_rep::fmtwidth (u_long base) const
{
  return (br_len + 1) * BR_SHIFT / (::log2 (base) - 1) + 16;
}

char *
bignum_rep::to_ascii (char *b, int base, int dot, int sign,
                      const char *digit_chars) const
{
  *--b = 0;
  if (dot)
    *--b = '.';
  if (!br_len)
    *--b = '0';
  else
    {
      safe_bignum_rep r (br_alloc (0, this, br_len));
      int pow = 1;
      u_short max = u_short (BR_MAX / base);
      u_short div;
      for (div = base; div < max; div *= base, pow++)
        ;
      while (1)
        {
          int rem = divide (r->br_data, r->br_len, div, r->br_data);
          r->normalize ();
          if (!r->br_len)
            {
              while (rem)
                {
                  *--b = digit_chars[rem % base];
                  rem /= base;
                }
              break;
            }

          for (int i = 0; i < pow; i++)
            {
              *--b = digit_chars[rem % base];
              rem /= base;
            }
        }
    }

  if (minusp ())
    *--b = '-';
  else if (sign)
    *--b = '+';
  return b;
}

Char *
ato_bignum_rep (bignum_rep *&br, const Char *p, int pl, int radix)
{
  static bignum_rep_long brl (0);
  const Char *pe = p + pl;
  int width = pl * log2 (radix) / BR_SHIFT + 1;
  bignum_rep *rep;
  if (width <= SHORT_PER_LONG)
    {
      rep = &brl;
      rep->br_len = 0;
    }
  else
    rep = br_calloc (0, width);
  safe_bignum_rep r (rep);
  int rsign;
  if (p < pe && (*p == '-' || *p == '+'))
    {
      rsign = *p == '-' ? BR_NEGATIVE : BR_POSITIVE;
      p++;
    }
  else
    rsign = BR_POSITIVE;

  bignum_rep_short rdx (radix);
  for (; p < pe; p++)
    {
      int n = digit_char (*p);
      if (n >= radix)
        break;
      r = multiply (r, r, &rdx);
      bignum_rep_short nn (n);
      r = add (r, r, &nn, 0);
    }
  r->br_sign = rsign;
  br = r.release ();
  if (br == &brl && !br->is_long ())
    br = br_copy (0, br);
  return (Char *)p;
}

bignum_rep *
ato_bignum_rep (const Char *p, int pl, int radix)
{
  bignum_rep *r;
  ato_bignum_rep (r, p, pl, radix);
  return r;
}

bignum_rep *
double_to_bignum_rep_ratio (double x, bignum_rep **denp)
{
  safe_bignum_rep num (&bignum_rep_zero);
  bignum_rep *den = &bignum_rep_one;
  if (x)
    {
      int neg = x < 0;
      if (neg)
        x = -x;
      const long shift = 16;
      const double radix = 1L << shift;
      int exp;
      double mantissa = frexp (x, &exp);
      for (int prec = 20; mantissa && prec; prec--)
        {
          double integer;
          mantissa = modf (mantissa * radix, &integer);
          num = lshift (num, num, shift);
          num = add (num, num, long (integer), 0);
          exp -= shift;
        }
      if (exp > 0 || (!denp && exp < 0))
        num = lshift (num, num, long (exp));
      if (neg)
        num = negate (num, num);
      if (exp < 0 && denp)
        den = lshift (den, den, long (-exp));
    }
  if (denp)
    *denp = den;
  return num.release ();
}

bignum_rep *
double_to_bignum_rep (double x)
{
  modf (x, &x);
  return double_to_bignum_rep_ratio (x, 0);
}
