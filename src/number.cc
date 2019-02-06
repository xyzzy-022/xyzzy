#include "stdafx.h"
#include "ed.h"

#define YET return 0;

typedef double (__cdecl *FIX_FLONUM)(double);
typedef void (*FIX_BIGNUM)(bignum_rep *&, bignum_rep *&,
                           const bignum_rep *, const bignum_rep *);
typedef long (*FIX_FIXNUM)(long *, long, long);

lisp fix_number_2 (lisp, lisp, FIX_FLONUM, FIX_BIGNUM, FIX_FIXNUM);
static lisp number_log (lisp);

/*GENERIC_FUNCTION:INTEGER*/
long
fixnum_value (lisp x)
{
  if (short_int_p (x))
    return xshort_int_value (x);
  if (long_int_p (x))
    return xlong_int_value (x);
  return coerce_to_long_error (x);
}

/*GENERIC_FUNCTION:INTEGER*/
int
safe_fixnum_value (lisp x, long *n)
{
  if (short_int_p (x))
    {
      *n = xshort_int_value (x);
      return 1;
    }
  if (long_int_p (x))
    {
      *n = xlong_int_value (x);
      return 1;
    }
  *n = 0;
  return 0;
}

long
coerce_to_long_error (lisp object)
{
  if (bignump (object))
    FErange_error (object);
  else
    FEtype_error (object, Qinteger);
  return 0;
}

/*GENERIC_FUNCTION:INTEGER*/
u_long
unsigned_long_value (lisp x)
{
  long val;
  if (short_int_p (x))
    val = xshort_int_value (x);
  else if (long_int_p (x))
    val = xlong_int_value (x);
  else if (bignump (x))
    {
      if (!xbignum_rep (x)->is_ulong ())
        FErange_error (x);
      return xbignum_rep (x)->to_ulong ();
    }
  else
    FEtype_error (x, Qinteger);
  if (val < 0)
    FErange_error (x);
  return val;
}

static void
signal_divide_by_zero ()
{
  FEdivision_by_zero ();
}

lisp make_integer (char x) { return make_fixnum (x); }
lisp make_integer (u_char x) { return make_fixnum (x); }
lisp make_integer (short x) { return make_fixnum (x); }
lisp make_integer (u_short x) { return make_fixnum (x); }
lisp make_integer (int x) { return make_fixnum (x); }
lisp make_integer (u_int x) { return make_fixnum (x); }
lisp make_integer (long x) { return make_fixnum (x); }
lisp make_integer (u_long x) { return make_integer (int64_t (x)); }

lisp
make_integer (bignum_rep *rep)
{
  if (rep->is_long ())
    {
      long x = rep->to_long ();
      br_delete (rep);
      return make_fixnum (x);
    }
  else
    {
      safe_bignum_rep r (rep);
      lbignum *lb = make_bignum ();
      r.discard ();
      lb->rep = rep;
      return lb;
    }
}

lisp
make_integer (int64_t x)
{
  if (LONG_MIN <= x && x <= LONG_MAX)
    return make_fixnum (static_cast <long> (x));
  lbignum *lb = make_bignum ();
  lb->rep = br_copy (0, x);
  return lb;
}

lisp
make_integer (uint64_t x)
{
  if (x <= LONG_MAX)
    return make_fixnum (static_cast <long> (x));
  lbignum *lb = make_bignum ();
  lb->rep = br_copy (0, x);
  return lb;
}

static inline lisp
flonum_to_integer (double x)
{
  return make_integer (double_to_bignum_rep (x));
}

lisp
flonum_to_rational (double x)
{
  bignum_rep *num_rep, *den_rep;
  num_rep = double_to_bignum_rep_ratio (x, &den_rep);
  safe_bignum_rep tem (den_rep);
  lisp num = make_integer (num_rep);
  tem.release ();
  return make_ratio (num, make_integer (den_rep));
}

static void
check_float_range (double x)
{
  if (_finite (x))
    return;
  if (_isnan (x))
    FEfloating_point_underflow ();
  else
    FEfloating_point_overflow ();
}

lsingle_float *
make_single_float (float x)
{
  check_float_range (x);
  lsingle_float *p = ldata <lsingle_float, Tsingle_float>::lalloc ();
  p->value = x;
  return p;
}

ldouble_float *
make_double_float (double x)
{
  check_float_range (x);
  ldouble_float *p = ldata <ldouble_float, Tdouble_float>::lalloc ();
  p->value = x;
  return p;
}

static double __cdecl
truncate (double x)
{
  return x - fmod (x, 1.0);
}

static double __cdecl
round (double x)
{
  double q = truncate (x);
  if (q == x)
    return q;
  double r = fabs (q - x);
  if (r < .5 || (r == .5 && !fmod (q, 2.0)))
    return q;
  return x < 0 ? q - 1 : q + 1;
}

#ifdef _M_IX86
# pragma warning (disable:4035)
#endif

// y != -1
static long
truncate (long *r, long x, long y)
{
#ifdef _M_IX86
  __asm
    {
      mov ecx, r;
      mov eax, x;
      cdq;
      idiv y;
      mov DWORD PTR [ecx], edx;
    }
#else
  *r = x % y;
  return x / y;
#endif
}

#ifdef _M_IX86
# pragma warning (default:4035)
#endif

static long
floor (long *r, long x, long y)
{
  long q = truncate (r, x, y);
  if (*r && (!q ? x < 0 != y < 0 : q < 0))
    {
      q--;
      *r += y;
    }
  return q;
}

static long
ceiling (long *r, long x, long y)
{
  long q = truncate (r, x, y);
  if (*r && (!q ? x < 0 == y < 0 : q > 0))
    {
      q++;
      *r -= y;
    }
  return q;
}

static long
round (long *r, long x, long y)
{
  long q = truncate (r, x, y);
  if (*r)
    {
      u_long mr = *r < 0 ? -*r : *r;
      u_long my = y < 0 ? -y : y;
      my -= mr;
      if (my < mr || (my == mr && q % 2))
        {
          if (!q ? x < 0 != y < 0 : q < 0)
            {
              q--;
              *r += y;
            }
          else
            {
              q++;
              *r -= y;
            }
        }
    }
  return q;
}

static lisp
flonum_rationalize (double x, int prec)
{
  int dec, sign;
  const char *p = _ecvt (x, prec + 1, &dec, &sign), *pe;
  for (pe = p + strlen (p); pe > p && pe[-1] == '0'; pe--)
    ;
  if (p == pe)
    return make_fixnum (0);

  Char b[DBL_DIG + 16];
  int l = 0;
  if (sign)
    b[l++] = '-';
  while (p < pe)
    b[l++] = *p++;

  lisp num = make_integer (ato_bignum_rep (b, l, 10));
  if (sign)
    l--;
  l -= dec;
  if (!l)
    return num;
  if (l > 0)
    return make_ratio (num, Fexpt (make_fixnum (10), make_fixnum (l)));
  return number_multiply (num, Fexpt (make_fixnum (10), make_fixnum (-l)));
}

static inline lisp
complex_add (lisp xr, lisp xi, lisp yr, lisp yi)
{
  return make_complex (number_add (xr, yr), number_add (xi, yi));
}

static inline lisp
complex_subtract (lisp xr, lisp xi, lisp yr, lisp yi)
{
  return make_complex (number_subtract (xr, yr),
                       number_subtract (xi, yi));
}

static lisp
complex_multiply (lisp xr, lisp xi, lisp yr, lisp yi)
{
  return make_complex (number_subtract (number_multiply (xr, yr),
                                        number_multiply (xi, yi)),
                       number_add (number_multiply (xr, yi),
                                   number_multiply (xi, yr)));
}

static lisp
complex_divide (lisp xr, lisp xi, lisp yr, lisp yi)
{
  lisp z = number_add (number_multiply (yr, yr),
                       number_multiply (yi, yi));
  return make_complex (number_divide (number_add (number_multiply (xr, yr),
                                                  number_multiply (xi, yi)),
                                      z),
                       number_divide (number_subtract (number_multiply (xi, yr),
                                                       number_multiply (xr, yi)),
                                      z));
}

/* return only 0(equal) or 1(not equal) */
static inline int
complex_compare (lisp xr, lisp xi, lisp yr, lisp yi)
{
  return number_compare (xr, yr) || number_compare (xi, yi);
}

#include "num-arith.h"

static void
cast_number (lisp &x, lisp &y)
{
  lisp_object_type xt = number_typeof (x);
  if (xt == Tcomplex)
    xt = number_typeof (xcomplex_real (x));
  if (xt == Tdouble_float)
    {
      y = cast_to_double_float (y);
      return;
    }

  lisp_object_type yt = number_typeof (y);
  if (yt == Tcomplex)
    yt = number_typeof (xcomplex_real (y));
  if (yt == Tdouble_float)
    {
      x = cast_to_double_float (x);
      return;
    }
}

lisp
make_ratio (lisp num, lisp den)
{
  if (den == make_short_int (0))
    FEsimple_error (Ezero_denominator);
  if (num == make_short_int (0))
    return num;
  if (Fminusp (den) != Qnil)
    {
      num = number_negate (num);
      den = number_negate (den);
    }
  if (den == make_short_int (1))
    return num;
  lisp g = number_gcd (num, den);
  num = integer_divide (num, g);
  den = integer_divide (den, g);
  if (den == make_short_int (1))
    return num;
  return make_fraction (num, den);
}

static inline lisp
make_float (double value, int doublep)
{
  if (doublep)
    return make_double_float (value);
  else
    return make_single_float (float (value));
}

/*GENERIC_FUNCTION:FLONUM*/
static double
float_value (lisp x)
{
  if (single_float_p (x))
    return xsingle_float_value (x);
  check_double_float (x);
  return xdouble_float_value (x);
}

// 12.3

lisp
Fnumber_eql (lisp first, lisp rest)
{
  if (!numberp (first))
    FEtype_error (first, Qnumber);
  for (; consp (rest); rest = xcdr (rest))
    {
      if (number_compare (first, xcar (rest)))
        return Qnil;
      QUIT;
    }
  return Qt;
}

lisp
Fnumber_not_eql (lisp args)
{
  switch (xlist_length (args))
    {
    case 0:
      return FEtoo_few_arguments ();

    case 1:
      if (!numberp (xcar (args)))
        FEtype_error (xcar (args), Qnumber);
      return Qt;

    case 2:
      return boole (number_compare (xcar (args), xcar (xcdr (args))));

    default:
      for (lisp p = args; consp (p); p = xcdr (p))
        for (lisp q = xcdr (p); consp (q); q = xcdr (q))
          if (!number_compare (xcar (p), xcar (q)))
            return Qnil;
      return Qt;
    }
}

lisp
Fnumber_less (lisp first, lisp rest)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (rest); rest = xcdr (rest))
    {
      if (complexp (xcar (rest)))
        FEtype_error (xcar (rest), Qreal);
      if (number_compare (first, xcar (rest)) >= 0)
        return Qnil;
      first = xcar (rest);
      QUIT;
    }
  return Qt;
}

lisp
Fnumber_greater (lisp first, lisp rest)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (rest); rest = xcdr (rest))
    {
      if (complexp (xcar (rest)))
        FEtype_error (xcar (rest), Qreal);
      if (number_compare (first, xcar (rest)) <= 0)
        return Qnil;
      first = xcar (rest);
      QUIT;
    }
  return Qt;
}

lisp
Fnumber_not_greater (lisp first, lisp rest)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (rest); rest = xcdr (rest))
    {
      if (complexp (xcar (rest)))
        FEtype_error (xcar (rest), Qreal);
      if (number_compare (first, xcar (rest)) > 0)
        return Qnil;
      first = xcar (rest);
      QUIT;
    }
  return Qt;
}

lisp
Fnumber_not_less (lisp first, lisp rest)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (rest); rest = xcdr (rest))
    {
      if (complexp (xcar (rest)))
        FEtype_error (xcar (rest), Qreal);
      if (number_compare (first, xcar (rest)) < 0)
        return Qnil;
      first = xcar (rest);
      QUIT;
    }
  return Qt;
}

lisp
Fmax (lisp first, lisp args)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (args); args = xcdr (args))
    {
      if (complexp (xcar (args)))
        FEtype_error (xcar (args), Qreal);
      if (number_compare (first, xcar (args)) < 0)
        first = xcar (args);
      QUIT;
    }
  return first;
}

lisp
Fmin (lisp first, lisp args)
{
  if (!realp (first))
    FEtype_error (first, Qreal);
  for (; consp (args); args = xcdr (args))
    {
      if (complexp (xcar (args)))
        FEtype_error (xcar (args), Qreal);
      if (number_compare (first, xcar (args)) > 0)
        first = xcar (args);
      QUIT;
    }
  return first;
}

// 12.4

lisp
Fadd (lisp args)
{
  lisp result = make_fixnum (0);
  for (; consp (args); args = xcdr (args))
    {
      result = number_add (result, xcar (args));
      QUIT;
    }
  return result;
}

lisp
Fsubtract (lisp first, lisp args)
{
  if (!consp (args))
    return number_negate (first);
  for (; consp (args); args = xcdr (args))
    {
      first = number_subtract (first, xcar (args));
      QUIT;
    }
  return first;
}

lisp
Fmultiply (lisp args)
{
  lisp result = make_fixnum (1);
  for (; consp (args); args = xcdr (args))
    {
      result = number_multiply (result, xcar (args));
      QUIT;
    }
  return result;
}

lisp
Fdivide (lisp first, lisp args)
{
  if (!numberp (first))
    FEtype_error (first, Qnumber);
  if (!consp (args))
    return number_divide (make_fixnum (1), first);
  do
    {
      first = number_divide (first, xcar (args));
      args = xcdr (args);
      QUIT;
    }
  while (consp (args));
  return first;
}

lisp
Fgcd (lisp args)
{
  lisp result = make_fixnum (0);
  for (; consp (args); args = xcdr (args))
    {
      result = number_gcd (result, xcar (args));
      QUIT;
    }
  return result;
}

lisp
Flcm (lisp args)
{
  lisp result = make_fixnum (1);
  for (; consp (args); args = xcdr (args))
    {
      result = number_lcm (result, xcar (args));
      QUIT;
    }
  return result;
}

// 12.5.1

lisp
Fexp (lisp number)
{
  if (complexp (number))
    {
      /* exp (x) = (complex (* (exp xr) (cos xi))
                            (* (exp xr) (sin xi))) */
      lisp re = Fexp (xcomplex_real (number));
      return make_complex (number_multiply (re, Fcos (xcomplex_imag (number))),
                           number_multiply (re, Fsin (xcomplex_imag (number))));
    }

  return make_float (exp (coerce_to_double_float (number)),
                     double_float_p (number));
}

static lisp
number_expt_integer (lisp x, lisp yy)
{
  lisp z = make_fixnum (1);
  if (bignump (yy))
    {
      while (1)
        {
          if (Foddp (yy) != Qnil)
            z = number_multiply (z, x);
          yy = integer_divide (yy, make_fixnum (2));
          if (Fzerop (yy) != Qnil)
            break;
          x = number_multiply (x, x);
        }
    }
  else
    {
      long y = fixnum_value (yy);
      while (1)
        {
          if (y & 1)
            z = number_multiply (z, x);
          y /= 2;
          if (!y)
            break;
          x = number_multiply (x, x);
        }
    }
  return z;
}

static lisp
number_expt_number (lisp x, lisp y)
{
  cast_number (x, y);
  if (!complexp (x) && !complexp (y))
    {
      double d = coerce_to_double_float (x);
      double z = coerce_to_double_float (y);
      double q;
      if (d >= 0 || !modf (z, &q))
        return make_float (pow (d, z), double_float_p (x));
    }

  /* expt (x y) = (exp (* (log x) y)) */
  return Fexp (number_multiply (number_log (x), y));
}

lisp
Fexpt (lisp x, lisp y)
{
  if (y == make_fixnum (0))
    {
      if (rationalp (x))
        return make_fixnum (1);
      if (Fzerop (x) != Qnil)
        return number_add (x, make_fixnum (1));
      return number_divide (x, x);
    }

  if (Fzerop (y) != Qnil)
    return number_add (y, number_divide (x, x));

  if (integerp (y))
    {
      if (Fminusp (y) != Qnil)
        return number_divide (make_fixnum (1), Fexpt (x, number_negate (y)));

      if (integerp (x))
        return integer_expt_integer (x, y);

      if (fractionp (x))
        return make_ratio (integer_expt_integer (xfract_num (x), y),
                           integer_expt_integer (xfract_den (x), y));

      return number_expt_integer (x, y);
    }

  if (complexp (x) && Fzerop (xcomplex_imag (x)) != Qnil)
    {
      x = Fexpt (xcomplex_real (x), y);
      if (complexp (x))
        return x;
      return make_complex (x, make_fixnum (0));
    }

  return number_expt_number (x, y);
}

static lisp
number_log (lisp x)
{
  if (Fzerop (x) != Qnil)
    FElog_domain_error ();
  lisp r, i;
  if (complexp (x))
    {
      r = xcomplex_real (x);
      i = xcomplex_imag (x);
    }
  else if (Fminusp (x) != Qnil)
    {
      r = x;
      i = make_fixnum (0);
    }
  else
    return make_float (log (coerce_to_double_float (x)),
                       double_float_p (x));

  /* log (x) = (complex (log (abs x)) (phase x))
             = (complex (log (sqrt (+ (* xr xr) (* xi xi)))) (atan xi xr))
             = (complex (log (expt (+ (* xr xr) (* xi xi)) 1/2)) (atan xi xr))
             = (complex (/ (log (+ (* xr xr) (* xi xi))) 2) (atan xi xr))
   */
  return make_complex (number_divide (number_log (number_add (number_multiply (r, r),
                                                              number_multiply (i, i))),
                                      make_fixnum (2)),
                       Fatan (i, r));
}

lisp
Flog (lisp number, lisp base)
{
  if (!base)
    return number_log (number);
  else
    {
      cast_number (number, base);
      return number_divide (number_log (number), number_log (base));
    }
}

// 12.5.2

lisp
Fabs (lisp number)
{
  if (!complexp (number))
    {
      if (Fminusp (number) == Qnil)
        return number;
      else
        return number_negate (number);
    }
  else
    return Fsqrt (number_add (number_multiply (xcomplex_real (number),
                                               xcomplex_real (number)),
                              number_multiply (xcomplex_imag (number),
                                               xcomplex_imag (number))));
}

static lisp
mul_imag (lisp x)
{
  if (complexp (x))
    return make_complex (number_negate (xcomplex_imag (x)),
                         xcomplex_real (x));
  else
    return make_complex (make_fixnum (0), x);
}

static lisp
mul_negative_imag (lisp x)
{
  if (complexp (x))
    return make_complex (xcomplex_imag (x),
                         number_negate (xcomplex_real (x)));
  else
    return make_complex (make_fixnum (0), number_negate (x));
}

lisp
Fsin (lisp radians)
{
  if (!complexp (radians))
    return make_float (sin (coerce_to_double_float (radians)),
                       double_float_p (radians));
  else
    return number_divide (number_subtract (Fexp (mul_imag (radians)),
                                           Fexp (mul_negative_imag (radians))),
                          xsymbol_value (Qimag_two));
}

lisp
Fcos (lisp radians)
{
  if (!complexp (radians))
    return make_float (cos (coerce_to_double_float (radians)),
                       double_float_p (radians));
  else
    return number_divide (number_add (Fexp (mul_imag (radians)),
                                      Fexp (mul_negative_imag (radians))),
                          make_fixnum (2));
}

lisp
Ftan (lisp radians)
{
  if (!complexp (radians))
    return make_float (tan (coerce_to_double_float (radians)),
                       double_float_p (radians));
  else
    {
      lisp e1 = Fexp (mul_imag (radians));
      lisp e2 = Fexp (mul_negative_imag (radians));
      return number_divide (number_divide (number_subtract (e1, e2),
                                           xsymbol_value (Qimag_two)),
                            number_divide (number_add (e1, e2),
                                           make_fixnum (2)));
    }
}

lisp
Fasin (lisp number)
{
  if (!complexp (number))
    {
      double d = coerce_to_double_float (number);
      if (fabs (d) <= 1.0)
        return make_float (asin (d), double_float_p (number));
    }
  return mul_negative_imag
    (number_log (number_add (mul_imag (number),
                             Fsqrt (number_subtract (make_fixnum (1),
                                                     number_multiply (number,
                                                                      number))))));
}

lisp
Facos (lisp number)
{
  if (!complexp (number))
    {
      double d = coerce_to_double_float (number);
      if (fabs (d) <= 1.0)
        return make_float (acos (d), double_float_p (number));
    }
  return mul_negative_imag
    (number_log (number_add (number,
                             mul_imag (Fsqrt (number_subtract
                                              (make_fixnum (1),
                                               number_multiply (number,
                                                                number)))))));
}

lisp
Fatan (lisp y, lisp x)
{
  if (!x)
    {
      if (!complexp (y))
        return make_float (atan (coerce_to_double_float (y)),
                           double_float_p (y));
      else
        {
          lisp yi = mul_imag (y);
          return number_divide (number_subtract
                                (number_log (number_add (make_fixnum (1), yi)),
                                 number_log (number_subtract (make_fixnum (1), yi))),
                                xsymbol_value (Qimag_two));
        }
    }
  else
    {
      cast_number (x, y);
      return make_float (atan2 (coerce_to_double_float (y),
                                coerce_to_double_float (x)),
                         double_float_p (x));
    }
}

// 12.6

lisp
Ffloat (lisp number, lisp other)
{
  if (!other)
    {
      if (floatp (number))
        return number;
      else
        return make_single_float (coerce_to_single_float (number));
    }
  else
    {
      if (single_float_p (other))
        {
          if (single_float_p (number))
            return number;
          else
            return make_single_float (coerce_to_single_float (number));
        }
      else
        {
          check_double_float (other);
          if (double_float_p (number))
            return number;
          else
            return make_double_float (coerce_to_double_float (number));
        }
    }
}

static inline lisp
fix_number (lisp number, lisp divisor, FIX_FLONUM dfn, FIX_BIGNUM bfn, FIX_FIXNUM ffn)
{
  multiple_value::count () = 2;
  return fix_number_2 (number, divisor ? divisor : make_fixnum (1),
                       dfn, bfn, ffn);
}

lisp
Ffloor (lisp number, lisp divisor)
{
  return fix_number (number, divisor, floor, floor, floor);
}

lisp
Fceiling (lisp number, lisp divisor)
{
  return fix_number (number, divisor, ceil, ceiling, ceiling);
}

lisp
Ftruncate (lisp number, lisp divisor)
{
  return fix_number (number, divisor, truncate, truncate, truncate);
}

lisp
Fround (lisp number, lisp divisor)
{
  return fix_number (number, divisor, round, round, round);
}

lisp
Fmod (lisp number, lisp divisor)
{
  fix_number_2 (number, divisor, floor, floor, floor);
  return multiple_value::value (1);
}

lisp
Frem (lisp number, lisp divisor)
{
  fix_number_2 (number, divisor, truncate, truncate, truncate);
  return multiple_value::value (1);
}

static inline lisp
fix_flonum (lisp number, lisp divisor, FIX_FLONUM dfn)
{
  multiple_value::count () = 2;
  return fix_flonum_2 (number, divisor ? divisor : make_fixnum (1), dfn);
}

lisp
Fffloor (lisp number, lisp divisor)
{
  return fix_flonum (number, divisor, floor);
}

lisp
Ffceiling (lisp number, lisp divisor)
{
  return fix_flonum (number, divisor, ceil);
}

lisp
Fftruncate (lisp number, lisp divisor)
{
  return fix_flonum (number, divisor, truncate);
}

lisp
Ffround (lisp number, lisp divisor)
{
  return fix_flonum (number, divisor, round);
}

lisp
Fdecode_float (lisp flonum)
{
  double d = float_value (flonum);
  double sign;
  if (d < 0)
    {
      sign = -1.0;
      d = -d;
    }
  else
    sign = 1.0;
  int e;
  double m = frexp (d, &e);
  multiple_value::value (1) = make_fixnum (e);
  multiple_value::count () = 3;
  if (single_float_p (flonum))
    {
      multiple_value::value (2) = make_single_float (float (sign));
      return make_single_float (float (m));
    }
  else
    {
      multiple_value::value (2) = make_double_float (sign);
      return make_double_float (m);
    }
}

lisp
Fscale_float (lisp flonum, lisp integer)
{
  return make_float (ldexp (float_value (flonum), fixnum_value (integer)),
                     double_float_p (flonum));
}

lisp
Ffloat_radix (lisp flonum)
{
  if (single_float_p (flonum))
    return make_fixnum (FLT_RADIX);
  else
    {
      check_double_float (flonum);
      return make_fixnum (_DBL_RADIX);
    }
}

lisp
Ffloat_sign (lisp float1, lisp float2)
{
  double d = float_value (float1);
  double f = float2 ? fabs (float_value (float2)) : 1.0;
  return make_float (d >= 0.0 ? f : -f,
                     (double_float_p (float1)
                      || (float2 && double_float_p (float2))));
}

lisp
Ffloat_digits (lisp flonum)
{
  if (single_float_p (flonum))
    return make_fixnum (FLT_MANT_DIG);
  else
    {
      check_double_float (flonum);
      return make_fixnum (DBL_MANT_DIG);
    }
}

lisp
Ffloat_precision (lisp flonum)
{
  if (single_float_p (flonum))
    return make_fixnum (xsingle_float_value (flonum) ? FLT_MANT_DIG : 0);
  else
    {
      check_double_float (flonum);
      return make_fixnum (xdouble_float_value (flonum) ? DBL_MANT_DIG : 0);
    }
}

lisp
Fcomplex (lisp real, lisp imag)
{
  return make_complex (real, imag ? imag : make_fixnum (0));
}

// 12.7

lisp
Fboole (lisp op, lisp x, lisp y)
{
  int ope = fixnum_value (op);
  check_integer (x);
  check_integer (y);
  switch (ope)
    {
    default:
      FErange_error (op);

    case LOC_CLR:
      return make_fixnum (0);

    case LOC_SET:
      return make_fixnum (-1);

    case LOC_1:
      return x;

    case LOC_2:
      return y;

    case LOC_C1:
      return Flognot (x);

    case LOC_C2:
      return Flognot (y);

    case LOC_AND:
    case LOC_IOR:
    case LOC_XOR:
    case LOC_EQV:
    case LOC_NAND:
    case LOC_NOR:
    case LOC_ANDC1:
    case LOC_ANDC2:
    case LOC_ORC1:
    case LOC_ORC2:
      return number_logope (x, y, logope_code (ope));
    }
}

static lisp
fixnum_logope (long x, long y, logope_code ope)
{
  long r;
  switch (ope)
    {
    case LOC_AND:
      r = x & y;
      break;

    case LOC_IOR:
      r = x | y;
      break;

    case LOC_XOR:
      r = x ^ y;
      break;

    case LOC_EQV:
      r = ~(x ^ y);
      break;

    case LOC_NAND:
      r = ~(x & y);
      break;

    case LOC_NOR:
      r = ~(x | y);
      break;

    case LOC_ANDC1:
      r = ~x & y;
      break;

    case LOC_ANDC2:
      r = x & ~y;
      break;

    case LOC_ORC1:
      r = ~x | y;
      break;

    case LOC_ORC2:
      r = x | ~y;
      break;

    default:
      assert (0);
      return Qnil;
    }
  return make_fixnum (r);
}
