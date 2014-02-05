
float
coerce_to_single_float (x) real
{
s: {return float (x);}
l: {return float (x);}
b: {return float (x->to_double ());}
r: {return fract_to_single_float (x);}
F: {return x;}
D: {return float (x);}
}

double
coerce_to_double_float (x) real
{
s: {return x;}
l: {return x;}
b: {return x->to_double ();}
r: {return fract_to_double_float (x);}
F: {return x;}
D: {return x;}
}

long
coerce_to_long (x) real
{
s: {return x;}
l: {return x;}
b: {return x->coerce_to_long ();}
r: {return long (fract_to_double_float (x));}
F: {return long (x);}
D: {return long (x);}
}

int64_t
coerce_to_int64 (x) real
{
s: {return x;}
l: {return x;}
b: {return x->coerce_to_int64 ();}
r: {return int64_t (fract_to_double_float (x));}
F: {return int64_t (x);}
D: {return int64_t (x);}
}

##define PBIGNUM_REP bignum_rep *
PBIGNUM_REP
coerce_to_bignum_rep (x) (bignum_rep_long *rl) integer
{
s: {
     rl->init (x);
     return rl;
   }
l: s
b: {return (bignum_rep *)x;}
}
##undef PBIGNUM_REP

#static lisp cast_to_double_float (lisp);

lisp
cast_to_double_float (x)
{
s: {return make_double_float (x);}
l: {return make_double_float (x);}
b: {return make_double_float (x->to_double ());}
r: {return make_double_float (fract_to_double_float (x));}
F: {return make_double_float (x);}
D: {return lx;}
c: {
     if (number_typeof (x->real) == Tdouble_float)
       return lx;
     return make_complex (cast_to_double_float (x->real),
                          cast_to_double_float (x->imag));
   }
}

#// 12.2

lisp
Fzerop (x)
{
s: {return boole (!x);}
l: s
b: {return boole (x->zerop ());}
r: {return Qnil;}
F: {return boole (!x);}
D: {return boole (!x);}
c: {return boole (Fzerop (x->real) != Qnil && Fzerop (x->imag) != Qnil);}
}

lisp
Fplusp (x) real
{
s: {return boole (x > 0);}
l: s
b: {return boole (!x->zerop () && x->plusp ());}
r: {return Fplusp (x->num);}
F: {return boole (x > 0);}
D: {return boole (x > 0);}
}

lisp
Fminusp (x) real
{
s: {return boole (x < 0);}
l: s
b: {return boole (x->minusp ());}
r: {return Fminusp (x->num);}
F: {return boole (x < 0);}
D: {return boole (x < 0);}
}

lisp
Foddp (x) integer
{
s: {return boole (x & 1);}
l: s
b: {return boole (x->oddp ());}
}

lisp
Fevenp (x) integer
{
s: {return boole (!(x & 1));}
l: s
b: {return boole (x->evenp ());}
}

#// 12.3

int
number_compare (x, y)
{
DD: {
      if (x < y)
        return -1;
      if (x > y)
        return 1;
      return 0;
    }
ss: {
      if (x < y)
        return -1;
      if (x > y)
        return 1;
      return 0;
    }
sl: ss
sb: {
      bignum_rep_long xx (x);
      return br_compare (&xx, y);
    }
sr: {return number_compare (number_multiply (lx, y->den), y->num);}
sF: DD
sD: DD
ls: ss
ll: ss
lb: sb
lr: sr
lF: DD
lD: DD
bs: {return -number_comparesb (y, ly, x, lx);}
bl: bs
bb: {return br_compare (x, y);}
br: {return number_comparesr (0, lx, y, ly);}
bD: {return number_compareDD (x->to_double (), lx, y, ly);}
bF: bD
rs: {return number_compare (x->num, number_multiply (x->den, ly));}
rl: rs
rb: {return number_comparers (x, lx, 0, ly);}
rr: {
      int xplus = Fminusp (x->num) == Qnil;
      int yplus = Fminusp (y->num) == Qnil;
      if (xplus != yplus)
        return xplus - yplus;
      return number_compare (number_multiply (x->num, y->den),
                             number_multiply (y->num, x->den));
    }
rD: {return number_compareDD (fract_to_double_float (x), lx, y, ly);}
rF: rD
Ds: {return number_compareDD (x, lx, y, ly);}
Dl: Ds
Db: {return number_compareDD (x, lx, y->to_double (), ly);}
Dr: {return number_compareDD (x, lx, fract_to_double_float (y), ly);}
DF: DD
Fs: Ds
Fl: Ds
Fb: Db
Fr: Dr
FF: DD
FD: DD
sc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
lc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
bc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
rc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
Fc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
Dc: {return complex_compare (lx, make_fixnum (0), y->real, y->imag);}
cc: {return complex_compare (x->real, x->imag, y->real, y->imag);}
cs: @sc(y, x)
cl: @lc(y, x)
cb: @bc(y, x)
cr: @rc(y, x)
cF: @Fc(y, x)
cD: @Dc(y, x)
}

#// 12.4

lisp
number_negate (x)
{
s: {return make_fixnum (-x);}
l: {return make_integer (-int64_t (x));}
b: {return make_integer (negate (0, x));}
r: {return make_ratio (number_negate (x->num), x->den);}
F: {return make_single_float (-x);}
D: {return make_double_float (-x);}
c: {return make_complex (number_negate (x->real), number_negate (x->imag));}
}

lisp
number_add (x, y)
{
ss: {return make_fixnum (x + y);}
sl: {return make_integer (int64_t (x) + int64_t (y));}
sb: {
      bignum_rep_long xx (x);
      return make_integer (add (0, &xx, y, 0));
    }
sr: {return make_ratio (number_add (number_multiply (lx, y->den), y->num), y->den);}
sF: {return make_single_float (x + y);}
sD: {return make_double_float (x + y);}
ls: sl
ll: sl
lb: sb
lr: sr
lF: sF
lD: sD
bs: @sb(y, x)
bl: bs
bb: {return make_integer (add (0, x, y, 0));}
br: {return number_addsr (0, lx, y, ly);}
bF: {return make_single_float (float (x->to_double ()) + y);}
bD: {return make_double_float (x->to_double () + y);}
rs: @sr(y, x)
rl: @sr(y, x)
rb: @br(y, x)
rr: {return make_ratio (number_add (number_multiply (x->num, y->den),
                                    number_multiply (y->num, x->den)),
                        number_multiply (x->den, y->den));}
rF: {return make_single_float (fract_to_single_float (x) + y);}
rD: {return make_double_float (fract_to_double_float (x) + y);}
Fs: @sF(y, x)
Fl: Fs
Fb: @bF(y, x)
Fr: @rF(y, x)
FF: {return make_single_float (x + y);}
FD: {return make_double_float (x + y);}
Ds: @sD(y, x)
Dl: Ds
Db: @bD(y, x)
Dr: @rD(y, x)
DF: {return make_double_float (x + y);}
DD: {return make_double_float (x + y);}
sc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
lc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
bc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
rc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
Fc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
Dc: {return complex_add (lx, make_fixnum (0), y->real, y->imag);}
cc: {return complex_add (x->real, x->imag, y->real, y->imag);}
cs: @sc(y, x)
cl: @lc(y, x)
cb: @bc(y, x)
cr: @rc(y, x)
cF: @Fc(y, x)
cD: @Dc(y, x)
}

lisp
number_subtract (x, y)
{
ss: {return make_fixnum (x - y);}
sl: {return make_integer (int64_t (x) - int64_t (y));}
sb: {
      bignum_rep_long xx (x);
      return make_integer (add (0, &xx, y, 1));
    }
sr: {return make_ratio (number_subtract (number_multiply (lx, y->den), y->num), y->den);}
sF: {return make_single_float (x - y);}
sD: {return make_double_float (x - y);}
ls: sl
ll: sl
lb: sb
lr: sr
lF: sF
lD: sD
bs: {
      bignum_rep_long yy (y);
      return make_integer (add (0, x, &yy, 1));
    }
bl: bs
bb: {return make_integer (add (0, x, y, 1));}
br: {return number_subtractsr (0, lx, y, ly);}
bF: {return make_single_float (float (x->to_double ()) - y);}
bD: {return make_double_float (x->to_double () - y);}
rs: {return make_ratio (number_subtract (x->num, number_multiply (x->den, ly)), x->den);}
rl: rs
rb: {return number_subtractrs (x, lx, 0, ly);}
rr: {return make_ratio (number_subtract (number_multiply (x->num, y->den),
                                         number_multiply (y->num, x->den)),
                        number_multiply (x->den, y->den));}
rF: {return make_single_float (fract_to_single_float (x) - y);}
rD: {return make_double_float (fract_to_double_float (x) - y);}
Fs: {return make_single_float (x - y);}
Fl: Fs
Fb: {return make_single_float (x - float (y->to_double ()));}
Fr: {return make_single_float (x - fract_to_single_float (y));}
FF: {return make_single_float (x - y);}
FD: {return make_double_float (x - y);}
Ds: {return make_double_float (x - y);}
Dl: Ds
Db: {return make_double_float (x - y->to_double ());}
Dr: {return make_double_float (x - fract_to_double_float (y));}
DF: {return make_double_float (x - y);}
DD: {return make_double_float (x - y);}
sc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
lc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
bc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
rc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
Fc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
Dc: {return complex_subtract (lx, make_fixnum (0), y->real, y->imag);}
cc: {return complex_subtract (x->real, x->imag, y->real, y->imag);}
cs: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
cl: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
cb: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
cr: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
cF: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
cD: {return complex_subtract (x->real, x->imag, ly, make_fixnum (0));}
}

lisp
number_multiply (x, y)
{
ss: {return make_integer (int64_t (x) * int64_t (y));}
sl: ss
sb: {
      if (x == 1)
        return ly;
      bignum_rep_long xx (x);
      return make_integer (multiply (0, &xx, y));
    }
sr: {return x == 1 ? ly : make_ratio (number_multiply (lx, y->num), y->den);}
sF: {return make_single_float (x * y);}
sD: {return make_double_float (x * y);}
ls: ss
ll: ss
lb: sb
lr: sr
lF: sF
lD: sD
bs: @sb (y, x)
bl: bs
bb: {return make_integer (multiply (0, x, y));}
br: {return number_multiplysr (0, lx, y, ly);}
bF: {return make_single_float (float (x->to_double ()) * y);}
bD: {return make_double_float (x->to_double () * y);}
rs: @sr (y, x)
rl: rs
rb: @br (y, x)
rr: {return make_ratio (number_multiply (x->num, y->num), number_multiply (x->den, y->den));}
rF: {return make_single_float (fract_to_single_float (x) * y);}
rD: {return make_double_float (fract_to_double_float (x) * y);}
Fs: @sF (y, x)
Fl: Fs
Fb: @bF (y, x)
Fr: @rF (y, x)
FF: {return make_single_float (x * y);}
FD: {return make_double_float (x * y);}
Ds: @sD (y, x)
Dl: Ds
Db: @bD (y, x)
Dr: @rD (y, x)
DF: @FD (y, x)
DD: {return make_double_float (x * y);}
sc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
lc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
bc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
rc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
Fc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
Dc: {return complex_multiply (lx, make_fixnum (0), y->real, y->imag);}
cc: {return complex_multiply (x->real, x->imag, y->real, y->imag);}
cs: @sc(y, x)
cl: @lc(y, x)
cb: @bc(y, x)
cr: @rc(y, x)
cF: @Fc(y, x)
cD: @Dc(y, x)
}

lisp
number_divide (x, y)
{
ss: {
      if (!y)
        FEdivision_by_zero ();
      return make_ratio (lx, ly);
    }
sl: ss
sb: {
      if (y->zerop ())
        FEdivision_by_zero ();
      return make_ratio (lx, ly);
    }
sr: {return make_ratio (number_multiply (lx, y->den), y->num);}
sF: {
      if (!y)
        FEdivision_by_zero ();
      return make_single_float (x / y);
    }
sD: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (x / y);
    }
ls: ss
ll: ss
lb: sb
lr: sr
lF: sF
lD: sD
bs: {return number_dividess (0, lx, y, ly);}
bl: bs
bb: {return number_dividesb (0, lx, y, ly);}
br: {return number_dividesr (0, lx, y, ly);}
bF: {
      if (!y)
        FEdivision_by_zero ();
      return make_single_float (float (x->to_double ()) / y);
    }
bD: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (x->to_double () / y);
    }
rs: {
      if (!y)
        FEdivision_by_zero ();
      return make_ratio (x->num, number_multiply (ly, x->den));
    }
rl: rs
rb: {
      if (y->zerop ())
        FEdivision_by_zero ();
      return make_ratio (x->num, number_multiply (ly, x->den));
    }
rr: {return make_ratio (number_multiply (x->num, y->den),
                        number_multiply (x->den, y->num));}
rF: {
      if (!y)
        FEdivision_by_zero ();
      return make_single_float (fract_to_single_float (x) / y);
    }
rD: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (fract_to_double_float (x) / y);
    }
Fs: {
      if (!y)
        FEdivision_by_zero ();
      return make_single_float (x / y);
    }
Fl: Fs
Fb: {
      if (y->zerop ())
        FEdivision_by_zero ();
      return make_single_float (x / float (y->to_double ()));
    }
Fr: {return make_single_float (x / fract_to_single_float (y));}
FF: {
      if (!y)
        FEdivision_by_zero ();
      return make_single_float (x / y);
    }
FD: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (x / y);
    }
Ds: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (x / y);
    }
Dl: Ds
Db: {
      if (y->zerop ())
        FEdivision_by_zero ();
      return make_double_float (x / y->to_double ());
    }
Dr: {return make_double_float (x / fract_to_double_float (y));}
DD: {
      if (!y)
        FEdivision_by_zero ();
      return make_double_float (x / y);
    }
DF: DD
sc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
lc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
bc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
rc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
Fc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
Dc: {return complex_divide (lx, make_fixnum (0), y->real, y->imag);}
cc: {return complex_divide (x->real, x->imag, y->real, y->imag);}
cs: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
cl: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
cb: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
cr: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
cF: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
cD: {return complex_divide (x->real, x->imag, ly, make_fixnum (0));}
}

lisp
integer_divide (x, y) integer
{
ss: {
      if (!y)
        FEdivision_by_zero ();
      return make_fixnum (x / y);
    }
sl: ss
sb: {
      bignum_rep_long xx (x);
      return make_integer (divide (0, &xx, y));
    }
ls: {
      if (!y)
        FEdivision_by_zero ();
      if (y == -1)
        return make_integer (-int64_t (x));
      return make_integer (int64_t (x / y));
    }
ll: ss
lb: sb
bs: {
      bignum_rep_long yy (y);
      return make_integer (divide (0, x, &yy));
    }
bl: bs
bb: {return make_integer (divide (0, x, y));}
}

lisp
Fconjugate (x)
{
s: {return lx;}
l: {return lx;}
b: {return lx;}
r: {return lx;}
F: {return lx;}
D: {return lx;}
c: {return make_complex (x->real, number_negate (x->imag));}
}

lisp
number_lcm (x, y) integer
{
ss: {
      bignum_rep_long xx (x);
      bignum_rep_long yy (y);
      return make_integer (lcm (&xx, &yy));
    }
sl: ss
sb: {
      bignum_rep_long xx (x);
      return make_integer (lcm (&xx, y));
    }
ls: ss
ll: ss
lb: sb
bs: @sb (y, x)
bl: bs
bb: {return make_integer (lcm (x, y));}
}

lisp
number_gcd (x, y) integer
{
ss: {
      u_long i = x >= 0 ? x : -x;
      u_long j = y >= 0 ? y : -y;
      if (i < j)
        swap (i, j);
      while (j)
        {
          u_long k = j;
          j = i % j;
          i = k;
        }
      return make_integer (int64_t (i));
    }
sl: ss
sb: {
      bignum_rep_long xx (x);
      return make_integer (gcd (&xx, y));
    }
ls: ss
ll: ss
lb: sb
bs: @sb (y, x)
bl: bs
bb: {return make_integer (gcd (x, y));}
}

#//12.5

#//12.5.1

#static lisp number_expt_integer (lisp, lisp);

lisp
integer_expt_integer (x, y) integer
{
ss: {
      bignum_rep_long xx (x);
      return make_integer (expt (0, &xx, y));
    }
sl: ss
sb: {return number_expt_integer (lx, ly);}
ls: ss
ll: ss
lb: sb
bs: {return make_integer (expt (0, x, y));}
bl: bs
bb: {return number_expt_integer (lx, ly);}
}

lisp
Fsqrt (x)
{
D: {
     if (x < 0)
       return make_complex (make_fixnum (0),
                            make_double_float (sqrt (-x)));
     return make_double_float (sqrt (x));
   }
F: {
     if (x < 0)
       return make_complex (make_fixnum (0),
                            make_single_float (float (sqrt (-x))));
     return make_single_float (float (sqrt (x)));
   }
s: {return FsqrtF (float (x), lx);}
l: s
b: {return FsqrtF (float (x->to_double ()), lx);}
r: {return FsqrtF (fract_to_single_float (x), lx);}
c: {return Fexp (number_divide (number_log (lx), make_fixnum (2)));}
}

lisp
Fisqrt (x) rational
{
s: {
     if (!x || x == 1)
       return make_fixnum (x);
     if (x < 0)
       FErange_error (make_fixnum (x));
     int r = x >> 1;
     while (1)
       {
         int q = x / r;
         if (q >= r)
           return make_fixnum (r);
         r = (r + q) >> 1;
       }
   }
l: s
b: {
     if (x->minusp ())
       FErange_error (lx);
     return make_integer (isqrt (x));
   }
r: {return make_ratio (Fisqrt (x->num), Fisqrt (x->den));}
}

#//12.5.2

lisp
Fsignum (x)
{
s: {
     if (x < 0)
       return make_fixnum (-1);
     if (x > 0)
       return make_fixnum (1);
     return make_fixnum (0);
   }
l: s
b: {return make_fixnum (x->sign ());}
r: {return Fsignum (x->num);}
F: {
     if (x < 0)
       return make_single_float (-1.0F);
     if (x > 0)
       return make_single_float (1.0F);
     return make_single_float (0.0F);
   }
D: {
     if (x < 0)
       return make_double_float (-1);
     if (x > 0)
       return make_double_float (1);
     return make_double_float (0);
   }
c: {
     if (Fzerop (lx) != Qnil)
       return lx;
     return number_divide (lx, Fabs (lx));
   }
}

#//12.6

lisp
Frational (x) real
{
s: {return lx;}
l: {return lx;}
b: {return lx;}
r: {return lx;}
D: {return flonum_to_rational (x);}
F: D
}

lisp
Frationalize (x) real
{
s: {return lx;}
l: {return lx;}
b: {return lx;}
r: {return lx;}
D: {return flonum_rationalize (x, DBL_DIG);}
F: {return flonum_rationalize (x, FLT_DIG);}
}

lisp
Fnumerator (x) rational
{
s: {return lx;}
l: {return lx;}
b: {return lx;}
r: {return x->num;}
}

lisp
Fdenominator (x) rational
{
s: {return make_fixnum (1);}
l: {return make_fixnum (1);}
b: {return make_fixnum (1);}
r: {return x->den;}
}

lisp
fix_number_2 (x, y) (FIX_FLONUM dfn, FIX_BIGNUM bfn, FIX_FIXNUM ffn) real
{
ss: {
      if (!y)
        FEdivision_by_zero ();
      long q, r;
      q = (*ffn)(&r, x, y);
      multiple_value::value (1) = make_fixnum (r);
      return make_fixnum (q);
    }
bb: {
      if (y->zerop ())
        FEdivision_by_zero ();
      bignum_rep *q, *r;
      (*bfn)(q, r, x, y);
      safe_bignum_rep qq (q);
      multiple_value::value (1) = make_integer (r);
      qq.discard ();
      return make_integer (q);
    }
DD: {
      if (!y)
        FEdivision_by_zero ();
      double q = (*dfn)(x / y);
      multiple_value::value (1) = make_double_float (x - y * q);
      return flonum_to_integer (q);
    }
FF: {
      if (!y)
        FEdivision_by_zero ();
      double q = (*dfn)(x / y);
      multiple_value::value (1) = make_single_float (float (x - y * q));
      return flonum_to_integer (q);
    }
sl: ss
sb: {
      bignum_rep_long xx (x);
      return fix_number_2bb (&xx, lx, y, ly, dfn, bfn, ffn);
    }
sr: {
      lisp q = fix_number_2 (number_multiply (lx, y->den), y->num, dfn, bfn, ffn);
      multiple_value::value (1) = make_ratio (multiple_value::value (1), y->den);
      return q;
    }
sF: {return fix_number_2FF (float (x), lx, y, ly, dfn, bfn, ffn);}
sD: DD
ls: {
      if (y == -1)
        {
          multiple_value::value (1) = make_fixnum (0);
          return make_integer (-int64_t (x));
        }
      return fix_number_2ss (x, lx, y, ly, dfn, bfn, ffn);
    }
ll: ss
lb: sb
lr: sr
lF: sF
lD: DD
bs: {
      bignum_rep_long yy (y);
      return fix_number_2bb (x, lx, &yy, ly, dfn, bfn, ffn);
    }
bl: bs
br: {return fix_number_2sr (0, lx, y, ly, dfn, bfn, ffn);}
bF: {return fix_number_2FF (float (x->to_double ()), lx, y, ly, dfn, bfn, ffn);}
bD: {return fix_number_2DD (x->to_double (), lx, y, ly, dfn, bfn, ffn);}
rs: {
      if (!y)
        FEdivision_by_zero ();
      lisp den = number_multiply (x->den, ly);
      lisp q = fix_number_2 (x->num, den, dfn, bfn, ffn);
      multiple_value::value (1) = make_ratio (multiple_value::value (1), x->den);
      return q;
    }
rl: rs
rb: {
      if (y->zerop ())
        FEdivision_by_zero ();
      lisp den = number_multiply (x->den, ly);
      lisp q = fix_number_2 (x->num, den, dfn, bfn, ffn);
      multiple_value::value (1) = make_ratio (multiple_value::value (1), x->den);
      return q;
    }
rr: {
      lisp q = fix_number_2 (number_multiply (x->num, y->den),
                             number_multiply (x->den, y->num),
                             dfn, bfn, ffn);
      multiple_value::value (1) = make_ratio (multiple_value::value (1),
                                              number_multiply (x->den, y->den));
      return q;
    }
rF: {return fix_number_2FF (fract_to_single_float (x), lx, y, ly, dfn, bfn, ffn);}
rD: {return fix_number_2DD (fract_to_double_float (x), lx, y, ly, dfn, bfn, ffn);}
Fs: {return fix_number_2FF (x, lx, float (y), ly, dfn, bfn, ffn);}
Fl: Fs
Fb: {return fix_number_2FF (x, lx, float (y->to_double ()), ly, dfn, bfn, ffn);}
Fr: {return fix_number_2FF (x, lx, fract_to_single_float (y), ly, dfn, bfn, ffn);}
FD: DD
Ds: DD
Dl: DD
Db: {return fix_number_2DD (x, lx, y->to_double (), ly, dfn, bfn, ffn);}
Dr: {return fix_number_2DD (x, lx, fract_to_double_float (y), ly, dfn, bfn, ffn);}
DF: DD
}

lisp
fix_flonum_2 (x, y) (FIX_FLONUM dfn) real
{
DD: {
      if (!y)
        FEdivision_by_zero ();
      double q = (*dfn)(x / y);
      multiple_value::value (1) = make_double_float (x - y * q);
      return make_double_float (q);
    }
FF: {
      if (!y)
        FEdivision_by_zero ();
      double q = (*dfn)(x / y);
      multiple_value::value (1) = make_single_float (float (x - y * q));
      return make_single_float (float (q));
    }
ss: {return fix_flonum_2FF (float (x), lx, float (y), ly, dfn);}
sl: ss
sb: {return fix_flonum_2FF (float (x), lx, float (y->to_double ()), ly, dfn);}
sr: {return fix_flonum_2FF (float (x), lx, fract_to_single_float (y), ly, dfn);}
sF: {return fix_flonum_2FF (float (x), lx, y, ly, dfn);}
sD: DD
ls: ss
ll: ss
lb: sb
lr: sr
lF: sF
lD: DD
bs: {return fix_flonum_2FF (float (x->to_double ()), lx, float (y), ly, dfn);}
bl: bs
bb: {return fix_flonum_2FF (float (x->to_double ()), lx,
                            float (y->to_double ()), ly, dfn);}
br: {return fix_flonum_2FF (float (x->to_double ()), lx,
                            fract_to_single_float (y), ly, dfn);}
bF: {return fix_flonum_2FF (float (x->to_double ()), lx, y, ly, dfn);}
bD: {return fix_flonum_2DD (x->to_double (), lx, y, ly, dfn);}
rs: {return fix_flonum_2FF (fract_to_single_float (x), lx, float (y), ly, dfn);}
rl: rs
rb: {return fix_flonum_2FF (fract_to_single_float (x), lx,
                            float (y->to_double ()), ly, dfn);}
rr: {return fix_flonum_2FF (fract_to_single_float (x), lx,
                            fract_to_single_float (y), ly, dfn);}
rF: {return fix_flonum_2FF (fract_to_single_float (x), lx, y, ly, dfn);}
rD: {return fix_flonum_2DD (fract_to_double_float (x), lx, y, ly, dfn);}
Fs: {return fix_flonum_2FF (x, lx, float (y), ly, dfn);}
Fl: Fs
Fb: {return fix_flonum_2FF (x, lx, float (y->to_double ()), ly, dfn);}
Fr: {return fix_flonum_2FF (x, lx, fract_to_single_float (y), ly, dfn);}
FD: DD
Ds: DD
Dl: DD
Db: {return fix_flonum_2DD (x, lx, y->to_double (), ly, dfn);}
Dr: {return fix_flonum_2DD (x, lx, fract_to_double_float (y), ly, dfn);}
DF: DD
}

lisp
Frealpart (x)
{
s: {return lx;}
l: {return lx;}
b: {return lx;}
r: {return lx;}
F: {return lx;}
D: {return lx;}
c: {return x->real;}
}

lisp
Fimagpart (x)
{
s: {return make_fixnum (0);}
l: {return make_fixnum (0);}
b: {return make_fixnum (0);}
r: {return make_fixnum (0);}
F: {return make_single_float (0.0F);}
D: {return make_double_float (0.0);}
c: {return x->imag;}
}

lisp
make_complex (x, y) real
{
ss: {
      if (!y)
        return lx;
      return make_complex_1 (lx, ly);
    }
sl: ss
sb: {
      if (y->zerop ())
        return lx;
      return make_complex_1 (lx, ly);
    }
sr: {return make_complex_1 (lx, ly);}
sF: {return make_complex_1 (make_single_float (float (x)), ly);}
sD: {return make_complex_1 (make_double_float (x), ly);}
ls: ss
ll: ss
lb: sb
lr: sr
lF: sF
lD: sD
bs: {return make_complexss (0, lx, y, ly);}
bl: bs
bb: {return make_complexsb (0, lx, y, ly);}
br: {return make_complex_1 (lx, ly);}
bF: {return make_complex_1 (make_single_float (float (x->to_double ())), ly);}
bD: {return make_complex_1 (make_double_float (x->to_double ()), ly);}
rs: {return make_complexss (0, lx, y, ly);}
rl: rs
rb: {return make_complexsb (0, lx, y, ly);}
rr: {return make_complex_1 (lx, ly);}
rF: {return make_complex_1 (make_single_float (fract_to_single_float (x)), ly);}
rD: {return make_complex_1 (make_double_float (fract_to_double_float (x)), ly);}
Fs: {return make_complex_1 (lx, make_single_float (float (y)));}
Fl: Fs
Fb: {return make_complex_1 (lx, make_single_float (float (y->to_double ())));}
Fr: {return make_complex_1 (lx, make_single_float (fract_to_single_float (y)));}
FF: {return make_complex_1 (lx, ly);}
FD: {return make_complex_1 (make_double_float (x), ly);}
Ds: {return make_complex_1 (lx, make_double_float (y));}
Dl: Ds
Db: {return make_complex_1 (lx, make_double_float (y->to_double ()));}
Dr: {return make_complex_1 (lx, make_double_float (fract_to_double_float (y)));}
DF: {return make_complex_1 (lx, make_double_float (y));}
DD: {return make_complex_1 (lx, ly);}
}

#//12.7

#static lisp fixnum_logope (long x, long y, logope_code ope);

lisp
number_logope (x, y) (logope_code ope) integer
{
ss: {return fixnum_logope (x, y, ope);}
sl: ss
sb: {
      bignum_rep *r = 0;
      bignum_rep_long xx (x);
      logope (r, ope, &xx, y);
      return make_integer (r);
    }
ls: ss
ll: ss
lb: sb
bs: {
      bignum_rep *r = 0;
      bignum_rep_long yy (y);
      logope (r, ope, x, &yy);
      return make_integer (r);
    }
bl: bs
bb: {
      bignum_rep *r = 0;
      logope (r, ope, x, y);
      return make_integer (r);
    }
}

lisp
Flognot (x) integer
{
s: {return make_fixnum (~x);}
l: s
b: {
     bignum_rep *r = 0;
     lognot (r, x);
     return make_integer (r);
   }
}

lisp
Flogtest (x, y) integer
{
ss: {return boole (x & y);}
sl: ss
sb: {
      bignum_rep_long xx (x);
      return boole (logtest (&xx, y));
    }
ls: ss
ll: ss
lb: sb
bs: @sb(y, x)
bl: bs
bb: {return boole (logtest (x, y));}
}

lisp
Flogcount (x) integer
{
s: {
     int n = 0;
     for (u_long i = x >= 0 ? x : ~x; i; n++, i &= i - 1)
       ;
     return make_fixnum (n);
   }
l: s
b: {return make_fixnum (logcount (x));}
}

lisp
Flogbitp (index, x) integer
{
ss: {
      if (index < 0)
        FErange_error (lindex);
      if (index >= BITS_PER_LONG)
        return boole (x < 0);
      return boole (x & (1 << index));
    }
sl: ss
sb: {
      if (index < 0)
        FErange_error (lindex);
      bignum_rep_long i (index);
      return boole (logbitp (x, index));
    }
ls: ss
ll: ss
lb: sb
bs: {
      if (index->minusp ())
        FErange_error (lindex);
      return boole (x < 0);
    }
bl: bs
bb: {
      if (index->minusp ())
        FErange_error (lindex);
      return boole (x->minusp ());
    }
}

lisp
Fash (x, count) integer
{
bs: {
      bignum_rep *r = 0;
      ash (r, x, count);
      return make_integer (r);
    }
bl: bs
bb: {
      if (!count->minusp ())
        FEbignum_overflow ();
      return make_fixnum (x->minusp () ? -1 : 0);
    }
ss: {
      if (count < 0)
        return make_fixnum (x >> -count);
      bignum_rep_long xx (x);
      return Fashbs (&xx, lx, count, lcount);
    }
sl: ss
sb: {
      if (!count->minusp ())
        FEbignum_overflow ();
      return make_fixnum (x >= 0 ? 0 : -1);
    }
ls: ss
ll: ss
lb: sb
}

lisp
Finteger_length (x) integer
{
s: {return make_fixnum (static_cast <long> (log2 (x >= 0 ? x : ~x)));}
l: s
b: {return make_fixnum (x->howlong ());}
}
