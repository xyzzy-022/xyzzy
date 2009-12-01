#ifndef _sequence_h_
# define _sequence_h_

class key_proc
{
protected:
  lisp lkey;
  lfunction_proc_1 pkey;
public:
  key_proc (lisp);
  lisp call_keyfn (lisp) const;
};

inline
key_proc::key_proc (lisp keys)
{
  lkey = find_keyword (Kkey, keys, Qnil);
  if (lkey == Qnil)
    lkey = Sidentity;
  pkey = lfunction_proc_1 (fast_funcall_p (lkey, 1));
}

inline lisp
key_proc::call_keyfn (lisp x) const
{
  if (pkey == Fidentity)
    return x;
  stack_trace trace (stack_trace::apply, lkey, x, 0);
  return pkey ? pkey (x) : funcall_1 (lkey, x);
}

class test_proc: public key_proc
{
protected:
  test_proc (lisp);

  lisp ltest;
  lfunction_proc ptest;

  int call_testfn (lisp) const;
  int call_testfn (lisp, lisp) const;
public:
  virtual int test (lisp) = 0;
};

inline
test_proc::test_proc (lisp keys)
     : key_proc (keys)
{
}

inline int
test_proc::call_testfn (lisp x) const
{
  stack_trace trace (stack_trace::apply, ltest, x, 0);
  if (ptest)
    x = lfunction_proc_1 (ptest)(x);
  else
    x = funcall_1 (ltest, x);
  multiple_value::clear ();
  return x != Qnil;
}

inline int
test_proc::call_testfn (lisp x, lisp y) const
{
  stack_trace trace (stack_trace::apply, ltest, x, y);
  if (ptest)
    x = lfunction_proc_2 (ptest)(x, y);
  else
    x = funcall_2 (ltest, x, y);
  multiple_value::clear ();
  return x != Qnil;
}

class seq_testproc: public test_proc
{
  lisp item;
  int test_not;
public:
  seq_testproc (lisp item, lisp keys);
  virtual int test (lisp);
  int test (lisp, lisp) const;
};

inline int
seq_testproc::test (lisp x, lisp y) const
{
  return call_testfn (x, y) != test_not;
}

class seq_testproc_if: public test_proc
{
public:
  seq_testproc_if (lisp pred, lisp keys);
  virtual int test (lisp);
};

class seq_testproc_if_not: public seq_testproc_if
{
public:
  seq_testproc_if_not (lisp pred, lisp keys);
  virtual int test (lisp);
};

lisp seq_to_string (lisp);
lisp seq_to_vector (lisp);
lisp seq_to_list (lisp);
void seq_start_end (int, int &, int &, lisp, lisp);

#endif
