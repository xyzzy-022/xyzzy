// -*-C++-*-
#ifndef _symbol_h_
# define _symbol_h_

# define SFconstant          (1 << 0)      // 定数
# define SFspecial           (1 << 1)      // グローバルなスペシャル変数
# define SFlambda_key        (1 << 2)      // lambda-listキーワード
# define SFbuffer_local      (1 << 3)      // バッファローカル
# define SFmake_buffer_local (1 << 4)      // 値がセットされたらバッファローカル
# define SFdynamic_bind      (1 << 5)      // 動的にバインドされている

# ifndef NOT_COMPILE_TIME

class lsymbol: public lisp_object
{
public:
  u_int flags;
  lisp value;
  lisp fn;
  lisp plist;
  lisp package;
  lisp name;
};

# define symbolp(X) typep ((X), Tsymbol)

inline void
check_symbol (lisp x)
{
  check_type (x, Tsymbol, Qsymbol);
}

inline u_int &
xsymbol_flags (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->flags;
}

inline lisp &
xsymbol_function (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->fn;
}

inline lisp &
xsymbol_value (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->value;
}

inline lisp &
xsymbol_plist (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->plist;
}

inline lisp &
xsymbol_package (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->package;
}

inline lisp &
xsymbol_name (lisp x)
{
  assert (symbolp (x));
  return ((lsymbol *)x)->name;
}

# define constantp(X) (xsymbol_flags (X) & SFconstant)
# define specialp(X) (xsymbol_flags (X) & SFspecial)
# define lambda_key_p(X) (xsymbol_flags (X) & SFlambda_key)
# define buffer_local_p(X) (xsymbol_flags (X) & SFbuffer_local)
# define make_buffer_local_p(X) (xsymbol_flags (X) & SFmake_buffer_local)
# define dynamic_bind_p(X) (xsymbol_flags (X) & SFdynamic_bind)
# define void_function_p(X) (xsymbol_function (X) == Qunbound)

lsymbol *make_symbol (lisp name, u_int flags = 0);

# endif /* not NOT_COMPILE_TIME */

#endif
