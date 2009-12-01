// -*-C++-*-
#ifndef _array_h_
# define _array_h_

# define MAX_VECTOR_LENGTH (INT_MAX / sizeof (lisp))
# define ARRAY_RANK_LIMIT (SHRT_MAX + 1)

/* 配列の基底クラス
   なんで配列の基底クラスなのに名前がベクタなのかとか
   細かいことは気にしないように。arrayの実装が、subscriptsの
   解釈がarrayなだけで、実態はベクタのアクセスなので、
   これでいいのだ。*/
class lbase_vector: public lisp_object
{
public:
  int length;          // contentsに含まれる要素の個数
  void *contents;      // 要素の配列

  ~lbase_vector ()
    {
      xfree (contents);
    }

  void common_init ()
    {
      contents = 0;
    }
};

/* 多次元配列と非単純ベクタの基底クラス */
class lbase_array: public lbase_vector
{
public:
  int *dims;               // 次元数の配列
  lisp displaced_to;       // 共有先のObject。共有していないならnil
  lisp referenced_list;    // 自分を共有しているObjectのリスト
  short rank;              // rank数
  char adjustable;         // サイズ変更可能?
  char has_fillp;          // フィルポインタを持っている?

  ~lbase_array ();
  void common_init ();
};

/* 非単純ベクタの基底クラス */
class lbase_complex_vector: public lbase_array
{
public:
  /* 次元数。固定サイズをいちいちallocateするのもナニなのでここに持っている */
  int dimension;

  void common_init ()
    {
      lbase_array::common_init ();
      dims = &dimension;
      rank = 1;
    }
  ~lbase_complex_vector ()
    {
      dims = 0;
    }
};

/* 一般配列と文字配列は作りとしてはまったく同じである。
  アクセス関数がちょっと工夫して(汚いことをして)両者を区別している。
  わざわざ別クラスにしたのは単に実装上の都合だけである(Memory allocation
  を考えれば分かる)。*/

/* 一般配列 */
class lgeneral_array: public lbase_array
{
};

/* 文字配列 */
class lstring_array: public lbase_array
{
};

/* タイプチェックのマクロ
   ここら辺は名前がぐちゃぐちゃしていて非常に分かりにくいかも */

# define base_array_p(X) \
  object_type_mask_p ((X), TAarray | TAsimple, TAarray)

# define base_vector_p(X) object_type_bit_p ((X), TAarray)

# define common_vector_p(X) object_type_bit_p ((X), TAvector)

# define base_simple_vector_p(X) \
  object_type_mask_p ((X), TAvector | TAsimple, TAvector | TAsimple)

# define base_complex_vector_p(X) \
  object_type_mask_p ((X), TAvector | TAsimple, TAvector)

# define general_array_p(X) typep ((X), Tarray)
# define string_array_p(X) typep ((X), Tstring_array)

# define common_array_p(X) object_type_bit_p ((X), TAarray)

/* ベクタの要素数。フィルポインタを持つ場合はフィルポインタの値 */
inline int &
xvector_length (lisp x)
{
  assert (base_vector_p (x));
  return ((lbase_vector *)x)->length;
}

/* ベクタの要素の配列 */
inline void *&
xbase_vector_contents (lisp x)
{
  assert (base_vector_p (x));
  return ((lbase_vector *)x)->contents;
}

/* ベクタの次元。すなわち、ベクタの実際の長さ。*/
inline int &
xvector_dimension (lisp x)
{
  assert (base_complex_vector_p (x));
  return ((lbase_complex_vector *)x)->dimension;
}

/* 配列全体の要素数 */
inline int &
xarray_total_size (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->length;
}

/* 配列の次元の配列 */
inline int *&
xarray_dims (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->dims;
}

/* 配列の共有先のObject。共有していないならnil */
inline lisp &
xarray_displaced_to (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->displaced_to;
}

/* xを共有しているObjectのリスト */
inline lisp &
xarray_referenced_list (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->referenced_list;
}

/* 配列のrank */
inline short &
xarray_rank (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->rank;
}

/* 配列がサイズ変更可能? */
inline char &
xarray_adjustable (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->adjustable;
}

/* 配列がフィルポインタを持っている? */
inline char &
xarray_has_fillp (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_complex_vector *)x)->has_fillp;
}

/* 一般配列の要素の配列 */
inline lisp *&
xgeneral_array_contents (lisp x)
{
  assert (general_array_p (x));
  return (lisp *&)xbase_vector_contents (x);
}

/* 文字配列の要素の配列 */
inline Char *&
xstring_array_contents (lisp x)
{
  assert (string_array_p (x));
  return (Char *&)xbase_vector_contents (x);
}

inline void
check_array (lisp x)
{
  if (!common_array_p (x))
    FEtype_error (x, Qarray);
}

inline lgeneral_array *
make_general_array ()
{
  lgeneral_array *p = ldata <lgeneral_array, Tarray>::lalloc ();
  p->common_init ();
  return p;
}

inline lstring_array *
make_string_array ()
{
  lstring_array *p = ldata <lstring_array, Tstring_array>::lalloc ();
  p->common_init ();
  return p;
}

void check_array_type (lisp);
Char initial_char_elem (lisp);
lisp array_element_type (lisp);
void check_displace_type (lisp, lisp);
lisp displace_array (lisp, int, lisp, lisp, lisp);
void adjust_displace_array (lisp, void *);

#endif
