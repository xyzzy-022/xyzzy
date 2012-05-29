// -*-C++-*-
#ifndef _chunk_h_
# define _chunk_h_

class lchunk: public lisp_object
{
public:
  lisp type;       // chunkのタイプ
  int size;        // サイズ
  void *data;      // データ
  lisp owner;  // 領域を確保したオブジェクト(自分自身/他のchunk/nil)

  ~lchunk () {if (owner == this) xfree (data);}
};

#define chunkp(X) typep ((X), Tchunk)

inline void
check_chunk (lisp x)
{
  check_type (x, Tchunk, Qsi_chunk);
}

inline lisp &
xchunk_type (lisp x)
{
  assert (chunkp (x));
  return ((lchunk *)x)->type;
}

inline int &
xchunk_size (lisp x)
{
  assert (chunkp (x));
  return ((lchunk *)x)->size;
}

inline void *&
xchunk_data (lisp x)
{
  assert (chunkp (x));
  return ((lchunk *)x)->data;
}

inline lisp &
xchunk_owner (lisp x)
{
  assert (chunkp (x));
  return ((lchunk *)x)->owner;
}

long cast_to_long (lisp object);
int64_t cast_to_int64 (lisp object);

#endif
