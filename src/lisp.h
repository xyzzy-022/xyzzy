// -*-C++-*-
#ifndef _lisp_h_
# define _lisp_h_

# include "cdecl.h"

# define QUIT check_quit ()
# define QUITP (xsymbol_value (Vquit_flag) != Qnil \
                && xsymbol_value (Vinhibit_quit) == Qnil)

class lisp_object
{
  lisp_object ();
public:
  void operator delete (void *){}
};

typedef lisp_object *lisp;

# include "signal.h"
# include "data.h"
# include "utils.h"

extern lisp Qnil;
extern lisp Qunbound;

/*
   DATA TYPE REPRESENTATIONS

   B: Data bit
   P: Pointer address bit

                  MSB ........ ........ ........ ........  LSB
IMMEDIATE:
   Short Integer:     BBBBBBBB BBBBBBBB BBBBBBBB BBBBBB01
   Character:         BBBBBBBB BBBBBBBB 00000000 00000111
   Message:           BBBBBBBB BBBBBBBB 00000000 00001011
POINTER:
                      PPPPPPPP PPPPPPPP PPPPPPPP PPPPPP00

   ポインタ値の最下位ビットを見れば即値かどうかがわかる。
 */

# define IMMEDIATE_BIT 1
# define SHORT_INT_TEST_BITS (2 | IMMEDIATE_BIT)

# define LSHORT_INT_SHIFT 2

# define Lshort_int IMMEDIATE_BIT
# define Lchar ((1 << LSHORT_INT_SHIFT) | SHORT_INT_TEST_BITS)
# define Lmessage ((2 << LSHORT_INT_SHIFT) | SHORT_INT_TEST_BITS)

enum message_code;

enum lisp_object_type_bits
{
  // 配列用
  TAarray     = 0x80000000,  // 配列
  TAvector    = 0x40000000,  // ベクタ
  TAsimple    = 0x20000000,  // 単純配列
  TAtype_mask = 0x1f000000,  // 配列の型のマスク
    TAgeneral = 0x10000000,  // 一般配列
    TAstring  = 0x08000000,  // 文字配列
    TAfixnum  = 0x04000000,  // fixnum配列(できてない)
    TAbit     = 0x02000000,  // bit配列(できてない)

  // 数値用
  TNfixnum    = 0x00800000,  // fixnum
  TNbignum    = 0x00400000,  // bignum
  TNinteger   = 0x00200000,  // integer (fixnum|bignumとちゃうんかい?)
  TNrational  = 0x00100000,  // 有理数
  TNfloat     = 0x00080000,  // 浮動小数点
  TNreal      = 0x00040000,  // 実数
  TNnumber    = 0x00020000   // 数
};

enum lisp_object_type
{
  Tarray                 = TAarray | TAgeneral,     // 一般配列
  Tstring_array          = TAarray | TAstring,      // 文字配列
  Tfixnum_array          = TAarray | TAfixnum,      // fixnum配列(まだできてない)
  Tbit_array             = TAarray | TAbit,         // bit配列(まだできてない)

  Tcomplex_vector        = TAarray | TAvector | TAgeneral,             // 一般ベクタ
  Tsimple_vector         = TAarray | TAvector | TAgeneral | TAsimple,  // 一般単純ベクタ
  Tcomplex_string        = TAarray | TAvector | TAstring,              // 文字列
  Tsimple_string         = TAarray | TAvector | TAstring  | TAsimple,  // 単純文字列
  Tcomplex_fixnum_vector = TAarray | TAvector | TAfixnum,              // fixnumベクタ
  Tsimple_fixnum_vector  = TAarray | TAvector | TAfixnum  | TAsimple,  // 単純fixnumベクタ
  Tcomplex_bit_vector    = TAarray | TAvector | TAbit,                 // bitベクタ
  Tsimple_bit_vector     = TAarray | TAvector | TAbit     | TAsimple,  // 単純bitベクタ

  // 即値な整数。実際にこのタグを持つオブジェクトは存在しない。
  Tshort_intP   = TNnumber | TNreal | TNrational | TNinteger | TNfixnum | 0,
  // 即値でない整数。30bitと32bitにどれだけ違いがあるとか突っ込まないように。
  // 昔は即値な整数はなかったのだ。
  Tlong_int     = TNnumber | TNreal | TNrational | TNinteger | TNfixnum | 1,
  Tbignum       = TNnumber | TNreal | TNrational | TNinteger | TNbignum, // bignum
  Tfraction     = TNnumber | TNreal | TNrational,      // 分数
  Tsingle_float = TNnumber | TNreal | TNfloat | 1,     // 単精度浮動小数点
  Tdouble_float = TNnumber | TNreal | TNfloat | 2,     // 倍精度浮動小数点
  Tcomplex      = TNnumber,                            // 複素数

  TnilP = 0,             // ダミー
  TanyP,                 // ダミー(なんでこんなんがあるんだ?)
  TimmediateP,           // 即値をあらわす疑似タグ
  TcharP,                // 文字オブジェクトの疑似タグ
  TmessageP,             // メッセージオブジェクトの疑似タグ
  Tcons,                 // コンス
  Tsymbol,               // シンボル
  Tclosure,              // レキシカルクロージャ
  Tfunction,             // ネイティブ関数
  Thash_table,           // ハッシュテーブル
  Tstream,               // ストリーム
  Tpackage,              // パッケージ
  Trandom_state,         // ランダムステート
  Tstruct_def,           // 構造体の型
  Tstruct_data,          // 構造体のインスタンス
  Tchunk,                // 汎用バッファ領域
  Tdll_module,           // DLL
  Tdll_function,         // DLL内の関数
  Tc_callable,           // Cから呼べる関数
  Twindow,               // ウィンドウ
  Tbuffer,               // バッファ
  Tmarker,               // マーカ
  Tsyntax_table,         // シンタックステーブル
  Tprocess,              // プロセス
  Tregexp,               // コンパイルした正規表現
  Twin32_menu,           // メニュー
  Twin32_dde_handle,     // DDE
  Terror,                // エラーオブジェクト
  Toledata,              // IDispatch
  Treadtable,            // readtable
  Twait_object,          // wait-object
  Tchar_encoding,        // character encoding scheme
  Tenvironment           // environment object
};

class lcons;
class lsymbol;

class lex_env;

typedef lisp (__stdcall *lfunction_proc)();
typedef lisp (__stdcall *lfunction_proc_0)();
typedef lisp (__stdcall *lfunction_proc_1)(lisp);
typedef lisp (__stdcall *lfunction_proc_2)(lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_3)(lisp, lisp, lisp);

# include "fns.h"
# ifndef EXTERN
#  define EXTERN extern
# endif
# include "vars-decl.h"
# include "fns-decl.h"

# include "msgcode.h"

inline u_short
lowbits (pointer_t x)
{
  return u_short (x & 0xffff);
}

inline u_short
hibits (pointer_t x)
{
  return u_short ((x >> 16) & 0xffff);
}

inline lisp
make_immediate (u_short type, u_short data)
{
  return lisp ((pointer_t (data) << 16) | type);
}

inline u_short
ximmediate_data (lisp x)
{
  return hibits (pointer_t (x));
}

inline int
immediatep (lisp x)
{
  return pointer_t (x) & IMMEDIATE_BIT;
}

inline int
pointerp (lisp x)
{
  return !immediatep (x);
}

/* Lisp Objectのタグを取得する。あらかじめポインタであることを確認すること。
  即値を渡すと間違いなく死ぬ。 */
inline int
object_typeof (lisp x)
{
  assert (x);
  assert (pointerp (x));
  assert (bitisset (used_place (x), bit_index (x)));
  /* ldata_repはLDATA_PAGE_SIZE境界にあるからこれでタグが取れる */
  return ((ldata_rep *)(pointer_t (x) & ~LDATA_PAGE_MASK))->dr_type;
}

inline int
typep (int x, lisp_object_type type)
{
  return x == type;
}

/* xのObjectタイプがタグと一致するかどうか?
   いちいちpointerp()を呼んでいるので、複数のオブジェクトタイプと一致するか
   をチェックする場合は、あらかじめタグを取り出しておいた方が速い。GCCなら
   いざしらず、VCのオプティマイザがそこまで頭いいとは思えん。*/
inline int
typep (lisp x, lisp_object_type type)
{
  return pointerp (x) && typep (object_typeof (x), type);
}

/* xのObjectタイプがtypeでなければtype-errorを出す。
   expectedは期待する型名。 */
inline void
check_type (lisp x, lisp_object_type type, lisp expected)
{
  if (!typep (x, type))
    FEtype_error (x, expected);
}

inline int
object_type_bit_p (int x, lisp_object_type_bits bit)
{
  return (x & bit) == bit;
}

inline int
object_type_bit_p (lisp x, lisp_object_type_bits bit)
{
  return pointerp (x) && object_type_bit_p (object_typeof (x), bit);
}

inline int
object_type_mask_p (int x, int mask, int test)
{
  return (x & mask) == test;
}

inline int
object_type_mask_p (lisp x, int mask, int test)
{
  return pointerp (x) && object_type_mask_p (object_typeof (x), mask, test);
}

/* xのサブタイプがbitでなければtype-errorを吐く。
   expectedは期待する型名。 */
inline void
check_object_type_bit (lisp x, lisp_object_type_bits bit, lisp expected)
{
  if (!object_type_bit_p (x, bit))
    FEtype_error (x, expected);
}

inline lisp
boole (int x)
{
  return x ? Qt : Qnil;
}

inline lisp
boole (void *x)
{
  return x ? Qt : Qnil;
}

/* srcからdstへsize個のLisp Objectをコピーする。
   memcpyとはsrcとdstが逆なので注意。
   BSDのbcopyともちょと違う。Char*用のbcopyもある。*/
inline void
bcopy (lisp *src, lisp *dst, size_t size)
{
  memcpy (dst, src, sizeof (lisp) * size);
}

# include "cons.h"
# include "symbol.h"

void handle_quit ();

inline void
check_quit ()
{
  if (QUITP)
    handle_quit ();
}

# include "function.h"
# include "closure.h"
# include "number.h"
# include "char.h"
# include "list.h"
# include "vector.h"
# include "string.h"
# include "stream.h"
# include "package.h"
# include "hash.h"
# include "message.h"
# include "error.h"
# include "trace.h"
# include "random.h"
# include "structure.h"
# include "readtab.h"

class protect_gc
{
  static protect_gc *gcl;
  protect_gc *last;
  int nvars;
  lisp *var;
  void chain ();
public:
  protect_gc (lisp &);
  protect_gc (lisp *, int);
  ~protect_gc ();
  friend void gc_mark_object ();
};

inline void
protect_gc::chain ()
{
  last = gcl;
  gcl = this;
}

inline
protect_gc::protect_gc (lisp &v)
{
  var = &v;
  nvars = 1;
  chain ();
}

inline
protect_gc::protect_gc (lisp *v, int n)
{
  var = v;
  nvars = n;
  chain ();
}

inline
protect_gc::~protect_gc ()
{
  gcl = last;
}

class dyn_protect_gc
{
  static dyn_protect_gc *gcl;
  dyn_protect_gc *prev;
  dyn_protect_gc *next;
  int nvars;
  lisp *var;
  void chain ()
    {
      prev = 0;
      next = gcl;
      if (gcl)
        gcl->prev = this;
      gcl = this;
    }
public:
  dyn_protect_gc (lisp &v)
    {
      var = &v;
      nvars = 1;
      chain ();
    }
  dyn_protect_gc (lisp *v, int n)
    {
      var = v;
      nvars = n;
      chain ();
    }
  ~dyn_protect_gc ()
    {
      if (prev)
        prev->next = next;
      else
        gcl = next;
      if (next)
        next->prev = prev;
    }
  friend void gc_mark_object ();
};

/* special bindみたいな */
class dynamic_bind
{
  lisp old;
  lisp var;
  int f;
  protect_gc pgc;
public:
  dynamic_bind (lisp, lisp);
  ~dynamic_bind ();
};

inline
dynamic_bind::dynamic_bind (lisp x, lisp val)
     : old (xsymbol_value (x)), var (x), f (xsymbol_flags (x) & SFdynamic_bind), pgc (old)
{
  assert (symbolp (var));
  xsymbol_value (var) = val;
  xsymbol_flags (var) |= SFdynamic_bind;
}

inline
dynamic_bind::~dynamic_bind ()
{
  xsymbol_value (var) = old;
  if (!f)
    xsymbol_flags (var) &= ~SFdynamic_bind;
  else
    assert (xsymbol_flags (var) & SFdynamic_bind);
}

/* 非局所GOTO
   こいつは全然センスがない。*/
struct nonlocal_data
{              // RETURN-FROM     GO      THROW    ERROR
  lisp type;   //   Qblock     Qtagbody   Qcatch   Qtoplevel   Qexit_this_level
  lisp value;  //   VALUE      Qnil       VALUE    Qnil        VALUE
  lisp tag;    //   TAG        TAG        TAG      Qnil        Qnil
  lisp id;     //   FRAME-ID   FRAME-ID   Qnil     CONDITION   Qt/Qnil

  nonlocal_data () : type (Qnil), value (Qnil), tag (Qnil), id (Qnil) {}
};

class nonlocal_jump
{
  static nonlocal_data *d;
public:
  static nonlocal_data *data ();
  friend class save_nonlocal_jump;
};

inline nonlocal_data *
nonlocal_jump::data ()
{
  return d;
}

/* 飛び先を一時待避する(unwind-protectのprotected-form用) */
class save_nonlocal_jump
{
  protect_gc pgc;
  nonlocal_data *last;
  nonlocal_data data;
public:
  save_nonlocal_jump ();
  ~save_nonlocal_jump ();
};

inline
save_nonlocal_jump::save_nonlocal_jump ()
     : pgc ((lisp *)nonlocal_jump::d, sizeof *nonlocal_jump::d / sizeof (lisp))
{
  last = nonlocal_jump::d;
  nonlocal_jump::d = &data;
}

inline
save_nonlocal_jump::~save_nonlocal_jump ()
{
  nonlocal_jump::d = last;
}

/* 多値のバッファ
   一般に多値を返す関数はvalues[0]に値を入れずに戻り値で返すので、
  バッファの中身を使う場合は注意。*/

# define MULTIPLE_VALUES_LIMIT 32

struct multiple_value_data
{
  int count;
  lisp values[MULTIPLE_VALUES_LIMIT];
};

class multiple_value
{
  static multiple_value_data *d;
public:
  static multiple_value_data *data ();
  static void clear ();
  static lisp *values ();
  static lisp &value (int);
  static int &count ();
  friend class save_multiple_value;
};

inline multiple_value_data *
multiple_value::data ()
{
  return d;
}

inline void
multiple_value::clear ()
{
  d->count = 1;
}

inline lisp *
multiple_value::values ()
{
  return d->values;
}

inline lisp &
multiple_value::value (int n)
{
  assert (n >= 0 && n < MULTIPLE_VALUES_LIMIT);
  return d->values[n];
}

inline int &
multiple_value::count ()
{
  return d->count;
}

class save_multiple_value
{
  protect_gc pgc;
  multiple_value_data *last;
  multiple_value_data data;
public:
  save_multiple_value (lisp);
  ~save_multiple_value ();
};

inline
save_multiple_value::save_multiple_value (lisp first)
     : pgc (multiple_value::d->values, multiple_value::d->count)
{
  multiple_value::d->values[0] = first;
  last = multiple_value::d;
  multiple_value::d = &data;
  data.values[0] = Qnil;
  data.count = 1;
}

inline
save_multiple_value::~save_multiple_value ()
{
  multiple_value::d = last;
}

/* 一時的に使える文字列。こいつの存在期間中に別のところで
   temporary_stringを生成しないように気をつけて使わなきゃ
   ならんので、あまり使える場所がない。*/
class temporary_string
{
  Char *save;
public:
  temporary_string (Char *, int);
  ~temporary_string ();
  static lisp string ();
};

inline
temporary_string::temporary_string (Char *s, int l)
{
  assert (stringp (xsymbol_name (Qtemporary_string)));
  assert (!xstring_length (xsymbol_name (Qtemporary_string)));
  save = xstring_contents (xsymbol_name (Qtemporary_string));
  xstring_contents (xsymbol_name (Qtemporary_string)) = s;
  xstring_length (xsymbol_name (Qtemporary_string)) = l;
}

inline
temporary_string::~temporary_string ()
{
  xstring_contents (xsymbol_name (Qtemporary_string)) = save;
  xstring_length (xsymbol_name (Qtemporary_string)) = 0;
}

inline lisp
temporary_string::string ()
{
  return xsymbol_name (Qtemporary_string);
}

class suppress_gc
{
private:
  int sg_save;
  static int sg_suppress_p;
public:
  suppress_gc () : sg_save (sg_suppress_p) {sg_suppress_p = 1;}
  ~suppress_gc () {sg_suppress_p = sg_save;}
  static int gc_suppressed_p () {return sg_suppress_p;}
};

#endif /* not _lisp_h_ */
