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

   �|�C���^�l�̍ŉ��ʃr�b�g������Α��l���ǂ������킩��B
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
  // �z��p
  TAarray     = 0x80000000,  // �z��
  TAvector    = 0x40000000,  // �x�N�^
  TAsimple    = 0x20000000,  // �P���z��
  TAtype_mask = 0x1f000000,  // �z��̌^�̃}�X�N
    TAgeneral = 0x10000000,  // ��ʔz��
    TAstring  = 0x08000000,  // �����z��
    TAfixnum  = 0x04000000,  // fixnum�z��(�ł��ĂȂ�)
    TAbit     = 0x02000000,  // bit�z��(�ł��ĂȂ�)

  // ���l�p
  TNfixnum    = 0x00800000,  // fixnum
  TNbignum    = 0x00400000,  // bignum
  TNinteger   = 0x00200000,  // integer (fixnum|bignum�Ƃ��Ⴄ�񂩂�?)
  TNrational  = 0x00100000,  // �L����
  TNfloat     = 0x00080000,  // ���������_
  TNreal      = 0x00040000,  // ����
  TNnumber    = 0x00020000   // ��
};

enum lisp_object_type
{
  Tarray                 = TAarray | TAgeneral,     // ��ʔz��
  Tstring_array          = TAarray | TAstring,      // �����z��
  Tfixnum_array          = TAarray | TAfixnum,      // fixnum�z��(�܂��ł��ĂȂ�)
  Tbit_array             = TAarray | TAbit,         // bit�z��(�܂��ł��ĂȂ�)

  Tcomplex_vector        = TAarray | TAvector | TAgeneral,             // ��ʃx�N�^
  Tsimple_vector         = TAarray | TAvector | TAgeneral | TAsimple,  // ��ʒP���x�N�^
  Tcomplex_string        = TAarray | TAvector | TAstring,              // ������
  Tsimple_string         = TAarray | TAvector | TAstring  | TAsimple,  // �P��������
  Tcomplex_fixnum_vector = TAarray | TAvector | TAfixnum,              // fixnum�x�N�^
  Tsimple_fixnum_vector  = TAarray | TAvector | TAfixnum  | TAsimple,  // �P��fixnum�x�N�^
  Tcomplex_bit_vector    = TAarray | TAvector | TAbit,                 // bit�x�N�^
  Tsimple_bit_vector     = TAarray | TAvector | TAbit     | TAsimple,  // �P��bit�x�N�^

  // ���l�Ȑ����B���ۂɂ��̃^�O�����I�u�W�F�N�g�͑��݂��Ȃ��B
  Tshort_intP   = TNnumber | TNreal | TNrational | TNinteger | TNfixnum | 0,
  // ���l�łȂ������B30bit��32bit�ɂǂꂾ���Ⴂ������Ƃ��˂����܂Ȃ��悤�ɁB
  // �̂͑��l�Ȑ����͂Ȃ������̂��B
  Tlong_int     = TNnumber | TNreal | TNrational | TNinteger | TNfixnum | 1,
  Tlonglong_int = TNnumber | TNreal | TNrational | TNinteger | TNfixnum | 1,
  Tbignum       = TNnumber | TNreal | TNrational | TNinteger | TNbignum, // bignum
  Tfraction     = TNnumber | TNreal | TNrational,      // ����
  Tsingle_float = TNnumber | TNreal | TNfloat | 1,     // �P���x���������_
  Tdouble_float = TNnumber | TNreal | TNfloat | 2,     // �{���x���������_
  Tcomplex      = TNnumber,                            // ���f��

  TnilP = 0,             // �_�~�[
  TanyP,                 // �_�~�[(�Ȃ�ł���Ȃ񂪂����?)
  TimmediateP,           // ���l������킷�^���^�O
  TcharP,                // �����I�u�W�F�N�g�̋^���^�O
  TmessageP,             // ���b�Z�[�W�I�u�W�F�N�g�̋^���^�O
  Tcons,                 // �R���X
  Tsymbol,               // �V���{��
  Tclosure,              // ���L�V�J���N���[�W��
  Tfunction,             // �l�C�e�B�u�֐�
  Thash_table,           // �n�b�V���e�[�u��
  Tstream,               // �X�g���[��
  Tpackage,              // �p�b�P�[�W
  Trandom_state,         // �����_���X�e�[�g
  Tstruct_def,           // �\���̂̌^
  Tstruct_data,          // �\���̂̃C���X�^���X
  Tchunk,                // �ėp�o�b�t�@�̈�
  Tdll_module,           // DLL
  Tdll_function,         // DLL���̊֐�
  Tc_callable,           // C����Ăׂ�֐�
  Twindow,               // �E�B���h�E
  Tbuffer,               // �o�b�t�@
  Tmarker,               // �}�[�J
  Tsyntax_table,         // �V���^�b�N�X�e�[�u��
  Tprocess,              // �v���Z�X
  Tregexp,               // �R���p�C���������K�\��
  Twin32_menu,           // ���j���[
  Twin32_dde_handle,     // DDE
  Terror,                // �G���[�I�u�W�F�N�g
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
typedef lisp (__stdcall *lfunction_proc_4)(lisp, lisp, lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_5)(lisp, lisp, lisp, lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_6)(lisp, lisp, lisp, lisp, lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_7)(lisp, lisp, lisp, lisp, lisp, lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_8)(lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp);
typedef lisp (__stdcall *lfunction_proc_9)(lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp);

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

/* Lisp Object�̃^�O���擾����B���炩���߃|�C���^�ł��邱�Ƃ��m�F���邱�ƁB
  ���l��n���ƊԈႢ�Ȃ����ʁB */
inline int
object_typeof (lisp x)
{
  assert (x);
  assert (pointerp (x));
  assert (bitisset (used_place (x), bit_index (x)));
  /* ldata_rep��LDATA_PAGE_SIZE���E�ɂ��邩�炱��Ń^�O������ */
  return ((ldata_rep *)(pointer_t (x) & ~LDATA_PAGE_MASK))->dr_type;
}

inline int
typep (int x, lisp_object_type type)
{
  return x == type;
}

/* x��Object�^�C�v���^�O�ƈ�v���邩�ǂ���?
   ��������pointerp()���Ă�ł���̂ŁA�����̃I�u�W�F�N�g�^�C�v�ƈ�v���邩
   ���`�F�b�N����ꍇ�́A���炩���߃^�O�����o���Ă��������������BGCC�Ȃ�
   �������炸�AVC�̃I�v�e�B�}�C�U�������܂œ������Ƃ͎v����B*/
inline int
typep (lisp x, lisp_object_type type)
{
  return pointerp (x) && typep (object_typeof (x), type);
}

/* x��Object�^�C�v��type�łȂ����type-error���o���B
   expected�͊��҂���^���B */
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

/* x�̃T�u�^�C�v��bit�łȂ����type-error��f���B
   expected�͊��҂���^���B */
inline void
check_object_type_bit (lisp x, lisp_object_type_bits bit, lisp expected)
{
  if (!object_type_bit_p (x, bit))
    FEtype_error (x, expected);
}

inline lisp
boole (long long x)
{
  return x ? Qt : Qnil;
}

inline lisp
boole (void *x)
{
  return x ? Qt : Qnil;
}

/* src����dst��size��Lisp Object���R�s�[����B
   memcpy�Ƃ�src��dst���t�Ȃ̂Œ��ӁB
   BSD��bcopy�Ƃ�����ƈႤ�BChar*�p��bcopy������B*/
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

/* special bind�݂����� */
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

/* ��Ǐ�GOTO
   �����͑S�R�Z���X���Ȃ��B*/
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

/* ��ѐ���ꎞ�Ҕ�����(unwind-protect��protected-form�p) */
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

/* ���l�̃o�b�t�@
   ��ʂɑ��l��Ԃ��֐���values[0]�ɒl����ꂸ�ɖ߂�l�ŕԂ��̂ŁA
  �o�b�t�@�̒��g���g���ꍇ�͒��ӁB*/

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

/* �ꎞ�I�Ɏg���镶����B�����̑��݊��Ԓ��ɕʂ̂Ƃ����
   temporary_string�𐶐����Ȃ��悤�ɋC�����Ďg��Ȃ���
   �Ȃ��̂ŁA���܂�g����ꏊ���Ȃ��B*/
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
