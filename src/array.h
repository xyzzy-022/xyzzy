// -*-C++-*-
#ifndef _array_h_
# define _array_h_

# define MAX_VECTOR_LENGTH (INT_MAX / sizeof (lisp))
# define ARRAY_RANK_LIMIT (SHRT_MAX + 1)

/* �z��̊��N���X
   �Ȃ�Ŕz��̊��N���X�Ȃ̂ɖ��O���x�N�^�Ȃ̂��Ƃ�
   �ׂ������Ƃ͋C�ɂ��Ȃ��悤�ɁBarray�̎������Asubscripts��
   ���߂�array�Ȃ����ŁA���Ԃ̓x�N�^�̃A�N�Z�X�Ȃ̂ŁA
   ����ł����̂��B*/
class lbase_vector: public lisp_object
{
public:
  int length;          // contents�Ɋ܂܂��v�f�̌�
  int padding;
  void *contents;      // �v�f�̔z��

  ~lbase_vector ()
    {
      xfree (contents);
    }

  void common_init ()
    {
      contents = 0;
    }
};

/* �������z��Ɣ�P���x�N�^�̊��N���X */
class lbase_array: public lbase_vector
{
public:
  int *dims;               // �������̔z��
  lisp displaced_to;       // ���L���Object�B���L���Ă��Ȃ��Ȃ�nil
  lisp referenced_list;    // ���������L���Ă���Object�̃��X�g
  short rank;              // rank��
  char adjustable;         // �T�C�Y�ύX�\?
  char has_fillp;          // �t�B���|�C���^�������Ă���?

  ~lbase_array ();
  void common_init ();
};

/* ��P���x�N�^�̊��N���X */
class lbase_complex_vector: public lbase_array
{
public:
  /* �������B�Œ�T�C�Y����������allocate����̂��i�j�Ȃ̂ł����Ɏ����Ă��� */
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

/* ��ʔz��ƕ����z��͍��Ƃ��Ă͂܂����������ł���B
  �A�N�Z�X�֐���������ƍH�v����(�������Ƃ�����)���҂���ʂ��Ă���B
  �킴�킴�ʃN���X�ɂ����̂͒P�Ɏ�����̓s�������ł���(Memory allocation
  ���l����Ε�����)�B*/

/* ��ʔz�� */
class lgeneral_array: public lbase_array
{
};

/* �����z�� */
class lstring_array: public lbase_array
{
};

/* �^�C�v�`�F�b�N�̃}�N��
   ������ӂ͖��O�������Ⴎ���Ⴕ�Ă��Ĕ��ɕ�����ɂ������� */

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

/* �x�N�^�̗v�f���B�t�B���|�C���^�����ꍇ�̓t�B���|�C���^�̒l */
inline int &
xvector_length (lisp x)
{
  assert (base_vector_p (x));
  return ((lbase_vector *)x)->length;
}

/* �x�N�^�̗v�f�̔z�� */
inline void *&
xbase_vector_contents (lisp x)
{
  assert (base_vector_p (x));
  return ((lbase_vector *)x)->contents;
}

/* �x�N�^�̎����B���Ȃ킿�A�x�N�^�̎��ۂ̒����B*/
inline int &
xvector_dimension (lisp x)
{
  assert (base_complex_vector_p (x));
  return ((lbase_complex_vector *)x)->dimension;
}

/* �z��S�̗̂v�f�� */
inline int &
xarray_total_size (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->length;
}

/* �z��̎����̔z�� */
inline int *&
xarray_dims (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->dims;
}

/* �z��̋��L���Object�B���L���Ă��Ȃ��Ȃ�nil */
inline lisp &
xarray_displaced_to (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->displaced_to;
}

/* x�����L���Ă���Object�̃��X�g */
inline lisp &
xarray_referenced_list (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->referenced_list;
}

/* �z���rank */
inline short &
xarray_rank (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->rank;
}

/* �z�񂪃T�C�Y�ύX�\? */
inline char &
xarray_adjustable (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_array *)x)->adjustable;
}

/* �z�񂪃t�B���|�C���^�������Ă���? */
inline char &
xarray_has_fillp (lisp x)
{
  assert (base_array_p (x));
  return ((lbase_complex_vector *)x)->has_fillp;
}

/* ��ʔz��̗v�f�̔z�� */
inline lisp *&
xgeneral_array_contents (lisp x)
{
  assert (general_array_p (x));
  return (lisp *&)xbase_vector_contents (x);
}

/* �����z��̗v�f�̔z�� */
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
