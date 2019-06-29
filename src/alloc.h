// -*-C++-*-
#ifndef _alloc_h_
# define _alloc_h_

# include "cdecl.h"

/* �y�[�W�P�ʂŃ��������m�ۂ���BOS�ɑ΂��Ă�OS�̃��������蓖�ĒP�ʂ�
   ��������v�����A�Ăяo�����ɂ̓y�[�W�T�C�Y�̃����������蓖�Ă�B��
   �蓖�Ă�ꂽ�������̃A�h���X�́AOS�̃��������蓖�ĒP�ʂɐ��񂵂Ă�
   ��͂��B���郁�������蓖�ĒP�ʓ��̃y�[�W�����ׂĊJ�����ꂽ�ꍇ�̓�
   ������OS�ɕԂ��B���蓖�ĒP�ʂɂ�����y�[�W�̌���BITS_PER_INT�ȓ�
   �łȂ���΂Ȃ�Ȃ��B*/
class alloc_page
{
protected:
  /* ���g�p�̃y�[�W�̃��X�g */
  struct alloc_page_rep *ap_rep;

  /* OS�̃y�[�W�T�C�Y */
  static u_longlong ap_page_size;

  /* OS�̃��������蓖�ĒP�� */
  static u_longlong ap_block_size;

  /* �v�����ꂽ�y�[�W�T�C�Y(OS�̃y�[�W���E�ɐ؂�グ) */
  u_longlong ap_unit_size;

  /* ���蓖�ĒP�ʂ��Ƃ̃y�[�W�̌�
     0�̏ꍇ�̓y�[�W���Ƃ̊Ǘ��������Ɋ��蓖�ĒP�ʂ����̂܂ܕԂ� */
  unsigned long long ap_units_per_block;

public:
  alloc_page (unsigned long long size);
  ~alloc_page () {}

  void *alloc ();
  void free (void *);

  friend class fixed_heap;
};

/* alloc_page������ɌŒ蒷�T�C�Y�ɃT�u�A���P�[�V��������B
   �����̓s����A�T�C�Y��2�ׂ̂���łȂ���΂Ȃ�Ȃ��B���蓖�Ă�ꂽ
   �q�[�v�̃A�h���X�̓T�C�Y�̔{���ɂȂ��Ă���͂��B*/
class fixed_heap
{
protected:
  alloc_page fh_ap;
  struct fixed_heap_rep *fh_heap;

  u_int fh_heap_size;
  unsigned long long fh_heap_per_page;

public:
  fixed_heap (u_int size);
  ~fixed_heap () {}

  void *alloc ();
  void free (void *);
  u_int size () const {return fh_heap_size;}
};

#endif
