// -*-C++-*-
#ifndef _alloc_h_
# define _alloc_h_

# include "cdecl.h"

/* ページ単位でメモリを確保する。OSに対してはOSのメモリ割り当て単位で
   メモリを要求し、呼び出し側にはページサイズのメモリを割り当てる。割
   り当てられたメモリのアドレスは、OSのメモリ割り当て単位に整列してい
   るはず。あるメモリ割り当て単位内のページがすべて開放された場合はメ
   モリをOSに返す。割り当て単位におけるページの個数はBITS_PER_INT以内
   でなければならない。*/
class alloc_page
{
protected:
  /* 未使用のページのリスト */
  struct alloc_page_rep *ap_rep;

  /* OSのページサイズ */
  static u_int ap_page_size;

  /* OSのメモリ割り当て単位 */
  static u_int ap_block_size;

  /* 要求されたページサイズ(OSのページ境界に切り上げ) */
  u_int ap_unit_size;

  /* 割り当て単位ごとのページの個数
     0の場合はページごとの管理をせずに割り当て単位をそのまま返す */
  u_int ap_units_per_block;

public:
  alloc_page (u_int size);
  ~alloc_page () {}

  void *alloc ();
  void free (void *);

  friend class fixed_heap;
};

/* alloc_pageをさらに固定長サイズにサブアロケーションする。
   処理の都合上、サイズは2のべき乗でなければならない。割り当てられた
   ヒープのアドレスはサイズの倍数になっているはず。*/
class fixed_heap
{
protected:
  alloc_page fh_ap;
  struct fixed_heap_rep *fh_heap;

  u_int fh_heap_size;
  u_int fh_heap_per_page;

public:
  fixed_heap (u_int size);
  ~fixed_heap () {}

  void *alloc ();
  void free (void *);
  u_int size () const {return fh_heap_size;}
};

#endif
