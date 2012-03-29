// -*-C++-*-
#ifndef _data_h_
# define _data_h_

# include "cdecl.h"
# include "alloc.h"

# define LDATA_PAGE_SIZE 1024
# define LDATA_PAGE_MASK (LDATA_PAGE_SIZE - 1)
# define LDATA_MIN_SIZE 4
# define LDATA_RADIX 2

# define LDATA_MAX_OBJECTS_PER_LONG \
  ((LDATA_PAGE_SIZE / LDATA_MIN_SIZE + BITS_PER_LONG - 1) / BITS_PER_LONG)

struct ldata_rep
{
  ldata_rep *dr_prev;
  ldata_rep *dr_next;
  int dr_type;
  u_long dr_used[LDATA_MAX_OBJECTS_PER_LONG];
  u_long dr_gc[LDATA_MAX_OBJECTS_PER_LONG];
  char dr_data[1];
};

class ldataP
{
public:
  fixed_heap ld_heap;
  ldata_rep *ld_rep;
  struct ldata_free_rep *ld_freep;
  static int ld_nwasted;
  static char *ld_lower_bound;
  static char *ld_upper_bound;

  ldataP ();

  ldata_rep *alloc (int);
  void free (ldata_rep *);
  int count_reps ();
  void get_reps (ldata_rep **);
  void alloc_reps (ldata_rep **, int, int);
  void link_unused (int);
  void free_all_reps ();
  void morecore (int, int);
  char *do_alloc (int, int);
  int find (void *, int, int);
};

template <class T, u_int F>
class ldata: public ldataP
{
  static int l_nuses;
  static int l_nfrees;
  static ldataP l_ld;
public:
  static T *lalloc ();
  static void sweep ();
  static void unuse (T *);
  static void cleanup ();
  static lisp countof ();
  static int count_reps ();
  static void get_reps (ldata_rep **);
  static void alloc_reps (ldata_rep **, int);
  static void dump_reps (FILE *);
  static void rdump_reps (FILE *);
  static void array_fixup_displaced_offset ();
  static void chunk_fixup_data_offset ();
  static void link_unused ();
  static void free_all_reps ();
  static int find (void *p)
    {return l_ld.find (p, F, sizeof (T));}
};

# define LDATASIZE_NOBJS(SIZE) \
  ((LDATA_PAGE_SIZE - offsetof (ldata_rep, dr_data)) / (SIZE))

# define LDATA_NOBJS(T) (LDATASIZE_NOBJS (sizeof (T)))

struct ldata_free_rep
{
  ldata_free_rep *lf_next;
};

inline void
bitset (u_long *p, int i)
{
  p[i / (sizeof *p * CHAR_BIT)] |= 1 << i % (sizeof *p * CHAR_BIT);
}

inline int
bitisset (const u_long *p, int i)
{
  return p[i / (sizeof *p * CHAR_BIT)] & (1 << i % (sizeof *p * CHAR_BIT));
}

inline void
bitclr (u_long *p, int i)
{
  p[i / (sizeof *p * CHAR_BIT)] &= ~(1 << i % (sizeof *p * CHAR_BIT));
}

int find_zero_bit (u_long *, int);

inline u_long *
used_place (void *r)
{
  return ((ldata_rep *)(pointer_t (r) & ~LDATA_PAGE_MASK))->dr_used;
}

inline u_long *
gc_place (void *r)
{
  return ((ldata_rep *)(pointer_t (r) & ~LDATA_PAGE_MASK))->dr_gc;
}

static inline int
bit_index (const void *p)
{
  return (pointer_t (p) & LDATA_PAGE_MASK) / LDATA_MIN_SIZE;
}

template <class T, u_int F>
T *
ldata <T, F>::lalloc ()
{
  return (T *)l_ld.do_alloc (F, sizeof (T));
}

void gc (int);
void init_syms ();
void combine_syms ();
int rdump_xyzzy ();
void cleanup_lisp_objects ();

void rehash_all_hash_tables ();

#endif
