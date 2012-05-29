// -*-C++-*-
#ifndef _hash_h_
# define _hash_h_

/* hash table */

typedef lfunction_proc_2 hash_test_proc;

struct hash_entry
{
  lisp key;
  lisp value;
};

# define MAX_HASH_TABLE_SIZE (INT_MAX / sizeof (hash_entry))

class lhash_table: public lisp_object
{
public:
  hash_test_proc test;
  int size;
  lisp rehash_size;
  float rehash_threshold;
  int used;
  int count;
  hash_entry *entry;

  ~lhash_table () {xfree (entry);}
};

# define hash_table_p(X) typep ((X), Thash_table)

inline void
check_hash_table (lisp x)
{
  check_type (x, Thash_table, Qhash_table);
}

inline hash_test_proc &
xhash_table_test_fn (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->test;
}

inline int &
xhash_table_size (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->size;
}

inline lisp &
xhash_table_rehash_size (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->rehash_size;
}

inline float &
xhash_table_rehash_threshold (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->rehash_threshold;
}

inline int &
xhash_table_used (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->used;
}

inline int &
xhash_table_count (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->count;
}

inline hash_entry *&
xhash_table_entry (lisp x)
{
  assert (hash_table_p (x));
  return ((lhash_table *)x)->entry;
}

lhash_table *make_hash_table ();

int equalp (lhash_table *, lhash_table *);
lChar Char_hash (Char, lisp);

void hash_table_rehash (lisp hash_table, int inc = 0);
lisp gethash (lisp key, lisp hash_table, lisp defalt);

#endif
