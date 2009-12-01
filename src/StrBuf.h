// -*-C++-*-
#ifndef _StrBuf_h_
# define _StrBuf_h_

# include "cdecl.h"

class StrBuf
{
protected:
  struct strbuf_chunk
    {
      strbuf_chunk *cdr;
      Char *used;
      Char contents[1];
    };

  strbuf_chunk *sb_chunk;
  int sb_chunk_size;
  Char *sb_next;
  Char *sb_limit;
  int sb_finished;

  virtual void alloc ();

  void dump (strbuf_chunk *) const;

  void *sb_initial_buffer;

  void clear ();
  void init ();

  int linear_p () const;

public:
  StrBuf (void *, int);
  StrBuf (int = 2040);
  ~StrBuf ();

  void empty ();
  int empty_p () const;

  void add (int);
  void add (Char);
  void fill (int, int);
  void fill (Char, int);
  void add (const char *);
  void add (const Char *, int);
  void add (StrBuf &);
  void add_simple (const char *);
  void add_simple (const char *, int);

  void finish ();
  void copy (Char *);
  operator const Char * () const;
  int length () const;
  lisp make_string ();
  lisp make_substring (int, int);

  void dump () const;
};

inline
StrBuf::StrBuf (void *p, int size)
{
  assert (p);
  assert (size >= sizeof (strbuf_chunk));
  sb_initial_buffer = p;
  sb_chunk_size = (size - offsetof (strbuf_chunk, contents)) / sizeof (Char);
  init ();
}

inline
StrBuf::StrBuf (int size)
{
  assert (size > 0);
  sb_initial_buffer = 0;
  sb_chunk_size = size;
  init ();
}

inline
StrBuf::~StrBuf ()
{
  clear ();
}

inline void
StrBuf::empty ()
{
  clear ();
  init ();
}

inline void
StrBuf::add (Char c)
{
  assert (!sb_finished);
  if (sb_next == sb_limit)
    alloc ();
  *sb_next++ = c;
}

inline void
StrBuf::add (int c)
{
  add (Char (c & 0xff));
}

inline void
StrBuf::fill (int c, int n)
{
  fill (Char (c & 0xff), n);
}

inline
StrBuf::operator const Char * () const
{
  return sb_chunk->contents;
}

inline int
StrBuf::linear_p () const
{
  return sb_chunk && !sb_chunk->cdr;
}

#endif
