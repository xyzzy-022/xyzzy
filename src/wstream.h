#ifndef _wstream_h_
# define _wstream_h_

# include "StrBuf.h"

class wStream: public StrBuf
{
  char buf[2040];
  int col;
  void update_column (Char);
  void update_column (Char, int);
  void update_column (const Char *, int);
public:
  wStream (int = 0);
  void add (int);
  void add (Char);
  void fill (int, int);
  void fill (Char, int);
  void add (const char *);
  void add (const Char *, int);
  void add (wStream &);
  int columns () const;
  void case_conversion (wStream &, int, int);
};

class wStreamsStream: public wStream
{
  lisp dest;
protected:
  virtual void alloc ();
public:
  wStreamsStream (lisp);
  ~wStreamsStream ();
};

inline
wStream::wStream (int l)
     : StrBuf (buf, sizeof buf), col (l)
{
}

inline void
wStream::update_column (Char c)
{
  col = ::update_column (col, c);
}

inline void
wStream::update_column (const Char *s, int size)
{
  col = ::update_column (col, s, size);
}

inline void
wStream::update_column (Char c, int size)
{
  col = ::update_column (col, c, size);
}

inline void
wStream::add (Char c)
{
  StrBuf::add (c);
  update_column (c);
}

inline void
wStream::add (int c)
{
  add (Char (c & 0xff));
}

inline void
wStream::fill (Char c, int size)
{
  if (size <= 0)
    return;
  StrBuf::fill (c, size);
  update_column (c, size);
}

inline void
wStream::fill (int c, int size)
{
  fill (Char (c & 0xff), size);
}

inline void
wStream::add (const Char *s, int size)
{
  if (size <= 0)
    return;
  StrBuf::add (s, size);
  update_column (s, size);
}

inline void
wStream::add (wStream &sb)
{
  StrBuf::add (sb);
  for (const strbuf_chunk *cp = sb.sb_chunk; cp; cp = cp->cdr)
    update_column (cp->contents, cp->used - cp->contents);
}

inline int
wStream::columns () const
{
  return col;
}

inline
wStreamsStream::wStreamsStream (lisp s)
     : wStream (get_stream_column (s)), dest (s)
{
}

inline
wStreamsStream::~wStreamsStream ()
{
  if (sb_next - sb_chunk->contents)
    write_stream (dest, sb_chunk->contents, sb_next - sb_chunk->contents);
}

#endif
