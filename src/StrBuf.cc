#include "stdafx.h"
#include "ed.h"
#include "StrBuf.h"

int
StrBuf::length () const
{
  if (!sb_chunk)
    return 0;
  int l = sb_chunk_size - (sb_limit - sb_next);
  for (strbuf_chunk *p = sb_chunk->cdr; p; p = p->cdr)
    l += sb_chunk_size;
  return l;
}

int
StrBuf::empty_p () const
{
  if (!sb_chunk)
    return 1;
  if (sb_chunk_size - (sb_limit - sb_next))
    return 0;
  for (strbuf_chunk *p = sb_chunk->cdr; p; p = p->cdr)
    if (sb_chunk_size)
      return 0;
  return 1;
}

void
StrBuf::copy (Char *b)
{
  assert (sb_finished);
  for (const strbuf_chunk *cp = sb_chunk; cp; cp = cp->cdr)
    {
      int size = cp->used - cp->contents;
      bcopy (cp->contents, b, size);
      b += size;
    }
}

void
StrBuf::init ()
{
  sb_chunk = (strbuf_chunk *)sb_initial_buffer;
  if (sb_chunk)
    {
      sb_chunk->cdr = 0;
      sb_next = sb_chunk->contents;
      sb_limit = sb_next + sb_chunk_size;
    }
  else
    {
      sb_next = 0;
      sb_limit = 0;
    }
  sb_finished = 0;
}

void
StrBuf::clear ()
{
  while (sb_chunk)
    {
      strbuf_chunk *p = sb_chunk->cdr;
      if (sb_chunk != sb_initial_buffer)
        xfree (sb_chunk);
      sb_chunk = p;
    }
}

void
StrBuf::alloc ()
{
  assert (sb_chunk ? sb_limit == sb_chunk->contents + sb_chunk_size : 1);
  strbuf_chunk *p = (strbuf_chunk *)xmalloc (sizeof (strbuf_chunk)
                                             + sizeof (Char) * (sb_chunk_size - 1));
  if (sb_chunk)
    sb_chunk->used = sb_limit;
  p->cdr = sb_chunk;
  sb_chunk = p;
  sb_next = sb_chunk->contents;
  sb_limit = sb_next + sb_chunk_size;
}

void
StrBuf::fill (Char c, int size)
{
  assert (!sb_finished);
  if (!size)
    return;
  int rest = sb_limit - sb_next;
  if (size <= rest)
    {
      for (int i = 0; i < size; i++)
        *sb_next++ = c;
    }
  else
    {
      while (1)
        {
          for (int i = 0; i < rest; i++)
            sb_next[i] = c;
          size -= rest;
          if (!size)
            break;
          alloc ();
          rest = min (size, sb_chunk_size);
        }
      sb_next += rest;
    }
}

void
StrBuf::add (const Char *s, int size)
{
  assert (!sb_finished);
  if (!size)
    return;
  int rest = sb_limit - sb_next;
  if (size <= rest)
    {
      bcopy (s, sb_next, size);
      sb_next += size;
    }
  else
    {
      while (1)
        {
          bcopy (s, sb_next, rest);
          s += rest;
          size -= rest;
          if (!size)
            break;
          alloc ();
          rest = min (size, sb_chunk_size);
        }
      sb_next += rest;
    }
}

void
StrBuf::add (const char *s)
{
  assert (!sb_finished);
  if (!*s)
    return;
  int rest = sb_limit - sb_next;
  if (rest)
    {
      sb_next = s2w (sb_next, rest, &s);
      if (!*s)
        return;
      assert (sb_next == sb_limit);
    }
  do
    {
      alloc ();
      sb_next = s2w (sb_next, sb_chunk_size, &s);
    }
  while (*s);
}

void
StrBuf::add (StrBuf &sb)
{
  sb.finish ();
  for (const strbuf_chunk *cp = sb.sb_chunk; cp; cp = cp->cdr)
    add (cp->contents, cp->used - cp->contents);
}

void
StrBuf::finish ()
{
  if (sb_finished)
    return;
  sb_finished = 1;
  if (sb_chunk)
    sb_chunk->used = sb_next;
  strbuf_chunk *cp = 0;
  while (sb_chunk)
    {
      strbuf_chunk *cdr = sb_chunk->cdr;
      sb_chunk->cdr = cp;
      cp = sb_chunk;
      sb_chunk = cdr;
    }
  sb_chunk = cp;
}

lisp
StrBuf::make_string ()
{
  finish ();
  int l = length ();
  if (!l)
    make_string_simple ("", 0);
  Char *b = (Char *)xmalloc (sizeof (Char) * l);
  copy (b);
  lisp string = make_simple_string ();
  xstring_contents (string) = b;
  xstring_length (string) = l;
  return string;
}

lisp
StrBuf::make_substring (int start, int end)
{
  finish ();
  int l = length ();
  assert (start >= 0);
  assert (end <= l);
  assert (start <= end);
  l = end - start;
  if (l <= 0)
    make_string_simple ("", 0);
  Char *p = (Char *)xmalloc (sizeof (Char) * l);
  lisp string = make_simple_string ();
  xstring_contents (string) = p;
  xstring_length (string) = l;
  for (const strbuf_chunk *cp = sb_chunk; cp; cp = cp->cdr)
    {
      int size = cp->used - cp->contents;
      if (size <= start)
        start -= size;
      else
        {
          size -= start;
          if (l > size)
            {
              bcopy (cp->contents + start, p, size);
              p += size;
              l -= size;
            }
          else
            {
              bcopy (cp->contents + start, p, l);
#ifdef DEBUG
              l = 0;
#endif
              break;
            }
          start = 0;
        }
    }
  assert (!l);
  return string;
}

void
StrBuf::dump (strbuf_chunk *cp) const
{
  if (!cp)
    return;
  dump (cp->cdr);
  Char *p = cp->contents;
  Char *pe = cp == sb_chunk ? sb_next : p + sb_chunk_size;
  for (; p < pe; p++)
    {
      if (*p >= 0x100)
        putchar (*p >> 8);
      putchar (*p);
    }
}

void
StrBuf::dump () const
{
  dump (sb_chunk);
}
