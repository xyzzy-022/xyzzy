#include "stdafx.h"
#include "ed.h"
#include "wstream.h"

void
wStream::add (const char *string)
{
  StrBuf::add (string);

  const u_char *s = (const u_char *)string;
  while (*s)
    {
      if (SJISP (*s))
        {
          if (!s[1])
            {
              col++;
              break;
            }
          col += 2;
          s += 2;
        }
      else
        update_column (*s++);
    }
}

void
wStream::case_conversion (wStream &src, int colon, int atsign)
{
  src.finish ();
  if (colon && atsign)
    {
      for (const strbuf_chunk *cp = src.sb_chunk; cp; cp = cp->cdr)
        for (const Char *p = cp->contents, *pe = cp->used; p < pe; p++)
          add (char_upcase (*p));
    }
  else if (colon)
    {
      int f = 1;
      for (const strbuf_chunk *cp = src.sb_chunk; cp; cp = cp->cdr)
        for (const Char *p = cp->contents, *pe = cp->used; p < pe; p++)
          {
            Char c = *p;
            if (alphanumericp (c))
              {
                if (f)
                  add (char_upcase (c));
                else
                  add (char_downcase (c));
                f = 0;
              }
            else
              {
                add (c);
                f = 1;
              }
          }
    }
  else if (atsign)
    {
      int f = 1;
      for (const strbuf_chunk *cp = src.sb_chunk; cp; cp = cp->cdr)
        for (const Char *p = cp->contents, *pe = cp->used; p < pe; p++)
          {
            if (f)
              add (char_upcase (*p));
            else
              add (char_downcase (*p));
            f = 0;
          }
    }
  else
    {
      for (const strbuf_chunk *cp = src.sb_chunk; cp; cp = cp->cdr)
        for (const Char *p = cp->contents, *pe = cp->used; p < pe; p++)
          add (char_downcase (*p));
    }
}

void
wStreamsStream::alloc ()
{
  assert (sb_limit == sb_chunk->contents + sb_chunk_size);
  sb_next = sb_chunk->contents;
  write_stream (dest, sb_chunk->contents, sb_chunk_size);
}
