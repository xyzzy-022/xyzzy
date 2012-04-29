#include "stdafx.h"
#include "cdecl.h"

u_int
hashpjw (const Char *p, int size)
{
  u_int hash = 0;
  for (const Char *pe = p + size; p < pe; p++)
    {
      hash = (hash << 4) + *p;
      u_int g = hash & 0xf0000000;
      if (g)
        {
          hash ^= g >> 24;
          hash ^= g;
        }
    }
  return hash;
}

#ifndef NO_NEED_IHASHPJW
#include "ed.h"

u_int
ihashpjw (const Char *p, int size)
{
  u_int hash = 0;
  for (const Char *pe = p + size; p < pe; p++)
    {
      hash = (hash << 4) + char_downcase (*p);
      u_int g = hash & 0xf0000000;
      if (g)
        {
          hash ^= g >> 24;
          hash ^= g;
        }
    }
  return hash;
}

u_int
ihashpjw (const lisp *p, const lisp *pe)
{
  u_int hash = 0;
  for (; p < pe; p++)
    {
      hash = (hash << 4) + char_downcase (xchar_code (*p));
      u_int g = hash & 0xf0000000;
      if (g)
        {
          hash ^= g >> 24;
          hash ^= g;
        }
    }
  return hash;
}

#endif /* NO_NEED_IHASHPJW */
