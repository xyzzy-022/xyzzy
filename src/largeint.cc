#include "largeint.h"

#ifdef _M_IX86

# pragma warning (disable:4035)

large_int __declspec (naked) __stdcall
addsi (long a, long b)
{
  __asm
    {
      push ebx;
      mov eax, [esp+12];   b
      cdq;
      mov ebx, eax;
      mov ecx, edx;
      mov eax, [esp+8];    a
      cdq;
      add eax, ebx;
      adc edx, ecx;
      pop ebx;
      ret 8;
    }
}

large_int __declspec (naked) __stdcall
subsi (long a, long b)
{
  __asm
    {
      push ebx;
      mov eax, [esp+12];   b
      cdq;
      mov ebx, eax;
      mov ecx, edx;
      mov eax, [esp+8];    a
      cdq;
      sub eax, ebx;
      sbb edx, ecx;
      pop ebx;
      ret 8;
    }
}

large_int __declspec (naked) __stdcall
negsi (const large_int r)
{
  __asm
    {
      mov edx, [esp+4]r.hi;
      mov eax, [esp+4]r.lo;
      neg edx;
      neg eax;
      sbb edx, 0;
      ret 8;
    }
}

large_int __declspec (naked) __stdcall
mulsi (long a, long b)
{
  __asm
    {
      mov eax, [esp+4];         a
      imul dword ptr [esp+8];   b
      ret 8;
    }
}

large_int __declspec (naked) __stdcall
long_to_large_int (long x)
{
  __asm
    {
      mov eax, [esp+4];   x
      cdq;
      ret 4;
    }
}

large_int __declspec (naked) __stdcall
long_to_large_int (unsigned long x)
{
  __asm
    {
      mov eax, [esp+4];   x
      xor edx, edx;
      ret 4;
    }
}

int __declspec (naked) __stdcall
not_long (const large_int r)
{
  __asm
    {
      mov eax, [esp+4]r.lo;
      cdq;
      mov eax, [esp+4]r.hi;
      sub eax, edx;
      ret 8;
    }
}

# pragma warning (default:4035)

#else /* not _M_IX86 */

large_int __stdcall
addsi (long a, long b)
{
  large_int r;
  r.lo = a + b;
  if (a >= 0 != b >= 0)
    r.hi = r.lo >= 0 ? 0 : -1;
  else
    r.hi = a >= 0 ? 0 : -1;
  return r;
}

large_int __stdcall
subsi (long a, long b)
{
  large_int r;
  r.lo = a - b;
  if (a >= 0 == b >= 0)
    r.hi = r.lo >= 0 ? 0 : -1;
  else
    r.hi = a >= 0 ? 0 : -1;
  return r;
}

large_int __stdcall
negsi (const large_int x)
{
  large_int r;
  r.lo = -x.lo;
  r.hi = -x.hi;
  if (r.lo < 0)
    r.hi--;
  return r;
}

typedef unsigned long u_long;

u_long
addui (u_long x, u_long y, u_long &overflow)
{
  u_long r = x + y;
  overflow = r < x;
  return r;
}

# include <limits.h>
# define BITS_PER_LONG (CHAR_BIT * sizeof (long))
# define BITS_PER_LONG_2 (BITS_PER_LONG / 2)
# define RADIX ((1 << BITS_PER_LONG_2) - 1)

large_int __stdcall
mului (u_long x, u_long y)
{
  large_int r;
  u_long overflow;
  u_long xlo = x & RADIX;
  u_long xhi = x >> BITS_PER_LONG_2;
  u_long ylo = y & RADIX;
  u_long yhi = y >> BITS_PER_LONG_2;
  u_long z = addui (xlo * yhi, xhi * ylo, overflow);
  r.hi = xhi * yhi + (overflow << BITS_PER_LONG_2) + (z >> BITS_PER_LONG_2);
  z = addui (xlo * ylo, z << BITS_PER_LONG_2, overflow);
  r.hi += overflow;
  r.lo = z;
  return r;
}

large_int __stdcall
mulsi (long x, long y)
{
  large_int r = mului (x >= 0 ? x : -x, y >= 0 ? y : -y);
  if (x >= 0 == y >= 0)
    return r;
  return negsi (r);
}

large_int __stdcall
long_to_large_int (long x)
{
  large_int r;
  r.lo = x;
  r.hi = r.lo >= 0 ? 0 : -1;
  return r;
}

large_int __stdcall
long_to_large_int (u_long x)
{
  large_int r;
  r.lo = x;
  r.hi = 0;
  return r;
}

int __stdcall
not_long (const large_int r)
{
  return r.hi - (r.lo >= 0 ? 0 : -1);
}

#endif /* not _M_IX86 */
