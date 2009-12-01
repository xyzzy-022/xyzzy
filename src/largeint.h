#ifndef _LARGEINT_H_
# define _LARGEINT_H_

struct large_int
{
  long lo;
  long hi;
};

large_int __stdcall addsi (long, long);
large_int __stdcall subsi (long, long);
large_int __stdcall negsi (const large_int);
large_int __stdcall mulsi (long, long);
large_int __stdcall long_to_large_int (long);
large_int __stdcall long_to_large_int (unsigned long);
int __stdcall not_long (const large_int);

#endif
