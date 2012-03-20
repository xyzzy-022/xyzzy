#include <stdarg.h>
#include <stdio.h>
#include <windows.h>

#define BUF_SIZE 512

void
Debug(char *format, ...)
{
  va_list ap;
  char msg[BUF_SIZE];

  va_start (ap, format);
  vsprintf_s (msg, BUF_SIZE, format, ap);
  va_end (ap);

  char buf[BUF_SIZE * 2];
  sprintf_s (buf, "%s\n", msg);
  OutputDebugString (buf);
}
