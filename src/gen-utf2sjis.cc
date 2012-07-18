#include "gen-stdafx.h"

static const int internal[] =
  {0x005c, 0x007e, 0x815f, 0x8160, 0x8161, 0x817c, 0x8191, 0x8192, 0x81ca,};
static const int shiftjis[] =
  {0x00a5, 0x203e, 0x005c, 0x301c, 0x2016, 0x2212, 0x00a2, 0x00a3, 0x00ac,};
static const int cp932[] =
  {0x005c, 0x007e, 0xff3c, 0xff5e, 0x2225, 0xff0d, 0xffe0, 0xffe1, 0xffe2,};


static void
buildhash (const int *from, const int *to, int len, const char *name)
{
  char buf[65536];
  int wbuf[65536][2];
  for (int size = len; size < sizeof buf; size++)
    {
      memset (buf, 0, size);
      int i;
      for (i = 0; i < len; i++)
        {
          int n = from[i] % size;
          if (buf[n])
            break;
          buf[n] = 1;
          wbuf[n][0] = from[i];
          wbuf[n][1] = to[i];
        }
      if (i != len)
        continue;

      printf ("%s[] =\n{\n", name);
      for (i = 0; i < size; i++)
        if (buf[i])
          printf ("  {0x%04x, 0x%04x},\n", wbuf[i][0], wbuf[i][1]);
        else
          printf ("  {0xffff, 0xffff},\n");
      printf ("};\n\n");
      break;
    }
}

void
gen_utf2sjis (int argc, char **argv)
{
  buildhash (internal, shiftjis, numberof (internal),
             "static const struct {Char cc; ucs2_t wc;} utf_internal2shiftjis_hash");
  buildhash (shiftjis, internal, numberof (internal),
             "static const struct {ucs2_t wc; Char cc;} utf_shiftjis2internal_hash");
  exit (0);
}
