#include "gen-stdafx.h"

#define BIG5_TABSIZE (157 * 88)
//#define RUNTIME_TEST_CNS_TABLE
//#define RUNTIME_TEST_DIFF_TABLE

static void
invalid (const char *file, int linenum)
{
  fprintf (stderr, "%s: %d: Invalid format\n", file, linenum);
  exit (2);
}

static int
parse_line (char *b, int &mb, ucs2_t &wc, const char *file, int linenum, int undef_ok = 0)
{
  int l = strlen (b);
  if (l && b[l - 1] == '\n')
    b[l - 1] = 0;
  if (!*b || *b == '#')
    return 0;

  char *b1 = strchr (b, '\t');
  if (!b1)
    invalid (file, linenum);
  *b1++ = 0;
  char *b2 = strchr (b1, '\t');
  if (!b2)
    b2 = b1 + strlen (b1);
  if (!b2)
    invalid (file, linenum);
  *b2 = 0;
  if (b[0] == '0' && b[1] == 'x')
    mb = strtol (b + 2, 0, 16);
  else
    invalid (file, linenum);
  if (b1[0] == '0' && b1[1] == 'x')
    wc = static_cast<ucs2_t> (strtol (b1 + 2, 0, 16));
  else if (undef_ok)
    wc = static_cast<ucs2_t> (-1);
  else
    invalid (file, linenum);
  return 1;
}

static void
clear (ucs2_t *w, int size)
{
  for (int i = 0; i < size; i++)
    w[i] = ucs2_t (-1);
}

static void
print (const ucs2_t *wbuf, int size)
{
  for (int i = 0; i < size; i += 8)
    {
      putchar (' ');
      for (int j = 0; j < min (8, size - i); j++)
        printf (" 0x%04x,", wbuf[i + j]);
      putchar ('\n');
    }
}

static void
makehash (const char *name, const ucs2_t *const wc)
{
  ucs2_t f[65536];
  for (int size = 128; size < 65536; size++)
    {
      memset (f, 0, sizeof *f * size);
      int i;
      for (i = 0; i < 128; i++)
        if (wc[i] != 0xffff)
          {
            int n = wc[i] % size;
            if (f[n])
              break;
            f[n] = wc[i];
          }
      if (i == 128)
        {
          printf ("#define ");
          for (; *name; name++)
            putchar (toupper (*name));
          printf ("_HASHSIZE %d\n", size);
          return;
        }
    }
  fprintf (stderr, "%s: makehash failed\n", name);
  exit (2);
}

static void
read_iso8859 (ucs2_t *wbuf, const char *file, const char *name, const ucs2_t *wincp)
{
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  clear (wbuf, 128);

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum))
        {
          if (mb >= 256)
            invalid (file, linenum);
          if (mb >= 0x80)
            wbuf[mb - 0x80] = wc;
        }
    }
  int i;
  if (wincp)
    for (i = 0; i < 0x20; i++)
      if (wincp[i] != ucs2_t (-1))
        {
		  int j;
          for (j = 32; j < 128; j++)
            if (wbuf[j] == wincp[i])
              break;
          if (j == 128)
            wbuf[i] = wincp[i];
        }

  fclose (fp);

  makehash (name, wbuf);
}

static void
read_wincp (ucs2_t *wbuf, const char *file, const char *name)
{
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  clear (wbuf, 128);

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum, 1))
        {
          if (mb >= 256)
            invalid (file, linenum);
          if (mb >= 0x80)
            wbuf[mb - 0x80] = wc;
        }
    }

  fclose (fp);

  makehash (name, wbuf);
}

static void
wincp2int (const ucs2_t *wincp, ucs2_t *wbuf, ucs2_t *iso8859, int charset)
{
  for (int i = 0; i < 128; i++)
    if (wincp[i] != ucs2_t (-1) && wbuf[i] == ucs2_t (-1))
      for (int j = 0; j < 128; j++)
        if (iso8859[j] == wincp[i])
          {
            wbuf[i] = j | charset;
            break;
          }
}

#define DIFF_ENABLE 1
#define DIFF_IINCR 2
#define DIFF_WINCR 4
#define DIFF_WSAME 8

static void
encode_diff (const char *name, const Char *const diff, int dsize)
{
  char f[65536];
  int fsize = dsize / 2;
  memset (f, DIFF_ENABLE, fsize);

  int i;
  for (i = 4; i < dsize; i += 2)
    {
      if (diff[i] == diff[i - 2] + 1
          && diff[i - 2] == diff[i - 4] + 1)
        {
          f[i / 2] |= DIFF_IINCR;
          f[i / 2 - 1] |= DIFF_IINCR;
          f[i / 2 - 2] |= DIFF_IINCR;
        }

      if (diff[i + 1] == diff[i - 1] + 1
          && diff[i - 1] == diff[i - 3] + 1)
        {
          f[i / 2] |= DIFF_WINCR;
          f[i / 2 - 1] |= DIFF_WINCR;
          f[i / 2 - 2] |= DIFF_WINCR;
        }

      if (diff[i + 1] == diff[i - 1]
          && diff[i - 1] == diff[i - 3])
        {
          f[i / 2] |= DIFF_WSAME;
          f[i / 2 - 1] |= DIFF_WSAME;
          f[i / 2 - 2] |= DIFF_WSAME;
        }
    }

  int exists;

  printf ("static const Char %s_diff_both_incr[] = {\n", name);
  for (i = 0, exists = 0; i < fsize; i++)
    if ((f[i] & (DIFF_IINCR | DIFF_WINCR)) == (DIFF_IINCR | DIFF_WINCR))
      {
		int j;
        for (j = i + 1; j < fsize; j++)
          if ((f[j] & (DIFF_IINCR | DIFF_WINCR)) != (DIFF_IINCR | DIFF_WINCR)
              || diff[j * 2] != diff[j * 2 - 2] + 1
              || diff[j * 2 + 1] != diff[j * 2 - 1] + 1)
            break;
        if (j - i >= 3)
          {
            printf ("  %d, 0x%04x, 0x%04x,\n", j - i, diff[i * 2], diff[i * 2 + 1]);
            while (--j >= i)
              f[j] = 0;
            exists++;
          }
      }
  if (!exists)
    printf ("  0,\n");
  printf ("};\n\n");

  printf ("static const Char %s_diff_wsame[] = {\n", name);
  for (i = 0, exists = 0; i < fsize; i++)
    if ((f[i] & (DIFF_IINCR | DIFF_WSAME)) == (DIFF_IINCR | DIFF_WSAME))
      {
		int j;
        for (j = i + 1; j < fsize; j++)
          if ((f[j] & (DIFF_IINCR | DIFF_WSAME)) != (DIFF_IINCR | DIFF_WSAME)
              || diff[j * 2] != diff[j * 2 - 2] + 1
              || diff[j * 2 + 1] != diff[j * 2 - 1])
            break;
        if (j - i >= 3)
          {
            printf ("  %d, 0x%04x, 0x%04x,\n", j - i, diff[i * 2], diff[i * 2 + 1]);
            while (--j >= i)
              f[j] = 0;
            exists++;
          }
      }
  if (!exists)
    printf ("  0,\n");
  printf ("};\n\n");

  printf ("static const Char %s_diff_iincr[] = {\n", name);
  for (i = 0, exists = 0; i < fsize; i++)
    if (f[i] & DIFF_IINCR)
      {
		int j;
        for (j = i + 1; j < fsize; j++)
          if (!(f[j] & DIFF_IINCR)
              || diff[j * 2] != diff[j * 2 - 2] + 1)
            break;
        if (j - i >= 3)
          {
            printf ("  %d, 0x%04x,", j - i, diff[i * 2]);
            for (int k = i, w = 0; k < j; k++, w++)
              {
                if (!(w % 8))
                  printf ("\n    ");
                printf (" 0x%04x,", diff[k * 2 + 1]);
                f[k] = 0;
              }
            printf ("\n");
            exists++;
          }
      }
  if (!exists)
    printf ("  0,\n");
  printf ("};\n\n");

  printf ("static const Char %s_diff_wincr[] = {\n", name);
  for (i = 0, exists = 0; i < fsize; i++)
    if (f[i] & DIFF_WINCR)
      {
		int j;
        for (j = i + 1; j < fsize; j++)
          if (!(f[j] & DIFF_WINCR)
              || diff[j * 2 + 1] != diff[j * 2 - 1] + 1)
            break;
        if (j - i >= 3)
          {
            printf ("  %d, 0x%04x,", j - i, diff[i * 2 + 1]);
            for (int k = i, w = 0; k < j; k++, w++)
              {
                if (!(w % 8))
                  printf ("\n    ");
                printf (" 0x%04x,", diff[k * 2]);
                f[k] = 0;
              }
            printf ("\n");
            exists++;
          }
      }
  if (!exists)
    printf ("  0,\n");
  printf ("};\n\n");

  printf ("static const Char %s_diff_rest[] = {", name);
  int w = 0;
  for (int i = 0; i < fsize; i++)
    if (f[i])
      {
        if (!(w % 4))
          printf ("\n ");
        printf (" 0x%04x, 0x%04x,", diff[i * 2], diff[i * 2 + 1]);
        w++;
      }
  printf ("\n};\n\n");
}

static void
output_diff (const Char *wc2int, const ucs2_t *int2wc, const char *name)
{
  Char wc2int_2[65536];

  clear (wc2int_2, 65536);
  int i;
  for (i = 0; i < 65536; i++)
    if (int2wc[i] != ucs2_t (-1))
      wc2int_2[int2wc[i]] = i;

  Char diff[65536 * 2];
  int n = 0;
  for (int i = 0; i < 65536; i++)
    if (wc2int[i] != Char (-1) && wc2int[i] != wc2int_2[i])
      {
        diff[n++] = i;
        diff[n++] = wc2int[i];
      }

  encode_diff (name, diff, n);

#ifdef RUNTIME_TEST_DIFF_TABLE
  printf ("#ifndef RUNTIME_TEST_DIFF_TABLE\n");
  printf ("#define RUNTIME_TEST_DIFF_TABLE\n");
  printf ("#endif\n\n");
  printf ("static const Char %s_diff[] =\n{\n", name);
  print (diff, n);
  printf ("};\n\n");
#endif
}

static void
read_94x94 (ucs2_t *wbuf, const char *file, int ignore_errors = 0)
{
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  clear (wbuf, 94 * 94);

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum))
        {
          int c1 = mb / 256;
          int c2 = mb % 256;
          if (c1 >= 0x21 && c1 <= 0x7e && c2 >= 0x21 && c2 <= 0x7e)
            wbuf[(c1 - 0x21) * 94 + c2 - 0x21] = wc;
          else if (c1 >= 0xa1 && c1 <= 0xfe && c2 >= 0xa1 && c2 <= 0xfe)
            wbuf[(c1 - 0xa1) * 94 + c2 - 0xa1] = wc;
          else if (!ignore_errors)
            invalid (file, linenum);
        }
    }

  fclose (fp);
}

static void
read_jisx0212 (ucs2_t *wbuf)
{
  read_94x94 (wbuf, "unicode/JIS0212.TXT");

  const char *const file = "unicode/eucJP-ibmext.txt";
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum))
        {
          mb -= 0x8f0000;
          int c1 = mb / 256;
          int c2 = mb % 256;
          if (c1 >= 0xa1 && c1 <= 0xfe && c2 >= 0xa1 && c2 <= 0xfe)
            wbuf[(c1 - 0xa1) * 94 + c2 - 0xa1] = wc;
          else
            invalid (file, linenum);
        }
    }

  fclose (fp);
}

static void
make_wc2cp950 (ucs2_t *const wc2cp950)
{
  for (int i = 0; i < 65536; i++)
    {
      ucs2_t wc = i;
      char mb[3];
      BOOL f;
      int n = WideCharToMultiByte (CP_CN_TRADITIONAL, 0, (LPCWSTR)&wc, 1, mb, 3, 0, &f);
      if (f)
        wc2cp950[i] = ucs2_t (-1);
      else
        switch (n)
          {
          case 1:
            wc2cp950[i] = mb[0] & 255;
            break;

          case 2:
            {
              int c1 = mb[0] & 255, c2 = mb[1] & 255;
              if ((c1 >= 0xa1 && c1 <= 0xc7
                   || c1 >= 0xc9 && c1 <= 0xf9)
                  && (c2 >= 0x40 && c2 <= 0x7e
                      || c2 >= 0xa1 && c2 <= 0xfe))
                wc2cp950[i] = big5_to_int (c1, c2);
              else
                wc2cp950[i] = ucs2_t (-1);
              break;
            }

          default:
            wc2cp950[i] = ucs2_t (-1);
            break;
          }
    }
}

static void
read_big5 (ucs2_t *wbuf)
{
  const char *const file = "unicode/BIG5.TXT";
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  clear (wbuf, BIG5_TABSIZE);

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum))
        {
          int c1 = mb / 256;
          int c2 = mb % 256;
          if (!big5_lead_p (c1) || !big5_trail_p (c2))
            invalid (file, linenum);
          Char cc = big5_to_int (c1, c2);
          wbuf[cc - CCS_BIG5_MIN] = wc;
        }
    }

  fclose (fp);

  wbuf[big5_to_int (0xa1, 0x5a) - CCS_BIG5_MIN] = wbuf[big5_to_int (0xa1, 0xc4) - CCS_BIG5_MIN];
  wbuf[big5_to_int (0xa1, 0xfe) - CCS_BIG5_MIN] = wbuf[big5_to_int (0xa2, 0xac) - CCS_BIG5_MIN];
  wbuf[big5_to_int (0xa2, 0x40) - CCS_BIG5_MIN] = wbuf[big5_to_int (0xa2, 0xad) - CCS_BIG5_MIN];
  wbuf[big5_to_int (0xa2, 0xcc) - CCS_BIG5_MIN] = wbuf[big5_to_int (0xa4, 0x51) - CCS_BIG5_MIN];
  wbuf[big5_to_int (0xa2, 0xce) - CCS_BIG5_MIN] = wbuf[big5_to_int (0xa4, 0xca) - CCS_BIG5_MIN];
  wbuf[big5_to_int (0xa1, 0xc3) - CCS_BIG5_MIN] = 0xffe3;
  wbuf[big5_to_int (0xa1, 0xc5) - CCS_BIG5_MIN] = 0x02cd;

  ucs2_t wc2cp950[65536];
  make_wc2cp950 (wc2cp950);
  ucs2_t wbuf2[65536];
  clear (wbuf2, 65536);
  int i;
  for (i = 0; i < BIG5_TABSIZE; i++)
    wbuf2[i + CCS_BIG5_MIN] = wbuf[i];
  for (int i = 0; i < 0x80; i++)
    wbuf2[i] = i;
  output_diff (wc2cp950, wbuf2, "wc2big5");
}

static void
make_wc2cp949 (ucs2_t *const wc2cp949)
{
  for (int i = 0; i < 65536; i++)
    {
      ucs2_t wc = i;
      char mb[3];
      BOOL f;
      int n = WideCharToMultiByte (CP_KOREAN, 0,(LPCWSTR)&wc, 1, mb, 3, 0, &f);
      if (f)
        wc2cp949[i] = ucs2_t (-1);
      else
        switch (n)
          {
          case 1:
            wc2cp949[i] = mb[0] & 255;
            break;

          case 2:
            {
              int c1 = mb[0] & 255, c2 = mb[1] & 255;
              if (c1 >= 0xa1 && c1 <= 0xfe
                  && c2 >= 0xa1 && c2 <= 0xfe)
                wc2cp949[i] = ksc5601_to_int (c1 & 127, c2 & 127);
              else
                wc2cp949[i] = ucs2_t (-1);
              break;
            }

          default:
            wc2cp949[i] = ucs2_t (-1);
            break;
          }
    }
}

static void
read_ksc5601 (ucs2_t *wbuf)
{
  ucs2_t wc2cp949[65536];
  ucs2_t wbuf2[65536];
  clear (wbuf2, 65536);
  read_94x94 (wbuf, "unicode/KSC5601.TXT", 1);
  make_wc2cp949 (wc2cp949);
  int i;
  for (i = 0; i < 94 * 94; i++)
    wbuf2[i + CCS_KSC5601_MIN] = wbuf[i];
  for (int i = 0; i < 0x80; i++)
    wbuf2[i] = i;
  output_diff (wc2cp949, wbuf2, "wc2ksc5601");
}

static void
make_wc2cp936 (ucs2_t *const wc2cp936)
{
  for (int i = 0; i < 65536; i++)
    {
      ucs2_t wc = i;
      char mb[3];
      BOOL f;
      int n = WideCharToMultiByte (CP_CN_SIMPLIFIED, 0, (LPCWSTR)&wc, 1, mb, 3, 0, &f);
      if (f)
        wc2cp936[i] = ucs2_t (-1);
      else
        switch (n)
          {
          case 1:
            wc2cp936[i] = mb[0] & 255;
            break;

          case 2:
            {
              int c1 = mb[0] & 255, c2 = mb[1] & 255;
              if (c1 >= 0xa1 && c1 <= 0xfe
                  && c2 >= 0xa1 && c2 <= 0xfe)
                wc2cp936[i] = gb2312_to_int (c1 & 127, c2 & 127);
              else
                wc2cp936[i] = ucs2_t (-1);
              break;
            }

          default:
            wc2cp936[i] = ucs2_t (-1);
            break;
          }
    }
}

static void
read_gb2312 (ucs2_t *wbuf)
{
  ucs2_t wc2cp936[65536];
  ucs2_t wbuf2[65536];
  clear (wbuf2, 65536);
  read_94x94 (wbuf, "unicode/GB2312.TXT");
  make_wc2cp936 (wc2cp936);
  int i;
  for (i = 0; i < 94 * 94; i++)
    wbuf2[i + CCS_GB2312_MIN] = wbuf[i];
  for (int i = 0; i < 0x80; i++)
    wbuf2[i] = i;
  output_diff (wc2cp936, wbuf2, "wc2gb2312");
}

struct cns_table {Char c; u_short i;};
#define CNS_NIL_THRESHOLD 0x2000

static int
test_cns_table (Char *buf, Char cc, const cns_table *cnstab, int csize)
{
  int i = 0;
  for (const cns_table *cp = cnstab, *const ce = cnstab + csize; cp < ce; cp++)
    {
      int e = cp->i;
      for (; i < e; i++, cc++)
        buf[i] = cc;
      if (cp->c >= CNS_NIL_THRESHOLD)
        {
          buf[i++] = cp->c;
          cc = cp->c + 1;
        }
      else
        {
          for (e = i + cp->c; i < e; i++)
            buf[i] = Char (-1);
        }
    }
  return i;
}

static void
make_cns_table (const char *name, const Char *p, int size)
{
  cns_table cnstab[0x10000], *cp = cnstab;
  const Char *const pb = p, *const pe = p + size;
  for (Char cc = *p++; p < pe; p++)
    if (*p == cc + 1)
      cc = *p;
    else
      {
        cp->i = p - pb;
        if (*p == Char (-1))
          {
			int n;
            for (n = 0; p < pe && *p == Char (-1); p++, n++)
              ;
            if (n >= CNS_NIL_THRESHOLD)
              {
                fprintf (stderr, "%s: too many nil chars\n", name);
                exit (2);
              }
            cp->c = Char (n);
            p--;
          }
        else
          {
            if (*p < CNS_NIL_THRESHOLD)
              {
                fprintf (stderr, "%s: too small char code: %04x\n", name, *p);
                exit (2);
              }
            cp->c = *p;
            cc = *p;
          }
        cp++;
      }

  Char tem[0x10000];
  if (test_cns_table (tem, *pb, cnstab, cp - cnstab) != size
      || memcmp (tem, pb, sizeof *pb * size))
    {
      fprintf (stderr, "%s: build cns_table failed\n", name);
      exit (2);
    }

  printf ("#define ");
  int i;
  for (i = 0; name[i]; i++)
    putchar (toupper (name[i]));
  printf ("_START_CHAR 0x%04x\n\n", *pb);

  printf ("static const cns_table %s_tab[] =\n{", name);
  for (int i = 0; i < cp - cnstab; i++)
    {
      if (!(i % 4))
        printf ("\n ");
      printf (" {0x%04x, 0x%04x},", cnstab[i].c, cnstab[i].i);
    }
  printf ("\n};\n\n");
}


static void
make_cns11643 (const ucs2_t *const big5, const ucs2_t *const gb2312)
{
  ucs2_t cns2wc1[94 * 94], cns2wc2[94 * 94];

  const char *const file = "unicode/CNS11643.TXT";
  FILE *fp = fopen (file, "r");
  if (!fp)
    {
      fprintf (stderr, "%s: %s\n", file, strerror (errno));
      exit (2);
    }

  clear (cns2wc1, 94 * 94);
  clear (cns2wc2, 94 * 94);

  char b[1024];
  for (int linenum = 1; fgets (b, sizeof b, fp); linenum++)
    {
      int mb;
      ucs2_t wc;
      if (parse_line (b, mb, wc, file, linenum))
        {
          int plane = mb / 0x10000;
          int c1 = mb / 256 % 256;
          int c2 = mb % 256;
          if (c1 >= 0x21 && c1 <= 0x7e && c2 >= 0x21 && c2 <= 0x7e)
            {
              if (plane == 1)
                cns2wc1[(c1 - 0x21) * 94 + c2 - 0x21] = wc;
              else if (plane == 2)
                cns2wc2[(c1 - 0x21) * 94 + c2 - 0x21] = wc;
              else if (plane == 14)
                ;
              else
                invalid (file, linenum);
            }
          else
            invalid (file, linenum);
        }
    }

  fclose (fp);

  Char wc2int[65536];
  clear (wc2int, numberof (wc2int));

  int i;
  for (i = 0; i < 94 * 94; i++)
    if (gb2312[i] != ucs2_t (-1))
      wc2int[gb2312[i]] = i + CCS_GB2312_MIN;
  for (int i = 0; i < BIG5_TABSIZE; i++)
    if (big5[i] != ucs2_t (-1))
      wc2int[big5[i]] = i + CCS_BIG5_MIN;

  Char cns2int1[94 * 94], cns2int2[94 * 94];
  for (int i = 0; i < 94 * 94; i++)
    {
      if (cns2wc1[i] != ucs2_t (-1))
        cns2int1[i] = wc2int[cns2wc1[i]];
      else
        cns2int1[i] = Char (-1);
      if (cns2wc2[i] != ucs2_t (-1))
        cns2int2[i] = wc2int[cns2wc2[i]];
      else
        cns2int2[i] = Char (-1);
    }

  printf ("struct cns_table {Char c; u_short i;};\n");
  printf ("#define CNS_NIL_THRESHOLD %d\n\n", CNS_NIL_THRESHOLD);

  make_cns_table ("cns11643_1", cns2int1, 94 * 94);
  make_cns_table ("cns11643_2", cns2int2, 94 * 94);

#ifdef RUNTIME_TEST_CNS_TABLE
  printf ("#define RUNTIME_TEST_CNS_TABLE\n\n");
  printf ("static const Char test_cns11643_1_to_internal[] =\n{\n");
  print (cns2int1, 94 * 94);
  printf ("};\n\n");
  printf ("static const Char test_cns11643_2_to_internal[] =\n{\n");
  print (cns2int2, 94 * 94);
  printf ("};\n\n");
#endif /* RUNTIME_TEST_CNS_TABLE */

  clear (wc2int, numberof (wc2int));
  for (int i = 0; i < 94 * 94; i++)
    if (gb2312[i] != ucs2_t (-1))
      wc2int[gb2312[i]] = i / 94 * 256 + i % 94 + 0x2121 + BIG5CNS_GB2312;
  for (int i = 0; i < 94 * 94; i++)
    if (cns2wc2[i] != ucs2_t (-1))
      wc2int[cns2wc2[i]] = i / 94 * 256 + i % 94 + 0x2121 + BIG5CNS_CNS11643_2;
  for (int i = 0; i < 94 * 94; i++)
    if (cns2wc1[i] != ucs2_t (-1))
      wc2int[cns2wc1[i]] = i / 94 * 256 + i % 94 + 0x2121 + BIG5CNS_CNS11643_1;

  ucs2_t big5cns[BIG5_TABSIZE];
  clear (big5cns, numberof (big5cns));
  for (int i = 0; i < BIG5_TABSIZE; i++)
    big5cns[i] = wc2int[big5[i]];

  make_cns_table ("big5cns", big5cns, BIG5_TABSIZE);

#ifdef RUNTIME_TEST_CNS_TABLE
  printf ("static const Char test_big5cns_table[] =\n{\n");
  print (big5cns, BIG5_TABSIZE);
  printf ("};\n\n");
#endif /* RUNTIME_TEST_CNS_TABLE */
}

static void
make_wc2cp932 (ucs2_t *const wc2cp932)
{
  int i;
  for (i = 0; i < 65536; i++)
    {
      ucs2_t wc = i;
      char mb[3];
      BOOL f;
      int n = WideCharToMultiByte (CP_JAPANESE, 0, (LPCWSTR) &wc, 1, mb, 3, 0, &f);
      if (f)
        wc2cp932[i] = ucs2_t (-1);
      else
        switch (n)
          {
          case 1:
            wc2cp932[i] = mb[0] & 255;
            break;

          case 2:
            wc2cp932[i] = ((mb[0] & 255) << 8) | (mb[1] & 255);
            break;

          default:
            wc2cp932[i] = ucs2_t (-1);
            break;
          }
    }

  for (int i = CCS_UTF16_SURROGATE_HIGH_MIN; i <= CCS_UTF16_SURROGATE_LOW_MAX; i++)
    wc2cp932[i] = ucs2_t (i);
}

static void
make_cp932 (ucs2_t *wbuf)
{
  char mb[2];
  ucs2_t wc;
  int n;

  clear (wbuf, 65536);

  int i;
  for (i = 0; i <= 0xff; i++)
    {
      mb[0] = i;
      wc = 0;
      n = MultiByteToWideChar (CP_JAPANESE, 0/*MB_ERR_INVALID_CHARS*/, mb, 1, (LPWSTR) &wc, 1);
      if (n == 1 && (!i || wc))
        wbuf[i] = wc;
    }

  for (int i = 0x8100; i <= 0x9fff; i++)
    {
      mb[0] = i >> 8;
      mb[1] = i;
      n = MultiByteToWideChar (CP_JAPANESE, MB_ERR_INVALID_CHARS, mb, 2, (LPWSTR) &wc, 1);
      if (n == 1)
        wbuf[i] = wc;
    }

  for (int i = 0xe000; i <= 0xfcff; i++)
    {
      mb[0] = i >> 8;
      mb[1] = i;
      n = MultiByteToWideChar (CP_JAPANESE, MB_ERR_INVALID_CHARS, mb, 2, (LPWSTR) &wc, 1);
      if (n == 1)
        wbuf[i] = wc;
    }

  for (int i = CCS_UTF16_SURROGATE_HIGH_MIN; i <= CCS_UTF16_SURROGATE_LOW_MAX; i++)
    wbuf[i] = ucs2_t (i);

  ucs2_t wc2cp932[65536];
  make_wc2cp932 (wc2cp932);
  output_diff (wc2cp932, wbuf, "wc2cp932");
}

static void
merge_int2wc (ucs2_t *d, const ucs2_t *s, int size, int offset)
{
  for (int i = 0; i < size; i++)
    d[i + offset] = s[i];
}

static void
merge_unicode (ucs2_t *d)
{
  int i;
  /* Basic Georgian & Georgian Extended */
  for (i = CCS_GEORGIAN_MIN; i <= CCS_GEORGIAN_MAX; i++)
    d[i] = i + (UNICODE_GEORGIAN_MIN - CCS_GEORGIAN_MIN);
  /* IPA Extensions */
  for (int i = CCS_IPA_MIN; i <= CCS_IPA_MAX; i++)
    d[i] = i + (UNICODE_IPA_MIN - CCS_IPA_MIN);
  /* Spacing Modifier Letters & Combining Diacritical Marks */
  for (int i = CCS_SMLCDM_MIN; i <= CCS_SMLCDM_MAX; i++)
    d[i] = i + (UNICODE_SMLCDM_MIN - CCS_SMLCDM_MIN);
}

#if defined (CCS_UJP_MIN) || defined (CCS_ULATIN_MIN)
#include "fontrange.h"
#endif

#ifdef CCS_ULATIN_MIN
static int
init_ulatin1 (ucs2_t *ulatin, const u_char *f, const u_char *r,
              ucs2_t from, ucs2_t to, int nchars)
{
  for (ucs2_t c = from; c <= to; c++)
    if (!(f[c / 8] & (1 << c % 8)) && (!r || r[c / 8] & (1 << c % 8)))
      {
        if (nchars <= CCS_ULATIN_MAX - CCS_ULATIN_MIN)
          ulatin[nchars] = c;
        nchars++;
      }
  return nchars;
}

static void
init_ulatin (ucs2_t *ulatin, ucs2_t (*iso8859)[128], int n)
{
  u_char f[65536 / 8], r[65536 / 8];
  memset (f, 0, sizeof f);
  int i;
  for (i = 0; i < n; i++)
    for (int j = 0; j < 128; j++)
      if (iso8859[i][j] != ucs2_t (-1))
        f[iso8859[i][j] / 8] |= 1 << iso8859[i][j] % 8;

  memset (r, 0, sizeof r);
  for (int i = 0; i < numberof (courier_new_range); i++)
    if (courier_new_range[i].w == 8)
      r[courier_new_range[i].c / 8] |= 1 << courier_new_range[i].c % 8;

  int nchars = 0;
  nchars = init_ulatin1 (ulatin, f, r, 0x0080, 0x024f, nchars);
  nchars = init_ulatin1 (ulatin, f, r, 0x0370, 0x04ff, nchars);
  nchars = init_ulatin1 (ulatin, f, r, 0x1e00, 0x214f, nchars);
  nchars = init_ulatin1 (ulatin, f, r, 0x2190, 0x26ff, nchars);
  nchars = init_ulatin1 (ulatin, f, 0, 0xfb01, 0xfb02, nchars);
  if (nchars > CCS_ULATIN_MAX - CCS_ULATIN_MIN)
    {
      fprintf (stderr, "Too many ulatin chars: %04x\n", nchars);
      exit (2);
    }

  for (; nchars <= CCS_ULATIN_MAX - CCS_ULATIN_MIN; nchars++)
    ulatin[nchars] = ucs2_t (-1);
}
#endif /* CCS_ULATIN_MIN */

#ifdef CCS_UJP_MIN
static int
init_ujp1 (ucs2_t *ujp, const u_char *f, const u_char *r,
           ucs2_t from, ucs2_t to, int nchars, int range)
{
  for (ucs2_t c = from; c <= to; c++)
    if (!(f[c / 8] & (1 << c % 8)) && r[c / 8] & (1 << c % 8))
      {
        if (nchars <= range)
          ujp[nchars] = c;
        nchars++;
      }
  return nchars;
}

static void
init_ujp (ucs2_t *int2wc, const u_char *f, const u_char *r,
          const char *name, int min, int max)
{
  int range = max - min;
  ucs2_t *const ujp = int2wc + min;
  int nchars = 0;
  nchars = init_ujp1 (ujp, f, r, 0x0080, 0x024f, nchars, range);
  nchars = init_ujp1 (ujp, f, r, 0x0370, 0x04ff, nchars, range);
  nchars = init_ujp1 (ujp, f, r, 0x1e00, 0x27ff, nchars, range);
  nchars = init_ujp1 (ujp, f, r, 0x3000, 0x30ff, nchars, range);
  nchars = init_ujp1 (ujp, f, r, 0x3200, 0x33ff, nchars, range);
  if (nchars > range)
    {
      fprintf (stderr, "Too many ujp %s chars: %04x\n", name, nchars);
      exit (2);
    }
  //fprintf (stderr, "%s %d chars\n", name, nchars);
  for (; nchars <= range; nchars++)
    ujp[nchars] = ucs2_t (-1);
}

static void
init_ujp (ucs2_t *int2wc)
{
  u_char f[65536 / 8], r[65536 / 8];
  memset (f, 0, sizeof f);
  int i;
  for (i = 0; i < 65536; i++)
    if (int2wc[i] != ucs2_t (-1)
        && !(i >= CCS_KSC5601_MIN && i <= CCS_KSC5601_MAX)
        && !(i >= CCS_GB2312_MIN && i <= CCS_GB2312_MAX)
        && !(i >= CCS_BIG5_MIN && i <= CCS_BIG5_MAX))
      f[int2wc[i] / 8] |= 1 << int2wc[i] % 8;

  memset (r, 0, sizeof r);
  for (int i = 0; i < numberof (ms_gothic_range); i++)
    if (ms_gothic_range[i].w == 8)
      r[ms_gothic_range[i].c / 8] |= 1 << ms_gothic_range[i].c % 8;

  init_ujp (int2wc, f, r, "half", CCS_UJP_HALF_MIN, CCS_UJP_HALF_MAX);

  for (int i = 0; i < 65536; i++)
    if (int2wc[i] != ucs2_t (-1))
      f[int2wc[i] / 8] |= 1 << int2wc[i] % 8;

  memset (r, 0, sizeof r);
  for (int i = 0; i < numberof (ms_gothic_range); i++)
    if (ms_gothic_range[i].w != 8)
      r[ms_gothic_range[i].c / 8] |= 1 << ms_gothic_range[i].c % 8;

  init_ujp (int2wc, f, r, "full", CCS_UJP_FULL_MIN, CCS_UJP_FULL_MAX);
}
#endif /* CCS_UJP_MIN */

static void
output_simple (const ucs2_t *wbuf, const char *type, const char *name)
{
  printf ("%s %s[] =\n{\n", type, name);
  print (wbuf, 65536);
  printf ("};\n\n");
}

void
gen_ucs2tab (int argc, char **argv)
{
  static const struct {const char *file, *name; int charset;} cs[] =
    {
      {"unicode/8859-1.TXT", "iso8859_1", ccs_iso8859_1},
      {"unicode/8859-2.TXT", "iso8859_2", ccs_iso8859_2},
      {"unicode/8859-3.TXT", "iso8859_3", ccs_iso8859_3},
      {"unicode/8859-4.TXT", "iso8859_4", ccs_iso8859_4},
      {"unicode/8859-5.TXT", "iso8859_5", ccs_iso8859_5},
      {"unicode/8859-7.TXT", "iso8859_7", ccs_iso8859_7},
      {"unicode/8859-9.TXT", "iso8859_9", ccs_iso8859_9},
      {"unicode/8859-10.TXT", "iso8859_10", ccs_iso8859_10},
      {"unicode/8859-13.TXT", "iso8859_13", ccs_iso8859_13},
    };
  static const struct {const char *file, *name; int base, f;} wcp[] =
    {
      {"unicode/CP1252.TXT", "windows_latin1", ccs_iso8859_1, 1},
      {"unicode/CP1250.TXT", "windows_latin2", ccs_iso8859_2, 1},
      {"unicode/CP1251.TXT", "windows_cyrillic", ccs_iso8859_5, 1},
      {"unicode/CP1253.TXT", "windows_greek", ccs_iso8859_7, 1},
      {"unicode/CP1254.TXT", "windows_turkish", ccs_iso8859_9, 1},
      {"unicode/CP1257.TXT", "windows_baltic", ccs_iso8859_13, 1},
      {"unicode/KOI8-R.TXT", "koi8r", ccs_iso8859_5, 0},
      {"unicode/KOI8-U.TXT", "koi8u", ccs_iso8859_5, 0},
    };

  ucs2_t iso8859[numberof (cs)][128];
  ucs2_t wincp[numberof (wcp)][128];
  ucs2_t jisx0212[94 * 94];
  ucs2_t ksc5601[94 * 94];
  ucs2_t gb2312[94 * 94];
  ucs2_t big5[BIG5_TABSIZE];
  ucs2_t int2wc[65536];

  int i;
  for (i = 0; i < numberof (wcp); i++)
    read_wincp (wincp[i], wcp[i].file, wcp[i].name);
  printf ("\n");

  for (int i = 0; i < numberof (cs); i++)
    read_iso8859 (iso8859[i], cs[i].file, cs[i].name,
                  i < numberof (wcp) && wcp[i].f ? wincp[i] : 0);
  printf ("\n");

#ifdef CCS_ULATIN_MIN
  ucs2_t ulatin[CCS_ULATIN_MAX - CCS_ULATIN_MIN + 1];
  init_ulatin (ulatin, iso8859, numberof (cs));
#endif

  for (i = 0; i < numberof (wcp); i++)
    {
      ucs2_t wbuf[128];
      clear (wbuf, numberof (wbuf));
      int j;
      for (j = 0; j < numberof (cs); j++)
        if (cs[j].charset == wcp[i].base)
          {
            wincp2int (wincp[i], wbuf, iso8859[j], cs[j].charset << 7);
            break;
          }
      for (j = 0; j < numberof (cs); j++)
        if (cs[j].charset != wcp[i].base)
          wincp2int (wincp[i], wbuf, iso8859[j], cs[j].charset << 7);
      for (j = 0; j < 128; j++)
        if (wincp[i][j] != ucs2_t (-1) && wbuf[j] == ucs2_t (-1))
          {
#ifdef CCS_ULATIN_MIN
            int k;
            for (k = 0; k < numberof (ulatin); k++)
              if (wincp[i][j] == ulatin[k])
                {
                  wbuf[j] = CCS_ULATIN_MIN + k;
                  break;
                }
            if (k == numberof (ulatin))
#endif
              fprintf (stderr, "Warning: %s: 0x%02x: not found\n", wcp[i].file, j + 128);
          }
      printf ("Char %s_to_internal[] =\n{\n", wcp[i].name);
      print (wbuf, 128);
      printf ("};\n\n");
    }

  read_jisx0212 (jisx0212);
  read_ksc5601 (ksc5601);
  read_gb2312 (gb2312);

  read_big5 (big5);

  make_cns11643 (big5, gb2312);

  make_cp932 (int2wc);

  merge_int2wc (int2wc, jisx0212, numberof (jisx0212), CCS_JISX0212_MIN);
  merge_int2wc (int2wc, ksc5601, numberof (ksc5601), CCS_KSC5601_MIN);
  merge_int2wc (int2wc, gb2312, numberof (gb2312), CCS_GB2312_MIN);
  merge_int2wc (int2wc, big5, numberof (big5), CCS_BIG5_MIN);
  for (int i = 0; i < numberof (cs); i++)
    merge_int2wc (int2wc, iso8859[i], numberof (iso8859[i]), cs[i].charset << 7);
  merge_unicode (int2wc);

#ifdef CCS_ULATIN_MIN
  memcpy (int2wc + CCS_ULATIN_MIN, ulatin, sizeof ulatin);
#endif
#ifdef CCS_UJP_MIN
  init_ujp (int2wc);
#endif

  output_simple (int2wc, "ucs2_t", "internal2wc_table");

  exit (0);
}
