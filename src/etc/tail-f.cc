#include <windows.h>
#include <malloc.h>

#pragma data_seg (".text")

static char *
basename (char *path)
{
  char *base = 0;
  char *p = path;
  while (*p)
    {
      if (IsDBCSLeadByte (*p) && p[1])
        p += 2;
      else
        {
          if (*p == '\\' || *p == '/')
            base = p + 1;
          p++;
        }
    }
  return base ? base : path;
}

static void
error (const char *fmt, ...)
{
  DWORD n;
  void *buf = 0;
  va_list ap;
  va_start (ap, fmt);
  FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
                  | FORMAT_MESSAGE_FROM_STRING),
                 fmt, 0, 0, (char *)&buf, 0, &ap);
  va_end (ap);
  if (buf)
    {
      WriteFile (GetStdHandle (STD_ERROR_HANDLE),
                 buf, lstrlen ((char *)buf), &n, 0);
      LocalFree (buf);
    }
}

static char *
error_str (int e)
{
  void *buf = 0;
  FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
                  | FORMAT_MESSAGE_IGNORE_INSERTS
                  | FORMAT_MESSAGE_FROM_SYSTEM
                  | FORMAT_MESSAGE_MAX_WIDTH_MASK),
                 0, e, GetUserDefaultLangID (), (char *)&buf, 0, 0);
  return (char *)buf;
}

static int
xmain (int argc, char **argv)
{
  const char *const progname = basename (argv[0]);

  if (argc > 2)
    {
      error ("usage: %1 [filename]\n", progname);
      return 2;
    }

  HANDLE in, out = GetStdHandle (STD_OUTPUT_HANDLE);
  if (argc == 1)
    in = GetStdHandle (STD_INPUT_HANDLE);
  else
    {
      in = CreateFile (argv[1], GENERIC_READ,
                       FILE_SHARE_READ | FILE_SHARE_WRITE,
                       0, OPEN_EXISTING, 0, 0);
      if (in == INVALID_HANDLE_VALUE)
        {
          int e = GetLastError ();
          char buf[64], *b;
          char *es = error_str (e);
          if (es)
            b = es;
          else
            {
              wsprintf (buf, "error %d", e);
              b = buf;
            }
          error ("%1: %2: %3\n", progname, b, argv[1]);
          if (es)
            LocalFree (es);
          return 2;
        }
    }

  while (1)
    {
      DWORD n;
      char buf[4096];
      while (ReadFile (in, buf, sizeof buf, &n, 0) && n)
        WriteFile (out, buf, n, &n, 0);
      Sleep (1000);
    }

  return 0;
}


static int
parse_cmdline (const char *p, char *b, int &ac, char **av)
{
#define COPYCHAR(C) (nchars++, (b ? *b++ = (C) : 0))
#define COPYARGV(X) (ac++, (av ? *av++ = (X) : 0))
  int nchars = 0;
  ac = -1;

  COPYARGV (b);

  if (*p == '"')
    {
      for (p++; *p && *p != '"'; p++)
        {
          if (IsDBCSLeadByte (*p) && p[1])
            {
              COPYCHAR (*p);
              p++;
            }
          COPYCHAR (*p);
        }
      COPYCHAR (0);
      if (*p == '"')
        p++;
    }
  else
    {
      for (; *p && *p != ' ' && *p != '\t'; p++)
        {
          if (IsDBCSLeadByte (*p) && p[1])
            {
              COPYCHAR (*p);
              p++;
            }
          COPYCHAR (*p);
        }
      COPYCHAR (0);
    }

  while (1)
    {
      for (; *p == ' ' || *p == '\t'; p++)
        ;
      if (!*p)
        break;

      COPYARGV (b);

      int dq = 0;
      while (1)
        {
          int nbacksl;
          for (nbacksl = 0; *p == '\\'; nbacksl++, p++)
            ;

          int ignore = 0;
          if (*p == '"')
            {
              if (!(nbacksl & 1))
                {
                  if (dq && p[1] == '"')
                    p++;
                  else
                    ignore = 1;
                  dq = !dq;
                }
              nbacksl >>= 1;
            }

          while (nbacksl-- > 0)
            COPYCHAR ('\\');

          if (!*p || (!dq && (*p == ' ' || *p == '\t')))
            break;

          if (!ignore)
            {
              if (IsDBCSLeadByte (*p) && p[1])
                {
                  COPYCHAR (*p);
                  p++;
                }
              COPYCHAR (*p);
            }
          p++;
        }
      COPYCHAR (0);
    }

  COPYARGV (0);
  return nchars;
#undef COPYCHAR
#undef COPYARGV
}

int
main (void)
{
  const char *const cl = GetCommandLine ();
  int ac;
  int nchars = parse_cmdline (cl, 0, ac, 0);
  char **av = (char **)_alloca (sizeof *av * (ac + 1) + nchars);
  parse_cmdline (cl, (char *)(av + ac + 1), ac, av);
  ExitProcess (xmain (ac, av));
}
