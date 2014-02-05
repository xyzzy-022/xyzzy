#include <windows.h>
#include <malloc.h>

typedef unsigned long u_long;
typedef unsigned char u_char;

static char *
skip_token (char *p)
{
  if (*p == '"')
    {
      for (p++; *p && *p != '"'; p++)
        {
          if (IsDBCSLeadByte (*p) && p[1])
            p++;
        }
      if (*p == '"')
        p++;
    }
  else
    {
      for (; *p && *p != ' ' && *p != '\t'; p++)
        if (IsDBCSLeadByte (*p) && p[1])
          p++;
    }
  return p;
}

static char *
skip_white (char *p)
{
  for (; *p == ' ' || *p == '\t'; p++)
    ;
  return p;
}

static char *
split (char *&beg)
{
  char *p = skip_token (beg);
  if (*beg == '"')
    {
      beg++;
      if (*CharPrev (beg, p) == '"')
        p[-1] = 0;
    }
  else if (*p)
    *p++ = 0;
  return skip_white (p);
}

static char *
split (char *&beg, int &l)
{
  char *p = skip_token (beg);
  if (*beg == '"')
    {
      beg++;
      l = p - beg;
      if (*CharPrev (beg, p) == '"')
        l--;
    }
  else
    l = p - beg;
  return skip_white (p);
}

static u_long
parse_long (const char *p)
{
  u_long val = 0;
  for (; *p >= '0' && *p <= '9'; p++)
    val = val * 10 + *p - '0';
  return val;
}

static inline int
char_upcase (int c)
{
  return c >= 'a' && c <= 'Z' ? c - ('a' - 'A') : c;
}

static int
bcasecmp (const void *b1, const void *b2, int size)
{
  const u_char *p = (const u_char *)b1, *const pe = p + size;
  const u_char *q = (const u_char *)b2;
  int f;
  for (f = 0; p < pe && !(f = char_upcase (*p) - char_upcase (*q)); p++, q++)
    ;
  return f;
}

static void
doprint (const char *fmt, ...)
{
  char buf[1024];
  va_list ap;
  va_start (ap, fmt);
  wvsprintf (buf, fmt, ap);
  va_end (ap);
  DWORD n;
  WriteFile (GetStdHandle (STD_ERROR_HANDLE), buf, lstrlen (buf), &n, 0);
}

static void
syserror (int e, char *buf, int size)
{
  if (!FormatMessage ((FORMAT_MESSAGE_FROM_SYSTEM
                       | FORMAT_MESSAGE_IGNORE_INSERTS
                       | FORMAT_MESSAGE_MAX_WIDTH_MASK),
                      0, e, GetUserDefaultLangID (),
                      buf, size, 0))
    wsprintf (buf, "error %d", e);
}

static int
cmdmatch (const char *p, const char *pe, const char *s)
{
  if (pe - p >= 4 && (!bcasecmp (pe - 4, ".exe", 4)
                      || !bcasecmp (pe - 4, ".com", 4)))
    pe -= 4;
  int l = lstrlen (s);
  return pe - p >= l && !bcasecmp (pe - l, s, l);
}

static void
set_title (char *cmd)
{
  int cmdl;
  char *opt = split (cmd, cmdl);
  if (cmdmatch (cmd, cmd + cmdl, "cmd")
      || cmdmatch (cmd, cmd + cmdl, "command"))
    {
      int optl;
      char *arg = split (opt, optl);
      if (optl == 2 && !bcasecmp (opt, "/c", 2))
        {
          cmd = arg;
          split (cmd, cmdl);
        }
    }

  char *title = (char *)_alloca (cmdl + 1);
  memcpy (title, cmd, cmdl);
  title[cmdl] = 0;
  SetConsoleTitle (title);
}

int
main (void)
{
  char buf[256];
  char *myname = skip_white (GetCommandLine ());
  char *opt = split (myname);
  WORD show = 0;
  char *event;
  if (!strncmp (opt, "-s", 2))
    {
      if (strlen (opt) > 2)
        show = static_cast <WORD> (parse_long (opt + 2));
      event = split (opt);
    }
  else
    {
      event = opt;
    }
  char *cmdline = split (event);
  char *dir = 0;
  int no_events = !lstrcmp (event, "--");

  if (no_events)
    {
      dir = cmdline;
      cmdline = split (dir);
    }

  set_title (cmdline);

  PROCESS_INFORMATION pi;
  STARTUPINFO si = {sizeof si};

  si.dwFlags = STARTF_USESTDHANDLES;
  if (show)
    {
      si.dwFlags |= STARTF_USESHOWWINDOW;
      si.wShowWindow = show;
    }
  si.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  si.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  si.hStdError = GetStdHandle (STD_ERROR_HANDLE);

  if (!CreateProcess (0, cmdline, 0, 0, 1, CREATE_NEW_PROCESS_GROUP,
                      0, dir, &si, &pi))
    {
      syserror (GetLastError (), buf, sizeof buf);
      doprint ("%s: %s: %s\n", myname, cmdline, buf);
      ExitProcess (2);
    }

  CloseHandle (pi.hThread);

  if (no_events)
    {
      if (WaitForSingleObject (pi.hProcess, INFINITE) == WAIT_FAILED)
        {
          syserror (GetLastError (), buf, sizeof buf);
          doprint ("%s: %s\n", myname, buf);
          ExitProcess (2);
        }
    }
  else
    {
      HANDLE hevent = HANDLE (parse_long (event));

      HANDLE objects[2];
      objects[0] = hevent;
      objects[1] = pi.hProcess;
      while (1)
        {
          DWORD r = WaitForMultipleObjects (2, objects, 0, INFINITE);
          if (r == WAIT_FAILED)
            {
              syserror (GetLastError (), buf, sizeof buf);
              doprint ("%s: %s\n", myname, buf);
              ExitProcess (2);
            }
          if (r == WAIT_OBJECT_0 + 1)
            break;

          GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pi.dwProcessId);
          if (WaitForSingleObject (pi.hProcess, 3000) == WAIT_TIMEOUT)
            GenerateConsoleCtrlEvent (CTRL_C_EVENT, pi.dwProcessId);
          ResetEvent (hevent);
        }
    }

  DWORD code;
  GetExitCodeProcess (pi.hProcess, &code);
  ExitProcess (code);
}
