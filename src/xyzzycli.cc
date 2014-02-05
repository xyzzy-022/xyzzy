#include <windows.h>
#include <malloc.h>
#include "xyzzycli.h"
#include "listen.h"

#define PACK_VERSION(MAJ, MIN) MAKELONG ((MIN), (MAJ))

#ifndef SPI_GETFOREGROUNDLOCKTIMEOUT
#define SPI_GETFOREGROUNDLOCKTIMEOUT 0x2000
#define SPI_SETFOREGROUNDLOCKTIMEOUT 0x2001
#endif

void
ForceSetForegroundWindow (HWND hwnd)
{
  OSVERSIONINFO os;
  os.dwOSVersionInfoSize = sizeof os;
  GetVersionEx (&os);

  DWORD timeout;
  if (PACK_VERSION (os.dwMajorVersion, os.dwMinorVersion) >= PACK_VERSION (4, 10)
      && SystemParametersInfo (SPI_GETFOREGROUNDLOCKTIMEOUT, 0, &timeout, 0))
    {
      SystemParametersInfo (SPI_SETFOREGROUNDLOCKTIMEOUT, 0, 0, 0);
      SetForegroundWindow (hwnd);
      SystemParametersInfo (SPI_SETFOREGROUNDLOCKTIMEOUT, 0, (void *)timeout, 0);
    }
  else
    SetForegroundWindow (hwnd);
}

static int
error (int id)
{
  char buf[256];
  LoadString (GetModuleHandle (0), id, buf, sizeof buf);
  MessageBox (0, buf, 0, MB_SYSTEMMODAL	| MB_ICONHAND);
  return 2;
}

static char *
stpcpy (char *d, const char *s)
{
  while (*d++ = *s++)
    ;
  return d - 1;
}

static char *
store (char *d, const char *s)
{
  *d++ = '"';
  while (*s)
    {
      if (IsDBCSLeadByte (*s) && s[1])
        {
          *d++ = *s++;
          *d++ = *s++;
        }
      else if (*s == '\\' || *s == '"')
        {
          *d++ = '\\';
          *d++ = *s++;
        }
      else
        *d++ = *s++;
    }
  *d++ = '"';
  return d;
}

class xyzzysrv
{
  HANDLE m_hmap;
  void *m_base;
public:
  xyzzysrv () : m_hmap (0), m_base (0) {}
  ~xyzzysrv ()
    {
      if (m_base)
        UnmapViewOfFile (m_base);
      if (m_hmap)
        CloseHandle (m_hmap);
    }
  int alloc (int size)
    {
      size += sizeof (xyzzysrv_param);
      m_hmap = CreateFileMapping (HANDLE (-1), 0, PAGE_READWRITE, 0, size, 0);
      if (!m_hmap)
        return 0;
      m_base = MapViewOfFile (m_hmap, FILE_MAP_WRITE, 0, 0, 0);
      if (!m_base)
        return 0;
      param ()->size = size;
      param ()->pid = 0;
      param ()->hevent = 0;
      param ()->hwnd = 0;
      param ()->kill_ok = 0;
      return 1;
    }
  xyzzysrv_param *param () const {return (xyzzysrv_param *)m_base;}
  char *data () const {return param ()->data;}
  HANDLE handle () const {return m_hmap;}
};

static int
create_sexp (xyzzysrv &sv, int ac, char **av)
{
  char curdir[MAX_PATH + 1];
  GetCurrentDirectory (sizeof curdir, curdir);
  int l = 256 + lstrlen (curdir) * 2;
  for (int i = 0; i < ac; i++)
    l += lstrlen (av[i]) * 2 + 3;

  if (!sv.alloc (l))
    return 0;

  char *d = stpcpy (sv.data (), "(ed::*xyzzycli-helper ");

  d = store (d, curdir);
  *d++ = ' ';
  *d++ = '\'';
  *d++ = '(';
  for (int i = 0; i < ac; i++)
    d = store (d, av[i]);
  *d++ = ')';
  *d++ = ')';
  *d = 0;
  return 1;
}

static HANDLE
dup_handle (HANDLE hsrc, DWORD pid)
{
  HANDLE hproc = OpenProcess (PROCESS_DUP_HANDLE, 0, pid);
  if (!hproc)
    return 0;
  HANDLE hdst;
  if (!DuplicateHandle (hproc, hsrc, GetCurrentProcess (), &hdst,
                        0, 0, DUPLICATE_SAME_ACCESS))
    hdst = 0;
  CloseHandle (hproc);
  return hdst;
}

struct lookup_server
{
  HWND hwnd;
  HANDLE hevent;
};

static BOOL CALLBACK
enum_proc (HWND hwnd, LPARAM param)
{
  lookup_server *ls = (lookup_server *)param;
  HANDLE h = GetProp (hwnd, xyzzysrv_name);
  if (!h)
    return 1;

  DWORD pid = 0;
  GetWindowThreadProcessId (hwnd, &pid);
  if (!pid)
    return 1;

  ls->hevent = dup_handle (h, pid);
  if (!ls->hevent)
    return 1;
  ls->hwnd = hwnd;
  return 0;
}

static HWND
find_server (lookup_server &ls)
{
  ls.hwnd = 0;
  ls.hevent = 0;
  EnumWindows (enum_proc, LPARAM (&ls));
  return ls.hwnd;
}

static int
run_xyzzy (int argc, char **argv, const char *xyzzy)
{
  int l = lstrlen (xyzzy) + 1;
  for (int i = 1; i < argc; l += lstrlen (argv[i]) + 1, i++)
    ;
  char *const cl = (char *)_alloca (l);
  char *p = stpcpy (cl, xyzzy);
  for (int i = 1; i < argc; i++)
    {
      *p++ = ' ';
      p = stpcpy (p, argv[i]);
    }

  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  memset (&si, 0, sizeof si);
  si.cb = sizeof si;
  if (!CreateProcess (0, cl, 0, 0, 0, 0, 0, 0, &si, &pi))
    return 0;
  WaitForInputIdle (pi.hProcess, 60000);
  CloseHandle (pi.hProcess);
  CloseHandle (pi.hThread);
  return 1;
}

static void
wait_term (xyzzysrv &sv)
{
  if (!sv.param ()->hwnd)
    return;

  HANDLE hevent = dup_handle (sv.param ()->hevent, sv.param ()->pid);
  if (hevent)
    {
      WaitForSingleObject (hevent, INFINITE);
      CloseHandle (hevent);
    }
}

static int
skip_args (int argc, char **argv)
{
  int ac;
  for (ac = 1; ac < argc - 1; ac += 2)
    if (lstrcmp (argv[ac], "-image")
        && lstrcmp (argv[ac], "-config")
        && lstrcmp (argv[ac], "-ini"))
      break;
  if (ac < argc && (!lstrcmp (argv[ac], "-q")
                    || !lstrcmp (argv[ac], "-no-init-file")))
    ac++;
  return ac;
}

class synchronize
{
  HANDLE h;
public:
  synchronize (const char *name)
    {
      h = CreateMutex (0, 1, name);
      if (h && GetLastError () == ERROR_ALREADY_EXISTS)
        WaitForSingleObject (h, INFINITE);
    }
  ~synchronize ()
    {
      if (h)
        {
          ReleaseMutex (h);
          CloseHandle (h);
        }
    }
};

static int
xmain (int argc, char **argv, const char *xyzzy, int multi_instance)
{
  MSG msg;
  PostQuitMessage (0);
  GetMessage (&msg, 0, 0, 0);

  int ac = skip_args (argc, argv);

  xyzzysrv sv;
  if (!create_sexp (sv, argc - ac, argv + ac))
    return error (IDS_NO_MEMORY);

  lookup_server ls;
  {
    synchronize sync ("{FDFB3F8E-65AC-11D4-ADA0-0040053444B8}");
    if (multi_instance || !find_server (ls))
      {
        sv.param ()->kill_ok = 1;

        if (!run_xyzzy (ac, argv, xyzzy))
          return error (IDS_CALL_PROCESS);

#define RETRY_MAX 30
        int i;
        for (i = 0; i < RETRY_MAX; i++)
          {
            if (find_server (ls))
              break;
            Sleep (100);
          }
        if (i == RETRY_MAX)
          return error (IDS_CONNECT_FAILED);
      }
  }

  int wait_ok = ls.hevent && WaitForSingleObject (ls.hevent, 60000) == WAIT_OBJECT_0;
  CloseHandle (ls.hevent);
  if (!wait_ok)
    return error (IDS_CONNECT_FAILED);

  ForceSetForegroundWindow (ls.hwnd);

  int r = SendMessage (ls.hwnd, RegisterWindowMessage (xyzzysrv_name),
                       GetCurrentProcessId (), LPARAM (sv.handle ()));
  if (!r)
    return error (IDS_READ_FAILED);
  if (r > 0)
    wait_term (sv);
  return 0;
}

static const char *
skip_white (const char *p)
{
  for (; *p == ' ' || *p == '\t'; p++)
    ;
  return p;
}

#define COPYCHAR(C) (nchars++, (b ? *b++ = (C) : 0))
#define COPYARGV(X) (ac++, (av ? *av++ = (X) : 0))

static int
parse_cmdline1 (const char *p, char *&b0, int &ac, char **&av0, int nchars)
{
  char *b = b0;
  char **av = av0;
  while (1)
    {
      p = skip_white (p);
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
  b0 = b;
  av0 = av;
  return nchars;
}

static int
notepad_parse_cmdline (const char *p, char *&b0, int &ac, char **&av0, int nchars)
{
  char *b = b0;
  char **av = av0;
  p = skip_white (p);
  if (*p == '/' && (p[1] == 'p' || p[1] == 'P')
      && (p[2] == ' ' || p[2] == '\t' || !p[2]))
    {
      COPYARGV ("-p");
      p = skip_white (p + 2);
    }
  if (*p)
    {
      COPYARGV (b);
      do
        {
          if (*p != '"')
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
      while (*p);
      COPYCHAR (0);
    }
  b0 = b;
  av0 = av;
  return nchars;
}

struct config
{
  char xyzzy[MAX_PATH];
  char pre_opt[1024];
  char post_opt[1024];
  int notepad;
  int multi_instance;
};

static int
parse_cmdline (const char *p, char *b, int &ac, char **av, const config &cf)
{
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

  if (!cf.notepad)
    {
      nchars = parse_cmdline1 (cf.pre_opt, b, ac, av, nchars);
      nchars = parse_cmdline1 (p, b, ac, av, nchars);
    }
  else
    {
      COPYARGV ("-wait");
      nchars = parse_cmdline1 (cf.pre_opt, b, ac, av, nchars);
      nchars = notepad_parse_cmdline (p, b, ac, av, nchars);
    }
  nchars = parse_cmdline1 (cf.post_opt, b, ac, av, nchars);

  COPYARGV (0);
  return nchars;
#undef COPYCHAR
#undef COPYARGV
}

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
          if (*p == '\\')
            base = p + 1;
          p++;
        }
    }
  return base ? base : path;
}

static void
read_config (config &cf)
{
  char path[MAX_PATH + 16];
  GetModuleFileName (0, path, MAX_PATH);
  cf.notepad = !lstrcmpi (basename (path), "notepad.exe");
  int l = lstrlen (path);
  if (l > 4 && !lstrcmpi (&path[l - 4], ".exe"))
    lstrcpy (&path[l - 3], "ini");
  else
    lstrcpy (path + l, ".ini");
  GetPrivateProfileString ("xyzzy", "path", "xyzzy.exe",
                           cf.xyzzy, sizeof cf.xyzzy, path);
  if (!cf.notepad)
    cf.notepad = GetPrivateProfileInt ("xyzzy", "compatNotepad", 0, path);
  cf.multi_instance = GetPrivateProfileInt ("xyzzy", "multipleInstances", 0, path);
  GetPrivateProfileString ("xyzzy", "precedingOptions", "",
                           cf.pre_opt, sizeof cf.pre_opt, path);
  GetPrivateProfileString ("xyzzy", "followingOptions", "",
                           cf.post_opt, sizeof cf.post_opt, path);
}

int WINAPI
WinMain (HINSTANCE hinst, HINSTANCE, LPSTR, int cmdshow)
{
  config cf;
  read_config (cf);

  const char *const cl = GetCommandLine ();
  int ac;
  int nchars = parse_cmdline (cl, 0, ac, 0, cf);
  char **av = (char **)_alloca (sizeof *av * (ac + 1) + nchars);
  parse_cmdline (cl, (char *)(av + ac + 1), ac, av, cf);
  ExitProcess (xmain (ac, av, cf.xyzzy, cf.multi_instance));
}
