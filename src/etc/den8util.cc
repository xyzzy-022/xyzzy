#include <windows.h>
#include <fstream>
#include <time.h>

using namespace std;

enum STATUS
{
  NONE,
  RECV,
  READ,
  RPLY,
  RPLD,
  WRIT,
  SENT,
  ERRR
};

struct CACHE_RECORD
{
  STATUS status;
  int timeDate;
  int nSize;
  char cHeader[500];
};

struct CACHE_HEADER
{
  char cIdentifier[64];
  BOOL bDirty;
  BOOL bOutFolder;
  BYTE bSortByDate;
  BYTE bSortByFrom;
  BYTE bSortBySubject;
  BYTE bReverseOrder;
  int nUnreadCount;
  int nCaretIndex;
  int nTopIndex;
  char cPadding[40];
};

static int
open_cache (ifstream &is, const char *cache, CACHE_HEADER &ch)
{
  is.open (cache, ios::in | ios::binary);
  if (!is)
    return 0;

  is.read ((char *)&ch, sizeof ch);
  return is.good () && !strcmp (ch.cIdentifier, "Den8 Cache Format 3.01");
}

struct folder_info
{
  int min;
  int max;
  int unread;
};

extern "C" __declspec (dllexport) int __stdcall
den8_folder_info (const char *cache, folder_info *fi)
{
  CACHE_HEADER ch;
  ifstream is;
  if (!open_cache (is, cache, ch))
    return 0;

  fi->min = -1;
  fi->max = -1;
  fi->unread = -1;

  CACHE_RECORD r;
  for (int mailno = 0; (is.read ((char *)&r, sizeof r), is.good ()); mailno++)
    switch (r.status)
      {
      case NONE:
        break;

      case RECV:
        if (fi->unread < 0)
          fi->unread = mailno;
        /* fall thru... */
      default:
        if (fi->min < 0)
          fi->min = mailno;
        fi->max = mailno;
        break;
      }
  if (fi->min < 0)
    fi->min = 0;
  if (fi->max < 0)
    fi->max = 0;
  return 1;
}

extern "C" __declspec (dllexport) int __stdcall
parse_den8_cache (const char *cache,
                  void (__stdcall *callback) (int, int, int, int,
                                              const char *, const char *,
                                              const char *, const char *))
{
  CACHE_HEADER ch;
  ifstream is;
  if (!open_cache (is, cache, ch))
    return 0;

#define NPARAMS 4
  union
    {
      CACHE_RECORD r;
      char b[sizeof (CACHE_RECORD) + NPARAMS];
    } x;
  memset (&x.b[sizeof x.r], 0, sizeof x - sizeof x.r);

  for (int mailno = 0; (is.read ((char *)&x.r, sizeof x.r), is.good ()); mailno++)
    if (x.r.status != NONE)
      {
        const char *s[NPARAMS];
        s[0] = x.r.cHeader;
        for (int i = 1; i < NPARAMS; i++)
          s[i] = s[i - 1] + strlen (s[i - 1]) + 1;
        callback (mailno, x.r.status, x.r.timeDate, x.r.nSize,
                  s[0], s[1], s[2], s[3]);
      }
  return 1;
}

static BOOL CALLBACK
flush_cache_proc (HWND hwnd, LPARAM)
{
  char b[32];
  if (GetClassName (hwnd, b, sizeof b)
      && !memcmp (b, "D8FLD", 5)
      && IsIconic (hwnd))
    {
      ShowWindow (hwnd, SW_SHOWNORMAL);
      ShowWindow (hwnd, SW_SHOWMINIMIZED);
    }
  return 1;
}

extern "C" __declspec (dllexport) int __stdcall
den8_flush_cache ()
{
  HWND hwnd = FindWindow ("DeN8:MaIn", 0);
  if (!hwnd)
    return 0;

  if (SendMessage (hwnd, RegisterWindowMessage ("den8FlushCache"), 0, 0) != 0xde8)
    {
      int x = IsWindowVisible (hwnd) && !IsIconic (hwnd);
      if (x)
        SendMessage (hwnd, WM_SETREDRAW, 0, 0);
      EnumChildWindows (hwnd, flush_cache_proc, 0);
      if (x)
        SendMessage (hwnd, WM_SETREDRAW, 1, 0);
    }
  return 1;
}

extern "C" __declspec (dllexport) int __stdcall
den8_cache_dirty_p (const char *cache)
{
  CACHE_HEADER ch;
  ifstream is;
  if (!open_cache (is, cache, ch))
    return 0;
  return ch.bDirty;
}

BOOL __stdcall
DllMain (HINSTANCE, DWORD, LPVOID)
{
  return 1;
}
