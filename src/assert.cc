#include "stdafx.h"
#include "cdecl.h"

#ifdef DEBUG

static LONG double_fault = -1;

int
assert_failed (const char *file, int line)
{
  char msg[MAX_PATH * 2];

  wsprintf (msg, "Assertion failed: %s: %d\n", file, line);
  OutputDebugString (msg);

  if (InterlockedIncrement (&double_fault) > 0)
    InterlockedDecrement (&double_fault);
  else
    {
      HWND hwnd = GetActiveWindow ();
      if (hwnd)
        hwnd = GetLastActivePopup (hwnd);

      int r = MessageBox (hwnd, msg, 0,
                          (MB_TASKMODAL | MB_ICONHAND
                           | MB_ABORTRETRYIGNORE | MB_SETFOREGROUND));

      InterlockedDecrement (&double_fault);

      if (r == IDIGNORE)
        return 0;

      if (r == IDABORT)
        abort ();
    }
  DebugBreak ();
  return 0;
}

#endif
