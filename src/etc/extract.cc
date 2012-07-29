#include <windows.h>

#pragma data_seg (".text")

extern "C" int WINAPI Unlha (HWND, LPCSTR, LPSTR, DWORD);

static const char *
skip_argv0 (const char *p)
{
  if (*p == '"')
    {
      for (p++; *p && *p != '"'; p++)
        if (IsDBCSLeadByte (*p) && p[1])
          p++;
      if (*p == '"')
        p++;
    }
  else
    for (; *p && *p != ' ' && *p != '\t'; p++)
      if (IsDBCSLeadByte (*p) && p[1])
        p++;
  for (; *p == ' ' || *p == '\t'; p++)
    ;
  return p;
}

int __stdcall
WinMain (HINSTANCE, HINSTANCE, LPSTR, int)
{
  const char *cl = skip_argv0 (GetCommandLine ());
  if (*cl && MessageBox (0, "‚æ‚ë‚µ?", cl, MB_ICONQUESTION | MB_YESNO) == IDYES)
    Unlha (0, cl, 0, 0);
  ExitProcess (0);
}
