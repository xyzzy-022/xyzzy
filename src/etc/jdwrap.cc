#include <windows.h>

static int
spawn_jdate (const char *jdate, const char *script)
{
  char jdate_exe[MAX_PATH], *name;
  if (!SearchPath (0, jdate, ".exe", sizeof jdate_exe, jdate_exe, &name))
    {
      MessageBox (0, "Jdate.exe not found", 0, 0);
      return 2;
    }

  char jdate_ini[MAX_PATH];
  lstrcpy (jdate_ini, jdate_exe);
  lstrcpy (&jdate_ini[lstrlen (jdate_ini) - 3], "ini");
  if (GetFileAttributes (jdate_ini) == -1)
    {
      MessageBox (0, "Jdate.ini not found", 0, 0);
      return 2;
    }

  char null_cmd[MAX_PATH + 10];
  *null_cmd = '"';
  GetModuleFileName (0, null_cmd + 1, MAX_PATH);
  lstrcat (null_cmd, "\" \"%s\"");

  static const char sEditors[] = "Editors";
  static const char sEditor0[] = "Editor0";
  char editor0[MAX_PATH];
  GetPrivateProfileString (sEditors, sEditor0, "",
                           editor0, sizeof editor0, jdate_ini);
  WritePrivateProfileString (sEditors, sEditor0, null_cmd, jdate_ini);

  char cmdline[MAX_PATH * 2 + 10];
  wsprintf (cmdline, "\"%s\" %s", jdate_exe, script);
  for (char *p = cmdline; *p; p = CharNext (p))
    if (*p == '/')
      *p = '\\';

  STARTUPINFO si;
  ZeroMemory (&si, sizeof si);
  si.cb = sizeof si;
  PROCESS_INFORMATION pi;
  int r = CreateProcess (jdate_exe, cmdline, 0, 0, 0, CREATE_SUSPENDED,
                         0, 0, &si, &pi);
  if (r)
    {
      while (GetAsyncKeyState (VK_SHIFT) < 0
             || GetAsyncKeyState (VK_CONTROL) < 0
             || GetAsyncKeyState (VK_MENU) < 0)
        Sleep (100);
      ResumeThread (pi.hThread);
      CloseHandle (pi.hThread);
      WaitForSingleObject (pi.hProcess, INFINITE);
      CloseHandle (pi.hProcess);
    }
  else
    MessageBox (0, "Cannot execute Jdate.exe", 0, 0);

  WritePrivateProfileString (sEditors, sEditor0, editor0, jdate_ini);
  return !r;
}

int __stdcall
WinMain (HINSTANCE, HINSTANCE, LPSTR, int)
{
  char myname[MAX_PATH];
  GetModuleFileName (0, myname, sizeof myname);
  for (char *p = myname; *p; p = CharNext (p))
    if (*p == '\\')
      *p = '_';
  HANDLE hmutex = CreateMutex (0, 0, myname);
  if (!hmutex)
    return 2;
  int r = 2;
  if (GetLastError () == ERROR_ALREADY_EXISTS)
    {
      if (__argc == 2)
        {
          HANDLE h = CreateFile (__argv[1], GENERIC_READ | GENERIC_WRITE, 0, 0,
                                 OPEN_EXISTING, 0, 0);
          if (h != INVALID_HANDLE_VALUE)
            {
              FILETIME ft;
              GetFileTime (h, 0, 0, &ft);
              (__int64 &)ft += 20000000;
              SetFileTime (h, 0, 0, &ft);
              CloseHandle (h);
              r = 0;
            }
        }
    }
  else if (__argc == 3)
    r = spawn_jdate (__argv[1], __argv[2]);
  CloseHandle (hmutex);
  return r;
}
