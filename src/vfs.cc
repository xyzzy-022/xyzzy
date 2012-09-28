#include "stdafx.h"
#include "ed.h"
#include "dyn-handle.h"
#include "vwin32.h"

class NetPassDlg
{
  HWND hwnd;
public:
  char username[256];
  char passwd[256];
  const char *remote;

private:
  static BOOL CALLBACK netpass_dlgproc (HWND, UINT, WPARAM, LPARAM);
  BOOL dlgproc (UINT, WPARAM, LPARAM);
  void do_command (int, int);
  void init_dialog ();

public:
  NetPassDlg (const char *);
  int do_modal ();
};

NetPassDlg::NetPassDlg (const char *r)
     : remote (r)
{
  *username = 0;
  *passwd = 0;
}

void
NetPassDlg::do_command (int id, int code)
{
  switch (id)
    {
    case IDOK:
      GetDlgItemText (hwnd, IDC_USERNAME, username, sizeof username);
      GetDlgItemText (hwnd, IDC_PASSWD, passwd, sizeof passwd);
      /* fall thru... */
    case IDCANCEL:
      EndDialog (hwnd, id);
      break;
    }
}

void
NetPassDlg::init_dialog ()
{
  center_window (hwnd);
  set_window_icon (hwnd);
  SetDlgItemText (hwnd, IDC_SHARE_NAME, remote);
}

BOOL
NetPassDlg::dlgproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      init_dialog ();
      return 1;

    case WM_COMMAND:
      do_command (LOWORD (wparam), HIWORD (wparam));
      return 1;

    default:
      return 0;
    }
}

BOOL CALLBACK
NetPassDlg::netpass_dlgproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  NetPassDlg *p;
  if (msg == WM_INITDIALOG)
    {
      p = (NetPassDlg *)lparam;
      SetWindowLong (hwnd, DWL_USER, lparam);
      p->hwnd = hwnd;
    }
  else
    {
      p = (NetPassDlg *)GetWindowLong (hwnd, DWL_USER);
      if (!p)
        return 0;
    }
  return p->dlgproc (msg, wparam, lparam);
}

int
NetPassDlg::do_modal ()
{
  return DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_NETPASSWD),
                         get_active_window (), netpass_dlgproc, LPARAM (this)) == IDOK;
}

#define WINFS_CALL1(TYPE, FAILED, PATH, FN) \
  WINFS_CALL (TYPE, FAILED, askpass (PATH), FN)
#define WINFS_CALL2(TYPE, FAILED, PATH1, PATH2, FN) \
  WINFS_CALL (TYPE, FAILED, askpass (PATH1, PATH2), FN)
#define WINFS_CALL(TYPE, FAILED, ASKPASS, FN) \
  { TYPE r = ::FN; \
    if (r == (FAILED) && ASKPASS) \
      r = ::FN; \
    return r; }

#define WINFS_MAPSL(PATH) \
  { char *__path = (char *)alloca (strlen (PATH) + 1); \
    strcpy (__path, (PATH)); \
    map_sl_to_backsl (__path); \
    (PATH) = __path; }

static const char *
skip_share (const char *path, int noshare_ok)
{
  const char *p = path;
  if ((*p != '/' && *p != '\\')
      || (p[1] != '/' && p[1] != '\\'))
    return 0;
  p = find_slash (p + 2);
  if (p)
    {
      const char *e = find_slash (p + 1);
      return e ? e : p + strlen (p);
    }
  return noshare_ok ? path + strlen (path) : 0;
}

static int
try_connect (char *remote, int e)
{
  NETRESOURCE nr;
  nr.dwType = RESOURCETYPE_DISK;
  nr.lpLocalName = 0;
  nr.lpRemoteName = remote;
  nr.lpProvider = 0;

  if (e == ERROR_ACCESS_DENIED
      && WNetAddConnection2 (&nr, 0, 0, 0) == NO_ERROR)
    return 1;

  while (1)
    {
      NetPassDlg d (remote);
      if (!d.do_modal ())
        return 0;

      switch (WNetAddConnection2 (&nr, d.passwd, d.username, 0))
        {
        case NO_ERROR:
          return 1;

        case ERROR_INVALID_PASSWORD:
        case ERROR_LOGON_FAILURE:
        case ERROR_ACCESS_DENIED:
          break;

        default:
          return 0;
        }
    }
}

static int
askpass1 (const char *path, int noshare_ok)
{
  if (!path)
    return 0;

  int e = GetLastError ();
  switch (e)
    {
    default:
      return 0;

    case ERROR_ACCESS_DENIED:
    case ERROR_INVALID_PASSWORD:
    case ERROR_LOGON_FAILURE:
      break;
    }

  const char *root = skip_share (path, noshare_ok);
  if (!root)
    return 0;
  int l = root - path;
  char *remote = (char *)alloca (l + 1);
  memcpy (remote, path, l);
  remote[l] = 0;
  map_sl_to_backsl (remote);
  if (!_stricmp (WINFS::wfs_share_cache, remote))
    return 0;
  if (try_connect (remote, e))
    {
      *WINFS::wfs_share_cache = 0;
      return 1;
    }
  strcpy (WINFS::wfs_share_cache, remote);
  SetLastError (e);
  return 0;
}

static inline int
askpass (const char *path)
{
  return askpass1 (path, 0);
}

static inline int
askpass_noshare (const char *path)
{
  return askpass1 (path, 1);
}

static inline int
askpass (const char *path1, const char *path2)
{
  return askpass1 (path1, 0) || askpass1 (path2, 0);
}

char WINFS::wfs_share_cache[MAX_PATH * 2];

const WINFS::GETDISKFREESPACEEX WINFS::GetDiskFreeSpaceEx =
  (WINFS::GETDISKFREESPACEEX)GetProcAddress (GetModuleHandle ("KERNEL32"),
                                             "GetDiskFreeSpaceExA");

BOOL WINAPI
WINFS::CreateDirectory (LPCSTR lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  WINFS_CALL1 (BOOL, FALSE, lpPathName, CreateDirectory (lpPathName, lpSecurityAttributes));
}

HANDLE WINAPI
WINFS::CreateFile (LPCSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode,
                   LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition,
                   DWORD dwFlagsAndAttributes, HANDLE hTemplateFile)
{
  HANDLE r = ::CreateFile (lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes,
                           dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  if (r != INVALID_HANDLE_VALUE)
    return r;
  if (!sysdep.WinNTp () || !(dwFlagsAndAttributes & FILE_FLAG_BACKUP_SEMANTICS))
    {
      int e = GetLastError ();
      if (e == ERROR_ACCESS_DENIED)
        {
          DWORD a = ::GetFileAttributes (lpFileName);
          SetLastError (e);
          if (a != -1 && a & FILE_ATTRIBUTE_DIRECTORY)
            return r;
        }
    }
  if (askpass (lpFileName))
    r = ::CreateFile (lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes,
                      dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  return r;
}

BOOL WINAPI
WINFS::DeleteFile (LPCSTR lpFileName)
{
  WINFS_CALL1 (BOOL, FALSE, lpFileName, DeleteFile (lpFileName));
}

HANDLE WINAPI
WINFS::FindFirstFile (LPCSTR lpFileName, LPWIN32_FIND_DATAA lpFindFileData)
{
  WINFS_CALL1 (HANDLE, INVALID_HANDLE_VALUE, lpFileName,
               FindFirstFile (lpFileName, lpFindFileData));
}

BOOL WINAPI
WINFS::FindNextFile (HANDLE hFindFile, LPWIN32_FIND_DATAA lpFindFileData)
{
  *lpFindFileData->cFileName = 0;
  return (::FindNextFile (hFindFile, lpFindFileData)
          || (GetLastError () == ERROR_MORE_DATA
              && *lpFindFileData->cFileName));
}

static BOOL WINAPI
GetDiskFreeSpaceFAT32 (LPCSTR lpRootPathName, LPDWORD lpSectorsPerCluster,
                       LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters,
                       LPDWORD lpTotalNumberOfClusters)
{
  char buf[PATH_MAX + 1];
  if (!lpRootPathName)
    {
      if (!GetCurrentDirectory (sizeof buf, buf))
        return 0;
      lpRootPathName = root_path_name (buf, buf);
    }

  dyn_handle hvwin32 (CreateFile ("\\\\.\\vwin32", 0, 0, 0, 0,
                                  FILE_FLAG_DELETE_ON_CLOSE, 0));
  if (!hvwin32.valid ())
    return 0;

  ExtGetDskFreSpcStruc dfs = {0};
  DIOC_REGISTERS regs = {0};
  regs.reg_EAX = 0x7303;
  regs.reg_ECX = sizeof dfs;
  regs.reg_EDX = DWORD (lpRootPathName);
  regs.reg_EDI = DWORD (&dfs);

  DWORD nbytes;
  if (!DeviceIoControl (hvwin32, VWIN32_DIOC_DOS_DRIVEINFO,
                        &regs, sizeof regs, &regs, sizeof regs,
                        &nbytes, 0)
      || regs.reg_Flags & X86_CARRY_FLAG)
    return 0;

  *lpSectorsPerCluster = dfs.SectorsPerCluster;
  *lpBytesPerSector = dfs.BytesPerSector;
  *lpNumberOfFreeClusters = dfs.AvailableClusters;
  *lpTotalNumberOfClusters = dfs.TotalClusters;

  return 1;
}

BOOL WINAPI
WINFS::GetDiskFreeSpace (LPCSTR lpRootPathName, LPDWORD lpSectorsPerCluster,
                         LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters,
                         LPDWORD lpTotalNumberOfClusters)
{
  BOOL r = ::GetDiskFreeSpace (lpRootPathName, lpSectorsPerCluster, lpBytesPerSector,
                               lpNumberOfFreeClusters, lpTotalNumberOfClusters);
  if (!r)
    {
      if (GetLastError () == ERROR_NOT_SUPPORTED)
        {
          *lpSectorsPerCluster = 1;
          *lpBytesPerSector = 4096;
        }
      else
        {
          if (!askpass (lpRootPathName))
            return 0;
          r = ::GetDiskFreeSpace (lpRootPathName, lpSectorsPerCluster, lpBytesPerSector,
                                  lpNumberOfFreeClusters, lpTotalNumberOfClusters);
          if (!r)
            return 0;
        }
    }

  if (GetDiskFreeSpaceEx)
    {
      if (!sysdep.WinNTp ()
          && GetDiskFreeSpaceFAT32 (lpRootPathName, lpSectorsPerCluster,
                                    lpBytesPerSector, lpNumberOfFreeClusters,
                                    lpTotalNumberOfClusters))
        return 1;

      uint64_t FreeBytesAvailableToCaller;
      uint64_t TotalNumberOfBytes;
      uint64_t TotalNumberOfFreeBytes;
      if (GetDiskFreeSpaceEx (lpRootPathName,
                              (PULARGE_INTEGER)&FreeBytesAvailableToCaller,
                              (PULARGE_INTEGER)&TotalNumberOfBytes,
                              (PULARGE_INTEGER)&TotalNumberOfFreeBytes))
        {
          DWORD blk = *lpSectorsPerCluster * *lpBytesPerSector;
          if (!blk)
            blk = 512;
          *lpTotalNumberOfClusters = DWORD (TotalNumberOfBytes / blk);
          *lpNumberOfFreeClusters = DWORD (TotalNumberOfFreeBytes / blk);
          r = 1;
        }
    }

  return r;
}

DWORD WINAPI
WINFS::internal_GetFileAttributes (LPCSTR lpFileName)
{
  WINFS_CALL1 (DWORD, -1, lpFileName, GetFileAttributes (lpFileName));
}

DWORD WINAPI
WINFS::GetFileAttributes (LPCSTR lpFileName)
{
  DWORD attr = internal_GetFileAttributes (lpFileName);
  if (attr == DWORD (-1) && GetLastError () != ERROR_INVALID_NAME)
    {
      WIN32_FIND_DATA fd;
      if (get_file_data (lpFileName, fd))
        attr = fd.dwFileAttributes;
    }
  return attr;
}

UINT WINAPI
WINFS::GetTempFileName (LPCSTR lpPathName, LPCSTR lpPrefixString, UINT uUnique, LPSTR lpTempFileName)
{
  WINFS_CALL1 (UINT, 0, lpPathName,
               GetTempFileName (lpPathName, lpPrefixString, uUnique, lpTempFileName));
}

BOOL WINAPI
WINFS::GetVolumeInformation (LPCSTR lpRootPathName, LPSTR lpVolumeNameBuffer,
                             DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber,
                             LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags,
                             LPSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize)
{
  WINFS_CALL1 (BOOL, FALSE, lpRootPathName,
               GetVolumeInformation (lpRootPathName, lpVolumeNameBuffer, nVolumeNameSize,
                                     lpVolumeSerialNumber, lpMaximumComponentLength,
                                     lpFileSystemFlags, lpFileSystemNameBuffer, nFileSystemNameSize));
}

HMODULE WINAPI
WINFS::LoadLibrary (LPCSTR lpLibFileName)
{
  WINFS_CALL1 (HMODULE, NULL, lpLibFileName, LoadLibrary (lpLibFileName));
}

static BOOL
move_file (LPCSTR lpExistingFileName, LPCSTR lpNewFileName)
{
  WINFS_CALL2 (BOOL, FALSE, lpExistingFileName, lpNewFileName,
               MoveFile (lpExistingFileName, lpNewFileName));
}

BOOL WINAPI
WINFS::MoveFile (LPCSTR lpExistingFileName, LPCSTR lpNewFileName)
{
  for (int retry = 0;; retry++)
    {
      if (move_file (lpExistingFileName, lpNewFileName))
        return 1;
      if (retry >= 3)
        return 0;
      Sleep (50);
    }
}

BOOL WINAPI
WINFS::RemoveDirectory (LPCSTR lpPathName)
{
  WINFS_CALL1 (BOOL, FALSE, lpPathName, RemoveDirectory (lpPathName));
}

BOOL WINAPI
WINFS::SetFileAttributes (LPCSTR lpFileName, DWORD dwFileAttributes)
{
  WINFS_CALL1 (BOOL, FALSE, lpFileName,
               SetFileAttributes (lpFileName, dwFileAttributes));
}

DWORD WINAPI
WINFS::internal_GetFullPathName (LPCSTR lpFileName, DWORD nBufferLength,
                                 LPSTR lpBuffer, LPSTR *lpFilePart)
{
  WINFS_MAPSL (lpFileName);
  WINFS_CALL1 (DWORD, 0, lpFileName,
               GetFullPathName (lpFileName, nBufferLength, lpBuffer, lpFilePart));
}

BOOL WINAPI
WINFS::SetCurrentDirectory (LPCSTR lpPathName)
{
  WINFS_MAPSL (lpPathName);
  WINFS_CALL1 (BOOL, FALSE, lpPathName, SetCurrentDirectory (lpPathName));
}

DWORD WINAPI
WINFS::GetFullPathName (LPCSTR path, DWORD size, LPSTR buf, LPSTR *name)
{
  DWORD l = internal_GetFullPathName (path, size, buf, name);
  if (!l || l >= size)
    return l;
  if (!dir_separator_p (*path) || !dir_separator_p (path[1]))
    return l;
  if (alpha_char_p (*buf & 0xff) && buf[1] == ':'
      && dir_separator_p (buf[2]) && dir_separator_p (buf[3]))
    {
      strcpy (buf, buf + 2);
      l -= 2;
      if (name && *name >= buf + 2)
        *name -= 2;
    }
  return l;
}

DWORD WINAPI
WINFS::WNetOpenEnum (DWORD dwScope, DWORD dwType, DWORD dwUsage,
                     LPNETRESOURCE lpNetResource, LPHANDLE lphEnum)
{
  if (!lpNetResource)
    return ::WNetOpenEnum (dwScope, dwType, dwUsage, lpNetResource, lphEnum);

  DWORD r = ::WNetOpenEnum (dwScope, dwType, dwUsage, lpNetResource, lphEnum);
  if (r != NO_ERROR && askpass_noshare (lpNetResource->lpRemoteName))
    r = ::WNetOpenEnum (dwScope, dwType, dwUsage, lpNetResource, lphEnum);
  return r;
}

int WINAPI
WINFS::get_file_data (const char *path, WIN32_FIND_DATA &fd)
{
  HANDLE h = FindFirstFile (path, &fd);
  if (h == INVALID_HANDLE_VALUE)
    return 0;
  FindClose (h);
  return 1;
}
