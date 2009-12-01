#ifndef _vfs_h_
#define _vfs_h_

class WINFS
{
protected:
  typedef BOOL (WINAPI *GETDISKFREESPACEEX)(LPCTSTR, PULARGE_INTEGER,
                                            PULARGE_INTEGER, PULARGE_INTEGER);
  static const GETDISKFREESPACEEX GetDiskFreeSpaceEx;

  static DWORD WINAPI internal_GetFullPathName (LPCSTR lpFileName, DWORD nBufferLength,
                                                LPSTR lpBuffer, LPSTR *lpFilePart);
  static DWORD WINAPI internal_GetFileAttributes (LPCSTR lpFileName);
public:
  static char wfs_share_cache[MAX_PATH * 2];

  static void clear_share_cache () {*wfs_share_cache = 0;}

  static BOOL WINAPI CreateDirectory (LPCSTR lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  static HANDLE WINAPI CreateFile (LPCSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode,
                                   LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition,
                                   DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
  static BOOL WINAPI DeleteFile (LPCSTR lpFileName);
  static HANDLE WINAPI FindFirstFile (LPCSTR lpFileName, LPWIN32_FIND_DATAA lpFindFileData);
  static BOOL WINAPI FindNextFile (HANDLE hFindFile, LPWIN32_FIND_DATAA lpFindFileData);
  static BOOL WINAPI GetDiskFreeSpace (LPCSTR lpRootPathName, LPDWORD lpSectorsPerCluster,
                                       LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters,
                                       LPDWORD lpTotalNumberOfClusters);
  static DWORD WINAPI GetFileAttributes (LPCSTR lpFileName);
  static DWORD WINAPI GetFullPathName (LPCSTR lpFileName, DWORD nBufferLength, LPSTR lpBuffer, LPSTR *lpFilePart);
  static UINT WINAPI GetTempFileName (LPCSTR lpPathName, LPCSTR lpPrefixString, UINT uUnique, LPSTR lpTempFileName);
  static BOOL WINAPI GetVolumeInformation (LPCSTR lpRootPathName, LPSTR lpVolumeNameBuffer,
                                           DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber,
                                           LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags,
                                           LPSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize);
  static HMODULE WINAPI LoadLibrary (LPCSTR lpLibFileName);
  static BOOL WINAPI MoveFile (LPCSTR lpExistingFileName, LPCSTR lpNewFileName);
  static BOOL WINAPI RemoveDirectory (LPCSTR lpPathName);
  static BOOL WINAPI SetCurrentDirectory (LPCSTR lpPathName);
  static BOOL WINAPI SetFileAttributes (LPCSTR lpFileName, DWORD dwFileAttributes);
  static DWORD WINAPI WNetOpenEnum (DWORD dwScope, DWORD dwType, DWORD dwUsage,
                                    LPNETRESOURCE lpNetResource, LPHANDLE lphEnum);

  static int WINAPI get_file_data (const char *, WIN32_FIND_DATA &);
};

#endif /* _vfs_h_ */
