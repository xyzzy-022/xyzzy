#include "stdafx.h"
#include "arc-if.h"
#include "vfs.h"

const char *ArchiverInterface::ai_names[] =
{
  "GetVersion",
  "GetRunning",
  "GetBackGroundMode",
  "SetBackGroundMode",
  "",
  "CheckArchive",
  "GetFileCount",
  "OpenArchive",
  "CloseArchive",
  "FindFirst",
  "FindNext",
  "SetOwnerWindow",
  "ClearOwnerWindow",
  "GetSubVersion",
  "ConfigDialog",
};

ArchiverInterface::ArchiverInterface (const char *module, const char *prefix)
     : ai_hmodule (0), ai_module_name (module), ai_prefix (prefix)
{
  memset (ai_fns, 0, sizeof ai_fns);
}

FARPROC
ArchiverInterface::getfn (int i) const
{
  if (!ai_hmodule)
    return 0;
  if (ai_fns[i])
    return ai_fns[i];
#ifdef COMMARC_EMULATE_FNS
  const FARPROC *fns = emulate_fns ();
  if (fns && fns[i])
    return fns[i];
#endif
  char buf[256];
  strcat (strcpy (buf, ai_prefix), ai_names[i]);
  const_cast <FARPROC *> (ai_fns)[i] = GetProcAddress (ai_hmodule, buf);
  return ai_fns[i];
}

ArchiverInterface::lock::lock (const ArchiverInterface &ai)
     : l_ai (const_cast <ArchiverInterface &> (ai)), l_omodule (ai.ai_hmodule)
{
  if (l_omodule)
    return;
  UINT omode = SetErrorMode (SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);
  l_ai.ai_hmodule = WINFS::LoadLibrary (l_ai.ai_module_name);
  SetErrorMode (omode);
  if (l_ai.ai_hmodule)
    l_ai.patch_module ((void *)l_ai.ai_hmodule);
}

ArchiverInterface::lock::~lock ()
{
  if (!l_omodule)
    {
      FreeLibrary (l_ai.ai_hmodule);
      memset (l_ai.ai_fns, 0, sizeof l_ai.ai_fns);
    }
  l_ai.ai_hmodule = l_omodule;
}

#ifdef COMMARC_EMULATE_FNS
/*
    Name          Original    Packed  Ratio   Date     Time   Attr Method   CRC-32
--------------  --------  -------- ------ -------- -------- ---- -------- --------
shaslfjashflfhajahfasjfhaslfahffhafquweroqpwerhsff         0         0   0.5% 98-07-12  07:23:42 a--w Stored  00000000
uweroqpwerhsff         0         0   0.5% 98-07-12  07:23:42 a--w Stored  00000000
testunrar.rar      62459     61989   1.3% 98-03-06  13:50:34 a--w Deflate bb161e23
UNPACK.EXE         32768     10198  69.4% 97-05-26  22:41:14 a--w Deflate f80d60e2
UnPack.frm         12031      3370  72.5% 98-04-03  23:27:58 a--w Deflate 39ff662b
UNPACK.FRM.1~      11268      3232  71.8% 97-05-26  03:22:30 a--w Deflate 4da92596
UnPack.frm.2~      11732      3308  72.3% 98-03-09  05:54:56 a--w Deflate 5a9a1825
UNPACK.FRX            11        11   0.5% 98-04-03  23:27:58 a--w Stored  3201c5a7
UNPACK.TXT          5186      2431  53.6% 97-08-24  03:39:00 a--w Deflate 78fe7f18
--------------  --------  -------- ------ -------- --------
     8 files      135455     84539   37.6%

      pwerhsff         0         0   0.5% 98-07-12  07:23:42 a--w Stored  00000000
      6543210987654321098765432109876543210987654321098765432109876543210987654321
            7         6         5         4         3         2         1
 */

#define ZIPBUFSIZE (128 * 1024)
static char *zip_buffer;
static char *zip_next;

static HARC WINAPI
zip_open (HWND hwnd, const char *filename, DWORD mode)
{
  char cmdline[1024];
  sprintf (cmdline, "-l \"%s\"", filename);
  if (!zip_buffer)
    zip_buffer = (char *)malloc (ZIPBUFSIZE + 1);
  zip_next = 0;
  if (!zip_buffer)
    return 0;
  *zip_buffer = 0;
  UnzipInterface zi;
  if (!zi.doit (hwnd, cmdline, zip_buffer, ZIPBUFSIZE))
    {
      zip_buffer[ZIPBUFSIZE] = 0;
      zip_next = strchr (zip_buffer, '\n');
      if (zip_next)
        zip_next = strchr (zip_next + 1, '\n');
    }
  return HARC (zip_next);
}

static int WINAPI
zip_close (HARC)
{
  if (zip_buffer)
    {
      free (zip_buffer);
      zip_buffer = 0;
    }
  return 0;
}

static inline int
atoi2 (const char *p)
{
  return (*p - '0') * 10 + p[1] - '0';
}

static int WINAPI
zip_next_file (HARC, INDIVIDUALINFO *v)
{
  if (!zip_next)
    return -1;
  char *p0 = zip_next + 1;
  zip_next = strchr (p0, '\n');
  if (!zip_next)
    return -1;
  if (zip_next - p0 < 82)
    {
      zip_next = 0;
      return -1;
    }
  if (!v)
    return 0;

  v->dwCRC = strtol (zip_next - 8, 0, 16);
  memcpy (v->szAttribute, zip_next - 21, 4);
  v->szAttribute[4] = 0;
  v->wTime = ((atoi2 (zip_next - 30) << 11)
              + (atoi2 (zip_next - 27) << 5)
              + (atoi2 (zip_next - 24) >> 1));
  int y = atoi2 (zip_next - 40);
  if (y >= 80)
    y -= 80;
  else
    y += 20;
  v->wDate = ((y << 9)
              + (atoi2 (zip_next - 37) << 5)
              + atoi2 (zip_next - 34));
  v->dwOriginalSize = atoi (zip_next - 68);
  char *p;
  for (p = &zip_next[-68]; p > p0 && p[-1] == ' '; p--)
    ;
  *p = 0;
  strcpy (v->szFileName, p0);
  return 0;
}

static int WINAPI
zip_first_file (HARC, const char *, INDIVIDUALINFO *v)
{
  return zip_next_file (0, v);
}

FARPROC UnzipInterface::unzip_emulate_fns[MAX_METHOD];
#endif /* COMMARC_EMULATE_FNS */

#define UNZIP_GOOD_VERSION 97

static int __stdcall fake_MessageBeep (UINT) {return 1;}

#define P(PTR, OFFSET) ((char *)(PTR) + (long)(OFFSET))

int
UnzipInterface::patch_module (void *base) const
{
  if (get_version () >= UNZIP_GOOD_VERSION)
    return 1;

#ifdef COMMARC_EMULATE_FNS
  unzip_emulate_fns[METHOD_INDEX (open)] = FARPROC (zip_open);
  unzip_emulate_fns[METHOD_INDEX (close)] = FARPROC (zip_close);
  unzip_emulate_fns[METHOD_INDEX (find_first)] = FARPROC (zip_first_file);
  unzip_emulate_fns[METHOD_INDEX (find_next)] = FARPROC (zip_next_file);
#endif

  IMAGE_DOS_HEADER *dos = (IMAGE_DOS_HEADER *)base;
  if (IsBadReadPtr (dos, sizeof *dos)
      || dos->e_magic != IMAGE_DOS_SIGNATURE)
    return 0;

  IMAGE_NT_HEADERS *nt = (IMAGE_NT_HEADERS *)P (dos, dos->e_lfanew);
  if (IsBadReadPtr (nt, sizeof *nt)
      || nt->Signature != IMAGE_NT_SIGNATURE)
    return 0;

  IMAGE_IMPORT_DESCRIPTOR *desc;
  desc = (IMAGE_IMPORT_DESCRIPTOR *)
    P (base,
       nt->OptionalHeader.DataDirectory
       [IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
  if (desc == (IMAGE_IMPORT_DESCRIPTOR *)nt)
    return 0;

  FARPROC beep = GetProcAddress (GetModuleHandle ("user32"), "MessageBeep");
  if (!beep)
    return 0;

  for (; desc->Name; desc++)
    for (IMAGE_THUNK_DATA *thunk = (IMAGE_THUNK_DATA *)P(base, desc->FirstThunk);
         thunk->u1.Function; thunk++)
      if ((DWORD)thunk->u1.Function == (DWORD)beep)
        {
          DWORD o;
          VirtualProtect (&thunk->u1.Function, 4, PAGE_READWRITE, &o);
          *(DWORD *)&thunk->u1.Function = DWORD (fake_MessageBeep);
          VirtualProtect (&thunk->u1.Function, 4, o, &o);
          return 1;
        }
  return 0;
}
