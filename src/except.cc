#include "stdafx.h"
#include "ed.h"
#include "except.h"
#include "signal.h"
#include "version.h"

#define MAX_LISP_CALL_STACK_DEPTH 64

struct lisp_call_stack
{
  int type;
  lisp object;
};

static lisp_call_stack lisp_call_stack_buf[MAX_LISP_CALL_STACK_DEPTH];

const Win32Exception::known_exception Win32Exception::known_excep[] =
{
  {EXCEPTION_ACCESS_VIOLATION, "Access violation"},
  {EXCEPTION_ARRAY_BOUNDS_EXCEEDED, "Array bounds exceeded"},
  {EXCEPTION_BREAKPOINT, "Breakpoint"},
  {EXCEPTION_DATATYPE_MISALIGNMENT, "Data type misalignment"},
  {EXCEPTION_FLT_DENORMAL_OPERAND, "Floating point denormal operand"},
  {EXCEPTION_FLT_DIVIDE_BY_ZERO, "Floating point divide by zero"},
  {EXCEPTION_FLT_INEXACT_RESULT, "Floating point inexact result"},
  {EXCEPTION_FLT_INVALID_OPERATION, "Floating point invalid operation"},
  {EXCEPTION_FLT_OVERFLOW, "Floating point overflow"},
  {EXCEPTION_FLT_STACK_CHECK, "Floating point stack check"},
  {EXCEPTION_FLT_UNDERFLOW, "Floating point underflow"},
  {EXCEPTION_GUARD_PAGE, "Guard page violation"},
  {EXCEPTION_ILLEGAL_INSTRUCTION, "Illegal instruction"},
  {EXCEPTION_IN_PAGE_ERROR, "In page error"},
  {EXCEPTION_INT_DIVIDE_BY_ZERO, "Integer divide by zero"},
  {EXCEPTION_INT_OVERFLOW, "Integer overflow"},
  {EXCEPTION_INVALID_DISPOSITION, "Invalid disposition"},
  {EXCEPTION_INVALID_HANDLE, "Invalid handle"},
  {EXCEPTION_NONCONTINUABLE_EXCEPTION, "Noncontinuable exception"},
  {EXCEPTION_PRIV_INSTRUCTION, "Privileged instruction"},
  {EXCEPTION_SINGLE_STEP, "Single step"},
  {EXCEPTION_STACK_OVERFLOW, "Stack overflow"},
};

static const char*
get_exception_description (u_int code)
{
  for (int i = 0; i < numberof (Win32Exception::known_excep); i++)
    if (code == Win32Exception::known_excep[i].code)
      return Win32Exception::known_excep[i].desc;
  return "Unknown exception";
}


EXCEPTION_RECORD Win32Exception::r;
CONTEXT Win32Exception::c;
u_int Win32Exception::code;

inline
Win32Exception::Win32Exception (u_int code_, const EXCEPTION_POINTERS *ep)
{
  code = code_;
  r = *ep->ExceptionRecord;
  c = *ep->ContextRecord;
}

void Win32Exception::throw_lisp_error ()
{
  switch (code)
    {
    case EXCEPTION_BREAKPOINT:
    case EXCEPTION_NONCONTINUABLE_EXCEPTION:
    case EXCEPTION_SINGLE_STEP:
      return;
    default:
      const char* desc = get_exception_description (code);
      FEwin32_exception (desc, code, r.ExceptionAddress);
    }
}

void __cdecl
se_handler (u_int code, EXCEPTION_POINTERS *ep)
{
  int i = 0;
  for (stack_trace *p = stack_trace::stp; p; p = p->last)
    if (p->type != stack_trace::empty)
      {
        if (i == MAX_LISP_CALL_STACK_DEPTH)
          break;
        lisp_call_stack_buf[i].type = p->type;
        lisp_call_stack_buf[i].object = p->fn;
        i++;
      }

#ifdef DEBUG
  if (code == EXCEPTION_IN_PAGE_ERROR)
    throw Win32Exception (code, ep);
#else
# if 0
  for (int i = 0; i < numberof (Win32Exception::known_excep); i++)
    if (code == Win32Exception::known_excep[i].code)
      throw Win32Exception (code, ep);
# else
  throw Win32Exception (code, ep);
# endif
#endif
}

static int
get_section_name (void *base, void *p, char *buf, int size)
{
  unsigned long long nread;
  IMAGE_DOS_HEADER dos;
  if (!ReadProcessMemory (GetCurrentProcess (),
                          base, &dos, sizeof dos, &nread))
    return 0;

  if (dos.e_magic != IMAGE_DOS_SIGNATURE)
    return 0;

  IMAGE_NT_HEADERS nt;
  if (!ReadProcessMemory (GetCurrentProcess (),
                          (char *)base + dos.e_lfanew,
                          &nt, sizeof nt, &nread))
    return 0;
  if (nt.Signature != IMAGE_NT_SIGNATURE)
    return 0;

  unsigned long long rva = unsigned long long (p) - unsigned long long (base);

  IMAGE_SECTION_HEADER *section =
    (IMAGE_SECTION_HEADER *)((char *)base + dos.e_lfanew
                             + offsetof (IMAGE_NT_HEADERS, OptionalHeader)
                             + nt.FileHeader.SizeOfOptionalHeader);
  for (int i = 0; i < nt.FileHeader.NumberOfSections; i++, section++)
    {
      IMAGE_SECTION_HEADER sec;
      if (!ReadProcessMemory (GetCurrentProcess (), section,
                              &sec, sizeof sec, &nread))
        continue;
      if (rva >= sec.VirtualAddress
          && rva < sec.VirtualAddress + max (sec.SizeOfRawData, sec.Misc.VirtualSize))
        {
          int l = min ((int)sizeof sec.Name, size - 1);
          memcpy (buf, sec.Name, l);
          buf[l] = 0;
          return 1;
        }
    }
  return 0;
}

int
get_module_base_name (HMODULE h, LPSTR buf, DWORD size)
{
  if (!GetModuleFileName (h, buf, size))
    return 0;
  char *p = jrindex (buf, '\\');
  if (p)
    strcpy (buf, p + 1);
  int l = (int) strlen (buf);
  if (l >= 4 && !_stricmp (buf + l - 4, ".dll"))
    buf[l - 4] = 0;
  return 1;
}

static int
get_module_name (unsigned long long addr, MEMORY_BASIC_INFORMATION *bi, char *buf)
{
  switch (bi->AllocationProtect & ~(PAGE_GUARD | PAGE_NOCACHE))
    {
    case PAGE_READONLY:
    case PAGE_READWRITE:
    case PAGE_WRITECOPY:
    case PAGE_EXECUTE:
    case PAGE_EXECUTE_READ:
    case PAGE_EXECUTE_READWRITE:
    case PAGE_EXECUTE_WRITECOPY:
    case PAGE_NOACCESS:
      break;

    default:
      return 0;
    }

  char path[512];
  if (!get_module_base_name (HMODULE (bi->AllocationBase), path, sizeof path))
    return 0;

  strcpy (buf, path);
  if (get_section_name (bi->AllocationBase,
                        bi->BaseAddress,
                        path, sizeof path))
    strcpy (stpcpy (buf + strlen (buf), "!"), path);
  return 1;
}

static int
find_module_name (void *addr, char *buf)
{
  SYSTEM_INFO si;
  GetSystemInfo (&si);
  addr = (void *)((unsigned long long) addr & ~(unsigned long long) si.dwPageSize);

  MEMORY_BASIC_INFORMATION bi;
  memset (&bi, 0, sizeof bi);
  return (VirtualQuery (addr, &bi, sizeof bi)
          && get_module_name ((unsigned long long) addr, &bi, buf));
}

static void
print_modules (FILE *fp, unsigned long long addr, MEMORY_BASIC_INFORMATION *bi)
{
  switch (bi->AllocationProtect & ~(PAGE_GUARD | PAGE_NOCACHE))
    {
    case PAGE_READONLY:
    case PAGE_READWRITE:
    case PAGE_WRITECOPY:
    case PAGE_EXECUTE:
    case PAGE_EXECUTE_READ:
    case PAGE_EXECUTE_READWRITE:
    case PAGE_EXECUTE_WRITECOPY:
    case PAGE_NOACCESS:
      break;

    default:
      return;
    }

  char path[MAX_PATH + IMAGE_SIZEOF_SHORT_NAME + 2];
  if (!get_module_base_name (HMODULE (bi->AllocationBase), path, MAX_PATH))
    return;
  char *p = path + lstrlen (path);
  if (get_section_name (bi->AllocationBase, bi->BaseAddress, p + 1, (int)(path + sizeof path - p - 1)))
    *p = '!';
  fprintf (fp, "%16llx - %16llx: %s\n", addr, addr + bi->RegionSize, path);
}

static void
print_module_allocation (FILE *fp)
{
  for (unsigned long long addr = 0; addr < 0xffffffff;)
    {
      MEMORY_BASIC_INFORMATION bi;
      if (VirtualQuery ((void *)addr, &bi, sizeof bi))
        print_modules (fp, addr, &bi);
      else
        bi.RegionSize = 0;
      unsigned long long oaddr = addr;
      addr += bi.RegionSize ? bi.RegionSize : 64 * 1024;
      if (addr < oaddr)
        break;
    }
  putc ('\n', fp);
}

#ifdef _M_X64
static void
x64_print_registers (FILE *fp, const CONTEXT &c)
{
  fprintf (fp, "Registers:\n");
  fprintf (fp, "RAX: %16llx  RBX: %16llx  RCX: %16llx  RDX: %16llx  RSI: %16llx\n",
           c.Rax, c.Rbx, c.Rcx, c.Rdx, c.Rsi);
  fprintf (fp, "RDI: %16llx  RSP: %16llx  RBP: %16llx  RIP: %16llx  EFL: %8lx\n",
           c.Rdi, c.Rsp, c.Rbp, c.Rip, c.EFlags);
  fprintf (fp, "CS: %04x  DS: %04x  ES: %04x  SS: %04x  FS: %04x  GS: %04x\n\n",
           c.SegCs, c.SegDs, c.SegEs, c.SegSs, c.SegFs, c.SegGs);

  unsigned long long rip = c.Rip - 16;
  for (int j = 0; j < 2; j++)
    {
      fprintf (fp, "%16llx:", rip);
      for (int i = 0; i < 16; i++, rip++)
        {
          if (IsBadReadPtr ((void *)rip, 1))
            fprintf (fp, " ??");
          else
            fprintf (fp, " %02x", *(u_char *)rip);
        }
      putc ('\n', fp);
    }
  putc ('\n', fp);
}

static void
x64_stack_dump (FILE *fp, const CONTEXT &c)
{
  fprintf (fp, "Stack dump:\n");
  unsigned long long rsp = c.Rsp, rbp = c.Rbp;

  for (int i = 0; i < 64; i++)
    {
      unsigned long long buf[16], nread;
      if (!ReadProcessMemory (GetCurrentProcess (), (void *)rsp,
                              buf, sizeof buf, &nread)
          || nread != sizeof buf)
        break;
      for (int j = 0; j < 16; j += 4)
        fprintf (fp, "%16llx: %16llx %16llx %16llx %16llx\n",
                 rsp + j * 4, buf[j], buf[j + 1], buf[j + 2], buf[j + 3]);
      fprintf (fp, "\n");
      if (rbp <= rsp || rbp & 3)
        break;
      rsp = rbp;
      if (!ReadProcessMemory (GetCurrentProcess (), (void *)rsp,
                              &rbp, sizeof rbp, &nread))
        rbp = 0;
//      esp += sizeof ebp;
    }
}

#endif /* _M_X64 */

static int
bad_object_p (FILE *fp, lisp object)
{
  if (!IsBadReadPtr (object, sizeof object))
    return 0;
  fprintf (fp, "(???)\n");
  return 1;
}

static void
print_object (FILE *fp, lisp object, int f)
{
  if (f)
    putc ('(', fp);
  if (!bad_object_p (fp, object))
    {
      if (closurep (object))
        object = xclosure_body (object);
      if (!bad_object_p (fp, object))
        {
          if (consp (object))
            {
              if (bad_object_p (fp, xcar (object)))
                ;
              else if (xcar (object) == Qlambda)
                fprintf (fp, "(lambda sexp)");
              else if (xcar (object) == Qmacro)
                fprintf (fp, "(macro sexp)");
              else
                fprintf (fp, "(...)");
            }
          else
            {
              if (symbolp (object))
                object = xsymbol_name (object);
              else if (functionp (object))
                object = xfunction_name (object);
              if (bad_object_p (fp, object))
                ;
              else if (stringp (object))
                {
                  const Char *p = xstring_contents (object);
                  const Char *const pe = p + xstring_length (object);
                  if (IsBadStringPtr ((char *)p, sizeof *p * xstring_length (object)))
                    fprintf (fp, "(Invalid String)");
                  else
                    for (; p < pe; p++)
                      {
                        if (DBCP (*p))
                          putc (*p >> 8, fp);
                        putc (*p, fp);
                      }
                }
              else
                fprintf (fp, "...");
            }
        }
    }

  if (f)
    fprintf (fp, " calculating arguments...)");
  putc ('\n', fp);
}

static void
lisp_stack_trace (FILE *fp)
{
  fprintf (fp, "Lisp stack trace:\n");
  for (lisp_call_stack *p = lisp_call_stack_buf, *pe = p + MAX_LISP_CALL_STACK_DEPTH;
       p < pe; p++)
    switch (p->type)
      {
      case stack_trace::empty:
        return;

      case stack_trace::special_form:
      case stack_trace::macro:
      case stack_trace::apply:
        print_object (fp, p->object, 0);
        break;

      case stack_trace::eval_args:
        print_object (fp, p->object, 1);
        break;
      }
}

void
cleanup_exception ()
{
  const char* desc = get_exception_description (Win32Exception::code);
  char path[PATH_MAX];
  GetModuleFileName (0, path, PATH_MAX);
  int l = (int) strlen (path);
  if (l >= 4 && !_stricmp (path + l - 4, ".exe"))
    strcpy (path + l - 4, ".BUG");
  else
    strcat (path, ".BUG");

  char module[1024];
  if (!find_module_name (Win32Exception::r.ExceptionAddress, module))
    *module = 0;

  FILE *fp = fopen (path, "w");
  if (!fp && GetTempPath (sizeof path, path))
    {
      char *p = find_last_slash (path);
      if (!p || p[1])
        strcat (path, "\\");
      strcat (path, "xyzzy.BUG");
      fp = fopen (path, "w");
    }
  if (fp)
    {
      fprintf (fp, "%s %s Crash log:\n\n", ProgramName, VersionString);

      fprintf (fp, "Windows %s %d.%02d.%d %s\n\n",
               sysdep.windows_name,
               sysdep.os_ver.dwMajorVersion,
               sysdep.os_ver.dwMinorVersion,
               sysdep.os_ver.dwBuildNumber,
               sysdep.os_ver.szCSDVersion);

      fprintf (fp, "%08x: %s\n", Win32Exception::code, desc);
      fprintf (fp, "at %16llx", (unsigned long long) Win32Exception::r.ExceptionAddress);
      if (*module)
        fprintf (fp, " (%s)", module);
      fprintf (fp, "\n\n");

#ifdef _M_X64
      x64_print_registers (fp, Win32Exception::c);
      x64_stack_dump (fp, Win32Exception::c);
#else
# error "yet"
#endif
      fprintf (fp, "Initial stack: %16llx  GC: %d\n\n",
               (unsigned long long) app.initial_stack, app.in_gc);

      print_module_allocation (fp);
      lisp_stack_trace (fp);
#ifdef DEBUG_GC
      putc ('\n', fp);
      output_funcall_mark (fp);
#endif
      fclose (fp);
    }

  char msg[1024], *p = msg;
  p += sprintf (p, "�v���I�ȗ�O(%s)���������܂����B\nat %16llx",
                desc, (unsigned long long) Win32Exception::r.ExceptionAddress);
  if (*module)
    p += sprintf (p, " (%s)", module);
  *p++ = '\n';
  *p++ = '\n';
  if (fp)
    p += sprintf (p,
                  "�C����������ȉ��̃t�@�C����Y���č�҂ɕ񍐂��Ă��������B\n"
                  "\n%s\n\n"
                  "���̍ہA�ǂ̂悤�ȑ�����������A�܂��A������������čČ���\n"
                  "�邩�ǂ����Ȃǂ����킹�ĕ񍐂��Ă��������B�Ȃ��AIME�̑���\n"
                  "���ɔ����������̂ōČ���������ꍇ�́A���̃A�v���P�[�V����\n"
                  "(�������Ȃ�)�ł��Č����邩�ǂ������m�F���Ă݂Ă��������B"
                  "\n\n",
                  path);
  strcpy (p,
          "�^���悯��΁A���������̃t�@�C�����~���邩������܂���B\n"
          "�����Ɏ����Z�[�u���Ă݂܂���?");

  if (MsgBox (get_active_window (), msg, TitleBarString,
              MB_ICONHAND | MB_YESNO, 1) != IDYES)
    return;

  try
    {
      do_auto_save (0, 1);
    }
  catch (nonlocal_jump &)
    {
    }
}
