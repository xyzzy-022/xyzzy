#include "stdafx.h"
#include "ed.h"
#include "dyn-handle.h"
#include "sockinet.h"
#include "byte-stream.h"
#include "mainframe.h"

class EnvStrings
{
  char *e_env;
  char *e_buf;

  void set (char **, char **&, char *) const;
  char *set (char **, char **&, char *, lisp, lisp) const;
  static int __cdecl compare (const void *p1, const void *p2)
    {return stricmp (*(char **)p1, *(char **)p2);}
public:
  EnvStrings () : e_env (0), e_buf (0) {}
  ~EnvStrings ()
    {
      xfree (e_buf);
      xfree (e_env);
    }
  void setup (lisp);
  const char *str () const {return e_env;}
};

void
EnvStrings::set (char **nb, char **&ne, char *b) const
{
  char *eq = b;
  if (*eq == '=')
    eq++;
  eq = strchr (eq, '=');
  if (!eq)
    return;
  int l = eq - b + 1;
  for (; nb < ne; nb++)
    if (!memicmp (b, *nb, l))
      {
        *nb = b[l] ? b : "";
        return;
      }
  if (b[l])
    *ne++ = b;
}

char *
EnvStrings::set (char **nb, char **&ne, char *b, lisp var, lisp val) const
{
  char *b0 = b;
  b = w2s (b, var);
  *b++ = '=';
  if (val == Qnil)
    *b++ = 0;
  else
    b = w2s (b, val) + 1;
  set (nb, ne, b0);
  return b;
}

void
EnvStrings::setup (lisp lenv)
{
  int n = 0, l = 0;
  for (lisp le = lenv; consp (le); le = xcdr (le), n++)
    {
      lisp x = xcar (le);
      check_cons (x);
      check_string (xcar (x));
      l += xstring_length (xcar (x)) * 2 + 2;
      if (xcdr (x) != Qnil)
        {
          check_string (xcdr (x));
          l += xstring_length (xcdr (x)) * 2;
        }
    }

  for (int d = 0; d < 26; d++)
    {
      const char *dir = get_device_dir (d);
      int x = strlen (dir);
      if (x > 3)
        {
          l += x + sizeof "=X:=X:";
          n++;
        }
    }

  for (char **e = environ; *e; e++, n++)
    ;

  l = (l + sizeof (char **) - 1) / sizeof (char **) * sizeof (char **);
  e_buf = (char *)xmalloc (l + sizeof (char **) * n);
  char **nb = (char **)(e_buf + l);
  char **ne = nb;
  for (char **e = environ; *e; e++, ne++)
    *ne = *e;

  char *b = e_buf;
  for (lisp le = lenv; consp (le); le = xcdr (le))
    {
      lisp x = xcar (le);
      b = set (nb, ne, b, xcar (x), xcdr (x));
    }

  for (int d = 0; d < 26; d++)
    {
      const char *dir = get_device_dir (d);
      int x = strlen (dir);
      if (x > 3)
        {
          char *b0 = b;
          b += sprintf (b, "=%c:=%c:%s", 'A' + d, 'A' + d, dir) + 1;
          set (nb, ne, b0);
        }
    }

  qsort (nb, ne - nb, sizeof *nb, compare);

  l = 1;
  for (char **np = nb; np < ne; np++)
    if (**np)
      l += strlen (*np) + 1;

  e_env = (char *)xmalloc (l);
  char *p = e_env;
  for (char **np = nb; np < ne; np++)
    if (**np)
      p = stpcpy (p, *np) + 1;
  *p = 0;
}

static void
path_arg (int no_std_handles, lisp lpath, char *path)
{
  if (no_std_handles)
    *path = 0;
  else if (stringp (lpath))
    pathname2cstr (lpath, path);
  else if (lpath == Qnil)
    strcpy (path, "nul");
  else
    *path = 0;
}

static void
open_for_read (dyn_handle &dh, const char *path, lisp lpath,
               SECURITY_ATTRIBUTES *sa)
{
  if (!*path)
    return;
  dh.fix (WINFS::CreateFile (path, GENERIC_READ,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, sa,
                             OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (!dh.valid ())
    file_error (GetLastError (), lpath);
}

static void
open_for_write (dyn_handle &dh, const char *path, lisp lpath,
                SECURITY_ATTRIBUTES *sa)
{
  if (!*path)
    return;
  dh.fix (WINFS::CreateFile (path, GENERIC_WRITE,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, sa, CREATE_ALWAYS,
                             FILE_ATTRIBUTE_ARCHIVE | FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (dh.valid ())
    return;
  dh.fix (WINFS::CreateFile (path, GENERIC_WRITE,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, sa,
                             OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0));
  if (!dh.valid ())
    file_error (GetLastError (), lpath);
}

static int
show_window_parameter (lisp lshow, int defalt)
{
  if (lshow == Kshow)
    return SW_SHOWNORMAL;
  else if (lshow == Kno_active)
    return SW_SHOWNA;
  else if (lshow == Kmaximize)
    return SW_SHOWMAXIMIZED;
  else if (lshow == Khide)
    return SW_HIDE;
  else if (lshow == Kminimize)
    return SW_SHOWMINNOACTIVE;
  else
    return defalt;
}

lisp
Fcall_process (lisp cmd, lisp keys)
{
  check_string (cmd);

  EnvStrings env;
  env.setup (find_keyword (Kenviron, keys));

  char *cmdline = (char *)alloca (xstring_length (cmd) * 2 + 1);
  w2s (cmdline, cmd);

  int no_std_handles = find_keyword_bool (Kno_std_handles, keys);

  lisp lstdin = find_keyword (Kinput, keys);
  lisp lstdout = find_keyword (Koutput, keys);
  lisp lstderr = find_keyword (Kerror, keys, 0);
  if (!lstderr)
    lstderr = lstdout;
  char infile[PATH_MAX + 1], outfile[PATH_MAX + 1], errfile[PATH_MAX + 1];
  path_arg (no_std_handles, lstdin, infile);
  path_arg (no_std_handles, lstdout, outfile);
  path_arg (no_std_handles, lstderr, errfile);

  lisp exec_dir = find_keyword (Kexec_directory, keys);
  if (exec_dir == Qnil)
    exec_dir = selected_buffer ()->ldirectory;
  char dir[PATH_MAX + 1];
  pathname2cstr (exec_dir, dir);
  map_sl_to_backsl (dir);

  lisp lshow = find_keyword (Kshow, keys);
  int show = show_window_parameter (lshow, SW_SHOWNORMAL);

  lisp wait = find_keyword (Kwait, keys);
  if (wait != Qnil && !realp (wait))
    wait = Qt;

  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof sa;
  sa.lpSecurityDescriptor = 0;
  sa.bInheritHandle = 1;

  dyn_handle hin, hout, herr;
  open_for_read (hin, infile, lstdin, &sa);
  open_for_write (hout, outfile, lstdout, &sa);
  if (lstdout != lstderr)
    open_for_write (herr, errfile, lstderr, &sa);

  STARTUPINFO si;
  bzero (&si, sizeof si);
  si.cb = sizeof si;
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = show;
  if (!no_std_handles)
    {
      si.dwFlags |= STARTF_USESTDHANDLES;
      si.hStdInput = hin.valid () ? (HANDLE)hin : GetStdHandle (STD_INPUT_HANDLE);
      si.hStdOutput = hout.valid () ? (HANDLE)hout : GetStdHandle (STD_OUTPUT_HANDLE);
      si.hStdError = (lstdout != lstderr
                      ? herr.valid () ? (HANDLE)herr : GetStdHandle (STD_ERROR_HANDLE)
                      : hout.valid () ? (HANDLE)hout : GetStdHandle (STD_ERROR_HANDLE));
    }

  WINFS::SetCurrentDirectory (dir);

  PROCESS_INFORMATION pi;
  int result = CreateProcess (0, cmdline, 0, 0, !no_std_handles,
                              (CREATE_DEFAULT_ERROR_MODE
                               /*| CREATE_NEW_PROCESS_GROUP*/
                               | NORMAL_PRIORITY_CLASS),
                              (void *)env.str (), dir, &si, &pi);
  int error = GetLastError ();

  w2s (dir, xsymbol_value (Qdefault_dir));
  WINFS::SetCurrentDirectory (dir);

  DWORD exit_code = 0;
  if (!result)
    FEsimple_win32_error (error, cmd);

  CloseHandle (pi.hThread);
  if (wait == Qt)
    {
      Char cc = 0;
      temporary_string tem (&cc, 0);
      Fsi_minibuffer_message (tem.string (), Qt);
      wait_process_terminate (pi.hProcess);
      GetExitCodeProcess (pi.hProcess, &exit_code);
      Fsi_minibuffer_message (Qnil, Qnil);
    }
  else if (wait != Qnil)
    {
      double w = coerce_to_double_float (wait);
      DWORD d = min (DWORD (w * 1000), 300000UL);
      if (d > 0)
        WaitForInputIdle (pi.hProcess, d);
    }
  CloseHandle (pi.hProcess);

  return wait == Qt ? make_fixnum (exit_code) : Qt;
}

class Process
{
protected:
  struct read_data
    {
      const Char *data;
      int size;
      int done;
    };

  Buffer *p_bufp;
  lisp p_proc;
  lisp p_filter;
  lisp p_sentinel;
  lisp p_last_incode;
  lisp p_marker;
  xbuffered_read_stream *p_input_stream;
  StrBuf p_osbuf;
  CRITICAL_SECTION p_cri;
  int p_in_send_string;
  int p_pending;

  Process (Buffer *bp, lisp pl, lisp marker);
  virtual u_int read_process () = 0;

  static u_int __stdcall read_process (void *p)
    {return ((Process *)p)->read_process ();}

  void read_process_output ();
  void read_process_output (xbuffered_read_stream &, class process_output_stream &);
  void terminated (int);

  void notify_term () const
    {PostMessage (app.toplev, WM_PRIVATE_PROCESS_TERMINATE, 0, LPARAM (this));}

public:
  virtual ~Process ();
  virtual void wait_terminate () = 0;
  virtual void signal () = 0;
  virtual void kill () = 0;
  virtual void send (const char *, int) const = 0;
  void insert_process_output (void *);
  lisp process_buffer () const {return p_bufp->lbp;}
  void flush_input ();
  void store_output (const Char *, int);
  virtual int readin (u_char *, int) = 0;
  int incode_modified_p () const
    {return xprocess_incode (p_proc) != p_last_incode;}
  eol_code eolcode () const {return xprocess_eol_code (p_proc);}
  lisp &filter () {return p_filter;}
  lisp &sentinel () {return p_sentinel;}
  lisp &marker () {return p_marker;}
  static lisp make_process_marker (Buffer *bp)
    {
      lisp marker = Fmake_marker (bp->lbp);
      xmarker_point (marker) = bp->b_contents.p2;
      return marker;
    }
  int &in_send_string_p () {return p_in_send_string;}
  void end_send_string ()
    {
      if (!p_pending)
        {
          EnterCriticalSection (&p_cri);
          int empty_p = p_osbuf.empty_p ();
          LeaveCriticalSection (&p_cri);
          if (!empty_p)
            {
              PostMessage (app.toplev, WM_PRIVATE_PROCESS_OUTPUT, 0, LPARAM (this));
              p_pending = 1;
            }
        }
    }
};

static lisp
process_char_encoding (lisp encoding)
{
  if (encoding == Qnil)
    encoding = xsymbol_value (Vdefault_process_encoding);
  check_char_encoding (encoding);
  if (xchar_encoding_type (encoding) == encoding_auto_detect)
    FEtype_error (encoding, Qchar_encoding);
  return encoding;
}

void
process_io_encoding (lisp &incode, lisp &outcode, lisp keys)
{
  incode = process_char_encoding (find_keyword (Kincode, keys));
  outcode = process_char_encoding (find_keyword (Koutcode, keys));
}

static eol_code
process_eol_code (lisp code)
{
  if (code == Qnil)
    return eol_crlf;
  int n = fixnum_value (code);
  if (!valid_eol_code_p (n) || n == eol_guess)
    n = eol_crlf;
  return eol_code (n);
}

Process::Process (Buffer *bp, lisp pl, lisp marker)
     : p_bufp (bp), p_proc (pl), p_filter (Qnil), p_sentinel (Qnil),
       p_marker (marker), p_input_stream (0), p_in_send_string (0),
       p_pending (0)
{
  InitializeCriticalSection (&p_cri);
}

Process::~Process ()
{
  DeleteCriticalSection (&p_cri);
}

void
Process::terminated (int exit_code)
{
  xprocess_data (p_proc) = 0;
  xprocess_status (p_proc) = PS_EXIT;
  xprocess_exit_code (p_proc) = exit_code;

  delq (p_proc, &xsymbol_value (Vprocess_list));

  if (p_sentinel != Qnil)
    {
      try
        {
          dynamic_bind d (Vinhibit_quit, Qt);
          funcall_1 (p_sentinel, p_proc);
        }
      catch (nonlocal_jump &)
        {
        }
    }

  p_bufp->modify_mode_line ();
  for (Window *wp = app.active_frame.windows; wp; wp = wp->w_next)
    if (wp->w_bufp == p_bufp)
      {
        refresh_screen (0);
        g_frame.update_ui ();
        break;
      }
}

void
Process::insert_process_output (void *p)
{
  lisp lstring = 0;
  try
    {
      read_data *r = (read_data *)p;
      const Char *data;
      int size;
      if (r)
        {
          if (r->done)
            return;
          r->done = 1;
          if (p_in_send_string || !p_osbuf.empty_p ())
            {
              p_osbuf.add (r->data, r->size);
              return;
            }
          data = r->data;
          size = r->size;
        }
      else
        {
          p_pending = 0;
          if (p_osbuf.empty_p ())
            return;

          EnterCriticalSection (&p_cri);
          try
            {
              lstring = p_osbuf.make_string ();
            }
          catch (nonlocal_jump &)
            {
            }
          p_osbuf.empty ();
          LeaveCriticalSection (&p_cri);

          if (!lstring)
            return;
          data = xstring_contents (lstring);
          size = xstring_length (lstring);
        }

      if (p_filter != Qnil)
        {
          dynamic_bind d (Vinhibit_quit, Qt);
          lisp s = lstring ? lstring : make_string (data, size);
          lstring = 0;
          funcall_2 (p_filter, p_proc, s);
        }
      else
        {
          Window *wp = selected_window ();
          if (xmarker_point (p_marker) == NO_MARK_SET)
            xmarker_point (p_marker) = p_bufp->b_contents.p2;
          int goto_tail = (wp->w_bufp == p_bufp
                           && wp->w_point.p_point == xmarker_point (p_marker));
          Point point;
          p_bufp->set_point (point, xmarker_point (p_marker));
          p_bufp->check_read_only ();
          p_bufp->insert_chars (point, data, size);
          xmarker_point (p_marker) += size;
          if (goto_tail)
            p_bufp->goto_char (wp->w_point, xmarker_point (p_marker));
          int f = 0;
          for (wp = app.active_frame.windows; wp; wp = wp->w_next)
            if (wp->w_bufp == p_bufp)
              {
                wp->w_disp_flags |= Window::WDF_REFRAME_SCROLL;
                f = 1;
              }
          if (f)
            refresh_screen (0);
        }
    }
  catch (nonlocal_jump &)
    {
    }

  if (lstring)
    destruct_string (lstring);
}

static int
good_process_p (const Process *pr)
{
  for (lisp p = xsymbol_value (Vprocess_list); consp (p); p = xcdr (p))
    if (xprocess_data (xcar (p)) == pr)
      return 1;
  return 0;
}

void
read_process_output (WPARAM wparam, LPARAM lparam)
{
  if (good_process_p ((Process *)lparam))
    ((Process *)lparam)->insert_process_output ((void *)wparam);
}

void
Process::store_output (const Char *w, int l)
{
  if (!l)
    return;

  EnterCriticalSection (&p_cri);
  if (p_osbuf.empty_p ())
    {
      LeaveCriticalSection (&p_cri);

      read_data r;
      r.data = w;
      r.size = l;
      r.done = 0;
      DWORD result;

      do
        if (SendMessageTimeout (app.toplev, WM_PRIVATE_PROCESS_OUTPUT,
                                WPARAM (&r), LPARAM (this),
                                SMTO_NORMAL, 1000, &result)
            || !IsWindow (app.toplev)
            || r.done)
          return;
      while (!p_in_send_string);

      EnterCriticalSection (&p_cri);
    }

  try
    {
      p_osbuf.add (w, l);
    }
  catch (nonlocal_jump &)
    {
    }
  LeaveCriticalSection (&p_cri);
}

class process_output_stream: public Char_output_wstream
{
  Process &p_proc;
  virtual void swrite (const Char *w, int l)
    {p_proc.store_output (w, l);}
public:
  process_output_stream (Process &proc) : p_proc (proc) {}
};

class process_input_stream: public byte_input_stream
{
  Process &p_proc;
  Char_output_stream &p_os;
  u_char p_buf[1024];
  int p_eofp;
  virtual int refill ()
    {
      if (p_eofp)
        return eof;

      p_os.flush (0);
      p_proc.flush_input ();

      int l;
      do
        {
          l = p_proc.readin (p_buf, sizeof p_buf);
          if (!l)
            {
              p_eofp = 1;
              break;
            }
          switch (p_proc.eolcode ())
            {
            case eol_crlf:
              {
                u_char *d = p_buf, *s = p_buf, *const se = s + l;
                for (; s < se; s++)
                  if (*s != '\r')
                    *d++ = *s;
                l = d - p_buf;
                break;
              }

            case eol_cr:
              {
                for (u_char *s = p_buf, *const se = s + l; s < se; s++)
                  if (*s == '\r')
                    *s = '\n';
                break;
              }
            }
        }
      while (!l);
      int c = setbuf (p_buf, p_buf + l);
      if (!p_proc.incode_modified_p ())
        return c;
      putback (c);
      return eof;
    }
public:
  process_input_stream (Process &proc, Char_output_stream &os)
       : p_proc (proc), p_os (os), p_eofp (0) {}
  int eofp () const {return p_eofp;}
};

void
Process::flush_input ()
{
  if (p_input_stream)
    {
      int l;
      const Char *b;
      p_input_stream->flush (b, l);
      if (l)
        store_output (b, l);
    }
}

void
Process::read_process_output (xbuffered_read_stream &is, process_output_stream &os)
{
  p_input_stream = &is;
  int c;
  while ((c = is.get ()) != xstream::eof)
    os.put (c);
  os.flush (1);
  p_input_stream = 0;
}

void
Process::read_process_output ()
{
  process_output_stream os (*this);
  process_input_stream is (*this, os);
  while (!is.eofp ())
    {
      p_last_incode = xprocess_incode (p_proc);
      encoding_input_stream_helper s (p_last_incode, is);
      read_process_output ((xbuffered_read_stream &)s, os);
    }
  os.flush (1);
}

class process_output_byte_stream: public byte_output_stream
{
  Process &p_proc;
  u_char p_buf[1024];
protected:
  virtual u_char *sflush (u_char *b, u_char *be, int)
    {
      p_proc.send ((char *)b, be - b);
      return b;
    }
public:
  process_output_byte_stream (Process &proc)
       : byte_output_stream (p_buf, p_buf + sizeof p_buf), p_proc (proc) {}
};


class NormalProcess: public Process
{
protected:
  dyn_handle p_in;
  dyn_handle p_out;
  dyn_handle p_event;
  dyn_handle p_process;
  dyn_handle p_read_thread;
  dyn_handle p_wait_thread;
  DWORD p_process_id;
  DWORD p_exit_code;

  virtual u_int read_process ();

  u_int wait_process ();
  static u_int __stdcall wait_process (void *p)
    {return ((NormalProcess *)p)->wait_process ();}
  void signal_nt ()
    {
      if (!SetEvent (p_event))
        FEsimple_win32_error (GetLastError ());
    }
  void signal_win95 ();

  struct dos_prompt
    {
      HWND hwnd;
      DWORD pid;
    };
  static BOOL CALLBACK find_tty (HWND, LPARAM);

public:
  NormalProcess (Buffer *bp, lisp pl, lisp marker) : Process (bp, pl, marker) {}
  virtual ~NormalProcess () {}
  virtual void wait_terminate ();
  virtual void signal ()
    {
      if (sysdep.WinNTp ())
        signal_nt ();
      else
        signal_win95 ();
    }
  virtual void kill ()
    {
      if (!TerminateProcess (p_process, 2))
        FEsimple_win32_error (GetLastError ());
    }
  virtual void send (const char *s, int l) const
    {
      DWORD nwrite;
      if (!WriteFile (p_out, s, l, &nwrite, 0))
        file_error (GetLastError ());
    }
  void create (lisp, lisp, const char *, int);
  virtual int readin (u_char *, int);
};

int
NormalProcess::readin (u_char *buf, int size)
{
  u_char *b = buf, *be = buf + size;
  DWORD avail;

  int wait = 5;
  while (1)
    {
      if (!PeekNamedPipe (p_in, 0, 0, 0, &avail, 0))
        return 0;
      if (avail)
        break;
      if (!p_process.valid ())
        return 0;
      Sleep (wait);
      if (wait < 100)
        wait += 5;
    }

  int i = 0;
  while (1)
    {
      DWORD nread;
      if (!ReadFile (p_in, b, be - b, &nread, 0) || !nread)
        break;
      b += nread;
      if (++i >= 10 || b - buf >= size / 2)
        break;
      Sleep (5);
      if (!PeekNamedPipe (p_in, 0, 0, 0, &avail, 0) || !avail)
        break;
    }
  return b - buf;
}

u_int
NormalProcess::read_process ()
{
  if (!p_process.valid ())
    return 0;
  read_process_output ();
  return 0;
}

u_int
NormalProcess::wait_process ()
{
  if (!p_process.valid ())
    return 0;

  WaitForSingleObject (p_process, INFINITE);
  if (!GetExitCodeProcess (p_process, &p_exit_code))
    p_exit_code = DWORD (-1);
  p_process.close ();

  WaitForSingleObject (p_read_thread, INFINITE);

  notify_term ();
  return 0;
}

BOOL CALLBACK
NormalProcess::find_tty (HWND hwnd, LPARAM arg)
{
  DWORD pid;
  GetWindowThreadProcessId (hwnd, &pid);
  if (pid != ((dos_prompt *)arg)->pid)
    return 1;
  char name[32];
  if (!GetClassName (hwnd, name, sizeof name)
      || lstrcmp (name, "tty"))
    return 1;
  ((dos_prompt *)arg)->hwnd = hwnd;
  return 0;
}

void
NormalProcess::signal_win95 ()
{
  dos_prompt tty;
  tty.hwnd = 0;
  tty.pid = p_process_id;
  EnumWindows (find_tty, LPARAM (&tty));
  if (!tty.hwnd)
    return;

  SuspendThread (p_read_thread);

  UINT c = MapVirtualKey ('C', 0);
  UINT ctrl = MapVirtualKey (VK_CONTROL, 0);
  ShowWindow (tty.hwnd, SW_RESTORE);
  ForceSetForegroundWindow (tty.hwnd);
  keybd_event (VK_CONTROL, ctrl, 0, 0);
  keybd_event ('C', c, 0, 0);
  keybd_event ('C', c, KEYEVENTF_KEYUP, 0);
  keybd_event (VK_CONTROL, ctrl, KEYEVENTF_KEYUP, 0);

  ResumeThread (p_read_thread);
  Sleep (100);
  ShowWindow (tty.hwnd, SW_MINIMIZE);
}

void
NormalProcess::create (lisp command, lisp execdir, const char *env, int show)
{
  char dir[PATH_MAX + 1];
  pathname2cstr (execdir, dir);
  map_sl_to_backsl (dir);

  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof sa;
  sa.lpSecurityDescriptor = 0;
  sa.bInheritHandle = 1;

  dyn_handle opipe_r, opipe_w;
  if (!pipe (opipe_r, opipe_w, &sa))
    file_error (GetLastError ());

  dyn_handle ipipe_r, ipipe_w;
  if (!pipe (ipipe_r, ipipe_w, &sa))
    file_error (GetLastError ());

  dyn_handle d (ipipe_w);
  if (!d.valid ())
    file_error (GetLastError ());
  CloseHandle (ipipe_w.unfix ());
  ipipe_w.fix (d.unfix ());

  dyn_handle event (CreateEvent (&sa, 0, 0, 0));
  if (!event.valid ())
    file_error (GetLastError ());

  char *cmdline = (char *)alloca (128 + xstring_length (command) * 2 + 1);
  sprintf (cmdline, "xyzzyenv -s%u %u ", show, HANDLE (event));
  w2s (cmdline + strlen (cmdline), command);

  u_int thread_id;
  HANDLE hread_thread = HANDLE (_beginthreadex (0, 0, Process::read_process, this,
                                                CREATE_SUSPENDED, &thread_id));
  if (!hread_thread)
    FEsimple_error (Ecreate_thread_failed);

  HANDLE hwait_thread = HANDLE (_beginthreadex (0, 0, wait_process, this,
                                                CREATE_SUSPENDED, &thread_id));
  if (!hwait_thread)
    {
      ResumeThread (hread_thread);
      WaitForSingleObject (hread_thread, INFINITE);
      FEsimple_error (Ecreate_thread_failed);
    }

  lisp lxshow = symbol_value (Vxyzzyenv_show_flag, selected_buffer ());
  int xshow = show_window_parameter (lxshow, SW_SHOWMINNOACTIVE);

  STARTUPINFO si;
  bzero (&si, sizeof si);
  si.cb = sizeof si;
  si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  si.wShowWindow = xshow;
  si.hStdInput = ipipe_r;
  si.hStdOutput = opipe_w;
  si.hStdError = opipe_w;

  WINFS::SetCurrentDirectory (dir);

  PROCESS_INFORMATION pi;
  int result = CreateProcess (0, cmdline, 0, 0, 1,
                              (CREATE_NEW_PROCESS_GROUP
                               | CREATE_DEFAULT_ERROR_MODE
                               | NORMAL_PRIORITY_CLASS),
                              (void *)env, dir, &si, &pi);
  int error = GetLastError ();

  w2s (dir, xsymbol_value (Qdefault_dir));
  WINFS::SetCurrentDirectory (dir);

  p_in.fix (opipe_r.unfix ());
  p_out.fix (ipipe_w.unfix ());
  p_event.fix (event.unfix ());
  if (result)
    {
      CloseHandle (pi.hThread);
      p_process.fix (pi.hProcess);
      p_process_id = pi.dwProcessId;
    }
  p_read_thread.fix (hread_thread);
  p_wait_thread.fix (hwait_thread);

  ResumeThread (hread_thread);
  ResumeThread (hwait_thread);
  if (!result)
    {
      WaitForSingleObject (hread_thread, INFINITE);
      WaitForSingleObject (hwait_thread, INFINITE);
      file_error (error, command);
    }
}

void
NormalProcess::wait_terminate ()
{
  WaitForSingleObject (p_wait_thread, INFINITE);
  terminated (p_exit_code);
}

void
wait_process_terminate (WPARAM wparam, LPARAM lparam)
{
  if (good_process_p ((Process *)lparam))
    {
      Process *pl = (Process *)lparam;
      pl->wait_terminate ();
      delete pl;
    }
}

lisp
Fmake_process (lisp command, lisp keys)
{
  check_string (command);

  lisp execdir = find_keyword (Kexec_directory, keys);
  if (execdir == Qnil)
    execdir = selected_buffer ()->ldirectory;

  Buffer *bp = Buffer::coerce_to_buffer (find_keyword (Koutput, keys));
  if (buffer_has_process (bp))
    FEsimple_error (Esubprocess_is_already_running);

  lisp incode, outcode;
  process_io_encoding (incode, outcode, keys);
  lisp x = find_keyword (Keol_code, keys, 0);
  if (!x)
    x = find_keyword (Knewline_code, keys);
  eol_code eol = process_eol_code (x);

  EnvStrings env;
  env.setup (find_keyword (Kenviron, keys));

  lisp lshow = find_keyword (Kshow, keys);
  int show = show_window_parameter (lshow, SW_SHOWNORMAL);

  lisp process = make_process ();
  lisp pl = xcons (process, xsymbol_value (Vprocess_list));

  xprocess_buffer (process) = bp->lbp;
  xprocess_command (process) = command;
  xprocess_incode (process) = incode;
  xprocess_outcode (process) = outcode;
  xprocess_eol_code (process) = eol;

  NormalProcess *pr = new NormalProcess (bp, process, Process::make_process_marker (bp));
  try
    {
      pr->create (command, execdir, env.str (), show);
    }
  catch (nonlocal_jump &)
    {
      delete pr;
      throw;
    }

  xsymbol_value (Vprocess_list) = pl;
  xprocess_data (process) = pr;
  xprocess_status (process) = PS_RUN;

  bp->lprocess = process;
  bp->modify_mode_line ();

  return process;
}

class SocketProcess: public Process
{
protected:
  sockinet p_so;
  dyn_handle p_read_thread;
  int p_error_code;

  virtual u_int read_process ();

public:
  SocketProcess (Buffer *bp, lisp pl, lisp marker) : Process (bp, pl, marker) {}
  virtual ~SocketProcess () {}
  virtual void wait_terminate ();
  virtual void signal ()
    {
      try {p_so.close ();}
      catch (sock_error &e) {FEsocket_error (e.error_code (), e.ope ());}
    }
  virtual void kill ()
    {
      try {p_so.close (1);}
      catch (sock_error &e) {FEsocket_error (e.error_code (), e.ope ());}
    }
  virtual void send (const char *s, int l) const
    {
      try {p_so.send (s, l);}
      catch (sock_error &e) {FEsocket_error (e.error_code (), e.ope ());}
    }
  void create (lisp, lisp);
  virtual int readin (u_char *, int);
};

void
SocketProcess::wait_terminate ()
{
  WaitForSingleObject (p_read_thread, INFINITE);
  terminated (p_error_code);
}

int
SocketProcess::readin (u_char *buf, int size)
{
  try
    {
      return p_so.recv (buf, size);
    }
  catch (sock_error &e)
    {
      p_error_code = e.error_code ();
      return 0;
    }
}

u_int
SocketProcess::read_process ()
{
  if (!p_read_thread.valid ())
    return 0;
  p_error_code = 0;
  read_process_output ();
  notify_term ();
  return 0;
}

void
SocketProcess::create (lisp host, lisp service)
{
  u_int thread_id;
  HANDLE hread_thread = HANDLE (_beginthreadex (0, 0, Process::read_process, this,
                                                CREATE_SUSPENDED, &thread_id));
  if (!hread_thread)
    FEsimple_error (Ecreate_thread_failed);

  try
    {
      Fbegin_wait_cursor ();
      sockinet::saddr addr (host, service);
      p_so.set_eof_error_p (0);
      p_so.create ();
      p_so.connect (addr);
    }
  catch (sock_error &e)
    {
      Fend_wait_cursor ();
      ResumeThread (hread_thread);
      WaitForSingleObject (hread_thread, INFINITE);
      FEsocket_error (e.error_code (), e.ope ());
    }

  Fend_wait_cursor ();
  p_read_thread.fix (hread_thread);
  ResumeThread (hread_thread);
}

lisp
Fopen_network_stream (lisp buffer, lisp host, lisp service, lisp keys)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (buffer_has_process (bp))
    FEsimple_error (Esubprocess_is_already_running);

  lisp incode, outcode;
  process_io_encoding (incode, outcode, keys);
  lisp x = find_keyword (Keol_code, keys, 0);
  if (!x)
    x = find_keyword (Knewline_code, keys);
  eol_code eol = process_eol_code (x);

  lisp process = make_process ();
  lisp pl = xcons (process, xsymbol_value (Vprocess_list));

  xprocess_buffer (process) = bp->lbp;
  xprocess_command (process) = xcons (host, service);
  xprocess_incode (process) = incode;
  xprocess_outcode (process) = outcode;
  xprocess_eol_code (process) = eol;

  SocketProcess *pr = new SocketProcess (bp, process, Process::make_process_marker (bp));
  try
    {
      protect_gc gcpro1 (pl);
      protect_gc gcpro2 (pr->marker ());
      pr->create (host, service);
    }
  catch (nonlocal_jump &)
    {
      delete pr;
      throw;
    }

  xsymbol_value (Vprocess_list) = pl;
  xprocess_data (process) = pr;
  xprocess_status (process) = PS_RUN;

  bp->lprocess = process;
  bp->modify_mode_line ();

  return process;
}

int
buffer_has_process (const Buffer *bp)
{
  for (lisp p = xsymbol_value (Vprocess_list); consp (p); p = xcdr (p))
    if (xprocess_data (xcar (p))
        && xprocess_data (xcar (p))->process_buffer () == bp->lbp)
      return 1;
  return 0;
}

int
query_kill_subprocesses ()
{
  if (!consp (xsymbol_value (Vprocess_list)))
    return 1;
  if (!yes_or_no_p (Msubprocesses_are_running))
    return 0;
  for (lisp p = xsymbol_value (Vprocess_list); consp (p); p = xcdr (p))
    if (xprocess_data (xcar (p)))
      xprocess_data (xcar (p))->signal ();
  return 1;
}

void
process_gc_mark (void (*fn)(lisp))
{
  for (lisp p = xsymbol_value (Vprocess_list); consp (p); p = xcdr (p))
    {
      Process *pr = xprocess_data (xcar (p));
      if (pr)
        {
          (*fn)(pr->filter ());
          (*fn)(pr->sentinel ());
          (*fn)(pr->marker ());
        }
    }
}

lisp
Fbuffer_process (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lprocess;
}

lisp
Fprocess_buffer (lisp process)
{
  check_process (process);
  return xprocess_buffer (process);
}

lisp
Fprocess_command (lisp process)
{
  check_process (process);
  return xprocess_command (process);
}

lisp
Fprocess_status (lisp process)
{
  check_process (process);
  switch (xprocess_status (process))
    {
    case PS_RUN:
      return Krun;

    case PS_EXIT:
      return Kexit;

    default:
      return Qnil;
    }
}

lisp
Fprocess_exit_code (lisp process)
{
  check_process (process);
  return (xprocess_status (process) == PS_EXIT
          ? make_fixnum (xprocess_exit_code (process)) : Qnil);
}

lisp
Fprocess_incode (lisp process)
{
  check_process (process);
  return xprocess_incode (process);
}

lisp
Fprocess_outcode (lisp process)
{
  check_process (process);
  return xprocess_outcode (process);
}

lisp
Fset_process_incode (lisp process, lisp encoding)
{
  check_process (process);
  xprocess_incode (process) = process_char_encoding (encoding);
  return Qt;
}

lisp
Fset_process_outcode (lisp process, lisp encoding)
{
  check_process (process);
  xprocess_outcode (process) = process_char_encoding (encoding);
  return Qt;
}

lisp
Fprocess_eol_code (lisp process)
{
  check_process (process);
  return make_fixnum (xprocess_eol_code (process));
}

lisp
Fset_process_eol_code (lisp process, lisp code)
{
  check_process (process);
  xprocess_eol_code (process) = process_eol_code (code);
  return Qt;
}

lisp
Fsignal_process (lisp process)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (pr)
    pr->signal ();
  return Qt;
}

lisp
Fkill_process (lisp process)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (pr)
    pr->kill ();
  return Qt;
}

class in_process_send_string
{
  Process &i_pr;
public:
  in_process_send_string (Process &pr) : i_pr (pr)
    {i_pr.in_send_string_p () = 1;}
  ~in_process_send_string ()
    {
      i_pr.in_send_string_p () = 0;
      i_pr.end_send_string ();
    }
};

lisp
Fprocess_send_string (lisp process, lisp string)
{
  check_process (process);
  check_string (string);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  Char_input_string_stream is (string);
  process_output_byte_stream os (*pr);
  encoding_output_stream_helper s (xprocess_outcode (process), is, eol_noconv);

  in_process_send_string in (*pr);
  copy_xstream (s, os);

  return Qt;
}

lisp
Fset_process_filter (lisp process, lisp filter)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  pr->filter () = filter;
  return Qt;
}

lisp
Fprocess_filter (lisp process)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  return pr->filter ();
}

lisp
Fset_process_sentinel (lisp process, lisp sentinel)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  pr->sentinel () = sentinel;
  return Qt;
}

lisp
Fprocess_sentinel (lisp process)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  return pr->sentinel ();
}

lisp
Fprocess_marker (lisp process)
{
  check_process (process);
  Process *pr = xprocess_data (process);
  if (!pr)
    return Qnil;
  return pr->marker ();
}

static void
se_error (lisp lpath, int e)
{
  switch (e)
    {
    case SE_ERR_ASSOCINCOMPLETE:
      FEsimple_error (Eassoc_incomplete, lpath);

    case SE_ERR_DDEBUSY:
      FEdde_busy ();

    case SE_ERR_DDEFAIL:
      FEdde_error (lpath);

    case SE_ERR_DDETIMEOUT:
      FEdde_busy ();

    case SE_ERR_NOASSOC:
      FEsimple_error (Eno_assoc, lpath);

    case SE_ERR_SHARE:
      file_error (ERROR_SHARING_VIOLATION, lpath);

    case 0:
    case SE_ERR_OOM:
      FEstorage_error ();

    case SE_ERR_ACCESSDENIED:
      file_error (ERROR_ACCESS_DENIED, lpath);

    case ERROR_FILE_NOT_FOUND:
    case ERROR_PATH_NOT_FOUND:
    case ERROR_BAD_FORMAT:
      file_error (e, lpath);

    default:
      FEsimple_win32_error (e);
    }
}

lisp
Fshell_execute (lisp lpath, lisp ldir, lisp lparam, lisp keys)
{
  char *path, *dir, *param;
  if (ldir == Qt)
    {
      check_string (lpath);
      path = (char *)alloca (xstring_length (lpath) * 2 + 1);
      w2s (path, lpath);
      dir = 0;
    }
  else
    {
      path = (char *)alloca (PATH_MAX + 1);
      pathname2cstr (lpath, path);
      map_sl_to_backsl (path);

      dir = (char *)alloca (PATH_MAX + 1);
      if (ldir && ldir != Qnil)
        pathname2cstr (ldir, dir);
      else
        pathname2cstr (Fdirectory_namestring (lpath), dir);
      map_sl_to_backsl (dir);
    }

  if (lparam && lparam != Qnil)
    {
      check_string (lparam);
      param = (char *)alloca (xstring_length (lparam) * 2 + 1);
      w2s (param, lparam);
    }
  else
    param = 0;

  UINT omode = SetErrorMode (0);
  if (dir)
    WINFS::SetCurrentDirectory (dir);

  if (xsymbol_value (Vshell_execute_disregards_shift_key) != Qnil)
    {
      BYTE b[256];
      GetKeyboardState (b);
      b[VK_SHIFT] = 0;
      SetKeyboardState (b);
    }

  DWORD e;
  typedef int (WINAPI *SHELLEXECUTEEX)(SHELLEXECUTEINFO *);
  SHELLEXECUTEEX ex = (xsymbol_value (Vuse_shell_execute_ex) != Qnil
                       ? (SHELLEXECUTEEX)GetProcAddress (GetModuleHandle ("shell32.dll"),
                                                         "ShellExecuteExA")
                       : 0);

  char *verb = 0;
  lisp lverb = find_keyword (Kverb, keys);
  if (lverb != Qnil)
    {
      lverb = Fstring (lverb);
      verb = (char *)alloca (xstring_length (lverb) * 2 + 1);
      w2s (verb, lverb);
    }

  if (ex)
    {
      SHELLEXECUTEINFO sei = {sizeof sei};
      sei.fMask = SEE_MASK_FLAG_NO_UI;
      sei.hwnd = get_active_window ();
      sei.lpFile = path;
      sei.lpParameters = param;
      sei.lpDirectory = dir;
      sei.lpVerb = verb;
      sei.nShow = SW_SHOW;
      e = (*ex)(&sei) ? 33 : DWORD (sei.hInstApp);
    }
  else
    e = DWORD (ShellExecute (get_active_window (), verb ? verb : "open",
                             path, param, dir, SW_SHOWNORMAL));
  if (dir)
    WINFS::SetCurrentDirectory (sysdep.curdir);
  SetErrorMode (omode);
  if (e <= 32)
    se_error (lpath, e);
  return Qt;
}
