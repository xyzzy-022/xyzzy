#include "ed.h"
#include "sequence.h"
#include "wstream.h"
#include "sockinet.h"
#include <io.h>
#include <fcntl.h>
#include <math.h>

static void
close_file_stream (lisp stream, int abort)
{
  if (!xfile_stream_alt_pathname (stream))
    return;
  xstream_open_p (stream) = 0;
  if (abort)
    WINFS::DeleteFile (xfile_stream_alt_pathname (stream));
  else
    {
      char path[PATH_MAX + 1];
      w2s (path, xfile_stream_pathname (stream));
      WINFS::DeleteFile (path);
      if (!WINFS::MoveFile (xfile_stream_alt_pathname (stream), path))
        file_error (GetLastError (), xfile_stream_pathname (stream));
    }
  xfree (xfile_stream_alt_pathname (stream));
  xfile_stream_alt_pathname (stream) = 0;
}

lstream::~lstream ()
{
  if (file_stream_p (this))
    {
      if (input && input != stdin)
        fclose ((FILE *)input);
      if (output && output != input && output != stdout && output != stderr)
        fclose ((FILE *)output);
      if (alt_pathname)
        close_file_stream (this, 1);
    }
  else if (socket_stream_p (this))
    {
      if (xsocket_stream_sock (this))
        delete xsocket_stream_sock (this);
    }
}

static lstream *
make_stream (stream_type type)
{
  lstream *p = ldata <lstream, Tstream>::lalloc ();
  p->type = type;
  p->pending = lChar_EOF;
  p->column = 0;
  p->linenum = 1;
  p->pathname = Qnil;
  p->alt_pathname = 0;
  p->open_p = 1;
  p->encoding = lstream::ENCODE_CANON;
  return p;
}

static lisp
make_file_stream (stream_type type)
{
  lisp stream = make_stream (type);
  assert (file_stream_p (stream));
  xfile_stream_pathname (stream) = Qnil;
  xfile_stream_input (stream) = 0;
  xfile_stream_output (stream) = 0;
  return stream;
}

static lisp
make_composite_stream (stream_type type)
{
  lisp stream = make_stream (type);
  assert (composite_stream_p (stream));
  xcomposite_stream_input (stream) = Qnil;
  xcomposite_stream_output (stream) = Qnil;
  return stream;
}

static lisp
make_string_stream (stream_type type)
{
  lisp stream = make_stream (type);
  assert (string_stream_p (stream));
  xstring_stream_input (stream) = Qnil;
  xstring_stream_output (stream) = Qnil;
  return stream;
}

static inline lisp
make_status_stream ()
{
  return make_stream (st_status);
}

static lisp
make_buffer_stream ()
{
  lisp stream = make_stream (st_buffer);
  xbuffer_stream_eob (stream) = Qnil;
  xbuffer_stream_marker (stream) = Qnil;
  return stream;
}

static lisp
make_keyboard_stream ()
{
  return make_stream (st_keyboard);
}

static lisp
make_wstream_stream ()
{
  lisp stream = make_stream (st_wstream);
  xwstream_stream_wstream (stream) = 0;
  return stream;
}

static lisp
make_socket_stream ()
{
  lisp stream = make_stream (st_socket);
  xsocket_stream_sock (stream) = 0;
  return stream;
}

lisp
Fstreamp (lisp object)
{
  return boole (streamp (object));
}

lisp
Fsynonym_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_synonym);
}

lisp
Fbroadcast_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_broadcast);
}

lisp
Fconcatenated_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_concatenated);
}

lisp
Fecho_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_echo);
}

lisp
Ftwo_way_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_two_way);
}

lisp
Fstring_stream_p (lisp stream)
{
  return boole (streamp (stream) && string_stream_p (stream));
}

lisp
Fstring_input_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_string_input);
}

lisp
Fstring_output_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_string_output);
}

lisp
Fstatus_window_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_status);
}

lisp
Fbuffer_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_buffer);
}

lisp
Fkeyboard_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_keyboard);
}

lisp
Fsocket_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_socket);
}

lisp
Fgeneral_input_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_general_input);
}

lisp
Fgeneral_output_stream_p (lisp stream)
{
  return boole (streamp (stream) && xstream_type (stream) == st_general_output);
}

lisp
Fopen_stream_p (lisp stream)
{
  check_stream (stream);
  return boole (xstream_open_p (stream));
}

lisp
Finput_stream_p (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      if (xstream_type (stream) != st_synonym)
        return boole (input_stream_p (stream));
      assert (symbolp (xcomposite_stream_input (stream)));
      stream = symbol_value (xcomposite_stream_input (stream), selected_buffer ());
      QUIT;
    }
}

lisp
Foutput_stream_p (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      if (xstream_type (stream) != st_synonym)
        return boole (output_stream_p (stream));
      assert (symbolp (xcomposite_stream_output (stream)));
      stream = symbol_value (xcomposite_stream_output (stream), selected_buffer ());
      QUIT;
    }
}

static int
stream_encoding (lisp lencoding)
{
  if (lencoding == Kcanonical || lencoding == Ktext)
    return lstream::ENCODE_CANON;
  if (lencoding == Kraw)
    return lstream::ENCODE_RAW;
  if (lencoding == Kbinary)
    return lstream::ENCODE_BINARY;
  FEprogram_error (Einvalid_encoding_option, lencoding);
  return 0;
}

lisp
create_file_stream (lisp filename, lisp direction, lisp if_exists,
                    lisp if_does_not_exist, lisp lencoding,
                    lisp lshare)
{
  char path[PATH_MAX + 1];
  pathname2cstr (filename, path);

  if (if_does_not_exist != Kerror && if_does_not_exist != Kcreate
      && if_does_not_exist != Qnil)
    FEprogram_error (Einvalid_if_does_not_exist_option, if_does_not_exist);

  int encoding = stream_encoding (lencoding);
  int dont_create = if_does_not_exist != Kcreate;
  int need_alt = 0;
  int create, access;

  if (direction == Kinput || direction == Kprobe)
    {
      access = GENERIC_READ;
      create = dont_create ? OPEN_EXISTING : OPEN_ALWAYS;
      if_exists = Qnil;
    }
  else
    {
      access = direction == Kio ? (GENERIC_READ | GENERIC_WRITE) : GENERIC_WRITE;
      if (if_exists == Kerror || if_exists == Qnil)
        {
          if (dont_create)
            {
              if (if_exists == Kerror)
                file_error (Ecannot_create_file, filename);
              return Qnil;
            }
          create = CREATE_NEW;
        }
      else if (if_exists == Knew_version)
        {
          need_alt = 1;
          create = CREATE_ALWAYS;
        }
      else if (if_exists == Koverwrite)
        {
          if (direction == Kio)
            create = dont_create ? OPEN_EXISTING : OPEN_ALWAYS;
          else
            create = dont_create ? TRUNCATE_EXISTING : CREATE_ALWAYS;
        }
      else if (if_exists == Kappend)
        create = dont_create ? OPEN_EXISTING : OPEN_ALWAYS;
      else if (if_exists == Krename
               || if_exists == Krename_and_delete
               || if_exists == Ksupersede)
        {
          need_alt = 1;
          create = CREATE_ALWAYS;
        }
      else
        FEprogram_error (Einvalid_if_exists_option, if_exists);
    }

  int share;
  if (lshare == Kread)
    share = FILE_SHARE_READ;
  else if (lshare == Kwrite)
    share = FILE_SHARE_WRITE;
  else if (lshare == Kread_write)
    share = FILE_SHARE_READ | FILE_SHARE_WRITE;
  else if (lshare == Qnil)
    share = 0;
  else if (!lshare)
    share = access == GENERIC_READ ? FILE_SHARE_READ : 0;
  else
    FEprogram_error (Einvalid_share_option, lshare);

  if (need_alt && (if_exists == Kerror || dont_create))
    {
      HANDLE h = WINFS::CreateFile (path, 0, 0, 0, OPEN_EXISTING, 0, 0);
      if (h != INVALID_HANDLE_VALUE)
        {
          CloseHandle (h);
          if (if_exists == Kerror)
            file_error (ERROR_ALREADY_EXISTS, filename);
        }
      else
        {
          if (dont_create)
            {
              if (if_does_not_exist == Kerror)
                file_error (GetLastError (), filename);
              return Qnil;
            }
        }
    }

  lisp stream = make_file_stream (access == GENERIC_READ
                                  ? st_file_input
                                  : (access == GENERIC_WRITE
                                     ? st_file_output
                                     : st_file_io));
  xfile_stream_pathname (stream) = make_string (path);
  xfile_stream_encoding (stream) = encoding;

  if (need_alt)
    {
      char *sl = find_last_slash (path);
      if (sl)
        sl[1] = 0;
      char buf[PATH_MAX + 1];
      if (!WINFS::GetTempFileName (sl ? path : ".", "xyz", 0, buf))
        file_error (GetLastError ());
      if (sl)
        sl[1] = '/';
      xfile_stream_alt_pathname (stream) = xstrdup (buf);
    }

  HANDLE h = WINFS::CreateFile ((xfile_stream_alt_pathname (stream)
                                 ? xfile_stream_alt_pathname (stream) : path),
                                access, share, 0, create,
                                FILE_ATTRIBUTE_ARCHIVE, 0);
  if (h == INVALID_HANDLE_VALUE)
    {
      int e = GetLastError ();
      switch (e)
        {
        case ERROR_ALREADY_EXISTS:
        case ERROR_FILE_EXISTS:
          if (if_exists == Kerror)
            file_error (e, filename);
          return Qnil;

        case ERROR_FILE_NOT_FOUND:
        case ERROR_PATH_NOT_FOUND:
        case ERROR_BAD_NETPATH:
        case ERROR_BAD_PATHNAME:
          if (if_does_not_exist == Kerror)
            file_error (e, filename);
          return Qnil;

        default:
          file_error (e, filename);
        }
    }

  if (if_exists == Krename && xfile_stream_alt_pathname (stream))
    {
      xfree (xfile_stream_alt_pathname (stream));
      xfile_stream_alt_pathname (stream) = 0;
    }

  int fd = _open_osfhandle (long (h), access == GENERIC_READ ? _O_RDONLY : 0);
  if (fd == -1)
    {
      CloseHandle (h);
      FEsimple_crtl_error (errno, filename);
    }

  FILE *fp = _fdopen (fd, (access == GENERIC_READ
                           ? "rb"
                           : (access == GENERIC_WRITE ? "wb" : "r+b")));
  if (!fp)
    {
      int e = errno;
      _close (fd);
      FEsimple_crtl_error (e, filename);
    }

  if (if_exists == Kappend)
    fseek (fp, 0, SEEK_END);

  xfile_stream_input (stream) = (access & GENERIC_READ) ? fp : 0;
  xfile_stream_output (stream) = (access & GENERIC_WRITE) ? fp : 0;

  if (direction == Kprobe)
    Fclose (stream, Qnil);

  return stream;
}

lisp
Fopen (lisp filename, lisp keys)
{
  lisp direction = find_keyword (Kdirection, keys, Kinput);
  lisp if_exists = find_keyword (Kif_exists, keys, 0);
  lisp if_does_not_exist = find_keyword (Kif_does_not_exist, keys, 0);
  lisp encoding = find_keyword (Kencoding, keys, Kcanonical);
  lisp share = find_keyword (Kshare, keys, 0);

  if (direction == Kinput)
    {
      if_exists = Qnil;
      if (!if_does_not_exist)
        if_does_not_exist = Kerror;
    }
  else if (direction == Koutput || direction == Kio)
    {
      if (!if_exists)
        if_exists = Knew_version;
      if (!if_does_not_exist)
        {
          if (if_exists == Koverwrite || if_exists == Kappend)
            if_does_not_exist = Kerror;
          else
            if_does_not_exist = Kcreate;
        }
    }
  else if (direction == Kprobe)
    {
      if_exists = Qnil;
      if (!if_does_not_exist)
        if_does_not_exist = Qnil;
    }
  else
    FEprogram_error (Einvalid_direction_option, direction);

  return create_file_stream (filename, direction, if_exists,
                             if_does_not_exist, encoding, share);
}

lisp
Ffile_position (lisp stream, lisp lpos)
{
  check_stream (stream);
  if (!file_stream_p (stream))
    FEtype_error (stream, Qfile_stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);

  FILE *fp;
  switch (xstream_type (stream))
    {
    case st_file_input:
    case st_file_io:
      fp = xfile_stream_input (stream);
      break;

    case st_file_output:
      fp = xfile_stream_output (stream);
      break;

    default:
      assert (0);
      return Qnil;
    }

  if (lpos && lpos != Qnil)
    {
      int origin = SEEK_SET;
      long offset;
      if (lpos == Kstart)
        offset = 0;
      else if (lpos == Kend)
        {
          origin = SEEK_END;
          offset = 0;
        }
      else
        {
          offset = fixnum_value (lpos);
          if (offset < 0)
            FErange_error (lpos);
        }

      if (fseek (fp, offset, origin) == -1)
        return Qnil;
      if (origin == SEEK_SET && !offset)
        {
          xstream_linenum (stream) = 0;
          xstream_column (stream) = 0;
        }
      return Qt;
    }
  else
    {
      long offset = ftell (fp);
      return offset >= 0 ? make_fixnum (offset) : Qnil;
    }
}

lisp
Fset_end_of_file (lisp stream)
{
  check_stream (stream);
  if (!file_stream_p (stream))
    FEtype_error (stream, Qfile_stream);
  if (!output_stream_p (stream))
    FEtype_error (stream, Qoutput_stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);
  if (!SetEndOfFile (HANDLE (_get_osfhandle (_fileno (xfile_stream_output (stream))))))
    FEsimple_win32_error (GetLastError ());
  return Qnil;
}

lisp
Fmake_synonym_stream (lisp symbol)
{
  check_symbol (symbol);
  lisp stream = make_composite_stream (st_synonym);
  xcomposite_stream_input (stream) = symbol;
  xcomposite_stream_output (stream) = symbol;
  return stream;
}

lisp
Fmake_broadcast_stream (lisp streams)
{
  for (lisp p = streams; consp (p); p = xcdr (p))
    if (Foutput_stream_p (xcar (p)) == Qnil)
      FEtype_error (xcar (p), Qoutput_stream);
  lisp stream = make_composite_stream (st_broadcast);
  xcomposite_stream_input (stream) = Qnil;
  xcomposite_stream_output (stream) = Fcopy_list (streams);
  return stream;
}

lisp
Fmake_concatenated_stream (lisp streams)
{
  for (lisp p = streams; consp (p); p = xcdr (p))
    if (Finput_stream_p (xcar (p)) == Qnil)
      FEtype_error (xcar (p), Qinput_stream);
  lisp stream = make_composite_stream (st_concatenated);
  xcomposite_stream_input (stream) = Fcopy_list (streams);
  xcomposite_stream_output (stream) = Qnil;
  return stream;
}

lisp
Fmake_two_way_stream (lisp input, lisp output)
{
  if (Finput_stream_p (input) == Qnil)
    FEtype_error (input, Qinput_stream);
  if (Foutput_stream_p (output) == Qnil)
    FEtype_error (output, Qoutput_stream);
  lisp stream = make_composite_stream (st_two_way);
  xcomposite_stream_input (stream) = input;
  xcomposite_stream_output (stream) = output;
  return stream;
}

lisp
Fmake_echo_stream (lisp input, lisp output)
{
  lisp stream = Fmake_two_way_stream (input, output);
  xstream_type (stream) = st_echo;
  return stream;
}

lisp
Fmake_string_input_stream (lisp string, lisp lstart, lisp lend)
{
  check_string (string);
  int start, end;
  seq_start_end (xstring_length (string), start, end,
                 (lstart && lstart != Qnil) ? lstart : make_fixnum (0),
                 lend);
  lisp stream = make_string_stream (st_string_input);
  xstring_stream_input (stream) = string;
  xstring_stream_start (stream) = start;
  xstring_stream_end (stream) = end;
  return stream;
}

lisp
Fmake_string_output_stream ()
{
  lisp string = make_complex_string (Char (0), 0, 128, 1);
  lisp stream = make_string_stream (st_string_output);
  xstring_stream_output (stream) = string;
  return stream;
}

lisp
Fsi_make_string_output_stream_from_string (lisp string)
{
  check_string (string);
  if (!complex_string_p (string) || !xarray_has_fillp (string))
    FEprogram_error (Evector_has_no_fill_pointer, string);
  lisp stream = make_string_stream (st_string_output);
  xstring_stream_output (stream) = string;
  return stream;
}

lisp
Fsi_get_string_input_stream_index (lisp stream)
{
  if (Fstring_input_stream_p (stream) == Qnil)
    FEtype_error (stream, Qstring_input_stream);
  return make_fixnum (xstring_stream_start (stream));
}

lisp
Fget_output_stream_string (lisp stream)
{
  if (Fstring_output_stream_p (stream) == Qnil)
    FEtype_error (stream, Qstring_output_stream);
  lisp t = xstring_stream_output (stream);
  xstring_stream_output (stream) = make_complex_string (Char (0), 0, 128, 1);
  return t;
}

lisp
Fmake_buffer_stream (lisp buffer, lisp point, lisp eob)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  lisp stream = make_buffer_stream ();
  lisp marker = Fmake_marker (bp->lbp);
  xbuffer_stream_marker (stream) = marker;
  if (!point || point == Qnil)
    xmarker_point (marker) = bp->b_contents.p1;
  else
    xmarker_point (marker) = bp->coerce_to_restricted_point (point);
  if (eob && eob != Qnil)
    {
      point_t p = bp->coerce_to_restricted_point (eob);
      eob = Fmake_marker (bp->lbp);
      xbuffer_stream_eob (stream) = eob;
      xmarker_point (eob) = p;
    }
  return stream;
}

static void
valid_buffer_stream_p (lisp stream)
{
  check_stream (stream);
  if (!buffer_stream_p (stream))
    FEtype_error (stream, Qbuffer_stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);
  lisp marker = xbuffer_stream_marker (stream);
  check_marker (marker);
  if (!xmarker_buffer (marker))
    FEprogram_error (Edeleted_marker);
  lisp eob = xbuffer_stream_eob (stream);
  if (eob != Qnil)
    {
      check_marker (eob);
      if (!xmarker_buffer (eob))
        FEprogram_error (Edeleted_marker);
    }
}

lisp
Fbuffer_stream_buffer (lisp stream)
{
  valid_buffer_stream_p (stream);
  return xmarker_buffer (xbuffer_stream_marker (stream))->lbp;
}

lisp
Fbuffer_stream_point (lisp stream)
{
  valid_buffer_stream_p (stream);
  return make_fixnum (xmarker_point (xbuffer_stream_marker (stream)));
}

lisp
Fbuffer_stream_set_point (lisp stream, lisp point)
{
  valid_buffer_stream_p (stream);
  lisp marker = xbuffer_stream_marker (stream);
  Buffer *bp = xmarker_buffer (marker);
  xmarker_point (marker) = bp->coerce_to_restricted_point (point);
  return Qt;
}

static Buffer *
buffer_stream_point (lisp stream, Point &point)
{
  valid_buffer_stream_p (stream);
  lisp marker = xbuffer_stream_marker (stream);
  Buffer *bp = xmarker_buffer (marker);
  if (bp->b_stream_cache.p_chunk)
    bp->goto_char (bp->b_stream_cache, xmarker_point (marker));
  else
    bp->set_point (bp->b_stream_cache, xmarker_point (marker));
  point = bp->b_stream_cache;
  return bp;
}

static point_t
buffer_stream_eob (const Buffer *bp, lisp stream)
{
  lisp eob = xbuffer_stream_eob (stream);
  if (eob == Qnil)
    return bp->b_contents.p2;
  return xmarker_point (eob);
}

static void
write_buffer_stream (lisp stream, const Char *b, size_t size)
{
  Point point;
  Buffer *bp = buffer_stream_point (stream, point);
  bp->check_read_only ();
  bp->insert_chars (point, b, size);
  xmarker_point (xbuffer_stream_marker (stream)) = point.p_point;
}

lisp
Fconnect (lisp lhost, lisp lport, lisp keys)
{
  lisp stream = Qnil;
  try
    {
      Fbegin_wait_cursor ();
      int encoding = stream_encoding (find_keyword (Kencoding, keys, Kcanonical));
      sockinet::saddr addr (lhost, lport);
      stream = make_socket_stream ();
      protect_gc gcpro (stream);
      xsocket_stream_sock (stream) = new sockinet;
      xsocket_stream_sock (stream)->set_eof_error_p (0);
      xsocket_stream_sock (stream)->create ();
      xsocket_stream_sock (stream)->connect (addr);
      xsocket_stream_encoding (stream) = encoding;
    }
  catch (sock_error &e)
    {
      Fend_wait_cursor ();
      FEsocket_error (e.error_code ());
    }
  Fend_wait_cursor ();
  return stream;
}

lisp
Fmake_listen_socket (lisp lhost, lisp lport, lisp keys)
{
  lisp stream = Qnil;
  try
    {
      Fbegin_wait_cursor ();
      sockinet::saddr addr (lhost, lport);
      lisp lbacklog = find_keyword (Kbacklog, keys);
      int backlog = !lbacklog || lbacklog == Qnil ? 1 : fixnum_value (lbacklog);
      stream = make_socket_stream ();
      protect_gc gcpro (stream);
      xsocket_stream_sock (stream) = new sockinet;
      xsocket_stream_sock (stream)->set_eof_error_p (0);
      xsocket_stream_sock (stream)->create ();
      if (find_keyword_bool (Kreuseaddr, keys))
        xsocket_stream_sock (stream)->reuseaddr (1);
      xsocket_stream_sock (stream)->bind (addr);
      xsocket_stream_sock (stream)->listen (backlog);
    }
  catch (sock_error &e)
    {
      Fend_wait_cursor ();
      FEsocket_error (e.error_code ());
    }
  Fend_wait_cursor ();
  return stream;
}

static void
valid_socket_stream_p (lisp stream)
{
  check_stream (stream);
  if (!socket_stream_p (stream))
    FEtype_error (stream, Qsocket_stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);
}

lisp
Faccept_connection (lisp stream, lisp keys)
{
  valid_socket_stream_p (stream);
  int encoding = stream_encoding (find_keyword (Kencoding, keys, Kcanonical));
  lisp new_stream = Qnil;
  try
    {
      new_stream = make_socket_stream ();
      protect_gc gcpro (new_stream);
      SOCKET so = xsocket_stream_sock (stream)->accept ();
      try
        {
          xsocket_stream_sock (new_stream) = new sockinet (so);
          xsocket_stream_sock (new_stream)->set_eof_error_p (0);
          xsocket_stream_encoding (new_stream) = encoding;
        }
      catch (nonlocal_jump &)
        {
          sock::close_socket (so);
          throw;
        }
    }
  catch (sock_error &e)
    {
      FEsocket_error (e.error_code ());
    }
  return new_stream;
}

static void
close_socket_stream (lisp stream, int abort)
{
  sockinet *so = xsocket_stream_sock (stream);
  xsocket_stream_sock (stream) = 0;;
  xstream_open_p (stream) = 0;
  try
    {
      so->close (abort);
    }
  catch (sock_error &e)
    {
      delete so;
      FEsocket_error (e.error_code ());
    }
  delete so;
}

lisp
Fsocket_stream_local_address (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->localaddr (addr);
      return make_string (addr.addrstr ());
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_local_name (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->localaddr (addr);
      const char *name = addr.hostname ();
      return name ? make_string (name) : Qnil;
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_local_port (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->localaddr (addr);
      return make_fixnum (addr.port ());
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_peer_address (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->peeraddr (addr);
      return make_string (addr.addrstr ());
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_peer_name (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->peeraddr (addr);
      const char *name = addr.hostname ();
      return name ? make_string (name) : Qnil;
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_peer_port (lisp stream)
{
  valid_socket_stream_p (stream);
  try
    {
      sockinet::saddr addr;
      xsocket_stream_sock (stream)->peeraddr (addr);
      return make_fixnum (addr.port ());
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
}

lisp
Fsocket_stream_set_timeout (lisp stream, lisp ltimeout)
{
  valid_socket_stream_p (stream);
  int sec, usec;
  double d = ltimeout == Qnil ? -1.0 : coerce_to_double_float (ltimeout);
  if (d < 0)
    sec = usec = -1;
  else
    {
      double r, q;
      r = modf (d, &q);
      sec = int (q);
      usec = int (r * 1000000);
    }
  xsocket_stream_sock (stream)->send_timeout (sec, usec);
  xsocket_stream_sock (stream)->recv_timeout (sec, usec);
  return Qnil;
}

lisp
Fsocket_stream_get_timeout (lisp stream)
{
  valid_socket_stream_p (stream);
  const timeval &tv = xsocket_stream_sock (stream)->send_timeout ();
  if (tv.tv_sec < 0)
    return Qnil;
  if (tv.tv_usec)
    return make_double_float (tv.tv_sec + tv.tv_usec / 1000000.0);
  return make_fixnum (tv.tv_sec);
}

lisp
Fsocket_stream_set_oob_inline (lisp stream, lisp on)
{
  valid_socket_stream_p (stream);
  try
    {
      xsocket_stream_sock (stream)->oobinline (on != Qnil);
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
  return Qt;
}

lisp
Fsocket_stream_send_oob_data (lisp stream, lisp string)
{
  valid_socket_stream_p (stream);
  check_string (string);
  int l = w2sl (string);
  char *s = (char *)alloca (l);
  w2s (s, string);
  try
    {
      xsocket_stream_sock (stream)->send (s, l, MSG_OOB);
    }
  catch (sock_error &e)
    {
      return FEsocket_error (e.error_code ());
    }
  return Qnil;
}

lisp
Fmake_general_input_stream (lisp io_callback, lisp close_callback,
                            lisp listen_callback)
{
  lisp stream = make_stream (st_general_input);
  xgeneral_stream_io_callback (stream) = io_callback;
  xgeneral_stream_close_callback (stream) =
    close_callback ? close_callback : Qnil;
  xgeneral_input_stream_listen_callback (stream) =
    listen_callback ? listen_callback : Qnil;
  xgeneral_input_stream_string (stream) = Qnil;
  xgeneral_input_stream_index (stream) = 0;
  return stream;
}

lisp
Fmake_general_output_stream (lisp io_callback, lisp close_callback,
                             lisp flush_callback)
{
  lisp stream = make_stream (st_general_output);
  xgeneral_stream_io_callback (stream) = io_callback;
  xgeneral_stream_close_callback (stream) =
    close_callback ? close_callback : Qnil;
  xgeneral_output_stream_flush_callback (stream) =
    flush_callback ? flush_callback : Qnil;
  return stream;
}

lisp
Fclose (lisp stream, lisp keys)
{
  check_stream (stream);
  if (!xstream_open_p (stream))
    return Qnil;

  switch (xstream_type (stream))
    {
    case st_file_input:
      assert (xfile_stream_input (stream));
      assert (!xfile_stream_output (stream));
      fclose (xfile_stream_input (stream));
      xfile_stream_input (stream) = 0;
      break;

    case st_file_output:
      assert (!xfile_stream_input (stream));
      assert (xfile_stream_output (stream));
      fclose (xfile_stream_output (stream));
      xfile_stream_output (stream) = 0;
      close_file_stream (stream, find_keyword_bool (Kabort, keys));
      break;

    case st_file_io:
      assert (xfile_stream_input (stream));
      assert (xfile_stream_output (stream));
      fclose (xfile_stream_input (stream));
      if (xfile_stream_input (stream) != xfile_stream_output (stream))
        fclose (xfile_stream_output (stream));
      xfile_stream_input (stream) = 0;
      xfile_stream_output (stream) = 0;
      close_file_stream (stream, find_keyword_bool (Kabort, keys));
      break;

    case st_string_input:
      assert (xstring_stream_input (stream) != Qnil);
      assert (xstring_stream_output (stream) == Qnil);
      xstring_stream_input (stream) = Qnil;
      break;

    case st_string_output:
      assert (xstring_stream_input (stream) == Qnil);
      assert (xstring_stream_output (stream) != Qnil);
      xstring_stream_output (stream) = Qnil;
      break;

    case st_synonym:
    case st_two_way:
    case st_echo:
    case st_broadcast:
    case st_concatenated:
      break;

    case st_status:
    case st_keyboard:
      return Qt;

    case st_wstream:
      xwstream_stream_wstream (stream) = 0;
      break;

    case st_buffer:
      xbuffer_stream_marker (stream) = Qnil;
      xbuffer_stream_eob (stream) = Qnil;
      break;

    case st_socket:
      assert (xsocket_stream_sock (stream));
      close_socket_stream (stream, find_keyword_bool (Kabort, keys));
      break;

    case st_general_input:
    case st_general_output:
      if (xgeneral_stream_close_callback (stream) != Qnil)
        funcall_1 (xgeneral_stream_close_callback (stream),
                   find_keyword (Kabort, keys));
      break;

    default:
      assert (0);
      break;
    }

  xstream_open_p (stream) = 0;
  return Qt;
}

lisp
Fbroadcast_stream_streams (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_broadcast)
    FEtype_error (stream, Qbroadcast_stream);
  return Fcopy_list (xcomposite_stream_output (stream));
}

lisp
Fconcatenated_stream_streams (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_concatenated)
    FEtype_error (stream, Qconcatenated_stream);
  return Fcopy_list (xcomposite_stream_input (stream));
}

lisp
Fecho_stream_input_stream (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_echo)
    FEtype_error (stream, Qecho_stream);
  return xcomposite_stream_input (stream);
}

lisp
Fecho_stream_output_stream (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_echo)
    FEtype_error (stream, Qecho_stream);
  return xcomposite_stream_output (stream);
}

lisp
Ftwo_way_stream_input_stream (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_two_way)
    FEtype_error (stream, Qtwo_way_stream);
  return xcomposite_stream_input (stream);
}

lisp
Ftwo_way_stream_output_stream (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_two_way)
    FEtype_error (stream, Qtwo_way_stream);
  return xcomposite_stream_output (stream);
}

lisp
Fsynonym_stream_symbol (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) != st_synonym)
    FEtype_error (stream, Qsynonym_stream);
  return xcomposite_stream_input (stream);
}

lisp
Finteractive_stream_p (lisp stream)
{
  check_stream (stream);
  if (!xstream_open_p (stream))
    return Qnil;
  switch (xstream_type (stream))
    {
    case st_file_input:
      return boole (_isatty (_fileno (xfile_stream_input (stream))));

    case st_file_output:
      return boole (_isatty (_fileno (xfile_stream_output (stream))));

    case st_file_io:
      return boole (_isatty (_fileno (xfile_stream_input (stream)))
                    && _isatty (_fileno (xfile_stream_output (stream))));

    case st_keyboard:
      return Qt;

    default:
      return Qnil;
    }
}

lisp
Fstream_encoding (lisp stream)
{
  check_stream (stream);
  int encoding;
  switch (xstream_type (stream))
    {
    case st_file_input:
    case st_file_output:
    case st_file_io:
      encoding = xfile_stream_encoding (stream);
      break;

    case st_socket:
      encoding = xsocket_stream_encoding (stream);
      break;

    default:
      return Qnil;
    }

  switch (encoding)
    {
    case lstream::ENCODE_CANON:
      return Kcanonical;

    case lstream::ENCODE_RAW:
      return Kraw;

    default:
      return Kbinary;
    }
}

lisp
Fset_stream_encoding (lisp stream, lisp lencoding)
{
  check_stream (stream);
  int encoding = stream_encoding (lencoding);
  switch (xstream_type (stream))
    {
    case st_file_input:
    case st_file_output:
    case st_file_io:
      xfile_stream_encoding (stream) = encoding;
      return Qt;

    case st_socket:
      xsocket_stream_encoding (stream) = encoding;
      return Qt;

    default:
      return Qnil;
    }
}

lChar
readc_stream (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      if (!xstream_open_p (stream))
        FEtype_error (stream, Qopen_stream);

      if (xstream_type (stream) == st_keyboard)
        {
          check_kbd_enable ();
          return app.kbdq.fetch (0, 0);
        }

      lChar cc = xstream_pending (stream);
      if (cc != lChar_EOF)
        {
          xstream_pending (stream) = lChar_EOF;
          if (cc == '\n')
            xstream_linenum (stream)++;
          return cc;
        }

      switch (xstream_type (stream))
        {
        case st_file_io:
        case st_file_input:
          {
            int c = getc (xfile_stream_input (stream));
            if (c == EOF)
              return lChar_EOF;
            if (xfile_stream_encoding (stream) != lstream::ENCODE_BINARY)
              {
                if (SJISP (c))
                  {
                    int c2 = getc (xfile_stream_input (stream));
                    return c2 == EOF ? c : ((c << 8) | c2);
                  }
                else if (xfile_stream_encoding (stream) == lstream::ENCODE_CANON
                         && c == '\r')
                  {
                    int c2 = getc (xfile_stream_input (stream));
                    if (c2 == '\n')
                      c = c2;
                    else
                      ungetc (c2, xfile_stream_input (stream));
                  }
              }
            if (c == '\n')
              xstream_linenum (stream)++;
            return c;
          }

        case st_file_output:
        case st_string_output:
        case st_broadcast:
        case st_status:
        case st_wstream:
        case st_general_output:
          FEtype_error (stream, Qinput_stream);
          return 0;

        case st_string_input:
          {
            lisp string = xstring_stream_input (stream);
            if (xstring_stream_start (stream) >= xstring_length (string)
                || xstring_stream_start (stream) >= xstring_stream_end (stream))
              return lChar_EOF;
            cc = xstring_contents (string) [xstring_stream_start (stream)++];
            if (cc == '\n')
              xstream_linenum (stream)++;
            return cc;
          }

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_input (stream), selected_buffer ());
          break;

        case st_two_way:
          QUIT;
          stream = xcomposite_stream_input (stream);
          break;

        case st_echo:
          QUIT;
          cc = readc_stream (xcomposite_stream_input (stream));
          if (cc != lChar_EOF)
            writec_stream (xcomposite_stream_output (stream), Char (cc));
          if (cc == '\n')
            xstream_linenum (stream)++;
          return cc;

        case st_concatenated:
          while (consp (xcomposite_stream_input (stream)))
            {
              cc = readc_stream (xcar (xcomposite_stream_input (stream)));
              if (cc != lChar_EOF || !consp (xcomposite_stream_input (stream)))
                {
                  if (cc == '\n')
                    xstream_linenum (stream)++;
                  return cc;
                }
              QUIT;
              xcomposite_stream_input (stream) = xcdr (xcomposite_stream_input (stream));
            }
          return lChar_EOF;

        case st_buffer:
          {
            Point point;
            Buffer *bp = buffer_stream_point (stream, point);
            if (point.p_point >= buffer_stream_eob (bp, stream))
              return lChar_EOF;
            xmarker_point (xbuffer_stream_marker (stream)) = point.p_point + 1;
            return point.ch ();
          }

        case st_socket:
          try
            {
              int c = xsocket_stream_sock (stream)->sgetc ();
              if (c == sock::eof)
                return lChar_EOF;
              if (xsocket_stream_encoding (stream) != lstream::ENCODE_BINARY)
                {
                  if (SJISP (c))
                    {
                      int c2 = xsocket_stream_sock (stream)->sgetc ();
                      return c2 == sock::eof ? c : ((c << 8) | c2);
                    }
                  else if (xsocket_stream_encoding (stream) == lstream::ENCODE_CANON
                           && c == '\r')
                    {
                      int c2 = xsocket_stream_sock (stream)->sgetc ();
                      if (c2 == '\n')
                        c = c2;
                      else
                        xsocket_stream_sock (stream)->sungetc (c2);
                    }
                }
              if (c == '\n')
                xstream_linenum (stream)++;
              return c;
            }
          catch (sock_error &e)
            {
              FEsocket_error (e.error_code ());
            }

        case st_general_input:
          if (!stringp (xgeneral_input_stream_string (stream))
              || (xgeneral_input_stream_index (stream)
                  >= xstring_length (xgeneral_input_stream_string (stream))))
            {
              suppress_gc sgc;
              lisp r = Ffuncall (xgeneral_stream_io_callback (stream), Qnil);
              if (r == Qnil)
                return lChar_EOF;
              if (charp (r))
                {
                  cc = xchar_code (r);
                  if (cc == '\n')
                    xstream_linenum (stream)++;
                  return cc;
                }
              if (!stringp (r))
                FEtype_error (r, xsymbol_value (Qor_string_character));
              if (!xstring_length (r))
                return lChar_EOF;
              xgeneral_input_stream_string (stream) = r;
              xgeneral_input_stream_index (stream) = 0;
            }
          cc = xstring_contents (xgeneral_input_stream_string (stream))
            [xgeneral_input_stream_index (stream)++];
          if (cc == '\n')
            xstream_linenum (stream)++;
          return cc;

        default:
          assert (0);
          return lChar_EOF;
        }
    }
}

int
listen_stream (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      if (!xstream_open_p (stream))
        FEtype_error (stream, Qopen_stream);

      if (xstream_type (stream) == st_keyboard)
        {
          check_kbd_enable ();
          return app.kbdq.listen ();
        }

      lChar cc = xstream_pending (stream);
      if (cc != lChar_EOF)
        return 1;

      switch (xstream_type (stream))
        {
        case st_file_io:
        case st_file_input:
          {
#ifdef _MSC_VER
            if (xfile_stream_input (stream)->_cnt > 0)
              return 1;
#else
# error "Not Supported"
#endif
            if (WaitForSingleObject (HANDLE (_get_osfhandle (_fileno (xfile_stream_input (stream)))),
                                     0) == WAIT_TIMEOUT)
              return 0;

            int c = getc (xfile_stream_input (stream));
            ungetc (c, xfile_stream_input (stream));
            return c != EOF;
          }

        case st_file_output:
        case st_string_output:
        case st_broadcast:
        case st_status:
        case st_wstream:
        case st_general_output:
          FEtype_error (stream, Qinput_stream);
          return 0;

        case st_string_input:
          {
            lisp string = xstring_stream_input (stream);
            return (xstring_stream_start (stream) < xstring_length (string)
                    || xstring_stream_start (stream) < xstring_stream_end (stream));
          }

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_input (stream), selected_buffer ());
          break;

        case st_two_way:
        case st_echo:
          QUIT;
          stream = xcomposite_stream_input (stream);
          break;

        case st_concatenated:
          if (!consp (xcomposite_stream_input (stream)))
            return 1;
          QUIT;
          stream = xcar (xcomposite_stream_input (stream));
          break;

        case st_buffer:
          {
            Point point;
            Buffer *bp = buffer_stream_point (stream, point);
            return point.p_point < buffer_stream_eob (bp, stream);
          }

        case st_socket:
          try
            {
              xsocket_stream_sock (stream)->sflush ();
              return xsocket_stream_sock (stream)->no_hang_p ();
            }
          catch (sock_error &e)
            {
              FEsocket_error (e.error_code ());
            }

        case st_general_input:
          if (stringp (xgeneral_input_stream_string (stream))
              && (xgeneral_input_stream_index (stream)
                  < xstring_length (xgeneral_input_stream_string (stream))))
            return 1;
          if (xgeneral_input_stream_listen_callback (stream) != Qnil)
            {
              suppress_gc sgc;
              return Ffuncall (xgeneral_input_stream_listen_callback (stream), Qnil) != Qnil;
            }
          return 0;

        default:
          assert (0);
          return 0;
        }
    }
}

lChar
peekc_stream (lisp stream)
{
  check_stream (stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);

  lChar cc;
  switch (xstream_type (stream))
    {
    case st_keyboard:
      check_kbd_enable ();
      cc = app.kbdq.fetch (0, 0);
      app.kbdq.push_back (cc);
      break;

    case st_buffer:
      {
        Point point;
        Buffer *bp = buffer_stream_point (stream, point);
        cc = point.p_point < buffer_stream_eob (bp, stream) ? point.ch () : lChar_EOF;
        break;
      }

    default:
      cc = xstream_pending (stream);
      if (cc == lChar_EOF)
        {
          cc = readc_stream (stream);
          xstream_pending (stream) = cc;
        }
      break;
    }

  return cc;
}

int
unreadc_stream (lChar cc, lisp stream)
{
  check_stream (stream);
  if (!xstream_open_p (stream))
    FEtype_error (stream, Qopen_stream);
  switch (xstream_type (stream))
    {
    case st_keyboard:
      app.kbdq.push_back (cc);
      break;

    case st_buffer:
      {
        Point point;
        Buffer *bp = buffer_stream_point (stream, point);
        if (point.p_point <= bp->b_contents.p1)
          return 0;
        xmarker_point (xbuffer_stream_marker (stream)) = point.p_point - 1;
        break;
      }

    default:
      if (xstream_pending (stream) == lChar_EOF)
        {
          xstream_pending (stream) = cc;
          if (cc == '\n')
            xstream_linenum (stream)--;
        }
      else
        return 0;
      break;
    }
  return 1;
}

long
stream_linenum (lisp stream)
{
  check_stream (stream);
  if (xstream_type (stream) == st_buffer)
    {
      Point point;
      Buffer *bp = buffer_stream_point (stream, point);
      return bp->point_linenum (point.p_point);
    }
  else
    return xstream_linenum (stream);
}

static void
putc_file_stream (lisp stream, Char cc)
{
  if (DBCP (cc))
    putc (cc >> 8, xfile_stream_output (stream));
  else if (xfile_stream_encoding (stream) == lstream::ENCODE_CANON
           && cc == '\n')
    putc ('\r', xfile_stream_output (stream));
  putc (cc, xfile_stream_output (stream));
}

static void
putc_sock_stream (lisp stream, Char cc)
{
  if (DBCP (cc))
    xsocket_stream_sock (stream)->sputc (cc >> 8);
  else if (xsocket_stream_encoding (stream) == lstream::ENCODE_CANON
           && cc == '\n')
    xsocket_stream_sock (stream)->sputc ('\r');
  xsocket_stream_sock (stream)->sputc (cc);
}

void
writec_stream (lisp stream, Char cc)
{
  while (1)
    {
      check_stream (stream);
      if (!xstream_open_p (stream))
        FEtype_error (stream, Qopen_stream);

      switch (xstream_type (stream))
        {
        case st_file_input:
        case st_string_input:
        case st_concatenated:
        case st_keyboard:
        case st_general_input:
          FEtype_error (stream, Qoutput_stream);
          return;

        case st_file_output:
        case st_file_io:
          putc_file_stream (stream, cc);
          xstream_column (stream) = update_column (xstream_column (stream), cc);
          return;

        case st_string_output:
          Fvector_push_extend (make_char (cc), xstring_stream_output (stream),
                               make_fixnum (128));
          xstream_column (stream) = update_column (xstream_column (stream), cc);
          return;

        case st_broadcast:
          {
            for (lisp p = xcomposite_stream_output (stream); consp (p); p = xcdr (p))
              {
                QUIT;
                writec_stream (xcar (p), cc);
              }
            return;
          }

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_output (stream), selected_buffer ());
          break;

        case st_two_way:
        case st_echo:
          QUIT;
          stream = xcomposite_stream_output (stream);
          break;

        case st_status:
          app.status_window.putc (cc);
          xstream_column (stream) = update_column (xstream_column (stream), cc);
          return;

        case st_buffer:
          write_buffer_stream (stream, &cc, 1);
          return;

        case st_wstream:
          if (xwstream_stream_wstream (stream))
            xwstream_stream_wstream (stream)->add (cc);
          return;

        case st_socket:
          try
            {
              putc_sock_stream (stream, cc);
              xstream_column (stream) = update_column (xstream_column (stream), cc);
            }
          catch (sock_error &e)
            {
              FEsocket_error (e.error_code ());
            }
          return;

        case st_general_output:
          funcall_1 (xgeneral_stream_io_callback (stream),
                     make_string (&cc, 1));
          xstream_column (stream) = update_column (xstream_column (stream), cc);
          return;

        default:
          assert (0);
          return;
        }
    }
}

void
write_stream (lisp stream, const Char *b, size_t size)
{
  while (1)
    {
      check_stream (stream);
      if (!xstream_open_p (stream))
        FEtype_error (stream, Qopen_stream);

      switch (xstream_type (stream))
        {
        case st_file_input:
        case st_string_input:
        case st_concatenated:
        case st_keyboard:
        case st_general_input:
          FEtype_error (stream, Qoutput_stream);
          return;

        case st_file_output:
        case st_file_io:
          {
            for (const Char *be = b + size; b < be; b++)
              putc_file_stream (stream, *b);
            xstream_column (stream) = update_column (xstream_column (stream), b, size);
            return;
          }

        case st_string_output:
          {
            lisp string = xstring_stream_output (stream);
            int need = xstring_length (string) + size;
            int room = xstring_dimension (string);
            if (need > room)
              {
                if (!xarray_adjustable (string))
                  FEprogram_error (Evector_is_not_adjustable, string);
                need = (need + 128) & ~127;
                realloc_element (string, need - room, sizeof (Char));
              }
            bcopy (b, &xstring_contents (string) [xstring_length (string)], size);
            xstring_length (string) += size;
            xstream_column (stream) = update_column (xstream_column (stream), b, size);
            return;
          }

        case st_broadcast:
          {
            for (lisp p = xcomposite_stream_output (stream); consp (p); p = xcdr (p))
              {
                QUIT;
                write_stream (xcar (p), b, size);
              }
            return;
          }

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_output (stream), selected_buffer ());
          break;

        case st_two_way:
        case st_echo:
          QUIT;
          stream = xcomposite_stream_output (stream);
          break;

        case st_status:
          app.status_window.puts (b, size);
          xstream_column (stream) = update_column (xstream_column (stream), b, size);
          return;

        case st_buffer:
          write_buffer_stream (stream, b, size);
          return;

        case st_wstream:
          if (xwstream_stream_wstream (stream))
            xwstream_stream_wstream (stream)->add (b, size);
          return;

        case st_socket:
          try
            {
              for (const Char *be = b + size; b < be; b++)
                putc_sock_stream (stream, *b);
              xstream_column (stream) = update_column (xstream_column (stream), b, size);
            }
          catch (sock_error &e)
            {
              FEsocket_error (e.error_code ());
            }
          return;

        case st_general_output:
          funcall_1 (xgeneral_stream_io_callback (stream),
                     make_string (b, size));
          xstream_column (stream) = update_column (xstream_column (stream), b, size);
          return;

        default:
          assert (0);
          return;
        }
    }
}

int
get_stream_column (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      switch (xstream_type (stream))
        {
        case st_file_input:
        case st_string_input:
        case st_concatenated:
        case st_keyboard:
        case st_general_input:
          FEtype_error (stream, Qoutput_stream);
          return 0;

        case st_file_output:
        case st_file_io:
        case st_string_output:
        case st_status:
        case st_socket:
        case st_general_output:
          return xstream_column (stream);

        case st_wstream:
          if (xwstream_stream_wstream (stream))
            return xwstream_stream_wstream (stream)->columns ();
          return 0;

        case st_broadcast:
          if (!consp (xcomposite_stream_output (stream)))
            return 0;
          QUIT;
          stream = xcar (xcomposite_stream_output (stream));
          break;

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_output (stream), selected_buffer ());
          break;

        case st_two_way:
        case st_echo:
          QUIT;
          stream = xcomposite_stream_output (stream);
          break;

        case st_buffer:
          {
            Point point;
            Buffer *bp = buffer_stream_point (stream, point);
            return bp->point_column (point);
          }

        default:
          assert (0);
          return 0;
        }
    }
}

void
flush_stream (lisp stream)
{
  while (1)
    {
      check_stream (stream);
      if (!xstream_open_p (stream))
        FEtype_error (stream, Qopen_stream);
      switch (xstream_type (stream))
        {
        case st_file_input:
        case st_string_input:
        case st_concatenated:
        case st_keyboard:
        case st_general_input:
          FEtype_error (stream, Qoutput_stream);
          return;

        case st_file_output:
        case st_file_io:
          fflush (xfile_stream_output (stream));
          return;

        case st_string_output:
          return;

        case st_broadcast:
          {
            for (lisp p = xcomposite_stream_output (stream); consp (p); p = xcdr (p))
              {
                QUIT;
                flush_stream (xcar (p));
              }
            return;
          }

        case st_synonym:
          QUIT;
          stream = symbol_value (xcomposite_stream_output (stream), selected_buffer ());
          break;

        case st_two_way:
        case st_echo:
          QUIT;
          stream = xcomposite_stream_output (stream);
          break;

        case st_status:
          app.status_window.flush ();
          return;

        case st_buffer:
          return;

        case st_wstream:
          return;

        case st_socket:
          try
            {
              xsocket_stream_sock (stream)->sflush ();
            }
          catch (sock_error &e)
            {
              FEsocket_error (e.error_code ());
            }
          return;

        case st_general_output:
          if (xgeneral_output_stream_flush_callback (stream) != Qnil)
            {
              suppress_gc sgc;
              Ffuncall (xgeneral_output_stream_flush_callback (stream), Qnil);
            }
          return;

        default:
          assert (0);
          return;
        }
    }
}

static lisp
create_std_stream (stream_type type, FILE *fi, FILE *fo)
{
  lisp stream = make_file_stream (type);
  xfile_stream_input (stream) = fi;
  xfile_stream_output (stream) = fo;
  return stream;
}

void
create_std_streams ()
{
  xsymbol_value (Vterminal_io) = create_std_stream (st_file_io, stdin, stdout);
#if 0
  xsymbol_value (Vstandard_input) = create_std_stream (st_file_input, stdin, 0);
  xsymbol_value (Vstandard_output) = create_std_stream (st_file_output, 0, stdout);
#else
  xsymbol_value (Qkeyboard) = make_keyboard_stream ();
  xsymbol_value (Vstandard_input) = xsymbol_value (Qkeyboard);
  xsymbol_value (Vstatus_window) = make_status_stream ();
  xsymbol_value (Vstandard_output) = xsymbol_value (Vstatus_window);
#endif
  xsymbol_value (Verror_output) = create_std_stream (st_file_output, 0, stderr);
  xsymbol_value (Vquery_io) = xsymbol_value (Vterminal_io);
  xsymbol_value (Vdebug_io) = xsymbol_value (Vterminal_io);
  xsymbol_value (Vtrace_output) = xsymbol_value (Vdebug_io);
  xsymbol_value (Vwstream_stream) = make_wstream_stream ();
}
