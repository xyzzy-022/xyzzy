#include "stdafx.h"
#include "ed.h"
#include "byte-stream.h"

int
byte_input_string_stream::refill ()
{
  u_char *b = s_buf, *const be = s_buf + sizeof s_buf - 1;
  while (b < be && s_wp < s_we)
    {
      Char cc = *s_wp++;
      if (DBCP (cc))
        *b++ = u_char (cc >> 8);
      *b++ = u_char (cc);
    }
  return setbuf (s_buf, b);
}

int
byte_input_streams_stream::refill ()
{
  u_char *b = s_buf, *const be = s_buf + sizeof s_buf - 1;
  while (b < be)
    {
      lChar lcc = readc_stream (s_stream);
      if (lcc == lChar_EOF)
        break;
      if (DBCP (Char (lcc)))
        *b++ = u_char (lcc >> 8);
      *b++ = u_char (lcc);
    }
  return setbuf (s_buf, b);
}

u_char *
byte_output_wstream::sflush (u_char *b0, u_char *be, int eofp)
{
  u_char *b = b0;
  Char *w, wbuf[sizeof s_buf];
  for (w = wbuf; b < be;)
    {
      if (SJISP (*b))
        {
          if (b + 1 == be)
            {
              if (eofp)
                *w++ = *b;
              else
                *b0++ = *b;
              break;
            }
          *w++ = (*b << 8) | b[1];
          b += 2;
        }
      else
        *w++ = *b++;
    }
  if (w - wbuf)
    swrite (wbuf, w - wbuf);
  return b0;
}

int
Char_input_streams_stream::refill ()
{
  Char *b = s_buf, *const be = s_buf + numberof (s_buf);
  while (b < be)
    {
      lChar lcc = readc_stream (s_stream);
      if (lcc == lChar_EOF)
        break;
      *b++ = Char (lcc);
    }
  return setbuf (s_buf, b);
}

xstream_ibyte_helper::xstream_ibyte_helper (lisp obj)
{
  if (stringp (obj))
    s_stream = new (&s_xbuf) byte_input_string_stream (obj);
  else if (obj == Qnil || obj == Qt || streamp (obj))
    s_stream = new (&s_xbuf) byte_input_streams_stream (input_stream (obj));
  else
    FEtype_error (obj, xsymbol_value (Qor_string_stream));
}

xstream_obyte_helper::xstream_obyte_helper (lisp obj)
{
  if (!obj || obj == Qnil)
    {
      s_stream = new (&s_xbuf) byte_output_string_stream;
      s_string_stream_p = 1;
    }
  else
    {
      if (obj == Qt)
        obj = xsymbol_value (Vstandard_output);
      check_stream (obj);
      s_stream = new (&s_xbuf) byte_output_streams_stream (obj);
      s_string_stream_p = 0;
    }
}

xstream_iChar_helper::xstream_iChar_helper (lisp obj)
{
  if (stringp (obj))
    s_stream = new (&s_xbuf) Char_input_string_stream (obj);
  else if (obj == Qnil || obj == Qt || streamp (obj))
    s_stream = new (&s_xbuf) Char_input_streams_stream (input_stream (obj));
  else
    FEtype_error (obj, xsymbol_value (Qor_string_stream));
}

xstream_oChar_helper::xstream_oChar_helper (lisp obj)
{
  if (!obj || obj == Qnil)
    {
      s_stream = new (&s_xbuf) Char_output_string_stream;
      s_string_stream_p = 1;
    }
  else
    {
      if (obj == Qt)
        obj = xsymbol_value (Vstandard_output);
      check_stream (obj);
      s_stream = new (&s_xbuf) Char_output_streams_stream (obj);
      s_string_stream_p = 0;
    }
}

void
copy_xstream (xread_stream &i, byte_output_stream &o)
{
  int c;
  while ((c = i.get ()) != xstream::eof)
    o.put (c);
  o.flush (1);
}

void
copy_xstream (xread_stream &is, Char_output_stream &os)
{
  int c;
  while ((c = is.get ()) != xstream::eof)
    os.put (c);
  os.flush (1);
}

void
copy_xstream (xwrite_stream &is, byte_output_stream &os)
{
  int c;
  while ((c = is.get ()) != xstream::eof)
    os.put (c);
  os.flush (1);
}
