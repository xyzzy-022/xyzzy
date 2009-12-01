#ifndef _byte_stream_h_
#define _byte_stream_h_

#include "StrBuf.h"
#include "encoding.h"

typedef xoutput_stream <u_char> byte_output_stream;
typedef xinput_stream <u_char> byte_input_stream;
typedef xoutput_stream <Char> Char_output_stream;
typedef xinput_stream <Char> Char_input_stream;

class byte_input_string_stream: public byte_input_stream
{
  u_char s_buf[1024];
  const Char *s_wp;
  const Char *const s_we;
  virtual int refill ();
public:
  byte_input_string_stream (const Char *b, int l)
       : s_wp (b), s_we (b + l) {}
  byte_input_string_stream (lisp string)
       : s_wp (xstring_contents (string)),
         s_we (xstring_contents (string) + xstring_length (string)) {}
};

class byte_input_streams_stream: public byte_input_stream
{
  u_char s_buf[1024];
  lisp s_stream;
  virtual int refill ();
public:
  byte_input_streams_stream (lisp stream) : s_stream (stream) {}
};

class byte_output_wstream: public byte_output_stream
{
  u_char s_buf[1024];
protected:
  byte_output_wstream () : byte_output_stream (s_buf, s_buf + sizeof s_buf) {}
  virtual u_char *sflush (u_char *, u_char *, int);
  virtual void swrite (const Char *, int) = 0;
};

class byte_output_string_stream: public byte_output_wstream, public StrBuf
{
  char s_buf[2040];
  virtual void swrite (const Char *b, int l) {add (b, l);}
public:
  byte_output_string_stream () : StrBuf (s_buf, sizeof s_buf) {}
};

class byte_output_streams_stream: public byte_output_wstream
{
  lisp s_stream;
  virtual void swrite (const Char *b, int l)
    {write_stream (s_stream, b, l);}
public:
  byte_output_streams_stream (lisp stream) : s_stream (stream) {}
};

class Char_input_string_stream: public Char_input_stream
{
public:
  Char_input_string_stream (const Char *b, int l)
       : Char_input_stream (b, b + l) {}
  Char_input_string_stream (lisp string)
       : Char_input_stream (xstring_contents (string),
                            xstring_contents (string) + xstring_length (string)) {}
};

class Char_input_streams_stream: public Char_input_stream
{
  Char s_buf[1024];
  lisp s_stream;
  virtual int refill ();
public:
  Char_input_streams_stream (lisp stream) : s_stream (stream) {}
};

class Char_output_wstream: public Char_output_stream
{
  Char s_buf[1024];
protected:
  Char_output_wstream () : Char_output_stream (s_buf, s_buf + numberof (s_buf)) {}
  virtual Char *sflush (Char *b, Char *e, int)
    {if (b != e) swrite (b, e - b); return b;}
  virtual void swrite (const Char *, int) = 0;
};

class Char_output_string_stream: public Char_output_wstream, public StrBuf
{
  char s_buf[2040];
  virtual void swrite (const Char *b, int l) {add (b, l);}
public:
  Char_output_string_stream () : StrBuf (s_buf, sizeof s_buf) {}
};

class Char_output_streams_stream: public Char_output_wstream
{
  lisp s_stream;
  virtual void swrite (const Char *b, int l)
    {write_stream (s_stream, b, l);}
public:
  Char_output_streams_stream (lisp stream) : s_stream (stream) {}
};

class xstream_ibyte_helper
{
  union
    {
      XBUFDEF (byte_input_string_stream);
      XBUFDEF (byte_input_streams_stream);
    } s_xbuf;
  byte_input_stream *s_stream;
public:
  xstream_ibyte_helper (lisp);
  ~xstream_ibyte_helper () {delete s_stream;}
  operator byte_input_stream & () const {return *s_stream;}
  byte_input_stream *operator -> () const {return s_stream;}
};

class xstream_obyte_helper
{
  union
    {
      XBUFDEF (byte_output_string_stream);
      XBUFDEF (byte_output_streams_stream);
    } s_xbuf;
  byte_output_stream *s_stream;
  int s_string_stream_p;
public:
  xstream_obyte_helper (lisp);
  ~xstream_obyte_helper () {delete s_stream;}
  operator byte_output_stream & () const {return *s_stream;}
  byte_output_stream *operator -> () const {return s_stream;}
  lisp result () const
    {return (s_string_stream_p
             ? static_cast <byte_output_string_stream *> (s_stream)->make_string () : Qt);}
};

class xstream_iChar_helper
{
  union
    {
      XBUFDEF (Char_input_string_stream);
      XBUFDEF (Char_input_streams_stream);
    } s_xbuf;
  Char_input_stream *s_stream;
public:
  xstream_iChar_helper (lisp);
  ~xstream_iChar_helper () {delete s_stream;}
  operator Char_input_stream & () const {return *s_stream;}
};

class xstream_oChar_helper
{
  union
    {
      XBUFDEF (Char_output_string_stream);
      XBUFDEF (Char_output_streams_stream);
    } s_xbuf;
  Char_output_stream *s_stream;
  int s_string_stream_p;
public:
  xstream_oChar_helper (lisp);
  ~xstream_oChar_helper () {delete s_stream;}
  operator Char_output_stream & () const {return *s_stream;}
  lisp result () const
    {return (s_string_stream_p
             ? static_cast <Char_output_string_stream *> (s_stream)->make_string () : Qt);}
};

#undef MAX__

void copy_xstream (xread_stream &, byte_output_stream &);
void copy_xstream (xread_stream &, Char_output_stream &);
void copy_xstream (xwrite_stream &, byte_output_stream &);

#endif /* _byte_stream_h_ */
