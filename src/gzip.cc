#include "stdafx.h"
#include "ed.h"
#include "byte-stream.h"
#define ZEXPORT __cdecl
#include "zlib/zlib.h"

#define ZBUFSIZE 4096
static u_char gz_magic[] = {0x1f, 0x8b};

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
//#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define CONTINUATION 0x02 /* bit 1 set: continuation of multi-part gzip file */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define ENCRYPTED    0x20 /* bit 5 set: file is encrypted */
#define RESERVED     0xC0 /* bit 6,7:   reserved */

#if MAX_MEM_LEVEL >= 8
#define DEF_MEM_LEVEL 8
#else
#define DEF_MEM_LEVEL MAX_MEM_LEVEL
#endif

#define OS_CODE  0x0b

static int
read_bytes (byte_input_stream &is, u_char *buf, int size)
{
  u_char *b = buf, *const be = buf + size;
  int c;
  while (b < be && (c = is.get ()) != xstream::eof)
    *b++ = c;
  return b - buf;
}

static void
write_bytes (byte_output_stream &os, const u_char *buf, int size)
{
  for (const u_char *b = buf, *const be = buf + size; b < be; b++)
    os.put (*b);
}

static void
skip_gz_header (byte_input_stream &in)
{
  int i, c;

  for (i = 0; i < 2; i++)
    if (in.get () != gz_magic[i])
      FEsimple_error (Enot_in_gzip_format);

  if (in.get () != Z_DEFLATED)
    FEsimple_error (Enot_in_gzip_format);

  int flags = in.get ();
  if (flags & (RESERVED | ENCRYPTED | CONTINUATION))
    FEsimple_error (Enot_in_gzip_format);

  /* Discard time, xflags and OS code: */
  for (i = 0; i < 6; i++)
    if (in.get () == xstream::eof)
      FEsimple_error (Enot_in_gzip_format);

  if (flags & EXTRA_FIELD)
    {
      int len = in.get ();
      len += in.get () << 8;
      while (len-- > 0 && in.get () != xstream::eof)
        ;
    }

  if (flags & ORIG_NAME)
    while ((c = in.get ()) && c != xstream::eof)
      ;

  if (flags & COMMENT)
    while ((c = in.get ()) && c != xstream::eof)
      ;
}

class z_inflate_stream: public z_stream_s
{
public:
  z_inflate_stream ()
    {
      zalloc = 0;
      zfree = 0;
      opaque = 0;
    }
  ~z_inflate_stream ()
    {
      if (zalloc)
        inflateEnd (this);
    }
};

lisp
Fsi_inflate_stream (lisp input, lisp output)
{
  z_inflate_stream z;
  u_char ibuf[ZBUFSIZE];
  u_char obuf[ZBUFSIZE];

  xstream_ibyte_helper is (input);
  xstream_obyte_helper os (output);

  skip_gz_header (is);

  z.next_in = 0;
  z.avail_in = 0;
  z.next_out = obuf;
  z.avail_out = sizeof obuf;

  if (inflateInit2 (&z, -MAX_WBITS) != Z_OK)
    FEstorage_error ();

  while (1)
    {
      if (!z.avail_in)
        {
          z.next_in = ibuf;
          z.avail_in = read_bytes (is, ibuf, sizeof ibuf);
        }

      int r = inflate (&z, Z_NO_FLUSH);
      if (r == Z_STREAM_END)
        break;
      if (r != Z_OK)
        FEsimple_error (Egzip_data_error);

      if (!z.avail_out)
        {
          write_bytes (os, obuf, sizeof obuf);
          z.next_out = obuf;
          z.avail_out = sizeof obuf;
        }
    }

  write_bytes (os, obuf, sizeof obuf - z.avail_out);
  os->flush (1);
  return os.result ();
}

#if 0
class z_deflate_stream: public z_stream_s
{
public:
  z_deflate_stream ()
    {
      zalloc = 0;
      zfree = 0;
      opaque = 0;
    }
  ~z_deflate_stream ()
    {
      if (zalloc)
        deflateEnd (this);
    }
};

lisp
Fsi_deflate_stream (lisp input, lisp output, lisp llevel)
{
  z_deflate_stream z;
  u_char ibuf[ZBUFSIZE];
  u_char obuf[ZBUFSIZE];

  int level;
  if (!llevel || llevel == Qnil)
    level = Z_DEFAULT_COMPRESSION;
  else
    {
      level = fixnum_value (llevel);
      if (level < 0 || level > 9)
        FErange_error (llevel);
    }

  xstream_ibyte_helper is (input);
  xstream_obyte_helper os (output);

  if (deflateInit2 (&z, level, Z_DEFLATED, -MAX_WBITS,
                    DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY) != Z_OK)
    FEstorage_error ();

  os->put (gz_magic[0]);
  os->put (gz_magic[1]);
  os->put (Z_DEFLATED);
  for (int i = 0; i < 6; i++)
    os->put (0);
  os->put (OS_CODE);

  z.avail_in = 0;
  z.next_out = obuf;
  z.avail_out = sizeof obuf;

  int flush = Z_NO_FLUSH;
  while (1)
    {
      if (!z.avail_in)
        {
          z.next_in = ibuf;
          z.avail_in = read_bytes (is, ibuf, sizeof ibuf);
          if (z.avail_in < sizeof ibuf)
            flush = Z_FINISH;
        }

      int r = deflate (&z, flush);
      if (r == Z_STREAM_END)
        break;
      if (r != Z_OK)
        FEstorage_error ();

      if (!z.avail_out)
        {
          write_bytes (os, obuf, sizeof obuf);
          z.next_out = obuf;
          z.avail_out = sizeof obuf;
        }
    }

  write_bytes (os, obuf, sizeof obuf - z.avail_out);
  write crc;
  write size;
  os->flush (1);
  return os.result ();
}
#endif
