#include "stdafx.h"
#include "ed.h"
#include "byte-stream.h"
#include "md5.h"
#include "sha1.h"
#include "sha2.h"

static void
copy_xstream (xfilter_stream <u_char, u_char> &is, byte_output_stream &os)
{
  int c;
  while ((c = is.get ()) != xstream::eof)
    os.put (c);
  os.flush (1);
}

lisp
Fsi_base64_decode (lisp input, lisp output)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xdecode_b64_stream s (i);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_base64_encode (lisp input, lisp output, lisp lwidth)
{
  int width;
  if (!lwidth || lwidth == Qnil)
    width = 72;
  else if (lwidth == Qt)
    width = -1;
  else
    width = fixnum_value (lwidth);
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xencode_b64_stream s (i, width);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_uudecode (lisp input, lisp output)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xdecode_uu_stream s (i);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_uuencode (lisp input, lisp output)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xencode_uu_stream s (i);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_quoted_printable_decode (lisp input, lisp output, lisp underscore_to_space)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xdecode_qp_stream s (i, underscore_to_space && underscore_to_space != Qnil);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_quoted_printable_encode (lisp input, lisp output, lisp space_to_underscore)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xencode_qp_stream s (i, space_to_underscore && space_to_underscore != Qnil);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_www_url_encode (lisp input, lisp output, lisp literal_chars)
{
  char lc[256];
  bzero (lc, sizeof lc);
  if (!literal_chars || literal_chars == Qnil)
    {
      for (int i = '0'; i <= '9'; i++)
        lc[i] = 1;
      for (int i = 'A'; i <= 'Z'; i++)
        lc[i] = lc[i + ('a' - 'A')] = 1;
      for (const char *p = "$-_.+!*'(|),"; *p; p++)
        lc[*p] = 1;
    }
  else if (literal_chars != Qt)
    {
      check_string (literal_chars);
      const Char *p = xstring_contents (literal_chars);
      const Char *const pe = p + xstring_length (literal_chars);
      while (p < pe)
        {
          Char c = *p++;
          if (p < pe - 1 && *p == '-')
            {
              Char c2 = p[1];
              p += 2;
              if (c < sizeof lc)
                {
                  c2 = min (c2, Char (sizeof lc));
                  for (; c <= c2; c++)
                    lc[c] = 1;
                }
            }
          else if (c < sizeof lc)
            lc[c] = 1;
        }
    }

  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xencode_url_stream s (i, lc);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_www_url_decode (lisp input, lisp output)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xdecode_url_stream s (i);
  copy_xstream (s, o);
  return o.result ();
}

lisp
Fsi_binhex_decode (lisp input, lisp output)
{
  xstream_ibyte_helper i (input);
  xstream_obyte_helper o (output);
  xdecode_hqx_stream s (i);
  copy_xstream (s, o);
  lisp r = o.result ();
  if (!s.corrupted_p ())
    {
      multiple_value::value (1) = make_integer (int64_t (s.type ()));
      multiple_value::value (2) = make_integer (int64_t (s.creator ()));
      multiple_value::value (3) = make_string (s.name ());
      multiple_value::count () = 4;
    }
  return r;
}

class hash_method
{
protected:
  const int h_block_size;
  const int h_digest_size;

  hash_method (int block_size, int digest_size)
       : h_block_size (block_size), h_digest_size (digest_size) {}
public:
  virtual void init () = 0;
  virtual void update (const u_char *, size_t) = 0;
  virtual void final (u_char *) = 0;
  int block_size () const {return h_block_size;}
  int digest_size () const {return h_digest_size;}
  void update (lisp);
  lisp make_string (const u_char *, int) const;
  lisp apply (lisp, lisp);
  lisp hmac (lisp, lisp, lisp);
};

class md5: public hash_method
{
protected:
  MD5_CTX m_ctx;
public:
  md5 () : hash_method (64, 16) {}
  virtual void init () {MD5Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {MD5Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {MD5Final (digest, &m_ctx);}
};

class sha1: public hash_method
{
protected:
  SHA1_CTX m_ctx;
public:
  sha1 () : hash_method (64, 20) {}
  virtual void init () {SHA1Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {SHA1Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {SHA1Final (digest, &m_ctx);}
};

class sha224: public hash_method
{
protected:
  SHA224_CTX m_ctx;
public:
  sha224 () : hash_method (512 / 8, 224 / 8) {}
  virtual void init () {SHA224_Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {SHA224_Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {SHA224_Final (digest, &m_ctx);}
};

class sha256: public hash_method
{
protected:
  SHA256_CTX m_ctx;
public:
  sha256 () : hash_method (512 / 8, 256 / 8) {}
  virtual void init () {SHA256_Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {SHA256_Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {SHA256_Final (digest, &m_ctx);}
};

class sha384: public hash_method
{
protected:
  SHA384_CTX m_ctx;
public:
  sha384 () : hash_method (1024 / 8, 384 / 8) {}
  virtual void init () {SHA384_Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {SHA384_Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {SHA384_Final (digest, &m_ctx);}
};

class sha512: public hash_method
{
protected:
  SHA512_CTX m_ctx;
public:
  sha512 () : hash_method (1024 / 8, 512 / 8) {}
  virtual void init () {SHA512_Init (&m_ctx);}
  virtual void update (const u_char *data, size_t size)
    {SHA512_Update (&m_ctx, data, size);}
  virtual void final (u_char *digest)
    {SHA512_Final (digest, &m_ctx);}
};

void
hash_method::update (lisp input)
{
  int c;
  u_char buf[1024], *b = buf, *const be = b + sizeof buf;
  xstream_ibyte_helper is (input);
  while ((c = is->get ()) != xstream::eof)
    {
      *b++ = c;
      if (b == be)
        {
          update (buf, sizeof buf);
          b = buf;
        }
    }
  if (b != buf)
    update (buf, b - buf);
}

lisp
hash_method::make_string (const u_char *digest, int binary) const
{
  if (binary)
    return ::make_string_simple (reinterpret_cast <const char *> (digest), digest_size ());

  char *buf = (char *)alloca (digest_size () * 2), *b = buf;
  for (const u_char *d = digest, *const de = d + digest_size (); d < de; d++)
    {
      *b++ = downcase_digit_char[*d >> 4];
      *b++ = downcase_digit_char[*d & 15];
    }
  return ::make_string (buf, digest_size () * 2);
}

lisp
hash_method::apply (lisp input, lisp keys)
{
  init ();
  update (input);
  u_char *digest = (u_char *)alloca (digest_size ());
  final (digest);
  return make_string (digest, find_keyword_bool (Kbinary, keys));
}

lisp
hash_method::hmac (lisp lkey, lisp input, lisp keys)
{
  char *key;
  check_string (lkey);
  int key_len = w2sl (lkey);
  if (key_len <= block_size ())
    {
      key = (char *)alloca (key_len + 1);
      w2s (key, xstring_contents (lkey), xstring_length (lkey));
    }
  else
    {
      init ();
      update (lkey);
      key = (char *)alloca (digest_size ());
      final ((u_char *)key);
      key_len = digest_size ();
    }

  u_char *const ipad = (u_char *)alloca (block_size () * 2);
  u_char *const opad = ipad + block_size ();
  memset (ipad, 0, block_size ());
  memcpy (ipad, key, key_len);
  for (int i = 0; i < block_size (); i++)
    {
      opad[i] = ipad[i] ^ 0x5c;
      ipad[i] ^= 0x36;
    }

  u_char *digest = (u_char *)alloca (digest_size ());

  init ();
  update (ipad, block_size ());
  update (input);
  final (digest);

  init ();
  update (opad, block_size ());
  update (digest, digest_size ());
  final (digest);

  return make_string (digest, find_keyword_bool (Kbinary, keys));
}

lisp
Fsi_md5 (lisp input, lisp keys)
{
  md5 x;
  return x.apply (input, keys);
}

lisp
Fsi_sha_1 (lisp input, lisp keys)
{
  sha1 x;
  return x.apply (input, keys);
}

lisp
Fsi_sha_224 (lisp input, lisp keys)
{
  sha224 x;
  return x.apply (input, keys);
}

lisp
Fsi_sha_256 (lisp input, lisp keys)
{
  sha256 x;
  return x.apply (input, keys);
}

lisp
Fsi_sha_384 (lisp input, lisp keys)
{
  sha384 x;
  return x.apply (input, keys);
}

lisp
Fsi_sha_512 (lisp input, lisp keys)
{
  sha512 x;
  return x.apply (input, keys);
}

lisp
Fsi_hmac_md5 (lisp key, lisp input, lisp keys)
{
  md5 x;
  return x.hmac (key, input, keys);
}

lisp
Fsi_hmac_sha_1 (lisp key, lisp input, lisp keys)
{
  sha1 x;
  return x.hmac (key, input, keys);
}

lisp
Fsi_hmac_sha_224 (lisp key, lisp input, lisp keys)
{
  sha224 x;
  return x.hmac (key, input, keys);
}

lisp
Fsi_hmac_sha_256 (lisp key, lisp input, lisp keys)
{
  sha256 x;
  return x.hmac (key, input, keys);
}

lisp
Fsi_hmac_sha_384 (lisp key, lisp input, lisp keys)
{
  sha384 x;
  return x.hmac (key, input, keys);
}

lisp
Fsi_hmac_sha_512 (lisp key, lisp input, lisp keys)
{
  sha512 x;
  return x.hmac (key, input, keys);
}
