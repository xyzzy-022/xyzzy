/**************************************************************************
   THIS CODE AND INFORMATION IS PROVIDED 'AS IS' WITHOUT WARRANTY OF
   ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
   PARTICULAR PURPOSE.
   Author: Leon Finker  7/2002
**************************************************************************/

#ifndef _sockssl_h_
#define _sockssl_h_

#include <wincrypt.h>
#include <schannel.h>
#define SECURITY_WIN32
#include <security.h>
#include <sspi.h>

#include "sockinet.h"

template<int n>
class safe_secbuf
{
private:
  SecBufferDesc sb_desc;
  SecBuffer sb_buf[n];
  bool sb_release;

  void set (int n, ULONG type, void *buf, int len)
    {
      sb_buf[n].BufferType = type;
      sb_buf[n].pvBuffer = buf;
      sb_buf[n].cbBuffer = len;
    }

public:
  safe_secbuf (bool release) : sb_release (release)
    {
      sb_desc.cBuffers = n;
      sb_desc.pBuffers = sb_buf;
      sb_desc.ulVersion = SECBUFFER_VERSION;
    }

  ~safe_secbuf ()
    {
      if (!sb_release) return;
      for (int i = 0; i < n; i++)
        {
          if (sb_buf[i].pvBuffer == nullptr) continue;
          FreeContextBuffer (sb_buf[i].pvBuffer);
        }
    }

  SecBuffer &operator [] (unsigned int i) {return sb_buf[i];}
  SecBufferDesc *desc () {return &sb_desc;}

  void free (int n)
    {
      if (sb_buf[n].pvBuffer == nullptr) return;
      FreeContextBuffer (sb_buf[n].pvBuffer);
      sb_buf[n].pvBuffer = nullptr;
    }

  void set_token (int n, void *buf, int len)
    { set (n, SECBUFFER_TOKEN, buf, len); }
  void set_data (int n, void *buf, int len)
    { set (n, SECBUFFER_DATA, buf, len); }
  void set_stream_header (int n, void *buf, int len)
    { set (n, SECBUFFER_STREAM_HEADER, buf, len); }
  void set_stream_trailer (int n, void *buf, int len)
    { set (n, SECBUFFER_STREAM_TRAILER, buf, len); }
  void set_empty (int n)
    { set (n, SECBUFFER_EMPTY, nullptr, 0); }
};

class sockssl: public sockinet
{
public:
  enum ssl_verify_mode
    {
      verify_none,
      verify_peer,
    };

  sockssl (lisp lserver_name, lisp lverify_mode)
       : ss_verify_mode (check_ssl_verify_mode (lverify_mode)),
         ss_server_name (check_server_name (lserver_name))
    {}
  sockssl (SOCKET so, lisp lserver_name, lisp lverify_mode)
       : sockinet (so),
         ss_verify_mode (check_ssl_verify_mode (lverify_mode)),
         ss_server_name (check_server_name (lserver_name))
    {}
  sockssl (sockinet *so, lisp lserver_name, lisp lverify_mode);

  virtual ~sockssl ()
    {xfree ((void *)ss_server_name);}

  virtual void connect (const sock::saddr &addr) const;
  virtual void send (const void *buf, int len, int flags = 0) const;
  virtual int recv (void *buf, int len, int flags = 0) const;
  virtual void close (int abort);

  virtual void handshake () const;
  virtual bool sslp () const {return true;}

protected:
  class sslbuf
    {
    private:
      //  base     ptr
      // «       «
      // „¡„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„¢
      // „¤„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„£
      //          „¤„Ÿ  len  „Ÿ„£
      char *sb_base;
      char *sb_ptr;
      int sb_len;

    public:
      char *base () const {return sb_base;}
      char *ptr () const {return sb_ptr;}
      int len () const {return sb_len;}
      bool has_data_p () const {return sb_len > 0;}

      sslbuf () : sb_base (nullptr), sb_ptr (nullptr), sb_len (0) {}
      ~sslbuf () {free ();}

      void consume (int len)
        {
          assert (len <= sb_len);
          sb_ptr += len;
          sb_len -= len;
        }

      void set (char *base, int len)
        {
          sb_base = sb_ptr = base;
          sb_len = len;
        }

      void free ()
        {
          xfree (sb_base);
          sb_base = nullptr;
          sb_ptr = nullptr;
          sb_len = 0;
        }
    };

  const ssl_verify_mode ss_verify_mode;
  const char *ss_server_name;
  bool ss_handshake_p;
  bool ss_connected_p;

  sslbuf ss_extra_buf; // encrypted data
  sslbuf ss_recv_buf;  // plain data

  SCHANNEL_CRED *ss_schannel_cred;
  CredHandle *ss_client_creds;
  CtxtHandle *ss_context;

  void init ();
  void dispose ();
  void setup_credentials (DWORD protcols) const;
  void perform_handshake ();
  bool handshake_loop (void *buf, int &len, SecBuffer *extra_data);
  void verify_certificate (const char *server_name, DWORD cert_flags);
  int recv_decrypt (void *buf, int len, int flags);
  void decrypt_data (const char *data, int len);
  void encrypt_send (const void *buf, int len, int flags = 0) const;
  void raw_send (SecBuffer &buf) const;
  void disconnect ();
  int max_chunk_size (SecPkgContext_StreamSizes &sizes) const;
  int max_data_chunk_size () const;
  int max_initial_chunk_size () const;
  ssl_verify_mode check_ssl_verify_mode (lisp lverify_mode) const;
  const char *check_server_name (lisp lserver_name) const;
};

#endif /* _sockssl_h_ */
