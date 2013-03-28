/**************************************************************************
   THIS CODE AND INFORMATION IS PROVIDED 'AS IS' WITHOUT WARRANTY OF
   ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
   PARTICULAR PURPOSE.
   Author: Leon Finker  7/2002
**************************************************************************/

#include "stdafx.h"
#ifdef __XYZZY__
#include "ed.h"
#endif
#include "safe_ptr.h"
#include "sockinet.h"
#include "sockimpl.h"
#include "sockssl.h"

sockssl::sockssl (sockinet *so, lisp lserver_name, lisp lverify_mode)
     : sockinet (so->socket ()),
       ss_verify_mode (check_ssl_verify_mode (lverify_mode)),
       ss_server_name (check_server_name (lserver_name))
{
  // copy sockinet properties
  s_eof_error_p = so->eof_error_p ();
  send_timeout (so->send_timeout ());
  recv_timeout (so->recv_timeout ());

  // cleanup sockinet
  so->socket () = INVALID_SOCKET;
  delete so;

  // s_rbuf, s_wbuf とかどうしよう・・・
}

void
sockssl::connect (const sock::saddr &addr) const
{
  sock::connect (addr);
  handshake ();
}

void
sockssl::send (const void *buf, int len, int flags) const
{
  encrypt_send (buf, len, flags);
}

int
sockssl::recv (void *buf, int len, int flags) const
{
  sockssl *self = const_cast <sockssl *> (this);
  return self->recv_decrypt (buf, len, flags);
}

void
sockssl::close (int abort)
{
  if (s_so == INVALID_SOCKET) return;

  try
    {
      disconnect ();
    }
  catch (nonlocal_jump &)
    { }

  sock::close (abort);

  if (ss_context)
    dispose ();
}

void
sockssl::handshake () const
{
  sockssl *self = const_cast <sockssl *> (this);
  self->init ();
  self->setup_credentials (SP_PROT_SSL3TLS1_X_CLIENTS);
  self->perform_handshake ();
}


void
sockssl::init ()
{
  ss_extra_buf.set (nullptr, 0);
  ss_handshake_p = false;
  ss_connected_p = false;

  ss_context = new CtxtHandle;
  SecInvalidateHandle (ss_context);

  ss_client_creds = new CredHandle;
  SecInvalidateHandle (ss_client_creds);

  ss_schannel_cred = new SCHANNEL_CRED;
  memset (ss_schannel_cred, 0, sizeof (SCHANNEL_CRED));
}

void
sockssl::dispose ()
{
  ss_handshake_p = false;
  ss_connected_p = false;

  if (ss_extra_buf.has_data_p ())
    {
      ss_extra_buf.free ();
      ss_extra_buf.set (nullptr, 0);
    }

  if (ss_context)
    {
      if (SecIsValidHandle (ss_context))
        {
          DeleteSecurityContext (ss_context);
          SecInvalidateHandle (ss_context);
        }
      delete ss_context;
      ss_context = nullptr;
    }

  if (ss_client_creds)
    {
      if (SecIsValidHandle (ss_client_creds))
        {
          FreeCredentialsHandle (ss_client_creds);
          SecInvalidateHandle (ss_client_creds);
        }
      delete ss_client_creds;
      ss_client_creds = nullptr;
    }

  if (ss_schannel_cred)
    {
      delete ss_schannel_cred;
      ss_schannel_cred = nullptr;
    }
}

void
sockssl::setup_credentials (DWORD protcols) const
{
  ss_schannel_cred->dwVersion = SCHANNEL_CRED_VERSION;
  ss_schannel_cred->grbitEnabledProtocols = protcols;
  ss_schannel_cred->dwFlags |=
    SCH_CRED_MANUAL_CRED_VALIDATION
    | SCH_CRED_IGNORE_NO_REVOCATION_CHECK
    | SCH_CRED_IGNORE_REVOCATION_OFFLINE;

  TimeStamp expiry;
  SECURITY_STATUS status = AcquireCredentialsHandle (
    nullptr,
    UNISP_NAME_A,
    SECPKG_CRED_OUTBOUND,
    nullptr,
    ss_schannel_cred,
    nullptr,
    nullptr,
    ss_client_creds,
    &expiry);

  if (status != SEC_E_OK)
    throw sock_error ("AcquireCredentialsHandle", status);
}

void
sockssl::perform_handshake ()
{
  DWORD flags;
  DWORD out_flags;
  TimeStamp expiry;

  flags = ISC_REQ_SEQUENCE_DETECT | ISC_REQ_REPLAY_DETECT
    | ISC_REQ_CONFIDENTIALITY | ISC_RET_EXTENDED_ERROR
    | ISC_REQ_ALLOCATE_MEMORY | ISC_REQ_STREAM;

  //
  // Initiate a ClientHello message and generate a token.
  //
  safe_secbuf <1> out_buf (true);
  out_buf.set_token (0, nullptr, 0);

  SECURITY_STATUS status = InitializeSecurityContext (
    ss_client_creds,
    nullptr,
    const_cast <SEC_CHAR *> (ss_server_name),
    flags,
    0,
    SECURITY_NATIVE_DREP,
    nullptr,
    0,
    ss_context,
    out_buf.desc (),
    &out_flags,
    &expiry);

  if (status == SEC_I_INCOMPLETE_CREDENTIALS)
    ; // TODO: LoadNewClientCredentials
  else if (status != SEC_I_CONTINUE_NEEDED)
    throw sock_error ("InitializeSecurityContext (handshake)", status);

  ss_handshake_p = true;

  // Send ClientHello to server if there is one.
  if (out_buf[0].cbBuffer != 0 && out_buf[0].pvBuffer)
    raw_send (out_buf[0]);

  // Receive ServerHello from server.
  int chunk_size = max_initial_chunk_size ();
  safe_ptr <char> chunk = new char[chunk_size];
  while (!ss_connected_p)
    {
      int len = sock::recv (chunk, chunk_size);
      if (len <= 0)
        break;

      decrypt_data (chunk, len);
      ss_recv_buf.free ();
    }
}

bool
sockssl::handshake_loop (void *buf, int &len, SecBuffer *extra_buf)
{
  safe_secbuf <2> in_buf (false);
  safe_secbuf <1> out_buf (true);

  DWORD flags = ISC_REQ_SEQUENCE_DETECT | ISC_REQ_REPLAY_DETECT
    | ISC_REQ_CONFIDENTIALITY | ISC_RET_EXTENDED_ERROR
    | ISC_REQ_ALLOCATE_MEMORY | ISC_REQ_STREAM;
  DWORD out_flags;
  TimeStamp expiry;

  SECURITY_STATUS status = SEC_I_CONTINUE_NEEDED;
  while (status == SEC_I_CONTINUE_NEEDED || status == SEC_I_INCOMPLETE_CREDENTIALS)
    {
      //
      // Set up the input buffers. Buffer 0 is used to pass in data
      // received from the server. Schannel will consume some or all
      // of this. Leftover data (if any) will be placed in buffer 1 and
      // given a buffer type of SECBUFFER_EXTRA.
      //
      in_buf.set_token (0, buf, len);
      in_buf.set_empty (1);
      out_buf.set_token (0, nullptr, 0);
      status = InitializeSecurityContext (
        ss_client_creds,
        ss_context,
        nullptr,
        flags,
        0,
        SECURITY_NATIVE_DREP,
        in_buf.desc (),
        0,
        nullptr,
        out_buf.desc (),
        &out_flags,
        &expiry);

      //
      // If InitializeSecurityContext was successful (or if the error was
      // one of the special extended ones), send the contents of the output
      // buffer to the server.
      //
      if (status == SEC_E_OK || status == SEC_I_CONTINUE_NEEDED
          || (FAILED (status) && (out_flags & ISC_RET_EXTENDED_ERROR)))
        {
          if (out_buf[0].cbBuffer != 0 && out_buf[0].pvBuffer)
            raw_send (out_buf[0]);
          out_buf.free (0);
        }

      //
      // If InitializeSecurityContext returned SEC_E_INCOMPLETE_MESSAGE,
      // then we need to read more data from the server and try again.
      //
      if (status == SEC_E_INCOMPLETE_MESSAGE)
        {
          // tell caller to save the buffer for later input
          return false;
        }

      //
      // If InitializeSecurityContext returned SEC_E_OK, then the
      // handshake completed successfully.
      //
      if (status == SEC_E_OK)
        {
          //
          // If the "extra" buffer contains data, this is encrypted application
          // protocol layer stuff. It needs to be saved. The application layer
          // will later decrypt it with DecryptMessage.
          //
          if (in_buf[1].BufferType == SECBUFFER_EXTRA)
            {
              extra_buf->pvBuffer = xmalloc (in_buf[1].cbBuffer);
              MoveMemory (
                extra_buf->pvBuffer,
                reinterpret_cast <char *> (buf) + (len - in_buf[1].cbBuffer),
                in_buf[1].cbBuffer);

              extra_buf->cbBuffer = in_buf[1].cbBuffer;
              extra_buf->BufferType = SECBUFFER_TOKEN;
            }
          else
            {
              extra_buf->pvBuffer = nullptr;
              extra_buf->cbBuffer = 0;
              extra_buf->BufferType = SECBUFFER_EMPTY;
            }

          //
          // Bail out to quit
          //
          ss_handshake_p = false;
          if (ss_verify_mode == verify_peer)
            verify_certificate (ss_server_name, 0);
          ss_connected_p = true;
          break;
        }

      //
      // Check for fatal error.
      //
      if (FAILED (status))
        throw sock_error ("InitializeSecurityContext (handshake loop)", status);

      //
      // If InitializeSecurityContext returned SEC_I_INCOMPLETE_CREDENTIALS,
      // then the server just requested client authentication.
      //
      if (status == SEC_I_INCOMPLETE_CREDENTIALS)
        {
          // TODO: LoadNewClientCredentials
          continue;
        }

      //
      // Copy any leftover data from the "extra" buffer, and go around again.
      //
      if (in_buf[1].BufferType == SECBUFFER_EXTRA)
        {
          MoveMemory (
            buf,
            reinterpret_cast <char *> (buf) + (len - in_buf[1].cbBuffer),
            in_buf[1].cbBuffer);
          len = in_buf[1].cbBuffer;
        }
      else
        {
          len = 0;
          break;
        }
    }

  return true;
}

void
sockssl::verify_certificate (const char *server_name, DWORD cert_flags)
{
  HTTPSPolicyCallbackData policy_https;
  CERT_CHAIN_POLICY_PARA policy_para;
  CERT_CHAIN_POLICY_STATUS policy_status;
  CERT_CHAIN_PARA chain_para;
  PCCERT_CHAIN_CONTEXT chain_context = nullptr;
  PCCERT_CONTEXT cert_context = nullptr;

  SECURITY_STATUS status = QueryContextAttributes (
    ss_context, SECPKG_ATTR_REMOTE_CERT_CONTEXT, (void *) &cert_context);
  if (status != SEC_E_OK || !cert_context)
    FEprogram_error (Eget_remote_cert_context_failed);

  //
  // Build certificate chain.
  //
  memset (&chain_para, 0, sizeof (chain_para));
  chain_para.cbSize = sizeof (chain_para);
  chain_para.RequestedUsage.dwType = USAGE_MATCH_TYPE_OR;

  LPSTR server_usages[] = {
    szOID_PKIX_KP_SERVER_AUTH,
    szOID_SERVER_GATED_CRYPTO,
    szOID_SGC_NETSCAPE,
  };

  chain_para.RequestedUsage.Usage.cUsageIdentifier = 3;
  chain_para.RequestedUsage.Usage.rgpszUsageIdentifier = server_usages;

  if (!CertGetCertificateChain (
    nullptr,
    cert_context,
    nullptr,
    cert_context->hCertStore,
    &chain_para,
    0,
    nullptr,
    &chain_context))
    {
      if (chain_context)
        CertFreeCertificateChain (chain_context);
      FEprogram_error (Eget_certificate_chain_failed);
    }

  //
  // Validate certificate chain.
  //
  memset (&policy_https, 0, sizeof (HTTPSPolicyCallbackData));
  policy_https.cbStruct = sizeof (HTTPSPolicyCallbackData);
  policy_https.dwAuthType = AUTHTYPE_SERVER;
  policy_https.fdwChecks = cert_flags;

  size_t len = strlen (server_name) + 1;
  Char *w = (Char *)alloca (len * sizeof (Char));
  a2w (w, server_name, len);
  policy_https.pwszServerName = w;

  memset (&policy_para, 0, sizeof (policy_para));
  policy_para.cbSize = sizeof (policy_para);
  policy_para.pvExtraPolicyPara = &policy_https;

  memset (&policy_status, 0, sizeof (policy_status));
  policy_status.cbSize = sizeof (policy_status);


  BOOL verify_ok = CertVerifyCertificateChainPolicy (
    CERT_CHAIN_POLICY_SSL, chain_context, &policy_para, &policy_status);

  if (chain_context)
    CertFreeCertificateChain (chain_context);

  if (!verify_ok)
    FEprogram_error (Everify_certificate_chain_failed);

  if (policy_status.dwError)
    throw sock_error ("CertVerifyCertificateChainPolicy", policy_status.dwError);
}

int
sockssl::recv_decrypt (void *buf, int len, int flags)
{
  int chunk_size = max_data_chunk_size ();
  safe_ptr <char> chunk = new char [chunk_size];
  int nread = 0;

  while (true)
    {
      if (ss_recv_buf.has_data_p ())
        {
          int n = min (len - nread, ss_recv_buf.len ());
          MoveMemory (reinterpret_cast <char *> (buf) + nread, ss_recv_buf.ptr (), n);
          nread += n;
          ss_recv_buf.consume (n);
          if (!ss_recv_buf.has_data_p ())
            ss_recv_buf.free ();
          return nread;
        }

      assert (!ss_recv_buf.has_data_p ());
      int len = sock::recv (chunk, chunk_size, flags);
      if (len <= 0)
        return nread;

      decrypt_data (chunk, len);
    }

  return nread;
}

void
sockssl::decrypt_data (const char *data, int datalen)
{
  // TODO:
  //   * buf と extra_buf と ss_extra_buf と 3 つのバッファがあるのが無駄
  //   * ss_extra_buf と buf が同じ場所を指していてややこしいし、2 回 free
  //     するかもしれないので危ない
  //   * decrypt_data から handshake_loop を呼び出しているのがダサい
  //   * 例外が発生した場合に buf がメモリリークするかもしれない
  //   * 復号化したデータを保存するバッファを毎回 realloc しているのが無駄
  //   * もっと賢いバッファ管理が必要
  //   * 暗号化されたデータを保存するバッファと復号化したデータを保存するバッファの
  //     2 つをメンバ変数として持ってそれだけを使うようにしたい
  SecBuffer extra_buf = {0};

  // add previous leftover buffer
  int buflen = datalen + ss_extra_buf.len ();
  char *buf = (char *)xmalloc (buflen);

  // copy data, at position after extra data if any
  MoveMemory (buf + ss_extra_buf.len (), data, datalen);

  // copy from previous leftover data to beginning/before new one
  if (ss_extra_buf.has_data_p ())
    {
      MoveMemory (buf, ss_extra_buf.ptr (), ss_extra_buf.len ());
      ss_extra_buf.free ();
    }

  if (ss_handshake_p)
    {
      if (!handshake_loop (buf, buflen, &extra_buf))
        {
          // The input buffer contains only a fragment of an
          // encrypted record. Save the fragment and wait for more data.
          ss_extra_buf.set (buf, buflen);
          return;
        }

      if (extra_buf.cbBuffer == 0)
        {
          xfree (buf);
          return;
        }

      if (extra_buf.pvBuffer)
        {
          // save extra data and send it to DecryptMessage
          MoveMemory (buf, extra_buf.pvBuffer, extra_buf.cbBuffer);
          buflen = extra_buf.cbBuffer;
          xfree (extra_buf.pvBuffer);
          extra_buf.pvBuffer = nullptr;
          extra_buf.cbBuffer = 0;
        }
    }

  while (true)
    {
      safe_secbuf <4> buffers (false);
      buffers.set_data (0, buf, buflen);
      buffers.set_empty (1);
      buffers.set_empty (2);
      buffers.set_empty (3);

      SECURITY_STATUS status =
        DecryptMessage (ss_context, buffers.desc (), 0, nullptr);

      if (status == SEC_E_INCOMPLETE_MESSAGE)
        {
          // The input buffer contains only a fragment of an
          // encrypted data. Save the fragment and wait for more data.
          ss_extra_buf.set (buf, buflen);
          // buf is freed on next entry
          return;
        }
      if (status != SEC_E_OK &&
          status != SEC_I_RENEGOTIATE &&
          status != SEC_I_CONTEXT_EXPIRED)
        {
          xfree (buf);
          throw sock_error ("DecryptMessage", status);
        }

      // Server signalled end of session
      if (status == SEC_I_CONTEXT_EXPIRED)
        {
          // pass in empty buffers and send output to remote as per specs
          encrypt_send ("", 0);
          dispose ();
          xfree (buf);
          throw sock_error ("DecryptMessage (context expired)", status);
        }

      // Locate data and (optional) extra buffers.
      SecBuffer *p_data_buf = nullptr;
      SecBuffer *p_extra_buf = nullptr;
      for (int i = 1; i < 4; i++)
        {
          if (!p_data_buf && buffers[i].BufferType == SECBUFFER_DATA)
            p_data_buf = &buffers[i];
          if (!p_extra_buf && buffers[i].BufferType == SECBUFFER_EXTRA)
            p_extra_buf = &buffers[i];
        }

      // Display or otherwise process the decrypted data.
      if (p_data_buf && p_data_buf->cbBuffer > 0)
        {
          char *b = (char *)xrealloc (ss_recv_buf.base (), ss_recv_buf.len () + p_data_buf->cbBuffer);
          MoveMemory (b + ss_recv_buf.len (), p_data_buf->pvBuffer, p_data_buf->cbBuffer);
          ss_recv_buf.set (b, ss_recv_buf.len () + p_data_buf->cbBuffer);
        }

      // Move any "extra" data to the input buffer, update buflen and go around again
      bool has_extra_data = false;
      if (p_extra_buf && p_extra_buf->cbBuffer > 0)
        {
          MoveMemory (buf, p_extra_buf->pvBuffer, p_extra_buf->cbBuffer);
          buflen = p_extra_buf->cbBuffer;
          has_extra_data = true;
        }

      if (!has_extra_data && status == S_OK)
        break;

      if (status == SEC_I_RENEGOTIATE)
        {
          // The server wants to perform another handshake sequence.
          // TODO: LoadNewClientCredentials
          ss_handshake_p = true;

          int dummy = 0;
          if (p_extra_buf)
            handshake_loop (buf, buflen, &extra_buf);
          else
            handshake_loop (nullptr, dummy, &extra_buf);

          if (!extra_buf.pvBuffer)
            break;

          // Move any "extra" data to the input buffer.
          MoveMemory (buf, extra_buf.pvBuffer, extra_buf.cbBuffer);
          buflen = extra_buf.cbBuffer;
          xfree (extra_buf.pvBuffer);
          extra_buf.pvBuffer = nullptr;
          extra_buf.cbBuffer = 0;
        }
    }
  xfree (buf);
}

void
sockssl::encrypt_send (const void *buf, int len, int flags) const
{
  SecPkgContext_StreamSizes sizes;
  int msglen = max_chunk_size (sizes) + sizes.cbHeader + sizes.cbTrailer;

  safe_ptr <char> msg = new char [msglen];
  MoveMemory (msg + sizes.cbHeader, buf, len);

  safe_secbuf <4> buffers (false);
  buffers.set_stream_header (0, msg, sizes.cbHeader);
  buffers.set_data (1, msg + sizes.cbHeader, len);
  buffers.set_stream_trailer (2, msg + sizes.cbHeader + len, sizes.cbTrailer);
  buffers.set_empty (3);

  SECURITY_STATUS status = EncryptMessage (ss_context, 0, buffers.desc (), 0);
  if (FAILED (status) && status != SEC_E_CONTEXT_EXPIRED)
    throw sock_error ("EncryptMessage", status);

  int out_msglen = buffers[0].cbBuffer + buffers[1].cbBuffer + buffers[2].cbBuffer;

  sock::send (msg, out_msglen, flags);
}

void
sockssl::raw_send (SecBuffer &buf) const
{
  sock::send (buf.pvBuffer, buf.cbBuffer, 0);
}

void
sockssl::disconnect ()
{
  //
  // Notify schannel that we are about to close the connection.
  //
  DWORD type = SCHANNEL_SHUTDOWN;
  safe_secbuf <1> out_buf (false);
  out_buf.set_token (0, &type, sizeof (type));

  SECURITY_STATUS status = ApplyControlToken (ss_context, out_buf.desc ());
  if (FAILED (status))
    throw sock_error ("Disconnect", status);

  //
  // Build an SSL close notify message.
  //
  DWORD flags = ISC_REQ_SEQUENCE_DETECT
    | ISC_REQ_REPLAY_DETECT
    | ISC_REQ_CONFIDENTIALITY
    | ISC_RET_EXTENDED_ERROR
    | ISC_REQ_ALLOCATE_MEMORY
    | ISC_REQ_STREAM;

  out_buf.set_token (0, nullptr, 0);

  TimeStamp expiry;
  DWORD out_flags;
  status = InitializeSecurityContext (
    ss_client_creds,
    ss_context,
    nullptr,
    flags,
    0,
    SECURITY_NATIVE_DREP,
    nullptr,
    0,
    ss_context,
    out_buf.desc (),
    &out_flags,
    &expiry);

  if (FAILED (status))
    throw sock_error ("InitializeSecurityContext (shutting down)", status);

  // Send the close notify message to the server.
  raw_send (out_buf[0]);

  // Free output buffer.
  FreeContextBuffer (out_buf[0].pvBuffer);
}

int
sockssl::max_chunk_size (SecPkgContext_StreamSizes &sizes) const
{
  sizes.cbMaximumMessage = 0;
  SECURITY_STATUS status = QueryContextAttributesA (
    ss_context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (status != SEC_E_OK)
    throw sock_error ("QueryContextAttributes", status);

  return sizes.cbMaximumMessage;
}

int
sockssl::max_data_chunk_size () const
{
  SecPkgContext_StreamSizes notused;
  return max_chunk_size (notused);
}

//recommended initial chunk size when negotiation just starts, this is max auth token size
int
sockssl::max_initial_chunk_size () const
{
  PSecPkgInfo info;
  SECURITY_STATUS status = QuerySecurityPackageInfo (UNISP_NAME, &info);
  if (status != SEC_E_OK)
    throw sock_error ("QuerySecurityPackageInfo", status);

  return info->cbMaxToken;
}

sockssl::ssl_verify_mode
sockssl::check_ssl_verify_mode (lisp lverify_mode) const
{
  if (lverify_mode == Qnil || lverify_mode == Kpeer)
    return verify_peer;
  if (lverify_mode == Knone)
    return verify_none;

  FEprogram_error (Eunknown_verify_mode, lverify_mode);
  return verify_none;
}

const char *
sockssl::check_server_name (lisp lserver_name) const
{
  check_string (lserver_name);

  char *b = (char *)xmalloc (xstring_length (lserver_name) * 2 + 1);
  w2s (b, lserver_name);

  return b;
}
