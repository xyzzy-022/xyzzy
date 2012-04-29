#include "stdafx.h"
#ifdef __XYZZY__
#include "ed.h"
#undef CONCAT
#undef _CONCAT
#undef __CONCAT
#undef TOSTR
#undef _TOSTR
#endif
#include "sock.h"
#include "resolver.h"
#include "sockimpl.h"

resolver sock::s_resolver;

#define WSOCKDEF(TYPE, NAME, ARGS, RESULT) TYPE (WINAPI *WINSOCK::NAME) ARGS;
  WINSOCK_FUNCTIONS
#undef WSOCKDEF

#define __CONCAT(X, Y) X ## Y
#define _CONCAT(X, Y) __CONCAT (X, Y)
#define CONCAT(X, Y) _CONCAT (X, Y)
#define _TOSTR(X) #X
#define TOSTR(X) _TOSTR (X)

#define WSOCKDEF(TYPE, NAME, ARGS, RESULT) \
  static TYPE WINAPI CONCAT (dummy_, NAME) ARGS {return RESULT;}
  WINSOCK_FUNCTIONS
#undef WSOCKDEF

static FARPROC
get_wsock_fn (HINSTANCE h, const char *name, FARPROC dummy)
{
  if (!h)
    return dummy;
  FARPROC fn = GetProcAddress (h, name);
  return fn ? fn : dummy;
}

static void
init_winsock_functions ()
{
  HINSTANCE h = LoadLibrary ("WSOCK32.DLL");
#define WSOCKDEF(TYPE, NAME, ARGS, RESULT) \
  WINSOCK::NAME = \
    (TYPE (WINAPI *) ARGS)get_wsock_fn (h, TOSTR (NAME), \
                                        FARPROC (CONCAT (dummy_, NAME)));
  WINSOCK_FUNCTIONS
#undef WSOCKDEF
}

#ifdef __XYZZY__
static int WINAPI
blocking_hook ()
{
  Fdo_events ();
  if (QUITP)
    WS_CALL (WSACancelBlockingCall)();
  return 0;
}
#endif

sock_error::sock_error (const char *ope)
     : e_error (WS_CALL (WSAGetLastError)()), e_ope (ope)
{
}

const char *
sock::errmsg (int e)
{
  static const struct {int e; const char *s;} msg[] =
    {
      {WSAEINTR,           "Interrupted system call"},
      {WSAEBADF,           "Bad file descriptor"},
      {WSAEACCES,          "Permission denied"},
      {WSAEFAULT,          "Bad address"},
      {WSAEINVAL,          "Invalid argument"},
      {WSAEMFILE,          "Too many open files"},
      {WSAEWOULDBLOCK,     "Resource temporarily unavailable"},
      {WSAEINPROGRESS,     "Operation now in progress"},
      {WSAEALREADY,        "Operation already in progress"},
      {WSAENOTSOCK,        "Socket operation on non-socket"},
      {WSAEDESTADDRREQ,    "Destination address required"},
      {WSAEMSGSIZE,        "Message too long"},
      {WSAEPROTOTYPE,      "Protocol wrong type for socket"},
      {WSAENOPROTOOPT,     "Protocol not available"},
      {WSAEPROTONOSUPPORT, "Protocol not supported"},
      {WSAESOCKTNOSUPPORT, "Socket type not supported"},
      {WSAEOPNOTSUPP,      "Operation not supported"},
      {WSAEPFNOSUPPORT,    "Protocol family not supported"},
      {WSAEAFNOSUPPORT,    "Address family not supported by protocol family"},
      {WSAEADDRINUSE,      "Address already in use"},
      {WSAEADDRNOTAVAIL,   "Can't assign requested address"},
      {WSAENETDOWN,        "Network is down"},
      {WSAENETUNREACH,     "Network is unreachable"},
      {WSAENETRESET,       "Network dropped connection on reset"},
      {WSAECONNABORTED,    "Software caused connection abort"},
      {WSAECONNRESET,      "Connection reset by peer"},
      {WSAENOBUFS,         "No buffer space available"},
      {WSAEISCONN,         "Socket is already connected"},
      {WSAENOTCONN,        "Socket is not connected"},
      {WSAESHUTDOWN,       "Can't send after socket shutdown"},
      {WSAETOOMANYREFS,    "Too many references: can't splice"},
      {WSAETIMEDOUT,       "Operation timed out"},
      {WSAECONNREFUSED,    "Connection refused"},
      {WSAELOOP,           "Too many levels of symbolic links"},
      {WSAENAMETOOLONG,    "File name too long"},
      {WSAEHOSTDOWN,       "Host is down"},
      {WSAEHOSTUNREACH,    "No route to host"},
      {WSAENOTEMPTY,       "Directory not empty"},
      {WSAEPROCLIM,        "Too many processes"},
      {WSAEUSERS,          "Too many users"},
      {WSAEDQUOT,          "Disc quota exceeded"},
      {WSAESTALE,          "Stale NFS file handle"},
      {WSAEREMOTE,         "Too many levels of remote in path"},
      {WSASYSNOTREADY,     "The network subsystem is unusable"},
      {WSAVERNOTSUPPORTED, "The Windows Sockets DLL cannot support this app"},
      {WSANOTINITIALISED,  "A successful WSAStartup, has not yet been performed"},
      {WSAEDISCON,         "The message terminated gracefully"},
      {WSAHOST_NOT_FOUND,  "Authoritative Answer; Host not found"},
      {WSATRY_AGAIN,       "Non-Authoritative; Host not found, or SERVERFAIL"},
      {WSANO_RECOVERY,     "Non recoverable errors, FORMERR, REFUSED, NOTIMP"},
      {WSANO_DATA,         "Valid name, no data record of requested type"},
    };
  if (e < WSABASEERR || e > WSANO_DATA)
    return 0;
  for (int i = 0; i < numberof (msg); i++)
    if (e == msg[i].e)
      return msg[i].s;
  return 0;
}

int
sock::init_winsock (HINSTANCE hinst)
{
  init_winsock_functions ();

  WSADATA data;
  int e = WS_CALL (WSAStartup)(MAKEWORD (1, 1), &data);
  if (e)
    return 0;

#ifdef __XYZZY__
  WS_CALL (WSASetBlockingHook)(blocking_hook);
#endif

  if (!s_resolver.initialize (hinst)
      || !s_resolver.create (hinst))
    return 0;
  return 1;
}

void
sock::term_winsock ()
{
  WS_CALL (WSACleanup)();
}

void
sock::initsock (SOCKET so)
{
  s_so = so;
  s_rtimeo.tv_sec = s_wtimeo.tv_sec = -1;
  s_eof_error_p = 1;
  s_rbuf.b_ptr = s_rbuf.b_base;
  s_rbuf.b_cnt = 0;
  s_wbuf.b_ptr = s_wbuf.b_base;
  s_wbuf.b_cnt = 0;
}

void
sock::closesock (int no_throw)
{
  SOCKET so = s_so;
  initsock (INVALID_SOCKET);
  if (WS_CALL (closesocket)(so) < 0 && !no_throw)
    throw sock_error ("closesocket");
}

void
sock::close_socket (SOCKET s)
{
  WS_CALL (closesocket)(s);
}

sock::sock ()
{
  initsock (INVALID_SOCKET);
}

sock::sock (SOCKET so)
{
  initsock (so);
}

sock::~sock ()
{
  if (s_so != INVALID_SOCKET)
    {
      try {sflush ();} catch (sock_error &) {}
      shutdown (1, 1);
      closesock (1);
    }
}

void
sock::create (int domain, sock_type type, int proto)
{
  s_so = WS_CALL (socket)(domain, type, proto);
  if (s_so == INVALID_SOCKET)
    throw sock_error ("socket");
}

void
sock::close (int abort)
{
  if (s_so != INVALID_SOCKET)
    {
      if (!abort)
        {
          try
            {
              sflush ();
            }
          catch (sock_error &)
            {
              shutdown (1, 1);
              closesock (1);
              throw;
            }
        }
      shutdown (1, abort);
      closesock (abort);
    }
}

void
sock::shutdown (int how, int no_throw)
{
  if (WS_CALL (shutdown)(s_so, how) < 0 && !no_throw)
    {
      int e = WS_CALL (WSAGetLastError) ();
      if (e != WSAENOTCONN)
        throw sock_error ("shutdown", e);
    }
}

void
sock::cancel ()
{
  WS_CALL (WSACancelBlockingCall)();
}

int
sock::readablep (const timeval &tv) const
{
  fd_set fds;
  FD_ZERO (&fds);
  FD_SET (s_so, &fds);

  int n = WS_CALL (select)(1, &fds, 0, 0, &tv);
  if (n < 0)
    {
      int e = WS_CALL (WSAGetLastError)();
      if (!s_eof_error_p && e == WSAECONNRESET)
        return 1;
      throw sock_error ("select", e);
    }
  return n;
}

int
sock::writablep (const timeval &tv) const
{
  fd_set fds;
  FD_ZERO (&fds);
  FD_SET (s_so, &fds);

  int n = WS_CALL (select)(1, 0, &fds, 0, &tv);
  if (n < 0)
    throw sock_error ("select");
  return n;
}

void
sock::send (const void *buf, int len, int flags) const
{
  for (const char *b = (const char *)buf, *be = b + len; b < be;)
    {
      if (s_wtimeo.tv_sec >= 0 && !writablep (s_wtimeo))
        throw sock_error ("sock::send", WSAETIMEDOUT);
      int n = WS_CALL (send)(s_so, b, min (be - b, 65535), flags);
      if (n <= 0)
        throw sock_error ("send", n ? WS_CALL (WSAGetLastError)() : WSAECONNRESET);
      b += n;
    }
}

void
sock::sendto (const saddr &to, const void *buf, int len, int flags) const
{
  for (const char *b = (const char *)buf, *be = b + len; b < be;)
    {
      if (s_wtimeo.tv_sec >= 0 && !writablep (s_wtimeo))
        throw sock_error ("sock::sendto", WSAETIMEDOUT);
      int n = WS_CALL (sendto)(s_so, b, min (be - b, 65535), flags,
                               to.addr (), to.length ());
      if (n <= 0)
        throw sock_error ("sendto", n ? WS_CALL (WSAGetLastError)() : WSAECONNRESET);
      b += n;
    }
}

int
sock::recv (void *buf, int len, int flags) const
{
  if (s_rtimeo.tv_sec >= 0 && !readablep (s_rtimeo))
    throw sock_error ("sock::recv", WSAETIMEDOUT);
  int n = WS_CALL (recv)(s_so, (char *)buf, min (len, 65535), flags);
  if (n <= 0)
    {
      int e = n ? WS_CALL (WSAGetLastError)() : WSAECONNRESET;
      if (!s_eof_error_p && e == WSAECONNRESET)
        return 0;
      throw sock_error ("recv", e);
    }
  return n;
}

int
sock::recvfrom (saddr &from, void *buf, int len, int flags) const
{
  if (s_rtimeo.tv_sec >= 0 && !readablep (s_rtimeo))
    throw sock_error ("sock::recvfrom", WSAETIMEDOUT);
  int l = from.length ();
  int n = WS_CALL (recvfrom)(s_so, (char *)buf, min (len, 65535), flags,
                             from.addr (), &l);
  if (n <= 0)
    throw sock_error ("recvfrom", n ? WS_CALL (WSAGetLastError)() : WSAECONNRESET);
  return n;
}

void
sock::listen (int backlog) const
{
  if (WS_CALL (listen)(s_so, backlog) < 0)
    throw sock_error ("listen");
}

SOCKET
sock::accept (saddr &addr) const
{
  int l = addr.length ();
  SOCKET so = WS_CALL (accept)(s_so, addr.addr (), &l);
  if (so == INVALID_SOCKET)
    throw sock_error ("accept");
  return so;
}

SOCKET
sock::accept () const
{
  SOCKET so = WS_CALL (accept)(s_so, 0, 0);
  if (so == INVALID_SOCKET)
    throw sock_error ("accept");
  return so;
}

void
sock::bind (const saddr &addr) const
{
  if (WS_CALL (bind)(s_so, addr.addr (), addr.length ()) < 0)
    throw sock_error ("bind");
}

void
sock::connect (const saddr &addr) const
{
  if (WS_CALL (connect)(s_so, addr.addr (), addr.length ()) < 0)
    throw sock_error ("connect");
}

void
sock::peeraddr (saddr &addr) const
{
  int l = addr.length ();
  if (WS_CALL (getpeername)(s_so, addr.addr (), &l) < 0)
    throw sock_error ("getpeername");
}

void
sock::localaddr (saddr &addr) const
{
  int l = addr.length ();
  if (WS_CALL (getsockname)(s_so, addr.addr (), &l) < 0)
    throw sock_error ("getsockname");
}

void
sock::getopt (int level, optname opt, void *val, int l) const
{
  if (WS_CALL (getsockopt)(s_so, level, opt, (char *)val, &l) < 0)
    throw sock_error ("getsockopt");
}

void
sock::setopt (int level, optname opt, const void *val, int l) const
{
  if (WS_CALL (setsockopt)(s_so, level, opt, (const char *)val, l) < 0)
    throw sock_error ("setsockopt");
}

void
sock::ioctl (int cmd, u_long *arg) const
{
  if (WS_CALL (ioctlsocket)(s_so, cmd, arg) < 0)
    throw sock_error ("ioctl");
}

u_short
sock::htons (u_short x)
{
  return WS_CALL (htons)(x);
}

u_long
sock::htonl (u_long x)
{
  return WS_CALL (htonl)(x);
}

u_short
sock::ntohs (u_short x)
{
  return WS_CALL (ntohs)(x);
}

u_long
sock::ntohl (u_long x)
{
  return WS_CALL (ntohl)(x);
}

void
sock::sflush ()
{
  if (s_wbuf.b_ptr > s_wbuf.b_base)
    send (s_wbuf.b_base, s_wbuf.b_ptr - s_wbuf.b_base);
  s_wbuf.b_ptr = s_wbuf.b_base;
  s_wbuf.b_cnt = 0;
}

void
sock::sflush_buf (int c)
{
  sflush ();
  s_wbuf.b_ptr = s_wbuf.b_base + 1;
  s_wbuf.b_cnt = SOCKBUFSIZ - 1;
  *s_wbuf.b_base = c;
}

int
sock::srefill ()
{
  sflush ();
  s_rbuf.b_ptr = s_rbuf.b_base;
  s_rbuf.b_cnt = 0;
  int nread = recv (s_rbuf.b_base, SOCKBUFSIZ);
  if (!nread)
    return eof;
  s_rbuf.b_ptr = s_rbuf.b_base + 1;
  s_rbuf.b_cnt = nread - 1;
  return *s_rbuf.b_base & 0xff;
}

void
sock::sputs (const char *s)
{
  for (; *s; s++)
    sputc (*s);
}

int
sock::sgets (char *buf, size_t size)
{
  if (int (size) <= 0)
    return 0;
  char *b = buf;
  char *const be = buf + size - 1;
  while (b < be)
    {
      int c = sgetc ();
      if (c == eof)
        break;
      *b++ = c;
      if (c == '\n')
        break;
    }
  *b = 0;
  return b - buf;
}

void
sock::sungetc (int c)
{
  if (c >= 0 && s_rbuf.b_ptr > s_rbuf.b_base)
    {
      *--s_rbuf.b_ptr = (char)c;
      s_rbuf.b_cnt++;
    }
}

hostent *
sock::netdb::host (const char *hostname)
{
  return s_resolver.lookup_host (hostname);
}

hostent *
sock::netdb::host (const void *addr, int addrlen, int type)
{
  return s_resolver.lookup_host (addr, addrlen, type);
}

servent *
sock::netdb::serv (const char *service, const char *proto)
{
  return s_resolver.lookup_serv (service, proto);
}
