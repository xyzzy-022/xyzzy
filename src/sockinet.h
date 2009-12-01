#ifndef _sockinet_h_
#define _sockinet_h_

#include "sock.h"

class sockinet: public sock
{
public:
  enum {pf_inet = PF_INET};
  enum {af_inet = AF_INET};
  enum optname {tcp_nodelay = TCP_NODELAY};

  class saddr: public sock::saddr, public sockaddr_in
    {
    public:
      virtual const sockaddr *addr () const
        {return (const sockaddr *)(const sockaddr_in *)this;}
      virtual sockaddr *addr ()
        {return (sockaddr *)(sockaddr_in *)this;}
      virtual int length () const {return sizeof (sockaddr_in);}

      saddr ();
      saddr (const saddr &);
      saddr (u_long, u_short);
      saddr (u_long, const char *, const char * = "tcp");
      saddr (const char *, u_short);
      saddr (const char *, const char *, const char * = "tcp");
#ifdef __XYZZY__
      saddr (lisp, lisp);
#endif

      u_short port () const {return sock::ntohs (sin_port);}
      const char *addrstr () const {return sockinet::ntoa (sin_addr);}
      const char *hostname () const;

      void set_addr (const char *);
      void set_addr (u_long);
      void set_port (const char *, const char *);
      void set_port (u_short);
#ifdef __XYZZY__
      void set_addr (lisp);
      void set_port (lisp);
#endif
    };

public:
  sockinet () {}
  sockinet (SOCKET so) : sock (so) {}
  virtual ~sockinet () {}

  void create (sock::sock_type type = sock::sock_stream, int proto = 0)
    {sock::create (pf_inet, type, proto);}

  int tcpnodelay () const
    {return getopt (IPPROTO_TCP, sock::optname (tcp_nodelay));}
  void tcpnodelay (int on) const
    {setopt (IPPROTO_TCP, sock::optname (tcp_nodelay), on);}
  static const char *ntoa (in_addr);
};

#endif /* _sockinet_h_ */
