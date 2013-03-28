#ifndef _sock_h_
#define _sock_h_

#include <winsock.h>

class sock_error
{
  int e_error;
  const char *e_ope;
public:
  sock_error (const char *ope, int error)
       : e_error (error), e_ope (ope) {}
  sock_error (const char *ope);
  int error_code () const {return e_error;}
  const char *ope () const {return e_ope;}
};

class resolver;

class sock
{
public:
  enum sock_type
    {
      sock_stream = SOCK_STREAM,
      sock_dgram = SOCK_DGRAM
    };

  enum optname
    {
      so_acceptconn = SO_ACCEPTCONN,
      so_broadcast = SO_BROADCAST,
      so_debug = SO_DEBUG,
      so_dontlinger = SO_DONTLINGER,
      so_dontroute = SO_DONTROUTE,
      so_error = SO_ERROR,
      so_keepalive = SO_KEEPALIVE,
      so_linger = SO_LINGER,
      so_oobinline = SO_OOBINLINE,
      so_rcvbuf = SO_RCVBUF,
      so_reuseaddr = SO_REUSEADDR,
      so_sndbuf = SO_SNDBUF,
      so_type = SO_TYPE
    };

  enum {maxconn = SOMAXCONN};

  class saddr
    {
    public:
      virtual const sockaddr *addr () const = 0;
      virtual sockaddr *addr () = 0;
      virtual int length () const = 0;
    };

  class netdb
    {
    public:
      static hostent *host (const char *);
      static hostent *host (const void *, int, int);
      static servent *serv (const char *, const char *);
    };
  friend netdb;

  struct solinger: public ::linger
    {
      solinger (int onoff, int linger) {l_onoff = onoff; l_linger = linger;}
    };

  struct sotimeval: public timeval
    {
      sotimeval (int sec, int usec = 0) {tv_sec = sec; tv_usec = usec;}
    };

  enum {SOCKBUFSIZ = 2048};

protected:
  SOCKET s_so;

  timeval s_rtimeo;
  timeval s_wtimeo;

  int s_eof_error_p;

  struct sockbuf
    {
      char b_base[SOCKBUFSIZ];
      char *b_ptr;
      int b_cnt;
      int b_bufsiz;
    };

  sockbuf s_rbuf;
  sockbuf s_wbuf;

  static resolver s_resolver;

private:
  void initsock (SOCKET);
  void closesock (int);

public:
  sock ();
  sock (SOCKET);
  virtual ~sock ();

private:
  sock (const sock &);
  sock &operator = (const sock &);

public:
  static int init_winsock (HINSTANCE);
  static void term_winsock ();

  static const char *errmsg (int);

  void create (int, sock_type, int);
  virtual void close (int = 0);
  void shutdown (int, int);

  static void cancel ();

  void send_timeout (int sec, int usec = 0)
    {s_wtimeo.tv_sec = sec; s_wtimeo.tv_usec = usec;}
  void recv_timeout (int sec, int usec = 0)
    {s_rtimeo.tv_sec = sec; s_rtimeo.tv_usec = usec;}
  void send_timeout (timeval t)
    {send_timeout (t.tv_sec, t.tv_usec);}
  void recv_timeout (timeval t)
    {recv_timeout (t.tv_sec, t.tv_usec);}

  const timeval &send_timeout () const {return s_wtimeo;}
  const timeval &recv_timeout () const {return s_rtimeo;}

  int readablep (const timeval &) const;
  int writablep (const timeval &) const;
  int readablep (int sec, int usec = 0) const
    {return readablep (sotimeval (sec, usec));}
  int writablep (int sec, int usec = 0) const
    {return writablep (sotimeval (sec, usec));}

  virtual bool sslp () const {return false;}
  SOCKET &socket () {return s_so;}

protected:
  void sflush_buf (int);
  int srefill ();

public:
  enum {eof = -1};

  void sflush ();

  void sputc (int c)
    {--s_wbuf.b_cnt >= 0 ? (*s_wbuf.b_ptr++ = c) : sflush_buf (c);}
  int sgetc ()
    {return --s_rbuf.b_cnt >= 0 ? *s_rbuf.b_ptr++ & 0xff : srefill ();}
  void sputs (const char *);
  int sgets (char *, size_t);
  void sungetc (int c);
  int no_hang_p () const
    {return s_rbuf.b_cnt > 0 || readablep (0);}

  void set_eof_error_p (int f) {s_eof_error_p = f;}
  const int eof_error_p () const {return s_eof_error_p;}

  virtual void send (const void *, int, int = 0) const;
  void sendto (const saddr &, const void *, int, int = 0) const;
  virtual int recv (void *, int, int = 0) const;
  int recvfrom (saddr &, void *, int, int = 0) const;
  void listen (int = maxconn) const;
  SOCKET accept (saddr &) const;
  SOCKET accept () const;
  void bind (const saddr &) const;
  virtual void connect (const saddr &) const;
  void peeraddr (saddr &) const;
  void localaddr (saddr &) const;
  void getopt (int, optname, void *, int) const;
  void setopt (int, optname, const void *, int) const;
  void ioctl (int, u_long *) const;

  static u_short htons (u_short);
  static u_long htonl (u_long);
  static u_short ntohs (u_short);
  static u_long ntohl (u_long);
  static void close_socket (SOCKET);

  int getopt (int level, optname opt) const
    {int i; getopt (level, opt, &i, sizeof i); return i;}
  void setopt (int level, optname opt, int i) const
    {setopt (level, opt, &i, sizeof i);}

  int acceptconn () const
    {return getopt (SOL_SOCKET, so_acceptconn);}
  int broadcast () const
    {return getopt (SOL_SOCKET, so_broadcast);}
  void broadcast (int on) const
    {setopt (SOL_SOCKET, so_broadcast, on);}
  ::linger linger () const
    {::linger x; getopt (SOL_SOCKET, so_linger, &x, sizeof x); return x;}
  void linger (const ::linger &x) const
    {setopt (SOL_SOCKET, so_linger, &x, sizeof x);}
  int debug () const
    {return getopt (SOL_SOCKET, so_debug);}
  void debug (int on) const
    {setopt (SOL_SOCKET, so_debug, on);}
  int dontlinger () const
    {return getopt (SOL_SOCKET, so_dontlinger);}
  void dontlinger (int on) const
    {setopt (SOL_SOCKET, so_dontlinger, on);}
  int dontroute () const
    {return getopt (SOL_SOCKET, so_dontroute);}
  void dontroute (int on) const
    {setopt (SOL_SOCKET, so_dontroute, on);}
  int clearerror () const
    {return getopt (SOL_SOCKET, so_error);}
  int keepalive () const
    {return getopt (SOL_SOCKET, so_keepalive);}
  void keepalive (int on) const
    {setopt (SOL_SOCKET, so_keepalive, on);}
  int oobinline () const
    {return getopt (SOL_SOCKET, so_oobinline);}
  void oobinline (int on) const
    {setopt (SOL_SOCKET, so_oobinline, on);}
  int rcvbuf () const
    {return getopt (SOL_SOCKET, so_rcvbuf);}
  void rcvbuf (int n) const
    {setopt (SOL_SOCKET, so_rcvbuf, n);}
  int reuseaddr () const
    {return getopt (SOL_SOCKET, so_reuseaddr);}
  void reuseaddr (int on) const
    {setopt (SOL_SOCKET, so_reuseaddr, on);}
  int sndbuf () const
    {return getopt (SOL_SOCKET, so_sndbuf);}
  void sndbuf (int n) const
    {setopt (SOL_SOCKET, so_sndbuf, n);}
  sock_type socktype () const
    {return sock_type (getopt (SOL_SOCKET, so_type));}

  int pending_nread () const
    {u_long x = 0; ioctl (FIONREAD, &x); return x;}
};

#endif /* _sock_h_ */
