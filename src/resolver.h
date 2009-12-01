#ifndef _resolver_h_
#define _resolver_h_

#include <winsock.h>

class resolver
{
protected:
  struct params
    {
      int done;
      WPARAM wparam;
      LPARAM lparam;
    };

  HWND r_hwnd;
  HANDLE r_hsock;
  DWORD r_thread_id;
  int r_timeout;
  params r_params;
  char r_buf[MAXGETHOSTSTRUCT];

  static int r_initialized;

  LRESULT wndproc (UINT, WPARAM, LPARAM);
  static LRESULT CALLBACK wndproc (HWND, UINT, WPARAM, LPARAM);

  int wait (HANDLE);
  void post_result (WPARAM, LPARAM);
  void post_bad_result (int e)
    {post_result (0, WSAMAKEASYNCREPLY (0, e));}

  enum
    {
      wm_asyncsock = WM_USER + 5,
      wm_asyncsockreq,
      wm_cancel_asyncsock,
      wm_result_asyncsock
    };

public:
  resolver (int = 60000);
  ~resolver ();

  void cancel () {post_bad_result (WSAEINTR);}
  static int initialize (HINSTANCE);
  int create (HINSTANCE);

  hostent *lookup_host (const char *);
  hostent *lookup_host (const void *, int, int);
  servent *lookup_serv (const char *, const char *);
};

#endif /* _resolver_h_ */
