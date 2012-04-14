#include "stdafx.h"
#ifdef __XYZZY__
#include "ed.h"
#endif
#include "resolver.h"
#include "sockimpl.h"

static const char resolver_wndclass[] = "resolverClass";
int resolver::r_initialized;

resolver::resolver (int timeout)
     : r_hwnd (0), r_hsock (0), r_timeout (timeout)
{
}

resolver::~resolver ()
{
#ifndef __XYZZY__
  if (r_hwnd)
    DestroyWindow (r_hwnd);
#endif
}

int
resolver::initialize (HINSTANCE hinst)
{
  if (r_initialized)
    return 1;

  WNDCLASS wc;
  memset (&wc, 0, sizeof wc);
  wc.lpfnWndProc = wndproc;
  wc.cbWndExtra = sizeof (resolver *);
  wc.hInstance = hinst;
  wc.lpszClassName = resolver_wndclass;
  if (!RegisterClass (&wc))
    return 0;
  r_initialized = 1;
  return 1;
}

int
resolver::create (HINSTANCE hinst)
{
#ifndef __XYZZY__
  return r_hwnd || CreateWindow (resolver_wndclass, "", WS_OVERLAPPED,
                                 0, 0, 0, 0, HWND_DESKTOP, 0, hinst, this);
#else
  return r_hwnd || CreateWindow (resolver_wndclass, "", WS_CHILD,
                                 0, 0, 0, 0, app.toplev, 0, hinst, this);
#endif
}

void
resolver::post_result (WPARAM wparam, LPARAM lparam)
{
  if (r_hsock)
    {
      r_hsock = 0;
      r_params.wparam = wparam;
      r_params.lparam = lparam;
      r_params.done = 1;
      PostThreadMessage (r_thread_id, wm_result_asyncsock, 0, 0);
    }
}

LRESULT
resolver::wndproc (UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_CREATE:
      return 0;

    case wm_asyncsock:
      if (HANDLE (wparam) != r_hsock)
        break;
      KillTimer (r_hwnd, wparam);
      post_result (wparam, lparam);
      return 1;

    case wm_asyncsockreq:
      if (r_hsock)
        return 0;
      r_params.done = 0;
      r_hsock = HANDLE (wparam);
      r_thread_id = lparam;
      if (r_timeout > 0)
        SetTimer (r_hwnd, wparam, r_timeout, 0);
      return 1;

    case wm_cancel_asyncsock:
      if (HANDLE (wparam) != r_hsock)
        return 0;
      KillTimer (r_hwnd, wparam);
      r_hsock = 0;
      r_params.done = 1;
      return 1;

    case WM_TIMER:
      KillTimer (r_hwnd, wparam);
      if (HANDLE (wparam) != r_hsock)
        break;
      post_bad_result (WSAHOST_NOT_FOUND);
      return 0;

    case WM_DESTROY:
      post_bad_result (WSAEINTR);
      break;
    }
  return DefWindowProc (r_hwnd, msg, wparam, lparam);
}

LRESULT CALLBACK
resolver::wndproc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  resolver *d;
  if (msg == WM_CREATE)
    {
      d = (resolver *)((CREATESTRUCT *)lparam)->lpCreateParams;
      d->r_hwnd = hwnd;
      SetWindowLong (hwnd, 0, LPARAM (d));
    }
  else
    d = (resolver *)GetWindowLong (hwnd, 0);

  LRESULT r = (d ? d->wndproc (msg, wparam, lparam)
               : DefWindowProc (hwnd, msg, wparam, lparam));
  if (msg == WM_NCDESTROY)
    d->r_hwnd = 0;
  return r;
}

int
resolver::wait (HANDLE h)
{
  if (!h)
    return 0;

  r_params.done = 0;
  if (!SendMessage (r_hwnd, wm_asyncsockreq,
                    WPARAM (h), GetCurrentThreadId ()))
    {
      WS_CALL (WSACancelAsyncRequest)(h);
      WS_CALL (WSASetLastError)(WSAEINPROGRESS);
      return 0;
    }

#ifdef __XYZZY__
  kbd_queue::disable_kbd dkbd (app.kbdq);
#endif
  int quit = 0;

  MSG msg;
  while (r_hwnd && GetMessage (&msg, 0, 0, 0))
    {
      TranslateMessage (&msg);
      DispatchMessage (&msg);
#ifdef __XYZZY__
      if (QUITP)
        {
          quit = 1;
          break;
        }
#endif
      if (r_params.done)
        {
          if (!WSAGETASYNCERROR (r_params.lparam))
            return 1;
          if (!r_params.wparam)
            WS_CALL (WSACancelAsyncRequest)(h);
          WS_CALL (WSASetLastError)(WSAGETASYNCERROR (r_params.lparam));
          return 0;
        }
    }
  if (!quit && r_hwnd)
    PostQuitMessage (msg.wParam);
  if (r_hwnd)
    SendMessage (r_hwnd, wm_cancel_asyncsock, WPARAM (h), 0);
  WS_CALL (WSACancelAsyncRequest)(h);
  WS_CALL (WSASetLastError)(WSAEINTR);
  return 0;
}

hostent *
resolver::lookup_host (const char *hostname)
{
  return (r_hwnd && wait (WS_CALL (WSAAsyncGetHostByName)(r_hwnd, wm_asyncsock,
                                                          hostname, r_buf, sizeof r_buf))
          ? (hostent *)r_buf : 0);
}

hostent *
resolver::lookup_host (const void *addr, int addrlen, int type)
{
  return (r_hwnd && wait (WS_CALL (WSAAsyncGetHostByAddr)(r_hwnd, wm_asyncsock,
                                                          (const char *)addr, addrlen, type,
                                                          r_buf, sizeof r_buf))
          ? (hostent *)r_buf : 0);
}

servent *
resolver::lookup_serv (const char *service, const char *proto)
{
  return (r_hwnd && wait (WS_CALL (WSAAsyncGetServByName)(r_hwnd, wm_asyncsock,
                                                          service, proto,
                                                          r_buf, sizeof r_buf))
          ? (servent *)r_buf : 0);
}
