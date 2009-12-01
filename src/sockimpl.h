#ifndef _sockimpl_h_
#define _sockimpl_h_

#define WINSOCK_VOID

#define WINSOCK_FUNCTIONS \
  WSOCKDEF (SOCKET, accept, (SOCKET, struct sockaddr *, int *), INVALID_SOCKET) \
  WSOCKDEF (int, bind, (SOCKET, const struct sockaddr *, int), SOCKET_ERROR) \
  WSOCKDEF (int, closesocket, (SOCKET), SOCKET_ERROR) \
  WSOCKDEF (int, connect, (SOCKET, const struct sockaddr *, int), SOCKET_ERROR) \
  WSOCKDEF (int, ioctlsocket, (SOCKET, long, u_long *), SOCKET_ERROR) \
  WSOCKDEF (int, getpeername, (SOCKET, struct sockaddr *, int *), SOCKET_ERROR) \
  WSOCKDEF (int, getsockname, (SOCKET, struct sockaddr *, int *), SOCKET_ERROR) \
  WSOCKDEF (int, getsockopt, (SOCKET, int, int, char *, int *), SOCKET_ERROR) \
  WSOCKDEF (u_long, htonl, (u_long), 0) \
  WSOCKDEF (u_short, htons, (u_short), 0) \
  WSOCKDEF (unsigned long, inet_addr, (const char *), INADDR_NONE) \
  WSOCKDEF (char *, inet_ntoa, (struct in_addr), 0) \
  WSOCKDEF (int, listen, (SOCKET, int), SOCKET_ERROR) \
  WSOCKDEF (u_long, ntohl, (u_long), 0) \
  WSOCKDEF (u_short, ntohs, (u_short), 0) \
  WSOCKDEF (int, recv, (SOCKET, char *, int, int), SOCKET_ERROR) \
  WSOCKDEF (int, recvfrom, (SOCKET, char *, int, int, struct sockaddr *, int *), SOCKET_ERROR) \
  WSOCKDEF (int, select, (int, fd_set *, fd_set *, fd_set *, const struct timeval *), SOCKET_ERROR) \
  WSOCKDEF (int, send, (SOCKET, const char *, int, int), SOCKET_ERROR) \
  WSOCKDEF (int, sendto, (SOCKET, const char *, int, int, const struct sockaddr *, int), SOCKET_ERROR) \
  WSOCKDEF (int, setsockopt, (SOCKET, int, int, const char *, int), SOCKET_ERROR) \
  WSOCKDEF (int, shutdown, (SOCKET, int), SOCKET_ERROR) \
  WSOCKDEF (SOCKET, socket, (int, int, int), INVALID_SOCKET) \
  WSOCKDEF (int, gethostname, (char *, int), SOCKET_ERROR) \
  WSOCKDEF (int, WSAStartup, (WORD, LPWSADATA), WSASYSNOTREADY) \
  WSOCKDEF (int, WSACleanup, (), SOCKET_ERROR) \
  WSOCKDEF (void, WSASetLastError, (int), WINSOCK_VOID) \
  WSOCKDEF (int, WSAGetLastError, (), WSASYSNOTREADY) \
  WSOCKDEF (BOOL, WSAIsBlocking, (), 0) \
  WSOCKDEF (int, WSAUnhookBlockingHook, (), SOCKET_ERROR) \
  WSOCKDEF (FARPROC, WSASetBlockingHook, (FARPROC), 0) \
  WSOCKDEF (int, WSACancelBlockingCall, (), SOCKET_ERROR) \
  WSOCKDEF (HANDLE, WSAAsyncGetServByName, (HWND, u_int, const char *, const char *, char *, int), 0) \
  WSOCKDEF (HANDLE, WSAAsyncGetServByPort, (HWND, u_int, int, const char *, char *, int), 0) \
  WSOCKDEF (HANDLE, WSAAsyncGetProtoByName, (HWND, u_int, const char *, char *, int), 0) \
  WSOCKDEF (HANDLE, WSAAsyncGetProtoByNumber, (HWND, u_int, int, char *, int), 0) \
  WSOCKDEF (HANDLE, WSAAsyncGetHostByName, (HWND, u_int, const char *, char *, int), 0) \
  WSOCKDEF (HANDLE, WSAAsyncGetHostByAddr, (HWND, u_int, const char *, int, int, char *, int), 0) \
  WSOCKDEF (int, WSACancelAsyncRequest, (HANDLE), SOCKET_ERROR)


struct WINSOCK
{
#define WSOCKDEF(TYPE, NAME, ARGS, RESULT) static TYPE (WINAPI *NAME) ARGS;
  WINSOCK_FUNCTIONS
#undef WSOCKDEF
};

#define WS_CALL(FN) (*WINSOCK::FN)

#endif /* _sockimpl_h_ */
