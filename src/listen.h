#ifndef _listen_h_
#define _listen_h_

static const char xyzzysrv_name[] = "xyzzy-server v2";

// wm_private_xyzzysrv
//  wparam: process ID
//  lparam: handle

struct xyzzysrv_param
{
  int size;
  DWORD pid;
  HANDLE hevent;
  HWND hwnd;
  int kill_ok;
  char data[1];
};

#endif
