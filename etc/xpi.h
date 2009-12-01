#ifndef _XPI_H_
#define _XPI_H_

#ifdef __cplusplus
extern "C" {
#endif

#define XPIS_LEFT    0
#define XPIS_TOP     1
#define XPIS_RIGHT   2
#define XPIS_BOTTOM  3
#define XPIS_NOMOVE  4
#define XPIS_POSMASK 7
#define XPIS_ORDMASK 0x30
#define XPIS_OUTSIDE 0x10
#define XPIS_INSIDE  0x20
#define XPIS_NOORDER 0x30
#define XPIS_GROUP   0x40

typedef HANDLE XPIHANDLE;

BOOL WINAPI xpiInit (const void *arg);
XPIHANDLE WINAPI xpiCreatePane (HWND hwnd, int cx, int cy, DWORD flags);
BOOL WINAPI xpiSetPaneSize (XPIHANDLE h, int size, int min, int max, int step);
BOOL WINAPI xpiSetPanePos (XPIHANDLE h, DWORD flags);

#ifdef __cplusplus
}
#endif

#endif /* _XPI_H_ */
