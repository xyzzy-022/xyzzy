#ifndef _mousemsg_h_

#ifndef WM_XBUTTONDOWN
/* wParam:
   The low-order word indicates whether various virtual keys are down.
   The high-order word indicates which button was pressed. */
#define WM_XBUTTONDOWN 0x020B
#define WM_XBUTTONUP 0x020C
#define WM_XBUTTONDBLCLK 0x020D
#define WM_NCXBUTTONDOWN 0x00AB
#define WM_NCXBUTTONUP 0x00AC
#define WM_NCXBUTTONDBLCLK 0x00AD
#endif

#ifndef XBUTTON1
#define XBUTTON1 0x0001
#define XBUTTON2 0x0002
#endif

#ifndef MK_XBUTTON1
#define MK_XBUTTON1 0x0020
#define MK_XBUTTON2 0x0040
#endif

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL (WM_MOUSELAST+1)
#endif

#ifndef MSH_SCROLL_LINES
#define MSH_SCROLL_LINES "MSH_SCROLL_LINES_MSG"
#endif

#ifndef MSH_WHEELMODULE_CLASS
#define MSH_WHEELMODULE_CLASS "MouseZ"
#define MSH_WHEELMODULE_TITLE "Magellan MSWHEEL"
#endif

#ifndef SPI_GETWHEELSCROLLLINES
#define SPI_GETWHEELSCROLLLINES 104
#endif

#ifndef WHEEL_DELTA
#define WHEEL_DELTA 120
#endif
#ifndef WHEEL_PAGESCROLL
#define WHEEL_PAGESCROLL UINT_MAX
#endif

#ifndef MSH_MOUSEWHEEL
#define MSH_MOUSEWHEEL "MSWHEEL_ROLLMSG"
#endif

#endif
