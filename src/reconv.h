#ifndef _reconv_h_
#define _reconv_h_

#ifndef WM_IME_REQUEST
#define WM_IME_REQUEST 0x0288
#endif

#ifndef IMR_RECONVERTSTRING
#define IMR_RECONVERTSTRING 4

typedef struct tagRECONVERTSTRING
{
  DWORD dwSize;
  DWORD dwVersion;
  DWORD dwStrLen;
  DWORD dwStrOffset;
  DWORD dwCompStrLen;
  DWORD dwCompStrOffset;
  DWORD dwTargetStrLen;
  DWORD dwTargetStrOffset;
}
  RECONVERTSTRING;

#endif

#define WM_MSIME_RECONVERT "MSIMEReconvert"
#define WM_ATOK_RECONVERT "Atok Message for ReconvertString"

#endif /* _reconv_h_ */
