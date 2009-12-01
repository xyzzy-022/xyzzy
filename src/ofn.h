#ifndef _ofn_h_
#define _ofn_h_

struct tagOFN: public tagOFNA
{
#ifndef OPENFILENAME_SIZE_VERSION_400
  void *pvReserved;
  DWORD dwReserved;
  DWORD FlagsEx;
#endif /* OPENFILENAME_SIZE_VERSION_400 */
};

#ifndef OPENFILENAME_SIZE_VERSION_400
#define OPENFILENAME_SIZE_VERSION_400 (sizeof (tagOFNA))
#endif

#define OPENFILENAME_SIZE_VERSION_500 (sizeof (tagOFN))

#ifndef OFN_EXPLORER
#define OFN_EXPLORER             0x00080000
#define OFN_NODEREFERENCELINKS   0x00100000
#define OFN_LONGNAMES            0x00200000
#endif

#ifndef OFN_ENABLEINCLUDENOTIFY
#define OFN_ENABLEINCLUDENOTIFY  0x00400000
#define OFN_ENABLESIZING         0x00800000
#endif

#ifndef OFN_DONTADDTORECENT
#define OFN_DONTADDTORECENT      0x02000000
#define OFN_FORCESHOWHIDDEN      0x10000000
#endif

#ifndef OFN_EX_NOPLACESBAR
#define OFN_EX_NOPLACESBAR       0x00000001
#endif

#endif /* _ofn_h_ */
