#ifndef _vwin32_h_
#define _vwin32_h_

// DeviceIoControl infrastructure
#ifndef VWIN32_DIOC_DOS_IOCTL

#define VWIN32_DIOC_DOS_IOCTL 1
#define VWIN32_DIOC_DOS_DRIVEINFO 6

typedef struct
{
  DWORD reg_EBX;
  DWORD reg_EDX;
  DWORD reg_ECX;
  DWORD reg_EAX;
  DWORD reg_EDI;
  DWORD reg_ESI;
  DWORD reg_Flags;
}
  DIOC_REGISTERS;

#endif /* VWIN32_DIOC_DOS_IOCTL */

#define X86_CARRY_FLAG 1        // Intel x86 processor status flags

// DOS IOCTL function support
#pragma pack(1)
// Parameters for locking/unlocking removable media
typedef struct _PARAMBLOCK
{
  BYTE bOperation;
  BYTE bNumLocks;
} PARAMBLOCK, *PPARAMBLOCK;

#pragma pack()

typedef struct
{
  WORD Size;
  WORD Level;
  DWORD SectorsPerCluster;
  DWORD BytesPerSector;
  DWORD AvailableClusters;
  DWORD TotalClusters;
  DWORD AvailablePhysSectors;
  DWORD TotalPhysSectors;
  DWORD AvailableAllocationUnits;
  DWORD TotalAllocationUnits;
  DWORD Rsvd[2];
}
  ExtGetDskFreSpcStruc;

#endif /* _vwin32_h_ */
