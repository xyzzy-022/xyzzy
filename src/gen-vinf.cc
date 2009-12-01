#include <stdio.h>
#include "version.h"

#define COPYRIGHT "Copyright (C) 1996-2005 T.Kamei"

static const char inf[] = "\
VS_VERSION_INFO VERSIONINFO\n\
  FILEVERSION %d,%d,%d,%d\n\
  PRODUCTVERSION %d,%d,%d,%d\n\
  FILEFLAGSMASK 0x3fL\n\
#ifdef DEBUG\n\
  FILEFLAGS 0x1L\n\
#else\n\
  FILEFLAGS 0x0L\n\
#endif\n\
  FILEOS 0x40004L\n\
  FILETYPE 0x1L\n\
  FILESUBTYPE 0x0L\n\
BEGIN\n\
  BLOCK \"StringFileInfo\"\n\
  BEGIN\n\
    BLOCK \"041104b0\"\n\
    BEGIN\n\
      VALUE \"CompanyName\", \" \\0\"\n\
      VALUE \"FileDescription\", \"%s\\0\"\n\
      VALUE \"FileVersion\", \"%d, %d, %d, %d\\0\"\n\
      VALUE \"InternalName\", \"%s\\0\"\n\
      VALUE \"LegalCopyright\", \"%s\\0\"\n\
      VALUE \"OriginalFilename\", \"%s.exe\\0\"\n\
      VALUE \"ProductName\", \"%s\\0\"\n\
      VALUE \"ProductVersion\", \"%d, %d, %d, %d\\0\"\n\
    END\n\
  END\n\
  BLOCK \"VarFileInfo\"\n\
  BEGIN\n\
    VALUE \"Translation\", 0x411, 1200\n\
  END\n\
END\n\
";

int
main ()
{
  printf (inf,
          PROGRAM_MAJOR_VERSION, PROGRAM_MINOR_VERSION,
          PROGRAM_MAJOR_REVISION, PROGRAM_MINOR_REVISION,
          PROGRAM_MAJOR_VERSION, PROGRAM_MINOR_VERSION,
          PROGRAM_MAJOR_REVISION, PROGRAM_MINOR_REVISION,
          PROGRAM_NAME,
          PROGRAM_MAJOR_VERSION, PROGRAM_MINOR_VERSION,
          PROGRAM_MAJOR_REVISION, PROGRAM_MINOR_REVISION,
          PROGRAM_NAME,
          COPYRIGHT,
          PROGRAM_NAME,
          PROGRAM_NAME,
          PROGRAM_MAJOR_VERSION, PROGRAM_MINOR_VERSION,
          PROGRAM_MAJOR_REVISION, PROGRAM_MINOR_REVISION);
  return 0;
}
