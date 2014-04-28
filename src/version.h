#ifndef _version_h_
# define _version_h_

# define PROGRAM_NAME "xyzzy"
# define PROGRAM_APP_USER_MODEL_ID PROGRAM_NAME
// ファイルバージョンには複数書けない
// # define PROGRAM_COPYRIGHT "Copyright (C) 1996-2005 T.Kamei"
# define PROGRAM_COPYRIGHT "Copyright (C) 2012-2013 xyzzy Project"

# define PROGRAM_MAJOR_VERSION 0
# define PROGRAM_MINOR_VERSION 2
# define PROGRAM_MAJOR_REVISION 2
# define PROGRAM_MINOR_REVISION 253
# define PROGRAM_PATCH_LEVEL 0

# define TITLE_BAR_STRING_SIZE 256
extern char TitleBarString[];
extern const char VersionString[];
extern const char DisplayVersionString[];
extern const char ProgramName[];
extern const char ProgramNameWithVersion[];
extern const char ProgramAppUserModelId[];

#endif
